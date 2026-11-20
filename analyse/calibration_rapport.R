# Calcul de tous les chiffres du rapport (calibration par période et simulations).
# Usage : Rscript analyse/calibration_rapport.R <fichier_cac40> <fichier_taux_10ans>
# Fichiers : exports Investing.com (CSV ou XLSX), colonnes Date / Price et Date / Dernier.
suppressMessages({library(tidyverse); library(lubridate); library(readxl); library(jsonlite)})
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2)

# Lecture robuste d'un fichier de cours (CSV ou XLSX).
# Les exports d'Investing.com ouverts dans Excel mélangent deux formats de date :
# du texte "m/d/Y" (jour > 12) et des cellules date où Excel a inversé jour et mois
# (jour <= 12). Ces cellules sont remises dans le bon ordre.
read_market_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("csv", "txt")) {
    df <- readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
    dates <- lubridate::as_date(lubridate::parse_date_time(df[[1]], orders = c("Y-m-d", "m/d/Y", "d/m/Y")))
  } else {
    df <- readxl::read_excel(path, col_types = "list")
    raw <- df[[1]]
    is_dt <- vapply(raw, function(x) inherits(x, "POSIXct"), logical(1))
    mixed <- any(is_dt) && any(!is_dt)
    dates <- as.Date(rep(NA_real_))[seq_along(raw)]
    for (i in seq_along(raw)) {
      x <- raw[[i]]
      if (is_dt[i]) {
        d <- as.Date(x)
        dates[i] <- if (mixed) lubridate::make_date(lubridate::year(d), lubridate::day(d), lubridate::month(d)) else d
      } else {
        dates[i] <- lubridate::as_date(lubridate::parse_date_time(as.character(x), orders = c("Y-m-d", "m/d/Y", "d/m/Y"), quiet = TRUE))
      }
    }
    df <- as.data.frame(lapply(df, function(col) vapply(col, function(v) if (is.null(v) || length(v) == 0) NA_character_ else as.character(v), character(1))),
                        stringsAsFactors = FALSE, check.names = FALSE)
  }
  names(df) <- make.names(names(df))
  df$Date <- dates
  n_lost <- sum(is.na(df$Date))
  if (n_lost > 0) warning(sprintf("%d lignes sans date lisible ont été écartées.", n_lost))
  df <- df[!is.na(df$Date), ]
  df[order(df$Date), ]
}

cac <- read_market_file(args[1], args[1]) %>% transmute(Date, Price = as.numeric(gsub(",", "", Price)))
bund <- read_market_file(args[2], args[2]) %>% transmute(Date, Yield = as.numeric(gsub("%", "", gsub(",", ".", Dernier))))
stopifnot(!anyDuplicated(cac$Date), !anyDuplicated(bund$Date))
out <- list(n_cac = nrow(cac), cac_range = as.character(range(cac$Date)),
            n_bund = nrow(bund), bund_range = as.character(range(bund$Date)))
# ---- Market SCR (Solvency II standard formula vs. GSE scenarios) ----------
# Stylised life-insurer balance sheet: equities, bonds (zero-coupon equivalent
# of duration dur_a) and best-estimate liabilities (duration dur_l), all
# discounted at a flat rate equal to the 10-year yield. Amounts in EUR m,
# rates in %. Loss-absorbing capacity of technical provisions is ignored.
SII_UP   <- c(.70, .70, .64, .59, .55, .52, .49, .47, .44, .42, .39, .37, .35, .34, .33, .31, .30, .29, .27, .26)
SII_DOWN <- c(.75, .65, .56, .50, .46, .42, .39, .36, .33, .31, .30, .29, .28, .28, .27, .28, .28, .28, .29, .29)

# Relative shock at maturity t (Delegated Regulation 2015/35, art. 166-167):
# table for 1-20 years, linear interpolation, 20 % beyond 90 years.
sii_rel_shock <- function(t, tab, below_1y) {
  stopifnot(is.numeric(t), length(t) == 1, t >= 0)
  if (t < 1) return(below_1y)
  if (t <= 20) return(approx(1:20, tab, xout = t)$y)
  if (t >= 90) return(0.20)
  approx(c(20, 90), c(tab[20], 0.20), xout = t)$y
}
# Upward shock: at least +1 point. Downward shock: nil for negative rates.
sii_rate_up   <- function(r, t) r + max(r * sii_rel_shock(t, SII_UP, 0.70), 1)
sii_rate_down <- function(r, t) if (r > 0) r * (1 - sii_rel_shock(t, SII_DOWN, 0.75)) else r

# Change in net asset value for an equity return and new discount rates.
nav_change <- function(bs, eq_ret, r_a, r_l) {
  d_bonds <- bs$bonds * (exp(-(r_a - bs$r0) / 100 * bs$dur_a) - 1)
  d_liab  <- bs$liab  * (exp(-(r_l - bs$r0) / 100 * bs$dur_l) - 1)
  bs$equity * eq_ret + d_bonds - d_liab
}

scr_standard <- function(bs, equity_shock = 0.39, sym_adj = 0) {
  stopifnot(abs(sym_adj) <= 0.10)
  eq  <- bs$equity * (equity_shock + sym_adj)
  up   <- -nav_change(bs, 0, sii_rate_up(bs$r0, bs$dur_a),   sii_rate_up(bs$r0, bs$dur_l))
  down <- -nav_change(bs, 0, sii_rate_down(bs$r0, bs$dur_a), sii_rate_down(bs$r0, bs$dur_l))
  int <- max(up, down, 0)
  rho <- if (down > up && down > 0) 0.5 else 0   # correlation A of the market module
  mkt <- sqrt(eq^2 + int^2 + 2 * rho * eq * int)
  list(equity = eq, rate_up = up, rate_down = down, rate = int,
       direction = if (int == 0) "none" else if (down > up) "down" else "up",
       rho = rho, market = mkt, diversification = eq + int - mkt)
}

# SCR from the scenarios: 99.5 % loss of NAV over one year (12-month equity
# return and 12-month yield applied to today's balance sheet).
scr_gse <- function(bs, S0, S12, r12, level = 0.995) {
  stopifnot(length(S12) == length(r12), length(S12) >= 200)
  d_eq  <- bs$equity * (S12 / S0 - 1)
  d_int <- nav_change(bs, 0, r12, r12)
  d_nav <- d_eq + d_int
  q <- function(x) -unname(quantile(x, 1 - level))
  list(equity = q(d_eq), rate = q(d_int), market = q(d_nav),
       diversification = q(d_eq) + q(d_int) - q(d_nav), d_nav = d_nav)
}

make_balance_sheet <- function(total_assets, equity_share, dur_a, liab, dur_l, r0) {
  stopifnot(total_assets > 0, equity_share >= 0, equity_share <= 1, liab >= 0, dur_a >= 0, dur_l >= 0)
  list(equity = total_assets * equity_share, bonds = total_assets * (1 - equity_share),
       liab = liab, dur_a = dur_a, dur_l = dur_l, r0 = r0, own_funds = total_assets - liab)
}

calib <- function(sd, bd){
  lr <- diff(log(sd$Price)); s <- sd(lr)*sqrt(252); m <- mean(lr)*252
  bm <- bd %>% mutate(YM=format(Date,"%Y-%m")) %>% group_by(YM) %>% filter(Date==max(Date)) %>% ungroup() %>% arrange(Date) %>% mutate(Yn=lead(Yield)) %>% filter(!is.na(Yn))
  f <- lm(Yn ~ Yield, bm); dt <- 1/12; sl <- coef(f)[2]; ic <- coef(f)[1]
  a <- -log(sl)/dt; b <- ic/(1-exp(-a*dt)); sv <- sqrt(summary(f)$sigma^2*2*a/(1-exp(-2*a*dt)))
  ml <- function(df,col) df %>% mutate(YM=format(Date,"%Y-%m")) %>% group_by(YM) %>% filter(Date==max(Date)) %>% ungroup() %>% select(YM, v=all_of(col)) %>% arrange(YM)
  sm <- ml(sd,"Price") %>% mutate(dS=log(v/lag(v))); rm <- ml(bd,"Yield") %>% mutate(dR=v-lag(v))
  mm <- inner_join(select(sm,YM,dS), select(rm,YM,dR), by="YM") %>% drop_na()
  list(logret=m, sigma=s, mu=m+s^2/2, slope=unname(sl), a=unname(a), b=unname(b), sigma_r=unname(sv),
       halflife=log(2)/unname(a), rho=cor(mm$dS,mm$dR), n_months=nrow(mm), n_days=nrow(sd), n_bm=nrow(bm),
       S0=tail(sd$Price,1), r0=tail(bd$Yield,1), lastS=as.character(tail(sd$Date,1)), lastR=as.character(tail(bd$Date,1)))
}
per <- list(all=c("1900-01-01","2100-01-01"), pre=c("1900-01-01","2019-12-31"), post=c("2020-01-01","2100-01-01"))
cal <- lapply(per, function(p) calib(filter(cac, Date>=p[1], Date<=p[2]), filter(bund, Date>=p[1], Date<=p[2])))
# simulation (période complète), même schéma que l'appli
sim <- function(p, nScen=10000, shock=FALSE, seed=2025){
  set.seed(seed); n<-60; dt<-1/12
  S <- matrix(NA,n+1,nScen); R <- S; S[1,]<-p$S0; R[1,]<-p$r0
  for(i in 1:n){ Zs<-rnorm(nScen); Ze<-p$rho*Zs+sqrt(1-p$rho^2)*rnorm(nScen)
    S[i+1,]<-S[i,]*exp((p$mu-0.5*p$sigma^2)*dt+p$sigma*sqrt(dt)*Zs)
    R[i+1,]<-p$b+(R[i,]-p$b)*exp(-p$a*dt)+p$sigma_r*sqrt((1-exp(-2*p$a*dt))/(2*p$a))*Ze }
  if(shock){ S[13:61,]<-S[13:61,]*0.7; R[13:61,]<-R[13:61,]-1 }
  list(S=S,R=R)}
res <- list()
for (k in names(cal)) { p <- cal[[k]]; x <- sim(p); y <- sim(p, shock=TRUE)
  q <- function(v) unname(quantile(v, c(.005,.05,.25,.5,.75,.95)))
  res[[k]] <- list(S5=q(x$S[61,]), R5=q(x$R[61,]), S5_shock=q(y$S[61,]), R5_shock=q(y$R[61,]),
    p_loss=mean(x$S[61,]<p$S0), p_neg=mean(x$R[61,]<0), p_neg_shock=mean(y$R[61,]<0), p_neg_any=mean(apply(x$R,2,min)<0),
    rho_final=cor(x$S[61,],x$R[61,]), ret_q005=quantile(x$S[13,]/p$S0-1,.005)[[1]])
  bs <- make_balance_sheet(1000, 0.15, 6, 900, 10, p$r0)
  g <- scr_gse(bs, p$S0, x$S[13,], x$R[13,]); sf <- scr_standard(bs)
  res[[k]]$scr_gse <- g[c("equity","rate","market","diversification")]
  res[[k]]$scr_sf <- sf; res[[k]]$r12_q <- unname(quantile(x$R[13,], c(.005,.5,.995)))
  if (k=="all") write_csv(tibble(d_nav=g$d_nav), "dnav.csv")
  if (k=="all") { write_csv(as_tibble(t(apply(x$S,1,quantile,c(.05,.25,.5,.75,.95)))),"fanS.csv")
    write_csv(as_tibble(t(apply(x$R,1,quantile,c(.05,.25,.5,.75,.95)))),"fanR.csv")
    write_csv(tibble(S=x$S[61,],R=x$R[61,]),"final.csv"); write_csv(as_tibble(x$S[,1:8], .name_repair = ~ paste0("s", seq_along(.x))),"pathsS.csv"); write_csv(tibble(r12=x$S[13,]/p$S0-1),"ret12.csv") } }
out$cal <- cal; out$sim <- res
write_json(out,"stats.json",auto_unbox=TRUE,digits=6,pretty=TRUE)

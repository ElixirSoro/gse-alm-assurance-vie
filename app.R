#=== Load libraries and define UI ===
library(shiny)
library(tidyverse)
library(lubridate)
library(readxl)
library(plotly)

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

ui <- fluidPage(
  titlePanel("Economic Scenario Generator: CAC40 & 10-Year Bond Yield"),
  sidebarLayout(
    sidebarPanel(
      # File inputs for CAC40 and bond yield data
      fileInput("stockFile", "Upload CAC40 Data (CSV or XLSX)", 
                accept = c(".csv", ".xls", ".xlsx")),
      fileInput("bondFile", "Upload 10Y Bond Yield Data (CSV or XLSX)", 
                accept = c(".csv", ".xls", ".xlsx")),
      # Calibration period selection
      radioButtons("period", "Calibration Period",
                   choices = c("Pre-COVID (<=2019)" = "pre",
                               "Post-COVID (>=2020)" = "post",
                               "Full Period" = "all"),
                   selected = "all"),
      # Simulation settings
      numericInput("nScen", "Number of Scenarios", value = 10000, min=100, step=100),
      checkboxInput("shock", "Include COVID-style Shock", value = FALSE),
      actionButton("runSim", "Run Simulation")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview",
                 h4("CAC40 Data (first rows)"),
                 tableOutput("stockPreview"),
                 h4("Bond Yield Data (first rows)"),
                 tableOutput("bondPreview")
        ),
        tabPanel("Calibration",
                 h4("Stock GBM Calibration"),
                 verbatimTextOutput("stockCalib"),
                 h4("Bond Vasicek Calibration"),
                 verbatimTextOutput("bondCalib"),
                 h4("Stock / Yield Correlation"),
                 verbatimTextOutput("corrCalib"),
                 h4("Vasicek Diagnostic Plot"),
                 plotlyOutput("vasicekDiag")
        ),
        tabPanel("Simulation Results",
                 tabsetPanel(type = "tabs",
                             tabPanel("Time Series",
                                      h4("Historical vs Simulated Paths"),
                                      plotlyOutput("tsPlot"),
                                      br(),
                                      sliderInput("scenario_id", "Select Scenario to View", 
                                                  min = 1, max = 100, value = 1, step = 1),
                                      actionButton("update", "Update Scenario"),
                                      br(), br(),
                                      plotlyOutput("scenarioPlot")
                             ),
                             tabPanel("Distribution",
                                      h4("Histogram of Final Outcomes (5-year)"),
                                      plotlyOutput("distPlot"),
                                      h4("Fan Chart of CAC40 Simulations"),
                                      plotlyOutput("fanPlot")
                             ),
                             tabPanel("Correlation",
                                      h4("Final Stock vs. Yield Correlation"),
                                      plotlyOutput("corrPlot")
                             )
                 )
        ),
        tabPanel("Market SCR",
                 h4("Stylised life-insurer balance sheet (EUR m)"),
                 fluidRow(
                   column(4, numericInput("bsAssets", "Total assets", 1000, min = 1),
                             numericInput("bsEqShare", "Equity share (%)", 15, min = 0, max = 100)),
                   column(4, numericInput("bsDurA", "Bond duration (years)", 6, min = 0),
                             numericInput("bsLiab", "Best-estimate liabilities", 900, min = 0)),
                   column(4, numericInput("bsDurL", "Liability duration (years)", 10, min = 0),
                             numericInput("symAdj", "Equity symmetric adjustment (%)", 0, min = -10, max = 10))
                 ),
                 helpText("Assets and liabilities are discounted at a flat rate equal to the last observed 10-year yield.",
                          "The loss-absorbing capacity of technical provisions (profit sharing) is ignored.",
                          "The GSE figure is the 99.5 % one-year loss of net asset value across the simulated scenarios",
                          "(crisis shock excluded); it needs at least 1,000 scenarios to be meaningful."),
                 h4("Standard formula vs. scenarios"),
                 tableOutput("scrTable"),
                 verbatimTextOutput("scrNote"),
                 h4("One-year change in net asset value across scenarios"),
                 plotlyOutput("navPlot")
        )
      )
    )
  )
)



server <- function(input, output, session) {
  # Reactive expression to read and clean CAC40 data
  stockData <- reactive({
    req(input$stockFile)
    df <- read_market_file(input$stockFile$datapath, input$stockFile$name)
    # Convert Price/Close to numeric
    price_col <- if ("Price" %in% names(df)) "Price" else "Close"
    df$Price <- as.numeric(gsub(",", "", df[[price_col]]))
    df %>% filter(!is.na(Price))
  })
  
  # Reactive expression to read and clean bond yield data
  bondData <- reactive({
    req(input$bondFile)
    df <- read_market_file(input$bondFile$datapath, input$bondFile$name)
    # Convert "Dernier"/Close to numeric yield (remove % sign)
    yield_col <- if ("Dernier" %in% names(df)) "Dernier" else "Close"
    df$Yield <- as.numeric(gsub("%", "", gsub(",", ".", df[[yield_col]])))
    df %>% filter(!is.na(Yield))
  })
  
  # Display the first few rows of each dataset
  output$stockPreview <- renderTable({
    head(stockData(), 5)
  })
  output$bondPreview <- renderTable({
    head(bondData(), 5)
  })
  
  # Apply calibration period filter to both datasets
  calData <- reactive({
    sd <- stockData()
    bd <- bondData()
    if(input$period == "pre"){
      sd <- filter(sd, Date < as.Date("2020-01-01"))
      bd <- filter(bd, Date < as.Date("2020-01-01"))
    } else if(input$period == "post"){
      sd <- filter(sd, Date >= as.Date("2020-01-01"))
      bd <- filter(bd, Date >= as.Date("2020-01-01"))
    }
    list(stock = sd, bond = bd)
  })
  
  # Compute GBM parameters from log-returns
  stockParams <- reactive({
    sd <- calData()$stock %>% arrange(Date) %>%
      mutate(LogRet = log(Price/lag(Price))) %>%
      filter(!is.na(LogRet))
    mu_daily <- mean(sd$LogRet, na.rm=TRUE)
    sigma_daily <- sd(sd$LogRet, na.rm=TRUE)
    # Annualize (assume 252 trading days)
    sigma_annual <- sigma_daily * sqrt(252)
    # The mean log-return estimates (mu - sigma^2/2): add sigma^2/2 back to get
    # the GBM drift mu, so that the simulation term (mu - sigma^2/2) is not
    # corrected twice.
    mu_annual <- mu_daily * 252 + 0.5 * sigma_annual^2
    list(mu_ann = mu_annual, sigma_ann = sigma_annual,
         logret_ann = mu_daily * 252)
  })
  output$stockCalib <- renderPrint({
    p <- stockParams()
    cat(sprintf("Mean annual log-return (μ - σ²/2): %.4f\n", p$logret_ann))
    cat(sprintf("Estimated annual drift (μ): %.4f\n", p$mu_ann))
    cat(sprintf("Estimated annual volatility (σ): %.4f\n", p$sigma_ann))
  })
  
  # Compute Vasicek parameters via OLS on monthly yields
  bondParams <- reactive({
    bd <- calData()$bond
    # Take last yield of each month
    bd_monthly <- bd %>%
      mutate(YearMonth = format(Date, "%Y-%m")) %>%
      group_by(YearMonth) %>%
      filter(Date == max(Date)) %>%
      arrange(Date) %>% ungroup()
    # Prepare data for regression
    bd_monthly <- bd_monthly %>% mutate(Y_next = lead(Yield)) %>% filter(!is.na(Y_next))
    lm_fit <- lm(Y_next ~ Yield, data = bd_monthly)
    # Derive Vasicek parameters (dt = 1/12 year)
    dt <- 1/12
    slope <- coef(lm_fit)["Yield"]
    intercept <- coef(lm_fit)["(Intercept)"]
    a <- -log(slope) / dt
    b <- intercept / (1 - exp(-a * dt))
    sigma_res <- summary(lm_fit)$sigma  # std. error of regression
    # Convert regression sigma to continuous σ:
    sigma_v <- sqrt(sigma_res^2 * 2 * a / (1 - exp(-2 * a * dt)))
    list(a = a, b = b, sigma = sigma_v, lm = lm_fit)
  })
  output$bondCalib <- renderPrint({
    bp <- bondParams()
    cat(sprintf("Vasicek a (speed): %.4f\n", bp$a))
    cat(sprintf("Vasicek b (mean level): %.4f\n", bp$b))
    cat(sprintf("Vasicek σ: %.4f\n", bp$sigma))
  })
  # Historical correlation between monthly stock log-returns and monthly
  # yield changes, used to correlate the two Brownian motions
  corrParam <- reactive({
    monthly_last <- function(df, col) {
      df %>%
        mutate(YearMonth = format(Date, "%Y-%m")) %>%
        group_by(YearMonth) %>%
        filter(Date == max(Date)) %>%
        ungroup() %>%
        select(YearMonth, value = all_of(col))
    }
    s_m <- monthly_last(calData()$stock, "Price") %>%
      arrange(YearMonth) %>% mutate(dS = log(value / lag(value)))
    r_m <- monthly_last(calData()$bond, "Yield") %>%
      arrange(YearMonth) %>% mutate(dR = value - lag(value))
    m <- inner_join(select(s_m, YearMonth, dS), select(r_m, YearMonth, dR),
                    by = "YearMonth") %>%
      filter(!is.na(dS), !is.na(dR))
    rho <- if (nrow(m) >= 3) cor(m$dS, m$dR) else 0
    if (is.na(rho)) rho <- 0
    list(rho = rho, n = nrow(m))
  })
  output$corrCalib <- renderPrint({
    cp <- corrParam()
    cat(sprintf("Correlation (monthly stock log-returns vs yield changes): %.4f\n", cp$rho))
    cat(sprintf("Number of common months: %d\n", cp$n))
  })

  # Monthly projection dates (60 months after the last stock observation)
  futureDates <- reactive({
    last_date <- tail(calData()$stock$Date, 1)
    seq(from = last_date %m+% months(1), by = "1 month", length.out = 60)
  })

  # Diagnostic plot: actual vs. fitted for yields
  output$vasicekDiag <- renderPlotly({
    lm_fit <- bondParams()$lm
    df <- lm_fit$model
    p <- ggplot(df, aes(x=Yield, y=Y_next)) +
      geom_point() +
      geom_abline(slope=1, intercept=0, linetype="dashed") +
      labs(x="Yield (t)", y="Yield (t+1)",
           title="Vasicek Fit: Actual vs Next") +
      theme_minimal()
    ggplotly(p)
  })
  
  
  # Simulation: triggered by action button
  simulation <- eventReactive(input$runSim, {
    # Retrieve parameters
    sd <- calData()$stock; bd <- calData()$bond
    params_s <- stockParams(); params_b <- bondParams()
    S0 <- tail(sd$Price,1); r0 <- tail(bd$Yield,1)
    nScen <- input$nScen; nSteps <- 60; dt <- 1/12
    mu <- params_s$mu_ann; sigma <- params_s$sigma_ann
    a <- params_b$a; b <- params_b$b; sigma_v <- params_b$sigma
    rho <- corrParam()$rho
    # Initialize matrices for scenarios
    simS <- matrix(NA, nSteps+1, nScen)
    simR <- matrix(NA, nSteps+1, nScen)
    simS[1,] <- S0; simR[1,] <- r0
    # Simulate paths
    for(i in 1:nSteps){
      Zs <- rnorm(nScen)
      simS[i+1, ] <- simS[i, ] * exp((mu - 0.5*sigma^2)*dt + sigma*sqrt(dt)*Zs)
      # Correlated shock for the yield (Cholesky decomposition in dimension 2)
      Ze <- rho * Zs + sqrt(1 - rho^2) * rnorm(nScen)
      simR[i+1, ] <- b + (simR[i, ] - b)*exp(-a*dt) +
        sigma_v*sqrt((1 - exp(-2*a*dt))/(2*a)) * Ze
    }
    # Keep the 12-month values before any crisis shock, for the SCR tab
    S12 <- simS[13, ]; R12 <- simR[13, ]
    # Apply stress shock if requested
    if(input$shock){
      shock_month <- 12  # at 12 months
      if(shock_month <= nSteps){
        # Stock drops 30%
        simS[(shock_month+1):(nSteps+1), ] <- simS[(shock_month+1):(nSteps+1), ] * 0.7
        # Yield drops by 1 percentage point
        simR[(shock_month+1):(nSteps+1), ] <- simR[(shock_month+1):(nSteps+1), ] - 1.0
      }
    }
    list(simS = simS, simR = simR, S12 = S12, R12 = R12, S0 = S0, r0 = r0)
  })

  # Keep the scenario slider within the number of simulated scenarios
  observeEvent(simulation(), {
    n <- ncol(simulation()$simS)
    updateSliderInput(session, "scenario_id", min = 1, max = n,
                      value = min(input$scenario_id, n))
  })
  
  
  # Plot historical + simulated paths
  output$tsPlot <- renderPlotly({
    res <- simulation()
    simS <- res$simS; simR <- res$simR
    sd <- calData()$stock; bd <- calData()$bond
    future_dates <- futureDates()
    # Prepare data frames for plotting
    histS <- tibble(Date = sd$Date, Price = sd$Price)
    histR <- tibble(Date = bd$Date, Yield = bd$Yield)
    # Sample some simulated paths for plotting
    nShow <- min(50, ncol(simS))
    simS_df <- map_dfr(1:nShow, function(j) {
      tibble(Date = future_dates, Value = simS[-1,j], Scenario = j)
    })
    simR_df <- map_dfr(1:nShow, function(j) {
      tibble(Date = future_dates, Value = simR[-1,j], Scenario = j)
    })
    # CAC40 plot
    p1 <- ggplot() +
      geom_line(data = histS, aes(Date, Price), color="black") +
      geom_line(data = simS_df, aes(Date, Value, group=Scenario),
                color="steelblue", alpha=0.3) +
      labs(title="CAC40: Historical & Simulated", y="Price") + theme_minimal()
    # Yield plot
    p2 <- ggplot() +
      geom_line(data = histR, aes(Date, Yield), color="black") +
      geom_line(data = simR_df, aes(Date, Value, group=Scenario),
                color="darkgreen", alpha=0.3) +
      labs(title="10Y Yield: Historical & Simulated", y="Yield (%)") + theme_minimal()
    subplot(ggplotly(p1), ggplotly(p2), nrows = 2, shareX = FALSE)
  })
  
  # Histogram of final values
  output$distPlot <- renderPlotly({
    res <- simulation()
    finalS <- res$simS[nrow(res$simS), ]
    finalR <- res$simR[nrow(res$simR), ]
    df <- tibble(Value = c(finalS, finalR),
                 Asset = rep(c("CAC40", "Yield"), each=length(finalS)))
    p <- ggplot(df, aes(x=Value, fill=Asset)) +
      geom_histogram(alpha=0.6, position="identity", bins=50) +
      facet_wrap(~Asset, scales="free") +
      labs(title="Histogram of 5-Year Outcomes") + theme_minimal()
    ggplotly(p)
  })
  
  # Fan chart of simulated CAC40
  output$fanPlot <- renderPlotly({
    res <- simulation()
    simS <- res$simS[-1, ]  # drop initial row
    # Compute quantiles over scenarios for each time step
    probs <- seq(0.05, 0.95, by=0.05)
    q_matrix <- apply(simS, 1, quantile, probs=probs)
    dfQ <- as.data.frame(t(q_matrix))
    dfQ$Month <- 1:nrow(dfQ)
    # Build ribbon data for successive quantile bands
    ribbons <- data.frame(
      Month = rep(dfQ$Month, times=length(probs)/2),
      ymin = c(dfQ$X05., dfQ$X15., dfQ$X25., dfQ$X35., dfQ$X45.),
      ymax = c(dfQ$X95., dfQ$X85., dfQ$X75., dfQ$X65., dfQ$X55.)
    )
    p <- ggplot() +
      geom_ribbon(data = ribbons, aes(x=Month, ymin=ymin, ymax=ymax, fill=factor(ymin)), alpha=0.25) +
      geom_line(data = dfQ, aes(x=Month, y=X50.), color="red", linewidth=1) +
      labs(title="CAC40 Fan Chart (by Month)", x="Month", y="Price") + theme_minimal() +
      theme(legend.position="none")
    ggplotly(p)
  })
  
  # Correlation of final stock vs yield
  output$corrPlot <- renderPlotly({
    res <- simulation()
    finalS <- res$simS[nrow(res$simS), ]
    finalR <- res$simR[nrow(res$simR), ]
    corr_val <- round(cor(finalS, finalR), 2)
    df <- tibble(Stock=finalS, Yield=finalR)
    p <- ggplot(df, aes(x=Stock, y=Yield)) +
      geom_point(alpha=0.3) +
      labs(title=paste("Final Stock vs Yield (corr =", corr_val,")")) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Single scenario view, refreshed when "Update Scenario" is clicked
  output$scenarioPlot <- renderPlotly({
    input$update
    res <- simulation()
    req(res$simS, res$simR)
    sid <- isolate(input$scenario_id)
    sid <- min(max(1, sid), ncol(res$simS))  # bounded by the number of scenarios
    future_dates <- futureDates()
    sd <- calData()$stock; bd <- calData()$bond

    scenS <- tibble(Date = future_dates, Price = res$simS[-1, sid])
    scenR <- tibble(Date = future_dates, Yield = res$simR[-1, sid])

    p1 <- ggplot() +
      geom_line(data = tibble(Date = sd$Date, Price = sd$Price), aes(Date, Price)) +
      geom_line(data = scenS, aes(Date, Price), color = "blue") +
      labs(title = paste("CAC40 Scenario", sid), y = "Price") +
      theme_minimal()

    p2 <- ggplot() +
      geom_line(data = tibble(Date = bd$Date, Yield = bd$Yield), aes(Date, Yield)) +
      geom_line(data = scenR, aes(Date, Yield), color = "darkgreen") +
      labs(title = paste("Bond Yield Scenario", sid), y = "Yield (%)") +
      theme_minimal()

    subplot(ggplotly(p1), ggplotly(p2), nrows = 2, shareX = TRUE) %>%
      plotly::layout(title = paste("Scenario", sid))
  })

  # ---- Market SCR tab ----
  scrResults <- reactive({
    res <- simulation()
    req(input$bsAssets > 0, input$bsLiab >= 0, input$bsEqShare >= 0, input$bsEqShare <= 100,
        abs(input$symAdj) <= 10, ncol(res$simS) >= 200)
    bs <- make_balance_sheet(input$bsAssets, input$bsEqShare / 100, input$bsDurA,
                             input$bsLiab, input$bsDurL, res$r0)
    list(bs = bs, sf = scr_standard(bs, sym_adj = input$symAdj / 100),
         gse = scr_gse(bs, res$S0, res$S12, res$R12), n = ncol(res$simS))
  })
  output$scrTable <- renderTable({
    x <- scrResults(); sf <- x$sf; g <- x$gse; of <- x$bs$own_funds
    tibble(
      Module = c("Equity", "Interest rate", "Diversification", "Market SCR", "Solvency ratio (own funds / SCR)"),
      `Standard formula` = c(sf$equity, sf$rate, -sf$diversification, sf$market, 100 * of / sf$market),
      `GSE scenarios (99.5 %)` = c(g$equity, g$rate, -g$diversification, g$market, 100 * of / g$market)
    )
  }, digits = 1)
  output$scrNote <- renderPrint({
    x <- scrResults(); sf <- x$sf
    cat(sprintf("Own funds: %.1f | Starting 10-year yield: %.2f %%\n", x$bs$own_funds, x$bs$r0))
    cat(sprintf("Standard formula interest-rate scenario: %s (up loss %.1f, down loss %.1f), equity/rate correlation %.1f\n",
                sf$direction, sf$rate_up, sf$rate_down, sf$rho))
    if (x$n < 1000) cat("Warning: fewer than 1,000 scenarios, the 99.5 % quantile is unreliable.\n")
  })
  output$navPlot <- renderPlotly({
    x <- scrResults()
    p <- ggplot(tibble(dNAV = x$gse$d_nav), aes(dNAV)) +
      geom_histogram(bins = 80, fill = "steelblue", color = "white") +
      geom_vline(xintercept = -x$gse$market, color = "orange", linewidth = 1) +
      geom_vline(xintercept = -x$sf$market, linetype = "dashed") +
      labs(x = "One-year change in net asset value (EUR m)", y = "Scenarios",
           title = "Orange: GSE 99.5 % loss | dashed: standard formula SCR") +
      theme_minimal()
    ggplotly(p)
  })

}

shinyApp(ui, server)

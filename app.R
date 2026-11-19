#=== Load libraries and define UI ===
library(shiny)
library(tidyverse)
library(lubridate)
library(readxl)
library(plotly)

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
        )
      )
    )
  )
)



server <- function(input, output, session) {
  # Reactive expression to read and clean CAC40 data
  stockData <- reactive({
    req(input$stockFile)
    ext <- tools::file_ext(input$stockFile$name)
    if(ext %in% c("csv","txt")){
      df <- read_csv(input$stockFile$datapath)
    } else {
      df <- read_excel(input$stockFile$datapath)
    }
    # Standardize column names and parse dates
    df <- df %>% rename_all(~ make.names(.)) %>%
      mutate(Date = as.Date(parse_date_time(Date, 
                                            orders = c("Y-m-d","d/m/Y","m/d/Y")))) %>%
      filter(!is.na(Date)) %>% arrange(Date)
    # Convert Price/Close to numeric
    if("Price" %in% names(df)){
      df$Price <- as.numeric(gsub(",", "", df$Price))
    } else if("Close" %in% names(df)){
      df$Price <- as.numeric(gsub(",", "", df$Close))
    }
    df <- df %>% filter(!is.na(Price))
    return(df)
  })
  
  # Reactive expression to read and clean bond yield data
  bondData <- reactive({
    req(input$bondFile)
    ext <- tools::file_ext(input$bondFile$name)
    if(ext %in% c("csv","txt")){
      df <- read_csv(input$bondFile$datapath)
    } else {
      df <- read_excel(input$bondFile$datapath)
    }
    df <- df %>% rename_all(~ make.names(.)) %>%
      mutate(Date = as.Date(parse_date_time(Date, 
                                            orders = c("Y-m-d","d/m/Y","m/d/Y")))) %>%
      filter(!is.na(Date)) %>% arrange(Date)
    # Convert "Dernier"/Close to numeric yield (remove % sign)
    if("Dernier" %in% names(df)){
      df$Yield <- as.numeric(gsub("%","", df$Dernier))
    } else if("Close" %in% names(df)){
      df$Yield <- as.numeric(gsub("%","", df$Close))
    }
    df <- df %>% filter(!is.na(Yield))
    return(df)
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
    list(simS = simS, simR = simR)
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

}

shinyApp(ui, server)

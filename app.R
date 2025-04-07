# --- 必要的套件 ---
library(shiny)
library(ggplot2) # 用於繪圖
library(dplyr)   # 用於資料處理 (方便起見)
library(stringr) # 用於字串處理
library(patchwork) # **** ADDED: Ensure patchwork is loaded ****

# --- 輔助函數：計算眾數 ---
# Note: This function remains the same as in your original code.
calculate_mode <- function(dist_name, params) {
  mode_val <- NA # 預設值

  tryCatch({
    if (dist_name == "常態分佈 (Gaussian)") {
      mode_val <- params$mean
    } else if (dist_name == "對數常態分佈 (Lognormal)") {
      if (params$sdlog > 0) {
        mode_val <- exp(params$meanlog - params$sdlog^2) #
      } else {
        mode_val <- exp(params$meanlog) # 退化情況
      }
    } else if (dist_name == "伽瑪分佈 (Gamma)") {
      if (params$shape >= 1) {
        mode_val <- (params$shape - 1) / params$rate #
      } else {
        mode_val <- 0 # 或接近0，當 shape < 1 時，眾數在0 #
      }
    } else if (dist_name == "t 分佈 (Student's t)") {
      mode_val <- params$mu # t分佈的眾數是其位置參數 (若定義) #
    } else if (dist_name == "指數分佈 (Exponential)") {
      mode_val <- 0 # 指數分佈的眾數永遠是0 #
    } else if (dist_name == "柯西分佈 (Cauchy)") {
      mode_val <- params$location # 柯西分佈的眾數是其位置參數 #
    } else if (dist_name == "貝他分佈 (Beta)") {
      if (params$shape1 > 1 && params$shape2 > 1) {
        mode_val <- (params$shape1 - 1) / (params$shape1 + params$shape2 - 2) #
      } else if (params$shape1 == 1 && params$shape2 == 1) {
        mode_val <- "任意值 (0,1)" # 均勻分佈 #
      } else if (params$shape1 <= 1 && params$shape2 > 1) {
        mode_val <- 0 #
      } else if (params$shape1 > 1 && params$shape2 <= 1) {
        mode_val <- 1 #
      } else { # shape1 <= 1 and shape2 <= 1 (非均勻情況) #
        mode_val <- "0 或 1 (雙峰)" #
      }
    } else if (dist_name == "二項分佈 (Binomial)") {
      mode_val <- floor((params$size + 1) * params$prob) #
      if ( (params$size + 1) * params$prob == mode_val && params$prob != 0 && params$prob != 1) {
        mode_val <- paste(mode_val - 1, "和", mode_val) #
      }
    } else if (dist_name == "卜瓦松分佈 (Poisson)") {
      mode_val <- floor(params$lambda) #
      if (params$lambda == mode_val && params$lambda > 0) {
          mode_val <- paste(mode_val - 1, "和", mode_val) #
      }
    } else if (dist_name == "Uniform") { # Added for CLT
        # For a standard uniform [0,1], any value is technically a mode.
        # For a general uniform [a,b], any value in [a,b] is a mode.
        mode_val <- paste("[", params$min, ",", params$max, "]區間內任意值")
    }
  }, error = function(e) {
    mode_val <- NA #
  })

  if (is.numeric(mode_val)) {
    return(round(mode_val, 4))
  } else {
    return(mode_val) #
  }
}


# --- UI 定義 ---
# UI section remains identical to the previous version with the two tabs
ui <- fluidPage(
  titlePanel("機率分佈特性與中央極限定理展示 (繁體中文)"), # Updated Title

  tabsetPanel( # Use tabs to separate sections
    tabPanel("分佈特性", # Original content in the first tab
      sidebarLayout(
        sidebarPanel(
          selectInput("dist", "選擇分佈類型:",
                      choices = c("常態分佈 (Gaussian)", #
                                  "對數常態分佈 (Lognormal)", #
                                  "伽瑪分佈 (Gamma)", #
                                  "t 分佈 (Student's t)", #
                                  "指數分佈 (Exponential)", #
                                  "柯西分佈 (Cauchy)", #
                                  "貝他分佈 (Beta)", #
                                  "二項分佈 (Binomial)", #
                                  "卜瓦松分佈 (Poisson)")), #
          hr(),
          # --- Conditional panels for parameters (same as original) ---
           # 常態分佈
          conditionalPanel(
            condition = "input.dist == '常態分佈 (Gaussian)'",
            numericInput("norm_mean", "平均數 (μ):", value = 0), #
            numericInput("norm_sd", "標準差 (σ):", value = 1, min = 0.001) #
          ),
          # 對數常態分佈
          conditionalPanel(
            condition = "input.dist == '對數常態分佈 (Lognormal)'",
            numericInput("lnorm_meanlog", "對數平均數 (μ_log):", value = 0), #
            numericInput("lnorm_sdlog", "對數標準差 (σ_log):", value = 1, min = 0.001) #
          ),
          # 伽瑪分佈
          conditionalPanel(
            condition = "input.dist == '伽瑪分佈 (Gamma)'",
            numericInput("gamma_shape", "形狀 (Shape, α):", value = 2, min = 0.001), #
            numericInput("gamma_rate", "速率 (Rate, β):", value = 1, min = 0.001) #
          ),
           # t 分佈
          conditionalPanel(
            condition = "input.dist == 't 分佈 (Student\\'s t)'",
            numericInput("t_df", "自由度 (df):", value = 10, min = 1), #
            numericInput("t_mu", "位置 (μ):", value = 0), #
            numericInput("t_sigma", "尺度 (σ):", value = 1, min = 0.001) #
          ),
          # 指數分佈
          conditionalPanel(
            condition = "input.dist == '指數分佈 (Exponential)'", #
            numericInput("exp_rate", "速率 (Rate, λ):", value = 1, min = 0.001) #
          ),
          # 柯西分佈
          conditionalPanel(
            condition = "input.dist == '柯西分佈 (Cauchy)'",
            numericInput("cauchy_location", "位置 (x0):", value = 0), #
            numericInput("cauchy_scale", "尺度 (γ):", value = 1, min = 0.001) #
          ),
           # 貝他分佈
          conditionalPanel(
            condition = "input.dist == '貝他分佈 (Beta)'", #
            numericInput("beta_shape1", "形狀1 (α):", value = 2, min = 0.001), #
            numericInput("beta_shape2", "形狀2 (β):", value = 5, min = 0.001) #
          ),
           # 二項分佈
          conditionalPanel(
            condition = "input.dist == '二項分佈 (Binomial)'",
            numericInput("binom_size", "試驗次數 (n):", value = 10, min = 1, step = 1), #
            sliderInput("binom_prob", "成功機率 (p):", min = 0, max = 1, value = 0.5, step = 0.01) #
          ),
           # 卜瓦松分佈
          conditionalPanel(
            condition = "input.dist == '卜瓦松分佈 (Poisson)'",
            numericInput("pois_lambda", "平均發生率 (λ):", value = 3, min = 0) #
          ),

          hr(),
          p("注意：對於某些參數組合（如 t 分佈自由度 df=1 或柯西分佈），平均數或更高階動差可能不存在。") #
        ),
        mainPanel(
          plotOutput("distPlot"), #
          h4("統計摘要:"), #
          tableOutput("summaryStats"), #
          h4("分佈說明:"), #
          uiOutput("explanation"), #
          h4("醫學範例:"), #
          uiOutput("medical_example") #
        )
      )
    ), # End of first tabPanel

    tabPanel("Central Limit Theorem Demo", # New tab for CLT
      sidebarLayout(
        sidebarPanel(
          h4("中央極限定理 (CLT) 模擬"),
          p("CLT 指出，從任何具有有限平均數和變異數的獨立同分佈數據中抽取足夠大的隨機樣本，樣本平均數的分佈將近似於常態分佈。CLT 不適用於科西分布，因為它沒有平均值和變異數。"),
          selectInput("clt_dist", "選擇模擬的原始分佈:",
                      choices = c("Uniform", # New dropdown for CLT
                                  "Lognormal",
                                  "Gamma",
                                  "Beta")),
          hr(),
          # --- Parameters for CLT Distributions ---
          conditionalPanel(
            condition = "input.clt_dist == 'Uniform'",
            numericInput("clt_unif_min", "最小值 (a):", value = 0),
            numericInput("clt_unif_max", "最大值 (b):", value = 10)
          ),
          conditionalPanel(
            condition = "input.clt_dist == 'Lognormal'",
            numericInput("clt_lnorm_meanlog", "對數平均數 (μ_log):", value = 0), # Referenced parameter names
            numericInput("clt_lnorm_sdlog", "對數標準差 (σ_log):", value = 1, min = 0.001) # Referenced parameter names
          ),
          conditionalPanel(
            condition = "input.clt_dist == 'Gamma'",
            numericInput("clt_gamma_shape", "形狀 (Shape, α):", value = 2, min = 0.001), # Referenced parameter names
            numericInput("clt_gamma_rate", "速率 (Rate, β):", value = 1, min = 0.001) # Referenced parameter names
          ),
          conditionalPanel(
            condition = "input.clt_dist == 'Beta'",
            numericInput("clt_beta_shape1", "形狀1 (α):", value = 2, min = 0.001), # Referenced parameter names
            numericInput("clt_beta_shape2", "形狀2 (β):", value = 5, min = 0.001) # Referenced parameter names
          ),
          hr(),
          sliderInput("clt_n", "每個樣本的大小 (n):", min = 2, max = 200, value = 30, step = 1),
          sliderInput("clt_reps", "樣本數量 (模擬次數):", min = 100, max = 5000, value = 1000, step = 100),
          actionButton("clt_run", "執行模擬") # **** Make sure to click this button ****
        ),
        mainPanel(
          plotOutput("cltPlot"), # Output for the CLT plot
          h4("原始總體分佈特性:"),
          tableOutput("cltSummaryStats"), # Output for population stats
          h4("樣本平均數分佈特性 (模擬結果):"),
          verbatimTextOutput("cltSimResults") # Output for simulation text summary
        )
      )
    ) # End of second tabPanel
  ) # End of tabsetPanel
)

# --- Server 邏輯 ---
server <- function(input, output, session) {

  # --- Original Server Logic for Distribution Characteristics Tab ---
  # This section remains largely the same as your original server code
  # Make sure req() and validation checks use input IDs from the first tab (e.g., input$norm_sd)

  get_params <- reactive({
    params <- list()
    dist_name <- input$dist
    # --- Parameter fetching logic (same as original, uses input$dist, input$norm_mean etc.) ---
    if (dist_name == "常態分佈 (Gaussian)") {
      req(input$norm_sd) # Ensure input exists from the correct tab
      if(input$norm_sd <= 0) { #
          showNotification("常態分佈的標準差必須為正數。", type = "error") #
          return(NULL) # 返回 NULL 表示參數無效 #
      }
      params$mean <- input$norm_mean #
      params$sd <- input$norm_sd #
    } else if (dist_name == "對數常態分佈 (Lognormal)") { #
      req(input$lnorm_sdlog) #
      if(input$lnorm_sdlog <= 0) { #
          showNotification("對數常態分佈的對數標準差必須為正數。", type = "error") #
          return(NULL) #
      }
      params$meanlog <- input$lnorm_meanlog #
      params$sdlog <- input$lnorm_sdlog #
    } else if (dist_name == "伽瑪分佈 (Gamma)") { #
      req(input$gamma_shape, input$gamma_rate) #
      if(input$gamma_shape <= 0 || input$gamma_rate <= 0) { #
          showNotification("伽瑪分佈的形狀 (Shape) 和速率 (Rate) 必須為正數。", type = "error") #
          return(NULL) #
      }
      params$shape <- input$gamma_shape #
      params$rate <- input$gamma_rate #
    } else if (dist_name == "t 分佈 (Student's t)") { #
      req(input$t_df, input$t_sigma) #
      if(input$t_df < 1 || input$t_sigma <= 0) { #
          showNotification("t 分佈的自由度必須 >= 1 且尺度必須為正數。", type = "error") #
          return(NULL) #
      }
      params$df <- input$t_df #
      params$mu <- input$t_mu   # 位置參數 #
      params$sigma <- input$t_sigma # 尺度參數 #
    } else if (dist_name == "指數分佈 (Exponential)") { #
      req(input$exp_rate) #
      if(input$exp_rate <= 0) { #
          showNotification("指數分佈的速率 (Rate) 必須為正數。", type = "error") #
          return(NULL) #
      }
      params$rate <- input$exp_rate #
    } else if (dist_name == "柯西分佈 (Cauchy)") { #
      req(input$cauchy_scale) #
      if(input$cauchy_scale <= 0) { #
          showNotification("柯西分佈的尺度 (Scale) 必須為正數。", type = "error") #
          return(NULL) #
      }
      params$location <- input$cauchy_location #
      params$scale <- input$cauchy_scale #
    } else if (dist_name == "貝他分佈 (Beta)") { #
      req(input$beta_shape1, input$beta_shape2) #
      if(input$beta_shape1 <= 0 || input$beta_shape2 <= 0) { #
          showNotification("貝他分佈的形狀參數 (α, β) 必須為正數。", type = "error") #
          return(NULL) #
      }
      params$shape1 <- input$beta_shape1 #
      params$shape2 <- input$beta_shape2 #
    } else if (dist_name == "二項分佈 (Binomial)") { #
      req(input$binom_size, input$binom_prob) #
      if(input$binom_size < 1 || input$binom_prob < 0 || input$binom_prob > 1) { #
           showNotification("二項分佈的試驗次數 (n) 必須 >= 1，成功機率 (p) 必須在 [0, 1] 之間。", type = "error") #
           return(NULL) #
      }
      params$size <- as.integer(input$binom_size) # 確保為整數 #
      params$prob <- input$binom_prob #
    } else if (dist_name == "卜瓦松分佈 (Poisson)") { #
      req(input$pois_lambda) #
      if(input$pois_lambda < 0) { #
          showNotification("卜瓦松分佈的平均發生率 (λ) 必須 >= 0。", type = "error") #
          return(NULL) #
      }
      params$lambda <- input$pois_lambda #
    }
    return(params) #
  })

  calculate_stats <- reactive({
    params <- get_params()
    req(params) # 確保參數有效 #
    dist_name <- input$dist #
    stats <- list(Mean = NA, Median = NA, Mode = NA, Pct_2.5 = NA, Pct_97.5 = NA) #

    # --- Stat calculation logic (same as original) ---
    tryCatch({ #
       if (dist_name == "常態分佈 (Gaussian)") { #
        stats$Mean <- params$mean #
        stats$Median <- qnorm(0.5, mean = params$mean, sd = params$sd) #
        stats$Pct_2.5 <- qnorm(0.025, mean = params$mean, sd = params$sd) #
        stats$Pct_97.5 <- qnorm(0.975, mean = params$mean, sd = params$sd) #
      } else if (dist_name == "對數常態分佈 (Lognormal)") { #
        stats$Mean <- exp(params$meanlog + 0.5 * params$sdlog^2) #
        stats$Median <- qlnorm(0.5, meanlog = params$meanlog, sdlog = params$sdlog) #
        stats$Pct_2.5 <- qlnorm(0.025, meanlog = params$meanlog, sdlog = params$sdlog) #
        stats$Pct_97.5 <- qlnorm(0.975, meanlog = params$meanlog, sdlog = params$sdlog) #
      } else if (dist_name == "伽瑪分佈 (Gamma)") { #
        stats$Mean <- params$shape / params$rate #
        stats$Median <- qgamma(0.5, shape = params$shape, rate = params$rate) #
        stats$Pct_2.5 <- qgamma(0.025, shape = params$shape, rate = params$rate) #
        stats$Pct_97.5 <- qgamma(0.975, shape = params$shape, rate = params$rate) #
      } else if (dist_name == "t 分佈 (Student's t)") { #
        if (params$df > 1) {
          stats$Mean <- params$mu #
        } else {
          stats$Mean <- "未定義 (df ≤ 1)" #
        }
        stats$Median <- params$mu #
        stats$Pct_2.5 <- params$mu + params$sigma * qt(0.025, df = params$df) #
        stats$Pct_97.5 <- params$mu + params$sigma * qt(0.975, df = params$df) #
      } else if (dist_name == "指數分佈 (Exponential)") { #
        stats$Mean <- 1 / params$rate #
        stats$Median <- qexp(0.5, rate = params$rate) # == log(2) / rate #
        stats$Pct_2.5 <- qexp(0.025, rate = params$rate) #
        stats$Pct_97.5 <- qexp(0.975, rate = params$rate) #
      } else if (dist_name == "柯西分佈 (Cauchy)") { #
        stats$Mean <- "未定義" # 柯西分佈的平均數不存在 #
        stats$Median <- qcauchy(0.5, location = params$location, scale = params$scale) # == params$location #
        stats$Pct_2.5 <- qcauchy(0.025, location = params$location, scale = params$scale) #
        stats$Pct_97.5 <- qcauchy(0.975, location = params$location, scale = params$scale) #
      } else if (dist_name == "貝他分佈 (Beta)") { #
        stats$Mean <- params$shape1 / (params$shape1 + params$shape2) #
        stats$Median <- qbeta(0.5, shape1 = params$shape1, shape2 = params$shape2) #
        stats$Pct_2.5 <- qbeta(0.025, shape1 = params$shape1, shape2 = params$shape2) #
        stats$Pct_97.5 <- qbeta(0.975, shape1 = params$shape1, shape2 = params$shape2) #
      } else if (dist_name == "二項分佈 (Binomial)") { #
        stats$Mean <- params$size * params$prob #
        stats$Median <- qbinom(0.5, size = params$size, prob = params$prob) # 注意：離散中位數可能不唯一 #
        stats$Pct_2.5 <- qbinom(0.025, size = params$size, prob = params$prob) #
        stats$Pct_97.5 <- qbinom(0.975, size = params$size, prob = params$prob) #
      } else if (dist_name == "卜瓦松分佈 (Poisson)") { #
        stats$Mean <- params$lambda #
        stats$Median <- qpois(0.5, lambda = params$lambda) # 注意：離散中位數可能不唯一 #
        stats$Pct_2.5 <- qpois(0.025, lambda = params$lambda) #
        stats$Pct_97.5 <- qpois(0.975, lambda = params$lambda) #
      }
      stats$Mode <- calculate_mode(dist_name, params) #
    }, error = function(e) { #
      showNotification(paste("計算統計量時出錯:", e$message), type = "error") #
      stats <- list(Mean = NA, Median = NA, Mode = NA, Pct_2.5 = NA, Pct_97.5 = NA) #
    })

    stats_formatted <- lapply(stats, function(x) { #
        if (is.numeric(x)) { #
            format(round(x, 4), nsmall = 4) #
        } else { #
            as.character(x) # 保持 "未定義" 或眾數的字串形式 #
        }
    })
    return(stats_formatted) #
  })

  output$summaryStats <- renderTable({ #
    stats <- calculate_stats() #
    req(stats) # 確保 stats 有效 #
    data.frame( #
      指標 = c("平均數 (Mean)", "中位數 (Median)", "眾數 (Mode)", "2.5% 百分位數", "97.5% 百分位數"), #
      數值 = c(stats$Mean, stats$Median, stats$Mode, stats$Pct_2.5, stats$Pct_97.5), #
      stringsAsFactors = FALSE #
    )
  }, align = 'lr') # 靠左對齊指標，靠右對齊數值 #

  output$distPlot <- renderPlot({ #
    # This renderPlot block remains identical to the previous version
    params <- get_params() #
    req(params) # 確保參數有效 #
    dist_name <- input$dist #
    stats <- calculate_stats() # 獲取計算好的統計量 #
    raw_stats_for_plot <- reactive({ #
        s <- list(Mean = NA, Median = NA, Pct_2.5 = NA, Pct_97.5 = NA) #
        tryCatch({ #
         if (dist_name == "常態分佈 (Gaussian)") { #
            s$Mean <- params$mean; #
            s$Median <- qnorm(0.5, params$mean, params$sd); s$Pct_2.5 <- qnorm(0.025, params$mean, params$sd); #
            s$Pct_97.5 <- qnorm(0.975, params$mean, params$sd) #
          } else if (dist_name == "對數常態分佈 (Lognormal)") { #
             s$Mean <- exp(params$meanlog + 0.5 * params$sdlog^2); #
             s$Median <- qlnorm(0.5, params$meanlog, params$sdlog); s$Pct_2.5 <- qlnorm(0.025, params$meanlog, params$sdlog); #
             s$Pct_97.5 <- qlnorm(0.975, params$meanlog, params$sdlog) #
          } else if (dist_name == "伽瑪分佈 (Gamma)") { #
            s$Mean <- params$shape / params$rate; #
            s$Median <- qgamma(0.5, params$shape, params$rate); s$Pct_2.5 <- qgamma(0.025, params$shape, params$rate); #
            s$Pct_97.5 <- qgamma(0.975, params$shape, params$rate) #
          } else if (dist_name == "t 分佈 (Student's t)") { #
             if (params$df > 1) s$Mean <- params$mu; #
             s$Median <- params$mu; s$Pct_2.5 <- params$mu + params$sigma * qt(0.025, params$df); #
             s$Pct_97.5 <- params$mu + params$sigma * qt(0.975, params$df) #
          } else if (dist_name == "指數分佈 (Exponential)") { #
            s$Mean <- 1 / params$rate; #
            s$Median <- qexp(0.5, params$rate); s$Pct_2.5 <- qexp(0.025, params$rate); s$Pct_97.5 <- qexp(0.975, params$rate) #
          } else if (dist_name == "柯西分佈 (Cauchy)") { #
             s$Median <- qcauchy(0.5, params$location, params$scale); #
             s$Pct_2.5 <- qcauchy(0.025, params$location, params$scale); s$Pct_97.5 <- qcauchy(0.975, params$location, params$scale) #
          } else if (dist_name == "貝他分佈 (Beta)") { #
             s$Mean <- params$shape1 / (params$shape1 + params$shape2); #
             s$Median <- qbeta(0.5, params$shape1, params$shape2); s$Pct_2.5 <- qbeta(0.025, params$shape1, params$shape2); #
             s$Pct_97.5 <- qbeta(0.975, params$shape1, params$shape2) #
          } else if (dist_name == "二項分佈 (Binomial)") { #
             s$Mean <- params$size * params$prob; #
             s$Median <- qbinom(0.5, params$size, params$prob); s$Pct_2.5 <- qbinom(0.025, params$size, params$prob); #
             s$Pct_97.5 <- qbinom(0.975, params$size, params$prob) #
          } else if (dist_name == "卜瓦松分佈 (Poisson)") { #
             s$Mean <- params$lambda; #
             s$Median <- qpois(0.5, params$lambda); s$Pct_2.5 <- qpois(0.025, params$lambda); s$Pct_97.5 <- qpois(0.975, params$lambda) #
          }
        }, error = function(e) {}) # 忽略計算錯誤 #
        return(s) #
    })() # 立即執行 #

    plot_title <- paste(dist_name, "機率密度/質量函數") #
    p <- NULL # 初始化 ggplot 物件 #

    # --- Plotting logic (same as original, uses raw_stats_for_plot) ---
    # --- Continuous Distributions ---
    if (dist_name %in% c("常態分佈 (Gaussian)", "對數常態分佈 (Lognormal)", "伽瑪分佈 (Gamma)", "t 分佈 (Student's t)", "指數分佈 (Exponential)", "柯西分佈 (Cauchy)", "貝他分佈 (Beta)")) { #
      plot_range <- tryCatch({ #
          lower <- NA; upper <- NA #
          if (!is.na(raw_stats_for_plot$Pct_2.5) && !is.na(raw_stats_for_plot$Pct_97.5)) { #
              range_val <- raw_stats_for_plot$Pct_97.5 - raw_stats_for_plot$Pct_2.5 #
              extend_factor <- ifelse(dist_name == "柯西分佈 (Cauchy)", 1.5, 3) #
              lower <- raw_stats_for_plot$Pct_2.5 - extend_factor * range_val / 2 #
              upper <- raw_stats_for_plot$Pct_97.5 + extend_factor * range_val / 2 #
              if (dist_name == "貝他分佈 (Beta)") { #
                  lower <- max(0, lower); #
                  upper <- min(1, upper) #
              }
              if (dist_name %in% c("指數分佈 (Exponential)", "伽瑪分佈 (Gamma)")) { #
                  lower <- max(0, lower) #
              }
              if (is.na(lower) || is.na(upper) || lower >= upper) { #
                   if (dist_name == "貝他分佈 (Beta)") { c(0, 1) } #
                  else if (dist_name %in% c("指數分佈 (Exponential)", "伽瑪分佈 (Gamma)")) { c(0, qgamma(0.999, params$shape, params$rate) * 1.1) } #
                  else if (dist_name == "對數常態分佈 (Lognormal)") { c(0, qlnorm(0.999, params$meanlog, params$sdlog) * 1.1) } #
                  else { c(qnorm(0.001, params$mean, params$sd), qnorm(0.999, params$mean, params$sd)) } #
              } else { #
                  c(lower, upper) #
              }
          } else { # 如果百分位數計算失敗 #
              if (dist_name == "貝他分佈 (Beta)") { c(0, 1) } #
              else if (dist_name == "柯西分佈 (Cauchy)") { c(params$location - 5*params$scale, params$location + 5*params$scale)} # 柯西的範圍需要小心 #
              else if (dist_name %in% c("指數分佈 (Exponential)", "伽瑪分佈 (Gamma)")) { c(0, 10) } # 預設 #
              else { c(-5, 5) } # 預設 #
          }
      }, error = function(e){ if (dist_name == "貝他分佈 (Beta)") c(0,1) else c(-5,5)}) # 出錯時的最終預設範圍 #

      x_vals <- seq(plot_range[1], plot_range[2], length.out = 400) #
      y_vals <- NULL #
      
      if (dist_name == "常態分佈 (Gaussian)") { #
        y_vals <- dnorm(x_vals, mean = params$mean, sd = params$sd) #
      } else if (dist_name == "對數常態分佈 (Lognormal)") { #
        x_vals <- x_vals[x_vals > 1e-9] # 避免 log(0) 或負數 #
        y_vals <- dlnorm(x_vals, meanlog = params$meanlog, sdlog = params$sdlog) #
        if (length(x_vals) == 0) x_vals <- seq(1e-6, plot_range[2], length.out = 400) # 如果範圍全負，重新生成 #
        if (is.null(y_vals) || length(y_vals) != length(x_vals)) y_vals <- dlnorm(x_vals, meanlog = params$meanlog, sdlog = params$sdlog) #
      } else if (dist_name == "伽瑪分佈 (Gamma)") { #
         x_vals <- x_vals[x_vals >= 0] #
         if (length(x_vals) == 0) x_vals <- seq(1e-6, plot_range[2], length.out = 400) #
         y_vals <- dgamma(x_vals, shape = params$shape, rate = params$rate) #
      } else if (dist_name == "t 分佈 (Student's t)") { #
         y_vals <- (1 / params$sigma) * dt((x_vals - params$mu) / params$sigma, df = params$df) #
      } else if (dist_name == "指數分佈 (Exponential)") { #
         x_vals <- x_vals[x_vals >= 0] #
         if (length(x_vals) == 0) x_vals <- seq(1e-6, plot_range[2], length.out = 400) #
         y_vals <- dexp(x_vals, rate = params$rate) #
      } else if (dist_name == "柯西分佈 (Cauchy)") { #
        y_vals <- dcauchy(x_vals, location = params$location, scale = params$scale) #
        y_limit <- quantile(y_vals, 0.99, na.rm = TRUE) * 1.5 # 限制在 99% 分位數的 1.5 倍 #
         if(is.na(y_limit) || y_limit == 0) y_limit <- max(y_vals, na.rm = TRUE) * 0.8 # 備用方案 #
         y_vals[y_vals > y_limit] <- NA # 超出範圍的不繪製，避免 y 軸被拉伸 #
      } else if (dist_name == "貝他分佈 (Beta)") { #
         x_vals <- seq(max(0, plot_range[1]), min(1, plot_range[2]), length.out = 400) #
         eps <- 1e-6 #
         x_vals[x_vals < eps] <- eps #
         x_vals[x_vals > 1 - eps] <- 1 - eps #
         y_vals <- dbeta(x_vals, shape1 = params$shape1, shape2 = params$shape2) #
      }

      df_plot <- data.frame(x = x_vals, y = y_vals) %>% filter(!is.na(y) & is.finite(y)) # 過濾無效值 #
      if (nrow(df_plot) > 0) { #
         p <- ggplot(df_plot, aes(x = x, y = y)) + #
             geom_line(color = "blue", linewidth = 1) + #
             labs(title = plot_title, x = "數值", y = "機率密度") + #
             theme_minimal() #

         if (!is.na(raw_stats_for_plot$Mean) && is.numeric(raw_stats_for_plot$Mean)) p <- p + geom_vline(xintercept = raw_stats_for_plot$Mean, linetype = "dashed", color = "red") #
         if (!is.na(raw_stats_for_plot$Median) && is.numeric(raw_stats_for_plot$Median)) p <- p + geom_vline(xintercept = raw_stats_for_plot$Median, linetype = "dotted", color = "green4") #
         if (!is.na(raw_stats_for_plot$Pct_2.5) && is.numeric(raw_stats_for_plot$Pct_2.5)) p <- p + geom_vline(xintercept = raw_stats_for_plot$Pct_2.5, linetype = "solid", color = "orange") #
         if (!is.na(raw_stats_for_plot$Pct_97.5) && is.numeric(raw_stats_for_plot$Pct_97.5)) p <- p + geom_vline(xintercept = raw_stats_for_plot$Pct_97.5, linetype = "solid", color = "orange") #

         line_labels <- c() #
         line_colors <- c() #
         line_types <- c() #
         if (!is.na(raw_stats_for_plot$Mean) && is.numeric(raw_stats_for_plot$Mean)) { line_labels <- c(line_labels, "平均數"); line_colors <- c(line_colors, "red"); line_types <- c(line_types, "dashed") } #
         if (!is.na(raw_stats_for_plot$Median) && is.numeric(raw_stats_for_plot$Median)) { line_labels <- c(line_labels, "中位數"); line_colors <- c(line_colors, "green4"); line_types <- c(line_types, "dotted") } #
         if (!is.na(raw_stats_for_plot$Pct_2.5) && is.numeric(raw_stats_for_plot$Pct_2.5)) { line_labels <- c(line_labels, "2.5/97.5 百分位"); line_colors <- c(line_colors, "orange"); line_types <- c(line_types, "solid") } #
         
         if (length(line_labels) > 0) { #
            p <- p + scale_linetype_manual(name = "指標", values = line_types, labels = line_labels) + #
                     scale_color_manual(name = "指標", values = line_colors, labels = line_labels) + #
                     guides(linetype = guide_legend(), color = guide_legend()) # 確保圖例顯示 #
            p <- p + guides( #
                color = guide_legend(override.aes = list(linetype = line_types)), #
                linetype = guide_legend(override.aes = list(color = line_colors)) #
             )
         }
      } else { #
         p <- ggplot() + labs(title = plot_title, x="", y="") + annotate("text", x=0, y=0, label="無法繪製圖形（參數或範圍無效）") #
      }
    # --- Discrete Distributions ---
    } else if (dist_name %in% c("二項分佈 (Binomial)", "卜瓦松分佈 (Poisson)")) { #
      x_vals <- NULL #
      y_vals <- NULL #
      if (dist_name == "二項分佈 (Binomial)") { #
          x_vals <- 0:params$size #
          y_vals <- dbinom(x_vals, size = params$size, prob = params$prob) #
          plot_title <- paste(dist_name, "- n =", params$size, ", p =", round(params$prob, 3)) #
      } else if (dist_name == "卜瓦松分佈 (Poisson)") { #
          upper_limit <- qpois(0.9999, lambda = params$lambda) #
          upper_limit <- max(upper_limit, ceiling(params$lambda + 3 * sqrt(params$lambda)), 5) #
          x_vals <- 0:upper_limit #
          y_vals <- dpois(x_vals, lambda = params$lambda) #
          plot_title <- paste(dist_name, "- λ =", params$lambda) #
      }

      df_plot <- data.frame(x = x_vals, y = y_vals) #
      p <- ggplot(df_plot, aes(x = factor(x), y = y)) + # 將 x 轉為因子以正確繪製長條圖 #
           geom_col(fill = "skyblue", color = "black") + # 使用 geom_col 繪製長條圖 #
           labs(title = plot_title, x = "事件數量 (k)", y = "機率 P(X=k)") + #
           theme_minimal() + #
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 旋轉 x 軸標籤避免重疊 #

       if (!is.na(raw_stats_for_plot$Mean) && is.numeric(raw_stats_for_plot$Mean)) p <- p + geom_vline(xintercept = as.character(round(raw_stats_for_plot$Mean)), linetype = "dashed", color = "red", linewidth=1) #
       if (!is.na(raw_stats_for_plot$Median) && is.numeric(raw_stats_for_plot$Median)) p <- p + geom_vline(xintercept = as.character(raw_stats_for_plot$Median), linetype = "dotted", color = "green4", linewidth=1) #
       if (!is.na(raw_stats_for_plot$Pct_2.5) && is.numeric(raw_stats_for_plot$Pct_2.5)) p <- p + geom_vline(xintercept = as.character(raw_stats_for_plot$Pct_2.5), linetype = "solid", color = "orange", linewidth=1) #
       if (!is.na(raw_stats_for_plot$Pct_97.5) && is.numeric(raw_stats_for_plot$Pct_97.5)) p <- p + geom_vline(xintercept = as.character(raw_stats_for_plot$Pct_97.5), linetype = "solid", color = "orange", linewidth=1) #

        line_labels <- c() #
        line_colors <- c() #
        line_types <- c() #
        if (!is.na(raw_stats_for_plot$Mean) && is.numeric(raw_stats_for_plot$Mean)) { line_labels <- c(line_labels, "平均數"); line_colors <- c(line_colors, "red"); line_types <- c(line_types, "dashed") } #
        if (!is.na(raw_stats_for_plot$Median) && is.numeric(raw_stats_for_plot$Median)) { line_labels <- c(line_labels, "中位數"); line_colors <- c(line_colors, "green4"); line_types <- c(line_types, "dotted") } #
        if (!is.na(raw_stats_for_plot$Pct_2.5) && is.numeric(raw_stats_for_plot$Pct_2.5)) { line_labels <- c(line_labels, "2.5/97.5 百分位"); line_colors <- c(line_colors, "orange"); line_types <- c(line_types, "solid") } #
         
        if (length(line_labels) > 0) { #
            dummy_df <- data.frame(x = rep(as.character(x_vals[1]), length(line_labels)), #
                                  y = rep(max(y_vals, na.rm=TRUE)*1.1, length(line_labels)), # 放在圖上方 #
                                  Indicator = factor(line_labels, levels=line_labels)) #
            p <- p + geom_line(data = dummy_df, aes(x = x, y = y, color = Indicator, linetype = Indicator), inherit.aes = FALSE) + #
                  scale_linetype_manual(name = "指標", values = line_types, labels = line_labels) + #
                  scale_color_manual(name = "指標", values = line_colors, labels = line_labels) + #
                  guides(linetype = guide_legend(), color = guide_legend()) #
        }
    }
    print(p) # 顯示圖形 #
  })

  output$explanation <- renderUI({ #
    # This renderUI block remains identical to the previous version
    dist_name <- input$dist #
    text <- switch(dist_name, #
      "常態分佈 (Gaussian)" = HTML("常態分佈（又稱高斯分佈）是最重要的連續機率分佈之一。圖形呈鐘形，對稱於平均數。由平均數 (μ) 和標準差 (σ) 決定。μ 決定中心位置，σ 決定分散程度（圖形高矮胖瘦）。許多自然現象和測量誤差的抽樣分布大致遵循常態分佈（根據中央極限定理），其誤差是相加的。"), #
      "對數常態分佈 (Lognormal)" = HTML("如果一個隨機變數 X 的自然對數 ln(X) 服從常態分佈，則 X 服從對數常態分佈，其誤差是相乘的。此分佈是右偏的（尾部拖向右側），且值域為正數 (X > 0)。由對數空間的平均數 (μ_log) 和標準差 (σ_log) 決定。常用於描述其值不能為負且分佈不對稱的變數。"), #
      "伽瑪分佈 (Gamma)" = HTML("伽瑪分佈是包含指數分佈和卡方分佈的連續機率分佈族。值域為正數 (X ≥ 0)，通常是右偏的，類似對數常態分佈。由形狀參數 (α 或 k) 和速率參數 (β 或 λ，有時用尺度參數 θ = 1/β) 定義。常用於模擬等待時間、壽命或總量。當形狀參數 α=1 時，伽瑪分佈即為指數分佈。"), #
      "t 分佈 (Student's t)" = HTML("t 分佈（或稱學生 t 分佈）形狀類似常態分佈，呈鐘形且對稱，但具有更『重』的尾部（即尾部機率比常態分佈大）。由自由度 (df) 參數決定。當自由度增加時，t 分佈趨近於標準常態分佈。主要用於樣本量較小 (<30) 且總體標準差未知時，對總體平均數進行估計和假設檢定。位置(μ)和尺度(σ)參數允許產生非標準化的t分佈。"), #
      "指數分佈 (Exponential)" = HTML("指數分佈是描述獨立隨機事件發生之間的時間（或距離）的連續機率分佈。值域為正數 (X ≥ 0)，具有『無記憶性』，即未來事件發生的機率與過去等待了多久無關。由單一的速率參數 (λ) 決定，平均數為 1/λ。圖形從最高點急速下降，是右偏的。"), #
      "柯西分佈 (Cauchy)" = HTML("柯西分佈（也稱洛倫茲分佈）是一種對稱的鐘形連續分佈，但其尾部比常態分佈甚至 t 分佈都要『重』得多，以至於其平均數、變異數等高階動差均未定義（期望值不存在）。由位置參數 (x0，決定峰值位置，也是中位數和眾數) 和尺度參數 (γ，決定半峰全寬的一半) 決定。雖然在生物統計中直接應用較少，但它在物理學和穩健統計學、貝氏學派的弱先驗 cauchy(0, 0.707)中很重要。"), #
      "貝他分佈 (Beta)" = HTML("貝他分佈是定義在區間 [0, 1] 上的連續機率分佈族，由兩個正的形狀參數 α 和 β 控制。它可以呈現多種形狀（對稱、偏斜、U形、J形等），非常適合模擬百分比、比例、有上下限的數據或機率本身的分佈。"), #
      "二項分佈 (Binomial)" = HTML("二項分佈是描述在一系列固定的、獨立的『成功/失敗』試驗（稱為白努利試驗, 分佈是 Bernoulli(p)）中，成功次數的離散機率分佈。由試驗總次數 (n) 和單次試驗的成功機率 (p) 決定 Bernoulli(n, p)。其值域為 0, 1, 2, ..., n。"), #
      "卜瓦松分佈 (Poisson)" = HTML("卜瓦松分佈是描述在一個固定的時間間隔、空間區域或體積內，某事件發生的次數的離散機率分佈。假設事件是獨立發生的，且平均發生率是恆定的。由單一參數 λ (lambda) 決定，λ 既是平均發生次數，也是分佈的變異數。其值域為 0, 1, 2, ...。") #
    )
    return(text) #
  })

  output$medical_example <- renderUI({ #
    # This renderUI block remains identical to the previous version
    dist_name <- input$dist #
    text <- switch(dist_name, #
      "常態分佈 (Gaussian)" = HTML("<ul><li>成年男性的身高分佈。</li><li>重複測量同一樣本的血糖值時的測量誤差。</li><li>人群中收縮壓（舒張壓）的分佈（可能需要對數轉換或考慮年齡等因素）。</li></ul>"), #
      "對數常態分佈 (Lognormal)" = HTML("<ul><li>疾病的潛伏期（從感染到出現症狀的時間）。</li><li>空氣中或血液中污染物/藥物濃度的分佈。</li><li>某些生理指標的測量值，如比例、抗體滴度、收入、費用、住院天數。</li><li>傷口癒合所需時間。</li></ul>"), #
      "伽瑪分佈 (Gamma)" = HTML("<ul><li>在特定時間段內，病人需要進行某種檢查（如X光）的總等待時間（如果每次檢查時間服從指數分佈）。</li><li>癌症患者接受治療後の存活時間。</li><li>模擬某區域在一定時間內的降雨量總和。</li><li>細胞分裂所需的時間。</li></ul>"), #
      "t 分佈 (Student's t)" = HTML("<ul><li>當只有少量病人數據（例如 n < 30）時，估計新療法的平均療效改善程度的信賴區間。</li><li>比較兩組小樣本病人（例如，實驗組 vs. 對照組）平均血壓的差異是否顯著。</li><li>在未知總體變異數的情況下，檢定樣本平均值是否等於某個假設的總體平均值。</li></ul>"), #
      "指數分佈 (Exponential)" = HTML("<ul><li>醫院急診室兩次病人到達之間的間隔時間。</li><li>放射性同位素衰變所需的時間。</li><li>某個醫療設備（如起搏器）無故障運行的持續時間（假設故障率恆定）。</li><li>等待第一個病人對某種罕見病治療產生反應的時間。</li></ul>"), #
      "柯西分佈 (Cauchy)" = HTML("<ul><li>在生物醫學中直接應用較少，因為其缺乏平均值和變異數。</li><li>可能出現在某些物理模型（如共振現象）或作為對含有極端異常值數據進行穩健統計推斷時的一種模型假設（因其重尾特性對異常值不敏感）。</li><li>理論上，兩個獨立標準常態分佈隨機變數的比值服從標準柯西分佈。</li></ul>"), #
      "貝他分佈 (Beta)" = HTML("<ul><li>某種治療方法的成功率（治癒比例）本身的不確定性分佈（常用於貝氏統計）。</li><li>基因在人群中的等位基因頻率分佈。</li><li>病人對疼痛程度的主觀評分（標準化到 0 到 1 之間）的分佈。</li><li>評估診斷測試的敏感性或特異性的分佈。</li></ul>"), #
      "二項分佈 (Binomial)" = HTML("<ul><li>在 20 位接受新藥治療的患者中，出現副作用的人數。</li><li>隨機抽取 100 人，其中攜帶某特定基因變異的人數。</li><li>進行 5 次獨立的診斷測試，結果為陽性的次數（假設每次測試準確性相同且獨立）。</li><li>一個家庭有 4 個孩子，其中男孩的數量（假設生男生女機率相等且獨立）。</li></ul>"), #
      "卜瓦松分佈 (Poisson)" = HTML("<ul><li>某醫院急診室在一小時內接收到的病患數量。</li><li>在一定長度的 DNA 序列中發現的突變數量。</li><li>每平方公分皮膚上細菌菌落的數量。</li><li>某地區在一年內發生某種罕見疾病的病例數。</li></ul>") #
    )
    return(text) #
  })

  # --- NEW Server Logic for Central Limit Theorem Tab ---

  # Reactive expression to store CLT simulation results
  clt_results <- eventReactive(input$clt_run, {
    # This eventReactive block remains identical to the previous version
    dist_name <- input$clt_dist
    n <- input$clt_n
    reps <- input$clt_reps
    params <- list()
    sample_means <- numeric(reps)
    pop_mean <- NA
    pop_sd <- NA
    pop_var <- NA # Variance needed for CLT calculation

    # Validate and get parameters for the selected CLT distribution
    valid_params <- TRUE
    if (dist_name == "Uniform") {
      req(input$clt_unif_min, input$clt_unif_max)
      if (input$clt_unif_min >= input$clt_unif_max) {
        showNotification("Uniform: 最小值必須小於最大值。", type = "error")
        valid_params <- FALSE
      } else {
        params$min <- input$clt_unif_min
        params$max <- input$clt_unif_max
        pop_mean <- (params$min + params$max) / 2
        pop_var <- (params$max - params$min)^2 / 12
        pop_sd <- sqrt(pop_var)
      }
    } else if (dist_name == "Lognormal") {
       req(input$clt_lnorm_sdlog) # Referenced validation
       if(input$clt_lnorm_sdlog <= 0) { # Referenced validation
          showNotification("Lognormal: 對數標準差必須為正數。", type = "error") # Referenced message
          valid_params <- FALSE
       } else {
          params$meanlog <- input$clt_lnorm_meanlog
          params$sdlog <- input$clt_lnorm_sdlog
          pop_mean <- exp(params$meanlog + 0.5 * params$sdlog^2) # Formula
          pop_var <- (exp(params$sdlog^2) - 1) * exp(2 * params$meanlog + params$sdlog^2)
          pop_sd <- sqrt(pop_var)
       }
    } else if (dist_name == "Gamma") {
       req(input$clt_gamma_shape, input$clt_gamma_rate) # Referenced validation
       if(input$clt_gamma_shape <= 0 || input$clt_gamma_rate <= 0) { # Referenced validation
          showNotification("Gamma: 形狀 (Shape) 和速率 (Rate) 必須為正數。", type = "error") # Referenced message
          valid_params <- FALSE
       } else {
          params$shape <- input$clt_gamma_shape
          params$rate <- input$clt_gamma_rate
          pop_mean <- params$shape / params$rate # Formula
          pop_var <- params$shape / (params$rate^2)
          pop_sd <- sqrt(pop_var)
       }
    } else if (dist_name == "Beta") {
       req(input$clt_beta_shape1, input$clt_beta_shape2) # Referenced validation
       if(input$clt_beta_shape1 <= 0 || input$clt_beta_shape2 <= 0) { # Referenced validation
           showNotification("Beta: 形狀參數 (α, β) 必須為正數。", type = "error") # Referenced message
           valid_params <- FALSE
       } else {
          params$shape1 <- input$clt_beta_shape1
          params$shape2 <- input$clt_beta_shape2
          pop_mean <- params$shape1 / (params$shape1 + params$shape2) # Formula
          pop_var <- (params$shape1 * params$shape2) / ((params$shape1 + params$shape2)^2 * (params$shape1 + params$shape2 + 1))
          pop_sd <- sqrt(pop_var)
       }
    }

    # Proceed only if parameters are valid
    if (!valid_params) return(NULL)

    # Run simulation
    # Use a progress indicator
     withProgress(message = '執行模擬中...', value = 0, {
       for (i in 1:reps) {
         sample_data <- switch(dist_name,
                              "Uniform"   = runif(n, min = params$min, max = params$max),
                              "Lognormal" = rlnorm(n, meanlog = params$meanlog, sdlog = params$sdlog), # Base R function
                              "Gamma"     = rgamma(n, shape = params$shape, rate = params$rate), # Base R function
                              "Beta"      = rbeta(n, shape1 = params$shape1, shape2 = params$shape2)) # Base R function
         sample_means[i] <- mean(sample_data)
         incProgress(1/reps, detail = paste("樣本", i)) # Update progress
       }
     })

    # Return results
    return(list(
      dist_name = dist_name,
      params = params,
      n = n,
      reps = reps,
      sample_means = sample_means,
      pop_mean = pop_mean,
      pop_sd = pop_sd,
      pop_var = pop_var # Include variance
    ))
  })

  # Calculate theoretical stats for the chosen population distribution
  calculate_clt_pop_stats <- reactive({
      # This reactive block remains identical to the previous version
      dist_name <- input$clt_dist
      params <- list()
      stats <- list(Mean = NA, Median = NA, Mode = NA, SD = NA, Variance = NA)

      # Get parameters (similar logic to get_params but for clt inputs)
      valid_params <- TRUE
      if (dist_name == "Uniform") {
          req(input$clt_unif_min, input$clt_unif_max)
          if (input$clt_unif_min >= input$clt_unif_max) valid_params <- FALSE
          else {
              params$min <- input$clt_unif_min
              params$max <- input$clt_unif_max
              stats$Mean <- (params$min + params$max) / 2
              stats$Median <- stats$Mean # Median is the same as mean for uniform
              stats$Variance <- (params$max - params$min)^2 / 12
              stats$SD <- sqrt(stats$Variance)
              # Mode calculation is handled in calculate_mode
          }
      } else if (dist_name == "Lognormal") {
          req(input$clt_lnorm_sdlog)
          if(input$clt_lnorm_sdlog <= 0) valid_params <- FALSE
          else {
              params$meanlog <- input$clt_lnorm_meanlog
              params$sdlog <- input$clt_lnorm_sdlog
              stats$Mean <- exp(params$meanlog + 0.5 * params$sdlog^2) # Formula
              stats$Median <- qlnorm(0.5, meanlog = params$meanlog, sdlog = params$sdlog) # Formula
              stats$Variance <- (exp(params$sdlog^2) - 1) * exp(2 * params$meanlog + params$sdlog^2)
              stats$SD <- sqrt(stats$Variance)
              # Mode calculation handled below
          }
      } else if (dist_name == "Gamma") {
          req(input$clt_gamma_shape, input$clt_gamma_rate)
          if(input$clt_gamma_shape <= 0 || input$clt_gamma_rate <= 0) valid_params <- FALSE
          else {
              params$shape <- input$clt_gamma_shape
              params$rate <- input$clt_gamma_rate
              stats$Mean <- params$shape / params$rate # Formula
              stats$Median <- qgamma(0.5, shape = params$shape, rate = params$rate) # Formula
              stats$Variance <- params$shape / (params$rate^2)
              stats$SD <- sqrt(stats$Variance)
              # Mode calculation handled below
          }
      } else if (dist_name == "Beta") {
          req(input$clt_beta_shape1, input$clt_beta_shape2)
          if(input$clt_beta_shape1 <= 0 || input$clt_beta_shape2 <= 0) valid_params <- FALSE
          else {
              params$shape1 <- input$clt_beta_shape1
              params$shape2 <- input$clt_beta_shape2
              stats$Mean <- params$shape1 / (params$shape1 + params$shape2) # Formula
              stats$Median <- qbeta(0.5, shape1 = params$shape1, shape2 = params$shape2) # Formula
              stats$Variance <- (params$shape1 * params$shape2) / ((params$shape1 + params$shape2)^2 * (params$shape1 + params$shape2 + 1))
              stats$SD <- sqrt(stats$Variance)
              # Mode calculation handled below
          }
      }

      if (!valid_params) return(NULL)

      # Add Mode using the helper function
      stats$Mode <- calculate_mode(dist_name, params)

      # Format numeric output
      stats_formatted <- lapply(stats, function(x) {
          if (is.numeric(x)) {
              format(round(x, 4), nsmall = 4)
          } else {
              as.character(x)
          }
      })
      return(stats_formatted)
  })

  # Display population stats table
  output$cltSummaryStats <- renderTable({
      # This renderTable block remains identical to the previous version
      stats <- calculate_clt_pop_stats()
      req(stats)
      # Add Variance explicitly if it wasn't in the original list
      data.frame(
          指標 = c("平均數 (Mean)", "中位數 (Median)", "眾數 (Mode)", "標準差 (SD)", "變異數 (Variance)"),
          數值 = c(stats$Mean, stats$Median, stats$Mode, stats$SD, stats$Variance),
          stringsAsFactors = FALSE
      )
  }, align = 'lr')


  # Render the CLT plot
  output$cltPlot <- renderPlot({
    # **** ADDED tryCatch for overall plot rendering ****
    tryCatch({
        res <- clt_results()
        # req(res) will silently stop if res is NULL (e.g., before button press or if params invalid)
        # It's essential the user clicks the button!
        req(res)

        sample_means_df <- data.frame(mean = res$sample_means)

        # Calculate theoretical normal parameters for sample means
        clt_mean <- res$pop_mean
        clt_sd <- if (!is.na(res$pop_sd) && res$n > 0) res$pop_sd / sqrt(res$n) else NA
        clt_var <- if (!is.na(res$pop_var) && res$n > 0) res$pop_var / res$n else NA

        plot_title <- paste("樣本平均數的分佈 (原始分佈:", res$dist_name, ", n =", res$n, ", reps =", res$reps, ")")
        plot_subtitle <- ""
        if(!is.na(clt_mean) && !is.na(clt_sd)){
           plot_subtitle <- paste("理論常態分佈 (CLT): 平均數 ≈", round(clt_mean, 4), ", 標準差 ≈", round(clt_sd, 4))
        } else {
           plot_subtitle <- "無法計算理論常態分佈 (總體變異數可能不存在或 n=0)"
        }


        p_clt <- ggplot(sample_means_df, aes(x = mean)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
          labs(title = plot_title,
               subtitle = plot_subtitle,
               x = "樣本平均數",
               y = "密度") +
          theme_minimal()

        # Overlay theoretical normal curve if possible
        if (!is.na(clt_mean) && !is.na(clt_sd) && clt_sd > 0) {
          p_clt <- p_clt + stat_function(fun = dnorm, args = list(mean = clt_mean, sd = clt_sd), color = "red", linewidth = 1.2)
        }

        # Add vertical line for population mean
        if (!is.na(res$pop_mean)) {
          p_clt <- p_clt + geom_vline(xintercept = res$pop_mean, color = "blue", linetype = "dashed", linewidth = 1)
        }

         # --- Optional: Plot the original population distribution ---
         pop_dist_plot <- tryCatch({
             # This inner tryCatch block for population plot remains identical
             x_pop <- NULL
             y_pop <- NULL
             pop_plot_title <- paste("原始總體分佈:", res$dist_name)
             
             pop_range <- if (!is.na(res$pop_mean) && !is.na(res$pop_sd) && res$pop_sd > 0) {
                 c(res$pop_mean - 4 * res$pop_sd, res$pop_mean + 4 * res$pop_sd)
             } else if (res$dist_name == "Beta") {
                 c(0, 1)
             } else if (res$dist_name == "Uniform") {
                 c(res$params$min - 0.1*(res$params$max - res$params$min), res$params$max + 0.1*(res$params$max - res$params$min))
             } else {
                 quantile_func <- switch(res$dist_name,
                                        "Lognormal" = function(p) qlnorm(p, res$params$meanlog, res$params$sdlog),
                                        "Gamma" = function(p) qgamma(p, res$params$shape, res$params$rate),
                                        function(p) NA)
                  if(!is.function(quantile_func) || any(is.na(quantile_func(c(0.001, 0.999))))) {
                      c(0, 10)
                  } else {
                     quantile_func(c(0.001, 0.999))
                  }
             }
             
             if (res$dist_name == "Beta") { pop_range <- c(max(0, pop_range[1]), min(1, pop_range[2])) }
             if (res$dist_name %in% c("Gamma", "Lognormal")) { pop_range[1] <- max(0, pop_range[1]) }
              if(pop_range[1] >= pop_range[2]) {
                  pop_range <- if (res$dist_name == "Beta") c(0, 1) else if(res$dist_name == "Uniform") c(res$params$min, res$params$max) else c(0, 10)
              }

             x_pop <- seq(pop_range[1], pop_range[2], length.out = 400)
             
             y_pop <- switch(res$dist_name,
                            "Uniform"   = dunif(x_pop, min = res$params$min, max = res$params$max),
                            "Lognormal" = dlnorm(x_pop, meanlog = res$params$meanlog, sdlog = res$params$sdlog),
                            "Gamma"     = dgamma(x_pop, shape = res$params$shape, rate = res$params$rate),
                            "Beta"      = dbeta(x_pop, shape1 = res$params$shape1, shape2 = res$params$shape2))

             valid_indices <- is.finite(x_pop) & is.finite(y_pop)
             x_pop <- x_pop[valid_indices]
             y_pop <- y_pop[valid_indices]
             
             if(length(x_pop) > 1) {
                 df_pop <- data.frame(x = x_pop, y = y_pop)
                 ggplot(df_pop, aes(x=x, y=y)) + geom_line(color="darkgreen", linewidth=1) +
                   labs(title=pop_plot_title, x="數值", y="密度") + theme_minimal() +
                   geom_vline(xintercept = res$pop_mean, color="blue", linetype="dashed")
             } else { NULL }
         }, error = function(e) {
             warning("無法生成總體圖: ", e$message)
             NULL
         })

         # Combine plots if population plot exists, otherwise just show CLT plot
         # Ensure patchwork library is loaded at the top of the script
         if(!is.null(pop_dist_plot)) {
             # Use patchwork::plot_layout or just the division operator if loaded
              print(pop_dist_plot / p_clt + plot_layout(ncol = 1)) # Arrange vertically
         } else {
            print(p_clt)
         }

    }, error = function(e) {
        # **** ADDED: Display a message if the plot fails to render ****
        plot.new() # Create a blank plot area
        title(main="無法渲染圖形", sub=paste("錯誤:", e$message), col.main="red", col.sub="red")
        # Or use ggplot to display error
        # ggplot() + labs(title="無法渲染圖形", subtitle=paste("錯誤:", e$message)) + theme_void()
    }) # End of outer tryCatch for renderPlot
  })

  # Display simulation results summary
  output$cltSimResults <- renderPrint({
    # This renderPrint block remains identical to the previous version
    res <- clt_results()
    req(res)

    sim_mean <- mean(res$sample_means)
    sim_sd <- sd(res$sample_means)
    clt_mean <- res$pop_mean
    clt_sd <- if (!is.na(res$pop_sd) && res$n > 0) res$pop_sd / sqrt(res$n) else NA

    cat("模擬結果摘要:\n")
    cat("----------------------------\n")
    cat("觀察到的樣本平均數之平均值:", round(sim_mean, 6), "\n")
    cat("觀察到的樣本平均數之標準差:", round(sim_sd, 6), "\n\n")
    cat("根據中央極限定理的理論值:\n")
    cat("----------------------------\n")
    cat("理論平均數 (μ):", round(clt_mean, 6), "\n")
    cat("理論標準差 (σ/√n):", if(!is.na(clt_sd)) round(clt_sd, 6) else "NA", "\n")
  })

}

# --- 執行 App ---
shinyApp(ui = ui, server = server)

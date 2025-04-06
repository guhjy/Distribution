# --- 必要的套件 ---
library(shiny)
library(ggplot2) # 用於繪圖
library(dplyr)   # 用於資料處理 (方便起見)
library(stringr) # 用於字串處理

# --- 輔助函數：計算眾數 ---
# 注意：對於連續分佈，眾數是密度函數最高點對應的值。
#       對於離散分佈，眾數是機率質量函數最高點對應的值。
#       有些分佈（如柯西）或特定參數下的分佈（如某些Beta）可能沒有唯一的眾數或計算複雜。
#       這裡提供了一些常見分佈的解析解或常用計算方式。
calculate_mode <- function(dist_name, params) {
  mode_val <- NA # 預設值

  tryCatch({
    if (dist_name == "常態分佈 (Gaussian)") {
      mode_val <- params$mean
    } else if (dist_name == "對數常態分佈 (Lognormal)") {
      if (params$sdlog > 0) {
         mode_val <- exp(params$meanlog - params$sdlog^2)
      } else {
         mode_val <- exp(params$meanlog) # 退化情況
      }
    } else if (dist_name == "伽瑪分佈 (Gamma)") {
      if (params$shape >= 1) {
        mode_val <- (params$shape - 1) / params$rate
      } else {
        mode_val <- 0 # 或接近0，當 shape < 1 時，眾數在0
      }
    } else if (dist_name == "t 分佈 (Student's t)") {
      mode_val <- params$mu # t分佈的眾數是其位置參數 (若定義)
    } else if (dist_name == "指數分佈 (Exponential)") {
      mode_val <- 0 # 指數分佈的眾數永遠是0
    } else if (dist_name == "柯西分佈 (Cauchy)") {
      mode_val <- params$location # 柯西分佈的眾數是其位置參數
    } else if (dist_name == "貝他分佈 (Beta)") {
      if (params$shape1 > 1 && params$shape2 > 1) {
        mode_val <- (params$shape1 - 1) / (params$shape1 + params$shape2 - 2)
      } else if (params$shape1 == 1 && params$shape2 == 1) {
        mode_val <- "任意值 (0,1)" # 均勻分佈
      } else if (params$shape1 <= 1 && params$shape2 > 1) {
         mode_val <- 0
      } else if (params$shape1 > 1 && params$shape2 <= 1) {
         mode_val <- 1
      } else { # shape1 <= 1 and shape2 <= 1 (非均勻情況)
         mode_val <- "0 或 1 (雙峰)"
      }
    } else if (dist_name == "二項分佈 (Binomial)") {
      # floor((n+1)p)
      mode_val <- floor((params$size + 1) * params$prob)
      # 如果 (n+1)p 是整數，則 k 和 k-1 都是眾數
      if ( (params$size + 1) * params$prob == mode_val && params$prob != 0 && params$prob != 1) {
         mode_val <- paste(mode_val - 1, "和", mode_val)
      }
    } else if (dist_name == "卜瓦松分佈 (Poisson)") {
      # floor(lambda)
      mode_val <- floor(params$lambda)
       # 如果 lambda 是整數，則 k 和 k-1 都是眾數
      if (params$lambda == mode_val && params$lambda > 0) {
          mode_val <- paste(mode_val - 1, "和", mode_val)
      }
    }
  }, error = function(e) {
    # 發生錯誤時返回 NA
    mode_val <- NA
  })

  # 對於數值結果，格式化為有限位數
  if (is.numeric(mode_val)) {
     return(round(mode_val, 4))
  } else {
     return(mode_val) # 返回字串（如 "未定義" 或多個眾數）
  }
}

# --- UI 定義 ---
ui <- fluidPage(
  titlePanel("機率分佈特性展示與醫學範例 (繁體中文)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "選擇分佈類型:",
                  choices = c("常態分佈 (Gaussian)",
                              "對數常態分佈 (Lognormal)",
                              "伽瑪分佈 (Gamma)",
                              "t 分佈 (Student's t)",
                              "指數分佈 (Exponential)",
                              "柯西分佈 (Cauchy)",
                              "貝他分佈 (Beta)",
                              "二項分佈 (Binomial)",
                              "卜瓦松分佈 (Poisson)")),
      hr(),

      # --- 根據選擇的分佈顯示對應參數輸入 ---
      # 常態分佈
      conditionalPanel(
        condition = "input.dist == '常態分佈 (Gaussian)'",
        numericInput("norm_mean", "平均數 (μ):", value = 0),
        numericInput("norm_sd", "標準差 (σ):", value = 1, min = 0.001) # 標準差需為正
      ),
      # 對數常態分佈
      conditionalPanel(
        condition = "input.dist == '對數常態分佈 (Lognormal)'",
        numericInput("lnorm_meanlog", "對數平均數 (μ_log):", value = 0),
        numericInput("lnorm_sdlog", "對數標準差 (σ_log):", value = 1, min = 0.001) # 標準差需為正
      ),
      # 伽瑪分佈
      conditionalPanel(
        condition = "input.dist == '伽瑪分佈 (Gamma)'",
        # 使用 Shape & Rate 參數化
        numericInput("gamma_shape", "形狀 (Shape, α):", value = 2, min = 0.001),
        numericInput("gamma_rate", "速率 (Rate, β):", value = 1, min = 0.001) # Rate (或 Scale) 需為正
        # 備註: R 中的 rate = 1/scale
      ),
      # t 分佈
      conditionalPanel(
        condition = "input.dist == 't 分佈 (Student\\'s t)'",
        numericInput("t_df", "自由度 (df):", value = 10, min = 1),
        numericInput("t_mu", "位置 (μ):", value = 0), # 非中心 t 分佈的位置參數 mu (等同於常態的 mean)
        numericInput("t_sigma", "尺度 (σ):", value = 1, min = 0.001) # 非中心 t 分佈的尺度參數 sigma (等同於常態的 sd)
        # 注意：標準 t 分佈 mu=0, sigma=1
      ),
      # 指數分佈
      conditionalPanel(
        condition = "input.dist == '指數分佈 (Exponential)'",
        numericInput("exp_rate", "速率 (Rate, λ):", value = 1, min = 0.001) # Rate 需為正
      ),
      # 柯西分佈
      conditionalPanel(
        condition = "input.dist == '柯西分佈 (Cauchy)'",
        numericInput("cauchy_location", "位置 (x0):", value = 0),
        numericInput("cauchy_scale", "尺度 (γ):", value = 1, min = 0.001) # Scale 需為正
      ),
      # 貝他分佈
      conditionalPanel(
        condition = "input.dist == '貝他分佈 (Beta)'",
        numericInput("beta_shape1", "形狀1 (α):", value = 2, min = 0.001), # Shape 參數需為正
        numericInput("beta_shape2", "形狀2 (β):", value = 5, min = 0.001)
      ),
      # 二項分佈
      conditionalPanel(
        condition = "input.dist == '二項分佈 (Binomial)'",
        numericInput("binom_size", "試驗次數 (n):", value = 10, min = 1, step = 1),
        sliderInput("binom_prob", "成功機率 (p):", min = 0, max = 1, value = 0.5, step = 0.01)
      ),
      # 卜瓦松分佈
      conditionalPanel(
        condition = "input.dist == '卜瓦松分佈 (Poisson)'",
        numericInput("pois_lambda", "平均發生率 (λ):", value = 3, min = 0) # Lambda >= 0
      ),

      hr(),
      p("注意：對於某些參數組合（如 t 分佈自由度 df=1 或柯西分佈），平均數或更高階動差可能不存在。")

    ),
    mainPanel(
      plotOutput("distPlot"),
      h4("統計摘要:"),
      tableOutput("summaryStats"),
      h4("分佈說明:"),
      uiOutput("explanation"),
      h4("醫學範例:"),
      uiOutput("medical_example")
    )
  )
)

# --- Server 邏輯 ---
server <- function(input, output, session) {
  
# --- 載入與設定中文字型 ---
library(showtext)
showtext_auto()
font_add("kai", "DFKai-SB")
theme_set(theme_minimal(base_family = "kai"))
  
  # --- 獲取當前參數 ---
  get_params <- reactive({
    params <- list()
    dist_name <- input$dist
    if (dist_name == "常態分佈 (Gaussian)") {
      req(input$norm_sd) # 確保輸入存在
      if(input$norm_sd <= 0) {
          showNotification("常態分佈的標準差必須為正數。", type = "error")
          return(NULL) # 返回 NULL 表示參數無效
      }
      params$mean <- input$norm_mean
      params$sd <- input$norm_sd
    } else if (dist_name == "對數常態分佈 (Lognormal)") {
      req(input$lnorm_sdlog)
       if(input$lnorm_sdlog <= 0) {
          showNotification("對數常態分佈的對數標準差必須為正數。", type = "error")
          return(NULL)
       }
      params$meanlog <- input$lnorm_meanlog
      params$sdlog <- input$lnorm_sdlog
    } else if (dist_name == "伽瑪分佈 (Gamma)") {
      req(input$gamma_shape, input$gamma_rate)
       if(input$gamma_shape <= 0 || input$gamma_rate <= 0) {
          showNotification("伽瑪分佈的形狀 (Shape) 和速率 (Rate) 必須為正數。", type = "error")
          return(NULL)
       }
      params$shape <- input$gamma_shape
      params$rate <- input$gamma_rate
    } else if (dist_name == "t 分佈 (Student's t)") {
       req(input$t_df, input$t_sigma)
       if(input$t_df < 1 || input$t_sigma <= 0) {
           showNotification("t 分佈的自由度必須 >= 1 且尺度必須為正數。", type = "error")
           return(NULL)
       }
       params$df <- input$t_df
       params$mu <- input$t_mu   # 位置參數
       params$sigma <- input$t_sigma # 尺度參數
       # 注意：R 的 dt, pt, qt, rt 使用的是標準化的 t 分佈（ncp 非中心參數）
       # 若要處理有位置(mu)和尺度(sigma)的 t 分佈，需要在繪圖和計算時轉換
       # X = mu + sigma * T, 其中 T 是標準 t 分佈變量
    } else if (dist_name == "指數分佈 (Exponential)") {
      req(input$exp_rate)
      if(input$exp_rate <= 0) {
          showNotification("指數分佈的速率 (Rate) 必須為正數。", type = "error")
          return(NULL)
       }
      params$rate <- input$exp_rate
    } else if (dist_name == "柯西分佈 (Cauchy)") {
      req(input$cauchy_scale)
      if(input$cauchy_scale <= 0) {
          showNotification("柯西分佈的尺度 (Scale) 必須為正數。", type = "error")
          return(NULL)
       }
      params$location <- input$cauchy_location
      params$scale <- input$cauchy_scale
    } else if (dist_name == "貝他分佈 (Beta)") {
       req(input$beta_shape1, input$beta_shape2)
       if(input$beta_shape1 <= 0 || input$beta_shape2 <= 0) {
           showNotification("貝他分佈的形狀參數 (α, β) 必須為正數。", type = "error")
           return(NULL)
       }
      params$shape1 <- input$beta_shape1
      params$shape2 <- input$beta_shape2
    } else if (dist_name == "二項分佈 (Binomial)") {
      req(input$binom_size, input$binom_prob)
       if(input$binom_size < 1 || input$binom_prob < 0 || input$binom_prob > 1) {
           showNotification("二項分佈的試驗次數 (n) 必須 >= 1，成功機率 (p) 必須在 [0, 1] 之間。", type = "error")
           return(NULL)
       }
      params$size <- as.integer(input$binom_size) # 確保為整數
      params$prob <- input$binom_prob
    } else if (dist_name == "卜瓦松分佈 (Poisson)") {
      req(input$pois_lambda)
       if(input$pois_lambda < 0) {
           showNotification("卜瓦松分佈的平均發生率 (λ) 必須 >= 0。", type = "error")
           return(NULL)
       }
      params$lambda <- input$pois_lambda
    }
    return(params)
  })

  # --- 計算統計量 ---
  calculate_stats <- reactive({
    params <- get_params()
    req(params) # 確保參數有效
    dist_name <- input$dist
    stats <- list(Mean = NA, Median = NA, Mode = NA, Pct_2.5 = NA, Pct_97.5 = NA)

    # 嘗試計算，處理可能發生的錯誤 (例如柯西分佈的平均數)
    tryCatch({
      if (dist_name == "常態分佈 (Gaussian)") {
        stats$Mean <- params$mean
        stats$Median <- qnorm(0.5, mean = params$mean, sd = params$sd)
        stats$Pct_2.5 <- qnorm(0.025, mean = params$mean, sd = params$sd)
        stats$Pct_97.5 <- qnorm(0.975, mean = params$mean, sd = params$sd)
      } else if (dist_name == "對數常態分佈 (Lognormal)") {
        stats$Mean <- exp(params$meanlog + 0.5 * params$sdlog^2)
        stats$Median <- qlnorm(0.5, meanlog = params$meanlog, sdlog = params$sdlog) # == exp(params$meanlog)
        stats$Pct_2.5 <- qlnorm(0.025, meanlog = params$meanlog, sdlog = params$sdlog)
        stats$Pct_97.5 <- qlnorm(0.975, meanlog = params$meanlog, sdlog = params$sdlog)
      } else if (dist_name == "伽瑪分佈 (Gamma)") {
        stats$Mean <- params$shape / params$rate
        stats$Median <- qgamma(0.5, shape = params$shape, rate = params$rate)
        stats$Pct_2.5 <- qgamma(0.025, shape = params$shape, rate = params$rate)
        stats$Pct_97.5 <- qgamma(0.975, shape = params$shape, rate = params$rate)
      } else if (dist_name == "t 分佈 (Student's t)") {
        # 對於有位置和尺度的 t 分佈 X = mu + sigma * T
        if (params$df > 1) {
          stats$Mean <- params$mu # Mean of T is 0 for df > 1. Mean of X is mu + sigma * E[T] = mu.
        } else {
          stats$Mean <- "未定義 (df ≤ 1)"
        }
        # Median of T is 0. Median of X is mu + sigma * Median[T] = mu.
        stats$Median <- params$mu
        # Quantiles: Q_X(p) = mu + sigma * Q_T(p)
        stats$Pct_2.5 <- params$mu + params$sigma * qt(0.025, df = params$df)
        stats$Pct_97.5 <- params$mu + params$sigma * qt(0.975, df = params$df)

      } else if (dist_name == "指數分佈 (Exponential)") {
        stats$Mean <- 1 / params$rate
        stats$Median <- qexp(0.5, rate = params$rate) # == log(2) / rate
        stats$Pct_2.5 <- qexp(0.025, rate = params$rate)
        stats$Pct_97.5 <- qexp(0.975, rate = params$rate)
      } else if (dist_name == "柯西分佈 (Cauchy)") {
        stats$Mean <- "未定義" # 柯西分佈的平均數不存在
        stats$Median <- qcauchy(0.5, location = params$location, scale = params$scale) # == params$location
        stats$Pct_2.5 <- qcauchy(0.025, location = params$location, scale = params$scale)
        stats$Pct_97.5 <- qcauchy(0.975, location = params$location, scale = params$scale)
      } else if (dist_name == "貝他分佈 (Beta)") {
        stats$Mean <- params$shape1 / (params$shape1 + params$shape2)
        stats$Median <- qbeta(0.5, shape1 = params$shape1, shape2 = params$shape2)
        stats$Pct_2.5 <- qbeta(0.025, shape1 = params$shape1, shape2 = params$shape2)
        stats$Pct_97.5 <- qbeta(0.975, shape1 = params$shape1, shape2 = params$shape2)
      } else if (dist_name == "二項分佈 (Binomial)") {
        stats$Mean <- params$size * params$prob
        stats$Median <- qbinom(0.5, size = params$size, prob = params$prob) # 注意：離散中位數可能不唯一
        stats$Pct_2.5 <- qbinom(0.025, size = params$size, prob = params$prob)
        stats$Pct_97.5 <- qbinom(0.975, size = params$size, prob = params$prob)
      } else if (dist_name == "卜瓦松分佈 (Poisson)") {
        stats$Mean <- params$lambda
        stats$Median <- qpois(0.5, lambda = params$lambda) # 注意：離散中位數可能不唯一
        stats$Pct_2.5 <- qpois(0.025, lambda = params$lambda)
        stats$Pct_97.5 <- qpois(0.975, lambda = params$lambda)
      }

      # 計算眾數（使用輔助函數）
      stats$Mode <- calculate_mode(dist_name, params)

    }, error = function(e) {
      showNotification(paste("計算統計量時出錯:", e$message), type = "error")
      # 如果計算出錯，確保返回包含 NA 的列表
      stats <- list(Mean = NA, Median = NA, Mode = NA, Pct_2.5 = NA, Pct_97.5 = NA)
    })

    # 格式化數值輸出
    stats_formatted <- lapply(stats, function(x) {
        if (is.numeric(x)) {
            format(round(x, 4), nsmall = 4)
        } else {
            as.character(x) # 保持 "未定義" 或眾數的字串形式
        }
    })

    return(stats_formatted)
  })

  # --- 顯示統計摘要表格 ---
  output$summaryStats <- renderTable({
    stats <- calculate_stats()
    req(stats) # 確保 stats 有效

    # 創建一個數據框來顯示
    data.frame(
      指標 = c("平均數 (Mean)", "中位數 (Median)", "眾數 (Mode)", "2.5% 百分位數", "97.5% 百分位數"),
      數值 = c(stats$Mean, stats$Median, stats$Mode, stats$Pct_2.5, stats$Pct_97.5),
      stringsAsFactors = FALSE
    )
  }, align = 'lr') # 靠左對齊指標，靠右對齊數值

  # --- 繪製分佈圖 ---
  output$distPlot <- renderPlot({
    params <- get_params()
    req(params) # 確保參數有效
    dist_name <- input$dist
    stats <- calculate_stats() # 獲取計算好的統計量 (格式化前的數值較好用於繪圖)
    
    # 嘗試重新獲取數值型的統計量用於繪圖標線
    raw_stats_for_plot <- reactive({
        s <- list(Mean = NA, Median = NA, Pct_2.5 = NA, Pct_97.5 = NA)
        # 重新計算數值部分，忽略衆數和未定義情況
        tryCatch({
         if (dist_name == "常態分佈 (Gaussian)") {
            s$Mean <- params$mean; s$Median <- qnorm(0.5, params$mean, params$sd); s$Pct_2.5 <- qnorm(0.025, params$mean, params$sd); s$Pct_97.5 <- qnorm(0.975, params$mean, params$sd)
          } else if (dist_name == "對數常態分佈 (Lognormal)") {
             s$Mean <- exp(params$meanlog + 0.5 * params$sdlog^2); s$Median <- qlnorm(0.5, params$meanlog, params$sdlog); s$Pct_2.5 <- qlnorm(0.025, params$meanlog, params$sdlog); s$Pct_97.5 <- qlnorm(0.975, params$meanlog, params$sdlog)
          } else if (dist_name == "伽瑪分佈 (Gamma)") {
            s$Mean <- params$shape / params$rate; s$Median <- qgamma(0.5, params$shape, params$rate); s$Pct_2.5 <- qgamma(0.025, params$shape, params$rate); s$Pct_97.5 <- qgamma(0.975, params$shape, params$rate)
          } else if (dist_name == "t 分佈 (Student's t)") {
             if (params$df > 1) s$Mean <- params$mu; s$Median <- params$mu; s$Pct_2.5 <- params$mu + params$sigma * qt(0.025, params$df); s$Pct_97.5 <- params$mu + params$sigma * qt(0.975, params$df)
          } else if (dist_name == "指數分佈 (Exponential)") {
            s$Mean <- 1 / params$rate; s$Median <- qexp(0.5, params$rate); s$Pct_2.5 <- qexp(0.025, params$rate); s$Pct_97.5 <- qexp(0.975, params$rate)
          } else if (dist_name == "柯西分佈 (Cauchy)") {
             # Mean is undefined
             s$Median <- qcauchy(0.5, params$location, params$scale); s$Pct_2.5 <- qcauchy(0.025, params$location, params$scale); s$Pct_97.5 <- qcauchy(0.975, params$location, params$scale)
          } else if (dist_name == "貝他分佈 (Beta)") {
             s$Mean <- params$shape1 / (params$shape1 + params$shape2); s$Median <- qbeta(0.5, params$shape1, params$shape2); s$Pct_2.5 <- qbeta(0.025, params$shape1, params$shape2); s$Pct_97.5 <- qbeta(0.975, params$shape1, params$shape2)
          } else if (dist_name == "二項分佈 (Binomial)") {
             s$Mean <- params$size * params$prob; s$Median <- qbinom(0.5, params$size, params$prob); s$Pct_2.5 <- qbinom(0.025, params$size, params$prob); s$Pct_97.5 <- qbinom(0.975, params$size, params$prob)
          } else if (dist_name == "卜瓦松分佈 (Poisson)") {
             s$Mean <- params$lambda; s$Median <- qpois(0.5, params$lambda); s$Pct_2.5 <- qpois(0.025, params$lambda); s$Pct_97.5 <- qpois(0.975, params$lambda)
          }
        }, error = function(e) {}) # 忽略計算錯誤
        return(s)
    })() # 立即執行

    plot_title <- paste(dist_name, "機率密度/質量函數")
    p <- NULL # 初始化 ggplot 物件

    # --- 連續分佈 ---
    if (dist_name %in% c("常態分佈 (Gaussian)", "對數常態分佈 (Lognormal)", "伽瑪分佈 (Gamma)", "t 分佈 (Student's t)", "指數分佈 (Exponential)", "柯西分佈 (Cauchy)", "貝他分佈 (Beta)")) {
      
      # 確定繪圖範圍 (通常基於百分位數)
      plot_range <- tryCatch({
          lower <- NA; upper <- NA
          if (!is.na(raw_stats_for_plot$Pct_2.5) && !is.na(raw_stats_for_plot$Pct_97.5)) {
              range_val <- raw_stats_for_plot$Pct_97.5 - raw_stats_for_plot$Pct_2.5
              # 擴展範圍以更好地顯示尾部，但對柯西分佈要小心
              extend_factor <- ifelse(dist_name == "柯西分佈 (Cauchy)", 1.5, 3) 
              lower <- raw_stats_for_plot$Pct_2.5 - extend_factor * range_val / 2
              upper <- raw_stats_for_plot$Pct_97.5 + extend_factor * range_val / 2
              
              # 特殊處理：Beta 分佈範圍在 [0, 1]
              if (dist_name == "貝他分佈 (Beta)") {
                  lower <- max(0, lower); upper <- min(1, upper)
              }
              # 特殊處理：指數和伽瑪分佈範圍 >= 0
              if (dist_name %in% c("指數分佈 (Exponential)", "伽瑪分佈 (Gamma)")) {
                  lower <- max(0, lower)
              }
              # 如果範圍太小或無效，提供預設
              if (is.na(lower) || is.na(upper) || lower >= upper) {
                  if (dist_name == "貝他分佈 (Beta)") { c(0, 1) }
                  else if (dist_name %in% c("指數分佈 (Exponential)", "伽瑪分佈 (Gamma)")) { c(0, qgamma(0.999, params$shape, params$rate) * 1.1) } # 示例範圍
                  else if (dist_name == "對數常態分佈 (Lognormal)") { c(0, qlnorm(0.999, params$meanlog, params$sdlog) * 1.1) }
                  else { c(qnorm(0.001, params$mean, params$sd), qnorm(0.999, params$mean, params$sd)) } # 預設基於常態
              } else {
                  c(lower, upper)
              }
          } else { # 如果百分位數計算失敗
              if (dist_name == "貝他分佈 (Beta)") { c(0, 1) }
              else if (dist_name == "柯西分佈 (Cauchy)") { c(params$location - 5*params$scale, params$location + 5*params$scale)} # 柯西的範圍需要小心
              else if (dist_name %in% c("指數分佈 (Exponential)", "伽瑪分佈 (Gamma)")) { c(0, 10) } # 預設
              else { c(-5, 5) } # 預設
          }
      }, error = function(e){ if (dist_name == "貝他分佈 (Beta)") c(0,1) else c(-5,5)}) # 出錯時的最終預設範圍

      x_vals <- seq(plot_range[1], plot_range[2], length.out = 400)
      y_vals <- NULL
      
      if (dist_name == "常態分佈 (Gaussian)") {
        y_vals <- dnorm(x_vals, mean = params$mean, sd = params$sd)
      } else if (dist_name == "對數常態分佈 (Lognormal)") {
        y_vals <- dlnorm(x_vals, meanlog = params$meanlog, sdlog = params$sdlog)
        # 對數常態定義域 x > 0
        x_vals <- x_vals[x_vals > 1e-9] # 避免 log(0) 或負數
        y_vals <- dlnorm(x_vals, meanlog = params$meanlog, sdlog = params$sdlog)
        if (length(x_vals) == 0) x_vals <- seq(1e-6, plot_range[2], length.out = 400) # 如果範圍全負，重新生成
        if (is.null(y_vals) || length(y_vals) != length(x_vals)) y_vals <- dlnorm(x_vals, meanlog = params$meanlog, sdlog = params$sdlog)

      } else if (dist_name == "伽瑪分佈 (Gamma)") {
        # 伽瑪定義域 x >= 0
         x_vals <- x_vals[x_vals >= 0]
         if (length(x_vals) == 0) x_vals <- seq(1e-6, plot_range[2], length.out = 400)
         y_vals <- dgamma(x_vals, shape = params$shape, rate = params$rate)
      } else if (dist_name == "t 分佈 (Student's t)") {
         # dt 需要標準 t 的值，所以我們計算標準 t 的密度，然後轉換 x 軸
         # 或者，直接計算密度：f_X(x) = (1/sigma) * f_T((x-mu)/sigma)
         y_vals <- (1 / params$sigma) * dt((x_vals - params$mu) / params$sigma, df = params$df)
      } else if (dist_name == "指數分佈 (Exponential)") {
         # 指數定義域 x >= 0
         x_vals <- x_vals[x_vals >= 0]
         if (length(x_vals) == 0) x_vals <- seq(1e-6, plot_range[2], length.out = 400)
         y_vals <- dexp(x_vals, rate = params$rate)
      } else if (dist_name == "柯西分佈 (Cauchy)") {
        y_vals <- dcauchy(x_vals, location = params$location, scale = params$scale)
        # 限制 y 軸，因為柯西峰值可能很高但尾部很寬
         y_limit <- quantile(y_vals, 0.99, na.rm = TRUE) * 1.5 # 限制在 99% 分位數的 1.5 倍
         if(is.na(y_limit) || y_limit == 0) y_limit <- max(y_vals, na.rm = TRUE) * 0.8 # 備用方案
         y_vals[y_vals > y_limit] <- NA # 超出範圍的不繪製，避免 y 軸被拉伸
      } else if (dist_name == "貝他分佈 (Beta)") {
         # Beta 定義域 [0, 1]
         x_vals <- seq(max(0, plot_range[1]), min(1, plot_range[2]), length.out = 400)
         # 避免 x=0 或 x=1 時 shape < 1 導致的 Inf
         eps <- 1e-6
         x_vals[x_vals < eps] <- eps
         x_vals[x_vals > 1 - eps] <- 1 - eps
         y_vals <- dbeta(x_vals, shape1 = params$shape1, shape2 = params$shape2)
      }

      df_plot <- data.frame(x = x_vals, y = y_vals) %>% filter(!is.na(y) & is.finite(y)) # 過濾無效值
      if (nrow(df_plot) > 0) {
         p <- ggplot(df_plot, aes(x = x, y = y)) +
             geom_line(color = "blue", linewidth = 1) +
             labs(title = plot_title, x = "數值", y = "機率密度") +
             theme_minimal()

         # 添加垂直線標示統計量
         if (!is.na(raw_stats_for_plot$Mean) && is.numeric(raw_stats_for_plot$Mean)) p <- p + geom_vline(xintercept = raw_stats_for_plot$Mean, linetype = "dashed", color = "red")
         if (!is.na(raw_stats_for_plot$Median) && is.numeric(raw_stats_for_plot$Median)) p <- p + geom_vline(xintercept = raw_stats_for_plot$Median, linetype = "dotted", color = "green4")
         if (!is.na(raw_stats_for_plot$Pct_2.5) && is.numeric(raw_stats_for_plot$Pct_2.5)) p <- p + geom_vline(xintercept = raw_stats_for_plot$Pct_2.5, linetype = "solid", color = "orange")
         if (!is.na(raw_stats_for_plot$Pct_97.5) && is.numeric(raw_stats_for_plot$Pct_97.5)) p <- p + geom_vline(xintercept = raw_stats_for_plot$Pct_97.5, linetype = "solid", color = "orange")

         # 添加圖例說明垂直線
         line_labels <- c()
         line_colors <- c()
         line_types <- c()
         if (!is.na(raw_stats_for_plot$Mean) && is.numeric(raw_stats_for_plot$Mean)) { line_labels <- c(line_labels, "平均數"); line_colors <- c(line_colors, "red"); line_types <- c(line_types, "dashed") }
         if (!is.na(raw_stats_for_plot$Median) && is.numeric(raw_stats_for_plot$Median)) { line_labels <- c(line_labels, "中位數"); line_colors <- c(line_colors, "green4"); line_types <- c(line_types, "dotted") }
         if (!is.na(raw_stats_for_plot$Pct_2.5) && is.numeric(raw_stats_for_plot$Pct_2.5)) { line_labels <- c(line_labels, "2.5/97.5 百分位"); line_colors <- c(line_colors, "orange"); line_types <- c(line_types, "solid") }
         
         if (length(line_labels) > 0) {
            p <- p + scale_linetype_manual(name = "指標", values = line_types, labels = line_labels) +
                     scale_color_manual(name = "指標", values = line_colors, labels = line_labels) +
                     guides(linetype = guide_legend(), color = guide_legend()) # 確保圖例顯示
            
            # 由於 ggplot 可能會合併圖例，需要調整讓線條和顏色對應
             p <- p + guides(
                color = guide_legend(override.aes = list(linetype = line_types)),
                linetype = guide_legend(override.aes = list(color = line_colors))
             )

         }


      } else {
         # 如果沒有有效的繪圖數據
         p <- ggplot() + labs(title = plot_title, x="", y="") + annotate("text", x=0, y=0, label="無法繪製圖形（參數或範圍無效）")
      }

    # --- 離散分佈 ---
    } else if (dist_name %in% c("二項分佈 (Binomial)", "卜瓦松分佈 (Poisson)")) {
      x_vals <- NULL
      y_vals <- NULL
      
      if (dist_name == "二項分佈 (Binomial)") {
          # 繪圖範圍：從 0 到 n
          x_vals <- 0:params$size
          y_vals <- dbinom(x_vals, size = params$size, prob = params$prob)
          plot_title <- paste(dist_name, "- n =", params$size, ", p =", round(params$prob, 3))
      } else if (dist_name == "卜瓦松分佈 (Poisson)") {
          # 繪圖範圍：從 0 到一個合理的上限（例如 99.99% 百分位數再加一些）
          upper_limit <- qpois(0.9999, lambda = params$lambda)
          # 確保至少包含 lambda 附近的值，且至少有幾個點
          upper_limit <- max(upper_limit, ceiling(params$lambda + 3 * sqrt(params$lambda)), 5)
          x_vals <- 0:upper_limit
          y_vals <- dpois(x_vals, lambda = params$lambda)
          plot_title <- paste(dist_name, "- λ =", params$lambda)
      }

      df_plot <- data.frame(x = x_vals, y = y_vals)
      p <- ggplot(df_plot, aes(x = factor(x), y = y)) + # 將 x 轉為因子以正確繪製長條圖
           geom_col(fill = "skyblue", color = "black") + # 使用 geom_col 繪製長條圖
           labs(title = plot_title, x = "事件數量 (k)", y = "機率 P(X=k)") +
           theme_minimal() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 旋轉 x 軸標籤避免重疊

       # 添加垂直線標示統計量 (對於離散，標示在對應的 x 值上)
       # 注意：離散分佈的百分位數可能落在兩個值之間，這裡標示函數返回的值
        if (!is.na(raw_stats_for_plot$Mean) && is.numeric(raw_stats_for_plot$Mean)) p <- p + geom_vline(xintercept = as.character(round(raw_stats_for_plot$Mean)), linetype = "dashed", color = "red", linewidth=1)
        if (!is.na(raw_stats_for_plot$Median) && is.numeric(raw_stats_for_plot$Median)) p <- p + geom_vline(xintercept = as.character(raw_stats_for_plot$Median), linetype = "dotted", color = "green4", linewidth=1)
        if (!is.na(raw_stats_for_plot$Pct_2.5) && is.numeric(raw_stats_for_plot$Pct_2.5)) p <- p + geom_vline(xintercept = as.character(raw_stats_for_plot$Pct_2.5), linetype = "solid", color = "orange", linewidth=1)
        if (!is.na(raw_stats_for_plot$Pct_97.5) && is.numeric(raw_stats_for_plot$Pct_97.5)) p <- p + geom_vline(xintercept = as.character(raw_stats_for_plot$Pct_97.5), linetype = "solid", color = "orange", linewidth=1)

        # 添加圖例 (邏輯同連續分佈)
         line_labels <- c()
         line_colors <- c()
         line_types <- c()
         if (!is.na(raw_stats_for_plot$Mean) && is.numeric(raw_stats_for_plot$Mean)) { line_labels <- c(line_labels, "平均數"); line_colors <- c(line_colors, "red"); line_types <- c(line_types, "dashed") }
         if (!is.na(raw_stats_for_plot$Median) && is.numeric(raw_stats_for_plot$Median)) { line_labels <- c(line_labels, "中位數"); line_colors <- c(line_colors, "green4"); line_types <- c(line_types, "dotted") }
         if (!is.na(raw_stats_for_plot$Pct_2.5) && is.numeric(raw_stats_for_plot$Pct_2.5)) { line_labels <- c(line_labels, "2.5/97.5 百分位"); line_colors <- c(line_colors, "orange"); line_types <- c(line_types, "solid") }
         
         if (length(line_labels) > 0) {
            # 為了能在離散圖上顯示線條圖例，我們創建一個假的 geom_line 層
             dummy_df <- data.frame(x = rep(as.character(x_vals[1]), length(line_labels)), 
                                    y = rep(max(y_vals, na.rm=TRUE)*1.1, length(line_labels)), # 放在圖上方
                                    Indicator = factor(line_labels, levels=line_labels))
             p <- p + geom_line(data = dummy_df, aes(x = x, y = y, color = Indicator, linetype = Indicator), inherit.aes = FALSE) +
                      scale_linetype_manual(name = "指標", values = line_types, labels = line_labels) +
                      scale_color_manual(name = "指標", values = line_colors, labels = line_labels) +
                      guides(linetype = guide_legend(), color = guide_legend())
         }
    }

    print(p) # 顯示圖形
  })

  # --- 顯示分佈說明 ---
  output$explanation <- renderUI({
    dist_name <- input$dist
    text <- switch(dist_name,
      "常態分佈 (Gaussian)" = HTML("常態分佈（又稱高斯分佈）是最重要的連續機率分佈之一。圖形呈鐘形，對稱於平均數。由平均數 (μ) 和標準差 (σ) 決定。μ 決定中心位置，σ 決定分散程度（圖形高矮胖瘦）。許多自然現象和測量誤差的抽樣分布大致遵循常態分佈（根據中央極限定理），其誤差是相加的。"),
      "對數常態分佈 (Lognormal)" = HTML("如果一個隨機變數 X 的自然對數 ln(X) 服從常態分佈，則 X 服從對數常態分佈，其誤差是相乘的。此分佈是右偏的（尾部拖向右側），且值域為正數 (X > 0)。由對數空間的平均數 (μ_log) 和標準差 (σ_log) 決定。常用於描述其值不能為負且分佈不對稱的變數。"),
      "伽瑪分佈 (Gamma)" = HTML("伽瑪分佈是包含指數分佈和卡方分佈的連續機率分佈族。值域為正數 (X ≥ 0)，通常是右偏的，類似對數常態分佈。由形狀參數 (α 或 k) 和速率參數 (β 或 λ，有時用尺度參數 θ = 1/β) 定義。常用於模擬等待時間、壽命或總量。當形狀參數 α=1 時，伽瑪分佈即為指數分佈。"),
      "t 分佈 (Student's t)" = HTML("t 分佈（或稱學生 t 分佈）形狀類似常態分佈，呈鐘形且對稱，但具有更『重』的尾部（即尾部機率比常態分佈大）。由自由度 (df) 參數決定。當自由度增加時，t 分佈趨近於標準常態分佈。主要用於樣本量較小 (<30) 且總體標準差未知時，對總體平均數進行估計和假設檢定。位置(μ)和尺度(σ)參數允許產生非標準化的t分佈。"),
      "指數分佈 (Exponential)" = HTML("指數分佈是描述獨立隨機事件發生之間的時間（或距離）的連續機率分佈。值域為正數 (X ≥ 0)，具有『無記憶性』，即未來事件發生的機率與過去等待了多久無關。由單一的速率參數 (λ) 決定，平均數為 1/λ。圖形從最高點急速下降，是右偏的。"),
      "柯西分佈 (Cauchy)" = HTML("柯西分佈（也稱洛倫茲分佈）是一種對稱的鐘形連續分佈，但其尾部比常態分佈甚至 t 分佈都要『重』得多，以至於其平均數、變異數等高階動差均未定義（期望值不存在）。由位置參數 (x0，決定峰值位置，也是中位數和眾數) 和尺度參數 (γ，決定半峰全寬的一半) 決定。雖然在生物統計中直接應用較少，但它在物理學和穩健統計學、貝氏學派的弱先驗 cauchy(0, 0.707)中很重要。"),
      "貝他分佈 (Beta)" = HTML("貝他分佈是定義在區間 [0, 1] 上的連續機率分佈族，由兩個正的形狀參數 α 和 β 控制。它可以呈現多種形狀（對稱、偏斜、U形、J形等），非常適合模擬百分比、比例、有上下限的數據或機率本身的分佈。"),
      "二項分佈 (Binomial)" = HTML("二項分佈是描述在一系列固定的、獨立的『成功/失敗』試驗（稱為白努利試驗, 分佈是 Bernoulli(p)）中，成功次數的離散機率分佈。由試驗總次數 (n) 和單次試驗的成功機率 (p) 決定 Bernoulli(n, p)。其值域為 0, 1, 2, ..., n。"),
      "卜瓦松分佈 (Poisson)" = HTML("卜瓦松分佈是描述在一個固定的時間間隔、空間區域或體積內，某事件發生的次數的離散機率分佈。假設事件是獨立發生的，且平均發生率是恆定的。由單一參數 λ (lambda) 決定，λ 既是平均發生次數，也是分佈的變異數。其值域為 0, 1, 2, ...。")
    )
    return(text)
  })

  # --- 顯示醫學範例 ---
  output$medical_example <- renderUI({
    dist_name <- input$dist
    text <- switch(dist_name,
      "常態分佈 (Gaussian)" = HTML("<ul><li>成年男性的身高分佈。</li><li>重複測量同一樣本的血糖值時的測量誤差。</li><li>人群中收縮壓（舒張壓）的分佈（可能需要對數轉換或考慮年齡等因素）。</li></ul>"),
      "對數常態分佈 (Lognormal)" = HTML("<ul><li>疾病的潛伏期（從感染到出現症狀的時間）。</li><li>空氣中或血液中污染物/藥物濃度的分佈。</li><li>某些生理指標的測量值，如比例、抗體滴度、收入、費用、住院天數。</li><li>傷口癒合所需時間。</li></ul>"),
      "伽瑪分佈 (Gamma)" = HTML("<ul><li>在特定時間段內，病人需要進行某種檢查（如X光）的總等待時間（如果每次檢查時間服從指數分佈）。</li><li>癌症患者接受治療後的存活時間。</li><li>模擬某區域在一定時間內的降雨量總和。</li><li>細胞分裂所需的時間。</li></ul>"),
      "t 分佈 (Student's t)" = HTML("<ul><li>當只有少量病人數據（例如 n < 30）時，估計新療法的平均療效改善程度的信賴區間。</li><li>比較兩組小樣本病人（例如，實驗組 vs. 對照組）平均血壓的差異是否顯著。</li><li>在未知總體變異數的情況下，檢定樣本平均值是否等於某個假設的總體平均值。</li></ul>"),
      "指數分佈 (Exponential)" = HTML("<ul><li>醫院急診室兩次病人到達之間的間隔時間。</li><li>放射性同位素衰變所需的時間。</li><li>某個醫療設備（如起搏器）無故障運行的持續時間（假設故障率恆定）。</li><li>等待第一個病人對某種罕見病治療產生反應的時間。</li></ul>"),
      "柯西分佈 (Cauchy)" = HTML("<ul><li>在生物醫學中直接應用較少，因為其缺乏平均值和變異數。</li><li>可能出現在某些物理模型（如共振現象）或作為對含有極端異常值數據進行穩健統計推斷時的一種模型假設（因其重尾特性對異常值不敏感）。</li><li>理論上，兩個獨立標準常態分佈隨機變數的比值服從標準柯西分佈。</li></ul>"),
      "貝他分佈 (Beta)" = HTML("<ul><li>某種治療方法的成功率（治癒比例）本身的不確定性分佈（常用於貝氏統計）。</li><li>基因在人群中的等位基因頻率分佈。</li><li>病人對疼痛程度的主觀評分（標準化到 0 到 1 之間）的分佈。</li><li>評估診斷測試的敏感性或特異性的分佈。</li></ul>"),
      "二項分佈 (Binomial)" = HTML("<ul><li>在 20 位接受新藥治療的患者中，出現副作用的人數。</li><li>隨機抽取 100 人，其中攜帶某特定基因變異的人數。</li><li>進行 5 次獨立的診斷測試，結果為陽性的次數（假設每次測試準確性相同且獨立）。</li><li>一個家庭有 4 個孩子，其中男孩的數量（假設生男生女機率相等且獨立）。</li></ul>"),
      "卜瓦松分佈 (Poisson)" = HTML("<ul><li>某醫院急診室在一小時內接收到的病患數量。</li><li>在一定長度的 DNA 序列中發現的突變數量。</li><li>每平方公分皮膚上細菌菌落的數量。</li><li>某地區在一年內發生某種罕見疾病的病例數。</li></ul>")
    )
    return(text)
  })

}

# --- 執行 App ---
shinyApp(ui = ui, server = server)

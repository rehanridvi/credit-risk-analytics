ui <- fluidPage(
  titlePanel("Credit Risk Analytics Dashboard"),
  tags$head(tags$style(HTML("
    .kpi-box { background:#f5f7fa; border-radius:8px; padding:14px;
               border-left:4px solid #2E86AB; }
    .kpi-box h4 { margin:0 0 4px 0; font-size:13px; color:#555; }
    .kpi-box .v { font-size:22px; font-weight:600; color:#111; }
  "))),

  tabsetPanel(

    
    tabPanel("Portfolio Overview",
      fluidRow(
        column(3, div(class = "kpi-box",
                      h4("Total Applicants"), textOutput("kpi_n"))),
        column(3, div(class = "kpi-box",
                      h4("Mean PD (blend)"),  textOutput("kpi_pd"))),
        column(3, div(class = "kpi-box",
                      h4("Mean Score"),       textOutput("kpi_score"))),
        column(3, div(class = "kpi-box",
                      h4("High-Risk Share"),  textOutput("kpi_highrisk")))
      ),
      br(),
      fluidRow(
        column(6, plotOutput("chart_band",    height = 320)),
        column(6, plotOutput("chart_pd_dist", height = 320))
      ),
      br(),
      h4("Full Business KPI Table"),
      DTOutput("kpi_table")
    ),

    
    tabPanel("Risk Analysis",
      fluidRow(
        column(6, plotOutput("chart_age",     height = 320)),
        column(6, plotOutput("chart_delinq",  height = 320))
      ),
      br(),
      fluidRow(
        column(6, plotOutput("chart_util",    height = 320)),
        column(6, plotOutput("chart_income",  height = 320))
      )
    ),

    
    tabPanel("Segment Explorer",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("seg_var", "Segment by",
                      choices = c("Age group"          = "age_group",
                                  "Delinquency bucket" = "delinq_bucket",
                                  "Utilization band"   = "util_band",
                                  "Risk band"          = "risk_band")),
          sliderInput("seg_pd", "Filter: PD range",
                      min = 0, max = 1, value = c(0, 1), step = 0.01)
        ),
        mainPanel(width = 9,
          DTOutput("segment_table")
        )
      )
    ),

    
    tabPanel("Credit Scoring",
      sidebarLayout(
        sidebarPanel(width = 4,
          h4("Applicant Features"),
          numericInput("s_age",          "Age (years)",                40, min = 18, max = 110),
          numericInput("s_income",       "Monthly Income ($)",         5000, min = 0),
          numericInput("s_dependents",   "Number of Dependents",       0,  min = 0),
          numericInput("s_debt_ratio",   "Debt Ratio (0–1)",           0.4, min = 0, max = 5, step = 0.01),
          numericInput("s_revol_util",   "Revolving Utilization (0–1)", 0.5, min = 0, max = 5, step = 0.01),
          numericInput("s_open_lines",   "Open Credit Lines",          8,  min = 0),
          numericInput("s_re_loans",     "Real Estate Loans",          1,  min = 0),
          numericInput("s_late30",       "30–59 Days Late",            0,  min = 0),
          numericInput("s_late60",       "60–89 Days Late",            0,  min = 0),
          numericInput("s_late90",       "90+ Days Late",              0,  min = 0),
          actionButton("s_score", "Score Applicant", class = "btn-primary")
        ),
        mainPanel(width = 8,
          h3("Scoring Result"),
          uiOutput("score_boxes"),
          br(),
          h4("Model Inputs After Feature Engineering"),
          DTOutput("score_features")
        )
      )
    ),

    
    tabPanel("Model Performance",
      h3("Held-Out Test Set Metrics"),
      DTOutput("metrics_table"),
      br(),
      h4("ROC Curve — XGBoost (best model)"),
      plotOutput("chart_roc", height = 420)
    )
  )
)
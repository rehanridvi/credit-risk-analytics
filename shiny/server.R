# shiny/server.R

server <- function(input, output, session) {

  #
  # Portfolio Overview
  #
  output$kpi_n <- renderText({
    format(nrow(scored_full), big.mark = ",")
  })
  output$kpi_pd <- renderText({
    sprintf("%.2f%%", 100 * mean(scored_full$pd_blend))
  })
  output$kpi_score <- renderText({
    sprintf("%.0f", mean(scored_full$score))
  })
  output$kpi_highrisk <- renderText({
    pct <- mean(scored_full$risk_band %in% c("High Risk", "Very High Risk"))
    sprintf("%.1f%%", 100 * pct)
  })

  output$chart_band <- renderPlot({
    scored_full %>%
      count(risk_band) %>%
      mutate(pct = 100 * n / sum(n)) %>%
      ggplot(aes(risk_band, pct, fill = risk_band)) +
      geom_col(alpha = 0.9) +
      geom_text(aes(label = sprintf("%.1f%%", pct)),
                vjust = -0.4, size = 4) +
      scale_fill_manual(values = c("Low Risk"        = "#2E86AB",
                                   "Medium Risk"     = "#F5B041",
                                   "High Risk"       = "#E67E22",
                                   "Very High Risk"  = "#B22222")) +
      labs(title = "Applicants by Risk Band",
           x = NULL, y = "% of portfolio") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  output$chart_pd_dist <- renderPlot({
    ggplot(scored_full, aes(pd_blend)) +
      geom_histogram(bins = 50, fill = "#2E86AB", color = "white") +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(title = "Distribution of Predicted PD",
           x = "Predicted Probability of Default", y = "Count") +
      theme_minimal(base_size = 13)
  })

  output$kpi_table <- renderDT({
    datatable(kpis, options = list(pageLength = 15, dom = "tp"),
              rownames = FALSE)
  })

  #
  # Risk Analysis — default-proxy views using PD, since labels are all-NA
  #
  # Since `defaulted` isn't populated in cs-test, we proxy "risk concentration"
  # by mean PD per segment. This is the standard approach for unlabeled books.

  seg_summary <- function(var, label) {
    scored_full %>%
      group_by(.data[[var]]) %>%
      summarise(n         = n(),
                mean_pd   = mean(pd_blend),
                .groups   = "drop") %>%
      mutate(mean_pd_pct = 100 * mean_pd) %>%
      ggplot(aes(x = .data[[var]], y = mean_pd_pct, fill = .data[[var]])) +
      geom_col(alpha = 0.9) +
      geom_text(aes(label = sprintf("%.2f%%", mean_pd_pct)),
                vjust = -0.4, size = 3.6) +
      labs(title = paste("Mean PD by", label),
           x = NULL, y = "Mean PD (%)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  }

  output$chart_age    <- renderPlot(seg_summary("age_group",    "Age Group"))
  output$chart_delinq <- renderPlot(seg_summary("delinq_bucket", "Delinquency Bucket"))
  output$chart_util   <- renderPlot(seg_summary("util_band",    "Utilization Band"))

  output$chart_income <- renderPlot({
    scored_full %>%
      mutate(income_q = ntile(monthly_income, 4),
             income_q = factor(income_q, labels = c("Q1 (low)", "Q2", "Q3", "Q4 (high)"))) %>%
      group_by(income_q) %>%
      summarise(mean_pd_pct = 100 * mean(pd_blend), .groups = "drop") %>%
      ggplot(aes(income_q, mean_pd_pct, fill = income_q)) +
      geom_col(alpha = 0.9) +
      geom_text(aes(label = sprintf("%.2f%%", mean_pd_pct)),
                vjust = -0.4, size = 3.6) +
      labs(title = "Mean PD by Income Quartile",
           x = NULL, y = "Mean PD (%)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  #
  # Segment Explorer
  #
  output$segment_table <- renderDT({
    req(input$seg_var)
    scored_full %>%
      filter(pd_blend >= input$seg_pd[1],
             pd_blend <= input$seg_pd[2]) %>%
      group_by(Segment = .data[[input$seg_var]]) %>%
      summarise(
        Applicants   = n(),
        Mean_PD      = round(100 * mean(pd_blend), 2),
        Median_Score = round(median(score)),
        Mean_Income  = round(mean(monthly_income)),
        .groups      = "drop"
      ) %>%
      datatable(options = list(pageLength = 15, dom = "tp"), rownames = FALSE)
  })

  #
  # Credit Scoring (real model — XGBoost full-refit)
  #
  observeEvent(input$s_score, {

    applicant <- data.frame(
      revolving_util    = input$s_revol_util,
      age               = input$s_age,
      late_30_59        = input$s_late30,
      debt_ratio        = input$s_debt_ratio,
      monthly_income    = input$s_income,
      open_lines        = input$s_open_lines,
      late_90           = input$s_late90,
      real_estate_loans = input$s_re_loans,
      late_60_89        = input$s_late60,
      dependents        = input$s_dependents,
      stringsAsFactors  = FALSE
    )

    applicant <- engineer_features(applicant)

    # Align to training schema
    for (f in schema$features) {
      if (!f %in% names(applicant)) applicant[[f]] <- 0
    }
    applicant_X <- applicant[, schema$features, drop = FALSE] %>%
      mutate(across(everything(), as.numeric))

    # Score with all three full models
    pd_log <- get_proba(models$full$log, applicant_X)
    pd_rf  <- get_proba(models$full$rf,  applicant_X)
    pd_xgb <- get_proba(models$full$xgb, applicant_X)
    pd_bl  <- mean(c(pd_log, pd_rf, pd_xgb))

    score_val <- round(300 + (1 - pd_bl) * 600)
    band <- dplyr::case_when(
      score_val >= quantile(scored_full$score, 0.50) ~ "Low Risk",
      score_val >= quantile(scored_full$score, 0.20) ~ "Medium Risk",
      score_val >= quantile(scored_full$score, 0.10) ~ "High Risk",
      TRUE                                           ~ "Very High Risk"
    )

    output$score_boxes <- renderUI({
      color <- switch(band,
        "Low Risk"        = "#2E86AB",
        "Medium Risk"     = "#F5B041",
        "High Risk"       = "#E67E22",
        "Very High Risk"  = "#B22222")
      tagList(
        fluidRow(
          column(4, div(class = "kpi-box",
                        h4("Blended PD"),  tags$div(class = "v",
                        sprintf("%.2f%%", 100 * pd_bl)))),
          column(4, div(class = "kpi-box",
                        h4("Credit Score"), tags$div(class = "v",
                        sprintf("%d", score_val)))),
          column(4, div(class = "kpi-box",
                        h4("Risk Band"),   tags$div(class = "v",
                        style = paste0("color:", color, ";"), band)))
        ),
        br(),
        h4("Per-Model PD"),
        tableOutput("score_model_table")
      )
    })

    output$score_model_table <- renderTable({
      data.frame(
        Model        = c("Logistic", "Random Forest", "XGBoost", "Blend (mean)"),
        PD_percent   = round(100 * c(pd_log, pd_rf, pd_xgb, pd_bl), 3)
      )
    })

    output$score_features <- renderDT({
      datatable(round(applicant_X, 4),
                options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
    })
  })

  #
  # Model Performance
  #
  output$metrics_table <- renderDT({
    datatable(metrics, options = list(dom = "t"), rownames = FALSE)
  })

  output$chart_roc <- renderPlot({
    y_true  <- as.integer(test$defaulted == "Yes")
    preds_x <- get_proba(models$tuned$xgb, test)
    r <- pROC::roc(y_true, preds_x, quiet = TRUE, direction = "<")
    df_roc <- data.frame(
      fpr = 1 - r$specificities,
      tpr = r$sensitivities
    )
    auc_val <- as.numeric(pROC::auc(r))
    ggplot(df_roc, aes(fpr, tpr)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                  color = "grey60") +
      geom_line(color = "#F18F01", linewidth = 1.2) +
      labs(title = sprintf("ROC Curve — XGBoost (AUC = %.4f)", auc_val),
           x = "False Positive Rate", y = "True Positive Rate") +
      theme_minimal(base_size = 13)
  })
}
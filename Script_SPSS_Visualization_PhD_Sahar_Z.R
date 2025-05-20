# Set the working directory
setwd("C:")

library(haven)
df <- read_sav("Augmented Reality and Virtual Reality in Architectural Practice_March 4_ 2025_13.38.sav")

# Get rid of special characters

names(df) <- gsub(" ", "_", trimws(names(df)))
names(df) <- gsub("\\s+", "_", trimws(names(df), whitespace = "[\\h\\v\\s]+"))
names(df) <- gsub("\\(", "_", names(df))
names(df) <- gsub("\\)", "_", names(df))
names(df) <- gsub("\\-", "_", names(df))
names(df) <- gsub("/", "_", names(df))
names(df) <- gsub("\\\\", "_", names(df)) 
names(df) <- gsub("\\?", "", names(df))
names(df) <- gsub("\\'", "", names(df))
names(df) <- gsub("\\,", "_", names(df))
names(df) <- gsub("\\$", "", names(df))
names(df) <- gsub("\\+", "_", names(df))

# Trim all values

# Loop over each column in the dataframe
df <- data.frame(lapply(df, function(x) {
  if (is.character(x)) {
    # Apply trimws to each element in the column if it is character
    x <- trimws(x)
  }
  return(x)
}))

# Trim all values and convert to lowercase

# Loop over each column in the dataframe
df <- data.frame(lapply(df, function(x) {
  if (is.character(x)) {
    # Apply trimws to each element in the column if it is character
    x <- trimws(x)
    # Convert character strings to lowercase
    x <- tolower(x)
  }
  return(x)
}))

colnames(df)


df$QID49 <- ifelse(df$QID49 == 6, NA, df$QID49)

# AR/VR Adoption Level
iv_usage <- "QID49"

# Firm Size
iv_firmsize <- "Q7"

# Professional Experience
iv_experience <- "Q6"

# H1: AR/VR adoption → collaboration & decision-making
dvs_H1 <- c("Q51", "Q54")

# H2: usar o escore composto
dvs_H2 <- "SelfEfficacy_ARVR"

# H3: usar o escore composto
dvs_H3 <- "WorkflowEfficiency"

# H4: usar o escore composto
dvs_H4 <- "DecisionMakingConfidence"


ivs_final <- c(iv_usage, iv_firmsize, iv_experience, "ProficiencyScore")


# Scale Construction

library(psych)
library(dplyr)

reliability_analysis <- function(data, scales) {
  # Initialize a dataframe to store the results
  results <- data.frame(
    Variable = character(),
    Mean = numeric(),
    SEM = numeric(),
    StDev = numeric(),
    ITC = numeric(),  # Added for item-total correlation
    Alpha = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process each scale
  for (scale in names(scales)) {
    subset_data <- data[scales[[scale]]] %>%
      mutate(across(everything(), ~ as.numeric(.)))  # <---- ajuste aqui
    
    alpha_results <- alpha(subset_data)
    alpha_val <- alpha_results$total$raw_alpha

    # Calculate statistics for each item in the scale
    for (item in scales[[scale]]) {
      item_data <- data[[item]]
      item_itc <- alpha_results$item.stats[item, "raw.r"] # Get ITC for the item
      item_mean <- mean(item_data, na.rm = TRUE)
      item_sem <- sd(item_data, na.rm = TRUE) / sqrt(sum(!is.na(item_data)))
      
      results <- rbind(results, data.frame(
        Variable = item,
        Mean = item_mean,
        SEM = item_sem,
        StDev = sd(item_data, na.rm = TRUE),
        ITC = item_itc,  # Include the ITC value
        Alpha = NA
      ))
    }
    
    # Calculate the mean score for the scale and add as a new column
    scale_mean <- rowMeans(subset_data, na.rm = TRUE)
    data[[scale]] <- scale_mean
    scale_mean_overall <- mean(scale_mean, na.rm = TRUE)
    scale_sem <- sd(scale_mean, na.rm = TRUE) / sqrt(sum(!is.na(scale_mean)))
    
    # Append scale statistics
    results <- rbind(results, data.frame(
      Variable = scale,
      Mean = scale_mean_overall,
      SEM = scale_sem,
      StDev = sd(scale_mean, na.rm = TRUE),
      ITC = NA,  # ITC is not applicable for the total scale
      Alpha = alpha_val
    ))
  }
  
  return(list(data_with_scales = data, statistics = results))
}

scales_arvr <- list(

  
  # H1 – Individual work (avaliar manter essa escala depois)
  IndividualWork_ARVR = c("QID67_1", "QID67_2", "QID67_3", "QID67_4", "QID67_5", "QID67_6"),
  
  # H2 – Self-efficacy (ajustada para refletir os novos DVs mencionados)
  SelfEfficacy_ARVR = c("QID57", "QID58", "QID59", "QID61", "QID62"),
  
  # H3 – Workflow efficiency
  WorkflowEfficiency = c("Q52", "QID60", "Q54"),
  
  # H4 – Confidence in complex challenges
  DecisionMakingConfidence = c("Q53", "QID61", "QID62")
)




alpha_results <- reliability_analysis(df, scales_arvr)

df <- alpha_results$data_with_scales
df_descriptives <- alpha_results$statistics




# Outlier Evaluation

library(dplyr)

calculate_z_scores <- function(data, vars, id_var, z_threshold = 3) {
  # Prepare data by handling NA values and calculating Z-scores
  z_score_data <- data %>%
    mutate(across(all_of(vars), ~ as.numeric(.))) %>%
    mutate(across(all_of(vars), ~ replace(., is.na(.), mean(., na.rm = TRUE)))) %>%
    mutate(across(all_of(vars), ~ (.-mean(., na.rm = TRUE))/ifelse(sd(.) == 0, 1, sd(., na.rm = TRUE)), .names = "z_{.col}"))
  
  # Include original raw scores and Z-scores
  z_score_data <- z_score_data %>%
    select(!!sym(id_var), all_of(vars), starts_with("z_"))
  
  # Add a column to flag outliers based on the specified z-score threshold
  z_score_data <- z_score_data %>%
    mutate(across(starts_with("z_"), ~ ifelse(abs(.) > z_threshold, "Outlier", "Normal"), .names = "flag_{.col}"))
  
  return(z_score_data)
}

if (!"ID" %in% colnames(df)) {
  df$ID <- seq_len(nrow(df))
}


vars_outlier_check <- c(names(scales_arvr), ivs_final)

df_outliers <- calculate_z_scores(df, vars_outlier_check, id_var = "ID")

library(dplyr)
library(stringr)

transform_outliers_to_na <- function(data, vars, id_var, z_threshold = 3.5) {
  # Function to calculate Adjusted Z-Score
  calculate_adjusted_z <- function(x) {
    if (all(is.na(x))) return(rep(NA, length(x)))  # Handle case where column is all NA
    mad_x <- mad(x, na.rm = TRUE)
    if (mad_x == 0) mad_x <- 1  # Prevent division by zero if MAD is 0
    return(0.6745 * (x - median(x, na.rm = TRUE)) / mad_x)
  }
  
  # Compute Adjusted Z-Scores for each variable
  adjusted_z_data <- data %>%
    mutate(across(all_of(vars), ~ calculate_adjusted_z(.), .names = "z_adj_{.col}"))
  
  # Replace outliers with NA
  transformed_data <- adjusted_z_data %>%
    mutate(across(all_of(vars), ~ ifelse(abs(get(paste0("z_adj_", cur_column()))) > z_threshold, NA, .)))
  
  # Drop the Adjusted Z-Score columns
  transformed_data <- transformed_data %>%
    select(-starts_with("z_adj_"))
  
  return(transformed_data)
}

# Aplicar a função para substituir outliers por NA
df_cleaned <- transform_outliers_to_na(df, vars_outlier_check, id_var = "ID", z_threshold = 3.5)

# Substituir os dados originais pelas versões limpas
df[vars_outlier_check] <- df_cleaned[vars_outlier_check]



## CORRELATION ANALYSIS

calculate_correlation_matrix <- function(data, variables, method = "pearson") {
  # Ensure the method is valid
  if (!method %in% c("pearson", "spearman")) {
    stop("Method must be either 'pearson' or 'spearman'")
  }
  
  # Subset the data for the specified variables
  data_subset <- data[variables]
  
  # Calculate the correlation matrix
  corr_matrix <- cor(data_subset, method = method, use = "complete.obs")
  
  # Initialize a matrix to hold formatted correlation values
  corr_matrix_formatted <- matrix(nrow = nrow(corr_matrix), ncol = ncol(corr_matrix))
  rownames(corr_matrix_formatted) <- colnames(corr_matrix)
  colnames(corr_matrix_formatted) <- colnames(corr_matrix)
  
  # Calculate p-values and apply formatting with stars
  for (i in 1:nrow(corr_matrix)) {
    for (j in 1:ncol(corr_matrix)) {
      if (i == j) {  # Diagonal elements (correlation with itself)
        corr_matrix_formatted[i, j] <- format(round(corr_matrix[i, j], 3), nsmall = 3)
      } else {
        p_value <- cor.test(data_subset[[i]], data_subset[[j]], method = method)$p.value
        stars <- ifelse(p_value < 0.01, "***", 
                        ifelse(p_value < 0.05, "**", 
                               ifelse(p_value < 0.1, "*", "")))
        corr_matrix_formatted[i, j] <- paste0(format(round(corr_matrix[i, j], 3), nsmall = 3), stars)
      }
    }
  }
  
  return(corr_matrix_formatted)
}


# --- Correlation Matrix (ivs + dvs juntos) ---
correlation_vars <- unique(c(ivs_final, dvs_H1, dvs_H2, dvs_H3, dvs_H4))
correlation_matrix <- calculate_correlation_matrix(df, correlation_vars, method = "pearson")


# OLS Regression
library(dplyr)
library(broom)
library(car)
library(ggplot2)
library(ggfortify)

fit_ols_and_format <- function(data, predictors, response_vars, save_plots = TRUE, plot_dir = "ols_plots") {
  ols_results_list <- list()
  
  # Criar diretório de saída caso não exista
  if (save_plots && !dir.exists(plot_dir)) {
    dir.create(plot_dir)
  }
  
  for (response_var in response_vars) {
    cat("\n===========================\n")
    cat(paste("Fitting model for:", response_var, "\n"))
    
    # Construir fórmula
    formula <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + ")))
    lm_model <- lm(formula, data = data, na.action = na.omit)
    assign("lm_model", lm_model, envir = .GlobalEnv)
    
    # Verificar colinearidade
    alias_info <- alias(lm_model)$Complete
    if (!is.null(alias_info)) {
      print("Warning: The following predictors are perfectly collinear and will be removed:")
      print(colnames(alias_info))
      
      # Refazendo modelo sem os colineares
      predictors <- setdiff(predictors, colnames(alias_info))
      formula <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + ")))
      lm_model <- lm(formula, data = data, na.action = na.omit)
      assign("lm_model", lm_model, envir = .GlobalEnv)
    }
    
    # Resumo
    model_summary <- summary(lm_model)
    print(model_summary)
    
    r_squared <- model_summary$r.squared
    f_statistic <- model_summary$fstatistic[1]
    p_value <- pf(f_statistic, model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
    
    # VIF
    if (length(predictors) > 1) {
      vif_values <- car::vif(lm_model)
      vif_df <- data.frame(Variable = names(vif_values), VIF = vif_values)
    } else {
      vif_df <- data.frame(Variable = predictors, VIF = NA)
    }
    
    # Resultados tidy
    ols_results <- broom::tidy(lm_model) %>%
      mutate(ResponseVariable = response_var, R_Squared = r_squared, F_Statistic = f_statistic, P_Value = p_value) %>%
      left_join(vif_df, by = c("term" = "Variable"))
    
    print(ols_results)
    
    
    
    # Plots
    if (save_plots) {
      if (nobs(lm_model) > 0) {
        plots <- autoplot(lm_model, label.size = 3)
        for (i in seq_along(plots)) {
          plot_path <- file.path(plot_dir, paste0("diagnostic_", response_var, "_plot", i, ".png"))
          ggsave(plot_path, plot = plots[[i]], width = 8, height = 6, dpi = 300)
          cat("Saved:", plot_path, "\n")
        }
      } else {
        cat("No observations for model:", response_var, "- plot skipped.\n")
      }
    }
    
    
    
    ols_results_list[[response_var]] <- ols_results
  }
  
  return(ols_results_list)
}


# Combine the results into a single dataframe
# --- OLS regressions ---
ols_H1 <- fit_ols_and_format(df, ivs_final, dvs_H1)
ols_H2 <- fit_ols_and_format(df, ivs_final, dvs_H2)
ols_H3 <- fit_ols_and_format(df, ivs_final, dvs_H3)
ols_H4 <- fit_ols_and_format(df, ivs_final, dvs_H4)

# --- Consolidar resultados ---
df_modelresults_all <- bind_rows(
  bind_rows(ols_H1),
  bind_rows(ols_H2),
  bind_rows(ols_H3),
  bind_rows(ols_H4)
)



df$QID49 <- as.factor(df$QID49)  # AR/VR usage
df$Q6    <- as.factor(df$Q6)     # Experience
df$Q7    <- as.factor(df$Q7)     # Firm size


# Levenes Test

perform_levenes_test <- function(data, variables, factors) {
  # Load necessary library
  if (!require(car)) install.packages("car")
  library(car)
  
  # Initialize an empty dataframe to store results
  levene_results <- data.frame(Variable = character(), 
                               Factor = character(),
                               F_Value = numeric(), 
                               DF1 = numeric(),
                               DF2 = numeric(),
                               P_Value = numeric(),
                               stringsAsFactors = FALSE)
  
  # Perform Levene's test for each variable and factor
  for (var in variables) {
    for (factor in factors) {
      # Perform Levene's Test
      test_result <- leveneTest(reformulate(factor, response = var), data = data)
      
      # Extract the F value, DF, and p-value
      F_value <- as.numeric(test_result[1, "F value"])
      DF1 <- as.numeric(test_result[1, "Df"])
      DF2 <- as.numeric(test_result[2, "Df"])
      P_value <- as.numeric(test_result[1, "Pr(>F)"])
      
      # Append the results to the dataframe
      levene_results <- rbind(levene_results, 
                              data.frame(Variable = var, 
                                         Factor = factor,
                                         F_Value = F_value, 
                                         DF1 = DF1,
                                         DF2 = DF2,
                                         P_Value = P_value))
    }
  }
  
  return(levene_results)
}

# Todas as DVs compostas + DVs H1
response_vars <- c("Q51", "Q54", "SelfEfficacy_ARVR", "WorkflowEfficiency", "DecisionMakingConfidence")
factors <- c("QID49", "Q6", "Q7")  # Uso, experiência, tamanho da firma

levene_results <- perform_levenes_test(df, response_vars, factors)
print(levene_results)


### ONE WAY

perform_kruskal_test <- function(data, response_vars, factors) {
  results <- data.frame()
  
  for (dv in response_vars) {
    for (iv in factors) {
      # Garantir que a variável fator esteja como fator
      data[[iv]] <- as.factor(data[[iv]])
      
      # Kruskal-Wallis test
      kruskal_result <- kruskal.test(reformulate(iv, response = dv), data = data)
      
      # Estatísticas descritivas por grupo
      descriptives <- data %>%
        group_by(across(all_of(iv))) %>%
        summarise(
          Mean = round(mean(get(dv), na.rm = TRUE), 3),
          Median = round(median(get(dv), na.rm = TRUE), 3),
          SD = round(sd(get(dv), na.rm = TRUE), 3),
          SEM = round(sd(get(dv), na.rm = TRUE) / sqrt(sum(!is.na(get(dv)))), 3),
          .groups = "drop"
        )
      
      # Combina as descrições com os resultados do teste
      for (i in 1:nrow(descriptives)) {
        results <- rbind(results, data.frame(
          DV = dv,
          IV = iv,
          Group = as.character(descriptives[[iv]][i]),
          Mean = descriptives$Mean[i],
          Median = descriptives$Median[i],
          SD = descriptives$SD[i],
          SEM = descriptives$SEM[i],
          Statistic = round(kruskal_result$statistic, 3),
          Df = kruskal_result$parameter,
          P_Value = round(kruskal_result$p.value, 4)
        ))
      }
    }
  }
  
  return(results)
}


# Aplicar aos dados
kruskal_results <- perform_kruskal_test(df, response_vars, factors)
print(kruskal_results)


library(ggplot2)
library(dplyr)


label_dict <- c(
  Q6 = "Professional Experience",
  Q7 = "Firm Size",
  QID49 = "AR/VR Adoption Level",
  Q51 = "Collaboration (Team)",
  Q54 = "Decision-Making (Design)",
  SelfEfficacy_ARVR = "Self-Efficacy with AR/VR",
  WorkflowEfficiency = "Workflow Efficiency",
  DecisionMakingConfidence = "Confidence in Complex Challenges"
)



plot_group_means_with_sem <- function(data, response_vars, factors, output_dir = "plots_group_means") {
  if (!dir.exists(output_dir)) dir.create(output_dir)
  
  for (dv in response_vars) {
    for (iv in factors) {
      # Garantir que a IV seja fator
      data[[iv]] <- as.factor(data[[iv]])
      
      # Calcular média e SEM
      summary_df <- data %>%
        group_by(across(all_of(iv))) %>%
        summarise(
          Mean = mean(get(dv), na.rm = TRUE),
          SEM = sd(get(dv), na.rm = TRUE) / sqrt(sum(!is.na(get(dv)))),
          .groups = "drop"
        )
      
      # Renomear para facilitar
      colnames(summary_df)[1] <- "Group"
      
      # Criar o gráfico
      plot <- ggplot(summary_df, aes(x = Group, y = Mean)) +
        geom_point(size = 3) +
        geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.15, linewidth = 0.8) +
        theme_minimal(base_size = 14) +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "black"),
          plot.title = element_text(hjust = 0.5)
        ) +
        labs(
          title = paste("Mean ± SEM for", label_dict[[dv]], "by", label_dict[[iv]]),
          x = label_dict[[iv]],
          y = label_dict[[dv]]
        )
      
      # Salvar
      file_name <- paste0(output_dir, "/", gsub("[^a-zA-Z0-9_]", "_", paste0("plot_", dv, "_by_", iv)), ".png")
      ggsave(file_name, plot, width = 8, height = 6, dpi = 300)
      print(plot)
    }
  }
}


response_vars <- c("Q51", "Q54", "SelfEfficacy_ARVR", "WorkflowEfficiency", "DecisionMakingConfidence")
factors <- c("QID49", "Q6", "Q7")

plot_group_means_with_sem(df, response_vars, factors)


## Export Results

library(openxlsx)

save_apa_formatted_excel <- function(data_list, filename) {
  wb <- createWorkbook()  # Create a new workbook
  
  for (i in seq_along(data_list)) {
    # Define the sheet name
    sheet_name <- names(data_list)[i]
    if (is.null(sheet_name)) sheet_name <- paste("Sheet", i)
    addWorksheet(wb, sheet_name)  # Add a sheet to the workbook
    
    # Convert matrices to data frames, if necessary
    data_to_write <- if (is.matrix(data_list[[i]])) as.data.frame(data_list[[i]]) else data_list[[i]]
    
    # Include row names as a separate column, if they exist
    if (!is.null(row.names(data_to_write))) {
      data_to_write <- cbind("Index" = row.names(data_to_write), data_to_write)
    }
    
    # Write the data to the sheet
    writeData(wb, sheet_name, data_to_write)
    
    # Define styles
    header_style <- createStyle(textDecoration = "bold", border = "TopBottom", borderColour = "black", borderStyle = "thin")
    bottom_border_style <- createStyle(border = "bottom", borderColour = "black", borderStyle = "thin")
    
    # Apply styles
    addStyle(wb, sheet_name, style = header_style, rows = 1, cols = 1:ncol(data_to_write), gridExpand = TRUE)
    addStyle(wb, sheet_name, style = bottom_border_style, rows = nrow(data_to_write) + 1, cols = 1:ncol(data_to_write), gridExpand = TRUE)
    
    # Set column widths to auto
    setColWidths(wb, sheet_name, cols = 1:ncol(data_to_write), widths = "auto")
  }
  
  # Save the workbook
  saveWorkbook(wb, filename, overwrite = TRUE)
}

data_list <- list(
  "Data" = df,
  "Outlier Check" = df_outliers,
  "Descriptive Stats (Scales)" = df_descriptives,
  "Correlation" = correlation_matrix,
  "Levene's Test" = levene_results,
  "Kruskal-Wallis Test" = kruskal_results,
  "OLS Regression Results" = df_modelresults_all
)

save_apa_formatted_excel(data_list, "ARVR_Results_APA.xlsx")

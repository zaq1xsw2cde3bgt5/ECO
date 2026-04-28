
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(purrr)
library(openxlsx)
library(scales)


data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)


data <- data %>%
  filter(Set != "ITT.1") %>%
  filter(Set != "ITT.2") %>%
  filter(Set != "ITT.3") %>%
  filter(Set != "ITT.4") %>%
  filter(Set != "ITT.5")

data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)


observation_cols <- c("Overall.Stage.No.nausea",
                      "Acute.Stage.No.nausea",
                      "Delayed.Stage.No.nausea")

long_data <- data %>%
  pivot_longer(
    cols = observation_cols,
    names_to = "Observation",
    values_to = "Effectiveness"
  ) %>%
  filter(!is.na(Effectiveness))


group_summaries <- long_data %>%
  group_by(Group, Observation) %>%
  summarise(
    n       = n(),
    success = sum(Effectiveness == 0),   
    ControlRate = mean(Effectiveness == 0),
    .groups = 'drop'
  ) %>%
  rowwise() %>%
  mutate(ci = list(binom.test(success, n)$conf.int)) %>%
  mutate(LCL = ci[[1]], UCL = ci[[2]]) %>%
  ungroup() %>%
  select(-ci)


chisq_results <- long_data %>%
  group_by(Observation) %>%
  summarise(test = list(chisq.test(table(Group, Effectiveness))),
            .groups = "drop") %>%
  mutate(chisq_p = map_dbl(test, ~ .x$p.value)) %>%
  select(Observation, chisq_p) %>%
  mutate(
    p_label = ifelse(
      chisq_p < 1e-4,
      "< 0.0001",
      paste0("= ", formatC(chisq_p, format = "f", digits = 4))
    )
  )

group_summaries <- group_summaries %>%
  left_join(chisq_results, by = "Observation")

group_summaries$Observation <- factor(group_summaries$Observation,
                                      levels = observation_cols)
chisq_results$Observation <- factor(chisq_results$Observation,
                                    levels = observation_cols)


overall_data <- group_summaries %>% filter(Observation == "Overall.Stage.No.nausea")

p1 <- overall_data$ControlRate[overall_data$Group == "True electroacupuncture"]
n1 <- overall_data$n[overall_data$Group == "True electroacupuncture"]

p0 <- overall_data$ControlRate[overall_data$Group == "Sham electroacupuncture"]
n0 <- overall_data$n[overall_data$Group == "Sham electroacupuncture"]

RD <- p1 - p0
SE <- sqrt(p1*(1-p1)/n1 + p0*(1-p0)/n0)
CI_low <- RD - 1.96*SE
CI_high <- RD + 1.96*SE

RD_label <- sprintf("Risk difference\n%.1f (95%% CI, %.1f – %.1f)",
                    RD*100, CI_low*100, CI_high*100)


y_true <- p1
y_sham <- p0
y_top  <- max(y_true, y_sham) + 0.15  


x_left  <- 0.85
x_right <- 1.15


final_plot <- ggplot(group_summaries,
                     aes(x = Observation, y = ControlRate*100, fill = Group)) +  
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  
  
  geom_errorbar(
    aes(ymin = LCL*100, ymax = UCL*100),
    position = position_dodge(width = 0.7),
    width = 0.2,
    linewidth = 0.7,
    color = "black"
  ) +
  
  
  geom_text(
    aes(label = paste0(success, "/", n, " (", sprintf("%.1f", ControlRate * 100), ")"),
        y = UCL*100 + 3),  
    position = position_dodge(width = 0.7),
    size = 4, vjust = 0, color = "black"
  ) +
  
  
  geom_text(
    data = chisq_results,
    aes(x = Observation, y = 88, label = paste0("P ", p_label)),
    inherit.aes = FALSE,
    size = 5, fontface = "italic", color = "black"
  ) +
  
  
  
  annotate("text", x = 1, y = y_top*100 + 5, label = RD_label,
           size = 5, color = "black", hjust = 0.5) +
  
  scale_fill_manual(values = c("#D73027", "#4575B4")) +
  labs(
    x = "",
    y = "Control Rate (%)",
    fill = " "
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "sans"),
    legend.position  = c(0.5, 0.95),
    legend.direction = "horizontal",
    legend.text      = element_text(size = 14),
    legend.title     = element_text(size = 14),
    axis.text.x      = element_text(size = 14),
    axis.text.y      = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.margin      = margin(5, 5, 5, 5)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    labels = c("Overall.Stage.No.nausea" = "Overall stage no nausea",
               "Acute.Stage.No.nausea"   = "Acute stage no nausea",
               "Delayed.Stage.No.nausea" = "Delayed stage no nausea")
  )

print(final_plot)

pdf_out <- "/results/Figure 1.pdf"
ggsave(filename = pdf_out, plot = final_plot, width = 12, height = 8, units = "in")







######## Figure 2a

library(dplyr)
library(tidyr)

data <- read.csv("/data/ECO Nature data.csv") %>%
  filter(Set != "ITT.1", Set != "ITT.2",Set != "ITT.3",
         Set != "ITT.4",Set != "ITT.5") %>%
  
  
  mutate(
    
    Outcome = ifelse(Overall.Stage.No.nausea == 0, 1, 0),
    Group = factor(Group, levels = c(1, 0),
                   labels = c("True electroacupuncture", "Sham electroacupuncture")),
    
    Chemo2 = ifelse(Chemotherapy.regimen == "EC.based", "EC.based", "Carboplatin.Cisplatin.based")
  )



data$BMI <- cut(data$BMI,
                breaks = c(-Inf, 18.5, 24.9, Inf),
                labels = c("<18.5", "18.5-24.9", "≥25"),
                right = TRUE)


subgroup_analysis <- function(dat, var){
  dat <- dat %>% filter(!is.na(.data[[var]]))
  levels_vec <- unique(dat[[var]])
  res <- vector("list", length(levels_vec))
  
  for (i in seq_along(levels_vec)) {
    lvl <- levels_vec[i]
    subdat <- dat %>% filter(.data[[var]] == lvl)
    
    a <- sum(subdat$Group=="True electroacupuncture" & subdat$Outcome==1)
    b <- sum(subdat$Group=="True electroacupuncture" & subdat$Outcome==0)
    c <- sum(subdat$Group=="Sham electroacupuncture" & subdat$Outcome==1)
    d <- sum(subdat$Group=="Sham electroacupuncture" & subdat$Outcome==0)
    
    n_true <- a + b
    n_sham <- c + d
    rate_true <- if (n_true > 0) a / n_true else NA_real_
    rate_sham <- if (n_sham > 0) c / n_sham else NA_real_
    
    
    ci_true <- if (n_true > 0) binom.test(a, n_true)$conf.int else c(NA, NA)
    ci_sham <- if (n_sham > 0) binom.test(c, n_sham)$conf.int else c(NA, NA)
    
    OR <- LCL <- UCL <- NA_real_
    Pval <- NA_real_
    if (n_true > 0 && n_sham > 0) {
      tab <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE,
                    dimnames = list(c("True","Sham"), c("NoNausea(1)","Nausea(0)")))
      ft <- tryCatch(fisher.test(tab), error = function(e) NULL)
      if (!is.null(ft)) {
        OR   <- suppressWarnings(as.numeric(ft$estimate))
        LCL  <- suppressWarnings(unname(ft$conf.int[1]))
        UCL  <- suppressWarnings(unname(ft$conf.int[2]))
        Pval <- suppressWarnings(unname(ft$p.value))
      }
    }
    
    res[[i]] <- data.frame(
      Subgroup  = var,
      Level     = as.character(lvl),
      n_true    = n_true,
      rate_true = rate_true,
      true_LCL  = ci_true[1],
      true_UCL  = ci_true[2],
      n_sham    = n_sham,
      rate_sham = rate_sham,
      sham_LCL  = ci_sham[1],
      sham_UCL  = ci_sham[2],
      OR = OR, LCL = LCL, UCL = UCL, P = Pval,
      stringsAsFactors = FALSE
    )
  }
  
  dplyr::bind_rows(res)
}

vars <- c("BMI","Menstrual.status","Treatment.setting", "Chemo2", "NK1.RA", "HT3RA5.HT3RA")

results_list <- lapply(vars, function(v) subgroup_analysis(data, v))
final_df <- bind_rows(results_list)

final_df_out <- final_df %>%
  mutate(
    `True n` = n_true,
    `True % (95% CI)` = ifelse(is.na(rate_true), "NA",
                               sprintf("%.1f%% (%.1f–%.1f)", rate_true*100, true_LCL*100, true_UCL*100)),
    `Sham n` = n_sham,
    `Sham % (95% CI)` = ifelse(is.na(rate_sham), "NA",
                               sprintf("%.1f%% (%.1f–%.1f)", rate_sham*100, sham_LCL*100, sham_UCL*100)),
    `OR (95% CI)` = ifelse(is.na(OR), "NA",
                           sprintf("%.2f (%.2f–%.2f)", OR, LCL, UCL)),
    `P value` = ifelse(is.na(P), "NA",
                       ifelse(P < 0.001, "<0.001", sprintf("%.3f", P)))
  ) %>%
  select(Subgroup, Level, `True n`, `True % (95% CI)`, 
         `Sham n`, `Sham % (95% CI)`, `OR (95% CI)`, `P value`)

final_df_out <- final_df_out %>%
  
  filter(!Level %in% c("LZDX1", "LZDX2"))

write.csv(final_df_out,
          "/results/Figure 2.csv",
          row.names = FALSE)

print(final_df_out)





######## Figure 2b

library(dplyr)
library(readr)
library(stringr)
library(forestploter)
library(grid)

df <- read_csv("/results/Figure 2.csv")

risk_diff_CI <- function(a, n1, c, n0){
  p1 <- a/n1
  p0 <- c/n0
  RD <- (p1 - p0) * 100
  se <- sqrt(p1*(1-p1)/n1 + p0*(1-p0)/n0)
  CI_low <- RD - 1.96*se*100
  CI_high <- RD + 1.96*se*100
  return(c(RD, CI_low, CI_high))
}

df2 <- df %>%
  rowwise() %>%
  mutate(
    
    True_rate = ifelse(!is.na(`True n`), round((as.numeric(str_extract(`True % (95% CI)`, "^[0-9\\.]+"))/100) * `True n` / `True n`, NA),
                       NA),
    Sham_rate = ifelse(!is.na(`Sham n`), round((as.numeric(str_extract(`Sham % (95% CI)`, "^[0-9\\.]+"))/100) * `Sham n` / `Sham n`, NA),
                       NA),
    
    
    RD_vals = list(risk_diff_CI(
      a = round(as.numeric(str_extract(`True % (95% CI)`, "^[0-9\\.]+"))/100 * `True n`),
      n1 = `True n`,
      c = round(as.numeric(str_extract(`Sham % (95% CI)`, "^[0-9\\.]+"))/100 * `Sham n`),
      n0 = `Sham n`
    ))
  ) %>%
  ungroup() %>%
  mutate(
    RD      = sapply(RD_vals, `[`, 1),
    RD_LCL  = sapply(RD_vals, `[`, 2),
    RD_UCL  = sapply(RD_vals, `[`, 3),
    `True %` = ifelse(is.na(`True n`), "",
                      sprintf("%.1f%%", (round(as.numeric(str_extract(`True % (95% CI)`, "^[0-9\\.]+")), 2)))),
    `Sham %` = ifelse(is.na(`Sham n`), "",
                      sprintf("%.1f%%", (round(as.numeric(str_extract(`Sham % (95% CI)`, "^[0-9\\.]+")), 2)))),
    `Risk Difference (95% CI)` = ifelse(is.na(RD),
                                        "",
                                        sprintf("%.1f (%.1f–%.1f)", RD, RD_LCL, RD_UCL))
  ) %>%
  select(Subgroup, Level, `True n`, `True %`, `Sham n`, `Sham %`,
         `Risk Difference (95% CI)`, RD, RD_LCL, RD_UCL)


df2$Subgroup <- ifelse(!is.na(df2$Level), paste0("   ", df2$Level), df2$Subgroup)
df2$Subgroup <- gsub("Carboplatin.Carboplatin.based", "Carboplatin/Carboplatin-based", df2$Subgroup)
df2$Subgroup <- gsub("Premenopausal.Perimenopausal", "Premenopausal", df2$Subgroup)
df2$Subgroup <- gsub("EC.based", "EC-based", df2$Subgroup)

headers <- data.frame(
  Subgroup = c("5-HT3 RA", "BMI", "Menstrual status","Treatment", "Chemotherapy regimen","NK1 RA"),   
  `True n` = NA, `True %` = NA, `Sham n` = NA, `Sham %` = NA,
  `Risk Difference (95% CI)` = NA, RD = NA, RD_LCL = NA, RD_UCL = NA,
  stringsAsFactors = FALSE
)

bmi_order <- c("   <18.5", "   18.5-24.9", "   ≥25")

df2_bmi <- df2 %>%
  filter(Subgroup %in% bmi_order) %>%
  mutate(Subgroup = factor(Subgroup, levels = bmi_order)) %>%
  arrange(Subgroup)


df2_final <- bind_rows(
  headers[1,], df2 %>% filter(grepl("^   Palonosetron", Subgroup) | grepl("^   Ondansetron", Subgroup)),  
  headers[2,], df2_bmi,   
  headers[3,], df2 %>% filter(grepl("^   Premenopausal", Subgroup) | grepl("^   Postmenopausal", Subgroup)),   
  headers[4,], df2 %>% filter(grepl("^   Neoadjuvant", Subgroup) | grepl("^   Adjuvant", Subgroup) | grepl("^   Palliative", Subgroup)),  
  headers[5,], df2 %>% filter(grepl("^   EC-based", Subgroup) | grepl("^   Carboplatin", Subgroup)),  
  headers[6,], df2 %>% filter(grepl("^   Aprepitant", Subgroup) | grepl("^   Fosaprepitant", Subgroup) | grepl("^   Netupitant", Subgroup))  
)


tabletext <- df2_final %>%
  mutate(Subgroup = as.character(Subgroup)) %>%
  select(Subgroup, `True n`, `True %`, `Sham n`, `Sham %`, `Risk Difference (95% CI)`) %>%
  mutate(
    across(c(`True n`, `True %`, `Sham n`, `Sham %`, `Risk Difference (95% CI)`),
           ~ifelse(is.na(.), "", as.character(.))),  
    `True electroacupuncture Better` = "",   
    pelectroacu= ""    
  )

# ============= =================

p <- forest(
  data = tabletext,
  est = df2_final$RD,
  lower = df2_final$RD_LCL,
  upper = df2_final$RD_UCL,
  ci_column = 7,  
  ref_line = 0,
  xlim = c(-40, 40),   
  ticks_at = seq(-40, 40, 20),
  xlab = "Risk Difference (%)",
  
  widths = c(0.20, 0.08, 0.08, 0.08, 0.08, 0.18, 0.25, 0.05),   
  theme = forest_theme(
    base_size = 12,
    refline_gp = gpar(col = "grey40", lwd = 1.5, lty = 2),   
    ci_lwd = 2,        
    ci_pch = 15,       
    ci_cex = 1.5,      
    ci_col = "#3983B7",
    ci_fill = "#EE9E3C",
    vert_line = FALSE
  )
)

# =========== ============================

bold_rows <- which(df2_final$Subgroup %in% c("5-HT3 RA", "BMI", "Menstrual status","Treatment", 
                                             "Chemotherapy regimen", "NK1 RA"))   


p <- edit_plot(
  p,
  gp = gpar(fontface = "bold"),
  row = bold_rows
)

# =============== ============================
pdf_out <- "/results/Figure 2.pdf"
pdf(pdf_out, width = 14, height = 7)   
grid.newpage()
grid.draw(p)
dev.off()


print(p)



library(autoReg)
library(tidyverse)
library(ggsci)
library(survival)
library(broom)

data <- read.csv("/data/ECO Nature data.csv")



data <- data %>%
  mutate(Chemotherapy.regimen = case_when(
    Chemotherapy.regimen == "Cisplatin.based" ~ "Carboplatin.based",
    TRUE ~ as.character(Chemotherapy.regimen)
  ))




data$Overall.Stage.No.nausea <- ifelse(data$Overall.Stage.No.nausea == 0, 1, 0)


data$Age.group <- ifelse(data$Age < 50, "<50", "≥50")
data$Age.group <- factor(data$Age.group, levels = c("≥50", "<50"))


data$BMI.group <- cut(data$BMI,
                      breaks = c(-Inf, 18.5, 24.9, Inf),
                      labels = c("<18.5", "18.5-24.9", "≥25"),
                      right = TRUE)
data <- data %>%
  filter(BMI.group != "<18.5")
data$BMI.group <- factor(data$BMI.group, levels = c("18.5-24.9", "≥25"))

data$BMI <- as.numeric(data$BMI)
data$Age.group <- as.factor(data$Age.group)
data$Histologic.type <- as.factor(data$Histologic.type)
data$Stage <- as.factor(data$Stage)
data$Menstrual.status <- as.factor(data$Menstrual.status)
data$Chemotherapy.regimen <- as.factor(data$Chemotherapy.regimen)
data$Treatment.setting <- as.factor(data$Treatment.setting)
data$NK1.RA <- as.factor(data$NK1.RA)
data$HT3RA5.HT3RA <- as.factor(data$HT3RA5.HT3RA)


data$Group <- as.factor(data$Group)
print("Group levels:")
print(levels(data$Group))

colnames(data) <- gsub("\\.", "_", colnames(data))


subgroup_vars <- c( "HT3RA5_HT3RA", "BMI_group", "Menstrual_status", 
                    "Treatment_setting", "Chemotherapy_regimen", "NK1_RA")


interaction_results <- data.frame(
  Subgroup = character(),
  P_interaction = numeric(),
  stringsAsFactors = FALSE
)


for (subgroup_var in subgroup_vars) {
  cat("Processing:", subgroup_var, "\n")
  
  
  formula_str <- paste("Overall_Stage_No_nausea ~ Group *", subgroup_var)
  fit <- glm(as.formula(formula_str), data = data, family = "binomial")
  
  
  reduced_formula <- paste("Overall_Stage_No_nausea ~ Group +", subgroup_var)
  fit_reduced <- glm(as.formula(reduced_formula), data = data, family = "binomial")
  
  
  lrtest <- anova(fit_reduced, fit, test = "Chisq")
  p_value <- lrtest$`Pr(>Chi)`[2]
  
  cat("P value:", p_value, "\n\n")
  
  
  interaction_results <- rbind(interaction_results, 
                               data.frame(Subgroup = subgroup_var, 
                                          P_interaction = p_value))
}


interaction_results <- interaction_results %>%
  mutate(P_interaction = round(P_interaction, 4),
         P_interaction_formatted = ifelse(P_interaction < 0.0001, "<0.0001", 
                                          as.character(P_interaction)))

subgroup_names <- c(
  "HT3RA5_HT3RA" = "5-HT3 RA",
  "BMI_group" = "BMI",
  "Menstrual_status" = "Menstrual status",
  "Treatment_setting" = "Treatment setting",
  "Chemotherapy_regimen" = "Chemotherapy regimen",
  "NK1_RA" = "NK1 RA"
)

interaction_results$Subgroup <- subgroup_names[interaction_results$Subgroup]


print(interaction_results)


interaction_results <- interaction_results %>%
  mutate(
    
    P_interaction = round(P_interaction, 4),
    
    
    P_interaction_bonferroni = p.adjust(P_interaction, method = "bonferroni"),
    
    
    P_interaction_formatted = ifelse(P_interaction < 0.0001, "<0.0001", 
                                     sprintf("%.4f", P_interaction)),
    
    P_interaction_bonferroni_formatted = ifelse(P_interaction_bonferroni < 0.0001, "<0.0001",
                                                sprintf("%.4f", P_interaction_bonferroni))
  )


print("p and Bonferroni p:")
print(interaction_results %>%
        select(Subgroup, P_interaction_formatted, P_interaction_bonferroni_formatted) %>%
        rename(`原始P值` = P_interaction_formatted,
               `Bonferroni校正P值` = P_interaction_bonferroni_formatted))


library(flextable)
final_table <- interaction_results %>%
  select(Subgroup, P_interaction_formatted, P_interaction_bonferroni_formatted) %>%
  flextable() %>%
  set_header_labels(Subgroup = "Subgroup", 
                    P_interaction_formatted = "P interaction (原始)",
                    P_interaction_bonferroni_formatted = "P interaction (Bonferroni校正)") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 12, part = "all") %>%
  width(width = c(3, 2, 2))

print(final_table)




















######## Figure 3

library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(purrr)
library(openxlsx)
library(scales)


data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)

data <- data %>%
  filter(Set != "ITT.1") %>%
  filter(Set != "ITT.2") %>%
  filter(Set != "ITT.3") %>%
  filter(Set != "ITT.4") %>%
  filter(Set != "ITT.5")

data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)


observation_cols <- c(
  "Overall.Stage.total.control", "Acute.Stage.total.control", "Delayed.Stage.total.control",
  "Overall.Stage.complete.protection", "Acute.Stage.complete.protection", "Delayed.Stage.complete.protection",
  "Overall.Stage.Complete.response", "Acute.Stage.Complete.response", "Delayed.Stage.Complete.response"
)

long_data <- data %>%
  pivot_longer(
    cols = observation_cols,
    names_to = "Observation",
    values_to = "Effectiveness"
  ) %>%
  filter(!is.na(Effectiveness))


group_summaries <- long_data %>%
  group_by(Group, Observation) %>%
  summarise(
    n       = n(),
    success = sum(Effectiveness == 0),
    ControlRate = mean(Effectiveness == 0),
    .groups = 'drop'
  ) %>%
  rowwise() %>%
  mutate(ci = list(binom.test(success, n)$conf.int)) %>%
  mutate(LCL = ci[[1]], UCL = ci[[2]]) %>%
  ungroup() %>%
  select(-ci)

# ======================== ===================
chisq_results <- long_data %>%
  group_by(Observation) %>%
  summarise(test = list(chisq.test(table(Group, Effectiveness))),
            .groups = "drop") %>%
  mutate(chisq_p = map_dbl(test, ~ .x$p.value)) %>%
  select(Observation, chisq_p) %>%
  mutate(
    p_label = ifelse(
      chisq_p < 1e-4,
      "< 0.0001",
      paste0("= ", formatC(chisq_p, format = "f", digits = 4))
    )
  )

group_summaries <- group_summaries %>%
  left_join(chisq_results, by = "Observation")

group_summaries$Observation <- factor(group_summaries$Observation, levels = observation_cols)
chisq_results$Observation <- factor(chisq_results$Observation, levels = observation_cols)

# ======================== ===================
final_plot <- ggplot(group_summaries,
                     aes(x = Observation, y = ControlRate * 100, fill = Group)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +
  
  
  geom_errorbar(
    aes(ymin = LCL * 100, ymax = UCL * 100),
    position = position_dodge(width = 0.9),
    width = 0.2,
    linewidth = 0.7,
    color = "black"
  ) +
  
  
  geom_text(
    aes(label = paste0(success, "/", n, " (", sprintf("%.1f", ControlRate * 100), ")"),
        y = UCL * 100 + 4),
    position = position_dodge(width = 0.85),
    size = 3.3, vjust = 0, color = "black"
  ) +
  
  
  geom_text(
    data = chisq_results,
    aes(x = Observation, y = 108, label = paste0("P ", p_label)),
    inherit.aes = FALSE,
    size = 5, fontface = "italic", color = "black"
  ) +
  
  scale_fill_manual(values = c("#D73027", "#4575B4")) +
  labs(x = "", y = "Control Rate (%)", fill = " ") +
  theme_minimal(base_family = "sans", base_size = 14) +
  theme(
    text = element_text(family = "sans"),
    legend.position  = c(0.5, 0.96),
    legend.direction = "horizontal",
    legend.text      = element_text(size = 14),
    legend.title     = element_text(size = 14),
    axis.text.x      = element_text(size = 13, angle = 0, hjust = 0.5),
    axis.text.y      = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.margin      = margin(5, 5, 5, 5)
  ) +
  scale_y_continuous(
    limits = c(0, 120),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    labels = c(
      "Overall.Stage.total.control"       = "Overall stage\ntotal control",
      "Acute.Stage.total.control"         = "Acute stage\ntotal control",
      "Delayed.Stage.total.control"       = "Delayed stage\ntotal control",
      "Overall.Stage.complete.protection" = "Overall stage\ncomplete protection",
      "Acute.Stage.complete.protection"   = "Acute stage\ncomplete protection",
      "Delayed.Stage.complete.protection" = "Delayed stage\ncomplete protection",
      "Overall.Stage.Complete.response"   = "Overall stage\ncomplete response",
      "Acute.Stage.Complete.response"     = "Acute stage\ncomplete response",
      "Delayed.Stage.Complete.response"   = "Delayed stage\ncomplete response"
    )
  )

print(final_plot)

# ========================== ============================
ggsave(
  filename = "/results/Figure 3.pdf",
  plot = final_plot, width = 20, height = 10, units = "in"
)










########## Figure 4
# =================== ===========================
library(ggplot2)
library(dplyr)
library(broom)
library(patchwork)

# ============================ =======================
data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)

data <- data %>%
  filter(!Set %in% c("ITT.1", "ITT.2", "ITT.3",
                     "ITT.4", "ITT.5"))

data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)

# ====================== ================
eq_data <- data %>% filter(!is.na(EQ.5D.5L0), !is.na(EQ.5D.5L1))
ancova_eq <- lm(EQ.5D.5L1 ~ Group + EQ.5D.5L0, data = eq_data)

anova_eq <- anova(ancova_eq)
F_val_eq <- round(anova_eq["Group", "F value"], 2)
df_num_eq <- anova_eq["Group", "Df"]
df_den_eq <- anova_eq["Residuals", "Df"]

baseline_eq <- mean(eq_data$EQ.5D.5L0, na.rm = TRUE)
newdata_eq <- data.frame(
  Group = factor(c("True electroacupuncture", "Sham electroacupuncture"),
                 levels = levels(eq_data$Group)),
  EQ.5D.5L0 = baseline_eq
)
pred_eq <- predict(ancova_eq, newdata_eq, se.fit = TRUE)

plot_data_eq <- newdata_eq %>%
  mutate(
    Adjusted_Mean = pred_eq$fit,
    SE = pred_eq$se.fit,
    Lower = Adjusted_Mean - 1.96 * SE,
    Upper = Adjusted_Mean + 1.96 * SE
  )

delta_eq <- plot_data_eq$Adjusted_Mean[plot_data_eq$Group == "True electroacupuncture"] -
  plot_data_eq$Adjusted_Mean[plot_data_eq$Group == "Sham electroacupuncture"]
se_delta_eq <- sqrt(sum(plot_data_eq$SE^2))
delta_lower_eq <- delta_eq - 1.96 * se_delta_eq
delta_upper_eq <- delta_eq + 1.96 * se_delta_eq


delta_label_eq <- sprintf('Delta == %.2f * " (%.2f–%.2f)"', delta_eq, delta_lower_eq, delta_upper_eq)

p_val_eq <- tidy(ancova_eq) %>%
  filter(term == "GroupSham electroacupuncture") %>%
  pull(p.value)

p_str_eq <- if (p_val_eq < 0.0001) "< 0.0001" else formatC(p_val_eq, format = "f", digits = 4)
p_label_eq <- paste0("ANCOVA: F(", df_num_eq, ", ", df_den_eq, ") = ", F_val_eq,
                     ", P = ", p_str_eq)


p_eq <- ggplot(plot_data_eq, aes(x = Group, y = Adjusted_Mean, color = Group)) +
  geom_point(size = 5, shape = 18) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.15, linewidth = 1.2) +
  geom_text(aes(y = Upper + 0.008, label = paste0(
    sprintf("%.2f", Adjusted_Mean), " (",
    sprintf("%.2f", Lower), "–", sprintf("%.2f", Upper), ")")),
    size = 3.8, fontface = "plain", family = "sans") +
  annotate("text", x = 1.5, y = 0.90, label = delta_label_eq,
           size = 4.2, fontface = "bold", family = "sans", parse = TRUE) +
  annotate("text", x = 1.5, y = 0.88, label = p_label_eq,
           size = 3.8, fontface = "italic", family = "sans") +
  scale_color_manual(values = c("True electroacupuncture" = "#0072B5",
                                "Sham electroacupuncture" = "#BC3C29")) +
  coord_cartesian(ylim = c(min(plot_data_eq$Lower) - 0.02, 0.90)) +
  labs(x = NULL, y = "EQ-5D-5L index (post-intervention)") +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 13, margin = margin(r = 8)),
    axis.line = element_line(size = 0.6),
    axis.ticks = element_line(size = 0.6),
    panel.grid = element_blank(),
    plot.margin = margin(15, 5, 5, 5)
  )

# ====================== Figure 4b: VAS ANCOVA ======================
vas_data <- data %>% filter(!is.na(VAS0), !is.na(VAS1))
ancova_vas <- lm(VAS1 ~ Group + VAS0, data = vas_data)

anova_vas <- anova(ancova_vas)
F_val_vas <- round(anova_vas["Group", "F value"], 2)
df_num_vas <- anova_vas["Group", "Df"]
df_den_vas <- anova_vas["Residuals", "Df"]

baseline_vas <- mean(vas_data$VAS0, na.rm = TRUE)
newdata_vas <- data.frame(
  Group = factor(c("True electroacupuncture", "Sham electroacupuncture"),
                 levels = levels(vas_data$Group)),
  VAS0 = baseline_vas
)
pred_vas <- predict(ancova_vas, newdata_vas, se.fit = TRUE)

plot_data_vas <- newdata_vas %>%
  mutate(
    Adjusted_Mean = pred_vas$fit,
    SE = pred_vas$se.fit,
    Lower = Adjusted_Mean - 1.96 * SE,
    Upper = Adjusted_Mean + 1.96 * SE
  )

delta_vas <- plot_data_vas$Adjusted_Mean[plot_data_vas$Group == "True electroacupuncture"] -
  plot_data_vas$Adjusted_Mean[plot_data_vas$Group == "Sham electroacupuncture"]
se_delta_vas <- sqrt(sum(plot_data_vas$SE^2))
delta_lower_vas <- delta_vas - 1.96 * se_delta_vas
delta_upper_vas <- delta_vas + 1.96 * se_delta_vas

delta_label_vas <- sprintf('Delta == %.1f * " (%.1f–%.1f)"', delta_vas, delta_lower_vas, delta_upper_vas)

p_val_vas <- tidy(ancova_vas) %>%
  filter(term == "GroupSham electroacupuncture") %>%
  pull(p.value)

p_str_vas <- if (p_val_vas < 0.0001) "< 0.0001" else formatC(p_val_vas, format = "f", digits = 4)
p_label_vas <- paste0("ANCOVA: F(", df_num_vas, ", ", df_den_vas, ") = ", F_val_vas,
                      ", P = ", p_str_vas)

p_vas <- ggplot(plot_data_vas, aes(x = Group, y = Adjusted_Mean, color = Group)) +
  geom_point(size = 5, shape = 18) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.15, linewidth = 1.2) +
  geom_text(aes(y = Upper + 1.5, label = paste0(
    sprintf("%.1f", Adjusted_Mean), " (",
    sprintf("%.1f", Lower), "–", sprintf("%.1f", Upper), ")")),
    size = 3.8, fontface = "plain", family = "sans") +
  annotate("text", x = 1.5, y = 100, label = delta_label_vas,
           size = 4.2, fontface = "bold", family = "sans", parse = TRUE) +
  annotate("text", x = 1.5, y = 97, label = p_label_vas,
           size = 3.8, fontface = "italic", family = "sans") +
  scale_color_manual(values = c("True electroacupuncture" = "#0072B5",
                                "Sham electroacupuncture" = "#BC3C29")) +
  coord_cartesian(ylim = c(min(plot_data_vas$Lower) - 5, 100)) +
  labs(x = NULL, y = "VAS score (post-intervention)") +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 13, margin = margin(r = 8)),
    axis.line = element_line(size = 0.6),
    axis.ticks = element_line(size = 0.6),
    panel.grid = element_blank(),
    plot.margin = margin(15, 5, 5, 5)
  )

# =============== =================
fig4 <- p_eq + p_vas + plot_annotation(tag_levels = "a") &
  theme(text = element_text(family = "sans"))
fig4
ggsave(
  filename = "/results/Figure 4.pdf",
  plot = fig4,
  width = 11, height = 6, units = "in"
)













######### Figure 5 &  Extended Data Fig. 12


data_path <- "/data/ECO Nature SNP data.csv"
analysis_data_260 <- read.csv(data_path)














######
library(dplyr)
library(HardyWeinberg)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
hwe_test_one_exact <- function(data, snp, group_value, group_var = "Group") {
  
  df <- data %>%
    filter(.data[[group_var]] == group_value) %>%
    select(all_of(snp)) %>%
    filter(!is.na(.data[[snp]]))
  
  geno_counts <- table(df[[snp]])
  
  
  if (length(geno_counts) != 3) {
    return(data.frame(
      SNP = snp,
      Group = group_value,
      N = sum(geno_counts),
      Genotypes = paste(names(geno_counts), collapse = "/"),
      HWE_p = NA,
      Note = "HWE not applicable (≠3 genotypes)"
    ))
  }
  
  # =========================
  # Hardy–Weinberg Exact Test
  # =========================
  
  hwe <- HWExact(geno_counts)
  
  data.frame(
    SNP = snp,
    Group = group_value,
    N = sum(geno_counts),
    Genotypes = paste(names(geno_counts), collapse = "/"),
    HWE_p = hwe$pval,
    Note = "Hardy–Weinberg exact test"
  )
}
snp_list <- c(
  "rs6443930",
  "rs1045642",
  "rs1128503",
  "rs2530797",
  "rs4680",
  "rs3755468"
)

hwe_results_exact <- do.call(
  rbind,
  lapply(snp_list, function(snp) {
    do.call(
      rbind,
      lapply(c(0, 1), function(g) {
        hwe_test_one_exact(analysis_data_260, snp, g)
      })
    )
  })
)

hwe_results_exact

write.csv(
  hwe_results_exact,
  "/results/hwe_results_exact.csv",
  row.names = FALSE,
  na = ""
)





### 

snp_list <- c(
  "rs6443930",
  "rs1045642",
  "rs1128503",
  "rs2530797",
  "rs4680",
  "rs3755468"
)

event_var <- "Overall.Stage.No.nausea"
group_var <- "Group"
library(dplyr)

snp_event_summary <- function(data, snp, group_value,
                              group_var = "Group",
                              event_var = "Overall.Stage.No.nausea") {
  
  df <- data %>%
    filter(.data[[group_var]] == group_value) %>%
    select(all_of(c(snp, event_var))) %>%
    filter(!is.na(.data[[snp]])) 
  
  tab_test <- table(df[[snp]], df[[event_var]])
  
  p_value <- if (any(tab_test < 5)) {
    fisher.test(tab_test)$p.value
  } else {
    chisq.test(tab_test)$p.value
  }
  
  df %>%
    group_by(Genotype = .data[[snp]]) %>%
    summarise(
      SNP = snp,
      N = n(),
      Event_n = sum(.data[[event_var]] == 0),
      Event_pct = round(Event_n / N * 100, 1),
      P_value = signif(p_value, 3),
      .groups = "drop"
    ) %>%
    mutate(
      Event = paste0(Event_n, " (", Event_pct, ")")
    ) %>%
    select(
      SNP,
      Genotype,
      N,
      Event,
      P_value
    )
}



final_snp_table <- do.call(
  rbind,
  lapply(snp_list, function(snp) {
    do.call(
      rbind,
      lapply(c(0, 1), function(g) {
        snp_event_summary(
          data = analysis_data_260,
          snp = snp,
          group_value = g,
          group_var = "Group",
          event_var = "Overall.Stage.No.nausea"
        )
      })
    )
  })
)

final_snp_table






table(analysis_data_260$Group)  
final_snp_table




###  FDR
# ================================

# ================================
library(dplyr)

snp_list <- c(
  "rs6443930", "rs1045642", "rs1128503",
  "rs2530797", "rs4680", "rs3755468"
)

event_var <- "Overall.Stage.No.nausea"
group_var <- "Group"

# -------- ------
snp_event_summary <- function(data, snp, group_value,
                              group_var = "Group",
                              event_var = "Overall.Stage.No.nausea") {
  
  df <- data %>%
    filter(.data[[group_var]] == group_value) %>%
    select(all_of(c(snp, event_var))) %>%
    filter(!is.na(.data[[snp]]))
  
  tab_test <- table(df[[snp]], df[[event_var]])
  
  p_value <- if (any(tab_test < 5)) {
    fisher.test(tab_test)$p.value
  } else {
    chisq.test(tab_test)$p.value
  }
  
  df %>%
    group_by(Genotype = .data[[snp]]) %>%
    summarise(
      SNP = snp,
      Group_value = group_value,
      N = n(),
      Event_n = sum(.data[[event_var]] == 0),
      Event_pct = round(Event_n / N * 100, 1),    
      Raw_P = p_value,
      .groups = "drop"
    ) %>%
    mutate(
      Event = paste0(Event_n, " (", Event_pct, "%)")
    ) %>%
    select(SNP, Group_value, Genotype, N, Event, Raw_P)
}

# --------- ----
snp_results_raw <- do.call(
  rbind,
  lapply(snp_list, function(snp) {
    do.call(
      rbind,
      lapply(c(0, 1), function(g) {
        snp_event_summary(
          data = analysis_data_260,
          snp = snp,
          group_value = g,
          group_var = "Group",
          event_var = "Overall.Stage.No.nausea"
        )
      })
    )
  })
)

# ---------- ----------
unique_tests <- snp_results_raw %>%
  distinct(SNP, Group_value, Raw_P) %>%
  mutate(FDR_q = p.adjust(Raw_P, method = "BH"))

# --------- -------
final_snp_table <- snp_results_raw %>%
  left_join(unique_tests, by = c("SNP", "Group_value", "Raw_P")) %>%
  mutate(
    Group = ifelse(Group_value == 1, "True electroacupuncture", "Sham electroacupuncture"),
    Raw_P = signif(Raw_P, 4),
    FDR_q = signif(FDR_q, 4)
  ) %>%
  select(SNP, Group, Genotype, N, Event, Raw_P, FDR_q) %>%
  arrange(SNP, Group, Genotype)

# -------- ----
print(final_snp_table)

write.csv(
  final_snp_table,
  "/results/Extended Data Tab. 3.csv",
  row.names = FALSE,
  na = ""
)






























#####   
library(dplyr)
library(ggplot2)
library(scales)

#  
plot_df <- analysis_data_260 %>%
  filter(!is.na(rs3755468)) %>%
  mutate(
    Genotype = factor(rs3755468, levels = c("CC", "TC", "TT")),
    Group_label = factor(Group,
                         levels = c(1, 0),
                         labels = c("True electroacupuncture", "Sham electroacupuncture")
    )
  ) %>%
  group_by(Group, Group_label, Genotype) %>%
  summarise(
    N = n(),
    Control_n = sum(Overall.Stage.No.nausea == 0),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    Control_rate = Control_n / N,
    
    ci = list(binom.test(Control_n, N)$conf.int),
    ci_low = ci[[1]],
    ci_high = ci[[2]]
  ) %>%
  ungroup() %>%
  mutate(
    
    Control_rate_pct = Control_rate * 100,
    ci_low_pct = ci_low * 100,
    ci_high_pct = ci_high * 100
  )


plot_df <- plot_df %>%
  mutate(Genotype_label = Genotype)


p_values <- lapply(c(1, 0), function(g) {
  df <- analysis_data_260 %>%
    filter(Group == g, !is.na(rs3755468))
  tab <- table(df$rs3755468, df$Overall.Stage.No.nausea == 0)
  p <- if (any(tab < 5)) {
    fisher.test(tab)$p.value
  } else {
    chisq.test(tab)$p.value
  }
  data.frame(
    Group_label = factor(g, levels = c(1, 0),
                         labels = c("True electroacupuncture", "Sham electroacupuncture")),
    P_label = paste0("P = ", formatC(p, format = "f", digits = 4))
  )
}) %>% bind_rows()


geno_colors <- c(
  "CC" = "#3983B7",
  "TC" = "#EE9E3C",
  "TT" = "#4DAF4A"
)


p_rs3755468 <- ggplot(plot_df, aes(x = Genotype_label, y = Control_rate_pct, fill = Genotype)) +
  geom_col(width = 0.65, color = "grey30", linewidth = 0.3) +
  geom_errorbar(
    aes(ymin = ci_low_pct, ymax = ci_high_pct),
    width = 0.2,
    linewidth = 0.7,
    color = "black"
  ) +
  facet_wrap(~ Group_label, nrow = 1, scales = "free_x") +
  scale_y_continuous(
    limits = c(0, max(plot_df$ci_high_pct) * 1.2),   
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = geno_colors) +
  labs(
    title = " ",
    x = "Genotype",
    y = "Overall phase no nausea control rate (%)"
  ) +
  theme_classic(base_size = 15) +
  theme(
    text = element_text(family = "sans"),            
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black", size = 11),
    strip.background = element_rect(fill = "#F2F2F2", color = NA),
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    panel.spacing = unit(1.5, "lines")
  ) +
  geom_text(
    aes(
      label = paste0(Control_n, "/", N, " (", round(Control_rate_pct, 1), ")"),
      y = ci_high_pct + 1 
    ),
    vjust = 0,  
    size = 4,
    color = "black"
  ) +
  geom_text(
    data = p_values,
    aes(x = 2, y = max(plot_df$ci_high_pct) * 1.12, label = P_label),
    inherit.aes = FALSE,
    size = 5,
    fontface = "italic"
  )

p_rs3755468

ggsave(
  filename = "/results/Figure 5.pdf",
  plot = p_rs3755468,
  width = 8,
  height = 6,
  device = cairo_pdf
)
















analysis_data_260 <- analysis_data_260 %>%
  mutate(
    rs3755468_2grp = case_when(
      rs3755468 == "CC" ~ "CC",
      rs3755468 %in% c("TC", "TT") ~ "TC/TT",
      TRUE ~ NA_character_
    )
  )



snp_event_summary_2grp <- function(data, snp_2grp, group_value,
                                   group_var = "Group",
                                   event_var = "Overall.Stage.No.nausea") {
  
  df <- data %>%
    filter(.data[[group_var]] == group_value) %>%
    select(all_of(c(snp_2grp, event_var))) %>%
    filter(!is.na(.data[[snp_2grp]]))
  
  tab_test <- table(df[[snp_2grp]], df[[event_var]])
  
  p_value <- if (any(tab_test < 5)) {
    fisher.test(tab_test)$p.value
  } else {
    chisq.test(tab_test)$p.value
  }
  
  df %>%
    group_by(Genotype = .data[[snp_2grp]]) %>%
    summarise(
      N = n(),
      Event_n = sum(.data[[event_var]] == 0),
      Event_pct = round(Event_n / N * 100),
      .groups = "drop"
    ) %>%
    mutate(
      Event = paste0(Event_n, " (", Event_pct, ")"),
      P_value = signif(p_value, 3)
    ) %>%
    select(
      Genotype,
      N,
      Event,
      P_value
    )
}







final_snp_2grp_table <- do.call(
  rbind,
  lapply(c(0, 1), function(g) {
    tmp <- snp_event_summary_2grp(
      data = analysis_data_260,
      snp_2grp = "rs3755468_2grp",
      group_value = g,
      group_var = "Group",
      event_var = "Overall.Stage.No.nausea"
    )
    tmp$Group <- g
    tmp
  })
)

final_snp_2grp_table



library(dplyr)
library(ggplot2)
library(scales)

# --------- -----
plot_df_2grp <- analysis_data_260 %>%
  filter(!is.na(rs3755468_2grp)) %>%
  mutate(
    Genotype = factor(rs3755468_2grp, levels = c("CC", "TC/TT")),
    Group_label = factor(Group,
                         levels = c(1, 0),
                         labels = c("True electroacupuncture", "Sham electroacupuncture"))
  ) %>%
  group_by(Group_label, Genotype) %>%
  summarise(
    N = n(),
    Control_n = sum(Overall.Stage.No.nausea == 0),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    Control_rate = Control_n / N,
    ci = list(binom.test(Control_n, N)$conf.int),
    ci_low = ci[[1]],
    ci_high = ci[[2]]
  ) %>%
  ungroup() %>%
  mutate(
    Control_rate_pct = Control_rate * 100,
    ci_low_pct = ci_low * 100,
    ci_high_pct = ci_high * 100,
    
    label = paste0(Control_n, "/", N, " (", round(Control_rate_pct, 1), ")")
  )

# -------- -------
p_values_2grp <- lapply(c(1, 0), function(g) {
  df <- analysis_data_260 %>%
    filter(Group == g, !is.na(rs3755468_2grp))
  tab <- table(df$rs3755468_2grp, df$Overall.Stage.No.nausea == 0)
  p <- if (any(tab < 5)) fisher.test(tab)$p.value else chisq.test(tab)$p.value
  data.frame(
    Group_label = factor(g, levels = c(1, 0),
                         labels = c("True electroacupuncture", "Sham electroacupuncture")),
    P_label = paste0("P = ", formatC(p, format = "f", digits = 4))
  )
}) %>% bind_rows()

# ------- 
geno_colors_2grp <- c(
  "CC"    = "#3983B7",
  "TC/TT" = "#EE9E3C"
)

# ------- 
p_2grp <- ggplot(plot_df_2grp, aes(x = Genotype, y = Control_rate_pct, fill = Genotype)) +
  geom_col(width = 0.65, color = "grey30", linewidth = 0.3) +
  geom_errorbar(
    aes(ymin = ci_low_pct, ymax = ci_high_pct),
    width = 0.2,
    linewidth = 0.7,
    color = "black"
  ) +
  facet_wrap(~ Group_label, nrow = 1, scales = "free_x") +
  scale_y_continuous(
    limits = c(0, max(plot_df_2grp$ci_high_pct) * 1.2),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = geno_colors_2grp, guide = "none") +
  labs(
    x = "Genotype group",
    y = "Overall phase no nausea control rate (%)"
  ) +
  theme_classic(base_size = 15) +
  theme(
    text = element_text(family = "sans"),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black", size = 11),
    strip.background = element_rect(fill = "#F2F2F2", color = NA),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1.5, "lines")
  ) +
  geom_text(
    aes(label = label, y = ci_high_pct + 1),
    vjust = 0,
    size = 4,
    color = "black"
  ) +
  geom_text(
    data = p_values_2grp,
    aes(x = 1.5, y = max(plot_df_2grp$ci_high_pct) * 1.12, label = P_label),
    inherit.aes = FALSE,
    size = 5,
    fontface = "italic"
  )

p_2grp

# ------ 
ggsave(
  filename = "/results/Extended Data Fig. 12.pdf",
  plot = p_2grp,
  width = 8,
  height = 6,
  device = cairo_pdf
)













########Extended Data Fig. 2
# =================== ==========================
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(purrr)
library(openxlsx)
library(scales)

# ========================== ====================
data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)


data <- data %>%
  filter(!Set %in% c("ITT.1", "ITT.2", "ITT.3",
                     "ITT.4", "ITT.5", "PPS.1"))


data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)


observation_cols <- c("Overall.Stage.No.nausea")

long_data <- data %>%
  pivot_longer(
    cols = observation_cols,
    names_to = "Observation",
    values_to = "Effectiveness"
  ) %>%
  filter(!is.na(Effectiveness))

# ================= 
group_summaries <- long_data %>%
  group_by(Group, Observation) %>%
  summarise(
    n       = n(),
    success = sum(Effectiveness == 0),
    ControlRate = mean(Effectiveness == 0),
    .groups = 'drop'
  ) %>%
  rowwise() %>%
  mutate(ci = list(binom.test(success, n)$conf.int)) %>%
  mutate(LCL = ci[[1]], UCL = ci[[2]]) %>%
  ungroup() %>%
  select(-ci)

# ================ =========================
chisq_results <- long_data %>%
  group_by(Observation) %>%
  summarise(test = list(chisq.test(table(Group, Effectiveness))),
            .groups = "drop") %>%
  mutate(chisq_p = map_dbl(test, ~ .x$p.value)) %>%
  select(Observation, chisq_p) %>%
  mutate(
    p_label = ifelse(
      chisq_p < 1e-4,
      "< 0.0001",
      paste0("= ", formatC(chisq_p, format = "f", digits = 4))
    )
  )

group_summaries <- group_summaries %>%
  left_join(chisq_results, by = "Observation")

group_summaries$Observation <- factor(group_summaries$Observation,
                                      levels = observation_cols)
chisq_results$Observation <- factor(chisq_results$Observation,
                                    levels = observation_cols)

# ======================== =========================
final_plot <- ggplot(group_summaries,
                     aes(x = Observation, y = ControlRate * 100, fill = Group)) +
  
  
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  
  
  geom_errorbar(
    aes(ymin = LCL * 100, ymax = UCL * 100),
    position = position_dodge(width = 0.7),
    width = 0.2,
    linewidth = 0.7,
    color = "black"
  ) +
  
  
  geom_text(
    aes(label = paste0(success, "/", n, " (", sprintf("%.1f", ControlRate * 100), "%)"),
        y = UCL * 100 + 3),
    position = position_dodge(width = 0.7),
    size = 4.5, vjust = 0, color = "black"
  ) +
  
  
  geom_text(
    data = chisq_results,
    aes(x = Observation, y = 80, label = paste0("P ", p_label)),
    inherit.aes = FALSE,
    size = 5, fontface = "italic", color = "black"
  ) +
  
  scale_fill_manual(values = c("#D73027", "#4575B4")) +
  labs(
    x = "",
    y = "Control rate (%)",
    fill = " "
  ) +
  theme_minimal(base_family = "sans", base_size = 14) +
  theme(
    text = element_text(family = "sans"),
    legend.position  = c(0.5, 0.95),
    legend.direction = "horizontal",
    legend.text      = element_text(size = 14),
    legend.title     = element_text(size = 14),
    axis.text.x      = element_text(size = 14),
    axis.text.y      = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.margin      = margin(5, 5, 5, 5)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    labels = c("Overall.Stage.No.nausea" = "Overall stage no nausea")
  )

print(final_plot)

# ========== ======================
pdf_out <- "/results/Extended Data Fig. 2.pdf"
ggsave(filename = pdf_out, plot = final_plot, width = 12, height = 10, units = "in")
















########Extended Data Fig.  3
# ===================== ============================
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(purrr)
library(openxlsx)
library(scales)

# =========================== ================
data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)


data <- data %>%
  filter(!Set %in% c("ITT.1", "ITT.2", "ITT.3",
                     "ITT.4", "ITT.5")) %>%
  filter(Chemotherapy.regimen == "EC.based")


data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)

# ================ ======================
observation_cols <- "Overall.Stage.No.nausea"

long_data <- data %>%
  pivot_longer(
    cols = observation_cols,
    names_to = "Observation",
    values_to = "Effectiveness"
  ) %>%
  filter(!is.na(Effectiveness))

# =================== =================
group_summaries <- long_data %>%
  group_by(Group, Observation) %>%
  summarise(
    n       = n(),
    success = sum(Effectiveness == 0),
    ControlRate = mean(Effectiveness == 0),
    .groups = 'drop'
  ) %>%
  rowwise() %>%
  mutate(ci = list(binom.test(success, n)$conf.int)) %>%
  mutate(LCL = ci[[1]], UCL = ci[[2]]) %>%
  ungroup() %>%
  select(-ci)

# ======================= =====================
chisq_results <- long_data %>%
  group_by(Observation) %>%
  summarise(test = list(chisq.test(table(Group, Effectiveness))),
            .groups = "drop") %>%
  mutate(chisq_p = map_dbl(test, ~ .x$p.value)) %>%
  select(Observation, chisq_p) %>%
  mutate(
    p_label = ifelse(
      chisq_p < 1e-4,
      "< 0.0001",
      paste0("= ", formatC(chisq_p, format = "f", digits = 4))
    )
  )

group_summaries <- group_summaries %>%
  left_join(chisq_results, by = "Observation")


group_summaries$Observation <- "Overall phase no nausea"
chisq_results$Observation <- "Overall phase no nausea"

# ================= ===========
overall_data <- group_summaries
p1 <- overall_data$ControlRate[overall_data$Group == "True electroacupuncture"]
n1 <- overall_data$n[overall_data$Group == "True electroacupuncture"]
p0 <- overall_data$ControlRate[overall_data$Group == "Sham electroacupuncture"]
n0 <- overall_data$n[overall_data$Group == "Sham electroacupuncture"]

RD <- p1 - p0
SE <- sqrt(p1*(1-p1)/n1 + p0*(1-p0)/n0)
CI_low <- RD - 1.96*SE
CI_high <- RD + 1.96*SE

RD_label <- sprintf("Risk difference: %.1f (95%% CI, %.1f – %.1f)",
                    RD*100, CI_low*100, CI_high*100)

# ===================== =========================
final_plot <- ggplot(group_summaries,
                     aes(x = Observation, y = ControlRate*100, fill = Group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(
    aes(ymin = LCL*100, ymax = UCL*100),
    position = position_dodge(width = 0.7),
    width = 0.2,
    linewidth = 0.7,
    color = "black"
  ) +
  #  
  geom_text(
    aes(label = paste0(success, "/", n, " (", sprintf("%.1f", ControlRate * 100), ")"),
        y = UCL*100 + 3),
    position = position_dodge(width = 0.7),
    size = 4.5, vjust = 0, color = "black"
  ) +
  
  geom_text(
    data = chisq_results,
    aes(x = Observation, y = 85, label = paste0("P ", p_label)),
    inherit.aes = FALSE,
    size = 5, fontface = "italic", color = "black"
  ) +
  
  annotate("text", x = 1, y = max(p1, p0)*100 + 20, label = RD_label,
           size = 5, color = "black", hjust = 0.5) +
  scale_fill_manual(values = c("#D73027", "#4575B4")) +
  labs(
    x = "",
    y = "Control rate (%)",
    fill = " "
  ) +
  theme_minimal(base_family = "sans", base_size = 14) +
  theme(
    text = element_text(family = "sans"),
    legend.position  = c(0.5, 0.95),
    legend.direction = "horizontal",
    legend.text      = element_text(size = 14),
    legend.title     = element_text(size = 14),
    axis.text.x      = element_text(size = 14),
    axis.text.y      = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.margin      = margin(5, 5, 5, 5)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  )

print(final_plot)

# ========================= ===============
pdf_out <- "/results/Extended Data Fig. 3.pdf"
ggsave(filename = pdf_out, plot = final_plot, width = 8, height = 8, units = "in")










########## Extended Data Fig. 4
# ======================= =============
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(purrr)
library(openxlsx)
library(scales)

# ====================== ============
data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)

# 剔除指定姓名
data <- data %>%
  filter(!Set %in% c("ITT.1", "ITT.2", "ITT.3",
                     "ITT.4", "ITT.5"))

# ====================== ===========
data <- data %>%
  filter(is.na(Chemotherapy.TIME))


data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)

# ==================== ======
observation_cols <- c("Overall.Stage.No.nausea")

long_data <- data %>%
  pivot_longer(
    cols = observation_cols,
    names_to = "Observation",
    values_to = "Effectiveness"
  ) %>%
  filter(!is.na(Effectiveness))

group_summaries <- long_data %>%
  group_by(Group, Observation) %>%
  summarise(
    n       = n(),
    success = sum(Effectiveness == 0),
    ControlRate = mean(Effectiveness == 0),
    .groups = 'drop'
  ) %>%
  rowwise() %>%
  mutate(ci = list(binom.test(success, n)$conf.int)) %>%
  mutate(LCL = ci[[1]], UCL = ci[[2]]) %>%
  ungroup() %>%
  select(-ci)

# ================== ============
chisq_results <- long_data %>%
  group_by(Observation) %>%
  summarise(test = list(chisq.test(table(Group, Effectiveness))),
            .groups = "drop") %>%
  mutate(chisq_p = map_dbl(test, ~ .x$p.value)) %>%
  select(Observation, chisq_p) %>%
  mutate(
    p_label = ifelse(
      chisq_p < 1e-4,
      "< 0.0001",
      paste0("= ", formatC(chisq_p, format = "f", digits = 4))
    )
  )

group_summaries <- group_summaries %>%
  left_join(chisq_results, by = "Observation")

group_summaries$Observation <- factor(group_summaries$Observation,
                                      levels = observation_cols)
chisq_results$Observation <- factor(chisq_results$Observation,
                                    levels = observation_cols)

# =================== ==========================
final_plot <- ggplot(group_summaries,
                     aes(x = Observation, y = ControlRate * 100, fill = Group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(
    aes(ymin = LCL * 100, ymax = UCL * 100),
    position = position_dodge(width = 0.7),
    width = 0.2,
    linewidth = 0.7,
    color = "black"
  ) +
  
  geom_text(
    aes(label = paste0(success, "/", n, " (", sprintf("%.1f", ControlRate * 100), "%)"),
        y = UCL * 100 + 3),
    position = position_dodge(width = 0.7),
    size = 4.5, vjust = 0, color = "black"
  ) +
  
  geom_text(
    data = chisq_results,
    aes(x = Observation, y = 80, label = paste0("P ", p_label)),
    inherit.aes = FALSE,
    size = 5, fontface = "italic", color = "black"
  ) +
  scale_fill_manual(values = c("#D73027", "#4575B4")) +
  labs(
    x = "",
    y = "Control rate (%)",
    fill = " "
  ) +
  theme_minimal(base_family = "sans", base_size = 14) +
  theme(
    text = element_text(family = "sans"),
    legend.position  = c(0.5, 0.95),
    legend.direction = "horizontal",
    legend.text      = element_text(size = 14),
    legend.title     = element_text(size = 14),
    axis.text.x      = element_text(size = 14),
    axis.text.y      = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.margin      = margin(5, 5, 5, 5)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    labels = c("Overall.Stage.No.nausea" = "Overall stage no nausea")
  )

print(final_plot)

# ================== ====================
pdf_out <- "/results/Extended Data Fig. 4.pdf"
ggsave(filename = pdf_out, plot = final_plot, width = 12, height = 10, units = "in")







######  Extended Data Fig. 5 &  Extended Data Fig. 8

library(marginaleffects)
library(rio)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(broom)


data_path <- "/data/ECO Nature data.csv"
df <- read.csv(data_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")


df <- df %>%
  filter(!Set %in% c("ITT.1", "ITT.2", "ITT.3",
                     "ITT.4", "ITT.5"))


df$BMI <- factor(
  ifelse(df$BMI < 18.5, 1,
         ifelse(df$BMI < 25, 2, 3)),
  levels = c(1, 2, 3),
  labels = c("<18.5", "18.5~24.9", "≥25")
)


df$Sex <- factor(df$Sex)
df$ECOG <- factor(df$ECOG, levels = c(0, 1), labels = c("0", "1"))
df$Menstrual.status <- factor(df$Menstrual.status,
                              levels = c("Postmenopausal",
                                         "Premenopausal.Perimenopausal",
                                         "Not.applicable"))
df$Histologic.type <- factor(df$Histologic.type)
df$Stage <- factor(df$Stage)
df$Treatment.setting <- factor(df$Treatment.setting)
df$NK1.RA <- factor(df$NK1.RA)
df$HT3RA5.HT3RA <- factor(df$HT3RA5.HT3RA)
df$Chemotherapy.regimen <- factor(df$Chemotherapy.regimen)
df$Concurrent.antitumor.drugs <- factor(df$Concurrent.antitumor.drugs)
df$Acupuncture.days <- factor(df$Acupuncture.days, levels = c(4, 3, 2, 1))
df$Antiemetic.dose <- factor(df$Antiemetic.dose)
df$Group <- factor(df$Group, levels = c(0, 1), labels = c("Sham", "True"))
df$Center <- factor(df$Center)

cat("========== Outcome Variable Definition ==========\n")


df$Nausea_day1 <- ifelse(df$Nausea.d1 == 0, 0, 1)
df$Nausea_day2 <- ifelse(df$Nausea.d2 == 0, 0, 1)
df$Nausea_day3 <- ifelse(df$Nausea.d3 == 0, 0, 1)
df$Nausea_day4 <- ifelse(df$Nausea.d4 == 0, 0, 1)
df$Nausea_day5 <- ifelse(df$Nausea.d5 == 0, 0, 1)


df$time_to_nausea <- apply(
  df[, c("Nausea_day1", "Nausea_day2", "Nausea_day3", "Nausea_day4", "Nausea_day5")],
  1,
  function(x) {
    if (sum(x) == 0) return(5) else return(which(x == 1)[1])
  }
)

df$Nausea_overall <- ifelse(df$time_to_nausea < 5, 1, 0)

df$No_nausea <- ifelse(df$Nausea_overall == 1, 1, 0)

cat("Outcome variable distribution:\n")
cat("Nausea present (Nausea_overall = 1):", sum(df$Nausea_overall == 1, na.rm = TRUE), "cases\n")
cat("No nausea (Nausea_overall = 0):", sum(df$Nausea_overall == 0, na.rm = TRUE), "cases\n")
cat("Missing values:", sum(is.na(df$Nausea_overall)), "cases\n\n")


incidence_table <- df %>%
  filter(!is.na(Nausea_overall)) %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    nausea_cases = sum(Nausea_overall == 1),
    nausea_incidence = mean(Nausea_overall == 1),
    .groups = 'drop'
  )

cat("========== Nausea Incidence by Group ==========\n")
print(incidence_table)


absolute_risk_diff <- incidence_table$nausea_incidence[2] - incidence_table$nausea_incidence[1]
cat(sprintf("\nCrude absolute risk difference (True EA - Sham EA): %.3f\n", absolute_risk_diff))
cat(sprintf("Crude relative risk (True EA/Sham EA): %.2f\n", 
            incidence_table$nausea_incidence[2]/incidence_table$nausea_incidence[1]))



cat("\n========== Unadjusted Analysis (Outcome: Nausea=1) ==========\n")


unadj_fit <- glm(
  Nausea_overall ~ Group,
  data = df,
  family = binomial()
)

unadj_or <- tidy(unadj_fit, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term == "GroupTrue") %>%
  select(estimate, conf.low, conf.high, p.value)

cat("Unadjusted Odds Ratio (OR) [True EA vs Sham EA]:\n")
cat(sprintf("OR = %.2f (95%% CI: %.2f-%.2f), P = %.4f\n",
            unadj_or$estimate, unadj_or$conf.low, unadj_or$conf.high, unadj_or$p.value))


unadj_rd <- avg_comparisons(
  unadj_fit,
  variables = "Group",
  comparison = "difference",
  type = "response"
)

cat("\nUnadjusted Risk Difference (RD) [True EA - Sham EA]:\n")
cat(sprintf("RD = %.3f (95%% CI: %.3f-%.3f)\n",
            unadj_rd$estimate, unadj_rd$conf.low, unadj_rd$conf.high))


unadj_rr <- avg_comparisons(
  unadj_fit,
  variables = "Group",
  comparison = "ratio",
  type = "response"
)

cat("\nUnadjusted Relative Risk (RR) [True EA / Sham EA]:\n")
cat(sprintf("RR = %.2f (95%% CI: %.2f-%.2f)\n",
            unadj_rr$estimate, unadj_rr$conf.low, unadj_rr$conf.high))


cat("\n========== Adjusted Analysis (Covariates Adjusted) ==========\n")


adj_fit <- glm(
  Nausea_overall ~ Group + Center + BMI +
    Treatment.setting + Chemotherapy.regimen +
    NK1.RA + HT3RA5.HT3RA + Menstrual.status,
  data = df,
  family = binomial()
)


adj_or <- tidy(adj_fit, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term == "GroupTrue") %>%
  select(estimate, conf.low, conf.high, p.value)

cat("Adjusted Odds Ratio (OR) [True EA vs Sham EA]:\n")
cat(sprintf("OR = %.2f (95%% CI: %.2f-%.2f), P = %.4f\n",
            adj_or$estimate, adj_or$conf.low, adj_or$conf.high, adj_or$p.value))


adj_rd <- avg_comparisons(
  adj_fit,
  variables = "Group",
  comparison = "difference",
  type = "response",
  vcov = "HC0"  
)

cat("\nAdjusted Risk Difference (RD) [True EA - Sham EA]:\n")
cat(sprintf("RD = %.3f (95%% CI: %.3f-%.3f)\n",
            adj_rd$estimate, adj_rd$conf.low, adj_rd$conf.high))


adj_rr <- avg_comparisons(
  adj_fit,
  variables = "Group",
  comparison = "ratio",
  type = "response",
  vcov = "HC0"
)

cat("\nAdjusted Relative Risk (RR) [True EA / Sham EA]:\n")
cat(sprintf("RR = %.2f (95%% CI: %.2f-%.2f)\n",
            adj_rr$estimate, adj_rr$conf.low, adj_rr$conf.high))

# 
nnt <- 1 / abs(adj_rd$estimate)
cat(sprintf("\nNumber Needed to Treat (NNT): %.1f (i.e., treat approximately %d patients to prevent 1 additional nausea case)\n",
            nnt, round(nnt)))


results_summary <- data.frame(
  Analysis_Type = c("Unadjusted", "Adjusted"),
  Sham_EA_Incidence = sprintf("%.1f%%", incidence_table$nausea_incidence[1] * 100),
  True_EA_Incidence = sprintf("%.1f%%", incidence_table$nausea_incidence[2] * 100),
  Odds_Ratio_OR = c(
    sprintf("%.2f (%.2f-%.2f)", unadj_or$estimate, unadj_or$conf.low, unadj_or$conf.high),
    sprintf("%.2f (%.2f-%.2f)", adj_or$estimate, adj_or$conf.low, adj_or$conf.high)
  ),
  OR_P_value = c(
    sprintf("%.4f", unadj_or$p.value),
    sprintf("%.4f", adj_or$p.value)
  ),
  Risk_Difference_RD = c(
    sprintf("%.3f (%.3f-%.3f)", unadj_rd$estimate, unadj_rd$conf.low, unadj_rd$conf.high),
    sprintf("%.3f (%.3f-%.3f)", adj_rd$estimate, adj_rd$conf.low, adj_rd$conf.high)
  ),
  Relative_Risk_RR = c(
    sprintf("%.2f (%.2f-%.2f)", unadj_rr$estimate, unadj_rr$conf.low, unadj_rr$conf.high),
    sprintf("%.2f (%.2f-%.2f)", adj_rr$estimate, adj_rr$conf.low, adj_rr$conf.high)
  ),
  Interpretation = c(
    "Nausea risk significantly lower in True EA group compared to Sham EA",
    "Protective effect remains significant after covariate adjustment"
  )
)

cat("\n========== Detailed Results Summary Table ==========\n")
print(results_summary, row.names = FALSE)


write.csv(results_summary, "/results/Nausea_Analysis_Detailed_Results.csv", row.names = FALSE, fileEncoding = "UTF-8")

























###
library(survival)
library(survminer)
library(dplyr)
library(cowplot)


df$Group <- factor(df$Group, levels = c("Sham", "True"))


df$status_nausea <- ifelse(df$Nausea_overall == 1, 1, 0)
surv_obj <- Surv(time = df$time_to_nausea, event = df$status_nausea)


cox_fit <- coxph(surv_obj ~ Group, data = df)
cox_summary <- summary(cox_fit)
hr <- cox_summary$conf.int[1, "exp(coef)"]
hr_lower <- cox_summary$conf.int[1, "lower .95"]
hr_upper <- cox_summary$conf.int[1, "upper .95"]
hr_label <- paste0("HR = ", sprintf("%.2f", hr),
                   " (95% CI, ", sprintf("%.2f", hr_lower), "–", sprintf("%.2f", hr_upper), ")")


survdiff_res <- survdiff(surv_obj ~ Group, data = df)
logrank_p <- 1 - pchisq(survdiff_res$chisq, length(survdiff_res$n) - 1)
logrank_p_str <- ifelse(logrank_p < 0.0001, "< 0.0001",
                        paste0("= ", formatC(logrank_p, format = "f", digits = 4)))
logrank_label <- paste0("Log-rank P ", logrank_p_str)

n_summary <- df %>%
  filter(!is.na(Nausea_overall)) %>%
  group_by(Group) %>%
  summarise(n_total = n(), n_events = sum(Nausea_overall == 1))

legend_labs <- c(
  paste0("Sham electroacupuncture (n=", n_summary$n_total[n_summary$Group=="Sham"], 
         ", events=", n_summary$n_events[n_summary$Group=="Sham"], ")"),
  paste0("True electroacupuncture (n=", n_summary$n_total[n_summary$Group=="True"], 
         ", events=", n_summary$n_events[n_summary$Group=="True"], ")")
)


fit_km <- survfit(surv_obj ~ Group, data = df)

# 
p_cuminc <- ggsurvplot(
  fit_km, data = df, fun = "event",
  palette = c("#BC3C29", "#0072B5"),
  xlab = "Time (days)",
  ylab = "Cumulative nausea incidence (%)",
  legend.title = "",
  legend.labs = legend_labs,
  legend = c(0.68, 0.98),            
  pval = FALSE,
  risk.table = TRUE,
  risk.table.col = "strata",
  risk.table.height = 0.4,
  risk.table.y.text = FALSE,
  risk.table.legend = FALSE,
  ggtheme = theme_classic(base_family = "sans", base_size = 15),
  break.x.by = 1,
  xlim = c(0, 5),
  surv.median.line = "none",
  size = 1.2
)


p_cuminc$table <- p_cuminc$table + theme(legend.position = "none")


p_cuminc$plot <- p_cuminc$plot +
  scale_y_continuous(
    limits = c(0, 0.84),                           
    breaks = seq(0, 0.8, 0.2),                   
    labels = function(x) round(x * 100, 0),      
    expand = c(0, 0)
  ) +
  annotate("text", x = 0, y = 0.80, label = hr_label,    
           hjust = 0, size = 4.5, family = "sans") +
  annotate("text", x = 0, y = 0.70, label = logrank_label,
           hjust = 0, size = 4.5, family = "sans", fontface = "italic") +
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14),
    panel.grid = element_blank(),
    axis.line = element_line(size = 0.5),
    axis.ticks = element_line(size = 0.5)
  )


combined_plot <- plot_grid(
  p_cuminc$plot, p_cuminc$table,
  ncol = 1, rel_heights = c(0.7, 0.3),
  align = "v", axis = "lr"
)

print(combined_plot)

ggsave(
  filename = "/results/Extended Data Fig. 5.pdf",
  plot = combined_plot,
  width = 8, height = 6, units = "in"
)












results_summary
# ==========  
library(ggplot2)
library(dplyr)


bar_data <- df %>%
  filter(!is.na(Overall.Stage.No.nausea)) %>%
  mutate(Group = factor(Group, levels = c("True", "Sham"),    
                        labels = c("True electroacupuncture", "Sham electroacupuncture"))) %>%
  group_by(Group) %>%
  summarise(
    n_total = n(),
    n_success = sum(Overall.Stage.No.nausea == 0),    
    rate = mean(Overall.Stage.No.nausea == 0),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    ci = list(binom.test(n_success, n_total)$conf.int),
    LCL = ci[[1]],
    UCL = ci[[2]]
  ) %>%
  ungroup()


chisq_res <- chisq.test(table(df$Group, df$Overall.Stage.No.nausea == 0))
p_text <- ifelse(chisq_res$p.value < 0.0001,
                 "P < 0.0001",
                 paste0("P = ", formatC(chisq_res$p.value, format = "f", digits = 4)))


adj_rd_text <- "Adjusted RD (95% CI): 22.7 (13.1–32.3)"
adj_or_text <- "Adjusted OR (95% CI): 2.78 (1.75–4.35)"


p_bar <- ggplot(bar_data, aes(x = Group, y = rate * 100, fill = Group)) +
  
  geom_col(width = 0.6, color = "black", size = 0.3) +
  geom_errorbar(aes(ymin = LCL * 100, ymax = UCL * 100),
                width = 0.15, linewidth = 0.7) +
  
  geom_text(aes(y = UCL * 100 + 2,
                label = paste0(n_success, "/", n_total, " (",
                               sprintf("%.1f", rate * 100), ")")),
            size = 5, vjust = 0, family = "sans") +
  
  annotate("text", x = 1.5, y = max(bar_data$UCL * 100) + 9,
           label = p_text, size = 5, fontface = "italic", family = "sans") +
  
  annotate("text", x = 1.5, y = max(bar_data$UCL * 100) + 16,
           label = adj_or_text, size = 4.5, family = "sans", fontface = "bold") +
  annotate("text", x = 1.5, y = max(bar_data$UCL * 100) + 13,
           label = adj_rd_text, size = 4.5, family = "sans", fontface = "bold") +
  scale_fill_manual(values = c("True electroacupuncture" = "#0072B5",
                               "Sham electroacupuncture" = "#BC3C29")) +
  scale_y_continuous(
    limits = c(0, 80),               
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(x = "", y = "No‑nausea control rate (%)") +
  theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "sans"),
    legend.position = "none",
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.line = element_line(size = 0.5),
    axis.ticks = element_line(size = 0.5),
    panel.grid = element_blank()
  )


print(p_bar)
ggsave(
  filename = "/results/Extended Data Fig. 8.pdf",
  plot = p_bar,
  width = 8, height = 8, units = "in"
)














############ Extended Data Fig.  6
# ============= ====
library(dplyr)
library(ggplot2)
library(scales)
library(MASS)    

# ================ ========
data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)

# 剔除指定样本
data <- data %>%
  filter(!Set %in% c("ITT.1", "ITT.2", "ITT.3",
                     "ITT.4", "ITT.5"))


data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)

# ======== =============
data <- data %>%
  rowwise() %>%
  mutate(
    Max_Nausea_VAS = if (all(is.na(c(Nausea.d1, Nausea.d2, Nausea.d3, Nausea.d4, Nausea.d5)))) {
      NA_real_
    } else {
      max(c(Nausea.d1, Nausea.d2, Nausea.d3, Nausea.d4, Nausea.d5), na.rm = TRUE)
    }
  ) %>%
  ungroup()

# =============== =============
data <- data %>%
  mutate(
    Max_Nausea_Category = cut(
      Max_Nausea_VAS,
      breaks = c(-Inf, 0, 30, 60, 100),
      labels = c("0", "1–30", "31–60", "61–100"),
      right = TRUE,
      ordered_result = TRUE
    )
  ) %>%
  
  mutate(Max_Nausea_Category = factor(Max_Nausea_Category,
                                      levels = c("61–100",  "31–60","1–30","0" ),
                                      ordered = TRUE))

# ============== ============
# =============== ================
ord_model <- polr(Max_Nausea_Category ~ Group, data = data, Hess = TRUE)
coef_table <- coef(summary(ord_model))
coef_group <- coef_table["GroupSham electroacupuncture", ]

beta <- coef_group["Value"]
se   <- coef_group["Std. Error"]
z    <- coef_group["t value"]


OR_true <- exp(-beta)
CI_lower_true <- exp(-beta - 1.96 * se)
CI_upper_true <- exp(-beta + 1.96 * se)
P <- pnorm(abs(z), lower.tail = FALSE) * 2

stat_label <- paste0(
  " ",
  "OR = ", round(OR_true, 2),
  " (95% CI ", round(CI_lower_true, 2), "–", round(CI_upper_true, 2), ")\n",
  "P = ", formatC(P, format = "f", digits = 4)
)
# ============ ============
plot_data <- data %>%
  filter(!is.na(Max_Nausea_Category)) %>%
  count(Group, Max_Nausea_Category) %>%
  group_by(Group) %>%
  mutate(
    group_n = sum(n),                     
    Percent = n / group_n,
    
    Label_inner = ifelse(
      Max_Nausea_Category != "61-100" & Percent >= 0.05,
      paste0(n, "/", group_n, " (", round(Percent * 100, 1), ")"),
      ""
    )
  ) %>%
  ungroup()


external_labels <- plot_data %>%
  filter(Max_Nausea_Category == "61-100") %>%
  mutate(external_label = paste0(n, "/", group_n, " (", round(Percent * 100, 1), ")"))

# ============ ==========
vas_colors <- c(
  "0"      = "#66C9C5",
  "1–30"   = "#FFD92F",
  "31–60"  = "#377EB8",
  "61–100" = "#E41A1C"
)

# ============ ===================
p_cat <- ggplot(plot_data, aes(x = Group, y = Percent, fill = Max_Nausea_Category)) +
  geom_bar(stat = "identity", width = 0.65, color = "black", size = 0.25) +
  
  
  geom_text(
    aes(label = Label_inner),
    position = position_stack(vjust = 0.5),
    size = 3, color = "black", family = "sans"
  ) +
  
  
  geom_text(
    data = external_labels,
    aes(x = Group, y = 1.02, label = external_label),
    inherit.aes = FALSE,
    size = 3.2, color = "black", vjust = 0, family = "sans"
  ) +
  
  
  annotate(
    "text",
    x = 1.5,
    y = 1.15,
    label = stat_label,
    size = 3.4,
    hjust = 0.5,
    family = "sans"
  ) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.20),
    expand = expansion(mult = c(0, 0))
  ) +
  
  scale_fill_manual(values = vas_colors, name = "Maximum VAS") +
  
  labs(x = NULL, y = "Percentage of patients") +
  
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "sans"),   
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10),
    axis.line    = element_line(size = 0.5),
    axis.ticks   = element_line(size = 0.5),
    plot.margin  = margin(6, 6, 6, 6)
  )


p_cat

# ============== =================
ggsave(
  filename = "/results/Extended Data Fig. 6.pdf",
  plot = p_cat,
  width = 8,
  height = 7,
  units = "in"
)

# =================== ===============
cat("\n", stat_label, "\n")
table(data$Group, data$Max_Nausea_Category)













###### Extended Data Fig. 7
# ======================= ===========================
library(ggplot2)
library(dplyr)
library(scales)

# ============================= ===================
data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)

data <- data %>%
  filter(!Set %in% c("ITT.1", "ITT.2", "ITT.3",
                     "ITT.4", "ITT.5"))

data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)

# ================= =============
data <- data %>%
  rowwise() %>%
  mutate(
    Max_Nausea_VAS = max(
      c(Nausea.d1, Nausea.d2, Nausea.d3, Nausea.d4, Nausea.d5),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# ================== =============
summary_table <- data %>%
  group_by(Group) %>%
  summarise(
    Median = median(Max_Nausea_VAS, na.rm = TRUE),
    Q1     = quantile(Max_Nausea_VAS, 0.25, na.rm = TRUE),
    Q3     = quantile(Max_Nausea_VAS, 0.75, na.rm = TRUE)
  )


label_data <- summary_table %>%
  mutate(
    label = paste0(
      "Median (IQR)\n",
      sprintf("%.0f", Median), " (",
      sprintf("%.0f", Q1), "–",
      sprintf("%.0f", Q3), ")"
    ),
    y_pos = 98   
  )

# =========== =====
wilcox_res <- wilcox.test(Max_Nausea_VAS ~ Group, data = data)
p_label <- paste0(
  "P = ",
  formatC(wilcox_res$p.value, format = "f", digits = 4)
)

# ============= ================
p <- ggplot(data, aes(x = Group, y = Max_Nausea_VAS, fill = Group)) +
  geom_violin(
    trim = FALSE,
    alpha = 0.6,
    color = NA
  ) +
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    color = "black",
    size = 0.3
  ) +
  
  geom_text(
    data = label_data,
    aes(x = Group, y = y_pos, label = label),
    inherit.aes = FALSE,
    size = 3.2,
    hjust = 0.5,
    vjust = 1,
    family = "sans"    
  ) +
  
  annotate(
    "text",
    x = 1.5,
    y = 105,
    label = p_label,
    size = 3.5,
    fontface = "italic",    
    family = "sans"
  ) +
  scale_y_continuous(
    limits = c(0, 108),
    breaks = c(0, 20, 40, 60, 80, 100)
  ) +
  scale_fill_manual(
    values = c("True electroacupuncture" = "#E41A1C",
               "Sham electroacupuncture" = "#377EB8")
  ) +
  labs(
    x = NULL,
    y = "VAS score"
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "sans"),    
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.line   = element_line(size = 0.5),
    axis.ticks  = element_line(size = 0.5),
    plot.margin = margin(6, 6, 6, 6)
  )

p
data %>% filter(!is.na(Max_Nausea_VAS)) %>% count(Group)
# ================ =================
ggsave(
  filename = "/results/Extended Data Fig. 7.pdf",
  plot = p,
  width = 6,
  height = 4,
  units = "in"
)








####### Extended Data Fig. 9
# =================== =======================
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(openxlsx)
library(scales)

# ======================== ===================
data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)


data <- data %>%
  filter(!Set %in% c("ITT.1", "ITT.2", "ITT.3",
                     "ITT.4", "ITT.5"))


data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)

# ====================== ===============
supp_observation_cols <- c(
  "Overall.Stage.No.significant.nausea",
  "Acute.Stage.No.significant.nausea",
  "Delayed.Stage.No.significant.nausea",
  "Overall.Stage.No.nausea.VAS.Score...5",
  "Acute.Stage.No.nausea.VAS.Score...5",
  "Delayed.Stage.No.nausea.VAS.Score...5"
)

long_data_supp <- data %>%
  pivot_longer(
    cols = supp_observation_cols,
    names_to = "Observation",
    values_to = "Effectiveness"
  ) %>%
  filter(!is.na(Effectiveness))

# ================== ====================
supp_group_summaries <- long_data_supp %>%
  group_by(Group, Observation) %>%
  summarise(
    n       = n(),
    success = sum(Effectiveness == 0),
    ControlRate = mean(Effectiveness == 0),
    .groups = 'drop'
  ) %>%
  rowwise() %>%
  mutate(ci = list(binom.test(success, n)$conf.int)) %>%
  mutate(LCL = ci[[1]], UCL = ci[[2]]) %>%
  ungroup() %>%
  select(-ci)

# ======================== ==========
supp_chisq_results <- long_data_supp %>%
  group_by(Observation) %>%
  summarise(test = list(chisq.test(table(Group, Effectiveness))),
            .groups = "drop") %>%
  mutate(chisq_p = map_dbl(test, ~ .x$p.value)) %>%
  select(Observation, chisq_p) %>%
  mutate(
    p_label = ifelse(
      chisq_p < 1e-4,
      "< 0.0001",
      paste0("= ", formatC(chisq_p, format = "f", digits = 4))
    )
  )

supp_group_summaries <- supp_group_summaries %>%
  left_join(supp_chisq_results, by = "Observation")

supp_group_summaries$Observation <- factor(supp_group_summaries$Observation,
                                           levels = supp_observation_cols)
supp_chisq_results$Observation <- factor(supp_chisq_results$Observation,
                                         levels = supp_observation_cols)

# ============================= ======================
supp_plot <- ggplot(supp_group_summaries,
                    aes(x = Observation, y = ControlRate*100, fill = Group)) +
  
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +
  
  
  geom_errorbar(
    aes(ymin = LCL*100, ymax = UCL*100),
    position = position_dodge(width = 0.85),
    width = 0.2,
    linewidth = 0.7,
    color = "black"
  ) +
  
  
  geom_text(
    aes(label = paste0(success, "/", n, " (", sprintf("%.1f", ControlRate * 100), ")"),
        y = UCL*100 + 3),
    position = position_dodge(width = 0.85),
    size = 3.8, vjust = 0, color = "black"
  ) +
  
  
  geom_text(
    data = supp_chisq_results,
    aes(x = Observation, y = 107, label = paste0("P ", p_label)),
    inherit.aes = FALSE,
    size = 5, fontface = "italic", color = "black"
  ) +
  
  scale_fill_manual(values = c("#D73027", "#4575B4")) +
  
  labs(
    x = "",
    y = "Control Rate (%)",
    fill = " "
  ) +
  
  theme_minimal(base_family = "sans", base_size = 14) +
  theme(
    text = element_text(family = "sans"),
    legend.position  = c(0.5, 0.95),
    legend.direction = "horizontal",
    legend.text      = element_text(size = 14),
    legend.title     = element_text(size = 14),
    axis.text.x      = element_text(size = 12, lineheight = 0.9),
    axis.text.y      = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.margin      = margin(5, 5, 5, 5)
  ) +
  
  scale_y_continuous(
    limits = c(0, 120),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  
  scale_x_discrete(
    labels = c(
      "Overall.Stage.No.significant.nausea"   = "Overall stage\nno significant nausea",
      "Acute.Stage.No.significant.nausea"     = "Acute stage\nno significant nausea",
      "Delayed.Stage.No.significant.nausea"   = "Delayed stage\nno significant nausea",
      "Overall.Stage.No.nausea.VAS.Score...5" = "Overall stage\nno nausea VAS < 5mm",
      "Acute.Stage.No.nausea.VAS.Score...5"   = "Acute stage\nno nausea VAS < 5mm",
      "Delayed.Stage.No.nausea.VAS.Score...5" = "Delayed stage\nno nausea VAS < 5mm"
    )
  )

print(supp_plot)

# ===================== ============================
pdf_out <- "/results/Extended Data Fig. 9.pdf"
ggsave(filename = pdf_out, plot = supp_plot, width = 14, height = 8, units = "in")








####### Extended Data Fig. 10
# =================== =================
library(dplyr)
library(ggplot2)

# ====================== ===========================
data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)


data <- data %>%
  filter(!Set %in% c(
    "ITT.1", "ITT.2", "ITT.3",
    "ITT.4", "ITT.5"
  ))

# ======================  ===============================
data <- data %>%
  filter(!is.na(EQ.5D.5L0), !is.na(EQ.5D.5L1)) %>%
  mutate(
    Delta_EQ5D5L = EQ.5D.5L1 - EQ.5D.5L0,
    Nausea = factor(
      Overall.Stage.No.nausea,
      levels = c(0, 1),
      labels = c(
        "Overall phase\nno nausea",
        "Overall phase\nnausea"
      )
    )
  )

# ========================= ===================
desc_table <- data %>%
  group_by(Nausea) %>%
  summarise(
    Median = median(Delta_EQ5D5L, na.rm = TRUE),
    Q1 = quantile(Delta_EQ5D5L, 0.25, na.rm = TRUE),
    Q3 = quantile(Delta_EQ5D5L, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label = sprintf(
      "Median (IQR)\n%.3f (%.3f to %.3f)",
      Median, Q1, Q3
    )
  )

# ===================== =======================
wilcox_res <- wilcox.test(Delta_EQ5D5L ~ Nausea, data = data)
p_label <- paste0(
  "P = ",
  formatC(wilcox_res$p.value, format = "f", digits = 4)
)

# ======================= ====================
p <- ggplot(data, aes(x = Nausea, y = Delta_EQ5D5L, fill = Nausea)) +
  geom_violin(trim = FALSE, alpha = 0.6, color = NA) +
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    color = "black",
    size = 0.3
  ) +
  
  geom_text(
    data = desc_table,
    aes(x = Nausea, y = -0.76, label = label),
    inherit.aes = FALSE,
    size = 3.2,
    hjust = 0.5,
    family = "sans"        
  ) +
  
  annotate(
    "text",
    x = 1.5,
    y = -0.9,
    label = p_label,
    size = 3.5,
    fontface = "italic",
    family = "sans"
  ) +
  scale_fill_manual(
    values = c(
      "Overall phase\nno nausea" = "#E41A1C",
      "Overall phase\nnausea"    = "#377EB8"
    )
  ) +
  scale_y_continuous(
    limits = c(-1.0, 0.15),
    breaks = seq(-1.0, 0.15, by = 0.2)
  ) +
  labs(
    x = NULL,
    y = "Change in EQ-5D-5L index (post − baseline)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "sans"),   
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.margin = margin(6, 6, 6, 6)
  )

# ============= ==================
p

ggsave(
  filename = "/results/Extended Data Fig. 10.pdf",
  plot = p,
  width = 6,
  height = 6,
  units = "in"
)

table(data$Nausea)








####### Extended Data Fig. 11
# ================== ======================
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(openxlsx)
library(scales)

# ============================ =====================
data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)


data <- data %>%
  filter(!Set %in% c(
    "ITT.1", "ITT.2", "ITT.3",
    "ITT.4", "ITT.5"
  ))


data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)

# ====================== ===================
observation_cols <- c("Fatigue", "Insomnia", "Constipation")

# ======================== ==================
long_data <- data %>%
  pivot_longer(
    cols = all_of(observation_cols),
    names_to = "Observation",
    values_to = "Event"
  ) %>%
  filter(!is.na(Event))

# ==================== ==================
group_summaries <- long_data %>%
  group_by(Group, Observation) %>%
  summarise(
    n       = n(),
    events  = sum(Event == 1),
    Rate    = mean(Event == 1),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(ci = list(binom.test(events, n)$conf.int)) %>%
  mutate(LCL = ci[[1]], UCL = ci[[2]]) %>%
  ungroup() %>%
  dplyr::select(-ci)

# ======================== =====================
chisq_results <- long_data %>%
  group_by(Observation) %>%
  summarise(
    test = list(chisq.test(table(Group, Event))),
    .groups = "drop"
  ) %>%
  mutate(chisq_p = map_dbl(test, ~ .x$p.value)) %>%
  mutate(
    p_label = ifelse(
      chisq_p < 1e-4,
      "< 0.0001",
      paste0("= ", formatC(chisq_p, format = "f", digits = 4))
    )
  ) %>%
  dplyr::select(Observation, chisq_p, p_label)

group_summaries <- group_summaries %>%
  left_join(chisq_results, by = "Observation")

# ============================= ========================
final_plot <- ggplot(
  group_summaries,
  aes(x = Observation, y = Rate * 100, fill = Group)
) +
  geom_col(
    position = position_dodge(width = 0.85),
    width = 0.8,
    color = "black",
    linewidth = 0.3
  ) +
  
  
  geom_errorbar(
    aes(ymin = LCL * 100, ymax = UCL * 100),
    position = position_dodge(width = 0.85),
    width = 0.2,
    linewidth = 0.7,
    color = "black"
  ) +
  
  
  geom_text(
    aes(
      label = paste0(events, "/", n, " (", sprintf("%.1f", Rate * 100), ")"),
      y = UCL * 100 + 3
    ),
    position = position_dodge(width = 0.85),
    size = 4,
    vjust = 0,
    color = "black"
  ) +
  
  
  geom_text(
    data = chisq_results,
    aes(x = Observation, y = 60, label = paste0("P ", p_label)),
    inherit.aes = FALSE,
    size = 5,
    fontface = "italic",
    color = "black"
  ) +
  
  scale_fill_manual(
    values = c(
      "True electroacupuncture" = "#D73027",
      "Sham electroacupuncture" = "#4575B4"
    )
  ) +
  
  scale_y_continuous(
    limits = c(0, 75),
    breaks = seq(0, 75, 10),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "",
    y = "Incidence (%)",
    fill = " "
  ) +
  
  theme_minimal(base_family = "sans", base_size = 14) +
  theme(
    text = element_text(family = "sans"),
    legend.position  = c(0.5, 0.92),
    legend.direction = "horizontal",
    legend.text      = element_text(size = 14),
    legend.title     = element_text(size = 14),
    axis.text.x      = element_text(size = 14),
    axis.text.y      = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.margin      = margin(5, 5, 5, 5)
  )

print(final_plot)

# ========================= ======================
ggsave(
  filename = "/results/Extended Data Fig. 11.pdf",
  plot = final_plot,
  width = 12,
  height = 8,
  units = "in"
)








###########Extended Data Fig. 13
# ===================== ===============
library(ggplot2)
library(dplyr)
library(openxlsx)

# ============================= ====================
data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)


data <- data %>%
  filter(!Set %in% c("ITT.1", "ITT.2", "ITT.3",
                     "ITT.4", "ITT.5"))

data$Blinding[data$Blinding == "TRUE"] <- "True"

data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)

# =========================== ==========================
tab <- table(data$Group, data$Blinding)

if (any(tab < 5)) {
  test_res <- fisher.test(tab)
} else {
  test_res <- chisq.test(tab)
}
p_val <- test_res$p.value
p_label <- ifelse(p_val < 0.0001, "P < 0.0001", sprintf("P = %.4f", p_val))

# =========================== ==========================
blinding_summary <- data %>%
  filter(!is.na(Blinding)) %>%
  group_by(Group, Blinding) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(
    Group_total = sum(n),                 
    Percentage = n / Group_total,
    
    label = paste0(n, "/", Group_total, " (", sprintf("%.1f", Percentage * 100), ")")
  )

# ===================== =========================
final_plot <- ggplot(blinding_summary, aes(x = Group, y = Percentage * 100, fill = Blinding)) +
  geom_col(width = 0.6, color = "black", size = 0.3) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 4.5,
    color = "black",
    family = "sans"
  ) +
  scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = c("#3983B7", "#EE9E3C", "#4DAF4A"),
    name = " "
  ) +
  labs(x = "", y = "Percentage of patients (%)") +
  theme_minimal(base_family = "sans", base_size = 14) +
  theme(
    text = element_text(family = "sans"),
    legend.position = "top",
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate(
    "text",
    x = 1.5, y = 105,
    label = p_label,
    size = 5,
    fontface = "italic",
    family = "sans"
  )

print(final_plot)

# ======================= ========================
pdf_out <- "/results/Extended Data Fig. 13.pdf"
ggsave(filename = pdf_out, plot = final_plot, width = 8, height = 6, units = "in")












##########Extended Data Fig. 14
# ========================= ==============================
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(purrr)
library(openxlsx)
library(scales)

# ============================ ======================
data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)


data <- data %>%
  filter(!Set %in% c("ITT.1", "ITT.2", "ITT.3",
                     "ITT.4", "ITT.5"))


data <- data %>%
  mutate(Adverse.events.related.to.electroacupuncture = 
           ifelse(Adverse.events.related.to.electroacupuncture == "No", 1,
                  ifelse(Adverse.events.related.to.electroacupuncture == "Bleeding", 0,
                         Adverse.events.related.to.electroacupuncture)))


data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)

# ================== ========================
observation_cols <- c("Adverse.events.related.to.electroacupuncture")

long_data <- data %>%
  pivot_longer(
    cols = observation_cols,
    names_to = "Observation",
    values_to = "Effectiveness"
  ) %>%
  filter(!is.na(Effectiveness))

# ==================== =========================
# ============= ========================
group_summaries <- long_data %>%
  group_by(Group, Observation) %>%
  summarise(
    n       = n(),
    events  = sum(Effectiveness == 0),  
    Rate    = mean(Effectiveness == 0),   
    .groups = 'drop'
  ) %>%
  rowwise() %>%
  mutate(ci = list(binom.test(events, n)$conf.int)) %>%
  mutate(LCL = ci[[1]], UCL = ci[[2]]) %>%
  ungroup() %>%
  dplyr::select(-ci)    

# ======================= ====================
chisq_results <- long_data %>%
  group_by(Observation) %>%
  summarise(test = list(chisq.test(table(Group, Effectiveness))),
            .groups = "drop") %>%
  mutate(chisq_p = map_dbl(test, ~ .x$p.value)) %>%
  dplyr::select(Observation, chisq_p) %>% 
  mutate(
    p_label = ifelse(chisq_p < 1e-4, "< 0.0001",
                     paste0("= ", formatC(chisq_p, format = "f", digits = 4)))
  )

group_summaries <- group_summaries %>%
  left_join(chisq_results, by = "Observation")


group_summaries$Observation <- "Bruising"
chisq_results$Observation <- "Bruising"

# ================= ===============================
final_plot <- ggplot(group_summaries,
                     aes(x = Observation, y = Rate*100, fill = Group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  
  
  geom_errorbar(
    aes(ymin = LCL*100, ymax = UCL*100),
    position = position_dodge(width = 0.7),
    width = 0.2,
    linewidth = 0.7,
    color = "black"
  ) +
  
  
  geom_text(
    aes(label = paste0(events, "/", n, " (", sprintf("%.1f", Rate * 100), ")"),
        y = UCL*100 + 1.5),
    position = position_dodge(width = 0.7),
    size = 4.5, vjust = 0, color = "black"
  ) +
  
  
  geom_text(
    data = chisq_results,
    aes(x = Observation, y = max(group_summaries$UCL*100) + 5, label = paste0("P ", p_label)),
    inherit.aes = FALSE,
    size = 5, fontface = "italic", color = "black"
  ) +
  
  scale_fill_manual(values = c("#D73027", "#4575B4")) +
  labs(
    x = "",
    y = "Incidence (%)",
    fill = " "
  ) +
  theme_minimal(base_family = "sans", base_size = 14) +
  theme(
    text = element_text(family = "sans"),
    legend.position  = c(0.5, 0.90),
    legend.direction = "horizontal",
    legend.text      = element_text(size = 14),
    legend.title     = element_text(size = 14),
    axis.text.x      = element_text(size = 14),
    axis.text.y      = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.margin      = margin(5, 5, 5, 5)
  ) +
  scale_y_continuous(
    limits = c(0, max(group_summaries$UCL*100) + 10),  
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  )

print(final_plot)

# ===================== =======================
pdf_out <- "/results/Extended Data Fig. 14.pdf"
ggsave(filename = pdf_out, plot = final_plot, width = 8, height = 6, units = "in")
















########### Table 1

library(tidyverse)
library(janitor)
library(gtsummary)    
library(flextable)   
library(dplyr)


data <- read.csv("/data/ECO Nature data.csv")
data <- data %>%
  filter(Set != "ITT.1") %>%
  filter(Set != "ITT.2") %>%
  filter(Set != "ITT.3") %>%
  filter(Set != "ITT.4") %>%
  filter(Set != "ITT.5")


continuous_vars <- c("Age","BMI")  
categorical_vars <- c("Acupuncture.days","Sex","ECOG","Histologic.type",
                      "Menstrual.status","Stage",
                      "Treatment.setting","Chemotherapy.regimen",
                      "NK1.RA","HT3RA5.HT3RA","Concurrent.antitumor.drugs","Antiemetic.dose")


determine_tests <- function(data, categorical_vars, group_var = "Group") {
  test_list <- list()
  
  
  test_list[continuous_vars] <- "wilcox.test"
  
  
  for (var in categorical_vars) {
    
    tbl <- table(data[[var]], data[[group_var]])
    
    
    chi_test <- tryCatch(
      chisq.test(tbl),
      error = function(e) NULL,
      warning = function(w) NULL
    )
    
    if (!is.null(chi_test)) {
      expected <- chi_test$expected
      if (any(expected < 5, na.rm = TRUE)) {
        test_list[[var]] <- "fisher.test"
      } else {
        test_list[[var]] <- "chisq.test"
      }
    } else {
      
      test_list[[var]] <- "fisher.test"
    }
  }
  
  return(test_list)
}


test_methods <- determine_tests(data, categorical_vars)


table1 <- data %>%
  select(all_of(continuous_vars), all_of(categorical_vars), Group) %>%
  tbl_summary(
    by = Group,   
    type = list(all_of(continuous_vars) ~ "continuous",
                all_of(categorical_vars) ~ "categorical"),
    statistic = list(
      all_of(continuous_vars) ~ "{median} ({min}, {max})",   
      all_of(categorical_vars) ~ "{n} / {N} ({p}%)"
    ),
    digits = list(all_of(continuous_vars) ~ 1,    
                  all_of(categorical_vars) ~ 1)
  ) %>%
  add_p(test = test_methods) %>%   
  add_n() %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("**Table 1. Baseline characteristics of patients**") %>%
  modify_footnote(
    update = everything() ~ "Median (range) for continuous variables; n/N (%) for categorical variables; p values from Wilcoxon rank-sum test, χ² test or Fisher's exact test as appropriate"
  )


print(table1)


cat("检验方法分配:\n")
for (var in names(test_methods)) {
  cat(paste0(var, ": ", test_methods[[var]], "\n"))
}


table1_flextable <- as_flex_table(table1)
file_name_word <- "/results/Table 1.docx"  
save_as_docx(table1_flextable, path = file_name_word)











#######   Extended Data Tab. 2

library(tidyverse)
library(janitor)
library(gtsummary)    
library(flextable)   
library(dplyr)


data <- read.csv("/data/ECO Nature SNP data.csv")


continuous_vars <- c("Age","BMI")  
categorical_vars <- c("Acupuncture.days","Sex","ECOG","Histologic.type",
                      "Menstrual.status","Stage",
                      "Treatment.setting","Chemotherapy.regimen",
                      "NK1.RA","HT3RA5.HT3RA","Concurrent.antitumor.drugs","Antiemetic.dose")


determine_tests <- function(data, categorical_vars, group_var = "Group") {
  test_list <- list()
  
  
  test_list[continuous_vars] <- "wilcox.test"
  
  
  for (var in categorical_vars) {
    
    tbl <- table(data[[var]], data[[group_var]])
    
    
    chi_test <- tryCatch(
      chisq.test(tbl),
      error = function(e) NULL,
      warning = function(w) NULL
    )
    
    if (!is.null(chi_test)) {
      expected <- chi_test$expected
      if (any(expected < 5, na.rm = TRUE)) {
        test_list[[var]] <- "fisher.test"
      } else {
        test_list[[var]] <- "chisq.test"
      }
    } else {
      
      test_list[[var]] <- "fisher.test"
    }
  }
  
  return(test_list)
}


test_methods <- determine_tests(data, categorical_vars)


table1 <- data %>%
  select(all_of(continuous_vars), all_of(categorical_vars), Group) %>%
  tbl_summary(
    by = Group,
    type = list(
      all_of(continuous_vars) ~ "continuous",
      all_of(categorical_vars) ~ "categorical"
    ),
    statistic = list(
      all_of(continuous_vars) ~ "{median} ({min}, {max})",
      all_of(categorical_vars) ~ "{n} / {N} ({p}%)"
    ),
    digits = list(
      all_of(continuous_vars) ~ 1,
      all_of(categorical_vars) ~ 1
    )
  ) %>%
  add_p(
    test = test_methods,
    pvalue_fun = function(x) formatC(x, format = "f", digits = 4)
  ) %>%
  add_n() %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("**Table 1. Baseline characteristics of patients**") %>%
  modify_footnote(
    update = everything() ~
      "Median (range) for continuous variables; n/N (%) for categorical variables; p values from Wilcoxon rank-sum test, χ² test or Fisher's exact test as appropriate"
  )


print(table1)


cat("检验方法分配:\n")
for (var in names(test_methods)) {
  cat(paste0(var, ": ", test_methods[[var]], "\n"))
}


table1_flextable <- as_flex_table(table1)
file_name_word <- "/results/Extended Data Tab. 2.docx"  
save_as_docx(table1_flextable, path = file_name_word)






























########## Extended Data Tab. 4

library(autoReg)
library(tidyverse)
library(ggsci)
library(survival)



data <- read.csv("/data/ECO Nature SNP data.csv")


data <- data %>%
  mutate(Chemotherapy.regimen = case_when(
    Chemotherapy.regimen == "Cisplatin.based" ~ "Carboplatin.based",
    TRUE ~ as.character(Chemotherapy.regimen)
  ))




data$Overall.Stage.No.nausea <- ifelse(data$Overall.Stage.No.nausea == 0, 1, 0)



data$Age.group <- ifelse(data$Age < 40, "<40", "≥40")

data$Age.group <- factor(data$Age.group, levels = c("≥40", "<40"))



data$BMI.group <- cut(data$BMI,
                      breaks = c(-Inf, 18.5, 24.9, Inf),
                      labels = c("<18.5", "18.5-24.9", "≥25"),
                      right = TRUE)

data$BMI.group <- factor(data$BMI.group, levels = c("18.5-24.9", "<18.5", "≥25"))



data$BMI = as.numeric(data$BMI)

data$Age.group <- as.factor(data$Age.group)


data$Histologic.type <- as.factor(data$Histologic.type)
data$Stage <- as.factor(data$Stage)
data$Menstrual.status <- as.factor(data$Menstrual.status)
data$Chemotherapy.regimen <- as.factor(data$Chemotherapy.regimen)
data$Treatment.setting <- as.factor(data$Treatment.setting)
data$NK1.RA <- as.factor(data$NK1.RA)

data$HT3RA5.HT3RA <- as.factor(data$HT3RA5.HT3RA)
data$Group <- as.factor(data$Group)

colnames(data) <- gsub("\\.", "_", colnames(data))







data$rs3755468 <- ifelse(
  data$rs3755468 %in% c("TC", "TT"),
  "TC.TT",
  data$rs3755468
)
data <- data[data$Group != 1, ]

fit <- glm(Overall_Stage_No_nausea ~ 
             
             Chemotherapy_regimen +NK1_RA 
           
           + rs3755468
           ,
           data = data,
           family = "binomial")



autoReg(fit, uni = TRUE, threshold = 1) %>% myft()

library(broom)

tidy_fit <- tidy(fit, exponentiate = TRUE, conf.int = TRUE)

tidy_fit






library(purrr)
library(broom)
library(dplyr)


vars <- c(
  "Chemotherapy_regimen",
  "NK1_RA",
  "rs3755468"
)

uni_res <- map_dfr(vars, function(v) {
  
  fml <- as.formula(
    paste("Overall_Stage_No_nausea ~", v)
  )
  
  glm(fml, data = data, family = binomial) %>%
    tidy(exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(variable = v)
})
head(uni_res)










install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("broom")
install.packages("purrr")
install.packages("openxlsx")
install.packages("scales")
install.packages("readr")
install.packages("stringr")
install.packages("forestploter")
install.packages("grid")
install.packages("marginaleffects")
install.packages("rio")
install.packages("survival")
install.packages("survminer")
install.packages("survival")

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
  filter(Name != "Wang.Yuan") %>%
  filter(Name != "Jin.Yifan") %>%
  filter(Name != "SuoerdaiZhang") %>%
  filter(Name != "QingfangLi") %>%
  filter(Name != "HonglanMa")

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
    aes(label = sprintf("%.1f", ControlRate * 100),   
        y = UCL*100 + 2),                
    position = position_dodge(width = 0.7),
    size = 4.5, vjust = 0, color = "black"
  ) +
  
  geom_text(
    data = chisq_results,
    aes(x = Observation, y = 85, label = paste0("P ", p_label)),  
    inherit.aes = FALSE,
    size = 5, color = "black"
  ) +
  
  
  annotate("text", x = 1, y = y_top*100 + 5, label = RD_label,
           size = 5, color = "black", hjust = 0.5) +
  
  scale_fill_manual(values = c("red", "blue")) +
  labs(
    x = "",
    y = "Control Rate (%)",
    fill = " ",
    caption = "Figure 2. Proportion of patients with no nausea during the overall, acute, and delayed phases in the true and sham electroacupuncture groups."
  ) +
  theme_minimal() +
  theme(
    legend.position  = c(0.5, 0.95),
    legend.direction = "horizontal",
    legend.text      = element_text(size = 16),
    legend.title     = element_text(size = 16),
    axis.text.x      = element_text(size = 14),
    axis.text.y      = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.caption     = element_text(hjust = 1, size = 12)
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

pdf_out <- "/results/Figure 2.pdf"
ggsave(filename = pdf_out, plot = final_plot, width = 12, height = 8, units = "in")

group_summaries_export <- group_summaries %>%
  mutate(
    Rate_CI = sprintf("%.1f%% (%.1f–%.1f)",
                      ControlRate * 100, LCL * 100, UCL * 100),
    p_show  = ifelse(chisq_p < 1e-4, "< 0.0001",
                     formatC(chisq_p, format = "f", digits = 4))
  ) %>%
  select(Observation, Group, n, success, ControlRate, LCL, UCL, Rate_CI, p_show)

xlsx_out <- "/results/Figure 2.xlsx"
write.xlsx(group_summaries_export, xlsx_out, asTable = TRUE)









##### Figure 3

data <- read.csv("/data/ECO Nature data.csv") %>%
  filter(Name != "Wang.Yuan", Name != "Jin.Yifan",Name != "SuoerdaiZhang",
         Name != "QingfangLi",Name != "HonglanMa") %>%
  
  
  mutate(
    
    Outcome = ifelse(Overall.Stage.No.nausea == 0, 1, 0),
    Group = factor(Group, levels = c(1, 0),
                   labels = c("True electroacupuncture", "Sham electroacupuncture")),
    
    Chemo2 = ifelse(Chemotherapy.regimen == "EC.based", "EC.based", "Carboplatin.Carboplatin.based")
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
  #filter(!Level %in% c("LZDX1", "LZDX2", "Not.applicable","Netupitant"))
  filter(!Level %in% c("LZDX1", "LZDX2"))


write.csv(final_df_out,
          "/results/Figure 3.csv",
          row.names = FALSE)


print(final_df_out)





library(dplyr)
library(readr)
library(stringr)
library(forestploter)
library(grid)


df <- read_csv("/results/Figure 3.csv")

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
    ci_col = "blue",
    ci_fill = "red",
    vert_line = FALSE
  )
)

bold_rows <- which(df2_final$Subgroup %in% c("5-HT3 RA", "BMI", "Menstrual status","Treatment", 
                                             "Chemotherapy regimen", "NK1 RA")) 


p <- edit_plot(
  p,
  gp = gpar(fontface = "bold"),
  row = bold_rows
)


pdf_out <- "/results/Figure 3.pdf"
pdf(pdf_out, width = 14, height = 7)  
grid.newpage()
grid.draw(p)
dev.off()







###Figure4

data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)

data <- data %>%
  filter(Name != "Wang.Yuan") %>%
  filter(Name != "Jin.Yifan") %>%
  filter(Name != "SuoerdaiZhang") %>%
  filter(Name != "QingfangLi") %>%
  filter(Name != "HonglanMa")


data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)


observation_cols <- c("Overall.Stage.total.control","Acute.Stage.total.control","Delayed.Stage.total.control",
                      "Overall.Stage.complete.protection","Acute.Stage.complete.protection","Delayed.Stage.complete.protection",
                      "Overall.Stage.Complete.response","Acute.Stage.Complete.response","Delayed.Stage.Complete.response")


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


y_top_for_p <- 105  

final_plot <- ggplot(group_summaries,
                     aes(x = Observation, y = ControlRate*100, fill = Group)) +  
  
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  
  
  geom_text(
    aes(label = sprintf("%.1f", ControlRate * 100),
        y = ControlRate*100 + 2),  
    position = position_dodge(width = 0.7),
    size = 4.5, vjust = 0, color = "black"
  ) +
  
  geom_text(
    data = chisq_results,
    aes(x = Observation, y = y_top_for_p, label = paste0("P ", p_label)),
    inherit.aes = FALSE,
    size = 5, color = "black"
  ) +
  
  scale_fill_manual(values = c("red", "blue")) +
  
  
  labs(
    x = "",
    y = "Control Rate (%)",
    fill = " ",
    caption = "Figure 4. Proportion of patients achieving total control, complete protection, and complete response during the overall, acute, and delayed phases in the true and sham electroacupuncture groups."
  ) +
  
  
  theme_minimal() +
  theme(
    legend.position  = c(0.5, 0.95),
    legend.direction = "horizontal",
    legend.text      = element_text(size = 16),
    legend.title     = element_text(size = 16),
    axis.text.x      = element_text(size = 14),
    axis.text.y      = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.caption     = element_text(hjust = -0.2, size = 14)
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


pdf_out <- "/results/Figure 4.pdf"
ggsave(filename = pdf_out, plot = final_plot, width = 18, height = 10, units = "in")




















### Figure 5
 
data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)
 
data <- data %>%
  filter(!Name %in% c(
    "Wang.Yuan", "Jin.Yifan", "SuoerdaiZhang",
    "QingfangLi", "HonglanMa"
  ))

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
 wilcox_res <- wilcox.test(Delta_EQ5D5L ~ Nausea, data = data)

p_label <- paste0(
  "Wilcoxon rank-sum test, P = ",
  formatC(wilcox_res$p.value, format = "f", digits = 4)
)

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
    hjust = 0.5
  ) +
  
 
  annotate(
    "text",
    x = 1.5,
    y = -0.9,
    label = p_label,
    size = 3.5
  ) +
  
  scale_fill_manual(
    values = c(
      "Overall phase\nno nausea" = "#377EB8",   
      "Overall phase\nnausea"    = "#E41A1C"   
    )
  ) +
  
  scale_y_continuous(
    limits = c(-1.0, 0.15),
    breaks = seq(-1.0, 0.15, by = 0.2)
  ) +
  
  labs(
    x = NULL,
    y = "Change in EQ-5D-5L index  (post − baseline)",
    caption = "Figure 5. a. Post-intervention EQ-5D-5L index adjusted for baseline according to nausea occurrence during the overall phase;"
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.caption = element_text(hjust = 0, size = 10)
  )

 p
 
ggsave(
  filename = "/results/Figure 5a.pdf",
  plot = p,
  width = 6,
  height = 6,
  units = "in"
)

###
 library(dplyr)
library(ggplot2)

 data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)
 
data <- data %>%
  filter(!Name %in% c(
    "Wang.Yuan", "Jin.Yifan", "SuoerdaiZhang",
    "QingfangLi", "HonglanMa"
  ))

 data <- data %>%
  filter(!is.na(VAS0), !is.na(VAS1)) %>%
  mutate(
    Delta_VAS = VAS1 - VAS0,
    Nausea = factor(
      Overall.Stage.No.nausea,
      levels = c(0, 1),
      labels = c(
        "Overall phase\nno nausea",
        "Overall phase\nnausea"
      )
    )
  )

 desc_table <- data %>%
  group_by(Nausea) %>%
  summarise(
    Median = median(Delta_VAS, na.rm = TRUE),
    Q1 = quantile(Delta_VAS, 0.25, na.rm = TRUE),
    Q3 = quantile(Delta_VAS, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label = sprintf(
      "Median (IQR)\n%.1f (%.1f to %.1f)",
      Median, Q1, Q3
    )
  )

 wilcox_res <- wilcox.test(Delta_VAS ~ Nausea, data = data)

p_label <- paste0(
  "Wilcoxon rank-sum test, P = ",
  formatC(wilcox_res$p.value, format = "f", digits = 4)
)

 p <- ggplot(data, aes(x = Nausea, y = Delta_VAS, fill = Nausea)) +
  
  
  geom_violin(trim = FALSE, alpha = 0.6, color = NA) +
  
 
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    color = "black",
    size = 0.3
  ) +
  
 
  geom_text(
    data = desc_table,
    aes(x = Nausea, y = -75, label = label),
    inherit.aes = FALSE,
    size = 3.2,
    hjust = 0.5
  ) +
  
 
  annotate(
    "text",
    x = 1.5,
    y = -85,
    label = p_label,
    size = 3.5
  ) +
  
  scale_fill_manual(
    values = c(
      "Overall phase\nno nausea" = "#377EB8",  
      "Overall phase\nnausea"    = "#E41A1C"  
    )
  ) +
  
  scale_y_continuous(
    limits = c(-100, 25),
    breaks = seq(-100, 20, by = 20)
  ) +
  
  labs(
    x = NULL,
    y = "Change in VAS score (post − baseline)",
    caption = " Figure 5. b. Post-intervention nausea severity measured by EQ VAS according to nausea occurrence during the overall phase."
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.caption = element_text(hjust = 0, size = 10)
  )


p

ggsave(
  filename = "/results/Figure 5b.pdf",
  plot = p,
  width = 6,
  height = 6,
  units = "in"
)





















### Extended Data Fig. 1

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
  filter(!Name %in% c("Wang.Yuan", "Jin.Yifan", "SuoerdaiZhang",
                      "QingfangLi", "HonglanMa", "Zhao.Yingying"))

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
    aes(label = sprintf("%.1f", ControlRate * 100),
        y = UCL*100 + 2),
    position = position_dodge(width = 0.7),
    size = 4.5, vjust = 0, color = "black"
  ) +
  
  
  geom_text(
    data = chisq_results,
    aes(x = Observation, y = 80, label = paste0("P ", p_label)),
    inherit.aes = FALSE,
    size = 5, color = "black"
  ) +
  
  
  scale_fill_manual(values = c("red", "blue")) +
  labs(
    x = "",
    y = "Control Rate (%)",
    fill = " ",
    caption = "Extended Data Fig. 1. Proportion of patients with no nausea during the overall, acute, and delayed phases in the true and sham electroacupuncture groups (per-protocol set).\nError bars represent 95% confidence intervals."
  ) +
  theme_minimal() +
  theme(
    legend.position  = c(0.5, 0.95),
    legend.direction = "horizontal",
    legend.text      = element_text(size = 16),
    legend.title     = element_text(size = 16),
    axis.text.x      = element_text(size = 14),
    axis.text.y      = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.caption     = element_text(hjust = 1, size = 12)
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


pdf_out <- "/results/Extended Data Fig. 1.pdf"
ggsave(filename = pdf_out, plot = final_plot, width = 12, height = 10, units = "in")







### Extended Data Fig. 2

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
  filter(!Name %in% c(
    "Wang.Yuan", "Jin.Yifan", "SuoerdaiZhang",
    "QingfangLi", "HonglanMa"
  ))
 data <- data %>%
  filter(is.na(Chemotherapy.TIME))


 
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
  
   ）
  geom_text(
    aes(label = sprintf("%.1f", ControlRate * 100),
        y = UCL*100 + 2),
    position = position_dodge(width = 0.7),
    size = 4.5, vjust = 0, color = "black"
  ) +
  
   
  geom_text(
    data = chisq_results,
    aes(x = Observation, y = 80, label = paste0("P ", p_label)),
    inherit.aes = FALSE,
    size = 5, color = "black"
  ) +
   
scale_fill_manual(values = c("red", "blue")) +
  labs(
    x = "",
    y = "Control Rate (%)",
    fill = " ",
    caption = "Extended Data Fig. 2. Proportion of patients with no nausea during the overall phase in the true and sham electroacupuncture groups after exclusion of advanced-stage patients with prior chemotherapy."
  ) +
  theme_minimal() +
  theme(
    legend.position  = c(0.5, 0.95),
    legend.direction = "horizontal",
    legend.text      = element_text(size = 16),
    legend.title     = element_text(size = 16),
    axis.text.x      = element_text(size = 14),
    axis.text.y      = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.caption     = element_text(hjust = 1, size = 12)
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

 pdf_out <- "/results/Extended Data Fig. 2.pdf"
 



















### Extended Data Fig. 3 6
library(marginaleffects)
library(rio)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)

data_path <- "/data/ECO Nature data.csv"
df <- read.csv(data_path, stringsAsFactors = FALSE, fileEncoding="UTF-8")

df <- df %>%
  filter(Name != "Wang.Yuan") %>%
  filter(Name != "Jin.Yifan") %>%
  filter(Name != "SuoerdaiZhang") %>%
  filter(Name != "QingfangLi") %>%
  filter(Name != "HonglanMa")


df$BMI = ifelse(df$BMI < 18.5, 1,
                ifelse(df$BMI < 25, 2, 3))
df$BMI = factor(df$BMI, levels = c(1,2,3),
                labels = c("<18.5","18.5~24.9","≥25"))

df$Sex <- factor(df$Sex)
df$ECOG <- factor(df$ECOG, levels=c(0,1), labels=c("0","1"))

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

df$Acupuncture.days <- factor(df$Acupuncture.days, levels=c(4,3,2,1))
df$Antiemetic.dose <- factor(df$Antiemetic.dose)

df$Group <- factor(df$Group,
                   levels = c(0,1),
                   labels = c("Sham",
                              "True"))

df$Center <- factor(df$Center)


df$Nausea1 <- ifelse(df$Nausea.d1 == 0, 0, 1)
df$Nausea2 <- ifelse(df$Nausea.d2 == 0, 0, 1)
df$Nausea3 <- ifelse(df$Nausea.d3 == 0, 0, 1)
df$Nausea4 <- ifelse(df$Nausea.d4 == 0, 0, 1)
df$Nausea5 <- ifelse(df$Nausea.d5 == 0, 0, 1)

df$time_to_Nausea <- apply(
  df[, c("Nausea1","Nausea2","Nausea3","Nausea4","Nausea5")], 
  1, 
  function(x){
    if(sum(x)==0) return(5)
    else return(which(x==1)[1])
  }
)

df$Nausea <- ifelse(df$time_to_Nausea < 5, 1, 0)  




fit_RD_nausea <- glm(
  Overall.Stage.No.nausea ~ Group + Center + BMI +
    Treatment.setting + Chemotherapy.regimen +
    NK1.RA + HT3RA5.HT3RA + Menstrual.status,
  data = df,
  family = binomial()
)

RD_nausea <- avg_comparisons(
  fit_RD_nausea,
  variables = "Group",
  comparison = "difference",
  type = "response",
  vcov = "HC0"
)
RD_nausea


dt <- df %>% select(Group, time_to_Nausea, Nausea)
dt <- dt[!is.na(dt$time_to_Nausea) & !is.na(dt$Nausea), ]
dt$Group <- factor(dt$Group)

km_fit <- survfit(Surv(time_to_Nausea, Nausea) ~ Group, data = dt)

ggsurvplot(
  km_fit,
  fun = "event",
  ylab = "Cumulative nausea rate",
  xlab = "Day",
  risk.table = TRUE
)

cox_fit <- coxph(Surv(time_to_Nausea, Nausea) ~ Group, data = dt)
summary(cox_fit)





library(survminer)
library(ggplot2)

dt <- df %>% select(Group, time_to_Nausea, Nausea)
dt <- dt[!is.na(dt$time_to_Nausea) & !is.na(dt$Nausea), ]
dt$Group <- factor(dt$Group)


km_fit <- survfit(Surv(time_to_Nausea, Nausea) ~ Group, data = dt)


cox_fit <- coxph(Surv(time_to_Nausea, Nausea) ~ Group, data = dt)
cox_sum <- summary(cox_fit)

HR  <- round(cox_sum$coefficients[1,2], 2)
LCL <- round(cox_sum$conf.int[,"lower .95"][1], 2)
UCL <- round(cox_sum$conf.int[,"upper .95"][1], 2)
P   <- signif(cox_sum$coefficients[1,5], 2)


P_display <- ifelse(P < 0.001, "< 0.001", paste0("= ", P))


annot_text <- paste0("HR = ", HR, " (", LCL, "-", UCL, ")\nP ", P_display)


p <- ggsurvplot(
  km_fit,
  fun = "event",
  ylab = "Cumulative nausea rate",
  xlab = "Day",
  risk.table = TRUE,
  ggtheme = theme_bw(),
  censor = FALSE
)


p$plot <- p$plot +
  annotate("text",
           x = max(dt$time_to_Nausea) * 0.60,
           y = 0.25,
           label = annot_text,
           size = 5)


print(p)


pdf("/results/Extended Data Fig. 3.pdf", width = 6, height = 6)
print(p)
dev.off()

















### Extended Data Fig. 4
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(purrr)
library(openxlsx)
library(scales)

 
data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path, stringsAsFactors = FALSE, fileEncoding="UTF-8")


 data <- data %>%
  filter(Name != "Wang.Yuan") %>%
  filter(Name != "Jin.Yifan") %>%
  filter(Name != "SuoerdaiZhang") %>%
  filter(Name != "QingfangLi") %>%
  filter(Name != "HonglanMa")

 
data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)


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



table(data$Max_Nausea_VAS)

 data <- data %>%
  mutate(
    Max_Nausea_Category = cut(
      Max_Nausea_VAS,
      breaks = c(-Inf, 0, 30, 60, 100),
      labels = c("0", "1–30", "31–60", "61–100"),
      right = TRUE,
      ordered_result = TRUE
    )
  )


table(data$Max_Nausea_Category)


 plot_data <- data %>%
  count(Group, Max_Nausea_Category) %>%
  group_by(Group) %>%
  mutate(
    Percent = n / sum(n),
    Label = ifelse(
      Percent >= 0.05,
      paste0(sprintf("%.1f", Percent * 100), "%"),
      ""
    )
  )

 nature_colors <- c(
  "0"      = "#4DAF4A",   
  "1–30"   = "#FFD92F",  
  "31–60"  = "#377EB8",  
  "61–100" = "#E41A1C"    
)

 chisq_p <- chisq.test(table(data$Group, data$Max_Nausea_Category))$p.value
p_label <- paste0(
  "Chi-square test, P = ",
  formatC(chisq_p, format = "f", digits = 4)
)

 p <- ggplot(
  plot_data,
  aes(x = Group, y = Percent * 100, fill = Max_Nausea_Category)
) +
  geom_bar(
    stat = "identity",
    width = 0.65,
    color = "black",
    size = 0.25
  ) +
  
  geom_text(
    aes(label = Label),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black"
  ) +
  
  annotate(
    "text",
    x = 1.5,
    y = 105,
    label = p_label,
    size = 3.5
  ) +
  
  scale_y_continuous(
    limits = c(0, 110),
    breaks = c(0, 25, 50, 75, 100),
    expand = expansion(mult = c(0, 0))
  ) +
  
  scale_fill_manual(
    values = nature_colors,
    name = "VAS"
  ) +
  
  labs(
    x = NULL,
    y = "Percentage of patients (%)",
    caption = "Extended Data Fig. 4. Distribution of maximum nausea severity during the 5-day follow-up in the true and sham electroacupuncture groups."
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    
    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10),
    
    axis.line    = element_line(size = 0.5),
    axis.ticks   = element_line(size = 0.5),
    
    plot.caption = element_text(
      hjust = 0,      
      size  = 10,
      margin = margin(t = 8)
    ),
    
    plot.margin  = margin(6, 6, 6, 6)
  )

p

ggsave(
  filename = "/results/Extended Data Fig. 4.pdf",
  plot = p,
  width = 6,
  height = 4,
  units = "in"
)




 






 
###Extended Data Fig. 5
 library(ggplot2)
library(dplyr)
library(scales)

 data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path, stringsAsFactors = FALSE, fileEncoding="UTF-8")
 data <- data %>%
  filter(!Name %in% c("Wang.Yuan", "Jin.Yifan", "SuoerdaiZhang",
                      "QingfangLi", "HonglanMa"))

 data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)

 data <- data %>%
  rowwise() %>%
  mutate(
    Max_Nausea_VAS = max(
      c(Nausea.d1, Nausea.d2, Nausea.d3, Nausea.d4, Nausea.d5),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

 summary_table <- data %>%
  group_by(Group) %>%
  summarise(
    Median = median(Max_Nausea_VAS, na.rm = TRUE),
    Q1     = quantile(Max_Nausea_VAS, 0.25, na.rm = TRUE),
    Q3     = quantile(Max_Nausea_VAS, 0.75, na.rm = TRUE)
  )

print(summary_table)

 label_data <- summary_table %>%
  mutate(
    label = paste0(
      "Median (IQR)\n",
      sprintf("%.0f", Median), " (",
      sprintf("%.0f", Q1), "–",
      sprintf("%.0f", Q3), ")"
    ),
    y_pos = 95   
  )

 wilcox_res <- wilcox.test(Max_Nausea_VAS ~ Group, data = data)

p_label <- paste0(
  "Wilcoxon rank-sum test, P = ",
  formatC(wilcox_res$p.value, format = "f", digits = 4)
)

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
    vjust = 1
  ) +
  
 
  annotate(
    "text",
    x = 1.5,
    y = 105,
    label = p_label,
    size = 3.5
  ) +
  
  scale_y_continuous(
    limits = c(0, 105),
    breaks = c(0, 20, 40, 60, 80, 100)
  ) +
  
  scale_fill_manual(
    values = c(
      "True electroacupuncture" = "#377EB8",
      "Sham electroacupuncture" = "#E41A1C"
    )
  ) +
  
  labs(
    x = NULL,
    y = "VAS score",
    caption = "Extended Data Fig. 5. Distribution of maximum nausea severity assessed by VAS during the 5-day follow-up. VAS, visual analogue scale; IQR, interquartile range."
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    
    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10),
    
    axis.line    = element_line(size = 0.5),
    axis.ticks   = element_line(size = 0.5),
    
    plot.caption = element_text(
      hjust = 0,
      size = 10,
      margin = margin(t = 8)
    ),
    
    plot.margin  = margin(6, 6, 6, 6)
  )

p

 ggsave(
  filename = "/results/Extended Data Fig. 5.pdf",
  plot = p,
  width = 6,
  height = 4,
  units = "in"
)









 
 
 
 
 
 
 
 
 
 
### Extended Data Fig. 7
data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)

data <- data %>%
  filter(Name != "Wang.Yuan") %>%
  filter(Name != "Jin.Yifan") %>%
  filter(Name != "SuoerdaiZhang") %>%
  filter(Name != "QingfangLi") %>%
  filter(Name != "HonglanMa")


data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)

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


y_top_for_p <- 105 

supp_plot <- ggplot(supp_group_summaries,
                    aes(x = Observation, y = ControlRate*100, fill = Group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  
  geom_text(
    aes(label = sprintf("%.1f", ControlRate * 100),
        y = ControlRate*100 + 2),
    position = position_dodge(width = 0.7),
    size = 4.5, vjust = 0, color = "black"
  ) +
  
  
  geom_text(
    data = supp_chisq_results,
    aes(x = Observation, y = y_top_for_p, label = paste0("P ", p_label)),
    inherit.aes = FALSE,
    size = 5, color = "black"
  ) +
  
  scale_fill_manual(values = c("red", "blue")) +
  
  labs(
    x = "",
    y = "Control Rate (%)",
    fill = " ",
    caption = "Extended Data Fig. 7. Proportion of patients achieving no significant nausea and no nausea (VAS score < 5 mm) during the overall, acute, and delayed phases \n in the true and sham electroacupuncture groups."
  )+
  
  theme_minimal() +
  theme(
    legend.position  = c(0.5, 0.95),
    legend.direction = "horizontal",
    legend.text      = element_text(size = 16),
    legend.title     = element_text(size = 16),
    axis.text.x      = element_text(size = 12),
    axis.text.y      = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.caption     = element_text(hjust = 0, size = 14)
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


pdf_out <- "/results/Extended Data Fig. 7.pdf"
ggsave(filename = pdf_out, plot = supp_plot, width = 14, height = 8, units = "in")

 












###Extended Data Fig. 8
library(dplyr)
library(ggplot2)

 data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)

 
data <- data %>%
  filter(!Name %in% c(
    "Wang.Yuan", "Jin.Yifan", "SuoerdaiZhang",
    "QingfangLi", "HonglanMa"
  ))

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

 wilcox_res <- wilcox.test(Delta_EQ5D5L ~ Nausea, data = data)

p_label <- paste0(
  "Wilcoxon rank-sum test, P = ",
  formatC(wilcox_res$p.value, format = "f", digits = 4)
)

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
    hjust = 0.5
  ) +
  
   
  annotate(
    "text",
    x = 1.5,
    y = -0.9,
    label = p_label,
    size = 3.5
  ) +
  
  scale_fill_manual(
    values = c(
      "Overall phase\nno nausea" = "#377EB8",   
      "Overall phase\nnausea"    = "#E41A1C"   
    )
  ) +
  
  scale_y_continuous(
    limits = c(-1.0, 0.15),
    breaks = seq(-1.0, 0.15, by = 0.2)
  ) +
  
  labs(
    x = NULL,
    y = "Change in EQ-5D-5L index (post − baseline)",
    caption = "Extended Data Fig. 8. Distribution of change in EQ-5D-5L index according to nausea occurrence during the overall phase. IQR, interquartile range; EO-5D-5L: EuroQol-5-dimension 5-level."
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.caption = element_text(hjust = 0, size = 10)
  )

 p

ggsave(
  filename = "/results/Extended Data Fig. 8.pdf",
  plot = p,
  width = 6,
  height = 6,
  units = "in"
)












###Extended Data Fig. 9
 library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(openxlsx)
library(scales)

 data_path <-  "/data/ECO Nature data.csv"
data <- read.csv(data_path)

 
data <- data %>%
  filter(!Name %in% c(
    "Wang.Yuan", "Jin.Yifan", "SuoerdaiZhang",
    "QingfangLi", "HonglanMa"
  ))

 
data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)

 observation_cols <- c(
  "Fatigue",
  "Insomnia",
  "Constipation"
)

 long_data <- data %>%
  pivot_longer(
    cols = all_of(observation_cols),
    names_to = "Observation",
    values_to = "Event"
  ) %>%
  filter(!is.na(Event))

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

 final_plot <- ggplot(
  group_summaries,
  aes(x = Observation, y = Rate * 100, fill = Group)
) +
  
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.7
  ) +
  
   geom_text(
    aes(label = paste0(events, "/", n)),
    position = position_dodge(width = 0.7),
    vjust = 1.5,
    size = 5,
    color = "black"
  ) +
  
   geom_text(
    aes(
      label = sprintf("%.1f", Rate * 100),
      y = Rate * 100 + 2
    ),
    position = position_dodge(width = 0.7),
    size = 5,
    vjust = 0,
    color = "black"
  ) +
  
   geom_text(
    data = chisq_results,
    aes(x = Observation, y = 52, label = paste0("P ", p_label)),
    inherit.aes = FALSE,
    size = 5,
    color = "black"
  ) +
  
  scale_fill_manual(
    values = c(
      "True electroacupuncture" = "#377EB8",
      "Sham electroacupuncture" = "#E41A1C"
    )
  ) +
  
  scale_y_continuous(
    limits = c(0, 65),
    breaks = seq(0, 65, 10),
    expand = c(0, 0)
  ) +
  
  scale_x_discrete(
    labels = c(
      "Fatigue" = "Fatigue",
      "Insomnia" = "Insomnia",
      "Constipation" = "Constipation"
    )
  ) +
  
  labs(
    x = "",
    y = "Incidence (%)",
    fill = " ",
    caption = "Extended Data Fig. 9. Incidence of fatigue, insomnia, and constipation in the true and sham electroacupuncture groups."
  ) +
  
  theme_minimal() +
  theme(
    legend.position  = c(0.5, 0.92),
    legend.direction = "horizontal",
    legend.text      = element_text(size = 16),
    legend.title     = element_text(size = 16),
    axis.text.x      = element_text(size = 14),
    axis.text.y      = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.caption     = element_text(hjust = 0, size = 14)
  )

print(final_plot)

 ggsave(
  filename = "/results/Extended Data Fig. 3.pdf",
  plot = final_plot,
  width = 12,
  height = 8,
  units = "in"
)















###Extended Data Fig. 10

library(ggplot2)
library(dplyr)
library(openxlsx)


data_path <- "/data/ECO Nature data.csv"
data <- read.csv(data_path)


data <- data %>%
  filter(Name != "Wang.Yuan") %>%
  filter(Name != "Jin.Yifan") %>%
  filter(Name != "SuoerdaiZhang") %>%
  filter(Name != "QingfangLi") %>%
  filter(Name != "HonglanMa")


data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("True electroacupuncture", "Sham electroacupuncture")
)

tab <- table(data$Group, data$Blinding)

if(any(tab < 5)){
  test_res <- fisher.test(tab)
} else {
  test_res <- chisq.test(tab)
}
p_val <- test_res$p.value
p_label <- ifelse(p_val < 0.0001, "P < 0.0001", sprintf("P = %.4f", p_val))


blinding_summary <- data %>%
  filter(!is.na(Blinding)) %>%
  group_by(Group, Blinding) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(Percentage = n / sum(n))

final_plot <- ggplot(blinding_summary, aes(x = Group, y = Percentage*100, fill = Blinding)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", Percentage*100)),
            position = position_stack(vjust = 0.5), color = "white", size = 5) +
  scale_y_continuous(
    limits = c(0, 110), 
    breaks = seq(0, 100, 20), 
    expand = c(0,0)
  ) +
  scale_fill_manual(values = c("blue", "red","#4DAF4A")) +
  labs(
    x = "",
    y = "Percentage of patients (%)",
    fill = " ",
    caption = "Extended Data Fig. 10. Blinding assessment in the true and sham electroacupuncture groups."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.caption     = element_text(hjust = 0, size = 12) 
  ) +
  
  annotate("text", x = 1.5, y = 105,
           label = p_label,
           size = 5, color = "black")

print(final_plot)


pdf_out <- "/results/Extended Data Fig. 10.pdf"
ggsave(filename = pdf_out, plot = final_plot, width = 8, height = 6, units = "in")

 


















#######Extended Data Fig. 11
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
  filter(Name != "Wang.Yuan") %>%
  filter(Name != "Jin.Yifan") %>%
  filter(Name != "SuoerdaiZhang") %>%
  filter(Name != "QingfangLi") %>%
  filter(Name != "HonglanMa")

 
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

 observation_cols <- c("Adverse.events.related.to.electroacupuncture")

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
    events  = sum(Effectiveness == 0),   
    Rate    = mean(Effectiveness == 0),   
    .groups = 'drop'
  ) %>%
  rowwise() %>%
  mutate(ci = list(binom.test(events, n)$conf.int)) %>%
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
    p_label = ifelse(chisq_p < 1e-4, "< 0.0001",
                     paste0("= ", formatC(chisq_p, format = "f", digits = 4)))
  )

group_summaries <- group_summaries %>%
  left_join(chisq_results, by = "Observation")

 final_plot <- ggplot(group_summaries,
                     aes(x = "Bruising ", y = Rate*100, fill = Group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +  
  
  
  geom_text(
    aes(label = paste0(events, "/", n)),
    position = position_dodge(width = 0.7),
    vjust = 1.5, color = "black", size = 5
  ) +
  
  
  geom_text(
    aes(label = sprintf("%.1f", Rate * 100),   
        y = Rate*100 + 2),
    position = position_dodge(width = 0.7),
    size = 5, vjust = 0, color = "black"
  ) +
  
  
  
  geom_text(
    data = chisq_results,
    aes(x = "Bruising ", y = 25, label = paste0("P ", p_label)),
    inherit.aes = FALSE,
    size = 5, color = "black"
  ) +
  
  scale_fill_manual(values = c("blue", "red")) +
  labs(
    x = "",
    y = "Incidence (%)",
    fill = " ",
    caption = "Extended Data Fig. 11. Incidence of electroacupuncture-related adverse event in the true and sham electroacupuncture groups."
  ) +
  theme_minimal() +
  theme(
    legend.position  = c(0.5, 0.90),
    legend.direction = "horizontal",
    legend.text      = element_text(size = 16),
    legend.title     = element_text(size = 16),
    axis.text.x      = element_text(size = 14),
    axis.text.y      = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.caption     = element_text(hjust = 0, size = 14)
  ) +
  scale_y_continuous(
    limits = c(0, 35),
    breaks = seq(0, 35, 10),
    expand = c(0, 0)
  )

print(final_plot)

pdf_out <- "/results/Extended Data Fig. 11.pdf"
ggsave(filename = pdf_out, plot = final_plot, width = 12, height = 8, units = "in")

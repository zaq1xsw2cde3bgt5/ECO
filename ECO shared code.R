##Figure 2 
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(purrr)
library(openxlsx)
library(scales)

data_path <- "D:/Desktop/XXX.csv" # file path
data <- read.csv(data_path)

  


 
data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("A", "B")
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
  
 
  annotate("segment", x = x_left, xend = x_left, 
           y = y_true*100 + 4, yend = y_top*100,
           color = "black", linewidth = 0.6) +
  annotate("segment", x = x_right, xend = x_right, 
           y = y_sham*100 + 4, yend = y_top*100,
           color = "black", linewidth = 0.6) +
  annotate("segment", x = x_left, xend = x_right, 
           y = y_top*100, yend = y_top*100,
           color = "black", linewidth = 0.6) +
  annotate("text", x = 1, y = y_top*100 + 5, label = RD_label,
           size = 5, color = "black", hjust = 0.5) +
  
 
  scale_fill_manual(values = c("blue", "red")) +
  labs(
    x = "",
    y = "Control Rate (%)",
    fill = " ",
    caption = "Figure 2. Proportion of patients with no nausea during the overall, acute, and delayed phases in the true and sham electroacupuncture groups. \n CI, confidence interval."
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




 pdf_out <- "D:/Desktop/Figure 2.pdf"
ggsave(filename = pdf_out, plot = final_plot, width = 12, height = 8, units = "in")

group_summaries_export <- group_summaries %>%
  mutate(
    Rate_CI = sprintf("%.1f%% (%.1f–%.1f)",
                      ControlRate * 100, LCL * 100, UCL * 100),
    p_show  = ifelse(chisq_p < 1e-4, "< 0.0001",
                     formatC(chisq_p, format = "f", digits = 4))
  ) %>%
  select(Observation, Group, n, success, ControlRate, LCL, UCL, Rate_CI, p_show)

xlsx_out <- "D:/Desktop/Figure 2.xlsx"
write.xlsx(group_summaries_export, xlsx_out, asTable = TRUE)




















##3 Figure 3

library(dplyr)
library(readr)
library(stringr)
library(forestploter)
library(grid)

 
df <- read_csv("D:/Desktop/XXX.csv")

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
  Subgroup = c("5-HT3 RA", "BMI", "Menstrual status","Treatment", "Chemotherapy regimen","NK1 RA"),  # 5-HT3 RA放到第一个
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
  headers[1,], df2 %>% filter(grepl("^   Palonosetron", Subgroup) | grepl("^   Ondansetron", Subgroup)), # 5-HT3 RA
  headers[2,], df2_bmi,  # BMI
  headers[3,], df2 %>% filter(grepl("^   Premenopausal", Subgroup) | grepl("^   Postmenopausal", Subgroup)),  # Menstrual status
  headers[4,], df2 %>% filter(grepl("^   Neoadjuvant", Subgroup) | grepl("^   Adjuvant", Subgroup) | grepl("^   Palliative", Subgroup)), # Treatment
  headers[5,], df2 %>% filter(grepl("^   EC-based", Subgroup) | grepl("^   Carboplatin", Subgroup)), # Chemo regimen
  headers[6,], df2 %>% filter(grepl("^   Aprepitant", Subgroup) | grepl("^   Fosaprepitant", Subgroup) | grepl("^   Netupitant", Subgroup)) # NK1 RA
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
    ci_col = "#3983B7",
    ci_fill = "#EE9E3C",
    vert_line = FALSE
  )
)

 bold_rows <- which(df2_final$Subgroup %in% c("5-HT3 RA", "BMI", "Menstrual status","Treatment", 
                                             "Chemotherapy regimen", "NK1 RA"))  # 更新顺序

 
p <- edit_plot(
  p,
  gp = gpar(fontface = "bold"),
  row = bold_rows
)

 pdf_out <- "D:/Desktop/Figure 3.pdf"
pdf(pdf_out, width = 14, height = 7)  
grid.newpage()
grid.draw(p)
dev.off()


print(p)








### Figuer 4
 library(ggplot2)    
library(dplyr)     
library(tidyr)    
library(broom)    
library(purrr)     
library(openxlsx)  
library(scales)    

 data_path <- "D:/Desktop/XXX.csv"
data <- read.csv(data_path)

 
data$Group <- factor(
  data$Group,
  levels = c(1, 0),
  labels = c("A", "B")
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

 y_top_for_p <- 110   

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
    aes(x = Observation, y = y_top_for_p, label = paste0("P ", p_label)),
    inherit.aes = FALSE,
    size = 5, color = "black"
  ) +
   
  scale_fill_manual(values = c("#EE9E3C", "#3983B7")) +
  
 
  labs(
    x = "",
    y = "Control Rate (%)",
    fill = " ",
    caption = "Figure 4. Proportion of patients achieving total control, complete protection, and complete response during the overall, acute, and delayed phases in the true and sham electroacupuncture groups.\nError bars represent 95% confidence intervals."
  ) +
 
  theme_minimal() +
  theme(
    legend.position  = c(0.5, 0.95),
    legend.direction = "horizontal",
    legend.text      = element_text(size = 16),
    legend.title     = element_text(size = 16),
    axis.text.x      = element_text(size = 13, lineheight = 0.9),
    axis.text.y      = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    plot.caption     = element_text(hjust = -0.1, size = 13)
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


 pdf_out <- "D:/Desktop/Figure 4.pdf"
ggsave(filename = pdf_out, plot = final_plot, width = 18, height = 10, units = "in")




group_summaries_export <- group_summaries %>%
  mutate(
    Rate_CI = sprintf("%.1f%% (%.1f–%.1f)",
                      ControlRate * 100, LCL * 100, UCL * 100),
    p_show  = ifelse(chisq_p < 1e-4, "< 0.0001",
                     formatC(chisq_p, format = "f", digits = 4))
  ) %>%
  select(Observation, Group, n, success, ControlRate, LCL, UCL, Rate_CI, p_show)

xlsx_out <- "D:/Figure 4.xlsx"
write.xlsx(group_summaries_export, xlsx_out, asTable = TRUE)




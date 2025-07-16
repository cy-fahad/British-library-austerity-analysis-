# British-library-austerity-analysis-
Analysis of 25 years of British Library funding data (1998-2023) examining institutional resilience through fiscal austerity. Demonstrates policy impact assessment, financial trend analysis, and organizational adaptation strategies using R and data visualization.

This project analyzes how cultural institutions adapt their funding strategies during prolonged fiscal austerity, using the British Library as a case study. Through comprehensive data analysis of 25 years of funding data (1998-2023), the study reveals that despite recent "recovery," government funding remains 25% below historical peak levels, forcing significant institutional adaptation including revenue diversification and operational changes.



# libraries ----
library(tidyverse)
library(patchwork)
library(scales)
library(viridis)

# 
# Data source: TidyTuesday 2025-07-15
tuesdata <- tidytuesdayR::tt_load('2025-07-15')
bl_funding <- tuesdata$bl_funding

# Data preparation and feature engineering ----
bl_metrics <- bl_funding %>%
  mutate(
    # Define analytical periods
    period = case_when(
      year <= 2007 ~ "Pre-Crisis",
      year >= 2008 & year <= 2015 ~ "Austerity Era", 
      year >= 2016 ~ "Recovery Era"
    ),
    
    # Calculate revenue diversification index
    # Using 1 - Herfindahl-Hirschman Index for main revenue sources
    total_non_other = gia_gbp_millions + voluntary_gbp_millions + 
                      investment_gbp_millions + services_gbp_millions,
    gia_share = (gia_gbp_millions / total_non_other)^2,
    vol_share = (voluntary_gbp_millions / total_non_other)^2,
    inv_share = (investment_gbp_millions / total_non_other)^2,
    serv_share = (services_gbp_millions / total_non_other)^2,
    hhi = gia_share + vol_share + inv_share + serv_share,
    diversification_index = 1 - hhi,
    
    # Calculate additional metrics
    government_dependency = gia_gbp_millions / nominal_gbp_millions,
    real_change_pct = (total_y2000_gbp_millions - lag(total_y2000_gbp_millions)) / 
                      lag(total_y2000_gbp_millions) * 100
  )

# Key summary statistics ----
cat("=== BRITISH LIBRARY FUNDING ANALYSIS ===\n")
cat("Data Range:", min(bl_funding$year), "-", max(bl_funding$year), "\n")
cat("Total Observations:", nrow(bl_funding), "\n\n")

# Current vs Peak Analysis
current_gia_pct <- bl_funding$gia_as_percent_of_peak_gia[bl_funding$year == 2023]
cat("Key Finding: 2023 government funding is", 
    round((1 - current_gia_pct) * 100, 1), 
    "% below historical peak\n\n")

# Period-based analysis
period_summary <- bl_metrics %>%
  group_by(period) %>%
  summarise(
    avg_total_funding = round(mean(nominal_gbp_millions), 1),
    avg_government_pct = round(mean(government_dependency) * 100, 1),
    avg_diversification = round(mean(diversification_index), 3),
    .groups = "drop"
  )

print("Summary by Period:")
print(period_summary)

#Visualization 
# Panel A: Government Funding Recovery
p1 <- bl_metrics %>%
  filter(!is.na(gia_as_percent_of_peak_gia)) %>%
  ggplot(aes(x = year, y = gia_as_percent_of_peak_gia)) +
  geom_line(color = "#2c3e50", linewidth = 1.2) +
  geom_area(alpha = 0.3, fill = "#3498db") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_y_continuous(labels = percent_format(), limits = c(0.65, 1.05)) +
  annotate("text", x = 2020, y = 0.95, label = "Peak Level", 
           color = "red", size = 3) +
  labs(title = "A) Government Funding Recovery",
       subtitle = "Still 25% below historical peak",
       y = "% of Peak GIA", x = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"))

# Panel B: Revenue Diversification Index
p2 <- bl_metrics %>%
  ggplot(aes(x = year, y = diversification_index)) +
  geom_line(color = "#8e44ad", linewidth = 1.2) +
  geom_area(alpha = 0.3, fill = "#9b59b6") +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2, color = "#2c3e50") +
  scale_y_continuous(limits = c(0, 0.4)) +
  labs(title = "B) Revenue Diversification",
       subtitle = "Higher values = less government dependent",
       y = "Diversification Index", x = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"))

# Panel C: Services Income Decline
p3 <- bl_funding %>%
  ggplot(aes(x = year, y = services_gbp_millions)) +
  geom_line(color = "#e74c3c", linewidth = 1.2) +
  geom_area(alpha = 0.3, fill = "#e67e22") +
  annotate("text", x = 2010, y = 25, label = "50% decline\nsince peak", 
           color = "#c0392b", size = 3.5) +
  labs(title = "C) Commercial Services Revenue",
       subtitle = "Mysterious decline despite digitization",
       y = "Services Income (£M)", x = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"))

# Panel D: Real vs Nominal Funding
p4 <- bl_funding %>%
  select(year, nominal_gbp_millions, total_y2000_gbp_millions) %>%
  pivot_longer(cols = -year, names_to = "type", values_to = "amount") %>%
  mutate(type = if_else(type == "nominal_gbp_millions", "Nominal", "Real (2000 £)")) %>%
  ggplot(aes(x = year, y = amount, color = type)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("#27ae60", "#f39c12")) +
  labs(title = "D) Purchasing Power Erosion",
       subtitle = "Inflation's hidden impact on capacity",
       y = "Funding (£M)", x = "Year", color = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom")

# Combine into executive dashboard
dashboard <- (p1 + p2) / (p3 + p4)
dashboard <- dashboard + plot_annotation(
  title = "British Library Funding: 25 Years of Institutional Adaptation (1998-2023)",
  subtitle = "How cultural institutions navigate austerity and changing revenue landscapes",
  caption = "Data: TidyTuesday 2025-07-15 | Analysis demonstrates policy impact assessment and organizational resilience research",
  theme = theme(plot.title = element_text(size = 16, face = "bold"),
                plot.subtitle = element_text(size = 12),
                plot.caption = element_text(size = 10, hjust = 0))
)

# Display results
print(dashboard)

# Save outputs ----
if (!dir.exists("outputs/figures")) {
  dir.create("outputs/figures", recursive = TRUE)
}

ggsave("outputs/figures/british_library_dashboard.png", dashboard, 
       width = 12, height = 8, dpi = 300, bg = "white")

ggsave("outputs/figures/government_funding_recovery.png", p1, 
       width = 8, height = 6, dpi = 300, bg = "white")

# Save processed data
if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

write_csv(bl_metrics, "data/processed/bl_metrics.csv")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Dashboard saved to: outputs/figures/british_library_dashboard.png\n")
cat("Processed data saved to: data/processed/bl_metrics.csv\n")
cat("\nKey insights for policymakers:\n")
cat("1. Government funding recovery is incomplete (25% below peak)\n")
cat("2. Institutions are diversifying revenue but with mixed success\n") 
cat("3. Commercial services revenue declined unexpectedly\n")
cat("4. Inflation compounds the funding challenges\n")

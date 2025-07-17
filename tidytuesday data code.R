install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-07-15')

library(tidyverse)  # For data manipulation and visualization
library(tidytuesdayR)

# Extract the dataset
bl_funding <- tuesdata$bl_funding

glimpse(bl_funding)
head(bl_funding)
summary(bl_funding)

# Let's look at the data range and check for missing values
range(bl_funding$year)
colSums(is.na(bl_funding))

# Create a long format for easier plotting of funding sources
bl_funding_long <- bl_funding %>%
  select(year, gia_gbp_millions, voluntary_gbp_millions, 
         investment_gbp_millions, services_gbp_millions, other_gbp_millions) %>%
  pivot_longer(cols = -year, 
               names_to = "funding_source", 
               values_to = "amount_millions") %>%
  mutate(funding_source = str_remove(funding_source, "_gbp_millions"))


# Let's see the long format data first
head(bl_funding_long, 10)
tail(bl_funding_long, 10)

# Create a stacked area chart showing funding composition over time
p1 <- bl_funding_long %>%
  ggplot(aes(x = year, y = amount_millions, fill = funding_source)) +
  geom_area(alpha = 0.7) +
  scale_fill_viridis_d(name = "Funding Source",
                       labels = c("GIA (Government)", "Investment", "Other", 
                                  "Services", "Voluntary")) +
  labs(title = "British Library Funding Sources Over Time (1998-2023)",
       subtitle = "Stacked area chart showing composition of funding",
       x = "Year",
       y = "Funding Amount (£ millions)",
       caption = "Data: TidyTuesday 2025-07-15") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)

# Let's also look at just the total funding trend
p2 <- bl_funding %>%
  ggplot(aes(x = year, y = nominal_gbp_millions)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Total British Library Funding Over Time",
       subtitle = "Nominal values in millions of pounds",
       x = "Year",
       y = "Total Funding (£ millions)") +
  theme_minimal()

print(p2)


# Let's examine what happened around 2006 and compare nominal vs real funding
p3 <- bl_funding %>%
  select(year, nominal_gbp_millions, total_y2000_gbp_millions) %>%
  pivot_longer(cols = -year, names_to = "adjustment", values_to = "amount") %>%
  mutate(adjustment = if_else(adjustment == "nominal_gbp_millions", 
                              "Nominal (Current £)", 
                              "Real (2000 £)")) %>%
  ggplot(aes(x = year, y = amount, color = adjustment)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("steelblue", "darkred")) +
  labs(title = "British Library Funding: Nominal vs Real Values",
       subtitle = "Comparing current pounds vs inflation-adjusted (2000 baseline)",
       x = "Year",
       y = "Funding (£ millions)",
       color = "Value Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p3)

# Let's also look at GIA as percentage of peak to understand funding recovery
p4 <- bl_funding %>%
  ggplot(aes(x = year, y = gia_as_percent_of_peak_gia)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "darkgreen", size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Government Funding Recovery: GIA as % of Peak",
       subtitle = "How close is current government funding to its historical peak?",
       x = "Year",
       y = "GIA as % of Peak GIA",
       caption = "Red line shows 100% (peak level)") +
  theme_minimal()

print(p4)

# Let's look at the proportion of different funding sources over time
bl_funding_proportions <- bl_funding %>%
  mutate(
    gia_prop = gia_gbp_millions / nominal_gbp_millions,
    voluntary_prop = voluntary_gbp_millions / nominal_gbp_millions,
    services_prop = services_gbp_millions / nominal_gbp_millions,
    investment_prop = investment_gbp_millions / nominal_gbp_millions,
    other_prop = other_gbp_millions / nominal_gbp_millions
  ) %>%
  select(year, ends_with("_prop")) %>%
  pivot_longer(cols = -year, names_to = "source", values_to = "proportion") %>%
  mutate(source = str_remove(source, "_prop"),
         source = case_when(
           source == "gia" ~ "Government (GIA)",
           source == "voluntary" ~ "Voluntary/Donations", 
           source == "services" ~ "Services Income",
           source == "investment" ~ "Investment Income",
           source == "other" ~ "Other"
         ))

# Create a stacked percentage chart
p5 <- bl_funding_proportions %>%
  ggplot(aes(x = year, y = proportion, fill = source)) +
  geom_area(position = "stack") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis_d(name = "Funding Source") +
  labs(title = "British Library Funding Sources: Changing Composition",
       subtitle = "Percentage breakdown showing diversification over time",
       x = "Year",
       y = "Percentage of Total Funding") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p5)

# Let's also look at absolute growth in non-government sources
p6 <- bl_funding %>%
  select(year, voluntary_gbp_millions, services_gbp_millions, investment_gbp_millions) %>%
  pivot_longer(cols = -year, names_to = "source", values_to = "amount") %>%
  mutate(source = str_remove(source, "_gbp_millions"),
         source = str_to_title(source)) %>%
  ggplot(aes(x = year, y = amount, color = source)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Dark2", name = "Income Source") +
  labs(title = "Non-Government Income Streams Over Time",
       subtitle = "How has the British Library diversified its funding?",
       x = "Year", 
       y = "Amount (£ millions)") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p6)


# Let's create the foundation for your portfolio project
library(tidyverse)
library(patchwork)
library(scales)
library(viridis)

# Create the core analysis that will anchor your portfolio
# This will be your "hero" visualization

# 1. Calculate key metrics for the dashboard
bl_metrics <- bl_funding %>%
  mutate(
    period = case_when(
      year <= 2007 ~ "Pre-Crisis",
      year >= 2008 & year <= 2015 ~ "Austerity Era", 
      year >= 2016 ~ "Recovery Era"
    ),
    # Diversification index (1 - Herfindahl-Hirschman Index)
    total_non_other = gia_gbp_millions + voluntary_gbp_millions + 
      investment_gbp_millions + services_gbp_millions,
    gia_share = (gia_gbp_millions / total_non_other)^2,
    vol_share = (voluntary_gbp_millions / total_non_other)^2,
    inv_share = (investment_gbp_millions / total_non_other)^2,
    serv_share = (services_gbp_millions / total_non_other)^2,
    hhi = gia_share + vol_share + inv_share + serv_share,
    diversification_index = 1 - hhi
  )

# First, let's check our data
summary(bl_metrics$gia_as_percent_of_peak_gia)
sum(is.na(bl_metrics$gia_as_percent_of_peak_gia))

# Create the plot with updated syntax and handle any missing values
p1 <- bl_metrics %>%
  filter(!is.na(gia_as_percent_of_peak_gia)) %>%  # Remove any NA values
  ggplot(aes(x = year, y = gia_as_percent_of_peak_gia)) +
  geom_line(color = "#2c3e50", linewidth = 1.2) +  # Changed from size to linewidth
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

print(p1)


# Panel A - Government Funding Recovery (already created)
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

# Panel B - Revenue Diversification Index
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

# Panel C - Services Income Decline
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

# Panel D - Real vs Nominal Funding
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

# Combine into dashboard
dashboard <- (p1 + p2) / (p3 + p4)
dashboard <- dashboard + plot_annotation(
  title = "British Library Funding: 25 Years of Institutional Adaptation (1998-2023)",
  subtitle = "How cultural institutions navigate austerity and changing revenue landscapes",
  caption = "Data: TidyTuesday 2025-07-15 | Analysis demonstrates policy impact assessment and organizational resilience research",
  theme = theme(plot.title = element_text(size = 16, face = "bold"),
                plot.subtitle = element_text(size = 12),
                plot.caption = element_text(size = 10, hjust = 0))
)

print(dashboard)


# Create the directory structure and save your work
dir.create("british-library-austerity-analysis")
dir.create("british-library-austerity-analysis/data")
dir.create("british-library-austerity-analysis/data/raw")
dir.create("british-library-austerity-analysis/data/processed")
dir.create("british-library-austerity-analysis/scripts")
dir.create("british-library-austerity-analysis/outputs")
dir.create("british-library-austerity-analysis/outputs/figures")

# Save the dashboard
ggsave("british-library-austerity-analysis/outputs/figures/british_library_dashboard.png", 
       dashboard, width = 12, height = 8, dpi = 300, bg = "white")

# Save the processed data
write_csv(bl_metrics, "british-library-austerity-analysis/data/processed/bl_metrics.csv")
write_csv(bl_funding, "british-library-austerity-analysis/data/raw/bl_funding.csv")

# Save individual plots for flexibility
ggsave("british-library-austerity-analysis/outputs/figures/government_funding_recovery.png", 
       p1, width = 8, height = 6, dpi = 300, bg = "white")

print("Files saved! Check your working directory.")
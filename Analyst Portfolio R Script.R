#+ Analyst Portfolio
#+ Due May 6
#+ SIS 750

# Intro ----------------------------------------------------------------------
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggrepel)
library(ggtext)
library(scales)
library(broom)
library(readxl)

# Code Chunk -----------------------------------------------------------------
#Loading Relevant Data
trump <- read_csv("trump_approval.csv")

trump <- trump |>
  mutate(model_date = mdy(modeldate))

label_data <- trump |> 
  filter(model_date == max(model_date))

#The Chunk Itself
ggplot(trump, aes(x = model_date, y = approve)) +
  geom_ribbon(aes(ymin = approve_lo, ymax = approve_hi), fill = "#3B9C9C", alpha = 0.3, linetype = "dotted") +
  geom_line(aes(y = approve), color = "#3B9C9C", linewidth = 0.9) +
  geom_ribbon(aes(ymin = disapprove_lo, ymax = disapprove_hi), fill = "#F87217", alpha = 0.3, linetype = "dotted") +
  geom_line(aes(y = disapprove), color = "#F87217", linewidth = 0.9) +
  geom_line(aes(y = approve_hi), color = "#3B9C9C", linetype = "dotted") +
  geom_line(aes(y = approve_lo), color = "#3B9C9C", linetype = "dotted") +
  geom_line(aes(y = disapprove_hi), color = "#F87217", linetype = "dotted") +
  geom_line(aes(y = disapprove_lo), color = "#F87217", linetype = "dotted") +
  scale_color_manual(values = c("approve" = "#3B9C9C", "disapprove" = "#F87217")) +
  geom_hline(yintercept = 50, linetype = "solid", color = "black", linewidth = 0.5) +
  scale_y_continuous(
    limits = c(30, 70),
    labels = label_number(accuracy = 0.1, suffix = "%"),
    expand = expansion(mult = c(0, .1))
  ) + 
  scale_x_date(
    breaks = "3 months",
    labels = label_date(format = "%b"),
    limits = c(ymd("2025-01-21"), ymd("2026-02-11")),
    expand = expansion(mult = c(0, .1)) 
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = 'Approval',
    title = "Do Americans approve or disapprove of Donald Trump?",
    subtitle = "An updating polling average of Donald Trump's approval rating and disapproval rating in his second term, accounting for each poll's quality, recency, sample size, and partisan lean",
    caption = 'SILVER\nBULLETIN'
  ) +
  theme(
      panel.grid.major = element_line(color = "lightgrey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.margin = margin(t = 10, r = 30, b = 10, l = 10),
      plot.title = element_text(face = "bold", size = 18),
      plot.caption = element_text(face = "bold"),
    ) +
  geom_text(data = label_data, 
            aes(x = model_date, y = disapprove),
            label = "Disapprove\n55.5%", color = "#f87217", hjust = -0.1) +
  geom_text(data = label_data, 
            aes(x = model_date, y = approve),
            label = "Approve\n41.1%", color = "#3b9c96", hjust = -0.1) +
  coord_cartesian(clip = "off")

# Original Work 1 ------------------------------------------------------------
#Loading Relevant Data
UCDP <- readxl::read_excel("UCDP_conflict.xlsx", na = c("N/A", "-99"))

region_data <- UCDP |>
  mutate(region_label = case_when(
    region == 1 ~ "Europe",
    region == 2 ~ "The Middle East",
    region == 3 ~ "Asia",
    region == 4 ~ "Africa",
    region == 5 ~ "The Americas"
  )
  ) |>
  group_by(year, region_label) |>
  summarise(conflict_count = n(), .groups = "drop") |>
  filter(!is.na(region_label))

#The Work Itself - Statistics----------------------------
#Summary Table
summary_table <- region_data |>
  group_by(region_label) |>
  summarise(
    mean_conflicts = round(mean(conflict_count), 0),
    median_conflicts = median(conflict_count),
    min_conflicts = min(conflict_count),
    max_conflicts = max(conflict_count),
    sd_conflicts = round(sd(conflict_count), 2),
  ) |>
  arrange(desc(mean_conflicts)) |>
  rename(
    'Average Number of Conflicts' = mean_conflicts,
    'Median Number of Conflicts' = median_conflicts,
    'Lowest Number of Conflicts' = min_conflicts,
    'Highest Number of Conflicts' = max_conflicts,
    'Standard Deviation' = sd_conflicts,
    'Region' = region_label
  )

kable(
  summary_table,
  caption = 'Summary Statistics of Regional Trends in Active Conflicts, 1949-2024'
)

#Regional Share of Conflict Table
share_table <- region_data |>
  group_by(year) |>
  mutate(global_total = sum(conflict_count)) |>
  ungroup() |>
  group_by(region_label) |>
  summarise(
    avg_share = round(mean(conflict_count / global_total) * 100),
    max_share = round(max(conflict_count / global_total) * 100),
    min_share = round(min(conflict_count / global_total) * 100)
  ) |>
  arrange(desc(avg_share)) |>
  rename(
    'Average Share of Conflicts' = avg_share,
    'Highest Share of Conflicts' = max_share,
    'Lowest Share of Conflicts' = min_share,
    'Region' = region_label
  )

kable(
  share_table,
  caption = 'Regional Share of Active Conflicts, 1946-2024'
)

# Era Comparisons
era_table <- region_data |>
  mutate(period = case_when(
    year <= 1991 ~ "Cold War",
    year <= 2001 ~ "Post-Cold War",
    TRUE ~ "Post-9/11"
  )) |>
  mutate(period = factor(
    period,
    levels = c("Cold War", "Post-Cold War", "Post-9/11")
  )) |>
  group_by(region_label, period) |>
  summarise(avg_conflicts = round(mean(conflict_count))) |>
  arrange(region_label, period)

era_table <- era_table |>
  pivot_wider(
    names_from = region_label,
    values_from = avg_conflicts
  ) |>
  rename('Era' = period)

kable(
  era_table,
  caption = 'Number of Active Conflicts Per Historical Period'
)

#Combining Tables
master_table <- summary_table |>
  left_join(share_table, by = "Region")

era_long <- region_data |>
  mutate(period = case_when(
    year <= 1991 ~ "Cold War",
    year <= 2001 ~ "Post-Cold War",
    TRUE ~ "Post-9/11"
  )) |>
  group_by(region_label, period) |>
  summarise(avg_conflicts = round(mean(conflict_count)), .groups = "drop") |>
  pivot_wider(
    names_from = period,
    values_from = avg_conflicts
  ) |>
  rename(
    Region = region_label,
    `Cold War Avg` = `Cold War`,
    `Post-Cold War Avg` = `Post-Cold War`,
    `Post-9/11 Avg` = `Post-9/11`
  )

master_table <- master_table |>
  left_join(era_long, by = "Region")

kable(
  master_table,
  caption = "Master Table: Regional Conflict Trends, Shares, and Historical Period Averages"
)

#The Work Itself - The Scatterplot----------------------
myplot <- ggplot(region_data, aes(x = year, y = conflict_count, color = region_label, shape = region_label)) +
  geom_point(alpha = 0.3) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c(
    "Europe" = "#3bb9ff",
    "The Middle East" = "#ff8040",
    "Asia" = "#b048b5",
    "Africa" = "#c83f49",
    "The Americas" = "#3cb371" 
  )) +
  scale_x_continuous(breaks = seq(1950, 2020, by = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  labs(
    title = "Trends of Active Conflicts by Region (1946–2024)",
    x = "Year",
    y = "Number of Active Conflicts",
    color = "Region",
    shape = "Region",
    caption = "Source: UCDP Data"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
  ) 
coord_cartesian(clip = "off")

myplot

# Original Work 2 ------------------------------------------------------------
#Loading Relevant Data
df <- read_excel("wbl.xlsx", sheet = 'WBL Panel 2024')

long_df <- df |>
  pivot_longer(
    cols = c(Mobility, Workplace, Pay, Marriage, Parenthood,
             Entrepreneurship, Assets, Pension),
    names_to = "pillar",
    values_to = "score"
  )

df2 <- long_df |>
  select(country, region, income_group, year, wbl_index, pillar, score) |>
  mutate(
    year = as.numeric(as.character(year))
  )

#The Work Itself - Identify Top Pillar ---------------------------
visual1_data <- df2 |>
  group_by(pillar, year) |>
  summarise(
    mean_score = mean(score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(pillar) |>
  summarize(
    change = max(mean_score) - min(mean_score)
  )

ggplot(visual1_data, aes(x = reorder(pillar, change), y = change, color = pillar, fill = pillar)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Pillar Improvement Over Time",
    x = "Pillar",
    y = "Total Change in Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.title.position = "plot",
    legend.position = "none"
  )

#The Work Itself - Workplace Country Distribution -------------------------
workplace_change <- df2 |>
  filter(pillar == "Workplace") |>
  group_by(country) |>
  summarise(
    start = mean(score[year == min(year)], na.rm = TRUE),
    end = mean(score[year == max(year)], na.rm = TRUE),
    change = end - start,
    .groups = "drop"
  )

#Graph
ggplot(workplace_change, aes(x = change)) +
  geom_histogram(bins = 30, fill = "seagreen") +
  geom_jitter(aes(y = 0), height = 0.1, alpha = 0.5) +
  scale_x_continuous(
    limits = c(0, 110),
    breaks = seq(0, 110, 10)
  ) +
  labs(
    title = "Distribution of Workplace Pillar Score Changes Across Countries",
    x = "Change in Score",
    y = "Number of Countries"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.title.position = "plot")

#The Work Itself - Income Study -----------------------------------
workplace_income_box <- df2 |>
  filter(pillar == "Workplace", income_group != "Not classified") |>
  group_by(country, income_group) |>
  summarise(
    start = mean(score[year == min(year)], na.rm = TRUE),
    end = mean(score[year == max(year)], na.rm = TRUE),
    change = end - start,
    .groups = "drop"
  )

ggplot(workplace_income_box, aes(x = income_group, y = change)) +
  geom_boxplot() +
  labs(
    title = "Workplace Score Change by Income Group",
    x = "Income Group",
    y = "Change in Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.title.position = "plot",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#The Work Itself - Region Study ---------------------------------------
workplace_region_box <- df2 |>
  filter(pillar == "Workplace") |>
  group_by(country, region) |>
  summarise(
    start = mean(score[year == min(year)], na.rm = TRUE),
    end = mean(score[year == max(year)], na.rm = TRUE),
    change = end - start,
    .groups = "drop"
  )

ggplot(workplace_region_box, aes(x = region, y = change)) +
  geom_boxplot() +
  labs(
    title = "Workplace Score Change by Region",
    x = "Region",
    y = "Change in Score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.title.position = "plot"
  )

#The Work Itself - Regression -------------------------------------
regression_data <- df2 |>
  filter(pillar == "Workplace") |>
  group_by(country, income_group, region) |>
  summarise(
    initial_score = mean(score[year == min(year)], na.rm = TRUE),
    final_score = mean(score[year == max(year)], na.rm = TRUE),
    change = final_score - initial_score,
    .groups = "drop"
  ) |>
  mutate(
    income_group = as.factor(income_group),
    region = as.factor(region)
  )

reg_model <- lm(change ~ income_group + region + initial_score,
                data = regression_data)

#Fix Results
summary(reg_model)
tidy(reg_model)
results <- tidy(reg_model)

#Make a Table
reg_table <- results |>
  filter(term != "income_groupNot classified") |>
  mutate(
    term = case_when(
      term == "regionEurope & Central Asia" ~ "Region: Europe & Central Asia",
      term == "regionOECD" ~ "Region: OECD",
      term == "regionLatin America & Caribbean" ~ "Region: Latin America & Caribbean",
      term == "regionMiddle East & North Africa" ~ "Region: Middle East & North Africa",
      term == "regionSouth Asia" ~ "Region: South Asia",
      term == "regionSub-Saharan Africa" ~ "Region: Sub-Saharan Africa",
      term == "income_groupLow income" ~ "Income Group: Low Income",
      term == "income_groupLower middle income" ~ "Income Group: Lower Middle Income",
      term == "income_groupUpper middle income" ~ "Income Group: Upper Middle Income",
      term == "initial_score" ~ "Initial Score",
      TRUE ~ term
    )
  ) |>
  rename(
    "Variable" = term,
    "Estimate" = estimate,
    "Std. Error" = std.error,
    "Statistic" = statistic,
    "P-Value" = p.value
  )

kable(
  reg_table,
  digits = 3,
  caption = "Regression Results"
)



require(lubridate)
require(ggplot2)
require(tidyverse)
require(dplyr)
require(mgcv)
require(gratia)
require(patchwork)

Fishway <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/WORK/Post-Doc/Other Projects/FIshway/FISH.csv")

Fishway <- Fishway[Fishway$Year != 1996, ]
hist(Fishway$Year)

str(Fishway)

#cleaning up data
Fishway$Species <- ifelse(grepl("Gizzard", Fishway$Species), "Gizzard Shad", Fishway$Species)
Fishway$Species <- ifelse(grepl("Drum", Fishway$Species), "Freshwater Drum", Fishway$Species)
Fishway$Species <- ifelse(grepl("Bigmouth", Fishway$Species), "Bigmouth Buffalo", Fishway$Species)
Fishway$Species <- ifelse(grepl("bowfin", Fishway$Species), "Bowfin", Fishway$Species)
Fishway$Species <- ifelse(grepl("black bullhead", Fishway$Species), "Black Bullhead", Fishway$Species)
Fishway$Species <- ifelse(grepl("brown bullhead", Fishway$Species), "Brown Bullhead", Fishway$Species)
Fishway$Species <- ifelse(grepl("Walleye", Fishway$Species), "Walleye", Fishway$Species)
Fishway$Species <- ifelse(grepl("White Sucker", Fishway$Species), "White Sucker", Fishway$Species)

species_to_keep <- c(
  "Bigmouth Buffalo", "Channel Catfish", "Largemouth Bass", 
  "Bowfin", "Brown Bullhead", "Common Carp", "Freshwater Drum", 
  "Gizzard Shad", "Goldfish", "Yellow Perch", "Northern Pike", 
  "Rainbow Trout", "Rudd", "White Bass", "White Perch", "White Sucker"
)

# Filter the dataframe
fishway2 <- Fishway[Fishway$Species %in% species_to_keep, ]

species_count$Invasive_Native <- ifelse(species_count$Species %in% c("Common Carp", "Goldfish", "Rudd", "Rainbow Trout", "White Perch", "White Bass"), "Invasive", "Native")

#Filter out rows with missing Date and add Julian date column
fishway2 <- fishway2 %>%
  mutate(Date = make_date(Year, Month, Day)) %>% 
  mutate(JD = yday(Date))

#FIRST ARRIVAL
#Filter out rows with missing Date and add Julian date column
fishway2 <- fishway2 %>%
  mutate(Date = make_date(Year, Month, Day)) %>% 
  mutate(JD = yday(Date))

#Create a new dataframe with the first Julian date for each species for each year
first_julian <- fishway2 %>%
  group_by(Species, Year) %>%
  summarize(
    MinJD = min(JD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(is.finite(MinJD))  

first_julian$Invasive_Native <- ifelse(first_julian$Species %in% c("Common Carp", "Goldfish", "Gizzard Shad", "Rudd", "Rainbow Trout", "White Perch", "White Bass"), "Invasive", "Native")

#Plot Julian Date (Y) vs Year (X)
first_julian %>%
  filter(Species != "Black Bullhead", Year != 1999) %>%
  mutate(Invasive_Native = recode(Invasive_Native, "Invasive" = "Non-native")) %>%
  ggplot(aes(x = Year, y = MinJD, color = Invasive_Native)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add trendline without SE
  scale_color_manual(
    values = c("Non-native" = "blue", "Native" = "darkgreen")
  ) +
  labs(
    title = "First Julian Date for Each Species Over Time",
    x = "Year",
    y = "First Arrival Date (Julian Day)",
    color = NULL
  ) +
  facet_wrap(~Species, scales = "free_y") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Fit GAM
first_julian$Species <- as.factor(first_julian$Species)

m1 <- gam(MinJD ~ s(Year, by=Species, k = 6) + Species, data = first_julian, family = gaussian())

appraise(m1)
summary(m1)
car::Anova(m1)
draw(m1)+theme_bw()

#Generate prediction grid for m1
p <- expand_grid(
  Year = seq(1996, 2023, by = 1), 
  inv = c("Native", "Invasive"), 
  Species = unique(first_julian$Species)  # Include all unique species
) %>%
  mutate(
    predictions = predict.gam(m1, newdata = ., type = "link", exclude=c("s(Species)"), se.fit = TRUE)$fit,
    se = predict.gam(m1, newdata = ., type = "response", exclude=c("s(Species)"), se.fit = TRUE)$se.fit
  )
p <- p %>%
  mutate(inv = recode(inv, "Invasive" = "Non-native"))

# Visualize predictions
JD_pred1 <- ggplot(p, aes(x = Year, y = predictions, color = factor(inv))) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    aes(
      x = Year,
      ymin = predictions - 1.96 * se,
      ymax = predictions + 1.96 * se,
      fill = factor(inv)
    ),
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  scale_color_manual(
    values = c("Non-native" = "blue", "Native" = "darkgreen")
  ) +
  scale_fill_manual(
    values = c("Non-native" = "blue", "Native" = "darkgreen")
  ) +
  facet_wrap(~inv) +
  labs(
    title = "Predicted Species First Arrival Over Time",
    x = "Year",
    y = "Predicted Julian Date",
    color = "Invasiveness",
    fill = "Invasiveness"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )

JD_pred2 <- ggplot(p, aes(x = Year, y = predictions, color = factor(inv))) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c("Non-native" = "blue", "Native" = "darkgreen")
  ) +
  scale_fill_manual(
    values = c("Non-native" = "blue", "Native" = "darkgreen")
  ) +
  facet_wrap(~inv) +
  scale_x_continuous(limits = c(1995, 2025), breaks = seq(1995, 2025, 5)) +
  scale_y_continuous(limits = c(90, 110), breaks = seq(90, 110, 5)) +
  labs(
    title = "Predicted Species First Arrival Over Time",
    x = "Year",
    y = "Predicted Julian Date",
    color = "Invasiveness",
    fill = "Invasiveness"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold")
  )
JD_pred2

combined_plot1 <- JD_pred1 / JD_pred2 + 
  plot_layout(ncol = 1) +  # Arrange side by side
  plot_annotation(
    title = "Predicted Species Count: Comparative Analysis",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )
combined_plot1

#PEAK ARRIVAL
#Calculate the cumulative quantity per day, grouped by species and year
fishway3 <- fishway2 %>%
  group_by(Species, Year) %>%
  arrange(Date) %>%
  mutate(
    CumulativeQuantity = cumsum(quantity),
    AnnualTotal = sum(quantity, na.rm = TRUE),
    annual_percentage = CumulativeQuantity / AnnualTotal
  ) %>%
  ungroup()

closest_to_50_df <- fishway3 %>%
  mutate(Date_fixed = make_date(Year, Month, Day)) %>% 
  group_by(Species, Year) %>%
  slice_min(order_by = abs(annual_percentage - 0.5), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(annual_percentage != 1.00) %>%
  mutate(JulianDate = yday(Date_fixed))

#Create the plot for the closest-to-50% values
closest_to_50_df %>%
  filter(Species != "Black Bullhead", Year != 1999) %>%
  mutate(Invasive_Native = recode(Invasive_Native, "Invasive" = "Non-native")) %>%
  ggplot(aes(x = Year, y = JulianDate, color = Invasive_Native)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(
    values = c("Non-native" = "blue", "Native" = "darkgreen")
  ) +
  labs(
    x = "Year",
    y = "Peak Date (Julian Day)",
    color = NULL
  ) +
  facet_wrap(~Species, scales = "free_y") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

closest_to_50_clean <- closest_to_50_df %>%
  mutate(
    inv = factor(Invasive_Native, levels = c("Native", "Invasive")),
    Year = as.numeric(Year),
    Species = factor(Species)  # ✅ this line is critical
  ) %>%
  select(JulianDate, Year, inv, Species) %>%
  drop_na()

# Fit GAM to the 50% cumulative movement Julian dates
m3 <- gam(
  JulianDate ~ s(Year, by = inv, k = 20) + inv + s(Species, bs = "re"),
  data = closest_to_50_clean,
  family = gaussian()
)

appraise(m3)
summary(m3)
car::Anova(m3)
draw(m3)+theme_bw()

# Create prediction grid
p <- expand_grid(
  Year = seq(1996, 2023, by = 1), 
  inv = c("Native", "Invasive"), 
  Species = unique(closest_to_50_clean$Species)
) %>%
  mutate(
    predictions = predict.gam(m3, newdata = ., type = "link", exclude = c("s(Species)"), se.fit = TRUE)$fit,
    se = predict.gam(m3, newdata = ., type = "response", exclude = c("s(Species)"), se.fit = TRUE)$se.fit
  ) %>%
  mutate(inv = recode(inv, "Invasive" = "Non-native"))

# Summarize across all species for main trendline
p_all <- p %>%
  group_by(Year) %>%
  summarize(predictions = mean(predictions, na.rm = TRUE), .groups = "drop")

# Plot main trendline (all species)
JD_pred3 <- ggplot(p_all, aes(x = as.numeric(Year), y = predictions)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Year",
    y = "Peak Arrival (Julian Day)"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )

# Plot with invasiveness grouping
JD_pred4 <- ggplot(p, aes(x = Year, y = predictions, color = factor(inv))) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c("Non-native" = "blue", "Native" = "darkgreen")
  ) +
  scale_fill_manual(
    values = c("Non-native" = "blue", "Native" = "darkgreen")
  ) +
  labs(
    x = "Year",
    y = "Peak Arrival (Julian Day)",
    color = NULL,
    fill = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )

# Combine both plots vertically
combined_plot3 <- JD_pred3 / JD_pred4 +
  plot_layout(ncol = 1) +
  plot_annotation(
    theme = theme(plot.title = element_text(hjust = 0.5))
  )
combined_plot3

#DURATION

#LAST ARRIVAL 
#Create a new dataframe with the last Julian date for each species for each year
last_julian <- fishway2 %>%
  group_by(Species, Year) %>%
  summarize(
    MaxJD = max(JD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(is.finite(MaxJD))  

last_julian$Invasive_Native <- ifelse(last_julian$Species %in% c("Common Carp", "Goldfish", "Gizzard Shad", "Rudd", "Rainbow Trout", "White Perch", "White Bass"), "Invasive", "Native")

#Plot Julian Date (Y) vs Year (X)
last_julian %>%
  filter(Species != "Black Bullhead", Year != 1999) %>%
  mutate(Invasive_Native = recode(Invasive_Native, "Invasive" = "Non-native")) %>%
  ggplot(aes(x = Year, y = MaxJD, color = Invasive_Native)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(
    values = c("Non-native" = "blue", "Native" = "darkgreen")
  ) +
  labs(
    x = "Year",
    y = "Last Arrival Date (Julian Day)",
    color = NULL
  ) +
  facet_wrap(~Species, scales = "free_y") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# GAM with smooths by invasion status and random effect for species
m2 <- gam(
  MaxJD ~ s(Year, by = inv, k = 5) + inv + s(Species, bs = "re"),
  data = last_julian %>% mutate(inv = factor(Invasive_Native)),
  family = gaussian()
)

summary(m2)
summary(m2)
car::Anova(m2)
draw(m2)+theme_bw()

#Generate prediction grid
p <- expand_grid(
  Year = seq(1996, 2023, by = 1), 
  inv = c("Native", "Invasive"), 
  Species = unique(last_julian$Species)  # Include all unique species
) %>%
  mutate(
    predictions = predict.gam(m2, newdata = ., type = "link", exclude = c("s(Species)"), se.fit = TRUE)$fit,
    se = predict.gam(m2, newdata = ., type = "response", exclude = c("s(Species)"), se.fit = TRUE)$se.fit
  ) %>%
  mutate(inv = recode(inv, "Invasive" = "Non-native"))

# Visualize predictions
p_all <- p %>%
  group_by(Year) %>%
  summarize(predictions = mean(predictions, na.rm = TRUE))

JD_pred5 <- ggplot(p_all, aes(x = as.numeric(Year), y = predictions)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Year",
    y = "Predicted Last Julian Date") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )
JD_pred5

# Visualize predictions - with ribbon
JD_pred6 <- ggplot(p, aes(x = Year, y = predictions, color = factor(inv))) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c("Non-native" = "blue", "Native" = "darkgreen")
  ) +
  scale_fill_manual(
    values = c("Non-native" = "blue", "Native" = "darkgreen")
  ) +
  labs(
    x = "Year",
    y = "Predicted Last Julian Date",
    color = NULL,
    fill = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )
JD_pred6

# Combine plots
combined_plot2 <- JD_pred5 / JD_pred6 + 
  plot_layout(ncol = 1) +
  plot_annotation(
    theme = theme(plot.title = element_text(hjust = 0.5))
  )
combined_plot2

#DURATION
#duration
duration_df <- fishway2 %>%
  group_by(Species, Year) %>%
  summarize(
    MinJD = min(JD, na.rm = TRUE),
    MaxJD = max(JD, na.rm = TRUE),
    Duration = MaxJD - MinJD,
    .groups = "drop"
  ) %>%
  filter(Duration > 0)

hist(duration_df$Duration)

#Moodel duration with GAM
m4 <- gam(Duration ~ Invasive_Native + Year +
            s(Year, by = Invasive_Native, k = 5, bs = "tp") +
            s(Species, bs = "re"),
          data = duration_df,
          family = nb())

appraise(m4)
draw(m4)
summary(m4)
anova(m4)

#Create prediction grid
new_grid <- expand_grid(
  Invasive_Native = c("Native", "Non-native"),
  Year = 1997:2025
) %>%
  mutate(
    Invasive_Native = factor(Invasive_Native, levels = c("Native", "Non-native")),
    Species = factor(Species)
  )

#Predict from m4 and create figure
new_grid %>%
  group_by(Year, Invasive_Native) %>%
  summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(y = value, x = Year, fill = Invasive_Native)) +
  geom_col() +
  facet_wrap(~Invasive_Native, scales = "free") +
  scale_fill_manual(values = c("Native" = "blue", "Non-native" = "darkgreen")) +
  labs(
    x = "Year",
    y = "Predicted Duration"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.key.width = unit(3, "cm")
  ) +
  scale_x_continuous(limits = c(1995, 2025), breaks = seq(1995, 2025, 5)) +
  coord_cartesian(ylim = c(100, 150))

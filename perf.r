library(tidyverse)
library(extrafont)
library(ggpattern)

data <- tribble(
         ~category,   ~KIT,     ~BA,   ~BMW,   ~CLA,   ~HOU,   ~STR,   ~TEA,
  "Baseline_2_lat", 667397, 1154030, 260277, 580749, 638824, 681540, 397765,
     "AQB48_2_lat", 542590, 1003118, 263820, 638755, 441504, 646119, 277640,
  "Baseline_6_lat", 380660,  718649, 197263, 351789, 316186, 480063, 236456,
     "AQB48_6_lat", 347444,  641654, 176263, 372132, 285275, 388828, 182301,
)

data_long <- data |>
  pivot_longer(cols=!category, names_to="scene", values_to="value") |>
  pivot_wider(names_from=category, values_from=value) |>
  mutate(
    `Baseline-2`=1,
    `AQB48-2`=Baseline_2_lat/AQB48_2_lat,
    `Baseline-6`=Baseline_2_lat/Baseline_6_lat,
    `AQB48-6`=Baseline_2_lat/AQB48_6_lat
  ) |>
  select(!c(Baseline_2_lat, AQB48_2_lat, Baseline_6_lat, AQB48_6_lat)) |>
  print()

data_long_mean <- data_long |>
  summarise(across(!scene, ~exp(mean(log(.)))), .groups="drop") |>
  mutate(scene="MEAN") |>
  print()

data_long_combined <- bind_rows(data_long, data_long_mean) |>
  pivot_longer(cols=!scene, names_to="category", values_to="value") |>
  mutate(
    scene=factor(scene, levels=unique(scene)),
    category=factor(category, levels=unique(category))
  ) |>
  print()

fig <- ggplot(data_long_combined) +
  geom_col_pattern(
    aes(x=scene, y=value, fill=category, pattern=category),
    position="dodge",
    color="black",
    width=0.8,
    linewidth=0.3,
    pattern_density=0.01,
    pattern_spacing=0.06,
    pattern_color="#765541"
  ) +
  labs(
    x="Scenes",
    y="Normalized Speedup"
  ) +
  scale_pattern_manual(
    values=c("Baseline-2"="none", "AQB48-2"="stripe", "Baseline-6"="none", "AQB48-6"="stripe"),
    guide="none"
  ) +
  scale_fill_manual(
    values=c("Baseline-2"="#e5d8d1", "AQB48-2"="#b8947f", "Baseline-6"="#765541", "AQB48-6"="#3b2e25"),
    guide=guide_legend(title=NULL)
  ) +
  scale_y_continuous(
    expand=expansion(mult=c(0, 0)),
  ) +
  theme_minimal(base_family="Noto Serif") +
  theme(
    legend.position="top",
    legend.key.size=unit(0.4, "cm"),
    legend.text=element_text(size=11, color="grey20"),
    axis.text.x=element_text(size=11, color="grey20"),
    axis.text.y=element_text(size=11, color="grey20"),
    axis.title=element_text(size=16, color="black"),
    strip.text.x=element_text(size=12, color="black", face="bold")
  )

ggsave("perf.pdf", width=7, height=3.5)

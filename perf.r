library(tidyverse)
library(extrafont)
library(ggpattern)

data <- tribble(
         ~category,   ~KIT,     ~BA,   ~BMW,   ~CLA,   ~HOU,   ~STR,   ~TEA,
  "Baseline_2_lat", 667397, 1154030, 260277, 580749, 638824, 681540, 397765,
  "Compress_2_lat", 481772,  885932, 248722, 552232, 412162, 579316, 266282,
     "AQB48_2_lat", 290001,  512439, 152414, 316178, 232382, 348746, 154175,
  "Baseline_6_lat", 396883,  751237, 207308, 364136, 344809, 516370, 246587,
     "AQB48_6_lat", 314655,  558894, 153460, 316040, 251942, 331340, 168449,
)

data_long <- data |>
  pivot_longer(cols=!category, names_to="scene", values_to="value") |>
  pivot_wider(names_from=category, values_from=value) |>
  mutate(
    `Baseline-2`=1,
    `Compress-2`=Baseline_2_lat/Compress_2_lat,
    `AQB48-2`=Baseline_2_lat/AQB48_2_lat,
    `Baseline-6`=Baseline_2_lat/Baseline_6_lat,
    `AQB48-6`=Baseline_2_lat/AQB48_6_lat
  ) |>
  select(!c(Baseline_2_lat, Compress_2_lat, AQB48_2_lat, Baseline_6_lat, AQB48_6_lat)) |>
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
    aes(x=scene, y=value, fill=category, pattern=category, pattern_angle=category, pattern_density=category),
    position="dodge",
    color="black",
    width=0.8,
    linewidth=0.3,
    pattern_color="#765541",
  ) +
  labs(
    x="Scenes",
    y="Normalized\nSpeedup"
  ) +
  scale_pattern_manual(
    values=c("Baseline-2"="none", "Compress-2"="stripe", "AQB48-2"="crosshatch", "Baseline-6"="circle", "AQB48-6"="crosshatch"),
    guide="none"
  ) +
  scale_pattern_angle_manual(
    values=c("Baseline-2"=0, "Compress-2"=30, "AQB48-2"=30, "Baseline-6"=30, "AQB48-6"=0),
    guide="none"
  ) +
  scale_pattern_density_manual(
    values=c("Baseline-2"=0.01, "Compress-2"=0.01, "AQB48-2"=0.01, "Baseline-6"=0.1, "AQB48-6"=0.01),
    guide="none"
  ) +
  scale_fill_manual(
    values=c("Baseline-2"="#dccbc0", "Compress-2"="#cab09f", "AQB48-2"="#b8947f", "Baseline-6"="#a6795e", "AQB48-6"="#8d5d3e"),
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
    strip.text.x=element_text(size=12, color="black", face="bold"),
    panel.grid.major.x = element_blank()
  )

ggsave("perf.pdf", width=7, height=2)

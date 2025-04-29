library(tidyverse)
library(extrafont)
library(ggpattern)

data <- tribble(
         ~category,    ~KIT,     ~BA,   ~BMW,    ~CLA,   ~HOU,    ~STR,   ~TEA,
  "Baseline_2_lat", 1320494, 2630552, 627087, 1596255, 965794, 1459337, 605572,
  "Compress_2_lat", 1017213, 1919670, 497084, 1163731, 721385, 1135749, 483459,
      "AQB8_2_lat",  783332, 1189130, 378940,  846649, 497692,  846469, 362297,
  "Baseline_6_lat", 1153082, 2228588, 517859, 1246713, 773294, 1119258, 552480,
  "Compress_6_lat",  980912, 1818023, 451375, 1015105, 636755,  953661, 472520,
      "AQB8_6_lat",  846149, 1304845, 375922,  926077, 523746,  820355, 387814,
)

data_long <- data |>
  pivot_longer(cols=!category, names_to="scene", values_to="value") |>
  pivot_wider(names_from=category, values_from=value) |>
  mutate(
    `Baseline-2`=1,
    `Compress-2`=Baseline_2_lat/Compress_2_lat,
    `AQB8-2`=Baseline_2_lat/AQB8_2_lat,
    `Baseline-6`=1,
    `Compress-6`=Baseline_6_lat/Compress_6_lat,
    `AQB8-6`=Baseline_6_lat/AQB8_6_lat
  ) |>
  select(!c(Baseline_2_lat, Compress_2_lat, AQB8_2_lat, Baseline_6_lat, Compress_6_lat, AQB8_6_lat)) |>
  print()

data_long_mean <- data_long |>
  summarise(across(!scene, ~exp(mean(log(.)))), .groups="drop") |>
  mutate(scene="GMEAN") |>
  print()

data_long_combined <- bind_rows(data_long, data_long_mean) |>
  pivot_longer(cols=!scene, names_to="category", values_to="value") |>
  print()

dummy_data <- data_long_combined |>
  distinct(scene) |>
  mutate(category="Spacer", value=NA_real_) |>
  print()

data_long_spaced <- bind_rows(data_long_combined, dummy_data) |>
  mutate(
    scene=factor(scene, levels=unique(scene)),
    category=factor(category, levels=c("Baseline-2", "Compress-2", "AQB8-2", "Spacer", "Baseline-6", "Compress-6", "AQB8-6"))
  ) |>
  print()

text_data <- data_long_spaced |>
  filter(category %in% c("AQB8-2", "AQB8-6")) |>
  filter(!is.na(value)) |>
  mutate(label=paste0(format(round(value, 2), nsmall=2), "x")) |>
  print()

fig <- ggplot(data_long_spaced) +
  geom_col_pattern(
    aes(x=scene, y=value, fill=category, pattern=category, pattern_angle=category, pattern_color=category),
    position="dodge",
    color="black",
    width=0.9,
    linewidth=0.3,
    pattern_density=0.001
  ) +
  geom_text(
    data=text_data,
    aes(x=scene, y=value, label=label, group=category),
    position=position_dodge(width=0.9),
    vjust=-0.5,
    size=3,
    family="Noto Serif",
    color="black"
  ) +
  labs(
    x="Scenes",
    y="Normalized\nSpeedup"
  ) +
  scale_fill_manual(
    values=c("Baseline-2"="#d3bdb0", "Compress-2"="#b8947f", "AQB8-2"="#976c54", "Baseline-6"="#b0d3cf", "Compress-6"="#7fb8b1", "AQB8-6"="#54978f"),
    guide=guide_legend(title=NULL)
  ) +
  scale_pattern_manual(
    values=c("Baseline-2"="none", "Compress-2"="stripe", "AQB8-2"="crosshatch", "Spacer"="none", "Baseline-6"="circle", "Compress-6"="stripe", "AQB8-6"="crosshatch"),
    guide="none"
  ) +
  scale_pattern_angle_manual(
    values=c("Baseline-2"=0, "Compress-2"=30, "AQB8-2"=30, "Spacer"=0, "Baseline-6"=0, "Compress-6"=0, "AQB8-6"=0),
    guide="none"
  ) +
  scale_pattern_color_manual(
    values=c("Baseline-2"="#664939", "Compress-2"="#664939", "AQB8-2"="#664939", "Spacer"="#000000", "Baseline-6"="#396660", "Compress-6"="#396660", "AQB8-6"="#396660"),
    guide="none"
  ) +
  scale_y_continuous(
    expand=expansion(mult=c(0, 0)),
    limits=c(0, 2.4)
  ) +
  theme_minimal(base_family="Noto Serif") +
  theme(
    legend.position="top",
    legend.key.size=unit(0.4, "cm"),
    legend.text=element_text(size=11, color="grey20"),
    axis.text.x=element_text(size=11, color="grey20"),
    axis.text.y=element_text(size=11, color="grey20"),
    axis.title=element_text(size=14, color="black"),
    strip.text.x=element_text(size=12, color="black", face="bold"),
    panel.grid.major.x = element_blank()
  )

ggsave("perf.pdf", width=6.5, height=2.5)

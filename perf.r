library(tidyverse)
library(extrafont)
library(ggpattern)

data <- tribble(
         ~category,    ~KIT,     ~BA,   ~BMW,    ~CLA,   ~HOU,    ~STR,   ~TEA,
  "Baseline_2_lat", 1334990, 2667108, 633816, 1625539, 991170, 1496161, 612459,
  "Compress_2_lat", 1074016, 2066166, 521894, 1255431, 780369, 1223450, 508385,
      "AQB8_2_lat",  784908, 1203016, 379189,  850393, 499801,  848479, 363467,
  "Baseline_6_lat", 1171495, 2282907, 530276, 1270492, 794921, 1163622, 566794,
  "Compress_6_lat", 1024112, 1918002, 469819, 1067370, 676100, 1024673, 492483,
      "AQB8_6_lat",  846608, 1323658, 381888,  933309, 525038,  822634, 391301,
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
    limits=c(0, 2.6)
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

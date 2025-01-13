library(tidyverse)
library(extrafont)
library(ggpattern)

data <- tribble(
                             ~category,      ~type,     ~KIT,       ~BA,     ~BMW,      ~CLA,     ~HOU,      ~STR,     ~TEA,
       "(a) Ray-Box Intersection Test", "Baseline", 80039307,  93736200, 73315482, 121983935, 45775577,  92641215, 64172522,
       "(a) Ray-Box Intersection Test",    "AQB48", 87278088, 101420034, 76223221, 136460270, 48822114, 103765710, 67435411,
  "(b) Ray-Triangle Intersection Test", "Baseline", 12292964,  14845355, 23956184,  16331386,  9744976,  25946748, 21797483,
  "(b) Ray-Triangle Intersection Test",    "AQB48", 20533796,  21241906, 28976705,  31165607, 11751613,  33948321, 25236623,
)

data_long <- data |>
  pivot_longer(cols=!c(category, type), names_to="scene", values_to="value") |>
  print()

data_long_mean <- data_long |>
  group_by(category, type) |>
  summarise(value=mean(value), .groups="drop") |>
  mutate(scene="MEAN") |>
  print()

data_long_combined <- bind_rows(data_long, data_long_mean) |>
  mutate(
    value_norm=value/max(value),
    category=factor(category, levels=unique(category)),
    type=factor(type, levels=unique(type)),
    scene=factor(scene, levels=unique(scene))
  ) |>
  select(!value) |>
  print()

data_increment <- data_long_combined |>
  pivot_wider(names_from=type, values_from=value_norm) |>
  mutate(ratio=(AQB48-Baseline)/AQB48) |>
  print()

fig <- ggplot(data_long_combined) +
  geom_col_pattern(
    aes(x=scene, y=value_norm, fill=type, pattern=type),
    position="dodge",
    color="black",
    width=0.75,
    linewidth=0.3,
    pattern_density=0.01,
    pattern_spacing=0.06,
    pattern_color="#765541"
  ) +
  geom_text(
    data=data_increment,
    aes(x=scene, y=AQB48+max(AQB48)*0.05, label=sprintf("+%.0f%%", ratio*100)),
    color="grey20",
    size=3,
    family="Noto Serif",
    hjust=0.3,
  ) +
  facet_wrap(~category) +
  labs(
    x="Scenes",
    y="Normalized Operation\nCounts"
  ) +
  scale_pattern_manual(
    values=c("Baseline"="none", "AQB48"="stripe"),
    guide="none"
  ) +
  scale_fill_manual(
    values=c("Baseline"="#dccbc0", "AQB48"="#b8947f"),
    guide=guide_legend(title=NULL)
  ) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
  theme_minimal(base_family="Noto Serif") +
  theme(
    legend.position="top",
    legend.key.size=unit(0.4, "cm"),
    legend.text=element_text(size=11, color="grey20"),
    axis.text.x=element_text(size=11, color="grey20", angle=30),
    axis.text.y=element_text(size=11, color="grey20"),
    axis.title=element_text(size=16, color="black"),
    strip.text.x=element_text(size=12, color="grey20", face="bold"),
    strip.text.y=element_text(size=16, color="black", face="bold"),
    panel.spacing.x=unit(0, "cm"),
    panel.spacing.y=unit(0.65, "cm")
  )
ggsave("steps.pdf", width=7, height=3.5)
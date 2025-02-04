library(tidyverse)
library(extrafont)
library(ggpattern)

data <- tribble(
                             ~category,      ~type,     ~KIT,      ~BA,     ~BMW,     ~CLA,     ~HOU,     ~STR,    ~TEA,
       "(a) Ray-Box Intersection Test", "Baseline", 27350466, 32281744, 25391961, 34350719, 22064428, 51227574, 8819512,
       "(a) Ray-Box Intersection Test", "Compress", 27800813, 32689835, 25805727, 34722533, 22461394, 51650496, 9085097,
       "(a) Ray-Box Intersection Test",    "AQB48", 28852894, 34148649, 26358502, 35590843, 23226071, 52830697, 9203416,
  "(b) Ray-Triangle Intersection Test", "Baseline",  4141323,  5016284,  7231499,  4069250,  4436902, 14501956, 3106594,
  "(b) Ray-Triangle Intersection Test", "Compress",  4224343,  5145595,  7301799,  4168922,  4510828, 14674073, 3131536,
  "(b) Ray-Triangle Intersection Test",    "AQB48",  5277028,  6086967,  8150865,  5346766,  5132850, 15635343, 3290746,
)

data_long <- data |>
  pivot_longer(cols=!c(category, type), names_to="scene", values_to="value") |>
  group_by(scene) |>
  mutate(value=value/value[category == "(a) Ray-Box Intersection Test" & type == "Baseline"]) |>
  ungroup() |>
  print()

data_long_mean <- data_long |>
  group_by(category, type) |>
  summarise(value=mean(value), .groups="drop") |>
  mutate(scene="MEAN") |>
  print()

data_long_combined <- bind_rows(data_long, data_long_mean) |>
  mutate(
    category=factor(category, levels=unique(category)),
    type=factor(type, levels=unique(type)),
    scene=factor(scene, levels=unique(scene))
  ) |>
  print()

fig <- ggplot(data_long_combined) +
  geom_col_pattern(
    aes(x=scene, y=value, fill=type, pattern=type),
    position="dodge",
    color="black",
    width=0.75,
    linewidth=0.3,
    pattern_density=0.01,
    pattern_spacing=0.06,
    pattern_color="#765541"
  ) +
  facet_wrap(~category) +
  labs(
    x="Scenes",
    y="Normalized\nOperation Counts"
  ) +
  scale_pattern_manual(
    values=c("Baseline"="none", "Compress"="stripe", "AQB48"="crosshatch"),
    guide="none"
  ) +
  scale_fill_manual(
    values=c("Baseline"="#dccbc0", "Compress"="#b8947f", "AQB48"="#a6795e"),
    labels=c("Baseline"="Baseline-2", "Compress"="Compress-2", "AQB48"="AQB48-2"),
    guide=guide_legend(title=NULL)
  ) +
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
ggsave("steps.pdf", width=7, height=2.8)
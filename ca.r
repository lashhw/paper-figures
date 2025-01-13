library(tidyverse)
library(extrafont)
library(ggpattern)

data <- tribble(
     ~level, ~category,      ~type,      ~KIT,       ~BA,      ~BMW,      ~CLA,      ~HOU,      ~STR,      ~TEA,
  "(a) L1D",     "hit", "Baseline", 336367950, 391836458, 342548495, 494276013, 200362442, 420286412, 280598263,
  "(a) L1D",    "miss", "Baseline",  14858739,  20181052,  10513759,  36172892,   7031566,  13812596,  31405589,
   "(b) L2",     "hit", "Baseline",  11777387,  14738415,   8717445,  33831909,   6475336,  12899248,  25398815,
   "(b) L2",    "miss", "Baseline",   3081352,   5442637,   1796314,   2340983,    556230,    913348,   6006774,
  "(a) L1D",     "hit",    "AQB48", 167123393, 187451494, 191240674, 270610515, 102587033, 205750271, 159283689,
  "(a) L1D",    "miss",    "AQB48",   6385779,   9877892,   3944477,  14884634,   2562509,   5029540,  13375629,
   "(b) L2",     "hit",    "AQB48",   4947829,   7373719,   3122416,  14292742,   2326889,   4614565,  11276150,
   "(b) L2",    "miss",    "AQB48",   1437950,   2504173,    822061,    591892,    235620,    414975,   2099479,
)

data_long <- data |>
  pivot_longer(cols=!c(level, category, type), names_to="scene", values_to="value") |>
  print()

data_long_mean <- data_long |>
  group_by(level, category, type) |>
  summarise(value=mean(value), .groups="drop") |>
  mutate(scene="MEAN") |>
  print()

data_long_combined <- bind_rows(data_long, data_long_mean) |>
  pivot_wider(names_from=category, values_from=value) |>
  group_by(level) |>
  mutate(
    hit_norm=hit,
    miss_norm=miss
  ) |>
  ungroup() |>
  select(level, type, scene, hit_norm, miss_norm) |>
  pivot_longer(cols=!c(level, type, scene), names_to="category", values_to="value") |>
  mutate(
    level=factor(level, levels=unique(level)),
    type=factor(type, levels=unique(type)),
    scene=factor(scene, levels=unique(scene)),
    category=factor(category, levels=c("hit_norm", "miss_norm"))
  ) |>
  print()

data_reduction <- data_long_combined |>
  pivot_wider(names_from=type, values_from=value) |>
  group_by(level, scene) |>
  summarise(Baseline=sum(Baseline), AQB48=sum(AQB48), .groups="drop") |>
  mutate(ratio=(Baseline-AQB48)/Baseline) |>
  print()

fig <- ggplot(data_long_combined) +
  geom_col_pattern(
    aes(x=type, y=value, fill=category, pattern=type),
    position="stack",
    color="black",
    width=0.8,
    linewidth=0.3,
    pattern_density=0.01,
    pattern_spacing=0.12,
    pattern_color="#765541"
  ) +
  geom_text(
    data=data_reduction,
    aes(x=2, y=AQB48, label=sprintf("-%.0f%%", ratio*100)),
    color="black",
    size=3,
    family="Noto Serif",
    vjust=-0.5,
  ) +
  facet_grid(level~scene, switch="x", scales="free_y") +
  labs(
    x="Scenes",
    y="Cache Accesses (Lines)"
  ) +
  scale_pattern_manual(
    values=c("Baseline"="none", "AQB48"="stripe"),
    guide=guide_legend(title=NULL, order=1)
  ) +
  scale_fill_manual(
    values=c("hit_norm"="#dccbc0", "miss_norm"="#b8947f"),
    labels=c("hit_norm"="Cache Hit", "miss_norm"="Cache Miss"),
    guide=guide_legend(title=NULL, order=2)
  ) +
  scale_y_continuous(expand=expansion(mult=c(0, 0))) +
  theme_minimal(base_family="Noto Serif") +
  theme(
    legend.position="top",
    legend.key.size=unit(0.4, "cm"),
    legend.text=element_text(size=11, color="grey20"),
    axis.text.x=element_blank(),
    axis.text.y=element_text(size=11, color="grey20"),
    axis.title=element_text(size=16, color="black"),
    strip.text.x=element_text(size=12, color="grey20"),
    strip.text.y=element_text(size=16, color="black", face="bold"),
    panel.spacing.x=unit(0, "cm"),
    panel.spacing.y=unit(0.65, "cm")
  )

ggsave("ca.pdf", width=6.9, height=4.2)
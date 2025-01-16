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

l1d_total_baseline <- data |>
  filter(level == "(a) L1D", type == "Baseline") |>
  select(-level, -type) |>
  pivot_longer(-category, names_to = "scene", values_to = "value") |>
  group_by(scene) |>
  summarise(l1d_total = sum(value)) |>
  print()

data_long <- data |>
  pivot_longer(cols=-c(level, category, type), names_to="scene", values_to="value") |>
  left_join(l1d_total_baseline, by="scene") |>
  mutate(value_normalized=value/l1d_total) |>
  select(-value, -l1d_total) |>
  print()

data_long_mean <- data_long |>
  group_by(level, category, type) |>
  summarise(value_normalized=mean(value_normalized), .groups="drop") |>
  mutate(scene="MEAN") |>
  print()

base_data <- bind_rows(data_long, data_long_mean) |>
  pivot_wider(names_from=category, values_from=value_normalized) |>
  mutate(total=hit+miss)

dram_data <- base_data |>
  filter(level == "(b) L2") |>
  mutate(level="(c) DRAM", total=miss, hit=NA, miss=NA)

data_long_combined <- bind_rows(base_data, dram_data) |>
  mutate(
    level=factor(level, levels=unique(level)),
    type=factor(type, levels=unique(type)),
    scene=factor(scene, levels=unique(scene)),
  ) |>
  print()

#data_reduction <- data_long_combined |>
#  pivot_wider(names_from=type, values_from=value_normalized) |>
#  group_by(level, scene) |>
#  summarise(Baseline=sum(Baseline), AQB48=sum(AQB48), .groups="drop") |>
#  mutate(ratio=(Baseline-AQB48)/Baseline) |>
#  print()

fig <- ggplot(data_long_combined) +
  geom_col_pattern(
    aes(x=scene, y=total, fill=type, pattern=type),
    position="dodge",
    color="black",
    width=0.75,
    linewidth=0.3,
    pattern_density=0.01,
    pattern_spacing=0.12,
    pattern_color="#765541"
  ) +
  #geom_text(
  #  data=data_reduction,
  #  aes(x=2, y=AQB48, label=sprintf("-%.0f%%", ratio*100)),
  #  color="black",
  #  size=3,
  #  family="Noto Serif",
  #  vjust=-0.5,
  #) +
  facet_wrap(~level, nrow=3, scales="free_y") +
  labs(
    x="Scenes",
    y="Normalized Accesses"
  ) +
  scale_pattern_manual(
    values=c("Baseline"="none", "AQB48"="stripe"),
    guide=guide_legend(title=NULL, order=1)
  ) +
  scale_fill_manual(
    values=c("Baseline"="#dccbc0", "AQB48"="#b8947f"),
    guide=guide_legend(title=NULL, order=2)
  ) +
  scale_y_continuous(expand=expansion(mult=c(0, 0))) +
  theme_minimal(base_family="Noto Serif") +
  theme(
    legend.position="top",
    legend.key.size=unit(0.4, "cm"),
    legend.text=element_text(size=11, color="grey20"),
    axis.text.y=element_text(size=11, color="grey20"),
    axis.title=element_text(size=16, color="black"),
    strip.text.x=element_text(size=12, color="black", face="bold")
  )

ggsave("ca.pdf", width=6.9, height=5)
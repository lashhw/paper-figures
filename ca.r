library(tidyverse)
library(extrafont)
library(ggpattern)

data <- tribble(
     ~level, ~category,        ~type,       ~KIT,       ~BA,      ~BMW,      ~CLA,      ~HOU,      ~STR,      ~TEA,
  "(a) L1D",     "hit", "Baseline-2", 336367950, 391836458, 342548495, 494276013, 200362442, 420286412, 280598263,
  "(a) L1D",    "miss", "Baseline-2",  14858739,  20181052,  10513759,  36172892,   7031566,  13812596,  31405589,
   "(b) L2",     "hit", "Baseline-2",  11777387,  14738415,   8717445,  33831909,   6475336,  12899248,  25398815,
   "(b) L2",    "miss", "Baseline-2",   3081352,   5442637,   1796314,   2340983,    556230,    913348,   6006774,
  "(a) L1D",     "hit",    "AQB48-2", 167123393, 187451494, 191240674, 270610515, 102587033, 205750271, 159283689,
  "(a) L1D",    "miss",    "AQB48-2",   6385779,   9877892,   3944477,  14884634,   2562509,   5029540,  13375629,
   "(b) L2",     "hit",    "AQB48-2",   4947829,   7373719,   3122416,  14292742,   2326889,   4614565,  11276150,
   "(b) L2",    "miss",    "AQB48-2",   1437950,   2504173,    822061,    591892,    235620,    414975,   2099479,
  "(a) L1D",     "hit", "Baseline-6", 302731155, 352652812, 308293646, 444848412, 180326198, 378257771, 252538437,
  "(a) L1D",    "miss", "Baseline-6",  13372865,  18162947,   9462383,  32555603,   6328409,  12431336,  28265030,
   "(b) L2",     "hit", "Baseline-6",  10599648,  13264574,   7845701,  30448718,   5827802,  11609323,  22858934,
   "(b) L2",    "miss", "Baseline-6",   2773217,   4898373,   1616683,   2106885,    500607,    822013,   5406097,
  "(a) L1D",     "hit",    "AQB48-6", 150411054, 168706345, 172116607, 243549464,  92328330, 185175244, 143355320,
  "(a) L1D",    "miss",    "AQB48-6",   5747201,   8890103,   3550029,  13396171,   2306258,   4526586,  12038066,
   "(b) L2",     "hit",    "AQB48-6",   4453046,   6636347,   2810174,  12863468,   2094200,   4153109,  10148535,
   "(b) L2",    "miss",    "AQB48-6",   1294155,   2253756,    739855,    532703,    212058,    373478,   1889531,
)

data_long <- data |>
  pivot_longer(cols=-c(level, category, type), names_to="scene", values_to="value") |>
  group_by(level, scene) |>
  mutate(value_norm=value/sum(value[type=="Baseline-2"])) |>
  print()

data_long_mean <- data_long |>
  group_by(level, category, type) |>
  summarise(value_norm=mean(value_norm), .groups="drop") |>
  mutate(scene="MEAN") |>
  print()

data_long_combined <- bind_rows(data_long, data_long_mean) |>
  mutate(
    level=factor(level, levels=unique(level)),
    category=factor(category, levels=unique(category)),
    type=factor(type, levels=unique(type)),
    scene=factor(scene, levels=c("KIT", "BA", "BMW", "CLA", "HOU", "STR", "TEA", "MEAN"))
  ) |>
  select(-value) |>
  print()

fig <- ggplot(data_long_combined) +
  geom_col_pattern(
    aes(x=type, y=value_norm, fill=category, pattern=type, pattern_spacing=type),
    position="stack",
    color="black",
    width=1.0,
    linewidth=0.3,
    pattern_density=0.01,
    pattern_color="#765541"
  ) +
  facet_grid(level~scene, switch="x", scales="free_y") +
  labs(
    x="Scenes",
    y="Normalized Cache\nRequests (Lines)"
  ) +
  scale_pattern_manual(
    values=c("Baseline-2"="none", "AQB48-2"="stripe", "Baseline-6"="circle", "AQB48-6"="crosshatch"),
    guide=guide_legend(title=NULL, order=1)
  ) +
  scale_pattern_spacing_manual(
    values=c("Baseline-2"=0.12, "AQB48-2"=0.12, "Baseline-6"=0.09, "AQB48-6"=0.12),
    guide="none"
  ) +
  scale_fill_manual(
    values=c("hit"="#dccbc0", "miss"="#b8947f"),
    labels=c("hit"="Cache Hit", "miss"="Cache Miss"),
    guide=guide_legend(title=NULL, order=2)
  ) +
  scale_x_discrete(expand=expansion(mult=c(0.3, 0.3))) +
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
    panel.spacing.y=unit(0.65, "cm"), 
    panel.grid.major.x = element_blank()
  )

ggsave("ca.pdf", width=7.2, height=3.0)

data_l2_miss <- data_long_combined |>
  filter(level=="(b) L2", category=="miss") |>
  select(-level, -category) |>
  group_by(scene) |>
  mutate(value_norm=value_norm/value_norm[type=="Baseline-2"]) |>
  print()

fig <- ggplot(data_l2_miss) +
  geom_col_pattern(
    aes(x=type, y=value_norm, fill=type, pattern=type, pattern_color=type, pattern_spacing=type),
    position="stack",
    color="black",
    width=1.0,
    linewidth=0.3,
    pattern_density=0.01,
  ) +
  facet_wrap(~scene, nrow=1, strip.position="bottom") +
  labs(
    title="(c) Off-Chip Memory",
    x="Scenes",
    y="Normalized Mem\nAccesses (Bytes)"
  ) +
  scale_pattern_manual(
    values=c("Baseline-2"="none", "AQB48-2"="stripe", "Baseline-6"="circle", "AQB48-6"="crosshatch"),
    guide="none"
  ) +
  scale_pattern_spacing_manual(
    values=c("Baseline-2"=0.12, "AQB48-2"=0.12, "Baseline-6"=0.09, "AQB48-6"=0.12),
    guide="none"
  ) +
  scale_pattern_color_manual(
    values=c("Baseline-2"="#765541", "AQB48-2"="#765541", "Baseline-6"="#3b2e25", "AQB48-6"="#765541"),
    guide="none"
  ) +
  scale_fill_manual(
    values=c("Baseline-2"="#e5d8d1", "AQB48-2"="#b8947f", "Baseline-6"="#765541", "AQB48-6"="#3b2e25"),
    guide=guide_legend(title=NULL)
  ) +
  scale_x_discrete(expand=expansion(mult=c(0.3, 0.3))) +
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
    panel.spacing.y=unit(0.65, "cm"),
    panel.grid.major.x = element_blank(),
    plot.title=element_text(size=16, color="black", face="bold", hjust=0.5)
  )

ggsave("traffic.pdf", width=6.9, height=2.4)
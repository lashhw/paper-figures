library(tidyverse)
library(extrafont)
library(ggpattern)

data <- tribble(
     ~level, ~category,        ~type,       ~KIT,       ~BA,      ~BMW,      ~CLA,      ~HOU,      ~STR,      ~TEA,
  "(a) L1D",     "hit", "Baseline-2", 119216632, 141211820, 119502622, 147778503,  98657812, 238145556,  42943561,
  "(a) L1D",    "miss", "Baseline-2",    670466,    434118,    656652,    121754,    673067,   1858186,    202620,
   "(b) L2",     "hit", "Baseline-2",    262478,    304673,    439131,     85112,    591028,   1737586,    140507,
   "(b) L2",    "miss", "Baseline-2",    407988,    129445,    217521,     36642,     82039,    120600,     62113,
  "(a) L1D",     "hit", "Compress-2",  76636290,  90094387,  81048494,  93439944,  64160145, 158546521,  29247997,
  "(a) L1D",    "miss", "Compress-2",    462822,    204100,    304820,     62438,    212752,    506265,    108633,
   "(b) L2",     "hit", "Compress-2",    148163,    105467,    143980,     37155,    153589,    415395,     61348,
   "(b) L2",    "miss", "Compress-2",    314659,     98633,    160840,     25283,     59163,     90870,     47285,
  "(a) L1D",     "hit",    "AQB48-2",  53040091,  63666177,  62885412,  67463571,  49269288, 101977796,  24957168,
  "(a) L1D",    "miss",    "AQB48-2",    299412,    135373,    205983,     50967,     94207,    216521,     77226,
   "(b) L2",     "hit",    "AQB48-2",     72362,     58719,     93488,     28743,     48697,    143766,     42036,
   "(b) L2",    "miss",    "AQB48-2",    227050,     76654,    112495,     22224,     45510,     72755,     35190,
  "(a) L1D",     "hit", "Baseline-6", 119216632, 141211820, 119502622, 147778503,  98657812, 238145556,  42943561,
  "(a) L1D",    "miss", "Baseline-6",    670466,    434118,    656652,    121754,    673067,   1858186,    202620,
   "(b) L2",     "hit", "Baseline-6",    262478,    304673,    439131,     85112,    591028,   1737586,    140507,
   "(b) L2",    "miss", "Baseline-6",    407988,    129445,    217521,     36642,     82039,    120600,     62113,
  "(a) L1D",     "hit",    "AQB48-6",  53040091,  63666177,  62885412,  67463571,  49269288, 101977796,  24957168,
  "(a) L1D",    "miss",    "AQB48-6",    299412,    135373,    205983,     50967,     94207,    216521,     77226,
   "(b) L2",     "hit",    "AQB48-6",     72362,     58719,     93488,     28743,     48697,    143766,     42036,
   "(b) L2",    "miss",    "AQB48-6",    227050,     76654,    112495,     22224,     45510,     72755,     35190,
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
    aes(x=type, y=value_norm, fill=category, pattern=type, pattern_density=type, pattern_angle=type),
    position="stack",
    color="black",
    width=1.0,
    linewidth=0.3,
    pattern_color="#765541",
    pattern_spacing=0.05,
    pattern_size=0.3
  ) +
  facet_grid(level~scene, switch="x", scales="free_y") +
  labs(
    x="Scenes",
    y="Normalized Cache\nRequests (Lines)"
  ) +
  scale_pattern_manual(
    values=c("Baseline-2"="none", "Compress-2"="stripe", "AQB48-2"="crosshatch", "Baseline-6"="circle", "AQB48-6"="crosshatch"),
    guide=guide_legend(title=NULL, order=1)
  ) +
  scale_pattern_density_manual(
    values=c("Baseline-2"=0.01, "Compress-2"=0.01, "AQB48-2"=0.01, "Baseline-6"=0.1, "AQB48-6"=0.01),
    guide="none"
  ) +
  scale_pattern_angle_manual(
    values=c("Baseline-2"=0, "Compress-2"=30, "AQB48-2"=30, "Baseline-6"=30, "AQB48-6"=0),
    guide="none"
  ) +
  scale_fill_manual(
    values=c("hit"="#dccbc0", "miss"="#b8947f"),
    labels=c("hit"="Hit", "miss"="Miss"),
    guide=guide_legend(title=NULL, order=2)
  ) +
  scale_x_discrete(expand=expansion(mult=c(0.2, 0.2))) +
  scale_y_continuous(expand=expansion(mult=c(0, 0))) +
  theme_minimal(base_family="Noto Serif") +
  theme(
    legend.position="top",
    legend.key.size=unit(0.4, "cm"),
    legend.text=element_text(size=11, color="grey20"),
    axis.text.x=element_blank(),
    axis.text.y=element_text(size=11, color="grey20"),
    axis.title=element_text(size=14, color="black"),
    strip.text.x=element_text(size=12, color="grey20"),
    strip.text.y=element_text(size=14, color="black", face="bold"),
    panel.spacing.x=unit(0, "cm"),
    panel.spacing.y=unit(0.65, "cm"), 
    panel.grid.major.x = element_blank()
  )

ggsave("ca.pdf", width=8.2, height=3.0)

data_l2_miss <- data_long_combined |>
  filter(level=="(b) L2", category=="miss") |>
  select(-level, -category) |>
  group_by(scene) |>
  mutate(value_norm=value_norm/value_norm[type=="Baseline-2"]) |>
  print()

fig <- ggplot(data_l2_miss) +
  geom_col_pattern(
    aes(x=type, y=value_norm, pattern=type, pattern_density=type, pattern_angle=type),
    position="stack",
    color="black",
    fill="#dccbc0",
    width=1.0,
    linewidth=0.3,
    pattern_spacing=0.05,
    pattern_size=0.3,
    pattern_color="#765541",
  ) +
  facet_wrap(~scene, nrow=1, strip.position="bottom") +
  labs(
    title="(c) DRAM",
    x="Scenes",
    y="Normalized Mem\nAccesses (Bytes)"
  ) +
  scale_pattern_manual(
    values=c("Baseline-2"="none", "Compress-2"="stripe", "AQB48-2"="crosshatch", "Baseline-6"="circle", "AQB48-6"="crosshatch"),
    guide="none"
  ) +
  scale_pattern_density_manual(
    values=c("Baseline-2"=0.01, "Compress-2"=0.01, "AQB48-2"=0.01, "Baseline-6"=0.1, "AQB48-6"=0.01),
    guide="none"
  ) +
  scale_pattern_angle_manual(
    values=c("Baseline-2"=0, "Compress-2"=30, "AQB48-2"=30, "Baseline-6"=30, "AQB48-6"=0),
    guide="none"
  ) +
  scale_x_discrete(expand=expansion(mult=c(0.2, 0.2))) +
  scale_y_continuous(expand=expansion(mult=c(0, 0))) +
  theme_minimal(base_family="Noto Serif") +
  theme(
    legend.position="top",
    legend.key.size=unit(0.4, "cm"),
    legend.text=element_text(size=11, color="grey20"),
    axis.text.x=element_blank(),
    axis.text.y=element_text(size=11, color="grey20"),
    axis.title.x=element_text(size=14, color="black"),
    axis.title.y=element_text(size=14, color="black", hjust=0.65),
    strip.text.x=element_text(size=12, color="grey20"),
    strip.text.y=element_text(size=12, color="black", face="bold"),
    panel.spacing.x=unit(0, "cm"),
    panel.spacing.y=unit(0.65, "cm"),
    panel.grid.major.x = element_blank(),
    plot.title=element_text(size=14, color="black", face="bold", hjust=0.5)
  )

ggsave("traffic.pdf", width=7.9, height=1.8)
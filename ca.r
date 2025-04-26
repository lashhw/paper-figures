library(tidyverse)
library(extrafont)
library(ggpattern)

data <- tribble(
     ~level, ~category,      ~type, ~branch,     ~KIT,      ~BA,    ~BMW,     ~CLA,     ~HOU,     ~STR,    ~TEA,
  "(a) L1D",     "hit", "Baseline",     "2", 11544920, 22609530, 6351504, 21444156, 14234249, 36961640, 5901311,
  "(a) L1D",    "miss", "Baseline",     "2",  1224004,  3884442,  923760,  2396496,  1242079,  1847237,  767248,
   "(b) L2",     "hit", "Baseline",     "2",   945115,  2634326,  571442,  2249737,   991496,  1665698,  565686,
   "(b) L2",    "miss", "Baseline",     "2",   278889,  1250116,  352318,   146759,   250583,   181539,  201562,
  "(a) L1D",     "hit", "Compress",     "2",  4421333,  9040115, 2538406,  7317367,  5339704, 16283592, 2078955,
  "(a) L1D",    "miss", "Compress",     "2",   798787,  2488756,  624560,  1449974,   800869,  1103333,  513884,
   "(b) L2",     "hit", "Compress",     "2",   625985,  1710898,  414297,  1381383,   656017,   982871,  400510,
   "(b) L2",    "miss", "Compress",     "2",   172802,   777858,  210263,    68591,   144852,   120462,  113374,
  "(a) L1D",     "hit",     "AQB8",     "2",  3658981,  6680804, 1587135,  6627819,  3151786, 12763490, 1463804,
  "(a) L1D",    "miss",     "AQB8",     "2",   387349,  1316959,  340811,   767106,   468041,   722417,  268170,
   "(b) L2",     "hit",     "AQB8",     "2",   310050,   957797,  248104,   732406,   405394,   658801,  216289,
   "(b) L2",    "miss",     "AQB8",     "2",    77299,   359162,   92707,    34700,    62647,    63616,   51881,
  "(a) L1D",     "hit", "Baseline",     "6", 16605267, 24336271, 7104081, 32604866, 18707887, 27577148, 9531879,
  "(a) L1D",    "miss", "Baseline",     "6",  1243709,  3913363,  801546,  2074369,  1232605,  1479162,  803992,
   "(b) L2",     "hit", "Baseline",     "6",   913557,  2526773,  497811,  1923560,   971830,  1286790,  554429,
   "(b) L2",    "miss", "Baseline",     "6",   330152,  1386590,  303735,   150809,   260775,   192372,  249563,
  "(a) L1D",     "hit", "Compress",     "6",  4409864,  7709472, 1843487,  7263645,  5003490,  9130007, 2275491,
  "(a) L1D",    "miss", "Compress",     "6",   545908,  1826403,  418916,   919709,   592006,   778792,  428445,
   "(b) L2",     "hit", "Compress",     "6",   390151,  1132238,  275658,   862392,   473065,   667787,  306983,
   "(b) L2",    "miss", "Compress",     "6",   155757,   694165,  143258,    57317,   118941,   111005,  121462,
  "(a) L1D",     "hit",     "AQB8",     "6",  3293867,  5949092, 1369693,  6330556,  3238714,  8516754, 1456574,
  "(a) L1D",    "miss",     "AQB8",     "6",   538664,  1580245,  366993,  1012432,   562904,   766265,  354157,
   "(b) L2",     "hit",     "AQB8",     "6",   428760,  1101807,  275836,   971139,   481647,   690928,  275318,
   "(b) L2",    "miss",     "AQB8",     "6",   109904,   478438,   91157,    41293,    81257,    75337,   78839,
)

data_long <- data |>
  pivot_longer(cols=-c(level, category, type, branch), names_to="scene", values_to="value") |>
  group_by(level, branch, scene) |>
  mutate(value_norm=value/sum(value[type=="Baseline"])) |>
  ungroup() |>
  select(-value) |>
  print()

data_long_mean <- data_long |>
  group_by(level, category, type, branch) |>
  summarise(value_norm=mean(value_norm), .groups="drop") |>
  mutate(scene="MEAN") |>
  print()

data_long_combined <- bind_rows(data_long, data_long_mean) |>
  unite(type_branch, type, branch, sep="-", remove=FALSE) |>
  mutate(
    level=factor(level, levels=unique(level)),
    category=factor(category, levels=unique(category)),
    type_branch=factor(type_branch, levels=c("Baseline-2", "Compress-2", "AQB8-2", " ", "Baseline-6", "Compress-6", "AQB8-6")),
    scene=factor(scene, levels=c("KIT", "BA", "BMW", "CLA", "HOU", "STR", "TEA", "MEAN"))
  ) |>
  print()

fig <- ggplot(data_long_combined) +
  geom_col_pattern(
    aes(x=type_branch, y=value_norm, fill=interaction(category, branch), pattern=type_branch, pattern_angle=branch, pattern_color=branch, pattern_size=type_branch),
    position="stack",
    color="black",
    width=1.0,
    linewidth=0.3,
    pattern_density=0.001,
    pattern_spacing=0.05
  ) +
  facet_grid(level~scene, switch="x", scales="free_y") +
  labs(
    x="Scenes",
    y="Normalized Cache\nRequests (Bytes)"
  ) +
  scale_fill_manual(
    values = c(
      "hit.2"="#d3bdb0",
      "miss.2"="#976c54",
      "hit.6"="#b0d3cf",
      "miss.6"="#54978f"
    ),
    labels = c(
      "hit.2" = "Hit",
      "miss.2" = "Miss",
      "hit.6" = "Hit-6",
      "miss.6" = "Miss-6"
    ),
    guide=guide_legend(title=NULL, order=2)
  ) +
  scale_pattern_manual(
    values=c("Baseline-2"="none", "Compress-2"="stripe", "AQB8-2"="crosshatch", "Baseline-6"="circle", "Compress-6"="stripe", "AQB8-6"="crosshatch"),
    guide=guide_legend(title=NULL, order=1)
  ) +
  scale_pattern_angle_manual(
    values=c("2"=30, "6"=0),
    guide="none"
  ) +
  scale_pattern_color_manual(
    values=c("2"="#664939", "6"="#396660"),
    guide="none"
  ) +
  scale_pattern_size_manual(
    values=c("Baseline-2"=0.15, "Compress-2"=0.15, "AQB8-2"=0.15, "Baseline-6"=0.3, "Compress-6"=0.15, "AQB8-6"=0.15),
    guide="none"
  ) +
  scale_x_discrete(expand=expansion(mult=c(0.1, 0.1)), drop=FALSE) +
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

ggsave("ca.pdf", width=7, height=3.0)

data_l2_miss <- data_long_combined |>
  filter(level=="(b) L2", category=="miss") |>
  select(-level, -category) |>
  group_by(branch, scene) |>
  mutate(value_norm=value_norm/value_norm[type=="Baseline"]) |>
  ungroup() |>
  print()

fig <- ggplot(data_l2_miss) +
  geom_col_pattern(
    aes(x=type_branch, y=value_norm, fill=branch, pattern=type_branch, pattern_angle=branch, pattern_color=branch, pattern_size=type_branch),
    position="stack",
    color="black",
    width=1.0,
    linewidth=0.3,
    pattern_density=0.001,
    pattern_spacing=0.05
  ) +
  facet_wrap(~scene, nrow=1, strip.position="bottom") +
  labs(
    title="(c) DRAM",
    x="Scenes",
    y="Normalized Mem\nAccesses (Bytes)"
  ) +
  scale_fill_manual(
    values=c("2"="#d3bdb0", "6"="#b0d3cf"),
    guide="none"
  ) +
  scale_pattern_manual(
    values=c("Baseline-2"="none", "Compress-2"="stripe", "AQB8-2"="crosshatch", "Baseline-6"="circle", "Compress-6"="stripe", "AQB8-6"="crosshatch"),
    guide="none"
  ) +
  scale_pattern_angle_manual(
    values=c("2"=30, "6"=0),
    guide="none"
  ) +
  scale_pattern_color_manual(
    values=c("2"="#664939", "6"="#396660"),
    guide="none"
  ) +
  scale_pattern_size_manual(
    values=c("Baseline-2"=0.15, "Compress-2"=0.15, "AQB8-2"=0.15, "Baseline-6"=0.3, "Compress-6"=0.15, "AQB8-6"=0.15),
    guide="none"
  ) +
  scale_x_discrete(expand=expansion(mult=c(0.1, 0.1)), drop=FALSE) +
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

ggsave("traffic.pdf", width=6.656, height=1.65)
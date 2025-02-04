library(tidyverse)
library(extrafont)
library(ggpattern)

data <- tribble(
  ~category,      ~type,     ~KIT,      ~BA,     ~BMW,     ~CLA,     ~HOU,     ~STR,     ~TEA,
  "Compute", "Baseline",  5150119,  6189042,  5803762,  6146847,  4272675, 11313759,  2149853,
     "SRAM", "Baseline",   194638,   223555,   186861,   238789,   155312,   362820,    71355,
    "Cache", "Baseline",  2827593,  3032998,  2750185,  2973268,  2294851,  5621574,   965690,
     "DRAM", "Baseline",  1357784,   430793,   723910,   121945,   273026,   401357,   206712,
  "Compute", "Compress",  5557308,  6618883,  6255652,  6763995,  4644777,  12179344, 2331651,
     "SRAM", "Compress",   276693,   318644,   278654,   346128,   233126,    532615,  102978,
    "Cache", "Compress",  1854086,  1906193,  1802444,  1873787,  1385711,   3392632,  645475,
     "DRAM", "Compress",  1047185,   328251,   535276,    84142,   196894,    302415,  157364,
  "Compute",    "AQB48",  2928350,  3477905,  4125350,  3484235,  2762135,   6296072, 1646146,
     "SRAM",    "AQB48",   217895,   254869,   241738,   281673,   196804,    387792,   98353,
    "Cache",    "AQB48",  1276799,  1345526,  1378790,  1357316,  1030612,   2135008,  541216,
     "DRAM",    "AQB48",   755622,   255105,   374383,    73961,   151457,    242129,  117112,
)

data_long <- data |>
  pivot_longer(cols=!c(category, type), names_to="scene", values_to="value") |>
  group_by(scene) |>
  mutate(value=value/sum(value[type == "Baseline"])) |>
  ungroup() |>
  print()

data_long_mean <- data_long |>
  group_by(type, category) |>
  summarise(value=mean(value), .groups="drop") |>
  mutate(scene="MEAN") |>
  print()

data_long_combined <- bind_rows(data_long, data_long_mean) |>
  mutate(
    category=factor(category, levels=c("Compute", "SRAM", "Cache", "DRAM")),
    type=factor(type, levels=c("Baseline", "Compress", "AQB48")),
    scene=factor(scene, levels=unique(scene))
  ) |>
  print()

fig <- ggplot(data_long_combined) +
  geom_col_pattern(
    aes(x=type, y=value, fill=category, pattern=type),
    position="stack",
    color="black",
    width=1,
    linewidth=0.3,
    pattern_density=0.01,
    pattern_spacing=0.12,
    pattern_color="#765541"
  ) +
  facet_wrap(~scene, nrow=1, strip.position="bottom") +
  labs(
    x="Scenes",
    y="Normalized Energy\nConsumption",
  ) +
  scale_pattern_manual(
    values=c("Baseline"="none", "Compress"="stripe", "AQB48"="crosshatch"),
    labels=c("Baseline"="Baseline-2", "Compress"="Compress-2", "AQB48"="AQB48-2"),
    guide=guide_legend(title=NULL)
  ) +
  scale_fill_manual(
    values=c("Compute"="#dccbc0", "SRAM"="#cab09f", "Cache"="#b8947f", "DRAM"="#a6795e"),
    guide=guide_legend(title=NULL)
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
    panel.spacing=unit(0.2,"cm"),
    panel.grid.major.x=element_blank(),
  )

ggsave("energy.pdf", width=7.5, height=3)
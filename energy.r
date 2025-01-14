library(tidyverse)
library(extrafont)
library(ggpattern)

data <- tribble(
  ~category,      ~type,     ~KIT,      ~BA,     ~BMW,     ~CLA,     ~HOU,     ~STR,     ~TEA,
  "Compute", "Baseline", 20076413, 23701757, 22636378, 29909062, 11779315, 25678904, 20259737,
     "SRAM", "Baseline",  1958380,  2227647,  2001914,  2942725,  1141204,  2508358,  1778708,
    "Cache", "Baseline", 15035960, 19649994, 12536491, 28167493,  7562070, 15306747, 23133405,
     "DRAM", "Baseline", 31553044, 55732603, 18394255, 23971666,  5695795,  9352684, 61509366,
  "Compute",    "AQB48", 12797597, 14201975, 16306777, 20526485,  7640964, 16303283, 14194984,
     "SRAM",    "AQB48",  2888101,  3250191,  2882052,  4703179,  1658227,  3468712,  2558024,
    "Cache",    "AQB48",  6950230,  9468802,  5993460, 12760688,  3349207,  6640900, 10449123,
     "DRAM",    "AQB48", 14724608, 25642732,  8417905,  6060974,  2412749,  4249344, 21498665,
)

data_long <- data |>
  pivot_longer(cols=!c(category, type), names_to="scene", values_to="value") |>
  print()

data_long_mean <- data_long |>
  group_by(type, category) |>
  summarise(value=mean(value), .groups="drop") |>
  mutate(scene="MEAN") |>
  print()

data_long_combined <- bind_rows(data_long, data_long_mean) |>
  group_by(scene) |>
  mutate(value_normalized=value / sum(value[type == "Baseline"])) |>
  ungroup() |>
  mutate(
    category=factor(category, levels=c("Compute", "SRAM", "Cache", "DRAM")),
    type=factor(type, levels=c("Baseline", "AQB48")),
    scene=factor(scene, levels=unique(scene))
  ) |>
  print()

data_reduction <- data_long_combined |>
  group_by(type, scene) |>
  summarise(total=1-sum(value_normalized), y_location=sum(value), .groups="drop") |>
  filter(type == "AQB48") |>
  print()

fig <- ggplot(data_long_combined) +
  geom_col_pattern(
    aes(x=type, y=value, fill=category, pattern=type),
    position="stack",
    color="black",
    width=0.75,
    linewidth=0.3,
    pattern_density=0.01,
    pattern_spacing=0.12,
    pattern_color="#765541"
  ) +
  geom_text(
    data=data_reduction,
    aes(x=2, y=y_location, label=sprintf("-%.0f%%", total*100), fill=NULL),
    color="grey20",
    size=3,
    family="Noto Serif",
    vjust=-0.5,
  ) +
  facet_wrap(~scene, nrow=1, strip.position="bottom") +
  labs(
    x="Scenes",
    y="Energy Consumption (nJ)",
  ) +
  scale_pattern_manual(
    values=c("Baseline"="none", "AQB48"="stripe"),
    labels=c("Baseline"="Baseline-2", "AQB48"="AQB48-2"),
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
    panel.spacing=unit(0,"cm")
  )

ggsave("energy.pdf", width=6.5, height=3.2)
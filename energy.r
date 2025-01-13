library(tidyverse)
library(extrafont)

data <- tribble(
  ~category,      ~type,     ~KIT,      ~BA,     ~BMW,     ~CLA,     ~HOU,     ~STR,     ~TEA,
  "Compute", "Baseline", 20076413, 23701757, 22636378, 29909062, 11779315, 25678904, 20259737,
     "SRAM", "Baseline",  1958380,  2227647,  2001914,  2942725,  1141204,  2508358,  1778708,
    "Cache", "Baseline", 15035960, 19649994, 12536491, 28167493,  7562070, 15306747, 23133405,
     "DRAM", "Baseline", 18960375, 33489987, 11053196, 14404689,  3422631,  5620072, 36961272,
  "Compute",    "AQB48", 12797597, 14201975, 16306777, 20526485,  7640964, 16303283, 14194984,
     "SRAM",    "AQB48",  2888101,  3250191,  2882052,  4703179,  1658227,  3468712,  2558024,
    "Cache",    "AQB48",  6950230,  9468802,  5993460, 12760688,  3349207,  6640900, 10449123,
     "DRAM",    "AQB48",  8848087, 15408840,  5058359,  3642068,  1449832,  2553451, 12918651,
)

data_long <- data |>
  pivot_longer(cols=!c(category, type), names_to="scene", values_to="value") |>
  group_by(scene) |>
  mutate(value_normalized=value / sum(value[type == "Baseline"])) |>
  ungroup() |>
  print()

data_long_mean <- data_long |>
  group_by(type, category) |>
  summarise(value_normalized=exp(mean(log(value_normalized))), .groups="drop") |>
  mutate(scene = "GMEAN") |>
  print()

data_long_combined <- bind_rows(data_long, data_long_mean) |>
  mutate(
    category=factor(category, levels=c("Compute", "SRAM", "Cache", "DRAM")),
    type=factor(type, levels=c("Baseline", "AQB48")),
    scene=factor(scene, levels=unique(scene))
  )

data_reduction <- data_long_combined |>
  group_by(type, scene) |>
  summarise(total=1-sum(value_normalized), .groups="drop") |>
  filter(type == "AQB48") |>
  print()

fig <- ggplot(data_long_combined, aes(x=type, y=value_normalized, fill=category)) +
  geom_col(
    position="stack",
    color="black",
    width=0.7,
    linewidth=0.3
  ) +
  geom_text(
    data=data_reduction,
    aes(x=2, y=1.07-total, label=sprintf("-%.0f%%", total*100), fill=NULL),
    color="black",
    size=3,
    family="Noto Serif"
  ) +
  facet_wrap(~scene, nrow=1) +
  labs(
    x="Scenes",
    y="Normalized Energy\nConsumption",
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
    axis.text.x=element_text(size=11, color="grey20", angle=45),
    axis.text.y=element_text(size=11, color="grey20"),
    axis.title=element_text(size=16, color="black"),
    strip.text.x=element_text(size=12, color="black")
  )

ggsave("energy.pdf", width=7, height=3.5)
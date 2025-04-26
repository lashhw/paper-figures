library(tidyverse)
library(extrafont)

data <- tribble(
   ~category,                          ~type,      ~KIT,       ~BA,      ~BMW,       ~CLA,      ~HOU,      ~STR,      ~TEA,
       "nbp",  "(a) L1D Accesses (In Bytes)", 811278416, 955359350, 734093258, 1245992706, 439372298, 897508091, 663389863,
  "Triangle",  "(a) L1D Accesses (In Bytes)", 137632025, 164605296, 271490642,  183551714, 104263447, 274059931, 238551654,
  "prim_inx",  "(a) L1D Accesses (In Bytes)",         0,         0,         0,          0,         0,         0,         0,
       "nbp", "(b) DRAM Accesses (In Bytes)",  12869367,  18784942,   4578530,   18605656,   9083478,  20103798,   5450680,
  "Triangle", "(b) DRAM Accesses (In Bytes)",   2996350,   3078260,   1325845,    2602306,   2017782,   4850730,   1350989,
  "prim_inx", "(b) DRAM Accesses (In Bytes)",   1832978,   1682079,    772585,    1393751,   1233747,   3133155,    801709,
)

data_long <- data |>
  pivot_longer(cols=!c(category, type), names_to="scene", values_to="value")

nbp_data <- data_long |>
  filter(category == "nbp")

triangle_data <- data_long |>
  filter(category == "Triangle") |>
  mutate(value=ifelse(grepl("DRAM", type), value*36, value))

prim_idx_data <- data_long |>
  filter(category == "prim_inx") |>
  mutate(value=value*8)

bb_data <- nbp_data |>
  mutate(
    category="Bounding Box",
    value=ifelse(grepl("DRAM", type), value*48, value*(48/56))
  )

node_data <- nbp_data |>
  mutate(
    category="Other",
    value=ifelse(grepl("DRAM", type), value*8, value*(8/56))
  )

node_data <- node_data |>
  left_join(prim_idx_data |> select(type, scene, prim_value=value), by=c("type", "scene")) |>
  mutate(value = value + prim_value) |>
  select(-prim_value)

data_long <- bind_rows(triangle_data, bb_data, node_data) |>
  group_by(scene, type) |>
  mutate(percentage=value/sum(value)) |>
  ungroup() |>
  select(!value) |>
  mutate(category=factor(category, levels=c("Triangle", "Bounding Box", "Other"))) |>
  print()

gmean_data <- data_long |>
  group_by(category, type) |>
  summarise(percentage=mean(percentage)) |>
  ungroup() |>
  mutate(scene="MEAN") |>
  print()

data_combined <- bind_rows(data_long, gmean_data) |>
  mutate(scene=factor(scene, levels=unique(scene)))

fig <- ggplot(data_combined, aes(x=scene, y=percentage, fill=category)) +
  geom_col(
    color="black",
    width=0.7,
    linewidth=0.3
  ) +
  geom_text(
    data = data_combined |> filter(scene == "MEAN" & category != "Other"),
    aes(label=scales::percent(percentage, accuracy=1)),
    position=position_stack(),
    size=3,
    angle=45,
    family="Noto Serif",
    color="grey20"
  ) +
  facet_wrap(~type) +
  labs(
    x="Scenes",
    y="Percentage",
  ) +
  scale_fill_manual(
    values=c("Triangle"="#af876e", "Bounding Box"="#d3beaf", "Other"="#f7f4f1"),
    breaks=c("Other", "Bounding Box", "Triangle"),
    guide=guide_legend(title=NULL)
  ) +
  scale_y_continuous(
    expand=expansion(mult=c(0, 0)),
    labels=scales::percent_format()
  ) +
  theme_minimal(base_family="Noto Serif") +
  theme(
    legend.position="top",
    legend.key.size=unit(0.4, "cm"),
    legend.text=element_text(size=11, color="grey20"),
    axis.text.x=element_text(size=11, color="grey20", angle=45),
    axis.text.y=element_text(size=11, color="grey20"),
    axis.title=element_text(size=16, color="black"),
    strip.text.x=element_text(size=12, color="black", face="bold")
  )

ggsave("mem.pdf", width=7, height=3.5)

library(tidyverse)
library(extrafont)
library(ggpattern)
library(ggalluvial)

data <- tribble(
       ~type, ~category,        ~TRV,      ~QBOX,        ~BOX,       ~TRIG,      ~OTHER, 
  "Baseline",    "area", 7879.810646,          0,  82128.1334, 192918.5692, 90310.93536,
  "Baseline",   "count",          64,          0, 50.87768563, 10.93243687,           1,
  "Compress",    "area", 8116.816626,          0, 90575.52582, 192918.5692, 109541.7733,
  "Compress",   "count", 64.96359857,          0, 51.67897413, 11.09474694,           1,
     "AQB48",    "area", 9799.874552,16081.48044, 97048.85129, 192918.5692, 102510.2758,
     "AQB48",   "count", 77.99180773,53.17492914, 8.782606146,  12.5705611,           1,
)

data_long <- data |>
  pivot_longer(cols=!c(type, category), names_to="hwunit", values_to="value") |>
  pivot_wider(names_from=category, values_from=value) |>
  mutate(unit_area=area*count) |>
  mutate(
    type=factor(type, levels=c("AQB48", "Compress", "Baseline")),
    hwunit=factor(hwunit, levels=c("OTHER", "TRIG", "BOX", "QBOX", "TRV")),
  ) |>
  print()

data_increment <- data_long |>
  group_by(type) |>
  summarise(total_area=sum(unit_area)) |>
  mutate(Baseline=total_area[type=="Baseline"]) |>
  filter(type != "Baseline") |>
  mutate(ratio=(total_area-Baseline)/Baseline) |>
  print()

fig <- ggplot(data_long) +
  geom_flow(
    aes(x=type, y=unit_area, alluvium=hwunit, fill=hwunit),
    width=0.65,
    linewidth=0.1,
    alpha=0.8,
    color="black"
  ) +
  geom_col_pattern(
    aes(x=type, y=unit_area, fill=hwunit, pattern=hwunit, pattern_density=hwunit),
    position="stack",
    color="black",
    width=0.65,
    linewidth=0.3,
    pattern_spacing=0.12,
    pattern_color="#765541",
    pattern_fill="#765541"
  ) +
  geom_label(
    aes(x=type, y=unit_area, label=round(count), fill=hwunit),
    color="black",
    position=position_stack(vjust=0.5),
    size=3.6,
    fontface="bold",
    label.padding=unit(0.1, "lines"),
    label.size=0,
    family="Noto Serif",
    show.legend=FALSE,
    data=\(x) filter(x, !(hwunit == "OTHER" | ((type == "Baseline" | type == "Compress") & hwunit == "QBOX")))
  ) +
  geom_text(
    data=data_increment,
    aes(x=type, y=total_area, label=sprintf("%+.0f%%", ratio*100)),
    color="grey20",
    vjust=-0.9,
    angle=-90,
    size=3.4,
    fontface="bold",
    family="Noto Serif"
  ) +
  scale_pattern_manual(
    values=c("TRV"="none", "QBOX"="crosshatch", "BOX"="stripe", "TRIG"="circle", "OTHER"="none"),
    guide="none"
  ) +
  scale_pattern_density_manual(
    values=c("TRV"=0, "QBOX"=0.01, "BOX"=0.01, "TRIG"=0.15, "OTHER"=0),
    guide="none"
  ) +
  scale_fill_manual(
    values=c("TRV"="#dccbc0", "QBOX"="#cab09f", "BOX"="#b8947f", "TRIG"="#a6795e", "OTHER"="#8d5d3e"),
    guide=guide_legend(title=NULL),
    breaks=c("TRV", "QBOX", "BOX", "TRIG", "OTHER")
  ) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.07))) +
  scale_x_discrete(labels=c("AQB48-2", "Compress-2", "Baseline-2")) +
  labs(y=expression("Area"~(mu*m^2))) +
  theme_minimal(base_family="Noto Serif") +
  theme(
    legend.position="top",
    legend.key.size=unit(0.4, "cm"),
    legend.text=element_text(size=11, color="grey20"),
    axis.text.x=element_text(size=9.2, color="grey20"),
    axis.text.y=element_text(size=11, color="black"),
    axis.title.x=element_text(size=13, color="black"),
    axis.title.y=element_blank()
  ) +
  coord_flip()

ggsave("area.pdf", width=6.5, height=2.3)
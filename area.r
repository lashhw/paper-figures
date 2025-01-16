library(tidyverse)
library(extrafont)
library(ggpattern)
library(ggalluvial)

data <- tribble(
       ~type, ~category,        ~TRV,      ~QBOX,        ~BOX,       ~TRIG,      ~OTHER, 
  "Baseline",    "area", 79254.02042,          0,  82128.1334, 192918.5692, 82480.35363, 
  "Baseline",   "count",          64,          0,          51,          11,           1, 
     "AQB48",    "area", 110429.3865,16081.48044, 97048.85129, 192918.5692, 93108.21448, 
     "AQB48",   "count", 81.83586482,55.20538836, 8.551981553, 14.87624299,           1, 
)

data_long <- data |>
  pivot_longer(cols=!c(type, category), names_to="hwunit", values_to="value") |>
  pivot_wider(names_from=category, values_from=value) |>
  mutate(unit_area=area*count) |>
  mutate(
    type=factor(type, levels=c("AQB48", "Baseline")),
    hwunit=factor(hwunit, levels=c("OTHER", "TRIG", "BOX", "QBOX", "TRV")),
  ) |>
  print()

data_increment <- data_long |>
  group_by(type) |>
  summarise(total_area=sum(unit_area)) |>
  pivot_wider(names_from=type, values_from=total_area) |>
  mutate(increment=(AQB48-Baseline)/Baseline) |>
  print()

fig <- ggplot(data_long) +
  geom_flow(
    aes(x=type, y=unit_area, alluvium=hwunit, fill=hwunit),
    width=0.65,
    linewidth=0.1,
    alpha=0.8,
    color="black",
    curve_type="linear"
  ) +
  geom_col_pattern(
    aes(x=type, y=unit_area, fill=hwunit, pattern=type),
    position="stack",
    color="black",
    width=0.65,
    linewidth=0.3,
    pattern_density=0.01,
    pattern_spacing=0.12,
    pattern_color="#765541"
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
    data=\(x) filter(x, !(hwunit == "OTHER" | (type == "Baseline" & hwunit == "QBOX")))
  ) +
  geom_text(
    data=data_increment,
    aes(x="AQB48", y=AQB48, label=sprintf("+%.0f%%", increment*100)),
    color="grey20",
    vjust=-0.9,
    angle=-90,
    size=4,
    fontface="bold",
    family="Noto Serif",
    inherit.aes=FALSE
  ) +
  scale_pattern_manual(
    values=c("Baseline"="none", "AQB48"="stripe"),
    guide="none"
  ) +
  scale_fill_manual(
    values=c("TRV"="#dccbc0", "QBOX"="#cab09f", "BOX"="#b8947f", "TRIG"="#a6795e", "OTHER"="#8d5d3e"),
    guide=guide_legend(title=NULL),
    breaks=c("TRV", "QBOX", "BOX", "TRIG", "OTHER")
  ) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.07))) +
  scale_x_discrete(labels=c("AQB48-2", "Baseline-2")) +
  labs(y=expression("Area"~(mu*m^2))) +
  theme_minimal(base_family="Noto Serif") +
  theme(
    legend.position="top",
    legend.key.size=unit(0.4, "cm"),
    legend.text=element_text(size=11, color="grey20"),
    axis.text.x=element_text(size=9, color="grey20"),
    axis.text.y=element_text(size=11, color="black"),
    axis.title.x=element_text(size=13, color="black"),
    axis.title.y=element_blank(),
    panel.spacing=unit(0,"cm")
  ) +
  coord_flip()

ggsave("area.pdf", width=6.5, height=2.0)
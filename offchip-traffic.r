library(tidyverse)
library(extrafont)

data <- tribble(
       ~category,    ~KIT,     ~BA,    ~BMW,    ~CLA,   ~HOU,   ~STR,    ~TEA,
  "Bounding Box", 2369544, 3908831, 1263463, 1451032, 387877, 553699, 3973370,
      "Triangle",  756439, 1555584,  541289,  903982, 177387, 363432, 1907147
)

data_long <- data |>
  pivot_longer(cols=!category, names_to="scene", values_to="value") |>
  group_by(scene) |>
  mutate(percentage=value/sum(value)) |>
  ungroup() |>
  select(!value) |>
  mutate(category=factor(category, levels=c("Triangle", "Bounding Box"))) |>
  print()

gmean_data <- data_long |>
  group_by(category) |>
  summarise(percentage=exp(mean(log(percentage)))) |>
  ungroup() |>
  mutate(scene="GMEAN") |>
  print()

data_combined <- bind_rows(data_long, gmean_data) |>
  mutate(scene=factor(scene, levels=unique(scene)))

fig <- ggplot(data_combined, aes(x=scene, y=percentage, fill=category)) +
  geom_col(
    color="black",
    width=0.6,
    linewidth=0.3
  ) +
  labs(
    x="Scenes",
    y="Percentage of Off-Chip\nMemory Traffic",
  ) +
  scale_fill_manual(
    values=c("Triangle"="#bebada", "Bounding Box"="#ffffb3"),
    guide=guide_legend(title=NULL)
  ) +
  scale_y_continuous(
    expand=expansion(mult=c(0, 0)),
    labels=scales::percent_format()
  ) +
  theme_minimal(base_family="Noto Serif") +
  theme(
    legend.position="top",
    legend.key.size=unit(0.3, "cm"),
    legend.text=element_text(size=5, color="grey20"),
    axis.text.x=element_text(size=5, color="grey20", angle=45),
    axis.text.y=element_text(size=5, color="grey20"),
    axis.title=element_text(size=7, color="black")
  )

ggsave("mem.pdf", width=3, height=2)

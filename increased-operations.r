library(tidyverse)

data <- tribble(
  ~category,                                ~prec,  ~KIT,      ~BA,       ~BMW,      ~CLA,      ~HOU,      ~STR,      ~TEA,
  "(a) Ray-Box Intersection Test"     ,     "FP32", 80021178,  93703925,  72695635,  121964051, 45383870,  91295721,  64115325,
  "(a) Ray-Box Intersection Test"     ,     "FP16", 216869929, 157613286, 148872569, 155749309, 104859252, 114550687, 139065629,
  "(b) Ray-Triangle Intersection Test",     "FP32", 12321920,  14875502,  24052696,  16363234,  10038904,  27057171,  21861707,
  "(b) Ray-Triangle Intersection Test",     "FP16", 241888716, 120276941, 140252792, 55683089,  102925845, 65601058,  124571791
)

data_long <- data |>
  pivot_longer(cols=!c(category, prec), names_to="scene", values_to="value") |>
  group_by(category) |>
  mutate(normalized_value=value/min(value)) |>
  ungroup() |>
  mutate(
    prec=factor(prec, levels=c("FP32", "FP16")),
    scene=factor(scene, levels=unique(scene))
  )

data_ratio <- data_long |>
  select(category, prec, scene, normalized_value) |>
  pivot_wider(names_from=prec, values_from=normalized_value) |>
  mutate(ratio=FP16/FP32) |>
  group_by(category) |> 
  mutate(annotation_y=FP16+max(FP16)*0.07) |>
  ungroup()

print(data_ratio)

fig <- ggplot(data_long, aes(x=scene, y=normalized_value, fill=prec)) +
  geom_col(
    position="dodge",
    color="black",
    width=0.7,
    linewidth=0.3
  ) +
  geom_text(
    data=data_ratio,
    aes(x=scene, y=annotation_y, label=sprintf("%.1fx", ratio)),
    hjust=0.3,
    family="serif",
    inherit.aes=FALSE
  ) +
  facet_wrap(~category, scales="free_y") +
  labs(
    x="Scenes",
    y="Normalized Operation Counts",
    fill="Precisions"
  ) +
  scale_fill_manual(
    values = c("FP32"="#ffffb3", "FP16"="#bebada")
  ) +
  scale_y_continuous(
    expand=expansion(mult=c(0, 0.05)),
  ) +
  theme_minimal(base_family="serif") +
  theme(
    legend.position="none",
    axis.text.x = element_text(size=12, color="grey20"),
    axis.text.y = element_text(size=12, color="black"),
    axis.title = element_text(size=18, color="black"),
    strip.text = element_text(size=14, color="black", face="bold"),
  )

ggsave("trv-steps.pdf", width=7, height=3.2)
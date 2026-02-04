library(tidyverse)
library(ggbrace)
library(ggforce)
library(latex2exp)

# ---- SETUP ----

off <- .5
scale.factor <- 0.8
col.vals <- c("anchor" = "#F8CECC",
              "free" = "#DAE8FC",
              "target" = "#F5F5F5",
              "founder" = "white")

# --- DIRECT ----

nodes <- data.frame(
  id = c("founder", "desc1", "desc2", "desc3"),
  x = c(0, 0, 0, 0),
  y = c(6, 4, 2, 0),
  shape = c("diamond", "diamond", "diamond", "diamond"),
  type = c("target", "free", "free", "target")
)

diamond_nodes <- nodes |>
  filter(shape == "diamond") |>
  rowwise() |>
  do({
    x <- .$x
    y <- .$y
    col <- .$type
    data.frame(
      id = .$id,
      x = c(x - off, x, x + off, x),
      y = c(y, y + off, y, y - off),
      point = 1:4,
      type = .$type
    )
  })

edges <- data.frame(
  x = c(0, 0, 0),
  xend = c(0, 0, 0),
  y = c(6, 4, 2),
  yend = c(4, 2, 0),
  linetype = c("solid", "dashed", "solid")
)


ggplot() +
  geom_polygon(
    data = diamond_nodes,
    aes(x = x, y = y, group = id, fill = type),
    color = "black"
  ) +
  geom_point(data = data.frame(x = c(0, 0), y = c(0, 6)), aes(x, y)) +
  geom_segment(data = edges, aes(x = x, y = y-off, xend = xend, yend = yend+off),
               linetype = edges$linetype) +
  stat_brace(data = data.frame(x = c(0, 0), y = c(4.5, 1.5)), mapping = aes(x = x, y = y),
             rotate = 270, outerstart = -0.5, width = .5) +
  stat_bracetext(data.frame(x = c(2, 2), y = c(4.5, 1.5)), mapping = aes(x, y, label = latex2exp::TeX("$n")),
                 rotate = 270, outerstart = 0) +
  coord_equal(clip = "off") +
  scale_fill_manual(values = col.vals) +
  theme_void(base_size=14) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(-1, 9), expand = c(0, 0)) -> direct
direct

# --- HALF-SIB ----


nodes <- data.frame(
  id = c("founder", "desc11", "desc12", "desc21", "desc22", "desc31", "desc32"),
  x = c(1, 0, 2, 0, 2, 0, 2),
  y = c(6, 4, 4, 2, 2, 0, 0),
  shape = c("diamond", "diamond", "diamond", "diamond", "diamond", "diamond", "diamond"),
  type = c("anchor", "free", "free", "free", "free", "target", "target")
)

diamond_nodes <- nodes |>
  filter(shape == "diamond") |>
  rowwise() |>
  do({
    x <- .$x
    y <- .$y
    col <- .$type
    data.frame(
      id = .$id,
      x = c(x - off, x, x + off, x),
      y = c(y, y + off, y, y - off),
      point = 1:4,
      type = .$type
    )
  })

edges.top <- data.frame(
  x = c(1-off, 1+off, 0, 2),
  xend = c(0, 2, 0, 2),
  y = c(6, 6, 6, 6),
  yend = c(6, 6, 4+off, 4+off)
)

edges <- data.frame(
  x = c(0, 2, 0, 2),
  xend = c(0, 2, 0, 2),
  y = c(4, 4, 2, 2),
  yend = c(2, 2, 0, 0),
  linetype = c("dashed", "dashed", "solid", "solid")
)

ggplot() +
  geom_polygon(
    data = diamond_nodes,
    aes(x = x, y = y, group = id, fill = type),
    color = "black"
  ) +
  geom_point(data = data.frame(x = c(0, 2), y = c(0, 0)), aes(x, y)) +
  geom_segment(data = edges.top, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment(data = edges, aes(x = x, y = y-off, xend = xend, yend = yend+off),
              linetype = edges$linetype) +
  stat_brace(data = data.frame(x = c(0, 0), y = c(4.5, 1.5)), mapping = aes(x = x, y = y),
             rotate = 270, outerstart = -0.5, width = .5) +
  stat_brace(data = data.frame(x = c(2, 2), y = c(4.5, 1.5)), mapping = aes(x, y),
             rotate = 90, outerstart = 2.5, width = .5) +
  stat_bracetext(data.frame(x = c(2, 2), y = c(4.5, 1.5)), mapping = aes(x, y, label = latex2exp::TeX("$n_1")),
                 rotate = 270, outerstart = 0) +
  stat_bracetext(data = data.frame(x = c(2, 2), y = c(4.5, 1.5)), mapping = aes(x, y, label = latex2exp::TeX("$n_2")),
                 rotate = 90, outerstart = 2) +
  coord_equal(clip = "off") +
  scale_fill_manual(values = col.vals) +
  theme_void(base_size=14) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(-1, 9), expand = c(0, 0)) -> hsib
hsib

# --- AVUNCULAR TYPE ----

nodes <- data.frame(
  id = c("mother", "father", "desc11", "desc12",
         "desc21", "desc31", "desc41"),
  x = c(0, 2, 0, 2, 0, 0, 0),
  y = c(8, 8, 6, 6, 4, 2, 0),
  shape = c("square", "circle", "diamond", "diamond",
            "diamond", "diamond", "diamond"),
  type = c("founder", "founder", "anchor", "target",
           "free", "free", "target")
)

diamond_nodes <- nodes |>
  filter(shape == "diamond") |>
  rowwise() |>
  do({
    x <- .$x
    y <- .$y
    col <- .$type
    data.frame(
      id = .$id,
      x = c(x - off, x, x + off, x),
      y = c(y, y + off, y, y - off),
      point = 1:4,
      type = .$type
    )
  })

edges.top <- data.frame(
  x = c(0+off*scale.factor, 2-off*scale.factor, 1, 0, 2, 0, 2),
  xend = c(1, 1, 1, 1, 1, 0, 2),
  y = c(8, 8, 8, 7, 7, 7, 7),
  yend = c(8, 8, 7, 7, 7, 6+off, 6+off)
)

edges <- data.frame(
  x = c(0, 0, 0),
  xend = c(0, 0, 0),
  y = c(6, 4, 2),
  yend = c(4, 2, 0),
  linetype = c("solid", "dashed", "solid")
)

ggplot() +
  geom_rect(data = nodes[nodes$shape == "square",], mapping =
              aes(xmin = x - off*scale.factor, xmax = x + off*scale.factor, ymin = y-off*scale.factor, ymax = y+off*scale.factor,
                  fill = type),
            colour = "black") +
  geom_circle(data = nodes[nodes$shape == "circle",],
              aes(x0 = x, y0 = y, r = off*scale.factor, fill = type), color = "black") +
  geom_polygon(
    data = diamond_nodes,
    aes(x = x, y = y, group = id, fill = type),
    color = "black"
  ) +
  geom_point(data = data.frame(x = c(0, 2), y = c(0, 6)), aes(x, y)) +
  geom_segment(data = edges.top, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment(data = edges, aes(x = x, y = y-off, xend = xend, yend = yend+off),
               linetype = edges$linetype) +
  stat_brace(data = data.frame(x = c(0, 0), y = c(4.5, 1.5)), mapping = aes(x = x, y = y),
             rotate = 270, outerstart = -0.5, width = .5) +
  stat_bracetext(data.frame(x = c(2, 2), y = c(4.5, 1.5)), mapping = aes(x, y, label = latex2exp::TeX("$n")),
                 rotate = 270, outerstart = 0) +
  coord_equal(clip = "off") +
  scale_fill_manual(values = col.vals) +
  theme_void(base_size=14) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(-1, 9), expand = c(0, 0)) -> avuncular

# --- COUSIN TYPE ----

nodes <- data.frame(
  id = c("mother", "father", "desc11", "desc12",
         "desc21", "desc22", "desc31", "desc32",
         "desc41", "desc42"),
  x = c(0, 2, 0, 2,
        0, 2, 0, 2,
        0, 2),
  y = c(8, 8, 6, 6,
        4, 4, 2, 2,
        0, 0),
  shape = c("square", "circle", "diamond", "diamond",
            "diamond", "diamond", "diamond", "diamond",
            "diamond", "diamond"),
  type = c("founder", "founder", "anchor", "anchor",
           "free", "free", "free", "free",
           "target", "target")
)

diamond_nodes <- nodes |>
  filter(shape == "diamond") |>
  rowwise() |>
  do({
    x <- .$x
    y <- .$y
    col <- .$type
    data.frame(
      id = .$id,
      x = c(x - off, x, x + off, x),
      y = c(y, y + off, y, y - off),
      point = 1:4,
      type = .$type
    )
  })

edges.top <- data.frame(
  x = c(0+off*scale.factor, 2-off*scale.factor, 1, 0, 2, 0, 2),
  xend = c(1, 1, 1, 1, 1, 0, 2),
  y = c(8, 8, 8, 7, 7, 7, 7),
  yend = c(8, 8, 7, 7, 7, 6+off, 6+off)
)

edges <- data.frame(
  x = c(0, 2, 0, 2, 0, 2),
  xend = c(0, 2, 0, 2, 0, 2),
  y = c(6, 6, 4, 4, 2, 2),
  yend = c(4, 4, 2, 2, 0, 0),
  linetype = c("solid", "solid","dashed", "dashed",
               "solid", "solid")
)

ggplot() +
  geom_rect(data = nodes[nodes$shape == "square",], mapping =
              aes(xmin = x - off*scale.factor, xmax = x + off*scale.factor, ymin = y-off*scale.factor, ymax = y+off*scale.factor,
                  fill = type),
            colour = "black") +
  geom_circle(data = nodes[nodes$shape == "circle",],
              aes(x0 = x, y0 = y, r = off*scale.factor, fill = type), color = "black") +
  geom_polygon(
    data = diamond_nodes,
    aes(x = x, y = y, group = id, fill = type),
    color = "black"
  ) +
  geom_point(data = data.frame(x = c(0, 2), y = c(0, 0)), aes(x, y)) +
  geom_segment(data = edges.top, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment(data = edges, aes(x = x, y = y-off, xend = xend, yend = yend+off),
               linetype = edges$linetype) +
  stat_brace(data = data.frame(x = c(0, 0), y = c(4.5, 1.5)), mapping = aes(x = x, y = y),
             rotate = 270, outerstart = -0.5, width = .5) +
  stat_brace(data = data.frame(x = c(2, 2), y = c(4.5, 1.5)), mapping = aes(x, y),
             rotate = 90, outerstart = 2.5, width = .5) +
  stat_bracetext(data.frame(x = c(2, 2), y = c(4.5, 1.5)), mapping = aes(x, y, label = latex2exp::TeX("$n_1")),
                 rotate = 270, outerstart = 0) +
  stat_bracetext(data = data.frame(x = c(2, 2), y = c(4.5, 1.5)), mapping = aes(x, y, label = latex2exp::TeX("$n_2")),
                 rotate = 90, outerstart = 2) +
  coord_equal(clip = "off") +
  scale_fill_manual(values = col.vals) +
  theme_void(base_size=14) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(-1, 9), expand = c(0, 0)) -> cousin


# Path plots
library(patchwork)

p <- direct | hsib | avuncular | cousin |
  plot_annotation(tag_levels = "A")
p
ggsave("figures/peds_unilineal.emf")
ggsave("figures/peds_unilineal.png")
ggsave("figures/peds_unilineal.pdf", width = 12, height = 6)


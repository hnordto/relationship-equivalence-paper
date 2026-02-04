library(tidyverse)
library(ggbrace)
library(ggforce)
library(latex2exp)
library(pedtools)


# ---- SETUP ----

off <- 0.5
scale.factor <- 0.8
col.vals <- c("anchor" = "#F8CECC",
              "free" = "#DAE8FC",
              "target" = "#F8CECC",
              "founder" = "#F5F5F5")

# Avuncular

nodes <- data.frame(
  id = c("mother", "father", "desc111", "desc112",
         "desc121", "desc131", "desc141",
         "mother", "father", "desc211", "desc212",
         "desc221", "desc231", "desc241",
         "mother", "father", "desc311", "desc312",
         "desc321", "desc331", "desc341"),
  x = c(0, 2, 0, 2, 0, 0, 0,
        4, 6, 4, 6, 4, 4, 4,
        8, 10, 8, 10, 8, 8, 8),
  y = c(8, 8, 6, 6, 4, 2, 0,
        8, 8, 6, 6, 4, 2, 0,
        8, 8, 6, 6, 4, 2, 0),
  shape = c("square", "circle", "square", "diamond",
            "square", "circle", "diamond",
            "square", "circle", "square", "diamond",
            "circle", "square", "diamond",
            "square", "circle", "circle", "diamond",
            "square", "square", "diamond"),
  type = c("founder", "founder", "founder", "founder",
           "founder", "target", "founder",
           "founder", "founder", "founder", "founder",
           "target", "founder", "founder",
           "founder", "founder", "target", "founder",
           "founder", "founder", "founder")
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
  x = c(0, 2, 1, 0, 2, 0, 2,
        4, 6, 5, 4, 6, 4, 6,
        8, 10, 9, 8, 10, 8, 10),
  xend = c(1, 1, 1, 1, 1, 0, 2,
           5, 5, 5, 5, 5, 4, 6,
           9, 9, 9, 9, 9, 8, 10),
  y = c(8, 8, 8, 7, 7, 7, 7,
        8, 8, 8, 7, 7, 7, 7,
        8, 8, 8, 7, 7, 7, 7),
  yend = c(8, 8, 7, 7, 7, 6, 6,
           8, 8, 7, 7, 7, 6, 6,
           8, 8, 7, 7, 7, 6, 6)
)

edges <- data.frame(
  x = c(0, 0, 0,
        4, 4, 4,
        8, 8, 8),
  xend = c(0, 0, 0,
           4, 4, 4,
           8, 8, 8),
  y = c(6, 4, 2,
        6, 4, 2,
        6, 4, 2),
  yend = c(4, 2, 0,
           4, 2, 0,
           4, 2, 0),
  linetype = c("solid", "solid", "solid",
               "solid", "solid", "solid",
               "solid", "solid", "solid")
)

text <- data.frame(
  x = c(3, 7),
  y = c(4, 4),
  lab = c("=", "≠")
)

ggplot() +
  geom_segment(data = edges.top, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment(data = edges, aes(x = x, y = y-off*scale.factor, xend = xend, yend = yend+off*scale.factor),
               linetype = edges$linetype) +
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
  geom_point(data = data.frame(x = c(0, 2, 4, 6, 8, 10), y = c(0, 6, 0, 6, 0, 6)), aes(x, y)) +
  geom_text(data = text, mapping = aes(x = x, y = y, label = lab), size = 10) +
  coord_equal() +
  scale_fill_manual(values = col.vals) +
  theme_void(base_size=15) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(-1, 9), expand = c(0, 0)) -> avuncular

# Cousins

nodes <- data.frame(
  id = c("mother", "father", "desc111", "desc112",
         "desc121", "desc122", "desc131", "desc141",
         "mother", "father", "desc211", "desc212",
         "desc221", "desc222", "desc231", "desc232",
         "mother", "father", "desc311", "desc312",
         "desc321", "desc322", "desc331", "desc332"),
  x = c(0, 2, 0, 2, 0, 2, 0, 0,
        4, 6, 4, 6, 4, 6, 4, 6,
        8, 10, 8, 10, 8, 10, 8, 10),
  y = c(8, 8, 6, 6, 4, 4, 2, 0,
        8, 8, 6, 6, 4, 4, 2, 2,
        8, 8, 6, 6, 4, 4, 2, 2),
  shape = c("square", "circle", "square", "circle",
            "square", "diamond", "circle", "diamond",
            "square", "circle", "square", "circle",
            "circle", "square", "diamond", "diamond",
            "square", "circle", "circle", "circle",
            "square", "square", "diamond", "diamond"),
  type = c("founder", "founder", "founder", "founder",
           "founder", "founder", "target", "founder",
           "founder", "founder", "founder", "founder",
           "target", "founder", "founder", "founder",
           "founder", "founder", "target", "founder",
           "founder", "founder", "founder", "founder"))

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
  x = c(0, 2, 1, 0, 2, 0, 2,
        4, 6, 5, 4, 6, 4, 6,
        8, 10, 9, 8, 10, 8, 10),
  xend = c(1, 1, 1, 1, 1, 0, 2,
           5, 5, 5, 5, 5, 4, 6,
           9, 9, 9, 9, 9, 8, 10),
  y = c(8, 8, 8, 7, 7, 7, 7,
        8, 8, 8, 7, 7, 7, 7,
        8, 8, 8, 7, 7, 7, 7),
  yend = c(8, 8, 7, 7, 7, 6, 6,
           8, 8, 7, 7, 7, 6, 6,
           8, 8, 7, 7, 7, 6, 6)
)

edges <- data.frame(
  x = c(0, 2, 0, 0,
        4, 6, 4, 6,
        8, 10, 8, 10),
  xend = c(0, 2, 0, 0,
           4, 6, 4, 6,
           8, 10, 8, 10),
  y = c(6, 6, 4, 2,
        6, 6, 4, 4,
        6, 6, 4, 4),
  yend = c(4, 4, 2, 0,
           4, 4, 2, 2,
           4, 4, 2, 2),
  linetype = c("solid", "solid", "solid", "solid",
               "solid", "solid", "solid", "solid",
               "solid", "solid", "solid", "solid")
)

ggplot() +
  geom_segment(data = edges.top, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment(data = edges, aes(x = x, y = y-off*scale.factor, xend = xend, yend = yend+off*scale.factor),
               linetype = edges$linetype) +
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
  geom_point(data = data.frame(x = c(0, 2, 4, 6, 8, 10), y = c(0, 4, 2, 2, 2, 2)), aes(x, y)) +
  geom_text(data = text, mapping = aes(x = x, y = y, label = lab), size = 10) +
  coord_equal() +
  scale_fill_manual(values = col.vals) +
  theme_void(base_size=15) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(-1, 9), expand = c(0, 0)) -> cousin

# Simulations

library(ibdsim2)
library(paletteer)

extractFeatures <- function(segments, df = T) {

  features <- lapply(segments, function(x) {
    list(
      count = lengths(x),
      mean = vapply(x, mean, FUN.VALUE = 1),
      total = vapply(x, sum, FUN.VALUE = 1)
    )
  })

  if (isFALSE(df)) return(features)

  f <- do.call(rbind.data.frame, features)

  f <- f |>
    rownames_to_column(var = "rel")

  f$rel <- sapply(strsplit(f$rel, "\\."), `[[`, 1)

  f
}

# AVUNCULAR
avpeds <- list(avuncularPed(removal = 3) |> swapSex(8),
               avuncularPed(removal = 3) |> swapSex(6),
               avuncularPed(removal = 3) |> swapSex(4))
avannot <- list("A (ppm)", "A (pmp)", "A (mpp)")
avmeta <- ibdrel::pedsMetadata(avpeds)

avsim = ibdrel::ibdSimulations(avpeds, N = 5000)
avsegs = ibdrel::lengthIBD(avsim, avpeds, avannot)

avstats = extractFeatures(avsegs)


plotSegmentDistribution(avsim, type = "ibd1",
                        labels = c("Avuncular (ppm)",
                                   "Avuncular (pmp)",
                                   "Avuncular (mpp)"),
                        ylab = "Mean segment length (cM)") +
  coord_flip()

ggplot(avstats, aes(x = mean, y = count, colour = rel)) +
  geom_jitter(width = 0.35, alpha = .25, size = 0.5) +
  stat_ellipse(level = 0.99, linewidth = 1.2) +
  labs(x = "Mean segment length (cM)",
       y = "Number of segments",
       colour = "Pedigree") +
  scale_colour_paletteer_d("MoMAColors::Klein") +
  theme_bw(base_size=15) +
  theme(legend.position = "inside",
        legend.position.inside = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.title = element_text(size = rel(.75)),
        legend.text = element_text(size = rel(.75))) -> avsim

# COUSIN

couspeds <- list(cousinPed(1, removal = 2) |> swapSex(c(3, 10)),
                 cousinPed(2) |> swapSex(c(5, 7)),
                 cousinPed(2) |> swapSex(c(3,5)))
cousannot <- list("1C2R (mppm)",
                  "2C (mpmp)",
                  "2C (pmmp)")
cousmeta <- ibdrel::pedsMetadata(couspeds)

coussim <- ibdrel::ibdSimulations(couspeds, N = 5000)
coussegs = ibdrel::lengthIBD(coussim, couspeds, cousannot)

cousstats = extractFeatures(coussegs)

ggplot(cousstats, aes(x = mean, y = count, colour = rel)) +
  geom_jitter(width = 0.35, alpha = .25, size = 0.5) +
  stat_ellipse(level = 0.99, linewidth = 1.2) +
  labs(x = "Mean segment length (cM)",
       y = "Number of segments",
       colour = "Pedigree") +
  scale_colour_paletteer_d("MoMAColors::Klein") +
  theme_bw(base_size=15) +
  theme(legend.position = "inside",
        legend.position.inside = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.title = element_text(size = rel(.75)),
        legend.text = element_text(size = rel(.75))) -> csim

# Very similar. Need to measure actual difference.

f <- extractFeatures(coussegs, F)

ks.test(f[[1]]$count, f[[2]]$count)
ks.test(f[[1]]$count, f[[3]]$count)
ks.test(f[[2]]$count, f[[3]]$count)
ks.test(f[[1]]$mean, f[[2]]$mean)
ks.test(f[[1]]$mean, f[[3]]$mean)
ks.test(f[[2]]$mean, f[[3]]$mean)

# PATHCWORK

library(patchwork)

p <- (avuncular | avsim) / (cousin | csim) &
  plot_annotation(tag_levels = "A")
p

# Save

ggsave("figures/example_sims.pdf", dpi = 600, plot = p, width = 9, height = 8,
       device = cairo_pdf)

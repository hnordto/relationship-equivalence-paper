library(ibdsim2)
library(pedtools)
library(ibdrel)

peds <- list(cousinPed(2), cousinPed(2) |> swpSx(7))

annotation = sapply(peds, annotatePedigree)
names(peds) <- annotation


metadata = pedsMetadata(peds)


# SIMULATE TRAINING DATA

sims_training <- ibdSimulations(peds, N = 1000, seed = NULL)
segments <- lengthIBD(sims_training, peds, annotation)

test <- lapply(segments, prepareFeatures, cutoff = 0)

x <- test[[1]]$count
y <- test[[2]]$count

ks.test(x,y)

grid <- sort(unique(c(x, y)))

Fx = ecdf(x)
Fy = ecdf(y)

plot(grid, Fx(grid), type = "l")
lines(grid, Fy(grid), type = "l")


?ecdf


# 3rd cousins

l = identify_path_lengths(0,11)
rels = listRelationships(l)
rels <- removeSymmetries(rels)

tc.p.pppp <- cousinPed(3)
tc.p.mppp <- cousinPed(3) |> swapSex(11)
tc.p.mmpp <- cousinPed(3) |> swapSex(c(7,11))
tc.p.mmmp <- cousinPed(3) |> swapSex(c(7,11,9))
tc.p.mmmm <- cousinPed(3) |> swapSex(c(7,11,9,13))

peds <- list(tc.p.pppp, tc.p.mppp, tc.p.mmpp, tc.p.mmmp, tc.p.mmmm)
annotation = sapply(peds, annotatePedigree)
names(peds) <- annotation


metadata = pedsMetadata(peds)

sims_training <- ibdSimulations(peds, N = 1000, seed = NULL)
segments <- lengthIBD(sims_training, peds, annotation)

unlist(segments[[1]]) |> mean() -> v1
unlist(segments[[2]]) |> mean() -> v2
unlist(segments[[3]]) |> mean() -> v3
unlist(segments[[4]]) |> mean() -> v4
unlist(segments[[5]]) |> mean() -> v5

x <- seq(1, 5)
y <- c(v1, v2, v3, v4, v5)
df <- data.frame(x, y)
ggplot() +
  geom_col(data = df, mapping = aes(x,y))




# Compare with sex-averaged?

tc.p.pppp <- cousinPed(3)
tc.p.mppp <- cousinPed(3) |> swapSex(11)
tc.p.mmpp <- cousinPed(3) |> swapSex(c(7,11))
tc.p.mmmp <- cousinPed(3) |> swapSex(c(7,11,9))
tc.p.mmmm <- cousinPed(3) |> swapSex(c(7,11,9,13))
peds <- list(tc.p.pppp, tc.p.mppp, tc.p.mmpp, tc.p.mmmp, tc.p.mmmm)
annotation = sapply(peds, annotatePedigree)
metadata = pedsMetadata(peds)

# Baseline, sex averaged

MAP = loadMap(sexAverage = T)

sim = ibdsim2::ibdsim(tc.p.pppp, map = MAP, N = 1000)
segments <- lengthIBD(list(sim), list(tc.p.pppp), "baseline")
unlist(segments[[1]]) |> mean()


# Test range calculations

ped = cousinPed(1, half = T)
variants = pedVariants(ped)
peds = constructPedigrees(variants)

annotation = sapply(peds, ibdrel:::annotatePedigree)
sims_training <- ibdrel:::ibdSimulations(peds, N = 1000, seed = NULL)
segments <- ibdrel:::lengthIBD(sims_training, peds, annotation)

lapply(segments, function(x) {
  unlist(x) |> mean()
}) -> means.1
range(means.1)

ped = cousinPed(4, half = T)
variants = pedVariants(ped)
peds = constructPedigrees(variants)

annotation = sapply(peds, ibdrel:::annotatePedigree)
sims_training <- ibdrel:::ibdSimulations(peds, N = 1000, seed = NULL)
segments <- ibdrel:::lengthIBD(sims_training, peds, annotation)

lapply(segments, function(x) {
  unlist(x) |> mean()
}) -> means.2
range(means.2)

df <- data.frame(rel = c("h1c", "h4c"),
                 xstart = c(range(means.1)[1], range(means.2)[1]),
                 xend = c(range(means.1)[2], range(means.2)[2]))

library(ggplot2)

ggplot(df) +
  geom_linerange(aes(y = rel, xmin = xstart, xmax = xend),
                 size = 1.5, alpha = .25) +
  geom_point(aes(x = xstart, y = rel)) +
  geom_point(aes(x = xend, y = rel)) +
  theme_bw()

# Automate this
# Should perhaps remove empty dsimulations??
segmentranges <- function(pedlist) {

  df = data.frame(rel = character(),
                  xstart = numeric(),
                  xend = numeric())

  for (i in 1:length(pedlist)) {
    ped = pedlist[[i]]
    verb = verbalisr::verbalise(ped)[[1]]
    rel = verb$code
    deg = verb$degree
    type = verb$type
    variants = ibdrel::pedVariants(ped)
    peds = ibdrel::constructPedigrees(variants)

    annotation = sapply(peds, ibdrel:::annotatePedigree)
    sims_training <- ibdrel:::ibdSimulations(peds, N = 100, seed = NULL)
    segments <- ibdrel:::lengthIBD(sims_training, peds, annotation)

    lapply(segments, function(x) {
      unlist(x) |> mean()
    }) -> means

    df <- rbind(df, data.frame(rel = rel,
                               type = type,
                               deg = deg,
                               xstart = range(means)[1],
                               xend = range(means)[2]))
  }

  return (df)
}


lpeds <- list(linearPed(1), linearPed(2), linearPed(3), linearPed(4))
couspeds <- list(cousinPed(1), cousinPed(2), cousinPed(3), cousinPed(4), cousinPed(5))
hcouspeds <- list(cousinPed(1, half = T), cousinPed(2, half = T), cousinPed(3, half = T), cousinPed(4, half = T))
avpeds <- list(avuncularPed(removal = 1), avuncularPed(removal = 2), avuncularPed(removal = 3), avuncularPed(removal = 4))
havpeds <- list(avuncularPed(removal = 1, half = T), avuncularPed(removal = 2, half = T), avuncularPed(removal = 3, half = T), avuncularPed(removal = 4, half = T))

cousranges <- segmentranges(couspeds)
hcousranges <- segmentranges(hcouspeds)
avpedranges <- segmentranges(avpeds)
havpedranges <- segmentranges(havpeds)

pedranges <- rbind(cousranges, hcousranges, avpedranges, havpedranges)


ggplot(pedranges) +
  geom_linerange(aes(y = rel, xmin = xstart, xmax = xend,  colour = factor(deg)),
                 size = 1.5, alpha = .5) +
  geom_point(aes(x = xstart, y = rel, colour = factor(deg))) +
  geom_point(aes(x = xend, y = rel, colour = factor(deg))) +
  facet_grid(rows = vars(type))
  theme_bw()

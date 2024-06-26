library(tidyverse)
library(eurostat)
library(leaflet)
library(sf)
library(scales)
library(cowplot)
library(ggthemes)

folder.path <- "C:\\Users\\erezm\\coding\\stat215b\\final_project\\"
source(paste0(folder.path, "stat215b\\party.stance.R"))

# load data
load(paste0(folder.path, "data.Robj"))
data %<>% enrich.party.stance
data %<>% filter.party.stance

# placement within party
out1 <- data %>% analyze.placement.within.party(fdr.q=0.05, counterfactual.alpha=0.05)
out1
xx <- get_eurostat_geospatial(resolution=10, nuts_level=0, year=2016)[,c("geo", "geometry")]
key <- c(AT="Aus", BE="Bel", BG="Bul", HR="Cro", CZ="Cze", DK="Den", EE="Est",
         FI="Fin", FR="Fra", DE="Ger", EL="Gre", HU="Hun", IE="Ire", IT="Ita",
         LV="Lat", LT="Lit", NL="Net", PL="Pol", PT="Por", RO="Rom", 
         SK="Slovakia", SI="Slovenia", ES="Spa", SE="Swe", UK="UK")
xx$value <- out1$mean[match(key[xx$geo], out1$country)]
xx %>% ggplot(aes(fill=round(4 * value) / 4)) + 
  geom_sf() + 
  ggtitle("Placement Within Party") +
  theme(plot.title = element_text(size = 15, hjust=0.5)) +
  scale_fill_distiller(palette = 12, direction=1, name="") +
  scale_x_continuous(limits = c(-10, 35)) + 
  scale_y_continuous(limits = c(35, 65))

# opposite side perception
out2 <- data %>% analyze.opposide.side.perception(fdr.q=0.05, bt.num.iterations=2000)
out2
xx$value <- out2$mean[match(key[xx$geo], out2$country)]
xx %>% ggplot(aes(fill=round(4 * value) / 4)) + 
  geom_sf() + 
  ggtitle("Opposite Side Perception") +
  theme(plot.title = element_text(size = 15, hjust=0.5)) +
  scale_fill_gradient2(name="",
                       low = muted("green"),
                       mid = "white",
                       high = muted("red"),
                       midpoint = 0,) +
  scale_x_continuous(limits = c(-10, 35)) + 
  scale_y_continuous(limits = c(35, 65))


# party alignment
out3 <- data %>% predict.party.alignment(train.frac=0.5, nfolds=5, fdr.q=0.05)
mm <- as.data.frame(t(sapply(out3, function(o) o$mean)))
mm$r2 <- 1 - (1 - mm$lda.stein.ma) / (1 - mm$baseline)
xx$value <- mm$r2[match(key[xx$geo], names(out3))]
xx %>% ggplot(aes(fill=value)) + #round(16 * value) / 16)) + 
  geom_sf() + 
  ggtitle("Party Alignment Prediction") +
  theme(plot.title = element_text(size = 15, hjust=0.5)) +
  scale_fill_distiller(palette = 1, direction=1, name="") +
  scale_x_continuous(limits = c(-10, 35)) + 
  scale_y_continuous(limits = c(35, 65))
out3

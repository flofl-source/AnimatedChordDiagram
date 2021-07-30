#Code inspiré du site :
#https://guyabel.com/post/animated-directional-chord-diagrams/
#Les noms des variables n'ont pas été changés donc : 
#origien_reg = envoyeur d'email
#dest_reg = destinataire d'email
#year0 = heure de la journée entre 0 et 23
#install.packages("tidyverse",dependencies=TRUE)
library(tidyverse)

d0 <- read_csv("C:/Users/Ophélie/Documents/FLORA/Animated chord diagram/dataChordDiagram.csv")
d0

library(tweenr)

# On génère plus de lignes que ce qu'on a, pour faire un dégradé entre les états
d2 <- d0 %>%
  mutate(corridor = paste(origien_reg, dest_reg, sep = " -> ")) %>%
  select(corridor, year0, flow) %>%
  mutate(ease = "linear") %>%
  tween_elements(time = "year0", group = "corridor", ease = "ease", nframes = 200) %>%
  tibble::as_tibble()
d2
#Si on veut réduire la valeur du flow
d2 <- d2 %>%
  separate(col = .group, into = c("orig_reg", "dest_reg"), sep = " -> ") %>%
  select(orig_reg, dest_reg, flow, everything()) %>%
  mutate(flow = flow) #réduire ici avec flow/5 ou quoi
d2

library(magrittr)

#Calcul pour chaque mgmt_level le minimum de l'axe lorsque la valeur est 0 
reg_max <- d0 %>%
  group_by(year0, origien_reg) %>%
  mutate(tot_out = sum(flow)) %>%
  group_by(year0, dest_reg) %>%
  mutate(tot_in = sum(flow)) %>%
  filter(origien_reg == dest_reg) %>%
  mutate(tot = tot_in + tot_out) %>%
  mutate(reg = origien_reg) %>%
  group_by(reg) %>%
  summarise(tot_max = max(tot)) %$%
  'names<-'(tot_max, reg)
reg_max


# create a directory to store the individual plots
dir.create("./plot-gifTEST10/")

#install.packages("circlize")
library(circlize)
library(circlize)

# Espace entre les axes
scale_gap <- function(flow_m, flow_max, gap_at_max = 1, gaps = NULL) {
  p <- flow_m / flow_max
  if(length(gap_at_max) == 1 & !is.null(gaps)) {
    gap_at_max <- rep(gap_at_max, gaps)
  }
  gap_degree <- (360 - sum(gap_at_max)) * (1 - p)
  gap_m <- (gap_degree + sum(gap_at_max))/gaps
  return(gap_m)
}
d3 <- d2 %>%
  group_by(.frame) %>%
  summarise(flow = sum(flow)) %>%
  mutate(gaps = scale_gap(flow_m = flow, flow_max = max(.$flow), 
                          gap_at_max = 4, gaps = 3)) #" espaces

d3
print(d3, max=201)

for (f in unique(d2$.frame)){
  # open a PNG plotting device
  png(file = paste0("./plot-gifTEST10/globalchord", f, ".png"), height = 6, width = 5, 
      units = "in", res = 800)
  
  # initialise the circos plot
  circos.clear()
  par(mar = rep(0, 4), cex=1)
  circos.par(start.degree = 90, track.margin = c(-0.1, 0.1),
             gap.degree = filter(d3, .frame == f)$gaps, 
             points.overflow.warning = FALSE)
  
  # plot the chord diagram 
  chordDiagram(x = filter(d2, .frame == f)[,1:3, drop=FALSE], directional = 1, order = c('DIRIGEANT','MANAGER','COLLABORATEUR'),
               grid.col = c("#F84242","#EE954C","#F8EF42"), annotationTrack = c("grid","name","axis"),
               transparency = 0.25,  annotationTrackHeight = c(0.03, 0.1),
               direction.type = c("diffHeight", "arrows"), link.arr.type = "big.arrow",
               diffHeight  = -0.04, link.sort = TRUE, link.largest.ontop = TRUE, xmax=reg_max)
  
  text(-1,0.8,paste (trunc (as.numeric (filter (d2, .frame==f) [1,4] ) ),'H') )
  # close plotting device
  dev.off()
  
}

# et on va sur le site pour générer le gif




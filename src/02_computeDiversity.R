# https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/
# Load environment variables and libraries
source("C:/Users/tnauss/permanent/plygrnd/mekbib_vegtrans/BALE-VegTrans/src/00_set_environment.R")


#### Read vegetation matrix
vegdat = readRDS(paste0(path_rdata, "/vegdat.rds"))
vegdat_mtrx = readRDS(paste0(path_rdata, "/vegdat_mtrx.rds"))

vegdat_mtrx_pres_abs = vegdat_mtrx
vegdat_mtrx_pres_abs[vegdat_mtrx_pres_abs > 0] = 1
vegdat_mtrx_pres_abs[vegdat_mtrx_pres_abs == 0] = 0
rownames(vegdat_mtrx_pres_abs) = rownames(vegdat_mtrx)

for(c in seq(ncol(vegdat_mtrx_pres_abs))){
  test = any(is.na(vegdat_mtrx_pres_abs[, c]))
  if(test){
    vegdat_mtrx_pres_abs[, c][is.na(vegdat_mtrx_pres_abs[, c])] = 0
  }
}

  
#### Compute diversity meassures
# Compute diversity and merge them with matrix data and meta information
shannon = diversity(vegdat_mtrx, index = "shannon", MARGIN = 1, base = exp(1))
simpson = diversity(vegdat_mtrx, index = "simpson", MARGIN = 1, base = exp(1))
specnbr = specnumber(vegdat_mtrx)
evns = simpson/log(apply(vegdat_mtrx>0,1,sum))
div_df = data.frame(PlotID = rownames(vegdat_mtrx),
                    Shannon = shannon,
                    Simpson = simpson,
                    SpecNbr = specnbr,
                    Evenness = evns)

vegdat_div = merge(vegdat[, c(1,4:8)], div_df, by = "PlotID")
vegdat_div$Distance = as.factor(vegdat_div$Distance)


#### Compute turnover/nestnedness
# Compute pair-wise dissimilarities for spatial turnover, nestedness and 
# total dissimilarity. Replace NAs in dissimilarity matrixes by column mean 
# prior to the nonmetric multidimensional scaling
nmds = metaMDS(vegdat_mtrx_pres_abs)

bp = beta.pair(vegdat_mtrx_pres_abs[, -1], index.family = "sorensen")
bp_nmds = lapply(bp, function(d){metaMDS(d)})

# bp_narm = lapply(bp, function(d){
#   d = as.matrix(d)
#   for(i in seq(ncol(d))){
#     d[is.na(d[,i]), i] <- mean(d[,i], na.rm = TRUE)
#   }
#   rownames(d) = vegdat_mtrx$PlotID
#   colnames(d) = vegdat_mtrx$PlotID
#   return(d)
# })
# names(bp_narm) = names(bp)
# bp_nmds = lapply(bp_narm, function(d){metaMDS(d)})
names(bp_nmds) = paste0(names(bp), ".cms")




#### Some playing...
ggplot(data = vegdat_div, aes(x = Distance, y = Shannon)) + 
  geom_boxplot(notch = TRUE) + 
  theme_bw()

ggplot(data = vegdat_div, aes(x = Distance, y = Evenness)) + 
  geom_boxplot(notch = TRUE) + 
  theme_bw()

stressplot(nmds)
ordiplot(nmds, type="n")
orditorp(nmds,display="species",col="red",air=0.01)
orditorp(nmds,display="sites",cex=1.25,air=0.01)


stressplot(bp_nmds$beta.sim.cms)
ordiplot(bp_nmds$beta.sim.cms, type="n")
orditorp(bp_nmds$beta.sim.cms, display="sites",cex=1.25,air=0.01)

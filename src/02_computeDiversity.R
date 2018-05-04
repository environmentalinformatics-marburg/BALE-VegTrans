# https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/
# Load environment variables and libraries
source("C:/Users/tnauss/permanent/plygrnd/mekbib_vegtrans/BALE-VegTrans/src/00_set_environment.R")


#### Read vegetation matrix
vegdat = readRDS(paste0(path_rdata, "/vegdat.rds"))
vegdat_mtrx = readRDS(paste0(path_rdata, "/vegdat_mtrx.rds"))
vegdat_mtrx_pres_abs = readRDS(paste0(path_rdata, "/vegdat_mtrx_pres_abs.rds"))

  
#### Compute diversity meassures
# Compute diversity per plot and merge them with matrix data and meta 
# information
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
vegdat_div = vegdat_div[!duplicated(vegdat_div),]
vegdat_div$Distance = as.factor(vegdat_div$Distance)
vegdat_div_long = melt(vegdat_div[, c(1:2, 7:10)], id.var = c("PlotID", "Distance"))
colnames(vegdat_div_long)[3:4] = c("Variable", "Value")


# Compute diversity per distance
vegdat_mtrx_distsum = vegdat_mtrx
vegdat_mtrx_distsum$agg_id = substr(rownames(vegdat_mtrx), 3, 6)
vegdat_mtrx_distsum = aggregate(.~agg_id, vegdat_mtrx_distsum, FUN = sum, na.action = na.omit)
rownames(vegdat_mtrx_distsum) = vegdat_mtrx_distsum$agg_id
vegdat_mtrx_distsum$agg_id = NULL

shannon = diversity(vegdat_mtrx_distsum, index = "shannon", MARGIN = 1, base = exp(1))
simpson = diversity(vegdat_mtrx_distsum, index = "simpson", MARGIN = 1, base = exp(1))
specnbr = specnumber(vegdat_mtrx_distsum)
evns = simpson/log(apply(vegdat_mtrx_distsum>0,1,sum))
vegdat_div_distsum = data.frame(Distance = rownames(vegdat_mtrx_distsum),
                                Shannon = shannon,
                                Simpson = simpson,
                                SpecNbr = specnbr,
                                Evenness = evns)


#### Compute turnover/nestnedness
# Compute pair-wise dissimilarities for spatial turnover, nestedness and 
# total dissimilarity. Replace NAs in dissimilarity matrixes by column mean 
# prior to the nonmetric multidimensional scaling
nmds = metaMDS(vegdat_mtrx_pres_abs)

bp = beta.pair(vegdat_mtrx_pres_abs, index.family = "sorensen")
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


saveRDS(vegdat_div, file = paste0(path_rdata, "/vegdat_div.rds"))
saveRDS(vegdat_div_long, file = paste0(path_rdata, "/vegdat_div_long.rds"))
saveRDS(vegdat_div_distsum, file = paste0(path_rdata, "/vegdat_div_distsum.rds"))
saveRDS(nmds, file = paste0(path_rdata, "/nmds.rds"))
saveRDS(bp, file = paste0(path_rdata, "/bp.rds"))
saveRDS(bp_nmds, file = paste0(path_rdata, "/bp_nmds.rds"))

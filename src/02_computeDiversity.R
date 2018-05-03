# Load environment variables and libraries
source("C:/Users/tnauss/permanent/plygrnd/mekbib_vegtrans/BALE-VegTrans/src/00_set_environment.R")


# Read vegetation matrix
vegdat = readRDS(paste0(path_rdata, "/vegdat.rds"))
vegdat_mtrx = readRDS(paste0(path_rdata, "/vegdat_mtrx.rds"))

tmp = vegdat_mtrx[, -1]
tmp[tmp > 0] = 1
tmp[tmp == 0] = 0
vegdat_mtrx_pres_abs = data.frame(PlotID = vegdat_mtrx$PlotID,
                                  tmp)

for(c in seq(ncol(vegdat_mtrx_pres_abs))){
  test = any(is.na(vegdat_mtrx_pres_abs[, c]))
  if(test){
    vegdat_mtrx_pres_abs[, c][is.na(vegdat_mtrx_pres_abs[, c])] = 0
  }
}

  
# Compute diversity meassures
shannon = diversity(vegdat_mtrx[, -1], index = "shannon", MARGIN = 1, base = exp(1))
simpson = diversity(vegdat_mtrx[, -1], index = "simpson", MARGIN = 1, base = exp(1))
specnbr = specnumber(vegdat_mtrx)
evns = simpson/log(apply(vegdat_mtrx[,-1]>0,1,sum))
bp = beta.pair(vegdat_mtrx_pres_abs[, 2:80], index.family = "sorensen")
cmdscale(bp$beta.sim) 

div_df = data.frame(PlotID = vegdat_mtrx$PlotID,
                    Shannon = shannon,
                    Simpson = simpson,
                    SpecNbr = specnbr,
                    Evenness = evns)

# Merge diversity meassures with matrix data and meta information
vegdat_div = merge(vegdat[, c(1,4:8)], div_df, by = "PlotID")
vegdat_div$Distance = as.factor(vegdat_div$Distance)

ggplot(data = vegdat_div, aes(x = Distance, y = Shannon)) + 
  geom_boxplot(notch = TRUE) + 
  theme_bw()

ggplot(data = vegdat_div, aes(x = Distance, y = Evenness)) + 
  geom_boxplot(notch = TRUE) + 
  theme_bw()

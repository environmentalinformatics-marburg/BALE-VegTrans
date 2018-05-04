# Load environment variables and libraries
source("C:/Users/tnauss/permanent/plygrnd/mekbib_vegtrans/BALE-VegTrans/src/00_set_environment.R")


# Read vegetation dataset from Excel file and combine all subtables into a 
# single data frame
filepath_veg = paste0(path_data, "Mekbib_Settlement_Plot_Data_for_Analysis.xlsx")

vegdat = lapply(seq(6), function(i){
  read_excel(filepath_veg, sheet = i)  
})
vegdat = do.call("rbind", vegdat)


#### Clean the data frame and prepare for further analysis
# Split information on settlement name and plot ID and rearrange the columns
split_stlm = strsplit(vegdat$`Settlement name and  code`, "_(?=[^_]+$)", perl=TRUE)
vegdat$`Settlement name and  code` = unlist(lapply(split_stlm, function(p){p[2]}))
vegdat$Settlement = sub("_", " ", unlist(lapply(split_stlm, function(p){p[1]})))
colnames(vegdat)[c(2, 7)] = c("PlotID", "Alt")
vegdat = vegdat[, c(2, 3, 4, 1, 5, 6, 7, 8)]

# Compile 2 letter, 4 digit plot IDs
unique(vegdat$PlotID)
vegdat$PlotID[grepl("GGY", vegdat$PlotID)] = paste0("GY", substr(vegdat$PlotID[grepl("GGY", vegdat$PlotID)], 4, nchar(vegdat$PlotID[grepl("GGY", vegdat$PlotID)])))

vegdat$PlotID[which(nchar(vegdat$PlotID) == 3)] = paste0(vegdat$PlotID[which(nchar(vegdat$PlotID) == 3)], "000")
vegdat$PlotID[which(nchar(vegdat$PlotID) == 5)] = paste0(substr(vegdat$PlotID[which(nchar(vegdat$PlotID) == 5)], 1, 2),
                                                         "0",
                                                         substr(vegdat$PlotID[which(nchar(vegdat$PlotID) == 5)], 3, 5))
# unique(nchar(vegdat$PlotID))
# head(vegdat)

# Convert species names to lower cases
vegdat$Species = tolower(vegdat$Species)

# Convert parts of data frame to vegetation record matrix
vegdat_mtrx = dcast(vegdat, PlotID ~ Species, value.var = "Cover", fun.aggregate = sum)
rownames(vegdat_mtrx) = vegdat_mtrx$PlotID
vegdat_mtrx = vegdat_mtrx[, -1]

# Compute presence/absence matrix
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

# Save data for further usage
saveRDS(vegdat, file = paste0(path_rdata, "/vegdat.rds"))
saveRDS(vegdat_mtrx, file = paste0(path_rdata, "/vegdat_mtrx.rds"))
saveRDS(vegdat_mtrx_pres_abs, file = paste0(path_rdata, "/vegdat_mtrx_pres_abs.rds"))

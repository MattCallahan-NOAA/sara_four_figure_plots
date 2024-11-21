# make plots of catch totalbiomass, spawningbiomass, and recruitment
library(tidyverse)
library(httr)
library(gridExtra)
library(here)

# turn off scientific notation
options(scipen=999)

# download data
hq<-httr::content(
  httr::GET('https://apex.psmfc.org/akfin/data_marts/akmp/hq_time_series'),
  type = "application/json") %>%
  bind_rows() %>%
  rename_with(tolower) %>%
  # combine stock and region into a single identifier
  mutate(stockregion=paste0(stock,"_",region))

#### IMPORTANT! filter to current year ####
current_year<-2023

hq<-hq %>%
  dplyr::filter(asmt_year==current_year)


# function to generate 4 panel plot
generate_plots <- function(stockregion) {

  # filter to stock
  hq<-hq %>%
    dplyr::filter(stockregion==!!stockregion)

# Assuming the hq data is in tons
mtc <- mean(hq$totalcatch/1000, na.rm=T)
# Assuming the hq data is in tons
mb <- mean(hq$totalbiomass/1000, na.rm=T)
# Assuming bmsy is the same each year
bmsy <- hq$b_msy[1]/1000
# Assuming recruitment is in number of fish
mr <- mean(hq$recruitment/1000000, na.rm=T)

# define theme
mytheme <-theme_bw()+
  theme(legend.position="none")

# plot catch
p1 <- ggplot()+
  geom_col(data=hq, aes(x=fisheryyear,y=totalcatch/1000), color="white", fill="darkblue", width=1)+
  geom_hline(yintercept=mtc, color="darkgreen",lwd=2, lty=2)+
  xlab("Year")+ylab("Total Catch (kilitons)")+
  ggtitle("Total Catch")+
  expand_limits(y=0)+
  mytheme

# plot total biomass
p2<-ggplot()+
  geom_line(data=hq, aes(x=fisheryyear,y=totalbiomass/1000), color="darkblue", lwd=2)+
  geom_hline(yintercept=mb, color="darkgreen",lwd=2, lty=2)+
  xlab("Year")+ylab("Total Biomass (kilitons)")+
  ggtitle("Total Biomass")+
  expand_limits(y=0)+
  mytheme

# plot recruitment
p3<-ggplot()+
  geom_col(data=hq, aes(x=fisheryyear-2,y=recruitment/1000000,
                        fill=ifelse(recruitment/1000000>mr,"darkblue", "darkred" )), color="white",  width=1)+
  scale_fill_manual(values=c("darkblue", "darkred" ))+
  geom_hline(yintercept=mr, color="darkgreen", lwd=2)+
  xlab("Year")+ylab("Recruitment (millions)")+
  ggtitle("Age 2 Recruitment")+
  expand_limits(y=0)+
  mytheme

# plot spawning biomass
p4<-ggplot()+
  geom_line(data=hq, aes(x=fisheryyear,y=spawnbiomass/1000), color="darkblue", lwd=2)+
  geom_hline(yintercept=bmsy, color="darkred",lwd=2, lty=2)+
  xlab("Year")+ylab("Spawning Biomass (kilitons)")+
  ggtitle("Spawning Biomass")+
  expand_limits(y=0)+
  mytheme

# combine plots
p5 <- grid.arrange(p1,p2,p3,p4, ncol=2)
# display
p5

}

#test
generate_plots(stockregion="SABLE_AK")

# define stock/regions
stockregions<-unique(hq$stockregion)

# loop over stock region combos and save
for(ii in 1:length(stockregions)) {
  filepath <- here("figures")
  png(filename=paste0(filepath,"/",stockregions[ii],"_",current_year,".png"), width=1000, height=800)
  generate_plots(stockregion=stockregions[ii])
  dev.off()
}

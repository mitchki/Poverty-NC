setwd("~/github/Poverty-in-NC")

full <- read.csv("ACS_10_5YR_DP05_with_ann.csv",stringsAsFactors=FALSE,skip=1)
saveRDS(full,"full.rds")

use <- read.csv("use.csv",stringsAsFactors=FALSE,skip=0)
psaveRDS(use,"use.rds")

panel.cor <- function(x, y, digits=2, cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y)
  Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))  
  text(0.5, 0.25, paste("r=",txt))
  text(.5, .75, Signif)
}

pairs(use[,5:10],upper.panel=panel.cor,diag.panel=panel.hist)

pairs(use[,13:25],upper.panel=panel.cor,diag.panel=panel.hist)

library(acs)
## My own private census api key
api.key.install("26631a294aa8a3c9dd7ff362f294feee0660d93e")

install.packages("zipcode")

install.packages("choroplethrMaps")
library(choroplethrMaps)

nj <- geo.make(state="NJ", county="*")
nc <- geo.make(state="NC", county="*")

s1 <- acs.fetch(geography=nc, table.number="B05001", col.names="pretty")
str(s1)

poverty <- acs.lookup(keyword=c("below poverty level","poverty"))
str(poverty)
poverty

p1 <- acs.fetch(geography=nc, table.number="B06012", col.names="pretty")
str(p1)
estimate(p1)

head(results(poverty)
     
     p2 <- acs.fetch(geography=nc, table.number="B17004", col.names="pretty")
     str(p2)
     head(estimate(p2))
     write.csv(estimate(p2),"p2.csv")
     
write.csv(results(poverty),"povertyacsDatasets.csv")     
     
getwd()

p2.2011 <- estimate(p2)      

str(p2.2011)
pov2011 <- data.frame(p2.2011[,1:3])



use[,1]
(pov_perc)
p3<- cbind(pov_perc,use[,1:2],pov2011)

## percent of families in poverty ACS years 2006-2010 by county
pfamily <- acs.fetch(geography=nc, table.number="B17010", col.names="pretty",endyear=2010)
str(pfamily)
head(estimate(pfamily))
write.csv(estimate(pfamily),"pfamily.csv")
save.image()

pov_perc <- estimate(pfamily[,2])/estimate(pfamily[,1])
str(pov_perc)
counties <- row.names(estimate(pfamily))
df[,2] <- as.numeric(df[,2])
colnames(df)<- c("region","value")
str(df)
head(df)
data(county.map)

library(ggplot2)
ggplot(county.map, aes(long, lat, group=group)) + geom_polygon()

library(choroplethrMaps)
data(county.regions)

ncCounties <- county.regions[county.regions$state.abb=="NC",]
head(ncCounties)
str(ncCounties)
df <- data.frame(cbind(ncCounties$region,pov_perc),stringsAsFactors = FALSE)
colnames(df)<- c("region","value")
str(df)
head(df)

county_choropleth(df,zoom="north carolina")

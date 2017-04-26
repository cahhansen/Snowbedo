library(Snowbedo)
library(imputeTS)
library(ggplot2)
library(gtable)
library(grid)

#Read in data with all necessary parameters (and if needed, combine into a single data.frame)
data=read.csv('BigCottonwood.csv')

#Limit dataset to dates with common data
data=limitperiod(data=data,begin="2000-09-30",end="2011-10-01")


#Formatting---------------------------------------------------------------------------------------------------------
#Convert the shortwave radiation from W/m2/day to Wh/m2/day
data$solar_short_Whm2day=data$solar_short*24

#Convert snowcover to a percentage (0-1)
data$snowcover=data$snowcover/100
#Interpolate snowcover data for "cloud free" conditions
data$cloudfreesnowcover=data$snowcover
data[(data$cloudcover>25),"cloudfreesnowcover"]=NA
data$cloudfreesnowcover=na.interpolation(data$cloudfreesnowcover,option="linear")

#Calculate the daily precipitation in cm/day
data$precip_daily=dissipate(data=data$precip_accum)
data$precip_daily=data$precip_daily*0.1

#Convert flow to cms if needed
data$flow=data$flow*0.0283

#May need to use na.interpolation from imputeTS package if daily values are missing
data$tmax=na.interpolation(data$tmax,option='linear')
data$tmin=na.interpolation(data$tmin,option='linear')
data$tobs=na.interpolation(data$tobs,option='linear')
data$tavg=na.interpolation(data$tavg,option='linear')

lagpad <- function(x, k) {
  c(rep(0, k), x)[1 : length(x)]
}

#Lag flow
data$lagflow=lagpad(data$flow,1)
#------------------------------------------------------------------------------------------------------------------
grid.newpage()

# two plots
p1 <- ggplot(data, aes(date, albedo)) + geom_line(colour = "blue") + theme_bw()
p2 <- ggplot(data, aes(date, flow)) + geom_line(colour = "red") + theme_bw() %+replace%
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)

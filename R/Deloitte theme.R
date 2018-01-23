
require(ggplot2)
require(ggthemes)
require(plyr)
library(plotly)
require(reshape2)
library(grid)
require(lubridate)
require(stringr)
options(stringsAsFactors = FALSE)

###################
## theme

#http://www.color-hex.com/color-palette/22646
pal_deloitte<-c("#86BC25", "#C4D600",  "#43B02A",	"#046A38", 	"#2C5234","#0097A9","#62B5E5","#00A3E0","#0076A8","#012169")
# http://www.color-hex.com/color-palette/12376
pal_deloitte2<-c("#DDEFE8",	"#9DD4CF", 	"#6FC2B4","#00ABAB","#0097A9","#007680","#004F59")


############# color pieces!
scale_fill_deloitte <- function(){
  structure(list(
    scale_fill_gradientn(colours = rev(pal_deloitte2))
  ))
}

scale_color_discrete_deloitte <- function(){
  structure(list(
    scale_color_manual(values=pal_deloitte)
  ))
}

scale_color_continuous_deloitte <- function(){
  # structure(list(
    scale_color_gradientn(colours = pal_deloitte2)
  # ))
}
##################################

theme_deloitte <- function(base_size=11, base_family=""){
  theme_classic(base_size = base_size, base_family = base_family)%+replace%
    theme(
      axis.line = element_line(colour = "black",size = 0.7),
      panel.grid.major.y= element_line(colour = "lightgrey", size = 0.25)
    )
}


ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price, color = color))+
  scale_color_discrete_deloitte()+
  theme_deloitte()

ggplot(diamonds )+
  geom_hex(aes(carat, price))+
  scale_fill_deloitte()+
  theme_deloitte()



ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()+
  scale_color_discrete_deloitte()+
  theme_deloitte()



plotly::plot_ly(mpg,x=~displ,y=~hwy)%>%
  add_markers(color=~class,colors=pal_deloitte)
  


plot_ly(diamonds,x=~carat,y=~price)%>%
  add_markers(color=~color,colors=pal_deloitte)



  

plot_ly(diamonds, x = ~cut, color = ~clarity) %>%
  add_histogram(colors=rev(pal_deloitte))

plot_ly(diamonds, y=~price,x = ~carat) %>%
add_histogram2d(zsmooth = "best",colors=rev(pal_deloitte2))

# number of diamonds by cut and clarity (n)
cc <- count(diamonds, cut, clarity)
# number of diamonds by cut (nn)
cc2 <- left_join(cc, count(cc, cut, wt = n))
cc2 %>%
  mutate(prop = n / nn) %>%
  plot_ly(x = ~cut, y = ~prop, color = ~clarity) %>%
  add_bars(colors=(pal_deloitte)) %>%
  layout(barmode = "stack")









m <- lm(hwy ~ displ, data = mpg)
broom::augment(m)%>%
  plot_ly(x = ~displ, showlegend = FALSE) %>%
  add_markers(y = ~hwy, color = ~class) %>%
  add_ribbons(ymin = ~.fitted - 1.96 * .se.fit, 
              ymax = ~.fitted + 1.96 * .se.fit, color = I("gray80")) %>%
  add_lines(y = ~.fitted, color = I("steelblue"))





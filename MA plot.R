library(ggplot2)
library(ggforce)

## Read data
df <- read.csv("plot.MH.csv", stringsAsFactors=FALSE)

#define colours for dots and bars
dotCOLS = c("#a6d8f0","#f9b282", "#66cc99", "#cc99ff", "#CC0033", "#999999", "#3333FF") # Colour of dots
barCOLS = c("#008fd5","#de6b35", "#006666", "#6600FF", "#990033", "#333333", "#000066") # Colour of bars

orderlist <- rev(c("Composite mental health", "Depression", "Anxiety", "Stress", "PTSD", "Suicidal Ideation", "Rumination")) # Order in which variables will appear in the graph

tiff("test.tiff", units="in", width=7, height=5, res=300)
p <- ggplot(data=df,aes(x = factor(Variable, level = orderlist), y = Hedges.g, ymin = lower, ymax = upper, col=Variable, fill = Variable))+
  xlab('')+ ylab("\n Hedge's g (95% Confidence Interval)")+ 
  geom_linerange(size = 4, aes(ymin=lower, ymax=upper,col=Variable),width=0.5,cex=1)+
  geom_point(size=4, shape=21, colour="white", stroke = .5)+
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
    geom_hline(aes(fill=Variable),yintercept =0, linetype=2)+
  theme(axis.text.y=element_text(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=12,face="bold")) +
  theme_minimal()+
  theme(legend.position = "none")+
  coord_cartesian(clip = "off")+
  annotation_custom(grid::textGrob("favours control group", rot = 0), 
                    xmin = 0.6, xmax = 0.6, ymin = 0) +
  annotation_custom(grid::textGrob("favours intervention group", rot = 0), 
                    xmin = 0.6, xmax = 0.6, ymax = 0)+
  coord_flip()


dev.off()

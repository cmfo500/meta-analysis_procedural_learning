### Transform fisher z back to r
ztor<- function(z){
  ((exp(1)^(2*z))-1)/((exp(1)^(2*z))+1)
}

# Read data
df <- read.csv("plot.csv", stringsAsFactors=FALSE)

df[c(6:8)] <- lapply(df[c(3:5)], ztor) # transform Fisher z to Pearson's R

df$Group <- as.factor(df$Group)

#define colours for dots and bars
dotCOLS = c("#a6d8f0","#f9b282", "#66cc99") # Colour of dots
barCOLS = c("#008fd5","#de6b35", "#006666") # Colour of bars


orderlist <- c("Grammar","Phonology", "Vocabulary", "Reading", "Spelling") # Order in which variables will appear in the graph
  
tiff("test.tiff", units="in", width=5, height=5, res=300)
p = ggplot(data=df,aes(x = Group,y = Fisher.z.1, ymin = lower.1, ymax = upper.1, col=Group, fill = Group))+
    xlab('')+ ylab("\n Pearson's Correlation (95% Confidence Interval)")+
  geom_linerange(size = 4, aes(ymin=lower.1, ymax=upper.1,col=Group),width=0.5,cex=1)+
  geom_point(size=4, shape=21, colour="white", stroke = .5)+
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  facet_wrap(~factor(Variable, levels = orderlist), strip.position="left",nrow=9) +
  geom_hline(aes(fill=Group),yintercept =0, linetype=2)+
  coord_flip() +
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold")) +
  theme_minimal()+
  theme(
        axis.text.y=element_blank()

  )

p
dev.off()

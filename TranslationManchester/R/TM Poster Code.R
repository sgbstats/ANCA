library(tidyverse)
library(survival)
load("Data/Data.RDa")
load("Data/Models.RDa")


## KM plot
library(survminer)

Data3=Data %>% mutate(C1=case_when(CREATININE>=450~ "C2",
                                   CREATININE>=250~"C1",
                                   CREATININE>=0~"C0")) %>% 
  mutate(C4=case_when(C1=="C2"~11,
                      C1=="C1"~4,
                      C1=="C0"~0),
         NS4=case_when(N=="N2"~7,
                       N=="N1"~4,
                       N=="N0"~0),
         TA4=if_else(TA=="T1",3,0),
         RRS4=C4+NS4+TA4,
         RRS4GR=factor(case_when(RRS4<=4~"Low",
                                 RRS4<=11~"Moderate",
                                 RRS4<=18~"High",
                                 !is.na(RRS4)~"Very High"), c("Low", "Moderate","High", "Very High"), ordered = T)) %>% 
  filter(!is.na(RRS4)) %>% 
  mutate(GRP=factor(paste(COHORT, RRS4GR), c("DEV Low", "VAL Low","DEV Moderate", "VAL Moderate","DEV High", "VAL High", "DEV Very High", "VAL Very High"), ordered = T))


surv2=survfit(Surv(TTEESKDCNSR, ESKD)~RRS4GR+COHORT , data=Data3)
g2=ggsurvplot(
  surv2,
  data = Data3,
  size = 1,
  fun = NULL,
  # palette = rep(c("#4DAF4A","#FF7F00","#E41A1C", "#660000" ),2),
  risk.table = TRUE,
  break.time.by=5,
  risk.table.col = "Black",
  legend.labs = c("Development Low","Validation Low", "Development Moderate", "Validation Moderate",
                  "Development High","Validation High", "Development Very High",   "Validation Very High"),
  linetype = c("COHORT"),
  risk.table.height = 0.16, 
  ggtheme = theme_classic2()+theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title.y=element_text(vjust = -2),
    # text = element_text(size = 6),
    legend.key.size = unit(0.3, 'cm'),
    # legend.spacing.x = unit(1, 'cm')
  ),
  risk.table.y.text.col = F,
  censor.shap="'",
  legend.title="RRS Group",
  xlab="Years",
  ylab="Renal Survival",
  xlim=c(0,12),
  break.x.by=2 ,
  surv.scale="percent",
  tables.theme = theme_classic2()+theme(axis.title.x=element_blank(),
                                        
                                        axis.text.x=element_blank(),
                                        axis.ticks=element_blank(),
                                        axis.line.x = element_blank(),
                                        axis.line.y = element_blank())
)
g2$table=g2$table+theme(plot.title = element_blank(),
                        # axis.title.y = element_blank()
)


cols=c("Development Low"="#4DAF4A","Development Moderate"="#FF7F00","Development High"="#E41A1C","Development Very High"="#660000",
       "Validation Low"="#4DAF4A","Validation Moderate"="#FF7F00","Validation High"="#E41A1C","Validation Very High"="#660000")


hold=g2$plot+scale_colour_manual(name = 'RRS Group', 
                                 values=cols,
                                 labels = rep(c("Low", "Moderate", "High", "Very High")),
                                 breaks = c("Development Low", "Development Moderate", "Development High", "Development Very High" )
)+
  scale_linetype_manual(name = "Cohort",values=c(1,6), labels = c("Development", "Validation"))+
  theme(axis.title.y= element_text(vjust = -40),
        legend.box = "vertical",
        legend.spacing.y = unit(-0.25, "cm"),
        legend.box.just = "left",
        legend.justification = "left"
  )+
  guides(colour = guide_legend(order=1),
         linetype= guide_legend(order=2))
hold
library(patchwork)
hold2=
  (hold)/g2$table+ plot_layout(ncol = 1, heights = c(3, 1)) + 
  theme(axis.text.y = element_text(color = "Black"),
        axis.title.y=element_blank())
hold2
png("Translation Manchester/Img/Creat RRS2.png", width=900, height=750)
hold2
dev.off()




library(tidyverse)
library(survival)
load("Data/Data.RDa")
#PERCN=Percentage normal
#TA= IFTA score
m0=coxph(Surv(TTEESKDCNSR, ESKD)~I(log(CREATININE))+PERCN+TA, 
         data=Data %>% filter(COHORT=="DEV"))
summary(m0)

#a hypothetical patient with zero prognostic index
#in reality this person could never exist
contnd=data.frame("CREATININE"=1, "PERCN"=0, "TA"="T0")
xhz2=survfit(m0, newdata = contnd)

bl2=data.frame("time"=xhz2$time, "surv"=xhz2$surv, "model"="KM")%>% 
  filter(time!=0) %>% #using log time with zero would break it
  mutate(logh=log(-log(surv))) 

#survival function
x=lm(logh~I(time^-0.1)+I(time^0.1*log(time)), data=bl2 %>% filter(time<10))

fit=bl2 %>%
  cbind.data.frame("pred"=predict(x, bl2)) %>% 
  filter(time<10) %>% ggplot(aes(x=time, y=logh))+
  geom_line()+
  geom_line(aes(y=pred), col="RED")+
  ylab("log(H)")+
  xlab("Time (years)")+
  theme_bw()

ggsave("NHSRC/Slides/Img/fit.jpg", plot=fit, device="jpeg", units = "px", width=1750, height=1000)

# getting the linear predictions/prognostic index
PIcont=cbind.data.frame(Data, "PIcont"=predict(m0, newdata = Data , reference = "zero")) %>% 
  group_by(COHORT) %>% 
  mutate(decile=ntile(PIcont, 10)) %>% 
  ungroup()

# regressing the LP/PI on the two cohorts
m0a=coxph(Surv(TTEESKDCNSR, ESKD)~PIcont, data=PIcont %>% filter(COHORT=="DEV"))
m0b=coxph(Surv(TTEESKDCNSR, ESKD)~PIcont, data=PIcont %>% filter(COHORT=="VAL"))

# validation concordance
summary(m0b)

#getting the PI for deciles
ndcont=data.frame("PIcont"=quantile((PIcont %>% filter(COHORT=="VAL"))$PIcont, 
                                    na.rm=T, 
                                    seq(from=0.05, to=0.95, by=0.1)))

# get the survival for each decile point in each cohort
x1=survfit(m0a, newdata = ndcont) 
x2=survfit(m0b, newdata = ndcont)


surv=survfit(Surv(TTEESKDCNSR, ESKD)~decile, data=PIcont %>% filter(COHORT=="VAL"))

#km estimates for each decile at each time point
x3=summary(surv, times = c(1,3,5))
km=cbind.data.frame("group"= rep(1:10, each=3), 
                    "time"=rep(c(1,3,5), 10),
                    "surv"= x3$surv,
                    "model"="KM")

bl=data.frame("time"=x1$time, "surv"=x1$surv, "model"="RRSCDEV") %>% 
  rbind.data.frame(data.frame("time"=x2$time, "surv"=x2$surv, "model"="RRSCVAL")) %>%
  pivot_longer(cols=-c("time", "model"), names_to="group", values_to = "surv") %>% 
  mutate(group=(as.numeric(paste(substr(group, 6,8), "0", sep=""))+5)/10)

bl12=bl %>% group_by(model, group) %>% 
  slice_min(abs(time-1)) %>% 
  mutate(time=1)

bl36=bl %>% group_by(model, group) %>% 
  slice_min(abs(time-3))%>% 
  mutate(time=3)

bl60=bl %>% group_by(model, group) %>% 
  slice_min(abs(time-5))%>% 
  mutate(time=5)

cali=rbind.data.frame(km, bl12, bl36, bl60) %>% 
  pivot_wider(names_from="model", values_from = "surv") %>% 
  pivot_longer(cols=-c("group", "time", "RRSCDEV"), names_to = "model", values_to = "surv") %>% 
  mutate(time=case_when(time==1~"1 year",
                        time==3~"3 years",
                        time==5~"5 years"))

foo=data.frame(x=seq(from=0, to=1, by=0.1)) %>% mutate(y=x)


f3d=cali %>% 
  filter(model=="KM") %>%
  ggplot(aes(x=RRSCDEV, y=surv, col=model))+
  geom_point(size=0.75)+
  geom_line(data=cali %>% filter(model=="RRSCVAL"), aes(x=RRSCDEV, y=surv, col=model))+
  geom_line(data=foo, aes(x=x, y=y, color="APerfect"), size=0.25)+
  xlab("Predicted Survival")+
  ylab("Observed Suvival")+
  xlim(c(0,1))+
  scale_color_manual(name="",
                     labels=c("Perfect Calibration","Observed Calibration" ),
                     values = c("APerfect"="#005EB8","KM"="#67B437", "RRSCVAL"= "#67B437"),
                     breaks = c("APerfect", "KM"))+
  theme_bw() +
  theme(legend.position = "bottom")+
  scale_y_continuous(breaks = seq(0,1,0.2),
                     labels=c("0", "0.2", "0.4","0.6", "0.8", 1))+
  scale_x_continuous(breaks = seq(0,1,0.2),
                     labels=c("0", "0.2", "0.4","0.6", "0.8", 1))+
  facet_wrap(vars(time))
f3d
ggsave(plot=f3d, "NHSRC/Slides/Img/RRS Cali.jpg", width = 6, height = 3, unit="in"  )

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
png("NHSRC/Slides/Img/Creat RRS2.png", width=600, height=750)
hold2
dev.off()


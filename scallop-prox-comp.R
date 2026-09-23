require(arm)
require(bbmle)
require(dplyr)
require(ggplot2)
require(lme4)
require(lubridate)
require(mgcv)
require(openxlsx)
require(patchwork)
require(tidyverse)
require(ggrepel)
require(cowplot)
require(geomtextpath)
require(gratia)

################## Set up #######################
# how many simulations for bootstrapped CIs?
nsim <- 1000 # 1000 sims takes about 20 minutes to knit

new_hyd <- read.csv("scallop-prox-comp.csv")

new_hyd$month.fac <- as.factor(month(ymd(new_hyd$Date.fished)))
new_hyd$year <- year(ymd(new_hyd$Date.fished))
new_hyd$day <- day(ymd(new_hyd$Date.fished))
new_hyd$wmw <- new_hyd$ww_meat
new_hyd$scalnum <- new_hyd$ID
new_hyd$sh <- as.numeric(new_hyd$sh)
new_hyd$X <- new_hyd$lon
new_hyd$Y <- new_hyd$lat
new_hyd$date <- ymd(new_hyd$Date.fished)
new_hyd$month <- month(new_hyd$Date.fished)
new_hyd$day <- day(new_hyd$Date.fished)
new_hyd$year <- year(new_hyd$Date.fished)
new_hyd$scalnum <- new_hyd$ID
new_hyd$dmw <- new_hyd$wmw*new_hyd$water_meat
new_hyd$wgw <- new_hyd$ww_gonad
new_hyd$dgw <- new_hyd$wgw*new_hyd$water_gonad
new_hyd$lipid..[new_hyd$Date.fished=="2024-05-30"] <- NA

################## Sampling summary tables #######################
samp_table <- new_hyd %>%
  group_by(Date.fished) %>%
  dplyr::summarize(num.mw = length(wmw[!is.na(wmw)]),
                   #median.mw = median(wmw[!is.na(wmw)]),
                   min.mw = round(min(wmw[!is.na(wmw)]),2),
                   max.mw = round(max(wmw[!is.na(wmw)]),2),
                   num.dmw = length(dmw[!is.na(dmw)]),
                   #median.dmw = median(dmw[!is.na(dmw)]),
                   min.dmw = round(min(dmw[!is.na(dmw)]),2),
                   max.dmw = round(max(dmw[!is.na(dmw)]),2),
                   num.sh = length(sh[!is.na(sh)]),
                   #median.sh = median(sh[!is.na(sh)]),
                   min.sh = round(min(sh[!is.na(sh)]),2),
                   max.sh = round(max(sh[!is.na(sh)]),2),
                   num.gw = length(wgw[!is.na(wgw)]),
                   #median.mw = median(wmw[!is.na(wmw)]),
                   min.gw = round(min(wgw[!is.na(wgw)]),2),
                   max.gw = round(max(wgw[!is.na(wgw)]),2),
                   num.lipid = length(lipid..[!is.na(lipid..)]),
                   #median.lipid = median(lipid..[!is.na(lipid..)]),
                   min.lipid = round(min(lipid..[!is.na(lipid..)]),2),
                   max.lipid = round(max(lipid..[!is.na(lipid..)]),2),
                   num.ash = length(perc.ash[!is.na(perc.ash)]),
                   #median.ash = median(perc.ash[!is.na(perc.ash)]),
                   min.ash = round(min(perc.ash[!is.na(perc.ash)]),2),
                   max.ash = round(max(perc.ash[!is.na(perc.ash)]),2),
                   num.protein = length(Nx5.6[!is.na(Nx5.6)]),
                   #median.protein = median(Nx5.6[!is.na(Nx5.6)]),
                   min.protein = round(min(Nx5.6[!is.na(Nx5.6)]),2),
                   max.protein = round(max(Nx5.6[!is.na(Nx5.6)]),2))

samp_table$mw <- paste0(samp_table$num.mw, " (", samp_table$min.mw, "-", samp_table$max.mw, ")")
samp_table$dmw <- paste0(samp_table$num.dmw, " (", samp_table$min.dmw, "-", samp_table$max.dmw, ")")
samp_table$sh <- paste0(samp_table$num.sh, " (", samp_table$min.sh, "-", samp_table$max.sh, ")")
samp_table$lipid <- paste0(samp_table$num.lipid, " (", samp_table$min.lipid, "-", samp_table$max.lipid, ")")
samp_table$ash <- paste0(samp_table$num.ash, " (", samp_table$min.ash, "-", samp_table$max.ash, ")")
samp_table$protein <- paste0(samp_table$num.protein, " (", samp_table$min.protein, "-", samp_table$max.protein, ")")
samp_table$gw <- paste0(samp_table$num.gw, " (", samp_table$min.gw, "-", samp_table$max.gw, ")")

samp_table2 <- new_hyd %>%
  summarize(num.mw = length(wmw[!is.na(wmw)]),
            #median.mw = median(wmw[!is.na(wmw)]),
            min.mw = round(min(wmw[!is.na(wmw)]),2),
            max.mw = round(max(wmw[!is.na(wmw)]),2),
            num.dmw = length(dmw[!is.na(dmw)]),
            #median.dmw = median(dmw[!is.na(dmw)]),
            min.dmw = round(min(dmw[!is.na(dmw)]),2),
            max.dmw = round(max(dmw[!is.na(dmw)]),2),
            num.sh = length(sh[!is.na(sh)]),
            #median.sh = median(sh[!is.na(sh)]),
            min.sh = round(min(sh, na.rm=T),2),
            max.sh = round(max(sh[!is.na(sh)]),2),
            num.gw = length(wgw[!is.na(wgw)]),
            #median.mw = median(wmw[!is.na(wmw)]),
            min.gw = round(min(wgw[!is.na(wgw)]),2),
            max.gw = round(max(wgw[!is.na(wgw)]),2),
            num.lipid = length(lipid..[!is.na(lipid..)]),
            #median.lipid = median(lipid..[!is.na(lipid..)]),
            min.lipid = round(min(lipid..[!is.na(lipid..)]),2),
            max.lipid = round(max(lipid..[!is.na(lipid..)]),2),
            num.ash = length(perc.ash[!is.na(perc.ash)]),
            #median.ash = median(perc.ash[!is.na(perc.ash)]),
            min.ash = round(min(perc.ash[!is.na(perc.ash)]),2),
            max.ash = round(max(perc.ash[!is.na(perc.ash)]),2),
            num.protein = length(Nx5.6[!is.na(Nx5.6)]),
            #median.protein = median(Nx5.6[!is.na(Nx5.6)]),
            min.protein = round(min(Nx5.6[!is.na(Nx5.6)]),2),
            max.protein = round(max(Nx5.6[!is.na(Nx5.6)]),2))

samp_table2$mw <- paste0(samp_table2$num.mw, " (", samp_table2$min.mw, "-", samp_table2$max.mw, ")")
samp_table2$dmw <- paste0(samp_table2$num.dmw, " (", samp_table2$min.dmw, "-", samp_table2$max.dmw, ")")
samp_table2$sh <- paste0(samp_table2$num.sh, " (", samp_table2$min.sh, "-", samp_table2$max.sh, ")")
samp_table2$lipid <- paste0(samp_table2$num.lipid, " (", samp_table2$min.lipid, "-", samp_table2$max.lipid, ")")
samp_table2$ash <- paste0(samp_table2$num.ash, " (", samp_table2$min.ash, "-", samp_table2$max.ash, ")")
samp_table2$protein <- paste0(samp_table2$num.protein, " (", samp_table2$min.protein, "-", samp_table2$max.protein, ")")
samp_table2$gw <- paste0(samp_table2$num.gw, " (", samp_table2$min.gw, "-", samp_table2$max.gw, ")")

samp_table2$Date.fished = "2024-2025"

samp_table <- rbind(samp_table, samp_table2)
samp_table <- dplyr::select(samp_table, Date.fished, mw, dmw, sh, gw, lipid, protein, ash)
samp_table$sh <- gsub(x=samp_table$sh, pattern="(Inf--Inf)", replacement="", fixed=T)
samp_table$lipid <- gsub(x=samp_table$lipid, pattern="(Inf--Inf)", replacement="", fixed=T)
samp_table$protein <- gsub(x=samp_table$protein, pattern="(Inf--Inf)", replacement="", fixed=T)
samp_table %>% View()

samp_table_med <- new_hyd %>%
  group_by(Date.fished) %>%
  summarize(median.mw = median(wmw[!is.na(wmw)]),
            median.dmw = median(dmw[!is.na(dmw)]),
            median.sh = median(sh[!is.na(sh)]),
            median.gw = median(wgw[!is.na(wgw)]),
            median.lipid = median(lipid..[!is.na(lipid..)]),
            median.ash = median(perc.ash[!is.na(perc.ash)]),
            median.protein = median(Nx5.6[!is.na(Nx5.6)]),
  )


shapiro.test(new_hyd$wmw) # not normal
hist(new_hyd$wmw) # left skewed
shapiro.test(log(new_hyd$wmw)) # reject null, so normal
hist(log(new_hyd$wmw)) # looks pretty normal
qqnorm(log(new_hyd$wmw), pch = 1, frame = FALSE)
qqline(log(new_hyd$wmw), col = "steelblue", lwd = 2)
# great, looks normal

################## Interpolating missing months (or not) #######################
# ignore the months with no SH data first
new_hyd$month.fac <- as.factor(new_hyd$Date.fished)
mod.new.mwsh <- lm(log(wmw) ~ log(sh)*month.fac, data = new_hyd[!is.na(new_hyd$sh),])
summary(mod.new.mwsh)
anova(mod.new.mwsh)
plot(mod.new.mwsh$residuals)
unique(new_hyd$month.fac)

png("curves_wmw_resid.png", height=5, width=6, res=400, units="in")
ggplot() +
  geom_point(data=mod.new.mwsh, aes(x = .fitted, y = .resid)) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("fitted") +
  ylab("residual")
dev.off()

month.fac <- sort(unique(new_hyd[!is.na(new_hyd$sh),]$month.fac))
month_tab <- NULL
for(i in 1:length(month.fac)){
  if(i==1) a=0; b=0
  if(!i==1){
    b <- coef(mod.new.mwsh)[[paste0("log(sh):month.fac",month.fac[i])]]
    a <- coef(mod.new.mwsh)[[month.fac[i]]]
  }
  b <- data.frame(month=month.fac[i], 
                  a=(coef(mod.new.mwsh)[["(Intercept)"]] + a), 
                  b= (coef(mod.new.mwsh)[["log(sh)"]] + b))
  month_tab <- rbind(month_tab, b)
  month_tab <- arrange(month_tab, as.numeric(month))
}
#log(wmw) = aL^b
#wmw = exp(bL+a) # IS THIS OK INSTEAD OF log(sh)? 
# h100 <- exp(month_tab$a + month_tab$b*100)
h100 <- exp(month_tab$a)*(100^month_tab$b)

month_tab$month.fac <- as.factor(month_tab$month)
month_tab$sh <- 100
month_tab <- cbind(month_tab, predict(mod.new.mwsh, newdata=month_tab, se.fit=T))
month_tab$h100 <- h100
month_tab$pred <- exp(month_tab$fit)
month_tab$UCI <- exp(month_tab$fit + 1.96*month_tab$se.fit)
month_tab$LCI <- exp(month_tab$fit - 1.96*month_tab$se.fit)

curves <- expand.grid(month.fac=unique(month_tab$month.fac), sh=seq(floor(min(new_hyd$sh, na.rm=T)),ceiling(max(new_hyd$sh, na.rm=T)), 1))
curves <- cbind(curves, predict(mod.new.mwsh, newdata=curves, se.fit=T))
curves$pred <- exp(curves$fit)
curves$UCI <- exp(curves$fit + 1.96*curves$se.fit)
curves$LCI <- exp(curves$fit - 1.96*curves$se.fit)

png("MWSH_curves_glm_whole.png", height=5, width=6, res=400, units="in")
ggplot() + 
  geom_point(data=new_hyd, aes(x=sh,y=wmw), size=0.1)+
  geom_line(data=curves, aes(x=sh, group=as.numeric(month.fac), colour=(month.fac),y=pred), linewidth=1)+
  geom_text_repel(data=unique(curves[curves$sh==max(curves$sh),c("sh", "month.fac", "pred")]), aes(x=max(curves$sh), group=as.numeric(month.fac), colour=(month.fac), y=pred, label=month.fac),segment.colour="darkgrey", nudge_x = 5)+
  ylab("Wet meat weight (g)") + 
  xlab("Shell height (mm)")+
  theme_classic()+ scale_colour_viridis_d(guide="none", end = 0.9) 
dev.off()

ggplot() + #geom_line(aes(x=month.fac,y=wmw)) +
  geom_point(data=month_tab, aes(x=ymd(month),y=pred))+
  geom_line(data=month_tab, aes(x=ymd(month),y=pred))+
  geom_ribbon(data=month_tab, aes(x=ymd(month),ymin=LCI,ymax=UCI), alpha=0.5)+
  ylab("Wet meat weight (g, 100 mm shell)") + #ylim(c(10,18)) +
  scale_x_date(name="Date", breaks="month", date_labels = "%b %Y") +
  theme_classic()


# so because there are samples without SH, let's try methods to fill in those SH.
# first, use an overall MWSH relationship to fill in those SH
no_month_mod <- glm(log(sh) ~ log(wmw), data=new_hyd[!is.na(new_hyd$sh),])
new_hyd <- cbind(new_hyd, fit2=predict(no_month_mod, newdata=new_hyd))
ggplot()+geom_point(data=new_hyd[new_hyd$date=="2025-01-30",], aes(exp(fit2), wmw))
new_hyd$sh2 <- new_hyd$sh
new_hyd$sh2[is.na(new_hyd$sh2)]  <- exp(new_hyd$fit2[is.na(new_hyd$sh2)])
#new_hyd$sh[new_hyd$date=="2025-01-30"]

new_hyd$month.fac <- as.factor(new_hyd$Date.fished)
mod.new.mwsh <- glm(log(wmw) ~ log(sh2)*month.fac, data = new_hyd[!is.na(new_hyd$sh2),])
summary(mod.new.mwsh)
anova(mod.new.mwsh)
plot(mod.new.mwsh$residuals)
unique(new_hyd$month.fac)

month.fac <- sort(unique(new_hyd[!is.na(new_hyd$sh2),]$month.fac))
month_tab2 <- NULL
for(i in 1:length(month.fac)){
  if(i==1) a2=0; b2=0
  if(!i==1){
    b2 <- coef(mod.new.mwsh)[[paste0("log(sh2):month.fac",month.fac[i])]]
    a2 <- coef(mod.new.mwsh)[[month.fac[i]]]
  }
  b2 <- data.frame(month=month.fac[i], 
                   a2=(coef(mod.new.mwsh)[["(Intercept)"]] + a2), 
                   b2= (coef(mod.new.mwsh)[["log(sh2)"]] + b2))
  month_tab2 <- rbind(month_tab2, b2)
  month_tab2 <- arrange(month_tab2, as.numeric(month))
}
#log(wmw) = aL^b
#wmw = exp(bL+a) # IS THIS OK INSTEAD OF log(sh)? 
# h100 <- exp(month_tab$a + month_tab$b*100)
h100 <- exp(month_tab2$a2)*(100^month_tab2$b2)

month_tab2$month.fac <- as.factor(month_tab2$month)
month_tab2$sh2 <- 100
month_tab2 <- cbind(month_tab2, predict(mod.new.mwsh, newdata=month_tab2, se.fit=T))
month_tab2$h100 <- h100
month_tab2$pred2 <- exp(month_tab2$fit)
month_tab2$UCI2 <- exp(month_tab2$fit + 1.96*month_tab2$se.fit)
month_tab2$LCI2 <- exp(month_tab2$fit - 1.96*month_tab2$se.fit)

curves2 <- expand.grid(month.fac=unique(month_tab2$month.fac), sh2=seq(floor(min(new_hyd$sh2, na.rm=T)),ceiling(max(new_hyd$sh2, na.rm=T)), 1))
curves2 <- cbind(curves2, predict(mod.new.mwsh, newdata=curves2, se.fit=T))
curves2$pred2 <- exp(curves2$fit)
curves2$UCI2 <- exp(curves2$fit + 1.96*curves2$se.fit)
curves2$LCI2 <- exp(curves2$fit - 1.96*curves2$se.fit)

png("MWSH_curves_glm_overallSH.png", height=5, width=6, res=400, units="in")
ggplot() + 
  geom_point(data=new_hyd, aes(x=sh2,y=wmw), size=0.1)+
  geom_line(data=curves2, aes(x=sh2, group=as.numeric(month.fac), colour=(month.fac),y=pred2), linewidth=1)+
  geom_text_repel(data=unique(curves2[curves2$sh2==max(curves2$sh2),c("sh2", "month.fac", "pred2")]), aes(x=max(curves2$sh2), group=as.numeric(month.fac), colour=(month.fac), y=pred2, label=month.fac),segment.colour="darkgrey", nudge_x = 5)+
  ylab("Wet meat weight (g)") + 
  xlab("Shell height (mm)")+
  theme_classic()+ scale_colour_viridis_d(guide="none", end = 0.9) 
dev.off()

# and what if I use the nearest month...
feb_mod <- glm(log(sh) ~ log(wmw), data=new_hyd[new_hyd$month==2,])
new_hyd <- cbind(new_hyd, fit3=predict(feb_mod, newdata=new_hyd))
jul_mod <- glm(log(sh) ~ log(wmw), data=new_hyd[new_hyd$month==7,])
new_hyd <- cbind(new_hyd, fit4=predict(jul_mod, newdata=new_hyd))
new_hyd$sh3 <- new_hyd$sh
new_hyd$sh3[is.na(new_hyd$sh3) & new_hyd$month==1] <- exp(new_hyd$fit3[is.na(new_hyd$sh3) & new_hyd$month==1])
new_hyd$sh3[is.na(new_hyd$sh3) & new_hyd$month==7] <- exp(new_hyd$fit4[is.na(new_hyd$sh3) & new_hyd$month==7])
#new_hyd$sh[new_hyd$date=="2025-01-30"]

mod.new.mwsh <- glm(log(wmw) ~ log(sh3)*month.fac, data = new_hyd[!is.na(new_hyd$sh3),])
summary(mod.new.mwsh)
anova(mod.new.mwsh)
plot(mod.new.mwsh$residuals)
unique(new_hyd$month.fac)

month.fac <- sort(unique(new_hyd[!is.na(new_hyd$sh3),]$month.fac))
month_tab3 <- NULL
for(i in 1:length(month.fac)){
  if(i==1) a3=0; b3=0
  if(!i==1){
    b3 <- coef(mod.new.mwsh)[[paste0("log(sh3):month.fac",month.fac[i])]]
    a3 <- coef(mod.new.mwsh)[[month.fac[i]]]
  }
  b3 <- data.frame(month=month.fac[i], 
                   a3=(coef(mod.new.mwsh)[["(Intercept)"]] + a3), 
                   b3= (coef(mod.new.mwsh)[["log(sh3)"]] + b3))
  month_tab3 <- rbind(month_tab3, b3)
  month_tab3 <- arrange(month_tab3, as.numeric(month))
}
#log(wmw) = aL^b
#wmw = exp(bL+a) # IS THIS OK INSTEAD OF log(sh)? 
# h100 <- exp(month_tab$a + month_tab$b*100)
h100 <- exp(month_tab3$a3)*(100^month_tab3$b3)

month_tab3$month.fac <- as.factor(month_tab3$month)
month_tab3$sh3 <- 100
month_tab3 <- cbind(month_tab3, predict(mod.new.mwsh, newdata=month_tab3, se.fit=T))
month_tab3$h100 <- h100
month_tab3$pred3 <- exp(month_tab3$fit)
month_tab3$UCI3 <- exp(month_tab3$fit + 1.96*month_tab3$se.fit)
month_tab3$LCI3 <- exp(month_tab3$fit - 1.96*month_tab3$se.fit)

curves3 <- expand.grid(month.fac=unique(month_tab3$month.fac), sh3=seq(floor(min(new_hyd$sh3, na.rm=T)),ceiling(max(new_hyd$sh3, na.rm=T)), 1))
curves3 <- cbind(curves3, predict(mod.new.mwsh, newdata=curves3, se.fit=T))
curves3$pred3 <- exp(curves3$fit)
curves3$UCI3 <- exp(curves3$fit + 1.96*curves3$se.fit)
curves3$LCI3 <- exp(curves3$fit - 1.96*curves3$se.fit)

png("MWSH_curves_glm_nearSH.png", height=5, width=6, res=400, units="in")
ggplot() + 
  geom_point(data=new_hyd, aes(x=sh3,y=wmw), size=0.1)+
  geom_line(data=curves3, aes(x=sh3, group=as.numeric(month.fac), colour=(month.fac),y=pred3), linewidth=1)+
  geom_text_repel(data=unique(curves3[curves3$sh3==max(curves3$sh3),c("sh3", "month.fac", "pred3")]), aes(x=max(curves3$sh3), group=as.numeric(month.fac), colour=(month.fac), y=pred3, label=month.fac),segment.colour="darkgrey", nudge_x = 5)+
  ylab("Wet meat weight (g)") + 
  xlab("Shell height (mm)")+
  theme_classic()+ scale_colour_viridis_d(guide="none", end = 0.9) 
dev.off()

month_tab3 <- dplyr::select(month_tab3, month, a3, b3, pred3, UCI3, LCI3)
month_tab2 <- dplyr::select(month_tab2, month, a2, b2, pred2, UCI2, LCI2)
month_tab <- dplyr::select(month_tab, month, a, b, pred, UCI, LCI)

month_tab_comp <- left_join(left_join(month_tab2, month_tab), month_tab3)

month_tab_comp <- month_tab_comp %>% pivot_longer(cols=c(a2, b2, pred2, UCI2, LCI2, a, b, pred, UCI, LCI, a3, b3, pred3, UCI3, LCI3))
month_tab_comp$type<- NA
month_tab_comp$type[grep(x = month_tab_comp$name, pattern=2)] <- "overall"
month_tab_comp$type[grep(x = month_tab_comp$name, pattern=3)] <- "nearest"
month_tab_comp$type[is.na(month_tab_comp$type)] <- "whole"
month_tab_comp$name <- gsub(x=month_tab_comp$name, pattern="2", replacement="")
month_tab_comp$name <- gsub(x=month_tab_comp$name, pattern="3", replacement="")
month_tab_comp <- pivot_wider(month_tab_comp, id_cols = c(month, type))

ggplot() + geom_line(data=month_tab_comp, aes(ymd(month), pred, colour=as.factor(type))) +
  geom_point(data=month_tab_comp, aes(ymd(month), pred, colour=as.factor(type)))

bottom <- ggplot() + geom_line(data=month_tab_comp, aes(ymd(month), pred)) +
  geom_point(data=month_tab_comp, aes(ymd(month), pred)) +
  facet_wrap(~type) +
  ylab("Wet meat weight (g, 100 mm shell)") + #ylim(c(10,18)) +
  #scale_x_continuous(breaks=1:12,labels = months, name="Month")+
  theme_bw() +
  geom_ribbon(data=month_tab_comp, aes(x=ymd(month),ymin=LCI,ymax=UCI), alpha=0.5) +
  xlab("Sample collection date")

curves3$sh <- curves3$sh3
curves3 <- dplyr::select(curves3, month.fac, sh, pred3, UCI3, LCI3)
curves2$sh <- curves2$sh2
curves2 <- dplyr::select(curves2, month.fac, sh, pred2, UCI2, LCI2)
curves <- dplyr::select(curves, month.fac, sh, pred, UCI, LCI)

curves_comp <- left_join(left_join(curves2, curves), curves3)

curves_comp <- curves_comp %>% pivot_longer(cols=c(pred2, UCI2, LCI2, pred, UCI, LCI, pred3, UCI3, LCI3))
curves_comp$type<- NA
curves_comp$type[grep(x = curves_comp$name, pattern=2)] <- "overall"
curves_comp$type[grep(x = curves_comp$name, pattern=3)] <- "nearest"
curves_comp$type[is.na(curves_comp$type)] <- "whole"
curves_comp$name <- gsub(x=curves_comp$name, pattern="2", replacement="")
curves_comp$name <- gsub(x=curves_comp$name, pattern="3", replacement="")
curves_comp <- pivot_wider(curves_comp, id_cols = c(month.fac, sh, type))

top <- ggplot() + 
  geom_line(data=curves_comp, aes(sh, pred, colour=month.fac)) +
  facet_wrap(~type) +
  ylab("Wet meat weight (g)") + #ylim(c(10,18)) +
  xlab("Shell height") +
  scale_colour_viridis_d(end = 0.9, name="Sample collection date") +
  theme_bw() 

png("sh comparison.png", height=7, width=9, units="in", res=400)
top/bottom
dev.off()

################## Spatial/Depth checks #######################
funs <- c("https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/github_spatial_import.R",
          "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/pectinid_projector_sf.R")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
dir <- tempdir()
for(fun in funs) 
{
  temp <- dir
  download.file(fun,destfile = paste0(dir, "\\", basename(fun)))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
} # end for(un in funs)

offshore <- github_spatial_import(subfolder = "offshore", zipname = "offshore.zip")

source("C:/Users/freya/Documents/Github/Assessment_fns/Maps/pectinid_projector_sf.R")
extentin <- data.frame(x=c(-68.25, -65.5), y=c(40.75, 42.5), crs=4326)
p <- pecjector(area=extentin, plot = T, 
               add_layer = list(land = 'grey',eez = 'eez', bathy = c(10, "c", 1000), sfa = 'offshore', scale.bar = c('tl',0.2, -1, -1)))

locs <- new_hyd[!is.na(new_hyd$lon),] %>%
  dplyr::group_by(Date.fished, lon, lat) %>%
  dplyr::summarize(nsamp = length(unique(ID)))

locs$month <- month(locs$Date.fished)
locs$month[locs$month==5 & year(locs$Date.fished)==2024] <- "5/24"
locs$month[locs$month==5 & year(locs$Date.fished)==2025] <- "5/25"

map <- p +
  geom_point(data=locs, aes(lon, lat, size=nsamp)) +
  geom_text_repel(data=locs, aes(lon, lat, label = month), box.padding = 0.5, min.segment.length = 0, max.overlaps=100) +
  scale_size_continuous(name=NULL)+
  theme(legend.position = "inside", legend.position.inside = c(0.8,0.2), legend.background = element_rect(fill="lightgrey", colour="black"), legend.key = element_blank(), panel.border = element_rect(colour="black")) 

extentlrg <- data.frame(x=c(-73, -52), y=c(39.5, 52), crs=4326)
map_tmp <- pecjector(area = extentlrg, repo="github", add_layer=list(
  bathy=c(1000, 'c', 5000), 
  eez="eez", 
  land="world",
  #nafo="main",
  #sfa="offshore", 
  scale.bar = c("br", 0.3, -1,-1)), 
  # direct = "C:/Users/keyserf/Documents/", 
  # direct_fns="C:/Users/keyserf/Documents/Github/Assessment_fns/", 
  plot_package = "ggplot2")

extent2 <- data.frame(x=c(-68.25, -65.5), y=c(40.75, 42.5), crs=4326)
box <- st_as_sfc(st_bbox(st_as_sf(extent2, coords = c("x", "y"), crs=4326)))
eezlab <- data.frame(labs = c("US", "CA"),
                     x = c(-71,-66),
                     y = c(44.5,46.5))

map_lrg <- map_tmp + geom_sf(data=box, fill=NA, colour="black", lwd=0.5) +
  geom_text(data=eezlab, aes(x=x, y=y, label=labs), angle=0, colour="darkred", size=6) +
  coord_sf(expand=F) 

png("new_hyd_map.png", height=19, width=25, res=400, units="cm")
map %>% 
  ggdraw() +
  draw_plot(
    {
      map_lrg + 
        theme(axis.text = element_blank(),
              axis.ticks=element_blank(),
              plot.background=element_blank(),
              panel.border = element_rect(fill=NA, colour="black"))+
        coord_sf(expand = FALSE) +
        theme(legend.position = "none")
    },
    x = 0.1, 
    y = 0.05,
    width = 0.5, 
    height = 0.5)
dev.off()

# get depths from pecjector map?
p <- pecjector(obj = NULL, area = "GB",plot = T, 
               add_layer = list(land = 'grey',eez = 'eez', bathy = c(1, "both", 100), nafo = 'main',sfa = 'offshore',survey = c('offshore','outline'),scale.bar = c('bl',0.2, -1, -1)))

map_dat <- ggplot_build(p)

bathy <- map_dat$data[[3]]
bathy_sf <- st_as_sf(x=bathy, coords=c(X="x", Y="y"), crs=4326)
locs_sf <- st_as_sf(x=locs, coords=c(X="lon", Y="lat"), crs=4326)
test <- st_nearest_feature(locs_sf, bathy_sf)
bathy_sf[test,]$level
locs$depth <- bathy_sf[test,]$level

ggplot() + geom_point(data=locs, aes(Date.fished, depth)) + theme_bw() +
  ylab("Depth (m)") + 
  xlab("Date fished (yyyy-mm-dd)")
# so everything is approximately between 60 and 80m depth.
# looked at geology maps too, and northern area is consistently postglacial sand and gravel

ggplot() + geom_point(data=locs, aes(lon, Date.fished))

#locs
new_hyd <- left_join(new_hyd, dplyr::select(locs,-month))

hist(log(new_hyd$wmw))

depth_mod <- glm(log(wmw) ~ log(sh) + depth + log(sh):depth, data = new_hyd[!is.na(new_hyd$sh),])
summary(depth_mod)

depth_mod <- glm(log(wmw) ~ log(sh) + depth, data = new_hyd[!is.na(new_hyd$sh),])
summary(depth_mod)
# AIC WINS, no interaction

depth_tab4 <- expand.grid(depth=seq(min(new_hyd[!is.na(new_hyd$sh),]$depth), max(new_hyd[!is.na(new_hyd$sh),]$depth), 1), sh=100)
depth_tab4 <- cbind(depth_tab4, predict(depth_mod, newdata=depth_tab4, se.fit=T))
depth_tab4$pred4 <- exp(depth_tab4$fit)
depth_tab4$UCI4 <- exp(depth_tab4$fit + 1.96*depth_tab4$se.fit)
depth_tab4$LCI4 <- exp(depth_tab4$fit - 1.96*depth_tab4$se.fit)

curves4 <- expand.grid(depth=c(-60,-70,-80), sh=seq(floor(min(new_hyd$sh, na.rm=T)),ceiling(max(new_hyd$sh, na.rm=T)), 1))
curves4 <- cbind(curves4, predict(depth_mod, newdata=curves4, se.fit=T))
curves4$pred4 <- exp(curves4$fit)
curves4$UCI4 <- exp(curves4$fit + 1.96*curves4$se.fit)
curves4$LCI4 <- exp(curves4$fit - 1.96*curves4$se.fit)

ggplot() + 
  geom_line(data=curves4, aes(sh, pred4, colour=as.factor(depth), group=as.factor(depth))) + 
  theme_bw() +
  ylab("wmw") +
  scale_colour_discrete(name="Depth (m)") +
  xlab("Shell height (mm)") +
  ylab("Wet meat weight (g)")

summary(glm(data=new_hyd[!is.na(new_hyd$depth),], depth ~ month))

ggplot() + geom_boxplot(data=curves4, aes(depth, pred4, group=depth))

ggplot() + 
  geom_point(data=new_hyd[!is.na(new_hyd$depth) & new_hyd$sh > 95 & new_hyd$sh<105,], aes(depth, wmw, colour=month))+
  geom_line(data=depth_tab4, aes(depth, pred4)) + 
  geom_line(data=depth_tab4, aes(depth, UCI4)) +
  geom_line(data=depth_tab4, aes(depth, LCI4))+ 
  theme_bw() + 
  ylab("Wet meat weight (g)") +
  xlab("Depth (m)") +
  scale_colour_continuous(name="Month")


month_tab_comp %>%
  group_by(type) %>%
  summarize(minpred=min(pred, na.rm=T),
            maxpred=max(pred, na.rm=T))
#range is 12.1-23.4 (max is May, min is Nov)

min(depth_tab4$pred4)
max(depth_tab4$pred4)
#range is 15.9 to 18.8 # so the seasonal variability > depth variability
# May is -71; Nov is -81
# max is -71; min is -81
# locs

################## SEASONAL GAMs #############################
new_hyd_gam <- new_hyd
new_hyd_gam <- new_hyd_gam[!is.na(new_hyd_gam$sh) & !is.na(new_hyd_gam$wmw),]
#convert to numeric time
new_hyd_gam <- transform(new_hyd_gam, Time = as.numeric(ymd(date))/1000)

m <- gam(formula = log(wmw) ~ s(sh) + s(Time), data=new_hyd_gam, method="ML")
summary(m)
layout(matrix(1:2, ncol=2))
plot(m)
layout(matrix(1:2, ncol=2))
acf(resid(m), lag.max=36, main="ACF")
pacf(resid(m), lag.max=36, main="pACF")
layout(1)
plot(resid(m))

png("gam_wmw_resid.png", height=5, width=6, res=400, units="in")
gratia::residuals_linpred_plot(m) + 
  theme_bw()
dev.off()

sm <- smooth_estimates(m) |>
  add_confint()
eg1 <- new_hyd_gam |>
  add_partial_residuals(m)

s1 <- sm |>
  filter(.smooth == "s(Time)") |>
  ggplot() +
  geom_rug(aes(x = Time),
           data = eg1,
           sides = "b", length = grid::unit(0.02, "npc")
  ) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Time),
              alpha = 0.2
  ) +
  geom_line(aes(x = Time, y = .estimate)) +
  labs(y = "Partial effect", title = "s(Time)") + scale_x_date(date_labels="%b") + 
  theme_bw() + 
  xlab("Day of year")

s2 <- draw(m, select="s(sh)") + theme_bw() + xlab("sh")

s1 + s2 + plot_layout(ncol=2) 


pdat <- expand.grid(sh=90:140, date=seq(min(ymd(new_hyd_gam$date[!is.na(new_hyd_gam$sh)])), max(ymd(new_hyd_gam$date[!is.na(new_hyd_gam$sh)])), 1))
pdat$Time <- as.numeric(pdat$date)/1000
## predict trend contributions
pdat <- cbind(pdat, predict(m,  newdata = pdat, type="response", se.fit = TRUE))
pdat$wmw_fit <- exp(pdat$fit)
pdat$wmw_UCI <- exp(pdat$fit + 1.96*pdat$se.fit)
pdat$wmw_LCI <- exp(pdat$fit - 1.96*pdat$se.fit)
pdat <- dplyr::select(pdat, -fit, -se.fit)

ggplot() + 
  geom_ribbon(data=pdat[pdat$sh==100,], aes(as.Date(Time*1000), ymin=wmw_LCI, ymax=wmw_UCI), fill="grey") +
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_line(data=pdat[pdat$sh==100,], aes(as.Date(Time*1000), wmw_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("wet meat weight (g for 100 mm shell height)") +
  theme_bw()

min(pdat$date)
max(pdat$date)

pdat$date_adj <- pdat$date 
pdat$date_adj[year(pdat$date_adj)==2024] <- 
  pdat$date_adj[year(pdat$date_adj)==2024]+years(1)

pdat <- pdat[pdat$date<"2025-05-21",]

ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_ribbon(data=pdat[pdat$sh==100,], aes(date, ymin=wmw_LCI, ymax=wmw_UCI), fill="grey") +
  geom_line(data=pdat[pdat$sh==100,], aes(date, wmw_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("wet meat weight (g for 100 mm shell height)") +
  theme_bw()

samp_dates <- unique(data.frame(date=new_hyd$date[!is.na(new_hyd$sh)], month=as.numeric(as.character(month(new_hyd$date[!is.na(new_hyd$sh)])))))
#month_tab <- left_join(month_tab, samp_dates)
month_tab$date_adj <- ymd(month_tab$month)
month_tab$date_adj[year(month_tab$date_adj)==2024] <- month_tab$date_adj[year(month_tab$date_adj)==2024]+years(1)

new_hyd_gam$date_adj <- new_hyd_gam$date
new_hyd_gam$date_adj[year(new_hyd_gam$date_adj)==2024] <- new_hyd_gam$date_adj[year(new_hyd_gam$date_adj)==2024]+years(1)

new_hyd_gam <- dplyr::select(new_hyd_gam, -fit2)

png("MW_gam_seasonal.png", height=5, width=6, res=400, units="in")
ggplot() + 
  geom_rug(data=new_hyd_gam[!new_hyd_gam$date %in% c("2024-07-16", "2025-01-30"),], aes(date))+
  # geom_rug(data=new_hyd_gam[new_hyd_gam$date %in% c("2024-07-16", "2025-01-30"),], aes(date), colour="red")+
  # geom_text(data=new_hyd_gam[new_hyd_gam$date %in% c("2024-07-16", "2025-01-30"),], aes(date_adj, y=-Inf, label="*"), vjust=-1.2, colour="red")+
  geom_ribbon(data=pdat[pdat$sh==100,], aes(date, ymin=wmw_LCI, ymax=wmw_UCI), fill="black", alpha=0.2) + 
  # geom_ribbon(data=month_tab, aes(date, ymin=LCI, ymax=UCI),fill="blue", alpha=0.2) +
  geom_line(data=pdat[pdat$sh==100,], aes(date, wmw_fit), colour="black") +
  # geom_line(data=month_tab, aes(date, pred), colour="blue") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Wet meat weight\n(g for 100 mm shell height)") +
  theme_bw() +
  theme(panel.grid=element_blank())
dev.off()


#Lipid % by month modelled using a GAM, show spline pattern 
new_hyd$Time <- as.numeric(ymd(new_hyd$Date.fished))/1000
lipid_gam <- new_hyd[!is.na(new_hyd$lipid..),]
lipid_gam <- lipid_gam[lipid_gam$Date.fished>"2024-05-30",]
# temp <- lipid_gam[lipid_gam$Date.fished=="2024-07-23",]
# temp$Date.fished <- "2025-07-23"
# new_hyd$Time <- as.numeric(ymd(new_hyd$Date.fished))/1000
# lipid_gam <- lipid_gam
hist(lipid_gam$lipid..)
shapiro.test(lipid_gam$lipid..) #not normal...
qqnorm(lipid_gam$lipid.., pch = 1, frame = FALSE)
qqline(lipid_gam$lipid.., col = "steelblue", lwd = 2)

#go with gamma, since continuous and positive

#ml <- gam(formula = lipid.. ~ s(Time, bs = "cc"), family = Gamma(), data=lipid_gam)
ml <- gam(formula = lipid../100 ~ s(Time), family = betar(link="logit"), data=lipid_gam, method = "ML")
summary(ml)
ml_sh <- gam(formula = lipid../100 ~ s(sh) + s(Time), family = betar(link="logit"), data=lipid_gam, method = "ML")
summary(ml_sh)
AIC(ml_sh)
AIC(ml)
tab_mod_sh <- data.frame(type=rep("lipid",2), 
                         formula = c(deparse(formula(ml)), deparse(formula(ml_sh))),
                         p.sh = c(NA, summary(ml_sh)$s.pv[1]),
                         p.time = c(summary(ml)$s.pv[1], summary(ml_sh)$s.pv[2]),
                         aic= c(AIC(ml), AIC(ml_sh)),
                         rsq = c(summary(ml)$r.sq, summary(ml_sh)$r.sq),
                         dev = c(summary(ml)$dev.expl, summary(ml_sh)$dev.expl))


png("gam_ml_resid.png", height=5, width=6, res=400, units="in")
gratia::residuals_linpred_plot(ml) + 
  theme_bw()
dev.off()

# NO SH
#layout(matrix(1:2, ncol=2))
plot(ml)
plot(resid(ml))
layout(matrix(1:2, ncol=2))
acf(resid(ml), lag.max=36, main="ACF")
pacf(resid(ml), lag.max=36, main="pACF")
layout(1)

## predict trend contributions
pdat_lip <- pdat[pdat$date > min(lipid_gam$Date.fished) & pdat$date < max(lipid_gam$Date.fished),]
pdat_lip <- cbind(pdat_lip, predict(ml,  newdata =pdat_lip, se.fit = TRUE))
pdat <- left_join(pdat, pdat_lip)
pdat$lipid_fit <- arm::invlogit(pdat$fit)*100
pdat$lipid_UCI <- arm::invlogit(pdat$fit + 1.96*pdat$se.fit)*100
pdat$lipid_LCI <- arm::invlogit(pdat$fit - 1.96*pdat$se.fit)*100
pdat <- dplyr::select(pdat, -fit, -se.fit)

ggplot() + 
  geom_ribbon(data=pdat, aes(date, ymin=lipid_LCI, ymax=lipid_UCI), fill="grey") +
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_line(data=pdat, aes(date, lipid_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("lipid %") +
  theme_bw()

ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_ribbon(data=pdat[pdat$sh==100,], aes(date_adj, ymin=lipid_LCI, ymax=lipid_UCI), fill="grey") +
  geom_line(data=pdat[pdat$sh==100,], aes(date_adj, lipid_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("lipid %") +
  theme_bw()


### Protein % by month modelled using a GAM, show spline pattern 
pro_gam <- new_hyd[!is.na(new_hyd$Nx5.6),]
hist(pro_gam$Nx5.6)
summary(pro_gam$Nx5.6)
shapiro.test(pro_gam$Nx5.6) #normal enough...
qqnorm(pro_gam$Nx5.6, pch = 1, frame = FALSE)
qqline(pro_gam$Nx5.6, col = "steelblue", lwd = 2)

mp <- gam(formula = Nx5.6/100 ~ s(Time), family=betar(link="logit"), data=pro_gam, method = "ML")
summary(mp)
AIC(mp)
mp_sh <- gam(formula = Nx5.6/100 ~ s(sh) + s(Time), family=betar(link="logit"), data=pro_gam, method = "ML")
summary(mp_sh)
AIC(mp_sh)
#SH minimally important
tab_mod_sh <- rbind(tab_mod_sh, data.frame(type=rep("protein",2), 
                                           formula = c(deparse(formula(mp)), deparse(formula(mp_sh))),
                                           p.sh = c(NA, summary(mp_sh)$s.pv[1]),
                                           p.time = c(summary(mp)$s.pv[1], summary(mp_sh)$s.pv[2]),
                                           aic= c(AIC(mp), AIC(mp_sh)),
                                           rsq = c(summary(mp)$r.sq, summary(mp_sh)$r.sq),
                                           dev = c(summary(mp)$dev.expl, summary(mp_sh)$dev.expl)))

png("gam_pro_resid.png", height=5, width=6, res=400, units="in")
gratia::residuals_linpred_plot(mp) + 
  theme_bw()
dev.off()


#layout(matrix(1:2, ncol=2))
plot(mp)
plot(resid(mp))
layout(matrix(1:2, ncol=2))
acf(resid(mp), lag.max=36, main="ACF")
pacf(resid(mp), lag.max=36, main="pACF")
layout(1)

## predict trend contributions
pdat_pro <- pdat[pdat$date > min(pro_gam$Date.fished) & pdat$date < max(pro_gam$Date.fished),]
pdat_pro <- cbind(pdat_pro, predict(mp,  newdata = pdat_pro, se.fit = TRUE))
pdat <- left_join(pdat, pdat_pro)
pdat$protein_fit <- arm::invlogit(pdat$fit)*100
pdat$protein_UCI <- arm::invlogit(pdat$fit + 1.96*pdat$se.fit)*100
pdat$protein_LCI <- arm::invlogit(pdat$fit - 1.96*pdat$se.fit)*100
pdat <- dplyr::select(pdat, -fit, -se.fit)

ggplot() + 
  geom_ribbon(data=pdat, aes(date, ymin=protein_LCI, ymax=protein_UCI), fill="grey") +
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_line(data=pdat, aes(date, protein_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Protein %") +
  theme_bw()

ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_ribbon(data=pdat[pdat$sh==100,], aes(date_adj, ymin=protein_LCI, ymax=protein_UCI), fill="grey") +
  geom_line(data=pdat[pdat$sh==100,], aes(date_adj, protein_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Protein %") +
  theme_bw()

### Ash % by month was modelled using GAMs for each element, show spline pattern 
ash_gam <- new_hyd[!is.na(new_hyd$perc.ash),]
hist(ash_gam$ash.wet)
summary(ash_gam$ash.wet)
shapiro.test(ash_gam$ash.wet) #not normal...
qqnorm(ash_gam$ash.wet, pch = 1, frame = FALSE)
qqline(ash_gam$ash.wet, col = "steelblue", lwd = 2)

ma <- gam(formula = ash.wet ~ s(Time), family=betar(link="logit"), data=ash_gam, method = "ML")
summary(ma)
AIC(ma)
ma_sh <- gam(formula = ash.wet ~ s(sh) + s(Time), family=betar(link="logit"), data=ash_gam, method = "ML")
summary(ma_sh)
AIC(ma_sh)
# sh not sig or better
tab_mod_sh <- rbind(tab_mod_sh, data.frame(type=rep("ash",2), 
                                           formula = c(deparse(formula(ma)), deparse(formula(ma_sh))),
                                           p.sh = c(NA, summary(ma_sh)$s.pv[1]),
                                           p.time = c(summary(ma)$s.pv[1], summary(ma_sh)$s.pv[2]),
                                           aic= c(AIC(ma), AIC(ma_sh)),
                                           rsq = c(summary(ma)$r.sq, summary(ma_sh)$r.sq),
                                           dev = c(summary(ma)$dev.expl, summary(ma_sh)$dev.expl)))

png("gam_ash_resid.png", height=5, width=6, res=400, units="in")
gratia::residuals_linpred_plot(ma) + 
  theme_bw()
dev.off()

#layout(matrix(1:2, ncol=2))
plot(ma)
plot(resid(ma))
layout(matrix(1:2, ncol=2))
acf(resid(ma), lag.max=36, main="ACF")
pacf(resid(ma), lag.max=36, main="pACF")
layout(1)

## predict trend contributions
pdat_ash <- pdat[pdat$date > min(ash_gam$Date.fished) & pdat$date < max(ash_gam$Date.fished),]
pdat_ash <- cbind(pdat_ash, predict(ma,  newdata =pdat_ash, se.fit = TRUE))
pdat <- left_join(pdat, pdat_ash) 
pdat$ash_fit <- arm::invlogit(pdat$fit) *100
pdat$ash_UCI <- arm::invlogit(pdat$fit + 1.96*pdat$se.fit)*100
pdat$ash_LCI <- arm::invlogit(pdat$fit - 1.96*pdat$se.fit)*100
pdat <- dplyr::select(pdat, -fit, -se.fit)

ggplot() + 
  geom_ribbon(data=pdat, aes(date, ymin=ash_LCI, ymax=ash_UCI), fill="grey") +
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_line(data=pdat, aes(date, ash_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Ash %") +
  theme_bw()

ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_ribbon(data=pdat[pdat$sh==100,], aes(date_adj, ymin=ash_LCI, ymax=ash_UCI), fill="grey") +
  geom_line(data=pdat[pdat$sh==100,], aes(date_adj, ash_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Ash %") +
  theme_bw()

# Water % by month was modelled using GAMs, show spline pattern 
# Nutritional composition estimate for each month 
water_gam <- new_hyd[new_hyd$dmw < new_hyd$wmw & !is.na(new_hyd$dmw),]
hist(water_gam$water_meat)
shapiro.test(water_gam$water_meat) #not normal...
qqnorm(water_gam$water_meat, pch = 1, frame = FALSE)
qqline(water_gam$water_meat, col = "steelblue", lwd = 2)
mw <- gam(formula = water_meat ~ s(Time), family=betar(link="logit"), data=water_gam, method = "ML")
summary(mw)
AIC(mw)
mw_sh <- gam(formula = water_meat ~ s(sh) + s(Time), family=betar(link="logit"), data=water_gam, method = "ML")
summary(mw_sh)
AIC(mw_sh)
plot(mw_sh)
# very significant and better AIC
tab_mod_sh <- rbind(tab_mod_sh, data.frame(type=rep("water",2), 
                                           formula = c(deparse(formula(mw)), deparse(formula(mw_sh))),
                                           p.sh = c(NA, summary(mw_sh)$s.pv[1]),
                                           p.time = c(summary(mw)$s.pv[1], summary(mw_sh)$s.pv[2]),
                                           aic= c(AIC(mw), AIC(mw_sh)),
                                           rsq = c(summary(mw)$r.sq, summary(mw_sh)$r.sq),
                                           dev = c(summary(mw)$dev.expl, summary(mw_sh)$dev.expl)))
mw <- mw_sh

png("gam_water_resid.png", height=5, width=6, res=400, units="in")
gratia::residuals_linpred_plot(mw) + 
  theme_bw()
dev.off()

#layout(matrix(1:2, ncol=2))
plot(mw)
plot(resid(mw))
layout(matrix(1:2, ncol=2))
acf(resid(mw), lag.max=36, main="ACF")
pacf(resid(mw), lag.max=36, main="pACF")
layout(1)

## predict trend contributions
pdat_water <- pdat[pdat$date > min(water_gam$Date.fished) & pdat$date < max(water_gam$Date.fished),]
pdat_water <- cbind(pdat_water, predict(mw, newdata =pdat_water, se.fit = TRUE))
pdat <- left_join(pdat, pdat_water)
pdat$water_fit <- arm::invlogit(pdat$fit)*100
pdat$water_UCI <- arm::invlogit(pdat$fit + 1.96*pdat$se.fit)*100
pdat$water_LCI <- arm::invlogit(pdat$fit - 1.96*pdat$se.fit)*100
pdat <- dplyr::select(pdat, -fit, -se.fit)

png("water_seasonal.png", height=5, width=6, res=400, units="in")
ggplot() + 
  geom_rug(data=water_gam, aes(date))+
  geom_ribbon(data=pdat[pdat$sh==100,], aes(date, ymin=water_LCI, ymax=water_UCI), fill="grey") +
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_line(data=pdat[pdat$sh==100,], aes(date, water_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Water % (100 mm shell height)") +
  theme_bw() +
  theme(panel.grid=element_blank())
dev.off()

labs <- pdat[day(pdat$date) ==15,]
pick <- data.frame(date=unique(labs$date),sh=c(125,125,125,110,115,115,115,120,105,115,115,120))
labs <- left_join(pick, labs)
labs$month <- month(labs$date)
ggplot() + 
  #geom_point(data=new_hyd_gam, aes(sh, water_meat*100))+
  geom_line(data=pdat[day(pdat$date)==15,], aes(sh, water_fit, colour=date, group=date)) +	
  geom_label(data=labs, aes(sh, water_fit, label=month, colour=date))+
  xlab("Shell height") +
  scale_colour_date(name="Month")+
  ylab("Water %") +
  theme_bw()

ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_ribbon(data=pdat[pdat$sh==100,], aes(date_adj, ymin=water_LCI, ymax=water_UCI), fill="grey") +
  geom_line(data=pdat[pdat$sh==100,], aes(date_adj, water_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Water %") +
  theme_bw()

#carbohydrate estimates
pdat$carbohydrate_fit <- 100-pdat$ash_fit-pdat$water_fit-pdat$lipid_fit-pdat$protein_fit
pdat$carbohydrate_UCI <-100-pdat$ash_UCI-pdat$water_UCI-pdat$lipid_UCI-pdat$protein_UCI
pdat$carbohydrate_LCI <-100-pdat$ash_LCI-pdat$water_LCI-pdat$lipid_LCI-pdat$protein_LCI

#plot together
pdat_melt <- pdat %>%
  pivot_longer(cols=c("ash_fit", "protein_fit", "water_fit", "lipid_fit", "carbohydrate_fit")) %>%
  rename(component="name",
         fit="value") %>% 
  pivot_longer(cols=c("ash_UCI", "protein_UCI", "water_UCI", "lipid_UCI","carbohydrate_UCI")) %>%
  rename(component1="name",
         UCI="value") %>% 
  pivot_longer(cols=c("ash_LCI", "protein_LCI", "water_LCI", "lipid_LCI",
                      "carbohydrate_LCI")) %>%
  rename(component2="name",
         LCI="value")

pdat_melt$component <- gsub(x=pdat_melt$component, pattern="_fit", replacement="", fixed=T)
pdat_melt$component1 <- gsub(x=pdat_melt$component1, pattern="_UCI", replacement="", fixed=T)
pdat_melt$component2 <- gsub(x=pdat_melt$component2, pattern="_LCI", replacement="", fixed=T)
pdat_melt <- pdat_melt[pdat_melt$component==pdat_melt$component1,]
pdat_melt <- pdat_melt[pdat_melt$component1==pdat_melt$component2,]
pdat_melt <- dplyr::select(pdat_melt, -component1, -component2)

lab_dates <- expand.grid(date=unique(lipid_gam$date), component=c("lipid", "ash", "protein", "carbohydrate"), measured=T)
# lab_dates[lab_dates$component %in% c("lipid", "ash","carbohydrate","protein") & lab_dates$date=="2024-05-30",]$measured <- FALSE
dry_dates <- expand.grid(date=unique(new_hyd$date), component=c("water"), measured=T)
dates <- rbind(lab_dates, dry_dates)
pdat_melt <- left_join(pdat_melt, dates)
pdat_melt$measured[is.na(pdat_melt$measured)] <- "F"
cols <- c("water"="blue", "ash"="black", "protein"="forestgreen", "lipid"="orange", "carbohydrate"="red")

pdat_melt$component <- factor(x = pdat_melt$component, levels=c("water", "protein", "ash", "lipid", "carbohydrate"))

png("Percent_composition_gam.png", height=8, width=5, res=400, units="in")
ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  #geom_ribbon(data=pdat[pdat$sh==100,], aes(date_adj, ymin=ash_LCI, ymax=ash_UCI), fill="grey") +
  geom_ribbon(data=pdat_melt[pdat_melt$sh==100,], aes(date, ymin=LCI, ymax=UCI, colour=component), fill=NA, linetype="dashed", show.legend=F)+
  geom_line(data=pdat_melt[pdat_melt$sh==100,], aes(date, fit, colour=component), show.legend=F)+
  geom_rug(data=pdat_melt[pdat_melt$measured==T,], aes(date), length = unit(0.5,"cm"), show.legend=F) +
  scale_colour_manual(values=cols)+
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Mass percent (wet weight)") +
  facet_wrap(~component, scales="free_y", ncol=1)+
  theme_bw() +
  theme(panel.grid=element_blank())
dev.off()

panel_wet <- ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  #geom_ribbon(data=pdat[pdat$sh==100,], aes(date_adj, ymin=ash_LCI, ymax=ash_UCI), fill="grey") +
  geom_ribbon(data=pdat_melt[pdat_melt$sh==100,], aes(date, ymin=LCI, ymax=UCI, colour=component), fill=NA, linetype="dashed", show.legend=F)+
  geom_line(data=pdat_melt[pdat_melt$sh==100,], aes(date, fit, colour=component), show.legend=F)+
  geom_rug(data=pdat_melt[pdat_melt$measured==T,], aes(date), length = unit(0.5,"cm"), show.legend=F) +
  scale_colour_manual(values=cols)+
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Mass percent (wet weight)") +
  facet_wrap(~component, scales="free_y", ncol=1)+
  theme_bw() +
  theme(panel.grid=element_blank())

png("Percent_composition_gam_wide.png", height=2, width=14, res=400, units="in")
ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  #geom_ribbon(data=pdat[pdat$sh==100,], aes(date_adj, ymin=ash_LCI, ymax=ash_UCI), fill="grey") +
  geom_ribbon(data=pdat_melt[pdat_melt$sh==100,], aes(date, ymin=LCI, ymax=UCI, colour=component), fill=NA, linetype="dashed", show.legend=F)+
  geom_line(data=pdat_melt[pdat_melt$sh==100,], aes(date, fit, colour=component), show.legend=F)+
  geom_rug(data=pdat_melt[pdat_melt$measured==T,], aes(date), length = unit(0.5,"cm"), show.legend=F) +
  scale_colour_manual(values=cols)+
  scale_x_date(name="Date", breaks="3 months", date_labels = "%b") +
  ylab("Mass percent (wet weight)") +
  facet_wrap(~component, scales="free_y", nrow=1)+
  theme_bw() +
  theme(panel.grid=element_blank())
dev.off()

wmw_min <- min(pdat_melt[pdat_melt$sh==100,]$wmw_fit)
wmw_max <- max(pdat_melt[pdat_melt$sh==100,]$wmw_fit)
wmw_med <- median(pdat_melt[pdat_melt$sh==100,]$wmw_fit)
(wmw_max-wmw_med)/wmw_med
(wmw_med-wmw_min)/wmw_med

pdat_sum <- pdat_melt[pdat_melt$sh==100,] %>%
  group_by(component) %>%
  summarize(min=min(fit), 
            med=median(fit), 
            max=max(fit))

pdat_sum$diff <- (pdat_sum$max-pdat_sum$med)
pdat_sum$diff2 <- (pdat_sum$med-pdat_sum$min)

pdat_wet <- pdat

# what about by dry weight...
new_hyd_gam_dry <- new_hyd_gam[!is.na(new_hyd_gam$sh) & !is.na(new_hyd_gam$wmw) & !is.na(new_hyd_gam$dmw),]

mod.new.mwsh.dry <- lm(log(dmw) ~ log(sh)*month.fac, data = new_hyd[!is.na(new_hyd$sh),])
summary(mod.new.mwsh.dry)
anova(mod.new.mwsh.dry)
plot(mod.new.mwsh.dry$residuals)
unique(new_hyd$month.fac)

png("curves_dmw_resid.png", height=5, width=6, res=400, units="in")
ggplot(mod.new.mwsh.dry) + 
  geom_point(aes(.fitted, .resid))+
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("fitted") +
  ylab("residual")
dev.off()

month.fac <- sort(unique(new_hyd[!is.na(new_hyd$sh),]$month.fac))
month_tab <- NULL
for(i in 1:length(month.fac)){
  if(i==1) a=0; b=0
  if(!i==1){
    b <- coef(mod.new.mwsh.dry)[[paste0("log(sh):month.fac",month.fac[i])]]
    a <- coef(mod.new.mwsh.dry)[[month.fac[i]]]
  }
  b <- data.frame(month=month.fac[i], 
                  a=(coef(mod.new.mwsh.dry)[["(Intercept)"]] + a), 
                  b= (coef(mod.new.mwsh.dry)[["log(sh)"]] + b))
  month_tab <- rbind(month_tab, b)
  month_tab <- arrange(month_tab, as.numeric(month))
}
#log(wmw) = aL^b
#wmw = exp(bL+a) # IS THIS OK INSTEAD OF log(sh)? 
# h100 <- exp(month_tab$a + month_tab$b*100)
h100 <- exp(month_tab$a)*(100^month_tab$b)

month_tab$month.fac <- as.factor(month_tab$month)
month_tab$sh <- 100
month_tab <- cbind(month_tab, predict(mod.new.mwsh.dry, newdata=month_tab, se.fit=T))
month_tab$h100 <- h100
month_tab$pred <- exp(month_tab$fit)
month_tab$UCI <- exp(month_tab$fit + 1.96*month_tab$se.fit)
month_tab$LCI <- exp(month_tab$fit - 1.96*month_tab$se.fit)

curves <- expand.grid(month.fac=unique(month_tab$month.fac), sh=seq(floor(min(new_hyd_gam_dry$sh, na.rm=T)),ceiling(max(new_hyd_gam_dry$sh, na.rm=T)), 1))
curves <- cbind(curves, predict(mod.new.mwsh.dry, newdata=curves, se.fit=T))
curves$pred <- exp(curves$fit)
curves$UCI <- exp(curves$fit + 1.96*curves$se.fit)
curves$LCI <- exp(curves$fit - 1.96*curves$se.fit)

png("MWSH_curves_glm_dry.png", height=5, width=6, res=400, units="in")
ggplot() + 
  geom_point(data=new_hyd_gam_dry, aes(x=sh,y=dmw), size=0.1)+
  geom_line(data=curves, aes(x=sh, group=as.numeric(month.fac), colour=(month.fac),y=pred), linewidth=1)+
  geom_text_repel(data=unique(curves[curves$sh==max(curves$sh),c("sh", "month.fac", "pred")]), aes(x=max(curves$sh), group=as.numeric(month.fac), colour=(month.fac), y=pred, label=month.fac),segment.colour="darkgrey", nudge_x = 5)+
  ylab("Dry meat weight (g)") + 
  xlab("Shell height (mm)")+
  theme_classic()+ scale_colour_viridis_d(guide="none", end = 0.9) 
dev.off()

#convert to numeric time
new_hyd_gam_dry <- transform(new_hyd_gam_dry, Time = as.numeric(ymd(date))/1000)

m <- gam(formula = log(dmw) ~ s(sh) + s(Time), data=new_hyd_gam_dry, method="ML")
summary(m)
layout(matrix(1:2, ncol=2))
plot(m)
layout(matrix(1:2, ncol=2))
acf(resid(m), lag.max=36, main="ACF")
pacf(resid(m), lag.max=36, main="pACF")
layout(1)
plot(resid(m))

png("gam_dmw_resid.png", height=5, width=6, res=400, units="in")
gratia::residuals_linpred_plot(m) + 
  theme_bw()
dev.off()

## predict trend contributions
pdat_dry <- pdat[pdat$date > min(water_gam$Date.fished) & pdat$date < max(water_gam$Date.fished),]
pdat_dry <- cbind(pdat_dry, predict(m,  newdata =pdat_dry, se.fit = TRUE))
pdat <- left_join(pdat, pdat_dry)
pdat$dmw_fit <- exp(pdat$fit)
pdat$dmw_UCI <- exp(pdat$fit + 1.96*pdat$se.fit)
pdat$dmw_LCI <- exp(pdat$fit - 1.96*pdat$se.fit)
pdat <- dplyr::select(pdat, -fit, -se.fit)

ggplot() + 
  geom_ribbon(data=pdat[pdat$sh==100,], aes(as.Date(Time*1000), ymin=dmw_LCI, ymax=dmw_UCI), fill="grey") +
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_line(data=pdat[pdat$sh==100,], aes(as.Date(Time*1000), dmw_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("dry meat weight (g for 100 mm shell height)") +
  theme_bw()

min(pdat$date)
max(pdat$date)

pdat$date_adj <- pdat$date 
pdat$date_adj[year(pdat$date_adj)==2024] <- 
  pdat$date_adj[year(pdat$date_adj)==2024]+years(1)

pdat <- pdat[pdat$date<"2025-05-21",]

ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_ribbon(data=pdat[pdat$sh==100,], aes(date, ymin=dmw_LCI, ymax=dmw_UCI), fill="grey") +
  geom_line(data=pdat[pdat$sh==100,], aes(date, dmw_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("dry meat weight (g for 100 mm shell height)") +
  theme_bw()

samp_dates <- unique(data.frame(date=new_hyd_gam_dry$date[!is.na(new_hyd_gam_dry$sh)], month=as.numeric(as.character(month(new_hyd_gam_dry$date[!is.na(new_hyd_gam_dry$sh)])))))
#month_tab <- left_join(month_tab, samp_dates)
month_tab$date_adj <- ymd(month_tab$month)
month_tab$date_adj[year(month_tab$date_adj)==2024] <- month_tab$date_adj[year(month_tab$date_adj)==2024]+years(1)

new_hyd_gam_dry$date_adj <- new_hyd_gam_dry$date
new_hyd_gam_dry$date_adj[year(new_hyd_gam_dry$date_adj)==2024] <- new_hyd_gam_dry$date_adj[year(new_hyd_gam_dry$date_adj)==2024]+years(1)

png("MW_dry_gam_seasonal.png", height=5, width=6, res=400, units="in")
ggplot() + 
  geom_rug(data=new_hyd_gam_dry[!new_hyd_gam_dry$date %in% c("2024-07-16", "2025-01-30"),], aes(date))+
  geom_rug(data=new_hyd_gam_dry[new_hyd_gam_dry$date %in% c("2024-07-16", "2025-01-30"),], aes(date), colour="red")+
  geom_text(data=new_hyd_gam_dry[new_hyd_gam_dry$date %in% c("2024-07-16", "2025-01-30"),], aes(date_adj, y=-Inf, label="*"), vjust=-1.2, colour="red")+
  geom_ribbon(data=pdat[pdat$sh==100,], aes(date, ymin=dmw_LCI, ymax=dmw_UCI), fill="black", alpha=0.2) + 
  # geom_ribbon(data=month_tab, aes(date, ymin=LCI, ymax=UCI),fill="blue", alpha=0.2) +
  geom_line(data=pdat[pdat$sh==100,], aes(date, dmw_fit), colour="black") +
  # geom_line(data=month_tab, aes(date, pred), colour="blue") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Dry meat weight\n(g for 100 mm shell height)") +
  theme_bw() +
  theme(panel.grid=element_blank())
dev.off()

# Lipid % by month modelled using a GAM, show spline pattern 
hist(lipid_gam$lipid.dry)
shapiro.test(lipid_gam$lipid.dry) #not normal...
qqnorm(lipid_gam$lipid.dry, pch = 1, frame = FALSE)
qqline(lipid_gam$lipid.dry, col = "steelblue", lwd = 2)

#ml <- gam(formula = lipid.. ~ s(Time, bs = "cc"), family = Gamma(), data=lipid_gam)
mld <- gam(formula = lipid.dry ~ s(Time), family = betar(link="logit"), data=lipid_gam, method="ML")
summary(mld)
AIC(mld)
mld_sh <- gam(formula = lipid.dry ~ s(sh) + s(Time), family = betar(link="logit"), data=lipid_gam, method="ML")
summary(mld_sh)
AIC(mld_sh)
#no SH
#layout(matrix(1:2, ncol=2))
plot(mld)
layout(matrix(1:2, ncol=2))
acf(resid(mld), lag.max=36, main="ACF")
pacf(resid(mld), lag.max=36, main="pACF")
layout(1)
plot(resid(mld))

tab_mod_sh <- rbind(tab_mod_sh, data.frame(type=rep("lipid.dry",2), 
                                           formula = c(deparse(formula(mld)), deparse(formula(mld_sh))),
                                           p.sh = c(NA, summary(mld_sh)$s.pv[1]),
                                           p.time = c(summary(mld)$s.pv[1], summary(mld_sh)$s.pv[2]),
                                           aic= c(AIC(mld), AIC(mld_sh)),
                                           rsq = c(summary(mld)$r.sq, summary(mld_sh)$r.sq),
                                           dev = c(summary(mld)$dev.expl, summary(mld_sh)$dev.expl)))


png("gam_lipid_dry_resid.png", height=5, width=6, res=400, units="in")
gratia::residuals_linpred_plot(mld) + 
  theme_bw()
dev.off()

## predict trend contributions
pdat_lip <- dplyr::select(pdat_lip, -fit, -se.fit)
pdat_lip <- cbind(pdat_lip, predict(mld,  newdata =pdat_lip, se.fit = TRUE))
pdat <- left_join(pdat, pdat_lip)
pdat$lipidd_fit <- arm::invlogit(pdat$fit)*100
pdat$lipidd_UCI <- arm::invlogit(pdat$fit + 1.96*pdat$se.fit)*100
pdat$lipidd_LCI <- arm::invlogit(pdat$fit - 1.96*pdat$se.fit)*100
pdat <- dplyr::select(pdat, -fit, -se.fit)

ggplot() + 
  geom_ribbon(data=pdat, aes(date, ymin=lipidd_LCI, ymax=lipidd_UCI), fill="grey") +
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_line(data=pdat, aes(date, lipidd_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("lipid % (dry)") +
  theme_bw()

ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_ribbon(data=pdat[pdat$sh==100,], aes(date_adj, ymin=lipidd_LCI, ymax=lipidd_UCI), fill="grey") +
  geom_line(data=pdat[pdat$sh==100,], aes(date_adj, lipidd_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("lipid % (dry)") +
  theme_bw()

#Protein % by month modelled using a GAM, show spline pattern 
pro_gam$protein.dry <- pro_gam$protein.g.wet/(pro_gam$protein.g*(1-pro_gam$water_meat))
hist(pro_gam$protein.dry)
summary(pro_gam$protein.dry)
shapiro.test(pro_gam$protein.dry) #normal enough...
qqnorm(pro_gam$protein.dry, pch = 1, frame = FALSE)
qqline(pro_gam$protein.dry, col = "steelblue", lwd = 2)

mpd <- gam(formula = protein.dry ~ s(Time), family=betar(link="logit"), data=pro_gam, method = "ML")
summary(mpd)
AIC(mpd)
mpd_sh <- gam(formula = protein.dry ~ s(sh) + s(Time), family=betar(link="logit"), data=pro_gam, method = "ML")
summary(mpd_sh)
AIC(mpd_sh)
# no sh
#layout(matrix(1:2, ncol=2))
plot(mpd)
layout(matrix(1:2, ncol=2))
acf(resid(mpd), lag.max=36, main="ACF")
pacf(resid(mpd), lag.max=36, main="pACF")
layout(1)
plot(resid(mpd))

tab_mod_sh <- rbind(tab_mod_sh, data.frame(type=rep("protein.dry",2), 
                                           formula = c(deparse(formula(mpd)), deparse(formula(mpd_sh))),
                                           p.sh = c(NA, summary(mpd_sh)$s.pv[1]),
                                           p.time = c(summary(mpd)$s.pv[1], summary(mpd_sh)$s.pv[2]),
                                           aic= c(AIC(mpd), AIC(mpd_sh)),
                                           rsq = c(summary(mpd)$r.sq, summary(mpd_sh)$r.sq),
                                           dev = c(summary(mpd)$dev.expl, summary(mpd_sh)$dev.expl)))


png("gam_pro_dry_resid.png", height=5, width=6, res=400, units="in")
gratia::residuals_linpred_plot(mpd) + 
  theme_bw()
dev.off()

## predict trend contributions
pdat_pro <- dplyr::select(pdat_pro, -fit, -se.fit)
pdat_pro <- cbind(pdat_pro, predict(mpd,  newdata =pdat_pro, se.fit = TRUE))
pdat <- dplyr::left_join(pdat, pdat_pro)
pdat$proteind_fit <- arm::invlogit(pdat$fit)*100
pdat$proteind_UCI <- arm::invlogit(pdat$fit + 1.96*pdat$se.fit)*100
pdat$proteind_LCI <- arm::invlogit(pdat$fit - 1.96*pdat$se.fit)*100
pdat <- dplyr::select(pdat, -fit, -se.fit)

ggplot() + 
  geom_ribbon(data=pdat, aes(date, ymin=proteind_LCI, ymax=proteind_UCI), fill="grey") +
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_line(data=pdat, aes(date, proteind_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Protein % (dry)") +
  theme_bw()

ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_ribbon(data=pdat[pdat$sh==100,], aes(date_adj, ymin=proteind_LCI, ymax=proteind_UCI), fill="grey") +
  geom_line(data=pdat[pdat$sh==100,], aes(date_adj, proteind_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Protein % (dry)") +
  theme_bw()

#Ash % by month was modelled using GAMs for each element, show spline pattern 
ash_gam <- new_hyd[!is.na(new_hyd$perc.ash),]
hist(ash_gam$perc.ash)
summary(ash_gam$perc.ash)
shapiro.test(ash_gam$perc.ash) #not normal...
qqnorm(ash_gam$perc.ash, pch = 1, frame = FALSE)
qqline(ash_gam$perc.ash, col = "steelblue", lwd = 2)

mad <- gam(formula = perc.ash ~ s(Time), family=betar(link="logit"), data=ash_gam, method="ML")
summary(mad)
AIC(mad)
mad_sh <- gam(formula = perc.ash ~ s(sh) + s(Time), family=betar(link="logit"), data=ash_gam, method = "ML")
summary(mad_sh)
AIC(mad_sh)
# no sh
#layout(matrix(1:2, ncol=2))
plot(mad)
layout(matrix(1:2, ncol=2))
acf(resid(mad), lag.max=36, main="ACF")
pacf(resid(mad), lag.max=36, main="pACF")
layout(1)
plot(resid(mad))

tab_mod_sh <- rbind(tab_mod_sh, data.frame(type=rep("ash.dry",2), 
                                           formula = c(deparse(formula(mad)), deparse(formula(mad_sh))),
                                           p.sh = c(NA, summary(mad_sh)$s.pv[1]),
                                           p.time = c(summary(mad)$s.pv[1], summary(mad_sh)$s.pv[2]),
                                           aic= c(AIC(mad), AIC(mad_sh)),
                                           rsq = c(summary(mad)$r.sq, summary(mad_sh)$r.sq),
                                           dev = c(summary(mad)$dev.expl, summary(mad_sh)$dev.expl)))

png("gam_ash_dry_resid.png", height=5, width=6, res=400, units="in")
gratia::residuals_linpred_plot(mad) + 
  theme_bw()
dev.off()

## predict trend contributions
pdat_ash <- pdat[pdat$date > min(ash_gam$Date.fished) & pdat$date < max(ash_gam$Date.fished),]
pdat_ash <- cbind(pdat_ash, predict(mad,  newdata =pdat_ash, se.fit = TRUE))
pdat <- left_join(pdat, pdat_ash) 
pdat$ashd_fit <- arm::invlogit(pdat$fit) *100
pdat$ashd_UCI <- arm::invlogit(pdat$fit + 1.96*pdat$se.fit)*100
pdat$ashd_LCI <- arm::invlogit(pdat$fit - 1.96*pdat$se.fit)*100
pdat <- dplyr::select(pdat, -fit, -se.fit)

ggplot() + 
  geom_ribbon(data=pdat, aes(date, ymin=ashd_LCI, ymax=ashd_UCI), fill="grey") +
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_line(data=pdat, aes(date, ashd_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Ash % (dry)") +
  theme_bw()

ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_ribbon(data=pdat[pdat$sh==100,], aes(date_adj, ymin=ashd_LCI, ymax=ashd_UCI), fill="grey") +
  geom_line(data=pdat[pdat$sh==100,], aes(date_adj, ashd_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Ash % (dry)") +
  theme_bw()

#carbohydrate estimates
pdat$carbohydrated_fit <- 100-pdat$ashd_fit-pdat$lipidd_fit-pdat$proteind_fit
pdat$carbohydrated_UCI <-100-pdat$ashd_UCI-pdat$lipidd_UCI-pdat$proteind_UCI
pdat$carbohydrated_LCI <-100-pdat$ashd_LCI-pdat$lipidd_LCI-pdat$proteind_LCI

#plot together
pdat_melt_d <- pdat %>%
  pivot_longer(cols=c("ashd_fit", "proteind_fit", "lipidd_fit", "carbohydrated_fit")) %>%
  rename(component="name",
         fit="value") %>% 
  pivot_longer(cols=c("ashd_UCI", "proteind_UCI", "lipidd_UCI","carbohydrated_UCI")) %>%
  rename(component1="name",
         UCI="value") %>% 
  pivot_longer(cols=c("ashd_LCI", "proteind_LCI", "lipidd_LCI",
                      "carbohydrated_LCI")) %>%
  rename(component2="name",
         LCI="value")

pdat_melt_d <- dplyr::select(pdat_melt_d, sh, date, Time, date_adj, component, component1, component2, fit, UCI, LCI)

pdat_melt_d$component <- gsub(x=pdat_melt_d$component, pattern="_fit", replacement="", fixed=T)
pdat_melt_d$component1 <- gsub(x=pdat_melt_d$component1, pattern="_UCI", replacement="", fixed=T)
pdat_melt_d$component2 <- gsub(x=pdat_melt_d$component2, pattern="_LCI", replacement="", fixed=T)
pdat_melt_d <- pdat_melt_d[pdat_melt_d$component==pdat_melt_d$component1,]
pdat_melt_d <- pdat_melt_d[pdat_melt_d$component1==pdat_melt_d$component2,]
pdat_melt_d <- dplyr::select(pdat_melt_d, -component1, -component2)

lab_dates <- expand.grid(date=unique(lipid_gam$date), component=c("lipid", "ash", "protein", "carbohydrate"), measured=T)
# lab_dates[lab_dates$component %in% c("ash","carbohydrate", "protein") & lab_dates$date=="2024-05-30",]$measured <- FALSE
dry_dates <- expand.grid(date=unique(new_hyd_gam$date), component=c("water"), measured=T)
dates <- rbind(lab_dates, dry_dates)
dates$component <- paste0(dates$component, "d")
pdat_melt_d <- left_join(pdat_melt_d, dates)
pdat_melt_d$measured[is.na(pdat_melt_d$measured)] <- "F"
cols <- c("water"="blue", "ash"="black", "protein"="forestgreen", "lipid"="orange", "carbohydrate"="red")

pdat_melt_d$component <- gsub(x = pdat_melt_d$component, pattern="lipidd", "lipid")
pdat_melt_d$component <- gsub(x = pdat_melt_d$component, pattern="ashd", "ash")
pdat_melt_d$component <- gsub(x = pdat_melt_d$component, pattern="proteind", "protein")
pdat_melt_d$component <- gsub(x = pdat_melt_d$component, pattern="carbohydrated", "carbohydrate")

pdat_melt_d$component <- factor(pdat_melt_d$component, levels=c("protein", "ash", "lipid", "carbohydrate"))

png("Percent_composition_dry_gam.png", height=6, width=5, res=400, units="in")
ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  #geom_ribbon(data=pdat[pdat$sh==100,], aes(date_adj, ymin=ash_LCI, ymax=ash_UCI), fill="grey") +
  geom_ribbon(data=pdat_melt_d[pdat_melt_d$sh==100,], aes(date, ymin=LCI, ymax=UCI, colour=component), fill=NA, linetype="dashed", show.legend=F)+
  geom_line(data=pdat_melt_d[pdat_melt_d$sh==100,], aes(date, fit, colour=component), show.legend=F)+
  geom_rug(data=pdat_melt_d[pdat_melt_d$measured==T,], aes(date), length = unit(0.5,"cm"), show.legend=F) +
  scale_colour_manual(values=cols)+
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Mass percent (dry weight)") +
  facet_wrap(~component, scales="free_y", ncol=1)+
  theme_bw() +
  theme(panel.grid=element_blank())
dev.off()

panel_dry <- ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  #geom_ribbon(data=pdat[pdat$sh==100,], aes(date_adj, ymin=ash_LCI, ymax=ash_UCI), fill="grey") +
  geom_ribbon(data=pdat_melt_d[pdat_melt_d$sh==100,], aes(date, ymin=LCI, ymax=UCI, colour=component), fill=NA, linetype="dashed", show.legend=F)+
  geom_line(data=pdat_melt_d[pdat_melt_d$sh==100,], aes(date, fit, colour=component), show.legend=F)+
  geom_rug(data=pdat_melt_d[pdat_melt_d$measured==T,], aes(date), length = unit(0.5,"cm"), show.legend=F) +
  scale_colour_manual(values=cols)+
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Mass percent (dry weight)") +
  facet_wrap(~component, scales="free_y", ncol=1)+
  theme_bw() +
  theme(panel.grid=element_blank())


png("Percent_composition_gam_dry_wide.png", height=2, width=11, res=400, units="in")
ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  #geom_ribbon(data=pdat[pdat$sh==100,], aes(date_adj, ymin=ash_LCI, ymax=ash_UCI), fill="grey") +
  geom_ribbon(data=pdat_melt_d[pdat_melt_d$sh==100,], aes(date, ymin=LCI, ymax=UCI, colour=component), fill=NA, linetype="dashed", show.legend=F)+
  geom_line(data=pdat_melt_d[pdat_melt_d$sh==100,], aes(date, fit, colour=component), show.legend=F)+
  geom_rug(data=pdat_melt_d[pdat_melt_d$measured==T,], aes(date), length = unit(0.5,"cm"), show.legend=F) +
  scale_colour_manual(values=cols)+
  scale_x_date(name="Date", breaks="3 months", date_labels = "%b") +
  ylab("Mass percent (dry weight)") +
  facet_wrap(~component, scales="free_y", nrow=1)+
  theme_bw() +
  theme(panel.grid=element_blank())
dev.off()

layout <- "
A#
A#
AB
AB
AB
AB
AB
AB
AB
AB
AB
AB
"
png("Percent_composition_gam_both.png", height=8, width=8, res=400, units="in")
panel_wet + panel_dry + plot_layout(design=layout)
dev.off()

#################### GONADS ####################
gonad <- read.csv("gonad.csv")
gonad$sh[!is.na(gonad$sh2)] <- gonad$sh2[!is.na(gonad$sh2)]
unique(gonad$Date.fished)
shapiro.test(gonad$maxwt) # not normal
hist(gonad$maxwt) # right skewed
qqnorm((gonad$maxwt), pch = 1, frame = FALSE)
qqline((gonad$maxwt), col = "steelblue", lwd = 2)

shapiro.test(log(gonad$maxwt)) # not normal
hist(log(gonad$maxwt)) # left skewed
qqnorm(log(gonad$maxwt), pch = 1, frame = FALSE)
qqline(log(gonad$maxwt), col = "steelblue", lwd = 2)

gonad$month.fac <- as.factor(gonad$Date.fished)
gonad$sh <- as.numeric(gonad$sh)

# model time
mod.new.gwsh <- lm(log(maxwt) ~ log(sh)*month.fac, data = gonad[!is.na(gonad$sh),])
summary(mod.new.gwsh)
anova(mod.new.gwsh)
plot(mod.new.gwsh$residuals)
unique(gonad$month.fac)

png("curves_wgw_resid.png", height=5, width=6, res=400, units="in")
ggplot() +
  geom_point(data=mod.new.gwsh, aes(x = .fitted, y = .resid)) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("fitted") +
  ylab("residual")
dev.off()

month.fac <- sort(unique(as.factor(gonad[!is.na(gonad$sh),]$Date.fished)))
month_tab <- NULL
for(i in 1:length(month.fac)){
  if(i==1) a=0; b=0
  if(!i==1){
    b <- coef(mod.new.gwsh)[[paste0("log(sh):month.fac",month.fac[i])]]
    a <- coef(mod.new.gwsh)[[month.fac[i]]]
  }
  b <- data.frame(month=month.fac[i], 
                  a=(coef(mod.new.gwsh)[["(Intercept)"]] + a), 
                  b= (coef(mod.new.gwsh)[["log(sh)"]] + b))
  month_tab <- rbind(month_tab, b)
  month_tab <- arrange(month_tab, as.numeric(month))
}
#log(wmw) = aL^b
#wmw = exp(bL+a) # IS THIS OK INSTEAD OF log(sh)? 
# h100 <- exp(month_tab$a + month_tab$b*100)
h100 <- exp(month_tab$a)*(100^month_tab$b)

month_tab$month.fac <- as.factor(month_tab$month)
month_tab$sh <- 100
month_tab <- cbind(month_tab, predict(mod.new.gwsh, newdata=month_tab, se.fit=T))
month_tab$h100 <- h100
month_tab$pred <- exp(month_tab$fit)
month_tab$UCI <- exp(month_tab$fit + 1.96*month_tab$se.fit)
month_tab$LCI <- exp(month_tab$fit - 1.96*month_tab$se.fit)

curves <- expand.grid(month.fac=unique(month_tab$month.fac), sh=seq(floor(min(gonad$sh, na.rm=T)),
                                                                    ceiling(max(gonad$sh, na.rm=T)), 1))
curves <- cbind(curves, predict(mod.new.gwsh, newdata=curves, se.fit=T))
curves$pred <- exp(curves$fit)
curves$UCI <- exp(curves$fit + 1.96*curves$se.fit)
curves$LCI <- exp(curves$fit - 1.96*curves$se.fit)

png("MWSH_curves_glm_wetgonad.png", height=5, width=6, res=400, units="in")
ggplot() + 
  geom_point(data=gonad, aes(x=sh,y=maxwt), size=0.1)+
  geom_line(data=curves, aes(x=sh, group=as.numeric(month.fac), colour=(month.fac),y=pred), linewidth=1)+
  geom_text_repel(data=unique(curves[curves$sh==max(curves$sh),c("sh", "month.fac", "pred")]), aes(x=max(curves$sh), group=as.numeric(month.fac), colour=(month.fac), y=pred, label=month.fac),segment.colour="darkgrey", nudge_x = 5)+
  ylab("Wet gonad weight (g)") + 
  xlab("Shell height (mm)")+
  theme_classic()+ scale_colour_viridis_d(guide="none", end = 0.9) 
dev.off()

gonad$date <- as.character(gonad$Date.fished)
gonad_gam <- gonad
gonad_gam <- gonad_gam[!is.na(gonad_gam$sh) & !is.na(gonad_gam$maxwt),]
#convert to numeric time
gonad_gam <- transform(gonad_gam, Time = as.numeric(ymd(date))/1000)
gonad_gam$bin <- NA
gonad_gam$bin[gonad_gam$sh<115] <- "small"
gonad_gam$bin[gonad_gam$sh>=115] <- "large"
gonad_gam$bin <- as.factor(gonad_gam$bin)
m <- gam(formula = log(maxwt) ~ s(sh) + s(Time), data=gonad_gam, method="ML")
summary(m)
layout(matrix(1:2, ncol=2))
plot(m)
layout(matrix(1:2, ncol=2))
acf(resid(m), lag.max=36, main="ACF")
pacf(resid(m), lag.max=36, main="pACF")
layout(1)

png("gam_wgw_resid.png", height=5, width=6, res=400, units="in")
gratia::residuals_linpred_plot(m) + 
  theme_bw()
dev.off()

sm <- smooth_estimates(m) |>
  add_confint()
eg1 <- gonad_gam |>
  add_partial_residuals(m)

s1 <- sm |>
  filter(.smooth == "s(Time)") |>
  ggplot() +
  geom_rug(aes(x = Time),
           data = eg1,
           sides = "b", length = grid::unit(0.02, "npc")
  ) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Time),
              alpha = 0.2
  ) +
  geom_line(aes(x = Time, y = .estimate)) +
  labs(y = "Partial effect", title = "s(Time)") + scale_x_date(date_labels="%b") + 
  theme_bw() + 
  xlab("Day of year")

s2 <- draw(m, select="s(sh)") + theme_bw() + xlab("sh") + ylim(-2.5, 1.5)

s1 + s2 + plot_layout(ncol=2) 

pdat <- expand.grid(sh=90:140,
date=seq(min(ymd(gonad_gam$date[!is.na(gonad_gam$sh)])), 
         max(ymd(gonad_gam$date[!is.na(gonad_gam$sh)])), 1))
pdat$Time <- as.numeric(pdat$date)/1000
## predict trend contributions
pdat <- cbind(pdat, predict(m,  newdata = pdat, type="response", se.fit = TRUE))
pdat$wgw_fit <- exp(pdat$fit)
pdat$wgw_UCI <- exp(pdat$fit + 1.96*pdat$se.fit)
pdat$wgw_LCI <- exp(pdat$fit - 1.96*pdat$se.fit)
pdat <- dplyr::select(pdat, -fit, -se.fit)

ggplot() + 
  geom_ribbon(data=pdat[pdat$sh==100,], aes(as.Date(Time*1000), ymin=wgw_LCI, ymax=wgw_UCI), fill="grey") +
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_line(data=pdat[pdat$sh==100,], aes(as.Date(Time*1000), wgw_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b %Y") +
  ylab("wet gonad weight\n(g for 100 mm shell height)") +
  theme_bw()

pdat$date_adj <- pdat$date 
pdat$date_adj[year(pdat$date_adj)==2024] <- 
  pdat$date_adj[year(pdat$date_adj)==2024]+years(1)

pdat <- pdat[pdat$date<"2025-05-21",]

ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_ribbon(data=pdat[pdat$sh==100,], aes(date, ymin=wgw_LCI, ymax=wgw_UCI), fill="grey") +
  geom_line(data=pdat[pdat$sh==100,], aes(date, wgw_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("wet meat weight (g for 100 mm shell height)") +
  theme_bw()

samp_dates <- unique(data.frame(date=gonad$date[!is.na(gonad$sh)], month=as.numeric(as.character(month(gonad$date[!is.na(gonad$sh)])))))
samp_dates$month <- samp_dates$date
month_tab <- left_join(month_tab, samp_dates)
month_tab$date_adj <- ymd(month_tab$month)
month_tab$date_adj[year(month_tab$date_adj)==2024] <- ymd(month_tab$date_adj[year(month_tab$date_adj)==2024])+years(1)

gonad_gam$date_adj <- ymd(gonad_gam$date)
gonad_gam$date_adj[year(gonad_gam$date_adj)==2024] <- gonad_gam$date_adj[year(gonad_gam$date_adj)==2024]+years(1)

gonad_gam$date <- ymd(gonad_gam$date)
gonad_gam$date_adj <- ymd(gonad_gam$date_adj)

png("GW_gam_seasonal.png", height=5, width=6, res=400, units="in")
ggplot() + 
  # geom_point(data=gonad[gonad$sh >80 & gonad$sh < 120,], aes(ymd(Date.fished), maxwt))+
  geom_rug(data=gonad_gam[!gonad_gam$date %in% c("2024-07-16", "2025-01-30", "2025-07-23"),], aes(date))+
  geom_rug(data=gonad_gam[gonad_gam$date %in% c("2024-07-16", "2025-01-30"),], aes(date), colour="red")+
  geom_text(data=gonad_gam[gonad_gam$date %in% c("2024-07-16", "2025-01-30"),], aes(date_adj, y=-Inf, label="*"), vjust=-1.2, colour="red")+
  geom_ribbon(data=pdat[pdat$sh==100,], aes(date, ymin=wgw_LCI, ymax=wgw_UCI), fill="black", alpha=0.2) + 
  # geom_ribbon(data=month_tab, aes(date, ymin=LCI, ymax=UCI),fill="blue", alpha=0.2) +
  geom_line(data=pdat[pdat$sh==100,], aes(date, wgw_fit), colour="black") +
  # geom_line(data=month_tab, aes(date, pred), colour="blue") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Wet gonad weight\n(g for 100 mm shell height)") +
  theme_bw() +
  theme(panel.grid=element_blank())
dev.off()

# dry weights!
gonad_dry <- gonad[gonad$minwt < gonad$maxwt & !is.na(gonad$minwt),]
mod.new.dgwsh <- glm(log(minwt) ~ log(sh)*month.fac, data = gonad_dry[!is.na(gonad_dry$sh),])
summary(mod.new.dgwsh)
anova(mod.new.dgwsh)
plot(mod.new.dgwsh$residuals)
unique(gonad$month.fac)

png("curves_dgw_resid.png", height=5, width=6, res=400, units="in")
ggplot() +
  geom_point(data=mod.new.dgwsh, aes(x = .fitted, y = .resid)) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("fitted") +
  ylab("residual")
dev.off()

month.fac <- sort(unique(as.factor(gonad_dry[!is.na(gonad_dry$sh),]$Date.fished)))
month_tab <- NULL
for(i in 1:length(month.fac)){
  if(i==1) a=0; b=0
  if(!i==1){
    b <- coef(mod.new.dgwsh)[[paste0("log(sh):month.fac",month.fac[i])]]
    a <- coef(mod.new.dgwsh)[[month.fac[i]]]
  }
  b <- data.frame(month=month.fac[i], 
                  a=(coef(mod.new.dgwsh)[["(Intercept)"]] + a), 
                  b= (coef(mod.new.dgwsh)[["log(sh)"]] + b))
  month_tab <- rbind(month_tab, b)
  month_tab <- arrange(month_tab, as.numeric(month))
}
#log(wmw) = aL^b
#wmw = exp(bL+a) # IS THIS OK INSTEAD OF log(sh)? 
# h100 <- exp(month_tab$a + month_tab$b*100)
h100 <- exp(month_tab$a)*(100^month_tab$b)

month_tab$month.fac <- as.factor(month_tab$month)
month_tab$sh <- 100
month_tab <- cbind(month_tab, predict(mod.new.dgwsh, newdata=month_tab, se.fit=T))
month_tab$h100 <- h100
month_tab$pred <- exp(month_tab$fit)
month_tab$UCI <- exp(month_tab$fit + 1.96*month_tab$se.fit)
month_tab$LCI <- exp(month_tab$fit - 1.96*month_tab$se.fit)

curves <- expand.grid(month.fac=unique(month_tab$month.fac), sh=seq(floor(min(gonad_dry$sh, na.rm=T)),
                                                                    ceiling(max(gonad_dry$sh, na.rm=T)), 1))
curves <- cbind(curves, predict(mod.new.dgwsh, newdata=curves, se.fit=T))
curves$pred <- exp(curves$fit)
curves$UCI <- exp(curves$fit + 1.96*curves$se.fit)
curves$LCI <- exp(curves$fit - 1.96*curves$se.fit)

png("MWSH_curves_glm_drygonad.png", height=5, width=6, res=400, units="in")
ggplot() + 
  geom_point(data=gonad_dry, aes(x=sh,y=minwt), size=0.1)+
  geom_line(data=curves, aes(x=sh, group=as.numeric(month.fac), colour=(month.fac),y=pred), linewidth=1)+
  geom_text_repel(data=unique(curves[curves$sh==max(curves$sh),c("sh", "month.fac", "pred")]), aes(x=max(curves$sh), group=as.numeric(month.fac), colour=(month.fac), y=pred, label=month.fac),segment.colour="darkgrey", nudge_x = 5)+
  ylab("Dry gonad weight (g)") + 
  xlab("Shell height (mm)")+
  theme_classic()+ scale_colour_viridis_d(guide="none", end = 0.9) 
dev.off()

ggplot() + 
  geom_point(data=gonad_dry, aes(x=month.fac,y=minwt), size=0.1)+
  geom_line(data=curves[curves$sh==100,], aes(x=as.numeric(month.fac), y=pred), linewidth=1)+
  geom_line(data=curves[curves$sh==100,], aes(x=as.numeric(month.fac), y=UCI), linewidth=1, linetype="dashed")+
  geom_line(data=curves[curves$sh==100,], aes(x=as.numeric(month.fac), y=LCI), linewidth=1, linetype="dashed")+
  ylab("Wet gonad weight (g) for 100mm") + 
  xlab("Month")+
  theme_classic()+ scale_colour_viridis_d(guide="none", end = 0.9) 

# gonad_dry$date <- as.character(gonad_dry$Date.fished)
# temp <- gonad[gonad$date=="2024-05-30",]
# temp$date <- "2025-05-30"
# gonad_gam <- rbind(gonad_dry, temp)
#gonad_gam <- gonad_gam[!is.na(gonad_gam$sh) & !is.na(gonad_gam$minwt),]
gonad_gam <- gonad_dry[!is.na(gonad_dry$sh) & !is.na(gonad_dry$minwt),]

#convert to numeric time
gonad_gam <- transform(gonad_gam, Time = as.numeric(ymd(date))/1000)
m <- gam(formula = log(minwt) ~ s(sh) + s(Time), data=gonad_gam, method="ML")
summary(m)
layout(matrix(1:2, ncol=2))
plot(m)
layout(matrix(1:2, ncol=2))
acf(resid(m), lag.max=36, main="ACF")
pacf(resid(m), lag.max=36, main="pACF")
layout(1)

png("gam_dgw_resid.png", height=5, width=6, res=400, units="in")
gratia::residuals_linpred_plot(m) + 
  theme_bw()
dev.off()

pdat <- expand.grid(sh=90:140, 
                    date=seq(min(ymd(gonad_gam$date[!is.na(gonad_gam$sh)])), 
                             max(ymd(gonad_gam$date[!is.na(gonad_gam$sh)]))-days(1), 1))
pdat$Time <- as.numeric(pdat$date)/1000
## predict trend contributions
pdat <- cbind(pdat, predict(m,  newdata = pdat, type="response", se.fit = TRUE))
pdat$wgw_fit <- exp(pdat$fit)
pdat$wgw_UCI <- exp(pdat$fit + 1.96*pdat$se.fit)
pdat$wgw_LCI <- exp(pdat$fit - 1.96*pdat$se.fit)
pdat <- dplyr::select(pdat, -fit, -se.fit)

ggplot() + 
  geom_ribbon(data=pdat[pdat$sh==100,], aes(as.Date(Time*1000), ymin=wgw_LCI, ymax=wgw_UCI), fill="grey") +
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_line(data=pdat[pdat$sh==100,], aes(as.Date(Time*1000), wgw_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b %Y") +
  ylab("Dry gonad weight\n(g for 100 mm shell height)") +
  theme_bw()

min(pdat$date)
max(pdat$date)

pdat$date_adj <- pdat$date 
pdat$date_adj[year(pdat$date_adj)==2024] <- 
  pdat$date_adj[year(pdat$date_adj)==2024]+years(1)

pdat <- pdat[pdat$date<"2025-05-21",]

ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  geom_ribbon(data=pdat[pdat$sh==100,], aes(date, ymin=wgw_LCI, ymax=wgw_UCI), fill="grey") +
  geom_line(data=pdat[pdat$sh==100,], aes(date, wgw_fit), colour="black") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Dry meat weight (g for 100 mm shell height)") +
  theme_bw()

# samp_dates <- unique(data.frame(date=gonad_dry$date[!is.na(gonad_dry$sh)], month=as.numeric(as.character(month(gonad_dry$date[!is.na(gonad_dry$sh)])))))
# month_tab <- left_join(month_tab, samp_dates)
month_tab$date_adj <- ymd(month_tab$month.fac)
month_tab$date_adj[year(month_tab$date_adj)==2024] <- ymd(month_tab$date_adj[year(month_tab$date_adj)==2024])+years(1)

gonad_gam$date_adj <- ymd(gonad_gam$date)
gonad_gam$date_adj[year(gonad_gam$date_adj)==2024] <- gonad_gam$date_adj[year(gonad_gam$date_adj)==2024]+years(1)

gonad_gam$date <- ymd(gonad_gam$date)
gonad_gam$date_adj <- ymd(gonad_gam$date_adj)


png("GW_dry_seasonal.png", height=5, width=6, res=400, units="in")
ggplot() + 
  # geom_point(data=gonad[gonad$sh >80 & gonad$sh < 120,], aes(ymd(Date.fished), maxwt))+
  geom_rug(data=gonad_gam[!gonad_gam$date %in% c("2024-07-16", "2025-01-30", "2025-07-23"),], aes(date))+
  geom_rug(data=gonad_gam[gonad_gam$date %in% c("2024-07-16", "2025-01-30"),], aes(date), colour="red")+
  geom_text(data=gonad_gam[gonad_gam$date %in% c("2024-07-16", "2025-01-30"),], aes(date_adj, y=-Inf, label="*"), vjust=-1.2, colour="red")+
  geom_ribbon(data=pdat[pdat$sh==100,], aes(date, ymin=wgw_LCI, ymax=wgw_UCI), fill="black", alpha=0.2) + 
  # geom_ribbon(data=month_tab, aes(date, ymin=LCI, ymax=UCI),fill="blue", alpha=0.2) +
  geom_line(data=pdat[pdat$sh==100,], aes(date, wgw_fit), colour="black") +
  # geom_line(data=month_tab, aes(date, pred), colour="blue") +
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab("Dry gonad weight\n(g for 100 mm shell height)") +
  theme_bw() +
  theme(panel.grid=element_blank())
dev.off()

#################### MINERALS ##########################
mineral <- read.csv("ICP_combined.csv")

# mg/kg
mineral$Se <- as.numeric(mineral$Se)
mineral$Zn <- as.numeric(mineral$Zn)
mineral<- mineral %>%
  pivot_longer(!sample, names_to="name", values_to="mg.kg")

totals <- mineral %>%
  group_by(sample) %>%
  summarize(measured=sum(mg.kg, na.rm=T))
totals$unmeasured <- 1000000-totals$measured

totals2 <- mineral %>%
  filter(!name=="Al")%>%
  group_by(sample) %>%
  summarize(desired=sum(mg.kg, na.rm=T))


totals <- left_join(totals, totals2)
totals$new <- totals$unmeasured + totals$desired
# 1000000 mg in a kg

mineral <- left_join(mineral, totals)
mineral <- mineral %>% filter(!name=="Al")
mineral$mg.kg <- mineral$new*mineral$mg.kg/1000000

mineral$mg.kg <- as.numeric(mineral$mg.kg)

mineral[!mineral$sample %in% c("QA/QC SY-4","QA/QC Till-2","QA/QC CCU-1d","QA/QC CZN-4","QA/QC CPB-2", "QA/QC SY-4-2", "QA/QC Till-2-2", "QA/QC CCU-1d-2", "QA/QC CZN-4-2", "QA/QC CPB-2-2"),] %>% 
  group_by(name) %>%
  summarize(avg_mg_kg=mean(mg.kg, na.rm=T))

mineral[mineral$name=="Se",]

#join with full sample
ash <- new_hyd[, c("ID", "Date.fished", "perc.ash", "ash.wet", "ww_meat")]
ash$sample <- as.character(ash$ID)
mineral <- left_join(mineral, ash)
#mg/kg ash

# what about mg/kg wet weight
mineral[!is.na(mineral$ID) & mineral$name=="Mg",]
mineral$mg.kg.wet <- mineral$mg.kg * mineral$ash.wet

ggplot() + geom_boxplot(data=mineral[!mineral$name == "Se" & !mineral$sample %in% c("QA/QC SY-4","QA/QC Till-2","QA/QC CCU-1d","QA/QC CZN-4","QA/QC CPB-2"),], 
                        aes(ymd(Date.fished), mg.kg, group=ymd(Date.fished)),outliers=F) + facet_wrap(~name, scale="free_y")

ggplot() + geom_boxplot(data=mineral[!mineral$name == "Se" & !mineral$sample %in% c("QA/QC SY-4","QA/QC Till-2","QA/QC CCU-1d","QA/QC CZN-4","QA/QC CPB-2"),], 
                        aes(ymd(Date.fished), mg.kg.wet, group=ymd(Date.fished)),outliers=F) + facet_wrap(~name, scale="free_y")

usda <- read.csv("USDA_FoodDataCentral_Scallop2025.csv")
head(usda)
usda <- usda %>% 
  dplyr::select(-Unit, -Technique, -Method) %>%
  pivot_wider(names_from = Name, values_from = Amount.100g) %>%
  rename(ash.g=Ash)

mins <- names(usda)
mins <- mins[c(grep(x=mins, "Ca"), grep(x=mins, "Cu"), grep(x=mins, "Fe"), grep(x=mins, "Mg"), grep(x=mins, "Se"), grep(x=mins, "Zn"))]

usda_mins <- usda %>%
  pivot_longer(cols = mins) %>%
  dplyr::select(Sample, ash.g, name, value) %>%
  mutate(value.mg = as.numeric(value)) %>%
  mutate(ash.kg = as.numeric(ash.g)/1000) %>%
  mutate(mg.kg = value.mg/ash.kg)

usda_mins$name <- stringr::str_sub(usda_mins$name, start = -2)

ggplot() + 
  geom_boxplot(data=mineral[!mineral$name == "Se" & !mineral$sample %in% c("QA/QC SY-4","QA/QC Till-2","QA/QC CCU-1d","QA/QC CZN-4","QA/QC CPB-2"),],
               aes(x="This work", mg.kg), outliers=F) +
  # geom_point(data=mineral[!mineral$name == "Se" & !mineral$sample %in% c("QA/QC SY-4","QA/QC Till-2","QA/QC CCU-1d","QA/QC CZN-4","QA/QC CPB-2"),],
  #            aes(x="mine", mg.kg), colour="red") +
  geom_boxplot(data=usda_mins[!usda_mins$name=="Se",], aes(x="USDA", mg.kg),  outliers=F) +
  #geom_point(data=usda_mins, aes(x="usda", mg.kg), colour="blue") +
  facet_wrap(~name, scale="free") +
  ylab(expression(paste("Concentration (mg  ", kg^-1, ")"))) +
  xlab(NULL)+
  theme_bw()

# gam
temp <- mineral
temp$Time <- as.numeric(ymd(temp$Date.fished))/1000
gam_min <- function(x){
  x$mg.mg <- x$mg.kg/1000000
  if(median(x$mg.mg, na.rm=T)<0.001) adj<-100
  if(!median(x$mg.mg, na.rm=T)<0.001) adj <- 1
  mf <- gam(formula = mg.mg*adj ~ s(Time), data=x, family=tw(link="log"), method="ML")
  print(summary(mf))
  #layout(matrix(1:2, ncol=2))
  #plot(mf)
  #layout(matrix(1:2, ncol=2))
  #acf(resid(mf), lag.max=36, main="ACF")
  #pacf(resid(mf), lag.max=36, main="pACF")
  #layout(1)
  
  ## predict trend contributions
  pdat <- expand.grid(date=seq(min(ymd(x$Date.fished), na.rm=T), max(ymd(x$Date.fished), na.rm=T)-days(1), 1))
  pdat$Time <- as.numeric(ymd(pdat$date))/1000
  pdat <- cbind(pdat, predict(mf,  newdata = pdat, se.fit = TRUE))
  pdat$min_fit <- exp(pdat$fit)*1000000/adj
  pdat$min_UCI <- exp(pdat$fit + 1.96*pdat$se.fit)*1000000/adj
  pdat$min_LCI <- exp(pdat$fit - 1.96*pdat$se.fit)*1000000/adj
  pdat <- dplyr::select(pdat, -fit, -se.fit)
  pdat$name <- unique(x$name)
  
  p <- ggplot() + 
    geom_ribbon(data=pdat, aes(as.Date(Time*1000), ymin=min_LCI, ymax=min_UCI), fill="grey") +
    #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
    geom_line(data=pdat, aes(as.Date(Time*1000), min_fit), colour="black") +
    scale_x_date(name="Date", breaks="month", date_labels = "%b") +
    ylab(unique(pdat$name)) +
    theme_bw()
  
  resid <- gratia::residuals_linpred_plot(mf) 
  
  return(list(pdat=pdat, p=p, summary_stat=summary(mf), aic=AIC(mf), adj=adj, resid=resid))
}

mineral <- mineral[!is.na(mineral$Date.fished),]
mineral <- mineral[!mineral$name=="Se",]
mineral <- mineral[!is.na(mineral$ID),]

pdat_all <- NULL
summary_all <- NULL
resid_all <- NULL
for(i in unique(mineral$name)){
  print(i)
  temp <- mineral[mineral$name == i,]
  temp$Time <- as.numeric(ymd(temp$Date.fished))/1000
  out <- gam_min(temp)
  pdat_all <- rbind(pdat_all, out$pdat)
  summary_all[[i]] <- list(out$summary_stat, out$aic, out$adj)
  if(!is.null(resid_all)) resid_all <- resid_all + out$resid + theme_bw() + ggtitle(NULL, subtitle = i)
  if(is.null(resid_all)) resid_all <- out$resid + theme_bw() + ggtitle(NULL, subtitle=i)
}

png("gam_min_resid.png",  width=8, height=6, units="in", res=400)
resid_all + plot_layout(ncol=3, axis_titles = "collect") + theme_bw() +
  plot_annotation(title = expression(paste("mg ", kg^-1," ~ s(date)")), subtitle="Family: Tweedie (log-link)")
dev.off()

summary_df <- NULL
for(i in names(summary_all)){
  summary_all_df <- data.frame(name=i,
                               formula = deparse(summary_all[[i]][[1]]$formula),
                               smooth.pv = summary_all[[i]][[1]]$s.pv,
                               r.sq = summary_all[[i]][[1]]$r.sq,
                               dev.expl = summary_all[[i]][[1]]$dev.expl,
                               aic = summary_all[[i]][[2]],
                               adj = summary_all[[i]][[3]])
  summary_df <- rbind(summary_df, summary_all_df)
}
summary_df$smooth.pv <- ifelse(summary_df$smooth.pv<0.0001, "<0.0001", paste0("=",round(summary_df$smooth.pv, 4)))
summary_df$r.sq <- round(summary_df$r.sq, 4)
summary_df$dev.expl <- round(summary_df$dev.expl, 4)
summary_df %>% arrange(smooth.pv)
summary_df <- summary_df[summary_df$smooth.pv<0.1,]

mins <- pdat_all[pdat_all$name %in% summary_df$name,] %>%
  group_by(name) %>%
  summarize(val = min(min_fit), valDate = paste(date[which(min_fit == min(min_fit))], collapse = ", "))# %>%
maxs <- pdat_all[pdat_all$name %in% summary_df$name,] %>%
  group_by(name) %>%
  summarize(val = max(min_fit), valDate = paste(date[which(min_fit == max(min_fit))], collapse = ", "))# %>%
maxmin <- rbind(mins, maxs)

png("mineral_mgkg_gam.png", width=6, height=4, units="in", res=400)
ggplot() + 
  geom_ribbon(data=pdat_all[pdat_all$name %in% summary_df$name,], aes(date, ymin=min_LCI, ymax=min_UCI), colour="black", fill=NA, linetype="dashed", show.legend=F)+
  geom_line(data=pdat_all[pdat_all$name %in% summary_df$name,], aes(date, min_fit), show.legend=F)+
  scale_x_date(name="Date", breaks="2 months", date_labels = "%b") +
  ylab(expression(paste("Concentration (mg * ", kg^-1, ")"))) +
  facet_wrap(~name, scales="free_y", ncol=3)+
  theme_bw() +
  theme(panel.grid=element_blank())
dev.off()

#################### FAME #########################
gc <- read.csv("gc.csv")

names(gc) <- gsub(x = names(gc), pattern = "n.", replacement="n-", fixed=T)
names(gc) <- gsub(x = names(gc), pattern = "a.", replacement="a", fixed=T)
names(gc) <- gsub(x = names(gc), pattern = "y.", replacement="y", fixed=T)
names(gc) <- gsub(x = names(gc), pattern = ".", replacement=":", fixed=T)

fa_order <- names(gc)
fa_order <- fa_order[grep(x=fa_order, pattern="Quantity")]
fa_order <- gsub(x=fa_order, pattern="Quantity", replacement="")
fa_order <- factor(fa_order, levels=unique(fa_order))

redo <- gc %>% dplyr::select(sample, file) %>%
  distinct() %>%
  group_by(sample) %>%
  summarize(n=n()) %>%
  filter(n>1)
redo <- gc[gc$sample %in% redo$sample,]  
remove <- as.data.frame(redo[!grepl(x=redo$file, pattern="2025-11-23", fixed=T),])
remove <- unique(remove[,c("sample", "file")])

gc <- gc[!(gc$sample %in% remove$sample & gc$file %in% remove$file),]

gc %>% dplyr::select(sample, file) %>%
  distinct() %>%
  group_by(sample) %>%
  summarize(n=n()) %>%
  filter(n>1)

gc <- gc[-grep(x=gc$sample, pattern="\\T", fixed=T),]
quantity <- dplyr::select(gc, sample, starts_with("Quantity"))
area <- dplyr::select(gc, sample, contains("Area."), contains("Total"))

#sample mass is 1.5g 
# mass = 1.5*quantity/quantity23:0
std <- data.frame(sample=quantity$sample, std=quantity$`Quantity23:0`)
quantity <- pivot_longer(quantity, cols = starts_with("Quantity"))
quantity$name <- gsub(x=quantity$name, pattern="Quantity", "", fixed=T)
quantity <- left_join(quantity, std)
quantity$mass <- (quantity$value*1.5)/quantity$std

# # drop sample 528 for now (Nov 22)
# quantity <- quantity[!quantity$sample == "2025\\09\\03\\SCAL_0528.DATA",]

hist(quantity$mass)

larger <- quantity %>%
  group_by(name) %>%
  summarize(med=median(mass, na.rm=T),
            maxmass=max(mass, na.rm=T)) %>%
  filter(!is.na(med))
summary(larger)

combo <- unique(larger$name[larger$med>0.03 | larger$maxmass>0.5])
larger_max <- unique(larger$name[larger$maxmass>0.5])
larger <- unique(larger$name[larger$med>0.05])

quantity$name <- factor(quantity$name, levels=fa_order)

require(ggplot2)
png("FAME_boxplots.png", width=7, height=3, res=400,units="in")
ggplot() + geom_boxplot(data=quantity[!quantity$name=="23:0" & quantity$name %in% combo,], 
                        aes(name, mass), outliers = F) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab(NULL) +
  ylab("mg")
dev.off()

quantity$sample2 <- substr(x=quantity$sample, 17, 20)
quantity$date <- substr(x=quantity$sample, 1, 10)
quantity$date <- gsub(x=quantity$date, pattern="\\", replacement="-", fixed=T)
quantity$date <- ymd(quantity$date)
quantity$run <- substr(x=quantity$sample, 21,23)
quantity$run <- gsub(x=quantity$run, ".", "", fixed=T)
quantity$run <- gsub(x=quantity$run, "_", "", fixed=T)
quantity$run <- gsub(x=quantity$run, "8", "", fixed=T)
quantity$run <- gsub(x=quantity$run, "DA", "1",fixed=T)
quantity$run[is.na(quantity$run)] <- 1
quantity$run[quantity$run==""] <- 1

redo <- quantity %>%
  group_by(sample2) %>%
  summarize(date = max(date)) %>%
  mutate(keep="Y")

quantity <- left_join(quantity, redo)
quantity <- quantity[quantity$keep=="Y" & !is.na(quantity$keep) & !is.na(quantity$date),]
quantity$sample2 <- as.numeric(quantity$sample2)

#remove duplicates
quantity <- quantity[quantity$run==1,]

# joined data separately

# bring over all the other data to get month
quantity <- read.csv("quantity_fame.csv")
total_fa <- quantity %>%
  group_by(sample2, run) %>%
  summarize(total=sum(mass, na.rm=T))

ggplot() + geom_boxplot(data=quantity[quantity$name %in% larger,], 
                        aes(ymd(Date.fished), perc_fa, group=ymd(Date.fished)), outliers = F) + 
  facet_wrap(~name, scale="free_y") +
  xlab("Date caught") +
  ylab("% by wet mass")

ggplot() + geom_boxplot(data=quantity[quantity$name %in% larger,], 
                        aes(ymd(Date.fished), perc_fa_d, group=ymd(Date.fished)), outliers = F) + 
  facet_wrap(~name, scale="free_y") +
  xlab("Date caught") +
  ylab("% by dry mass")

quantity <- quantity[quantity$run==1,]

main_fa <- quantity[quantity$name %in% larger,] %>%
  pivot_wider(id_cols=c(sample2, run), names_from=name, values_from=perc_fa)

main_fa <- left_join(main_fa, total_fa)
names(main_fa)

main_fa_d <- quantity[quantity$name %in% larger,] %>%
  pivot_wider(id_cols=c(sample2, run), names_from=name, values_from=perc_fa_d)

main_fa_d <- left_join(main_fa_d, total_fa)


# gam
#for each FA
temp <- quantity[quantity$name %in% larger,]
temp$Time <- as.numeric(ymd(temp$Date.fished))/1000

gam_fa <- function(x){
  mf <- gam(formula = perc_fa ~ s(Time), data=x, family=betar(link="logit"), method="ML")
  print(summary(mf))
  #layout(matrix(1:2, ncol=2))
  #plot(mf)
  #layout(matrix(1:2, ncol=2))
  #acf(resid(mf), lag.max=36, main="ACF")
  #pacf(resid(mf), lag.max=36, main="pACF")
  #layout(1)
  
  ## predict trend contributions
  pdat <- expand.grid(date=seq(min(ymd(x$Date.fished), na.rm=T), max(ymd(x$Date.fished), na.rm=T)-days(1), 1))
  pdat$Time <- as.numeric(ymd(pdat$date))/1000
  pdat <- cbind(pdat, predict(mf,  newdata = pdat, se.fit = TRUE))
  pdat$fa_fit <- invlogit(pdat$fit)*1000
  pdat$fa_UCI <- invlogit(pdat$fit + 1.96*pdat$se.fit)*1000
  pdat$fa_LCI <- invlogit(pdat$fit - 1.96*pdat$se.fit)*1000
  pdat <- dplyr::select(pdat, -fit, -se.fit)
  pdat$name <- unique(x$name)
  
  p <- ggplot() + 
    geom_ribbon(data=pdat, aes(as.Date(Time*1000), ymin=fa_LCI, ymax=fa_UCI), fill="grey") +
    #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
    geom_line(data=pdat, aes(as.Date(Time*1000), fa_fit), colour="black") +
    scale_x_date(name="Date", breaks="month", date_labels = "%b") +
    ylab(unique(pdat$name)) +
    theme_bw()
  
  resid <- gratia::residuals_linpred_plot(mf) 
  
  return(list(pdat=pdat, p=p, summary_stat=summary(mf), aic=AIC(mf), resid=resid))
}

larger <- larger[!larger %in% c("23:0", "18:1n-7")]

pdat_all <- NULL
summary_all <- NULL
resid_all <- NULL
for(i in larger){
  print(i)
  temp <- quantity[quantity$name == i,]
  temp$Time <- as.numeric(ymd(temp$Date.fished))/1000
  out <- gam_fa(temp)
  pdat_all <- rbind(pdat_all, out$pdat)
  summary_all[[i]] <- list(out$summary_stat, out$aic)
  if(!is.null(resid_all)) resid_all <- resid_all + out$resid + theme_bw() + ggtitle(NULL, subtitle = i)
  if(is.null(resid_all)) resid_all <- out$resid + theme_bw() + ggtitle(NULL, subtitle=i)
}

png("gam_FA_resid.png",  width=10, height=10, units="in", res=400)
resid_all + plot_layout(ncol=2, axis_titles = "collect") + 
  theme_bw() +  
  plot_annotation(title = expression(paste("mg ", g^-1, " wet mass ~ s(date)")), subtitle="Family: Beta (logit-link)")
dev.off()

summary_df <- NULL
for(i in names(summary_all)){
  summary_all_df <- data.frame(name=i,
                               smooth.pv = summary_all[[i]][[1]]$s.pv,
                               r.sq = summary_all[[i]][[1]]$r.sq,
                               dev.expl = summary_all[[i]][[1]]$dev.expl,
                               aic = summary_all[[i]][[2]])
  summary_df <- rbind(summary_df, summary_all_df)
}
summary_df$smooth.pv <- ifelse(summary_df$smooth.pv<0.0001, "<0.0001", paste0("=",round(summary_df$smooth.pv, 3)))
summary_df$r.sq <- round(summary_df$r.sq, 4)
summary_df$dev.expl <- round(summary_df$dev.expl, 4)
summary_df %>% arrange(smooth.pv)
summary_df <- summary_df[summary_df$smooth.pv<0.1,]

mins <- pdat_all[pdat_all$name %in% summary_df$name,] %>%
  group_by(name) %>%
  summarize(val = min(fa_fit), valDate = paste(date[which(fa_fit == min(fa_fit))], collapse = ", "))# %>%
maxs <- pdat_all[pdat_all$name %in% summary_df$name,] %>%
  group_by(name) %>%
  summarize(val = max(fa_fit), valDate = paste(date[which(fa_fit == max(fa_fit))], collapse = ", "))# %>%
maxmin <- rbind(mins, maxs)

png("FA_quantity_wet.png", width=10, height=10, units="in", res=400)
ggplot() + 
  geom_ribbon(data=pdat_all[pdat_all$name %in% summary_df$name,], aes(date, ymin=fa_LCI, ymax=fa_UCI), colour="black", fill=NA, linetype="dashed", show.legend=F)+
  geom_line(data=pdat_all[pdat_all$name %in% summary_df$name,], aes(date, fa_fit), show.legend=F)+
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab(expression(paste("Concentration (mg * ", g^-1, ", by wet mass)"))) +
  facet_wrap(~name, scales="free_y", ncol=2)+
  theme_bw() +
  theme(panel.grid=element_blank())
dev.off()

pdat_all$name2 <- NA
pdat_all$name2[pdat_all$name =="20:5n-3"] <- "EPA"
pdat_all$name2[pdat_all$name =="22:6n-3"] <- "DHA"

png("FA_quantity_wet_2.png", width=6, height=3, units="in", res=400)
fa_wet2 <- ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  #geom_ribbon(data=pdat[pdat$sh==100,], aes(date, ymin=ash_LCI, ymax=ash_UCI), fill="grey") +
  geom_ribbon(data=pdat_all[pdat_all$name %in% c("20:5n-3", "22:6n-3"),], aes(date, ymin=fa_LCI, ymax=fa_UCI), colour="black", fill=NA, linetype="dashed", show.legend=F)+
  geom_line(data=pdat_all[pdat_all$name %in% c("20:5n-3", "22:6n-3"),], aes(date, fa_fit), show.legend=F)+
  #geom_rug(data=pdat_all[pdat_all$measured==T,], aes(date_adj), length = unit(0.5,"cm"), show.legend=F) +
  #scale_colour_manual(values=cols)+
  #geom_text(data=summary_df[summary_df$name %in% c("20.5n.3", "22.6n.3"),], aes(x = max(pdat_all$date), Inf, label=paste0("p.value ", smooth.pv)), vjust=2, hjust=1)+
  #geom_point(data=maxmin[maxmin$name %in% c("20.5n.3", "22.6n.3"),], aes(ymd(valDate), val))+
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab(expression(paste("Concentration (mg * ", g^-1, ", by wet mass)"))) +
  facet_wrap(~name2, scales="free_y", ncol=2)+
  theme_bw() +
  theme(panel.grid=element_blank())
print(fa_wet2)
dev.off()

# by dry weight

pdat_all_d <- NULL
summary_all_d <- NULL
resid_all_d <- NULL
for(i in larger[!larger %in% c("20:1n-11", "DMA:2")]){
  temp <- quantity[quantity$name == i,]
  print(i)
  temp$perc_fa <- temp$perc_fa_d
  temp$Time <- as.numeric(ymd(temp$Date.fished))/1000
  out <- gam_fa(temp)
  pdat_all_d <- rbind(pdat_all_d, out$pdat)
  summary_all_d[[i]] <- list(out$summary_stat, out$aic)
  if(!is.null(resid_all_d)) resid_all_d <- resid_all_d + theme_bw() + out$resid + ggtitle(NULL, subtitle = i)
  if(is.null(resid_all_d)) resid_all_d <- out$resid + theme_bw() + ggtitle(NULL, subtitle=i)
}

png("gam_FAdry_resid.png",  width=10, height=10, units="in", res=400)
resid_all_d + plot_layout(ncol=2, axis_titles = "collect") + 
  theme_bw() +  
  plot_annotation(title = expression(paste("mg ", g^-1, " dry mass ~ s(date)")), subtitle="Family: Beta (logit-link)")
dev.off()
# resids also fine

summary_df_d <- NULL
for(i in names(summary_all_d)){
  summary_all_df <- data.frame(name=i,
                               smooth.pv = summary_all_d[[i]][[1]]$s.pv,
                               r.sq = summary_all_d[[i]][[1]]$r.sq,
                               dev.expl = summary_all_d[[i]][[1]]$dev.expl,
                               aic = summary_all_d[[i]][[2]])
  summary_df_d <- rbind(summary_df_d, summary_all_df)
}
summary_df_d$smooth.pv <- ifelse(summary_df_d$smooth.pv<0.0001, "<0.0001", paste0("=",round(summary_df_d$smooth.pv, 0)))
summary_df_d$r.sq <- round(summary_df_d$r.sq, 4)
summary_df_d$dev.expl <- round(summary_df_d$dev.expl, 4)
summary_df_d %>% arrange(smooth.pv)

mins <- pdat_all_d[pdat_all_d$name %in% summary_df_d$name,] %>%
  group_by(name) %>%
  summarize(val = min(fa_fit), valDate = paste(date[which(fa_fit == min(fa_fit))], collapse = ", "))# %>%
maxs <- pdat_all_d[pdat_all_d$name %in% summary_df$name,] %>%
  group_by(name) %>%
  summarize(val = max(fa_fit), valDate = paste(date[which(fa_fit == max(fa_fit))], collapse = ", "))# %>%
maxmind<- rbind(mins, maxs)

png("FA_quantity_dry.png", width=10, height=10, units="in", res=400)
ggplot() + 
  geom_ribbon(data=pdat_all_d[pdat_all_d$name %in% summary_df_d$name,], aes(date, ymin=fa_LCI, ymax=fa_UCI), colour="black", fill=NA, linetype="dashed", show.legend=F)+
  geom_line(data=pdat_all_d[pdat_all_d$name %in% summary_df_d$name,], aes(date, fa_fit), show.legend=F)+
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab(expression(paste("Concentration (mg * ", g^-1, ", by dry mass)"))) +
  facet_wrap(~name, scales="free_y", ncol=2)+
  theme_bw() +
  theme(panel.grid=element_blank())
dev.off()
maxmind

pdat_all_d$name2 <- NA
pdat_all_d$name2[pdat_all_d$name =="20:5n-3"] <- "EPA"
pdat_all_d$name2[pdat_all_d$name =="22:6n-3"] <- "DHA"

png("FA_quantity_dry_2.png", width=6, height=3, units="in", res=400)
ggplot() + 
  #geom_point(data=new_hyd, aes(as.Date(Time*1000), wmw), colour="black") +
  #geom_ribbon(data=pdat[pdat$sh==100,], aes(date, ymin=ash_LCI, ymax=ash_UCI), fill="grey") +
  geom_ribbon(data=pdat_all_d[pdat_all_d$name %in% c("20:5n-3", "22:6n-3"),], aes(date, ymin=fa_LCI, ymax=fa_UCI), colour="black", fill=NA, linetype="dashed", show.legend=F)+
  geom_line(data=pdat_all_d[pdat_all_d$name %in% c("20:5n-3", "22:6n-3"),], aes(date, fa_fit), show.legend=F)+
  #geom_rug(data=pdat_all[pdat_all$measured==T,], aes(date_adj), length = unit(0.5,"cm"), show.legend=F) +
  #scale_colour_manual(values=cols)+
  #geom_text(data=summary_df_d[summary_df_d$name %in% c("20.5n.3", "22.6n.3"),], aes(x = max(pdat_all_d$date), Inf, label=paste0("p.value ", smooth.pv)), vjust=2, hjust=1)+
  #geom_point(data=maxmind[maxmind$name %in% c("20.5n.3", "22.6n.3"),], aes(ymd(valDate), val))+
  scale_x_date(name="Date", breaks="month", date_labels = "%b") +
  ylab(expression(paste("Concentration (mg * ", g^-1, ", by dry mass)"))) +
  facet_wrap(~name2, scales="free_y", ncol=2)+
  theme_bw() +
  theme(panel.grid=element_blank())
dev.off()


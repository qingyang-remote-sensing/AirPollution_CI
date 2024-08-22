rm(list = ls())

require(dplyr)
require(lubridate)
require(lme4)
require(lmerTest)

### sample mixed-effect model

mem_annual = lmer(MMSE_score ~ (1|ID) + yr_PM + yr_oz + yr_no2 + age + sex + factor(ethnicity)  +
              smoke + drink + exercise + year_education + HBP + BMI + diabetes + urban +
              heart_disease + past_smoke + past_drink , data = dat2)

summary(mem_annual)

### sample logistic regression model

glm1 = glm(CI ~ yr_no2 + yr_oz + yr_PM + age + sex + factor(ethnicity) + time + 
             smoke + drink + exercise + year_education + HBP + BMI + diabetes + urban+
             heart_disease + past_smoke + past_drink, data = dat2)

summary(glm1)

### sample concentration-response relationship and plotting code

require(mgcv)

gam1 = gam(data = dat2,CI ~ s(warm_oz) + warm_no2 + warm_PM + age + sex + factor(ethnicity) +
       smoke + drink + exercise + year_education + HBP + BMI + diabetes + urban +
       heart_disease + past_smoke + past_drink,family = 'binomial'
     )

p1 = plot(gam1)
p1 = p1[[1]]
adjust = p1$fit[42]
p1$x = pl$x - adjust
data_gam1 = data.frame(p1$x,p1$fit + 1.96 * p1$se,p1$fit,p1$fit - 1.96* p1$se)
colnames(data_gam1) = c("x","upper","y","lower")
part1 = data_gam1[,c(1,2)]
part1$group = "upper"
colnames(part1)[2] = "y"
part2 = data_gam1[,c(1,3)]
part2$group = "effect"
colnames(part2)[2] = "y"
part3 = data_gam1[,c(1,4)]
part3$group = "lower"
colnames(part3)[2] = "y"
data_gam1 = rbind(part1,part2,part3)

plot1 <- ggplot(data= data_gam1 , aes(x = x, y = exp(y), group = group)) +  xlim(c(75,140)) + geom_abline(intercept = 1.0, slope = 0, color ="gray",lty = 1,size = 0.5)  + geom_line(aes(color = group,size = group, lty = group),show.legend = F) + theme_bw() 
plot1 <- plot1 + scale_color_manual(values = c("royalblue","darkslategray","darkslategray")) + scale_size_manual(values = c(2,1,1)) + scale_linetype_manual(values = c(1,2,2)) + scale_y_continuous(breaks = c(0.5,1,1.5,2))
plot1 <- plot1 + ylab("Odds ratio") + theme(axis.line = element_line(color = "black")) + theme(strip.placement = "outside",strip.background.x=element_rect(color = NA,  fill=NA),strip.background.y=element_rect(color = NA,  fill=NA)) 
plot1 <- plot1 + xlab(expression(Warm~season~average~O[3]~concentrations ~ (mu*g/m^3))) +
  coord_cartesian(ylim = c(0.5, 3)) +
  theme(axis.title.x = element_text(size = 18, face = "bold"))  +
  theme(axis.title.y = element_text(size = 18, face = "bold"))  +
  theme(axis.text.x = element_text(size = 15)) + 
  theme(axis.text.y = element_text(size = 15)) 
plot1


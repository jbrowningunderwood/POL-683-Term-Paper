library(foreign)
library(MASS)
library(magrittr)
library(tidyverse) 
install.packages("dplyr")
library(dplyr)
library(pscl)
install.packages("stargazer")
library(stargazer)
install.packages("car")
library(car)
install.packages("performance")
library(performance)
install.packages("brms")
library(brms)
install.packages("tidybayes")
library(tidybayes)
install.packages("ggplot2")
library(ggplot2)
library(tidybayes)
install.packages("modelr")
library(modelr)

library(readr)
Splinter_Group_Data <- read_csv("Desktop/Splinter_Group_Data.csv")
View(Splinter_Group_Data)

df <-(Splinter_Group_Data)
df$number_of_attacks <- df$pt + df$st
df$log_GDP <- log(df$GDP)
df$log_Population <- log(df$Population)



##########################Hypothesis 1a: As the number of groups increase, the number of targets will increase. 


H1a <- glm(number_of_attacks ~ numb_groups+ split+ Split_time+ Split_time^2 + log_Population+ Regime_Type + Minorities_Risk + Military_GDP, data = df, family = quasipoisson('log'))
summary(H1a)

check_zeroinflation(H1a)

stargazer(H1a, type = "text")
stargazer(H1a)

###Roboustness Checks############
######Appendix###################################################################

H1.1a <- glm(number_of_attacks ~ numb_groups+ split+ Split_time+ Split_time^2+ log_Population+ Regime_Type + df$et_fra+ df$Military_GDP, data = df, family = quasipoisson('log'))
summary(H1.1a)

stargazer(H1a,H1.1a, type = "text")
stargazer(H1a,H1.1a)

####Hypothesis 1b: As the number of attacks increase for the splinter organization, the number of civilian targets will increase######

H1b <- glm(number_of_attacks ~ st + split+ Split_time+ Split_time^2+ st_gov + st_mil + st_pol + log_GDP+ Minorities_Risk+ Regime_Type+ Military_GDP, data = df, family = quasipoisson("log"))

check_zeroinflation(H1b)

stargazer(H1b, type = "text")
stargazer(H1b)

###Interatction term for military targets and split time#####################
H1.1b <- glm(number_of_attacks ~ st + split+ Split_time+ Split_time^2+ st_gov*Split_time + st_mil*Split_time + st_pol*Split_time + log_GDP+ Minorities_Risk+ Regime_Type+ Military_GDP, data = df, family = quasipoisson("log"))

summary(H1.1b)

stargazer(H1b,H1.1b, type = "text")
stargazer(H1b, H1.1b)

#########Prediction/Estimation model for Splinter Terrorism Military Targets#####################

model <- brm(
  formula = number_of_attacks ~ st + st_military_target,
  data = df,
  family = negbinomial("log"),
  iter = 3000,
  warmup = 1000,
  chains = 4,
  cores = 4,  
  seed = 10
)


tmpdat <- data_long %>%
  data_grid(
    st  =  seq(0, 75, by = 0.05),
    st_military_target = c(0.5)) %>%
  add_epred_draws(model) %>%
  group_by(st, st_military_target) %>%
  summarize(mean = mean(.epred),
            lower = quantile(.epred, 0.025),
            upper = quantile(.epred, 0.975)) 


plot =   ggplot(tmpdat,
                aes(
                  x = st,
                  y = mean, ymin = lower,
                  ymax = upper )) +
  geom_point(size = 1, alpha = 0.7) +
  geom_errorbar(width = .15, alpha = 0.5, color = "grey") +
  ggtitle("Predicted Number of Attacks Based on Splinter Terrorism")+ 
  theme_minimal() +
  # label axes
  xlab("Splinter Terrorism Military Target") +
  ylab("Number of Attacks") 
print(plot)

plot(dpois(c(0:20),3.70), type="l", main="mu=3.70")

summary(glm(number_of_attacks ~ st + split+ Split_time+ Split_time^2+ st_gov*Split_time + st_mil*Split_time + st_pol*Split_time + log_GDP+ Minorities_Risk+ Regime_Type+ Military_GDP, data = df, family = quasipoisson("log")))

plot(dpois(c(0:40),0.89), type="l", main="mu=0.89")


library(pscl)
mean(df$st_military_target)
hist(df$st_military_target, 
     main = "Distribution of Splinter Terrorism Against Military Targets mu=3.70",
     xlab = "Splinter Terrorism (Number of Attacks on Military Targets)",
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black")
lines(dpois(c(0:40), 3.70)*length(df$st_military_target), type="l")


###Roboustness Checks############
##########Appendix#######################################################

H1.2b <- glm(number_of_attacks ~ st + split+ Split_time+ Split_time^2+ st_gov*Split_time + st_mil*Split_time + st_pol*Split_time + log_GDP+ et_fra+ Regime_Type+ Military_GDP, data = df, family = quasipoisson("log"))

summary(H1.2b)
stargazer(H1.1b,H1.2b, type = "text")
stargazer(H1.1b,H1.2b)

####Hypothesis 1c: As the number of attacks increase for the splinter organization, the number of civilian targets increase. 

H1c <- glm(number_of_attacks ~ st + split+ Split_time+ Split_time^2+ st_bus+ st_prt_civ_prop + st_edu+ log_Population+ Minorities_Risk+Regime_Type+ Military_GDP, data = df, family = quasipoisson("log"))
summary(H1c)

check_zeroinflation(H1c)

stargazer(H1c, type = "text")
stargazer(H1c)

###Interaction Term with Civilian targets and Split time##############

H1.1c <- glm(number_of_attacks ~ st + split+ Split_time+ Split_time^2+ st_bus*Split_time+ st_prt_civ_prop*Split_time + st_edu*Split_time+ log_Population+ Minorities_Risk+Regime_Type+ Military_GDP, data = df, family = quasipoisson("log"))
summary(H1.1c)

stargazer(H1c, H1.1c, type = "text")
stargazer(H1c, H1.1c)

#########Prediction/Estimation model for Splinter Terrorism Civilian Targets#####################
model <- brm(
  formula = number_of_attacks ~ st + st_civilian_target,
  data = df,
  family = negbinomial("log"),
  iter = 3000,
  warmup = 1000,
  chains = 4,
  cores = 4,  
  seed = 10
)

tmpdat <- data_long %>%
  data_grid(
    st = seq(0, 75, by = 0.05),
    st_civilian_target = 0.5
  ) %>%
  add_epred_draws(model) %>%
  group_by(st, st_civilian_target) %>%
  summarize(
    mean = mean(.epred),
    lower = quantile(.epred, 0.025),
    upper = quantile(.epred, 0.975)
  )


plot =   ggplot(tmpdat,
                aes(
                  x = st,
                  y = mean, ymin = lower,
                  ymax = upper )) +
  geom_point(size = 1, alpha = 0.5) +
  geom_errorbar(width = .1, alpha = 0.4, color = "grey") +
  ggtitle("Predicted Number of Attacks Based on Splinter Terrorism")+ 
  theme_minimal() +
  # label axes
  xlab("Splinter Terrorism") +
  ylab("Civilian Targets") 
print(plot)


summary(glm(number_of_attacks ~ st + split+ Split_time+ Split_time^2+ st_bus*Split_time+ st_prt_civ_prop*Split_time + st_edu*Split_time+ log_Population+ Minorities_Risk+Regime_Type+ Military_GDP, data = df, family = quasipoisson("log")))

mean(df$st_civilian_target)
hist(df$st_civilian_target, 
     main = "Distribution of Splinter Terrorism Against Civilian Targets mu=2.93",
     xlab = "Splinter Terrorism (Number of Attacks on Civilian Targets)",
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black")
lines(dpois(c(0:40),2.93)*length(df$st_civilian_target), type="l")


###Roboustness Checks############
###Appendix########################################

H1.2c <-  glm(number_of_attacks ~ st + split+ Split_time+ Split_time^2+ st_bus*Split_time+ st_prt_civ_prop*Split_time + st_edu*Split_time+ log_Population+ et_fra+Regime_Type+ Military_GDP, data = df, family = quasipoisson("log"))

summary(H1.2c)


stargazer(H1.1c, H1.2c, type = "text")
stargazer(H1.1c, H1.2c)

###########Hypothesis 2: Before a split occurs within a rebel group, the number of attacks from a parent organization against a target will decrease######################################

df$pt_military_target <- df$pt_gov + df$pt_mil+ df$pt_pol
df$pt_civilian_target <- df$pt_bus + df$pt_edu + df$pt_prt_civ_prop

df$pre_split_time <- ifelse(df$Split_time < 0, df$Split_time, 0)
df$post_split_time <- ifelse(df$Split_time > 0, df$Split_time, 0)

H2a <- glm(number_of_attacks ~ pt + pt_military_target + pt_civilian_target + split+ pre_split_time+ +log_GDP+log_Population+ Minorities_Risk+ Regime_Type+ Military_GDP, data = df, family = quasipoisson("log"))

check_zeroinflation(H2a)

summary(H2a)
stargazer(H2a, type = "text")
stargazer(H2a)


##################Prediction/Estimation for Parent Attacks against Military targets##################
summary(glm(number_of_attacks ~ pt + pt_military_target + pt_civilian_target + split+ pre_split_time+ +log_GDP+log_Population+ Minorities_Risk+ Regime_Type+ Military_GDP, data = df, family = quasipoisson("log")))

model <- brm(
  formula = number_of_attacks ~ pt + pt_military_target,
  data = df,
  family = negbinomial("log"),
  iter = 3000,
  warmup = 1000,
  chains = 4,
  cores = 4,  
  seed = 10
)

tmpdat <- data_long %>%
  data_grid(
    pt = seq(0, 75, by = 0.05),
    pt_military_target = 0.5
  ) %>%
  add_epred_draws(model) %>%
  group_by(pt, pt_military_target) %>%
  summarize(
    mean = mean(.epred),
    lower = quantile(.epred, 0.025),
    upper = quantile(.epred, 0.975)
  )


plot =   ggplot(tmpdat,
                aes(
                  x = pt,
                  y = mean, ymin = lower,
                  ymax = upper )) +
  geom_point(size = 1, alpha = 0.5) +
  geom_errorbar(width = .1, alpha = 0.4, color = "grey") +
  ggtitle("Predicted Number of Attacks Based on Parent Terrorism")+ 
  theme_minimal() +
  # label axes
  xlab("Parent Terrorism") +
  ylab("Military Targets") 
print(plot)


mean(df$pt_military_target)

hist(df$pt_military_target,
     main = "Distribution of Parent Terrorism Against Military Targets mu=0.82",
     xlab = "Parent Terrorism (Number of Attacks on Military Targets)",
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black")
lines(dpois(c(0:40),1.30)*length(df$pt_military_target), type="l")

##################Prediction/Estimation for Parent Attacks against Civilian targets##################
model <- brm(
  formula = number_of_attacks ~ pt + pt_civilian_target,
  data = df,
  family = negbinomial("log"),
  iter = 3000,
  warmup = 1000,
  chains = 4,
  cores = 4,  
  seed = 10
)

tmpdat <- data_long %>%
  data_grid(
    pt = seq(0, 75, by = 0.05),
    pt_civilian_target = 0.5
  ) %>%
  add_epred_draws(model) %>%
  group_by(pt, pt_civilian_target, pre_split_time) %>%
  summarize(
    mean = mean(.epred),
    lower = quantile(.epred, 0.025),
    upper = quantile(.epred, 0.975)
  )


plot =   ggplot(tmpdat,
                aes(
                  x = pt,
                  y = mean, ymin = lower,
                  ymax = upper )) +
  geom_point(size = 1, alpha = 0.5) +
  geom_errorbar(width = .1, alpha = 0.4, color = "grey") +
  ggtitle("Predicted Number of Attacks Based on Parent Terrorism")+ 
  theme_minimal() +
  # label axes
  xlab("Parent Terrorism") +
  ylab("Civilian Targets") 
print(plot)



mean(df$pt_civilian_target)
hist(df$pt_civilian_target,
     main = "Distribution of Parent Terrorism Against Civilian Targets mu=2.93",
     xlab = "Parent Terrorism (Number of Attacks on Civilian Targets)",
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black")
lines(dpois(c(0:40),2.93)*length(df$pt_military_target), type="l")

###Roboustness Checks############
######Appendix################################################################

H2b <- glm(number_of_attacks ~ pt + pt_military_target + pt_civilian_target + split+ pre_split_time +log_GDP + log_Population+ et_fra+Regime_Type+ Military_GDP, data = df, family = quasipoisson("log"))

summary(H2b)

stargazer(H2a,H2b, type = "text")
stargazer(H2a, H2b)

#######Hypothesis 3: After a split occurs within a rebel group, the number of attacks from the splinter organization will increase against a target###########

df$st_military_target <- df$st_gov + df$st_mil+ df$st_pol
df$st_civilian_target <- df$st_bus + df$st_edu + df$st_prt_civ_prop

H3a <-glm(number_of_attacks ~ st + st_military_target+ st_civilian_target + split+ post_split_time+log_GDP+log_Population+ Minorities_Risk+ Regime_Type+ Military_GDP, data = df, family = quasipoisson("log"))

check_zeroinflation(H3a)

summary(H3a) 
stargazer(H3a, type = "text")
stargazer(H3a)

###Roboustness Checks############
######Appendix########################################################################

H3b <-glm(number_of_attacks ~ st + st_military_target+ st_civilian_target + split+ post_split_time +log_GDP+log_Population+ et_fra+ Regime_Type+ Military_GDP, data = df, family = quasipoisson("log"))

summary(H3b) 
stargazer(H3a,H3b, type = "text")
stargazer(H3a, H3b)

#######################################Descriptive Table#########################
library(ggplot2)

data_long <- df %>%
  pivot_longer(cols = c(pt, st), 
               names_to = "Type", 
               values_to = "Attacks")


ggplot(data_long, aes(x = Split_time, y = Attacks, color = Type)) +
  geom_line(linewidth = 0.1) +                     
  geom_point(linewidth = 0.5) +                    
  labs(
    title = "Outbidding",
    x = "Split Time",
    y = "Number of Attacks",
    color = "Organization"
  ) +
  theme_minimal() +                         
  theme(
    text = element_text(size = 14),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
    
  )



# estadistica QP Patrick
# by Jennifer

library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(emmeans)

# cargar base de datos

df= read_csv('data/data.csv')

# crear una columna donde aparezca solo el numero 

df=df %>%
mutate(participant= str_extract(Participant_ID, '[0-9]+'))

# mean by expgroup

df %>%
  group_by(ExpGroup) %>%
  summarise(mean_Av = mean(Average, na.rm = T), sd_Av = sd(Average, na.rm = T))

# mean by Hergroup

df %>%
  group_by(HerGroup) %>%
  summarise(mean_Av = mean(Average, na.rm = T), sd_Av = sd(Average, na.rm = T))

# ploy response by condition by group

df %>%
  ggplot(aes(x = Condition, y = Average, color = ExpGroup)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
 geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'condition', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 3: Proportion of accurate responsesacross conditions') 


# modelo pregunta 1
 # ExpGroup + LF_Main + LF_Sub + (1|sub) + (1|items)

mod1 = glmer(
  Average ~ 0 + ExpGroup + LF_Main + LF_Sub +
    (1 | participant) + 
    (1 | Item),
  data = df,
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(mod1)

# modelo pregunta 2

# Average ~ (FofA_Prod + FofA_Comp) + (LF_Main + LF_Sub) + DELE

# arreglar data para que la suma se asocie a participante

df0_temp = df  %>% select(participant, FofA_Prod, FofA_Comp, LF_Main, LF_Sub)    

df0_temp = df0_temp %>%
mutate(sum1 = FofA_Prod + FofA_Comp) %>%
mutate(sum2 = LF_Main + LF_Sub) 

df =   left_join( df, df0_temp, by = "participant")


mod2 = glmer(
  Average ~ 0 + sum1 + sum2 + DELE +
    (1 | participant) + 
    (1 | Item),
  data = df,
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(mod2)




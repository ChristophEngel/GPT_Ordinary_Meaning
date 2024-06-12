##### ENGEL McADAMS
##### ANALYSIS
##### 231210

##### HOUSEKEEPING ----
rm(list = ls())
graphics.off()

setwd("/Users/engel/Documents/word/Manuskript/Verhalten/Engel McAdams/Data/ReplicationPackage")

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5))


##### TOBIA FIG 5 ----
load("Tobia.RData")

ggplot(aes(x = reorder(name, yes), y = yes, label = round(yes, 2)),
       data = tobia) +
  geom_col(fill = "#00BFC4") +
  geom_text(hjust = -0.2) +
  coord_flip() +
  ylim(0, 105) +
  xlab("")
ggsave(device = pdf,
       file = "Fig1.pdf",
       height = 4,
       width = 6,
       unit = "in")

##### DIRECT ----
load("results_conc.RData")

  #### descriptives ----
    ### data preparation
results_conc %>% 
  select(- key) %>% 
  mutate(cond = "GPT") -> results
cpresults <- rbind(tobia, results)
cpresults %>% 
  arrange(desc(cond == "Tobia")) %>% 
  mutate(name = factor(name,
                       levels = rev(unique(cpresults$name[cpresults$cond == "Tobia"])))) -> cpresults

ggplot(cpresults, 
       aes(x = name, y = yes, fill = cond)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = "") +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle("GPT direct")

ggsave(device = pdf,
       file = "Fig2.pdf",
       height = 4,
       width = 6,
       units = "in")

  #### Kolmogorov Smirnov ----
cptobia <- cpresults[cpresults$cond == "Tobia",]
cpgpt <- cpresults[cpresults$cond == "GPT",]
tobia_values <- as.numeric(cptobia$yes)
gpt_values <- as.numeric(cpgpt$yes)
ks.test(tobia_values, gpt_values)

##### CHAIN OF THOUGHT: DEFINITION ----
load("results_definition.RData")

  #### descriptives ----
    ### data preparation
results_definition %>% 
  select(- key) %>% 
  mutate(cond = "GPT") -> results
cpresults <- rbind(tobia, results)
cpresults %>% 
  arrange(desc(cond == "Tobia")) %>% 
  mutate(name = factor(name,
                       levels = rev(unique(cpresults$name[cpresults$cond == "Tobia"])))) -> cpresults

ggplot(cpresults, 
       aes(x = name, y = yes, fill = cond)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = "") +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle("GPT chain of thought: definition")

ggsave(device = pdf,
       file = "Fig3.pdf",
       height = 4,
       width = 6,
       units = "in")

  #### Kolmogorov Smirnov ----
cptobia <- cpresults[cpresults$cond == "Tobia",]
cpgpt <- cpresults[cpresults$cond == "GPT",]
tobia_values <- as.numeric(cptobia$yes)
gpt_values <- as.numeric(cpgpt$yes)
ks.test(tobia_values, gpt_values)



##### BELIEFS CONTINUOUS ----
load("results_belief.RData")

  #### descriptives ----
    ### data preparation
results_belief %>% 
  select(- key) %>% 
  mutate(cond = "GPT") -> results
cpresults <- rbind(tobia, results)
cpresults %>% 
  arrange(desc(cond == "Tobia")) %>% 
  mutate(name = factor(name,
                       levels = rev(unique(cpresults$name[cpresults$cond == "Tobia"])))) -> cpresults

ggplot(cpresults, 
       aes(x = name, y = yes, fill = cond)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = "") +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle("GPT belief (numerical)")

ggsave(device = pdf,
       file = "Fig4.pdf",
       height = 4,
       width = 6,
       units = "in")

  #### Kolmogorov Smirnov ----
cptobia <- cpresults[cpresults$cond == "Tobia",]
cpgpt <- cpresults[cpresults$cond == "GPT",]
tobia_values <- as.numeric(cptobia$yes)
gpt_values <- as.numeric(cpgpt$yes)
ks.test(tobia_values, gpt_values)




##### BELIEFS LIKERT ----
load("results_bellik.RData")

  #### descriptives ----
    ### data preparation
results_bellik %>% 
  mutate(cond = "GPT") -> results
cpresults <- rbind(tobia, results)
cpresults %>% 
  arrange(desc(cond == "Tobia")) %>% 
  mutate(name = factor(name,
                       levels = rev(unique(cpresults$name[cpresults$cond == "Tobia"])))) -> cpresults

ggplot(cpresults, 
       aes(x = name, y = yes, fill = cond)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = "") +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle("GPT belief (Likert)")

ggsave(device = pdf,
       file = "Fig5.pdf",
       height = 4,
       width = 6,
       units = "in")

  #### Kolmogorov Smirnov ----
cptobia <- cpresults[cpresults$cond == "Tobia",]
cpgpt <- cpresults[cpresults$cond == "GPT",]
tobia_values <- as.numeric(cptobia$yes)
gpt_values <- as.numeric(cpgpt$yes)
ks.test(tobia_values, gpt_values)

##### NEUTRAL VS. CONTEXT PARK ----
load("results_contpark.RData")

  #### descriptives ----
cplikpark <- rbind(results_bellik, results_contpark)
cplikpark$cond2 <- ifelse(cplikpark$cond == "bellik", 0, 1)
cplikpark$cond <- factor(cplikpark$cond2,
                       levels = c(0, 1),
                       labels = c("neutral", "park"))
cplikpark %>% 
  arrange(cond, yes) %>% 
  mutate(name = factor(name, levels = unique(name))) -> cplikpark

    # Identify the order based on cond == "neutral"
order_cond0 <- cplikpark$name[cplikpark$cond == "neutral"]

    # Reverse the order of levels in the cond factor
cplikpark$cond <- factor(cplikpark$cond, levels = rev(levels(cplikpark$cond)))

  ### plot
ggplot(cplikpark, aes(x = reorder(name, match(name, order_cond0)),
                    y = yes, fill = as.factor(cond))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  xlab("") +
  ylab("mean assessment") +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#619CFF", "#F8766D")) +
  ggtitle("context park")

ggsave(device = pdf,
       file = "Fig6.pdf",
       height = 4,
       width = 6,
       units = "in")

  #### Kolmogorov Smirnov ----
cplik <- cplikpark[cplikpark$cond == "neutral",]
cppark <- cplikpark[cplikpark$cond == "park",]
lik_values <- as.numeric(cplik$yes)
park_values <- as.numeric(cppark$yes)
ks.test(lik_values, park_values)


##### ALTERNATIVE RULES ----
  #### descriptives ----
    ### define order as in park data
results_contpark %>% 
  arrange(desc(yes)) -> results_contpark
ordercont <- factor(results_contpark$name)

    ### load other datasets
load("results_contdui.RData")
load("results_contliab.RData")
load("results_contenha.RData")
load("results_contcens.RData")

    ### reorder other datasets
results_contdui %>%
  arrange(match(name, ordercont)) -> results_contdui

results_contdui %>% 
  arrange(match(name, ordercont)) -> results_contdui
results_contliab %>% 
  arrange(match(name, ordercont)) -> results_contliab
results_contenha %>% 
  arrange(match(name, ordercont)) -> results_contenha
results_contcens %>% 
  arrange(match(name, ordercont)) -> results_contcens

cpcont <- rbind(results_contpark, results_contdui,
                results_contliab, results_contenha,
                results_contcens)

cpcont %>% 
  mutate(cond = case_when(cond == "contpark" ~ "park",
                          cond == "contdui" ~ "dui",
                          cond == "contliab" ~ "liab",
                          cond == "contenha" ~ "enhance", 
                          cond == "contcens" ~ "census"),
         cond = factor(cond,
                       levels = c("park", "dui", "liab",
                                  "enhance", "census"))) -> cpcont

    # Reverse the order of levels in the cond factor
cpcont$cond <- factor(cpcont$cond, levels = rev(levels(cpcont$cond)))
cpcont$name <- factor(cpcont$name, levels = ordercont)

ggplot(cpcont, aes(x = as.factor(cond),
                    y = yes, 
                   fill = as.factor(cond),
                   label = round(yes, 0))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(hjust = ifelse(cpcont$yes > 30, 2, -.2)) +
  coord_flip() +
  xlab("") +
  ylab("mean assessment") +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#F8766D","green", "yellow",
                               "brown", "#619CFF")) +
  ggtitle("alternative contexts") +
  facet_wrap(~ name)

ggsave(device = pdf,
       file = "Fig7.pdf",
       height = 8,
       width = 8,
       units = "in")

  #### regression ----
summary(lm(yes ~ name + cond, data = cpcont))
    # for IE, one needs individual data

##### HISTORICAL MEANING ----
  #### descriptives ----
    ### define order as in park data
results_contpark %>% 
  arrange(yes) -> results_contpark
ordercont <- factor(results_contpark$name)

    ### load other datasets
load("results_orext.RData")
load("results_orint.RData")

    ### reorder other datasets
results_orext %>%
  arrange(match(name, ordercont)) -> results_orext
results_orint %>% 
  arrange(match(name, ordercont)) -> results_orint

cpor <- rbind(results_contpark, results_orext,
                results_orint)

cpor %>% 
  mutate(cond = case_when(cond == "contpark" ~ "textualist",
                          cond == "orext" ~ "extensional",
                          cond == "orint" ~ "intensional"),
         cond = factor(cond,
                       levels = c("intensional", "extensional",
                                  "textualist")),
         name = factor(name,
                       levels = ordercont)) -> cpor

ggplot(cpor, aes(x = as.factor(name),
                    y = yes, 
                   fill = as.factor(cond),
                   label = round(yes, 0))) +
  geom_col(position = "dodge") +
  coord_flip() +
  xlab("") +
  ylab("mean assessment") +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("tan4", "chartreuse4", "#619CFF")) +
  ggtitle("textualism vs. originalism") 

ggsave(device = pdf,
       file = "Fig8.pdf",
       height = 4,
       width = 6,
       units = "in")

  #### descriptives w/o textualism ----
    ### load datasets
load("results_orext.RData")
load("results_orint.RData")

    ### define order as in extensional data
results_orext %>% 
  arrange(yes) -> results_orext
ordercont <- factor(results_orext$name)

    ### reorder other dataset
results_orint %>% 
  arrange(match(name, ordercont)) -> results_orint

cpor2 <- rbind(results_orext,
                results_orint)

cpor2 %>% 
  mutate(cond = case_when(cond == "orext" ~ "extensional",
                          cond == "orint" ~ "intensional"),
         cond = factor(cond,
                       levels = c("intensional", "extensional")),
         name = factor(name,
                       levels = ordercont)) -> cpor2

ggplot(cpor2, aes(x = as.factor(name),
                    y = yes, 
                   fill = as.factor(cond),
                   label = round(yes, 0))) +
  geom_col(position = "dodge") +
  coord_flip() +
  xlab("") +
  ylab("mean assessment") +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("tan4", "chartreuse4")) +
  ggtitle("historical meaning") 

ggsave(device = pdf,
       file = "Fig8a.pdf",
       height = 4,
       width = 6,
       units = "in")

  #### regression ----
summary(lm(yes ~ name + relevel(cond, ref = "textualist"), data = cpor))
    # for IE, one needs individual data


##### PURPOSIVISM ----
  #### descriptives ----
    ### load datasets
load("results_purpannoy.RData")
load("results_purpaccident.RData")
load("results_purpspace.RData")
load("results_purpdamage.RData")
load("results_purplocal.RData")
load("results_purpbeauty.RData")

results_purpannoy %>% 
  arrange(desc(yes)) -> results_purpannoy
orderpurp <- factor(results_purpannoy$name)

cppurp <- rbind(results_purpannoy, results_purpaccident,
                results_purpspace, results_purpdamage,
                results_purplocal, results_purpbeauty)

cppurp %>% 
  mutate(cond = case_when(cond == "purpannoy" ~ "annoyance",
                          cond == "purpaccident" ~ "accident",
                          cond == "purpspace" ~ "space",
                          cond == "purpdamage" ~ "damage", 
                          cond == "purplocal" ~ "local",
                          cond == "purpbeauty" ~ "beauty",),
         cond = factor(cond,
                       levels = c("annoyance", "accident", "space",
                                  "damage", "local",
                                  "beauty"))) -> cppurp

    # Reverse the order of levels in the cond factor
cppurp$cond <- factor(cppurp$cond, levels = rev(levels(cppurp$cond)))
cppurp$name <- factor(cppurp$name, levels = orderpurp)

ggplot(cppurp, aes(x = as.factor(cond),
                    y = yes, 
                   fill = as.factor(cond),
                   label = round(yes, 0))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(hjust = ifelse(cppurp$yes > 30, 2, -.2)) +
  coord_flip() +
  xlab("") +
  ylab("mean assessment") +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("cadetblue4","burlywood3", "darkseagreen",
                               "darkorange3", "cyan3", "mediumpurple3")) +
  ggtitle("alternative purposes") +
  facet_wrap(~ name)

ggsave(device = pdf,
       file = "Fig9.pdf",
       height = 8,
       width = 8,
       units = "in")

  #### stats ----
    ### regression of summary data
summary(lm(yes ~ name + relevel(cond, ref = "annoyance"), data = cppurp))
    # for IE, one needs individual data

    ### chi square tests of individual comparisons
comp <- function (x1, x2, mult) {
  y1 <- x1 * mult
  y2 <- x2 * mult
  yes <- c(y1, y2)
  no <- 0
  df <- data.frame(yes = yes, no = no)
  df$no <- (100 * mult) - yes
  df
  chisq.test(df)
}

  ### pogo stick annoyance vs. accident, as observed
comp(36, 50, 1)
  ### life raft accident vs. beauty, as observed
comp(38, 52, 1)
  ### crutches space vs. damage, as observed
comp(39, 52, 1)
  ### zip line annoyance vs. beauty, as observed
comp(39, 52, 1)

  ### automobile accident vs. space, 1000
comp(78, 82, 10)

  ### automobile local vs. beauty, 20000
comp(81, 82, 10)

  ### automobile local vs. beauty, 20000
comp(81, 82, 200)

  ### automobile local vs. beauty, bonferroni, 50000
comp(81, 82, 500)
factorial(6)
4.827e-05*factorial(6)

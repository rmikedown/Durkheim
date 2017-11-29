# Load packages
library(dplyr)
library(ggplot2)

# Read in the data
alcoholism <- read.csv("suicide010.csv")
germany <- read.csv("suicide011.csv")

germany$SP100K <- germany$SP1M / 10

# Fix the order of the groups
names <- c("0-4 liters", "4.5-6.4 liters", "7.2-9.2 liters", "10.8-13 liters")
sections <- c("0-50", "51-75", "76-100", "101-150", "151-200", "201-250", "251-300", "Above 300")
germany$LCPC <- names
germany$LCPC <- factor(germany$LCPC, levels = names)
alcoholism$SP100K <- sections 
alcoholism$SP100K <- factor(alcoholism$SP100K, levels = sections)

# Visualize
alcoholism %>% ggplot(aes(x = SP1M,
                          y = AIP100A)) +
  geom_bar(stat = "identity") +
  xlab("Sections of France Categorized by Suicide Rate Per 1 Million") +
  ylab("Alcoholic Insane Per 100 Admissions to Psych Clinics") +
  ggtitle("Clinical insanity due to alcoholism and regions categorized by suicide")


alc.model <- lm(AIP100A ~ SP100K, data = alcoholism)


germany %>% ggplot(aes(x = LCPC,
                       y = SP100K)) + geom_bar(stat = "identity") +
  xlab("Liters of Alcohol Consumed Per Capita in 1884-86") +
  ylab("Suicides Per One Hundred Thousand") +
  ggtitle("Suicide rate in areas of Germany grouped by alcohol consumption")


# We have contemporary World Bank Data for comparison
suicide <- read.csv("wb_alcsuicide.csv")

# Regions are divided roughly according to these summary statistics
summary(suicide$ACPC)
summary(suicide$Suicide.mortality.rate..per.100.000.population.)

suicide %>% filter(complete.cases(suicide$alcohol.code)) %>% 
  ggplot(aes(x = alcohol.code,
            y = Suicide.mortality.rate..per.100.000.population.)) + 
  geom_boxplot() +
  ggtitle("Average suicide rate for countries grouped by alcohol consumption") +
  xlab("Liters of Alcohol Consumed Per Capita Per Person in 2015") +
  ylab("Suicides Per One Hundred Thousand")
  
suicide %>% ggplot(aes(x = ACPC,
                       y = Suicide.mortality.rate..per.100.000.population.)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = .5) +
  ggtitle("The linear relationship between alcohol consumption and suicide") +
  xlab("Liters of Alcohol Consumed Per Capita Per Person in 2015") +
  ylab("Suicides Per One Hundred Thousand") +
  labs(caption = "Source: World Bank")

# Let's do a regression analysis
s.model <- lm(Suicide.mortality.rate..per.100.000.population. ~ 
                ACPC, 
              data = suicide)

summary(s.model)

# Chi-Square
s.tab <- table(suicide$suicide.level, suicide$alcohol.code)

chisq.test(s.tab)

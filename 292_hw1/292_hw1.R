setwd("C:/Users/kadir_vum34h3/OneDrive/Desktop/292_hw1/")
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggmosaic)

#1
cps <- read.table("cps.txt", header = FALSE)
colnames(cps) <- c("EDUCATION","SOUTH",
                   "SEX","EXPERIENCE",
                   "UNION","WAGE","AGE",
                   "RACE","OCCUPATION",
                   "SECTOR","MARR")
cps$SOUTH <- factor(cps$SOUTH, levels = 0:1,
                    labels = c("South_No","South_Yes"))
cps$SEX <- factor(cps$SEX, levels = 0:1,
                  labels = c("Male","Female"))
cps$UNION <- factor(cps$UNION, levels = 0:1,
                    labels = c("Union_No","Union_Yes"))
cps$RACE <- factor(cps$RACE, levels = 1:3,
                   labels = c("Other","Hispanic","White"))
cps$OCCUPATION <- factor(cps$OCCUPATION, levels = 1:6,
                         labels = c("Management","Sales","Clerical",
                                    "Service","Professional","Other"))
cps$SECTOR <- factor(cps$SECTOR, levels = 0:2,
                     labels = c("Other","Manufacturing","Construction"))
cps$MARR <- factor(cps$MARR, levels = 0:1, labels = c("Unmarried", "Married"))

#a
newData_partA <- cps %>% select(SEX, EXPERIENCE, WAGE, AGE, MARR)

#b
newData_partA %>% select_if(is.numeric) %>% cor

#c
newData_partC <- cps %>%
  select(-c(SOUTH,UNION,MARR)) %>%
  filter(between(AGE,30,50), SECTOR == "Construction")

#d
newData_partC %>%
  mutate(New_Column = WAGE / AGE) %>%
  filter(New_Column > 0.25) %>%
  nrow() %>%
  paste("There are", ., "observations satisfying that condition.")

#e
cps %>% subset(SEX == "Female") %>% aggregate(WAGE ~ OCCUPATION, ., mean)

#f
cps %$% table(SEX,MARR)

#2
#a
ggplot(data = cps, aes(x = AGE, y = WAGE, color = SEX)) +
  geom_point() +
  scale_color_manual(values = c("Gold","Dark Blue")) +
  labs("Scatter Plot for AGE vs WAGE")

#b
ggplot(cps, aes(WAGE, color = RACE)) +
  geom_density(linetype = 3, size = 1.5) +
  labs(title = "Density Curves for WAGE")

#c
q2_partc <-ggplot(data = cps) +
  geom_boxplot(mapping = aes(x = RACE, y = WAGE, fill = RACE)) +
  labs(title = "Boxplots for WAGE", y = "WAGE ($/hour)") +
  facet_wrap(~ OCCUPATION, nrow = 2)
q2_partc

#d
cps %>% filter(SEX == "Male") %>%
  ggplot(., aes(x = AGE)) +
  geom_histogram(fill = "Blue", bins = 20) +
  labs(title = "Histogram of Ages of Males") -> hist_males
cps %>% filter(SEX == "Female") %>%
  ggplot(., aes(x = AGE)) +
  geom_histogram(fill = "Pink", bins = 20) +
  labs(title = "Histogram of Ages of Females") -> hist_females
gridExtra::grid.arrange(hist_males, hist_females, ncol = 1)

#e
ggplot(cps) +
  geom_mosaic(aes(x = product(RACE, OCCUPATION), fill = RACE))

#3
#a
set.seed(292)
obj1 <- list(X = rnorm(1000),
             Y = rnorm(50, 10, 2),
             Z = runif(200, -5, 20))
lapply(obj1, mean)

#b
obj2 <- matrix(obj1$X, ncol = 20)
apply(obj2, 2, sd)

#c
lett <- rep(LETTERS[1:4], each = 50)
obj3 <- data.frame(Z = obj1$Z,lett)
tapply(obj3$Z, obj3$lett, mean)

#d
splitted_Y <- split(obj1$Y, gl(5,10))
sapply(splitted_Y, range)

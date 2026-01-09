library(palmerpenguins)
library(ggplot2)
ggplot(data=penguins,aes(x=flipper_length_mm,y=body_mass_g)) +
  geom_point()

ggplot(data=penguins,aes(x=flipper_length_mm,y=body_mass_g)) +
  geom_point(color="purple")

ggplot(data=penguins,aes(x=flipper_length_mm,y=body_mass_g)) +
  geom_point(aes(color=species))

ggplot(data=penguins,aes(x=flipper_length_mm,y=body_mass_g)) +
  geom_point(aes(shape=species))

ggplot(data=penguins,aes(x=flipper_length_mm,y=body_mass_g)) +
  geom_point(aes(color=species, shape=species))

ggplot(data=penguins,aes(x=flipper_length_mm,y=body_mass_g)) +
  geom_point(aes(color=species, shape=species)) +
  facet_wrap(~species)

ggplot(data=penguins,aes(x=flipper_length_mm,y=body_mass_g)) +
  geom_point(aes(color=species, shape=species)) +
  facet_wrap(~species) +
  labs(title="Palmer Penguins: Body Mass vs. Flipper Length")


print("Coding in R")

library(tidyverse)

data("ToothGrowth")
View(ToothGrowth)
filtered_tg <- filter(ToothGrowth,dose==0.5)
View(fitered_tg)

arrange(filtered_tg,len)

arrange(filter(ToothGrowth,dose==0.5),len)

filtered_toothgrowth <- ToothGrowth %>% 
  filter(dose == 0.5) %>% 
  arrange (len)

filtered_toothgrowth <- ToothGrowth %>% 
  filter(dose == 0.5) %>% 
  group_by(supp ) %>% 
  summarize(mean_len=mean(len,na.rm=T), .group="drop")
  
library(ggplot2)
data("diamonds")
View(diamonds)
head(diamonds)

str(diamonds)
colnames(diamonds)
mutate(diamonds,carat_2 = carat*100)

# Clean data 
library(here)
library(skimr)
library(janitor)
data("penguins")

skim_without_charts(penguins)
glimps(penguins)
head(penguins)
penguins %>% select(species)
penguins %>% select(-species)

penguins %>% rename(island_new = island)

rename_with(penguins,toupper)



rename_with(penguins,tolower)

clean_names(penguins)

penguins %>% arrange (bill_length_mm)

penguins %>% arrange (-bill_length_mm)

penguins2 <- penguins %>% arrange (-bill_length_mm)

penguins %>% group_by(island) %>% drop_na() %>% 
  summarize(mean_bill_length_mm = mean(bill_length_mm))
  
  
  penguins %>% group_by(island) %>% drop_na() %>% 
    summarize(max_bill_length_mm = max(bill_length_mm))
  
  penguins %>% group_by(species,island) %>% drop_na() %>% 
    summarize(max_bill_length_mm = max(bill_length_mm),mean_bill_length_mm = mean(bill_length_mm) )
 
penguins %>% filter(species == "Adelie")

# organize data 

id <- c(1:6)
name <- c("John Mendes","Rob Stewart","Rachel Abrahamson","Christy Hickman","Johnson Harper","Candace Bob")
job_title <- c("Professor","Engineer","Teacher","Technician", "Assistant","Director")
employee <- data.frame(id,name, job_title)
employee_1 <- separate(employee,name, into=c('first_name','last_name'),sep=' ')
unite(employee_1,'name',first_name,last_name,sep=' ')

penguins %>% mutate(body_mass_kg = body_mass_g/1000,flipper_length_m=flipper_length_mm/1000)

library(Tmisc)
data("quartet")

quartet %>% group_by(set ) %>% 
  summarize(mean(x),sd(x),mean(y),sd(y),cor(x,y))

ggplot(quartet,aes(x,y))+geom_point()+
  geom_smooth(method=lm,se=FALSE) +facet_wrap(~set)
library(datasauRus)

ggplot(datasaurus_dozen,aes(x=x,y=y,colour=dataset))+
  geom_point() +theme_void()+
  theme(legend.position = "none")+
  facet_wrap(~dataset)
  
# Check for bias

install.packages("SimDesign")
library(SimDesign)
actual_temp <- c(68.3, 70, 72.4, 71, 67, 70)
predicted_temp <- c(67.9, 69, 71.5, 70, 67, 69)
bias(actual_temp,predicted_temp)

actual_sales <- c( 150, 203, 137, 247, 116, 287)
predicted_sales <- c(200, 300, 150, 250, 150, 300)
bias(actual_sales,predicted_sales)


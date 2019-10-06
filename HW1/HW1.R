rm(list=ls())

library(babynames)
library(mdsr)
library(dplyr)
library(Hmisc)
library(scales)
library(quantreg)
library(ggplot2)

baby_data <- make_babynames_dist()
baby_data
str(baby_data)
colnames(baby_data)


### 1. age distribution of American boys named Joseph
# number of Josephs born each year
joseph <- filter(baby_data, name == "Joseph", sex == "M")

# median age and number of joseph
alive_n.median <- with(joseph, wtd.quantile(year, est_alive_today, prob=0.5))
alive_n.median

# plot the number of joseph
joseph_num <- ggplot(data=joseph) +
  geom_line(mapping = aes(x=year, y=n/1000)) +
  labs(title = "Age Distribution of American Boys Named Joseph",
       subtitle = "By year of birth") +
  geom_text(x = 1940, y = 34, label = "Number of Josephs\nborn each year") 

# plot the number of joseph alive
joseph_num <- joseph_num +
  geom_bar(mapping = aes(x=year, y=est_alive_today/1000), 
           stat="identity", fill="skyblue3", width=0.5) +
  geom_text(x = 1915, y = 15, 
            label = "Number of Josephs\nborn each year\nestimated to be alive\non 1/1/2014",
            colour = "skyblue3")

# highlight the median 
joseph_num <- joseph_num +
  geom_bar(mapping = aes(x=alive_n.median, y=ifelse(year == alive_n.median, est_alive_today/1000, 0)), 
           stat="identity", fill="dodgerblue3", width=0.7) +
  coord_cartesian(ylim=c(0, 40), expand=TRUE) +
  geom_text(x = 1990, y = 37,
            label = "The median living Joseph\nis 37 years old",
            colour = "dodgerblue3") +
  geom_curve(x = 1990, xend = 1975, y = 34, yend = 24,
             arrow = arrow(length = unit(0.3,"cm")), curvature = 0.5) + ylim(0, 42)

joseph_num



### 2. median ages for males with the 25 most common names
rm(list=ls())
baby_data <- make_babynames_dist()
colnames(baby_data)

# top 25 names
male <- baby_data %>% filter(sex == "M")
top25_male <- male %>%
  group_by(name) %>%
  summarise(N=sum(n)) %>%
  arrange(desc(N)) %>%
  head(25)

# top 25 names
male <- male %>%
  filter(name %in% top25_male$name)

# make Q1, Q2, Q3 age
colnames(male)
male_quant <- male %>%
  group_by(name) %>%
  summarise(q1_age = wtd.quantile(age_today, est_alive_today, probs = 0.25),
            med_age = wtd.quantile(age_today, est_alive_today, probs = 0.5),
            q3_age = wtd.quantile(age_today, est_alive_today, probs = 0.75))

# make plot
top25_male_plot <- ggplot(male_quant, 
                          aes(x = reorder(name, - med_age), y = med_age)) +
  xlab(NULL) +
  ylab("Age (in years)") +
  labs(title = "Median Ages For Males with the 25 Most Common Names",
       subtitle = "Among Americans estimated to be alive as of Jan. 1, 2014") 
top25_male_plot

# add Q1 - Q3 boxplot
top25_male_plot <- top25_male_plot +
  geom_linerange(aes(ymin = q1_age, ymax = q3_age),
                 color = "skyblue3", size = 8, alpha = 0.8)
top25_male_plot


# add median point and flip the coordinate
top25_male_plot <- top25_male_plot +
  geom_point(fill = "#ed3324", color = "white", size = 3, shape = 21) +
  coord_flip()

top25_male_plot

# add q1, q3 text and flip the coordinate
top25_male_plot <- top25_male_plot +
  geom_point(aes(y = 55, x = 24), fill = "#ed3324", colour = "white",
             size = 3, shape = 21) +
  geom_text(aes(y = 58, x = 24, label = "median")) +
  geom_text(aes(y = 28, x = 17, label = "25th")) +
  geom_text(aes(y = 51, x = 17, label = "75th percentile")) +
  geom_point(aes(y = 25, x = 17), shape = 17) +
  geom_point(aes(y = 55, x = 17), shape = 17)

top25_male_plot

top25_male_plot <- top25_male_plot + 
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color = "grey")) +
  scale_y_continuous(minor_breaks = seq(20, 60, 10),
                     breaks = seq(20, 60, 10),
                     labels = c('20 years old', '30', '40', '50', '60'),
                     position = 'right')
top25_male_plot
#   summarise(N = n(), 
#             est_num_alive = sum(est_alive_today),
#             q1_age = wtd.quantile(age_today, est_alive_today, probs = 0.25),
#             med_age = wtd.quantile(age_today, est_alive_today, probs = 0.5),
#             q3_age = wtd.quantile(age_today,est_alive_today,  probs = 0.75)) %>%
#   arrange(desc(est_num_alive)) %>%
#   head(25)
# 
# male <- baby_data %>%
#   filter(sex == "M") %>%
#   group_by(name) %>%
#   rq(age_today ~ 1., tau=c(0.25, 0.5, 0.75), weight = est_alive_today) %>%
#   summary()


############ By ME ############
# male <- baby_data %>%
#   filter(sex=="M") %>%
#   arrange(desc(year), desc(n))
# top25_male_name <- head(male, 25)$name; top25_male_name
# 
# theme_set(theme_bw())
# 
# male_25 <- male %>% 
#   group_by(name) %>%
#   filter(name %in% top25_male_name) %>%
#   mutate(age = 2014 - year) %>%
#   select(age, name, alive_prob) # 2010년 기준 살아있는 사람 숫자
# 
# male_25 <- male_25 %>%
#   group_by(name) %>%
#   mutate("1qt" = wtd.quantile(age, prob=0.25),
#          "med" = wtd.quantile(age, prob=0.5),
#          "3qt" = wtd.quantile(age, prob=0.75)) %>%
#   select(name, "1qt", med, "3qt") %>%
#   unique()
# 
# male_plot <- ggplot(data = male_25) +
#   geom_pointrange(mapping = aes(x = name, y=age),
#                   stat = "summary",)


library(readr)
library(tidyverse)
library(ggrepel)
library(ggthemes)

density <- read_csv("R/Covid/population-density.csv") #https://ourworldindata.org/most-densely-populated-countries
covid <- read_csv("R/Covid/owid-covid-data.csv") #https://ourworldindata.org/coronavirus
obesity <- read_csv("R/Covid/obesity-in-men-vs-obesity-in-women.csv") #https://ourworldindata.org/obesity
country <- read_csv("R/Covid/country-and-continent-codes-list-csv.csv") #https://datahub.io/JohnSnowLabs/country-and-continent-codes-list
political <- read_csv("R/Covid/political.csv") #https://ourworldindata.org/democracy
age <- read_csv("R/Covid/median-age.csv") #https://ourworldindata.org/age-structure 

data <- covid %>% filter(date=="2020-5-5") %>% left_join(density %>% filter(Year==2017), by=(c("location"="Entity"))) %>%
  left_join(obesity %>% filter(Year==2016), by=(c("location"="Entity"))) %>%
  rename("pop_density"="Population density (people per sq. km of land area) (people per km² of land area)",
         "obese_men"="Share of men who are obese (%)", "obese_women"="Share of women who are obese (%)") %>%
  mutate(obesity_rate=(obese_men+obese_women)/2) %>% left_join(country, by=c("Code.x"="Three_Letter_Country_Code")) %>%
  left_join(political %>% filter(Year==2015),by=(c("location"="Entity"))) %>% 
  left_join(age %>% filter(Year==2015),by=(c("location"="Entity"))) %>% rename("political_score"="Political Regime (OWID based on Polity IV and Wimmer & Min) (Score)",
                                                                               "median_age"="UN Population Division (Median Age) (2017) (years)")

#obesity chart
ggplot(data %>% filter(total_cases>500), aes(x=obesity_rate, y=total_deaths_per_million, label=location)) + geom_point(size=3, alpha=0.5) +
  theme_minimal() + geom_text_repel(data=data %>% filter(total_deaths_per_million > 60), size=5, nudge_y = 20) +
  labs(title="A big problem?", subtitle="Obesity rates and Covid deaths per million", x="Obesity rate", y="Total deaths per million") +
  ylim(c(0,750)) + geom_smooth(se=F, col="orange") +
  theme(plot.title = element_text(size=30, 
                                  vjust=1, family="Impact"))
ggsave("obesity_covid.png", width=15)

#democracy
ggplot(data, aes(x=political_score, y=total_deaths_per_million, label=location)) + geom_jitter(size=3, alpha=0.5) +
  theme_minimal() + geom_text_repel(data=data %>% filter(total_deaths_per_million > 60), segment.color = NA, size=3, nudge_y = 20) +
  labs(title="Democracy kills?", subtitle="Political regime score and Covid deaths per million. The scale goes from -10 (full autocracy) to 10 (full democracy).", 
       x="Political regime (OWID)", y="Total deaths per million", caption="ource data: Our World in Data. Visualisation by @lauriejhop") +
  ylim(c(0,750)) + geom_smooth(se=F, col="orange") +
  theme(plot.title = element_text(size=30, 
                                  vjust=1, family="Impact"))
ggsave("political_covid.png", width=15)

#age plots
ggplot(data %>% filter(total_deaths>0), aes(x=median_age, y=log(total_deaths_per_million), label=location)) + 
  geom_point(size=3, alpha=0.5) +
  theme_minimal() + geom_text(size=3, nudge_y = 1) +
  labs(title="The Age of Covid-19", subtitle="Median age and Covid deaths per million by country.", 
       x="Median age (2015)", y="Total deaths per million (log)", caption="ource data: Our World in Data. Visualisation by @lauriejhop") +
  geom_smooth(se=F, col="orange") +
  theme(plot.title = element_text(size=30, 
                                  vjust=1, family="Impact"))
ggsave("age_covid.png", width=15)

ggplot(data %>% filter(total_deaths>0, location !="World"), aes(x=median_age, y=log(total_deaths_per_million), size=`Total population (Gapminder)`, label=location)) +
  theme_minimal() + geom_text( alpha=0.8) +
  scale_size_continuous(range = c(1, 12)) +
  labs(title="The Age of Covid-19", subtitle="Median age of population and Covid deaths per million by country.", 
       x="Median age (2015)", y="Total deaths per million (log)", caption="Source data: Our World in Data. Visualisation by @lauriejhop") +
  geom_smooth(se=F, col="orange", method="lm") +
  theme(plot.title = element_text(size=30, 
                                  vjust=1, family="Impact"),
        legend.position="none")
ggsave("age_covid_names.png", width=15)

#continent plot
ggplot(data %>% filter(!is.na(Continent_Name), iso_code !="MSR", total_deaths>5), aes(y=total_deaths_per_million, x=1, label=location, size=`Total population (Gapminder)`, col=Continent_Name)) + 
  geom_jitter(alpha=0.5, position=pos) +
  scale_size_continuous(range = c(1, 12)) +
  theme_clean()+ 
  labs(title="Democracy? Mobility? Obesity? Age? Better reporting? What's happened in the West?", 
       subtitle="Coronavirus deaths per million by country and continent", x="", y="Total deaths per million",
       caption="Source data: Our World in Data. Size of bubble represents population. Visualisation by @lauriejhop") +
  ylim(c(0,750)) + xlim(c(0.5,1.5)) + 
  scale_color_brewer(palette="Dark2") + facet_wrap(~Continent_Name, nrow=1, strip.position = "bottom")+
  theme(plot.title = element_text(size=22, 
                                  vjust=1, family="Impact")) + 
  theme(legend.position = "none",
                                                                     axis.text.x = element_blank(),
                                                                     axis.ticks.x= element_blank(),
                                                                      strip.text.x = element_text(size=12))
ggsave("westsidestory.png", dpi="retina", width=14)

#linear model
lm_covid <- lm(log(total_deaths_per_million) ~ median_age + obesity_rate + political_score,data=data %>% filter(total_deaths>0))
summary(lm_covid, robust=T)

#partial regression
#Create the residuals (same as in your question)
lm_covid2 <- lm(log(total_deaths_per_million) ~ obesity_rate + political_score,data=data %>% filter(total_deaths>0))
resid.model1 <- residuals(lm_covid)
resid.model2b <- residuals(lm_covid2)

#Create new data frame containing those residuals
NEWDF <- data.frame(RES1 = resid.model1, RES2b = resid.model2b)

#Now generate your plot using this NEWDF as your data
library(ggplot2);
ggplot(data = NEWDF, aes(x = resid.model2b, y = resid.model1)) + geom_point(na.rm=T);
ggsave("partialregression.png")













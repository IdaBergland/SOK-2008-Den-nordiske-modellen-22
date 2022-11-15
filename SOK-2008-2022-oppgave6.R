rm(list=ls())
library(tidyverse)
library(lubridate)
library(rjstat)
library(janitor)
library(gdata)
library(httr) 
library(PxWebApiData)
library(ggplot2)
library(dplyr)
library(patchwork)
library(cowplot)

url <- "https://data.ssb.no/api/v0/no/table/12441/"

query <- '{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "item",
        "values": [
          "00-99"
        ]
      }
    },
    {
      "code": "Sykefraver2",
      "selection": {
        "filter": "item",
        "values": [
          "Alt"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2005",
          "2006",
          "2007",
          "2008",
          "2009",
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'

sykefravær <- url %>%
  POST(body = query, encode = "json")

sykefravær <-  sykefravær %>%
  content("text") %>%
  fromJSONstat() %>%
  as_tibble()


url2 <- "https://data.ssb.no/api/v0/no/table/05111/"

query2 <- '{
  "query": [
    {
      "code": "ArbStyrkStatus",
      "selection": {
        "filter": "item",
        "values": [
          "2"
        ]
      }
    },
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "15-74"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Prosent"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2005",
          "2006",
          "2007",
          "2008",
          "2009",
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'

arbeidsledig <- url2 %>%
  POST(body = query2, encode = "json")

arbeidsledig <-  arbeidsledig %>%
  content("text") %>%
  fromJSONstat() %>%
  as_tibble()


# Sette sammen de to datasettene
hen <- merge(sykefravær,arbeidsledig, by = c("år", "kjønn"))

#Dame og menn hver for seg og få år som tall og ikke character.
kjerringe <- hen %>% filter(kjønn == "Kvinner") 
gubber <- hen %>% filter(kjønn == "Menn")
kjerringe$år <- as.numeric(as.character(kjerringe$år))
gubber$år <- as.numeric(as.character(gubber$år))

#Så må vi sette en koffesient 
koff = 1.5
# Lage plot for gubbene for seg og kjerringen førr seg
gubber2 <- gubber %>% 
  ggplot(aes(x=år, y=value.y)) +
  geom_line(color = "blue4") +
  geom_line(aes(y = value.x/koff), color = "darkgreen") +
  scale_x_continuous(breaks = 2005:2019)+
  scale_y_continuous("Prosent Arbeidsledighet",
                     sec.axis = sec_axis
                     (~.*koff, name = "Prosent Sykefravær")) +
  ggtitle("Menns sykefravær 
          og arbeidsledighet") +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  xlab("År")

kjerringe2 <- kjerringe %>% 
  ggplot(aes(x=år, y=value.y)) +
  geom_line(color = "coral3") +
  geom_line(aes(y = value.x/koff), color = "cyan2") +
  scale_x_continuous(breaks = 2005:2019)+
  scale_y_continuous("Prosent Arbeidsledige",
                     sec.axis = sec_axis
                     (~.*koff,name ="Prosent Sykefravær"))+
  ggtitle("Kvinners sykefravær
          og Arbeidsledighet") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("År")
kjerringe2

plot_grid(gubber2, kjerringe2, ncol = 2)


#Data er hentet fra SSB tabell 12441 og tabell 05111

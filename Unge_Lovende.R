#######   Unge lovende   #######
#  1) Hvilke nøkkeltall vil du trekke frem for å oppsummere Unge Lovende i NRK TV? 
#     Visualiser der det er nyttig.
# 2) Alle episodene av Unge Lovende ble sluppet samtidig. Hva kan du si om bingewatching knyttet til slippet
#    av Unge Lovende? Visualiser.



##  --------  Laster nyttige pakker  -------- ## 
library(readr,Rcpp) # lese csv
library(dplyr)      # databehandling
library(ggplot2)    # visualisering


##  -------- Laster datafil -------- ## 
x <- read_csv("unge-lovende.csv")


## Printer grunnleggende info om datasett
head(x,5)
dim(x)
str(x)


##  -------- Prosessering -------- ## 
# Trekk ut episodenummer fra program ID (3'dje siste siffer)
x$Episode_Nr=substr(x$programId,nchar(x$programId)-2,nchar(x$programId)-2)

# Transformerer fra millisekund til minutt
x$timeWithinVisit_Minutes=x$timeWithinVisit/(1000*60)  

## Datovariabler: gjør om fra unix timestamp til tolkbare kolonner (dato, ukedag, time)
x$visitStartTime_DateTime=as.POSIXct(x$visitStartTime, origin="1970-01-01",tz="Etc/GMT+1")
x$visitStartTime_WDay=format(as.POSIXct(x$visitStartTime_DateTime,format="%H:%M:%S"),format="%A")
x$visitStartTime_Hour=format(as.POSIXct(x$visitStartTime_DateTime,format="%H:%M:%S"),"%H")
x$visitStartTime_Date=format(as.POSIXct(x$visitStartTime_DateTime,format="%H:%M:%S"),"%y/%m/%d")


##  -------- Forståelse av dataen -------- ## 
# Funksjon for å trekke ut data om tilfeldige userIds
get_x_sample <- function(x,n_unique_userIds) {
  userId_Uniques_n=n_distinct(x$userId)    # finn antall unike userIds.
  userId_sample=unique(x$userId) [ sample(1:userId_Uniques_n, n_unique_userIds) ] # velg n unique userIds
  x_sample=x[x$userId %in%  (userId_sample),]
  return(x_sample)
}

# Visualiserer sammenheng mellom UserId,  VisitStart, 
# episode_Nr og timeWithinVisit_Minutes for tilfeldige userIds
set.seed(123)
sample_size=5
x_sample=get_x_sample(x,sample_size)
ggplot(data = x_sample, aes(x = visitStartTime_DateTime, size=timeWithinVisit_Minutes,  
                            y= Episode_Nr, color=factor(visitStartTime_Date))) +
  geom_point() + facet_grid(rows = vars(userId)) + 
  labs(title = "Visualisering av data for utvalgte userIds",
       subtitle = "Farge avhenger av dagen sesjonen ble startet (visitStartTime).")

#Kommentar: figuren illustrerer at enkelte brukere ser flere episoder samme dag, andre ser kun 1,...


##  -------- Nøkkeltall på serie-nivå -------- ## 
x$Serie="Unge lovende"
KPI=x %>% group_by(Serie) %>% 
  summarise(
    episoder_n=n_distinct(x$programId),
    visninger_unike=dim(x)[1],
    userId_unike=n_distinct(x$userId),
    visning_pr_user_id=visninger_unike/userId_unike,
    Dag_1=names(sort(table(x$visitStartTime_WDay),decreasing=TRUE)[1:1])
  )
KPI  %>%  kable %>%kable_styling()  # output i viewer til høyre


##  -------- Nøkkeltall på userId-nivå -------- ## 
KPI_UserId_Niv=x %>% group_by(userId) %>% 
  summarise(
    episode_forste=min(Episode_Nr),
    episode_siste=max(Episode_Nr),
    episode_unike=n_distinct(Episode_Nr),
    sesjoner_unike=n_distinct(visitStartTime),
    sesjoner_unikeDager=n_distinct(visitStartTime_Date),
    BW_flg=  ( n_distinct(Episode_Nr)  /  n_distinct(visitStartTime_Date) ) >= 3  # indikator for "Binge Watching":
    #userId ser i snitt minst 3 episoder pr dag han har sett unge lovende 
  ) 

KPI_UserId_Niv[1:4,] %>%  kable %>%kable_styling()
#Koden under kan benyttes for å se på rådataen for de 4 userId'ene det vises nøkkeltall for  
#   valid_ids=KPI_UserId_Niv$userId[1:4]
#   valid_data=x[x$userId %in% valid_ids,]
#   View(valid_data)


##  -------- Nøkkeltall: aggregerer fra userId-nivå til serie-nivå for å finne nøkkeltall a)-d) -------- ## 
#a)	Andel userId som har sett alle episodene,
#b)	Andel userId som har sett episode nr. 1, men ikke flere
#c)	Andel "binge watchere"
#d)	Median-antall episoder userIder ser 

# a)
KPI$sett_alle_episoder_andel=
  round( 
    sum(KPI_UserId_Niv$episode_unike == KPI$episoder_n) /
      KPI$userId_unike 
    ,2)
# b)
KPI$churn_etter_1_episode= 
  round( 
    sum(KPI_UserId_Niv$episode_siste==1 ) / 
      sum(KPI_UserId_Niv$episode_forste==1 )
    ,2)

# c) 
KPI$BW_andel =round( mean(KPI_UserId_Niv$BW_flg) ,2)

# d) 
KPI$episode_unike_median = median(KPI_UserId_Niv$episode_unike)

KPI  %>%  kable %>%kable_styling()      # output i viewer til høyre

##  -------- Bingewatching  -------- ##
# Plotter andelen sesjoner gjennomført av hhv. "binge-watchere" og "ikke-bingewatchere"

# Starter med x, left join KPI'er på userID-nivå
x_kpi_merged=merge(x = x, y = KPI_UserId_Niv, by = "userId", all.x = TRUE)
head(x_kpi_merged)

# Andel sesjoner gjennomført av Binge-watchere og ikke bingewatchere
table(x_kpi_merged$BW_flg) / dim(x_kpi_merged)[1]


# Visualiserer antall sesjoner  for hhv. "Binge-watchere" VS ikke-"Binge-watchere"
ggplot(x_kpi_merged,aes(x=BW_flg,fill=BW_flg)) + geom_bar()+theme_minimal() +ylab("Antall sesjoner") 
#Tolkning: overvekten av sesjoner er gjennomført av UserIds av typen "Binge-watchere"

# Visualiserer antall sesjoner pr uke for hhv. "Binge-watchere" VS ikke-"Binge-watchere"
x_kpi_merged$visitStartTime_Uke=
  strftime(x_kpi_merged$visitStartTime_Date,format="%W") 
ggplot(x_kpi_merged,aes(x=visitStartTime_Uke,fill=BW_flg)) + geom_bar() +theme_minimal() +ylab("Antall sesjoner")



#library(prophet)

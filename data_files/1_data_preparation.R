

# LOAD DATA ####
library(lme4)
library(MASS)
library(readstata13)

rm(list = ls())
# setwd("~")
load("data/stacked_raw_data.RData")





dffin$coalition_partners_now <- NA
dffin$coalition_partners_now[dffin$country == "Aus"] <- 0
dffin$coalition_partners_now[dffin$country == "Aus" & dffin$cleanstartdate < as.Date("2019-06-03") & dffin$Q52 %in% c("Österreichische Volkspartei (ÖVP)") & dffin$partydrawn %in% "Freiheitliche Partei Österreichs (FPÖ)"] <- 1
dffin$coalition_partners_now[dffin$country == "Aus" & dffin$cleanstartdate < as.Date("2019-06-03") & dffin$Q52 %in% c("Freiheitliche Partei Österreichs (FPÖ)") & dffin$partydrawn %in% "Österreichische Volkspartei (ÖVP)"] <- 1

dffin$coalition_partners_now[dffin$country == "Bel"] <- 0
dffin$coalition_partners_now[dffin$country == "Bel" & dffin$Q52 %in% c("Mouvement Réformateur (MR)") & dffin$partydrawn %in% c("Christen-Democratisch en Vlaams (CD&V)", "Open Vlaamse Liberalen en Democraten (Open Vld)")] <- 1
dffin$coalition_partners_now[dffin$country == "Bel" & dffin$Q52 %in% c("Christen-Democratisch en Vlaams (CD&V)") & dffin$partydrawn %in% c("Mouvement Réformateur (MR)", "Open Vlaamse Liberalen en Democraten (Open Vld)")] <- 1
dffin$coalition_partners_now[dffin$country == "Bel" & dffin$Q52 %in% c("Open Vlaamse Liberalen en Democraten (Open Vld)") & dffin$partydrawn %in% c("Christen-Democratisch en Vlaams (CD&V)","Mouvement Réformateur (MR)")] <- 1

dffin$coalition_partners_now[dffin$country == "Bul"] <- 0
dffin$coalition_partners_now[dffin$country == "Bul" & dffin$Q52 %in% c("Граждани за европейско развитие на България (ГЕРБ)") & dffin$partydrawn %in% c("Национален фронт за спасение на България (НФСБ)")] <- 1
dffin$coalition_partners_now[dffin$country == "Bul" & dffin$Q52 %in% c("Национален фронт за спасение на България (НФСБ)") & dffin$partydrawn %in% c("Граждани за европейско развитие на България (ГЕРБ)")] <- 1

dffin$coalition_partners_now[dffin$country == "Cro"] <- 0
dffin$coalition_partners_now[dffin$country == "Cro" & dffin$Q52 %in% c("Hrvatska demokratska zajednica (HDZ)") & dffin$partydrawn %in% c("Hrvatska narodna stranka – Liberalni demokrati (HNS)")] <- 1
dffin$coalition_partners_now[dffin$country == "Cro" & dffin$Q52 %in% c("Hrvatska narodna stranka – Liberalni demokrati (HNS)") & dffin$partydrawn %in% c("Hrvatska demokratska zajednica (HDZ)")] <- 1

dffin$coalition_partners_now[dffin$country == "Cze"] <- 0
dffin$coalition_partners_now[dffin$country == "Cze" & dffin$Q52 %in% c("ANO 2011 (ANO)") & dffin$partydrawn %in% c("Česká strana sociálně demokratická (ČSSD)")] <- 1
dffin$coalition_partners_now[dffin$country == "Cze" & dffin$Q52 %in% c("Česká strana sociálně demokratická (ČSSD)") & dffin$partydrawn %in% c("ANO 2011 (ANO)")] <- 1

dffin$coalition_partners_now[dffin$country == "Den"] <- 0
dffin$coalition_partners_now[dffin$country == "Den" & dffin$cleanstartdate < as.Date("2019-06-27") & dffin$Q52 %in% c("Venstre (V)") & dffin$partydrawn %in% c("Liberal Alliance (I)","Det Konservative Folkeparti (C)")] <- 1
dffin$coalition_partners_now[dffin$country == "Den" & dffin$cleanstartdate < as.Date("2019-06-27") & dffin$Q52 %in% c("Liberal Alliance (I)") & dffin$partydrawn %in% c("Venstre (V)","Det Konservative Folkeparti (C)")] <- 1
dffin$coalition_partners_now[dffin$country == "Den" & dffin$cleanstartdate < as.Date("2019-06-27") & dffin$Q52 %in% c("Det Konservative Folkeparti (C)") & dffin$partydrawn %in% c("Liberal Alliance (I)","Venstre (V)")] <- 1

dffin$coalition_partners_now[dffin$country == "Est"] <- 0
dffin$coalition_partners_now[dffin$country == "Est" & dffin$Q52 %in% c("Eesti Keskerakond (KE)") & dffin$partydrawn %in% c("Eesti Konservatiivne Rahvaerakond (EKRE)","Isamaa (I)")] <- 1
dffin$coalition_partners_now[dffin$country == "Est" & dffin$Q52 %in% c("Eesti Konservatiivne Rahvaerakond (EKRE)") & dffin$partydrawn %in% c("Eesti Keskerakond (KE)","Isamaa (I)")] <- 1
dffin$coalition_partners_now[dffin$country == "Est" & dffin$Q52 %in% c("Isamaa (I)") & dffin$partydrawn %in% c("Eesti Konservatiivne Rahvaerakond (EKRE)","Eesti Keskerakond (KE)")] <- 1

dffin$coalition_partners_now[dffin$country == "Fin"] <- 0
dffin$coalition_partners_now[dffin$country == "Fin" & dffin$cleanstartdate < as.Date("2019-06-06") & dffin$Q52 %in% c("Suomen Keskusta (KESK)") & dffin$partydrawn %in% c("Kansallinen Kokoomus (KOK)","Sininen tulevaisuus (SIN)")] <- 1
dffin$coalition_partners_now[dffin$country == "Fin" & dffin$cleanstartdate < as.Date("2019-06-06") & dffin$Q52 %in% c("Kansallinen Kokoomus (KOK)") & dffin$partydrawn %in% c("Suomen Keskusta (KESK)","Sininen tulevaisuus (SIN)")] <- 1
dffin$coalition_partners_now[dffin$country == "Fin" & dffin$cleanstartdate < as.Date("2019-06-06") & dffin$Q52 %in% c("Sininen tulevaisuus (SIN)") & dffin$partydrawn %in% c("Kansallinen Kokoomus (KOK)","Suomen Keskusta (KESK)")] <- 1
dffin$coalition_partners_now[dffin$country == "Fin" & dffin$cleanstartdate >= as.Date("2019-06-06") & dffin$Q52 %in% c("Suomen Sosialidemokraattinen Puolue (SDP)") & dffin$partydrawn %in% c("Suomen Keskusta (KESK)","Vihreä liitto (VIHR)","Vasemmistoliitto (VAS)","Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))")] <- 1
dffin$coalition_partners_now[dffin$country == "Fin" & dffin$cleanstartdate >= as.Date("2019-06-06") & dffin$Q52 %in% c("Suomen Keskusta (KESK)") & dffin$partydrawn %in% c("Suomen Sosialidemokraattinen Puolue (SDP)","Vihreä liitto (VIHR)","Vasemmistoliitto (VAS)","Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))")] <- 1
dffin$coalition_partners_now[dffin$country == "Fin" & dffin$cleanstartdate >= as.Date("2019-06-06") & dffin$Q52 %in% c("Vihreä liitto (VIHR)") & dffin$partydrawn %in% c("Suomen Keskusta (KESK)","Suomen Sosialidemokraattinen Puolue (SDP)","Vasemmistoliitto (VAS)","Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))")] <- 1
dffin$coalition_partners_now[dffin$country == "Fin" & dffin$cleanstartdate >= as.Date("2019-06-06") & dffin$Q52 %in% c("Vasemmistoliitto (VAS)") & dffin$partydrawn %in% c("Suomen Keskusta (KESK)","Vihreä liitto (VIHR)","Suomen Sosialidemokraattinen Puolue (SDP)","Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))")] <- 1
dffin$coalition_partners_now[dffin$country == "Fin" & dffin$cleanstartdate >= as.Date("2019-06-06") & dffin$Q52 %in% c("Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))") & dffin$partydrawn %in% c("Suomen Keskusta (KESK)","Vihreä liitto (VIHR)","Vasemmistoliitto (VAS)","Suomen Sosialidemokraattinen Puolue (SDP)")] <- 1

dffin$coalition_partners_now[dffin$country == "Fra"] <- 0 
# PRG is missing
dffin$coalition_partners_now[dffin$country == "Fra" & dffin$Q52 %in% c("La République En Marche (REM)") & dffin$partydrawn %in% c("Les Républicains (LR)","Mouvement Démocrate (MoDem)")] <- 1
dffin$coalition_partners_now[dffin$country == "Fra" & dffin$Q52 %in% c("Les Républicains (LR)") & dffin$partydrawn %in% c("La République En Marche (REM)","Mouvement Démocrate (MoDem)")] <- 1
dffin$coalition_partners_now[dffin$country == "Fra" & dffin$Q52 %in% c("Mouvement Démocrate (MoDem)") & dffin$partydrawn %in% c("Les Républicains (LR)","La République En Marche (REM)")] <- 1

dffin$coalition_partners_now[dffin$country == "Ger"] <- 0 
dffin$coalition_partners_now[dffin$country == "Ger" & dffin$Q52 %in% c("Christlich Demokratische Union (CDU)") & dffin$partydrawn %in% c("Christlich Soziale Union (CSU)","Sozialdemokratische Partei Deutschlands (SPD)")] <- 1
dffin$coalition_partners_now[dffin$country == "Ger" & dffin$Q52 %in% c("Christlich Soziale Union (CSU)") & dffin$partydrawn %in% c("Christlich Demokratische Union (CDU)","Sozialdemokratische Partei Deutschlands (SPD)")] <- 1
dffin$coalition_partners_now[dffin$country == "Ger" & dffin$Q52 %in% c("Sozialdemokratische Partei Deutschlands (SPD)") & dffin$partydrawn %in% c("Christlich Demokratische Union (CDU)","Christlich Soziale Union (CSU)")] <- 1

dffin$coalition_partners_now[dffin$country == "Gre"] <- 0 

dffin$coalition_partners_now[dffin$country == "Hun"] <- 0 
dffin$coalition_partners_now[dffin$country == "Hun" & dffin$Q52 %in% c("Fidesz – Magyar Polgári Szövetség (FIDESZ)") & dffin$partydrawn %in% c("Kereszténydemokrata Néppárt (KDNP)")] <- 1
dffin$coalition_partners_now[dffin$country == "Hun" & dffin$Q52 %in% c("Kereszténydemokrata Néppárt (KDNP)") & dffin$partydrawn %in% c("Fidesz – Magyar Polgári Szövetség (FIDESZ)")] <- 1

dffin$coalition_partners_now[dffin$country == "Ire"] <- 0 
dffin$coalition_partners_now[dffin$country == "Ire" & dffin$Q52 %in% c("Fine Gael (FG)") & dffin$partydrawn %in% c("Independent Alliance")] <- 1
dffin$coalition_partners_now[dffin$country == "Ire" & dffin$Q52 %in% c("Independent Alliance") & dffin$partydrawn %in% c("Fine Gael (FG)")] <- 1

dffin$coalition_partners_now[dffin$country == "Ita"] <- 0 
dffin$coalition_partners_now[dffin$country == "Ita" & dffin$Q52 %in% c("Movimento 5 Stelle (M5S)") & dffin$partydrawn %in% c("Lega (Lega Salvini Premier)")] <- 1
dffin$coalition_partners_now[dffin$country == "Ita" & dffin$Q52 %in% c("Lega (Lega Salvini Premier)") & dffin$partydrawn %in% c("Movimento 5 Stelle (M5S) ")] <- 1

dffin$coalition_partners_now[dffin$country == "Lat"] <- 0 
dffin$coalition_partners_now[dffin$country == "Lat" & dffin$Q52 %in% c("Jaunā konservatīvā partija (JKP)") & dffin$partydrawn %in% c("'Kam pieder valsts' (KPV LV)","Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)","Latvijas attīstībai (LA)","Vienotība (V)")] <- 1
dffin$coalition_partners_now[dffin$country == "Lat" & dffin$Q52 %in% c("'Kam pieder valsts' (KPV LV)") & dffin$partydrawn %in% c("Jaunā konservatīvā partija (JKP)","Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)","Latvijas attīstībai (LA)","Vienotība (V)")] <- 1
dffin$coalition_partners_now[dffin$country == "Lat" & dffin$Q52 %in% c("Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)") & dffin$partydrawn %in% c("'Kam pieder valsts' (KPV LV)","Jaunā konservatīvā partija (JKP)","Latvijas attīstībai (LA)","Vienotība (V)")] <- 1
dffin$coalition_partners_now[dffin$country == "Lat" & dffin$Q52 %in% c("Latvijas attīstībai (LA)") & dffin$partydrawn %in% c("'Kam pieder valsts' (KPV LV)","Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)","Jaunā konservatīvā partija (JKP)","Vienotība (V)")] <- 1
dffin$coalition_partners_now[dffin$country == "Lat" & dffin$Q52 %in% c("Vienotība (V)") & dffin$partydrawn %in% c("'Kam pieder valsts' (KPV LV)","Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)","Latvijas attīstībai (LA)","Jaunā konservatīvā partija (JKP)")] <- 1

dffin$coalition_partners_now[dffin$country == "Lit"] <- 0
dffin$coalition_partners_now[dffin$country == "Lit" & dffin$cleanstartdate < as.Date("2019-07-23") & dffin$Q52 %in% c("Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)") & dffin$partydrawn %in% c("Lietuvos socialdemokratų darbo partija (LSDDP)")] <- 1
dffin$coalition_partners_now[dffin$country == "Lit" & dffin$cleanstartdate < as.Date("2019-07-23") & dffin$Q52 %in% c("Lietuvos socialdemokratų darbo partija (LSDDP)") & dffin$partydrawn %in% c("Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)")] <- 1
dffin$coalition_partners_now[dffin$country == "Lit" & dffin$cleanstartdate >= as.Date("2019-07-23") & dffin$Q52 %in% c("Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)") & dffin$partydrawn %in% c("Lietuvos socialdemokratų darbo partija (LSDDP)","Lietuvos lenkų rinkimų akcija – Krikščioniškų šeimų sąjunga (LLRA–KŠS)","Tvarka ir teisingumas (TT)")] <- 1
dffin$coalition_partners_now[dffin$country == "Lit" & dffin$cleanstartdate >= as.Date("2019-07-23") & dffin$Q52 %in% c("Lietuvos socialdemokratų darbo partija (LSDDP)") & dffin$partydrawn %in% c("Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)","Lietuvos lenkų rinkimų akcija – Krikščioniškų šeimų sąjunga (LLRA–KŠS)","Tvarka ir teisingumas (TT)")] <- 1
dffin$coalition_partners_now[dffin$country == "Lit" & dffin$cleanstartdate >= as.Date("2019-07-23") & dffin$Q52 %in% c("Lietuvos lenkų rinkimų akcija – Krikščioniškų šeimų sąjunga (LLRA–KŠS)") & dffin$partydrawn %in% c("Lietuvos socialdemokratų darbo partija (LSDDP)","Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)","Tvarka ir teisingumas (TT)")] <- 1
dffin$coalition_partners_now[dffin$country == "Lit" & dffin$cleanstartdate >= as.Date("2019-07-23") & dffin$Q52 %in% c("Tvarka ir teisingumas (TT)") & dffin$partydrawn %in% c("Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)","Lietuvos lenkų rinkimų akcija – Krikščioniškų šeimų sąjunga (LLRA–KŠS)","Lietuvos socialdemokratų darbo partija (LSDDP)")] <- 1

dffin$coalition_partners_now[dffin$country == "Net"] <- 0
dffin$coalition_partners_now[dffin$country == "Net" & dffin$Q52 %in% c("Volkspartij voor Vrijheid en Democratie (VVD)") & dffin$partydrawn %in% c("Christen-Democratisch Appèl (CDA)","Democraten 66 (D66)","ChristenUnie (CU)")] <- 1
dffin$coalition_partners_now[dffin$country == "Net" & dffin$Q52 %in% c("Christen-Democratisch Appèl (CDA)") & dffin$partydrawn %in% c("Volkspartij voor Vrijheid en Democratie (VVD)","Democraten 66 (D66)","ChristenUnie (CU)")] <- 1
dffin$coalition_partners_now[dffin$country == "Net" & dffin$Q52 %in% c("Democraten 66 (D66)") & dffin$partydrawn %in% c("Christen-Democratisch Appèl (CDA)","Volkspartij voor Vrijheid en Democratie (VVD)","ChristenUnie (CU)")] <- 1
dffin$coalition_partners_now[dffin$country == "Net" & dffin$Q52 %in% c("ChristenUnie (CU)") & dffin$partydrawn %in% c("Christen-Democratisch Appèl (CDA)","Democraten 66 (D66)","Volkspartij voor Vrijheid en Democratie (VVD)")] <- 1

dffin$coalition_partners_now[dffin$country == "Pol"] <- 0

dffin$coalition_partners_now[dffin$country == "Por"] <- 0

dffin$coalition_partners_now[dffin$country == "Rom"] <- 0
dffin$coalition_partners_now[dffin$country == "Rom" & dffin$Q52 %in% c("Partidul Social Democrat (PSD)") & dffin$partydrawn %in% c("Alianța Liberalilor și Democraților (ALDE)")] <- 1
dffin$coalition_partners_now[dffin$country == "Rom" & dffin$Q52 %in% c("Alianța Liberalilor și Democraților (ALDE)") & dffin$partydrawn %in% c("Partidul Social Democrat (PSD)")] <- 1

dffin$coalition_partners_now[dffin$country == "Slovakia"] <- 0
dffin$coalition_partners_now[dffin$country == "Slovakia" & dffin$Q52 %in% c("Smer – sociálna demokracia (Smer-SD)") & dffin$partydrawn %in% c("Slovenská národná strana (SNS)","Most–Híd")] <- 1
dffin$coalition_partners_now[dffin$country == "Slovakia" & dffin$Q52 %in% c("Slovenská národná strana (SNS)") & dffin$partydrawn %in% c("Smer – sociálna demokracia (Smer-SD)","Most–Híd")] <- 1
dffin$coalition_partners_now[dffin$country == "Slovakia" & dffin$Q52 %in% c("Most–Híd") & dffin$partydrawn %in% c("Slovenská národná strana (SNS)","Smer – sociálna demokracia (Smer-SD)")] <- 1

dffin$coalition_partners_now[dffin$country == "Slovenia"] <- 0
dffin$coalition_partners_now[dffin$country == "Slovenia" & dffin$Q52 %in% c("Lista Marjana Šarca (LMŠ)") & dffin$partydrawn %in% c("Socialni demokrati (SD)","Stranka modernega centra (SMC)","Stranka Alenke Bratušek","Demokratična stranka upokojencev Slovenije (DeSUS)")] <- 1
dffin$coalition_partners_now[dffin$country == "Slovenia" & dffin$Q52 %in% c("Socialni demokrati (SD)") & dffin$partydrawn %in% c("Lista Marjana Šarca (LMŠ)","Stranka modernega centra (SMC)","Stranka Alenke Bratušek","Demokratična stranka upokojencev Slovenije (DeSUS)")] <- 1
dffin$coalition_partners_now[dffin$country == "Slovenia" & dffin$Q52 %in% c("Stranka modernega centra (SMC)") & dffin$partydrawn %in% c("Socialni demokrati (SD)","Lista Marjana Šarca (LMŠ)","Stranka Alenke Bratušek","Demokratična stranka upokojencev Slovenije (DeSUS)")] <- 1
dffin$coalition_partners_now[dffin$country == "Slovenia" & dffin$Q52 %in% c("Stranka Alenke Bratušek") & dffin$partydrawn %in% c("Socialni demokrati (SD)","Stranka modernega centra (SMC)","Lista Marjana Šarca (LMŠ)","Demokratična stranka upokojencev Slovenije (DeSUS)")] <- 1
dffin$coalition_partners_now[dffin$country == "Slovenia" & dffin$Q52 %in% c("Demokratična stranka upokojencev Slovenije (DeSUS)") & dffin$partydrawn %in% c("Socialni demokrati (SD)","Stranka modernega centra (SMC)","Stranka Alenke Bratušek","Lista Marjana Šarca (LMŠ)")] <- 1

dffin$coalition_partners_now[dffin$country == "Spa"] <- 0

dffin$coalition_partners_now[dffin$country == "Swe"] <- 0
dffin$coalition_partners_now[dffin$country == "Swe" & dffin$Q52 %in% c("Socialdemokratiska arbetarpartiet (SAP)") & dffin$partydrawn %in% c("Miljöpartiet de Gröna (MP)")] <- 1
dffin$coalition_partners_now[dffin$country == "Swe" & dffin$Q52 %in% c("Miljöpartiet de Gröna (MP)") & dffin$partydrawn %in% c("Socialdemokratiska arbetarpartiet (SAP)")] <- 1

dffin$coalition_partners_now[dffin$country == "UK"] <- 0




dffin$coalitiongovsupporter <- 0
dffin$coalitiongovsupporter[dffin$cleanstartdate < as.Date('2019-06-03') & dffin$Q52 %in%  c("Österreichische Volkspartei (ÖVP)","Freiheitliche Partei Österreichs (FPÖ)")] <- 1
dffin$coalitiongovsupporter[dffin$Q52 %in%  c("Mouvement Réformateur (MR)","Christen-Democratisch en Vlaams (CD&V)","Open Vlaamse Liberalen en Democraten (Open Vld)")] <- 1
dffin$coalitiongovsupporter[dffin$Q52 %in%  c("Граждани за европейско развитие на България (ГЕРБ)","Национален фронт за спасение на България (НФСБ)")] <- 1
dffin$coalitiongovsupporter[dffin$Q52 %in%  c("Hrvatska demokratska zajednica (HDZ)","Hrvatska narodna stranka – Liberalni demokrati (HNS)")] <- 1
dffin$coalitiongovsupporter[dffin$Q52 %in%  c("ANO 2011 (ANO)","Česká strana sociálně demokratická (ČSSD)")] <- 1
dffin$Q52[dffin$Q52 == "Venstre, Danmarks Liberale Parti (V)"] <- "Venstre (V)"
dffin$coalitiongovsupporter[dffin$cleanstartdate < as.Date('2019-06-27') & dffin$Q52 %in%  c("Venstre (V)","Liberal Alliance (I)","Det Konservative Folkeparti (C)")] <- 1
dffin$coalitiongovsupporter[dffin$cleanstartdate < as.Date('2019-04-29') & dffin$Q52 %in%  c("Eesti Keskerakond (KE)","Sotsiaaldemokraatlik Erakond (SDE)","Isamaa (I)")] <- 1
dffin$coalitiongovsupporter[dffin$cleanstartdate >= as.Date('2019-04-29') & dffin$Q52 %in%  c("Eesti Keskerakond (KE)","Eesti Konservatiivne Rahvaerakond (EKRE)","Isamaa (I)")] <- 1
dffin$Q52[dffin$Q52 == "Svenska folkpartiet i Finland (SFP, Suomen ruotsalainen kansanpuolue (RKP))"] <- "Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))"
dffin$coalitiongovsupporter[dffin$cleanstartdate < as.Date('2019-06-06') & dffin$Q52 %in%  c("Suomen Keskusta (KESK)","Kansallinen Kokoomus (KOK)","Sininen tulevaisuus (SIN)")] <- 1
dffin$coalitiongovsupporter[dffin$cleanstartdate >= as.Date('2019-06-06') & dffin$Q52 %in%  c("Suomen Sosialidemokraattinen Puolue (SDP)","Suomen Keskusta (KESK)","Vihreä liitto (VIHR)","Vasemmistoliitto (VAS)","Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))")] <- 1
dffin$coalitiongovsupporter[dffin$Q52 %in%  c("La République En Marche (REM)","Les Républicains (LR)","Mouvement Démocrate (MoDem)")] <- 1
dffin$coalitiongovsupporter[dffin$Q52 %in%  c("Christlich Demokratische Union (CDU)","Christlich Soziale Union (CSU)","Sozialdemokratische Partei Deutschlands (SPD)")] <- 1
dffin$coalitiongovsupporter[dffin$Q52 %in%  c("Fidesz – Magyar Polgári Szövetség (FIDESZ)","Kereszténydemokrata Néppárt (KDNP)")] <- 1
dffin$coalitiongovsupporter[dffin$Q52 %in%  c("Fine Gael (FG)","Independent Alliance")] <- 1
dffin$Q52[dffin$Q52 == "Lega Nord (LN)"] <- "Lega (Lega Salvini Premier)"
dffin$coalitiongovsupporter[dffin$Q52 %in%  c("Movimento 5 Stelle (M5S)","Lega (Lega Salvini Premier)")] <- 1
dffin$Q52[dffin$Q52 == "\"Kam pieder valsts\" (KPV LV)"] <- "'Kam pieder valsts' (KPV LV)"
dffin$Q52[dffin$Q52 == "Kustība Par! (Par!)"] <- "Latvijas attīstībai (LA)"
dffin$coalitiongovsupporter[dffin$Q52 %in%  c("Jaunā konservatīvā partija (JKP)","'Kam pieder valsts' (KPV LV)","Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)","Latvijas attīstībai (LA)","Vienotība (V)")] <- 1
dffin$coalitiongovsupporter[dffin$cleanstartdate < as.Date('2019-07-23') & dffin$Q52 %in%  c("Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)","Lietuvos socialdemokratų darbo partija (LSDDP)")] <- 1
dffin$coalitiongovsupporter[dffin$cleanstartdate >= as.Date('2019-07-23') & dffin$Q52 %in%  c("Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)","Lietuvos socialdemokratų darbo partija (LSDDP)","Lietuvos lenkų rinkimų akcija – Krikščioniškų šeimų sąjunga (LLRA–KŠS)","Tvarka ir teisingumas (TT)")] <- 1
dffin$coalitiongovsupporter[dffin$Q52 %in%  c("Volkspartij voor Vrijheid en Democratie (VVD)","Christen-Democratisch Appèl (CDA)","Democraten 66 (D66)","ChristenUnie (CU)")] <- 1
dffin$coalitiongovsupporter[dffin$Q52 %in%  c("Partidul Social Democrat (PSD)","Alianța Liberalilor și Democraților (ALDE)")] <- 1
dffin$coalitiongovsupporter[dffin$Q52 %in%  c("Smer – sociálna demokracia (Smer-SD)","Slovenská národná strana (SNS)","Most–Híd")] <- 1
dffin$coalitiongovsupporter[dffin$Q52 %in%  c("Lista Marjana Šarca (LMŠ)","Socialni demokrati (SD)","Stranka modernega centra (SMC)","Stranka Alenke Bratušek","Demokratična stranka upokojencev Slovenije (DeSUS)")] <- 1
dffin$coalitiongovsupporter[dffin$Q52 %in%  c("Socialdemokratiska arbetarpartiet (SAP)","Miljöpartiet de Gröna (MP)")] <- 1





# # Party Positions (for Distances) ####

# external party positions: cmp ####
cmp19b <- read.csv('data/cmp21a.csv', stringsAsFactors = F)

# eu28 and last election per country
eu28 <- sort(unique(cmp19b$countryname[cmp19b$eumember == 10]))
cmp19b <- cmp19b[cmp19b$countryname %in% eu28,]
cmp19b <- cmp19b[cmp19b$date <= 201908,]

cmp19b$cmp_vote_share <- cmp19b$pervote


tmp <- aggregate(cmp19b$date, list(cmp19b$party), max)
tmp
cmp19b <- merge(cmp19b, tmp, all.x = T, by.x = 'party', by.y = 'Group.1')
formerge <- cmp19b[cmp19b$date == cmp19b$x,]
formerge$x <- NULL

formerge <- subset(formerge, select = c('countryname',
                                        "date",
                                        'party',
                                        'partyname','partyabbrev',
                                        "cmp_vote_share",
                                        'rile'))
formerge <- formerge[order(formerge$countryname, formerge$date),]

cmp19b2000 <- cmp19b[cmp19b$date >= 200001,]
cmp19b2000 <- aggregate(cmp19b2000$rile, 
                        list(party=cmp19b2000$party),
                        mean
                        , na.rm = T # if party position is NA in particular election
)
names(cmp19b2000)[names(cmp19b2000) == "x"] <- "avg_rile_since2000"


dffin$q52_cmp_id <- NA
dffin$q52_cmp_id[dffin$Q52 == "Sozialdemokratische Partei Österreich (SPÖ)"] <- 42320
dffin$q52_cmp_id[dffin$Q52 == "Österreichische Volkspartei (ÖVP)"] <- 42520
dffin$q52_cmp_id[dffin$Q52 == "Freiheitliche Partei Österreichs (FPÖ)"] <- 42420
dffin$q52_cmp_id[dffin$Q52 == "Die Grünen – Die grüne Alternative (GRÜNE)"] <- 42110
dffin$q52_cmp_id[dffin$Q52 == "NEOS – Das Neue Österreich und Liberales Forum (NEOS)"] <- 42430
dffin$q52_cmp_id[dffin$Q52 == "JETZT - Liste Pilz (JETZT)"] <- 42120
dffin$q52_cmp_id[dffin$Q52 == "Kommunistische Partei Österreichs (KPÖ)"] <- 42220 # added jan 2022

dffin$drawn_cmp_id <- NA
dffin$drawn_cmp_id[dffin$partydrawn == "Sozialdemokratische Partei Österreich (SPÖ)"] <- 42320
dffin$drawn_cmp_id[dffin$partydrawn == "Österreichische Volkspartei (ÖVP)"] <- 42520
dffin$drawn_cmp_id[dffin$partydrawn == "Freiheitliche Partei Österreichs (FPÖ)"] <- 42420
dffin$drawn_cmp_id[dffin$partydrawn == "Die Grünen – Die grüne Alternative (GRÜNE)"] <- 42110
dffin$drawn_cmp_id[dffin$partydrawn == "NEOS – Das Neue Österreich und Liberales Forum (NEOS)"] <- 42430
dffin$drawn_cmp_id[dffin$partydrawn == "JETZT - Liste Pilz (JETZT)"] <- 42120
dffin$drawn_cmp_id[dffin$partydrawn == "Kommunistische Partei Österreichs (KPÖ)"] <- 42220



dffin$q52_cmp_id[dffin$Q52 == "Groen"] <- 21112
dffin$q52_cmp_id[dffin$Q52 == "Partij van de Arbeid van België (PVDA) - Parti du Travail de Belgique (PTB)"] <- 21230
dffin$q52_cmp_id[dffin$Q52 == "Socialistische Partij Anders (sp.a)"] <- 21321
dffin$q52_cmp_id[dffin$Q52 == "Open Vlaamse Liberalen en Democraten (Open Vld)"] <- 21421
dffin$q52_cmp_id[dffin$Q52 == "Christen-Democratisch en Vlaams (CD&V)"] <- 21521
dffin$q52_cmp_id[dffin$Q52 == "Nieuw-Vlaamse Alliantie (N-VA)"] <- 21916
dffin$q52_cmp_id[dffin$Q52 == "Vlaams Belang (VB)"] <- 21917
dffin$q52_cmp_id[dffin$Q52 == "Mouvement Réformateur (MR)"] <- 21426
dffin$q52_cmp_id[dffin$Q52 == "Parti socialiste (PS)"] <- 21322
dffin$q52_cmp_id[dffin$Q52 == "Centre démocrate humaniste (cdH)"] <- 21522
dffin$q52_cmp_id[dffin$Q52 == "Écologistes Confédérés pour l'organisation de luttes originales (Ecolo)"] <- 21111

dffin$drawn_cmp_id[dffin$partydrawn == "Groen"] <- 21112
dffin$drawn_cmp_id[dffin$partydrawn == "Partij van de Arbeid van België (PVDA) - Parti du Travail de Belgique (PTB)"] <- 21230
dffin$drawn_cmp_id[dffin$partydrawn == "Socialistische Partij Anders (sp.a)"] <- 21321
dffin$drawn_cmp_id[dffin$partydrawn == "Open Vlaamse Liberalen en Democraten (Open Vld)"] <- 21421
dffin$drawn_cmp_id[dffin$partydrawn == "Christen-Democratisch en Vlaams (CD&V)"] <- 21521
dffin$drawn_cmp_id[dffin$partydrawn == "Nieuw-Vlaamse Alliantie (N-VA)"] <- 21916
dffin$drawn_cmp_id[dffin$partydrawn == "Vlaams Belang (VB)"] <- 21917
dffin$drawn_cmp_id[dffin$partydrawn == "Centre démocrate humaniste (cdH)"] <- 21522
dffin$drawn_cmp_id[dffin$partydrawn == "Mouvement Réformateur (MR)"] <- 21426
dffin$drawn_cmp_id[dffin$partydrawn == "Parti socialiste (PS)"] <- 21322
dffin$drawn_cmp_id[dffin$partydrawn == "Écologistes Confédérés pour l'organisation de luttes originales (Ecolo)"] <- 21111



dffin$q52_cmp_id[dffin$Q52 == "ВМРО – Българско Национално Движение" ] <- 80071 # United Patriots in 2017
dffin$drawn_cmp_id[dffin$partydrawn == "ВМРО – Българско Национално Движение" ] <- 80071# United Patriots in 2017
dffin$q52_cmp_id[dffin$Q52 == "Атака" ] <- 80071 # United Patriots in 2017
dffin$drawn_cmp_id[dffin$partydrawn == "Атака" ] <- 80071 # United Patriots in 2017
dffin$q52_cmp_id[dffin$Q52 == "Национален фронт за спасение на България (НФСБ)" ] <- 80071# United Patriots in 2017
dffin$drawn_cmp_id[dffin$partydrawn == "Национален фронт за спасение на България (НФСБ)" ] <- 80071# United Patriots in 2017
dffin$q52_cmp_id[dffin$Q52 == "Българска социалистическа партия (БСП)" ] <- 80221
dffin$drawn_cmp_id[dffin$partydrawn == "Българска социалистическа партия (БСП)" ] <- 80221
dffin$q52_cmp_id[dffin$Q52 == "Граждани за европейско развитие на България (ГЕРБ)" ] <- 80510
dffin$drawn_cmp_id[dffin$partydrawn == "Граждани за европейско развитие на България (ГЕРБ)" ] <- 80510
dffin$q52_cmp_id[dffin$Q52 == "Воля" ] <- 80640
dffin$drawn_cmp_id[dffin$partydrawn == "Воля" ] <- 80640
dffin$q52_cmp_id[dffin$Q52 == "Движение за права и свободи (ДПС)"  ] <- 80951
dffin$drawn_cmp_id[dffin$partydrawn == "Движение за права и свободи (ДПС)"  ] <- 80951
dffin$q52_cmp_id[dffin$Q52 == "Алтернатива за българско възраждане (АБВ)"  ] <- 80330
dffin$drawn_cmp_id[dffin$partydrawn == "Алтернатива за българско възраждане (АБВ)"  ] <- 80330



dffin$q52_cmp_id[dffin$Q52 == "Živi zid"] <- 81022 # only option coalition
dffin$drawn_cmp_id[dffin$partydrawn =="Živi zid" ] <- 81022 # only option coalition
dffin$q52_cmp_id[dffin$Q52 == "Hrvatska demokršćanska stranka (HDS)"] <- NA # 81022 # actually in coalition with hdz
dffin$drawn_cmp_id[dffin$partydrawn == "Hrvatska demokršćanska stranka (HDS)" ] <- NA # 81022
dffin$q52_cmp_id[dffin$Q52 == "Socijaldemokratska partija Hrvatske  (SDP)"] <- 81032 # people's coalition
dffin$drawn_cmp_id[dffin$partydrawn == "Socijaldemokratska partija Hrvatske  (SDP)" ] <- 81032 # people's coalition
dffin$q52_cmp_id[dffin$Q52 == "Hrvatska narodna stranka – Liberalni demokrati (HNS)"] <- 81032 # people's coalition
dffin$drawn_cmp_id[dffin$partydrawn == "Hrvatska narodna stranka – Liberalni demokrati (HNS)" ] <- 81032 # people's coalition
dffin$q52_cmp_id[dffin$Q52 == "Hrvatska seljačka stranka  (HSS)"] <- 81032 # people's coalition
dffin$drawn_cmp_id[dffin$partydrawn == "Hrvatska seljačka stranka  (HSS)" ] <- 81032 # people's coalition
dffin$q52_cmp_id[dffin$Q52 == "Bandić Milan 365 - Stranka rada i solidarnosti (BM365)"] <- 81043 # Coalition for prime minister
dffin$drawn_cmp_id[dffin$partydrawn == "Bandić Milan 365 - Stranka rada i solidarnosti (BM365)" ] <- 81043
dffin$q52_cmp_id[dffin$Q52 == "Istarski demokratski sabor (IDS)"] <- 81091
dffin$drawn_cmp_id[dffin$partydrawn == "Istarski demokratski sabor (IDS)" ] <- 81091
dffin$q52_cmp_id[dffin$Q52 == "Most nezavisnih lista (MOST)"] <- 81460
dffin$drawn_cmp_id[dffin$partydrawn == "Most nezavisnih lista (MOST)" ] <- 81460
dffin$q52_cmp_id[dffin$Q52 == "Hrvatska demokratska zajednica (HDZ)"] <- 81711
dffin$drawn_cmp_id[dffin$partydrawn == "Hrvatska demokratska zajednica (HDZ)" ] <- 81711
dffin$q52_cmp_id[dffin$Q52 == "Samostalna demokratska srpska stranka (SDSS (Самостална демократска српска странка (СДСС)))"] <- 81910
dffin$drawn_cmp_id[dffin$partydrawn == "Samostalna demokratska srpska stranka (SDSS (Самостална демократска српска странка (СДСС)))" ] <- 81910



dffin$q52_cmp_id[dffin$Q52 == "Komunistická strana Čech a Moravy (KSČM)"] <- 82220
dffin$drawn_cmp_id[dffin$partydrawn == "Komunistická strana Čech a Moravy (KSČM)"] <- 82220
dffin$q52_cmp_id[dffin$Q52 == "Česká strana sociálně demokratická (ČSSD)"] <- 82320
dffin$drawn_cmp_id[dffin$partydrawn == "Česká strana sociálně demokratická (ČSSD)"] <- 82320
dffin$q52_cmp_id[dffin$Q52 == "Občanská demokratická strana (ODS)"] <- 82413
dffin$drawn_cmp_id[dffin$partydrawn == "Občanská demokratická strana (ODS)"] <- 82413
dffin$q52_cmp_id[dffin$Q52 == "ANO 2011 (ANO)"] <- 82430
dffin$drawn_cmp_id[dffin$partydrawn == "ANO 2011 (ANO)"] <- 82430
dffin$q52_cmp_id[dffin$Q52 == "Křesťanská a demokratická unie - Československá strana lidová (KDU-ČSL)"] <- 82523
dffin$drawn_cmp_id[dffin$partydrawn == "Křesťanská a demokratická unie - Československá strana lidová (KDU-ČSL)"] <- 82523
dffin$drawn_cmp_id[dffin$partydrawn == "Křesťanská a demokratická unie – Československá strana lidová (KDU-ČSL)"] <- 82523
dffin$q52_cmp_id[dffin$Q52 == "Tradice Odpovědnost Prosperita 09 (TOP 09)"] <- 82530
dffin$drawn_cmp_id[dffin$partydrawn == "Tradice Odpovědnost Prosperita 09 (TOP 09)"] <- 82530
dffin$q52_cmp_id[dffin$Q52 == "Starostové a nezávislí (STAN)"] <- 82610
dffin$drawn_cmp_id[dffin$partydrawn == "Starostové a nezávislí (STAN)"] <- 82610
dffin$q52_cmp_id[dffin$Q52 == "Svoboda a přímá demokracie (SPD)"] <- 82721
dffin$drawn_cmp_id[dffin$partydrawn == "Svoboda a přímá demokracie (SPD)"] <- 82721
dffin$q52_cmp_id[dffin$Q52 == "Česká pirátská strana (Piráti)"] <- 82953
dffin$drawn_cmp_id[dffin$partydrawn == "Česká pirátská strana (Piráti)"] <- 82953
dffin$q52_cmp_id[dffin$Q52 == "Strana zelených (SZ)"] <- 82110
dffin$drawn_cmp_id[dffin$partydrawn == "Strana zelených (SZ)"] <- 82110



dffin$q52_cmp_id[dffin$Q52 == "Liberal Alliance (I)"] <- 13001
dffin$drawn_cmp_id[dffin$partydrawn == "Liberal Alliance (I)"] <- 13001
dffin$q52_cmp_id[dffin$Q52 == "Alternativet (Å)"] <- 13110
dffin$drawn_cmp_id[dffin$partydrawn == "Alternativet (Å)"] <- 13110
dffin$q52_cmp_id[dffin$Q52 == "Enhedslisten – De rød-grønne (Ø)"] <- 13229
dffin$drawn_cmp_id[dffin$partydrawn == "Enhedslisten – De rød-grønne (Ø)"] <- 13229
dffin$q52_cmp_id[dffin$Q52 == "Socialistisk Folkeparti (F)"] <- 13230
dffin$drawn_cmp_id[dffin$partydrawn == "Socialistisk Folkeparti (F)"] <- 13230
dffin$q52_cmp_id[dffin$Q52 == "Socialdemokratiet (A)"] <- 13320
dffin$drawn_cmp_id[dffin$partydrawn == "Socialdemokratiet (A)"] <- 13320
dffin$q52_cmp_id[dffin$Q52 == "Det Radikale Venstre (B)"] <- 13410
dffin$drawn_cmp_id[dffin$partydrawn == "Det Radikale Venstre (B)"] <- 13410
dffin$q52_cmp_id[dffin$Q52 == "Venstre (V)"] <- 13420
dffin$drawn_cmp_id[dffin$partydrawn == "Venstre (V)"] <- 13420
dffin$q52_cmp_id[dffin$Q52 == "Det Konservative Folkeparti (C)"] <- 13620
dffin$drawn_cmp_id[dffin$partydrawn == "Det Konservative Folkeparti (C)"] <- 13620
dffin$q52_cmp_id[dffin$Q52 == "Dansk Folkeparti (O)"] <- 13720
dffin$drawn_cmp_id[dffin$partydrawn == "Dansk Folkeparti (O)"] <- 13720
dffin$q52_cmp_id[dffin$Q52 == "Kristendemokraterne (K)"] <- 13520
dffin$drawn_cmp_id[dffin$partydrawn == "Kristendemokraterne (K)"] <- 13520
dffin$q52_cmp_id[dffin$Q52 == "Nye Borgerlige (D)"] <- 13730
dffin$drawn_cmp_id[dffin$partydrawn == "Nye Borgerlige (D)"] <- 13730



dffin$q52_cmp_id[dffin$Q52 == "Sotsiaaldemokraatlik Erakond (SDE)"] <- 83410
dffin$drawn_cmp_id[dffin$partydrawn == "Sotsiaaldemokraatlik Erakond (SDE)"] <- 83410
dffin$q52_cmp_id[dffin$Q52 == "Eesti Keskerakond (KE)"] <- 83411
dffin$drawn_cmp_id[dffin$partydrawn == "Eesti Keskerakond (KE)"] <- 83411
dffin$q52_cmp_id[dffin$Q52 == "Eesti Reformierakond (ER)"] <- 83430
dffin$drawn_cmp_id[dffin$partydrawn == "Eesti Reformierakond (ER)"] <- 83430
dffin$q52_cmp_id[dffin$Q52 == "Eesti Vabaerakond (EVA)"] <- 83440
dffin$drawn_cmp_id[dffin$partydrawn == "Eesti Vabaerakond (EVA)"] <- 83440
dffin$q52_cmp_id[dffin$Q52 == "Isamaa (I)"] <- 83611 # 2238     Estonia 201903 83611                                                          Pro Patria                     11.444  17.667  0.115
dffin$drawn_cmp_id[dffin$partydrawn == "Isamaa (I)"] <- 83611
dffin$q52_cmp_id[dffin$Q52 == "Eesti Konservatiivne Rahvaerakond (EKRE)"] <- 83720
dffin$drawn_cmp_id[dffin$partydrawn == "Eesti Konservatiivne Rahvaerakond (EKRE)"] <- 83720
dffin$q52_cmp_id[dffin$Q52 == "Erakond Eestimaa Rohelised (EER)"] <- 83110
dffin$drawn_cmp_id[dffin$partydrawn == "Erakond Eestimaa Rohelised (EER)"] <- 83110

dffin$q52_cmp_id[dffin$Q52 == "Vihreä liitto (VIHR)"] <- 14110
dffin$drawn_cmp_id[dffin$partydrawn == "Vihreä liitto (VIHR)"] <- 14110
dffin$q52_cmp_id[dffin$Q52 == "Vasemmistoliitto (VAS)"] <- 14223
dffin$drawn_cmp_id[dffin$partydrawn == "Vasemmistoliitto (VAS)"] <- 14223
dffin$q52_cmp_id[dffin$Q52 == "Suomen Sosialidemokraattinen Puolue (SDP)"] <- 14320
dffin$drawn_cmp_id[dffin$partydrawn == "Suomen Sosialidemokraattinen Puolue (SDP)"] <- 14320
dffin$q52_cmp_id[dffin$country == "Fin" & dffin$Q52 == "Kristillisdemokraatit (KD)"] <- 14520
dffin$drawn_cmp_id[dffin$country == "Fin" & dffin$partydrawn == "Kristillisdemokraatit (KD)"] <- 14520
dffin$q52_cmp_id[dffin$Q52 == "Kansallinen Kokoomus (KOK)"] <- 14620
dffin$drawn_cmp_id[dffin$partydrawn == "Kansallinen Kokoomus (KOK)"] <- 14620
dffin$q52_cmp_id[dffin$Q52 == "Suomen Keskusta (KESK)"] <- 14810
dffin$drawn_cmp_id[dffin$partydrawn == "Suomen Keskusta (KESK)"] <- 14810
dffin$q52_cmp_id[dffin$Q52 == "Perussuomalaiset (PS)"] <- 14820
dffin$drawn_cmp_id[dffin$partydrawn == "Perussuomalaiset (PS)"] <- 14820
dffin$q52_cmp_id[dffin$Q52 == "Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))"] <- 14901
dffin$drawn_cmp_id[dffin$partydrawn == "Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))"] <- 14901



dffin$q52_cmp_id[dffin$Q52 == "Europe Écologie-Les Verts (EELV)"] <- 31110
dffin$q52_cmp_id[dffin$Q52 == "Parti Communiste Francais (PCF)"] <- 31220
dffin$q52_cmp_id[dffin$Q52 == "Mouvement radical (MR)"] <- 31230 # prg
dffin$q52_cmp_id[dffin$Q52 == "La France Insoumise (FI)"] <- 31240
dffin$q52_cmp_id[dffin$country == "Fra" & dffin$Q52 == "Parti Socialiste (PS)"] <- 31320
dffin$q52_cmp_id[dffin$Q52 == "La République En Marche (REM)"] <- 31425
dffin$q52_cmp_id[dffin$Q52 == "Union des Démocrates et Indépendants (UDI)"] <- 31430
dffin$q52_cmp_id[dffin$country == "Fra" & dffin$Q52 == "Mouvement Démocrate (MoDem)"] <- 31624
dffin$q52_cmp_id[dffin$Q52 == "Les Républicains (LR)"] <- 31626
dffin$q52_cmp_id[dffin$Q52 == "Rassemblement national (RN (ex. Front National (FN)))"] <- 31720

dffin$drawn_cmp_id[dffin$partydrawn == "Europe Écologie-Les Verts (EELV)"] <- 31110
dffin$drawn_cmp_id[dffin$partydrawn == "Parti Communiste Francais (PCF)"] <- 31220
dffin$drawn_cmp_id[dffin$partydrawn == "Mouvement radical (MR)"] <- 31230 # prg
dffin$drawn_cmp_id[dffin$partydrawn == "La France Insoumise (FI)"] <- 31240
dffin$drawn_cmp_id[dffin$country == "Fra" & dffin$partydrawn == "Parti Socialiste (PS)"] <- 31320
dffin$drawn_cmp_id[dffin$partydrawn == "La République En Marche (REM)"] <- 31425
dffin$drawn_cmp_id[dffin$partydrawn == "Union des Démocrates et Indépendants (UDI)"] <- 31430
dffin$drawn_cmp_id[dffin$country == "Fra" & dffin$partydrawn == "Mouvement Démocrate (MoDem)"] <- 31624
dffin$drawn_cmp_id[dffin$partydrawn == "Les Républicains (LR)"] <- 31626
dffin$drawn_cmp_id[dffin$partydrawn == "Rassemblement national (RN (ex. Front National (FN)))"] <- 31720
dffin$q52_cmp_id[dffin$Q52 == "Union des Démocrates et Indépendants (UDI)"] <- 31430
dffin$drawn_cmp_id[dffin$partydrawn == "Union des Démocrates et Indépendants (UDI)"] <- 31430



dffin$q52_cmp_id[dffin$Q52 == "Bündnis 90 / Die Grünen (Grüne)"] <- 41113
dffin$q52_cmp_id[dffin$Q52 == "Die Linke (Linke)"] <- 41223
dffin$q52_cmp_id[dffin$Q52 == "Sozialdemokratische Partei Deutschlands (SPD)"] <- 41320
dffin$q52_cmp_id[dffin$Q52 == "Freie Demokratische Partei (FDP)"] <- 41420
dffin$q52_cmp_id[dffin$Q52 == "Christlich Demokratische Union (CDU)"] <- 41521
dffin$q52_cmp_id[dffin$Q52 == "Christlich Soziale Union (CSU)"] <- 41521
dffin$q52_cmp_id[dffin$Q52 == "Alternative für Deutschland (AfD)"] <- 41953
dffin$drawn_cmp_id[dffin$partydrawn == "Bündnis 90 / Die Grünen (Grüne)"] <- 41113
dffin$drawn_cmp_id[dffin$partydrawn == "Die Linke (Linke)"] <- 41223
dffin$drawn_cmp_id[dffin$partydrawn == "Sozialdemokratische Partei Deutschlands (SPD)"] <- 41320
dffin$drawn_cmp_id[dffin$partydrawn == "Freie Demokratische Partei (FDP)"] <- 41420
dffin$drawn_cmp_id[dffin$partydrawn == "Christlich Demokratische Union (CDU)"] <- 41521
dffin$drawn_cmp_id[dffin$partydrawn == "Christlich Soziale Union (CSU)"] <- 41521
dffin$drawn_cmp_id[dffin$partydrawn == "Alternative für Deutschland (AfD)"] <- 41953
dffin$q52_cmp_id[dffin$Q52 == "Piratenpartei Deutschland (Piraten)"] <- 41952
dffin$drawn_cmp_id[dffin$partydrawn == "Piratenpartei Deutschland (Piraten)"] <- 41952



dffin$q52_cmp_id[dffin$Q52 == "Kομμουνιστικό Kόμμα Ελλάδας (KKΕ)"] <- 34210
dffin$drawn_cmp_id[dffin$partydrawn == "Kομμουνιστικό Kόμμα Ελλάδας (KKΕ)"] <- 34210
dffin$q52_cmp_id[dffin$Q52 == "Συνασπισμός Ριζοσπαστικής Αριστεράς (ΣΥ.ΡIΖ.Α)"] <- 34212
dffin$drawn_cmp_id[dffin$partydrawn == "Συνασπισμός Ριζοσπαστικής Αριστεράς (ΣΥ.ΡIΖ.Α)"] <- 34212
dffin$q52_cmp_id[dffin$Q52 == "Δημοκρατική Αριστερά (ΔΗM.ΑΡ.)"] <- 34213
dffin$drawn_cmp_id[dffin$partydrawn == "Δημοκρατική Αριστερά (ΔΗM.ΑΡ.)"] <- 34213
dffin$q52_cmp_id[dffin$Q52 == "Λαϊκή Ενότητα (ΛΑ.E.)"] <- 34214
dffin$drawn_cmp_id[dffin$partydrawn == "Λαϊκή Ενότητα (ΛΑ.E.)"] <- 34214
dffin$q52_cmp_id[dffin$Q52 == "Πανελλήνιο Σοσιαλιστικό ίνημα (ΠΑΣΟK)"] <- 34313
dffin$drawn_cmp_id[dffin$partydrawn == "Πανελλήνιο Σοσιαλιστικό ίνημα (ΠΑΣΟK)"] <- 34313
dffin$q52_cmp_id[dffin$Q52 == "Το Ποτάμι"] <- 34340
dffin$drawn_cmp_id[dffin$partydrawn == "Το Ποτάμι"] <- 34340
dffin$q52_cmp_id[dffin$Q52 == "Ένωση Κεντρώων (EK)"] <- 34410
dffin$drawn_cmp_id[dffin$partydrawn == "Ένωση Κεντρώων (EK)"] <- 34410
dffin$q52_cmp_id[dffin$Q52 == "Nέα Δημοκρατία (NΔ)"] <- 34511
dffin$drawn_cmp_id[dffin$partydrawn == "Nέα Δημοκρατία (NΔ)"] <- 34511
dffin$q52_cmp_id[dffin$Q52 == "Λαϊκός Σύνδεσμος -Χρυσή Αυγή (ΛΣ-ΧΑ)"] <- 34720
dffin$drawn_cmp_id[dffin$partydrawn == "Λαϊκός Σύνδεσμος -Χρυσή Αυγή (ΛΣ-ΧΑ)"] <- 34720
dffin$q52_cmp_id[dffin$Q52 == "Ανεξάρτητοι Έλληνες (ΑΝΕΛ)"] <- 34730
dffin$drawn_cmp_id[dffin$partydrawn == "Ανεξάρτητοι Έλληνες (ΑΝΕΛ)"] <- 34730
dffin$q52_cmp_id[dffin$Q52 == "Λαϊκός Ορθόδοξος Συναγερμός (ΛΑ.Ο.Σ)" ] <- 34710
dffin$q52_cmp_id[dffin$partydrawn == "Λαϊκός Ορθόδοξος Συναγερμός (ΛΑ.Ο.Σ)" ] <- 34710



dffin$q52_cmp_id[dffin$Q52 == "Lehet Más a Politika (LMP)"] <- 86110
dffin$drawn_cmp_id[dffin$partydrawn == "Lehet Más a Politika (LMP)"] <- 86110
dffin$q52_cmp_id[dffin$Q52 == "Magyar Szocialista Párt (MSZP)"] <- 86220
dffin$drawn_cmp_id[dffin$partydrawn == "Magyar Szocialista Párt (MSZP)"] <- 86220
dffin$q52_cmp_id[dffin$Q52 == "Demokratikus Koalíció (DK)"] <- 86221
dffin$drawn_cmp_id[dffin$partydrawn == "Demokratikus Koalíció (DK)"] <- 86221
dffin$q52_cmp_id[dffin$Q52 == "Párbeszéd Magyarországért (Párbeszéd)"] <- 86340
dffin$drawn_cmp_id[dffin$partydrawn == "Párbeszéd Magyarországért (Párbeszéd)"] <- 86340
dffin$q52_cmp_id[dffin$Q52 == "Fidesz – Magyar Polgári Szövetség (FIDESZ)"] <- 86421
dffin$drawn_cmp_id[dffin$partydrawn == "Fidesz – Magyar Polgári Szövetség (FIDESZ)"] <- 86421
dffin$q52_cmp_id[dffin$Q52 == "Kereszténydemokrata Néppárt (KDNP)"] <- 86421
dffin$drawn_cmp_id[dffin$partydrawn == "Kereszténydemokrata Néppárt (KDNP)"] <- 86421
dffin$q52_cmp_id[dffin$Q52 == "Jobbik Magyarországért Mozgalom (JOBBIK)"] <- 86710
dffin$drawn_cmp_id[dffin$partydrawn == "Jobbik Magyarországért Mozgalom (JOBBIK)"] <- 86710
dffin$q52_cmp_id[dffin$Q52 == "Párbeszéd Magyarországért (Párbeszéd)"] <- 86111
dffin$drawn_cmp_id[dffin$partydrawn == "Párbeszéd Magyarországért (Párbeszéd)"] <- 86111



dffin$q52_cmp_id[dffin$country == "Ire" & dffin$Q52 == "Green Party"] <- 53110
dffin$drawn_cmp_id[dffin$country == "Ire" & dffin$partydrawn == "Green Party"] <- 53110
dffin$q52_cmp_id[dffin$country == "Ire" & dffin$Q52 == "Solidarity - People Before Profit (Solidarity-PBP)"] <- 53231
dffin$drawn_cmp_id[dffin$country == "Ire" & dffin$partydrawn == "Solidarity - People Before Profit (Solidarity-PBP)"] <- 53231
dffin$q52_cmp_id[dffin$country == "Ire" & dffin$Q52 == "Workers' Party (WP)"] <- 53250
dffin$drawn_cmp_id[dffin$country == "Ire" & dffin$partydrawn == "Workers' Party (WP)"] <- 53250
dffin$q52_cmp_id[dffin$country == "Ire" & dffin$Q52 == "Labour Party (LP)"] <- 53320
dffin$drawn_cmp_id[dffin$country == "Ire" & dffin$partydrawn == "Labour Party (LP)"] <- 53320
dffin$q52_cmp_id[dffin$country == "Ire" & dffin$Q52 == "Social Democrats (Daonlathaigh Shóisialta)"] <- 53321
dffin$drawn_cmp_id[dffin$country == "Ire" & dffin$partydrawn == "Social Democrats (Daonlathaigh Shóisialta)"] <- 53321
dffin$q52_cmp_id[dffin$country == "Ire" & dffin$Q52 == "Fine Gael (FG)"] <- 53520
dffin$drawn_cmp_id[dffin$country == "Ire" & dffin$partydrawn == "Fine Gael (FG)"] <- 53520
dffin$q52_cmp_id[dffin$country == "Ire" & dffin$Q52 == "Fianna Fáil (FF)"] <- 53620
dffin$drawn_cmp_id[dffin$country == "Ire" & dffin$partydrawn == "Fianna Fáil (FF)"] <- 53620
dffin$q52_cmp_id[dffin$country == "Ire" & dffin$Q52 == "Sinn Féin (SF)"] <- 53951
dffin$drawn_cmp_id[dffin$country == "Ire" & dffin$partydrawn == "Sinn Féin (SF)"] <- 53951
dffin$q52_cmp_id[dffin$country == "Ire" & dffin$Q52 == "Independent Alliance"] <- 53981
dffin$drawn_cmp_id[dffin$country == "Ire" & dffin$partydrawn == "Independent Alliance"] <- 53981



dffin$q52_cmp_id[dffin$Q52 == "Articolo 1 – Movimento Democratico e Progressista (MDP)"] <- 32031 # LEU
dffin$q52_cmp_id[dffin$Q52 == "Possibile (P)"] <- 32031 # LEU
dffin$q52_cmp_id[dffin$Q52 == "Sinistra Italiana (SI)"] <- 32031 # LEU
dffin$q52_cmp_id[dffin$Q52 == "Alternativa Popolare (AP)"] <- 32051 # popular civic list
dffin$q52_cmp_id[dffin$Q52 == "Noi con l'Italia (NcL)"] <- 32055
dffin$q52_cmp_id[dffin$Q52 == "Partito Democratico (PD)"] <- 32440
dffin$q52_cmp_id[dffin$Q52 == "Più Europa (+E)"] <- 32451
dffin$q52_cmp_id[dffin$Q52 == "Forza Italia (FI)"] <- 32610
dffin$q52_cmp_id[dffin$Q52 == "Fratelli d'Italia (FDI)"] <- 32630
dffin$q52_cmp_id[dffin$Q52 == "Lega (Lega Salvini Premier)"] <- 32720
dffin$q52_cmp_id[dffin$Q52 == "Movimento 5 Stelle (M5S)"] <- 32956

dffin$drawn_cmp_id[dffin$partydrawn== "Articolo 1 – Movimento Democratico e Progressista (MDP)"] <- 32031 # LEU
dffin$drawn_cmp_id[dffin$partydrawn== "Possibile (P)"] <- 32031 # LEU
dffin$drawn_cmp_id[dffin$partydrawn== "Sinistra Italiana (SI)"] <- 32031 # LEU
dffin$drawn_cmp_id[dffin$partydrawn== "Alternativa Popolare (AP)"] <- 32051 # popular civic list
dffin$drawn_cmp_id[dffin$partydrawn== "Noi con l'Italia (NcL)"] <- 32055
dffin$drawn_cmp_id[dffin$partydrawn== "Partito Democratico (PD)"] <- 32440
dffin$drawn_cmp_id[dffin$partydrawn== "Più Europa (+E)"] <- 32451
dffin$drawn_cmp_id[dffin$partydrawn== "Forza Italia (FI)"] <- 32610
dffin$drawn_cmp_id[dffin$partydrawn== "Fratelli d'Italia (FDI)"] <- 32630
dffin$drawn_cmp_id[dffin$partydrawn== "Lega (Lega Salvini Premier)"] <- 32720
dffin$drawn_cmp_id[dffin$partydrawn== "Movimento 5 Stelle (M5S)"] <- 32956
dffin$q52_cmp_id[dffin$Q52 == "Unione di Centro (UDC)"] <- 32530
dffin$drawn_cmp_id[dffin$partydrawn== "Unione di Centro (UDC)"] <- 32530



dffin$q52_cmp_id[dffin$Q52 == "Kustība Par! (Par!)" ] <- 87042
dffin$drawn_cmp_id[dffin$partydrawn == "Kustība Par! (Par!)" ] <- 87042
dffin$q52_cmp_id[dffin$Q52 == "Vienotība (V)" ] <- 87062
dffin$drawn_cmp_id[dffin$partydrawn == "Vienotība (V)" ] <- 87062
dffin$q52_cmp_id[dffin$Q52 == "Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)"  ] <- 87071
dffin$drawn_cmp_id[dffin$partydrawn == "Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)"  ] <- 87071
dffin$q52_cmp_id[dffin$Q52 == "Zaļo un Zemnieku savienība (ZZS: Latvijas Zemnieku savienība (LZS) + Latvijas Zaļā partija (LZP))" ] <- 87110
dffin$drawn_cmp_id[dffin$partydrawn == "Zaļo un Zemnieku savienība (ZZS: Latvijas Zemnieku savienība (LZS) + Latvijas Zaļā partija (LZP))"] <- 87110
dffin$q52_cmp_id[dffin$Q52 == "Sociāldemokrātiskā partija „Saskaņa“ (SDP)"  ] <- 87340
dffin$drawn_cmp_id[dffin$partydrawn == "Sociāldemokrātiskā partija „Saskaņa“ (SDP)"  ] <- 87340
dffin$q52_cmp_id[dffin$Q52 == "Jaunā konservatīvā partija (JKP)"  ] <- 87640
dffin$drawn_cmp_id[dffin$partydrawn == "Jaunā konservatīvā partija (JKP)" ] <- 87640
dffin$q52_cmp_id[dffin$Q52 == "'Kam pieder valsts' (KPV LV)" ] <- 87730
dffin$drawn_cmp_id[dffin$partydrawn == "'Kam pieder valsts' (KPV LV)" ] <- 87730
dffin$q52_cmp_id[dffin$Q52 ==  "Latvijas Reģionu apvienība (LRA)"] <- 87901
dffin$drawn_cmp_id[dffin$partydrawn ==  "Latvijas Reģionu apvienība (LRA)" ] <- 87901
dffin$q52_cmp_id[dffin$Q52 ==  "No sirds Latvijai (NSL)"] <- 87630
dffin$drawn_cmp_id[dffin$partydrawn ==  "No sirds Latvijai (NSL)" ] <- 87630



dffin$q52_cmp_id[dffin$Q52 == "Lietuvos socialdemokratų partija (LSDP)" ] <- 88320
dffin$drawn_cmp_id[dffin$partydrawn == "Lietuvos socialdemokratų partija (LSDP)" ] <- 88320
dffin$q52_cmp_id[dffin$Q52 == "Darbo partija (DP)"  ] <- 88440
dffin$drawn_cmp_id[dffin$partydrawn == "Darbo partija (DP)"  ] <- 88440
dffin$q52_cmp_id[dffin$Q52 == "Liberalų sąjūdis (LRLS)"] <- 88450
dffin$drawn_cmp_id[dffin$partydrawn == "Liberalų sąjūdis (LRLS)"] <- 88450
dffin$q52_cmp_id[dffin$Q52 == "Tvarka ir teisingumas (TT)"] <- 88460
dffin$drawn_cmp_id[dffin$partydrawn == "Tvarka ir teisingumas (TT)"] <- 88460
dffin$q52_cmp_id[dffin$Q52 == "Tėvynės sąjunga - Lietuvos krikščionys demokratai (TS-LKD)"] <- 88621
dffin$drawn_cmp_id[dffin$partydrawn == "Tėvynės sąjunga - Lietuvos krikščionys demokratai (TS-LKD)"] <- 88621
dffin$q52_cmp_id[dffin$Q52 == "Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)"] <- 88820
dffin$drawn_cmp_id[dffin$partydrawn == "Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)"] <- 88820
dffin$q52_cmp_id[dffin$Q52 == "Lietuvos lenkų rinkimų akcija – Krikščioniškų šeimų sąjunga (LLRA–KŠS)"] <- 88951
dffin$drawn_cmp_id[dffin$partydrawn == "Lietuvos lenkų rinkimų akcija – Krikščioniškų šeimų sąjunga (LLRA–KŠS)"] <- 88951



dffin$q52_cmp_id[dffin$Q52 == "GroenLinks (GL)"] <- 22110
dffin$q52_cmp_id[dffin$Q52 == "Socialistische Partij (SP)"] <- 22220
dffin$q52_cmp_id[dffin$Q52 == "Partij van de Arbeid (PvdA)"] <- 22320
dffin$q52_cmp_id[dffin$Q52 == "DENK"] <- 22321
dffin$q52_cmp_id[dffin$Q52 == "Democraten 66 (D66)"] <- 22330
dffin$q52_cmp_id[dffin$Q52 == "Volkspartij voor Vrijheid en Democratie (VVD)"] <- 22420
dffin$q52_cmp_id[dffin$Q52 == "Christen-Democratisch Appèl (CDA)"] <- 22521
dffin$q52_cmp_id[dffin$Q52 == "ChristenUnie (CU)"] <- 22526
dffin$q52_cmp_id[dffin$Q52 == "Partij voor de Vrijheid (PVV)"] <- 22722
dffin$q52_cmp_id[dffin$Q52 == "Forum voor Democratie (FvD)"] <- 22730
dffin$q52_cmp_id[dffin$Q52 == "Partij voor de Dieren (PvdD)"] <- 22951
dffin$q52_cmp_id[dffin$Q52 == "Staatkundig Gereformeerde Partij (SGP)"] <- 22952
dffin$q52_cmp_id[dffin$Q52 == "50Plus"] <- 22953

dffin$drawn_cmp_id[dffin$partydrawn == "GroenLinks (GL)"] <- 22110
dffin$drawn_cmp_id[dffin$partydrawn == "Socialistische Partij (SP)"] <- 22220
dffin$drawn_cmp_id[dffin$partydrawn == "Partij van de Arbeid (PvdA)"] <- 22320
dffin$drawn_cmp_id[dffin$partydrawn == "DENK"] <- 22321
dffin$drawn_cmp_id[dffin$partydrawn == "Democraten 66 (D66)"] <- 22330
dffin$drawn_cmp_id[dffin$partydrawn == "Volkspartij voor Vrijheid en Democratie (VVD)"] <- 22420
dffin$drawn_cmp_id[dffin$partydrawn == "Christen-Democratisch Appèl (CDA)"] <- 22521
dffin$drawn_cmp_id[dffin$partydrawn == "ChristenUnie (CU)"] <- 22526
dffin$drawn_cmp_id[dffin$partydrawn == "Partij voor de Vrijheid (PVV)"] <- 22722
dffin$drawn_cmp_id[dffin$partydrawn == "Forum voor Democratie (FvD)"] <- 22730
dffin$drawn_cmp_id[dffin$partydrawn == "Partij voor de Dieren (PvdD)"] <- 22951
dffin$drawn_cmp_id[dffin$partydrawn == "Staatkundig Gereformeerde Partij (SGP)"] <- 22952
dffin$drawn_cmp_id[dffin$partydrawn == "50Plus"] <- 22953



dffin$q52_cmp_id[dffin$Q52 == "Sojusz Lewicy Demokratycznej (SLD)"] <- 92210
dffin$drawn_cmp_id[dffin$partydrawn == "Sojusz Lewicy Demokratycznej (SLD)"] <- 92210
dffin$q52_cmp_id[dffin$Q52 == "Platforma Obywatelska (PO)"] <- 92435
dffin$drawn_cmp_id[dffin$partydrawn == "Platforma Obywatelska (PO)"] <- 92435
dffin$q52_cmp_id[dffin$Q52 == "Prawo i Sprawiedliwość (PiS)"] <- 92436
dffin$drawn_cmp_id[dffin$partydrawn == "Prawo i Sprawiedliwość (PiS)"] <- 92436
dffin$q52_cmp_id[dffin$Q52 == "Twój Ruch"] <- 92440
dffin$drawn_cmp_id[dffin$partydrawn == "Twój Ruch"] <- 92440
dffin$q52_cmp_id[dffin$Q52 == "Polskie Stronnictwo Ludowe (PSL)"] <- 92811
dffin$drawn_cmp_id[dffin$partydrawn == "Polskie Stronnictwo Ludowe (PSL)"] <- 92811
dffin$q52_cmp_id[dffin$Q52 == "Mniejszość Niemiecka"] <- 92953
dffin$drawn_cmp_id[dffin$partydrawn == "Mniejszość Niemiecka"] <- 92953
dffin$q52_cmp_id[dffin$Q52 == "Kukuiz'15"] <- 92720
dffin$drawn_cmp_id[dffin$partydrawn == "Kukuiz'15"] <- 92720
dffin$q52_cmp_id[dffin$Q52 == ".Nowoczesna (.N)" ] <- 92450
dffin$drawn_cmp_id[dffin$partydrawn == ".Nowoczesna (.N)" ] <- 92450



dffin$q52_cmp_id[dffin$Q52 == "Partido Social Democrata (PSD)"] <- 35060 # portugal ahead
dffin$drawn_cmp_id[dffin$partydrawn == "Partido Social Democrata (PSD)"] <- 35060 # portugal ahead
dffin$q52_cmp_id[dffin$Q52 == "Centro Democrático e Social – Partido Popular (CDS-PP)"] <- 35060 # portugal ahead
dffin$drawn_cmp_id[dffin$partydrawn == "Centro Democrático e Social – Partido Popular (CDS-PP)"] <- 35060 # portugal ahead
dffin$q52_cmp_id[dffin$Q52 == "Partido Ecologista 'os Verdes' (PEV)"] <- 35110
dffin$drawn_cmp_id[dffin$partydrawn == "Partido Ecologista 'os Verdes' (PEV)"] <- 35110
dffin$q52_cmp_id[dffin$Q52 == "Pessoas-Animais-Natureza (PAN)"] <- 35120
dffin$drawn_cmp_id[dffin$partydrawn == "Pessoas-Animais-Natureza (PAN)"] <- 35120
dffin$q52_cmp_id[dffin$Q52 == "Bloco de Esquerda (BE)"] <- 35211
dffin$drawn_cmp_id[dffin$partydrawn == "Bloco de Esquerda (BE)"] <- 35211
dffin$q52_cmp_id[dffin$Q52 == "Partido Comunista Português (PCP)"] <- 35220
dffin$drawn_cmp_id[dffin$partydrawn == "Partido Comunista Português (PCP)"] <- 35220
dffin$q52_cmp_id[dffin$Q52 == "Partido Socialista (PS)"] <- 35311
dffin$drawn_cmp_id[dffin$partydrawn == "Partido Socialista (PS)"] <- 35311



dffin$q52_cmp_id[dffin$Q52 == "Partidul Social Democrat (PSD)"] <- 93223
dffin$drawn_cmp_id[dffin$partydrawn == "Partidul Social Democrat (PSD)" ] <- 93223
dffin$q52_cmp_id[dffin$Q52 == "Alianța Liberalilor și Democraților (ALDE)"] <- 93420
dffin$drawn_cmp_id[dffin$partydrawn == "Alianța Liberalilor și Democraților (ALDE)" ] <- 93420
dffin$q52_cmp_id[dffin$Q52 == "Partidul Național Liberal (PNL)"] <- 93430
dffin$drawn_cmp_id[dffin$partydrawn == "Partidul Național Liberal (PNL)" ] <- 93430
dffin$q52_cmp_id[dffin$Q52 == "Uniunea Salvați România (USR)"] <- 93440
dffin$drawn_cmp_id[dffin$partydrawn == "Uniunea Salvați România (USR)"] <- 93440
dffin$q52_cmp_id[dffin$Q52 == "Partidul Mișcarea Populară (PMP)"] <- 93540
dffin$drawn_cmp_id[dffin$partydrawn == "Partidul Mișcarea Populară (PMP)"] <- 93540
dffin$q52_cmp_id[dffin$Q52 == "Uniunea Democrată Maghiară din România (UDMR - Romániai Magyar Demokrata Szövetség (RMDSZ))"] <- 93951
dffin$drawn_cmp_id[dffin$partydrawn == "Uniunea Democrată Maghiară din România (UDMR - Romániai Magyar Demokrata Szövetség (RMDSZ))"] <- 93951



dffin$q52_cmp_id[dffin$Q52 == "Smer – sociálna demokracia (Smer-SD)"] <- 96423
dffin$drawn_cmp_id[dffin$partydrawn == "Smer – sociálna demokracia (Smer-SD)"] <- 96423
dffin$q52_cmp_id[dffin$Q52 == "Sloboda a Solidarita (SaS)"] <- 96440
dffin$drawn_cmp_id[dffin$partydrawn == "Sloboda a Solidarita (SaS)"] <- 96440
dffin$q52_cmp_id[dffin$Q52 == "Kresťanskodemokratické hnutie (KDH)"] <- 96521
dffin$drawn_cmp_id[dffin$partydrawn == "Kresťanskodemokratické hnutie (KDH)"] <- 96521
dffin$q52_cmp_id[dffin$Q52 == "Obyčajní Ľudia a nezávislé osobnosti (OĽaNO)"] <- 96620
dffin$drawn_cmp_id[dffin$partydrawn == "Obyčajní Ľudia a nezávislé osobnosti (OĽaNO)"] <- 96620
dffin$q52_cmp_id[dffin$Q52 == "Slovenská národná strana (SNS)"] <- 96710
dffin$drawn_cmp_id[dffin$partydrawn == "Slovenská národná strana (SNS)"] <- 96710
dffin$q52_cmp_id[dffin$Q52 == "Kotleba – Ľudová strana Naše Slovensko (ĽSNS)" ] <- 96720
dffin$drawn_cmp_id[dffin$partydrawn == "Kotleba – Ľudová strana Naše Slovensko (ĽSNS)" ] <- 96720
dffin$q52_cmp_id[dffin$Q52 == "SME RODINA – Boris Kollár (SME RODINA)" ] <- 96725
dffin$drawn_cmp_id[dffin$partydrawn == "SME RODINA – Boris Kollár (SME RODINA)" ] <- 96725
dffin$q52_cmp_id[dffin$Q52 == "Most–Híd" ] <- 96955
dffin$drawn_cmp_id[dffin$partydrawn == "Most–Híd" ] <- 96955
dffin$q52_cmp_id[dffin$Q52 == "Slovenská demokratická a kresťanská únia – Demokratická strana (SDKÚ–DS)" ] <- 96523
dffin$drawn_cmp_id[dffin$partydrawn == "Slovenská demokratická a kresťanská únia – Demokratická strana (SDKÚ–DS)" ] <- 96523
dffin$q52_cmp_id[dffin$Q52 == "Strana maďarskej komunity - Magyar Közösség Pártja (SMK-MPK)" ] <- 96952
dffin$drawn_cmp_id[dffin$partydrawn == "Strana maďarskej komunity - Magyar Közösség Pártja (SMK-MPK)"] <- 96952



dffin$q52_cmp_id[dffin$Q52 == "Socialni demokrati (SD)"] <- 97322
dffin$drawn_cmp_id[dffin$partydrawn == "Socialni demokrati (SD)"] <- 97322
dffin$q52_cmp_id[dffin$Q52 == "Slovenska demokratska stranka (SDS)"] <- 97330
dffin$drawn_cmp_id[dffin$partydrawn == "Slovenska demokratska stranka (SDS)"] <- 97330
dffin$q52_cmp_id[dffin$Q52 == "Stranka Alenke Bratušek"] <- 97460
dffin$drawn_cmp_id[dffin$partydrawn == "Stranka Alenke Bratušek"] <- 97460
dffin$q52_cmp_id[dffin$Q52 == "Stranka modernega centra (SMC)" ] <- 97461
dffin$drawn_cmp_id[dffin$partydrawn == "Stranka modernega centra (SMC)" ] <- 97461
dffin$q52_cmp_id[dffin$Q52 == "Nova Slovenija - Krščanski demokrati (N.Si)" ] <- 97522
dffin$drawn_cmp_id[dffin$partydrawn == "Nova Slovenija - Krščanski demokrati (N.Si)" ] <- 97522
dffin$q52_cmp_id[dffin$Q52 == "Demokratična stranka upokojencev Slovenije (DeSUS)" ] <- 97951
dffin$drawn_cmp_id[dffin$partydrawn == "Demokratična stranka upokojencev Slovenije (DeSUS)" ] <- 97951

dffin$q52_cmp_id[dffin$Q52 == "Slovenska nacionalna stranka (SNS)" ] <- 97710
dffin$drawn_cmp_id[dffin$partydrawn == "Slovenska nacionalna stranka (SNS)" ] <- 97710
dffin$q52_cmp_id[dffin$Q52 == "Slovenska ljudska stranka (SLS)" ] <- 97521
dffin$drawn_cmp_id[dffin$partydrawn == "Slovenska ljudska stranka (SLS)" ] <- 97521
dffin$q52_cmp_id[dffin$Q52 == "Levica" ] <- 97230
dffin$drawn_cmp_id[dffin$partydrawn == "Levica" ] <- 97230
dffin$q52_cmp_id[dffin$Q52 == "Lista Marjana Šarca (LMŠ)" ] <- 97341
dffin$drawn_cmp_id[dffin$partydrawn == "Lista Marjana Šarca (LMŠ)" ] <- 97341
dffin$q52_cmp_id[dffin$Q52 == "Pozitivna Slovenija (PS)" ] <- 97340
dffin$drawn_cmp_id[dffin$partydrawn == "Pozitivna Slovenija (PS)" ] <- 97340



dffin$q52_cmp_id[dffin$Q52 == "Euskal Herria Bildu (EH Bildu)"] <- 33095
dffin$drawn_cmp_id[dffin$partydrawn == "Euskal Herria Bildu (EH Bildu)"] <- 33095
dffin$q52_cmp_id[dffin$Q52 == "Partido Socialista Obrero Español (PSOE)"] <- 33320
dffin$drawn_cmp_id[dffin$partydrawn == "Partido Socialista Obrero Español (PSOE)"] <- 33320
dffin$q52_cmp_id[dffin$Q52 == "Ciudadanos - Partido de la Ciudadanía (C's)"] <- 33420
dffin$drawn_cmp_id[dffin$partydrawn == "Ciudadanos - Partido de la Ciudadanía (C's)"] <- 33420
dffin$q52_cmp_id[dffin$Q52 == "Partido Popular (PP)"] <- 33610
dffin$drawn_cmp_id[dffin$partydrawn == "Partido Popular (PP)"] <- 33610
dffin$q52_cmp_id[dffin$Q52 == "Euzko Alderdi Jeltzalea (EAJ/PNV)"] <- 33902
dffin$drawn_cmp_id[dffin$partydrawn == "Euzko Alderdi Jeltzalea (EAJ/PNV)"] <- 33902
dffin$q52_cmp_id[dffin$Q52 == "Esquerra Republicana de Catalunya (ERC)"] <- 33905
dffin$drawn_cmp_id[dffin$partydrawn == "Esquerra Republicana de Catalunya (ERC)"] <- 33905

dffin$q52_cmp_id[dffin$Q52 == "Podemos"] <- 33210
dffin$drawn_cmp_id[dffin$partydrawn == "Podemos"] <- 33210
dffin$q52_cmp_id[dffin$Q52 == "Izquierda Unida (IU)"] <- 33220
dffin$drawn_cmp_id[dffin$partydrawn == "Izquierda Unida (IU)"] <- 33220
dffin$q52_cmp_id[dffin$Q52 == "Vox"] <- 33710
dffin$drawn_cmp_id[dffin$partydrawn == "Vox"] <- 33710
dffin$q52_cmp_id[dffin$Q52 == "Bloque Nacionalista Galego (BNG)"] <- 33908
dffin$drawn_cmp_id[dffin$partydrawn == "Bloque Nacionalista Galego (BNG)"] <- 33908
dffin$q52_cmp_id[dffin$Q52 == "Unión Progreso y Democracia (UPyD)"] <- 33440
dffin$drawn_cmp_id[dffin$partydrawn == "Unión Progreso y Democracia (UPyD)"] <- 33440
dffin$q52_cmp_id[dffin$Q52 == "Partit Demòcrata Europeo Català (PDeCAT)"] <- 33912
dffin$drawn_cmp_id[dffin$partydrawn == "Partit Demòcrata Europeo Català (PDeCAT)"] <- 33912



dffin$q52_cmp_id[dffin$Q52 == "Miljöpartiet de Gröna (MP)"] <- 11110
dffin$drawn_cmp_id[dffin$partydrawn == "Miljöpartiet de Gröna (MP)"] <- 11110
dffin$q52_cmp_id[dffin$Q52 == "Vänsterpartiet (V)"] <- 11220
dffin$drawn_cmp_id[dffin$partydrawn == "Vänsterpartiet (V)"] <- 11220
dffin$q52_cmp_id[dffin$Q52 == "Socialdemokratiska arbetarpartiet (SAP)"] <- 11320
dffin$drawn_cmp_id[dffin$partydrawn == "Socialdemokratiska arbetarpartiet (SAP)"] <- 11320
dffin$q52_cmp_id[dffin$Q52 == "Liberalerna (L)"] <- 11420
dffin$drawn_cmp_id[dffin$partydrawn == "Liberalerna (L)"] <- 11420
dffin$q52_cmp_id[dffin$country == "Swe" & dffin$Q52 == "Kristdemokraterna (KD)"] <- 11520
dffin$drawn_cmp_id[dffin$country == "Swe" & dffin$partydrawn == "Kristdemokraterna (KD)"] <- 11520
dffin$q52_cmp_id[dffin$country == "Swe" & dffin$Q52 == "Moderata samlingspartiet (M)"] <- 11620
dffin$drawn_cmp_id[dffin$country == "Swe" & dffin$partydrawn == "Moderata samlingspartiet (M)"] <- 11620
dffin$q52_cmp_id[dffin$country == "Swe" & dffin$Q52 == "Sverigedemokraterna (SD)"] <- 11710
dffin$drawn_cmp_id[dffin$country == "Swe" & dffin$partydrawn == "Sverigedemokraterna (SD)"] <- 11710
dffin$q52_cmp_id[dffin$country == "Swe" & dffin$Q52 == "Centerpartiet (C)"] <- 11810
dffin$drawn_cmp_id[dffin$country == "Swe" & dffin$partydrawn == "Centerpartiet (C)"] <- 11810



dffin$q52_cmp_id[dffin$Q52 == "Green Party of England and Wales (GP)"] <- 51110
dffin$q52_cmp_id[dffin$country == "UK" & dffin$Q52 == "Sinn Féin (SF)"] <- 51210
dffin$q52_cmp_id[dffin$Q52 == "Labour Party (Labour)"] <- 51320
dffin$q52_cmp_id[dffin$Q52 == "Liberal Democrats (Lib Dem)"] <- 51421
dffin$q52_cmp_id[dffin$Q52 == "Conservative and Unionist Party (Conservative Party)"] <- 51620
dffin$q52_cmp_id[dffin$Q52 == "Plaid Cymru – Party of Wales (PL-PW)"] <- 51901
dffin$q52_cmp_id[dffin$Q52 == "Scotish National Party (SNP)"] <- 51902
dffin$q52_cmp_id[dffin$Q52 == "Democratic Unionist Party (DUP)"] <- 51903
dffin$q52_cmp_id[dffin$Q52 == "UK Independence Party (UKIP)"] <- 51951

dffin$drawn_cmp_id[dffin$partydrawn == "Green Party of England and Wales (GP)"] <- 51110
dffin$drawn_cmp_id[dffin$country == "UK" & dffin$partydrawn == "Sinn Féin (SF)"] <- 51210
dffin$drawn_cmp_id[dffin$partydrawn == "Labour Party (Labour)"] <- 51320
dffin$drawn_cmp_id[dffin$partydrawn == "Liberal Democrats (Lib Dem)"] <- 51421
dffin$drawn_cmp_id[dffin$partydrawn == "Conservative and Unionist Party (Conservative Party)"] <- 51620
dffin$drawn_cmp_id[dffin$partydrawn == "Plaid Cymru – Party of Wales (PL-PW)"] <- 51901
dffin$drawn_cmp_id[dffin$partydrawn == "Scotish National Party (SNP)"] <- 51902
dffin$drawn_cmp_id[dffin$partydrawn == "Democratic Unionist Party (DUP)"] <- 51903
dffin$drawn_cmp_id[dffin$partydrawn == "UK Independence Party (UKIP)"] <- 51951
dffin$q52_cmp_id[dffin$Q52 == "Ulster Unionist Party (UUP)"] <- 51621
dffin$drawn_cmp_id[dffin$partydrawn == "Ulster Unionist Party (UUP)"] <- 51621
dffin$q52_cmp_id[dffin$Q52 == "Social Democratic & Labour Party (SDLP)"] <- 51340
dffin$drawn_cmp_id[dffin$partydrawn == "Social Democratic & Labour Party (SDLP)"] <- 51340





dffin <- merge(dffin, formerge, all.x = T, by.x = "q52_cmp_id", by.y = "party")
names(dffin)[names(dffin) == "rile"] <- "Q52_partypos_lr"
names(dffin)[names(dffin) == "eu"] <- "Q52_partypos_eu"
dffin <- merge(dffin, formerge, all.x = T, by.x = "drawn_cmp_id", by.y = "party")
names(dffin)[names(dffin) == "rile"] <- "drawn_partypos_lr"
names(dffin)[names(dffin) == "eu"] <- "drawn_partypos_eu"


dffin$cmp_dist_lr <- abs(dffin$Q52_partypos_lr - dffin$drawn_partypos_lr)

dffin <- merge(dffin, cmp19b2000, all.x = T, by.x = "q52_cmp_id", by.y = "party")
names(dffin)[names(dffin) == "avg_rile_since2000"] <- "Q52_avg_rile_since2000"
dffin <- merge(dffin, cmp19b2000, all.x = T, by.x = "drawn_cmp_id", by.y = "party")
names(dffin)[names(dffin) == "avg_rile_since2000"] <- "drawn_avg_rile_since2000"
dffin$avg_cmp_dist_lr_2000 <- abs(dffin$Q52_avg_rile_since2000 - dffin$drawn_avg_rile_since2000)




# Country-Level Variables ####

# gdp pc (eurostat) 
# source: https://ec.europa.eu/eurostat/web/main/data/database
# downloaded on january 4, 2021
es <- read.csv("data/eurostat/estat_nama_10_pc_filtered.tsv", sep = "\t")
es <- subset(es, select = c(freq.unit.na_item.geo.TIME_PERIOD,X2019))
es$country <- NA
es$country[grep(",AT" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Aus"
es$country[grep(",BE" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Bel"
es$country[grep(",BG" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Bul"
es$country[grep(",CZ" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Cze"
es$country[grep(",DK" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Den"
es$country[grep(",DE" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Ger"
es$country[grep(",EE" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Est"
es$country[grep(",EL" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Gre"
es$country[grep(",ES" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Spa"
es$country[grep(",FI" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Fin"
es$country[grep(",FR" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Fra"
es$country[grep(",HR" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Cro"
es$country[grep(",HU" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Hun"
es$country[grep(",IE" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Ire"
es$country[grep(",IT" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Ita"
es$country[grep(",LT" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Lit"
es$country[grep(",LV" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Lat"
es$country[grep(",NL" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Net"
es$country[grep(",PL" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Pol"
es$country[grep(",PT" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Por"
es$country[grep(",RO" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Rom"
es$country[grep(",SE" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Swe"
es$country[grep(",SI" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Slovenia"
es$country[grep(",SK" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "Slovakia"
es$country[grep(",UK" , es$freq.unit.na_item.geo.TIME_PERIOD)] <- "UK"

es$gdppc <- es$X2019
es$gdppc <- gsub(" p", "", es$gdppc)
es$gdppc <- gsub(" e", "", es$gdppc)
es$gdppc <- trimws(es$gdppc, "both")
es$gdppc <- as.numeric(es$gdppc)

dffin <- merge(dffin, es, all.x = T, by = "country")
rm(es)


# gini (world bank) ####
eu25 <- levels(dffin$country)
labcountries <- eu25
labcountries[eu25 == "Aus"] <- "Austria"
labcountries[eu25 == "Bel"] <- "Belgium"
labcountries[eu25 == "Bul"] <- "Bulgaria"
labcountries[eu25 == "Cro"] <- "Croatia"
labcountries[eu25 == "Cze"] <- "Czech Republic"
labcountries[eu25 == "Den"] <- "Denmark"
labcountries[eu25 == "Est"] <- "Estonia"
labcountries[eu25 == "Fin"] <- "Finland"
labcountries[eu25 == "Fra"] <- "France"
labcountries[eu25 == "Ger"] <- "Germany"
labcountries[eu25 == "Gre"] <- "Greece"
labcountries[eu25 == "Hun"] <- "Hungary"
labcountries[eu25 == "Ire"] <- "Ireland"
labcountries[eu25 == "Ita"] <- "Italy"
labcountries[eu25 == "Lat"] <- "Latvia"
labcountries[eu25 == "Lit"] <- "Lithuania"
labcountries[eu25 == "Net"] <- "Netherlands"
labcountries[eu25 == "Pol"] <- "Poland"
labcountries[eu25 == "Por"] <- "Portugal"
labcountries[eu25 == "Rom"] <- "Romania"
labcountries[eu25 == "Slovakia"] <- "Slovakia"
labcountries[eu25 == "Slovenia"] <- "Slovenia"
labcountries[eu25 == "Spa"] <- "Spain"
labcountries[eu25 == "Swe"] <- "Sweden"
labcountries[eu25 == "UK"] <- "United Kingdom"


# source: https://www.worldbank.org/en/home
# downloaded on December 29, 2020
wb <- read.csv("data/world bank/API_SI.POV.GINI_DS2_en_csv_v2_1864976.csv", header = F, stringsAsFactors = F)
names(wb) <- wb[3,]
wb <- wb[-c(1:3),]
wb <- wb[wb$`Country Name` %in% c(labcountries, "Slovak Republic"),]
wb$`Country Name`[ wb$`Country Name` == "Slovak Republic"] <- "Slovakia"
wb <- subset(wb, select = c("Country Name","2015","2016","2017","2018","2019","2020"))
# use most recent data (after visual examination)
wb$gini <- wb$`2017`
wb$gini[is.na(wb$gini)] <- wb$`2016`[is.na(wb$gini)]
wb <- subset(wb, select = c("Country Name","gini"))

tmp <- data.frame(cbind(eu25=as.character(eu25),labcountries))
wb <- merge(wb, tmp, by.x = "Country Name", by.y = "labcountries")
dffin <- merge(dffin, wb, by.x = "country", by.y = "eu25", all.x = T)
rm(wb)
dffin$`Country Name` <- NULL




# population size
# population figures: eurostat 2020 
# https://ec.europa.eu/eurostat/databrowser/view/tps00001/default/table?lang=en
# https://en.wikipedia.org/wiki/List_of_European_Union_member_states_by_population
dffin$population <- NA
dffin$population[dffin$country %in% c("Ger")] <- 83.1
dffin$population[dffin$country %in% c("Fra")] <- 67.1
dffin$population[dffin$country %in% c("Ita")] <- 60.2
dffin$population[dffin$country %in% c("UK")] <- 67.9
dffin$population[dffin$country %in% c("Spa")] <- 47.3
dffin$population[dffin$country %in% c("Pol")] <- 38.0
dffin$population[dffin$country %in% c("Rom")] <- 19.3
dffin$population[dffin$country %in% c("Net")] <- 17.4
dffin$population[dffin$country %in% c("Bel")] <- 11.5
dffin$population[dffin$country %in% c("Gre")] <- 10.7
dffin$population[dffin$country %in% c("Cze")] <- 10.7
dffin$population[dffin$country %in% c("Swe")] <- 10.3
dffin$population[dffin$country %in% c("Por")] <- 10.3
dffin$population[dffin$country %in% c("Hun")] <- 9.8
dffin$population[dffin$country %in% c("Aus")] <- 8.9
dffin$population[dffin$country %in% c("Bul")] <- 7.0
dffin$population[dffin$country %in% c("Den")] <- 5.8
dffin$population[dffin$country %in% c("Fin")] <- 5.5
dffin$population[dffin$country %in% c("Slovakia")] <- 5.5
dffin$population[dffin$country %in% c("Ire")] <- 5.0
dffin$population[dffin$country %in% c("Cro")] <- 4.1
dffin$population[dffin$country %in% c("Lit")] <- 2.8
dffin$population[dffin$country %in% c("Slovenia")] <- 2.1
dffin$population[dffin$country %in% c("Lat")] <- 1.9
dffin$population[dffin$country %in% c("Est")] <- 1.3




# effective number of parties
# source: parlgov, downloaded on August 5, 2021
pg <- read.csv("data/parlgov/pg_aug2021.csv")
pg <- pg[!(as.numeric(substr(pg$election_date,1,4)) >= 2020 & pg$country_name == "Croatia"),]
pg <- pg[!(as.numeric(substr(pg$election_date,1,4)) >= 2020 & pg$country_name == "Ireland"),]
pg <- pg[!(as.numeric(substr(pg$election_date,1,4)) >= 2020 & pg$country_name == "Lithuania"),]
pg <- pg[!(as.numeric(substr(pg$election_date,1,4)) >= 2020 & pg$country_name == "Romania"),]

pg$start_date <- as.Date(pg$start_date)
lastcabs <- aggregate(pg$start_date, list(pg$country_name), max)
names(lastcabs) <- c("country_name","lastcabinet")
pg <- merge(pg, lastcabs, all.x = T, by = "country_name")
pg <- pg[pg$start_date == pg$lastcabinet,]

pg$country <- pg$country_name
pg$country <- substr(pg$country, 1,3)
pg$country[pg$country_name == "Slovenia"] <- "Slovenia"
pg$country[pg$country_name == "Slovakia"] <- "Slovakia"
pg$country[pg$country_name == "United Kingdom"] <- "UK"
pg <- pg[pg$country_name != "Australia",]


# According to Laakso and Taagepera (1979), the effective number of parties is computed by the following formula:
pg$seatshare <- pg$seats / pg$election_seats_total
pg$seatshare2 <- pg$seatshare ^2

stuff <- aggregate(pg$seatshare2, list(pg$country), sum)
names(stuff) <- c("country","sumsquaredseatshare")
stuff$eff_n_parties <-  1 / stuff$sumsquaredseatshare
dffin <- merge(dffin, stuff, all.x = T, by = "country")




# polarization ####
# cmp polarization: 
# polarization (LR dimension)
# dalton index
cmp <- read.csv("data/cmp2020b.csv", stringsAsFactors = F)
cmp$election_date <- as.Date(cmp$edate, "%d/%m/%Y")
cmp <- cmp[cmp$election_date >= "1948-05-08" & cmp$election_date <= "2019-09-01",] 
tmp <- aggregate( cmp$date, list(cmp$countryname), max)
names(tmp) <- c("countryname","lastelection_before_survey")
cmp <- merge(cmp, tmp, all.x = T, by = "countryname")
cmp <- cmp[cmp$date == cmp$lastelection,]
cmp <- cmp[cmp$eumember == 10,]
aggregate( cmp$date, list(cmp$countryname), max)

tmp <- aggregate(cmp$rile, list(cmp$countryname), mean, na.rm = T)
names(tmp) <- c("countryname","mean_rileposition")
cmp <- merge(cmp, tmp, all.x = T, by = "countryname")
cmp$weighted_deviations <- (cmp$rile - cmp$mean_rileposition)^2 * cmp$pervote / 100
tmp <- aggregate(cmp$weighted_deviations, list(cmp$countryname), mean, na.rm = T)
names(tmp) <- c("countryname","sum_weighted_deviations")
tmp$dalton_polarization_lr <- sqrt(tmp$sum_weighted_deviations)
cmp <- tmp 
tmp <- data.frame(cbind(eu25=as.character(eu25), labcountries))
cmp <- merge(cmp, tmp , all.x = T, by.x = "countryname", by.y = "labcountries")
cmp$countryname <- cmp$sum_weighted_deviations <- NULL

dffin <- merge(dffin, cmp, all.x = T, by.x = "country", by.y = "eu25")




# logged average district magnitude (see Gidron et al. 2020)
# bormann et al data (source: http://mattgolder.com/elections)
bg <- read.dta13("data/bormann golder 2013/es_data-v3.dta")

bg$test <- as.numeric(bg$seats) / as.numeric(bg$tier1_districts)

tmp <- aggregate(bg$year,
                 list(country=bg$country),
                 max)
bg <- merge(bg, tmp, all.x = T, by = "country")
bg <- bg[bg$year == bg$x,]

bg <- subset(bg, select = c("country", "tier1_avemag"))
bg <- bg[bg$tier1_avemag != "NA",]
bg <- bg[!duplicated(bg),]

cdf <- data.frame(eu25,labcountries)
dffin <- merge(dffin, cdf, all.x = T, by.x = "country", by.y = "eu25")
rm(cdf)

dffin$labcountries <- as.character(dffin$labcountries)
unique(dffin$labcountries[!(dffin$labcountries %in% bg$country)])
dffin <- merge(dffin, bg, all.x = T, by.x = "labcountries", by.y = "country")
dffin$tier1_avemag <- as.numeric(dffin$tier1_avemag)

dffin$tier1_avemag[dffin$country == "Aus"] <- 4.26	
dffin$tier1_avemag[dffin$country == "Por"] <- 10.45


dffin$tier1_avemag <- log(dffin$tier1_avemag)

aggregate(dffin$tier1_avemag, list(dffin$country), mean)
rm(bg)




# # bipolarity index : NPI (Maoz and Somer-Topcu) #### 
cmp19b <- read.csv('data/cmp21a.csv', stringsAsFactors = F)
cmp19b <-cmp19b[cmp19b$countryname %in%  cmp19b$countryname[cmp19b$eumember == 10],]
cmp19b$party_seatshare <- cmp19b$absseat / cmp19b$totseats

wdf <- subset(cmp19b, select = c(countryname,date,party,rile,party_seatshare))


wdf$rile <- ( wdf$rile - min(wdf$rile, na.rm = T ) ) / ( max(wdf$rile, na.rm = T) - min(wdf$rile, na.rm = T ) )
summary(wdf$rile)
wdf$rile <- wdf$rile + 0.001

countries <- sort(unique(wdf$countryname))
country_npi <- list()
for ( country in 1:length(countries)){
  countrydf <- wdf[wdf$countryname == countries[country],]
  uniquedates <- unique(countrydf$date)
  
  country_dates <- list()
  for ( d in 1:length(uniquedates)){
    electiondf <- countrydf[countrydf$date == uniquedates[d],] 
    tmpmat <- cbind(electiondf$party,
                    electiondf$rile)
    # sorting step probably not necessary:
    tmpmat <- tmpmat[order(tmpmat[,1]),]
    
    # exclude parties with NA on position: no way to calculate their network position
    tmpmat <- tmpmat[!is.na(tmpmat[,2]),]
    # fix : positions cannot be exactly 0 or we get divide-by-zero error; correct by tiny constant #####
    if ( min(tmpmat[,2]) <= 0){
      tmpmat[,2] <- tmpmat[,2] + abs(min(tmpmat[,2])) + runif(1, 0.000001, 0.00001)
    }
    
    
    
    outmat <- cbind(tmpmat[,2]) %*% rbind(tmpmat[,2])
    
    # standardize, right? ####
    outmat_stdized <- outmat
    for ( row in 1:nrow(outmat)){
      for ( col in 1:nrow(outmat)){
        outmat_stdized[row, col] <- outmat_stdized[row, col] / min(c (outmat[row, row], outmat[col, col]))
      }
    }
    
    rownames(outmat_stdized) <- colnames(outmat_stdized) <- tmpmat[,1]
    
    # actually mean, but median is more robust to distortions (this is also what they propose as an alternative, see maoz & st 2010, footnote 29)
    outmat_stdized <- ifelse(outmat_stdized >= mean(outmat_stdized) , 1, 0)
    

    
    # STEP 1: IDENTIFY PROTO-COALITIONS ####
    # original: see notes dec 20, 2021 .R
    
    e <- 1
    candidatesets <- list()
    for ( i in 2:nrow(outmat_stdized)){
      (combinations <- combn(1:nrow(outmat_stdized), i))
      
      for ( c in 1:ncol(combinations)){
        combinations[,c]
        submatrix <- outmat_stdized[c(combinations[,c] ), c(combinations[,c])]
        # diagonal elements are always 1 (unit has relationship with itself) : 
        diag(submatrix) <- 1
        indicator <- all(submatrix == 1)
        if ( indicator == T){
          candidatesets[[e]] <- combinations[,c]
          e <- e + 1
          
        }
      }
    }
    
    # now drop perfect subsets
    for ( e in 1:length(candidatesets)){
      candidatesets[[e]]
      for ( not_e in c(1:length(candidatesets))[!(1:length(candidatesets) %in% e)]){
        if (all(candidatesets[[e]] %in% candidatesets[[not_e]]) == T ){
          candidatesets[[e]] <- NA
        }
      }
    }
    
    
    for ( e in length(candidatesets):1){
      if ( is.na(candidatesets[[e]][1] ) == T){
        candidatesets[[e]] <- NULL
      }
    }
    
    
    protocoalitions <- candidatesets
    
    # are all parties covered?
    coveredparties <- sort(unique(unlist(protocoalitions)))
    stilltobecovered <- c(1:nrow(outmat_stdized))[!( c(1:nrow(outmat_stdized)) %in% coveredparties)]
    
    # stilltobecovered <- c(444,666)
    if ( length(stilltobecovered) > 0){
      for ( i in 1:length(stilltobecovered))
        protocoalitions[[length(protocoalitions) + 1]] <- stilltobecovered[i]
    }
    
    
    
    protocoalitions
    

    
    # now for each pro-coalition i, compute
    #  seat share p_i
    #  cohesion c_i
    
    coalition_stats <- list()
    for ( i in 1:length(protocoalitions)){
      # Seatshare ####
      seatshare_i <- sum(electiondf$party_seatshare[electiondf$party %in% tmpmat[ protocoalitions[[i]] ,1]])
      
      
      
      
      # Cohesion ####
      # c_i = 1 - ( sqrt(sum sum (d_r - dq)^2 ) ) / ( s_i * (s_i - 1 ) * max(d) )
      # (formula 1, p.8)
      cohesion_i <- NA
      
      protocoalitions[[i]]
      if ( length(protocoalitions[[i]]) > 1){
        sumvec <- NA
        for ( j in 1:(length(protocoalitions[[i]])- 1) ){
          d_r <- tmpmat[protocoalitions[[i]][j] ,2] 
          d_q <- tmpmat[protocoalitions[[i]][j +1] ,2]
          sumvec[j] <- (d_r - d_q)^2
        }
        enumerator <- sqrt(sum(sumvec))
        
        denominator <- length(protocoalitions[[i]]) * ( length(protocoalitions[[i]]) - 1) * max(tmpmat[protocoalitions[[i]], 2])
        
        cohesion_i <- 1 - enumerator / denominator
        
      } else {
        # if a protocoalition consists of 1 party only, then cohesion is automatically 1
        cohesion_i <- 1
      }
      
      
      coalition_stats[[i]] <- cbind(seatshare_i, cohesion_i)
      
    }
    
    coalition_stats <- do.call(rbind , coalition_stats)
    # cpol = (4 * sum( p_i * (1-p_i) * c_i)) / k
    cpol <- (4 * sum(coalition_stats[,1] * ( 1 - coalition_stats[,1] ) * coalition_stats[,2])) / 
      nrow(coalition_stats)
    
    # create CA (parties x proto-coals)
    ca <- matrix(0, nrow = nrow(tmpmat) , ncol = length(protocoalitions))
    for ( i in 1:length(protocoalitions)){
      ca[protocoalitions[[i]],i] <- 1
    }
    
    co <- t(ca) %*% ca
    
    # standardize (also standardize diagonal entries?)
    co_hat <- co
    for ( row in 1:nrow(co)){
      for ( col in 1:ncol(co)){
        co_hat[row, col] <- co[row,col] / co[col,col]
      }
    }
    
    coi <- ( sum(co_hat) - length(protocoalitions) ) / ( length(protocoalitions) * (length(protocoalitions) - 1) )
    
    
    
    npi <- cpol * ( 1 - coi)
    
    
    
    country_dates[[d]] <- data.frame(country=unique(electiondf$countryname),
                                     electiondate=unique(electiondf$date),
                                     npi=npi,
                                     rile_sd=sd(electiondf$rile, na.rm = T))
  } # end d-loop
  
  country_npi[[country]] <- do.call(rbind, country_dates)
} # end country-loop


country_npi <- do.call(rbind, country_npi)




country_npi <- country_npi[!is.na(country_npi$npi),]
country_npi$country <- as.character(country_npi$country)
country_npi$country[country_npi$country == "Austria"]  <- "Aus"
country_npi$country[country_npi$country == "Belgium"]  <- "Bel"
country_npi$country[country_npi$country == "Bulgaria"]  <- "Bul"
country_npi$country[country_npi$country == "Croatia"]  <- "Cro"
country_npi$country[country_npi$country == "Czech Republic"]  <- "Cze"
country_npi$country[country_npi$country == "Denmark"]  <- "Den"
country_npi$country[country_npi$country == "Estonia"]  <- "Est"
country_npi$country[country_npi$country == "Finland"]  <- "Fin"
country_npi$country[country_npi$country == "France"]  <- "Fra"
country_npi$country[country_npi$country == "Germany"]  <- "Ger"
country_npi$country[country_npi$country == "Greece"]  <- "Gre"
country_npi$country[country_npi$country == "Hungary"]  <- "Hun"
country_npi$country[country_npi$country == "Ireland"]  <- "Ire"
country_npi$country[country_npi$country == "Italy"]  <- "Ita"
country_npi$country[country_npi$country == "Latvia"]  <- "Lat"
country_npi$country[country_npi$country == "Lithuania"]  <- "Lit"
country_npi$country[country_npi$country == "Netherlands"]  <- "Net"
country_npi$country[country_npi$country == "Poland"]  <- "Pol"
country_npi$country[country_npi$country == "Portugal"]  <- "Por"
country_npi$country[country_npi$country == "Romania"]  <- "Rom"
country_npi$country[country_npi$country == "Spain"]  <- "Spa"
country_npi$country[country_npi$country == "Sweden"]  <- "Swe"
country_npi$country[country_npi$country == "United Kingdom"] <- "UK"

country_npi1 <- aggregate(country_npi$npi, 
                          list(country=country_npi$country),
                          mean)
names(country_npi1)[names(country_npi1) == "x"] <- "avg_npi"
dffin <- merge(dffin, country_npi1, all.x = T, by = "country")

country_npi <- country_npi[country_npi$electiondate >= 200001,]
country_npi2 <- aggregate(country_npi$npi, 
                          list(country=country_npi$country),
                          mean)
names(country_npi2)[names(country_npi2) == "x"] <- "avg_npi_since2000"
dffin <- merge(dffin, country_npi2, all.x = T, by = "country")


tmp <- aggregate(country_npi$electiondate, 
                 list(country=country_npi$country),
                 max)
country_npi <- merge(country_npi, tmp, all.x = T, by = "country")
country_npi <- country_npi[country_npi$electiondate == country_npi$x ,]

country_npi$x <- NULL
country_npi$electiondate <- NULL
names(country_npi)[names(country_npi) == "npi"] <- "current_npi"

dffin <- merge(dffin, country_npi, all.x = T, by = "country")








pg <- read.csv("data/parlgov/pg_aug2021.csv")
pg$election_date <- as.Date(pg$election_date)
pg$start_date <- as.Date(pg$start_date)


# fixes / bridge ids
tmp0 <- sort(unique(pg$start_date[pg$party_id == 1029])) # 212 1029                                                 Socialistische Partij                   SP    Belgium
tmp1 <- sort(unique(pg$start_date[pg$party_id == 1113])) #  1113                Socialistische Partij Anders / Sociaal-Liberale Partij              SPa+Spi    Belgium
pg$party_id[pg$party_id == 1029] <- 1113
tmp0 <- sort(unique(pg$start_date[pg$party_id == 167])) # 167            Belgische Socialistische Partij â€“ Parti Socialiste Belge              BSP-PSB    Belgium
tmp1 <- sort(unique(pg$start_date[pg$party_id == 1378])) # 1378                                                      Parti Socialiste                   PS    Belgium
intersect(tmp0 , tmp1) # they should not intersect, because they are the same party
pg$party_id[pg$party_id == 167] <- 1378

tmp1 <- tmp0 <- pg[pg$party_id == 972,] # 85   972               Christen-Democratisch & Vlaams / Nieuw-Vlaams Alliantie               CD+NVA    Belgium
pg <- pg[pg$party_id != 972,]
tmp0$party_id <- 723 # dffin$drawn_pg_id[ dffin$partydrawn %in% c("Christen-Democratisch en Vlaams (CD&V)")] <- 723
tmp1$party_id <- 501 # dffin$Q52_pg_id[ dffin$Q52 %in% c("Nieuw-Vlaamse Alliantie (N-VA)")] <- 501
pg <- rbind(pg, tmp0, tmp1)
tmp1 <- tmp0 <- pg[pg$party_id == 1051,] # # 79  1051                   Parti Social ChrÃ©tien â€“ Christelijke Volkspartij              PSC-CVP    Belgium PSC=cdh, CVP = CDV
pg <- pg[pg$party_id != 1051,]
tmp0$party_id <- 723 # dffin$drawn_pg_id[ dffin$partydrawn %in% c("Christen-Democratisch en Vlaams (CD&V)")] <- 723
tmp1$party_id <- 1192 # dffin$Q52_pg_id[ dffin$Q52 %in% c("Centre démocrate humaniste (cdH)")] <- 1192
pg <- rbind(pg, tmp0, tmp1)

pg$party_id[pg$party_id == 982] <- 1160 # "Българска социалистическа партия (БСП)"

# ESTONIA
#dffin$drawn_pg_id[ dffin$partydrawn %in% c("Isamaa (I)")] <- 1597
# 549  846                                              Isamaaliit                    I    Estonia
# 550 1597                      Erakond Isamaa ja Res Publica Liit                  IRL    Estonia
# 551  243                         Rahvuslik Koonderakond "Isamaa"                  RKI    Estonia
# 552  428                                     Erakond Res Publica                  ERP    Estonia
pg$party_id[pg$party_id == 846] <- 1597 
pg$party_id[pg$party_id == 243] <- 1597 
pg$party_id[pg$party_id == 428] <- 1597 

#  ITALY
#dffin$drawn_pg_id[ dffin$partydrawn %in% c("Partito Democratico (PD)")] <- 382
# 8288                                     Democratici di Sinistra               DS      809
pg$party_id[pg$party_id == 809] <- 382 

#dffin$drawn_pg_id[ dffin$partydrawn %in% c("Unione di Centro (UDC)")] <- 226
# 8366                                             Unione / Centro               UC      226
# 8240                                Unione Democratica di Centro             UdCe      397
# 8225                                Centro Cristiano Democratico              CCD       99 # https://de.wikipedia.org/wiki/Centro_Cristiano_Democratico
# 8343 Centro Cristiano Democratico  / Cristiani Democratici Uniti          CCD+CDU      627 
pg$party_id[pg$party_id == 397] <- 226 
pg$party_id[pg$party_id == 99] <- 226 
pg$party_id[pg$party_id == 627] <- 226 


# LATVIA
#dffin$drawn_pg_id[ dffin$partydrawn %in% c("Zaļo un Zemnieku savienība (ZZS: Latvijas Zemnieku savienība (LZS) + Latvijas Zaļā partija (LZP))")] <- 466
# 40  1368                                      Latvijas Zemnieku SavienÄ«ba                  LZS     Latvia
# 118 1001                                      Latvijas ZaÄ¼Ä\u0081 partija                  LZP     Latvia
pg$party_id[pg$party_id == 1368] <- 466
pg$party_id[pg$party_id == 1001] <- 466

#dffin$drawn_pg_id[ dffin$partydrawn %in% c("Jaunā konservatīvā partija (JKP)")] <- 2717
# JaunÄ partija	= 550
#dffin$Q52_pg_id[ dffin$Q52 %in% c("Jaunā konservatīvā partija (JKP)")] <- 550
#dffin$drawn_pg_id[ dffin$partydrawn %in% c("Jaunā konservatīvā partija (JKP)")] <- 550
pg$party_id[pg$party_id == 550] <- 2717

#dffin$drawn_pg_id[ dffin$partydrawn %in% c("Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)")] <- 203
# 39   203                                           TÄ“vzemei un BrÄ«vÄ«bai                   TB     Latvia
# 26  1557             Latvijas NacionÄ\u0081lÄ\u0081s NeatkarÄ«bas KustÄ«ba                 LNNK     Latvia
# 9485 NacionÄ\u0081lÄ\u0081 apvienÄ«ba / TÄ“vzemei un BrÄ«vÄ«bai / LNNK       NA/TB/LNNK      521
pg$party_id[pg$party_id == 1557] <- 203
pg$party_id[pg$party_id == 521] <- 203

#dffin$drawn_pg_id[ dffin$partydrawn %in% c("Vienotība (V)")] <- 1666
# 33  1518                                                     Jaunais Laiks                   JL     Latvia
# 29  1942                                                   Reformu partija                   RP     Latvia # https://de.wikipedia.org/wiki/Reformu_partija
# 27   445                                       PilsoniskÄ\u0081 savienÄ«ba                   PS     Latvia https://en.wikipedia.org/wiki/Civic_Union_(Latvia)
pg$party_id[pg$party_id == 1518] <- 1666
pg$party_id[pg$party_id == 1942] <- 1666
pg$party_id[pg$party_id == 445] <- 1666


# ROMANIA
#dffin$drawn_pg_id[ dffin$partydrawn %in% c("Partidul Național Liberal (PNL)")] <- 1015
# 11196                                Partidul Democrat               PD      419
# 11322                        Partidul Democrat-Liberal             PD-L      958
pg$party_id[pg$party_id == 419] <- 1015
pg$party_id[pg$party_id == 958] <- 1015

#dffin$drawn_pg_id[ dffin$partydrawn %in% c("Alianța Liberalilor și Democraților (ALDE)")] <- 2647
# 11288                             Partidul Conservator               PC        5
# 11243         Partidul Unitatii NaÅ£ionale a Romanilor             PUNR      648 https://en.wikipedia.org/wiki/Conservative_Party_(Romania)
# 11370                      Partidul Liberal Reformator              LRP     2401 https://en.wikipedia.org/wiki/Liberal_Reformist_Party_(Romania)
pg$party_id[pg$party_id == 5] <- 2647
pg$party_id[pg$party_id == 648] <- 2647
pg$party_id[pg$party_id == 2401] <- 2647



# create work pg 
wpg <- pg 

wpg <- wpg[wpg$start_date < as.Date("2019-08-30"),]
wpg <- wpg[wpg$election_date > as.Date("1945-05-08"),]

eu25 <- c("Austria","Belgium","Bulgaria","Croatia","Czech Republic","Denmark","Estonia",
          "Finland","France","Germany","Greece","Hungary",
          "Ireland","Italy" ,"Latvia","Lithuania","Netherlands",
          "Poland","Portugal","Romania",
          "Slovakia","Slovenia","Spain","Sweden","United Kingdom")
wpg <- wpg[wpg$country_name %in% eu25,]

wpg <- wpg[wpg$cabinet_party == 1,]
wpg$cabinet_party <- NULL

cabs <- subset(wpg, select = c(start_date, previous_cabinet_id))
cabs <- cabs[!duplicated(cabs),]
names(cabs)[names(cabs) == "start_date"] <- "next_cabinet_start_date"

wpg <- merge(wpg, cabs, all.x = T, by.x = "cabinet_id", by.y = "previous_cabinet_id")
rm(cabs)

# durations of individual cabinets, e.g. , Merkel IV
wpg$next_cabinet_start_date[is.na(wpg$next_cabinet_start_date)] <- as.Date("2019-08-31")
wpg$cabinet_duration <- as.numeric(wpg$next_cabinet_start_date - wpg$start_date)




wpg <- subset(wpg, select = c( "cabinet_id","country_name","election_date","start_date","cabinet_name","party_name_short","party_name","party_name_english","party_id","next_cabinet_start_date","cabinet_duration"   ))



coal_spells <- list()
for ( c in 1:length(eu25)){
  # get all unique parties in a country
  tmpdf <- wpg[wpg$country_name == eu25[c],]
  parties <- sort(unique(tmpdf$party_id))
  
  # create timeline that gives new dataset, each row represents one day
  tdf <- data.frame(timeline=as.Date("1945-05-08"):as.Date("2019-08-31"))
  
  for ( p in 1:length(parties)){
    
    days <- list()
    twpg <- tmpdf[tmpdf$party_id == parties[p],]
    for ( i in 1:nrow(twpg)){
      days[[i]] <- (twpg$start_date[i]):(twpg$next_cabinet_start_date[i] - 1)
    }
    days <- sort(unlist(days))
    
    tdf[, 1 + p] <- 0
    tdf[tdf$timeline %in% days, 1 + p] <- 1
  }
  
  
  names(tdf)[grep("V", names(tdf))] <- parties
  
  tdf$country <- eu25[c]
  
  tdf$cabinet_id <- NA
  
  for ( r in 1:nrow(tmpdf)){
    # counts cabinets double when there are more than 1 parties involved, but just overrides, should do no harm (other than taking a bit longer)
    tdf$cabinet_id[tdf$timeline %in% (tmpdf$start_date[r]):(tmpdf$next_cabinet_start_date[r] - 1)] <- tmpdf$cabinet_id[r]
  }
  coal_spells[[c]] <- tdf
}




#  loop through parties again; loop through others: how many shared days with... party j
# second loop: for each party and each partner, how much experience together?

countryframe_since2000 <- list()
for ( c in 1:length(eu25)){
  coal_spells[[c]] <- coal_spells[[c]][coal_spells[[c]]$timeline >= as.Date("2000-01-01"),]
  parties <- names(coal_spells[[c]])
  parties <- parties[parties != "timeline"]
  parties <- parties[parties != "country"]
  parties <- parties[parties != "cabinet_id"]
  
  partyframe <- list()
  for (p in 1:length(parties)){
    ego <- parties[p]
    alteri <- parties[parties != parties[p]]
    
    stuff <- list()
    
    coalition_experience_ego <- which(coal_spells[[c]][ , names(coal_spells[[c]]) == ego ] == 1  & 
                                        apply(coal_spells[[c]][ , names(coal_spells[[c]]) %in% alteri ], 1, sum) >= 1
    )
    
    gov_experience_ego <- which(coal_spells[[c]][ , names(coal_spells[[c]]) == ego ] == 1    )
    
    for ( a in 1:length(alteri)){
      #coalition_experience_ego <- which(coal_spells[[c]][ , names(coal_spells[[c]]) == ego ] == 1  )
      
      
      commonspell <- which(coal_spells[[c]][ , names(coal_spells[[c]]) == ego ] == 1 & 
                             coal_spells[[c]][ , names(coal_spells[[c]]) == alteri[a] ] == 1 )
      

      
      
      stuff[[a]] <- data.frame(ego=ego,
                               #name_ego=ego,
                               alter=alteri[a],
                               gov_experience_ego=length(gov_experience_ego),
                               coalition_experience_ego=length(coalition_experience_ego),
                               joint_spell_overall=length(commonspell)
      )
      
      
      
    }
    
    partyframe[[p]] <- do.call(rbind, stuff)
  }
  
  countryframe_since2000[[c]] <- data.frame(country_pg=eu25[c],do.call(rbind, partyframe))
}

countryframe_since2000 <- do.call(rbind ,countryframe_since2000)







dffin$Q52_pg_id <- NA 
dffin$drawn_pg_id <- NA
dffin$Q52_pg_id[ dffin$Q52 %in% c("Österreichische Volkspartei (ÖVP)")] <- 1013
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Österreichische Volkspartei (ÖVP)")] <- 1013
dffin$Q52_pg_id[ dffin$Q52 %in% c("Freiheitliche Partei Österreichs (FPÖ)")] <- 50
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Freiheitliche Partei Österreichs (FPÖ)")] <- 50
dffin$Q52_pg_id[ dffin$Q52 %in% c("Sozialdemokratische Partei Österreich (SPÖ)")] <- 973
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Sozialdemokratische Partei Österreich (SPÖ)")] <- 973


# Bel 
dffin$Q52_pg_id[ dffin$Q52 %in% c("Christen-Democratisch en Vlaams (CD&V)")] <- 723
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Christen-Democratisch en Vlaams (CD&V)")] <- 723
dffin$Q52_pg_id[ dffin$Q52 %in% c("Nieuw-Vlaamse Alliantie (N-VA)")] <- 501
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Nieuw-Vlaamse Alliantie (N-VA)")] <- 501
dffin$Q52_pg_id[ dffin$Q52 %in% c("Socialistische Partij Anders (sp.a)")] <- 1113
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Socialistische Partij Anders (sp.a)")] <- 1113
dffin$Q52_pg_id[ dffin$Q52 %in% c("Parti socialiste (PS)")] <- 1378
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Parti socialiste (PS)")] <- 1378
dffin$Q52_pg_id[ dffin$Q52 %in% c("Écologistes Confédérés pour l'organisation de luttes originales (Ecolo)")] <- 161
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Écologistes Confédérés pour l'organisation de luttes originales (Ecolo)")] <- 161
dffin$Q52_pg_id[ dffin$Q52 %in% c("Mouvement Réformateur (MR)")] <- 915
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Mouvement Réformateur (MR)")] <- 915
dffin$Q52_pg_id[ dffin$Q52 %in% c("Centre démocrate humaniste (cdH)")] <- 1192
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Centre démocrate humaniste (cdH)")] <- 1192
dffin$Q52_pg_id[ dffin$Q52 %in% c("Open Vlaamse Liberalen en Democraten (Open Vld)")] <- 1110
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Open Vlaamse Liberalen en Democraten (Open Vld)")] <- 1110
dffin$Q52_pg_id[ dffin$Q52 %in% c("Groen")] <- 1594
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Groen")] <- 1594
dffin$Q52_pg_id[ dffin$Q52 %in% c("DéFI - Démocrate Fédéraliste Indépendant")] <- 969
dffin$drawn_pg_id[ dffin$partydrawn %in% c("DéFI - Démocrate Fédéraliste Indépendant")] <- 969



dffin$Q52_pg_id[ dffin$Q52 %in% c("Граждани за европейско развитие на България (ГЕРБ)")] <- 1541
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Граждани за европейско развитие на България (ГЕРБ)")] <- 1541
dffin$Q52_pg_id[ dffin$Q52 %in% c("Национален фронт за спасение на България (НФСБ")] <- 2211
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Национален фронт за спасение на България (НФСБ")] <- 2211
dffin$Q52_pg_id[ dffin$Q52 %in% c("Българска социалистическа партия (БСП)")] <- 1160 # 982
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Българска социалистическа партия (БСП)")] <- 1160 # 982
dffin$Q52_pg_id[ dffin$Q52 %in% c("Алтернатива за българско възраждане (АБВ)")] <- 2364
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Алтернатива за българско възраждане (АБВ)")] <- 2364
dffin$Q52_pg_id[ dffin$Q52 %in% c("Движение за права и свободи (ДПС)")] <- 1286
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Движение за права и свободи (ДПС)")] <- 1286



dffin$Q52_pg_id[ dffin$Q52 %in% c("Most nezavisnih lista (MOST)")] <- 2615
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Most nezavisnih lista (MOST)")] <- 2615
dffin$Q52_pg_id[ dffin$Q52 %in% c("Hrvatska seljačka stranka  (HSS)")] <- 1465
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Hrvatska seljačka stranka  (HSS)")] <- 1465
dffin$Q52_pg_id[ dffin$Q52 %in% c("Hrvatska demokratska zajednica (HDZ)")] <- 276
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Hrvatska demokratska zajednica (HDZ)")] <- 276
dffin$Q52_pg_id[ dffin$Q52 %in% c("Istarski demokratski sabor (IDS)")] <- 1627
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Istarski demokratski sabor (IDS)")] <- 1627
dffin$Q52_pg_id[ dffin$Q52 %in% c("Samostalna demokratska srpska stranka (SDSS (Самостална демократска српска странка (СДСС)))")] <- 2133
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Samostalna demokratska srpska stranka (SDSS (Самостална демократска српска странка (СДСС)))")] <- 2133
dffin$Q52_pg_id[ dffin$Q52 %in% c("Hrvatska narodna stranka – Liberalni demokrati (HNS)")] <- 1384
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Hrvatska narodna stranka – Liberalni demokrati (HNS)")] <- 1384
dffin$Q52_pg_id[ dffin$Q52 %in% c("Socijaldemokratska partija Hrvatske  (SDP)")] <- 1493
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Socijaldemokratska partija Hrvatske  (SDP)")] <- 1493


# cze
dffin$Q52_pg_id[ dffin$Q52 %in% c("Tradice Odpovědnost Prosperita 09 (TOP 09)")] <- 2
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Tradice Odpovědnost Prosperita 09 (TOP 09)")] <- 2
dffin$Q52_pg_id[ dffin$Q52 %in% c("ANO 2011 (ANO)")] <- 2263
dffin$drawn_pg_id[ dffin$partydrawn %in% c("ANO 2011 (ANO)")] <- 2263
dffin$Q52_pg_id[ dffin$Q52 %in% c("Křesťanská a demokratická unie - Československá strana lidová (KDU-ČSL)")] <- 2263
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Křesťanská a demokratická unie - Československá strana lidová (KDU-ČSL)", 
                                           "Křesťanská a demokratická unie – Československá strana lidová (KDU-ČSL)" )] <- 2263
dffin$Q52_pg_id[ dffin$Q52 %in% c("Strana zelených (SZ)")] <- 196
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Strana zelených (SZ)" )] <- 196
dffin$Q52_pg_id[ dffin$Q52 %in% c("Česká strana sociálně demokratická (ČSSD)")] <- 789
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Česká strana sociálně demokratická (ČSSD)" )] <- 789
dffin$Q52_pg_id[ dffin$Q52 %in% c("Občanská demokratická strana (ODS)")] <- 829
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Občanská demokratická strana (ODS)" )] <- 829



dffin$Q52_pg_id[ dffin$Q52 %in% c("Venstre (V)")] <- 1605
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Venstre (V)")] <- 1605
dffin$Q52_pg_id[ dffin$Q52 %in% c("Socialdemokratiet (A)")] <- 1629
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Socialdemokratiet (A)")] <- 1629
dffin$Q52_pg_id[ dffin$Q52 %in% c("Socialistisk Folkeparti (F)")] <- 1644
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Socialistisk Folkeparti (F)")] <- 1644
dffin$Q52_pg_id[ dffin$Q52 %in% c("Det Radikale Venstre (B)")] <- 211
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Det Radikale Venstre (B)")] <- 211
dffin$Q52_pg_id[ dffin$Q52 %in% c("Liberal Alliance (I)")] <- 376
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Liberal Alliance (I)")] <- 376
dffin$Q52_pg_id[ dffin$Q52 %in% c("Det Konservative Folkeparti (C)")] <- 590
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Det Konservative Folkeparti (C)")] <- 590
dffin$Q52_pg_id[ dffin$Q52 %in% c("Kristendemokraterne (K)")] <- 1331
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Kristendemokraterne (K)")] <- 1331



dffin$Q52_pg_id[ dffin$Q52 %in% c("Eesti Keskerakond (KE)")] <- 1137
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Eesti Keskerakond (KE)")] <- 1137
dffin$Q52_pg_id[ dffin$Q52 %in% c("Isamaa (I)")] <- 1597
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Isamaa (I)")] <- 1597
dffin$Q52_pg_id[ dffin$Q52 %in% c("Sotsiaaldemokraatlik Erakond (SDE)")] <- 1448
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Sotsiaaldemokraatlik Erakond (SDE)")] <- 1448
dffin$Q52_pg_id[ dffin$Q52 %in% c("Eesti Reformierakond (ER)")] <- 113
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Eesti Reformierakond (ER)")] <- 113
dffin$Q52_pg_id[ dffin$Q52 %in% c("Eesti Konservatiivne Rahvaerakond (EKRE)")] <- 417
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Eesti Konservatiivne Rahvaerakond (EKRE)")] <- 417



dffin$Q52_pg_id[ dffin$Q52 %in% c("Sininen tulevaisuus (SIN)")] <- 2645
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Sininen tulevaisuus (SIN)")] <- 2645
dffin$Q52_pg_id[ dffin$Q52 %in% c("Kansallinen Kokoomus (KOK)")] <- 1118
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Kansallinen Kokoomus (KOK)")] <- 1118
dffin$Q52_pg_id[ dffin$Q52 %in% c("Suomen Sosialidemokraattinen Puolue (SDP)")] <- 395
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Suomen Sosialidemokraattinen Puolue (SDP)")] <- 395
dffin$Q52_pg_id[ dffin$Q52 %in% c("Suomen Keskusta (KESK)")] <- 94
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Suomen Keskusta (KESK)")] <- 94
dffin$Q52_pg_id[ dffin$Q52 %in% c("Perussuomalaiset (PS)")] <- 200
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Perussuomalaiset (PS)")] <- 200
dffin$Q52_pg_id[ dffin$Q52 %in% c("Vasemmistoliitto (VAS)")] <- 1292
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Vasemmistoliitto (VAS)")] <- 1292
dffin$Q52_pg_id[ dffin$Q52 %in% c("Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))")] <- 585
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))")] <- 585
dffin$Q52_pg_id[ dffin$Q52 %in% c("Kristillisdemokraatit (KD)")] <- 1463
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Kristillisdemokraatit (KD)")] <- 1463
dffin$Q52_pg_id[ dffin$Q52 %in% c("Vihreä liitto (VIHR)")] <- 1062
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Vihreä liitto (VIHR)")] <- 1062



dffin$Q52_pg_id[ dffin$Q52 %in% c("La République En Marche (REM)")] <- 2643
dffin$drawn_pg_id[ dffin$partydrawn %in% c("La République En Marche (REM)")] <- 2643
dffin$Q52_pg_id[ dffin$Q52 %in% c("Parti Socialiste (PS)")] <- 1539
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Parti Socialiste (PS)")] <- 1539
dffin$Q52_pg_id[ dffin$Q52 %in% c("Les Républicains (LR)")] <- 658
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Les Républicains (LR)")] <- 658
dffin$Q52_pg_id[ dffin$Q52 %in% c("Parti Communiste Francais (PCF)")] <- 686
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Parti Communiste Francais (PCF)")] <- 686
dffin$Q52_pg_id[ dffin$Q52 %in% c("Europe Écologie-Les Verts (EELV)")] <- 873
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Europe Écologie-Les Verts (EELV)")] <- 873



dffin$Q52_pg_id[ dffin$Q52 %in% c("Christlich Demokratische Union (CDU)","Christlich Soziale Union (CSU)")] <- 1727
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Christlich Demokratische Union (CDU)","Christlich Soziale Union (CSU)")] <- 1727
dffin$Q52_pg_id[ dffin$Q52 %in% c("Sozialdemokratische Partei Deutschlands (SPD)")] <- 558
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Sozialdemokratische Partei Deutschlands (SPD)")] <- 558
dffin$Q52_pg_id[ dffin$Q52 %in% c("Bündnis 90 / Die Grünen (Grüne)")] <- 772
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Bündnis 90 / Die Grünen (Grüne)")] <- 772
dffin$Q52_pg_id[ dffin$Q52 %in% c("Freie Demokratische Partei (FDP)")] <- 543
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Freie Demokratische Partei (FDP)")] <- 543


# gre
dffin$Q52_pg_id[ dffin$Q52 %in% c("Πανελλήνιο Σοσιαλιστικό ίνημα (ΠΑΣΟK)")] <- 1338
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Πανελλήνιο Σοσιαλιστικό ίνημα (ΠΑΣΟK)")] <- 1338
dffin$Q52_pg_id[ dffin$Q52 %in% c("Συνασπισμός Ριζοσπαστικής Αριστεράς (ΣΥ.ΡIΖ.Α)")] <- 1592
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Συνασπισμός Ριζοσπαστικής Αριστεράς (ΣΥ.ΡIΖ.Α)")] <- 1592
dffin$Q52_pg_id[ dffin$Q52 %in% c("Λαϊκός Ορθόδοξος Συναγερμός (ΛΑ.Ο.Σ)")] <- 1179
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Λαϊκός Ορθόδοξος Συναγερμός (ΛΑ.Ο.Σ)")] <- 1179
dffin$Q52_pg_id[ dffin$Q52 %in% c("Nέα Δημοκρατία (NΔ)")] <- 47
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Nέα Δημοκρατία (NΔ)")] <- 47
dffin$Q52_pg_id[ dffin$Q52 %in% c("Ανεξάρτητοι Έλληνες (ΑΝΕΛ)")] <- 2091
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Ανεξάρτητοι Έλληνες (ΑΝΕΛ)")] <- 2091



dffin$Q52_pg_id[ dffin$Q52 %in% c("Fine Gael (FG)")] <- 1393
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Fine Gael (FG)")] <- 1393
dffin$Q52_pg_id[ dffin$Q52 %in% c("Green Party")] <- 1573
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Green Party")] <- 1573
dffin$Q52_pg_id[ dffin$Q52 %in% c("Independent Alliance")] <- 2622
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Independent Alliance")] <- 2622
dffin$Q52_pg_id[ dffin$Q52 %in% c("Fianna Fáil (FF)")] <- 280
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Fianna Fáil (FF)")] <- 280
dffin$Q52_pg_id[ dffin$Q52 %in% c("Labour Party (LP)")] <- 318
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Labour Party (LP)")] <- 318



dffin$Q52_pg_id[ dffin$Q52 %in% c("Forza Italia (FI)")] <- 596
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Forza Italia (FI)")] <- 596
dffin$Q52_pg_id[ dffin$Q52 %in% c("Partito Democratico (PD)")] <- 382
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Partito Democratico (PD)")] <- 382
dffin$Q52_pg_id[ dffin$Q52 %in% c("Italia dei Valori (IdV)")] <- 693
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Italia dei Valori (IdV)")] <- 693
dffin$Q52_pg_id[ dffin$Q52 %in% c("Movimento 5 Stelle (M5S)")] <- 2155
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Movimento 5 Stelle (M5S)")] <- 2155
dffin$Q52_pg_id[ dffin$Q52 %in% c("Lega (Lega Salvini Premier)")] <- 1436
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Lega (Lega Salvini Premier)")] <- 1436
dffin$Q52_pg_id[ dffin$Q52 %in% c("Alternativa Popolare (AP)")] <- 2268
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Alternativa Popolare (AP)")] <- 2268
dffin$Q52_pg_id[ dffin$Q52 %in% c("Unione di Centro (UDC)")] <- 226
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Unione di Centro (UDC)")] <- 226
dffin$Q52_pg_id[ dffin$Q52 %in% c("Sinistra Italiana (SI)")] <- 465 # ???
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Sinistra Italiana (SI)")] <- 465 # ???



dffin$Q52_pg_id[ dffin$Q52 %in% c("Magyar Szocialista Párt (MSZP)")] <- 1591
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Magyar Szocialista Párt (MSZP)")] <- 1591
dffin$Q52_pg_id[ dffin$Q52 %in% c("Fidesz – Magyar Polgári Szövetség (FIDESZ)")] <- 921
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Fidesz – Magyar Polgári Szövetség (FIDESZ)")] <- 921
dffin$Q52_pg_id[ dffin$Q52 %in% c("Kereszténydemokrata Néppárt (KDNP)")] <- 434
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Kereszténydemokrata Néppárt (KDNP)")] <- 434
dffin$Q52_pg_id[ dffin$Q52 %in% c("Jobbik Magyarországért Mozgalom (JOBBIK)")] <- 600
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Jobbik Magyarországért Mozgalom (JOBBIK)")] <- 600



dffin$Q52_pg_id[ dffin$Q52 %in% c("'Kam pieder valsts' (KPV LV)")] <- 2715
dffin$drawn_pg_id[ dffin$partydrawn %in% c("'Kam pieder valsts' (KPV LV)")] <- 2715
dffin$Q52_pg_id[ dffin$Q52 %in% c("Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)")] <- 521
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)")] <- 521
dffin$Q52_pg_id[ dffin$Q52 %in% c("Vienotība (V)")] <- 1666
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Vienotība (V)")] <- 1666
dffin$Q52_pg_id[ dffin$Q52 %in% c("Jaunā konservatīvā partija (JKP)")] <- 2717
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Jaunā konservatīvā partija (JKP)")] <- 2717
dffin$Q52_pg_id[ dffin$Q52 %in% c("Zaļo un Zemnieku savienība (ZZS: Latvijas Zemnieku savienība (LZS) + Latvijas Zaļā partija (LZP))")] <- 466
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Zaļo un Zemnieku savienība (ZZS: Latvijas Zemnieku savienība (LZS) + Latvijas Zaļā partija (LZP))")] <- 466
dffin$Q52_pg_id[ dffin$Q52 %in% c("Latvijas attīstībai (LA)")] <- 2392
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Latvijas attīstībai (LA)")] <- 2392
dffin$Q52_pg_id[ dffin$Q52 %in% c("Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)")] <- 203
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)")] <- 203
dffin$Q52_pg_id[ dffin$Q52 %in% c("Sociāldemokrātiskā partija „Saskaņa“ (SDP)")] <- 1100
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Sociāldemokrātiskā partija „Saskaņa“ (SDP)")] <- 1100


# lit
dffin$Q52_pg_id[ dffin$Q52 %in% c("Darbo partija (DP)")] <- 581
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Darbo partija (DP)")] <- 581
dffin$Q52_pg_id[ dffin$Q52 %in% c("Tvarka ir teisingumas (TT)")] <- 1421
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Tvarka ir teisingumas (TT)")] <- 1421
dffin$Q52_pg_id[ dffin$Q52 %in% c("Liberalų sąjūdis (LRLS)")] <- 482
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Liberalų sąjūdis (LRLS)")] <- 482
dffin$Q52_pg_id[ dffin$Q52 %in% c("Lietuvos lenkų rinkimų akcija – Krikščioniškų šeimų sąjunga (LLRA–KŠS)")] <- 28
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Lietuvos lenkų rinkimų akcija – Krikščioniškų šeimų sąjunga (LLRA–KŠS)")] <- 28
dffin$Q52_pg_id[ dffin$Q52 %in% c("Tėvynės sąjunga - Lietuvos krikščionys demokratai (TS-LKD)")] <- 1045
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Tėvynės sąjunga - Lietuvos krikščionys demokratai (TS-LKD)")] <- 1045
dffin$Q52_pg_id[ dffin$Q52 %in% c("Lietuvos socialdemokratų darbo partija (LSDDP)")] <- 2763
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Lietuvos socialdemokratų darbo partija (LSDDP)")] <- 2763
dffin$Q52_pg_id[ dffin$Q52 %in% c("Lietuvos socialdemokratų partija (LSDP)")] <- 1277
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Lietuvos socialdemokratų partija (LSDP)")] <- 1277
dffin$Q52_pg_id[ dffin$Q52 %in% c("Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)")] <- 191
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)")] <- 191



dffin$Q52_pg_id[ dffin$Q52 %in% c("Christen-Democratisch Appèl (CDA)")] <- 235
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Christen-Democratisch Appèl (CDA)")] <- 235
dffin$Q52_pg_id[ dffin$Q52 %in% c("Partij van de Arbeid (PvdA)")] <- 742
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Partij van de Arbeid (PvdA)")] <- 742
dffin$Q52_pg_id[ dffin$Q52 %in% c("Volkspartij voor Vrijheid en Democratie (VVD)")] <- 1409
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Volkspartij voor Vrijheid en Democratie (VVD)")] <- 1409
dffin$Q52_pg_id[ dffin$Q52 %in% c("Democraten 66 (D66)")] <- 345
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Democraten 66 (D66)")] <- 345
dffin$Q52_pg_id[ dffin$Q52 %in% c("ChristenUnie (CU)")] <- 1206
dffin$drawn_pg_id[ dffin$partydrawn %in% c("ChristenUnie (CU)")] <- 1206
dffin$Q52_pg_id[ dffin$Q52 %in% c("Partij voor de Vrijheid (PVV)")] <- 1501
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Partij voor de Vrijheid (PVV)")] <- 1501



dffin$Q52_pg_id[ dffin$Q52 %in% c("Platforma Obywatelska (PO)")] <- 512
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Platforma Obywatelska (PO)")] <- 512
dffin$Q52_pg_id[ dffin$Q52 %in% c("Prawo i Sprawiedliwość (PiS)")] <- 528
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Prawo i Sprawiedliwość (PiS)")] <- 528
dffin$Q52_pg_id[ dffin$Q52 %in% c("Unia Pracy")] <- 838
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Unia Pracy")] <- 838
dffin$Q52_pg_id[ dffin$Q52 %in% c("Sojusz Lewicy Demokratycznej (SLD)")] <- 629
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Sojusz Lewicy Demokratycznej (SLD)")] <- 629
dffin$Q52_pg_id[ dffin$Q52 %in% c("Wolność")] <- 1104
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Wolność")] <- 1104
dffin$Q52_pg_id[ dffin$Q52 %in% c("Polskie Stronnictwo Ludowe (PSL)")] <- 664
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Polskie Stronnictwo Ludowe (PSL)")] <- 664
dffin$Q52_pg_id[ dffin$Q52 %in% c("Samoobrona")] <- 207
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Samoobrona")] <- 207



dffin$Q52_pg_id[ dffin$Q52 %in% c("Partido Popular Monárquico (PPM)")] <- 107
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Partido Popular Monárquico (PPM)")] <- 107
dffin$Q52_pg_id[ dffin$Q52 %in% c("Partido Social Democrata (PSD)")] <- 1273
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Partido Social Democrata (PSD)")] <- 1273
dffin$Q52_pg_id[ dffin$Q52 %in% c("Centro Democrático e Social – Partido Popular (CDS-PP)")] <- 251
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Centro Democrático e Social – Partido Popular (CDS-PP)")] <- 251
dffin$Q52_pg_id[ dffin$Q52 %in% c("Partido Socialista (PS)")] <- 725
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Partido Socialista (PS)")] <- 725


# rom 
dffin$Q52_pg_id[ dffin$Q52 %in% c("Alianța Liberalilor și Democraților (ALDE)")] <- 2647
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Alianța Liberalilor și Democraților (ALDE)")] <- 2647
dffin$Q52_pg_id[ dffin$Q52 %in% c("Uniunea Democrată Maghiară din România (UDMR - Romániai Magyar Demokrata Szövetség (RMDSZ))")] <- 948
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Uniunea Democrată Maghiară din România (UDMR - Romániai Magyar Demokrata Szövetség (RMDSZ))")] <- 948
dffin$Q52_pg_id[ dffin$Q52 %in% c("Partidul National Taranesc Creştin Democrat (PNȚ-CD)")] <- 888
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Partidul National Taranesc Creştin Democrat (PNȚ-CD)")] <- 888
dffin$Q52_pg_id[ dffin$Q52 %in% c("Partidul Național Liberal (PNL)")] <- 1015
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Partidul Național Liberal (PNL)")] <- 1015
dffin$Q52_pg_id[ dffin$Q52 %in% c("Partidul Social Democrat (PSD)")] <- 1120
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Partidul Social Democrat (PSD)")] <- 1120
dffin$Q52_pg_id[ dffin$Q52 %in% c("Partidul Ecologist Român (PER)")] <- 1639
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Partidul Ecologist Român (PER)")] <- 1639
dffin$Q52_pg_id[ dffin$Q52 %in% c("Partidul Libertății Unității și Solidarității (PLUS)")] <- 2812
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Partidul Libertății Unității și Solidarității (PLUS)")] <- 2812
dffin$Q52_pg_id[ dffin$Q52 %in% c("Uniunea Salvați România (USR)")] <- 2812
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Uniunea Salvați România (USR)")] <- 2812


# sk
dffin$Q52_pg_id[ dffin$Q52 %in% c("Most–Híd")] <- 1620
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Most–Híd")] <- 1620
dffin$Q52_pg_id[ dffin$Q52 %in% c("Smer – sociálna demokracia (Smer-SD)")] <- 220
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Smer – sociálna demokracia (Smer-SD)")] <- 220
dffin$Q52_pg_id[ dffin$Q52 %in% c("Kresťanskodemokratické hnutie (KDH)")] <- 1432
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Kresťanskodemokratické hnutie (KDH)")] <- 1432
dffin$Q52_pg_id[ dffin$Q52 %in% c("Slovenská národná strana (SNS)")] <- 1072
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Slovenská národná strana (SNS)")] <- 1072
dffin$Q52_pg_id[ dffin$Q52 %in% c("Slovenská demokratická a kresťanská únia – Demokratická strana (SDKÚ–DS)")] <- 131
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Slovenská demokratická a kresťanská únia – Demokratická strana (SDKÚ–DS)")] <- 131
dffin$Q52_pg_id[ dffin$Q52 %in% c("Sloboda a Solidarita (SaS)")] <- 1460
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Sloboda a Solidarita (SaS)")] <- 1460


# sl
dffin$Q52_pg_id[ dffin$Q52 %in% c("Demokratična stranka upokojencev Slovenije (DeSUS)")] <- 1587
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Demokratična stranka upokojencev Slovenije (DeSUS)")] <- 1587
dffin$Q52_pg_id[ dffin$Q52 %in% c("Slovenska demokratska stranka (SDS)")] <- 179
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Slovenska demokratska stranka (SDS)")] <- 179
dffin$Q52_pg_id[ dffin$Q52 %in% c("Lista Marjana Šarca (LMŠ)")] <- 2668
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Lista Marjana Šarca (LMŠ)")] <- 2668
dffin$Q52_pg_id[ dffin$Q52 %in% c("Stranka modernega centra (SMC)")] <- 2333
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Stranka modernega centra (SMC)")] <- 2333
dffin$Q52_pg_id[ dffin$Q52 %in% c("Nova Slovenija - Krščanski demokrati (N.Si)")] <- 1047
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Nova Slovenija - Krščanski demokrati (N.Si)")] <- 1047
dffin$Q52_pg_id[ dffin$Q52 %in% c("Slovenska ljudska stranka (SLS)")] <- 16
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Slovenska ljudska stranka (SLS)")] <- 16
dffin$Q52_pg_id[ dffin$Q52 %in% c("Pozitivna Slovenija (PS)")] <- 1987
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Pozitivna Slovenija (PS)")] <- 1987



dffin$Q52_pg_id[ dffin$Q52 %in% c("Miljöpartiet de Gröna (MP)")] <- 1154
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Miljöpartiet de Gröna (MP)")] <- 1154
dffin$Q52_pg_id[ dffin$Q52 %in% c("Centerpartiet (C)")] <- 1461
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Centerpartiet (C)")] <- 1461
dffin$Q52_pg_id[ dffin$Q52 %in% c("Kristdemokraterna (KD)")] <- 282
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Kristdemokraterna (KD)")] <- 282
dffin$Q52_pg_id[ dffin$Q52 %in% c("Moderata samlingspartiet (M)")] <- 657
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Moderata samlingspartiet (M)")] <- 657
dffin$Q52_pg_id[ dffin$Q52 %in% c("Liberalerna (L)")] <- 892
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Liberalerna (L)")] <- 892
dffin$Q52_pg_id[ dffin$Q52 %in% c("Socialdemokratiska arbetarpartiet (SAP)")] <- 904
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Socialdemokratiska arbetarpartiet (SAP)")] <- 904



dffin$Q52_pg_id[ dffin$Q52 %in% c("Partido Popular (PP)")] <- 645
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Partido Popular (PP)")] <- 645
dffin$Q52_pg_id[ dffin$Q52 %in% c("Partido Socialista Obrero Español (PSOE)")] <- 902
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Partido Socialista Obrero Español (PSOE)")] <- 902



dffin$Q52_pg_id[ dffin$Q52 %in% c("Conservative and Unionist Party (Conservative Party)")] <- 773
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Conservative and Unionist Party (Conservative Party)")] <- 773
dffin$Q52_pg_id[ dffin$Q52 %in% c("Labour Party (Labour)")] <- 1556
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Labour Party (Labour)")] <- 1556
dffin$Q52_pg_id[ dffin$Q52 %in% c("Liberal Democrats (Lib Dem)")] <- 659
dffin$drawn_pg_id[ dffin$partydrawn %in% c("Liberal Democrats (Lib Dem)")] <- 659





countryframe_since2000$country_pg <- NULL

names(countryframe_since2000)[names(countryframe_since2000) == "gov_experience_ego"] <- "gov_experience_ego_since2000"
names(countryframe_since2000)[names(countryframe_since2000) == "coalition_experience_ego"] <- "coalition_experience_ego_since2000"
names(countryframe_since2000)[names(countryframe_since2000) == "joint_spell_overall"] <- "joint_spell_since2000"

tmp <- subset(countryframe_since2000, select = c(ego,gov_experience_ego_since2000,coalition_experience_ego_since2000))
tmp <- tmp[!duplicated(tmp),]
countryframe_since2000$gov_experience_ego_since2000 <- NULL
countryframe_since2000$coalition_experience_ego_since2000 <- NULL

dffin <- merge(dffin, countryframe_since2000, all.x = T, by.x = c("Q52_pg_id","drawn_pg_id"), by.y = c("ego","alter"))
dffin <- merge(dffin, tmp, all.x = T, by.x = c("Q52_pg_id"), by.y = c("ego"))

# only cabinet parties coded; hence all remaining parties get 0 
# (also 0 experience for control condition)
dffin$joint_spell_since2000[is.na(dffin$joint_spell_since2000)] <- 0

dffin$joint_spell_since2000[dffin$outpartisan2 %in% c("2_partisan_copartisan", "0_partisan_control")] <- 0

dffin$joint_spell_since2000[dffin$trmnt %in% c("3_eunat-eustance", "4_somenat")] <- 0

dffin$coalition_experience_ego_since2000[is.na(dffin$coalition_experience_ego_since2000)] <- 0






# alignment ####


# party support (identification)
dffin$Q52
# drawn party
dffin$partydrawn
# lr self-placement (we observe "" in UK because apparently, we forgot to "lock" the question)
table(dffin$country, dffin$lrpos)
# eu self-placement (we observe "" in UK because apparently, we forgot to "lock" the question)
table(dffin$country, dffin$eupos)
# lr placement parties
table(dffin$country, dffin$lrpos_party8) # hr: 12,14 (dk categories where numbered, too)
dffin$lrpos_party1[dffin$lrpos_party1 %in% c(12,14)] <- NA
dffin$lrpos_party2[dffin$lrpos_party2 %in% c(12,14)] <- NA
dffin$lrpos_party3[dffin$lrpos_party3 %in% c(12,14)] <- NA
dffin$lrpos_party4[dffin$lrpos_party4 %in% c(12,14)] <- NA
dffin$lrpos_party5[dffin$lrpos_party5 %in% c(12,14)] <- NA
dffin$lrpos_party6[dffin$lrpos_party6 %in% c(12,14)] <- NA
dffin$lrpos_party7[dffin$lrpos_party7 %in% c(12,14)] <- NA
dffin$lrpos_party8[dffin$lrpos_party8 %in% c(12,14)] <- NA
# eu placement parties
dffin$eupos_party1[dffin$eupos_party1 %in% c(12:14)] <- NA
dffin$eupos_party2[dffin$eupos_party2 %in% c(12:14)] <- NA
dffin$eupos_party3[dffin$eupos_party3 %in% c(12:14)] <- NA
dffin$eupos_party4[dffin$eupos_party4 %in% c(12:14)] <- NA
dffin$eupos_party5[dffin$eupos_party5 %in% c(12:14)] <- NA
dffin$eupos_party6[dffin$eupos_party6 %in% c(12:14)] <- NA
dffin$eupos_party7[dffin$eupos_party7 %in% c(12:14)] <- NA
dffin$eupos_party8[dffin$eupos_party8 %in% c(12:14)] <- NA



# perceived position shown party
# this code is "mediadet" by the party drawn, but partydrawn in turn conditions on Q52
dffin$lrpos_partydrawn <- NA
dffin$lrpos_partydrawn[dffin$country == "Aus" & dffin$partydrawn == "Österreichische Volkspartei (ÖVP)"] <- dffin$lrpos_party1[dffin$country == "Aus" & dffin$partydrawn == "Österreichische Volkspartei (ÖVP)"]
dffin$lrpos_partydrawn[dffin$country == "Aus" & dffin$partydrawn == "Sozialdemokratische Partei Österreich (SPÖ)"] <- dffin$lrpos_party2[dffin$country == "Aus" & dffin$partydrawn == "Sozialdemokratische Partei Österreich (SPÖ)"]
dffin$lrpos_partydrawn[dffin$country == "Aus" & dffin$partydrawn == "Freiheitliche Partei Österreichs (FPÖ)" ] <- dffin$lrpos_party3[dffin$country == "Aus" & dffin$partydrawn == "Freiheitliche Partei Österreichs (FPÖ)" ]
dffin$lrpos_partydrawn[dffin$country == "Aus" & dffin$partydrawn == "NEOS – Das Neue Österreich und Liberales Forum (NEOS)" ] <- dffin$lrpos_party4[dffin$country == "Aus" & dffin$partydrawn == "NEOS – Das Neue Österreich und Liberales Forum (NEOS)" ]
dffin$lrpos_partydrawn[dffin$country == "Aus" & dffin$partydrawn == "Die Grünen – Die grüne Alternative (GRÜNE)" ] <- dffin$lrpos_party5[dffin$country == "Aus" & dffin$partydrawn == "Die Grünen – Die grüne Alternative (GRÜNE)" ]
dffin$lrpos_partydrawn[dffin$country == "Aus" & dffin$partydrawn == "JETZT - Liste Pilz (JETZT)" ] <- dffin$lrpos_party6[dffin$country == "Aus" & dffin$partydrawn == "JETZT - Liste Pilz (JETZT)" ]
dffin$lrpos_partydrawn[dffin$country == "Aus" & dffin$partydrawn ==  "EU-Austrittspartei (EUAUS)" ] <- dffin$lrpos_party7[dffin$country == "Aus" & dffin$partydrawn ==  "EU-Austrittspartei (EUAUS)" ]
dffin$lrpos_partydrawn[dffin$country == "Aus" & dffin$partydrawn ==  "Kommunistische Partei Österreichs (KPÖ)"] <- dffin$lrpos_party8[dffin$country == "Aus" & dffin$partydrawn ==  "Kommunistische Partei Österreichs (KPÖ)" ]

dffin$lrpos_partydrawn[dffin$country == "Bel" & dffin$partydrawn == "Nieuw-Vlaamse Alliantie (N-VA)"] <- dffin$lrpos_party1[dffin$country == "Bel" & dffin$partydrawn == "Nieuw-Vlaamse Alliantie (N-VA)"]
dffin$lrpos_partydrawn[dffin$country == "Bel" & dffin$partydrawn == "Parti socialiste (PS)"] <- dffin$lrpos_party2[dffin$country == "Bel" & dffin$partydrawn == "Parti socialiste (PS)"]
dffin$lrpos_partydrawn[dffin$country == "Bel" & dffin$partydrawn == "Mouvement Réformateur (MR)" ] <- dffin$lrpos_party3[dffin$country == "Bel" & dffin$partydrawn == "Mouvement Réformateur (MR)" ]
dffin$lrpos_partydrawn[dffin$country == "Bel" & dffin$partydrawn == "Christen-Democratisch en Vlaams (CD&V)"  ] <- dffin$lrpos_party4[dffin$country == "Bel" & dffin$partydrawn == "Christen-Democratisch en Vlaams (CD&V)"  ]
dffin$lrpos_partydrawn[dffin$country == "Bel" & dffin$partydrawn == "Open Vlaamse Liberalen en Democraten (Open Vld)" ] <- dffin$lrpos_party5[dffin$country == "Bel" & dffin$partydrawn == "Open Vlaamse Liberalen en Democraten (Open Vld)" ]
dffin$lrpos_partydrawn[dffin$country == "Bel" & dffin$partydrawn == "Socialistische Partij Anders (sp.a)" ] <- dffin$lrpos_party6[dffin$country == "Bel" & dffin$partydrawn == "Socialistische Partij Anders (sp.a)" ]
dffin$lrpos_partydrawn[dffin$country == "Bel" & dffin$partydrawn ==  "Centre démocrate humaniste (cdH)" ] <- dffin$lrpos_party7[dffin$country == "Bel" & dffin$partydrawn ==  "Centre démocrate humaniste (cdH)" ]
dffin$lrpos_partydrawn[dffin$country == "Bel" & dffin$partydrawn ==  "Vlaams Belang (VB)" ] <- dffin$lrpos_party8[dffin$country == "Bel" & dffin$partydrawn ==   "Vlaams Belang (VB)"  ]

dffin$lrpos_partydrawn[dffin$country == "Bul" & dffin$partydrawn ==  "Граждани за европейско развитие на България (ГЕРБ)"] <- dffin$lrpos_party1[dffin$country == "Bul" & dffin$partydrawn ==  "Граждани за европейско развитие на България (ГЕРБ)"]
dffin$lrpos_partydrawn[dffin$country == "Bul" & dffin$partydrawn == "Българска социалистическа партия (БСП)"] <- dffin$lrpos_party2[dffin$country == "Bul" & dffin$partydrawn == "Българска социалистическа партия (БСП)"]
dffin$lrpos_partydrawn[dffin$country == "Bul" & dffin$partydrawn == "Движение за права и свободи (ДПС)"] <- dffin$lrpos_party3[dffin$country == "Bul" & dffin$partydrawn == "Движение за права и свободи (ДПС)" ]
dffin$lrpos_partydrawn[dffin$country == "Bul" & dffin$partydrawn == "ВМРО – Българско Национално Движение" ] <- dffin$lrpos_party4[dffin$country == "Bul" & dffin$partydrawn == "ВМРО – Българско Национално Движение"]
dffin$lrpos_partydrawn[dffin$country == "Bul" & dffin$partydrawn == "Атака"] <- dffin$lrpos_party5[dffin$country == "Bul" & dffin$partydrawn == "Атака" ]
dffin$lrpos_partydrawn[dffin$country == "Bul" & dffin$partydrawn == "Воля" ] <- dffin$lrpos_party6[dffin$country == "Bul" & dffin$partydrawn == "Воля" ]
dffin$lrpos_partydrawn[dffin$country == "Bul" & dffin$partydrawn ==  "Национален фронт за спасение на България (НФСБ)"  ] <- dffin$lrpos_party7[dffin$country == "Bul" & dffin$partydrawn ==   "Национален фронт за спасение на България (НФСБ)"  ]
dffin$lrpos_partydrawn[dffin$country == "Bul" & dffin$partydrawn == "Алтернатива за българско възраждане (АБВ)"  ] <- dffin$lrpos_party8[dffin$country == "Bul" & dffin$partydrawn ==   "Алтернатива за българско възраждане (АБВ)"   ]

dffin$lrpos_partydrawn[dffin$country == "Cro" & dffin$partydrawn == "Hrvatska demokratska zajednica (HDZ)"] <- dffin$lrpos_party1[dffin$country == "Cro" & dffin$partydrawn == "Hrvatska demokratska zajednica (HDZ)" ]
dffin$lrpos_partydrawn[dffin$country == "Cro" & dffin$partydrawn == "Socijaldemokratska partija Hrvatske  (SDP)"] <- dffin$lrpos_party2[dffin$country == "Cro" & dffin$partydrawn == "Socijaldemokratska partija Hrvatske  (SDP)"]
dffin$lrpos_partydrawn[dffin$country == "Cro" & dffin$partydrawn == "Živi zid" ] <- dffin$lrpos_party3[dffin$country == "Cro" & dffin$partydrawn == "Živi zid" ]
dffin$lrpos_partydrawn[dffin$country == "Cro" & dffin$partydrawn == "Most nezavisnih lista (MOST)"  ] <- dffin$lrpos_party4[dffin$country == "Cro" & dffin$partydrawn == "Most nezavisnih lista (MOST)"  ]
dffin$lrpos_partydrawn[dffin$country == "Cro" & dffin$partydrawn ==  "Hrvatska seljačka stranka  (HSS)"  ] <- dffin$lrpos_party5[dffin$country == "Cro" & dffin$partydrawn ==  "Hrvatska seljačka stranka  (HSS)"  ]
dffin$lrpos_partydrawn[dffin$country == "Cro" & dffin$partydrawn == "Bandić Milan 365 - Stranka rada i solidarnosti (BM365)"] <- dffin$lrpos_party6[dffin$country == "Cro" & dffin$partydrawn == "Bandić Milan 365 - Stranka rada i solidarnosti (BM365)" ]
dffin$lrpos_partydrawn[dffin$country == "Cro" & dffin$partydrawn ==  "Bruna Esih – Zlatko Hasanbegović: Neovisni za Hrvatsku (NHR)" ] <- dffin$lrpos_party7[dffin$country == "Cro" & dffin$partydrawn ==  "Bruna Esih – Zlatko Hasanbegović: Neovisni za Hrvatsku (NHR)" ]
dffin$lrpos_partydrawn[dffin$country == "Cro" & dffin$partydrawn == "Pametno"] <- dffin$lrpos_party8[dffin$country == "Cro" & dffin$partydrawn ==  "Pametno"]

dffin$lrpos_partydrawn[dffin$country == "Cze" & dffin$partydrawn == "ANO 2011 (ANO)"] <- dffin$lrpos_party1[dffin$country == "Cze" & dffin$partydrawn == "ANO 2011 (ANO)"]
dffin$lrpos_partydrawn[dffin$country == "Cze" & dffin$partydrawn == "Občanská demokratická strana (ODS)"] <- dffin$lrpos_party2[dffin$country == "Cze" & dffin$partydrawn == "Občanská demokratická strana (ODS)"]
dffin$lrpos_partydrawn[dffin$country == "Cze" & dffin$partydrawn == "Česká pirátská strana (Piráti)" ] <- dffin$lrpos_party3[dffin$country == "Cze" & dffin$partydrawn == "Česká pirátská strana (Piráti)" ]
dffin$lrpos_partydrawn[dffin$country == "Cze" & dffin$partydrawn == "Svoboda a přímá demokracie (SPD)" ] <- dffin$lrpos_party4[dffin$country == "Cze" & dffin$partydrawn == "Svoboda a přímá demokracie (SPD)"  ]
dffin$lrpos_partydrawn[dffin$country == "Cze" & dffin$partydrawn == "Komunistická strana Čech a Moravy (KSČM)" ] <- dffin$lrpos_party5[dffin$country == "Cze" & dffin$partydrawn == "Komunistická strana Čech a Moravy (KSČM)" ]
dffin$lrpos_partydrawn[dffin$country == "Cze" & dffin$partydrawn == "Česká strana sociálně demokratická (ČSSD)" ] <- dffin$lrpos_party6[dffin$country == "Cze" & dffin$partydrawn == "Česká strana sociálně demokratická (ČSSD)" ]
dffin$lrpos_partydrawn[dffin$country == "Cze" & dffin$partydrawn == "Křesťanská a demokratická unie – Československá strana lidová (KDU-ČSL)"] <- dffin$lrpos_party7[dffin$country == "Cze" & dffin$partydrawn ==  "Křesťanská a demokratická unie – Československá strana lidová (KDU-ČSL)" ]
dffin$lrpos_partydrawn[dffin$country == "Cze" & dffin$partydrawn == "Tradice Odpovědnost Prosperita 09 (TOP 09)"] <- dffin$lrpos_party8[dffin$country == "Cze" & dffin$partydrawn =="Tradice Odpovědnost Prosperita 09 (TOP 09)" ]

dffin$lrpos_partydrawn[dffin$country == "Den" & dffin$partydrawn == "Socialdemokratiet (A)"] <- dffin$lrpos_party1[dffin$country == "Den" & dffin$partydrawn == "Socialdemokratiet (A)"]
dffin$lrpos_partydrawn[dffin$country == "Den" & dffin$partydrawn == "Venstre (V)" ] <- dffin$lrpos_party2[dffin$country == "Den" & dffin$partydrawn == "Venstre (V)"]
dffin$lrpos_partydrawn[dffin$country == "Den" & dffin$partydrawn == "Dansk Folkeparti (O)"] <- dffin$lrpos_party3[dffin$country == "Den" & dffin$partydrawn == "Dansk Folkeparti (O)" ]
dffin$lrpos_partydrawn[dffin$country == "Den" & dffin$partydrawn == "Det Radikale Venstre (B)"] <- dffin$lrpos_party4[dffin$country == "Den" & dffin$partydrawn == "Det Radikale Venstre (B)"]
dffin$lrpos_partydrawn[dffin$country == "Den" & dffin$partydrawn == "Socialistisk Folkeparti (F)"] <- dffin$lrpos_party5[dffin$country == "Den" & dffin$partydrawn == "Socialistisk Folkeparti (F)"]
dffin$lrpos_partydrawn[dffin$country == "Den" & dffin$partydrawn == "Enhedslisten – De rød-grønne (Ø)"] <- dffin$lrpos_party6[dffin$country == "Den" & dffin$partydrawn == "Enhedslisten – De rød-grønne (Ø)"]
dffin$lrpos_partydrawn[dffin$country == "Den" & dffin$partydrawn == "Liberal Alliance (I)"] <- dffin$lrpos_party7[dffin$country == "Den" & dffin$partydrawn == "Liberal Alliance (I)"]
dffin$lrpos_partydrawn[dffin$country == "Den" & dffin$partydrawn == "Alternativet (Å)"] <- dffin$lrpos_party8[dffin$country == "Den" & dffin$partydrawn == "Alternativet (Å)"]

dffin$lrpos_partydrawn[dffin$country == "Est" & dffin$partydrawn == "Eesti Reformierakond (ER)"] <- dffin$lrpos_party1[dffin$country == "Est" & dffin$partydrawn == "Eesti Reformierakond (ER)"]
dffin$lrpos_partydrawn[dffin$country == "Est" & dffin$partydrawn == "Eesti Keskerakond (KE)"] <- dffin$lrpos_party2[dffin$country == "Est" & dffin$partydrawn == "Eesti Keskerakond (KE)"]
dffin$lrpos_partydrawn[dffin$country == "Est" & dffin$partydrawn == "Eesti Konservatiivne Rahvaerakond (EKRE)"] <- dffin$lrpos_party3[dffin$country == "Est" & dffin$partydrawn == "Eesti Konservatiivne Rahvaerakond (EKRE)"]
dffin$lrpos_partydrawn[dffin$country == "Est" & dffin$partydrawn == "Sotsiaaldemokraatlik Erakond (SDE)"] <- dffin$lrpos_party4[dffin$country == "Est" & dffin$partydrawn == "Sotsiaaldemokraatlik Erakond (SDE)"]
dffin$lrpos_partydrawn[dffin$country == "Est" & dffin$partydrawn == "Isamaa (I)"] <- dffin$lrpos_party5[dffin$country == "Est" & dffin$partydrawn == "Isamaa (I)"]
dffin$lrpos_partydrawn[dffin$country == "Est" & dffin$partydrawn == "Eesti 200"] <- dffin$lrpos_party6[dffin$country == "Est" & dffin$partydrawn == "Eesti 200"]
dffin$lrpos_partydrawn[dffin$country == "Est" & dffin$partydrawn == "Erakond Eestimaa Rohelised (EER)"] <- dffin$lrpos_party7[dffin$country == "Est" & dffin$partydrawn == "Erakond Eestimaa Rohelised (EER)"]
dffin$lrpos_partydrawn[dffin$country == "Est" & dffin$partydrawn == "Eesti Vabaerakond (EVA)"] <- dffin$lrpos_party8[dffin$country == "Est" & dffin$partydrawn == "Eesti Vabaerakond (EVA)"]

dffin$lrpos_partydrawn[dffin$country == "Fin" & dffin$partydrawn == "Suomen Sosialidemokraattinen Puolue (SDP)"] <- dffin$lrpos_party1[dffin$country == "Fin" & dffin$partydrawn == "Suomen Sosialidemokraattinen Puolue (SDP)"]
dffin$lrpos_partydrawn[dffin$country == "Fin" & dffin$partydrawn == "Kansallinen Kokoomus (KOK)"] <- dffin$lrpos_party2[dffin$country == "Fin" & dffin$partydrawn == "Kansallinen Kokoomus (KOK)"]
dffin$lrpos_partydrawn[dffin$country == "Fin" & dffin$partydrawn == "Perussuomalaiset (PS)"] <- dffin$lrpos_party3[dffin$country == "Fin" & dffin$partydrawn == "Perussuomalaiset (PS)" ]
dffin$lrpos_partydrawn[dffin$country == "Fin" & dffin$partydrawn == "Suomen Keskusta (KESK)"] <- dffin$lrpos_party4[dffin$country == "Fin" & dffin$partydrawn == "Suomen Keskusta (KESK)"]
dffin$lrpos_partydrawn[dffin$country == "Fin" & dffin$partydrawn == "Vihreä liitto (VIHR)"] <- dffin$lrpos_party5[dffin$country == "Fin" & dffin$partydrawn == "Vihreä liitto (VIHR)"]
dffin$lrpos_partydrawn[dffin$country == "Fin" & dffin$partydrawn == "Vasemmistoliitto (VAS)"] <- dffin$lrpos_party6[dffin$country == "Fin" & dffin$partydrawn == "Vasemmistoliitto (VAS)"]
dffin$lrpos_partydrawn[dffin$country == "Fin" & dffin$partydrawn == "Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))"] <- dffin$lrpos_party7[dffin$country == "Fin" & dffin$partydrawn == "Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))"]
dffin$lrpos_partydrawn[dffin$country == "Fin" & dffin$partydrawn == "Kristillisdemokraatit (KD)" ] <- dffin$lrpos_party8[dffin$country == "Fin" & dffin$partydrawn == "Kristillisdemokraatit (KD)"]

dffin$lrpos_partydrawn[dffin$country == "Fra" & dffin$partydrawn == "La République En Marche (REM)"] <- dffin$lrpos_party1[dffin$country == "Fra" & dffin$partydrawn == "La République En Marche (REM)"]
dffin$lrpos_partydrawn[dffin$country == "Fra" & dffin$partydrawn == "Les Républicains (LR)" ] <- dffin$lrpos_party2[dffin$country == "Fra" & dffin$partydrawn == "Les Républicains (LR)" ]
dffin$lrpos_partydrawn[dffin$country == "Fra" & dffin$partydrawn == "Rassemblement national (RN (ex. Front National (FN)))" ] <- dffin$lrpos_party3[dffin$country == "Fra" & dffin$partydrawn == "Rassemblement national (RN (ex. Front National (FN)))" ]
dffin$lrpos_partydrawn[dffin$country == "Fra" & dffin$partydrawn == "La France Insoumise (FI)"] <- dffin$lrpos_party4[dffin$country == "Fra" & dffin$partydrawn == "La France Insoumise (FI)"]
dffin$lrpos_partydrawn[dffin$country == "Fra" & dffin$partydrawn == "Parti Socialiste (PS)"] <- dffin$lrpos_party5[dffin$country == "Fra" & dffin$partydrawn == "Parti Socialiste (PS)"]
dffin$lrpos_partydrawn[dffin$country == "Fra" & dffin$partydrawn == "Europe Écologie-Les Verts (EELV)"] <- dffin$lrpos_party6[dffin$country == "Fra" & dffin$partydrawn == "Europe Écologie-Les Verts (EELV)"]
dffin$lrpos_partydrawn[dffin$country == "Fra" & dffin$partydrawn == "Debout la France (DLF)"] <- dffin$lrpos_party7[dffin$country == "Fra" & dffin$partydrawn =="Debout la France (DLF)"]
dffin$lrpos_partydrawn[dffin$country == "Fra" & dffin$partydrawn == "Génération.s"] <- dffin$lrpos_party8[dffin$country == "Fra" & dffin$partydrawn == "Génération.s"]

dffin$lrpos_partydrawn[dffin$country == "Ger" & dffin$partydrawn == "Christlich Demokratische Union (CDU)"] <- dffin$lrpos_party1[dffin$country == "Ger" & dffin$partydrawn == "Christlich Demokratische Union (CDU)"]
dffin$lrpos_partydrawn[dffin$country == "Ger" & dffin$partydrawn == "Christlich Soziale Union (CSU)"] <- dffin$lrpos_party2[dffin$country == "Ger" & dffin$partydrawn == "Christlich Soziale Union (CSU)"]
dffin$lrpos_partydrawn[dffin$country == "Ger" & dffin$partydrawn == "Sozialdemokratische Partei Deutschlands (SPD)"] <- dffin$lrpos_party3[dffin$country == "Ger" & dffin$partydrawn ==  "Sozialdemokratische Partei Deutschlands (SPD)"]
dffin$lrpos_partydrawn[dffin$country == "Ger" & dffin$partydrawn == "Alternative für Deutschland (AfD)" ] <- dffin$lrpos_party4[dffin$country == "Ger" & dffin$partydrawn == "Alternative für Deutschland (AfD)" ]
dffin$lrpos_partydrawn[dffin$country == "Ger" & dffin$partydrawn == "Freie Demokratische Partei (FDP)"] <- dffin$lrpos_party5[dffin$country == "Ger" & dffin$partydrawn == "Freie Demokratische Partei (FDP)"]
dffin$lrpos_partydrawn[dffin$country == "Ger" & dffin$partydrawn == "Die Linke (Linke)"] <- dffin$lrpos_party6[dffin$country == "Ger" & dffin$partydrawn == "Die Linke (Linke)"]
dffin$lrpos_partydrawn[dffin$country == "Ger" & dffin$partydrawn == "Bündnis 90 / Die Grünen (Grüne)"] <- dffin$lrpos_party7[dffin$country == "Ger" & dffin$partydrawn == "Bündnis 90 / Die Grünen (Grüne)"]
dffin$lrpos_partydrawn[dffin$country == "Ger" & dffin$partydrawn == "Freie Wähler (FW)"] <- dffin$lrpos_party8[dffin$country == "Ger" & dffin$partydrawn == "Freie Wähler (FW)"]

dffin$lrpos_partydrawn[dffin$country == "Gre" & dffin$partydrawn == "Nέα Δημοκρατία (NΔ)"] <- dffin$lrpos_party1[dffin$country == "Gre" & dffin$partydrawn == "Nέα Δημοκρατία (NΔ)"]
dffin$lrpos_partydrawn[dffin$country == "Gre" & dffin$partydrawn == "Συνασπισμός Ριζοσπαστικής Αριστεράς (ΣΥ.ΡIΖ.Α)"] <- dffin$lrpos_party2[dffin$country == "Gre" & dffin$partydrawn == "Συνασπισμός Ριζοσπαστικής Αριστεράς (ΣΥ.ΡIΖ.Α)"]
dffin$lrpos_partydrawn[dffin$country == "Gre" & dffin$partydrawn == "Πανελλήνιο Σοσιαλιστικό ίνημα (ΠΑΣΟK)"] <- dffin$lrpos_party3[dffin$country == "Gre" & dffin$partydrawn == "Πανελλήνιο Σοσιαλιστικό ίνημα (ΠΑΣΟK)"]
dffin$lrpos_partydrawn[dffin$country == "Gre" & dffin$partydrawn == "Κίνημα Δημοκρατών Σοσιαλιστών (ΚΙΔΗΣΟ)"] <- dffin$lrpos_party4[dffin$country == "Gre" & dffin$partydrawn == "Κίνημα Δημοκρατών Σοσιαλιστών (ΚΙΔΗΣΟ)"]
dffin$lrpos_partydrawn[dffin$country == "Gre" & dffin$partydrawn == "Λαϊκός Σύνδεσμος -Χρυσή Αυγή (ΛΣ-ΧΑ)"] <- dffin$lrpos_party5[dffin$country == "Gre" & dffin$partydrawn == "Λαϊκός Σύνδεσμος -Χρυσή Αυγή (ΛΣ-ΧΑ)"]
dffin$lrpos_partydrawn[dffin$country == "Gre" & dffin$partydrawn == "Kομμουνιστικό Kόμμα Ελλάδας (KKΕ)"] <- dffin$lrpos_party6[dffin$country == "Gre" & dffin$partydrawn == "Kομμουνιστικό Kόμμα Ελλάδας (KKΕ)"]
dffin$lrpos_partydrawn[dffin$country == "Gre" & dffin$partydrawn == "Ένωση Κεντρώων (EK)"] <- dffin$lrpos_party7[dffin$country == "Gre" & dffin$partydrawn == "Ένωση Κεντρώων (EK)"]
dffin$lrpos_partydrawn[dffin$country == "Gre" & dffin$partydrawn == "Ανεξάρτητοι Έλληνες (ΑΝΕΛ)" ] <- dffin$lrpos_party8[dffin$country == "Gre" & dffin$partydrawn == "Ανεξάρτητοι Έλληνες (ΑΝΕΛ)"]

dffin$lrpos_partydrawn[dffin$country == "Hun" & dffin$partydrawn == "Fidesz – Magyar Polgári Szövetség (FIDESZ)"] <- dffin$lrpos_party1[dffin$country == "Hun" & dffin$partydrawn == "Fidesz – Magyar Polgári Szövetség (FIDESZ)"]
dffin$lrpos_partydrawn[dffin$country == "Hun" & dffin$partydrawn == "Kereszténydemokrata Néppárt (KDNP)"] <- dffin$lrpos_party2[dffin$country == "Hun" & dffin$partydrawn == "Kereszténydemokrata Néppárt (KDNP)"]
dffin$lrpos_partydrawn[dffin$country == "Hun" & dffin$partydrawn == "Jobbik Magyarországért Mozgalom (JOBBIK)"] <- dffin$lrpos_party3[dffin$country == "Hun" & dffin$partydrawn == "Jobbik Magyarországért Mozgalom (JOBBIK)"]
dffin$lrpos_partydrawn[dffin$country == "Hun" & dffin$partydrawn == "Magyar Szocialista Párt (MSZP)" ] <- dffin$lrpos_party4[dffin$country == "Hun" & dffin$partydrawn == "Magyar Szocialista Párt (MSZP)"   ]
dffin$lrpos_partydrawn[dffin$country == "Hun" & dffin$partydrawn == "Párbeszéd Magyarországért (Párbeszéd)"] <- dffin$lrpos_party5[dffin$country == "Hun" & dffin$partydrawn == "Párbeszéd Magyarországért (Párbeszéd)"]
dffin$lrpos_partydrawn[dffin$country == "Hun" & dffin$partydrawn == "Demokratikus Koalíció (DK)"] <- dffin$lrpos_party6[dffin$country == "Hun" & dffin$partydrawn == "Demokratikus Koalíció (DK)"]
dffin$lrpos_partydrawn[dffin$country == "Hun" & dffin$partydrawn == "Momentum Mozgalom (Momentum)"] <- dffin$lrpos_party7[dffin$country == "Hun" & dffin$partydrawn =="Momentum Mozgalom (Momentum)"]
dffin$lrpos_partydrawn[dffin$country == "Hun" & dffin$partydrawn == "Lehet Más a Politika (LMP)"] <- dffin$lrpos_party8[dffin$country == "Hun" & dffin$partydrawn == "Lehet Más a Politika (LMP)"]

dffin$lrpos_partydrawn[dffin$country == "Ire" & dffin$partydrawn == "Fine Gael (FG)"] <- dffin$lrpos_party1[dffin$country == "Ire" & dffin$partydrawn == "Fine Gael (FG)"]
dffin$lrpos_partydrawn[dffin$country == "Ire" & dffin$partydrawn == "Fianna Fáil (FF)"] <- dffin$lrpos_party2[dffin$country == "Ire" & dffin$partydrawn == "Fianna Fáil (FF)"]
dffin$lrpos_partydrawn[dffin$country == "Ire" & dffin$partydrawn == "Sinn Féin (SF)"] <- dffin$lrpos_party3[dffin$country == "Ire" & dffin$partydrawn == "Sinn Féin (SF)" ]
dffin$lrpos_partydrawn[dffin$country == "Ire" & dffin$partydrawn == "Labour Party (LP)" ] <- dffin$lrpos_party4[dffin$country == "Ire" & dffin$partydrawn == "Labour Party (LP)"  ]
dffin$lrpos_partydrawn[dffin$country == "Ire" & dffin$partydrawn == "Independent Alliance"] <- dffin$lrpos_party5[dffin$country == "Ire" & dffin$partydrawn == "Independent Alliance"]
dffin$lrpos_partydrawn[dffin$country == "Ire" & dffin$partydrawn == "Solidarity - People Before Profit (Solidarity-PBP)"] <- dffin$lrpos_party6[dffin$country == "Ire" & dffin$partydrawn == "Solidarity - People Before Profit (Solidarity-PBP)"]
dffin$lrpos_partydrawn[dffin$country == "Ire" & dffin$partydrawn == "Social Democrats (Daonlathaigh Shóisialta)" ] <- dffin$lrpos_party7[dffin$country == "Ire" & dffin$partydrawn == "Social Democrats (Daonlathaigh Shóisialta)"]
dffin$lrpos_partydrawn[dffin$country == "Ire" & dffin$partydrawn == "Green Party"] <- dffin$lrpos_party8[dffin$country == "Ire" & dffin$partydrawn == "Green Party"]

dffin$lrpos_partydrawn[dffin$country == "Ita" & dffin$partydrawn == "Lega (Lega Salvini Premier)"] <- dffin$lrpos_party1[dffin$country == "Ita" & dffin$partydrawn == "Lega (Lega Salvini Premier)"]
dffin$lrpos_partydrawn[dffin$country == "Ita" & dffin$partydrawn == "Movimento 5 Stelle (M5S)"] <- dffin$lrpos_party2[dffin$country == "Ita" & dffin$partydrawn == "Movimento 5 Stelle (M5S)"]
dffin$lrpos_partydrawn[dffin$country == "Ita" & dffin$partydrawn == "Partito Democratico (PD)"] <- dffin$lrpos_party3[dffin$country == "Ita" & dffin$partydrawn == "Partito Democratico (PD)"]
dffin$lrpos_partydrawn[dffin$country == "Ita" & dffin$partydrawn == "Forza Italia (FI)"] <- dffin$lrpos_party4[dffin$country == "Ita" & dffin$partydrawn == "Forza Italia (FI)"]
dffin$lrpos_partydrawn[dffin$country == "Ita" & dffin$partydrawn == "Fratelli d'Italia (FDI)"] <- dffin$lrpos_party5[dffin$country == "Ita" & dffin$partydrawn == "Fratelli d'Italia (FDI)"]
dffin$lrpos_partydrawn[dffin$country == "Ita" & dffin$partydrawn ==  "Più Europa (+E)"  ] <- dffin$lrpos_party6[dffin$country == "Ita" & dffin$partydrawn ==  "Più Europa (+E)"  ]
dffin$lrpos_partydrawn[dffin$country == "Ita" & dffin$partydrawn == "Sinistra Italiana (SI)"] <- dffin$lrpos_party7[dffin$country == "Ita" & dffin$partydrawn == "Sinistra Italiana (SI)" ]
dffin$lrpos_partydrawn[dffin$country == "Ita" & dffin$partydrawn == "Possibile (P)"] <- dffin$lrpos_party8[dffin$country == "Ita" & dffin$partydrawn == "Possibile (P)"]

dffin$lrpos_partydrawn[dffin$country == "Lat" & dffin$partydrawn == "Sociāldemokrātiskā partija „Saskaņa“ (SDP)"] <- dffin$lrpos_party1[dffin$country == "Lat" & dffin$partydrawn == "Sociāldemokrātiskā partija „Saskaņa“ (SDP)"]
dffin$lrpos_partydrawn[dffin$country == "Lat" & dffin$partydrawn == "Jaunā konservatīvā partija (JKP)"] <- dffin$lrpos_party2[dffin$country == "Lat" & dffin$partydrawn == "Jaunā konservatīvā partija (JKP)"]
dffin$lrpos_partydrawn[dffin$country == "Lat" & dffin$partydrawn == "Zaļo un Zemnieku savienība (ZZS: Latvijas Zemnieku savienība (LZS) + Latvijas Zaļā partija (LZP))" ] <- dffin$lrpos_party3[dffin$country == "Lat" & dffin$partydrawn == "Zaļo un Zemnieku savienība (ZZS: Latvijas Zemnieku savienība (LZS) + Latvijas Zaļā partija (LZP))" ]
dffin$lrpos_partydrawn[dffin$country == "Lat" & dffin$partydrawn == "Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)"] <- dffin$lrpos_party4[dffin$country == "Lat" & dffin$partydrawn == "Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)"]
dffin$lrpos_partydrawn[dffin$country == "Lat" & dffin$partydrawn == "'Kam pieder valsts' (KPV LV)"] <- dffin$lrpos_party5[dffin$country == "Lat" & dffin$partydrawn == "'Kam pieder valsts' (KPV LV)"]
dffin$lrpos_partydrawn[dffin$country == "Lat" & dffin$partydrawn == "Kustība Par! (Par!)"] <- dffin$lrpos_party6[dffin$country == "Lat" & dffin$partydrawn == "Kustība Par! (Par!)" ]
dffin$lrpos_partydrawn[dffin$country == "Lat" & dffin$partydrawn == "Latvijas attīstībai (LA)"] <- dffin$lrpos_party7[dffin$country == "Lat" & dffin$partydrawn ==  "Latvijas attīstībai (LA)" ]
dffin$lrpos_partydrawn[dffin$country == "Lat" & dffin$partydrawn == "Vienotība (V)"] <- dffin$lrpos_party8[dffin$country == "Lat" & dffin$partydrawn == "Vienotība (V)"]

dffin$lrpos_partydrawn[dffin$country == "Lit" & dffin$partydrawn =="Tėvynės sąjunga - Lietuvos krikščionys demokratai (TS-LKD)"] <- dffin$lrpos_party1[dffin$country == "Lit" & dffin$partydrawn == "Tėvynės sąjunga - Lietuvos krikščionys demokratai (TS-LKD)"]
dffin$lrpos_partydrawn[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)"] <- dffin$lrpos_party2[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)" ]
dffin$lrpos_partydrawn[dffin$country == "Lit" & dffin$partydrawn ==  "Lietuvos socialdemokratų partija (LSDP)" ] <- dffin$lrpos_party3[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos socialdemokratų partija (LSDP)" ]
dffin$lrpos_partydrawn[dffin$country == "Lit" & dffin$partydrawn == "Tvarka ir teisingumas (TT)"] <- dffin$lrpos_party4[dffin$country == "Lit" & dffin$partydrawn == "Tvarka ir teisingumas (TT)" ]
dffin$lrpos_partydrawn[dffin$country == "Lit" & dffin$partydrawn == "Darbo partija (DP)" ] <- dffin$lrpos_party5[dffin$country == "Lit" & dffin$partydrawn == "Darbo partija (DP)"]
dffin$lrpos_partydrawn[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos socialdemokratų darbo partija (LSDDP)"] <- dffin$lrpos_party6[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos socialdemokratų darbo partija (LSDDP)"]
dffin$lrpos_partydrawn[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos centro partija (LCP)"] <- dffin$lrpos_party7[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos centro partija (LCP)"]
dffin$lrpos_partydrawn[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos lenkų rinkimų akcija – Krikščioniškų šeimų sąjunga (LLRA–KŠS)"] <- dffin$lrpos_party8[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos lenkų rinkimų akcija – Krikščioniškų šeimų sąjunga (LLRA–KŠS)" ]

dffin$lrpos_partydrawn[dffin$country == "Net" & dffin$partydrawn == "Volkspartij voor Vrijheid en Democratie (VVD)"] <- dffin$lrpos_party1[dffin$country == "Net" & dffin$partydrawn == "Volkspartij voor Vrijheid en Democratie (VVD)"]
dffin$lrpos_partydrawn[dffin$country == "Net" & dffin$partydrawn == "Partij voor de Vrijheid (PVV)"] <- dffin$lrpos_party2[dffin$country == "Net" & dffin$partydrawn == "Partij voor de Vrijheid (PVV)"]
dffin$lrpos_partydrawn[dffin$country == "Net" & dffin$partydrawn ==  "GroenLinks (GL)"] <- dffin$lrpos_party3[dffin$country == "Net" & dffin$partydrawn ==  "GroenLinks (GL)" ]
dffin$lrpos_partydrawn[dffin$country == "Net" & dffin$partydrawn == "Christen-Democratisch Appèl (CDA)" ] <- dffin$lrpos_party4[dffin$country == "Net" & dffin$partydrawn == "Christen-Democratisch Appèl (CDA)" ]
dffin$lrpos_partydrawn[dffin$country == "Net" & dffin$partydrawn == "Democraten 66 (D66)" ] <- dffin$lrpos_party5[dffin$country == "Net" & dffin$partydrawn == "Democraten 66 (D66)" ]
dffin$lrpos_partydrawn[dffin$country == "Net" & dffin$partydrawn == "Socialistische Partij (SP)"] <- dffin$lrpos_party6[dffin$country == "Net" & dffin$partydrawn == "Socialistische Partij (SP)"]
dffin$lrpos_partydrawn[dffin$country == "Net" & dffin$partydrawn == "Partij van de Arbeid (PvdA)"] <- dffin$lrpos_party7[dffin$country == "Net" & dffin$partydrawn == "Partij van de Arbeid (PvdA)" ]
dffin$lrpos_partydrawn[dffin$country == "Net" & dffin$partydrawn ==  "Forum voor Democratie (FvD)"] <- dffin$lrpos_party8[dffin$country == "Net" & dffin$partydrawn ==   "Forum voor Democratie (FvD)" ]

dffin$lrpos_partydrawn[dffin$country == "Pol" & dffin$partydrawn == "Prawo i Sprawiedliwość (PiS)"] <- dffin$lrpos_party1[dffin$country == "Pol" & dffin$partydrawn == "Prawo i Sprawiedliwość (PiS)"]
dffin$lrpos_partydrawn[dffin$country == "Pol" & dffin$partydrawn == "Platforma Obywatelska (PO)"] <- dffin$lrpos_party2[dffin$country == "Pol" & dffin$partydrawn == "Platforma Obywatelska (PO)"]
dffin$lrpos_partydrawn[dffin$country == "Pol" & dffin$partydrawn == "Wiosna" ] <- dffin$lrpos_party3[dffin$country == "Pol" & dffin$partydrawn == "Wiosna" ]
dffin$lrpos_partydrawn[dffin$country == "Pol" & dffin$partydrawn ==  "Kukuiz'15"] <- dffin$lrpos_party4[dffin$country == "Pol" & dffin$partydrawn == "Kukuiz'15" ]
dffin$lrpos_partydrawn[dffin$country == "Pol" & dffin$partydrawn == "Polskie Stronnictwo Ludowe (PSL)"] <- dffin$lrpos_party5[dffin$country == "Pol" & dffin$partydrawn == "Polskie Stronnictwo Ludowe (PSL)"]
dffin$lrpos_partydrawn[dffin$country == "Pol" & dffin$partydrawn == "Sojusz Lewicy Demokratycznej (SLD)"] <- dffin$lrpos_party6[dffin$country == "Pol" & dffin$partydrawn == "Sojusz Lewicy Demokratycznej (SLD)" ]
dffin$lrpos_partydrawn[dffin$country == "Pol" & dffin$partydrawn == ".Nowoczesna (.N)" ] <- dffin$lrpos_party7[dffin$country == "Pol" & dffin$partydrawn == ".Nowoczesna (.N)"]
dffin$lrpos_partydrawn[dffin$country == "Pol" & dffin$partydrawn == "Partia Roberta Biedronia"] <- dffin$lrpos_party8[dffin$country == "Pol" & dffin$partydrawn == "Partia Roberta Biedronia" ]

dffin$lrpos_partydrawn[dffin$country == "Por" & dffin$partydrawn == "Partido Socialista (PS)"] <- dffin$lrpos_party1[dffin$country == "Por" & dffin$partydrawn == "Partido Socialista (PS)"]
dffin$lrpos_partydrawn[dffin$country == "Por" & dffin$partydrawn == "Partido Social Democrata (PSD)"] <- dffin$lrpos_party2[dffin$country == "Por" & dffin$partydrawn == "Partido Social Democrata (PSD)"]
dffin$lrpos_partydrawn[dffin$country == "Por" & dffin$partydrawn == "Centro Democrático e Social – Partido Popular (CDS-PP)"] <- dffin$lrpos_party3[dffin$country == "Por" & dffin$partydrawn == "Centro Democrático e Social – Partido Popular (CDS-PP)"]
dffin$lrpos_partydrawn[dffin$country == "Por" & dffin$partydrawn == "Bloco de Esquerda (BE)"] <- dffin$lrpos_party4[dffin$country == "Por" & dffin$partydrawn == "Bloco de Esquerda (BE)"]
dffin$lrpos_partydrawn[dffin$country == "Por" & dffin$partydrawn == "Partido Comunista Português (PCP)" ] <- dffin$lrpos_party5[dffin$country == "Por" & dffin$partydrawn == "Partido Comunista Português (PCP)" ]
dffin$lrpos_partydrawn[dffin$country == "Por" & dffin$partydrawn == "Partido Ecologista 'os Verdes' (PEV)"] <- dffin$lrpos_party6[dffin$country == "Por" & dffin$partydrawn == "Partido Ecologista 'os Verdes' (PEV)"]
dffin$lrpos_partydrawn[dffin$country == "Por" & dffin$partydrawn == "Partido Democrático Republicano (PDR)"] <- dffin$lrpos_party7[dffin$country == "Por" & dffin$partydrawn ==  "Partido Democrático Republicano (PDR)"]
dffin$lrpos_partydrawn[dffin$country == "Por" & dffin$partydrawn == "Aliança"] <- dffin$lrpos_party8[dffin$country == "Por" & dffin$partydrawn == "Aliança"]

dffin$lrpos_partydrawn[dffin$country == "Rom" & dffin$partydrawn == "Partidul Social Democrat (PSD)"] <- dffin$lrpos_party1[dffin$country == "Rom" & dffin$partydrawn == "Partidul Social Democrat (PSD)"]
dffin$lrpos_partydrawn[dffin$country == "Rom" & dffin$partydrawn == "Partidul Național Liberal (PNL)"] <- dffin$lrpos_party2[dffin$country == "Rom" & dffin$partydrawn == "Partidul Național Liberal (PNL)"]
dffin$lrpos_partydrawn[dffin$country == "Rom" & dffin$partydrawn == "Alianța Liberalilor și Democraților (ALDE)"] <- dffin$lrpos_party3[dffin$country == "Rom" & dffin$partydrawn == "Alianța Liberalilor și Democraților (ALDE)"]
dffin$lrpos_partydrawn[dffin$country == "Rom" & dffin$partydrawn == "Uniunea Salvați România (USR)"] <- dffin$lrpos_party4[dffin$country == "Rom" & dffin$partydrawn == "Uniunea Salvați România (USR)"]
dffin$lrpos_partydrawn[dffin$country == "Rom" & dffin$partydrawn == "PRO România (PRO)"] <- dffin$lrpos_party5[dffin$country == "Rom" & dffin$partydrawn == "PRO România (PRO)"]
dffin$lrpos_partydrawn[dffin$country == "Rom" & dffin$partydrawn == "Uniunea Democrată Maghiară din România (UDMR - Romániai Magyar Demokrata Szövetség (RMDSZ))"] <- dffin$lrpos_party6[dffin$country == "Rom" & dffin$partydrawn == "Uniunea Democrată Maghiară din România (UDMR - Romániai Magyar Demokrata Szövetség (RMDSZ))"]
dffin$lrpos_partydrawn[dffin$country == "Rom" & dffin$partydrawn == "Partidul Libertății Unității și Solidarității (PLUS)"] <- dffin$lrpos_party7[dffin$country == "Rom" & dffin$partydrawn == "Partidul Libertății Unității și Solidarității (PLUS)"]
dffin$lrpos_partydrawn[dffin$country == "Rom" & dffin$partydrawn == "Partidul Mișcarea Populară (PMP)"] <- dffin$lrpos_party8[dffin$country == "Rom" & dffin$partydrawn == "Partidul Mișcarea Populară (PMP)"]

dffin$lrpos_partydrawn[dffin$country == "Slovakia" & dffin$partydrawn == "Smer – sociálna demokracia (Smer-SD)"] <- dffin$lrpos_party1[dffin$country == "Slovakia" & dffin$partydrawn == "Smer – sociálna demokracia (Smer-SD)"]
dffin$lrpos_partydrawn[dffin$country == "Slovakia" & dffin$partydrawn == "Sloboda a Solidarita (SaS)"] <- dffin$lrpos_party2[dffin$country == "Slovakia" & dffin$partydrawn == "Sloboda a Solidarita (SaS)"]
dffin$lrpos_partydrawn[dffin$country == "Slovakia" & dffin$partydrawn == "Obyčajní Ľudia a nezávislé osobnosti (OĽaNO)"] <- dffin$lrpos_party3[dffin$country == "Slovakia" & dffin$partydrawn == "Obyčajní Ľudia a nezávislé osobnosti (OĽaNO)"]
dffin$lrpos_partydrawn[dffin$country == "Slovakia" & dffin$partydrawn == "Kotleba – Ľudová strana Naše Slovensko (ĽSNS)"] <- dffin$lrpos_party4[dffin$country == "Slovakia" & dffin$partydrawn == "Kotleba – Ľudová strana Naše Slovensko (ĽSNS)"]
dffin$lrpos_partydrawn[dffin$country == "Slovakia" & dffin$partydrawn == "Progresívne Slovensko (PS)"] <- dffin$lrpos_party5[dffin$country == "Slovakia" & dffin$partydrawn == "Progresívne Slovensko (PS)" ]
dffin$lrpos_partydrawn[dffin$country == "Slovakia" & dffin$partydrawn == "SME RODINA – Boris Kollár (SME RODINA)"] <- dffin$lrpos_party6[dffin$country == "Slovakia" & dffin$partydrawn == "SME RODINA – Boris Kollár (SME RODINA)"]
dffin$lrpos_partydrawn[dffin$country == "Slovakia" & dffin$partydrawn == "Kresťanskodemokratické hnutie (KDH)"] <- dffin$lrpos_party7[dffin$country == "Slovakia" & dffin$partydrawn == "Kresťanskodemokratické hnutie (KDH)"]
dffin$lrpos_partydrawn[dffin$country == "Slovakia" & dffin$partydrawn == "Slovenská národná strana (SNS)"] <- dffin$lrpos_party8[dffin$country == "Slovakia" & dffin$partydrawn == "Slovenská národná strana (SNS)"]

dffin$lrpos_partydrawn[dffin$country == "Slovenia" & dffin$partydrawn == "Lista Marjana Šarca (LMŠ)"] <- dffin$lrpos_party1[dffin$country == "Slovenia" & dffin$partydrawn == "Lista Marjana Šarca (LMŠ)"]
dffin$lrpos_partydrawn[dffin$country == "Slovenia" & dffin$partydrawn == "Slovenska demokratska stranka (SDS)"] <- dffin$lrpos_party2[dffin$country == "Slovenia" & dffin$partydrawn == "Slovenska demokratska stranka (SDS)"]
dffin$lrpos_partydrawn[dffin$country == "Slovenia" & dffin$partydrawn == "Socialni demokrati (SD)"] <- dffin$lrpos_party3[dffin$country == "Slovenia" & dffin$partydrawn == "Socialni demokrati (SD)"]
dffin$lrpos_partydrawn[dffin$country == "Slovenia" & dffin$partydrawn == "Levica"] <- dffin$lrpos_party4[dffin$country == "Slovenia" & dffin$partydrawn == "Levica"]
dffin$lrpos_partydrawn[dffin$country == "Slovenia" & dffin$partydrawn ==  "Stranka modernega centra (SMC)"] <- dffin$lrpos_party5[dffin$country == "Slovenia" & dffin$partydrawn ==  "Stranka modernega centra (SMC)"]
dffin$lrpos_partydrawn[dffin$country == "Slovenia" & dffin$partydrawn == "Nova Slovenija - Krščanski demokrati (N.Si)"] <- dffin$lrpos_party6[dffin$country == "Slovenia" & dffin$partydrawn == "Nova Slovenija - Krščanski demokrati (N.Si)"]
dffin$lrpos_partydrawn[dffin$country == "Slovenia" & dffin$partydrawn == "Slovenska nacionalna stranka (SNS)"] <- dffin$lrpos_party7[dffin$country == "Slovenia" & dffin$partydrawn ==  "Slovenska nacionalna stranka (SNS)"]
dffin$lrpos_partydrawn[dffin$country == "Slovenia" & dffin$partydrawn == "Stranka Alenke Bratušek"] <- dffin$lrpos_party8[dffin$country == "Slovenia" & dffin$partydrawn ==  "Stranka Alenke Bratušek" ]

dffin$lrpos_partydrawn[dffin$country == "Spa" & dffin$partydrawn == "Partido Socialista Obrero Español (PSOE)"] <- dffin$lrpos_party1[dffin$country == "Spa" & dffin$partydrawn == "Partido Socialista Obrero Español (PSOE)"]
dffin$lrpos_partydrawn[dffin$country == "Spa" & dffin$partydrawn == "Partido Popular (PP)" ] <- dffin$lrpos_party2[dffin$country == "Spa" & dffin$partydrawn == "Partido Popular (PP)" ]
dffin$lrpos_partydrawn[dffin$country == "Spa" & dffin$partydrawn == "Ciudadanos - Partido de la Ciudadanía (C's)"] <- dffin$lrpos_party3[dffin$country == "Spa" & dffin$partydrawn == "Ciudadanos - Partido de la Ciudadanía (C's)" ]
dffin$lrpos_partydrawn[dffin$country == "Spa" & dffin$partydrawn == "Podemos" ] <- dffin$lrpos_party4[dffin$country == "Spa" & dffin$partydrawn == "Podemos"]
dffin$lrpos_partydrawn[dffin$country == "Spa" & dffin$partydrawn == "Vox"] <- dffin$lrpos_party5[dffin$country == "Spa" & dffin$partydrawn == "Vox"]
dffin$lrpos_partydrawn[dffin$country == "Spa" & dffin$partydrawn == "Izquierda Unida (IU)"] <- dffin$lrpos_party6[dffin$country == "Spa" & dffin$partydrawn == "Izquierda Unida (IU)"]
dffin$lrpos_partydrawn[dffin$country == "Spa" & dffin$partydrawn == "Esquerra Republicana de Catalunya (ERC)" ] <- dffin$lrpos_party7[dffin$country == "Spa" & dffin$partydrawn == "Esquerra Republicana de Catalunya (ERC)" ]
dffin$lrpos_partydrawn[dffin$country == "Spa" & dffin$partydrawn == "Partit Demòcrata Europeo Català (PDeCAT)"] <- dffin$lrpos_party8[dffin$country == "Spa" & dffin$partydrawn == "Partit Demòcrata Europeo Català (PDeCAT)"]

dffin$lrpos_partydrawn[dffin$country == "Swe" & dffin$partydrawn == "Socialdemokratiska arbetarpartiet (SAP)"] <- dffin$lrpos_party1[dffin$country == "Swe" & dffin$partydrawn == "Socialdemokratiska arbetarpartiet (SAP)"]
dffin$lrpos_partydrawn[dffin$country == "Swe" & dffin$partydrawn == "Sverigedemokraterna (SD)"] <- dffin$lrpos_party2[dffin$country == "Swe" & dffin$partydrawn == "Sverigedemokraterna (SD)"]
dffin$lrpos_partydrawn[dffin$country == "Swe" & dffin$partydrawn == "Moderata samlingspartiet (M)"] <- dffin$lrpos_party3[dffin$country == "Swe" & dffin$partydrawn == "Moderata samlingspartiet (M)"]
dffin$lrpos_partydrawn[dffin$country == "Swe" & dffin$partydrawn == "Kristdemokraterna (KD)"] <- dffin$lrpos_party4[dffin$country == "Swe" & dffin$partydrawn == "Kristdemokraterna (KD)"]
dffin$lrpos_partydrawn[dffin$country == "Swe" & dffin$partydrawn == "Vänsterpartiet (V)" ] <- dffin$lrpos_party5[dffin$country == "Swe" & dffin$partydrawn == "Vänsterpartiet (V)"]
dffin$lrpos_partydrawn[dffin$country == "Swe" & dffin$partydrawn == "Centerpartiet (C)" ] <- dffin$lrpos_party6[dffin$country == "Swe" & dffin$partydrawn == "Centerpartiet (C)"  ]
dffin$lrpos_partydrawn[dffin$country == "Swe" & dffin$partydrawn == "Miljöpartiet de Gröna (MP)"] <- dffin$lrpos_party7[dffin$country == "Swe" & dffin$partydrawn == "Miljöpartiet de Gröna (MP)"]
dffin$lrpos_partydrawn[dffin$country == "Swe" & dffin$partydrawn == "Liberalerna (L)"] <- dffin$lrpos_party8[dffin$country == "Swe" & dffin$partydrawn == "Liberalerna (L)" ]

dffin$lrpos_partydrawn[dffin$country == "UK" & dffin$partydrawn == "Labour Party (Labour)"] <- dffin$lrpos_party1[dffin$country == "UK" & dffin$partydrawn == "Labour Party (Labour)"]
dffin$lrpos_partydrawn[dffin$country == "UK" & dffin$partydrawn == "Conservative and Unionist Party (Conservative Party)"] <- dffin$lrpos_party2[dffin$country == "UK" & dffin$partydrawn == "Conservative and Unionist Party (Conservative Party)"]
dffin$lrpos_partydrawn[dffin$country == "UK" & dffin$partydrawn == "Brexit Party" ] <- dffin$lrpos_party3[dffin$country == "UK" & dffin$partydrawn == "Brexit Party" ]
dffin$lrpos_partydrawn[dffin$country == "UK" & dffin$partydrawn == "Liberal Democrats (Lib Dem)"  ] <- dffin$lrpos_party4[dffin$country == "UK" & dffin$partydrawn =="Liberal Democrats (Lib Dem)"  ]
dffin$lrpos_partydrawn[dffin$country == "UK" & dffin$partydrawn == "Scotish National Party (SNP)"  ] <- dffin$lrpos_party5[dffin$country == "UK" & dffin$partydrawn == "Scotish National Party (SNP)"  ]
dffin$lrpos_partydrawn[dffin$country == "UK" & dffin$partydrawn ==  "Green Party of England and Wales (GP)" ] <- dffin$lrpos_party6[dffin$country == "UK" & dffin$partydrawn ==  "Green Party of England and Wales (GP)"]
dffin$lrpos_partydrawn[dffin$country == "UK" & dffin$partydrawn ==  "Change UK – The Independent Group" ] <- dffin$lrpos_party7[dffin$country == "UK" & dffin$partydrawn == "Change UK – The Independent Group" ]
dffin$lrpos_partydrawn[dffin$country == "UK" & dffin$partydrawn == "UK Independence Party (UKIP)" ] <- dffin$lrpos_party8[dffin$country == "UK" & dffin$partydrawn == "UK Independence Party (UKIP)"]

partisplacd <- unique(dffin$partydrawn[!is.na(dffin$lrpos_partydrawn)]) 

supportrsofplacablpartis <- dffin$country_id[dffin$Q52 %in% partisplacd]
dffin$supportsplacablparty <- 0
dffin$supportsplacablparty[dffin$country_id %in% supportrsofplacablpartis] <- 1





dffin$eupos_partydrawn <- NA
dffin$eupos_partydrawn[dffin$country == "Aus" & dffin$partydrawn == "Österreichische Volkspartei (ÖVP)"] <- dffin$eupos_party1[dffin$country == "Aus" & dffin$partydrawn == "Österreichische Volkspartei (ÖVP)"]
dffin$eupos_partydrawn[dffin$country == "Aus" & dffin$partydrawn == "Sozialdemokratische Partei Österreich (SPÖ)"] <- dffin$eupos_party2[dffin$country == "Aus" & dffin$partydrawn == "Sozialdemokratische Partei Österreich (SPÖ)"]
dffin$eupos_partydrawn[dffin$country == "Aus" & dffin$partydrawn == "Freiheitliche Partei Österreichs (FPÖ)" ] <- dffin$eupos_party3[dffin$country == "Aus" & dffin$partydrawn == "Freiheitliche Partei Österreichs (FPÖ)" ]
dffin$eupos_partydrawn[dffin$country == "Aus" & dffin$partydrawn == "NEOS – Das Neue Österreich und Liberales Forum (NEOS)" ] <- dffin$eupos_party4[dffin$country == "Aus" & dffin$partydrawn == "NEOS – Das Neue Österreich und Liberales Forum (NEOS)" ]
dffin$eupos_partydrawn[dffin$country == "Aus" & dffin$partydrawn == "Die Grünen – Die grüne Alternative (GRÜNE)" ] <- dffin$eupos_party5[dffin$country == "Aus" & dffin$partydrawn == "Die Grünen – Die grüne Alternative (GRÜNE)" ]
dffin$eupos_partydrawn[dffin$country == "Aus" & dffin$partydrawn == "JETZT - Liste Pilz (JETZT)" ] <- dffin$eupos_party6[dffin$country == "Aus" & dffin$partydrawn == "JETZT - Liste Pilz (JETZT)" ]
dffin$eupos_partydrawn[dffin$country == "Aus" & dffin$partydrawn ==  "EU-Austrittspartei (EUAUS)" ] <- dffin$eupos_party7[dffin$country == "Aus" & dffin$partydrawn ==  "EU-Austrittspartei (EUAUS)" ]
dffin$eupos_partydrawn[dffin$country == "Aus" & dffin$partydrawn ==  "Kommunistische Partei Österreichs (KPÖ)"] <- dffin$eupos_party8[dffin$country == "Aus" & dffin$partydrawn ==  "Kommunistische Partei Österreichs (KPÖ)" ]

dffin$eupos_partydrawn[dffin$country == "Bel" & dffin$partydrawn == "Nieuw-Vlaamse Alliantie (N-VA)"] <- dffin$eupos_party1[dffin$country == "Bel" & dffin$partydrawn == "Nieuw-Vlaamse Alliantie (N-VA)"]
dffin$eupos_partydrawn[dffin$country == "Bel" & dffin$partydrawn == "Parti socialiste (PS)"] <- dffin$eupos_party2[dffin$country == "Bel" & dffin$partydrawn == "Parti socialiste (PS)"]
dffin$eupos_partydrawn[dffin$country == "Bel" & dffin$partydrawn == "Mouvement Réformateur (MR)" ] <- dffin$eupos_party3[dffin$country == "Bel" & dffin$partydrawn == "Mouvement Réformateur (MR)" ]
dffin$eupos_partydrawn[dffin$country == "Bel" & dffin$partydrawn == "Christen-Democratisch en Vlaams (CD&V)"  ] <- dffin$eupos_party4[dffin$country == "Bel" & dffin$partydrawn == "Christen-Democratisch en Vlaams (CD&V)"  ]
dffin$eupos_partydrawn[dffin$country == "Bel" & dffin$partydrawn == "Open Vlaamse Liberalen en Democraten (Open Vld)" ] <- dffin$eupos_party5[dffin$country == "Bel" & dffin$partydrawn == "Open Vlaamse Liberalen en Democraten (Open Vld)" ]
dffin$eupos_partydrawn[dffin$country == "Bel" & dffin$partydrawn == "Socialistische Partij Anders (sp.a)" ] <- dffin$eupos_party6[dffin$country == "Bel" & dffin$partydrawn == "Socialistische Partij Anders (sp.a)" ]
dffin$eupos_partydrawn[dffin$country == "Bel" & dffin$partydrawn ==  "Centre démocrate humaniste (cdH)" ] <- dffin$eupos_party7[dffin$country == "Bel" & dffin$partydrawn ==  "Centre démocrate humaniste (cdH)" ]
dffin$eupos_partydrawn[dffin$country == "Bel" & dffin$partydrawn ==  "Vlaams Belang (VB)" ] <- dffin$eupos_party8[dffin$country == "Bel" & dffin$partydrawn ==   "Vlaams Belang (VB)"  ]

dffin$eupos_partydrawn[dffin$country == "Bul" & dffin$partydrawn ==  "Граждани за европейско развитие на България (ГЕРБ)"] <- dffin$eupos_party1[dffin$country == "Bul" & dffin$partydrawn ==  "Граждани за европейско развитие на България (ГЕРБ)"]
dffin$eupos_partydrawn[dffin$country == "Bul" & dffin$partydrawn == "Българска социалистическа партия (БСП)"] <- dffin$eupos_party2[dffin$country == "Bul" & dffin$partydrawn == "Българска социалистическа партия (БСП)"]
dffin$eupos_partydrawn[dffin$country == "Bul" & dffin$partydrawn == "Движение за права и свободи (ДПС)"] <- dffin$eupos_party3[dffin$country == "Bul" & dffin$partydrawn == "Движение за права и свободи (ДПС)" ]
dffin$eupos_partydrawn[dffin$country == "Bul" & dffin$partydrawn == "ВМРО – Българско Национално Движение" ] <- dffin$eupos_party4[dffin$country == "Bul" & dffin$partydrawn == "ВМРО – Българско Национално Движение"]
dffin$eupos_partydrawn[dffin$country == "Bul" & dffin$partydrawn == "Атака"] <- dffin$eupos_party5[dffin$country == "Bul" & dffin$partydrawn == "Атака" ]
dffin$eupos_partydrawn[dffin$country == "Bul" & dffin$partydrawn == "Воля" ] <- dffin$eupos_party6[dffin$country == "Bul" & dffin$partydrawn == "Воля" ]
dffin$eupos_partydrawn[dffin$country == "Bul" & dffin$partydrawn ==  "Национален фронт за спасение на България (НФСБ)"  ] <- dffin$eupos_party7[dffin$country == "Bul" & dffin$partydrawn ==   "Национален фронт за спасение на България (НФСБ)"  ]
dffin$eupos_partydrawn[dffin$country == "Bul" & dffin$partydrawn == "Алтернатива за българско възраждане (АБВ)"  ] <- dffin$eupos_party8[dffin$country == "Bul" & dffin$partydrawn ==   "Алтернатива за българско възраждане (АБВ)"   ]

dffin$eupos_partydrawn[dffin$country == "Cro" & dffin$partydrawn == "Hrvatska demokratska zajednica (HDZ)"] <- dffin$eupos_party1[dffin$country == "Cro" & dffin$partydrawn == "Hrvatska demokratska zajednica (HDZ)" ]
dffin$eupos_partydrawn[dffin$country == "Cro" & dffin$partydrawn == "Socijaldemokratska partija Hrvatske  (SDP)"] <- dffin$eupos_party2[dffin$country == "Cro" & dffin$partydrawn == "Socijaldemokratska partija Hrvatske  (SDP)"]
dffin$eupos_partydrawn[dffin$country == "Cro" & dffin$partydrawn == "Živi zid" ] <- dffin$eupos_party3[dffin$country == "Cro" & dffin$partydrawn == "Živi zid" ]
dffin$eupos_partydrawn[dffin$country == "Cro" & dffin$partydrawn == "Most nezavisnih lista (MOST)"  ] <- dffin$eupos_party4[dffin$country == "Cro" & dffin$partydrawn == "Most nezavisnih lista (MOST)"  ]
dffin$eupos_partydrawn[dffin$country == "Cro" & dffin$partydrawn ==  "Hrvatska seljačka stranka  (HSS)"  ] <- dffin$eupos_party5[dffin$country == "Cro" & dffin$partydrawn ==  "Hrvatska seljačka stranka  (HSS)"  ]
dffin$eupos_partydrawn[dffin$country == "Cro" & dffin$partydrawn == "Bandić Milan 365 - Stranka rada i solidarnosti (BM365)"] <- dffin$eupos_party6[dffin$country == "Cro" & dffin$partydrawn == "Bandić Milan 365 - Stranka rada i solidarnosti (BM365)" ]
dffin$eupos_partydrawn[dffin$country == "Cro" & dffin$partydrawn ==  "Bruna Esih – Zlatko Hasanbegović: Neovisni za Hrvatsku (NHR)" ] <- dffin$eupos_party7[dffin$country == "Cro" & dffin$partydrawn ==  "Bruna Esih – Zlatko Hasanbegović: Neovisni za Hrvatsku (NHR)" ]
dffin$eupos_partydrawn[dffin$country == "Cro" & dffin$partydrawn == "Pametno"] <- dffin$eupos_party8[dffin$country == "Cro" & dffin$partydrawn ==  "Pametno"]

dffin$eupos_partydrawn[dffin$country == "Cze" & dffin$partydrawn == "ANO 2011 (ANO)"] <- dffin$eupos_party1[dffin$country == "Cze" & dffin$partydrawn == "ANO 2011 (ANO)"]
dffin$eupos_partydrawn[dffin$country == "Cze" & dffin$partydrawn == "Občanská demokratická strana (ODS)"] <- dffin$eupos_party2[dffin$country == "Cze" & dffin$partydrawn == "Občanská demokratická strana (ODS)"]
dffin$eupos_partydrawn[dffin$country == "Cze" & dffin$partydrawn == "Česká pirátská strana (Piráti)" ] <- dffin$eupos_party3[dffin$country == "Cze" & dffin$partydrawn == "Česká pirátská strana (Piráti)" ]
dffin$eupos_partydrawn[dffin$country == "Cze" & dffin$partydrawn == "Svoboda a přímá demokracie (SPD)" ] <- dffin$eupos_party4[dffin$country == "Cze" & dffin$partydrawn == "Svoboda a přímá demokracie (SPD)"  ]
dffin$eupos_partydrawn[dffin$country == "Cze" & dffin$partydrawn == "Komunistická strana Čech a Moravy (KSČM)" ] <- dffin$eupos_party5[dffin$country == "Cze" & dffin$partydrawn == "Komunistická strana Čech a Moravy (KSČM)" ]
dffin$eupos_partydrawn[dffin$country == "Cze" & dffin$partydrawn == "Česká strana sociálně demokratická (ČSSD)" ] <- dffin$eupos_party6[dffin$country == "Cze" & dffin$partydrawn == "Česká strana sociálně demokratická (ČSSD)" ]
dffin$eupos_partydrawn[dffin$country == "Cze" & dffin$partydrawn == "Křesťanská a demokratická unie – Československá strana lidová (KDU-ČSL)"] <- dffin$eupos_party7[dffin$country == "Cze" & dffin$partydrawn ==  "Křesťanská a demokratická unie – Československá strana lidová (KDU-ČSL)" ]
dffin$eupos_partydrawn[dffin$country == "Cze" & dffin$partydrawn == "Tradice Odpovědnost Prosperita 09 (TOP 09)"] <- dffin$eupos_party8[dffin$country == "Cze" & dffin$partydrawn =="Tradice Odpovědnost Prosperita 09 (TOP 09)" ]

dffin$eupos_partydrawn[dffin$country == "Den" & dffin$partydrawn == "Socialdemokratiet (A)"] <- dffin$eupos_party1[dffin$country == "Den" & dffin$partydrawn == "Socialdemokratiet (A)"]
dffin$eupos_partydrawn[dffin$country == "Den" & dffin$partydrawn == "Venstre (V)" ] <- dffin$eupos_party2[dffin$country == "Den" & dffin$partydrawn == "Venstre (V)"]
dffin$eupos_partydrawn[dffin$country == "Den" & dffin$partydrawn == "Dansk Folkeparti (O)"] <- dffin$eupos_party3[dffin$country == "Den" & dffin$partydrawn == "Dansk Folkeparti (O)" ]
dffin$eupos_partydrawn[dffin$country == "Den" & dffin$partydrawn == "Det Radikale Venstre (B)"] <- dffin$eupos_party4[dffin$country == "Den" & dffin$partydrawn == "Det Radikale Venstre (B)"]
dffin$eupos_partydrawn[dffin$country == "Den" & dffin$partydrawn == "Socialistisk Folkeparti (F)"] <- dffin$eupos_party5[dffin$country == "Den" & dffin$partydrawn == "Socialistisk Folkeparti (F)"]
dffin$eupos_partydrawn[dffin$country == "Den" & dffin$partydrawn == "Enhedslisten – De rød-grønne (Ø)"] <- dffin$eupos_party6[dffin$country == "Den" & dffin$partydrawn == "Enhedslisten – De rød-grønne (Ø)"]
dffin$eupos_partydrawn[dffin$country == "Den" & dffin$partydrawn == "Liberal Alliance (I)"] <- dffin$eupos_party7[dffin$country == "Den" & dffin$partydrawn == "Liberal Alliance (I)"]
dffin$eupos_partydrawn[dffin$country == "Den" & dffin$partydrawn == "Alternativet (Å)"] <- dffin$eupos_party8[dffin$country == "Den" & dffin$partydrawn == "Alternativet (Å)"]

dffin$eupos_partydrawn[dffin$country == "Est" & dffin$partydrawn == "Eesti Reformierakond (ER)"] <- dffin$eupos_party1[dffin$country == "Est" & dffin$partydrawn == "Eesti Reformierakond (ER)"]
dffin$eupos_partydrawn[dffin$country == "Est" & dffin$partydrawn == "Eesti Keskerakond (KE)"] <- dffin$eupos_party2[dffin$country == "Est" & dffin$partydrawn == "Eesti Keskerakond (KE)"]
dffin$eupos_partydrawn[dffin$country == "Est" & dffin$partydrawn == "Eesti Konservatiivne Rahvaerakond (EKRE)"] <- dffin$eupos_party3[dffin$country == "Est" & dffin$partydrawn == "Eesti Konservatiivne Rahvaerakond (EKRE)"]
dffin$eupos_partydrawn[dffin$country == "Est" & dffin$partydrawn == "Sotsiaaldemokraatlik Erakond (SDE)"] <- dffin$eupos_party4[dffin$country == "Est" & dffin$partydrawn == "Sotsiaaldemokraatlik Erakond (SDE)"]
dffin$eupos_partydrawn[dffin$country == "Est" & dffin$partydrawn == "Isamaa (I)"] <- dffin$eupos_party5[dffin$country == "Est" & dffin$partydrawn == "Isamaa (I)"]
dffin$eupos_partydrawn[dffin$country == "Est" & dffin$partydrawn == "Eesti 200"] <- dffin$eupos_party6[dffin$country == "Est" & dffin$partydrawn == "Eesti 200"]
dffin$eupos_partydrawn[dffin$country == "Est" & dffin$partydrawn == "Erakond Eestimaa Rohelised (EER)"] <- dffin$eupos_party7[dffin$country == "Est" & dffin$partydrawn == "Erakond Eestimaa Rohelised (EER)"]
dffin$eupos_partydrawn[dffin$country == "Est" & dffin$partydrawn == "Eesti Vabaerakond (EVA)"] <- dffin$eupos_party8[dffin$country == "Est" & dffin$partydrawn == "Eesti Vabaerakond (EVA)"]

dffin$eupos_partydrawn[dffin$country == "Fin" & dffin$partydrawn == "Suomen Sosialidemokraattinen Puolue (SDP)"] <- dffin$eupos_party1[dffin$country == "Fin" & dffin$partydrawn == "Suomen Sosialidemokraattinen Puolue (SDP)"]
dffin$eupos_partydrawn[dffin$country == "Fin" & dffin$partydrawn == "Kansallinen Kokoomus (KOK)"] <- dffin$eupos_party2[dffin$country == "Fin" & dffin$partydrawn == "Kansallinen Kokoomus (KOK)"]
dffin$eupos_partydrawn[dffin$country == "Fin" & dffin$partydrawn == "Perussuomalaiset (PS)"] <- dffin$eupos_party3[dffin$country == "Fin" & dffin$partydrawn == "Perussuomalaiset (PS)" ]
dffin$eupos_partydrawn[dffin$country == "Fin" & dffin$partydrawn == "Suomen Keskusta (KESK)"] <- dffin$eupos_party4[dffin$country == "Fin" & dffin$partydrawn == "Suomen Keskusta (KESK)"]
dffin$eupos_partydrawn[dffin$country == "Fin" & dffin$partydrawn == "Vihreä liitto (VIHR)"] <- dffin$eupos_party5[dffin$country == "Fin" & dffin$partydrawn == "Vihreä liitto (VIHR)"]
dffin$eupos_partydrawn[dffin$country == "Fin" & dffin$partydrawn == "Vasemmistoliitto (VAS)"] <- dffin$eupos_party6[dffin$country == "Fin" & dffin$partydrawn == "Vasemmistoliitto (VAS)"]
dffin$eupos_partydrawn[dffin$country == "Fin" & dffin$partydrawn == "Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))"] <- dffin$eupos_party7[dffin$country == "Fin" & dffin$partydrawn == "Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))"]
dffin$eupos_partydrawn[dffin$country == "Fin" & dffin$partydrawn == "Kristillisdemokraatit (KD)" ] <- dffin$eupos_party8[dffin$country == "Fin" & dffin$partydrawn == "Kristillisdemokraatit (KD)"]

dffin$eupos_partydrawn[dffin$country == "Fra" & dffin$partydrawn == "La République En Marche (REM)"] <- dffin$eupos_party1[dffin$country == "Fra" & dffin$partydrawn == "La République En Marche (REM)"]
dffin$eupos_partydrawn[dffin$country == "Fra" & dffin$partydrawn == "Les Républicains (LR)" ] <- dffin$eupos_party2[dffin$country == "Fra" & dffin$partydrawn == "Les Républicains (LR)" ]
dffin$eupos_partydrawn[dffin$country == "Fra" & dffin$partydrawn == "Rassemblement national (RN (ex. Front National (FN)))" ] <- dffin$eupos_party3[dffin$country == "Fra" & dffin$partydrawn == "Rassemblement national (RN (ex. Front National (FN)))" ]
dffin$eupos_partydrawn[dffin$country == "Fra" & dffin$partydrawn == "La France Insoumise (FI)"] <- dffin$eupos_party4[dffin$country == "Fra" & dffin$partydrawn == "La France Insoumise (FI)"]
dffin$eupos_partydrawn[dffin$country == "Fra" & dffin$partydrawn == "Parti Socialiste (PS)"] <- dffin$eupos_party5[dffin$country == "Fra" & dffin$partydrawn == "Parti Socialiste (PS)"]
dffin$eupos_partydrawn[dffin$country == "Fra" & dffin$partydrawn == "Europe Écologie-Les Verts (EELV)"] <- dffin$eupos_party6[dffin$country == "Fra" & dffin$partydrawn == "Europe Écologie-Les Verts (EELV)"]
dffin$eupos_partydrawn[dffin$country == "Fra" & dffin$partydrawn == "Debout la France (DLF)"] <- dffin$eupos_party7[dffin$country == "Fra" & dffin$partydrawn =="Debout la France (DLF)"]
dffin$eupos_partydrawn[dffin$country == "Fra" & dffin$partydrawn == "Génération.s"] <- dffin$eupos_party8[dffin$country == "Fra" & dffin$partydrawn == "Génération.s"]

dffin$eupos_partydrawn[dffin$country == "Ger" & dffin$partydrawn == "Christlich Demokratische Union (CDU)"] <- dffin$eupos_party1[dffin$country == "Ger" & dffin$partydrawn == "Christlich Demokratische Union (CDU)"]
dffin$eupos_partydrawn[dffin$country == "Ger" & dffin$partydrawn == "Christlich Soziale Union (CSU)"] <- dffin$eupos_party2[dffin$country == "Ger" & dffin$partydrawn == "Christlich Soziale Union (CSU)"]
dffin$eupos_partydrawn[dffin$country == "Ger" & dffin$partydrawn == "Sozialdemokratische Partei Deutschlands (SPD)"] <- dffin$eupos_party3[dffin$country == "Ger" & dffin$partydrawn ==  "Sozialdemokratische Partei Deutschlands (SPD)"]
dffin$eupos_partydrawn[dffin$country == "Ger" & dffin$partydrawn == "Alternative für Deutschland (AfD)" ] <- dffin$eupos_party4[dffin$country == "Ger" & dffin$partydrawn == "Alternative für Deutschland (AfD)" ]
dffin$eupos_partydrawn[dffin$country == "Ger" & dffin$partydrawn == "Freie Demokratische Partei (FDP)"] <- dffin$eupos_party5[dffin$country == "Ger" & dffin$partydrawn == "Freie Demokratische Partei (FDP)"]
dffin$eupos_partydrawn[dffin$country == "Ger" & dffin$partydrawn == "Die Linke (Linke)"] <- dffin$eupos_party6[dffin$country == "Ger" & dffin$partydrawn == "Die Linke (Linke)"]
dffin$eupos_partydrawn[dffin$country == "Ger" & dffin$partydrawn == "Bündnis 90 / Die Grünen (Grüne)"] <- dffin$eupos_party7[dffin$country == "Ger" & dffin$partydrawn == "Bündnis 90 / Die Grünen (Grüne)"]
dffin$eupos_partydrawn[dffin$country == "Ger" & dffin$partydrawn == "Freie Wähler (FW)"] <- dffin$eupos_party8[dffin$country == "Ger" & dffin$partydrawn == "Freie Wähler (FW)"]

dffin$eupos_partydrawn[dffin$country == "Gre" & dffin$partydrawn == "Nέα Δημοκρατία (NΔ)"] <- dffin$eupos_party1[dffin$country == "Gre" & dffin$partydrawn == "Nέα Δημοκρατία (NΔ)"]
dffin$eupos_partydrawn[dffin$country == "Gre" & dffin$partydrawn == "Συνασπισμός Ριζοσπαστικής Αριστεράς (ΣΥ.ΡIΖ.Α)"] <- dffin$eupos_party2[dffin$country == "Gre" & dffin$partydrawn == "Συνασπισμός Ριζοσπαστικής Αριστεράς (ΣΥ.ΡIΖ.Α)"]
dffin$eupos_partydrawn[dffin$country == "Gre" & dffin$partydrawn == "Πανελλήνιο Σοσιαλιστικό ίνημα (ΠΑΣΟK)"] <- dffin$eupos_party3[dffin$country == "Gre" & dffin$partydrawn == "Πανελλήνιο Σοσιαλιστικό ίνημα (ΠΑΣΟK)"]
dffin$eupos_partydrawn[dffin$country == "Gre" & dffin$partydrawn == "Κίνημα Δημοκρατών Σοσιαλιστών (ΚΙΔΗΣΟ)"] <- dffin$eupos_party4[dffin$country == "Gre" & dffin$partydrawn == "Κίνημα Δημοκρατών Σοσιαλιστών (ΚΙΔΗΣΟ)"]
dffin$eupos_partydrawn[dffin$country == "Gre" & dffin$partydrawn == "Λαϊκός Σύνδεσμος -Χρυσή Αυγή (ΛΣ-ΧΑ)"] <- dffin$eupos_party5[dffin$country == "Gre" & dffin$partydrawn == "Λαϊκός Σύνδεσμος -Χρυσή Αυγή (ΛΣ-ΧΑ)"]
dffin$eupos_partydrawn[dffin$country == "Gre" & dffin$partydrawn == "Kομμουνιστικό Kόμμα Ελλάδας (KKΕ)"] <- dffin$eupos_party6[dffin$country == "Gre" & dffin$partydrawn == "Kομμουνιστικό Kόμμα Ελλάδας (KKΕ)"]
dffin$eupos_partydrawn[dffin$country == "Gre" & dffin$partydrawn == "Ένωση Κεντρώων (EK)"] <- dffin$eupos_party7[dffin$country == "Gre" & dffin$partydrawn == "Ένωση Κεντρώων (EK)"]
dffin$eupos_partydrawn[dffin$country == "Gre" & dffin$partydrawn == "Ανεξάρτητοι Έλληνες (ΑΝΕΛ)" ] <- dffin$eupos_party8[dffin$country == "Gre" & dffin$partydrawn == "Ανεξάρτητοι Έλληνες (ΑΝΕΛ)"]

dffin$eupos_partydrawn[dffin$country == "Hun" & dffin$partydrawn == "Fidesz – Magyar Polgári Szövetség (FIDESZ)"] <- dffin$eupos_party1[dffin$country == "Hun" & dffin$partydrawn == "Fidesz – Magyar Polgári Szövetség (FIDESZ)"]
dffin$eupos_partydrawn[dffin$country == "Hun" & dffin$partydrawn == "Kereszténydemokrata Néppárt (KDNP)"] <- dffin$eupos_party2[dffin$country == "Hun" & dffin$partydrawn == "Kereszténydemokrata Néppárt (KDNP)"]
dffin$eupos_partydrawn[dffin$country == "Hun" & dffin$partydrawn == "Jobbik Magyarországért Mozgalom (JOBBIK)"] <- dffin$eupos_party3[dffin$country == "Hun" & dffin$partydrawn == "Jobbik Magyarországért Mozgalom (JOBBIK)"]
dffin$eupos_partydrawn[dffin$country == "Hun" & dffin$partydrawn == "Magyar Szocialista Párt (MSZP)" ] <- dffin$eupos_party4[dffin$country == "Hun" & dffin$partydrawn == "Magyar Szocialista Párt (MSZP)"   ]
dffin$eupos_partydrawn[dffin$country == "Hun" & dffin$partydrawn == "Párbeszéd Magyarországért (Párbeszéd)"] <- dffin$eupos_party5[dffin$country == "Hun" & dffin$partydrawn == "Párbeszéd Magyarországért (Párbeszéd)"]
dffin$eupos_partydrawn[dffin$country == "Hun" & dffin$partydrawn == "Demokratikus Koalíció (DK)"] <- dffin$eupos_party6[dffin$country == "Hun" & dffin$partydrawn == "Demokratikus Koalíció (DK)"]
dffin$eupos_partydrawn[dffin$country == "Hun" & dffin$partydrawn == "Momentum Mozgalom (Momentum)"] <- dffin$eupos_party7[dffin$country == "Hun" & dffin$partydrawn =="Momentum Mozgalom (Momentum)"]
dffin$eupos_partydrawn[dffin$country == "Hun" & dffin$partydrawn == "Lehet Más a Politika (LMP)"] <- dffin$eupos_party8[dffin$country == "Hun" & dffin$partydrawn == "Lehet Más a Politika (LMP)"]

dffin$eupos_partydrawn[dffin$country == "Ire" & dffin$partydrawn == "Fine Gael (FG)"] <- dffin$eupos_party1[dffin$country == "Ire" & dffin$partydrawn == "Fine Gael (FG)"]
dffin$eupos_partydrawn[dffin$country == "Ire" & dffin$partydrawn == "Fianna Fáil (FF)"] <- dffin$eupos_party2[dffin$country == "Ire" & dffin$partydrawn == "Fianna Fáil (FF)"]
dffin$eupos_partydrawn[dffin$country == "Ire" & dffin$partydrawn == "Sinn Féin (SF)"] <- dffin$eupos_party3[dffin$country == "Ire" & dffin$partydrawn == "Sinn Féin (SF)" ]
dffin$eupos_partydrawn[dffin$country == "Ire" & dffin$partydrawn == "Labour Party (LP)" ] <- dffin$eupos_party4[dffin$country == "Ire" & dffin$partydrawn == "Labour Party (LP)"  ]
dffin$eupos_partydrawn[dffin$country == "Ire" & dffin$partydrawn == "Independent Alliance"] <- dffin$eupos_party5[dffin$country == "Ire" & dffin$partydrawn == "Independent Alliance"]
dffin$eupos_partydrawn[dffin$country == "Ire" & dffin$partydrawn == "Solidarity - People Before Profit (Solidarity-PBP)"] <- dffin$eupos_party6[dffin$country == "Ire" & dffin$partydrawn == "Solidarity - People Before Profit (Solidarity-PBP)"]
dffin$eupos_partydrawn[dffin$country == "Ire" & dffin$partydrawn == "Social Democrats (Daonlathaigh Shóisialta)" ] <- dffin$eupos_party7[dffin$country == "Ire" & dffin$partydrawn == "Social Democrats (Daonlathaigh Shóisialta)"]
dffin$eupos_partydrawn[dffin$country == "Ire" & dffin$partydrawn == "Green Party"] <- dffin$eupos_party8[dffin$country == "Ire" & dffin$partydrawn == "Green Party"]

dffin$eupos_partydrawn[dffin$country == "Ita" & dffin$partydrawn == "Lega (Lega Salvini Premier)"] <- dffin$eupos_party1[dffin$country == "Ita" & dffin$partydrawn == "Lega (Lega Salvini Premier)"]
dffin$eupos_partydrawn[dffin$country == "Ita" & dffin$partydrawn == "Movimento 5 Stelle (M5S)"] <- dffin$eupos_party2[dffin$country == "Ita" & dffin$partydrawn == "Movimento 5 Stelle (M5S)"]
dffin$eupos_partydrawn[dffin$country == "Ita" & dffin$partydrawn == "Partito Democratico (PD)"] <- dffin$eupos_party3[dffin$country == "Ita" & dffin$partydrawn == "Partito Democratico (PD)"]
dffin$eupos_partydrawn[dffin$country == "Ita" & dffin$partydrawn == "Forza Italia (FI)"] <- dffin$eupos_party4[dffin$country == "Ita" & dffin$partydrawn == "Forza Italia (FI)"]
dffin$eupos_partydrawn[dffin$country == "Ita" & dffin$partydrawn == "Fratelli d'Italia (FDI)"] <- dffin$eupos_party5[dffin$country == "Ita" & dffin$partydrawn == "Fratelli d'Italia (FDI)"]
dffin$eupos_partydrawn[dffin$country == "Ita" & dffin$partydrawn ==  "Più Europa (+E)"  ] <- dffin$eupos_party6[dffin$country == "Ita" & dffin$partydrawn ==  "Più Europa (+E)"  ]
dffin$eupos_partydrawn[dffin$country == "Ita" & dffin$partydrawn == "Sinistra Italiana (SI)"] <- dffin$eupos_party7[dffin$country == "Ita" & dffin$partydrawn == "Sinistra Italiana (SI)" ]
dffin$eupos_partydrawn[dffin$country == "Ita" & dffin$partydrawn == "Possibile (P)"] <- dffin$eupos_party8[dffin$country == "Ita" & dffin$partydrawn == "Possibile (P)"]

dffin$eupos_partydrawn[dffin$country == "Lat" & dffin$partydrawn == "Sociāldemokrātiskā partija „Saskaņa“ (SDP)"] <- dffin$eupos_party1[dffin$country == "Lat" & dffin$partydrawn == "Sociāldemokrātiskā partija „Saskaņa“ (SDP)"]
dffin$eupos_partydrawn[dffin$country == "Lat" & dffin$partydrawn == "Jaunā konservatīvā partija (JKP)"] <- dffin$eupos_party2[dffin$country == "Lat" & dffin$partydrawn == "Jaunā konservatīvā partija (JKP)"]
dffin$eupos_partydrawn[dffin$country == "Lat" & dffin$partydrawn == "Zaļo un Zemnieku savienība (ZZS: Latvijas Zemnieku savienība (LZS) + Latvijas Zaļā partija (LZP))" ] <- dffin$eupos_party3[dffin$country == "Lat" & dffin$partydrawn == "Zaļo un Zemnieku savienība (ZZS: Latvijas Zemnieku savienība (LZS) + Latvijas Zaļā partija (LZP))" ]
dffin$eupos_partydrawn[dffin$country == "Lat" & dffin$partydrawn == "Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)"] <- dffin$eupos_party4[dffin$country == "Lat" & dffin$partydrawn == "Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)"]
dffin$eupos_partydrawn[dffin$country == "Lat" & dffin$partydrawn == "'Kam pieder valsts' (KPV LV)"] <- dffin$eupos_party5[dffin$country == "Lat" & dffin$partydrawn == "'Kam pieder valsts' (KPV LV)"]
dffin$eupos_partydrawn[dffin$country == "Lat" & dffin$partydrawn == "Kustība Par! (Par!)"] <- dffin$eupos_party6[dffin$country == "Lat" & dffin$partydrawn == "Kustība Par! (Par!)" ]
dffin$eupos_partydrawn[dffin$country == "Lat" & dffin$partydrawn == "Latvijas attīstībai (LA)"] <- dffin$eupos_party7[dffin$country == "Lat" & dffin$partydrawn ==  "Latvijas attīstībai (LA)" ]
dffin$eupos_partydrawn[dffin$country == "Lat" & dffin$partydrawn == "Vienotība (V)"] <- dffin$eupos_party8[dffin$country == "Lat" & dffin$partydrawn == "Vienotība (V)"]

dffin$eupos_partydrawn[dffin$country == "Lit" & dffin$partydrawn =="Tėvynės sąjunga - Lietuvos krikščionys demokratai (TS-LKD)"] <- dffin$eupos_party1[dffin$country == "Lit" & dffin$partydrawn == "Tėvynės sąjunga - Lietuvos krikščionys demokratai (TS-LKD)"]
dffin$eupos_partydrawn[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)"] <- dffin$eupos_party2[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)" ]
dffin$eupos_partydrawn[dffin$country == "Lit" & dffin$partydrawn ==  "Lietuvos socialdemokratų partija (LSDP)" ] <- dffin$eupos_party3[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos socialdemokratų partija (LSDP)" ]
dffin$eupos_partydrawn[dffin$country == "Lit" & dffin$partydrawn == "Tvarka ir teisingumas (TT)"] <- dffin$eupos_party4[dffin$country == "Lit" & dffin$partydrawn == "Tvarka ir teisingumas (TT)" ]
dffin$eupos_partydrawn[dffin$country == "Lit" & dffin$partydrawn == "Darbo partija (DP)" ] <- dffin$eupos_party5[dffin$country == "Lit" & dffin$partydrawn == "Darbo partija (DP)"]
dffin$eupos_partydrawn[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos socialdemokratų darbo partija (LSDDP)"] <- dffin$eupos_party6[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos socialdemokratų darbo partija (LSDDP)"]
dffin$eupos_partydrawn[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos centro partija (LCP)"] <- dffin$eupos_party7[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos centro partija (LCP)"]
dffin$eupos_partydrawn[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos lenkų rinkimų akcija – Krikščioniškų šeimų sąjunga (LLRA–KŠS)"] <- dffin$eupos_party8[dffin$country == "Lit" & dffin$partydrawn == "Lietuvos lenkų rinkimų akcija – Krikščioniškų šeimų sąjunga (LLRA–KŠS)" ]

dffin$eupos_partydrawn[dffin$country == "Net" & dffin$partydrawn == "Volkspartij voor Vrijheid en Democratie (VVD)"] <- dffin$eupos_party1[dffin$country == "Net" & dffin$partydrawn == "Volkspartij voor Vrijheid en Democratie (VVD)"]
dffin$eupos_partydrawn[dffin$country == "Net" & dffin$partydrawn == "Partij voor de Vrijheid (PVV)"] <- dffin$eupos_party2[dffin$country == "Net" & dffin$partydrawn == "Partij voor de Vrijheid (PVV)"]
dffin$eupos_partydrawn[dffin$country == "Net" & dffin$partydrawn ==  "GroenLinks (GL)"] <- dffin$eupos_party3[dffin$country == "Net" & dffin$partydrawn ==  "GroenLinks (GL)" ]
dffin$eupos_partydrawn[dffin$country == "Net" & dffin$partydrawn == "Christen-Democratisch Appèl (CDA)" ] <- dffin$eupos_party4[dffin$country == "Net" & dffin$partydrawn == "Christen-Democratisch Appèl (CDA)" ]
dffin$eupos_partydrawn[dffin$country == "Net" & dffin$partydrawn == "Democraten 66 (D66)" ] <- dffin$eupos_party5[dffin$country == "Net" & dffin$partydrawn == "Democraten 66 (D66)" ]
dffin$eupos_partydrawn[dffin$country == "Net" & dffin$partydrawn == "Socialistische Partij (SP)"] <- dffin$eupos_party6[dffin$country == "Net" & dffin$partydrawn == "Socialistische Partij (SP)"]
dffin$eupos_partydrawn[dffin$country == "Net" & dffin$partydrawn == "Partij van de Arbeid (PvdA)"] <- dffin$eupos_party7[dffin$country == "Net" & dffin$partydrawn == "Partij van de Arbeid (PvdA)" ]
dffin$eupos_partydrawn[dffin$country == "Net" & dffin$partydrawn ==  "Forum voor Democratie (FvD)"] <- dffin$eupos_party8[dffin$country == "Net" & dffin$partydrawn ==   "Forum voor Democratie (FvD)" ]

dffin$eupos_partydrawn[dffin$country == "Pol" & dffin$partydrawn == "Prawo i Sprawiedliwość (PiS)"] <- dffin$eupos_party1[dffin$country == "Pol" & dffin$partydrawn == "Prawo i Sprawiedliwość (PiS)"]
dffin$eupos_partydrawn[dffin$country == "Pol" & dffin$partydrawn == "Platforma Obywatelska (PO)"] <- dffin$eupos_party2[dffin$country == "Pol" & dffin$partydrawn == "Platforma Obywatelska (PO)"]
dffin$eupos_partydrawn[dffin$country == "Pol" & dffin$partydrawn == "Wiosna" ] <- dffin$eupos_party3[dffin$country == "Pol" & dffin$partydrawn == "Wiosna" ]
dffin$eupos_partydrawn[dffin$country == "Pol" & dffin$partydrawn ==  "Kukuiz'15"] <- dffin$eupos_party4[dffin$country == "Pol" & dffin$partydrawn == "Kukuiz'15" ]
dffin$eupos_partydrawn[dffin$country == "Pol" & dffin$partydrawn == "Polskie Stronnictwo Ludowe (PSL)"] <- dffin$eupos_party5[dffin$country == "Pol" & dffin$partydrawn == "Polskie Stronnictwo Ludowe (PSL)"]
dffin$eupos_partydrawn[dffin$country == "Pol" & dffin$partydrawn == "Sojusz Lewicy Demokratycznej (SLD)"] <- dffin$eupos_party6[dffin$country == "Pol" & dffin$partydrawn == "Sojusz Lewicy Demokratycznej (SLD)" ]
dffin$eupos_partydrawn[dffin$country == "Pol" & dffin$partydrawn == ".Nowoczesna (.N)" ] <- dffin$eupos_party7[dffin$country == "Pol" & dffin$partydrawn == ".Nowoczesna (.N)"]
dffin$eupos_partydrawn[dffin$country == "Pol" & dffin$partydrawn == "Partia Roberta Biedronia"] <- dffin$eupos_party8[dffin$country == "Pol" & dffin$partydrawn == "Partia Roberta Biedronia" ]

dffin$eupos_partydrawn[dffin$country == "Por" & dffin$partydrawn == "Partido Socialista (PS)"] <- dffin$eupos_party1[dffin$country == "Por" & dffin$partydrawn == "Partido Socialista (PS)"]
dffin$eupos_partydrawn[dffin$country == "Por" & dffin$partydrawn == "Partido Social Democrata (PSD)"] <- dffin$eupos_party2[dffin$country == "Por" & dffin$partydrawn == "Partido Social Democrata (PSD)"]
dffin$eupos_partydrawn[dffin$country == "Por" & dffin$partydrawn == "Centro Democrático e Social – Partido Popular (CDS-PP)"] <- dffin$eupos_party3[dffin$country == "Por" & dffin$partydrawn == "Centro Democrático e Social – Partido Popular (CDS-PP)"]
dffin$eupos_partydrawn[dffin$country == "Por" & dffin$partydrawn == "Bloco de Esquerda (BE)"] <- dffin$eupos_party4[dffin$country == "Por" & dffin$partydrawn == "Bloco de Esquerda (BE)"]
dffin$eupos_partydrawn[dffin$country == "Por" & dffin$partydrawn == "Partido Comunista Português (PCP)" ] <- dffin$eupos_party5[dffin$country == "Por" & dffin$partydrawn == "Partido Comunista Português (PCP)" ]
dffin$eupos_partydrawn[dffin$country == "Por" & dffin$partydrawn == "Partido Ecologista 'os Verdes' (PEV)"] <- dffin$eupos_party6[dffin$country == "Por" & dffin$partydrawn == "Partido Ecologista 'os Verdes' (PEV)"]
dffin$eupos_partydrawn[dffin$country == "Por" & dffin$partydrawn == "Partido Democrático Republicano (PDR)"] <- dffin$eupos_party7[dffin$country == "Por" & dffin$partydrawn ==  "Partido Democrático Republicano (PDR)"]
dffin$eupos_partydrawn[dffin$country == "Por" & dffin$partydrawn == "Aliança"] <- dffin$eupos_party8[dffin$country == "Por" & dffin$partydrawn == "Aliança"]

dffin$eupos_partydrawn[dffin$country == "Rom" & dffin$partydrawn == "Partidul Social Democrat (PSD)"] <- dffin$eupos_party1[dffin$country == "Rom" & dffin$partydrawn == "Partidul Social Democrat (PSD)"]
dffin$eupos_partydrawn[dffin$country == "Rom" & dffin$partydrawn == "Partidul Național Liberal (PNL)"] <- dffin$eupos_party2[dffin$country == "Rom" & dffin$partydrawn == "Partidul Național Liberal (PNL)"]
dffin$eupos_partydrawn[dffin$country == "Rom" & dffin$partydrawn == "Alianța Liberalilor și Democraților (ALDE)"] <- dffin$eupos_party3[dffin$country == "Rom" & dffin$partydrawn == "Alianța Liberalilor și Democraților (ALDE)"]
dffin$eupos_partydrawn[dffin$country == "Rom" & dffin$partydrawn == "Uniunea Salvați România (USR)"] <- dffin$eupos_party4[dffin$country == "Rom" & dffin$partydrawn == "Uniunea Salvați România (USR)"]
dffin$eupos_partydrawn[dffin$country == "Rom" & dffin$partydrawn == "PRO România (PRO)"] <- dffin$eupos_party5[dffin$country == "Rom" & dffin$partydrawn == "PRO România (PRO)"]
dffin$eupos_partydrawn[dffin$country == "Rom" & dffin$partydrawn == "Uniunea Democrată Maghiară din România (UDMR - Romániai Magyar Demokrata Szövetség (RMDSZ))"] <- dffin$eupos_party6[dffin$country == "Rom" & dffin$partydrawn == "Uniunea Democrată Maghiară din România (UDMR - Romániai Magyar Demokrata Szövetség (RMDSZ))"]
dffin$eupos_partydrawn[dffin$country == "Rom" & dffin$partydrawn == "Partidul Libertății Unității și Solidarității (PLUS)"] <- dffin$eupos_party7[dffin$country == "Rom" & dffin$partydrawn == "Partidul Libertății Unității și Solidarității (PLUS)"]
dffin$eupos_partydrawn[dffin$country == "Rom" & dffin$partydrawn == "Partidul Mișcarea Populară (PMP)"] <- dffin$eupos_party8[dffin$country == "Rom" & dffin$partydrawn == "Partidul Mișcarea Populară (PMP)"]

dffin$eupos_partydrawn[dffin$country == "Slovakia" & dffin$partydrawn == "Smer – sociálna demokracia (Smer-SD)"] <- dffin$eupos_party1[dffin$country == "Slovakia" & dffin$partydrawn == "Smer – sociálna demokracia (Smer-SD)"]
dffin$eupos_partydrawn[dffin$country == "Slovakia" & dffin$partydrawn == "Sloboda a Solidarita (SaS)"] <- dffin$eupos_party2[dffin$country == "Slovakia" & dffin$partydrawn == "Sloboda a Solidarita (SaS)"]
dffin$eupos_partydrawn[dffin$country == "Slovakia" & dffin$partydrawn == "Obyčajní Ľudia a nezávislé osobnosti (OĽaNO)"] <- dffin$eupos_party3[dffin$country == "Slovakia" & dffin$partydrawn == "Obyčajní Ľudia a nezávislé osobnosti (OĽaNO)"]
dffin$eupos_partydrawn[dffin$country == "Slovakia" & dffin$partydrawn == "Kotleba – Ľudová strana Naše Slovensko (ĽSNS)"] <- dffin$eupos_party4[dffin$country == "Slovakia" & dffin$partydrawn == "Kotleba – Ľudová strana Naše Slovensko (ĽSNS)"]
dffin$eupos_partydrawn[dffin$country == "Slovakia" & dffin$partydrawn == "Progresívne Slovensko (PS)"] <- dffin$eupos_party5[dffin$country == "Slovakia" & dffin$partydrawn == "Progresívne Slovensko (PS)" ]
dffin$eupos_partydrawn[dffin$country == "Slovakia" & dffin$partydrawn == "SME RODINA – Boris Kollár (SME RODINA)"] <- dffin$eupos_party6[dffin$country == "Slovakia" & dffin$partydrawn == "SME RODINA – Boris Kollár (SME RODINA)"]
dffin$eupos_partydrawn[dffin$country == "Slovakia" & dffin$partydrawn == "Kresťanskodemokratické hnutie (KDH)"] <- dffin$eupos_party7[dffin$country == "Slovakia" & dffin$partydrawn == "Kresťanskodemokratické hnutie (KDH)"]
dffin$eupos_partydrawn[dffin$country == "Slovakia" & dffin$partydrawn == "Slovenská národná strana (SNS)"] <- dffin$eupos_party8[dffin$country == "Slovakia" & dffin$partydrawn == "Slovenská národná strana (SNS)"]

dffin$eupos_partydrawn[dffin$country == "Slovenia" & dffin$partydrawn == "Lista Marjana Šarca (LMŠ)"] <- dffin$eupos_party1[dffin$country == "Slovenia" & dffin$partydrawn == "Lista Marjana Šarca (LMŠ)"]
dffin$eupos_partydrawn[dffin$country == "Slovenia" & dffin$partydrawn == "Slovenska demokratska stranka (SDS)"] <- dffin$eupos_party2[dffin$country == "Slovenia" & dffin$partydrawn == "Slovenska demokratska stranka (SDS)"]
dffin$eupos_partydrawn[dffin$country == "Slovenia" & dffin$partydrawn == "Socialni demokrati (SD)"] <- dffin$eupos_party3[dffin$country == "Slovenia" & dffin$partydrawn == "Socialni demokrati (SD)"]
dffin$eupos_partydrawn[dffin$country == "Slovenia" & dffin$partydrawn == "Levica"] <- dffin$eupos_party4[dffin$country == "Slovenia" & dffin$partydrawn == "Levica"]
dffin$eupos_partydrawn[dffin$country == "Slovenia" & dffin$partydrawn ==  "Stranka modernega centra (SMC)"] <- dffin$eupos_party5[dffin$country == "Slovenia" & dffin$partydrawn ==  "Stranka modernega centra (SMC)"]
dffin$eupos_partydrawn[dffin$country == "Slovenia" & dffin$partydrawn == "Nova Slovenija - Krščanski demokrati (N.Si)"] <- dffin$eupos_party6[dffin$country == "Slovenia" & dffin$partydrawn == "Nova Slovenija - Krščanski demokrati (N.Si)"]
dffin$eupos_partydrawn[dffin$country == "Slovenia" & dffin$partydrawn == "Slovenska nacionalna stranka (SNS)"] <- dffin$eupos_party7[dffin$country == "Slovenia" & dffin$partydrawn ==  "Slovenska nacionalna stranka (SNS)"]
dffin$eupos_partydrawn[dffin$country == "Slovenia" & dffin$partydrawn == "Stranka Alenke Bratušek"] <- dffin$eupos_party8[dffin$country == "Slovenia" & dffin$partydrawn ==  "Stranka Alenke Bratušek" ]

dffin$eupos_partydrawn[dffin$country == "Spa" & dffin$partydrawn == "Partido Socialista Obrero Español (PSOE)"] <- dffin$eupos_party1[dffin$country == "Spa" & dffin$partydrawn == "Partido Socialista Obrero Español (PSOE)"]
dffin$eupos_partydrawn[dffin$country == "Spa" & dffin$partydrawn == "Partido Popular (PP)" ] <- dffin$eupos_party2[dffin$country == "Spa" & dffin$partydrawn == "Partido Popular (PP)" ]
dffin$eupos_partydrawn[dffin$country == "Spa" & dffin$partydrawn == "Ciudadanos - Partido de la Ciudadanía (C's)"] <- dffin$eupos_party3[dffin$country == "Spa" & dffin$partydrawn == "Ciudadanos - Partido de la Ciudadanía (C's)" ]
dffin$eupos_partydrawn[dffin$country == "Spa" & dffin$partydrawn == "Podemos" ] <- dffin$eupos_party4[dffin$country == "Spa" & dffin$partydrawn == "Podemos"]
dffin$eupos_partydrawn[dffin$country == "Spa" & dffin$partydrawn == "Vox"] <- dffin$eupos_party5[dffin$country == "Spa" & dffin$partydrawn == "Vox"]
dffin$eupos_partydrawn[dffin$country == "Spa" & dffin$partydrawn == "Izquierda Unida (IU)"] <- dffin$eupos_party6[dffin$country == "Spa" & dffin$partydrawn == "Izquierda Unida (IU)"]
dffin$eupos_partydrawn[dffin$country == "Spa" & dffin$partydrawn == "Esquerra Republicana de Catalunya (ERC)" ] <- dffin$eupos_party7[dffin$country == "Spa" & dffin$partydrawn == "Esquerra Republicana de Catalunya (ERC)" ]
dffin$eupos_partydrawn[dffin$country == "Spa" & dffin$partydrawn == "Partit Demòcrata Europeo Català (PDeCAT)"] <- dffin$eupos_party8[dffin$country == "Spa" & dffin$partydrawn == "Partit Demòcrata Europeo Català (PDeCAT)"]

dffin$eupos_partydrawn[dffin$country == "Swe" & dffin$partydrawn == "Socialdemokratiska arbetarpartiet (SAP)"] <- dffin$eupos_party1[dffin$country == "Swe" & dffin$partydrawn == "Socialdemokratiska arbetarpartiet (SAP)"]
dffin$eupos_partydrawn[dffin$country == "Swe" & dffin$partydrawn == "Sverigedemokraterna (SD)"] <- dffin$eupos_party2[dffin$country == "Swe" & dffin$partydrawn == "Sverigedemokraterna (SD)"]
dffin$eupos_partydrawn[dffin$country == "Swe" & dffin$partydrawn == "Moderata samlingspartiet (M)"] <- dffin$eupos_party3[dffin$country == "Swe" & dffin$partydrawn == "Moderata samlingspartiet (M)"]
dffin$eupos_partydrawn[dffin$country == "Swe" & dffin$partydrawn == "Kristdemokraterna (KD)"] <- dffin$eupos_party4[dffin$country == "Swe" & dffin$partydrawn == "Kristdemokraterna (KD)"]
dffin$eupos_partydrawn[dffin$country == "Swe" & dffin$partydrawn == "Vänsterpartiet (V)" ] <- dffin$eupos_party5[dffin$country == "Swe" & dffin$partydrawn == "Vänsterpartiet (V)"]
dffin$eupos_partydrawn[dffin$country == "Swe" & dffin$partydrawn == "Centerpartiet (C)" ] <- dffin$eupos_party6[dffin$country == "Swe" & dffin$partydrawn == "Centerpartiet (C)"  ]
dffin$eupos_partydrawn[dffin$country == "Swe" & dffin$partydrawn == "Miljöpartiet de Gröna (MP)"] <- dffin$eupos_party7[dffin$country == "Swe" & dffin$partydrawn == "Miljöpartiet de Gröna (MP)"]
dffin$eupos_partydrawn[dffin$country == "Swe" & dffin$partydrawn == "Liberalerna (L)"] <- dffin$eupos_party8[dffin$country == "Swe" & dffin$partydrawn == "Liberalerna (L)" ]

dffin$eupos_partydrawn[dffin$country == "UK" & dffin$partydrawn == "Labour Party (Labour)"] <- dffin$eupos_party1[dffin$country == "UK" & dffin$partydrawn == "Labour Party (Labour)"]
dffin$eupos_partydrawn[dffin$country == "UK" & dffin$partydrawn == "Conservative and Unionist Party (Conservative Party)"] <- dffin$eupos_party2[dffin$country == "UK" & dffin$partydrawn == "Conservative and Unionist Party (Conservative Party)"]
dffin$eupos_partydrawn[dffin$country == "UK" & dffin$partydrawn == "Brexit Party" ] <- dffin$eupos_party3[dffin$country == "UK" & dffin$partydrawn == "Brexit Party" ]
dffin$eupos_partydrawn[dffin$country == "UK" & dffin$partydrawn == "Liberal Democrats (Lib Dem)"  ] <- dffin$eupos_party4[dffin$country == "UK" & dffin$partydrawn =="Liberal Democrats (Lib Dem)"  ]
dffin$eupos_partydrawn[dffin$country == "UK" & dffin$partydrawn == "Scotish National Party (SNP)"  ] <- dffin$eupos_party5[dffin$country == "UK" & dffin$partydrawn == "Scotish National Party (SNP)"  ]
dffin$eupos_partydrawn[dffin$country == "UK" & dffin$partydrawn ==  "Green Party of England and Wales (GP)" ] <- dffin$eupos_party6[dffin$country == "UK" & dffin$partydrawn ==  "Green Party of England and Wales (GP)"]
dffin$eupos_partydrawn[dffin$country == "UK" & dffin$partydrawn ==  "Change UK – The Independent Group" ] <- dffin$eupos_party7[dffin$country == "UK" & dffin$partydrawn == "Change UK – The Independent Group" ]
dffin$eupos_partydrawn[dffin$country == "UK" & dffin$partydrawn == "UK Independence Party (UKIP)" ] <- dffin$eupos_party8[dffin$country == "UK" & dffin$partydrawn == "UK Independence Party (UKIP)"]





# get distance to respondents own party 
dffin$perceptionofownpartylr <- NA
dffin$perceptionofownpartylr[dffin$Q52 == dffin$partydrawn] <- 
  dffin$lrpos_partydrawn[dffin$Q52 == dffin$partydrawn]

tmp <- aggregate(dffin$perceptionofownpartylr, list(dffin$country_id), mean, na.rm = T)


names(tmp) <- c("country_id","respondentperceptionofownpartylr")
dffin <- merge(dffin, tmp, all.x = T, by = "country_id")


dffin$respondentdistancetoownpartylr <- abs(dffin$respondentperceptionofownpartylr - dffin$lrpos)
dffin$respondentdistancetoownpartylr[dffin$respondentperceptionofownpartylr %in% 98:99] <- NA



# now respondent distance to other parties (all parties)
dffin$respondentdistancetoparty1lr <- abs(dffin$lrpos_party1 - dffin$lrpos)
dffin$respondentdistancetoparty2lr <- abs(dffin$lrpos_party2 - dffin$lrpos)
dffin$respondentdistancetoparty3lr <- abs(dffin$lrpos_party3 - dffin$lrpos)
dffin$respondentdistancetoparty4lr <- abs(dffin$lrpos_party4 - dffin$lrpos)
dffin$respondentdistancetoparty5lr <- abs(dffin$lrpos_party5 - dffin$lrpos)
dffin$respondentdistancetoparty6lr <- abs(dffin$lrpos_party6 - dffin$lrpos)
dffin$respondentdistancetoparty7lr <- abs(dffin$lrpos_party7 - dffin$lrpos)
dffin$respondentdistancetoparty8lr <- abs(dffin$lrpos_party8 - dffin$lrpos)

dffin$respondentdistancetoparty1lr[dffin$lrpos_party1 %in% 98:99] <- NA
dffin$respondentdistancetoparty2lr[dffin$lrpos_party2 %in% 98:99] <- NA
dffin$respondentdistancetoparty3lr[dffin$lrpos_party3 %in% 98:99] <- NA
dffin$respondentdistancetoparty4lr[dffin$lrpos_party4 %in% 98:99] <- NA
dffin$respondentdistancetoparty5lr[dffin$lrpos_party5 %in% 98:99] <- NA
dffin$respondentdistancetoparty6lr[dffin$lrpos_party6 %in% 98:99] <- NA
dffin$respondentdistancetoparty7lr[dffin$lrpos_party7 %in% 98:99] <- NA
dffin$respondentdistancetoparty8lr[dffin$lrpos_party8 %in% 98:99] <- NA

# dffin$respondentdistancetooutpartylr_avg <- 
#   dffin$respondentdistancetoparty1lr + 
#   dffin$respondentdistancetoparty2lr + 
#   ...
#   dffin$respondentdistancetoparty8lr - 
#   dffin$respondentdistancetoownpartylr

dffin$respondentdistancetooutpartylr_avg <- apply(dffin[,grep("respondentdistancetoparty\\dlr", names(dffin) ) ], 1, sum, na.rm = T)
# this simple subtraction will not account for NAs, making NA whenever dffin$respondentdistancetoownpartylr is NA
# but these lines would be dropped from analysis anywaz
dffin$respondentdistancetooutpartylr_avg <- dffin$respondentdistancetooutpartylr_avg - dffin$respondentdistancetoownpartylr
dffin$respondentdistancetooutpartylr_avg <- dffin$respondentdistancetooutpartylr_avg / 5  





# now the same for EU dimension ####
# get distance to respondents own party 
dffin$perceptionofownpartyeu <- NA
dffin$perceptionofownpartyeu[dffin$Q52 == dffin$partydrawn] <- 
  dffin$eupos_partydrawn[dffin$Q52 == dffin$partydrawn]
tmp <- aggregate(dffin$perceptionofownpartyeu, list(dffin$country_id), mean, na.rm = T)

names(tmp) <- c("country_id","respondentperceptionofownpartyeu")
dffin <- merge(dffin, tmp, all.x = T, by = "country_id")


dffin$respondentdistancetoownpartyeu <- abs(dffin$respondentperceptionofownpartyeu - dffin$eupos)
dffin$respondentdistancetoownpartyeu[dffin$respondentperceptionofownpartyeu %in% 98:99] <- NA



# now respondent distance to other parties (all parties)
dffin$respondentdistancetoparty1eu <- abs(dffin$eupos_party1 - dffin$eupos)
dffin$respondentdistancetoparty2eu <- abs(dffin$eupos_party2 - dffin$eupos)
dffin$respondentdistancetoparty3eu <- abs(dffin$eupos_party3 - dffin$eupos)
dffin$respondentdistancetoparty4eu <- abs(dffin$eupos_party4 - dffin$eupos)
dffin$respondentdistancetoparty5eu <- abs(dffin$eupos_party5 - dffin$eupos)
dffin$respondentdistancetoparty6eu <- abs(dffin$eupos_party6 - dffin$eupos)
dffin$respondentdistancetoparty7eu <- abs(dffin$eupos_party7 - dffin$eupos)
dffin$respondentdistancetoparty8eu <- abs(dffin$eupos_party8 - dffin$eupos)

dffin$respondentdistancetoparty1eu[dffin$eupos_party1 %in% 98:99] <- NA
dffin$respondentdistancetoparty2eu[dffin$eupos_party2 %in% 98:99] <- NA
dffin$respondentdistancetoparty3eu[dffin$eupos_party3 %in% 98:99] <- NA
dffin$respondentdistancetoparty4eu[dffin$eupos_party4 %in% 98:99] <- NA
dffin$respondentdistancetoparty5eu[dffin$eupos_party5 %in% 98:99] <- NA
dffin$respondentdistancetoparty6eu[dffin$eupos_party6 %in% 98:99] <- NA
dffin$respondentdistancetoparty7eu[dffin$eupos_party7 %in% 98:99] <- NA
dffin$respondentdistancetoparty8eu[dffin$eupos_party8 %in% 98:99] <- NA

dffin$respondentdistancetooutpartyeu_avg <- apply(dffin[,grep("respondentdistancetoparty\\deu", names(dffin) ) ], 1, sum, na.rm = T)
dffin$respondentdistancetooutpartyeu_avg <- dffin$respondentdistancetooutpartyeu_avg - dffin$respondentdistancetoownpartyeu
dffin$respondentdistancetooutpartyeu_avg <- dffin$respondentdistancetooutpartyeu_avg / 5  
table(dffin$respondentdistancetooutpartyeu_avg, useNA = "ifany")


dffin$lr_extremity <- abs(dffin$lrpos - 6)





source("helper_scripts/translations.R")

dffin$clean_agedrawn <- dffin$agedrawn

dffin$clean_sexdrawn <- dffin$sexdrawn
dffin$clean_sexdrawn[dffin$sexdrawn %in% pl2female] <- "1_female"
dffin$clean_sexdrawn[dffin$sexdrawn %in% pl2male] <- "2_male"
table(dffin$clean_sexdrawn)
rm(pl2female, pl2male)

dffin$clean_classdrawn <- dffin$classdrawn
dffin$clean_classdrawn[dffin$classdrawn %in% pl2mi] <- "1_mid"
dffin$clean_classdrawn[dffin$classdrawn %in% pl2lo] <- "2_low_class"
dffin$clean_classdrawn[dffin$classdrawn %in% pl2hi] <- "3_high_class"
table(dffin$clean_classdrawn)
rm(pl2lo,pl2mi,pl2hi)

dffin$clean_relidrawn <- dffin$relidrawn
dffin$clean_relidrawn[dffin$relidrawn %in% pl2norel] <- "1_non_religious"
dffin$clean_relidrawn[dffin$relidrawn %in% pl2catholic] <- "2_catholic"
dffin$clean_relidrawn[dffin$relidrawn %in% pl2protestant] <- "3_protestant"
dffin$clean_relidrawn[dffin$relidrawn %in% pl2muslim] <- "4_muslim"
table(dffin$clean_relidrawn)
rm(pl2norel,pl2catholic,pl2protestant,pl2muslim)










dffin$co_partisan <- 0
dffin$out_partisan <- 0
dffin$co_partisan[dffin$outpartisan2 == "2_partisan_copartisan"] <- 1
dffin$out_partisan[dffin$outpartisan2 == "1_partisan_outpartisan"] <- 1

dffin$sd_years_in_gov_since2000 <- dffin$gov_experience_ego_since2000 / 365

dffin$sd_years_in_coalition_since2000 <- dffin$coalition_experience_ego_since2000 / 365

dffin$sd_years_wi_coalition_partner_since2000 <- dffin$joint_spell_since2000 / 365

dffin$sd_years_in_coalition_since2000 <- (dffin$sd_years_in_coalition_since2000 - min(dffin$sd_years_in_coalition_since2000)) / (max(dffin$sd_years_in_coalition_since2000) - min(dffin$sd_years_in_coalition_since2000))

dffin$sd_years_wi_coalition_partner_since2000 <- (dffin$sd_years_wi_coalition_partner_since2000 - min(dffin$sd_years_wi_coalition_partner_since2000)) / (max(dffin$sd_years_wi_coalition_partner_since2000) - min(dffin$sd_years_wi_coalition_partner_since2000))

dffin$sd_avg_district_magnitude_now <- (dffin$tier1_avemag - min(dffin$tier1_avemag)) / (max(dffin$tier1_avemag) - min(dffin$tier1_avemag))
dffin$sd_eff_n_parties_now <- (dffin$eff_n_parties - min(dffin$eff_n_parties)) / (max(dffin$eff_n_parties) - min(dffin$eff_n_parties))
dffin$sd_npi_now <- (dffin$current_npi - min(dffin$current_npi,na.rm=T)) / (max(dffin$current_npi,na.rm=T) - min(dffin$current_npi,na.rm=T))


dffin$sd_dalton_polarization_lr <- (dffin$dalton_polarization_lr - min(dffin$dalton_polarization_lr,na.rm=T)) / (max(dffin$dalton_polarization_lr,na.rm=T) - min(dffin$dalton_polarization_lr,na.rm=T))




aggregate(dffin$sd_years_wi_coalition_partner_since2000, list(dffin$outpartisan2), mean, na.rm = T)
#                  Group.1          x
# 1  2_partisan_copartisan 0.00000000
# 2     0_partisan_control 0.00000000
# 3 1_partisan_outpartisan 0.02768529
# 4         99_partisan_nd 0.00000000
# impute score for co-partisan? No, because for this group we have years in coalition already
# impute score for control group? Yes, that might be necessary
# create average values for control group
tmp <- aggregate(dffin$sd_years_wi_coalition_partner_since2000, list("Q52"=dffin$Q52, dffin$outpartisan2), mean, na.rm = T)
tmp <- tmp[tmp$Group.2 == "1_partisan_outpartisan",]
tmp$Group.2 <- NULL
names(tmp)[names(tmp) == "x"] <- "average_sd_years_wi_coalition_partner_since2000"
dffin <- merge(dffin, tmp, all.x = T, by = "Q52")
dffin$sd_years_wi_coalition_partner_since2000[dffin$outpartisan2 == "0_partisan_control"] <- dffin$average_sd_years_wi_coalition_partner_since2000[dffin$outpartisan2 == "0_partisan_control"]





summary(dffin$pl2gets[dffin$nopid == 0 & dffin$nationality == "co-national"])
summary(dffin$co_partisan[dffin$nopid == 0 & dffin$nationality == "co-national"])
summary(dffin$out_partisan[dffin$nopid == 0 & dffin$nationality == "co-national"])
summary(dffin$coalitiongovsupporter[dffin$nopid == 0 & dffin$nationality == "co-national"])
summary(dffin$coalition_partners_now[dffin$nopid == 0 & dffin$nationality == "co-national"])
summary(dffin$coalition_experience_ego_since2000[dffin$nopid == 0 & dffin$nationality == "co-national"] / 365)
summary(dffin$joint_spell_since2000[dffin$nopid == 0 & dffin$nationality == "co-national"] / 365)

summary(dffin$tier1_avemag[dffin$nopid == 0 & dffin$nationality == "co-national"])
summary(dffin$eff_n_parties[dffin$nopid == 0 & dffin$nationality == "co-national"])
summary(dffin$dalton_polarization_lr[dffin$nopid == 0 & dffin$nationality == "co-national"])
summary(dffin$current_npi[dffin$nopid == 0 & dffin$nationality == "co-national"])

tmp <- table(dffin$sex[dffin$nopid == 0 & dffin$nationality == "co-national"], useNA = "ifany")
tmp[1] / sum(tmp)
summary(dffin$age_continuous[dffin$nopid == 0 & dffin$nationality == "co-national"])
dffin$edu[dffin$edu > 30] <- 30
summary(dffin$edu[dffin$nopid == 0 & dffin$nationality == "co-national"])
table(!is.na(dffin$edu[dffin$nopid == 0 & dffin$nationality == "co-national"]))



summary(dffin$cmp_dist_lr[dffin$nopid == 0 & dffin$nationality == "co-national"])
table(!is.na(dffin$cmp_dist_lr[dffin$nopid == 0 & dffin$nationality == "co-national"]))
summary(dffin$avg_cmp_dist_lr_2000[dffin$nopid == 0 & dffin$nationality == "co-national"])
table(!is.na(dffin$avg_cmp_dist_lr_2000[dffin$nopid == 0 & dffin$nationality == "co-national"]))

summary(dffin$respondentdistancetooutpartylr_avg[dffin$nopid == 0 & dffin$nationality == "co-national"] - 
          dffin$respondentdistancetoownpartylr[dffin$nopid == 0 & dffin$nationality == "co-national"])
tmp <- dffin$respondentdistancetooutpartylr_avg[dffin$nopid == 0 & dffin$nationality == "co-national"] - 
  dffin$respondentdistancetoownpartylr[dffin$nopid == 0 & dffin$nationality == "co-national"]
table(!is.na(tmp))

summary(dffin$population[dffin$nopid == 0 & dffin$nationality == "co-national"])
summary(dffin$gdppc[dffin$nopid == 0 & dffin$nationality == "co-national"] / 1000)
summary(dffin$gini[dffin$nopid == 0 & dffin$nationality == "co-national"])






# source: https://ec.europa.eu/eurostat/de/web/migration-asylum/international-migration-citizenship/database
euro <- read.csv("data/eurostat/eurostat_immigration.tsv",
                 sep = ",", stringsAsFactors = F) # stringsasfactors = F added on Nov 28
euro <- data.frame(euro$geo.TIME_PERIOD.2018, stringsAsFactors = F)# stringsasfactors = F added on Nov 28
tmp <- strsplit(euro$euro.geo.TIME_PERIOD.2018, "\t")
euro <- matrix(unlist(tmp), ncol = 2, byrow = T)
euro[,2] <- gsub("[a-z]","", euro[,2])
euro <- data.frame(euro)
euro$num_immigrants_2018 <- as.numeric(as.character(euro$X2))

euro$country <- NA
euro$country[euro$X1 == "AT"] <- "Aus"
euro$country[euro$X1 == "BE"] <- "Bel"
euro$country[euro$X1 == "BG"] <- "Bul"
euro$country[euro$X1 == "CZ"] <- "Cze"
euro$country[euro$X1 == "DE"] <- "Ger"
euro$country[euro$X1 == "DK"] <- "Den"
euro$country[euro$X1 == "EE"] <- "Est"
euro$country[euro$X1 == "EL"] <- "Gre"
euro$country[euro$X1 == "ES"] <- "Spa"
euro$country[euro$X1 == "FI"] <- "Fin"
euro$country[euro$X1 == "FR"] <- "Fra"
euro$country[euro$X1 == "HR"] <- "Cro"
euro$country[euro$X1 == "HU"] <- "Hun"
euro$country[euro$X1 == "IE"] <- "Ire"
euro$country[euro$X1 == "IT"] <- "Ita"
euro$country[euro$X1 == "LT"] <- "Lit"
euro$country[euro$X1 == "LV"] <- "Lat"
euro$country[euro$X1 == "NL"] <- "Net"
euro$country[euro$X1 == "PL"] <- "Pol"
euro$country[euro$X1 == "PT"] <- "Por"
euro$country[euro$X1 == "RO"] <- "Rom"
euro$country[euro$X1 == "SE"] <- "Swe"
euro$country[euro$X1 == "SI"] <- "Slovenia"
euro$country[euro$X1 == "SK"] <- "Slovakia"
euro$country[euro$X1 == "UK"] <- "UK"

euro$X1 <- euro$X2 <- NULL

dffin <- merge(dffin, euro, all.x = T, by = "country")

dffin$num_immigrants_2018 <- dffin$num_immigrants_2018 / 1000
rm(euro)

dffin$num_immigrants_2018_01 <- (dffin$num_immigrants_2018 - min(dffin$num_immigrants_2018)) / (max(dffin$num_immigrants_2018) - min(dffin$num_immigrants_2018))




save(dffin, file = "data/aff_pol_df_analy_jan2023.RData")



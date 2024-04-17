lr.parties <- list(
  ### ordered list of party1, party2, ..., party8 by country ###
  Aus = c("Österreichische Volkspartei (ÖVP)", 
          "Sozialdemokratische Partei Österreich (SPÖ)",
          "Freiheitliche Partei Österreichs (FPÖ)",
          "NEOS – Das Neue Österreich und Liberales Forum (NEOS)",
          "Die Grünen – Die grüne Alternative (GRÜNE)",
          "JETZT - Liste Pilz (JETZT)",
          "EU-Austrittspartei (EUAUS)",
          "Kommunistische Partei Österreichs (KPÖ)"),
  Bel = c("Nieuw-Vlaamse Alliantie (N-VA)",
          "Parti socialiste (PS)",
          "Mouvement Réformateur (MR)",
          "Christen-Democratisch en Vlaams (CD&V)",
          "Open Vlaamse Liberalen en Democraten (Open Vld)",
          "Socialistische Partij Anders (sp.a)",
          "Centre démocrate humaniste (cdH)",
          "Vlaams Belang (VB)"),
  Bul = c("Граждани за европейско развитие на България (ГЕРБ)",
          "Българска социалистическа партия (БСП)",
          "Движение за права и свободи (ДПС)",
          "ВМРО – Българско Национално Движение",
          "Атака",
          "Воля",
          "Национален фронт за спасение на България (НФСБ)",
          "Алтернатива за българско възраждане (АБВ)"),
  Cro = c("Hrvatska demokratska zajednica (HDZ)",
          "Socijaldemokratska partija Hrvatske  (SDP)",
          "Živi zid",
          "Most nezavisnih lista (MOST)",
          "Hrvatska seljačka stranka  (HSS)",
          "Bandić Milan 365 - Stranka rada i solidarnosti (BM365)",
          "Bruna Esih – Zlatko Hasanbegović: Neovisni za Hrvatsku (NHR)",
          "Pametno"),
  Cze = c("ANO 2011 (ANO)",
          "Občanská demokratická strana (ODS)",
          "Česká pirátská strana (Piráti)",
          "Svoboda a přímá demokracie (SPD)",
          "Komunistická strana Čech a Moravy (KSČM)",
          "Česká strana sociálně demokratická (ČSSD)",
          "Křesťanská a demokratická unie - Československá strana lidová (KDU-ČSL)",
          "Tradice Odpovědnost Prosperita 09 (TOP 09)"),
  Den = c("Socialdemokratiet (A)",
          "Venstre (V)",
          "Dansk Folkeparti (O)",
          "Det Radikale Venstre (B)",
          "Socialistisk Folkeparti (F)",
          "Enhedslisten – De rød-grønne (Ø)",
          "Liberal Alliance (I)",
          "Alternativet (Å)"),
  Est = c("Eesti Reformierakond (ER)",
          "Eesti Keskerakond (KE)",
          "Eesti Konservatiivne Rahvaerakond (EKRE)",
          "Sotsiaaldemokraatlik Erakond (SDE)",
          "Isamaa (I)",
          "Eesti 200",
          "Erakond Eestimaa Rohelised (EER)",
          "Eesti Vabaerakond (EVA)"),
  Fin = c("Suomen Sosialidemokraattinen Puolue (SDP)",
          "Kansallinen Kokoomus (KOK)",
          "Perussuomalaiset (PS)",
          "Suomen Keskusta (KESK)",
          "Vihreä liitto (VIHR)",
          "Vasemmistoliitto (VAS)",
          "Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))",
          "Kristillisdemokraatit (KD)"),
  Fra = c("La République En Marche (REM)",
          "Les Républicains (LR)",
          "Rassemblement national (RN (ex. Front National (FN)))",
          "La France Insoumise (FI)",
          "Parti Socialiste (PS)",
          "Europe Écologie-Les Verts (EELV)",
          "Debout la France (DLF)",
          "Génération.s"),
  Ger = c("Christlich Demokratische Union (CDU)",
          "Christlich Soziale Union (CSU)",
          "Sozialdemokratische Partei Deutschlands (SPD)",
          "Alternative für Deutschland (AfD)",
          "Freie Demokratische Partei (FDP)",
          "Die Linke (Linke)",
          "Bündnis 90 / Die Grünen (Grüne)",
          "Freie Wähler (FW)"),
  Gre = c("Nέα Δημοκρατία (NΔ)",
          "Συνασπισμός Ριζοσπαστικής Αριστεράς (ΣΥ.ΡIΖ.Α)",
          "Πανελλήνιο Σοσιαλιστικό ίνημα (ΠΑΣΟK)",
          "Κίνημα Δημοκρατών Σοσιαλιστών (ΚΙΔΗΣΟ)",
          "Λαϊκός Σύνδεσμος -Χρυσή Αυγή (ΛΣ-ΧΑ)",
          "Kομμουνιστικό Kόμμα Ελλάδας (KKΕ)",
          "Ένωση Κεντρώων (EK)",
          "Ανεξάρτητοι Έλληνες (ΑΝΕΛ)"),
  Hun = c("Fidesz – Magyar Polgári Szövetség (FIDESZ)",
          "Kereszténydemokrata Néppárt (KDNP)",
          "Jobbik Magyarországért Mozgalom (JOBBIK)",
          "Magyar Szocialista Párt (MSZP)",
          "Párbeszéd Magyarországért (Párbeszéd)",
          "Demokratikus Koalíció (DK)",
          "Momentum Mozgalom (Momentum)",
          "Lehet Más a Politika (LMP)"),
  Ire = c("Fine Gael (FG)",
          "Fianna Fáil (FF)",
          "Sinn Féin (SF)",
          "Labour Party (LP)",
          "Independent Alliance",
          "Solidarity - People Before Profit (Solidarity-PBP)",
          "Social Democrats (Daonlathaigh Shóisialta)",
          "Green Party"),
  Ita = c("Lega (Lega Salvini Premier)",
          "Movimento 5 Stelle (M5S)",
          "Partito Democratico (PD)",
          "Forza Italia (FI)",
          "Fratelli d'Italia (FDI)",
          "Più Europa (+E)",
          "Sinistra Italiana (SI)",
          "Possibile (P)"),
  Lat = c("Sociāldemokrātiskā partija „Saskaņa“ (SDP)",
          "Jaunā konservatīvā partija (JKP)",
          "Zaļo un Zemnieku savienība (ZZS: Latvijas Zemnieku savienība (LZS) + Latvijas Zaļā partija (LZP))",
          "Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)",
          "'Kam pieder valsts' (KPV LV)",
          "Kustība Par! (Par!)",
          "Latvijas attīstībai (LA)",
          "Vienotība (V)"),
  Lit = c("Tėvynės sąjunga - Lietuvos krikščionys demokratai (TS-LKD)",
          "Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)",
          "Lietuvos socialdemokratų partija (LSDP)",
          "Tvarka ir teisingumas (TT)",
          "Darbo partija (DP)",
          "Lietuvos socialdemokratų darbo partija (LSDDP)",
          "Lietuvos centro partija (LCP)",
          "Lietuvos lenkų rinkimų akcija – Krikščioniškų šeimų sąjunga (LLRA–KŠS)"),
  Net = c("Volkspartij voor Vrijheid en Democratie (VVD)",
          "Partij voor de Vrijheid (PVV)",
          "GroenLinks (GL)",
          "Christen-Democratisch Appèl (CDA)",
          "Democraten 66 (D66)",
          "Socialistische Partij (SP)",
          "Partij van de Arbeid (PvdA)",
          "Forum voor Democratie (FvD)"),
  Pol = c("Prawo i Sprawiedliwość (PiS)",
          "Platforma Obywatelska (PO)",
          "Wiosna",
          "Kukuiz'15",
          "Polskie Stronnictwo Ludowe (PSL)",
          "Sojusz Lewicy Demokratycznej (SLD)",
          ".Nowoczesna (.N)",
          "Partia Roberta Biedronia"),
  Por = c("Partido Socialista (PS)",
          "Partido Social Democrata (PSD)",
          "Centro Democrático e Social – Partido Popular (CDS-PP)",
          "Bloco de Esquerda (BE)",
          "Partido Comunista Português (PCP)",
          "Partido Ecologista 'os Verdes' (PEV)",
          "Partido Democrático Republicano (PDR)",
          "Aliança"),
  Rom = c("Partidul Social Democrat (PSD)",
          "Partidul Național Liberal (PNL)",
          "Alianța Liberalilor și Democraților (ALDE)",
          "Uniunea Salvați România (USR)",
          "PRO România (PRO)",
          "Uniunea Democrată Maghiară din România (UDMR - Romániai Magyar Demokrata Szövetség (RMDSZ))",
          "Partidul Libertății Unității și Solidarității (PLUS)",
          "Partidul Mișcarea Populară (PMP)"),
  Slovakia = c("Smer – sociálna demokracia (Smer-SD)",
               "Sloboda a Solidarita (SaS)",
               "Obyčajní Ľudia a nezávislé osobnosti (OĽaNO)",
               "Kotleba – Ľudová strana Naše Slovensko (ĽSNS)",
               "Progresívne Slovensko (PS)",
               "SME RODINA – Boris Kollár (SME RODINA)",
               "Kresťanskodemokratické hnutie (KDH)",
               "Slovenská národná strana (SNS)"),
  Slovenia = c("Lista Marjana Šarca (LMŠ)",
               "Slovenska demokratska stranka (SDS)",
               "Socialni demokrati (SD)",
               "Levica",
               "Stranka modernega centra (SMC)",
               "Nova Slovenija - Krščanski demokrati (N.Si)",
               "Slovenska nacionalna stranka (SNS)",
               "Stranka Alenke Bratušek"),
  Spa = c("Partido Socialista Obrero Español (PSOE)",
          "Partido Popular (PP)",
          "Ciudadanos - Partido de la Ciudadanía (C's)",
          "Podemos",
          "Vox",
          "Izquierda Unida (IU)",
          "Esquerra Republicana de Catalunya (ERC)",
          "Partit Demòcrata Europeo Català (PDeCAT)"),
  Swe = c("Socialdemokratiska arbetarpartiet (SAP)",
          "Sverigedemokraterna (SD)",
          "Moderata samlingspartiet (M)",
          "Kristdemokraterna (KD)",
          "Vänsterpartiet (V)",
          "Centerpartiet (C)",
          "Miljöpartiet de Gröna (MP)",
          "Liberalerna (L)"),
  UK = c("Labour Party (Labour)",
         "Conservative and Unionist Party (Conservative Party)",
         "Brexit Party",
         "Liberal Democrats (Lib Dem)",
         "Scotish National Party (SNP)",
         "Green Party of England and Wales (GP)",
         "Change UK – The Independent Group",
         "UK Independence Party (UKIP)")
)
no.party.response <- c(
  ### Q52 response indicating no party ###
  Aus="Nein, ich stehe keiner politischen Partei nahe",
  Bel="Nee, ik voel mij niet verbonden met een politieke partij",
  Bul="Не, не се чувствате близък/а до която и да е политическа партия",
  Cro="Ne, ne osjećate bliskost prema nijednoj političkoj stranci",
  Cze="Ne, žádná politická strana vám nepřijde blízká",
  Den="Nej, du føler ikke, at dine holdninger ligger tæt op ad et af de politiske partier",
  Est="Ei, Te ei tunne end lähedasena mitte ühegi poliitilise erakonnaga",
  Fin="En tunne olevani läheinen minkään poliittisen puolueen kanssa",
  Fra="Non, je ne me sens proche d’aucun parti politique",
  Ger="Nein, ich stehe keiner politischen Partei nahe",
  Gre="Όχι, δεν συμμερίζεστε τις απόψεις κανενός πολιτικού κόμματος",
  Hun="Nem, nem érzi magát közelinek egy politikai párthoz sem",
  Ire="No, I do not feel close to any political party",
  Ita="No, non mi sento vicino a nessun partito politico in particolare",
  Lat="Nē, es nejūtos pietuvināts(-a) nevienai politiskajai partijai",
  Lit="Ne, Aš nesijaučiu artimas (-a) jokiai politinei partijai",
  Net="Nee, u voelt zich niet verbonden met één bepaalde politieke partij",
  Pol="Nie, żadna partia polityczna nie jest Panu(i) bliska",
  Por="Não, não se sente próximo de nenhum partido político",
  Rom="Nu, nu vă consideraţi apropiat(ă) de niciun partid politic",
  Slovakia="Nie, necítim afinitu k žiadnej politickej strane",
  Slovenia="Ne počutite se blizu nobeni politični stranki",
  Spa="No, no me siento próximo/a a ningún partido",
  Swe="Nej, du anser dig inte stå nära något politiskt",
  UK="No, I do not feel close to any political party"
  )
other.party.response <- c(
  ### Q52 response 'other (fill in the blank)' ###
  Aus="Andere (bitte ausfüllen)",
  Bel="Anders (vul in)",
  Bul="Друго [попълнете празното поле]",
  Cro="Ostalo [upišite u prazninu]",
  Cze="Jine",
  Den="Andet (udfyld tomt felt)",
  Est="Muu [täitke väli]",
  Fin="Muu [täytä tyhjä kenttä]",
  Fra="Autre (veuillez compléter)",
  Ger="Andere (bitte ausfüllen)",
  Gre="Άλλο",
  Hun="Egyéb (töltse ki)",
  Ire="Other (fill the blank)",
  Ita="Altro (completare)",
  Lat="Cits [aizpildiet tukšo lauciņu]",
  Lit=NA,
  Net="Anders (vul in)",
  Pol="Inne (proszę uzupełnić)",
  Por="Outro [preencha o espaço em branco]",
  Rom="Altul",
  Solvakia="Iné [vyplňte prázdne miesto]",
  Slovenia="Drugo (izpolnite)",
  Spa="Otro (escríbalo en el espacio en blanco)",
  Swe="Annat (fyll i)",
  UK="Other (fill the blank)"
  )
inner.match <- function(x, nm, l) {
  ### look for the index of x in l[[nm]] ###
  stopifnot(length(x) == length(nm))
  l.ln <- sapply(l, length)
  l.nm <- rep(names(l), l.ln)
  l.loc <- l.nm != c(paste0("!", head(l.nm, 1)), head(l.nm, -1))
  l.offset <- cumsum(l.loc * unname(rep(l.ln, l.ln))) - l.ln[1]
  l.idx <- 1:length(l.nm) - l.offset
  l.idx[match(paste(nm, x), paste(l.nm, unlist(l, use.names=F)))]
}
na.set <- function(x, val) ifelse(is.na(x), val, x)
normalize.party.name <- function(nm) gsub("–", "-", nm)
encode.party <- function(party.nm, country) {
  ### party name to {1, 2, ..., 8, None, Other, Unranked} ###
  lr.parties.nrm <- lapply(lr.parties, normalize.party.name)
  party.nm %<>% normalize.party.name
  ifelse(party.nm == no.party.response[country], "None",
         ifelse(party.nm == na.set(other.party.response[country], "unlikely to match"), "Other",
                na.set(inner.match(party.nm, country, lr.parties.nrm), "Unranked")))
}
get.cols <- function(idx, cols) {
  ### returns out[i] = cols[idx[i]] ###
  stopifnot(length(idx) == nrow(cols))
  vcols <- unlist(cols, use.names=F)
  suppressWarnings(idx <- as.numeric(idx))
  vcols[1:length(idx) + (idx - 1) * nrow(cols)]
}
enrich.party.stance <- function(data) {
  ### add columns with information on party stance, fix missing values ###
  cols <- paste0("lrpos_party", 1:8)
  for (val in c(98, 99)) {
    data[,cols][data[,cols] == val] <- NA
  }
  within(data, {
    player1.party <- encode.party(Q52, country);
    player2.party <- encode.party(partydrawn, country);
    player1.party.freq <- ifelse(player1.party == "None", NA, in.place.tapply(Q52, list(country, Q52), length) / in.place.tapply(player1.party != "None", country, sum));
    player1.party.lrpos <- get.cols(player1.party, data[,cols]);
    player2.party.lrpos <- get.cols(player2.party, data[,cols]);
  })
}
filter.party.stance <- function(data) {
  ### subset data as appropriate for party stance analysis ###
  data %<>% subset(PID %in% names(which(table(PID) == 6)))
  data %<>% subset(player1.party %in% 1:8)
  data %<>% subset(in.place.tapply(PID, PID, seq_along) == 1)
  cols <- c("country", "age_continuous", "sex", "class", "rel_belief", "lrpos", 
            paste0("lrpos_party", 1:8), "player1.party", "player1.party.freq", 
            "player1.party.lrpos")
  data %<>% subset(select=cols)
  data
}
analyze.placement.within.party <- function(data, fdr.q, counterfactual.alpha) {
  ### compute placement within party analysis ###
  n.total <- table(as.character(data$country)) # remember total including missing values
  data %<>% subset(!is.na(player1.party.lrpos) & !is.na(lrpos)) # drop missing values
  # loop over countries
  out <- do.call(rbind, lapply(names(n.total), function(cc) {
    data.cc <- subset(data, country == cc)
    x <- with(data.cc, abs(player1.party.lrpos - 6) - abs(lrpos - 6)) # difference in extremism
    n <- length(x)
    p <- pnorm(-abs(mean(x)) / sd(x) * sqrt(n)) # p value of z test
    p <- min(2 * p, 1) # make two-sided
    # estimate how different missing data should be to negate significance
    missing.x <- rnorm(n.total[cc] - n, 0, 1)
    missing.x <- missing.x - mean(missing.x)
    missing.x <- missing.x / sd(missing.x) * sd(x)
    missing.mean <- seq(-11, abs(mean(x)), 0.01)
    total.p <- sapply(missing.mean, function(m) { 
      total.x <- c(x * sign(mean(x)), missing.x + m)
      pnorm(-abs(mean(total.x)) / sd(total.x) * sqrt(length(total.x)))
    })
    counterfactual.mean <- missing.mean[max(which(2 * total.p >= counterfactual.alpha))] * sign(mean(x))
    data.frame(country=cc, n=n, mean=mean(x), p=p, n.missing=n.total[cc] - n, counterfactual.mean=counterfactual.mean)
  }))
  # compute false discovery rate
  p <- sort(setNames(out$p, out$country))
  i.max <- max(which(p <= 1:length(p) / length(p) * fdr.q))
  out$rejected <- out$country %in% names(p)[1:i.max]
  out
}
analyze.opposide.side.perception <- function(data, fdr.q) {
  self.perception <- with(data, tapply(player1.party.lrpos, list(country, player1.party), mean, na.rm=T))
  left.parties <- self.perception < 6
  left.perception <- with(subset(data, player1.party %in% left.parties))
  
}
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
    player1.party <- encode.party(Q52, country)
    player2.party <- encode.party(partydrawn, country)
    player1.party.freq <- ifelse(player1.party %in% c("None", "Other"), NA, in.place.tapply(Q52, paste(country, Q52), length) / in.place.tapply(player1.party != "None", country, sum))
    player1.party.lrpos <- get.cols(player1.party, data[,cols])
    player2.party.lrpos <- get.cols(player2.party, data[,cols])
  })
}
fdr <- function(p.val, q) {
  ### compute rejections controlling false discovery rate <= q ###
  n <- length(p.val)
  # sort p.val and keep track of ordering
  idx <- 1:n
  ord <- order(p.val)
  p.val <- p.val[ord]
  idx <- idx[ord]
  # compute fdr
  rejected <- p.val <= 1:n / n * q
  rejected <- rev(cumsum(rev(rejected)) >= 1) # set all F before T to T
  # revert ordering
  rejected[order(idx)]
}
filter.party.stance <- function(data) {
  ### subset data as appropriate for party stance analysis ###
  data %<>% transform(country = as.character(country))
  data %<>% subset(PID %in% names(which(table(PID) == 6)))
  data %<>% subset(in.place.tapply(PID, PID, seq_along) == 1)
  data %<>% subset(player1.party %in% 1:8) # data were not collected for other parties
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
  out$rejected <- fdr(out$p, fdr.q)
  out
}
analyze.opposide.side.perception <- function(data, fdr.q, bt.num.iterations) {
  compute.statistic <- function(data) {
    # TODO: handle missing data
    is.right <- with(data, tapply(player1.party.lrpos, player1.party, mean, na.rm=T) > 6)
    data %<>% transform(.is.right = is.right[player1.party])
    left.perception <- setNames(colMeans(data[!data$.is.right, paste0("lrpos_party", 1:8)], na.rm=T), 1:8)
    right.perception <- setNames(colMeans(data[data$.is.right, paste0("lrpos_party", 1:8)], na.rm=T), 1:8)
    data %<>% within({
      .left.perception = left.perception[player1.party]
      .right.perception = right.perception[player1.party]
      .proponent.perception = ifelse(.is.right, .right.perception, .left.perception)
      .opponent.perception = ifelse(!.is.right, .right.perception, .left.perception)
    })
    stat <- with(data, mean(abs(.opponent.perception - 6) - abs(.proponent.perception - 6), na.rm=T))
    stat <- na.set(stat, 0) # possibly data contain parties from one side only
    stat
  }
  # loop over countries
  out <- do.call(rbind, lapply(unique(data$country), function(cc) {
    cat(cc)
    data.cc <- subset(data, country == cc)
    stat <- compute.statistic(data.cc)
    # bootstrap iterations
    bt.stat <- sapply(1:bt.num.iterations, function(i) {
      if (i %% 100 == 1) cat(".")
      compute.statistic(data.cc[sample.int(nrow(data.cc), replace=T),])
    })
    cat("\n")
    # bias corrected bootstrap percentile
    z0 <- qnorm(mean(bt.stat <= stat)) # (negative) bias
    p0 <- mean(bt.stat < 0) # sample percentile of null hypothesis
    p <- pnorm(qnorm(p0) - 2 * z0)
    data.frame(country=cc, n=nrow(data.cc), mean=stat, p=p)
  }))
  out$rejected <- fdr(out$p, fdr.q)
  out
}
lda <- function(xt, yt, xv) {
  ### linear discriminant analysis ###
  cl <- sort(unique(yt))
  pi <- prop.table(table(yt))
  mu <- sapply(cl, function(i) colMeans(xt[yt == i,,drop=F]))
  sigma <- Reduce("+", lapply(cl, function(i) {
    xtc <- t(t(xt[yt == i,,drop=F]) - mu[,match(i, cl)]) # xt centered
    t(xtc) %*% xtc
  }), 0) / (nrow(xt) - length(cl))
  b <- solve(sigma) %*% mu # y.hat = argmax(b*x + a)
  a <- as.numeric(log(pi) - diag(t(mu) %*% b / 2))
  yv.hat <- t(t(xv %*% b) + a)
  yv.hat <- unname(cl[apply(yv.hat, 1, which.max)])
  yv.hat
}
lda.stein.a <- function(xt, yt, xv) {
  ### linear discriminant analysis with stein shrinkage N(0,A) ###
  cl <- sort(unique(yt))
  n <- as.numeric(table(yt))
  pi <- prop.table(n)
  mu <- sapply(cl, function(i) colMeans(xt[yt == i,,drop=F]))
  sigma <- Reduce("+", lapply(cl, function(i) {
    xtc <- t(t(xt[yt == i,,drop=F]) - mu[,match(i, cl)]) # xt centered
    t(xtc) %*% xtc
  }), 0) / (nrow(xt) - length(cl))
  inv.sigma <- solve(sigma)
  r <- chol(inv.sigma)
  z <- r %*% mu
  a <- mean(t(t(z^2) - 1/n))
  theta <- t(t(z) * (1 - 1/(1 + a * n)))
  mu.adj <- solve(r) %*% theta
  b <- inv.sigma %*% mu.adj # y.hat = argmax(b*x + a)
  a <- as.numeric(log(pi) - diag(t(mu.adj) %*% b / 2))
  yv.hat <- t(t(xv %*% b) + a)
  yv.hat <- unname(cl[apply(yv.hat, 1, which.max)])
  yv.hat
}
lda.stein.ma <- function(xt, yt, xv) {
  ### linear discriminant analysis with stein shrinkage N(m,A) ###
  cl <- sort(unique(yt))
  n <- as.numeric(table(yt))
  pi <- prop.table(n)
  mu <- sapply(cl, function(i) colMeans(xt[yt == i,,drop=F]))
  sigma <- Reduce("+", lapply(cl, function(i) {
    xtc <- t(t(xt[yt == i,,drop=F]) - mu[,match(i, cl)]) # xt centered
    t(xtc) %*% xtc
  }), 0) / (nrow(xt) - length(cl))
  r <- chol(solve(sigma))
  inv.r <- solve(r)
  z <- r %*% mu
  m <- rowMeans(z)
  a <- sum((z - m)^2) / nrow(z) / (ncol(z) - 1) - mean(1/n)
  u <- as.numeric(inv.r %*% m)
  mu.adj <- u + t(t(mu - u) * (1 - 1/(1 + a*n)))
  b <- solve(sigma) %*% mu.adj # y.hat = argmax(b*x + a)
  a <- as.numeric(log(pi) - diag(t(mu.adj) %*% b / 2))
  yv.hat <- t(t(xv %*% b) + a)
  yv.hat <- unname(cl[apply(yv.hat, 1, which.max)])
  yv.hat
}
qda <- function(xt, yt, xv, shrinkage) {
  ### quadratic discriminant analysis with covariance shrinkage (1=no shrinkage) ###
  stopifnot(shrinkage >= 0 & shrinkage <= 1)
  cl <- sort(unique(yt))
  pi <- prop.table(table(yt))
  mu <- lapply(cl, function(i) colMeans(xt[yt == i,,drop=F]))
  sigma1 <- lapply(cl, function(i) { # sigma with shrinkage=1
    xtc <- t(t(xt[yt == i,,drop=F]) - mu[[match(i, cl)]]) # xt centered
    t(xtc) %*% xtc / (nrow(xtc) - 1)
  })
  sigma0 <- Reduce("+", lapply(cl, function(i) { # sigma with shrinkage=0
    xtc <- t(t(xt[yt == i,,drop=F]) - mu[[match(i, cl)]]) # xt centered
    t(xtc) %*% xtc
  }), 0) / (nrow(xt) - length(cl))
  sigma <- lapply(cl, function(i) {
    sigma1[[match(i, cl)]] * shrinkage + sigma0 * (1 - shrinkage)
  })
  det.sigma <- sapply(sigma, det)
  inv.sigma <- lapply(sigma, solve)
  a <- as.numeric(log(pi) - log(det.sigma) / 2)
  yv.hat <- sapply(cl, function(i) {
    xvc <- t(t(xv) - mu[[match(i, cl)]])
    a[i] - diag(xvc %*% inv.sigma[[match(i, cl)]] %*% t(xvc)) / 2
  })
  yv.hat <- unname(cl[apply(yv.hat, 1, which.max)])
  yv.hat
}
score.predictions <- function(y, ...) {
  preds <- list(...)
  stopifnot(all(sapply(preds, function(pred) length(pred) == length(y))))
  nms <- names(preds)
  means <- sapply(nms, function(nm) mean(y == preds[[nm]]))
  p <- sapply(nms, function(nm1) {
    sapply(nms, function(nm2) {
      se <- sd((y == preds[[nm1]]) - (y == preds[[nm2]])) / sqrt(length(y))
      unname(2 * pnorm(-abs(means[nm1] - means[nm2]) / se))
    })
  })
  p <- p * outer(1:nrow(p), 1:nrow(p), function(i, j) ifelse(i >= j, NA, 1))
  list(mean=means, p=p)
}
add.fdr.for.prediction.scores <- function(out, q) {
  ### large scale FDR over the prediction scores output ###
  mat <- sapply(out, function(o) o$p)
  nms <- rownames(out[[1]]$p)
  k <- length(nms)
  stopifnot(k %% 1 == 0)
  p <- mat[!is.na(mat)]
  rejected <- fdr(p, q)
  mat[!is.na(mat)] <- rejected
  for (i in 1:length(out)) {
    out[[i]]$rejected <- matrix(as.logical(mat[,i]), nrow=k, ncol=k, dimnames=list(nms, nms))
  }
  out
}
predict.party.alignment <- function(data, train.frac, nfolds, fdr.q) {
  countries <- unique(data$country)
  out <- lapply(countries, function(cc) {
    data.cc <- subset(data, country == cc)
    set.seed(2)
    ord <- sample(1:nrow(data.cc))
    dt <- head(data.cc[ord,], nrow(data.cc) * train.frac)
    dv <- tail(data.cc[ord,], -nrow(dt))
    # prepare covariates and response
    cl <- 1:8 # classes
    avg.lrpos <- colMeans(dt[,paste0("lrpos_party", 1:8)], na.rm=T)
    xt <- na.set(t(t(as.matrix(dt[, paste0("lrpos_party", cl)])) - avg.lrpos), 0)
    yt <- as.numeric(dt$player1.party)
    xv <- na.set(t(t(as.matrix(dv[, paste0("lrpos_party", cl)])) - avg.lrpos), 0)
    # predictions
    yv.hat.baseline <- rep(which.max(table(yt)), nrow(xv)) # predict mode
    yv.hat.lda <- lda(xt, yt, xv) # LDA
    yv.hat.lda.stein.a <- lda.stein.a(xt, yt, xv)
    yv.hat.lda.stein.ma <- lda.stein.ma(xt, yt, xv)
    # QDA with shrinkage determined by cross validation
    fac <- floor((1:nrow(xt) - 1) / nrow(xt) * nfolds) + 1
    shrinkage.vals <- 0:19/20
    cv.res <- sapply(1:nfolds, function(f) {
      mask <- fac != f
      xtt <- xt[mask,]
      ytt <- yt[mask]
      xtv <- xt[!mask,]
      ytv <- yt[!mask]
      sapply(shrinkage.vals, function(i) {
        ytv.hat <- qda(xtt, ytt, xtv, i)
        sum(ytv == ytv.hat)
        })
      })
    shrinkage <- shrinkage.vals[which.max(rowSums(cv.res))]
    yv.hat.qda.cv <- qda(xt, yt, xv, shrinkage)
    # score predictions
    yv <- as.numeric(dv$player1.party)
    score.predictions(yv, baseline=yv.hat.baseline, lda=yv.hat.lda,
                      lda.stein.ma=yv.hat.lda.stein.ma,
                      qda.cv=yv.hat.qda.cv)
  })
  names(out) <- countries
  out %<>% add.fdr.for.prediction.scores(fdr.q)
  out
}

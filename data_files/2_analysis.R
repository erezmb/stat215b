library(lme4)
library(MASS)
library(foreign)
library(modelsummary) # for R2 stats


rm(list = ls())

# setwd("~")
load("data/aff_pol_df_analy_jan2023.RData")


# specify path of output file: 
path <- "output"


# TABLE A1: Age breakdown #####
table(dffin$country, dffin$agegroup) / 6

# TABLE A2: Gender breakdown #####
table(dffin$country, dffin$sex) / 6

# TABLE C1: Treatment breakdown ####
summary(dffin$pl2gets)
sd(dffin$pl2gets)
table(dffin$diffsex)
table(dffin$diffage)
table(dffin$diffclass)
table(dffin$diffreligion)
table(dffin$nationality != "co-national")
table(dffin$outpartisan2)


# FIGURE C1 AND TABLE C2 #####
par(mfrow = c(1,2), mar = c(2,3,0,3), oma = c(1,16.5,1,1))
tmp <- aggregate(dffin$coalition_experience_ego_since2000[dffin$nopid == 0],
                 list(dffin$Q52[dffin$nopid == 0]),
                 mean, na.rm = T)
tmp$x <- tmp$x / 365
tmp <- tmp[rev(order(tmp$x)),]
tmp <- tmp[1:25,]

tmp$Group.1[tmp$Group.1 == "Svenska folkpartiet i Finland (SFP (Suomen ruotsalainen kansanpuolue (RKP)))"] <- "SFP/RKP"
tmp$Group.1[tmp$Group.1 == "Zaļo un Zemnieku savienība (ZZS: Latvijas Zemnieku savienība (LZS) + Latvijas Zaļā partija (LZP))"] <- "Zaļo un Zemnieku savienība (ZZS)"
tmp$Group.1[tmp$Group.1 == "Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)"] <- "Nacionālā apvienība „Visu Latvijai!” (NA)"   
plot(1,1, type = "n", xlim = c(0,20), ylim = c(nrow(tmp),0), axes = F)
rect(0, 1:nrow(tmp)-.3, tmp$x, 1:nrow(tmp)+.3, col = "grey", border = F)
axis(1)
axis(2, at = 1:nrow(tmp), tmp$Group.1, cex.axis = .9, las = 2, tick = F)
mtext("Years in Coalition Government", side = 1, line = 2)

tmp <- aggregate(dffin$coalition_experience_ego_since2000[dffin$nopid == 0],
                 list(dffin$Q52[dffin$nopid == 0]),
                 mean, na.rm = T)

tmp$y <- tmp$x <- tmp$x / 365

tmp$y[tmp$x >= 0] <- 0
tmp$y[tmp$x >= 3] <- 3
tmp$y[tmp$x >= 6] <- 6
tmp$y[tmp$x >= 9] <- 9
tmp$y[tmp$x >= 12] <- 12
tmp$y[tmp$x >= 15] <- 15
tmp$y[tmp$x >= 18] <- 18
table(tmp$y)



tmp <- aggregate(dffin$joint_spell_since2000,
                 list(dffin$Q52,dffin$partydrawn, dffin$partisanshipshown),mean, na.rm = T)
tmp <- tmp[tmp$Group.3 == 1,]
tmp$Group.3 <- NULL
tmp$x <- tmp$x / 365
summary(tmp$x)


# weed out duplicates
tmp$duplicated_ind <- 0
for ( i in 1:nrow(tmp)){
  tmpgroups <- c(tmp$Group.1[i], tmp$Group.2[i])
  for ( j in (i+1):nrow(tmp)){
    if ( tmp$Group.1[j] %in% tmpgroups & tmp$Group.2[j] %in% tmpgroups ){
      tmp$duplicated_ind[j] <- 1
    }
  }
}

tmp <- tmp[tmp$duplicated_ind == 0,]
tmp$duplicated_ind <- NULL

tmp <- tmp[rev(order(tmp$x)),]
tmp <- tmp[1:25,]

tmp$Group.1[tmp$Group.1 == "Zaļo un Zemnieku savienība (ZZS: Latvijas Zemnieku savienība (LZS) + Latvijas Zaļā partija (LZP))"] <- "(ZZS)"
tmp$Group.2[tmp$Group.2 == "Zaļo un Zemnieku savienība (ZZS: Latvijas Zemnieku savienība (LZS) + Latvijas Zaļā partija (LZP))"] <- "(ZZS)"
tmp$clean_Group1 <-  unlist(regmatches(tmp$Group.1, gregexpr("(?<=\\().*?(?=\\))", tmp$Group.1, perl=T)))
tmp$clean_Group2 <-  unlist(regmatches(tmp$Group.2, gregexpr("(?<=\\().*?(?=\\))", tmp$Group.2, perl=T)))
tmp$clean_Group1[tmp$clean_Group1 == "SFP (Suomen ruotsalainen kansanpuolue (RKP"] <- "SFP/RKP"
tmp$clean_Group2[tmp$clean_Group2 == "SFP (Suomen ruotsalainen kansanpuolue (RKP"] <- "SFP/RKP"
#tmp <- tmp[order(tmp$x),]

plot(1,1, type = "n", xlim = c(0,20), ylim = c(nrow(tmp),0), axes = F)
rect(0, 1:nrow(tmp)-.3, tmp$x, 1:nrow(tmp)+.3, col = "grey", border = F)
axis(1)
axis(2, at = 1:nrow(tmp), paste0(tmp$clean_Group1, " & ", tmp$clean_Group2), cex.axis = .9, las = 2, tick = F)
mtext("Experience with Coalition Partner (Years)", side = 1, line = 2)


tmp <- aggregate(dffin$joint_spell_since2000,
                 list(dffin$Q52,dffin$partydrawn, dffin$partisanshipshown),mean, na.rm = T)
tmp <- tmp[tmp$Group.3 == 1,]


tmp$y <- tmp$x <- tmp$x / 365

tmp$y[tmp$x == 0] <- -1
tmp$y[tmp$x > 0] <- 0
tmp$y[tmp$x >= 3] <- 3
tmp$y[tmp$x >= 6] <- 6
tmp$y[tmp$x >= 9] <- 9
tmp$y[tmp$x >= 12] <- 12
tmp$y[tmp$x >= 15] <- 15
tmp$y[tmp$x >= 18] <- 18
table(tmp$y)
sum(table(tmp$y))


# TABLE C3 ####

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











# FIGURE 2 / TABLE D1: MAIN ANALYSIS POOLED EU25 ####

main_ex_att_dict <- lmer(pl2gets ~ 
                           round + 
                           euattachmentshown + 
                           partisanshipshown +
                           diffsex +
                           diffage +
                           diffclass + 
                           diffreligion + 
                           I(nationality != 'co-national') + 
                           outpartisan2 * nopid 
                         + (1 | country/country_id )
                         , dffin[
                           dffin$game == "dict",])
summary(main_ex_att_dict)

aggregate(dffin$pl2gets, 
          list(dffin$game), mean)


main_ex_att_trust <- lmer(pl2gets ~ 
                            round + 
                            euattachmentshown + 
                            partisanshipshown + 
                            diffsex +
                            diffage +
                            diffclass + 
                            diffreligion + 
                            I(nationality != 'co-national') + 
                            outpartisan2 * nopid
                          + (1 | country/country_id )
                          , dffin[
                            dffin$game == "trust",])
summary(main_ex_att_trust)


stargazer::stargazer(main_ex_att_dict, main_ex_att_trust, single.row = T,
                     title = c("Hierarchical Linear Model"),
                     dep.var.labels = "Tokens for Player 2",
                     column.labels = c("Dictator Game",
                                       "Trust Game" ), 
                     covariate.labels = c(
                       "Round 2",
                       "Round 3",
                       "EU Attachment Shown", # Treatment 1
                       "Partisanship Shown",
                       "Gender: Out-Group",
                       "(Respondent: Other Gender)",
                       "Age: Out-Group",
                       "Class: Out-Group",
                       "(Respondent: Don't Know Class)",
                       "Religion: Out-Group",
                       "(Respondent: Other Religion)",
                       "(Respondent: Non-Believer)",
                       "Nationality: Out-Group",
                       "Partisanship: Out-Group",
                       "(Respondent: No PID)",
                       "Partisanship: Control Group * (Respondent: No PID)",
                       "Partisanship: Out-Group * (Respondent: No PID)"
                     ),
                     add.lines = list(
                       
                       c("SD Respondent-level",
                         round(as.numeric(attributes(VarCorr(main_ex_att_dict)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(main_ex_att_trust)$"country_id:country")$stddev), 3)),
                       c("SD Country-level",
                         round(as.numeric(attributes(VarCorr(main_ex_att_dict)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(main_ex_att_trust)$"country")$stddev), 3)),
                       c("Conditional R-Squared",
                         round(get_gof(main_ex_att_dict)$r2.conditional, 3),
                         round(get_gof(main_ex_att_trust)$r2.conditional, 3)),
                       c("Respondents",
                         as.numeric(sapply(ranef(main_ex_att_dict),nrow)[1]),
                         as.numeric(sapply(ranef(main_ex_att_trust),nrow)[1])),
                       c("Countries",
                         as.numeric(sapply(ranef(main_ex_att_dict),nrow)[2]),
                         as.numeric(sapply(ranef(main_ex_att_trust),nrow)[2]))
                     ),
                     #digits = 2,
                     star.cutoffs = c(.1, .05, .01),
                     star.char = c("+", "*", "**"),
                     notes = c("+ p<0.1; * p<0.05; ** p<0.01") ,
                     notes.append=FALSE,
                     omit.stat = c("ll","aic","bic")
                     ,out = paste0(path, "/tabD1_main_v1.html")
                     )


# (number of obs: )
lmer(pl2gets ~ 
       (round + 
          euattachmentshown + 
          partisanshipshown + 
          diffsex +
          diffage +
          diffclass + 
          diffreligion + 
          I(nationality != 'co-national') + 
          outpartisan2 * nopid)  * game
     + (1 | country/country_id )
     , dffin)
# Number of obs: 178936, groups:  country_id:country, 29827; country, 25
table(dffin$country_id[dffin$game == "dict"] %in% dffin$country_id[dffin$game == "trust"])
table(dffin$country_id[dffin$game == "trust"] %in% dffin$country_id[dffin$game == "dict"] )


un_coefs1 <- main_ex_att_dict@beta
un_ses1 <- sqrt(diag(vcov(main_ex_att_dict)))
un_coefs2 <- main_ex_att_trust@beta
un_ses2 <- sqrt(diag(vcov(main_ex_att_trust)))

coefs <- cbind(un_coefs1,un_coefs2)
ses <- cbind(un_ses1,un_ses2)

coefsofinterest <- c("diffsex0_diff",
                     "diffage0_diff",
                     "diffclass0_diff",
                     "diffreligion0_diff",
                     "I(nationality != \"co-national\")TRUE",
                     "outpartisan21_partisan_outpartisan")

coefs <- coefs[rownames(vcov(main_ex_att_dict)) %in% coefsofinterest,]
ses <- ses[rownames(vcov(main_ex_att_dict)) %in% coefsofinterest,]

labels <- c("Gender: In-group","Gender: Out-group",
            "Age: In-group","Age: Out-group",
            "Class: In-group","Class: Out-group",
            "Religion: In-group","Religion: Out-group",
            "Nationality: In-group","Nationality: Out-group",
            "Partisanship: In-group","Partisanship: Out-group")

spots_ref <- c(15,13,11,9,7,5)
spots_eff <- c(14,12,10,8,6,4)

#jpeg(paste0(path, "/fig2_main_v1.jpeg"), units="in", width=9, height=5, res=200)
pdf(paste0(path, "/fig2_main_v1.pdf"), width=9, height=5)
par(mfrow = c(1,2), mar = c(3,.5,2,0), oma = c(1,1,1,1))
for (i in 1:2){
  plot( 1,1, type = "n", xlim = c(-1.5,1.5), ylim = c(4,15), axes = F, xlab = "",ylab = "" )
  abline( h = c(13.5, 11.5, 9.5, 7.5, 5.5), col = "grey")
  axis(1)
  abline(v=0, col = "grey")
  points(rep(0, length(spots_ref)) , spots_ref, pch = 16, cex = 1)
  segments(coefs[,i]-2*ses[,i], spots_eff , coefs[,i]+2*ses[,i], spots_eff, lwd = 2)
  points(coefs[,i],spots_eff , pch = 16, cex = 1, col = "white")
  points(coefs[,i],spots_eff , pch = 1, cex = 1)
  mtext(c("Dictator game",
          "Trust game")[i], side = 3, line = 1, cex = 1.25, outer = F)
  
  main_ex_att_dict@Gp
  # N
  nrow(main_ex_att_dict@frame)
  # J
  length(unique(main_ex_att_dict@frame$country_id))
  # C
  length(unique(main_ex_att_dict@frame$country))
  
  mtext(c("Dictator game",
          "Trust game")[i], side = 3, line = 1, cex = 1.25, outer = F)
  
  if ( i == 1){
    legend("topleft", legend = c("In-group","Out-group"), pch = c(16,1), box.lwd = 0, box.col = "white")
  }
  text(0.5, spots_eff + .5, labels = c("Gender", "Age", "Class","Religion", "Nationality", "Partisanship") , cex = .95, adj = 0)
  
  #text(c(0,coefs[5:6, i]) - .125, 7:5, labels = c("Co-national", "Out-national (EU)", "Out-national (Non-EU)"), cex = .85, adj = 1)
  
}
mtext("Allocation of Tokens", side = 1, line = 0 , cex = 1, outer = T)
dev.off()





# FIGURE 3 : MAIN ANALYSIS BY COUNTRY ####
countries <- levels(dffin$country)
dict_c <- list()
trust_c <- list()

Nobs_d <- NA
Nresp_d <- NA
Nobs_t <- NA
Nresp_t <- NA
for ( i in 1:25){
  dict_c[[i]] <- lmer(pl2gets ~ 
                        round + 
                        euattachmentshown + 
                        partisanshipshown + 
                        diffsex +
                        diffage +
                        diffclass + 
                        diffreligion + 
                        I(nationality != 'co-national') + 
                        outpartisan2 * nopid 
                      + (1 | country_id )
                      , dffin[dffin$country == countries[i] & 
                                dffin$game == "dict",])
  Nobs_d[i] <- dict_c[[i]]@devcomp$dims[names(dict_c[[i]]@devcomp$dims) == "N"]
  Nresp_d[i] <- dict_c[[i]]@devcomp$dims[names(dict_c[[i]]@devcomp$dims) == "q"]
  
  trust_c[[i]] <- lmer(pl2gets ~ 
                         round + 
                         euattachmentshown + 
                         partisanshipshown + 
                         diffsex +
                         diffage +
                         diffclass + 
                         diffreligion + 
                         I(nationality != 'co-national') + 
                         outpartisan2 * nopid 
                       + (1 | country_id )
                       , dffin[dffin$country == countries[i] &
                                 dffin$game == "trust",])
  Nobs_t[i] <- trust_c[[i]]@devcomp$dims[names(trust_c[[i]]@devcomp$dims) == "N"]
  Nresp_t[i] <- trust_c[[i]]@devcomp$dims[names(trust_c[[i]]@devcomp$dims) == "q"]
}

labels <- c("Sex: In-group","Sex: Out-group",
            "Age: In-group","Age: Out-group",
            "Class: In-group","Class: Out-group",
            "Religion: In-group","Religion: Out-group",
            "Nationality: In-group","Nationality: Out-group",
            "Partisanship: In-group","Partisanship: Out-group")


#jpeg(paste0(path, "/fig3_main_bycountry_v1.jpeg"), units="in", width=9, height=12, res=200)
pdf(paste0(path, "/fig3_main_bycountry_v1.pdf"), width=9, height=12)
par(mfrow = c(5,5), mar = c(3,.5,2,0), oma = c(1,1,1,1))
for ( i in 1:25){
  un_coefs1 <- dict_c[[i]]@beta
  un_ses1 <- sqrt(diag(vcov(dict_c[[i]])))
  un_coefs2 <- trust_c[[i]]@beta
  un_ses2 <- sqrt(diag(vcov(trust_c[[i]])))
  
  coefs <- cbind(un_coefs1,un_coefs2)
  ses <- cbind(un_ses1,un_ses2)
  
  coefs <- coefs[rownames(vcov(dict_c[[i]])) %in% coefsofinterest,]
  ses <- ses[rownames(vcov(dict_c[[i]])) %in% coefsofinterest,]
  
  
  plot( 1,1, type = "n", xlim = c(-1.5,1.5), ylim = c(4,15.5), axes = F, xlab = "",ylab = "" )
  mtext(labcountries[i], side = 3, cex = 1, line = 1)
  #mtext(labcountries[i], side = 3, cex = .75, line = .5)
  mtext(paste0(Nobs_d[i] + Nobs_t[i], " Obs., ", Nresp_t[i], " Respondents"), side = 3, cex = .73, line = 0)
  
  abline( h = c(13.5, 11.5, 9.5, 7.5, 5.5), col = "grey")
  axis(1, at = c(-1.5,-1,-.5,0,.5,1,1.5), labels = c("-1.5","-1","-.5","0",".5","1","1.5"), cex.axis = 1)
  abline(v=0, col = "grey")
  
  points(rep(0, length(spots_ref)) , spots_ref, pch = 16, cex = 1)
  # dictator game
  segments(coefs[,1]-1.96*ses[,1], spots_eff+.2 , coefs[,1]+1.96*ses[,1], spots_eff+.2, lwd = 2)
  points(coefs[,1],spots_eff+.2 , pch = 16, cex = 1, col = "white")
  points(coefs[,1],spots_eff+.2 , pch = 1, cex = 1)
  # trust game
  segments(coefs[,2]-1.96*ses[,2], spots_eff-.2 , coefs[,2]+1.96*ses[,2], spots_eff-.2, lwd = 2)
  points(coefs[,2],spots_eff-.2 , pch = 15, cex = 1, col = "white")
  points(coefs[,2],spots_eff-.2 , pch = 0, cex = 1)
  
  
  text(0.5, spots_eff + .5, labels = c("Gender", "Age", "Class","Religion", "Nationality", 
                                       "Partisanship") , cex = .95, adj = 0) # cex = .75
  
  
}

mtext("Allocation of Tokens", side = 1, line = 0 , cex = 1, outer = T)
dev.off()



for ( i in 1:25){
  dict_c[[i]] <- cbind(dict_c[[i]]@beta[rownames(vcov(dict_c[[i]])) == "outpartisan21_partisan_outpartisan"]
                       , countries[i]
  )
  
}

dict_c <- do.call(rbind, dict_c)
dict_c <- dict_c[order(dict_c[,1]),]

for ( i in 1:25){
  trust_c[[i]] <- cbind(trust_c[[i]]@beta[rownames(vcov(trust_c[[i]])) == "outpartisan21_partisan_outpartisan"]
                        , countries[i]
  )
}

trust_c <- do.call(rbind, trust_c)
trust_c <- trust_c[order(trust_c[,1]),]






# FIGURE 4 / TABLE D2: IN-GROUP FAVORITISM V OUT-GROUP DEROGATION ####

dffin$inoutattach2 <- relevel(dffin$outattach2 , 2)
dffin$inoutpartisan <- relevel(dffin$outpartisan2 , 2)



m1dict <- m1d <- lmer(pl2gets ~ 
                        inoutpartisan +
                        clean_agedrawn + 
                        clean_sexdrawn + 
                        clean_classdrawn + 
                        clean_relidrawn + 
                        euattachmentshown + 
                        round +
                        (1 | country/country_id),
                      data = dffin[dffin$game == "dict" & dffin$nationality == "co-national" & dffin$nopid == 0,])
summary(m1d)




lmer(pl2gets ~ 
       (inoutpartisan +
          clean_agedrawn + 
          clean_sexdrawn + 
          clean_classdrawn + 
          clean_relidrawn + 
          euattachmentshown + 
          round) * game
     + (1 | country/country_id),
     data = dffin[dffin$nationality == "co-national" & dffin$nopid == 0,])
#Number of obs: 77456, groups:  country_id:country, 18925; country, 25


#jpeg(paste0(path, "/fig4_components_pooledeu25_v1.jpeg"), units="in", width=9, height=5, res=200)
pdf(paste0(path, "/fig4_components_pooledeu25_v1.pdf"), width=9, height=5)
par(mfcol = c(2,2), mar = c(2,2,3,2), oma = c(2,4,0,1))
plot(1,1,type = "n", xlim = c(-1,1), ylim = c(.75,1.25), axes = F, xlab = "", ylab = "")
mtext("Allocation of Tokens", side = 1, line = 2)
#mtext("Overall effect", side = 3, line = -2,at=par("usr")[1]+0.25*diff(par("usr")[1:2]))
mtext("Dictator Game", side = 3, line = 0)
#mtext("Partisanship effect", side = 2, line = -1, las = 2)

abline(v=0, lty = 2, col = "grey")
axis(1)

set.seed(123)
betas <- mvrnorm(1000, mu = m1d@beta, vcov(m1d))

betas2 <- rbind(betas[,colnames(betas) %in% c("inoutpartisan2_partisan_copartisan","inoutpartisan1_partisan_outpartisan")]  )


points(median(betas2[,1]), 1, pch = 16, cex = 1.5)
segments(quantile(betas2[,1], .025), 1, 
         quantile(betas2[,1], .975), 1, lwd = 3)
points(median(betas2[,2]), 1, pch = 17, cex = 1.5)
segments(quantile(betas2[,2], .025), 1, 
         quantile(betas2[,2], .975), 1, lwd = 3)

text(median(betas2[,1]), 1.05, labels = "Co-Partisan (In-Group)",  cex = .8)
text(median(betas2[,2]), 1.05, labels = "Out-Partisan (Out-Group)", cex = .8)

text(0, 1.15, labels = "No Info (Control Group)",  cex = .8, adj = .5)
#segments(0,.8, 0.05, .75)



plot(1,1,type = "n", xlim = c(0,1.1), ylim = c(0,3), axes = F, xlab = "", ylab = "")
axis(1)
mtext("Dictator Game", side = 3, line = 0)

mtext("Absolute size", side = 1, line = 2)

for ( i in 1:nrow(betas2)){
  betas2[i,1] <- max(betas2[i,1], 0)
  betas2[i,2] <- abs(min(betas2[i,2], 0))
}

abline( h  = c(1,2), col = "grey")
segments(quantile(betas2[,1], .025),2, quantile(betas2[,1], .975),2, lwd = 3)
points(quantile(betas2[,1], .5), 2, pch = 16, cex = 1.5)
segments(quantile(betas2[,2], .025),1, quantile(betas2[,2], .975),1, lwd = 3)
points(quantile(betas2[,2], .5),1, pch = 17, cex = 1.5)


mtext(paste0("Difference: " ,
             round(quantile(betas2[,1] - betas2[,2], c(.5)), 2), 
             " (", round(quantile(betas2[,1] - betas2[,2], c(.025)), 2), 
             ", ", round(quantile(betas2[,1] - betas2[,2], c(.975)), 2), ")"),
      side = 3, line = -1)

axis(2, at = c(2,1), c("In-group\nfavoritism",
                       "Out-group\nderogation"), las = 2, tick = F)








m1d <- lmer(pl2gets ~ 
              inoutpartisan +
              clean_agedrawn + 
              clean_sexdrawn + 
              clean_classdrawn + 
              clean_relidrawn + 
              euattachmentshown + 
              round +
              (1 | country/country_id),
            data = dffin[dffin$game == "trust" & dffin$nationality == "co-national" & dffin$nopid == 0,])
summary(m1d)

plot(1,1,type = "n", xlim = c(-1,1), ylim = c(.75,1.25), axes = F, xlab = "", ylab = "")
mtext("Allocation of Tokens", side = 1, line = 2)
#mtext("Overall effect", side = 3, line = -2,at=par("usr")[1]+0.25*diff(par("usr")[1:2]))
mtext("Trust Game", side = 3, line = 0)
#mtext("Partisanship effect", side = 2, line = -1, las = 2)

abline(v=0, lty = 2, col = "grey")
axis(1)

set.seed(123)
betas <- mvrnorm(1000, mu = m1d@beta, vcov(m1d))

betas2 <- rbind(betas[,colnames(betas) %in% c("inoutpartisan2_partisan_copartisan","inoutpartisan1_partisan_outpartisan")]  )


points(median(betas2[,1]), 1, pch = 16, cex = 1.5)
segments(quantile(betas2[,1], .025), 1, 
         quantile(betas2[,1], .975), 1, lwd = 3)
points(median(betas2[,2]), 1, pch = 17, cex = 1.5)
segments(quantile(betas2[,2], .025), 1, 
         quantile(betas2[,2], .975), 1, lwd = 3)

text(median(betas2[,1]), 1.05, labels = "Co-Partisan (In-Group)",  cex = .8)
text(median(betas2[,2]), 1.05, labels = "Out-Partisan (Out-Group)", cex = .8)

text(0, 1.15, labels = "No Info (Control Group)",  cex = .8, adj = .5)



plot(1,1,type = "n", xlim = c(0,1.1), ylim = c(0,3), axes = F, xlab = "", ylab = "")
axis(1)
mtext("Trust Game", side = 3, line = 0)

mtext("Absolute size", side = 1, line = 2)

for ( i in 1:nrow(betas2)){
  betas2[i,1] <- max(betas2[i,1], 0)
  betas2[i,2] <- abs(min(betas2[i,2], 0))
}

abline( h  = c(1,2), col = "grey")
segments(quantile(betas2[,1], .025),2, quantile(betas2[,1], .975),2, lwd = 3)
points(quantile(betas2[,1], .5), 2, pch = 16, cex = 1.5)
segments(quantile(betas2[,2], .025),1, quantile(betas2[,2], .975),1, lwd = 3)
points(quantile(betas2[,2], .5),1, pch = 17, cex = 1.5)
axis(2, at = c(2,1), c("In-group\nfavoritism",
                       "Out-group\nderogation"), las = 2, tick = F)


mtext(paste0("Difference: " ,
             round(quantile(betas2[,1] - betas2[,2], c(.5)), 2), 
             " (", round(quantile(betas2[,1] - betas2[,2], c(.025)), 2), 
             ", ", round(quantile(betas2[,1] - betas2[,2], c(.975)), 2), ")"),
      side = 3, line = -1)

dev.off()

round(quantile(betas2[,1] - betas2[,2], c(.5)), 2)
round(quantile(betas2[,1] - betas2[,2], c(.025)), 2)
round(quantile(betas2[,1] - betas2[,2], c(.975)), 2)


stargazer::stargazer(m1dict, m1d, single.row = T,
                     title = c("Hierarchical Linear Model"),
                     dep.var.labels = "Tokens for Player 2",
                     column.labels = c("Dictator Game",
                                       "Trust Game" ), 
                     covariate.labels = c(
                       "Pl 2: Partisanship In-Group",
                       "Pl 2: Partisanship Out-Group",
                       "Pl 2 Age: 30",
                       "Pl 2 Age: 42",
                       "Pl 2 Age: 53",
                       "Pl 2 Age: 65",
                       "Pl 2: Male",
                       "Pl 2: Low Class",
                       "Pl 2: High Class",
                       "Pl 2: Catholic",
                       "Pl 2: Protestant",
                       "Pl 2: Muslim",
                       "Subset Condition 1",
                       "Round 2",
                       "Round 3"),
                     add.lines = list(
                       
                       c("SD Respondent-level",
                         round(as.numeric(attributes(VarCorr(m1dict)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(m1d)$"country_id:country")$stddev), 3)),
                       c("SD Country-level",
                         round(as.numeric(attributes(VarCorr(m1dict)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(m1d)$"country")$stddev), 3)),
                       c("Conditional R-Squared",
                         round(get_gof(m1dict)$r2.conditional, 3),
                         round(get_gof(m1d)$r2.conditional, 3)),
                       c("Respondents",
                         as.numeric(sapply(ranef(m1dict),nrow)[1]),
                         as.numeric(sapply(ranef(m1d),nrow)[1])),
                       c("Countries",
                         as.numeric(sapply(ranef(m1dict),nrow)[2]),
                         as.numeric(sapply(ranef(m1d),nrow)[2]))
                     ),
                     #digits = 2,
                     star.cutoffs = c(.1, .05, .01),
                     star.char = c("+", "*", "**"),
                     notes = c("+ p<0.1; * p<0.05; ** p<0.01") ,
                     notes.append=FALSE,
                     omit.stat = c("ll","aic","bic")
                     , out = paste0(path, "/tabD2_inout_v1.html")
                     )



# FIGURE 5 : BY COUNTRY ####
countries <- levels(dffin$country)

#jpeg(paste0(path, "/fig5_components_bycountry_v1.jpeg"), units="in", width=9, height=6, res=200)
pdf(paste0(path, "/fig5_components_bycountry_v1.pdf"), width=9, height=6)
par(mfrow = c(1,2), mar = c(3,1,1,1), oma = c(1,8,1,1))
plot(1,1,type = "n", xlim = c(-1,1), ylim = c(25,0), axes = F, xlab = "", ylab = "")
mtext("Allocation of Tokens", side = 1, line = 2)
#mtext("Overall effect", side = 3, line = -2,at=par("usr")[1]+0.25*diff(par("usr")[1:2]))
mtext("Dictator Game", side = 3, line = 0)
abline(v=0)
axis(1)

effects <- list()
for ( c in 1:25){
  m1d_c <- lmer(pl2gets ~ #diffreligion +
                  inoutpartisan +
                  clean_agedrawn + 
                  clean_sexdrawn + 
                  clean_classdrawn + 
                  clean_relidrawn + 
                  euattachmentshown + 
                  round + (1 | country_id),
                data = dffin[dffin$country == countries[c] & dffin$game == "dict" & dffin$nationality == "co-national" & dffin$nopid == 0,])
  
  
  
  set.seed(123)
  betas <- mvrnorm(1000, mu = m1d_c@beta, vcov(m1d_c))
  
  betas2 <- rbind(betas[,colnames(betas) %in% c("inoutpartisan2_partisan_copartisan","inoutpartisan1_partisan_outpartisan")]  )
  effects[[c]] <- apply(betas2,2, quantile, c(0.025,.5,.975))
  
}

effs1 <- effects

# sort
sortingvector <- NA
vec_in <- NA
vec_out <- NA
for ( c in 1:25){
  # sort by size of in-group favoritism
  #sortingvector[c] <- abs(effects[[c]][2,1] )
  
  # sort by gap between in-group favoritism and out-group derogation
  # vec_in <- max(effects[[c]][2,1] , 0 )
  # vec_out <- min(effects[[c]][2,2] , 0 )
  # sortingvector[c] <- vec_in - abs(vec_out)
  
  # sort by size of affective polarization
  vec_in[c] <- max(effects[[c]][2,1] , 0 )
  vec_out[c] <- min(effects[[c]][2,2] , 0 )
  sortingvector[c] <- vec_in[c] - vec_out[c]
}
sortingvector <- rev(order(sortingvector))

axis(2, at = 1:25, labcountries[sortingvector], tick = F, las = 2, cex.axis = .9)
abline( h = 1:25, col = "grey")
abline( v = mean(vec_in), col = "grey")
abline( v = mean(vec_out), col = "grey", lty = 2)

for ( c in 1:25){
  sortingvector
  segments( effects[[sortingvector[c] ]][1,1] ,c+.15, effects[[sortingvector[c] ]][3,1], c+.15, lwd = 2)
  points( effects[[sortingvector[c] ]][2,1] ,c+.15, pch = 16)
  segments( effects[[sortingvector[c] ]][1,2] ,c-.15, effects[[sortingvector[c] ]][3,2], c-.15, lwd = 2)
  points( effects[[sortingvector[c] ]][2,2] ,c-.15, pch = 17)
}



text(.15, 0, labels = "In-group favoritism", adj = 0, cex = .8, pos = 4)
points(.15,0, pch = 16)
text(-1, 0, labels = "Out-group derogation", adj = 1, cex = .8, pos = 4)
points(-1,0, pch = 17)


plot(1,1,type = "n", xlim = c(-1,1), ylim = c(25,0), axes = F, xlab = "", ylab = "")
abline( h = 1:25, col = "grey")
mtext("Allocation of Tokens", side = 1, line = 2)
#mtext("Overall effect", side = 3, line = -2,at=par("usr")[1]+0.25*diff(par("usr")[1:2]))
mtext("Trust Game", side = 3, line = 0)
abline(v=0)
axis(1)

effects <- list()
for ( c in 1:25){
  m1d_c <- lmer(pl2gets ~ #diffreligion +
                  inoutpartisan +
                  clean_agedrawn + 
                  clean_sexdrawn + 
                  clean_classdrawn + 
                  clean_relidrawn + 
                  euattachmentshown + 
                  round + (1 | country_id),
                data = dffin[dffin$country == countries[c] & dffin$game == "trust" & dffin$nationality == "co-national" & dffin$nopid == 0,])
  
  
  
  set.seed(123)
  betas <- mvrnorm(1000, mu = m1d_c@beta, vcov(m1d_c))
  
  betas2 <- rbind(betas[,colnames(betas) %in% c("inoutpartisan2_partisan_copartisan","inoutpartisan1_partisan_outpartisan")]  )
  table(betas2[,1] > betas2[,2] * -1)
  effects[[c]] <- apply(betas2,2, quantile, c(0.025,.5,.975))
  
}

effs2 <- effects

for ( c in 1:25){
  vec_in[c] <- max(effects[[c]][2,1] , 0 )
  vec_out[c] <- min(effects[[c]][2,2] , 0 )
}
abline( v = mean(vec_in), col = "grey")
abline( v = mean(vec_out), col = "grey", lty = 2)

for ( c in 1:25){
  sortingvector
  segments( effects[[sortingvector[c] ]][1,1] ,c+.15, effects[[sortingvector[c] ]][3,1], c+.15, lwd = 2)
  points( effects[[sortingvector[c] ]][2,1] ,c+.15, pch = 16)
  segments( effects[[sortingvector[c] ]][1,2] ,c-.15, effects[[sortingvector[c] ]][3,2], c-.15, lwd = 2)
  points( effects[[sortingvector[c] ]][2,2] ,c-.15, pch = 17)
}

dev.off() 







# TABLE D3: IN-GROUP FAVORITISM AND OUT-GROUP DEROGATION BY COUNTRY ####

effects <- list()
for ( c in 1:25){
  m1d_c <- lmer(pl2gets ~ #diffreligion +
                  inoutpartisan +
                  clean_agedrawn + 
                  clean_sexdrawn + 
                  clean_classdrawn + 
                  clean_relidrawn + 
                  euattachmentshown + 
                  round + (1 | country_id),
                data = dffin[dffin$country == countries[c] & dffin$game == "dict" & dffin$nationality == "co-national" & dffin$nopid == 0,])
  
  
  
  set.seed(123)
  betas <- mvrnorm(1000, mu = m1d_c@beta, vcov(m1d_c))
  
  betas2 <- rbind(betas[,colnames(betas) %in% c("inoutpartisan2_partisan_copartisan","inoutpartisan1_partisan_outpartisan")]  )
  effects[[c]] <- data.frame(country=countries[c],
                             in_50=mean(betas2[,1]),
                             in_025=quantile(betas2[,1],.025),
                             in_975=quantile(betas2[,1],.975),
                             
                             out_50=mean(betas2[,2]),
                             out_025=quantile(betas2[,2],.025),
                             out_975=quantile(betas2[,2],.975),
                             
                             diff_50=mean(betas2[,1] - betas2[,2] * -1),
                             diff_025=quantile(betas2[,1] - betas2[,2] * -1,.025),
                             diff_975=quantile(betas2[,1] - betas2[,2] * -1,.975) )
  
}
effects <- do.call(rbind, effects)
effects[,2:ncol(effects)] <- round(effects[,2:ncol(effects)],2)
effects




effects <- list()
for ( c in 1:25){
  m1d_c <- lmer(pl2gets ~ #diffreligion +
                  inoutpartisan +
                  clean_agedrawn + 
                  clean_sexdrawn + 
                  clean_classdrawn + 
                  clean_relidrawn + 
                  euattachmentshown + 
                  round + (1 | country_id),
                data = dffin[dffin$country == countries[c] & dffin$game == "trust" & dffin$nationality == "co-national" & dffin$nopid == 0,])
  
  
  
  set.seed(123)
  betas <- mvrnorm(1000, mu = m1d_c@beta, vcov(m1d_c))
  
  betas2 <- rbind(betas[,colnames(betas) %in% c("inoutpartisan2_partisan_copartisan","inoutpartisan1_partisan_outpartisan")]  )
  effects[[c]] <- data.frame(country=countries[c],
                             in_50=mean(betas2[,1]),
                             in_025=quantile(betas2[,1],.025),
                             in_975=quantile(betas2[,1],.975),
                             
                             out_50=mean(betas2[,2]),
                             out_025=quantile(betas2[,2],.025),
                             out_975=quantile(betas2[,2],.975),
                             
                             diff_50=mean(betas2[,1] - betas2[,2] * -1),
                             diff_025=quantile(betas2[,1] - betas2[,2] * -1,.025),
                             diff_975=quantile(betas2[,1] - betas2[,2] * -1,.975) )
  
}
effects <- do.call(rbind, effects)
effects[,2:ncol(effects)] <- round(effects[,2:ncol(effects)],2)
effects


test1 <- NA
test2 <- NA
test3 <- NA
for ( i in 1:25){
  # in sig?
  test1[i] <- effs1[[i]][1,1] > 0
  # out sig?
  test2[i] <- effs1[[i]][3,2] < 0
  # in trumps out?
  test3[i] <- effs1[[i]][2,1] > max(0, effs1[[i]][2,2] * -1)
}
table(test1)
table(test2)
table(test3)



test1 <- NA
test2 <- NA
test3 <- NA
for ( i in 1:25){
  # in sig?
  test1[i] <- effs2[[i]][1,1] > 0
  # out sig?
  test2[i] <- effs2[[i]][3,2] < 0
  # in trumps out?
  test3[i] <- effs2[[i]][2,1] > max(0, effs2[[i]][2,2] * -1)
}
table(test1)
table(test2)
table(test3)



# TABLE 1 / FIGURE 6 / TABLE D4: COALITION EXPERIENCE ####



summary(maint1 <- lmer(pl2gets ~ 
                         coalitiongovsupporter
                       #+ drawnproeupartner
                       #+ I(respondentdistancetooutpartylr_avg-respondentdistancetoownpartylr)
                       + I(tier1_avemag - mean(dffin$tier1_avemag))
                       + I(eff_n_parties - mean(dffin$eff_n_parties))
                       + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                       #+ I(gini - mean(dffin$gini))
                       + I((gdppc - mean(dffin$gdppc)) / 1000 )
                       + I(population - mean(dffin$population))
                       + I(sex == "female") + age_continuous + edu 
                       + clean_agedrawn
                       + clean_sexdrawn
                       + clean_classdrawn
                       + clean_relidrawn
                       + euattachmentshown
                       + round
                       + (1 | country/country_id)
                       , data = dffin[dffin$nopid == 0 & 
                                        dffin$game == "dict" & 
                                        dffin$outpartisan2 == "2_partisan_copartisan" ,]))

summary(maint2 <- lmer(pl2gets ~ 
                         coalition_partners_now 
                       + I(tier1_avemag - mean(dffin$tier1_avemag))
                       + I(eff_n_parties - mean(dffin$eff_n_parties))
                       + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                       #+ I(gini - mean(dffin$gini))
                       + I((gdppc - mean(dffin$gdppc)) / 1000 )
                       + I(population - mean(dffin$population))
                       + I(sex == "female") + age_continuous + edu 
                       + clean_agedrawn
                       + clean_sexdrawn
                       + clean_classdrawn
                       + clean_relidrawn
                       + euattachmentshown
                       + round
                       + (1 | country/country_id)
                       , data = dffin[dffin$nopid == 0 & 
                                        dffin$game == "dict" & 
                                        dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

summary(maint3 <- lmer(pl2gets ~ 
                         coalitiongovsupporter
                       + I(tier1_avemag - mean(dffin$tier1_avemag))
                       + I(eff_n_parties - mean(dffin$eff_n_parties))
                       + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                       #+ I(gini - mean(dffin$gini))
                       + I((gdppc - mean(dffin$gdppc)) / 1000 )
                       + I(population - mean(dffin$population))
                       + I(sex == "female") + age_continuous + edu 
                       + clean_agedrawn
                       + clean_sexdrawn
                       + clean_classdrawn
                       + clean_relidrawn
                       + euattachmentshown
                       + round
                       + (1 | country/country_id)
                       , data = dffin[dffin$nopid == 0 & 
                                        dffin$game == "trust" & 
                                        dffin$outpartisan2 == "2_partisan_copartisan" ,]))


summary(maint4 <- lmer(pl2gets ~ 
                         coalition_partners_now 
                       + I(tier1_avemag - mean(dffin$tier1_avemag))
                       + I(eff_n_parties - mean(dffin$eff_n_parties))
                       + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                       #+ I(gini - mean(dffin$gini))
                       + I((gdppc - mean(dffin$gdppc)) / 1000 )
                       + I(population - mean(dffin$population))
                       + I(sex == "female") + age_continuous + edu 
                       + clean_agedrawn
                       + clean_sexdrawn
                       + clean_classdrawn
                       + clean_relidrawn
                       + euattachmentshown
                       + round
                       + (1 | country/country_id)
                       , data = dffin[dffin$nopid == 0 & 
                                        dffin$game == "trust" & 
                                        dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

stargazer::stargazer(maint1,maint2,maint3,maint4, single.row = T,
                     title = c("Hierarchical Linear Model - Current Coalition Partnership"),
                     dep.var.labels = "Tokens for Player 2",
                     column.labels = c("Co-Partisan","Out-Partisan",
                                       "Co-Partisan","Out-Partisan"),
                     covariate.labels = c("Coalition Partner",
                                          "Coalition Partner",
                                          #"Ideological Proximity",
                                          #"Left-Right Partisan-Ideological Alignment",
                                          "Average District Magnitude (logged)",
                                          "Effective Number of Parties",
                                          "Elite Polarization",
                                          #"Economic Inequality (Gini)",
                                          "Wealth (GDP per capita, in thousand Euros)",
                                          "Population size (in Million)",
                                          "Female",
                                          "Age",
                                          "Education",
                                          "Pl 2 Age: 30 (Reference: 18)",
                                          "Pl 2 Age: 42",
                                          "Pl 2 Age: 53",
                                          "Pl 2 Age: 65",
                                          "Pl 2 Gender: Male (Reference: Female)",
                                          "Pl 2 Class: Low (Reference: Middle)",
                                          "Pl 2 Class: High",
                                          "Pl 2 Religion: Catholic (Reference: Non-Religious)",
                                          "Pl 2 Religion: Protestant",
                                          "Pl 2 Religion: Muslim",
                                          "Subset Condition 1",
                                          "Round 2",
                                          "Round 3"
                     ),
                     add.lines = list(
                       
                       c("SD Respondent-level",
                         round(as.numeric(attributes(VarCorr(maint1)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maint2)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maint3)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maint4)$"country_id:country")$stddev), 3)),
                       c("SD Country-level",
                         round(as.numeric(attributes(VarCorr(maint1)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maint2)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maint3)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maint4)$"country")$stddev), 3)),
                       c("Conditional R-Squared",
                         round(get_gof(maint1)$r2.conditional, 3),
                         round(get_gof(maint2)$r2.conditional, 3),
                         round(get_gof(maint3)$r2.conditional, 3),
                         round(get_gof(maint4)$r2.conditional, 3)),
                       c("Respondents",
                         as.numeric(sapply(ranef(maint1),nrow)[1]),
                         as.numeric(sapply(ranef(maint2),nrow)[1]),
                         as.numeric(sapply(ranef(maint3),nrow)[1]),
                         as.numeric(sapply(ranef(maint4),nrow)[1])),
                       c("Countries",
                         as.numeric(sapply(ranef(maint1),nrow)[2]),
                         as.numeric(sapply(ranef(maint2),nrow)[2]),
                         as.numeric(sapply(ranef(maint3),nrow)[2]),
                         as.numeric(sapply(ranef(maint4),nrow)[2]))
                     ),
                     #digits = 2,
                     star.cutoffs = c(.1, .05, .01),
                     star.char = c("+", "*", "**"),
                     notes = c("+ p<0.1; * p<0.05; ** p<0.01") ,
                     notes.append=FALSE,
                     omit.stat = c("ll","aic","bic")
                     ,out = paste0(path, "/tab1_coal_now_full_v1.html")
                     )




#jpeg(paste0(path, "/fig6_mlm_coalcurrent_wieu_v1.jpeg"), units="in", width=12, height=4, res=200)
pdf(paste0(path, "/fig6_mlm_coalcurrent_wieu_v1.pdf"), width=12, height=4)
par(mfrow = c(1,4), mar = c(2,2,2,2), oma = c(2,2,2,2))
set.seed(123)
draws <- mvrnorm(10000, maint1@beta, vcov(maint1))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
tmp1 <- pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
tmp2 <- pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(0), labels = c("Coalition:"), cex.axis = 1.5, tick = F)
axis(1, at = c(1,2), labels = c("No", "Yes"), cex.axis = 1.5)
axis(2, las = 2, cex.axis = 1.5)
mtext("Tokens", side = 2, line = 2) 
mtext("Dictator Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

pred_change
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maint2@beta, vcov(maint2))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
tmp3 <- pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
tmp4 <- pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(0), labels = c("Coalition:"), cex.axis = 1.5, tick = F)
axis(1, at = c(1,2), labels = c("No", "Yes"), cex.axis = 1.5)
axis(2, las = 2, cex.axis = 1.5)
mtext("Tokens", side = 2, line = 2) 

mtext("Dictator Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maint3@beta, vcov(maint3))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(0), labels = c("Coalition:"), cex.axis = 1.5, tick = F)
axis(1, at = c(1,2), labels = c("No", "Yes"), cex.axis = 1.5)
axis(2, las = 2, cex.axis = 1.5)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maint4@beta, vcov(maint4))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              #median(dffin$cmp_dist_lr, na.rm= T) * -1,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(0), labels = c("Coalition:"), cex.axis = 1.5, tick = F)
axis(1, at = c(1,2), labels = c("No", "Yes"), cex.axis = 1.5)
axis(2, las = 2, cex.axis = 1.5)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])
dev.off()






# TABLE 2 / FIGURE 7 / TABLE D5: COALITION EXPERIENCE ####


summary(maint1 <- lmer(pl2gets ~ 
                         sd_years_in_coalition_since2000
                       + I(tier1_avemag - mean(dffin$tier1_avemag))
                       + I(eff_n_parties - mean(dffin$eff_n_parties))
                       + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                       #+ I(gini - mean(dffin$gini))
                       + I((gdppc - mean(dffin$gdppc)) / 1000 )
                       + I(population - mean(dffin$population))
                       + I(sex == "female") + age_continuous + edu 
                       + clean_agedrawn
                       + clean_sexdrawn
                       + clean_classdrawn
                       + clean_relidrawn
                       + euattachmentshown
                       + round
                       + (1 | country/country_id)
                       , data = dffin[dffin$nopid == 0 & 
                                        dffin$game == "dict" & 
                                        dffin$outpartisan2 == "2_partisan_copartisan" ,]))


summary(maint2 <- lmer(pl2gets ~ 
                         sd_years_wi_coalition_partner_since2000
                       + I(tier1_avemag - mean(dffin$tier1_avemag))
                       + I(eff_n_parties - mean(dffin$eff_n_parties))
                       + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                       #+ I(gini - mean(dffin$gini))
                       + I((gdppc - mean(dffin$gdppc)) / 1000 )
                       + I(population - mean(dffin$population))
                       + I(sex == "female") + age_continuous + edu 
                       + clean_agedrawn
                       + clean_sexdrawn
                       + clean_classdrawn
                       + clean_relidrawn
                       + euattachmentshown
                       + round
                       + (1 | country/country_id)
                       , data = dffin[dffin$nopid == 0 & 
                                        dffin$game == "dict" & 
                                        dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

summary(maint3 <- lmer(pl2gets ~ 
                         sd_years_in_coalition_since2000
                       + I(tier1_avemag - mean(dffin$tier1_avemag))
                       + I(eff_n_parties - mean(dffin$eff_n_parties))
                       + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                       #+ I(gini - mean(dffin$gini))
                       + I((gdppc - mean(dffin$gdppc)) / 1000 )
                       + I(population - mean(dffin$population))
                       + I(sex == "female") + age_continuous + edu 
                       + clean_agedrawn
                       + clean_sexdrawn
                       + clean_classdrawn
                       + clean_relidrawn
                       + euattachmentshown
                       + round
                       + (1 | country/country_id)
                       , data = dffin[dffin$nopid == 0 & 
                                        dffin$game == "trust" & 
                                        dffin$outpartisan2 == "2_partisan_copartisan" ,]))

summary(maint4 <- lmer(pl2gets ~ 
                         
                         sd_years_wi_coalition_partner_since2000
                       + I(tier1_avemag - mean(dffin$tier1_avemag))
                       + I(eff_n_parties - mean(dffin$eff_n_parties))
                       + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                       #+ I(gini - mean(dffin$gini))
                       + I((gdppc - mean(dffin$gdppc)) / 1000 )
                       + I(population - mean(dffin$population))
                       + I(sex == "female") + age_continuous + edu 
                       + clean_agedrawn
                       + clean_sexdrawn
                       + clean_classdrawn
                       + clean_relidrawn
                       + euattachmentshown
                       + round
                       + (1 | country/country_id)
                       , data = dffin[dffin$nopid == 0 & 
                                        dffin$game == "trust" & 
                                        dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

stargazer::stargazer(maint1,maint2,maint3,maint4, single.row = T,
                     title = c("Hierarchical Linear Model - Coalition Experience since 2000"),
                     dep.var.labels = "Tokens for Player 2",
                     column.labels = c("Co-Partisan","Out-Partisan",
                                       "Co-Partisan","Out-Partisan"),
                     covariate.labels = c("Coalition Experience",
                                          "Coalition Experience",
                                          #"Ideological Proximity",
                                          #"Left-Right Partisan-Ideological Alignment",
                                          "Average District Magnitude (logged)",
                                          "Effective Number of Parties",
                                          "Elite Polarization",
                                          #"Economic Inequality (Gini)",
                                          "Wealth (GDP per capita, in thousand Euros)",
                                          "Population size (in Million)",
                                          "Female",
                                          "Age",
                                          "Education",
                                          "Pl 2 Age: 30 (Reference: 18)",
                                          "Pl 2 Age: 42",
                                          "Pl 2 Age: 53",
                                          "Pl 2 Age: 65",
                                          "Pl 2 Gender: Male (Reference: Female)",
                                          "Pl 2 Class: Low (Reference: Middle)",
                                          "Pl 2 Class: High",
                                          "Pl 2 Religion: Catholic (Reference: Non-Religious)",
                                          "Pl 2 Religion: Protestant",
                                          "Pl 2 Religion: Muslim",
                                          "Subset Condition 1",
                                          "Round 2", "Round 3"),
                     add.lines = list(
                       
                       c("SD Respondent-level",
                         round(as.numeric(attributes(VarCorr(maint1)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maint2)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maint3)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maint4)$"country_id:country")$stddev), 3)),
                       c("SD Country-level",
                         round(as.numeric(attributes(VarCorr(maint1)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maint2)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maint3)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maint4)$"country")$stddev), 3)),
                       c("Conditional R-Squared",
                         round(get_gof(maint1)$r2.conditional, 3),
                         round(get_gof(maint2)$r2.conditional, 3),
                         round(get_gof(maint3)$r2.conditional, 3),
                         round(get_gof(maint4)$r2.conditional, 3)),
                       c("Respondents",
                         as.numeric(sapply(ranef(maint1),nrow)[1]),
                         as.numeric(sapply(ranef(maint2),nrow)[1]),
                         as.numeric(sapply(ranef(maint3),nrow)[1]),
                         as.numeric(sapply(ranef(maint4),nrow)[1])),
                       c("Countries",
                         as.numeric(sapply(ranef(maint1),nrow)[2]),
                         as.numeric(sapply(ranef(maint2),nrow)[2]),
                         as.numeric(sapply(ranef(maint3),nrow)[2]),
                         as.numeric(sapply(ranef(maint4),nrow)[2]))
                     ),
                     #digits = 2,
                     star.cutoffs = c(.1, .05, .01),
                     star.char = c("+", "*", "**"),
                     notes = c("+ p<0.1; * p<0.05; ** p<0.01") ,
                     notes.append=FALSE,
                     omit.stat = c("ll","aic","bic")
                     , out = paste0(path, "/tab2_coal_2000_full_v1.html")
                     )




#jpeg(paste0(path, "/fig7_mlm_coalsince2000_wieu_v1.jpeg"), units="in", width=12, height=4, res=200)
pdf(paste0(path, "/fig7_mlm_coalsince2000_wieu_v1.pdf"), width=12, height=4)
par(mfrow = c(1,4), mar = c(2,2,2,2), oma = c(2,2,2,2))
set.seed(123)
draws <- mvrnorm(10000, maint1@beta, vcov(maint1))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
tmp5 <- pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
tmp6 <- pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"), cex.axis = 1.5)
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2, cex.axis = 1.5)
mtext("Tokens", side = 2, line = 2) 
mtext("Dictator Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])



set.seed(123)
draws <- mvrnorm(10000, maint2@beta, vcov(maint2))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
tmp7 <- pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
tmp8 <- pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"), cex.axis = 1.5)
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2, cex.axis = 1.5)
mtext("Tokens", side = 2, line = 2) 

mtext("Dictator Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)


polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])




set.seed(123)
draws <- mvrnorm(10000, maint3@beta, vcov(maint3))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)

axis(1, at = c(1,100), labels = c("Low", "High"), cex.axis = 1.5)
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2, cex.axis = 1.5)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])




set.seed(123)
draws <- mvrnorm(10000, maint4@beta, vcov(maint4))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"), cex.axis = 1.5)
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2, cex.axis = 1.5)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])

dev.off()


# APPENDIX F1: current coalition plus ideology - cmp ####

summary(maintc1 <- lmer(pl2gets ~ 
                          coalitiongovsupporter
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))

summary(maintc2 <- lmer(pl2gets ~ 
                          coalition_partners_now
                        + I(cmp_dist_lr * -1)
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

summary(maintc3 <- lmer(pl2gets ~ 
                          coalitiongovsupporter
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))


summary(maintc4 <- lmer(pl2gets ~ 
                          coalition_partners_now 
                        + I(cmp_dist_lr * -1)
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))


jpeg(paste0(path, "/figF1_1_mlm_coalcurrent_wiideol_wieu_v1.jpeg"), units="in", width=12, height=4, res=200)
par(mfrow = c(1,4), mar = c(2,2,2,2), oma = c(2,2,2,2))
set.seed(123)
draws <- mvrnorm(10000, maintc1@beta, vcov(maintc1))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 
mtext("Dictator Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

pred_change
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maintc2@beta, vcov(maintc2))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$cmp_dist_lr, na.rm= T) * -1,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Dictator Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maintc3@beta, vcov(maintc3))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maintc4@beta, vcov(maintc4))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$cmp_dist_lr, na.rm= T) * -1,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])
dev.off()






# APPENDIX F1: coalition since 2000 plus ideology 1 (cmp) ####

summary(mainth1 <- lmer(pl2gets ~ 
                          sd_years_in_coalition_since2000
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))


summary(mainth2 <- lmer(pl2gets ~ 
                          sd_years_wi_coalition_partner_since2000
                        + I(avg_cmp_dist_lr_2000 * -1)
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

summary(mainth3 <- lmer(pl2gets ~ 
                          sd_years_in_coalition_since2000
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))

summary(mainth4 <- lmer(pl2gets ~ 
                          sd_years_wi_coalition_partner_since2000
                        + I(avg_cmp_dist_lr_2000 * -1)
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))


stargazer::stargazer(maintc1,maintc2,maintc3,maintc4, 
                     mainth1,mainth2,mainth3,mainth4,
                     single.row = T,
                     title = c("Hierarchical Linear Model - Coalition Partnership and Coalition Experience since 2000 plus Ideological Proximity"),
                     dep.var.labels = "Tokens for Player 2",
                     column.labels = c("Co-Partisan","Out-Partisan",
                                       "Co-Partisan","Out-Partisan",
                                       "Co-Partisan","Out-Partisan",
                                       "Co-Partisan","Out-Partisan"),
                     covariate.labels = c("Coalition Partner",
                                          "Coalition Partner",
                                          "Ideological Proximity",
                                          "Coalition Experience",
                                          "Coalition Experience",
                                          "Ideological Proximity",
                                          #"Left-Right Partisan-Ideological Alignment",
                                          "Average District Magnitude (logged)",
                                          "Effective Number of Parties",
                                          "Elite Polarization",
                                          #"Economic Inequality (Gini)",
                                          "Wealth (GDP per capita, in thousand Euros)",
                                          "Population size (in Million)",
                                          "Female",
                                          "Age",
                                          "Education",
                                          "Pl 2 Age: 30 (Reference: 18)",
                                          "Pl 2 Age: 42",
                                          "Pl 2 Age: 53",
                                          "Pl 2 Age: 65",
                                          "Pl 2 Gender: Male (Reference: Female)",
                                          "Pl 2 Class: Low (Reference: Middle)",
                                          "Pl 2 Class: High",
                                          "Pl 2 Religion: Catholic (Reference: Non-Religious)",
                                          "Pl 2 Religion: Protestant",
                                          "Pl 2 Religion: Muslim",
                                          "Subset Condition 1",
                                          "Round 2", "Round 3"),
                     add.lines = list(
                       
                       c("SD Respondent-level",
                         round(as.numeric(attributes(VarCorr(maintc1)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc2)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc3)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc4)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth1)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth2)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth3)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth4)$"country_id:country")$stddev), 3)),
                       c("SD Country-level",
                         round(as.numeric(attributes(VarCorr(maintc1)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc2)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc3)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc4)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth1)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth2)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth3)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth4)$"country")$stddev), 3)),
                       c("Conditional R-Squared",
                         round(get_gof(maintc1)$r2.conditional, 3),
                         round(get_gof(maintc2)$r2.conditional, 3),
                         round(get_gof(maintc3)$r2.conditional, 3),
                         round(get_gof(maintc4)$r2.conditional, 3),
                         round(get_gof(mainth1)$r2.conditional, 3),
                         round(get_gof(mainth2)$r2.conditional, 3),
                         round(get_gof(mainth3)$r2.conditional, 3),
                         round(get_gof(mainth4)$r2.conditional, 3)),
                       c("Respondents",
                         as.numeric(sapply(ranef(maintc1),nrow)[1]),
                         as.numeric(sapply(ranef(maintc2),nrow)[1]),
                         as.numeric(sapply(ranef(maintc3),nrow)[1]),
                         as.numeric(sapply(ranef(maintc4),nrow)[1]),
                         as.numeric(sapply(ranef(mainth1),nrow)[1]),
                         as.numeric(sapply(ranef(mainth2),nrow)[1]),
                         as.numeric(sapply(ranef(mainth3),nrow)[1]),
                         as.numeric(sapply(ranef(mainth4),nrow)[1])),
                       c("Countries",
                         as.numeric(sapply(ranef(maintc1),nrow)[2]),
                         as.numeric(sapply(ranef(maintc2),nrow)[2]),
                         as.numeric(sapply(ranef(maintc3),nrow)[2]),
                         as.numeric(sapply(ranef(maintc4),nrow)[2]),
                         as.numeric(sapply(ranef(mainth1),nrow)[2]),
                         as.numeric(sapply(ranef(mainth2),nrow)[2]),
                         as.numeric(sapply(ranef(mainth3),nrow)[2]),
                         as.numeric(sapply(ranef(mainth4),nrow)[2]))
                     ),
                     #digits = 2,
                     star.cutoffs = c(.1, .05, .01),
                     star.char = c("+", "*", "**"),
                     notes = c("+ p<0.1; * p<0.05; ** p<0.01") ,
                     notes.append=FALSE,
                     omit.stat = c("ll","aic","bic")
                     ,out = paste0(path, "/tabF1_coal_both_ideol_v1.html")
                     )





jpeg(paste0(path, "/figF1_2_mlm_coalsince2000_wiideol_wieu_v1.jpeg"), units="in", width=12, height=4, res=200)
par(mfrow = c(1,4), mar = c(2,2,2,2), oma = c(2,2,2,2))
set.seed(123)
draws <- mvrnorm(10000, mainth1@beta, vcov(mainth1))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 
mtext("Dictator Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])



set.seed(123)
draws <- mvrnorm(10000, mainth2@beta, vcov(mainth2))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$avg_cmp_dist_lr_2000, na.rm= T) * -1,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Dictator Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)


polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])




set.seed(123)
draws <- mvrnorm(10000, mainth3@beta, vcov(mainth3))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)

axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])




set.seed(123)
draws <- mvrnorm(10000, mainth4@beta, vcov(mainth4))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$avg_cmp_dist_lr_2000, na.rm= T) * -1,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])

dev.off()




# APPENDIX F2: current coalition with sorting ####

summary(maintc1 <- lmer(pl2gets ~ 
                          coalitiongovsupporter
                        + I(respondentdistancetooutpartylr_avg-respondentdistancetoownpartylr)
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))

summary(maintc2 <- lmer(pl2gets ~ 
                          coalition_partners_now 
                        + I(respondentdistancetooutpartylr_avg-respondentdistancetoownpartylr)
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

summary(maintc3 <- lmer(pl2gets ~ 
                          coalitiongovsupporter
                        + I(respondentdistancetooutpartylr_avg-respondentdistancetoownpartylr)
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))


summary(maintc4 <- lmer(pl2gets ~ 
                          coalition_partners_now 
                        + I(respondentdistancetooutpartylr_avg-respondentdistancetoownpartylr)
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))



jpeg(paste0(path, "/figF2_1_mlm_coalcurrent_wisorting_wieu_v1.jpeg"), units="in", width=12, height=4, res=200)

par(mfrow = c(1,4), mar = c(2,2,2,2), oma = c(2,2,2,2))
set.seed(123)
draws <- mvrnorm(10000, maintc1@beta, vcov(maintc1))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$respondentdistancetooutpartylr_avg - dffin$respondentdistancetoownpartylr, na.rm= T),
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 
mtext("Dictator Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

pred_change
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maintc2@beta, vcov(maintc2))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$respondentdistancetooutpartylr_avg - dffin$respondentdistancetoownpartylr, na.rm= T),
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Dictator Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maintc3@beta, vcov(maintc3))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$respondentdistancetooutpartylr_avg - dffin$respondentdistancetoownpartylr, na.rm= T),
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maintc4@beta, vcov(maintc4))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$respondentdistancetooutpartylr_avg - dffin$respondentdistancetoownpartylr, na.rm= T),
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])
dev.off()



# APPENDIX F2: coalition since 2000 with sorting ####

summary(mainth1 <- lmer(pl2gets ~ 
                          sd_years_in_coalition_since2000
                        + I(respondentdistancetooutpartylr_avg-respondentdistancetoownpartylr)
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))


summary(mainth2 <- lmer(pl2gets ~ 
                          sd_years_wi_coalition_partner_since2000
                        + I(respondentdistancetooutpartylr_avg-respondentdistancetoownpartylr)
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

summary(mainth3 <- lmer(pl2gets ~ 
                          sd_years_in_coalition_since2000
                        + I(respondentdistancetooutpartylr_avg-respondentdistancetoownpartylr)
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))

summary(mainth4 <- lmer(pl2gets ~ 
                          sd_years_wi_coalition_partner_since2000
                        + I(respondentdistancetooutpartylr_avg-respondentdistancetoownpartylr)
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

stargazer::stargazer(maintc1,maintc2,maintc3,maintc4, 
                     mainth1,mainth2,mainth3,mainth4, single.row = T,
                     title = c("Hierarchical Linear Model - Coalition Experience and Coalition Experience since 2000"),
                     dep.var.labels = "Tokens for Player 2",
                     column.labels = c("Co-Partisan","Out-Partisan",
                                       "Co-Partisan","Out-Partisan"),
                     covariate.labels = c("Coalition Partner",
                                          "Coalition Partner",
                                          "Coalition Experience",
                                          "Coalition Experience",
                                          #"Ideological Proximity",
                                          "Partisan-Ideological Alignment",
                                          "Average District Magnitude (logged)",
                                          "Effective Number of Parties",
                                          "Elite Polarization",
                                          #"Economic Inequality (Gini)",
                                          "Wealth (GDP per capita, in thousand Euros)",
                                          "Population size (in Million)",
                                          "Female",
                                          "Age",
                                          "Education",
                                          "Pl 2 Age: 30 (Reference: 18)",
                                          "Pl 2 Age: 42",
                                          "Pl 2 Age: 53",
                                          "Pl 2 Age: 65",
                                          "Pl 2 Gender: Male (Reference: Female)",
                                          "Pl 2 Class: Low (Reference: Middle)",
                                          "Pl 2 Class: High",
                                          "Pl 2 Religion: Catholic (Reference: Non-Religious)",
                                          "Pl 2 Religion: Protestant",
                                          "Pl 2 Religion: Muslim",
                                          "Subset Condition 1",
                                          "Round 2", "Round 3"),
                     add.lines = list(
                       
                       c("SD Respondent-level",
                         round(as.numeric(attributes(VarCorr(maintc1)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc2)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc3)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc4)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth1)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth2)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth3)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth4)$"country_id:country")$stddev), 3)),
                       c("SD Country-level",
                         round(as.numeric(attributes(VarCorr(maintc1)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc2)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc3)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc4)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth1)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth2)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth3)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth4)$"country")$stddev), 3)),
                       c("Conditional R-Squared",
                         round(get_gof(maintc1)$r2.conditional, 3),
                         round(get_gof(maintc2)$r2.conditional, 3),
                         round(get_gof(maintc3)$r2.conditional, 3),
                         round(get_gof(maintc4)$r2.conditional, 3),
                         round(get_gof(mainth1)$r2.conditional, 3),
                         round(get_gof(mainth2)$r2.conditional, 3),
                         round(get_gof(mainth3)$r2.conditional, 3),
                         round(get_gof(mainth4)$r2.conditional, 3)),
                       c("Respondents",
                         as.numeric(sapply(ranef(maintc1),nrow)[1]),
                         as.numeric(sapply(ranef(maintc2),nrow)[1]),
                         as.numeric(sapply(ranef(maintc3),nrow)[1]),
                         as.numeric(sapply(ranef(maintc4),nrow)[1]),
                         as.numeric(sapply(ranef(mainth1),nrow)[1]),
                         as.numeric(sapply(ranef(mainth2),nrow)[1]),
                         as.numeric(sapply(ranef(mainth3),nrow)[1]),
                         as.numeric(sapply(ranef(mainth4),nrow)[1])),
                       c("Countries",
                         as.numeric(sapply(ranef(maintc1),nrow)[2]),
                         as.numeric(sapply(ranef(maintc2),nrow)[2]),
                         as.numeric(sapply(ranef(maintc3),nrow)[2]),
                         as.numeric(sapply(ranef(maintc4),nrow)[2]),
                         as.numeric(sapply(ranef(mainth1),nrow)[2]),
                         as.numeric(sapply(ranef(mainth2),nrow)[2]),
                         as.numeric(sapply(ranef(mainth3),nrow)[2]),
                         as.numeric(sapply(ranef(mainth4),nrow)[2]))
                     ),
                     #digits = 2,
                     star.cutoffs = c(.1, .05, .01),
                     star.char = c("+", "*", "**"),
                     notes = c("+ p<0.1; * p<0.05; ** p<0.01") ,
                     notes.append=FALSE,
                     omit.stat = c("ll","aic","bic")
                     ,out = paste0(path, "/tabF2_coal_both_sort_v1.html")
                     )




jpeg(paste0(path, "/figF2_2_mlm_coalsince2000_wisorting_wieu_v1.jpeg"), units="in", width=12, height=4, res=200)
par(mfrow = c(1,4), mar = c(2,2,2,2), oma = c(2,2,2,2))
set.seed(123)
draws <- mvrnorm(10000, mainth1@beta, vcov(mainth1))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$respondentdistancetooutpartylr_avg - dffin$respondentdistancetoownpartylr, na.rm= T),
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 
mtext("Dictator Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])



set.seed(123)
draws <- mvrnorm(10000, mainth2@beta, vcov(mainth2))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$respondentdistancetooutpartylr_avg - dffin$respondentdistancetoownpartylr, na.rm= T),
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Dictator Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)


polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])




set.seed(123)
draws <- mvrnorm(10000, mainth3@beta, vcov(mainth3))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$respondentdistancetooutpartylr_avg - dffin$respondentdistancetoownpartylr, na.rm= T),
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)

axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])




set.seed(123)
draws <- mvrnorm(10000, mainth4@beta, vcov(mainth4))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$respondentdistancetooutpartylr_avg - dffin$respondentdistancetoownpartylr, na.rm= T),
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])

dev.off()







# APPENDIX F3: current coalition with bipolarity ####

summary(maintc1 <- lmer(pl2gets ~ 
                          coalitiongovsupporter
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + current_npi
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))

summary(maintc2 <- lmer(pl2gets ~ 
                          coalition_partners_now 
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + current_npi
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

summary(maintc3 <- lmer(pl2gets ~ 
                          coalitiongovsupporter
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + current_npi
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))


summary(maintc4 <- lmer(pl2gets ~ 
                          coalition_partners_now 
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + current_npi
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))


jpeg(paste0(path, "/figF3_1_mlm_coalcurrent_wibipolarity_wieu_v1.jpeg"), units="in", width=12, height=4, res=200)

par(mfrow = c(1,4), mar = c(2,2,2,2), oma = c(2,2,2,2))
set.seed(123)
draws <- mvrnorm(10000, maintc1@beta, vcov(maintc1))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$current_npi, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 
mtext("Dictator Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

pred_change
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maintc2@beta, vcov(maintc2))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$current_npi, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Dictator Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maintc3@beta, vcov(maintc3))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$current_npi, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maintc4@beta, vcov(maintc4))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$current_npi, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])
dev.off()


# APPENDIX F3: coalition since 2000 with bipolarity ####

summary(mainth1 <- lmer(pl2gets ~ 
                          sd_years_in_coalition_since2000
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + current_npi
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        #+ I(gini - mean(dffin$gini))  
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))


summary(mainth2 <- lmer(pl2gets ~ 
                          sd_years_wi_coalition_partner_since2000
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + current_npi
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

summary(mainth3 <- lmer(pl2gets ~ 
                          sd_years_in_coalition_since2000
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + current_npi
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))

summary(mainth4 <- lmer(pl2gets ~ 
                          sd_years_wi_coalition_partner_since2000
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + current_npi
                        #+ I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

stargazer::stargazer(maintc1,maintc2,maintc3,maintc4,
                     mainth1,mainth2,mainth3,mainth4,single.row = T,
                     title = c("Hierarchical Linear Model - Coalition Experience since 2000"),
                     dep.var.labels = "Tokens for Player 2",
                     column.labels = c("Co-Partisan","Out-Partisan",
                                       "Co-Partisan","Out-Partisan"),
                     covariate.labels = c("Coalition Partner",
                                          "Coalition Partner",
                                          "Coalition Experience",
                                          "Coalition Experience",
                                          #"Ideological Proximity",
                                          "Average District Magnitude (logged)",
                                          "Effective Number of Parties",
                                          "Elite Polarization",
                                          #"Economic Inequality (Gini)",
                                          "Wealth (GDP per capita, in thousand Euros)",
                                          "Population size (in Million)",
                                          "Female",
                                          "Age",
                                          "Education",
                                          "Pl 2 Age: 30 (Reference: 18)",
                                          "Pl 2 Age: 42",
                                          "Pl 2 Age: 53",
                                          "Pl 2 Age: 65",
                                          "Pl 2 Gender: Male (Reference: Female)",
                                          "Pl 2 Class: Low (Reference: Middle)",
                                          "Pl 2 Class: High",
                                          "Pl 2 Religion: Catholic (Reference: Non-Religious)",
                                          "Pl 2 Religion: Protestant",
                                          "Pl 2 Religion: Muslim",
                                          "Subset Condition 1",
                                          "Round 2", "Round 3"),
                     add.lines = list(
                       
                       c("SD Respondent-level",
                         round(as.numeric(attributes(VarCorr(maintc1)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc2)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc3)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc4)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth1)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth2)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth3)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth4)$"country_id:country")$stddev), 3)),
                       c("SD Country-level",
                         round(as.numeric(attributes(VarCorr(maintc1)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc2)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc3)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc4)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth1)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth2)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth3)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth4)$"country")$stddev), 3)),
                       c("Conditional R-Squared",
                         round(get_gof(maintc1)$r2.conditional, 3),
                         round(get_gof(maintc2)$r2.conditional, 3),
                         round(get_gof(maintc3)$r2.conditional, 3),
                         round(get_gof(maintc4)$r2.conditional, 3),
                         round(get_gof(mainth1)$r2.conditional, 3),
                         round(get_gof(mainth2)$r2.conditional, 3),
                         round(get_gof(mainth3)$r2.conditional, 3),
                         round(get_gof(mainth4)$r2.conditional, 3)),
                       c("Respondents",
                         as.numeric(sapply(ranef(maintc1),nrow)[1]),
                         as.numeric(sapply(ranef(maintc2),nrow)[1]),
                         as.numeric(sapply(ranef(maintc3),nrow)[1]),
                         as.numeric(sapply(ranef(maintc4),nrow)[1]),
                         as.numeric(sapply(ranef(mainth1),nrow)[1]),
                         as.numeric(sapply(ranef(mainth2),nrow)[1]),
                         as.numeric(sapply(ranef(mainth3),nrow)[1]),
                         as.numeric(sapply(ranef(mainth4),nrow)[1])),
                       c("Countries",
                         as.numeric(sapply(ranef(maintc1),nrow)[2]),
                         as.numeric(sapply(ranef(maintc2),nrow)[2]),
                         as.numeric(sapply(ranef(maintc3),nrow)[2]),
                         as.numeric(sapply(ranef(maintc4),nrow)[2]),
                         as.numeric(sapply(ranef(mainth1),nrow)[2]),
                         as.numeric(sapply(ranef(mainth2),nrow)[2]),
                         as.numeric(sapply(ranef(mainth3),nrow)[2]),
                         as.numeric(sapply(ranef(mainth4),nrow)[2]))
                     ),
                     #digits = 2,
                     star.cutoffs = c(.1, .05, .01),
                     star.char = c("+", "*", "**"),
                     notes = c("+ p<0.1; * p<0.05; ** p<0.01") ,
                     notes.append=FALSE,
                     omit.stat = c("ll","aic","bic")
                     , out = paste0(path, "/tabF3_coal_both_bipolar_v1.html")
                     )



jpeg(paste0(path, "/figF3_2_mlm_coalsince2000_wibipolarity_wieu_v1.jpeg"), units="in", width=12, height=4, res=200)
par(mfrow = c(1,4), mar = c(2,2,2,2), oma = c(2,2,2,2))
set.seed(123)
draws <- mvrnorm(10000, mainth1@beta, vcov(mainth1))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$current_npi, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 
mtext("Dictator Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])



set.seed(123)
draws <- mvrnorm(10000, mainth2@beta, vcov(mainth2))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$current_npi, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Dictator Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)


polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])




set.seed(123)
draws <- mvrnorm(10000, mainth3@beta, vcov(mainth3))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$current_npi, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)

axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])




set.seed(123)
draws <- mvrnorm(10000, mainth4@beta, vcov(mainth4))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$current_npi, na.rm= T),
              #median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])

dev.off()




# APPENDIX F4: current coalition with gini ####

summary(maintc1 <- lmer(pl2gets ~ 
                          coalitiongovsupporter
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        + I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))

summary(maintc2 <- lmer(pl2gets ~ 
                          coalition_partners_now 
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        + I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

summary(maintc3 <- lmer(pl2gets ~ 
                          coalitiongovsupporter
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        + I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))


summary(maintc4 <- lmer(pl2gets ~ 
                          coalition_partners_now 
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        + I(gini - mean(dffin$gini))
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))





jpeg(paste0(path, "/figF4_1_mlm_coalcurrent_wigini_wieu_v1.jpeg"), units="in", width=12, height=4, res=200)

par(mfrow = c(1,4), mar = c(2,2,2,2), oma = c(2,2,2,2))
set.seed(123)
draws <- mvrnorm(10000, maintc1@beta, vcov(maintc1))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 
mtext("Dictator Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

pred_change
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maintc2@beta, vcov(maintc2))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Dictator Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maintc3@beta, vcov(maintc3))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maintc4@beta, vcov(maintc4))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])
dev.off()






# APPENDIX F4: coalition since 2000 with gini ####

summary(mainth1 <- lmer(pl2gets ~ 
                          sd_years_in_coalition_since2000
                        + I(tier1_avemag - mean(dffin$tier1_avemag))  
                        + I(eff_n_parties - mean(dffin$eff_n_parties))  
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ current_npi
                        + I(population - mean(dffin$population))  
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )    
                        + I(gini - mean(dffin$gini))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))


summary(mainth2 <- lmer(pl2gets ~ 
                          sd_years_wi_coalition_partner_since2000
                        + I(tier1_avemag - mean(dffin$tier1_avemag))  
                        + I(eff_n_parties - mean(dffin$eff_n_parties))  
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ current_npi
                        + I(population - mean(dffin$population))  
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )    
                        + I(gini - mean(dffin$gini))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

summary(mainth3 <- lmer(pl2gets ~ 
                          sd_years_in_coalition_since2000
                        + I(tier1_avemag - mean(dffin$tier1_avemag))  
                        + I(eff_n_parties - mean(dffin$eff_n_parties))  
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ current_npi
                        + I(population - mean(dffin$population))  
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )    
                        + I(gini - mean(dffin$gini))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))

summary(mainth4 <- lmer(pl2gets ~ 
                          sd_years_wi_coalition_partner_since2000
                        #+ I(respondentdistancetooutpartylr_avg-respondentdistancetoownpartylr)
                        + I(tier1_avemag - mean(dffin$tier1_avemag))  
                        + I(eff_n_parties - mean(dffin$eff_n_parties))  
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        #+ current_npi
                        + I(population - mean(dffin$population))  
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )    
                        + I(gini - mean(dffin$gini))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

stargazer::stargazer(maintc1,maintc2,maintc3,maintc4,
                     mainth1,mainth2,mainth3,mainth4,single.row = T,
                     title = c("Hierarchical Linear Model - Coalition Experience since 2000"),
                     dep.var.labels = "Tokens for Player 2",
                     column.labels = c("Co-Partisan","Out-Partisan",
                                       "Co-Partisan","Out-Partisan"),
                     covariate.labels = c("Coalition Partner",
                                          "Coalition Partner",
                                          "Coalition Experience",
                                          "Coalition Experience",
                                          #"Ideological Proximity",
                                          "Average District Magnitude (logged)",
                                          "Effective Number of Parties",
                                          "Elite Polarization",
                                          "Economic Inequality (Gini)",
                                          "Wealth (GDP per capita, in thousand Euros)",
                                          "Population size (in Million)",
                                          "Female",
                                          "Age",
                                          "Education",
                                          "Pl 2 Age: 30 (Reference: 18)",
                                          "Pl 2 Age: 42",
                                          "Pl 2 Age: 53",
                                          "Pl 2 Age: 65",
                                          "Pl 2 Gender: Male (Reference: Female)",
                                          "Pl 2 Class: Low (Reference: Middle)",
                                          "Pl 2 Class: High",
                                          "Pl 2 Religion: Catholic (Reference: Non-Religious)",
                                          "Pl 2 Religion: Protestant",
                                          "Pl 2 Religion: Muslim",
                                          "Subset Condition 1",
                                          "Round 2", "Round 3"),
                     add.lines = list(
                       
                       c("SD Respondent-level",
                         round(as.numeric(attributes(VarCorr(maintc1)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc2)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc3)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc4)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth1)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth2)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth3)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth4)$"country_id:country")$stddev), 3)),
                       c("SD Country-level",
                         round(as.numeric(attributes(VarCorr(maintc1)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc2)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc3)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc4)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth1)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth2)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth3)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth4)$"country")$stddev), 3)),
                       c("Conditional R-Squared",
                         round(get_gof(maintc1)$r2.conditional, 3),
                         round(get_gof(maintc2)$r2.conditional, 3),
                         round(get_gof(maintc3)$r2.conditional, 3),
                         round(get_gof(maintc4)$r2.conditional, 3),
                         round(get_gof(mainth1)$r2.conditional, 3),
                         round(get_gof(mainth2)$r2.conditional, 3),
                         round(get_gof(mainth3)$r2.conditional, 3),
                         round(get_gof(mainth4)$r2.conditional, 3)),
                       c("Respondents",
                         as.numeric(sapply(ranef(maintc1),nrow)[1]),
                         as.numeric(sapply(ranef(maintc2),nrow)[1]),
                         as.numeric(sapply(ranef(maintc3),nrow)[1]),
                         as.numeric(sapply(ranef(maintc4),nrow)[1]),
                         as.numeric(sapply(ranef(mainth1),nrow)[1]),
                         as.numeric(sapply(ranef(mainth2),nrow)[1]),
                         as.numeric(sapply(ranef(mainth3),nrow)[1]),
                         as.numeric(sapply(ranef(mainth4),nrow)[1])),
                       c("Countries",
                         as.numeric(sapply(ranef(maintc1),nrow)[2]),
                         as.numeric(sapply(ranef(maintc2),nrow)[2]),
                         as.numeric(sapply(ranef(maintc3),nrow)[2]),
                         as.numeric(sapply(ranef(maintc4),nrow)[2]),
                         as.numeric(sapply(ranef(mainth1),nrow)[2]),
                         as.numeric(sapply(ranef(mainth2),nrow)[2]),
                         as.numeric(sapply(ranef(mainth3),nrow)[2]),
                         as.numeric(sapply(ranef(mainth4),nrow)[2]))
                     ),
                     #digits = 2,
                     star.cutoffs = c(.1, .05, .01),
                     star.char = c("+", "*", "**"),
                     notes = c("+ p<0.1; * p<0.05; ** p<0.01") ,
                     notes.append=FALSE,
                     omit.stat = c("ll","aic","bic")
                     , out = paste0(path, "/tabF4_coal_both_gini_v1.html")
                     )



jpeg(paste0(path, "/figF4_2_mlm_coalsince2000_wigini_wieu_v1.jpeg"), units="in", width=12, height=4, res=200)
par(mfrow = c(1,4), mar = c(2,2,2,2), oma = c(2,2,2,2))
set.seed(123)
draws <- mvrnorm(10000, mainth1@beta, vcov(mainth1))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 
mtext("Dictator Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])



set.seed(123)
draws <- mvrnorm(10000, mainth2@beta, vcov(mainth2))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Dictator Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)


polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])




set.seed(123)
draws <- mvrnorm(10000, mainth3@beta, vcov(mainth3))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)

axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])




set.seed(123)
draws <- mvrnorm(10000, mainth4@beta, vcov(mainth4))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              median(dffin$gini, na.rm= T),
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])

dev.off()




# APPENDIX F5: coalition with immigration stats 2018 (eurostat) ####

summary(maintc1 <- lmer(pl2gets ~ 
                          coalitiongovsupporter
                        #+ drawnproeupartner
                        #+ I(respondentdistancetooutpartylr_avg-respondentdistancetoownpartylr)
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        + num_immigrants_2018_01
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))

summary(maintc2 <- lmer(pl2gets ~ 
                          coalition_partners_now 
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        + num_immigrants_2018_01
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

summary(maintc3 <- lmer(pl2gets ~ 
                          coalitiongovsupporter
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        + num_immigrants_2018_01
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))


summary(maintc4 <- lmer(pl2gets ~ 
                          coalition_partners_now 
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        + num_immigrants_2018_01
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))



summary(mainth1 <- lmer(pl2gets ~ 
                          sd_years_in_coalition_since2000
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        + num_immigrants_2018_01
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))


summary(mainth2 <- lmer(pl2gets ~ 
                          sd_years_wi_coalition_partner_since2000
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        + num_immigrants_2018_01
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "dict" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

summary(mainth3 <- lmer(pl2gets ~ 
                          sd_years_in_coalition_since2000
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        + num_immigrants_2018_01
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "2_partisan_copartisan" ,]))

summary(mainth4 <- lmer(pl2gets ~ 
                          
                          sd_years_wi_coalition_partner_since2000
                        + I(tier1_avemag - mean(dffin$tier1_avemag))
                        + I(eff_n_parties - mean(dffin$eff_n_parties))
                        + I(dalton_polarization_lr - mean(dffin$dalton_polarization_lr))
                        + num_immigrants_2018_01
                        + I((gdppc - mean(dffin$gdppc)) / 1000 )
                        + I(population - mean(dffin$population))
                        + I(sex == "female") + age_continuous + edu 
                        + clean_agedrawn
                        + clean_sexdrawn
                        + clean_classdrawn
                        + clean_relidrawn
                        + euattachmentshown
                        + round
                        + (1 | country/country_id)
                        , data = dffin[dffin$nopid == 0 & 
                                         dffin$game == "trust" & 
                                         dffin$outpartisan2 == "1_partisan_outpartisan" ,]))

stargazer::stargazer(maintc1,maintc2,maintc3,maintc4,
                     mainth1,mainth2,mainth3,mainth4,single.row = T,
                     title = c("Hierarchical Linear Model - Coalition Experience since 2000"),
                     dep.var.labels = "Tokens for Player 2",
                     column.labels = c("Co-Partisan","Out-Partisan",
                                       "Co-Partisan","Out-Partisan"),
                     covariate.labels = c("Coalition Partner",
                                          "Coalition Partner",
                                          "Coalition Experience",
                                          "Coalition Experience",
                                          #"Ideological Proximity",
                                          #"Left-Right Partisan-Ideological Alignment",
                                          "Average District Magnitude (logged)",
                                          "Effective Number of Parties",
                                          "Elite Polarization",
                                          #"Economic Inequality (Gini)",
                                          "Wealth (GDP per capita, in thousand Euros)",
                                          "Population size (in Million)",
                                          "Immigration Inflow",
                                          "Female",
                                          "Age",
                                          "Education",
                                          "Pl 2 Age: 30 (Reference: 18)",
                                          "Pl 2 Age: 42",
                                          "Pl 2 Age: 53",
                                          "Pl 2 Age: 65",
                                          "Pl 2 Gender: Male (Reference: Female)",
                                          "Pl 2 Class: Low (Reference: Middle)",
                                          "Pl 2 Class: High",
                                          "Pl 2 Religion: Catholic (Reference: Non-Religious)",
                                          "Pl 2 Religion: Protestant",
                                          "Pl 2 Religion: Muslim",
                                          "Subset Condition 1",
                                          "Round 2", "Round 3"),
                     add.lines = list(
                       
                       c("SD Respondent-level",
                         round(as.numeric(attributes(VarCorr(maintc1)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc2)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc3)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc4)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth1)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth2)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth3)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth4)$"country_id:country")$stddev), 3)),
                       c("SD Country-level",
                         round(as.numeric(attributes(VarCorr(maintc1)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc2)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc3)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(maintc4)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth1)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth2)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth3)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(mainth4)$"country")$stddev), 3)),
                       c("Conditional R-Squared",
                         round(get_gof(maintc1)$r2.conditional, 3),
                         round(get_gof(maintc2)$r2.conditional, 3),
                         round(get_gof(maintc3)$r2.conditional, 3),
                         round(get_gof(maintc4)$r2.conditional, 3),
                         round(get_gof(mainth1)$r2.conditional, 3),
                         round(get_gof(mainth2)$r2.conditional, 3),
                         round(get_gof(mainth3)$r2.conditional, 3),
                         round(get_gof(mainth4)$r2.conditional, 3)),
                       c("Respondents",
                         as.numeric(sapply(ranef(maintc1),nrow)[1]),
                         as.numeric(sapply(ranef(maintc2),nrow)[1]),
                         as.numeric(sapply(ranef(maintc3),nrow)[1]),
                         as.numeric(sapply(ranef(maintc4),nrow)[1]),
                         as.numeric(sapply(ranef(mainth1),nrow)[1]),
                         as.numeric(sapply(ranef(mainth2),nrow)[1]),
                         as.numeric(sapply(ranef(mainth3),nrow)[1]),
                         as.numeric(sapply(ranef(mainth4),nrow)[1])),
                       c("Countries",
                         as.numeric(sapply(ranef(maintc1),nrow)[2]),
                         as.numeric(sapply(ranef(maintc2),nrow)[2]),
                         as.numeric(sapply(ranef(maintc3),nrow)[2]),
                         as.numeric(sapply(ranef(maintc4),nrow)[2]),
                         as.numeric(sapply(ranef(mainth1),nrow)[2]),
                         as.numeric(sapply(ranef(mainth2),nrow)[2]),
                         as.numeric(sapply(ranef(mainth3),nrow)[2]),
                         as.numeric(sapply(ranef(mainth4),nrow)[2]))
                     ),
                     #digits = 2,
                     star.cutoffs = c(.1, .05, .01),
                     star.char = c("+", "*", "**"),
                     notes = c("+ p<0.1; * p<0.05; ** p<0.01") ,
                     notes.append=FALSE,
                     omit.stat = c("ll","aic","bic")
                     ,out = paste0(path, "/tabF5_coal_both_immigration_v1.html")
                     )





jpeg(paste0(path, "/figF5_1_coal_now_immigration_v1.jpeg"), units="in", width=12, height=4, res=200)

par(mfrow = c(1,4), mar = c(2,2,2,2), oma = c(2,2,2,2))
set.seed(123)
draws <- mvrnorm(10000, maintc1@beta, vcov(maintc1))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              median(dffin$num_immigrants_2018_01), # immigration
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 
mtext("Dictator Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

pred_change
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maintc2@beta, vcov(maintc2))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              median(dffin$num_immigrants_2018_01), # immigration
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Dictator Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maintc3@beta, vcov(maintc3))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              median(dffin$num_immigrants_2018_01), # immigration
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])



set.seed(123)
draws <- mvrnorm(10000, maintc4@beta, vcov(maintc4))

coal <- c(0,1)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              median(dffin$num_immigrants_2018_01), # immigration
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,2] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,3), ylim = c(2,6), axes = F)
axis(1, at = c(1,2), labels = c("Coalition: N", "Coalition: Y"))
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
points(1, pred[3,1], pch = 16)
segments(1, pred[1,1], 1, pred[5,1])

points(2, pred[3,2], pch = 16)
segments(2, pred[1,2], 2, pred[5,2])
dev.off()




jpeg(paste0(path, "/figF5_2_coal_2000_immigration.jpeg"), units="in", width=12, height=4, res=200)
par(mfrow = c(1,4), mar = c(2,2,2,2), oma = c(2,2,2,2))
set.seed(123)
draws <- mvrnorm(10000, mainth1@beta, vcov(mainth1))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              median(dffin$num_immigrants_2018_01), # immigration
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 
mtext("Dictator Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)

polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])



set.seed(123)
draws <- mvrnorm(10000, mainth2@beta, vcov(mainth2))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              median(dffin$num_immigrants_2018_01), # immigration
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Dictator Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)


polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])




set.seed(123)
draws <- mvrnorm(10000, mainth3@beta, vcov(mainth3))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              median(dffin$num_immigrants_2018_01), # immigration
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)

axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Co-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])




set.seed(123)
draws <- mvrnorm(10000, mainth4@beta, vcov(mainth4))

coal <- seq(min(dffin$sd_years_in_coalition_since2000, na.rm= T),
            max(dffin$sd_years_in_coalition_since2000, na.rm= T),
            length.out = 100)
scen <- cbind(1,
              coal,
              median(dffin$tier1_avemag, na.rm= T),
              median(dffin$eff_n_parties, na.rm= T),
              median(dffin$dalton_polarization_lr, na.rm= T),
              median(dffin$num_immigrants_2018_01), # immigration
              median(dffin$gdppc, na.rm= T)/1000,
              median(dffin$population, na.rm= T),
              1, # female
              median(dffin$age_continuous, na.rm= T),
              median(dffin$edu, na.rm= T),
              1,0,0,0, # 30 year old,
              0, # female
              0,0, # middle class
              0,0,0, #atheist
              0,0,0 # neither anti nor pro EUrope
)

xb <- draws %*% t(scen)
pred <- apply(xb, 2, quantile, c(.025,.05,.5,.95,.975))
pred_change <- quantile( xb[,100] - xb[,1], c(.025,.05,.5,.95,.975))

plot(1,1,type = "n", xlim = c(0,100), ylim = c(2,6), axes = F)
axis(1, at = c(1,100), labels = c("Low", "High"))
mtext("Coalition Experience", side = 1, line = 1)
axis(2, las = 2)
mtext("Tokens", side = 2, line = 2) 

mtext("Trust Game: Out-Partisan", side = 3, line = 2)
mtext(paste0("Coalition Effect: ", round(pred_change[3],2),
             " (", round(pred_change[1],2), ", ", round(pred_change[5],2), ")"), side = 3, line = 0)
polygon(c(100:1, 1:100),
        c(rev(pred[1,]),pred[5,]), 
        col = "grey", border = F)
lines(1:100, pred[3,])

dev.off()


# APPENDIX E1: Societal divides ####

dffin$christian_muslim_divide <- NA
dffin$christian_muslim_divide[dffin$rel_belief %in% c("protstnt","catholic") & (dffin$pl2cathlic == 1 | dffin$pl2prtstnt)] <- 0
dffin$christian_muslim_divide[dffin$rel_belief %in% c("muslim") & (dffin$pl2muslimc == 1 )] <- 0
dffin$christian_muslim_divide[dffin$rel_belief %in% c("protstnt","catholic") & (dffin$pl2muslimc == 1 )] <- 1
dffin$christian_muslim_divide[dffin$rel_belief %in% c("muslim") & (dffin$pl2cathlic == 1 | dffin$pl2prtstnt)] <- 1

dffin$class_divide <- NA
dffin$class_divide[dffin$class %in% c("low") & dffin$clean_classdrawn == "2_low_class"] <- 0
dffin$class_divide[dffin$class %in% c("high") & dffin$clean_classdrawn == "3_high_class"] <- 0
dffin$class_divide[dffin$class %in% c("low") & dffin$clean_classdrawn == "3_high_class"] <- 1
dffin$class_divide[dffin$class %in% c("high") & dffin$clean_classdrawn == "2_low_class"] <- 1
table(dffin$class_divide)

# drops atheistic respondents / atheist draws


rar_mod_societal_dict1 <- lmer(pl2gets ~ 
                                 round + 
                                 euattachmentshown + 
                                 partisanshipshown +
                                 diffsex +
                                 diffage +
                                 class_divide + 
                                 #I(diffclass == "99_dk") + 
                                 diffreligion +
                                 I(nationality != 'co-national') + 
                                 outpartisan2 * nopid 
                               + (1 | country/country_id )
                               , dffin[
                                 dffin$game == "dict",])
summary(rar_mod_societal_dict1)

rar_mod_societal_dict2 <- lmer(pl2gets ~ 
                                 round + 
                                 euattachmentshown + 
                                 partisanshipshown +
                                 diffsex +
                                 diffage +
                                 #class_divide + 
                                 # diffreligion + 
                                 diffclass +
                                 christian_muslim_divide +
                                 I(nationality != 'co-national') + 
                                 outpartisan2 * nopid 
                               + (1 | country/country_id )
                               , dffin[
                                 dffin$game == "dict",])
summary(rar_mod_societal_dict2)

rar_mod_societal_dict3 <- lmer(pl2gets ~ 
                                 round + 
                                 euattachmentshown + 
                                 partisanshipshown +
                                 diffsex +
                                 diffage +
                                 class_divide + 
                                 # diffreligion + 
                                 christian_muslim_divide +
                                 I(nationality != 'co-national') + 
                                 outpartisan2 * nopid 
                               + (1 | country/country_id )
                               , dffin[
                                 dffin$game == "dict",])
summary(rar_mod_societal_dict3)



rar_mod_societal_trust1 <- lmer(pl2gets ~ 
                                  round + 
                                  euattachmentshown + 
                                  partisanshipshown +
                                  diffsex +
                                  diffage +
                                  class_divide + 
                                  #I(diffclass == "99_dk") + 
                                  diffreligion +
                                  I(nationality != 'co-national') + 
                                  outpartisan2 * nopid 
                                + (1 | country/country_id )
                                , dffin[
                                  dffin$game == "trust",])
summary(rar_mod_societal_trust1)

rar_mod_societal_trust2 <- lmer(pl2gets ~ 
                                  round + 
                                  euattachmentshown + 
                                  partisanshipshown +
                                  diffsex +
                                  diffage +
                                  diffclass +
                                  christian_muslim_divide +
                                  I(nationality != 'co-national') + 
                                  outpartisan2 * nopid 
                                + (1 | country/country_id )
                                , dffin[
                                  dffin$game == "trust",])
summary(rar_mod_societal_trust2)

rar_mod_societal_trust3 <- lmer(pl2gets ~ 
                                  round + 
                                  euattachmentshown + 
                                  partisanshipshown +
                                  diffsex +
                                  diffage +
                                  class_divide + 
                                  # diffreligion + 
                                  christian_muslim_divide +
                                  I(nationality != 'co-national') + 
                                  outpartisan2 * nopid 
                                + (1 | country/country_id )
                                , dffin[
                                  dffin$game == "trust",])
summary(rar_mod_societal_trust3)


stargazer::stargazer(rar_mod_societal_dict1,rar_mod_societal_dict2,
                     #rar_mod_societal_dict3,
                     rar_mod_societal_trust1,rar_mod_societal_trust2,
                     #rar_mod_societal_trust3, 
                     single.row = T,
                     title = c("Hierarchical Linear Model"),
                     dep.var.labels = "Tokens for Player 2",
                     column.labels = c("Dictator Game","Dictator Game",
                                       "Trust Game","Trust Game"), 
                     covariate.labels = c(
                       "Round 2",
                       "Round 3",
                       "Subset Condition 1",#"Subset Condition 1", # Treatment 1
                       "Subset Condition 2", # "Subset Condition 2",
                       "Gender: Out-Group",
                       "(Respondent: Other Gender)",
                       "Age: Out-Group",
                       "Class Divide (Upper v Lower)",
                       "Religion: Out-Group",
                       "(Respondent: Other Religion)",
                       "(Respondent: Non-Believer)",
                       "Class: Out-Group",
                       "(Respondent: Don't Know Class)",
                       "Religious Divide (Christian v Muslim)",
                       "Nationality: Out-Group",
                       "Partisanship: Out-Group",
                       "(Respondent: No PID)",
                       "Partisanship: Control Group * (Respondent: No PID)",
                       "Partisanship: Out-Group * (Respondent: No PID)"
                     ),
                     add.lines = list(
                       
                       c("SD Respondent-level",
                         round(as.numeric(attributes(VarCorr(rar_mod_societal_dict1)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(rar_mod_societal_dict2)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(rar_mod_societal_trust1)$"country_id:country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(rar_mod_societal_trust2)$"country_id:country")$stddev), 3)),
                       c("SD Country-level",
                         round(as.numeric(attributes(VarCorr(rar_mod_societal_dict1)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(rar_mod_societal_dict2)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(rar_mod_societal_trust1)$"country")$stddev), 3),
                         round(as.numeric(attributes(VarCorr(rar_mod_societal_trust2)$"country")$stddev), 3)),
                       c("Conditional R-Squared",
                         round(get_gof(rar_mod_societal_dict1)$r2.conditional, 3),
                         round(get_gof(rar_mod_societal_dict2)$r2.conditional, 3),
                         round(get_gof(rar_mod_societal_trust1)$r2.conditional, 3),
                         round(get_gof(rar_mod_societal_trust2)$r2.conditional, 3)),
                       c("Respondents",
                         as.numeric(sapply(ranef(rar_mod_societal_dict1),nrow)[1]),
                         as.numeric(sapply(ranef(rar_mod_societal_dict2),nrow)[1]),
                         as.numeric(sapply(ranef(rar_mod_societal_trust1),nrow)[1]),
                         as.numeric(sapply(ranef(rar_mod_societal_trust2),nrow)[1])),
                       c("Countries",
                         as.numeric(sapply(ranef(rar_mod_societal_dict1),nrow)[2]),
                         as.numeric(sapply(ranef(rar_mod_societal_dict2),nrow)[2]),
                         as.numeric(sapply(ranef(rar_mod_societal_trust1),nrow)[2]),
                         as.numeric(sapply(ranef(rar_mod_societal_trust2),nrow)[2]))
                     ),
                     digits = 2,
                     star.cutoffs = c(.1, .05, .01),
                     star.char = c("+", "*", "**"),
                     notes = c("+ p<0.1; * p<0.05; ** p<0.01") ,
                     notes.append=FALSE,
                     omit.stat = c("ll","aic","bic"),
                     out = paste0(path, "/tabE1_main_socialbenchmark_v2.html"))





un_coefs1 <- rar_mod_societal_dict3@beta
un_ses1 <- sqrt(diag(vcov(rar_mod_societal_dict3)))
un_coefs2 <- rar_mod_societal_trust3@beta
un_ses2 <- sqrt(diag(vcov(rar_mod_societal_trust3)))

coefs <- cbind(un_coefs1,un_coefs2)
ses <- cbind(un_ses1,un_ses2)

coefsofinterest <- c("diffsex0_diff",
                     "diffage0_diff",
                     "class_divide",
                     "christian_muslim_divide",
                     "I(nationality != \"co-national\")TRUE",
                     "outpartisan21_partisan_outpartisan")

coefs <- coefs[rownames(vcov(rar_mod_societal_dict3)) %in% coefsofinterest,]
ses <- ses[rownames(vcov(rar_mod_societal_dict3)) %in% coefsofinterest,]

labels <- c("Gender: In-group","Gender: Out-group",
            "Age: In-group","Age: Out-group",
            "Class Divide: In-group","Class Divide: Out-group",
            "Christian-Muslim Divide: In-group","Christian-Muslim Divide: Out-group",
            "Nationality: In-group","Nationality: Out-group",
            "Partisanship: In-group","Partisanship: Out-group")

spots_ref <- c(15,13,11,9,7,5)
spots_eff <- c(14,12,10,8,6,4)







jpeg(paste0(path, "/figE1_main_societaldivides.jpeg"), units="in", width=9, height=3.5, res=200)
par(mfrow = c(1,4), mar = c(2,4,0,4), oma = c(2,3,0,0))

set.seed(123)
draws <- mvrnorm(10000, rar_mod_societal_dict1@beta, vcov(rar_mod_societal_dict1))
draws <- draws[, colnames(draws) %in% c( "class_divide", "outpartisan21_partisan_outpartisan")]
draws <- apply(draws, 2, quantile, c(.025,.5,.975))


plot(1,1,type = "n", xlim = c(-1.25,0), ylim = c(.5,2.5), axes = F, xlab = "", ylab = "")
abline(h= 1:2, col = "grey")
axis(2, at = 1:2, labels = c("Out-Partisan", "Class Divide\n(Upper v Lower)"), las = 2, tick = F)
axis(1)
mtext("Difference in Tokens", side = 1, line = 2.5)
mtext("Dictator Game", side = 3, line = -3)

points(draws[2,], 2:1, pch = 16)
segments(draws[1,1],2, draws[3,1],2,lwd = 2)
segments(draws[1,2],1, draws[3,2],1,lwd = 2)



set.seed(123)
draws <- mvrnorm(10000, rar_mod_societal_dict2@beta, vcov(rar_mod_societal_dict2))
draws <- draws[, colnames(draws) %in% c( "christian_muslim_divide", "outpartisan21_partisan_outpartisan")]
draws <- apply(draws, 2, quantile, c(.025,.5,.975))

plot(1,1,type = "n", xlim = c(-1.25,0), ylim = c(.5,2.5), axes = F, xlab = "", ylab = "")
abline(h= 1:2, col = "grey")
axis(2, at = 1:2, labels = c("Out-Partisan", "Religious Divide\n(Christian v Muslim)"), las = 2, tick = F)
axis(1)
mtext("Difference in Tokens", side = 1, line = 2.5)
mtext("Dictator Game", side = 3, line = -3)

points(draws[2,], 2:1, pch = 16)
segments(draws[1,1],2, draws[3,1],2,lwd = 2)
segments(draws[1,2],1, draws[3,2],1,lwd = 2)


set.seed(123)
draws <- mvrnorm(10000, rar_mod_societal_trust1@beta, vcov(rar_mod_societal_trust1))
draws <- draws[, colnames(draws) %in% c( "class_divide", "outpartisan21_partisan_outpartisan")]
draws <- apply(draws, 2, quantile, c(.025,.5,.975))


plot(1,1,type = "n", xlim = c(-1.25,0), ylim = c(.5,2.5), axes = F, xlab = "", ylab = "")
abline(h= 1:2, col = "grey")
axis(2, at = 1:2, labels = c("Out-Partisan", "Class Divide\n(Upper v Lower)"), las = 2, tick = F)
axis(1)
mtext("Difference in Tokens", side = 1, line = 2.5)
mtext("Trust Game", side = 3, line = -3)

points(draws[2,], 2:1, pch = 16)
segments(draws[1,1],2, draws[3,1],2,lwd = 2)
segments(draws[1,2],1, draws[3,2],1,lwd = 2)



set.seed(123)
draws <- mvrnorm(10000, rar_mod_societal_trust2@beta, vcov(rar_mod_societal_trust2))
draws <- draws[, colnames(draws) %in% c( "christian_muslim_divide", "outpartisan21_partisan_outpartisan")]
draws <- apply(draws, 2, quantile, c(.025,.5,.975))

plot(1,1,type = "n", xlim = c(-1.25,0), ylim = c(.5,2.5), axes = F, xlab = "", ylab = "")
abline(h= 1:2, col = "grey")
axis(2, at = 1:2, labels = c("Out-Partisan", "Religious Divide\n(Christian v Muslim)"), las = 2, tick = F)
axis(1)
mtext("Difference in Tokens", side = 1, line = 2.5)
mtext("Trust Game", side = 3, line = -3)

points(draws[2,], 2:1, pch = 16)
segments(draws[1,1],2, draws[3,1],2,lwd = 2)
segments(draws[1,2],1, draws[3,2],1,lwd = 2)
dev.off()



# APPENDIX E2: Feeling thermometer #####

tdf <- subset(dffin, select = c(country_id,country,Q52,thermscore_party1,thermscore_party2,thermscore_party3,thermscore_party4,thermscore_party5,thermscore_party6,nopid))
tdf <- tdf[!duplicated(tdf),]
tdf <- tdf[tdf$nopid == 0,]

tmp1 <- subset(tdf, select = c(country_id,country,Q52,thermscore_party1))
tmp2 <- subset(tdf, select = c(country_id,country,Q52,thermscore_party2))
tmp3 <- subset(tdf, select = c(country_id,country,Q52,thermscore_party3))
tmp4 <- subset(tdf, select = c(country_id,country,Q52,thermscore_party4))
tmp5 <- subset(tdf, select = c(country_id,country,Q52,thermscore_party5))
tmp6 <- subset(tdf, select = c(country_id,country,Q52,thermscore_party6))

names(tmp1)[grep("thermscore_party", names(tmp1))] <- "thermscore_party"
names(tmp2)[grep("thermscore_party", names(tmp2))] <- "thermscore_party"
names(tmp3)[grep("thermscore_party", names(tmp3))] <- "thermscore_party"
names(tmp4)[grep("thermscore_party", names(tmp4))] <- "thermscore_party"
names(tmp5)[grep("thermscore_party", names(tmp5))] <- "thermscore_party"
names(tmp6)[grep("thermscore_party", names(tmp6))] <- "thermscore_party"

tmp1$therm_num <- 1
tmp2$therm_num <- 2
tmp3$therm_num <- 3
tmp4$therm_num <- 4
tmp5$therm_num <- 5
tmp6$therm_num <- 6

tdf <- rbind(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6)
length(unique(tdf$country_id))
18397



tdf$copartisan <- 0
tdf$copartisan[tdf$Q52 %in% c("Österreichische Volkspartei (ÖVP)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Sozialdemokratische Partei Österreich (SPÖ)") & tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Freiheitliche Partei Österreichs (FPÖ)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("NEOS – Das Neue Österreich und Liberales Forum (NEOS)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Die Grünen – Die grüne Alternative (GRÜNE)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("JETZT - Liste Pilz (JETZT)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Nieuw-Vlaamse Alliantie (N-VA)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Parti socialiste (PS)") & tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Mouvement Réformateur (MR)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Christen-Democratisch en Vlaams (CD&V)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Open Vlaamse Liberalen en Democraten (Open Vld)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Socialistische Partij Anders (sp.a)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Граждани за европейско развитие на България (ГЕРБ)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Българска социалистическа партия (БСП)") & tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Движение за права и свободи (ДПС)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("ВМРО – Българско Национално Движение") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Атака") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Воля") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Hrvatska demokratska zajednica (HDZ)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Socijaldemokratska partija Hrvatske  (SDP)") & tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Živi zid") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Most nezavisnih lista (MOST)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Bruna Esih – Zlatko Hasanbegović: Neovisni za Hrvatsku (NHR)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Bandić Milan 365 - Stranka rada i solidarnosti (BM365)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("ANO 2011 (ANO)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Občanská demokratická strana (ODS)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Česká pirátská strana (Piráti)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Svoboda a přímá demokracie (SPD)" ) & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Komunistická strana Čech a Moravy (KSČM)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Česká strana sociálně demokratická (ČSSD)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c( "Socialdemokratiet (A)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Venstre (V)" )& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Dansk Folkeparti (O)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Det Radikale Venstre (B)"  ) & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Socialistisk Folkeparti (F)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Enhedslisten – De rød-grønne (Ø)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Eesti Reformierakond (ER)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Eesti Keskerakond (KE)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Eesti Konservatiivne Rahvaerakond (EKRE)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Sotsiaaldemokraatlik Erakond (SDE)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Isamaa (I)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Eesti 200") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Suomen Sosialidemokraattinen Puolue (SDP)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Kansallinen Kokoomus (KOK)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Perussuomalaiset (PS)" ) & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Suomen Keskusta (KESK)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Vihreä liitto (VIHR)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Vasemmistoliitto (VAS)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("La République En Marche (REM)" ) & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Les Républicains (LR)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Rassemblement national (RN (ex. Front National (FN)))" ) & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("La France Insoumise (FI)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Parti Socialiste (PS)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Europe Écologie-Les Verts (EELV)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("La République En Marche (REM)" ) & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Les Républicains (LR)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Rassemblement national (RN (ex. Front National (FN)))" ) & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("La France Insoumise (FI)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Parti Socialiste (PS)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Europe Écologie-Les Verts (EELV)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Christlich Demokratische Union (CDU)","Christlich Soziale Union (CSU)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Sozialdemokratische Partei Deutschlands (SPD)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Alternative für Deutschland (AfD)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Freie Demokratische Partei (FDP)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Die Linke (Linke)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Bündnis 90 / Die Grünen (Grüne)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Nέα Δημοκρατία (NΔ)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Συνασπισμός Ριζοσπαστικής Αριστεράς (ΣΥ.ΡIΖ.Α)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Πανελλήνιο Σοσιαλιστικό ίνημα (ΠΑΣΟK)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Κίνημα Δημοκρατών Σοσιαλιστών (ΚΙΔΗΣΟ)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c( "Λαϊκός Σύνδεσμος -Χρυσή Αυγή (ΛΣ-ΧΑ)" ) & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Kομμουνιστικό Kόμμα Ελλάδας (KKΕ)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Fidesz – Magyar Polgári Szövetség (FIDESZ)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Kereszténydemokrata Néppárt (KDNP)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Jobbik Magyarországért Mozgalom (JOBBIK)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Magyar Szocialista Párt (MSZP)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Párbeszéd Magyarországért (Párbeszéd)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Demokratikus Koalíció (DK)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Fine Gael (FG)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Fianna Fáil (FF)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Sinn Féin (SF)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Labour Party (LP)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Independent Alliance") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Green Party") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Lega (Lega Salvini Premier)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Movimento 5 Stelle (M5S)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Partito Democratico (PD)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Forza Italia (FI)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Fratelli d'Italia (FDI)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Più Europa (+E)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Sociāldemokrātiskā partija „Saskaņa“ (SDP)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Jaunā konservatīvā partija (JKP)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Zaļo un Zemnieku savienība (ZZS: Latvijas Zemnieku savienība (LZS) + Latvijas Zaļā partija (LZP))") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Nacionālā apvienība „Visu Latvijai!” – „Tēvzemei un Brīvībai/LNNK” (NA)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("'Kam pieder valsts' (KPV LV)") & tdf$therm_num == 5] <- 1
# Kustiba Par!
#tdf$copartisan[tdf$Q52 %in% c("Più Europa (+E)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Tėvynės sąjunga - Lietuvos krikščionys demokratai (TS-LKD)" ) & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Lietuvos valstiečių ir žaliųjų sąjunga (LVŽS)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Lietuvos socialdemokratų partija (LSDP)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Tvarka ir teisingumas (TT)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Darbo partija (DP)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Lietuvos socialdemokratų darbo partija (LSDDP)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Volkspartij voor Vrijheid en Democratie (VVD)" ) & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Partij voor de Vrijheid (PVV)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("GroenLinks (GL)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Christen-Democratisch Appèl (CDA)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Democraten 66 (D66)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Socialistische Partij (SP)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Prawo i Sprawiedliwość (PiS)" ) & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Platforma Obywatelska (PO)" )& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Wiosna" ) & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Kukuiz'15") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Polskie Stronnictwo Ludowe (PSL)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Sojusz Lewicy Demokratycznej (SLD)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Partido Socialista (PS)" ) & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Partido Social Democrata (PSD)" )& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Centro Democrático e Social – Partido Popular (CDS-PP)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Bloco de Esquerda (BE)" ) & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Partido Comunista Português (PCP)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Partido Ecologista 'os Verdes' (PEV)" ) & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Partidul Social Democrat (PSD)" ) & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c( "Partidul Național Liberal (PNL)" )& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Alianța Liberalilor și Democraților (ALDE)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Uniunea Salvați România (USR)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("PRO România (PRO)" ) & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Uniunea Democrată Maghiară din România (UDMR - Romániai Magyar Demokrata Szövetség (RMDSZ))") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Smer – sociálna demokracia (Smer-SD)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Sloboda a Solidarita (SaS)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Obyčajní Ľudia a nezávislé osobnosti (OĽaNO)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Kotleba – Ľudová strana Naše Slovensko (ĽSNS)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Progresívne Slovensko (PS)", "Progresívne Slovensko") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("SME RODINA – Boris Kollár (SME RODINA)" ) & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Lista Marjana Šarca (LMŠ)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Slovenska demokratska stranka (SDS)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Socialni demokrati (SD)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Levica") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Stranka modernega centra (SMC)" ) & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Nova Slovenija - Krščanski demokrati (N.Si)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Partido Socialista Obrero Español (PSOE)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Partido Popular (PP)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Ciudadanos - Partido de la Ciudadanía (C's)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Podemos") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Vox" ) & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Izquierda Unida (IU)" ) & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Socialdemokratiska arbetarpartiet (SAP)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Sverigedemokraterna (SD)" )& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Moderata samlingspartiet (M)") & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Kristdemokraterna (KD)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Vänsterpartiet (V)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Centerpartiet (C)") & tdf$therm_num == 6] <- 1

tdf$copartisan[tdf$Q52 %in% c("Labour Party (Labour)") & tdf$therm_num == 1] <- 1
tdf$copartisan[tdf$Q52 %in% c("Conservative and Unionist Party (Conservative Party)")& tdf$therm_num == 2] <- 1
tdf$copartisan[tdf$Q52 %in% c("Brexit Party" ) & tdf$therm_num == 3] <- 1
tdf$copartisan[tdf$Q52 %in% c("Liberal Democrats (Lib Dem)") & tdf$therm_num == 4] <- 1
tdf$copartisan[tdf$Q52 %in% c("Scotish National Party (SNP)") & tdf$therm_num == 5] <- 1
tdf$copartisan[tdf$Q52 %in% c("Green Party of England and Wales (GP)") & tdf$therm_num == 6] <- 1

table(tdf$country,tdf$copartisan)

summary(m1 <- lmer(thermscore_party ~ copartisan + (1 | country/country_id), tdf))

stargazer::stargazer(m1, single.row = T,
                     title = c("Hierarchical Linear Model - Feeling Thermometer"),
                     dep.var.labels = "Feeling Thermometer Score",
                     covariate.labels = c(
                       "Co-Partisan"),
                     add.lines = list(
                       c("SD Respondent-level", 
                         round(as.numeric(attributes(VarCorr(m1)$"country_id:country")$stddev), 3)),
                       c("SD Country-level", 
                         round(as.numeric(attributes(VarCorr(m1)$"country")$stddev), 3)),
                       c("Conditional R-Squared",
                         round(get_gof(m1)$r2.conditional, 3)),
                       c("Respondents",
                         as.numeric(sapply(ranef(m1),nrow)[1])),
                       c("Countries", 
                         num_cntr <- as.numeric(sapply(ranef(m1),nrow)[2]))
                     ),
                     star.cutoffs = c(.1, .05, .01),
                     star.char = c("+", "*", "**"),
                     notes = c("+ p<0.1; * p<0.05; ** p<0.01") ,
                     notes.append=FALSE,
                     omit.stat = c("ll","aic","bic"),
                     out = paste0(path, "/tabE2_1_inout_feeltherm_v2.html"))


jpeg(paste0(path, "/figE2_inout_feelingthermometer.jpeg"), units="in", width=9, height=5, res=200)
set.seed(123)
draws <- mvrnorm(10000, m1@beta, vcov(m1))
par(mfrow = c(2,1), mar = c(1,1,1,1), oma = c(2,4,0,1))
plot(1,1,type = "n", ylim = c(0,2), xlim = c(0,100), axes = F)
axis(1)
abline( v = 50, col = "grey")
text(mean(draws[,1]), 1, pos = 3, "Out-Party (Out-Group)")
points(mean(draws[,1]), 1, pch = 16)
segments(quantile(draws[,1], .025), 1, 
         quantile(draws[,1], .975), 1)
text(mean(draws[,1] + draws[,2]), 1, pos = 3, "Co-Party (In-Group)")
points(mean(draws[,1] + draws[,2]), 1, pch = 16)
segments(quantile(draws[,1] + draws[,2], .025), 1, 
         quantile(draws[,1] + draws[,2], .975), 1)
mtext("Feeling Thermometer Score", side = 1, line = 2)

plot(1,1,type = "n", ylim = c(0,3), xlim = c(0,100), axes = F)
axis(1)
abline( h = 1:2, col = "grey")
points(mean(50 - draws[,1]), 1, pch = 16)
segments(quantile(50 - draws[,1], .025), 1, 
         quantile(50 - draws[,1], .975), 1)

points(mean(draws[,1] + draws[,2] - 50), 2, pch = 16)
segments(quantile(draws[,1] + draws[,2] - 50, .025), 2, 
         quantile(draws[,1] + draws[,2] - 50, .975), 2)
axis(2, at = c(1,2), c("Out-group\nderogation","In-group\nfavoritism"), las = 2, tick = F)
mtext("Absolute size", side = 1, line = 2)
dev.off()

eu25 <- levels(tdf$country)
out <- list()


# APPENDIX E2: Feeling Thermometer by Country #####
for ( i in 1:25){
  summary(out[[i]] <- lmer(thermscore_party ~ copartisan + (1 | country_id), tdf[tdf$country == eu25[i],]))
  set.seed(123)
  out[[i]] <- mvrnorm(10000, out[[i]]@beta, vcov(out[[i]]))
  
}

tabout <- list()
for ( i in 1:25){
  tabout[[i]] <- cbind( paste0(round(mean(out[[i]][,1]),1) ,
                               " (" ,
                               round(quantile(out[[i]][,1], .025),1) ,
                               ", ",
                               round(quantile(out[[i]][,1], .975),1) ,
                               ")"
  )
  ,
  paste0(round(mean(out[[i]][,1] + out[[i]][,2]),1) ,
         " (" ,
         round(quantile(out[[i]][,1] + out[[i]][,2], .025),1) ,
         ", ",
         round(quantile(out[[i]][,1] + out[[i]][,2], .975),1) ,
         ")"
  )
  ,
  paste0(round(mean(50 - out[[i]][,1]),1) ,
         " (" ,
         round(quantile(50 - out[[i]][,1], .025),1) ,
         ", ",
         round(quantile(50 - out[[i]][,1], .975),1) ,
         ")"
  )
  ,
  paste0(round(mean((out[[i]][,1] + out[[i]][,2]) - 50),1) ,
         " (" ,
         round(quantile((out[[i]][,1] + out[[i]][,2]) - 50, .025),1) ,
         ", ",
         round(quantile((out[[i]][,1] + out[[i]][,2]) - 50, .975),1) ,
         ")"
  )
  ,
  paste0(round(mean((50 - out[[i]][,1]) - ((out[[i]][,1] + out[[i]][,2]) - 50)),1) ,
         " (" ,
         round(quantile((50 - out[[i]][,1]) - ((out[[i]][,1] + out[[i]][,2]) - 50), .025),1) ,
         ", ",
         round(quantile((50 - out[[i]][,1]) - ((out[[i]][,1] + out[[i]][,2]) - 50), .975),1) ,
         ")"
  ) )
}
tabout <- do.call(rbind, tabout)
tabout

# File name: "recoding_script.R"
# Goal: Recode data for matching analysis
# Dependency: "salta_data.Rdata"
# Output: "datamatch.Rdata"
####################################################

load("salta_data.Rdata")

attach(salta.data)

#___________________________________________________#
# Polling place 
#___________________________________________________#

polling.place <- escuela

#___________________________________________________#
# Voting System
#___________________________________________________#

# electronic (VE) or traditional (VT) voting?

system <- sistema

EV <- NULL
EV[system == "VE"] <- 1
EV[system == "VT"] <- 0

#___________________________________________________# 
# Recode outcome variables 
#___________________________________________________#

# poll workers qualified enough?

prop.table(table(capaci_autoridades))

capable.auth <- NULL
capable.auth[capaci_autoridades == "Nada Capacitadas"] <- 0
capable.auth[capaci_autoridades == "Poco Capacitadas"] <- 0
capable.auth[capaci_autoridades == "Bastante Capacitadas"] <- 1
capable.auth[capaci_autoridades == "Muy Capacitadas"] <- 1

# quality of voting experience?

prop.table(table(calif_votac))

eval.voting <- NULL
eval.voting[calif_votac == "Muy Malo"] <- 0
eval.voting[calif_votac == "Malo"] <- 0
eval.voting[calif_votac == "Bueno"] <- 0
eval.voting[calif_votac == "Muy Bueno"] <- 1

# difficulty of voting experience?

prop.table(table(facil))

easy.voting <- NULL
easy.voting[as.numeric(facil) == 2] <- 1
easy.voting[as.numeric(facil) == 3] <- 0
easy.voting[as.numeric(facil) == 4] <- 0
easy.voting[as.numeric(facil) == 5] <- 0

# how sure vote counted?

prop.table(table(cuàn_seguro))

sure.counted <- NULL
sure.counted[as.numeric(cuàn_seguro) == 2] <- 1
sure.counted[as.numeric(cuàn_seguro) == 3] <- 1
sure.counted[as.numeric(cuàn_seguro) == 4] <- 0
sure.counted[as.numeric(cuàn_seguro) == 5] <- 0

# how confident vote secret?

prop.table(table(cuàn_confiado))

conf.secret <- NULL
conf.secret[as.numeric(cuàn_confiado) == 2] <- 1
conf.secret[as.numeric(cuàn_confiado) == 3] <- 1
conf.secret[as.numeric(cuàn_confiado) == 4] <- 0
conf.secret[as.numeric(cuàn_confiado) == 5] <- 0

# believe provincial elections are clean?

prop.table(table(elecc_limpias))

how.clean <- NULL
how.clean[as.numeric(elecc_limpias) == 2] <- 1
how.clean[as.numeric(elecc_limpias) == 3] <- 1
how.clean[as.numeric(elecc_limpias) == 4] <- 0
how.clean[as.numeric(elecc_limpias) == 5] <- 0

# how quick was process?

prop.table(table(rapidez_proceso))

speed <- NULL
speed[as.numeric(rapidez_proceso) == 2] <- 1
speed[as.numeric(rapidez_proceso) == 3] <- 1
speed[as.numeric(rapidez_proceso) == 4] <- 0
speed[as.numeric(rapidez_proceso) == 5] <- 0

# agree replacing VT by VE?

prop.table(table(reemplazoVTxVE))

agree.evoting <- NULL
agree.evoting[as.numeric(reemplazoVTxVE) == 2] <- 1
agree.evoting[as.numeric(reemplazoVTxVE) == 3] <- 1
agree.evoting[as.numeric(reemplazoVTxVE) == 4] <- 0
agree.evoting[as.numeric(reemplazoVTxVE) == 5] <- 0

# select candidates electronically?

prop.table(table(sist_voto_categ))

eselect.cand <- NULL
eselect.cand[as.numeric(sist_voto_categ) == 2] <- 0
eselect.cand[as.numeric(sist_voto_categ) == 3] <- 1

#___________________________________________________#
# Recode covariates 
#___________________________________________________#

age <- edad
age.group <- NULL
age.group[age < 30] <- 1
age.group[age > 29 & age < 40] <- 2
age.group[age > 39 & age < 50] <- 3
age.group[age > 49 & age < 65] <- 4
age.group[age > 64] <- 5

male <- NULL
male[sexo == "MASCULINO"] <- 1
male[sexo == "FEMENINO"] <- 0

educ <- NULL
educ[educ_enc == "Sin Estudios" | educ_enc == "Primario Incompleto"] <- 1
educ[educ_enc == "Primario Completo"] <- 2
educ[educ_enc == "Secundario Incompleto"] <- 3
educ[educ_enc == "Secundario Completo"] <- 4
educ[educ_enc == "Terciario Incompleto"] <- 5
educ[educ_enc == "Terciario Completo"] <- 6
educ[educ_enc == "Universitario Incompleto"] <- 7
educ[educ_enc == "Universitario Completo" | educ_enc == "Posgrado"] <- 9

white.collar <- NULL
white.collar[ocupac == "EMPLEADO PUBLICO"|ocupac == "COMERCIANTE SIN EMPLEADOS" | ocupac == "EMPLEADO SECTOR PRIVADO" | ocupac == "PROF/COMERCIANTE EMPLEADOS A CARGO"] <- 1
white.collar[ocupac != "EMPLEADO PUBLICO"&ocupac != "COMERCIANTE SIN EMPLEADOS"&ocupac != "EMPLEADO SECTOR PRIVADO"&ocupac != "PROF/COMERCIANTE EMPLEADOS A CARGO"] <- 0

not.full.time <- NULL
not.full.time[ocupac == "ESTUDIANTE" | ocupac == "AMA DE CASA" | ocupac == "DESOCUPADO" | ocupac == "SUBSIDIADO/PLANES/ASIGNACIONES" | ocupac == "TRABAJOS TEMPORARIOS" | ocupac == "EMPLEADO INFORMAL" | ocupac == "JUBILADO/PENSIONADO" | ocupac == "RENTISTA"] <- 1
not.full.time[ocupac == "ESTUDIANTE" | ocupac != "AMA DE CASA" & ocupac != "DESOCUPADO" & ocupac != "SUBSIDIADO/PLANES/ASIGNACIONES" & ocupac != "TRABAJOS TEMPORARIOS" & ocupac != "EMPLEADO INFORMAL" & ocupac != "JUBILADO/PENSIONADO" & ocupac != "RENTISTA"] <- 0

internet.work <- NULL
internet.work[internet_trabajar == "No"] <- 0
internet.work[internet_trabajar == "Si"] <- 1

internet.play <- NULL
internet.play[internet_jugar == "No"] <- 0
internet.play[internet_jugar == "Si"] <- 1

atm <- NULL
atm[cajeros == "No"] <- 0
atm[cajeros == "Si"] <- 1

cell <- NULL
cell[celular == "No"] <- 0
cell[celular == "Si"] <- 1

pc.own <- NULL
pc.own[PC_propia == "No"] <- 0
pc.own[PC_propia == "Si"] <- 1

tech <- internet.work+internet.play+atm+cell+pc.own+1

table(randazzo)
table(figueroa)
table(alperovich)

info1 <- NULL
info1[as.numeric(randazzo) == 1 | as.numeric(randazzo) == 2 | as.numeric(randazzo) == 3] <- 1
info1[as.numeric(randazzo) == 4 | as.numeric(randazzo) == 5] <- 0

info2 <- NULL
info2[as.numeric(figueroa) == 1 | as.numeric(figueroa) == 2 | as.numeric(figueroa) == 3] <- 1
info2[as.numeric(figueroa) == 4 | as.numeric(figueroa) == 5] <- 0


info3 <- NULL
info3[as.numeric(alperovich) == 1 | as.numeric(alperovich) == 2 | as.numeric(alperovich) == 3] <- 1
info3[as.numeric(alperovich) == 4 | as.numeric(alperovich) == 5] <- 0

pol.info <- 1 + info1 + info2 + info3

table(pol.info)

#___________________________________________________#
# Create and save dataframe for matching analysis 
#___________________________________________________#

datamatch <- data.frame(polling.place, EV, age.group, educ, male, tech, pol.info, white.collar, not.full.time, capable.auth, eval.voting, easy.voting, sure.counted, conf.secret, how.clean, speed, agree.evoting, eselect.cand)

save(datamatch, file="datamatch.Rdata")


# File name: "matching_script.R"
# Goal: Estimate effect of e-voting using matching
# Dependency: "datamatch.Rdata" 
########################################################################

library(MatchIt)
#install.packages("Zelig")
library(Zelig)
library(rbounds)

load("datamatch.Rdata")

outcomes <- datamatch[10:18]

outcomes.lbls <- names(outcomes)

n.outcomes <- dim(outcomes)[2]

#_________________________________ Table 1 _________________________________#

tab1 <- matrix(NA, nrow = n.outcomes, ncol = 6)
rownames(tab1) <- outcomes.lbls
colnames(tab1) <- c("N", "prop.all", "prop.ev", "prop.tv", "diff", "pvalue")

for (i in 1:n.outcomes) {
  tab1[i, 1] <- length(na.omit(outcomes[, i]))
  tab1[i, 2] <- prop.table(table(outcomes[, i]))[2] * 100	
  tab1[i, 3:4] <- rev(prop.table(table(outcomes[, i], datamatch$EV), 2)[2, ]) * 100
  tab1[i, 5] <- tab1[i, 3] - tab1[i, 4]	
  tab1[i, 6] <- prop.test(table(outcomes[, i], datamatch$EV)[2, ], n = apply(table(outcomes[, i], datamatch$EV), 2, sum))$p.value
}

tab1 <- tab1[rev(order(tab1[, "diff"])), ]

### Table 1 ###

print(tab1, digits = 4)

#___________________________________________________________________________#

# Drop observations with missing values in covariates

datamatch[, 10:18][is.na(datamatch[, 10:18]) == "TRUE"] <- 99999
datamatch <- na.omit(datamatch)

#__________________________ Table 2, pre-matching __________________________#

EV <- datamatch[2]

covariates <- datamatch[c("age.group", "educ", "white.collar", "not.full.time", "male", "tech", "pol.info")]
covariate.lbls <- names(covariates)

n.covariates <- dim(covariates)[2]

tab2.pre <- matrix(NA, nrow = n.covariates, ncol = 4)
rownames(tab2.pre) <- covariate.lbls
colnames(tab2.pre) <- c("ev", "tv", "diff", "pvalue")

tab2.pre[, 1:2] <- cbind(apply(covariates[EV == 1,], 2, mean), apply(covariates[EV == 0,], 2, mean))
tab2.pre[, 3] <- tab2.pre[, 1] - tab2.pre[, 2]

for (i in c(1, 2, 6, 7)){
  tab2.pre[i, 4] <- ks.boot(covariates[, i][EV == 1], covariates[, i][EV == 0], nboots = 500)$ks.boot.pvalue
}
for (i in c(3, 4, 5)){
  tab2.pre[i, 4] <- prop.test(table(covariates[, i], EV$EV), n = apply(table(covariates[,i],EV$EV),2, sum))$p.value
}

#__________________________ Table 3, pre-matching __________________________#

datamatch[datamatch == 99999] <- NA

outcomes.pre <- datamatch[10:18]

tab3.pre <- matrix(NA,nrow = n.outcomes,ncol = 5)
rownames(tab3.pre) <- outcomes.lbls
colnames(tab3.pre) <- c("N", "prop.ev", "prop.tv", "diff", "pvalue")

for (i in 1:n.outcomes) {
  tab3.pre[i, 1] <- length(na.omit(outcomes.pre[, i]))
  tab3.pre[i, 2:3] <- rev(prop.table(table(outcomes.pre[,i],datamatch$EV),2)[2,])*100
  tab3.pre[i, 4] <- tab3.pre[i, 2] - tab3.pre[i, 3]	
  tab3.pre[i, 5] <- prop.test(table(outcomes.pre[, i], datamatch$EV)[2, ], n = apply(table(outcomes.pre[, i], datamatch$EV), 2, sum))$p.value
}

datamatch[, 10:18][is.na(datamatch[, 10:18]) == "TRUE"] <- 99999



#__________________________ Matching (with MatchIt) ________________________#

print("Matching")

set.seed(36466)

m.out <- matchit(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + tech:pol.info + white.collar + not.full.time + male, caliper = 0.05, data = datamatch, method = "nearest", verbose = "TRUE")

#save(m.out, file = "m.out.Rdata")

print("Balance Improvement")
print(summary(m.out))

#pdf("balance.pdf")

#plot(m.out)
#plot(m.out, type = "hist")
#plot(m.out, type = "jitter")

#dev.off()

#___________________________________________________________________________#

# matched sample

datamatched <- match.data(m.out)
datamatched[datamatched == 99999] <- NA

save(datamatched, file = "datamatched.Rdata")

#__________________________ Table 2, post-matching _________________________#

EV.post <- datamatched[2]

covariates.post <- datamatched[, covariate.lbls]

tab2.post <- matrix(NA, nrow = n.covariates, ncol = 4)
rownames(tab2.post) <- covariate.lbls
colnames(tab2.post) <- c("ev", "tv", "diff", "pvalue")

tab2.post[, 1:2] <- cbind(apply(covariates.post[EV.post == 1, ], 2, mean), apply(covariates.post[EV.post == 0,], 2, mean))
tab2.post[, 3] <- tab2.post[, 1] - tab2.post[, 2]
for (i in c(1, 2, 6 , 7)){
  tab2.post[i, 4]<-ks.boot(covariates.post[,i][EV.post==1],covariates.post[,i][EV.post==0], nboots = 500)$ks.boot.pvalue
}
for (i in c(3, 4, 5)){
  tab2.post[i, 4] <- prop.test(table(covariates.post[, i], EV.post$EV), n = apply(table(covariates.post[, i], EV.post$EV),2 , sum))$p.value
}

tab2 <- cbind(tab2.pre, tab2.post)
tab2[3:5, c(1:3, 5:7)] <- tab2[3:5, c(1:3, 5:7)] * 100

### Table 2 ###

print(tab2, digits = 4)

#__________________________ Table 3, post-matching _________________________#

outcomes.post <- datamatched[10:18]

tab3.post <- matrix(NA, nrow = n.outcomes, ncol = 5)
rownames(tab3.post) <- outcomes.lbls
colnames(tab3.post) <- c("N", "prop.ev", "prop.tv", "diff", "pvalue")

for (i in 1:n.outcomes) {
  tab3.post[i, 1] <- length(na.omit(outcomes.post[, i]))
  tab3.post[i, 2:3] <- rev(prop.table(table(outcomes.post[, i], datamatched$EV), 2)[2, ]) * 100
  tab3.post[i, 4] <- tab3.post[i, 2] - tab3.post[i, 3]	
  tab3.post[i, 5] <- prop.test(table(outcomes.post[, i], datamatched$EV)[2, ], n = apply(table(outcomes.post[, i], datamatched$EV), 2, sum))$p.value
}

tab3 <- cbind(tab3.pre, tab3.post)

tab3 <- tab3[rev(order(tab3[, 9])), ]

### Table 3 ###

print(tab3, digits = 4)


#extension

X<- cbind(datamatch$age.group, datamatch$educ, datamatch$male,datamatch$tech, datamatch$pol.info, datamatch$white.collar,datamatch$not.full.time)
Y <- datamatch[,c(10)]
Tr <- datamatch$EV #EV = treatment of electric voting

gen_match <- GenMatch(Tr=Tr, X=X, estimand="ATT",M=1, pop.size=100, max.generations=15, wait.generations=15, caliper=0.01)
matched_gen_match <- Match(Tr=Tr, X=X, estimand="ATT", M=1, Weight.matrix=gen_match, caliper=0.01, Y=Y)
summary(matched_gen_match, full=TRUE)
match_balance_gen_match <- MatchBalance(Tr ~ age.group + educ + male + tech + pol.info + white.collar + not.full.time, data=datamatch, match.out=matched_gen_match, nboots=100)


treatmenteffects <- NA

for (i in 10:18) {
  datamatch.results <- datamatch
  datamatch.results[datamatch == 99999] <- NA
  datamatch.results <- datamatch.results[complete.cases(datamatch.results[,i]), ]
  X2 <- cbind(datamatch.results$age.group, datamatch.results$educ, datamatch.results$male,datamatch.results$tech, datamatch.results$pol.info, datamatch.results$white.collar,datamatch.results$not.full.time)
  Y2 <- datamatch.results[,c(i)]
  Tr2 <- datamatch.results$EV
  matched2 <- Match(Tr=Tr2, X=X2, estimand="ATT",M=1, Weight.matrix=matched_gen_match, caliper=0.01, Y=Y2)
  treatmentoutcomes[i-9] <- matched2$est
  
}

summary(matched2)
print(treatmentoutcomes)




library(swat)
conn <- CAS('sas-isc.policlinicogemelli.it/cas-shared-default-http/', 443,'sb000363','Ucsc_4ry68ip0',protocol='https')
psuf <- defCasTable(conn, caslib = "Covid", tablename="p_su_f.sas7bdat")
psuf_df <- to.casDataFrame(psuf)
psuf_df <- psuf_df[which(psuf_df$BASELINE == "SI"),]
psuf_df <- psuf_df[,which(names(psuf_df) %in% c("NOSOGRAFICO","value"))]

psuf_df <- psuf_df[which(!is.na(psuf_df$value)),]
psuf_df <- psuf_df[which(psuf_df$value > 20),]
psuf_df <- psuf_df[which(psuf_df$value < 600),]

#prendo i nosografici del training
load("/media/kbocalc/Data/Progetti/Progetti/Progetti/COVID/NOSO_train.RData")
noso_train <- dati_analisi$NOSO

psuf_df_ALL <- psuf_df
psuf_df <- psuf_df[which(psuf_df$NOSOGRAFICO %in% noso_train),]

#prendo lo score dalla tabella di Chiara
score_modello <- defCasTable(conn, caslib = "Covid", tablename="score.sas7bdat")
score_modello_df <- to.casDataFrame(score_modello)
score_modello_df <- as.data.frame(score_modello_df)

psuf_df <- merge(psuf_df,score_modello_df,by = "NOSOGRAFICO")
psuf_df <- psuf_df[complete.cases(psuf_df),]

psuf_df_ALL <- merge(psuf_df_ALL,score_modello_df,by = "NOSOGRAFICO")
psuf_df_ALL <- psuf_df_ALL[complete.cases(psuf_df_ALL),]

#prendo il file a 48h dalla tabella di Dino
library(readr)
tbl_imputazione_pf_base_joined <- read_csv("tbl_imputazione_pf_base_joined.csv")
psuf_df <- merge(psuf_df,tbl_imputazione_pf_base_joined,by = "NOSOGRAFICO")

psuf_df <- psuf_df[,which(names(psuf_df) %in% c("SCORE","ETA","value","VALORE_REALE","VALORE_IMPUTATO_24h","DELTA_24",
                                                                           "VALORE_IMPUTATO","DELTA_48","VALORE_IMPUTATO_72h","DELTA_72"))]


psuf_df[,"BASELINE_cat"] <- NA
psuf_df[which(psuf_df$value <= 200),"BASELINE_cat"] <- "<200"
psuf_df[which(psuf_df$value > 200 & psuf_df$value <= 250 ),"BASELINE_cat"] <- "(200,250]"
psuf_df[which(psuf_df$value > 250 ),"BASELINE_cat"] <- ">250"

psuf_df[,"SCORE_cat"] <- NA
psuf_df[which(psuf_df$SCORE < 0.126),"SCORE_cat"] <- "low-risk"
psuf_df[which(psuf_df$SCORE >= 0.126 & psuf_df$SCORE < 0.366 ),"SCORE_cat"] <- "high-risk"
psuf_df[which(psuf_df$SCORE > 0.366 ),"SCORE_cat"] <- "very high-risk"


psuf_df[,"DELTA24_cat"] <- NA
psuf_df[which(psuf_df$DELTA_24 <= -21.488),"DELTA24_cat"] <- "1stQ"
psuf_df[which(psuf_df$DELTA_24 > -21.488 & psuf_df$DELTA_24 <= 23.800 ),"DELTA24_cat"] <- "1stQ-3rdQ"
psuf_df[which(psuf_df$DELTA_24 > 23.800 ),"DELTA24_cat"] <- "4thQ"

psuf_df[,"DELTA48_cat"] <- NA
psuf_df[which(psuf_df$DELTA_48 <= -36.938),"DELTA48_cat"] <- "1stQ"
psuf_df[which(psuf_df$DELTA_48 > -36.938 & psuf_df$DELTA_48 <= 40.212 ),"DELTA48_cat"] <- "1stQ-3rdQ"
psuf_df[which(psuf_df$DELTA_48 > 40.212 ),"DELTA48_cat"] <- "4thQ"

##################
# Basic scatter plot
psuf_df$SCORE_cat <- factor(psuf_df$SCORE_cat, levels = c("very high-risk", "low-risk", "high-risk"))

ggplot(psuf_df, aes(x=value, y=VALORE_IMPUTATO, color=SCORE_cat, shape=SCORE_cat)) +
  geom_point() + ylim(0, 700) + 
  geom_smooth(method=lm, aes(fill=SCORE_cat)) +
  xlab("Baseline P/F value") + ylab("48 Hours P/F")

psuf_df_high <- psuf_df[which(psuf_df$SCORE_cat == "high"),]
psuf_df_medium <- psuf_df[which(psuf_df$SCORE_cat == "medium"),]
psuf_df_low <- psuf_df[which(psuf_df$SCORE_cat == "low"),]
lm_high <- lm(data = psuf_df_high, formula = VALORE_IMPUTATO ~ value )
lm_medium <- lm(data = psuf_df_medium, formula = VALORE_IMPUTATO ~ value )
lm_low <- lm(data = psuf_df_low, formula = VALORE_IMPUTATO ~ value )


##################


#### SU TUTTI ma non ho i p/f... ####
psuf_df_ALL[,"BASELINE_cat"] <- NA
psuf_df_ALL[which(psuf_df_ALL$value <= 200),"BASELINE_cat"] <- "<200"
psuf_df_ALL[which(psuf_df_ALL$value > 200 & psuf_df_ALL$value <= 250 ),"BASELINE_cat"] <- "(200,250]"
psuf_df_ALL[which(psuf_df_ALL$value > 250 ),"BASELINE_cat"] <- ">250"

psuf_df_ALL[,"SCORE_cat"] <- NA
psuf_df_ALL[which(psuf_df_ALL$SCORE < 0.126),"SCORE_cat"] <- "low"
psuf_df_ALL[which(psuf_df_ALL$SCORE >= 0.126 & psuf_df_ALL$SCORE < 0.366 ),"SCORE_cat"] <- "medium"
psuf_df_ALL[which(psuf_df_ALL$SCORE > 0.366 ),"SCORE_cat"] <- "high"
#### SU TUTTI fine ####

chisq.test(table(psuf_df$BASELINE_cat,psuf_df$SCORE_cat))
chisq.test(table(psuf_df_ALL$BASELINE_cat,psuf_df_ALL$SCORE_cat))

psuf_df$SCORE_cat <- as.factor(psuf_df$SCORE_cat)
psuf_df_ALL$SCORE_cat <- as.factor(psuf_df_ALL$SCORE_cat)

# costruisco la variabile che avevo messo nel modello
psuf_df[,"DELTA_48_RIDUZ_PERC"] <- NA
psuf_df$DELTA_48_RIDUZ_PERC <- -(psuf_df$DELTA_48)/psuf_df$value
psuf_df$DELTA_48_RIDUZ_PERC <- psuf_df$DELTA_48_RIDUZ_PERC*100

psuf_df_middle <- psuf_df[which(psuf_df$BASELINE_cat == "(200,250]"),]

### GRAPHS BASELINE 

# density plot of baseline P/F by score category
library(plyr)
mu <- ddply(psuf_df, "SCORE_cat", summarise, grp.mean=mean(value))
head(mu)

ggplot(psuf_df, aes(x=value, color=SCORE_cat)) +
  geom_density()+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=SCORE_cat),
             linetype="dashed")

mu_ALL <- ddply(psuf_df_ALL, "SCORE_cat", summarise, grp.mean=mean(value))
head(mu_ALL)

ggplot(psuf_df_ALL, aes(x=value, color=SCORE_cat)) +
  geom_density()+
  geom_vline(data=mu_ALL, aes(xintercept=grp.mean, color=SCORE_cat),
             linetype="dashed")


#####################################################
mu <- ddply(psuf_df_middle, "SCORE_cat", summarise, grp.mean=mean(SCORE))
mu

#####################################################


# violin plot
ggplot(psuf_df, aes(x=value, y=SCORE_cat)) + 
  geom_violin(trim=T) + stat_summary(fun.y=median, geom="point", size=2, color="red") + 
  geom_vline(xintercept=275,
             linetype="dashed") + geom_boxplot(width=0.1) + ggtitle("Baseline P/F")

# violin plot
ggplot(psuf_df_ALL, aes(x=value, y=SCORE_cat)) + 
  geom_violin(trim=T) + stat_summary(fun.y=median, geom="point", size=2, color="red") + 
  geom_vline(xintercept=275,
             linetype="dashed") + geom_boxplot(width=0.1) + ggtitle("Baseline P/F")

## GRAPHS 24 H

# violin plot
ggplot(psuf_df, aes(x=VALORE_IMPUTATO_24h, y=SCORE_cat)) + 
  geom_violin(trim=T) + stat_summary(fun.y=median, geom="point", size=2, color="red") + 
  geom_vline(xintercept=275,
             linetype="dashed") + geom_boxplot(width=0.1) + ggtitle("24H P/F")

ggplot(psuf_df, aes(x=DELTA_24, y=SCORE_cat)) + 
  geom_violin(trim=T) + stat_summary(fun.y=median, geom="point", size=2, color="red") + 
  geom_vline(xintercept=0,
             linetype="dashed") + geom_boxplot(width=0.1) + ggtitle("24H Delta P/F")


## GRAPHS 48 H

# violin plot
ggplot(psuf_df, aes(x=VALORE_IMPUTATO, y=SCORE_cat)) + 
  geom_violin(trim=T) + stat_summary(fun.y=median, geom="point", size=2, color="red") + 
  geom_vline(xintercept=275,
             linetype="dashed") + geom_boxplot(width=0.1) + ggtitle("48H P/F")

# violin plot
ggplot(psuf_df, aes(x=DELTA_48, y=SCORE_cat)) + 
  geom_violin(trim=T) + stat_summary(fun.y=median, geom="point", size=2, color="red") + 
  geom_vline(xintercept=0,
             linetype="dashed") + geom_boxplot(width=0.1) + ggtitle("48H Delta P/F")


## GRAPHS 72 H

# violin plot
ggplot(psuf_df, aes(x=VALORE_IMPUTATO_72h, y=SCORE_cat)) + 
  geom_violin(trim=T) + stat_summary(fun.y=median, geom="point", size=2, color="red") + 
  geom_vline(xintercept=275,
             linetype="dashed") + geom_boxplot(width=0.1) + ggtitle("72H P/F")

# violin plot
ggplot(psuf_df, aes(x=DELTA_72, y=SCORE_cat)) + 
  geom_violin(trim=T) + stat_summary(fun.y=median, geom="point", size=2, color="red") + 
  geom_vline(xintercept=0,
             linetype="dashed") + geom_boxplot(width=0.1) + ggtitle("72H Delta P/F")



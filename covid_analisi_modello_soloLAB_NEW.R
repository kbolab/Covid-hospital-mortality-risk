library(ggplot2)
library(swat)
library(caret)
library(pROC)
library(rmarkdown)

##########################################
##########################################
### prendo la tabella psuf con l'outcome
##########################################
##########################################

load("esami_baseline_NEW.RData")
esami <- names(esami_baseline)

conn <- CAS('sas-isc.policlinicogemelli.it/cas-shared-default-http/', 443,'00653022','PG_2wr46yi8',protocol='https')

psuf <- defCasTable(conn, caslib = "Covid", tablename="p_su_f.sas7bdat")
psuf_df <- to.casDataFrame(psuf)

psuf_df_sub <- psuf_df[,which(names(psuf_df) %in% c("NOSOGRAFICO","DATA_INIZIO_CONTATTO","ETA","SESSO","STATO_RICOVERO"))]
psuf_baseline_sub_df <- psuf_df_sub[-which(duplicated(psuf_df_sub)),]

psuf_baseline_sub_df$DATA_INIZIO_CONTATTO <- as.Date(psuf_baseline_sub_df$DATA_INIZIO_CONTATTO,'1960-01-01')

##########################################
##########################################
### unisco gli esami al df p/f
##########################################
##########################################

dati_analisi <- psuf_baseline_sub_df

names(dati_analisi)[1]  <- "NOSO"
for(esame in names(esami_baseline)){
  names(esami_baseline[[esame]])[2] <- esame
  names(esami_baseline[[esame]])[3] <- paste0("datetime_",esame)
  dati_analisi <- merge(dati_analisi,esami_baseline[[esame]],by="NOSO",all.x = T)
}


na_esami <- list()
for(esame in names(esami_baseline)){
  na_esami[[esame]] <- length(which(is.na(dati_analisi[[esame]])))/length(dati_analisi[[esame]])
}
na_esami_df <- unlist(na_esami)

#seleziono quelli conm NA < x%
to_include <- names(na_esami_df[which(na_esami_df < 0.05)])

dati_analisi <- dati_analisi[which(dati_analisi$STATO_RICOVERO %in% c("Dimesso","Deceduto","Dimesso con Malattia")),]

dati_analisi$STATO_RICOVERO[which(dati_analisi$STATO_RICOVERO == "Dimesso con Malattia")] <- "Dimesso"

#in questa versione seleziono quelli con NA < x%
dati_analisi_modello <- dati_analisi[,which(names(dati_analisi) %in% c("DATA_INIZIO_CONTATTO","STATO_RICOVERO","ETA","SESSO","DDIMERO_cat",to_include))]

dati_analisi_report <- dati_analisi_modello[complete.cases(dati_analisi_modello),]

dati_analisi_modello_final <- dati_analisi_report

dati_analisi_modello_final[,"Sodio_IQR"] <- ""
dati_analisi_modello_final[which(dati_analisi_modello_final$`SODIOSiero_MEQ/L` <= 136 ),"Sodio_IQR"] <- "<=136"
dati_analisi_modello_final[which(dati_analisi_modello_final$`SODIOSiero_MEQ/L` > 136 & dati_analisi_modello_final$`SODIOSiero_MEQ/L` <= 141),"Sodio_IQR"] <- "(136-141)"
dati_analisi_modello_final[which(dati_analisi_modello_final$`SODIOSiero_MEQ/L` > 141 ),"Sodio_IQR"] <- ">=141"

dati_analisi_modello_final <- dati_analisi_modello_final[-which(dati_analisi_modello_final$SATO2_NUM_ > 99),]

dati_analisi_modello_final[,"SatO2_IQR"] <- ""
dati_analisi_modello_final[which(dati_analisi_modello_final$SATO2_NUM_ <= 93.4 ),"SatO2_IQR"] <- "0-25%"
dati_analisi_modello_final[which(dati_analisi_modello_final$SATO2_NUM_ > 93.4 & dati_analisi_modello_final$SATO2_NUM_ <= 97),"SatO2_IQR"] <- "26-75%"
dati_analisi_modello_final[which(dati_analisi_modello_final$SATO2_NUM_ > 97 ),"SatO2_IQR"] <- "76-100%"

################################################################

dati_analisi_modello_final[,"DECEDUTO"] <- 0
dati_analisi_modello_final[which(dati_analisi_modello_final$STATO_RICOVERO == "Deceduto"),"DECEDUTO"] <- 1
dati_analisi_modello_final <- dati_analisi_modello_final[,-1]
dati_analisi_modello_final_featsel <- dati_analisi_modello_final[,-which(names(dati_analisi_modello_final) %in% c("SatO2_IQR"))]

logitic_model_full <- step(glm(formula = DECEDUTO ~ . , family = "binomial", data = dati_analisi_modello_final))

n_iter <- 10
logitic_model <- list()
coeffs <- c()
for(i in 1:n_iter){
  set.seed(i)
  train_index <- sample(dim(dati_analisi_modello_final_featsel)[1], 0.8*dim(dati_analisi_modello_final_featsel)[1],replace = T)
  dati_analisi_modello_final_train <- dati_analisi_modello_final_featsel[train_index,]
  logitic_model[[i]] <- step(glm(formula = DECEDUTO ~ . , family = "binomial", data = dati_analisi_modello_final_train))
  coeffs <- c(names(logitic_model[[i]]$coefficients),coeffs)
}

sort(table(coeffs),decreasing = T)

#################################################
#################################################
#### PARTE LA CROSS VALIDATION
#################################################
#################################################
# define training control
train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                              classProbs = T, savePredictions = "all",summaryFunction=twoClassSummary)

dati_analisi_modello_final$DECEDUTO <- as.factor(dati_analisi_modello_final$DECEDUTO)
levels(dati_analisi_modello_final$DECEDUTO) <- c("dimesso","deceduto")


dati_analisi_modello_final$SatO2_IQR <- as.factor(dati_analisi_modello_final$SatO2_IQR)
dati_analisi_modello_final$SatO2_IQR<- factor(dati_analisi_modello_final$SatO2_IQR, levels = c("76-100%","26-75%","0-25%"))

dati_analisi_modello_final$Sodio_IQR <- as.factor(dati_analisi_modello_final$Sodio_IQR)
dati_analisi_modello_final$Sodio_IQR<- factor(dati_analisi_modello_final$Sodio_IQR, levels = c("(136-141)","<=136",">=141"))

model <- train(DECEDUTO ~ `PIASTRINESangue_X10^9/L`+
                 `EMOGLOBINASangue_G/DL` +
                 `AZOTO UREICOSiero_MG/DL`+
                 `PROTEINA C REATTIVASiero_MG/L` +
                 `GRANULOCITI NEUTROFILISangue_X10^9/L` +
                 Sodio_IQR +
                 ETA +
                 SatO2_IQR, 
               data = dati_analisi_modello_final,
               trControl = train_control,
               method = "glm",
               family=binomial(link = "logit"),metric="ROC")
#summary(model)

#per calcolare la ROC della cv ridefinisco la soglia a model$pred$pred
thr_logit <- as.numeric(table(dati_analisi_modello_final$DECEDUTO)[2]/sum(table(dati_analisi_modello_final$DECEDUTO)))
model$pred[which(model$pred[,"deceduto"] > thr_logit),"pred"] <- "deceduto"
model$pred[which(model$pred[,"deceduto"] <= thr_logit),"pred"] <- "dimesso"

val_folds <- c("Fold5.Rep1","Fold5.Rep2","Fold5.Rep3")
pred_val_folds <- model$pred[which(model$pred$Resample %in% val_folds),]


rr_cv <- roc(predictor = pred_val_folds$deceduto,response = pred_val_folds$obs)
plot(rr_cv, main = paste("AUC:",round(rr_cv$auc, 2)))

cv_table <- confusionMatrix(table(pred_val_folds$pred,pred_val_folds$obs),positive = "deceduto")
cv_table
#ora faccio il modello completo sul dataset
pred <- model$finalModel$fitted.values

#################################################
#################################################
#lo faccio anche con la solita glm per i valori shap
##devo pure cambiare momentaneamente i nomi per farli poi girare nell'app
#################################################
#################################################

dati_analisi_shap <- dati_analisi_modello_final

dati_analisi_shap[,"sodio136_input"] <- 0
dati_analisi_shap$sodio136_input[which(dati_analisi_shap$Sodio_IQR == "<=136")] <- 1

dati_analisi_shap[,"sodio141_input"] <- 0
dati_analisi_shap$sodio141_input[which(dati_analisi_shap$Sodio_IQR == ">=141")] <- 1

dati_analisi_shap[,"so297_input"] <- 0
dati_analisi_shap$so297_input[which(dati_analisi_shap$SatO2_IQR == "26-75%")] <- 1

dati_analisi_shap[,"so294_input"] <- 0
dati_analisi_shap$so294_input[which(dati_analisi_shap$SatO2_IQR == "0-25%")] <- 1

names(dati_analisi_shap)[which(names(dati_analisi_shap)=="ETA")] <- "eta_input"
names(dati_analisi_shap)[which(names(dati_analisi_shap)=="EMOGLOBINASangue_G/DL")] <- "emoglobina_input"
names(dati_analisi_shap)[which(names(dati_analisi_shap)=="AZOTO UREICOSiero_MG/DL")] <- "ureico_input"
names(dati_analisi_shap)[which(names(dati_analisi_shap)=="PIASTRINESangue_X10^9/L")] <- "piastrine_input"
names(dati_analisi_shap)[which(names(dati_analisi_shap)=="PROTEINA C REATTIVASiero_MG/L")] <- "pcr_input"
names(dati_analisi_shap)[which(names(dati_analisi_shap)=="GRANULOCITI NEUTROFILISangue_X10^9/L")] <- "neutrofili_input"

dati_analisi_shap <- dati_analisi_shap[,-which(names(dati_analisi_shap) %in% c("SESSO",
                                                                               "BILIRUBINA TOTALESiero_MG/DL",
                                                                               "BODY MASS INDEX_",
                                                                               "CALCIOSiero_MG/DL",
                                                                               "CREATINCHINASISiero_UI/L",
                                                                               "CREATININASiero_MG/DL",
                                                                               "EMATOCRITOSangue_%",
                                                                               "GLOBULI BIANCHISangue_X10^9/L",
                                                                               "GLUCOSIOSiero_MG/DL",
                                                                               "LINFOCITISangue_X10^9/L",
                                                                               "SODIOSiero_MEQ/L",
                                                                               "WEIGHT_KILOGRAMMI",
                                                                               "Sodio_IQR",
                                                                               "SatO2_IQR",
                                                                               "SATO2_NUM_",
                                                                               "POTASSIOSiero_MEQ/L"))]

model_logit_shap <- glm(data = dati_analisi_shap, formula = DECEDUTO ~ ., family = binomial)

save(model_logit_shap,file = "model_logit_shap.RData")

rr <- roc(predictor = pred,response = dati_analisi_modello_final$DECEDUTO)
plot(rr, main = paste("AUC:",round(rr$auc, 2)))
pred_bin <- pred

thr_logit <- coords(rr, x="best", input="threshold", best.method="youden")[1] # Same than last line

pred_bin[which(pred > thr_logit)] <- 1
pred_bin[which(pred <= thr_logit)] <- 0
pred_bin <- as.factor(pred_bin)
levels(pred_bin) <- c("dimesso","deceduto")

library(caret)
actual <- dati_analisi_modello_final$DECEDUTO
actual <- as.factor(actual)
cm <- confusionMatrix(pred_bin,actual,positive = "deceduto")


aa <- as.data.frame(cbind(pred,"DECEDUTO" = dati_analisi_modello_final[,c("DECEDUTO")]))

pred_data_sort <- aa[order(pred),] 
ggplot(pred_data_sort, aes(x= seq(1:dim(aa)[1]),y = pred)) +
  geom_point(aes(color = factor(DECEDUTO))) + xlab("patients") + ylab("prediction")

# calibration plot
library(classifierplots)
aa$DECEDUTO <- aa$DECEDUTO - 1
calibration_plot(aa$DECEDUTO, aa$pred)
classifierplots(aa$DECEDUTO, aa$pred)
classifierplots_folder(aa$DECEDUTO, aa$pred, "./", height = 7, width = 7)


lift <- function(depvar, predcol, groups=10) {
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
  helper = data.frame(cbind(depvar, predcol))
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(depvar), funs(total = n(),
                                    totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

df = lift(aa$DECEDUTO, aa$pred, groups = 10)
names(df)[1] <- "Decile"

library(dplyr); library(ggplot2)

df2 <- df %>%
  add_row(Decile = 0, Gain =0) %>%
  arrange(Decile)

ggplot(df2, aes(Decile,  Gain)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0,100)) +
  scale_x_continuous(breaks = c(1:10)) +
  labs(title = "Cumulative Gains Plot",
       y = "Cumulative Gain %")


ggplot(df, aes(Decile,  Cumlift)) +
  geom_point() +
  geom_line() +
  scale_y_continuous() +
  scale_x_continuous(breaks = c(1:10)) +
  labs(title = "Cumulative Lift Plot",
       y = "Cumulative Lift %")

#gruppi di rischio
t1 <- quantile(pred_data_sort$pred,probs = c(.70))
t2 <- quantile(pred_data_sort$pred,probs = c(.90))

pred_data_sort_mild <- pred_data_sort[which(pred_data_sort$pred < 0.1258),]
pred_data_sort_high <- pred_data_sort[which(pred_data_sort$pred >= 0.1258 & pred_data_sort$pred <= t2),]
pred_data_sort_veryhigh <- pred_data_sort[which(pred_data_sort$pred > t2),]


table(pred_data_sort_mild$DECEDUTO)[2]/sum(table(pred_data_sort_mild$DECEDUTO))
table(pred_data_sort_high$DECEDUTO)[2]/sum(table(pred_data_sort_high$DECEDUTO))
table(pred_data_sort_veryhigh$DECEDUTO)[2]/sum(table(pred_data_sort_veryhigh$DECEDUTO))

fileName.PDF<-'./modello_baseline_decesso_LAB.html'
render("./report_modello_decesso_LAB.Rmd", "html_document", fileName.PDF, output_dir="report")

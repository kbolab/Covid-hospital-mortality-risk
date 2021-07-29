library(swat)
library(pROC)
library(caret)
library(ggplot2)
library(classifierplots)

conn <- CAS('sas-isc.policlinicogemelli.it/cas-shared-default-http/', 443,'00653022','PG_2wr46yi8',protocol='https')

score_modello <- defCasTable(conn, caslib = "Covid", tablename="score.sas7bdat")
score_modello_df <- to.casDataFrame(score_modello)

#li prendo dal training
load("/media/kbocalc/Data/Progetti/Progetti/Progetti/COVID/NOSO_train.RData")
noso_train <- dati_analisi$NOSO



test_data <- score_modello_df[-which(score_modello_df$NOSOGRAFICO %in% noso_train),]
test_data <- test_data[-which(test_data$D_STATUSG == "Malattia in corso"),]
test_data <- test_data[complete.cases(test_data),]
#test_data <- test_data[,] # al 2/2/2021
test_data$DECESSO <- 0
test_data[which(test_data$D_STATUSG == "Deceduto"),"DECESSO"] <- 1
rr_test <- roc(predictor = test_data$SCORE,response = test_data$DECESSO)
plot(rr_test, main = paste("AUC:",round(rr_test$auc, 2)))

thr_logit <- coords(rr, x="best", input="threshold", best.method="youden")[1]

pred_bin <- test_data$SCORE
pred_bin[which(test_data$SCORE > .115)] <- 1
pred_bin[which(test_data$SCORE <= .115)] <- 0
pred_bin <- as.factor(pred_bin)

actual <- test_data$DECESSO
actual <- as.factor(actual)
cm <- confusionMatrix(pred_bin,actual,positive = "1")

aa <- as.data.frame(cbind("pred"=test_data$SCORE,"DECEDUTO" = test_data$DECESSO))

f_t = test_data$SCORE
o_t = test_data$DECESSO
mean((f_t - o_t)^2)

pred_data_sort <- aa[order(aa$pred),]
ggplot(pred_data_sort, aes(x= seq(1:dim(aa)[1]),y = pred)) +
  geom_point(aes(color = factor(DECEDUTO))) + xlab("patients") + ylab("prediction")

#decision curve
library(rmda)
full.model <- decision_curve(DECEDUTO~pred,
                             data = aa,
                             #confidence.intervals = "none",
                             fitted.risk = TRUE, 
                             thresholds = seq(0, .5, by = .1),
                             bootstraps = 10) 


plot_decision_curve(full.model,curve.names = "Model")


# calibration plot
calibration_plot(aa$DECEDUTO, aa$pred)
classifierplots(aa$DECEDUTO, aa$pred)
classifierplots_folder(aa$DECEDUTO, aa$pred, "./", height = 7, width = 7)

library(caret)
aa$DECEDUTO <- as.factor(aa$DECEDUTO)
calCurve <- calibration(x = DECEDUTO ~ pred, data = aa,cuts = 20)
calCurve$data$Percent <- 100 - calCurve$data$Percent
calCurve$data$Lower <- 100 - calCurve$data$Lower
calCurve$data$Upper <- 100 - calCurve$data$Upper
xyplot(calCurve, lwd = 3, pch = 19, cex = 2,
       xlab="Predicited probability")

model_cal <- lm(calCurve$data$midpoint ~ calCurve$data$Percent)
summary(model_cal)



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
# t1 <- quantile(pred_data_sort$pred,probs = c(.70))
# t2 <- quantile(pred_data_sort$pred,probs = c(.90))

t1 <- quantile(pred_data_sort$pred,probs = c(.25))
t2 <- quantile(pred_data_sort$pred,probs = c(.75))

pred_data_sort_mild <- pred_data_sort[which(pred_data_sort$pred <= t1),]
pred_data_sort_high <- pred_data_sort[which(pred_data_sort$pred > t1 & pred_data_sort$pred <= t2),]
pred_data_sort_veryhigh <- pred_data_sort[which(pred_data_sort$pred > t2),]

# pred_data_sort_mild <- pred_data_sort[which(pred_data_sort$pred <= t1),]
# pred_data_sort_high <- pred_data_sort[which(pred_data_sort$pred > t1 & pred_data_sort$pred <= t2),]
# pred_data_sort_veryhigh <- pred_data_sort[which(pred_data_sort$pred > t2),]


table(pred_data_sort_mild$DECEDUTO)[2]/sum(table(pred_data_sort_mild$DECEDUTO))
table(pred_data_sort_high$DECEDUTO)[2]/sum(table(pred_data_sort_high$DECEDUTO))
table(pred_data_sort_veryhigh$DECEDUTO)[2]/sum(table(pred_data_sort_veryhigh$DECEDUTO))



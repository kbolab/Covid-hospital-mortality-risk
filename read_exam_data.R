library(swat)
conn <- CAS('sas-isc.policlinicogemelli.it/cas-shared-default-http/', 443,'','',protocol='https')

#lista tabelle in memory nella caslib
cas.table.tableInfo(object = conn, caslib = 'Covid')
cas.table.caslibInfo(object = conn)

# import in R una tabella in memory dal CAS
DIARIO_MISURE <- defCasTable(conn, caslib = "Covid", tablename="DIARIO_MISURE")
analis_lab_freqESAME <- cas.simple.freq(DIARIO_MISURE$d_measure)

ANALISI_LABORATORIO.grouped <- defCasTable(conn, caslib = "Covid", tablename="DIARIO_MISURE",groupby=c("d_measure"))

#PER LAVORARCI IN LOCALE MA E' TROPPO GRANDE... LA FILTRO PRIMA PER GLI ESAMI CHE CI INTERESSANO
esami <- c("ALBUMINA","COLESTEROLO","D-DIMERO","BILIRUBINA","Body Mass Index","CALCIO","CREATINCHINASI",
           "EMOGLOBINA","FERRITINA","FIO2_mag40","FIO2_min40","GLUCOSIO","GLOBULI BIANCHI","GRANULOCITI NEUTROFILI",
           "LDL COLESTEROLO","Oxygen Saturation","PCR","PIASTRINE","PO2","POTASSIO","PROCALCITONINA",
           "SatO2_num","SODIO","TRIGLICERIDI","Weight","ACIDO URICO","AZOTO UREICO","BILIRUBINA TOTALE","EMATOCRITO","LINFOCITI","PCR","PIASTRINE","PROTEINA C REATTIVA","SODIO","Weight")


dataframe_esami <- list()
for(esame in esami){
  cat(esame)
  cat("\n")
  dataframe_esami[[esame]] <- to.casDataFrame(DIARIO_MISURE[DIARIO_MISURE$d_measure == esame,])
  cat("\n")
}

##########################################
##########################################
### prendo i baseline laboratorio
##########################################
##########################################
esami_baseline <- list()
dataframe_um_tables <- list()
dataframe_materiale_tables <- list()
dataframe_um_materiale_tables <- list()
for(esame in esami_new){
  i <- 1
  cat(esame)
  cat("\n")
  esami_baseline[[esame]] <- as.data.frame(matrix(NA,nrow = length(unique(dataframe_esami[[esame]]$NOSOGRAFICO)),ncol = 3))
  names(esami_baseline[[esame]]) <- c("NOSO","value","datetime")
  for(noso in unique(dataframe_esami[[esame]]$NOSOGRAFICO)){
    tmp <- dataframe_esami[[esame]][which(dataframe_esami[[esame]]$NOSOGRAFICO == noso),]
    tmp_sort <- tmp[with(tmp,order(datahour)),]
    baseline_noso <- tmp_sort[1,]
    esami_baseline[[esame]][i,"NOSO"] <- as.character(baseline_noso$NOSOGRAFICO)
    esami_baseline[[esame]][i,"value"] <- baseline_noso$value
    esami_baseline[[esame]][i,"datetime"] <- as.character(baseline_noso$datahour)
    
    i <- i + 1
  }
}

#save(esami_baseline, file = "esami_baseline_NEW.RData")
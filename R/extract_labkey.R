#' Extract data from Labkey
#'
#' @param rodbc_chan channel odbc created by labkey_connect{RLabkey}. Connexion à Labkey créée au préalable
#' @param protocols a vector of character (length 3 = protocol code). Un vecteur contenant les codes protocoles à 3 lettres Labkey souhaités
#' @param annees a vector of years. Un vecteur d'années à 4 chiffre.
#' @param cultures a vector of character (length 1 = code culture). Un vecteur contenant l'ensemble des codes à 1 lettre labkey correspond aux cultures souhaitées.
#' @param essais a vector of character (max length = 11). Un vecteur pouvant contenir des "bouts" de codes essais, donc de 1 à 11 caractères
#' @param na.col a logical. FALSE pour ne pas conserver les variables vides, TRUE pour les conserver
#'
#' @return a dataframe
#' @export
#' @importFrom glue glue
#' @import magrittr
#' @importFrom RODBC sqlQuery
#' @import dplyr
#' @examples

extract_labkey <- function(rodbc_chan, cultures=NULL, protocols=NULL, annees=NULL, essais=NULL, na.col=FALSE){

  #Table experiment
  experiment <- extract_experiment(rodbc_chan, cultures=cultures, protocols=protocols, annees=annees, essais=essais)

  #Design
  design <- sqlQuery(rodbc_chan, paste("SELECT * FROM DESIGN WHERE DESIGN_ID IN ('",
                                paste(experiment$DESIGN_ID[!is.na(experiment$DESIGN_ID)],collapse="','"),"')",
                                sep=""))

  experiment <- design %>%
    select(DESIGN_ID,DESIGN_NAME,DEVICE) %>%
    right_join(experiment,by="DESIGN_ID")

  #Données
  data_L0<-sqlQuery(rodbc_chan, paste("SELECT * FROM ", paste(experiment$PROTOCOL_NAME,"_L0",sep="")," WHERE EXPERIMENT_ID IN ('",paste(experiment$EXPERIMENT_ID,collapse="','"),"')",sep=""))
  data_L1<-sqlQuery(rodbc_chan, paste("SELECT * FROM ", paste(experiment$PROTOCOL_NAME,"_L1",sep="")," WHERE EXPERIMENT_ID IN ('",paste(experiment$EXPERIMENT_ID,collapse="','"),"')",sep=""))
  data_L2<-sqlQuery(rodbc_chan, paste("SELECT * FROM ", paste(experiment$PROTOCOL_NAME,"_L2",sep="")," WHERE EXPERIMENT_ID IN ('",paste(experiment$EXPERIMENT_ID,collapse="','"),"')",sep=""))
  data_L3<-sqlQuery(rodbc_chan, paste("SELECT * FROM ", paste(experiment$PROTOCOL_NAME,"_L3",sep="")," WHERE EXPERIMENT_ID IN ('",paste(experiment$EXPERIMENT_ID,collapse="','"),"')",sep=""))

  #Merge L0
  data_L0<-data_L0 %>%
    full_join(experiment,by="EXPERIMENT_ID")


  #Ajout code essais dans les données
  data_L3<-data_L0 %>%
    select(EXPERIMENT_ID,EXPERIMENT_NAME) %>%
    right_join(data_L3,by="EXPERIMENT_ID")
  data_L2<-data_L0 %>%
    select(EXPERIMENT_ID,EXPERIMENT_NAME,DESIGN_ID,DESIGN_NAME,DEVICE) %>%
    right_join(data_L2,by="EXPERIMENT_ID")
  data_L1<-data_L0 %>%
    select(EXPERIMENT_ID,EXPERIMENT_NAME) %>%
    right_join(data_L1,by="EXPERIMENT_ID")


  #Modalités
  index_s1<-sqlQuery(rodbc_chan, paste("SELECT * FROM INDEX_VARIETE_S1 WHERE ID_S1 IN ('" ,paste(data_L1$ID_S1,collapse="','"),"')",sep=""))

  data_L1<-index_s1 %>%
    select(ID_S1,C0000) %>%
    right_join(data_L1,by="ID_S1")

  data_L2<-data_L1 %>%
    select(C0000,ID_L1,EXPERIMENT_ID) %>%
    distinct() %>%
    right_join(data_L2,by=c("ID_L1","EXPERIMENT_ID"))

  data_L3<-data_L1 %>%
    select(C0000,ID_L1,EXPERIMENT_ID) %>%
    distinct() %>%
    right_join(data_L3,by=c("ID_L1","EXPERIMENT_ID"))


  #Bloc colonne du L2
  design_plot <- sqlQuery(rodbc_chan,paste("SELECT * FROM DESIGN_PLOT WHERE DESIGN_ID IN ('",paste(data_L0$DESIGN_ID[!is.na(data_L0$DESIGN_ID)],collapse="','"),"')",sep=""))

  data_L2 <- design_plot %>%
    select(NUM_REP,NUM_MOD,DESIGN_ID,PARC_CODE) %>%
    full_join(data_L2,by=c("DESIGN_ID","PARC_CODE"))

  #Supression des colonnes vides
  if (!na.col){
    data_L0<-data_L0 %>% select_if(colSums(!is.na(.)) > 0)
    data_L1<-data_L1 %>% select_if(colSums(!is.na(.)) > 0)
    data_L2<-data_L2 %>% select_if(colSums(!is.na(.)) > 0)
    data_L3<-data_L3 %>% select_if(colSums(!is.na(.)) > 0)
  }



  #Nom des variables
  variables<-sqlQuery(rodbc_chan, "SELECT * FROM DYN_USER_COLUMN")%>%
    mutate_if(is.factor,as.character)

  dyn_user_column_info<-sqlQuery(rodbc_chan,paste("SELECT * FROM DYN_USER_COLUMN_INFO WHERE TABLE_CONTEXT_ID IN ('",paste(data_L0$EXPERIMENT_ID,collapse="','"),"')",sep="")) %>%
    mutate_if(is.factor,as.character)
  obs_information<-sqlQuery(rodbc_chan, paste("SELECT * FROM OBS_INFORMATION WHERE INFO_ID IN ('",paste(unique(dyn_user_column_info$INFO_ID),collapse ="','"),"')",sep=""))


  variables<-variables %>%
    select(COLUMN_NAME,LIBELLE)

  dyn_user_column_info<-obs_information %>%
    select(INFO_ID,INFO_NAME) %>%
    mutate_if(is.factor,as.character) %>%
    full_join(dyn_user_column_info,by="INFO_ID") %>%
    left_join(variables,by="COLUMN_NAME")


  data_L2b<-data_L2[,FALSE]
  for (c in 1:ncol(data_L2)){
    if (colnames(data_L2)[c] %in% variables$COLUMN_NAME){
      name <- variables %>%
        filter(COLUMN_NAME==colnames(data_L2)[c]) %>%
        pull(LIBELLE) %>% as.character()

      data_L2b<-cbind(data_L2b,data_L2 %>% select(colnames(data_L2)[c]))
      colnames(data_L2b)[ncol(data_L2b)]<-name

      if (colnames(data_L2)[c] %in% dyn_user_column_info$COLUMN_NAME){
        info<-dyn_user_column_info %>%
          filter(COLUMN_NAME==colnames(data_L2)[c])

        for (INFO in unique(info$INFO_NAME)){
          data_L2b[,paste(name,gsub(" ","_",INFO),sep="_")]=NA
        }
        for (i in 1:nrow(info)){
          e<-as.factor(info$TABLE_CONTEXT_ID)[i]
          data_L2b[data_L2b$EXPERIMENT_ID==e,paste(name,gsub(" ","_",info[i,"INFO_NAME"]),sep="_")]<-as.character(info[i,"INFO_VALUE"])
        }
      }
      colnames(data_L2)[c]<-name
    } else {
      data_L2b<-cbind(data_L2b,data_L2 %>% select(colnames(data_L2)[c]))
    }
  }



  data_L1b<-data_L1[,FALSE]
  for (c in 1:ncol(data_L1)){
    if (colnames(data_L1)[c] %in% variables$COLUMN_NAME){
      name<-variables %>%
        filter(COLUMN_NAME==colnames(data_L1)[c]) %>%
        pull(LIBELLE) %>% as.character()

      data_L1b<-cbind(data_L1b,data_L1 %>% select(colnames(data_L1)[c]))
      colnames(data_L1b)[ncol(data_L1b)]<-name

      if (colnames(data_L1)[c] %in% dyn_user_column_info$COLUMN_NAME){
        info<-dyn_user_column_info %>%
          filter(COLUMN_NAME==colnames(data_L1)[c])

        for (INFO in unique(info$INFO_NAME)){
          data_L1b[,paste(name,gsub(" ","_",INFO),sep="_")]=NA
        }
        for (i in 1:nrow(info)){
          e<-as.factor(info$TABLE_CONTEXT_ID)[i]
          data_L1b[data_L1b$EXPERIMENT_ID==e,paste(name,gsub(" ","_",info[i,"INFO_NAME"]),sep="_")]<-as.character(info[i,"INFO_VALUE"])
        }
      }
      colnames(data_L1)[c]<-name
    } else {
      data_L1b<-cbind(data_L1b,data_L1 %>% select(colnames(data_L1)[c]))
    }
  }




  data_L0b<-data_L0[,FALSE]
  for (c in 1:ncol(data_L0)){
    if (colnames(data_L0)[c] %in% variables$COLUMN_NAME){
      name<-variables %>%
        filter(COLUMN_NAME==colnames(data_L0)[c]) %>%
        pull(LIBELLE) %>% as.character()

      data_L0b<-cbind(data_L0b,data_L0 %>% select(colnames(data_L0)[c]))
      colnames(data_L0b)[ncol(data_L0b)]<-name

      if (colnames(data_L0)[c] %in% dyn_user_column_info$COLUMN_NAME){
        info<-dyn_user_column_info %>%
          filter(COLUMN_NAME==colnames(data_L0)[c])

        for (INFO in unique(info$INFO_NAME)){
          data_L0b[,paste(name,gsub(" ","_",INFO),sep="_")]=NA
        }
        for (i in 1:nrow(info)){
          e<-as.factor(info$TABLE_CONTEXT_ID)[i]
          data_L0b[data_L0b$EXPERIMENT_ID==e,paste(name,gsub(" ","_",info[i,"INFO_NAME"]),sep="_")]<-as.character(info[i,"INFO_VALUE"])
        }
      }
      colnames(data_L0)[c]<-name
    } else {
      data_L0b<-cbind(data_L0b,data_L0 %>% select(colnames(data_L0)[c]))
    }
  }



  data_L3b<-data_L3[,FALSE]
  for (c in 1:ncol(data_L3)){
    if (colnames(data_L3)[c] %in% variables$COLUMN_NAME){
      name<-variables %>%
        filter(COLUMN_NAME==colnames(data_L3)[c]) %>%
        pull(LIBELLE) %>% as.character()

      data_L3b<-cbind(data_L3b,data_L3 %>% select(colnames(data_L3)[c]))
      colnames(data_L3b)[ncol(data_L3b)]<-name

      if (colnames(data_L3)[c] %in% dyn_user_column_info$COLUMN_NAME){
        info<-dyn_user_column_info %>%
          filter(COLUMN_NAME==colnames(data_L3)[c])

        for (INFO in unique(info$INFO_NAME)){
          data_L3b[,paste(name,gsub(" ","_",INFO),sep="_")]=NA
        }
        for (i in 1:nrow(info)){
          e<-as.factor(info$TABLE_CONTEXT_ID)[i]
          data_L3b[data_L3b$EXPERIMENT_ID==e,paste(name,gsub(" ","_",info[i,"INFO_NAME"]),sep="_")]<-as.character(info[i,"INFO_VALUE"])
        }
      }
      colnames(data_L3)[c]<-name
    } else {
      data_L3b<-cbind(data_L3b,data_L3 %>% select(colnames(data_L3)[c]))
    }
  }

  return(list(data_L0=data_L0,data_L1=data_L1,data_L1_b=data_L1b,data_L2=data_L2,data_L2b=data_L2b,data_L3=data_L3,data_L3b=data_L3b))

}

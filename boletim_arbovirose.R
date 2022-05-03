library("lubridate")
library("stringr")
library("tidyverse")
library("foreign")
library("writexl")
library("dplyr")
library("rio")
library("base")
library("purrr")

#comentar mais e escrever o que cada objeto e também lembrar da semana epidemiológica

############### DESCOMPACTAR ARQUIVOS ARBOVIROSE ###############################
#abrir arquivos dengue
getwd()
data_pasta_dengue = list.files(pattern = '^DENGON', recursive = TRUE)

extrator = function(data_pasta_dengue){
  read.dbf(data_pasta_dengue)
}

data_dengue = map_dfr(data_pasta_dengue, extrator)

data_dengue = filter(data_dengue, data_dengue$SG_UF == 25)

#abrir arquivos chik
data_pasta_chik = list.files(pattern = '^CHIKON', recursive = TRUE)

extrator1 = function(data_pasta_chik){
  read.dbf(data_pasta_chik)
}

data_chik = map_dfr(data_pasta_chik, extrator)
data_chik = filter(data_chik, data_chik$SG_UF == 25)

#abrir arquivos zika
data_pasta_zika = list.files(pattern = '^NINDINET', recursive = TRUE)

extrator1 = function(data_pasta_zika){
  read.dbf(data_pasta_zika)
}

data_pasta_zika = map_dfr(data_pasta_zika, extrator)
data_zika = filter(data_pasta_zika, data_pasta_zika$ID_AGRAVO == "A928")
data_zika = filter(data_zika, data_zika$SG_UF == 25)

#################### LIMPAR DADOS E AJUSTE DE CLASSES #########################
#função remover acentos
RemoveAcentos <- function(textoComAcentos) {
  if(!is.character(textoComAcentos)){
    on.exit()
  }
  letrasComAcentos <- "??????????????????????????????????????????????????Ǵ`^~?"
  letrasSemAcentos <- "aeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC     "
  textoSemAcentos <- chartr(
    old = letrasComAcentos,
    new = letrasSemAcentos,
    x = textoComAcentos
  ) 
  return(textoSemAcentos)
}

RemoveAcentos(data_dengue$NM_PACIENT)
RemoveAcentos(data_chik$NM_PACIENT)
RemoveAcentos(data_zika$NM_PACIENT)

#ajustar numeric
data_dengue$RESUL_PCR_ <- as.numeric(levels(data_dengue$RESUL_PCR_))[data_dengue$RESUL_PCR_]
data_chik$RESUL_PCR_ <- as.numeric(levels(data_chik$RESUL_PCR_))[data_chik$RESUL_PCR_]

#ajustar data
data_dengue$DT_SIN_PRI <- as.Date(data_dengue$DT_SIN_PRI, format="%d-%m-%Y")
data_dengue$DT_NASC <- as.Date(data_dengue$DT_NASC, format="%d-%m-%Y")

############################# CRIACAO DE CHAVE ###############################
#criar id pessoa
#criação de ID_Pessoa dengue
id_pessoa_dengue <- paste(data_dengue$NM_PACIENT,
                          data_dengue$DT_NASC,
                          data_dengue$ID_MN_RESI)

#criação de ID_Pessoa zika
id_pessoa_zika <- paste(data_zika$NM_PACIENT,
                          data_zika$DT_NASC,
                          data_zika$ID_MN_RESI)

#criação de ID_Pessoa chik
id_pessoa_chik <- paste(data_chik$NM_PACIENT,
                        data_chik$DT_NASC,
                        data_chik$ID_MN_RESI)

#acrescentar coluna com ID_Pessoa
data_dengue <- mutate(data_dengue, id_pessoa_dengue)
data_zika <- mutate(data_zika, id_pessoa_zika)
data_chik <- mutate(data_chik, id_pessoa_chik)

################## ORDENAR INICIO DE SINTOMAS E FILTRAR OS VALIDOS #############
#ordenar por início de sintomas
data_dengue <- arrange(data_dengue, data_dengue$DT_SIN_PRI)
data_chik <- arrange(data_chik, data_chik$DT_SIN_PRI)
data_zika <- arrange(data_zika, data_zika$DT_SIN_PRI)

#separar registros que tem início de sintomas anterior a data de surgimento de
#dengue, zika e chik
data_dengue_menor_2007 <- filter(data_dengue, data_dengue$DT_SIN_PRI < 2007)
data_chik_menor_2016 <- filter(data_chik, data_chik$DT_SIN_PRI < 2016)
data_zika_menor_2016 <- filter(data_zika, data_zika$DT_SIN_PRI < 2016)

#separar registros válidos para análise
data_dengue <- filter(data_dengue, data_dengue$DT_SIN_PRI >=2007)
data_chik <- filter(data_chik, data_chik$DT_SIN_PRI >=2016)
data_zika <- filter(data_zika, data_zika$DT_SIN_PRI >=2016)

#separar registros válidos para análise por data
data_dengue <- filter(data_dengue, data_dengue$DT_SIN_PRI > "2006-12-31")
data_chik <- filter(data_chik, data_chik$DT_SIN_PRI > "2015-12-31")
data_zika <- filter(data_zika, data_zika$DT_SIN_PRI > "2015-12-31")

#filtrar registros válidos para 2022 contando da semana epidemiológica
data_dengue <- filter(data_dengue, data_dengue$DT_SIN_PRI > "2022-01-01")

#dengue
#grupo1 (confirmados)
data_dengue_pcr <- filter(data_dengue, data_dengue$RESUL_PCR_ == "1")
data_dengue_outros_positivos <-   filter(data_dengue,
                                      data_dengue$RESUL_SORO == "1" |
                                      data_dengue$RESUL_NS1 == "1" |
                                      data_dengue$RESUL_VI_N == "1" |
                                      data_dengue$HISTOPA_N == "1" |
                                      data_dengue$IMUNOH_N == "1")
                                        

data_dengue_clin_epidemio <- filter(data_dengue,
                             data_dengue$CRITERIO =="2")

#confirmados_dengue (laboratorial e clínico epidemiológico)
data_dengue_confirmados <- bind_rows(data_dengue_pcr, 
                                     data_dengue_outros_positivos,
                                     data_dengue_clin_epidemio)
############################ REGRA DOS 90 DIAS PARA OS CONFIMRADOS #############
#regra de 90 dias para dengue
#dentro do mesmo ano epidemiológico, se o id_pessoa for o mesmo, o registro a ser
#contado será o de DT_SIN_PRI mais antigo e depois de 90 dias
DT_dengue_90_dias <- arrange(data_dengue_confirmados, data_dengue_confirmados$NM_PACIENT)

#preciso pegar somente os duplicados e aplicar a regra
d1 <- subset(data_dengue_confirmados, select = c("NU_NOTIFIC", "DT_SIN_PRI","id_pessoa_dengue"))
d1 <- arrange(d1, d1$id_pessoa_dengue)  

#testando **lembrar de pegar somente confirmado
#vê quando IS for nulo**

d2 <- d1
d3 <- table(d2$id_pessoa_dengue)
d3 <- data.frame(d3)
d3 <- rename(d3, id_pessoa_dengue = Var1)
d4 <- left_join(d2, d3, by = "id_pessoa_dengue")
d4 <- filter(d4, d4$Freq >= 2)

d4 <- arrange(d4, d4$id_pessoa_dengue, d4$DT_SIN_PRI)
#o by deve ser aqui pra depois fazer a diferença

d4 <- d4 %>% mutate(diff =  d4$DT_SIN_PRI - lag (d4$DT_SIN_PRI)) #falta especificar o by
d4 <- rename(d4, nu_dias = diff)
d4$nu_dias[is.na(d4$nu_dias)] <- 0

#lembrar de retirar (mesmo id_pessoa e dt_sin_pri)
d4 <- d4 %>% group_split(d4$id_pessoa_dengue)




d4 <- aggregate(d4$DT_SIN_PRI,
                by = list(d4$id_pessoa_dengue),
                FUN = diff) ### pode estar no caminho



############################# COMENTAR O RESTANTE #########################

data_dengue_confirmados <- data_dengue_confirmados[!duplicated(data_dengue_confirmados$id_pessoa_dengue), ]

data_dengue_confirmados <- subset(data_dengue_confirmados, 
                             select = -c(NU_NOTIFIC,
                                         ID_REGIONA,
                                         NM_PACIENT,
                                         FONETICA_N,
                                         SOUNDEX,
                                         ID_CNS_SUS,
                                         NM_MAE_PAC,
                                         NM_BAIRRO,
                                         ID_LOGRADO,
                                         NM_LOGRADO,
                                         NU_NUMERO,
                                         NM_COMPLEM,
                                         NM_REFEREN,
                                         NU_CEP,
                                         NU_DDD_TEL,
                                         NU_TELEFON,
                                         DDD_HOSP,
                                         TEL_HOSP,
                                         NOBAIINF,
                                         DS_OBS,
                                         NU_LOTE_V,
                                         NU_LOTE_H,
                                         IDENT_MICR,
                                         id_pessoa_dengue))

marc_dengue0 <- data.frame("confirmados")
data_dengue_confirmados <- mutate(data_dengue_confirmados, marc_dengue0) 
data_dengue_confirmados <- rename(data_dengue_confirmados, marcador = X.confirmados.)

#grupo2 descartados_dengue (exame negativo e sem clínico epidemiológico)
data_dengue_descartados <- filter(data_dengue, data_dengue$CLASSI_FIN == "5")

data_dengue_descartados <- data_dengue_descartados[!duplicated(data_dengue_descartados$id_pessoa_dengue), ]
data_dengue_descartados <- subset(data_dengue_descartados, 
                                  select = -c(NU_NOTIFIC,
                                              ID_REGIONA,
                                              NM_PACIENT,
                                              FONETICA_N,
                                              SOUNDEX,
                                              ID_CNS_SUS,
                                              NM_MAE_PAC,
                                              NM_BAIRRO,
                                              ID_LOGRADO,
                                              NM_LOGRADO,
                                              NU_NUMERO,
                                              NM_COMPLEM,
                                              NM_REFEREN,
                                              NU_CEP,
                                              NU_DDD_TEL,
                                              NU_TELEFON,
                                              DDD_HOSP,
                                              TEL_HOSP,
                                              NOBAIINF,
                                              DS_OBS,
                                              NU_LOTE_V,
                                              NU_LOTE_H,
                                              IDENT_MICR,
                                              id_pessoa_dengue))

marc_dengue1 <- data.frame("descartados")
data_dengue_descartados <- mutate(data_dengue_descartados, marc_dengue1)
data_dengue_descartados <- rename(data_dengue_descartados, marcador = X.descartados.)

#grupo3 prováveis_dengue (todo mundo, menos os descartados)
data_dengue_sem_resul0 <- data_dengue %>% filter(is.na(data_dengue$RESUL_SORO))
data_dengue_sem_resul1 <- data_dengue %>% filter(is.na(data_dengue$RESUL_NS1))
data_dengue_sem_resul2 <- data_dengue %>% filter(is.na(data_dengue$RESUL_VI_N))
data_dengue_sem_resul3 <- data_dengue %>% filter(is.na(data_dengue$HISTOPA_N))
data_dengue_sem_resul4 <- data_dengue %>% filter(is.na(data_dengue$IMUNOH_N))
data_dengue_sem_resul5 <- data_dengue %>% filter(is.na(data_dengue$CLASSI_FIN))
data_dengue_sem_resul6 <- data_dengue %>% filter(is.na(data_dengue$RESUL_PCR_))
data_dengue_sem_resul7 <- filter(data_dengue,
                                 data_dengue$RESUL_SORO == "3" |
                                 data_dengue$RESUL_NS1 == "3" |
                                 data_dengue$RESUL_VI_N == "3" |
                                 data_dengue$HISTOPA_N == "3" |
                                 data_dengue$IMUNOH_N == "3")

data_dengue_sem_resul8 <- filter(data_dengue,
                                 data_dengue$RESUL_SORO == "4" |
                                 data_dengue$RESUL_NS1 == "4" |
                                 data_dengue$RESUL_VI_N == "4" |
                                 data_dengue$HISTOPA_N == "4" |
                                 data_dengue$IMUNOH_N == "4")


data_dengue_sem_resultado <- bind_rows(data_dengue_sem_resul0, data_dengue_sem_resul1,
                                       data_dengue_sem_resul2, data_dengue_sem_resul3,
                                       data_dengue_sem_resul4, data_dengue_sem_resul5,
                                       data_dengue_sem_resul6, data_dengue_sem_resul7,
                                       data_dengue_sem_resul8)

data_dengue_sem_resultado <- subset(data_dengue_sem_resultado, 
                          select = -c(NU_NOTIFIC,
                                      ID_REGIONA,
                                      NM_PACIENT,
                                      FONETICA_N,
                                      SOUNDEX,
                                      ID_CNS_SUS,
                                      NM_MAE_PAC,
                                      NM_BAIRRO,
                                      ID_LOGRADO,
                                      NM_LOGRADO,
                                      NU_NUMERO,
                                      NM_COMPLEM,
                                      NM_REFEREN,
                                      NU_CEP,
                                      NU_DDD_TEL,
                                      NU_TELEFON,
                                      DDD_HOSP,
                                      TEL_HOSP,
                                      NOBAIINF,
                                      DS_OBS,
                                      NU_LOTE_V,
                                      NU_LOTE_H,
                                      IDENT_MICR,
                                      id_pessoa_dengue))

marc_dengue2 <- data.frame("prováveis")
data_dengue_sem_resultado <- mutate(data_dengue_sem_resultado, marc_dengue2)
data_dengue_sem_resultado <- rename(data_dengue_sem_resultado, marcador = X.prováveis.)

data_dengue_prov <- filter(data_dengue, 
                           data_dengue$CLASSI_FIN == "10" |
                           data_dengue$CLASSI_FIN == "11" |
                          data_dengue$CLASSI_FIN == "12") 

data_dengue_prov <- subset(data_dengue_prov, 
                                    select = -c(NU_NOTIFIC,
                                                ID_REGIONA,
                                                NM_PACIENT,
                                                FONETICA_N,
                                                SOUNDEX,
                                                ID_CNS_SUS,
                                                NM_MAE_PAC,
                                                NM_BAIRRO,
                                                ID_LOGRADO,
                                                NM_LOGRADO,
                                                NU_NUMERO,
                                                NM_COMPLEM,
                                                NM_REFEREN,
                                                NU_CEP,
                                                NU_DDD_TEL,
                                                NU_TELEFON,
                                                DDD_HOSP,
                                                TEL_HOSP,
                                                NOBAIINF,
                                                DS_OBS,
                                                NU_LOTE_V,
                                                NU_LOTE_H,
                                                IDENT_MICR,
                                                id_pessoa_dengue))

data_dengue_prov <- mutate(data_dengue_prov, marc_dengue2)
data_dengue_prov <- rename(data_dengue_prov, marcador = X.prováveis.)

data_dengue_provaveis <- bind_rows(data_dengue_confirmados,
                                   data_dengue_prov,
                                   data_dengue_sem_resultado)

id_pessoa_dengue1 <- paste(data_dengue_provaveis$NM_PACIENT,
                           data_dengue_provaveis$DT_NASC,
                           data_dengue_provaveis$ID_MN_RESI)

data_dengue_provaveis <- mutate(data_dengue_provaveis, id_pessoa_dengue1)
data_dengue_provaveis <- data_dengue_provaveis[!duplicated(data_dengue_provaveis$id_pessoa_dengue1), ]

data_dengue_provaveis <- subset(data_dengue_provaveis, 
                              select = -c(id_pessoa_dengue1))

data_dengue_provaveis <- mutate(data_dengue_provaveis, marc_dengue2)
data_dengue_provaveis <- subset(data_dengue_provaveis, 
                              select = -c(marcador))

data_dengue_provaveis <- rename(data_dengue_provaveis, marcador = X.prováveis.)


#base para PBI
data_dengue_PBI <- bind_rows(data_dengue_confirmados,
                             data_dengue_descartados,
                             data_dengue_provaveis)

#chikungunya
#grupo1 (confirmados)
data_chik_pcr <- filter(data_chik, data_chik$RESUL_PCR_ == "1")
data_chik_outros_positivos <-   filter(data_chik,
                                       data_chik$RES_CHIKS1 == "1" |
                                       data_chik$RES_CHIKS2 == "1" |
                                       data_chik$RESUL_PRNT == "1" |
                                       data_chik$RESUL_SORO == "1" |
                                       data_chik$RESUL_VI_N == "1" |
                                       data_chik$HISTOPA_N == "1" |
                                       data_chik$IMUNOH_N == "1")

data_chik_clin_epidemio <- filter(data_chik,
                                  data_chik$CRITERIO =="2")

#confirmados_chik (laboratorial e clínico epidemiológico)
data_chik_confirmados <- bind_rows(data_chik_pcr, 
                                     data_chik_outros_positivos,
                                     data_chik_clin_epidemio)
data_chik_confirmados <- data_chik_confirmados[!duplicated(data_chik_confirmados$id_pessoa_chik), ]
data_chik_confirmados <- subset(data_chik_confirmados, 
                                select = -c(NU_NOTIFIC,
                                            ID_REGIONA,
                                            NM_PACIENT,
                                            FONETICA_N,
                                            SOUNDEX,
                                            ID_CNS_SUS,
                                            NM_MAE_PAC,
                                            NM_BAIRRO,
                                            ID_LOGRADO,
                                            NM_LOGRADO,
                                            NU_NUMERO,
                                            NM_COMPLEM,
                                            NM_REFEREN,
                                            NU_CEP,
                                            NU_DDD_TEL,
                                            NU_TELEFON,
                                            DDD_HOSP,
                                            TEL_HOSP,
                                            NOBAIINF,
                                            DS_OBS,
                                            NU_LOTE_V,
                                            NU_LOTE_H,
                                            IDENT_MICR,
                                            id_pessoa_chik))


marc_chik0 <- data.frame("confirmados")
data_chik_confirmados <- mutate(data_chik_confirmados, marc_chik0)
data_chik_confirmados <- rename(data_chik_confirmados, marcador = X.confirmados.)

#grupo2 descartados_chik (exame negativo e sem clínico epidemiológico)
data_chik_descartados <- filter(data_chik, data_chik$CLASSI_FIN == "5")

data_chik_descartados <- data_chik_descartados[!duplicated(data_chik_descartados$id_pessoa_chik), ]
data_chik_descartados <- subset(data_chik_descartados, 
                                select = -c(NU_NOTIFIC,
                                            ID_REGIONA,
                                            NM_PACIENT,
                                            FONETICA_N,
                                            SOUNDEX,
                                            ID_CNS_SUS,
                                            NM_MAE_PAC,
                                            NM_BAIRRO,
                                            ID_LOGRADO,
                                            NM_LOGRADO,
                                            NU_NUMERO,
                                            NM_COMPLEM,
                                            NM_REFEREN,
                                            NU_CEP,
                                            NU_DDD_TEL,
                                            NU_TELEFON,
                                            DDD_HOSP,
                                            TEL_HOSP,
                                            NOBAIINF,
                                            DS_OBS,
                                            NU_LOTE_V,
                                            NU_LOTE_H,
                                            IDENT_MICR,
                                            id_pessoa_chik))

marc_chik1 <- data.frame("descartados")
data_chik_descartados <- mutate(data_chik_descartados, marc_chik1)
data_chik_descartados <- rename(data_chik_descartados, marcador = X.descartados.)

#grupo3 prováveis_chik (todo mundo, menos os descartados)
data_chik_sem_resul0 <- data_chik %>% filter(is.na(data_chik$RES_CHIKS1))
data_chik_sem_resul1 <- data_chik %>% filter(is.na(data_chik$RES_CHIKS2))
data_chik_sem_resul2 <- data_chik %>% filter(is.na(data_chik$RESUL_PRNT))
data_chik_sem_resul3 <- data_chik %>% filter(is.na(data_chik$RESUL_SORO))
data_chik_sem_resul4 <- data_chik %>% filter(is.na(data_chik$RESUL_VI_N))
data_chik_sem_resul5 <- data_chik %>% filter(is.na(data_chik$HISTOPA_N))
data_chik_sem_resul6 <- data_chik %>% filter(is.na(data_chik$RESUL_PCR_))
data_chik_sem_resul7 <- data_chik %>% filter(is.na(data_chik$IMUNOH_N))
data_chik_sem_resul8 <- filter(data_chik,
                                 data_chik$RES_CHIKS1 == "3" |
                                   data_chik$RES_CHIKS2 == "3" |
                                   data_chik$RESUL_PRNT == "3" |
                                   data_chik$RESUL_SORO == "3" |
                                   data_chik$RESUL_VI_N == "3" |
                                   data_chik$HISTOPA_N == "3" |
                                   data_chik$IMUNOH_N == "3" |
                                   data_chik$RESUL_PCR_ == "3")

data_chik_sem_resul9 <- filter(data_chik,
                               data_chik$RES_CHIKS1 == "4" |
                                 data_chik$RES_CHIKS2 == "4" |
                                 data_chik$RESUL_PRNT == "4" |
                                 data_chik$RESUL_SORO == "4" |
                                 data_chik$RESUL_VI_N == "4" |
                                 data_chik$HISTOPA_N == "4" |
                                 data_chik$IMUNOH_N == "4" |
                                 data_chik$RESUL_PCR_ == "4")

data_chik_sem_resultado <- bind_rows(data_chik_sem_resul0, data_chik_sem_resul1,
                                       data_chik_sem_resul2, data_chik_sem_resul3,
                                       data_chik_sem_resul4, data_chik_sem_resul5,
                                       data_chik_sem_resul6, data_chik_sem_resul7,
                                       data_chik_sem_resul8, data_chik_sem_resul9)
                                     
data_chik_sem_resultado <- subset(data_chik_sem_resultado, 
                                    select = -c(NU_NOTIFIC,
                                                ID_REGIONA,
                                                NM_PACIENT,
                                                FONETICA_N,
                                                SOUNDEX,
                                                ID_CNS_SUS,
                                                NM_MAE_PAC,
                                                NM_BAIRRO,
                                                ID_LOGRADO,
                                                NM_LOGRADO,
                                                NU_NUMERO,
                                                NM_COMPLEM,
                                                NM_REFEREN,
                                                NU_CEP,
                                                NU_DDD_TEL,
                                                NU_TELEFON,
                                                DDD_HOSP,
                                                TEL_HOSP,
                                                NOBAIINF,
                                                DS_OBS,
                                                NU_LOTE_V,
                                                NU_LOTE_H,
                                                IDENT_MICR,
                                                id_pessoa_chik))

marc_chik2 <- data.frame("prováveis")
data_chik_sem_resultado <- mutate(data_chik_sem_resultado, marc_chik2)
data_chik_sem_resultado <- rename(data_chik_sem_resultado, marcador = X.prováveis.)

data_chik_prov <- filter(data_chik, 
                         data_chik$CLASSI_FIN == "10" |
                         data_chik$CLASSI_FIN == "11" |
                         data_chik$CLASSI_FIN == "12") 

data_chik_prov <- subset(data_chik_prov, 
                           select = -c(NU_NOTIFIC,
                                       ID_REGIONA,
                                       NM_PACIENT,
                                       FONETICA_N,
                                       SOUNDEX,
                                       ID_CNS_SUS,
                                       NM_MAE_PAC,
                                       NM_BAIRRO,
                                       ID_LOGRADO,
                                       NM_LOGRADO,
                                       NU_NUMERO,
                                       NM_COMPLEM,
                                       NM_REFEREN,
                                       NU_CEP,
                                       NU_DDD_TEL,
                                       NU_TELEFON,
                                       TEL_HOSP,
                                       NOBAIINF,
                                       DS_OBS,
                                       NU_LOTE_V,
                                       NU_LOTE_H,
                                       IDENT_MICR,
                                       id_pessoa_chik))

data_chik_prov <- mutate(data_chik_prov, marc_chik2)
data_chik_prov <- rename(data_chik_prov, marcador = X.prováveis.)

data_chik_provaveis <- bind_rows(data_chik_confirmados,
                                   data_chik_prov,
                                   data_chik_sem_resultado)

id_pessoa_chik1 <- paste(data_chik_provaveis$NM_PACIENT,
                         data_chik_provaveis$DT_NASC,
                         data_chik_provaveis$ID_MN_RESI)

data_chik_provaveis <- mutate(data_chik_provaveis, id_pessoa_chik1)

data_chik_provaveis <- data_chik_provaveis[!duplicated(data_chik_provaveis$id_pessoa_chik), ]

data_chik_provaveis <- subset(data_chik_provaveis, 
                              select = -c(id_pessoa_chik))

data_chik_provaveis <- mutate(data_chik_provaveis, marc_chik2)
data_chik_provaveis <- subset(data_chik_provaveis, 
                              select = -c(marcador))

data_chik_provaveis <- rename(data_chik_provaveis, marcador = X.prováveis.)

#base para PBI
data_chik_PBI <- bind_rows(data_chik_confirmados,
                             data_chik_descartados,
                             data_chik_provaveis)

#zika
#grupo1 (confirmados_zika)
data_zika_confirmados <- filter(data_zika, 
                    data_zika$CLASSI_FIN == 1)

data_zika_confirmados <- data_zika_confirmados[!duplicated(data_zika_confirmados$id_pessoa_zika), ]
data_zika_confirmados <- subset(data_zika_confirmados, 
                              select = -c(NU_NOTIFIC,
                                          ID_REGIONA,
                                          NM_PACIENT,
                                          FONETICA_N,
                                          SOUNDEX,
                                          ID_CNS_SUS,
                                          NM_MAE_PAC,
                                          ID_DISTRIT,
                                          ID_BAIRRO,
                                          NM_BAIRRO,
                                          ID_LOGRADO,
                                          NM_LOGRADO,
                                          NU_NUMERO,
                                          NM_COMPLEM,
                                          NM_REFEREN,
                                          NU_CEP,
                                          NU_DDD_TEL,
                                          NU_TELEFON,
                                          NOBAIINF,
                                          NU_LOTE_V,
                                          NU_LOTE_H,
                                          IDENT_MICR,
                                          id_pessoa_zika))

marc_zika0 <- data.frame("confirmados")
data_zika_confirmados <- mutate(data_zika_confirmados, marc_zika0)
data_zika_confirmados <- rename(data_zika_confirmados, marcador = X.confirmados.)

#grupo2 descartados_zika (exame negativo)
data_zika_descartados<-  filter(data_zika, 
                                data_zika$CLASSI_FIN == "2")

data_zika_descartados <- data_zika_descartados[!duplicated(data_zika_descartados$id_pessoa_zika), ]
data_zika_descartados <- subset(data_zika_descartados, 
                                select = -c(NU_NOTIFIC,
                                            ID_REGIONA,
                                            NM_PACIENT,
                                            FONETICA_N,
                                            SOUNDEX,
                                            ID_CNS_SUS,
                                            NM_MAE_PAC,
                                            ID_DISTRIT,
                                            ID_BAIRRO,
                                            NM_BAIRRO,
                                            ID_LOGRADO,
                                            NM_LOGRADO,
                                            NU_NUMERO,
                                            NM_COMPLEM,
                                            NM_REFEREN,
                                            NU_CEP,
                                            NU_DDD_TEL,
                                            NU_TELEFON,
                                            NOBAIINF,
                                            NU_LOTE_V,
                                            NU_LOTE_H,
                                            IDENT_MICR,
                                            id_pessoa_zika))

marc_zika1 <- data.frame("descartados")
data_zika_descartados <- mutate(data_zika_descartados, marc_zika1)
data_zika_descartados <- rename(data_zika_descartados, marcador = X.descartados.)

#grupo3 prováveis_zika (todo mundo, menos os descartados)
data_zika_sem_resul <- data_zika %>% filter(is.na(data_zika$CLASSI_FIN))

data_zika_sem_resultado <- data_zika_sem_resul

data_zika_sem_resultado <- data_zika_sem_resultado[!duplicated(data_zika_sem_resultado$id_pessoa_zika), ]

data_zika_sem_resultado <- subset(data_zika_sem_resultado, 
                                  select = -c(NU_NOTIFIC,
                                              ID_REGIONA,
                                              NM_PACIENT,
                                              FONETICA_N,
                                              SOUNDEX,
                                              ID_CNS_SUS,
                                              NM_MAE_PAC,
                                              ID_DISTRIT,
                                              ID_BAIRRO,
                                              NM_BAIRRO,
                                              ID_LOGRADO,
                                              NM_LOGRADO,
                                              NU_NUMERO,
                                              NM_COMPLEM,
                                              NM_REFEREN,
                                              NU_CEP,
                                              NU_DDD_TEL,
                                              NU_TELEFON,
                                              NOBAIINF,
                                              NU_LOTE_V,
                                              NU_LOTE_H,
                                              IDENT_MICR,
                                              id_pessoa_zika))

marc_zik2 <- data.frame("prováveis")
data_zika_sem_resultado <- mutate(data_zika_sem_resultado, marc_zik2)
data_zika_sem_resultado <- rename(data_zika_sem_resultado, marcador = X.prováveis.)

data_zika_provaveis <- bind_rows(data_zika_confirmados,
                                 data_zika_sem_resultado)

id_pessoa_zika1 <- paste(data_zika_provaveis$NM_PACIENT,
                         data_zika_provaveis$DT_NASC,
                         data_zika_provaveis$ID_MN_RESI)

data_zika_provaveis <- mutate(data_zika_provaveis, id_pessoa_zika1)

data_zika_provaveis <- data_zika_provaveis[!duplicated(data_zika_provaveis$id_pessoa_zika), ]

data_zika_provaveis <- subset(data_zika_provaveis, 
                                  select = -c(id_pessoa_zika1))

data_zika_provaveis <- mutate(data_zika_provaveis, marc_zik2)
data_zika_provaveis <- subset(data_zika_provaveis, 
                              select = -c(marcador))
data_zika_provaveis <- rename(data_zika_provaveis, marcador = X.prováveis.)


#base para PBI
data_zika_PBI <- bind_rows(data_zika_confirmados,
                             data_zika_descartados,
                             data_zika_provaveis)

#merge para uma única planilha
data_arbo <- full_join (data_dengue_PBI, data_chik_PBI)

data_arbo1 <- merge(data_arbo, data_zika_PBI, all = TRUE)

##saidas 
write.csv(data_dengue_PBI, "data_dengue_PBI.csv", row.names = FALSE)
write.csv(data_chik_PBI, "data_chik_PBI.csv", row.names = FALSE)
write.csv(data_zika_PBI, "data_zika_PBI.csv", row.names = FALSE)
write.csv(data_dengue_menor_2007, "dengue_menor_2007.csv", row.names = FALSE)
write.csv(data_chik_menor_2007, "chik_menor_2007.csv", row.names = FALSE)
write.csv(data_zika_menor_2016, "zika_menor_2016.csv", row.names = FALSE)
write.csv(data_arbo1, "data_arbo.csv", row.names = FALSE)
#####FIM
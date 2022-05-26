library("lubridate")
library("stringr")
library("tidyverse")
library("foreign")
library("writexl")
library("dplyr")
library("readxl")
library("rio")
library("base")
library("purrr")

############################ABRIR ARQUIVOS##########################################
getwd()
data_notindiv = list.files(pattern = '^NINDIN', recursive = TRUE)

extrator = function(data_notindiv){
  read.dbf(data_notindiv)
}

data_notindiv = map_dfr(data_notindiv, extrator)

CNES <- read.csv("C:/Users/NDTA/Desktop/Projeto Toxoplasmose/entradas/unidades_saude.csv", header = TRUE, sep = ",")

data_naf_toxo <- readxl::read_excel("C:/Users/NDTA/Desktop/Projeto Toxoplasmose/entradas/naf_toxo.xlsx", sheet = "Planilha1")

############################REMOVER ACENTOS#####################################
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

NM_pacient_ <- data.frame(RemoveAcentos(data_notindiv$NM_PACIENT))
NM_pacient_ <- str_replace_all(NM_pacient_$RemoveAcentos.data_notindiv.NM_PACIENT., " ", "")
NM_pacient_ <- data.frame(NM_pacient_)
data_notindiv <- cbind(data_notindiv, NM_pacient_)

########################RENOMEAR MUNICÍPIOS ####################################
#renomear código de município
cod_ibge <- c("250010", "250020", "250030", "250040", "250050", "250053",
              "250057", "250060", "250073", "250077", "250080", "250090",
              "250100", "250110", "250115", "250120", "250130", "250135",
              "250140", "250150", "250153", "250160", "250157", "250170",
              "250180", "250190", "250200", "250205", "250210", "250215",
              "250220", "250230", "250240", "250250", "250270", "250280",
              "250290", "250300", "250310", "250320", "250330", "250340",
              "250350", "250355", "250360", "250370", "250375", "250380",
              "250390", "250400", "250403", "250407", "250410", "250415",
              "250420", "250430", "250435", "250440", "250450", "250460",
              "250470", "250480", "250485", "250490", "250500", "250510",
              "250523", "250520", "250527", "250530", "250535", "250540",
              "250560", "250570", "250580", "250590", "250600", "250610",
              "250620", "250625", "250630", "250640", "250650", "250660",
              "250260", "250670", "250680", "250690", "250700", "250710",
              "250720", "250730", "250740", "250750", "251365", "250760",
              "250770", "250780", "250790", "250800", "250810", "250820",
              "250830", "250840", "250850", "250855", "250860", "250870",
              "250880", "250890", "250900", "250905", "250910", "250915",
              "250920", "250930", "250933", "250937", "250939", "250940",
              "250950", "250960", "250970", "250980", "250990", "251000",
              "251010", "251020", "251030", "251040", "251050", "251060",
              "251065", "251070", "251080", "251090", "251100", "251110",
              "251120", "251272", "251130", "251140", "251150", "251160",
              "251170", "251180", "251190", "251200", "251203", "251207",
              "251210", "251220", "251230", "251240", "251250", "251260",
              "251270", "251274", "251275", "251276", "251278", "251280",
              "251290", "251300", "251310", "251315", "251320", "251330",
              "251335", "251340", "251370", "251380", "251350", "251360",
              "251385", "251392", "251390", "251396", "251394", "251398",
              "251400", "250070", "251410", "251420", "251430", "251440",
              "251450", "251455", "251460", "251465", "251470", "251480",
              "251445", "251490", "251500", "251510", "251520", "251540",
              "251530", "251550", "251560", "251570", "251580", "251590",
              "251593", "251597", "251600", "251610", "251615", "251620",
              "251630", "251640", "251650", "251660", "251670", "251675",
              "251680", "251690", "251700", "251710", "251720", "250550",
              "251740")

cod_ibge <- data.frame(cod_ibge)
nm_municipios_PB <- c("Água Branca", "Aguiar", "Alagoa Grande", "Alagoa Nova",
                      "Alagoinha", "Alcantil","Algodão de Jandaíra", "Alhandra",
                      "Amparo", "Aparecida", "Araçagi", "Arara", "Araruna",
                      "Areia", "Areia de Baraúnas", "Areial", "Aroeiras",
                      "Assunção", "Baía da Traição", "Bananeiras", "Baraúna",
                      "Barra de Santa Rosa", "Barra de Santana", 
                      "Barra de São Miguel", "Bayeux", "Belém",
                      "Belém do Brejo do Cruz", "Bernardino Batista",
                      "Boa Ventura", "Boa Vista", "Bom Jesus", "Bom Sucesso",
                      "Bonito de Santa Fé", "Boqueirão", "Borborema",
                      "Brejo do Cruz", "Brejo dos Santos", "Caaporã",
                      "Cabaceiras", "Cabedelo", "Cachoeira dos Índios",
                      "Cacimba de Areia", "Cacimba de Dentro", "Cacimbas",
                      "Caiçara", "Cajazeiras", "Cajazeirinhas", "Caldas Brandão",
                      "Camalaú", "Campina Grande", "Capim", "Caraúbas",
                      "Carrapateira", "Casserengue", "Catingueira",
                      "Catolé do Rocha", "Caturité", "Conceição", "Condado",
                      "Conde", "Congo", "Coremas", "Coxixola", 
                      "Cruz do Espírito Santo", "Cubati", "Cuité",
                      "Cuité de Mamanguape", "Cuitegi", "Curral de Cima",
                      "Curral Velho", "Damião", "Desterro", "Diamante",
                      "Dona Inês", "Duas Estradas", "Emas", "Esperança",
                      "Fagundes", "Frei Martinho", "Gado Bravo", "Guarabira",
                      "Gurinhém", "Gurjão", "Ibiara", "Igaracy", "Imaculada",
                      "Ingá", "Itabaiana", "Itaporanga", "Itapororoca",
                      "Itatuba", "Jacaraú", "Jericó", "João Pessoa",
                      "Santarém", "Juarez Távora", "Juazeirinho",
                      "Junco do Seridó", "Juripiranga", "Juru", "Lagoa",
                      "Lagoa de Dentro", "Lagoa Seca", "Lastro", "Livramento",
                      "Logradouro", "Lucena", "Mãe d'Água", "Malta",
                      "Mamanguape", "Manaíra", "Marcação", "Mari", 
                      "Marizópolis", "Massaranduba", "Mataraca", "Matinhas",
                      "Mato Grosso", "Maturéia", "Mogeiro", "Montadas",
                      "Monte Horebe", "Monteiro", "Mulungu", "Natuba",
                      "Nazarezinho", "Nova Floresta", "Nova Olinda",
                      "Nova Palmeira", "Olho d'Água", "Olivedos", "Ouro Velho",
                      "Parari", "Passagem", "Patos", "Paulista", "Pedra Branca",
                      "Pedra Lavrada", "Pedras de Fogo", "Pedro Regis",
                      "Piancó", "Picuí", "Pilar", "Pilões","Pilõezinhos",
                      "Pirpirituba", "Pitimbu", "Pocinhos","Poço Dantas",
                      "Poço de José de Moura", "Pombal", "Prata",
                      "Princesa Isabel", "Puxinanã", "Queimadas", "Quixabá",
                      "Remígio", "Riachão", "Riachão do Bacamarte",
                      "Riachão do Poço", "Riacho de Santo Antônio",
                      "Riacho dos Cavalos", "Rio Tinto", "Salgadinho",
                      "Salgado de São Félix", "Santa Cecília", "Santa Cruz",
                      "Santa Helena", "Santa Inês", "Santa Luzia",
                      "Santa Rita", "Santa Teresinha", "Santana de Mangueira",
                      "Santana dos Garrotes", "Santo André", "São Bento de Pombal",
                      "São Bento", "São Domingos de Pombal", "São Domingos do Cariri",
                      "São Francisco", "São João do Cariri",
                      "São João do Rio do Peixe", "São João do Tigre",
                      "São José da Lagoa Tapada", "São José de Caiana",
                      "São José de Espinharas", "São José de Piranhas",
                      "São José de Princesa", "São José do Bonfim",
                      "São José do Brejo do Cruz", "São José do Sabugi",
                      "São José dos Cordeiros", "São José dos Ramos",
                      "São Mamede", "São Miguel de Taipu",
                      "São Sebastião de Lagoa de Roça","São Sebastião do Umbuzeiro",
                      "Seridó", "Sapé", "Serra Branca",
                      "Serra da Raiz", "Serra Grande", "Serra Redonda",
                      "Serraria", "Sertãozinho","Sobrado", "Solânea",
                      "Soledade", "Sossego", "Sousa", "Sumé", "Tacima",
                      "Taperoá", "Tavares", "Teixeira", "Tenório", "Triunfo",
                      "Uiraúna", "Umbuzeiro", "Várzea", "Vieirópolis",
                      "Vista Serrana", "Zabelê")

nm_municipios_PB <- data.frame(nm_municipios_PB)
municipios_PB <- c(cod_ibge, nm_municipios_PB) 
municipios_PB <- data.frame(municipios_PB)

#acrescentar RS
Região_de_Saúde <- c("11",	"7",	"3",	"3",	"2",	"15",	"3",	"1",	"5",	"10",	"2",	"3",
                     "2",	"3",	"6",	"3",	"15",	"16",	"14",	"2",	"4",	"4",	"15",	"15",	
                     "1",	"2",	"8",	"9",	"7",	"16",	"9",	"8",	"9",	"15",	"2",	"8",	
                     "8",	"1",	"15",	"1",	"9",	"6",	"2",	"6",	"2",	"9",	"13",	"12",	
                     "5",	"16",	"14",	"5",	"9",	"2",	"6",	"8",	"15",	"7",	"6",	"1",	
                     "5",	"7",	"5",	"1",	"4",	"4",	"14",	"2",	"14",	"7",	"4",	"6",
                     "7",	"2",	"2",	"6",	"3",	"16",	"4",	"15",	"2",	"12",	"5",	"7",	
                     "7",	"11",	"12",	"12",	"7",	"14",	"12",	"14",	"8",	"1",	"9",	"12",	
                     "16",	"6",	"12",	"11",	"13",	"2",	"3",	"10",	"5",	"2",	"1",	"6",
                     "6",	"14",	"11",	"14",	"1",	"10",	"16",	"14",	"3",	"8",	"6",	"12",	
                     "3",	"9",	"5",	"2",	"15",	"10",	"4",	"7",	"4",	"7",	"16",	"5",	
                     "5",	"6",	"6",	"13",	"7",	"4",	"12",	"14",	"7",	"4",	"12",	"2",
                     "2",	"2",	"1",	"16",	"9",	"9",	"13",	"5",	"11",	"16",	"15",	"6",
                     "3",	"2",	"12",	"1",	"15",	"8",	"14",	"6",	"12",	"15",	"10",	"9",
                     "7",	"6",	"1",	"6",	"7",	"7",	"16",	"13",	"8",	"13",	"15",	"10",	
                     "5",	"9",	"5",	"10",	"7",	"6",	"9",	"11",	"6",	"8",	"6",	"5",	
                     "12",	"6",	"12",	"3",	"5",	"4",	"1",	"5",	"2",	"7",	"16",	"2",
                     "2",	"1",	"2",	"16",	"4",	"10",	"5",	"2",	"16",	"11",	"6",	"16",	
                     "9",	"9",	"15",	"6",	"10",	"6",	"5")

Região_de_Saúde <- data.frame(Região_de_Saúde)
Região_de_Saúde_PB <- c(Região_de_Saúde, municipios_PB) 
Região_de_Saúde_PB <- data.frame(Região_de_Saúde_PB)

#acrescentar GRS
GRS1 <- c("11",	"7",	"3",	"3",	"2",	"3",	"3",	"1",	"5",	"10",	"2",	"3",
          "2",	"3",	"6",	"3",	"3",	"3",	"1",	"2",	"4",	"4",	"3",	"3",
          "1",	"2",	"8",	"9",	"7",	"3",	"9",	"8",	"9",	"3",	"2",	"8",
          "8",	"1",	"3",	"1",	"9",	"6",	"2",	"6",	"2",	"9",	"10",	"12",
          "5",	"3",	"1",	"5",	"9",	"2",	"6",	"8",	"3",	"7",	"6",	"1",
          "5",	"7",	"5",	"1",	"4",	"4",	"1",	"2",	"1",	"7",	"4",	"6",
          "7",	"2",	"2",	"6",	"3",	"3",	"4",	"3",	"2",	"12",	"5",	"7",
          "7",	"11",	"12",	"12",	"7",	"1",	"12",	"1",	"8",	"1",	"9",	"12",
          "3",	"6",	"12",	"11",	"10",	"2",	"3",	"10",	"3",	"2",	"1",	"6",
          "6",	"1",	"11",	"1",	"1",	"10",	"3",	"1",	"3",	"8",	"6",	"12",
          "3",	"9",	"5",	"2",	"3",	"10",	"4",	"7",	"4",	"7",	"3",	"5",
          "5",	"6",	"6",	"10",	"7",	"4",	"12",	"1",	"7",	"4",	"12",	"2",
          "2",	"2",	"1",	"3",	"9",	"9",	"10",	"5",	"11",	"3",	"3",	"6",
          "3",	"2",	"12",	"1",	"3",	"8",	"1",	"6",	"12",	"3",	"10",	"9",
          "7",	"6",	"1",	"6",	"7",	"7",	"3",	"10",	"8",	"10",	"3",	"10",
          "5",	"9",	"5",	"10",	"7",	"6",	"9",	"11",	"6",	"8",	"6",	"5",
          "12",	"6",	"12",	"3",	"5",	"4",	"1",	"5",	"2",	"7",	"3",	"2",
          "2",	"1",	"2",	"3",	"4",	"10",	"5",	"2",	"3",	"11",	"6",	"3",
          "9",	"9",	"3",	"6",	"10",	"6",	"5")

GRS1 <- data.frame(GRS1)
GRS_PB <- c(GRS1, municipios_PB) 
GRS_PB <- data.frame(GRS_PB)

#acrescentar macro
Macro <- c("3",	"3",	"2",	"2",	"1",	"2",	"2",	"1",	"2",	"3",	"1",	"2",
           "1",	"2",	"3",	"2",	"2",	"2",	"1",	"1",	"2",	"2",	"2",	"2",
           "1",	"1",	"3",	"3",	"3",	"2",	"3",	"3",	"3",	"2",	"1",	"3",
           "3",	"1",	"2",	"1",	"3",	"3",	"1",	"3",	"1",	"3",	"3",	"1",
           "2",	"2",	"1",	"2",	"3",	"1",	"3",	"3",	"2",	"3",	"3",	"1",
           "2",	"3",	"2",	"1",	"2",	"2",	"1",	"1",	"1",	"3",	"1",	"3",
           "3",	"1",	"1",	"3",	"2",	"2",	"2",	"2",	"1",	"1",	"2",	"3",
           "3",	"3",	"1",	"1",	"3",	"1",	"1",	"1",	"3",	"1",	"3",	"1",
           "2",	"3",	"1",	"3",	"3",	"1",	"2",	"3",	"2",	"1",	"1",	"3",
           "3",	"1",	"3",	"1",	"1",	"3",	"2",	"1",	"2",	"3",	"3",	"1",
           "2",	"3",	"2",	"1",	"2",	"3",	"2",	"3",	"2",	"3",	"2",	"2",
           "2",	"3",	"3",	"3",	"3",	"2",	"1",	"1",	"3",	"2",	"1",	"1",
           "1",	"1",	"1",	"2",	"3",	"3",	"3",	"2",	"3",	"2",	"2",	"3",
           "2",	"1",	"1",	"1",	"2",	"3",	"1",	"3",	"1",	"2",	"3",	"3",
           "3",	"3",	"1",	"3",	"3",	"3",	"2",	"3",	"3",	"3",	"2",	"3",
           "2",	"3",	"2",	"3",	"3",	"3",	"3",	"3",	"3",	"3",	"3",	"2",
           "1",	"3",	"1",	"2",	"2",	"2",	"1",	"2",	"1",	"3",	"2",	"1",
           "1",	"1",	"1",	"2",	"2",	"3",	"2",	"1",	"2",	"3",	"3",	"2",
           "3",	"3",	"2",	"3",	"3",	"3",	"2")

Macro <- data.frame(Macro)
Macro_PB <- c(Macro, municipios_PB) 
Macro_PB <- data.frame(Macro_PB)

#acrescentar GRS, RS e macro
municipios_PB <- left_join(municipios_PB, GRS_PB, by = "cod_ibge")
municipios_PB <- left_join(municipios_PB, Região_de_Saúde_PB, by = "cod_ibge")
municipios_PB <- left_join(municipios_PB, Macro_PB, by = "cod_ibge")

municipios_PB <- subset(municipios_PB,
                        select = -c(nm_municipios_PB.y,
                                    nm_municipios_PB.x.x,
                                    nm_municipios_PB.y.y))

municipios_PB <- rename(municipios_PB, GRS = GRS1)

#renomear ID_MN_RESI para o left_join
data_notindiv <- rename(data_notindiv, cod_ibge = ID_MN_RESI)
data_notindiv <- left_join(data_notindiv, municipios_PB, by = "cod_ibge")

#renomear municipio de notificação
CNES$cnes <- as.factor(CNES$cnes)
data_notindiv <- rename(data_notindiv, cnes = ID_UNIDADE)
data_notindiv <- left_join(data_notindiv, CNES, by = "cnes")

data_notindiv <- rename(data_notindiv, Unidade_notificadora = nome)
data_notindiv <- rename(data_notindiv, municipio_residencia = nm_municipios_PB.x)

####################REGISTROS VÁLIDOS ##########################################  
#filtrar por cid
toxo_B58 <- filter(data_notindiv, data_notindiv$ID_AGRAVO == 'B58')
toxo_B58 <- data.frame(toxo_B58)   

toxo_O98.6 <- filter(data_notindiv, data_notindiv$ID_AGRAVO == 'O986')
toxo_O98.6 <- data.frame(toxo_O98.6)   

toxo_P37.1 <- filter(data_notindiv, data_notindiv$ID_AGRAVO == 'P371')
toxo_P37.1 <- data.frame(toxo_P37.1)

##########################CRIAÇÃO DE CHAVES#####################################
#criar chave
id_pessoa_B58 <- paste(toxo_B58$NM_PACIENT_,
                       toxo_B58$DT_NASC,
                       toxo_B58$nm_municipios_PB)
                       
id_pessoa_O98.6 <- paste(toxo_O98.6$NM_PACIENT_,
                         toxo_O98.6$DT_NASC,
                         toxo_O98.6$nm_municipios_PB)

id_pessoa_P37.1 <- paste(toxo_P37.1$NM_PACIENT_,
                         toxo_P37.1$DT_NASC,
                         toxo_P37.1$nm_municipios_PB)

#acrescentar coluna com ID_Pessoa
toxo_B58 <- mutate(toxo_B58, id_pessoa_B58)
toxo_O98.6 <- mutate(toxo_O98.6, id_pessoa_O98.6)
toxo_P37.1 <- mutate(toxo_P37.1, id_pessoa_P37.1)

##################DUPLICIDADES##################################################
# separar duplicidades
sum(duplicated(toxo_B58$id_pessoa_B58))
sum(duplicated(toxo_O98.6$id_pessoa_O98.6))
sum(duplicated(toxo_P37.1$id_pessoa_P37.1))

duplicidade_B58 <- toxo_B58[duplicated(toxo_B58$id_pessoa_B58), ]
duplicidade_O98.6 <- toxo_O98.6[duplicated(toxo_O98.6$id_pessoa_O98.6), ]
duplicidade_P37.1 <- toxo_P37.1[duplicated(toxo_P37.1$id_pessoa_P37.1), ]

duplicidade_B58 <- subset(duplicidade_B58, 
                              select = -c(NM_pacient_,
                                           id,
                                           municipio_id,
                                          id_pessoa_B58))

duplicidade_O98.6 <- subset(duplicidade_O98.6, 
                          select = -c(NM_pacient_,
                                      id,
                                      municipio_id,
                                      id_pessoa_O98.6))

duplicidade_P37.1 <- subset(duplicidade_P37.1, 
                          select = -c(NM_pacient_,
                                      id,
                                      municipio_id,
                                      id_pessoa_P37.1))

######################INCONSISTÊNCIAS###########################################
#inconsistência B58 que é gestante
toxo_B58_gestante <- filter(toxo_B58, 
                            toxo_B58$CS_GESTANT != 5, 
                            toxo_B58$CS_GESTANT != 6)

toxo_B58_gestante <- subset(toxo_B58_gestante, 
                          select = -c(NM_pacient_,
                                      id,
                                      municipio_id,
                                      id_pessoa_B58))

#B58 gestante que ainda não está em O98.6
notificar_B58_em_O98.6 <- anti_join(toxo_B58_gestante, toxo_O98.6, 
                                    by.x = "id_pessoa_B58", by.y = "id_pessoa_O98.6")

#corrigir criterio de confirmação 
toxo_B58_corrigir_criterio <- filter(toxo_B58, 
                            toxo_B58$CRITERIO != 1)

toxo_B58_corrigir_criterio <- subset(toxo_B58_corrigir_criterio, 
                                    select = -c(NM_pacient_,
                                      id,
                                      municipio_id,
                                      id_pessoa_B58))

toxo_O98.6_corrigir_criterio <- filter(toxo_O98.6, 
                                  toxo_O98.6$CRITERIO != 1)

toxo_P37.1_corrigir_criterio <- filter(toxo_P37.1, 
                                  toxo_P37.1$CRITERIO != 1)

#corrigir sexo no O98.6
toxo_O98.6_corrigir_sexo <- filter(toxo_O98.6, 
                                   toxo_O98.6$CS_SEXO == 'M')

#corrigir campo gestante
toxo_O98.6_corrigir_campo_gestante <- filter(toxo_O98.6,
                                             toxo_O98.6$CS_GESTANT != 1,
                                             toxo_O98.6$CS_GESTANT != 2,
                                             toxo_O98.6$CS_GESTANT != 3)

#corrigir idade no O98.6, notificar corretamente
toxo_O98.6_corrigir_idade <- filter(toxo_O98.6, 
                                    toxo_O98.6$NU_IDADE_N < '4000')

#fechar evolução de O98.6
DT_toxo_O98.6 <- Sys.Date() - toxo_O98.6$DT_SIN_PRI
toxo_O98.6 <- mutate(toxo_O98.6, DT_toxo_O98.6)

toxo_O98.6_evo_vazia <- toxo_O98.6 %>% filter(is.na(toxo_O98.6$EVOLUCAO))
toxo_O98.6_corrigir_evolucao1 <- filter(toxo_O98.6,
                              toxo_O98.6$EVOLUCAO != 1,
                              toxo_O98.6$DT_toxo_O98.6 > '280 days')
toxo_O98.6_corrigir_evolucao <-  bind_rows(toxo_O98.6_evo_vazia, toxo_O98.6_corrigir_evolucao1)

#verificar bebês que não foram notificados
toxo_O98.6_para_P37.1 <- filter(toxo_O98.6,
                                toxo_O98.6$DT_toxo_O98.6 > '280 days')

notificar_RN_P37.1 <- anti_join(toxo_O98.6_para_P37.1, toxo_P37.1, 
                                   by.x = "NM_PACIENT", by.y = "NM_MAE_PAC")
#fechar evolução de P37.1
DT_toxo_P37.1 <- Sys.Date() - toxo_P37.1$DT_SIN_PRI
toxo_P37.1 <- mutate(toxo_P37.1, DT_toxo_P37.1)

toxo_P37.1_evo_vazia <- toxo_P37.1 %>% filter(is.na(toxo_P37.1$EVOLUCAO))
toxo_P37.1_corrigir_evolucao1 <- filter(toxo_P37.1,
                                        toxo_P37.1$EVOLUCAO != 1,
                                        toxo_P37.1$DT_toxo_P37.1 > '420 days')
toxo_P37.1_corrigir_evolucao <-  bind_rows(toxo_P37.1_evo_vazia, toxo_P37.1_corrigir_evolucao1)

#olhar pela planilha do naf quem tá sem medicação e quem tá sem notificação

################################SAIDAS##########################################
##saidas #nome da aba = #nome do objeto
export(list(duplicidade_B58 = duplicidade_B58,
            notificar_B58_em_O98.6 = notificar_B58_em_O98.6, 
            toxo_B58_corrigir_criterio = toxo_B58_corrigir_criterio),
       file = "Qualificar_B58.xlsx")

export(list(duplicidade_O98.6 = duplicidade_O98.6,
            toxo_O98.6_corrigir_criterio = toxo_O98.6_corrigir_criterio, 
            toxo_O98.6_corrigir_sexo = toxo_O98.6_corrigir_sexo,
            toxo_O98.6_campo_gestante = toxo_O98.6_corrigir_campo_gestante,
            toxo_O98.6_corrigir_idade = toxo_O98.6_corrigir_idade,
            toxo_O98.6_corrigir_evolucao = toxo_O98.6_corrigir_evolucao),
            file = "Qualificar_O98.6.xlsx")


export(list(duplicidade_P37.1 = duplicidade_P37.1,
            toxo_P37.1_corrigir_criterio = toxo_P37.1_corrigir_criterio, 
            notificar_RN_P37.1 = notificar_RN_P37.1,
            toxo_P37.1_corrigir_evolucao = toxo_P37.1_corrigir_evolucao),
       file = "Qualificar_P37.1.xlsx")
########FIM

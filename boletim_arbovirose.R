library("lubridate")
library("stringr")
library("tidyverse")
library("foreign")
library("writexl")
library("dplyr")
library("readxl")
library("base")
library("purrr")
library("rio")

#posteriormente, verificar contagem por semana epidemiológica
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
data_dengue$CLASSI_FIN <- as.numeric(levels(data_dengue$CLASSI_FIN))[data_dengue$CLASSI_FIN]
data_dengue$RESUL_SORO <- as.numeric(levels(data_dengue$RESUL_SORO))[data_dengue$RESUL_SORO]
data_dengue$RESUL_NS1 <- as.numeric(levels(data_dengue$RESUL_NS1))[data_dengue$RESUL_NS1]
data_dengue$RESUL_VI_N <- as.numeric(levels(data_dengue$RESUL_PCR_))[data_dengue$RESUL_VI_N]
data_dengue$HISTOPA_N <- as.numeric(levels(data_dengue$HISTOPA_N))[data_dengue$HISTOPA_N]
data_dengue$IMUNOH_N <- as.numeric(levels(data_dengue$RESUL_PCR_))[data_dengue$IMUNOH_N]
data_chik$RESUL_PCR_ <- as.numeric(levels(data_chik$RESUL_PCR_))[data_chik$RESUL_PCR_]
data_chik$CLASSI_FIN <- as.numeric(levels(data_chik$RESUL_PCR_))[data_chik$RESUL_PCR_]
data_chik$RES_CHIKS1 <- as.numeric(levels(data_chik$RESUL_PCR_))[data_chik$RESUL_PCR_]
data_chik$RES_CHIKS2 <- as.numeric(levels(data_chik$RESUL_PCR_))[data_chik$RESUL_PCR_]
data_chik$RESUL_PRNT <- as.numeric(levels(data_chik$RESUL_PCR_))[data_chik$RESUL_PCR_]
data_chik$RESUL_SORO <- as.numeric(levels(data_chik$RESUL_PCR_))[data_chik$RESUL_PCR_]
data_chik$RESUL_VI_N <- as.numeric(levels(data_chik$RESUL_PCR_))[data_chik$RESUL_PCR_]
data_chik$HISTOPA_N <- as.numeric(levels(data_chik$RESUL_PCR_))[data_chik$RESUL_PCR_]
data_chik$IMUNOH_N <- as.numeric(levels(data_chik$RESUL_PCR_))[data_chik$RESUL_PCR_]

data_chik[is.na(data_chik)] <- 0
data_zika[is.na(data_zika)] <- 0

#ajustar data
data_dengue$DT_SIN_PRI <- as.Date(data_dengue$DT_SIN_PRI, format="%d-%m-%Y")
data_dengue$DT_NASC <- as.Date(data_dengue$DT_NASC, format="%d-%m-%Y")

############################### RENOMEAR MUNICÍPIOS ############################
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
                      "Joca Claudino", "Juarez Távora", "Juazeirinho",
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
                      "Pedra Lavrada", "Pedras de Fogo", "Pedro Régis",
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
                      "Santana dos Garrotes", "Santo André", "São Bentinho",
                      "São Bento", "São Domingos", "São Domingos do Cariri",
                      "São Francisco", "São João do Cariri",
                      "São João do Rio do Peixe", "São João do Tigre",
                      "São José da Lagoa Tapada", "São José de Caiana",
                      "São José de Espinharas", "São José de Piranhas",
                      "São José de Princesa", "São José do Bonfim",
                      "São José do Brejo do Cruz", "São José do Sabugi",
                      "São José dos Cordeiros", "São José dos Ramos",
                      "São Mamede", "São Miguel de Taipu",
                      "São Sebastião de Lagoa de Roça","São Sebastião do Umbuzeiro",
                      "São Vicente do Seridó", "Sapé", "Serra Branca",
                      "Serra da Raiz", "Serra Grande", "Serra Redonda",
                      "Serraria", "Sertãozinho","Sobrado", "Solânea",
                      "Soledade", "Sossêgo", "Sousa", "Sumé", "Tacima",
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

#renomear ID_MN_RESI para o left_join GRS, RS e Macro
data_dengue <- dplyr::rename(data_dengue, cod_ibge = ID_MN_RESI)
data_dengue <- left_join(data_dengue, municipios_PB, by = "cod_ibge")

#renomear ID_MN_RESI para o left_join GRS, RS e Macro
data_chik <- dplyr::rename(data_chik, cod_ibge = ID_MN_RESI)
data_chik <- left_join(data_chik, municipios_PB, by = "cod_ibge")

#renomear ID_MN_RESI para o left_join GRS, RS e Macro
data_zika <- dplyr::rename(data_zika, cod_ibge = ID_MN_RESI)
data_zika <- left_join(data_zika, municipios_PB, by = "cod_ibge")

############################# CRIACAO DE CHAVE ###############################
#criar id pessoa
#criação de ID_Pessoa
id_pessoa_dengue <- paste(data_dengue$NM_PACIENT,
                          data_dengue$DT_NASC,
                          data_dengue$ID_MN_RESI) 

id_pessoa_zika <- paste(data_zika$NM_PACIENT,
                        data_zika$DT_NASC,
                        data_zika$ID_MN_RESI)

id_pessoa_chik <- paste(data_chik$NM_PACIENT,
                        data_chik$DT_NASC,
                        data_chik$ID_MN_RESI)

#remover espaços em branco
id_pessoa_dengue <- str_remove_all (id_pessoa_dengue, " ")
id_pessoa_zika <- str_remove_all (id_pessoa_zika, " ")
id_pessoa_chik <- str_remove_all (id_pessoa_chik, " ")

#acrescentar coluna com ID_Pessoa
data_dengue <- mutate(data_dengue, id_pessoa_dengue)
data_zika <- mutate(data_zika, id_pessoa_zika)
data_chik <- mutate(data_chik, id_pessoa_chik)

################## FILTRAR REGISTROS VALIDOS ###################################
#separar registros que tem início de sintomas anterior a data de surgimento de
#dengue, zika e chik (inconsistências)
data_dengue_menor_2007 <- filter(data_dengue, data_dengue$DT_SIN_PRI < 2007)
data_chik_menor_2016 <- filter(data_chik, data_chik$DT_SIN_PRI < 2016)
data_zika_menor_2016 <- filter(data_zika, data_zika$DT_SIN_PRI < 2016)

#separar registros válidos para análise
data_dengue <- filter(data_dengue, data_dengue$DT_SIN_PRI > "2022-01-01")
data_chik <- filter(data_chik, data_chik$DT_SIN_PRI > "2022-01-01")
data_zika <- filter(data_zika, data_zika$DT_SIN_PRI > "2022-01-01")

####################### REMOVER DUPLICIDADES ###################################
#remover duplicados de IS + id_pessoa
id_pessoa_dengue1 <- paste(data_dengue$DT_SIN_PRI, data_dengue$id_pessoa_dengue)
id_pessoa_dengue1 <- data.frame(id_pessoa_dengue1)
data_dengue <- mutate(data_dengue, id_pessoa_dengue1)
data_dengue <- data_dengue[!duplicated(data_dengue$id_pessoa_dengue1), ]

data_dengue <- subset(data_dengue, 
                       select = -c(id_pessoa_dengue1))

id_pessoa_chik1 <- paste(data_chik$DT_SIN_PRI, data_chik$id_pessoa_chik)
id_pessoa_chik1 <- data.frame(id_pessoa_chik1)
data_chik <- mutate(data_chik, id_pessoa_chik1)
data_chik <- data_chik[!duplicated(data_chik$id_pessoa_chik1), ]

data_chik <- subset(data_chik, 
                    select = -c(id_pessoa_chik1))

id_pessoa_zika1 <- paste(data_zika$DT_SIN_PRI, data_zika$id_pessoa_zika)
id_pessoa_zika1 <- data.frame(id_pessoa_zika1)
data_zika <- mutate(data_zika, id_pessoa_zika1)
data_zika <- data_zika[!duplicated(data_zika$id_pessoa_zika1), ]

data_zika <- subset(data_zika, 
                    select = -c(id_pessoa_zika1))

########################## CONTABILIZAÇÃO DE CASOS #############################
############### DENGUE CONFIRMADOS, DESCARTADOS E PROVÁVEIS ####################
#grupo 1 confirmados_dengue
#Dengue_confirmado_laboratorial
data_dengue_laboratorio <-   filter(data_dengue,
                                         data_dengue$RESUL_PCR_ == "1" |
                                          data_dengue$RESUL_SORO == "1" |
                                          data_dengue$RESUL_NS1 == "1" |
                                          data_dengue$RESUL_VI_N == "1" |
                                          data_dengue$HISTOPA_N == "1" |
                                          data_dengue$IMUNOH_N == "1" |
                                          data_dengue$RESUL_PRNT == "1")

#Dengue_possui_clínica
#verificar quem dos que tem febre tem pelo menos 2 manifestações clínicas
data_dengue_clinico <- filter(data_dengue, 
                              data_dengue$FEBRE == "1")
data_dengue_clinico <- data_dengue_clinico %>%
                          filter (data_dengue_clinico$NAUSEA == 1 |
                          data_dengue_clinico$VOMITO == 1 |
                          data_dengue_clinico$MIALGIA == 1 |
                          data_dengue_clinico$ARTRALGIA == 1 |
                          data_dengue_clinico$CEFALEIA == 1 |
                          data_dengue_clinico$DOR_RETRO == 1 |
                          data_dengue_clinico$PETEQUIAS == 1 |
                          data_dengue_clinico$PETEQUIA_N == 1 |
                          data_dengue_clinico$LACO == 1 |
                          data_dengue_clinico$LEUCOPENIA == 1 |
                          data_dengue_clinico$ALRM_HIPOT == 1 |
                          data_dengue_clinico$ALRM_PLAQ == 1 |
                          data_dengue_clinico$ALRM_VOM == 1 |
                          data_dengue_clinico$ALRM_SANG == 1 |
                          data_dengue_clinico$ALRM_HEMAT == 1 |
                          data_dengue_clinico$ALRM_ABDOM == 1 |
                          data_dengue_clinico$ALRM_LETAR == 1 |
                          data_dengue_clinico$ALRM_HEPAT == 1 |
                          data_dengue_clinico$ALRM_LIQ == 1 |
                          data_dengue_clinico$GRAV_PULSO == 1 |
                          data_dengue_clinico$GRAV_CONV == 1 |
                          data_dengue_clinico$GRAV_ENCH == 1 |
                          data_dengue_clinico$GRAV_INSUF == 1 |
                          data_dengue_clinico$GRAV_TAQUI == 1 |
                          data_dengue_clinico$GRAV_EXTRE == 1 |
                          data_dengue_clinico$GRAV_HIPOT == 1 |
                          data_dengue_clinico$GRAV_HEMAT == 1 |
                          data_dengue_clinico$GRAV_MELEN == 1 |
                          data_dengue_clinico$GRAV_METRO == 1 |
                          data_dengue_clinico$GRAV_SANG == 1 |
                          data_dengue_clinico$GRAV_AST == 1 |
                          data_dengue_clinico$GRAV_MIOC == 1 |
                          data_dengue_clinico$GRAV_CONSC == 1 |
                          data_dengue_clinico$GRAV_ORGAO == 1 |
                          data_dengue_clinico$MANI_HEMOR == 1 |
                          data_dengue_clinico$EPISTAXE == 1 |
                          data_dengue_clinico$GENGIVO == 1 |
                          data_dengue_clinico$METRO == 1 |
                          data_dengue_clinico$PETEQUIAS == 1 |
                          data_dengue_clinico$HEMATURA == 1 |
                          data_dengue_clinico$SANGRAM == 1)

sinais_sintomas_clinico <- data_dengue_clinico %>% 
  pivot_longer(cols = c ("NAUSEA", 
                         "VOMITO",
                         "MIALGIA",
                         "ARTRALGIA",
                         "CEFALEIA",
                         "DOR_RETRO", 
                         "PETEQUIAS", 
                         "PETEQUIA_N", 
                         "LACO", 
                         "LEUCOPENIA", 
                         "ALRM_HIPOT", 
                         "ALRM_PLAQ", 
                         "ALRM_VOM",
                         "ALRM_SANG", 
                         "ALRM_HEMAT", 
                         "ALRM_ABDOM", 
                         "ALRM_LETAR", 
                         "ALRM_HEPAT", 
                         "ALRM_LIQ", 
                         "GRAV_PULSO", 
                         "GRAV_CONV", 
                         "GRAV_ENCH", 
                         "GRAV_INSUF", 
                         "GRAV_TAQUI", 
                         "GRAV_EXTRE", 
                         "GRAV_HIPOT", 
                         "GRAV_HEMAT", 
                         "GRAV_MELEN", 
                         "GRAV_METRO", 
                         "GRAV_SANG", 
                         "GRAV_AST", 
                         "GRAV_MIOC", 
                         "GRAV_CONSC", 
                         "GRAV_ORGAO", 
                         "MANI_HEMOR", 
                         "EPISTAXE", 
                         "GENGIVO", 
                         "METRO", 
                         "PETEQUIAS", 
                         "HEMATURA", 
                         "SANGRAM"),
               names_to = c("sintomas"),
               values_to = "valores")  

#filtrei somente quem tem sinais e sintomas
sinais_sintomas_clinico <- filter(sinais_sintomas_clinico,
                                  sinais_sintomas_clinico$valores == "1")                  

#criei data.frame pra separar id que tem pelo menos e sinais e sintomas
sinais <- table(sinais_sintomas_clinico$id_pessoa_dengue) %>% data.frame(.)
sinais <- filter(sinais, sinais$Freq >= 2)
sinais <- rename(sinais, id_pessoa_dengue = Var1)

#leftjoin para saber quem tem pelo menos 2 ou mais sinais e sintomas
data_dengue_clinico <- left_join(data_dengue_clinico, sinais, by = "id_pessoa_dengue", keep = FALSE)

#retirei NA
data_dengue_clinico <- filter(data_dengue_clinico, data_dengue_clinico$Freq >= "2")
data_dengue_clinico <- subset(data_dengue_clinico, 
                              select = -c(Freq))

####retirar todos os laboratoriais para ficar somente os casos clínicos
data_dengue_clinico <- filter(data_dengue_clinico, 
                                    data_dengue_clinico$RESUL_PCR_ != "1" |
                                      is.na(data_dengue_clinico$RESUL_PCR_))
  
 data_dengue_clinico <- filter(data_dengue_clinico, 
                                    data_dengue_clinico$RESUL_SORO != "1" |
                                      is.na(data_dengue_clinico$RESUL_SORO))

data_dengue_clinico <- filter(data_dengue_clinico, 
                                    data_dengue_clinico$RESUL_NS1 != "1" |
                                      is.na(data_dengue_clinico$RESUL_NS1))
  
data_dengue_clinico <- filter(data_dengue_clinico, 
                                    data_dengue_clinico$RESUL_VI_N != "1" |
                                      is.na(data_dengue_clinico$RESUL_VI_N)) 
  
data_dengue_clinico <- filter(data_dengue_clinico, 
                                    data_dengue_clinico$HISTOPA_N != "1" |
                                      is.na(data_dengue_clinico$HISTOPA_N)) 

data_dengue_clinico <- filter(data_dengue_clinico, 
                                    data_dengue_clinico$IMUNOH_N != "1" |
                                      is.na(data_dengue_clinico$IMUNOH_N))

data_dengue_clinico <- filter(data_dengue_clinico, 
                                    data_dengue_clinico$RESUL_PRNT != "1" |
                                      is.na(data_dengue_clinico$RESUL_PRNT))

#pelo guia de vigilância, clínico epidemiologico é: Na impossibilidade de 
#realização de confirmação laboratorial específica ou para casos com
#resultados laboratoriais inconclusivos, deve-se considerar a confirmação por vínculo epidemiológico
#com um caso confirmado laboratorialmente, após avaliação da distribuição espacial dos casos
#confirmados.

data_dengue_clin_epidemiologico <- filter(data_dengue_clinico,
                                          data_dengue_clinico$CRITERIO == "2")

#Junção de confirmados dengue (laboratorial e clínico_epidemiológico)
data_dengue_confirmados <- bind_rows(data_dengue_laboratorio, 
                                     data_dengue_clin_epidemiologico)

#após junção de linhas, retirar possíveis duplicidades
id_pessoa_dengue1 <- paste(data_dengue_confirmados$DT_SIN_PRI, data_dengue_confirmados$id_pessoa_dengue)
id_pessoa_dengue1 <- data.frame(id_pessoa_dengue1)
data_dengue_confirmados <- mutate(data_dengue_confirmados, id_pessoa_dengue1)
data_dengue_confirmados <- data_dengue_confirmados[!duplicated(data_dengue_confirmados$id_pessoa_dengue1), ]
data_dengue_confirmados <- subset(data_dengue_confirmados, 
                                select = -c(id_pessoa_dengue1))

#regra de 90 dias para data_dengue_confirmados
#mesmo id_pessoa só pode ter 4 tipos de dengue ao longo da vida
#ordenar nome_paciente e id_pessoa_dengue
DT_dengue_90_dias <- data_dengue_confirmados %>% arrange(data_dengue_confirmados$DT_SIN_PRI) 
DT_dengue_90_dias <- DT_dengue_90_dias %>% arrange(DT_dengue_90_dias$NM_PACIENT)

#verificar frequências
freq_DT_90_dias <- table(DT_dengue_90_dias$id_pessoa_dengue) 
freq_DT_90_dias <- data.frame(freq_DT_90_dias)
freq_DT_90_dias <- rename(freq_DT_90_dias, id_pessoa_dengue = Var1)
#adicionar freq no objeto DT_dengue_90_dias
DT_dengue_90_dias <- left_join(DT_dengue_90_dias, freq_DT_90_dias, by = "id_pessoa_dengue")

#filtrar quem tem um registro ou mais de 2 registros
#ao final juntar o de frequência 1 + de frequência superior a 2, após aplicar regra dos 90 dias
DT_dengue_90_dias_igual1 <- filter(DT_dengue_90_dias, DT_dengue_90_dias$Freq < 2)
DT_dengue_90_dias_maior2 <- filter(DT_dengue_90_dias, DT_dengue_90_dias$Freq >= 2)

#2 ou mais registros aplicando a regra dos 90 dias
DT_dengue_pos_90_dias <- DT_dengue_90_dias_maior2 %>% 
  dplyr::left_join(.,
                   DT_dengue_90_dias_maior2 %>% 
                     dplyr::group_by(id_pessoa_dengue) %>% 
                     dplyr::summarise(dt_primeiro_sintoma = min (DT_SIN_PRI)) %>% 
                     dplyr::ungroup(.),
                   by = c("id_pessoa_dengue"="id_pessoa_dengue")) %>% 
  dplyr::mutate(diferenca = DT_SIN_PRI - dt_primeiro_sintoma,
                check = dplyr::if_else(diferenca == 0, "True", "False"),
                check1 = dplyr::if_else(diferenca >= 90, "True", "False"))

DT_dengue_pos_90_dias <- DT_dengue_pos_90_dias %>% 
  filter(DT_dengue_pos_90_dias$check == "True", DT_dengue_pos_90_dias$check1 == "True")


#unir os dados após aplicação da regra dos 90 dias
DT_dengue_pos_90_dias <- subset(DT_dengue_pos_90_dias, 
                                  select = -c(dt_primeiro_sintoma,
                                              diferenca,
                                              check,
                                              check1))

data_dengue_pos_90_dias <- bind_rows(DT_dengue_90_dias_igual1, DT_dengue_pos_90_dias)

data_dengue_confirmados <- data_dengue_pos_90_dias

data_dengue_confirmados <- subset(data_dengue_confirmados, 
                             select = -c(ID_REGIONA,
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
                                         id_pessoa_dengue,
                                         Freq))

marc_dengue0 <- data.frame("confirmados")
data_dengue_confirmados <- mutate(data_dengue_confirmados, marc_dengue0) 
data_dengue_confirmados <- rename(data_dengue_confirmados, marcador = X.confirmados.)

#grupo2 descartados_dengue (caso suspeito, com exame negativo ou que não tem exame)
data_dengue_descartados <- data_dengue_clinico %>% 
                              filter(data_dengue_clinico$RESUL_PCR_ == 2 |
                                      is.na(data_dengue_clinico$RESUL_PCR_))

data_dengue_descartados <- data_dengue_descartados %>% 
                            filter(data_dengue_descartados$RESUL_SORO == 2 |
                                     is.na(data_dengue_descartados$RESUL_SORO))

data_dengue_descartados <- data_dengue_descartados %>% 
                            filter(data_dengue_descartados$RESUL_NS1 == 2 |
                                     is.na(data_dengue_descartados$RESUL_NS1))

data_dengue_descartados <- data_dengue_descartados %>% 
                            filter(data_dengue_descartados$RESUL_VI_N == 2 |
                                     is.na(data_dengue_descartados$RESUL_VI_N))

data_dengue_descartados <- data_dengue_descartados %>% 
                            filter(data_dengue_descartados$HISTOPA_N == 2 |
                                     is.na(data_dengue_descartados$HISTOPA_N))

data_dengue_descartados <- data_dengue_descartados %>% 
                            filter(data_dengue_descartados$IMUNOH_N == 2 |
                                     is.na(data_dengue_descartados$IMUNOH_N))

data_dengue_descartados <- data_dengue_descartados %>% 
                            filter(data_dengue_descartados$RESUL_PRNT == 2 |
                                     is.na(data_dengue_descartados$RESUL_PRNT))

id_pessoa_dengue2 <- paste(data_dengue_descartados$DT_SIN_PRI, data_dengue_descartados$id_pessoa_dengue)
id_pessoa_dengue2 <- data.frame(id_pessoa_dengue2)
data_dengue_descartados <- mutate(data_dengue_descartados, id_pessoa_dengue2)
data_dengue_descartados <- data_dengue_descartados[!duplicated(data_dengue_descartados$id_pessoa_dengue2), ]
data_dengue_descartados <- subset(data_dengue_descartados, 
                                  select = -c(id_pessoa_dengue2))

data_dengue_descartados <- subset(data_dengue_descartados, 
                                  select = -c(ID_REGIONA,
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
data_dengue_provaveis <- bind_rows(data_dengue_clinico,
                                   data_dengue_confirmados)
id_pessoa_dengue3 <- paste(data_dengue_provaveis$DT_SIN_PRI, data_dengue_provaveis$id_pessoa_dengue)
id_pessoa_dengue3 <- data.frame(id_pessoa_dengue3)
data_dengue_provaveis <- mutate(data_dengue_provaveis, id_pessoa_dengue3)
data_dengue_provaveis <- data_dengue_provaveis[!duplicated(data_dengue_provaveis$id_pessoa_dengue3), ]
data_dengue_provaveis <- subset(data_dengue_provaveis, 
                                  select = -c(id_pessoa_dengue3))

data_dengue_provaveis <- subset(data_dengue_provaveis, 
                                    select = -c(ID_REGIONA,
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
                                                id_pessoa_dengue,
                                                marcador))

marc_dengue2 <- data.frame("prováveis")
data_dengue_provaveis <- mutate(data_dengue_provaveis, marc_dengue2)
data_dengue_provaveis <- rename(data_dengue_provaveis, marcador = X.prováveis.)

############### CHIKUNGUNYA CONFIRMADOS, DESCARTADOS E PROVÁVEIS ###############
#Chik_que possui a clínica
data_chik_clinico <- filter(data_chik,
                            data_chik$FEBRE == "1")

data_chik_clinico <- filter(data_chik_clinico,
                            data_chik_clinico$ARTRALGIA == "1" |
                              data_chik_clinico$ARTRITE == "1")
#grupo1 chik_confirmados
#Chik_confirmado_laboratorial
data_chik_laboratorio <-   filter(data_chik,
                                       data_chik$RESUL_PCR_ == "1" |
                                       data_chik$RES_CHIKS1 == "1" |
                                       data_chik$RES_CHIKS2 == "1" |
                                       data_chik$RESUL_PRNT == "1" |
                                       data_chik$RESUL_SORO == "1" |
                                       data_chik$RESUL_VI_N == "1" |
                                       data_chik$HISTOPA_N == "1" |
                                       data_chik$IMUNOH_N == "1")

#Chik_clínico_epidemiológico
data_chik_clin_epidemiologico <- filter(data_chik_clinico,
                                        data_chik_clinico$CRITERIO =="2")

#confirmados_chik (laboratorial + clínico epidemiológico)
data_chik_confirmados <- bind_rows(data_chik_laboratorio, 
                                   data_chik_clin_epidemiologico)

#após junção de linhas, retirar possíveis duplicidades
id_pessoa_chik1 <- paste(data_chik_confirmados$DT_SIN_PRI, data_chik_confirmados$id_pessoa_chik)
id_pessoa_chik1 <- data.frame(id_pessoa_chik1)
data_chik_confirmados <- mutate(data_chik_confirmados, id_pessoa_chik1)
data_chik_confirmados <- data_chik_confirmados[!duplicated(data_chik_confirmados$id_pessoa_chik1), ]
data_chik_confirmados <- subset(data_chik_confirmados, 
                                select = -c(id_pessoa_chik1))

data_chik_confirmados <- subset(data_chik_confirmados, 
                                select = -c(ID_REGIONA,
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

#grupo2 descartados_chik (exame negativo ou sem exame)
data_chik_descartados <- data_chik_clinico %>% 
                            filter(data_chik_clinico$RESUL_PCR_ == 2 |
                              is.na(data_chik_clinico$RESUL_PCR_))

data_chik_descartados <- data_chik_descartados %>% 
                            filter(data_chik_descartados$RESUL_SORO == 2 |
                              is.na(data_chik_descartados$RESUL_SORO))

data_chik_descartados <- data_chik_descartados %>% 
                            filter(data_chik_descartados$RESUL_NS1 == 2 |
                                     is.na(data_chik_descartados$RESUL_NS1))

data_chik_descartados <- data_chik_descartados %>% 
                              filter(data_chik_descartados$RESUL_VI_N == 2 |
                                       is.na(data_chik_descartados$RESUL_VI_N))

data_chik_descartados <- data_chik_descartados %>% 
                            filter(data_chik_descartados$HISTOPA_N == 2 |
                                     is.na(data_chik_descartados$HISTOPA_N))

data_chik_descartados <- data_chik_descartados %>% 
                            filter(data_chik_descartados$IMUNOH_N == 2 |
                                     is.na(data_chik_descartados$IMUNOH_N))

data_chik_descartados <- data_chik_descartados %>% 
                              filter(data_chik_descartados$RESUL_PRNT == 2 |
                                       is.na(data_chik_descartados$RESUL_PRNT))

id_pessoa_chik2 <- paste(data_chik_descartados$DT_SIN_PRI, data_chik_descartados$id_pessoa_chik)
id_pessoa_chik2 <- data.frame(id_pessoa_chik2)
data_chik_descartados <- mutate(data_chik_descartados, id_pessoa_chik2)
data_chik_descartados <- data_chik_descartados[!duplicated(data_chik_descartados$id_pessoa_chik2), ]
data_chik_descartados <- subset(data_chik_descartados, 
                                select = -c(id_pessoa_chik2))

data_chik_descartados <- subset(data_chik_descartados, 
                                select = -c(ID_REGIONA,
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
data_chik_provaveis <- bind_rows(data_chik_clinico,
                                 data_chik_confirmados)

#após junção de linhas, retirar possíveis duplicidades
id_pessoa_chik3<- paste(data_chik_provaveis$DT_SIN_PRI, data_chik_provaveis$id_pessoa_chik)
id_pessoa_chik3 <- data.frame(id_pessoa_chik3)
data_chik_provaveis <- mutate(data_chik_provaveis, id_pessoa_chik3)
data_chik_provaveis <- data_chik_provaveis[!duplicated(data_chik_provaveis$id_pessoa_chik3), ]
data_chik_provaveis <- subset(data_chik_provaveis, 
                              select = -c(id_pessoa_chik3))

data_chik_provaveis <- subset(data_chik_provaveis, 
                           select = -c(ID_REGIONA,
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
                                      id_pessoa_chik,
                                      marcador))


marc_chik2 <- data.frame("prováveis")
data_chik_provaveis <- mutate(data_chik_provaveis, marc_chik2)
data_chik_provaveis <- rename(data_chik_provaveis, marcador = X.prováveis.)

##################### ZIKA CONFIRMADOS, DESCARTADOS E PROVÁVEIS ################
#grupo1 confirmados_zika
data_zika_confirmados <- filter(data_zika, 
                                data_zika$CLASSI_FIN == "1")
#retirar duplicidades
id_pessoa_zik1<- paste(data_zika_confirmados$DT_SIN_PRI, data_zika_confirmados$id_pessoa_zika)
id_pessoa_zik1 <- data.frame(id_pessoa_zik1)
data_zika_confirmados <- mutate(data_zika_confirmados, id_pessoa_zik1)
data_zika_confirmados <- data_zika_confirmados[!duplicated(data_zika_confirmados$id_pessoa_zik1), ]
data_zika_confirmados <- subset(data_zika_confirmados, 
                              select = -c(id_pessoa_zik1))

data_zika_confirmados <- subset(data_zika_confirmados, 
                              select = -c(ID_REGIONA,
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

#retirar duplicidades
id_pessoa_zik2 <- paste(data_zika_descartados$DT_SIN_PRI, data_zika_descartados$id_pessoa_zika)
id_pessoa_zik2 <- data.frame(id_pessoa_zik2)
data_zika_descartados <- mutate(data_zika_descartados, id_pessoa_zik2)
data_zika_descartados <- data_zika_descartados[!duplicated(data_zika_descartados$id_pessoa_zik2), ]
data_zika_descartados <- subset(data_zika_descartados, 
                                select = -c(id_pessoa_zik2))

data_zika_descartados <- subset(data_zika_descartados, 
                                select = -c(ID_REGIONA,
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
data_zika_prov <- filter(data_zika, 
                              data_zika$CLASSI_FIN != "2")

data_zika_prov1 <- data_zika %>% filter(is.na(data_zika$CLASSI_FIN))

data_zika_provaveis <- bind_rows(data_zika_prov,
                                 data_zika_prov1)

#retirar duplicidades
id_pessoa_zik3 <- paste(data_zika_provaveis$DT_SIN_PRI, data_zika_provaveis$id_pessoa_zika)
id_pessoa_zik3 <- data.frame(id_pessoa_zik3)
data_zika_provaveis <- mutate(data_zika_provaveis, id_pessoa_zik3)
data_zika_provaveis <- data_zika_provaveis[!duplicated(data_zika_provaveis$id_pessoa_zik3), ]
data_zika_provaveis <- subset(data_zika_provaveis, 
                                select = -c(id_pessoa_zik3))

marc_zik2 <- data.frame("prováveis")
data_zika_provaveis <- mutate(data_zika_provaveis, marc_zik2)
data_zika_provaveis <- rename(data_zika_provaveis, marcador = X.prováveis.)
data_zika_provaveis <- subset(data_zika_provaveis, 
                                  select = -c(ID_REGIONA,
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

##################### ÓBITOS ARBOVIROSES #################################
#óbitos confirmados dengue
data_dengue_obito_confirmados <- filter(data_dengue_confirmados,
                                  data_dengue_confirmados$EVOLUCAO == "2")

marc_dengue3 <- data.frame("confirmados")
data_dengue_obito_confirmados <- mutate(data_dengue_obito_confirmados, marc_dengue3)
data_dengue_obito_confirmados <- rename(data_dengue_obito_confirmados, obitos = X.confirmados.)

#óbitos em investigação dengue
data_dengue_inv_obito1 <- filter(data_dengue_provaveis,
                                data_dengue_provaveis$EVOLUCAO == "4")

data_dengue_inv_obito2 <- filter(data_dengue_provaveis,
                                 data_dengue_provaveis$EVOLUCAO == "9") 

data_dengue_inv_obito2 <- data_dengue_inv_obito2 %>% 
                        filter(! is.na(data_dengue_inv_obito2$DT_OBITO))

data_dengue_inv_obito <- bind_rows(data_dengue_inv_obito1,
                                   data_dengue_inv_obito2)

marc_dengue4 <- data.frame("Em investigação")
data_dengue_inv_obito <- mutate(data_dengue_inv_obito, marc_dengue4)
data_dengue_inv_obito <- rename(data_dengue_inv_obito, obitos = X.Em.investigação.)

data_dengue_obitos <- bind_rows(data_dengue_obito_confirmados,
                                data_dengue_inv_obito)

#óbitos confirmados chik
data_chik_obito_confirmados <- filter(data_chik_confirmados,
                          data_chik_confirmados$EVOLUCAO == "2")

marc_chik3 <- data.frame("confirmados")
data_chik_obito_confirmados <- mutate(data_chik_obito_confirmados, marc_chik3)
data_chik_obito_confirmados <- rename(data_chik_obito_confirmados, obitos = X.confirmados.)

#óbitos em investigação chik
data_chik_inv_obito1 <- filter(data_chik_provaveis,
                              data_chik_provaveis$EVOLUCAO == "4")

data_chik_inv_obito2 <- filter(data_chik_provaveis,
                               data_chik_provaveis$EVOLUCAO == "9")

data_chik_inv_obito2 <- data_chik_inv_obito2 %>% 
                        filter(! is.na(data_chik_inv_obito2$DT_OBITO))

data_chik_inv_obito <- bind_rows(data_chik_inv_obito1,
                                 data_chik_inv_obito2)

marc_chik4 <- data.frame("Em investigação")
data_chik_inv_obito <- mutate(data_chik_inv_obito, marc_chik4)
data_chik_inv_obito <- rename(data_chik_inv_obito, obitos = X.Em.investigação.)

data_chik_obitos <- bind_rows(data_chik_obito_confirmados,
                              data_chik_inv_obito)

#óbitos confirmados zika
data_zika_obito_confirmados <- filter(data_zika_confirmados,
                          data_zika_confirmados$EVOLUCAO == "2")

marc_zika3 <- data.frame("confirmados")
data_zika_obito_confirmados <- mutate(data_zika_obito_confirmados, marc_zika3)
data_zika_obito_confirmados <- rename(data_zika_obito_confirmados, obitos = X.confirmados.)

#óbitos em investigação zika
data_zika_inv_obito <- filter(data_zika_provaveis,
                              data_zika_provaveis$EVOLUCAO == "9")

data_zika_inv_obito <- data_zika_inv_obito %>% 
                        filter(! is.na(data_zika_inv_obito$DT_OBITO))

marc_zika4 <- data.frame("Em investigação")
data_zika_inv_obito <- mutate(data_zika_inv_obito, marc_zika4)
data_zika_inv_obito <- rename(data_zika_inv_obito, obitos = X.Em.investigação.)

data_zika_obitos <- bind_rows(data_zika_obito_confirmados,
                              data_zika_inv_obito)

###########################JUNÇÃO POR DEFINIÇÃO DE CADA AGRAVO #################
#dengue
dengue_ <- bind_rows(data_dengue_confirmados,
                     data_dengue_descartados,
                     data_dengue_provaveis)
#chik
chikungunya_ <- bind_rows(data_chik_confirmados,
                          data_chik_descartados,
                          data_chik_provaveis)
#zika
zika_ <- bind_rows(data_zika_confirmados,
                   data_zika_descartados,
                   data_zika_provaveis)

############################## INCONSISTÊNCIAS ################################
#data de início de sintomas fora do padrão
#criado objeto em etapa inicial ao verificar registros válidos
data_dengue_menor_2007 <- filter(data_dengue, data_dengue$DT_SIN_PRI < 2007)
data_chik_menor_2016 <- filter(data_chik, data_chik$DT_SIN_PRI < 2016)
data_zika_menor_2016 <- filter(data_zika, data_zika$DT_SIN_PRI < 2016)

#se for confirmado, mas não tiver classificação 10, 11 ou 12 (para dengue)
#se for confirmado, mas não tiver classificação 13 (para chikungunya)
incon_dengue_classificacao <- filter(data_dengue_confirmados,
                                     data_dengue_confirmados$CLASSI_FIN != 10)
incon_dengue_classificacao <- filter(data_dengue_confirmados,
                                     data_dengue_confirmados$CLASSI_FIN != 11)
incon_dengue_classificacao <- filter(data_dengue_confirmados,
                                     data_dengue_confirmados$CLASSI_FIN != 12)
incon_chik_classificacao <- filter(data_chik_confirmados,
                                   data_chik_confirmados$CLASSI_FIN != 13)

#se tiver laboratorio, mas tiver criterio diferente de laboratorio
incon_dengue_criterio <- filter(data_dengue_laboratorio,
                                data_dengue_laboratorio$CRITERIO != "1")
incon_chik_criterio <- filter(data_chik_laboratorio,
                                data_chik_laboratorio$CRITERIO != "1")

#gestante com sexo masculino
incon_dengue_gestante <- filter(dengue_,
                                dengue_$CS_SEXO == "M")

incon_dengue_gestante <- filter(incon_dengue_gestante,
                                incon_dengue_gestante$CS_GESTANTE == "1" |
                                incon_dengue_gestante$CS_GESTANTE == "2" |
                                incon_dengue_gestante$CS_GESTANTE == "3" |
                                incon_dengue_gestante$CS_GESTANTE == "4")

incon_chik_gestante <- filter(chikungunya_,
                              chikungunya_$CS_SEXO == "M")

incon_chik_gestante <- filter(incon_chik_gestante,
                              incon_chik_gestante$CS_GESTANTE == "1" |
                              incon_chik_gestante$CS_GESTANTE == "2" |
                              incon_chik_gestante$CS_GESTANTE == "3" |
                              incon_chik_gestante$CS_GESTANTE == "4")

incon_zika_gestante <- filter(zika_,
                              zika_$CS_SEXO == "M")

incon_zika_gestante <- filter(incon_zika_gestante,
                              incon_zika_gestante$CS_GESTANTE == "1" |
                              incon_zika_gestante$CS_GESTANTE == "2" |
                              incon_zika_gestante$CS_GESTANTE == "3" |
                              incon_zika_gestante$CS_GESTANTE == "4")
                                  
#marcou dengue sinais de alarme na classificação, mas não marcaram sinais de alarme dengue (68)
incon_dengue_alarme <- filter(dengue_,
                             dengue_$CLASSI_FIN == "11")

incon_dengue_alarme <- filter(incon_dengue_alarme,
                             incon_dengue_alarme$ALRM_HIPOT != 1)
incon_dengue_alarme <- filter(incon_dengue_alarme,
                              incon_dengue_alarme$ALRM_PLAQ != 1)
incon_dengue_alarme <- filter(incon_dengue_alarme,
                              incon_dengue_alarme$ALRM_VOM != 1)
incon_dengue_alarme <- filter(incon_dengue_alarme,
                              incon_dengue_alarme$ALRM_SANG != 1)
incon_dengue_alarme <- filter(incon_dengue_alarme,
                              incon_dengue_alarme$ALRM_HEMAT != 1)
incon_dengue_alarme <- filter(incon_dengue_alarme,
                              incon_dengue_alarme$ALRM_ABDOM != 1)
incon_dengue_alarme <- filter(incon_dengue_alarme,
                              incon_dengue_alarme$ALRM_LETAR != 1)
incon_dengue_alarme <- filter(incon_dengue_alarme,
                              incon_dengue_alarme$ALRM_HEPAT != 1)
incon_dengue_alarme <- filter(incon_dengue_alarme,
                              incon_dengue_alarme$ALRM_LIQ != 1)

#marcou dengue grave na classificação, mas não marcaram sinais de alarme dengue (70)                              
incon_dengue_grave <- filter(dengue_,
                             dengue_$CLASSI_FIN == "12")
  
incon_dengue_grave <- filter(incon_dengue_grave,
                              incon_dengue_grave$GRAV_PULSO != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$GRAV_CONV != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$GRAV_ENCH != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$GRAV_INSUF != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$GRAV_TAQUI != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$GRAV_EXTRE != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$GRAV_HIPOT != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$GRAV_HEMAT != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$GRAV_MELEN != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$GRAV_METRO != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$GRAV_SANG != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$GRAV_AST != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$GRAV_MIOC != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$GRAV_CONSC != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$GRAV_MIOC != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$GRAV_ORGAO != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$MANI_HEMOR != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$EPISTAXE != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$GENGIVO != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$METRO != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$PETEQUIAS != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$HEMATURA != 1)
incon_dengue_grave <- filter(incon_dengue_grave,
                             incon_dengue_grave$SANGRAM != 1)
#se tiver classificação 5 (descartado), sem exame 
incon_dengue_descartado <- filter(dengue_,
                                  dengue_$CLASSI_FIN == 5)
incon_dengue_descartado <- filter(incon_dengue_descartado,
                                  incon_dengue_descartado$RESUL_PCR_ != 2)
incon_dengue_descartado <- filter(incon_dengue_descartado,
                                  incon_dengue_descartado$RESUL_SORO != 2)
incon_dengue_descartado <- filter(incon_dengue_descartado,
                                  incon_dengue_descartado$RESUL_NS1 != 2)
incon_dengue_descartado <- filter(incon_dengue_descartado,
                                  incon_dengue_descartado$RESUL_VI_N != 2)
incon_dengue_descartado <- filter(incon_dengue_descartado,
                                  incon_dengue_descartado$HISTOPA_N != 2)
incon_dengue_descartado <- filter(incon_dengue_descartado,
                                  incon_dengue_descartado$IMUNOH_N != 2)
incon_dengue_descartado <- filter(incon_dengue_descartado,
                                  incon_dengue_descartado$RESUL_PRNT != 2)

incon_chik_descartado <- filter(chikungunya_,
                                chikungunya_$CLASSI_FIN == 5)
incon_chik_descartado <- filter(incon_chik_descartado,
                                incon_chik_descartado$RESUL_PCR_ != 2)
incon_chik_descartado <- filter(incon_chik_descartado,
                                incon_chik_descartado$RESUL_SORO != 2)
incon_chik_descartado <- filter(incon_chik_descartado,
                                incon_chik_descartado$RESUL_NS1 != 2)
incon_chik_descartado <- filter(incon_chik_descartado,
                                incon_chik_descartado$RESUL_VI_N != 2)
incon_chik_descartado <- filter(incon_chik_descartado,
                                incon_chik_descartado$HISTOPA_N != 2)
incon_chik_descartado <- filter(incon_chik_descartado,
                                incon_chik_descartado$IMUNOH_N != 2)
incon_chik_descartado <- filter(incon_chik_descartado,
                                incon_chik_descartado$RESUL_PRNT != 2)

#inconsistências óbito dengue
incons_dengue_obito <- filter(data_dengue_confirmados,
                                data_dengue_confirmados$EVOLUCAO == "3")
#inconsistências óbito chik
incons_chik_obito <- filter(data_chik_confirmados,
                            data_chik_confirmados$EVOLUCAO == "3")
#inconsistências óbito zika
incons_zika_obito <- filter(data_zika_confirmados,
                            data_zika_confirmados$EVOLUCAO == "3")

#notificados sem a clínica e sem laboratorio 
incon_dengue_sem_suspeita <- anti_join(data_dengue, dengue_, by = "NU_NOTIFIC")
incon_chik_sem_suspeita <- anti_join(data_chik, chikungunya_, by = "NU_NOTIFIC")

###################################### saídas ##################################
##saidas #nome da aba = #nome do objeto
export(list(dengue_limpo = dengue_,
            chikungunya_limpo = chikungunya_,
            zika_limpo = zika_,
            dengue_obitos = data_dengue_obitos,
            chikungunya_obitos = data_chik_obitos,
            zika_obitos = data_zika_obitos),
       file = "C:/Users/NDTA.Dses01213518/Desktop/Azul BE_limpo/Azul_dados limpos_para_boletim_arbo/Para_boletim_arbovirose.xlsx")
##saidas #nome da aba = #nome do objeto
export(list(dengue_IS = data_dengue_menor_2007,
            chikungunya_IS = data_chik_menor_2016,
            zika_IS = data_zika_menor_2016,
            dengue_classificacao = incon_dengue_classificacao,
            chik_classificacao = incon_chik_classificacao,
            dengue_criterio = incon_dengue_criterio,
            chik_criterio = incon_chik_criterio,
            dengue_gestante = incon_dengue_gestante,
            chik_gestante = incon_chik_gestante,
            zika_gestante = incon_zika_gestante,
            dengue_alarme = incon_dengue_alarme,
            dengue_grave = incon_dengue_grave,
            dengue_descartado = incon_dengue_descartado,
            chik_descartado = incon_chik_descartado,
            dengue_obito = incons_dengue_obito,
            chik_obito = incons_chik_obito,
            zika_obito = incons_zika_obito),
       file = "C:/Users/NDTA.Dses01213518/Desktop/Azul BE_limpo/Inconsistências/Inconsistências_arbovirose.xlsx")
#############FIM

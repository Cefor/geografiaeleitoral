# limpa a memória
rm(list = ls(all = TRUE))

# Baixar bibliotecas
library(stringr)
library(rgdal)
library(spdep)
set.ZeroPolicyOption(TRUE)

# Inicia a contagem do tempo
ptm <- proc.time()

# siglas dos estados
siglas <- c(#"AL","AP","DF")
            "AC", "AL", "AP", "AM", "BA", "CE", "ES", "DF",  
            "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE",  
            "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", 
            "SE", "SP","TO")

# Criando vetores  
nome_arq <- siglas
nome_grv <- siglas
nome_shp <- siglas
nome_nb <- siglas
compara <- data.frame(estado="", votos="", areas_m="", areas_z="", areas_mz="",
                      vagas="", media_n_mun="", media_n_ze="", media_g_mun="", 
                      media_m="", media_m_z="", media_mzn="", maior_mzn="", 
                      menor_mzn="", maior_area="", media_moran="", media_moranP="")
compara <- compara[-1,]
BR <- data.frame(UF="", Candidato_Eleito="", Votos="", N_mun="", N_ZE="", G_Mun="",
                 M_Mun="", M_MZE="", MZN="", Moran_I="", Moran_P="", stringsAsFactors=FALSE)
BR <- BR[-1,]

# Baixa e seleciona os dados de cada estado
for(x in 1: length(siglas)){

  nome_arq[x] <- sub("UF", siglas[x], "votacao_candidato_munzona_2014_UF.txt") 
  EST <- read.table(nome_arq[x],sep=";", header = FALSE)
  EST <- EST[,c(8,9,10,11,12,15,16,22,24,28,29)]

  # Ajustando nomes de municípios aos dos Shapefiles
  # Rio Grande do Norte
  if (siglas[x]=="RN") {
    EST[,2] <- sub("ASSÚ","AÇU (ASSÚ)", EST[,2])
    EST[,2] <- sub("CAMPO GRANDE","AUGUSTO SEVERO (CAMPO GRANDE)", EST[,2])
    EST[,2] <- sub("BOA SAÚDE","JANUÁRIO CICCO (BOA SAÚDE)", EST[,2])
  }
  # Minas Gerais
  if (siglas[x]=="MG") {
    EST[,2] <- sub("PASSA VINTE","PASSA-VINTE", EST[,2])
  }
  
  # Rio Grande do Sul
  if (siglas[x]=="RS") {
    EST[,2] <- sub("SANTANA DO LIVRAMENTO","SANT'ANA DO LIVRAMENTO", EST[,2])
  }

  # Preparando dados p/ agregação mista e nomeando variáveis
  EST[,3] <- as.factor(EST[,3])
  EST[,3] <- str_pad(EST[,3], width = 3, side = "left", pad = "0")
  EST$V11 <- paste(EST[,2],EST[,3])
  EST$V11 <- as.factor(EST$V11)
  names(EST)[1:11] <- c("CD_MUNICIPIO","NM_MUNICIPIO","NR_ZONA", "NM_MUN_ZE",
                        "NR_CANDIDATO","NM_URNA_CANDIDATO", "DS_CARGO",
                        "DS_SIT_CAND_TOT","SG_PARTIDO", "COLIGACAO","QT_VOTOS")

  # Seleciona os votos p/ deputado federal dos candidatos eleitos
  EST <- subset(EST,DS_CARGO=="DEPUTADO FEDERAL")
  eleito_EST <- subset(EST,DS_SIT_CAND_TOT=="ELEITO POR QP"|DS_SIT_CAND_TOT=="ELEITO POR MÉDIA")
  eleito_tot_EST <- aggregate(x=eleito_EST$QT_VOTOS, by=list(eleito_EST$NM_URNA_CANDIDATO), FUN=sum)
  names(eleito_tot_EST) <- c("Candidato_Eleito", "Votos")

  # Seleciona os Votos por Municipio
  eleito_EST1 <- aggregate(QT_VOTOS ~ NM_MUNICIPIO + NM_URNA_CANDIDATO + NR_CANDIDATO, data = eleito_EST, sum)
  VPMUN <- aggregate(QT_VOTOS ~ NM_MUNICIPIO, data = EST, sum)
  VPMUN$QT_VOTOS_MUN <- VPMUN$QT_VOTOS/sum(VPMUN$QT_VOTOS)
  for(i in 1: length(eleito_EST1$QT_VOTOS)) {
    eleito_EST1$QT_VOTOS_MUN[i] <- subset(VPMUN$QT_VOTOS_MUN, eleito_EST1$NM_MUNICIPIO[i]==VPMUN$NM_MUNICIPIO)
  }
  
  ### Calcula o índice N de Todos os eleitos nos estados (por Municipio)
  for(i in 1:length(eleito_tot_EST$Votos))   {
    eleito_tot_EST$N_Mun[i] <- 1/ sum((subset(eleito_EST1$QT_VOTOS, 
    eleito_tot_EST$Candidato[i]==eleito_EST1$NM_URNA_CANDIDATO)/eleito_tot_EST$Votos[i])^2)
  }

  # Seleciona os Votos por zona eleitoral (ZE)
  eleito_EST2 <- aggregate(QT_VOTOS ~ NR_ZONA + NM_URNA_CANDIDATO + NR_CANDIDATO, data = eleito_EST, sum)
  VPZE <- aggregate(QT_VOTOS ~ NR_ZONA, data = EST, sum)
  VPZE$QT_VOTOS_MUN <- VPZE$QT_VOTOS/sum(VPZE$QT_VOTOS)
  for(i in 1: length(eleito_EST2$QT_VOTOS)) {
    eleito_EST2$QT_VOTOS_MUN[i] <- subset(VPZE$QT_VOTOS_MUN, eleito_EST2$NR_ZONA[i]==VPZE$NR_ZONA)
  }
  
  ### Calcula o índice N de Todos os eleitos nos estados (por ZE)
  for(i in 1:length(eleito_tot_EST$Votos))   {
    eleito_tot_EST$N_ZE[i] <- 1/sum((subset(eleito_EST2$QT_VOTOS, 
    eleito_tot_EST$Candidato[i]==eleito_EST2$NM_URNA_CANDIDATO)/eleito_tot_EST$Votos[i])^2)
  }

  ### Calcula o Ìndice G de Todos os eleitos nos estados 
  for(i in 1:length(eleito_tot_EST$Votos))   {
    eleito_tot_EST$G_Mun[i] <-sum((subset(eleito_EST1$QT_VOTOS, 
    eleito_tot_EST$Candidato[i]==eleito_EST1$NM_URNA_CANDIDATO)/eleito_tot_EST$Votos[i]
    -subset(eleito_EST1$QT_VOTOS_MUN, eleito_tot_EST$Candidato[i]==eleito_EST1$NM_URNA_CANDIDATO))^2)
  }

  # Prepara os dados p/ calcular o ìndice M
  eleito_EST1 <- eleito_EST1[order(eleito_EST1$NM_URNA_CANDIDATO,-eleito_EST1$QT_VOTOS),]
  eleito_EST1$VOTOS_ACUM <- cumsum(eleito_EST1$QT_VOTOS_MUN)
  eleito_EST1$VOTOS_ACUM <- eleito_EST1$VOTOS_ACUM - floor(eleito_EST1$VOTOS_ACUM)
  eleito_EST1$VOTOS_ACUM[eleito_EST1$VOTOS_ACUM==0] <- 1
  for(i in 1: length(eleito_EST1$QT_VOTOS)) {
    eleito_EST1$PER_VOTOS_MUN[i] <- eleito_EST1$QT_VOTOS [i]/(subset(eleito_tot_EST$Votos, 
    eleito_EST1$NM_URNA_CANDIDATO[i]==eleito_tot_EST$Candidato_Eleito))                                    
  }
  eleito_EST1$PER_VOTOS_ACUM <- cumsum(eleito_EST1$PER_VOTOS_MUN)
  eleito_EST1$PER_VOTOS_ACUM <- eleito_EST1$PER_VOTOS_ACUM - floor(eleito_EST1$PER_VOTOS_ACUM)
  eleito_EST1$PER_VOTOS_ACUM[eleito_EST1$PER_VOTOS_ACUM==0] <- 1

  ### Cálculo do ìndice M de todos os eleitos nos estados
  PERCE <- 0.5
  for(i in 1:length(eleito_tot_EST$Votos))  {
    eleito_tot_EST$M_Mun[i] <- min(subset(eleito_EST1$VOTOS_ACUM, 
    eleito_tot_EST$Candidato[i]==eleito_EST1$NM_URNA_CANDIDATO & eleito_EST1$PER_VOTOS_ACUM > PERCE))
  }
  for(i in 1:length(eleito_tot_EST$Votos))  {
    eleito_tot_EST$Percentual_M[i] <- min(subset(eleito_EST1$PER_VOTOS_ACUM, 
    eleito_tot_EST$Candidato[i]==eleito_EST1$NM_URNA_CANDIDATO & eleito_EST1$PER_VOTOS_ACUM > PERCE))
  }

  # Organiza os dados por Agregação mista (Municipio+ZE)
  VPMUN_ZE <- aggregate(QT_VOTOS ~ NM_MUN_ZE + NM_MUNICIPIO, data = EST, sum)
  VPMUN_ZE$AREA <- 1
  votacao <- aggregate(AREA ~ NM_MUNICIPIO, data = VPMUN_ZE, sum)
  for(i in 1: length(VPMUN_ZE$QT_VOTOS)) {
    VPMUN_ZE$AREA[i] <- subset(votacao$AREA,VPMUN_ZE$NM_MUNICIPIO[i]==votacao$NM_MUNICIPIO)
  }
  VPMUN_ZE$QT_VOTOS_MUN_ZE <- VPMUN_ZE$QT_VOTOS/sum(VPMUN_ZE$QT_VOTOS)
  for(i in 1: length(VPMUN_ZE$QT_VOTOS)) {
    VPMUN_ZE$NM_AREA[i] <-if(VPMUN_ZE$AREA[i]==1){str_sub(VPMUN_ZE$NM_MUN_ZE[i],end = -4)}
    else    {str_sub(VPMUN_ZE$NM_MUN_ZE[i])}
  }
  VPMUN_ZE <- VPMUN_ZE[, c(6, 1, 3, 5)]
                            
  # Prepara os dados p/ calcular o ìndice por (Mun+ZE) 
  eleito_EST3 <- aggregate(QT_VOTOS ~ NM_MUN_ZE + NM_MUNICIPIO + NM_URNA_CANDIDATO 
                + NR_CANDIDATO, data = eleito_EST, sum)
  for(i in 1: length(eleito_EST3$QT_VOTOS)) {
    eleito_EST3$QT_VOTOS_MUN_ZE[i] <- subset(VPMUN_ZE$QT_VOTOS_MUN_ZE, 
    eleito_EST3$NM_MUN_ZE[i]==VPMUN_ZE$NM_MUN_ZE)
  }
  VPMUN_ZE$NM_MUN_ZE <- NULL

  # Prepara os dados p/ calcular o ìndice por (Mun+ZE) (2)
  eleito_EST3 <- eleito_EST3[order(eleito_EST3$NM_URNA_CANDIDATO,-eleito_EST3$QT_VOTOS),]
  eleito_EST3$VOTOS_ACUM <- cumsum(eleito_EST3$QT_VOTOS_MUN_ZE)
  eleito_EST3$VOTOS_ACUM <- eleito_EST3$VOTOS_ACUM - floor(eleito_EST3$VOTOS_ACUM)
  eleito_EST3$VOTOS_ACUM[eleito_EST3$VOTOS_ACUM==0] <- 1
  for(i in 1: length(eleito_EST3$QT_VOTOS)) {
    eleito_EST3$PER_VOTOS_MUN_ZE[i] <- eleito_EST3$QT_VOTOS [i]/(subset(eleito_tot_EST$Votos, 
    eleito_EST3$NM_URNA_CANDIDATO[i]==eleito_tot_EST$Candidato_Eleito))                                    
  }
  eleito_EST3$PER_VOTOS_ACUM <- cumsum(eleito_EST3$PER_VOTOS_MUN_ZE)
  eleito_EST3$PER_VOTOS_ACUM <- eleito_EST3$PER_VOTOS_ACUM - floor(eleito_EST3$PER_VOTOS_ACUM)
  eleito_EST3$PER_VOTOS_ACUM[eleito_EST3$PER_VOTOS_ACUM==0] <- 1

  ### Cálculo do ìndice M (por Mun+ZE) de todos os eleitos nos estados
  PERCE <- 0.5
  for(i in 1:length(eleito_tot_EST$Votos))   {
    eleito_tot_EST$M_MZE[i] <- min(subset(eleito_EST3$VOTOS_ACUM, 
    eleito_tot_EST$Candidato[i]==eleito_EST3$NM_URNA_CANDIDATO & 
    eleito_EST3$PER_VOTOS_ACUM > PERCE ))
  }

  for(i in 1:length(eleito_tot_EST$Votos))   {
    eleito_tot_EST$Percentual_M_ZE[i] <- min(subset(eleito_EST3$PER_VOTOS_ACUM, 
    eleito_tot_EST$Candidato[i]==eleito_EST3$NM_URNA_CANDIDATO & 
    eleito_EST3$PER_VOTOS_ACUM > PERCE ))
  }

  ### Cálculo do MZN
  eleito_tot_EST$MZN <- eleito_tot_EST$M_MZE/(1/(length(eleito_tot_EST$Candidato_Eleito)))

  ## Prepara os dados para o cálculo do I de Moran
  # Importa e ordena Dados dos Shapefiles
  nome_shp[x] <- sub("UF", siglas[x],"UF_municipios")
  shape <- readOGR(nome_shp[x]) 
  proc.time() - ptm
  shape$NM_MUNICIP <- iconv(shape$NM_MUNICIP, from="UTF-8", to="latin1//TRANSLIT")

  # Retira Lagoa dos Patos e Lagoa Mirim do RS
  if (siglas[x]=="RS") {shape <- shape[-c(229,231),]}

  ### Calcula Matriz de Ponderação dos Shapefiles
  shape <- shape[order(shape$NM_MUNICIP),]

  # Exclui o DF do cálculo de Moran I 
  if (siglas[x]=="DF")  { eleito_tot_EST[c(3,5,6,7)] <- NA } else {
    queen_R_nb <- poly2nb(shape)
  }

  # ordena dados p/ o cálculo
  votacao <- votacao[order(votacao$NM_MUNICIPIO),]
  eleito_EST1 <- eleito_EST1[order(eleito_EST1$NM_MUNICIPIO),]

  ### Calcula I de Moran
  for(z in 1:length(eleito_tot_EST$Votos))   {
    cand <- eleito_EST1$QT_VOTOS[eleito_EST1$NM_URNA_CANDIDATO==eleito_tot_EST$Candidato_Eleito[z]]
    votacao <- cbind(votacao,cand)
    vt <- as.data.frame(votacao[z+2])
    if (siglas[x]=="DF")  { 
      eleito_tot_EST$Moran_I[z] <- NA    
      eleito_tot_EST$Moran_P[z] <- NA} 
    else {
      eleito_tot_EST$Moran_I[z] <- as.numeric(moran.mc(vt$cand, nb2listw(queen_R_nb), nsim=99)[1])
      eleito_tot_EST$Moran_P[z] <- as.numeric(moran.mc(vt$cand, nb2listw(queen_R_nb), nsim=99)[3])  
    }
  }

  # Prepara e grava as Matrizes de votos dos deputados por Estado/Município
  dep <- as.character(eleito_tot_EST$Candidato_Eleito)
  names(votacao)[3:(length(dep)+2)] <- c(dep[1:length(dep)])
  votacao <- cbind(votacao, shape$NM_MUNICIP)[,c(1,(length(dep)+3),2:(length(dep)+2))]
  nome_nb[x] <- nome_nb[x] <- sub("UF", siglas[x],"Municipios_UF.csv") 
  write.table(votacao,file = nome_nb[x],row.names = FALSE, sep = ";", dec = ",")
  
  # Armazena Matriz com todos os índices de todos os eleitos do país
  BR <- rbind(BR,cbind(siglas[x], eleito_tot_EST[,-c(7,9)]))
  
  # Ordena os candidatos pelo N (por Mun)
  eleito_tot_EST <- eleito_tot_EST[order(eleito_tot_EST$N_Mun),]

  # Insere as médias do estado e número de Moran P(value) < 0.05
  media <- as.list(replicate(10,paste("Média de",siglas[x])))
  media[2:11] <- lapply(eleito_tot_EST[2:11], mean)
  if (is.na(min(eleito_tot_EST$Moran_P))) { media[12] <- NA } else {
    media[12] <- as.numeric(length(eleito_tot_EST$Moran_P[eleito_tot_EST$Moran_P<0.05]))
  }
  media <- as.data.frame(media)
  names(media) <- colnames(eleito_tot_EST)
  eleito_tot_EST <- rbind(eleito_tot_EST, media)

  #Arredonda médias do estado
  eleito_tot_EST[2:10] <- round(eleito_tot_EST[,2:10],2)
  eleito_tot_EST[11] <- round(eleito_tot_EST[,11],5)
  
  #Grava planilhas dos estados com todos os índices dos candidatos
  nome_grv[x] <- nome_grv[x] <- sub("UF", siglas[x],"Indices_UF.csv") 
  write.table(eleito_tot_EST,file = nome_grv[x], row.names = FALSE, sep = ";", dec = ",")

  # Monta e Grava a planilha de comparação entre os estados
  linha <- data.frame(estado=siglas[x],votos=sum(VPMUN_ZE$QT_VOTOS), areas_m=length(VPMUN$NM_MUNICIPIO),
            areas_z=length(VPZE$NR_ZONA), areas_mz=length(VPMUN_ZE$NM_AREA),
            vagas=length(eleito_tot_EST$Candidato_Eleito)-1, media_n_mun=mean(eleito_tot_EST$N_Mun), 
            media_n_ze=mean(eleito_tot_EST$N_ZE), media_g_mun=mean(eleito_tot_EST$G_Mun), 
            media_m=mean(eleito_tot_EST$M_Mun), media_m_z=mean(eleito_tot_EST$M_MZE), 
            media_mzn=mean(eleito_tot_EST$MZN), maior_mzn=max(eleito_tot_EST$MZN), 
            menor_mzn=min(eleito_tot_EST$MZN), maior_area=max(VPMUN_ZE$QT_VOTOS_MUN_ZE), 
            media_moran=mean(eleito_tot_EST$Moran_I), N_moranP=as.numeric(media[12]))
  linha[2:15] <- round(linha[,2:15],3)
  compara <- rbind(compara, linha)

}


names(BR)[1] <- "UF"
write.table(compara,file = "Indices_Medios_estados.csv", row.names = FALSE, sep = ";", dec = ",")

# Grava a planilha com os índices de todos os deputados eleitos em todo o Brasil
write.table(BR,file = "Indices_Brasil.csv", row.names = FALSE, sep = ";", dec = ",")

#Para de contar o tempo
proc.time() - ptm
timestamp()





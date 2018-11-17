#Analise inicial dos arquivos geraos pelo e-Lattes
#LEMBRAR DE CONFIGURAR O DIRETORIO NO COMANDO setwd()

#Pacotes para serem ativados
library(tidyverse)
library(jsonlite)
library(listviewer)
library(igraph)
library(dplyr) #Operador %>%
library(tidyr) #spread()
library(ggplot2) #ggplot()

#CONFIGURAR - DIRETORIO DOS .R
setwd("~/Repository/DataScience/DS4A-BioAni-BioMic-BioMol-PatMol")
source("elattes.ls2df.R")

#CONFIGURAR - DIRETORIO DOS .JSON (Arquivos eLattes)
perfil <- fromJSON("240BiologiaAnimal/240profile.json")
public <- fromJSON("240BiologiaAnimal/240publication.json")
orient <- fromJSON("240BiologiaAnimal/240advise.json")
graphl <- fromJSON("240BiologiaAnimal/240graph.json")

#Arquivos Sucupira? Pesquisar
res.area <- fromJSON("UnBPosGeral/researchers_by_area.json")
df.prog <- read.table("UnBPosGeral/prof_prog.csv", sep = ",", 
                      colClasses = "character", encoding = "UTF-8", header = TRUE)

#SEPARACAO DOS CAMPOS DE DF.PROG - Depende dos arquivos Sucupira
df.prog <- df.prog %>% separate(idLattes.Docente.Categoria.Grande.Area.Area.de.Avaliacao.Codigo.AreaPos.Programa,
                     c("idLattes", "Docente", "Categoria", "GrandeArea", "AreaDeAvaliacao", "Codigo", "AreaPos", "Programa"),
                     sep = ";", extra = "drop", fill = "right")

######
#Analise do arquivo perfil

#jsonedit(perfil)
#jsonedit(public)

#Numero de Docentes encontrados
length(perfil)

#Constroi lista de nomes de docentes/pessoas
ProfileList <- list()
for (i in 1:length(perfil)) {
  ProfileList <- rbind(ProfileList, perfil[[i]]$nome)
}

##Analise dos dados em formato list
# Numero de areas de atuacao cumulativo
sum(sapply(perfil, function(x) nrow(x$areas_de_atuacao)))
# Numero de areas de atuacao por pessoa
table(unlist(sapply(perfil, function(x) nrow(x$areas_de_atuacao))))
# Numero de pessoas por grande area
table(unlist(sapply(perfil, function(x) (x$areas_de_atuacao$grande_area))))
# Numero de pessoas que produziram os especificos tipos de producao
table(unlist(sapply(perfil, function(x) names(x$producao_bibiografica))))
# Numero de publicacoes por tipo
sum(sapply(perfil, function(x) length(x$producao_bibiografica$ARTIGO_ACEITO$ano)))
sum(sapply(perfil, function(x) length(x$producao_bibiografica$CAPITULO_DE_LIVRO$ano)))
sum(sapply(perfil, function(x) length(x$producao_bibiografica$LIVRO$ano)))
sum(sapply(perfil, function(x) length(x$producao_bibiografica$PERIODICO$ano)))
sum(sapply(perfil, function(x) length(x$producao_bibiografica$TEXTO_EM_JORNAIS$ano)))
# Numero de pessoas por quantitativo de producoes por pessoa 0 = 1; 1 = 2...
table(unlist(sapply(perfil, function(x) length(x$producao_bibiografica$ARTIGO_ACEITO$ano))))
table(unlist(sapply(perfil, function(x) length(x$producao_bibiografica$CAPITULO_DE_LIVRO$ano))))
table(unlist(sapply(perfil, function(x) length(x$producao_bibiografica$LIVRO$ano))))
table(unlist(sapply(perfil, function(x) length(x$producao_bibiografica$PERIODICO$ano))))
table(unlist(sapply(perfil, function(x) length(x$producao_bibiografica$TEXTO_EM_JORNAIS$ano))))
# Numero de producoes por ano
table(unlist(sapply(perfil, function(x) (x$producao_bibiografica$ARTIGO_ACEITO$ano))))
table(unlist(sapply(perfil, function(x) (x$producao_bibiografica$CAPITULO_DE_LIVRO$ano))))
table(unlist(sapply(perfil, function(x) (x$producao_bibiografica$LIVRO$ano))))
table(unlist(sapply(perfil, function(x) (x$producao_bibiografica$PERIODICO$ano))))
table(unlist(sapply(perfil, function(x) (x$producao_bibiografica$TEXTO_EM_JORNAIS$ano))))
# Numero de pessoas que realizaram diferentes tipos de orientacoes
length(unlist(sapply(perfil, function(x) names(x$orientacoes_academicas))))
# Numero de pessoas por tipo de orientacao
table(unlist(sapply(perfil, function(x) names(x$orientacoes_academicas))))
#Numero de orientacoes concluidas
sum(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano)))
sum(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano)))
sum(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano)))

# Numero de pessoas por quantitativo de orientacoes por pessoa 0 = 1; 1 = 2...
table(unlist(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano))))
table(unlist(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano))))
table(unlist(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano))))

# Numero de orientacoes por ano
table(unlist(sapply(perfil, function(x) (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano))))
table(unlist(sapply(perfil, function(x) (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano))))
table(unlist(sapply(perfil, function(x) (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano))))

###Analise dos dados em formato Data Frame
#Arquivo Profile por Curriculo
# extrai perfis dos professores 
perfil.df.professores <- extrai.perfis(perfil)

# extrai producao bibliografica de todos os professores 
perfil.df.publicacoes <- extrai.producoes(perfil) %>%
  select(tipo_producao, everything()) %>% arrange(tipo_producao)

#extrai orientacoes 
perfil.df.orientacoes <- extrai.orientacoes(perfil) %>%
  select(id_lattes_orientadores, natureza, ano, orientacao, everything())

#extrai areas de atuacao 
perfil.df.areas.de.atuacao <- extrai.areas.atuacao(perfil) %>%
  select(idLattes, everything())

#cria arquivo com dados quantitativos para analise
perfil.df <- data.frame()
perfil.df <- perfil.df.professores %>% 
  select(idLattes, nome, resumo_cv, senioridade) %>% 
  left_join(
    perfil.df.orientacoes %>% 
      select(orientacao, idLattes) %>% 
      filter(!grepl("EM_ANDAMENTO", orientacao)) %>% 
      group_by(idLattes) %>% 
      count(orientacao) %>% 
      spread(key = orientacao, value = n), 
    by = "idLattes") %>% 
  left_join(
    perfil.df.publicacoes %>% 
      select(tipo_producao, idLattes) %>% 
      filter(!grepl("ARTIGO_ACEITO", tipo_producao)) %>% 
      group_by(idLattes) %>% 
      count(tipo_producao) %>% 
      spread(key = tipo_producao, value = n), 
    by = "idLattes") %>% 
  left_join(
    perfil.df.areas.de.atuacao %>% 
      select(area, idLattes) %>% 
      group_by(idLattes) %>% 
      summarise(n_distinct(area)), 
    by = "idLattes")

glimpse(perfil.df)


####
###Publicacao
##Analise dos dados no formato lista
#Numero de Publicacoes em periódicos
sum(sapply(public$PERIODICO, function(x) length(x$natureza)))
#anos analisados
names(public$PERIODICO)
#20 revistas mais publicadas
head(sort(table(as.data.frame(unlist
    (sapply(public$PERIODICO, function(x) unlist(x$periodico)))
  )), decreasing = TRUE),20)

##Analise dos dados no formato DF
public.periodico.df <- pub.ls2df(public, 1) #artigos
public.livros.df <- pub.ls2df(public, 2) #livros
public.eventos.df <- pub.ls2df(public, 5) #eventos
#Publicacao por ano
table(public.periodico.df$ano)
#20 revistas mais publicadas
#Mesma visao que anterior mas agora trabalhando no DataFrame
head(sort(table(public.periodico.df$periodico), decreasing = TRUE), 20)

####
#Orientacao
#Analise dos dados em formato lista
##Numero de Orientacoes Mestrado e Doutorado
sum(sapply(orient$ORIENTACAO_CONCLUIDA_DOUTORADO, function(x) length(x$natureza))) + 
  sum(sapply(orient$ORIENTACAO_CONCLUIDA_MESTRADO, function(x) length(x$natureza)))

##Analise dos dados no formato DF
orient.posdoutorado.df <- ori.ls2df(orient, 6) #pos-Doutorado concluido
orient.doutorado.df <- ori.ls2df(orient, 7) #Doutorado concluido
orient.mestrado.df <- ori.ls2df(orient, 8) #Mestrado concluido

orient.df <- rbind(rbind(orient.posdoutorado.df, orient.doutorado.df), orient.mestrado.df)

###
#Grafo
g <- g.ls2ig(graphl)
df <- as.data.frame(V(g)$name); colnames(df) <- "Idlattes"

#BLOCO ABAIXO REQUER BASE DE DADOS df.prog
df <- left_join(df, df.prog, by = c("Idlattes" = "idLattes")) #

#Apenas para fins de analise inicial, foram retiradas as observacoes 
#com duplicacao de pesquisadores no caso de haver professores em mais 
#de um programa
df <- df %>% group_by(Idlattes) %>% 
  slice(1L)
V(g)$programa <- df$Programa
V(g)$orient_dout <- perfil.df$ORIENTACAO_CONCLUIDA_DOUTORADO
V(g)$orient_mest <- perfil.df$ORIENTACAO_CONCLUIDA_MESTRADO
V(g)$publicacao <- perfil.df$PERIODICO
V(g)$eventos <- perfil.df$EVENTO

###

#Visualizacao
# Grafico de barras; periodicos por ano
public.periodico.df %>%
  group_by(ano) %>%
  summarise(Quantidade = n()) %>%
  ggplot(aes(x = ano, y = Quantidade)) +
  geom_bar(position = "stack",stat = "identity", fill = "darkcyan")+
  ggtitle("Periodicos publicados entre 2010 e 2017") +
  geom_text(aes(label=Quantidade), vjust=-0.3, size=2.5)+
  theme_minimal() + labs(x="Ano",y="Quantidade de Periodicos")

#Quantidade de periodicos publicados por professor(a) entre 2010 e 2017 - by Jonas
perfil.df %>%
  ggplot(aes(idLattes,PERIODICO)) +
  geom_col(fill = "purple") +
  ggtitle("Periodicos publicados entre 2010 e 2017") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Pesquisador(a)",y="Numero de publicacoes") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

#publicacao de livros por pais/ano
public.livros.df %>%
  filter(pais_de_publicacao %in% c("Brasil", "Estados Unidos", "Holanda",
                                   "Gra-Bretanha", "Alemanha", "Suica")) %>%
  group_by(ano,pais_de_publicacao) %>%
  ggplot(aes(x=ano,y=pais_de_publicacao, color= pais_de_publicacao)) +
  xlab("Ano") + ylab("Pais") + geom_point() + geom_jitter()

#Eventos nacionais e internacionais
public.eventos.df %>%
  filter(pais_do_evento %in% 
           c(names(head(sort(table(public.eventos.df$pais_do_evento)
                             , decreasing = TRUE), 10)))) %>%
  group_by(ano_do_trabalho,pais_do_evento) %>%
  ggplot(aes(x=ano_do_trabalho,y=pais_do_evento, color= pais_do_evento)) +
  xlab("Ano") + ylab("Pais") + geom_point() + geom_jitter()

#Orientacoes completas por ano e natureza
ggplot(orient.df,aes(ano,fill=natureza)) +
  geom_bar(stat = "count", position="dodge") +
  ggtitle("Natureza das Orientacoes Completas Por Ano") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Ano",y="Quantidade")

#Bolsas distribuidas por ano - by Jonas

orient.df %>% filter(bolsa == "SIM") %>%
ggplot(aes(ano,fill=natureza)) +
  geom_bar(stat = "count", position = "dodge") +
  ggtitle("Bolsas disponibilizadas por ano") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Ano",y="Quantidade de bolsas")

#Grafo de proximidade entre pesquisadores do Programa de Pos-Graduacao - by Jonas
plot(g, vertex.label = NA)

#Número de professores por grande área:
ggplot(df.prog, aes(GrandeArea, col=AreaPos)) +
  geom_bar(stat = 'count', position = 'dodge') +
  ggtitle('Número de áreas por grande área') +
  theme(legend.position = 'right') +
  labs(x='Grande Área de conhecimento',y='Área de conhecimento')

#Natureza das orientações por tipo de orientação
ggplot(perfil.df.orientacoes, aes(natureza,col=orientacao)) +
  geom_bar(stat = 'count') +
  ggtitle('Natureza das orientações por tipo de orientação')
  theme(legend.position='right',legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = 'top')) +
  labs(x='Natureza da orientação',y=' ')

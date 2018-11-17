#Analise inicial dos arquivos geraos pelo e-Lattes
#LEMBRAR DE CONFIGURAR O DIRETORIO NO COMANDO setwd()

#Pacotes para serem ativados
library(tidyverse) #Importado para manipulação de tibbles
library(jsonlite) #Importado para carga dos arquivos JSON para o R
library(listviewer) #Importado para análise dos arquivos JSON
library(igraph) #Importado para manipulação de grafo
library(dplyr) #Importado para uso do Operador Pipe
library(tidyr) #Importado para uso da função spread()
library(ggplot2) #Importado para visualizações com ggplot()

#CONFIGURAR - DIRETORIO DOS .R
setwd("~/Repository/DataScience/DS4A-BioAni-BioMic-BioMol-PatMol")
source("elattes.ls2df.R")

#CONFIGURAR - DIRETORIO DOS .JSON (Arquivos eLattes)
perfil <- fromJSON("243BiologiaMolecular/243profile.json")
public <- fromJSON("243BiologiaMolecular/243publication.json")
orient <- fromJSON("243BiologiaMolecular/243advise.json")
graphl <- fromJSON("243BiologiaMolecular/243graph.json")

#Arquivos Sucupira? Pesquisar
#res.area <- fromJSON("UnBPosGeral/researchers_by_area.json") #NÃO UTILIZADO
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

#Atributos presentes em um dos pesquisadores
glimpse(perfil[[1]], width = 30)

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
  ggtitle("Periodicos publicados por ano") +
  geom_text(aes(label=Quantidade), vjust=-0.3, size=2.5)+
  theme_minimal() + labs(x="Ano",y="Quantidade de Periodicos")

#Sob uma perspectiva de publicações em periódicos, o Programa de Pós-Graduação
#de Biologia Molecular apresentou pouco crescimento entre 2010 e 2017, registrando
#de 110 a 130 publicações por ano em grande parte da amostra. Uma curiosidade a
#ser observada é que os anos seguidos de 2011 e 2012 foram os de maior e menor
#produção, respectivamente.

#Quantidade de periodicos publicados por professor(a) entre 2010 e 2017
perfil.df %>%
  ggplot(aes(idLattes,PERIODICO)) +
  geom_col(fill = "purple") +
  ggtitle("Periodicos publicados por pesquisador") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Pesquisador(a)",y="Numero de publicacoes") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  geom_hline(yintercept = sum(perfil.df %>% summarize(x = median(PERIODICO))), color = "red")

#Investigando o comportamento dos pesquisadores do programa, os professores
#foram ordenados pelo número total de publicações em periódicos entre 2010 e 2017, 
#cuja mediana foi apresentada pela linha vermelha do gráfico acima, que sinaliza
#28 publicações no período total. O gráfico indica que, dos 36 pesquisadores
#inclusos na base de dados, três se destacaram com publicações que mais que
#duplicaram este valor - com um deles chegando a 77 e outro atingindo 104
#publicações em periódicos.

#publicacao de livros por pais/ano
public.livros.df %>%
  group_by(ano,pais_de_publicacao) %>%
  ggplot(aes(x=ano,y=pais_de_publicacao, color= pais_de_publicacao)) +
  ggtitle("Livros publicados por ano") +
  xlab("Ano") + ylab("Pais") + geom_point() + geom_jitter()

#O Programa em questão registrou publicações de livros em todos os anos no período
#de 2010 a 2017, com grande concentração destas publicações em território nacional.

#Eventos nacionais e internacionais
public.eventos.df %>%
  filter(pais_do_evento %in% 
           c(names(head(sort(table(public.eventos.df$pais_do_evento)
                             , decreasing = TRUE), 10)))) %>%
  group_by(ano_do_trabalho,pais_do_evento) %>%
  ggplot(aes(x=ano_do_trabalho,y=pais_do_evento, color= pais_do_evento)) +
  ggtitle("Participacoes em eventos") +
  xlab("Ano") + ylab("Pais") + geom_point() + geom_jitter()

#Considerando eventos, o Programa de Biologia Molecular teve comparecimento maior
#em eventos no Brasil que em países estrangeiros. Ainda assim, o gráfico mostra
# que o programa esteve presente em eventos de sede internacional em 
#todos os anos registrados na base de dados, principalmente nos Estados Unidos.

#Orientacoes completas por ano e natureza
ggplot(orient.df,aes(ano,fill=natureza)) +
  geom_bar(stat = "count", position="dodge") +
  ggtitle("Natureza das Orientacoes Completas Por Ano") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Ano",y="Quantidade") + scale_y_continuous(limits = c(0, 45))

#Observando a evolução do número de orientações completas ao longo dos anos,
#percebe-se que o Programa de mestrado cresceu consideravelmente entre
#2010 e 2013, mas apresentou regressão nos períodos posteriores, chegando ao seu
#menor índice em 2017. Em contrapartida, o número de orientações de doutorado
#e pós-doutorado apresentou maior estabilidade ao longo da amostra. Todas
#as naturezas de orientação do Programa parecem bem estabelecidas, visto que
#mesmo os índices de pós-doutorado indicam uma média de mais de 10 observações/ano.

#Bolsas distribuidas por ano

orient.df %>% filter(bolsa == "SIM") %>%
ggplot(aes(ano,fill=natureza)) +
  geom_bar(stat = "count", position = "dodge") +
  ggtitle("Bolsas disponibilizadas por ano") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Ano",y="Quantidade de bolsas") + scale_y_continuous(limits = c(0, 45))

#Comparando os gráficos de orientações completas e de bolsas, é possível
#perceber que o número de bolsas oferecidas para o Programa acompanhou o
#total de orientações de maneira satisfatória ao longo dos anos.
#Durante todo o período, todas as naturezas apresentaram um índice de pelo menos
#50% de bolsas, chegando a 100% em alguns casos.
#Além disso, as teses de pós-doutorado se mostram a natureza de pesquisa melhor
#contemplada pelas agências financiadoras, visto que apenas uma das observações
#não recebeu bolsa.

#Grafo de proximidade entre pesquisadores do Programa de Pos-Graduacao
plot(g, vertex.label = NA)

#O grafo acima representa os pesquisadores do Programa de Pós-Graduação em seus vértices
#e a existência de cooperação entre eles em suas arestas. Portanto, é possível
#observar uma considerável cooperação entre os membros do Programa, com muitos
#pesquisadores em vizinhanças próximas. Ainda assim, é possível observar a existência
#de alguns nós periféricos, com baixa diversidade em suas colaborações no período,
#variando entre zero e dois colaboradores.

#REVER COMENTARIOS DAQUI PRA BAIXO

#Natureza das orientações por tipo de orientação
ggplot(perfil.df.orientacoes, aes(natureza,fill=orientacao)) +
  geom_bar(stat = 'count') +
  theme(legend.position = 'right') +
  ggtitle('Natureza das orientações por tipo de orientação') +
  labs(x='Natureza',y='Quantidade de orientações') +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Orientações por ano:
ggplot(perfil.df.orientacoes, aes(ano, fill=orientacao)) +
  geom_bar(stat = 'count') +
  ggtitle('Orientações por ano') +
  theme(legend.position = 'right') +
  labs(x='Ano',y='Quantidade de orientações')

# A partir do número de orientações por ano e pela natureza destas,
# percebe-se que a quantidade de orientações de pós graduações no
# sentido restrito era pequena em proporção a outras orientações
# conduzidas por PPG por ano, até sua volta em 2015, onde o número
# de pós graduações no sentido restrito foram maiores que outras
# orientações.

# Mestrados e cursos que mais ocorrem por ano
ggplot(orient.mestrado.df, aes(ano, fill=curso)) +
  geom_bar(stat = 'count') +
  ggtitle('Orientações por ano') +
  theme(legend.position = 'right') +
  labs(x='Ano',y='Cursos')

# Dentre os mestrados nos programas de pós graduação estudados, pode se perceber
# o grande domínio em volume da pós graduação em Biologia Molecular, que na maioria
# dos anos corresponde a quase metade das orientações de pós graduação por ano
# dentre os PPG estudados.

# Publicações em países:
perfil.df.publicacoes %>%
  filter(!(tipo_producao %in% c('EVENTO','TEXTO_EM_JORNAIS','PERIODICO','ARTIGO_ACEITO'))) %>%
  group_by(tipo_producao,pais_de_publicacao) %>%
  ggplot(aes(ano,tipo_producao,col=pais_de_publicacao)) +
  geom_point(alpha = 0.7) + geom_jitter() +
  labs(x='Tipo de produção',y='País')

# Observa-se deficiência de dados quanto ao país de publicação para periódicos,
# textos em jornais e artigos aceitos para os PPG analizados. Ainda assim, este
# gráfico demonstra bem a heterogeniedade das publicações de livros e/ou capítu-
# los no Brasil e demais países.

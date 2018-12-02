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
perfil <- fromJSON("240BiologiaAnimal/240profile.json")
public <- fromJSON("240BiologiaAnimal/240publication.json")
orient <- fromJSON("240BiologiaAnimal/240advise.json")
graphl <- fromJSON("240BiologiaAnimal/240graph.json")

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
      summarise(num_areas = n_distinct(area)), 
    by = "idLattes") %>%
  rename(OUTRAS_PRODUCOES = DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA)

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

#Indice de bolsas

bolsas.df <- orient.df %>% filter(bolsa == "SIM") %>%
  group_by(natureza, ano) %>%
  summarise(num_bolsas = n()) %>%
  inner_join((orient.df %>%
                group_by(natureza, ano) %>%
                summarise(num_orients = n())), by = c("natureza", "ano")) %>%
  mutate(ratio = num_bolsas/num_orients)

###

#Visualizacao
# Grafico de barras; periodicos por ano
public.periodico.df %>%
  group_by(ano) %>%
  summarise(Quantidade = n()) %>%
  ggplot(aes(ano, Quantidade)) +
  geom_bar(position = "stack",stat = "identity", fill = "darkcyan")+
  ggtitle("Periodicos publicados por ano") +
  geom_text(aes(label=Quantidade), vjust=-0.3, size=2.5)+
  theme_minimal() + labs(x="Ano",y="Quantidade de Periodicos")

#Sob uma perspectiva de publicações em periódicos, o Programa de Pós-Graduação
#de Biologia Animal apresentou pouco crescimento entre 2010 e 2017, registrando
#de 60 a 70 publicações por ano em grande parte da amostra.

#Quantidade de periodicos publicados por professor(a) entre 2010 e 2017
perfil.df %>%
  ggplot(aes(idLattes,PERIODICO)) +
  geom_col(fill = "purple") +
  ggtitle("Periodicos publicados por pesquisador (incluindo mediana)") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Pesquisador(a)",y="Numero de publicacoes") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  geom_hline(yintercept = sum(perfil.df %>% summarize(x = median(PERIODICO))), color = "red")

#Investigando o comportamento dos pesquisadores do programa, os professores
#foram ordenados pelo número total de publicações em periódicos entre 2010 e 2017, 
#cuja mediana foi apresentada pela linha vermelha do gráfico acima, que sinaliza
#26 publicações no período total. O gráfico indica que, dos 19 pesquisadores
#inclusos na base de dados, dois totalizaram publicações que duplicaram este valor
#e outros dois quadruplicaram este valor, com mais de 104 publicações em periódicos.

#publicacao de livros por pais/ano
public.livros.df %>%
  group_by(ano,pais_de_publicacao) %>%
  ggplot(aes(x=ano,y=pais_de_publicacao, color= pais_de_publicacao)) +
  ggtitle("Livros publicados por ano") +
  xlab("Ano") + ylab("Pais") + geom_point() + geom_jitter()

#Apenas três anos apresentaram registros de publicação de livros por parte
#do Programa no período de 2010 a 2017. Entretanto, uma observação notável
#é um maior número de publicações em países estrangeiros que no Brasil, o que
#sinaliza um bom grau de internacionalização do programa.

#Eventos nacionais e internacionais
public.eventos.df %>%
  filter(pais_do_evento %in% 
           c(names(head(sort(table(public.eventos.df$pais_do_evento)
                             , decreasing = TRUE), 10)))) %>%
  group_by(ano_do_trabalho,pais_do_evento) %>%
  ggplot(aes(x=ano_do_trabalho,y=pais_do_evento, color= pais_do_evento)) +
  ggtitle("Participacoes em eventos") +
  xlab("Ano") + ylab("Pais") + geom_point() + geom_jitter()

#Considerando eventos, o Programa de Biologia Animal teve comparecimento maior
#em eventos no Brasil que em países estrangeiros. Ainda assim, o gráfico mostra
# que o programa esteve presente em pelo menos um evento de sede internacional em 
#todos os anos registrados na base de dados.

#Orientacoes completas por ano e natureza
ggplot(orient.df,aes(ano,fill=natureza)) +
  geom_bar(stat = "count", position="dodge") +
  ggtitle("Natureza das Orientacoes Completas Por Ano") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Ano",y="Quantidade") + scale_y_continuous(limits = c(0, 25))

#Observando a evolução do número de orientações completas ao longo dos anos,
#percebe-se que o Programa de Pós-Graduação cresceu consideravelmente nas
#naturezas de doutorado e pós-doutorado após 2010.
#Entretanto, não há um comportamento linear na evolução do número de orientações
#finalizadas. A partir de 2011, todos os anos pares apresentaram um aumento no
#número de pós-doutorados enquanto os anos ímpares seguintes mostram
#retrocesso neste número. Além disso, o ano de 2017 apresentou um aumento
#considerável no número de teses de doutorado, mas uma queda vertiginosa nas
#demais naturezas de orientações - sugerindo uma mudança brusca nas bases
#do programa.

#Bolsas distribuidas por ano - DEPRECIADO; GRÁFICO MELHOR FOI PRODUZIDO

#orient.df %>% filter(bolsa == "SIM") %>%
#ggplot(aes(ano,fill=natureza)) +
#  geom_bar(stat = "count", position = "dodge") +
#  ggtitle("Bolsas disponibilizadas por ano") +
#  theme(legend.position="right",legend.text=element_text(size=7)) +
#  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
#  labs(x="Ano",y="Quantidade de bolsas") + scale_y_continuous(limits = c(0, 25))

#Índice de bolsas entre naturezas e evolução temporal

bolsas.df %>%
  ggplot(aes(x=ano,y=ratio*100,color=natureza, group=natureza)) +
  geom_line() + #facet_wrap(. ~ natureza) + #CASO PREFIRA DIVIDIR AS NATUREZAS
  ggtitle("Porcentagem de orientações contempladas por bolsas") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Ano",y="Índice de bolsas")

#Comparando os gráficos de orientações completas e de bolsas, é possível
#perceber que o número de bolsas oferecidas
#para o Programa não apresentou um comportamento linear ao longo dos anos.
#Além disso, as teses de pós-doutorado se mostram a natureza de pesquisa melhor
#contemplada pelas agências financiadoras, visto que quase todas as observações
#receberam bolsa. Por fim, é possível observar que os anos com maior
#número de orientações de mestrado e doutorado também consistem nos
#anos com maior número de bolsas distribuídas entre o programa, mas a ocorrência
#de pesquisas realizadas sem este apoio financeiro foi marcante em todo o período.

#Grafo de proximidade entre pesquisadores do Programa de Pos-Graduacao
plot(g, vertex.label = NA)

#O grafo acima representa os pesquisadores do Programa de Pós-Graduação em seus vértices
#e a existência de cooperação entre eles em suas arestas. Portanto, é possível
#observar uma considerável cooperação entre os membros do Programa, mas
#alguns pesquisadores relatam baixa diversidade em suas colaborações no período (2010 a 2017).

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
# o grande domínio em volume da pós graduação em Biologia animal, que na maioria
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

#Perfil-Areas - Questao 12

perfil.areas <- perfil.df.areas.de.atuacao %>%
  left_join(perfil.df, by = "idLattes") %>%
  rowwise() %>% #realizar sum() corretamente
  mutate(orientacoes_concluidas = sum(ORIENTACAO_CONCLUIDA_DOUTORADO,
        ORIENTACAO_CONCLUIDA_POS_DOUTORADO, ORIENTACAO_CONCLUIDA_MESTRADO,
        OUTRAS_ORIENTACOES_CONCLUIDAS, na.rm = TRUE)) %>%
  mutate(publicacoes = sum(CAPITULO_DE_LIVRO, EVENTO, PERIODICO,
        LIVRO, TEXTO_EM_JORNAIS, OUTRAS_PRODUCOES, na.rm = TRUE)) %>%
  select(idLattes, grande_area, area, sub_area, especialidade, orientacoes_concluidas, publicacoes)
class(perfil.areas) <- c("tbl_df", "data.frame") #desfazer rowwise

#Graficos ignorando especialidade e subarea, como incluir estas variaveis?
perfil.areas %>%
  select(-sub_area, -especialidade) %>%
  distinct() %>%
  group_by(publicacoes) %>%
  ggplot(aes(publicacoes, orientacoes_concluidas, color = area)) +
  geom_point(shape = 2, size = .8) + geom_jitter() +
  ggtitle('Relação de Orientações Concluídas x Publicações') +
  labs(x='Publicações',y='Orientações concluídas') + facet_wrap(. ~ grande_area, ncol = 2)

#Relação de produção-orientação pelo número de áreas
#Quem trabalha em mais áreas diferentes publica/orienta mais?

perfil.areas %>% 
  select(-sub_area, -especialidade) %>%
  distinct() %>%
  group_by(idLattes, orientacoes_concluidas, publicacoes) %>%
  summarise(num_areas = n()) %>%
  ggplot(aes(publicacoes, orientacoes_concluidas, color = num_areas)) +
  geom_point() +
  ggtitle('Orientações e Publicações pelo Número de Áreas') +
  labs(x='Publicações',y='Orientações concluídas') + facet_wrap(. ~ num_areas)

#Perfil-Areas - Questao 14

public.eventos.df$`autores-endogeno` <- gsub('\\s+', '', public.eventos.df$`autores-endogeno`)
especialidade.orient <- public.eventos.df %>%
  filter (classificacao == "INTERNACIONAL") %>%
  select (`autores-endogeno`) %>%
  separate_rows(`autores-endogeno`, sep = ";") %>%
  distinct() %>%
  arrange(`autores-endogeno`) %>% mutate(internacional = "Sim") %>%
  right_join(perfil.df, by = c("autores-endogeno" = "idLattes")) %>%
  left_join(perfil.areas, by = c("autores-endogeno" = "idLattes")) %>%
  select(`autores-endogeno`, internacional, especialidade, orientacoes_concluidas) %>% distinct() %>%
  rename(idLattes = `autores-endogeno`)

especialidade.orient[is.na(especialidade.orient)] <- 0 #Remove NAs do dataframe
especialidade.orient$especialidade <- sub("^$", "Não informada", especialidade.orient$especialidade)
especialidade.orient$internacional <- sub(0, "Não", especialidade.orient$internacional)

especialidade.orient %>%
  ggplot(aes(internacional, orientacoes_concluidas, color = especialidade)) +
  geom_point(size = .8) + geom_jitter() +
  ggtitle('Orientações concluidas x Participação em congressos internacionais') +
  labs(x='Participacao internacional',y='Orientações concluídas')

#Grafo - Questao 15

V(g)$programa <- df$Programa
V(g)$orient_dout <- perfil.df$ORIENTACAO_CONCLUIDA_DOUTORADO
V(g)$orient_mest <- perfil.df$ORIENTACAO_CONCLUIDA_MESTRADO
V(g)$publicacao <- perfil.df$PERIODICO
E(g)$eventos <- perfil.df$EVENTO
plot(g, vertex.label = NA,vertex.color=V(g)$publicacao,edge.color=E(g)$eventos)
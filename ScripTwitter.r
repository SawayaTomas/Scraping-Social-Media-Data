install.packages("rtweet")
install.packages("httpuv")
install.packages("gganimate")
install.packages(c("gifski", "av"))
install.packages("twinetverse")
library(magick)
library(rtweet)
library(httpuv)
library(tidyverse)
library(knitr)
library(dplyr)
library(ggplot2)
library(gganimate)
library(graphTweets)
library(sigmajs)
library(twitteR)
library(twinetverse)
library(tidytext)
library(stopwords)

argentinaData <- search_tweets2("#Argentina", n = 2000)
argentinaData <- as.data.frame(argentinaData)
argentinaData <- as.list(argentinaData)
write.csv(argentinaData, file="argentina_tweets.csv")

#tendencias <- get_trends(woeid = "argentina")


#Calculamos las fechas de los tweets (de la mas vieja a la mas nueva)
fechasViejas <- argentinaData %>%
  arrange(desc(created_at)) %>%
  slice(n()) %>%
  pull(created_at)

fechasNuevas <- argentinaData %>%
  arrange(desc(created_at)) %>%
  slice(1L) %>%
  pull(created_at)

#Cuantos tweets hay en la base
ntweets <- nrow(argentinaData)

#Cuantos dias hay en el registro
ndias <- as.numeric(fechasNuevas - fechasViejas)

#Numero promedio de tweets por dia
statusPorDia <- nrow(argentinaData)/ndias

#Para saber el numero de usuarios diferentes
total_usuarios <- argentinaData %>%
  pull(screen_name) %>%
  unique() %>%
  length()

#Para ver cuantos tweets originales hay
total_tweet_number <- argentinaData %>%
  filter(!is_retweet) %>%
  pull(status_id) %>%
  unique() %>%
  length()

argentinaData <- argentinaData %>%
  mutate (created_at=as.Date(created_at))

#Graficamos los tweets por dia

ggplot(argentinaData, aes(x = created_at)) +
  geom_bar(position ="identity", fill="blue", alpha=0.3) + 
  labs(x = "Fecha", y = "Numero de Tweets") + 
  theme_minimal()

#Crear una tabla de frencuencia de tweets

dias <- table(weekdays(argentinaData$created_at))
kable(sort(dias, decreasing = T), col.names = c("Día", "Frecuencia"))

#Top N Tweets

top.n.tweets <- argentinaData %>%
  filter(!is_retweet) %>%
  count(screen_name) %>%
  arrange(desc(n)) %>%
  top_n(10)

#Top N RETweets

top.n.REtweets <- argentinaData %>%
  filter(is_retweet) %>%
  count(screen_name) %>%
  arrange(desc(n)) %>%
  top_n(10)

#Tweets mas populares (en base a favoritos o retweets)

mas_retweeted <- argentinaData %>%
  filter(is_retweet == FALSE ) %>%
  arrange(desc(retweet_count))

mas_retweeted %>%
  select(status_id, created_at, screen_name, retweet_count,
         favorite_count) %>%
  head(10)

#Foto del tweet mas popular

mas_popular_foto <- tweet_shot(statusid_or_url = mas_retweeted$status_id[1])
mas_popular_foto
image_write(mas_popular_foto, "tweet_popular.png")


#Tenemos varias herramientas para plasmar los tweets en graficos
##########################
#rtweet
#graphTweets -> Para visualizar la red de tweets
#sigmajs -> para visuarlizar la red de forma interactiva
#twinetverse
############################



#Construccion de la red

net <- argentinaData %>%
  filter(is_retweet == T) %>%
  gt_edges(screen_name, retweet_screen_name) %>%
  gt_nodes() %>%
  gt_collect()

#Extraccion de las aristas:

aristas <- net$edges
tail(aristas)

#Extraemos los nodos:

nodos <- net$nodes
tail(nodos,5)


#lo convertimos a un formato que sigmajs entienda:
nodos <- nodes2sg(nodos)
aristas <- edges2sg(aristas)

#Graficamos el grafo -> sirve para ver como se difunde la informacion a traves de los rts


grafo_plot <- sigmajs() %>%
  sg_nodes(nodos, id, label, size) %>%
  sg_edges(aristas, id, source, target) %>%
  sg_layout(layout = igraph::layout_nicely) %>%
  sg_cluster(
    colors = c(
      "yellow",
      "purple",
      "black",
      "green"
      )
  ) %>%
  sg_settings(
    minNodeSize = 1, 
    maxNodeSize = 5.0,
    edgeColor = "default",
    defaultEdgeColor = "red",
    labelThreshold = 3) %>%
  sg_neighbours()

grafo_plot

#Exploracion del contenido (textos)

#1. remover stop words (articulos por ejemplo que no sirve porque te desvirtua)
#2. descomponer en palabras (tokenizar)
#3. calcular la frecuencia de las palabras (cuales son mas usadas, etc)

head(stopwords("es"))
head(stopwords("en"))

lista_stopwords <- c(stopwords("es"), stopwords("en"), "#Argentina")

#Ya sacamos las stop words, ahora descomponemos en palabras

#remover caracteres indeseables
remove_reg <- "&amp;|&lt;|&gt;"

library(tidytext)
tidy_tweets <- argentinaData %>%
  #ignorar retweets
  filter(is_retweet == FALSE) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  #token = "tweets" permite preservar caracteres comunes en tweets
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% lista_stopwords,
         !word %in% srt_remove_all(lista_stopwords, "'"),
         str_detect(word, "[a-z]"))

#Cuento la frencuencia de palabras

frequency <- tidy_tweets %>%
  count(word, sort = T)

head(frequency, 10) %>%
  gt()

#Visualmente:
freq_palabras <- frequency %>%
  filter(n>80) %>%
  ggplot(aes(x=reorder(word,n), y = n)) +
  geom_col() + 
  theme_bw() +
  labs(y = "", x = "") + 
  theme(legend.position = "none") + 
  coord_flip() 

install.packages("wordcloud2")
library(wordcloud2)

freq20p <- frequency[frequency$n >= 20,]
nube <- wordcloud2(freq20p, size = 0.7, shape = "star")

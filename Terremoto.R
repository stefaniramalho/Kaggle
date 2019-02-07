# Importando os modulos
library(data.table)
library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)

# Definindo o diretorio raiz
setwd("D:/MachineLearning/Kaggle/Terremotos")

# Criando um novo csv com os dados por amostragem com 20 mil linhas
#fread("train.csv") %>% sample_n(20000) %>% fwrite("treino.csv", row.names = F)

### Lendo o conjunto de dados ###
df <- fread("treino.csv")


### Explorando e Tratando os dados ###

# Dimensao dos dados
dim(df)

# Visualizando os dados
head(df)

# Estrutura dos dados
str(df)

# Resumo dos dados
summary(df)

# Visualmente ja se percebe a presenca de outliers
# Frequencia do atributo acoustic_data
ggplot(df, aes(acoustic_data)) +
  geom_histogram(fill = "orange") +
  theme_light() +
  ggtitle("Distribuição da frequência do atributo acoustic_data") +
  xlab("acoustic_data") + ylab("Frequencia")


# Boxplot do atributo acoustic_data
ggplot(df, aes(x = "", y = acoustic_data)) +
  geom_boxplot(fill = "orange") +
  theme_light() +
  ggtitle("Boxplot do atributo acoustic_data") +
  xlab("") + ylab("acoustic_data")

# Coletando os outliers
outliers <- boxplot.stats(df$acoustic_data)$out

# Resumo e analise dos outliers
summary(outliers)

# Funcao para remover outliers com base no iqr
remove_out <- function(x){
  
  q1 = quantile(x$acoustic_data,0.25)
  q3 = quantile(x$acoustic_data, 0.75)
  iqr = q3 - q1
  
  menor = q1 - 1.5 * iqr
  maior = q3 + 1.5 * iqr
  
  # Novo dataset desconsiderando os outliers
  dataset = x[(x$acoustic_data > menor) & (acoustic_data < maior),]
  
  # Total de linhas removidas
  print(paste("Linhas removidas:", (nrow(x) - nrow(dataset))))
  
  return(dataset)
}

# Novo dataset sem outliers
df_2 <- remove_out(df)

# Novo resumo dos dados
summary(df_2)

# Boxplot do atributo acoustic_data dos dados tratados
ggplot(df_2, aes(x = "", y = acoustic_data)) +
  geom_boxplot(fill = "dodgerblue") +
  theme_light() +
  ggtitle("Boxplot do atributo acoustic_data") +
  xlab("") + ylab("acoustic_data")

# Frequencia do atributo acoustic_data do novo dataset
ggplot(df_2, aes(acoustic_data)) +
  geom_histogram(bins = 10, fill = "dodgerblue") +
  theme_light() +
  ggtitle("Distribuição da frequência do atributo acoustic_data") +
  xlab("acoustic_data") + ylab("Frequencia")

# Frequencia do atributo time_to_failure
ggplot(df_2, aes(time_to_failure)) +
  geom_histogram(fill = "dodgerblue") +
  theme_light() +
  ggtitle("Distribuição da frequência do atributo time_to_failure") +
  xlab("time_to_failure") + ylab("Frequencia")

# Boxplot do novo dataset acoustic_data (sem a presenca de outliers)
ggplot(df_2, aes(x = "", y = time_to_failure)) +
  geom_boxplot(fill = "dodgerblue") +
  theme_light() +
  ggtitle("Boxplot do atributo time_to_failure") +
  xlab("") + ylab("time_to_failure")

# Media de time_to_failure por acoustic_data
df_2 %>% group_by(acoustic_data) %>%
  summarise(media = mean(time_to_failure)) %>%
  ggplot(aes(x = acoustic_data, y = media, label = round(media,1))) + 
  geom_bar(stat="identity", fill = "dodgerblue") +
  geom_text(vjust=2, color = "white") +
  theme_light()

# correlacao entre os dados
correlacao = cor(df_2)

# Plotando as correlacoes (sem correlacao entre as variaveis)
corrplot(correlacao, method = "shade")



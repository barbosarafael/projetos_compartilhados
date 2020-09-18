#### analise exploratória ####

# pacote
library(tidyverse)

# base de dados
data_set <- rio::import("turnover_rodrigo/data/turnover-data-set.csv")

data_set <- data_set %>% 
  mutate(
    event = as.factor(event)
  )

glimpse(data_set)

#skimr::skim(data_set) %>% view()

table(data_set$event)
# podemos notar que a base de dados está bem equilibrada quanto ao número de trabalhadores "ativos"
#quanto desligados. Isso indica um alto número de desligamentos. Para melhorar nossa análise, tentaremos
#fazer alguns questionamentos a fim de direcionar nossa abordagem e tentar encontrar os principais
#fatores para o desligamentos destes individuos

# - Existe diferença entre os gêneros quanto ao desligamento?
# - Qual indústria ou setor de trabalho mais deliga?
# - Quanto a idade dos profissionais, existe influência no desligamento?
# - E o meio de candidatura? Impacta nos deligamentos também?



####Primeiros vamos tentar descobrir o perfil socioeconômico destes trabalhadores: #### 
# genero, idade, tempo Experiência, tipo de industria, setor que trabalha, meio da candidatura 

# Tempo experiência

data_set %>% 
  group_by(event) %>% 
  summarise(
    media_experiencia = mean(stag, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = event, y = media_experiencia)) +
  geom_col() +
  geom_label(aes(x = event, y = media_experiencia/2, label = round(media_experiencia,2)))

t.test(data_set$stag~data_set$event)

# não existe diferença entre o tempo de experiência

# Gênero
data_set %>% 
  count(gender, event) %>% 
  ggplot(aes(x = gender, y = n)) + 
  geom_col() + 
  geom_label(aes(x = gender, y = n, label = n)) +
  facet_wrap(~event)

chisq.test(x = data_set$event, y = data_set$gender)
# não há diferença entre os gêneros para os desligamentos dos funcionários

# Idade

data_set %>% 
  group_by(event) %>% 
  summarise(
    media_idade = mean(age, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = event, y = media_idade)) +
  geom_col() +
  geom_label(aes(x = event, y = media_idade/2, label = round(media_idade,2)))

t.test(data_set$age~data_set$event)

# não há diferença entre as idades para os desligamentos dos funcionários

# tipo de indústria
data_set %>% 
  count(industry, event) %>% 
  mutate(
    industry = forcats::fct_reorder(industry, n)
  ) %>% 
  ggplot(aes(x = industry, y = n)) + 
  geom_col() +
  geom_label(aes(x = industry, y = n/2, label = round(n,2))) +
  coord_flip() + 
  facet_wrap(~event)

teste_qui <- chisq.test(x = data_set$event, y = data_set$industry)

teste_qui$observed

# há diferença entre as industrias

# setor
data_set %>% 
  count(profession, event) %>% 
  mutate(
    industry = forcats::fct_reorder(profession, n)
  ) %>% 
  ggplot(aes(x = industry, y = n)) + 
  geom_col() +
  geom_label(aes(x = industry, y = n/2, label = round(n,2))) +
  coord_flip() +
  facet_wrap(~event)
# a grande maioria é do setor de RH

# meio de candidatura

data_set %>% 
  count(traffic, event) %>% 
  mutate(
    traffic = forcats::fct_reorder(traffic, n)
  ) %>% 
  ggplot(aes(x = traffic, y = n)) + 
  geom_col() +
  coord_flip() +
  facet_wrap(~event)

chisq.test(x = data_set$event, y = data_set$traffic)

# existe diferença entre as plataformas de candidatura


data_set %>% 
  count(way, event) %>% 
  mutate(
    traffic = forcats::fct_reorder(way, n)
  ) %>% 
  ggplot(aes(x = way, y = n)) + 
  geom_col() +
  geom_label(aes(x = way, y = n, label = n)) +
  coord_flip() +
  facet_wrap(~event)

chisq.test(x = data_set$event, y = data_set$way)

# existe diferença entre os meios de transporte para o trabalho


# De acordo com nossa primeira análise conseguimos perceber:

# - A indústria em que os funcionários trabalham tem influëncia nos desligamentos,
#onde o banco, outros meios e contrução civil são os que possuem maior percentual
#desligamento. Além disso o meio com menor percentual de desligamento é o de TI

# - Quem vai a pé para o trabalho possui menos percentual de desligamento. Pode
#indicar melhor qualidade de vida (Big_5)

# - O meio de candidatura também possui influência nos desligamentos. Isso pode 
#nos indicar que os sistemas de candidatura não estão fazendo boas indicações.
#Vale aprofundarmos esta análise posteriormente.

# Gênero e idade foram atributos que não expressaram influências nos desliamntos.
#Isto pode ser visto com bons olhos, pois nossa base não possui diferenças de
#gênero quanto às demissões nem discrinação com as idades


# Feito esta primeira análise e respondendo nossas primeiras perguntas, conseguimos
# ampliar nossa visão e agora vamos aprofundar um pouco mais e fazer outras perguntas:

# - Qual o perfil relacionado a personalidade de acordo com as variáveis que se mostraram
#influentes nos desligamentos de funcionários? Indústria

# - O meio de candidatura faz as indicações certas para as vagas de acordo com o 
# perfil de personalidade? De acordo com o Big_5

# - As variáveis que não se mostraram influentes possuem algum impacto nos atributos
# como: Indústria, Meio de transporte ou Meio de candidatura?

# Análise 

data_set %>% 
  filter(event == 0) %>% 
  group_by(industry) %>% 
  summarise(extraversion = mean(extraversion, na.rm = TRUE),
            independ = mean(independ, na.rm = TRUE),
            selfcontrol = mean(selfcontrol, na.rm = TRUE),
            anxiety = mean(anxiety, na.rm = TRUE),
            novator = mean(novator, na.rm = TRUE))


# facetamento
# genero, indústria, events
data_set %>% 
  count(event, gender, industry) %>% 
  ggplot(aes(x = gender, y = n, fill = event)) + 
  geom_col() +
  facet_wrap(~industry)

data_set %>% 
  nest(traffic)


dados_tops <- dados %>% 
  mutate(
    tops = paste(top1, "|", top2)
  )
dados_tops$tops %>%
  paste(collapse = "|") %>%
  str_split(pattern = "\\|", simplify = TRUE) %>%
  table() %>% 
  as.data.frame()
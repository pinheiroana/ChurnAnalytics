
#               Customer Churn Analytics 


# A rotatividade (churn) de clientes ocorre quando clientes ou assinantes param de fazer negócios 
# com uma empresa ou serviço. Também é conhecido como perda de clientes ou taxa de cancelamento.

# Um setor no qual saber e prever as taxas de cancelamento é particularmente útil é o setor de telecomunicações, 
# porque a maioria dos clientes tem várias opções de escolha dentro de uma localização geográfica.

# Neste projeto, vamos prever a rotatividade (churn) de clientes usando um conjunto de dados de telecomunicações. 
# Usaremos a regressão logística, a árvore de decisão e a floresta aleatória como modelos de Machine Learning. 

# Usaremos um dataset oferecido gratuitamente no portal IBM Sample Data Sets.

# Instalando pacotes

# install.packages('plyr')
# install.packages('corrplot')
# install.packages('ggplot2')
# install.packages('gridExtra')
# install.packages('ggthemes')
# install.packages('caret')
# install.packages('MASS')
# install.packages('randomForest')
# install.packages('party')

# Carregando os pacotes

library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)


# Carregando e Limpando Dados

churn <- read.csv('Telco-Customer-Churn.csv')
str(churn)
View(churn)


# A coluna "Churn" é o nosso alvo.

# Verificando e removendo valores ausentes

sapply(churn, function(x) sum(is.na(x)))
churn <- churn[complete.cases(churn), ]


# Limpezas e ajustes das variáveis categoricas.


# Mudando "No internet service" para "No" por seis colunas, que são: 
# "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "streamingTV", 
# "streamingMovies".

cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

# Mudando "No phone service" para "No" para a coluna “MultipleLines”.

churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

  

# Como a permanência mínima é de 1 mês e a permanência máxima é de 72 meses, 
# podemos agrupá-los em cinco grupos de posse (tenure): 
# “0-12 Mês”, “12–24 Mês”, “24–48 Meses”, “48–60 Mês” Mês ”,“> 60 Mês”

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

churn$tenure_group <- sapply(churn$tenure,group_tenure)

churn$tenure_group <- as.factor(churn$tenure_group)


# Alterando os valores na coluna “SeniorCitizen” de 0 ou 1 para “No” ou “Yes”.

churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))



# Removendo as colunas que não precisamos para a análise.

churn$customerID <- NULL
churn$tenure <- NULL
View(churn)


# Análise Exploratória de Dados e Seleção de Recursos 

# Correlação entre variáveis numéricas.

numeric.var <- sapply(churn, is.numeric)

corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nGráfico de Correlação para Variáveis Numéricas", method="number")


# Os encargos mensais e os encargos totais estão correlacionados. Se colocamos as duas 
# no modelo pode occorrer overfitting. Então será removido uma variável.

# Removendo Total Charges.

churn$TotalCharges <- NULL


# Gráficos de barra de variáveis categóricas

p1 <- ggplot(churn, aes(x=gender)) + ggtitle("Gender") + xlab("Sexo") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p2 <- ggplot(churn, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p3 <- ggplot(churn, aes(x=Partner)) + ggtitle("Partner") + xlab("Parceiros") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p4 <- ggplot(churn, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependentes") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()

grid.arrange(p1, p2, p3, p4, ncol=2)


p5 <- ggplot(churn, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Telefonia") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p6 <- ggplot(churn, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Múltiplas Linhas") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p7 <- ggplot(churn, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p8 <- ggplot(churn, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()

grid.arrange(p5, p6, p7, p8, ncol=2)


p9 <- ggplot(churn, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p10 <- ggplot(churn, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p11 <- ggplot(churn, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p12 <- ggplot(churn, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()

grid.arrange(p9, p10, p11, p12, ncol=2)


p13 <- ggplot(churn, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p14 <- ggplot(churn, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p15 <- ggplot(churn, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p16 <- ggplot(churn, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p17 <- ggplot(churn, aes(x=tenure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()

grid.arrange(p13, p14, p15, p16, p17, ncol=2)


# Verificando as variáveis categóricas, como todas parecem ter uma distribuição 
# razoalvemente ampla, portanto todas serão mantidas para análise


# Modelagem Preditiva 

# Regressão Logística

# Dividindo os dados em conjuntos de treinamento e testes

intrain <- createDataPartition(churn$Churn,p=0.7,list=FALSE)
set.seed(2017)
training <- churn[intrain,]
testing <- churn[-intrain,]


# Confirme se a divisão está correta

dim(training); dim(testing)
training$Churn <- as.factor(training$Churn)


# Treinando o modelo de regressão logística

LogModel <- glm(Churn ~ ., family=binomial(link="logit"), data=training)
print(summary(LogModel))


# Análise de Variância - ANOVA

anova(LogModel, test="Chisq")

# Avaliando a capacidade preditiva do modelo

testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))


# Matriz de Confusão de Regressão Logística

print("Confusion Matrix Para Logistic Regression"); table(testing$Churn, fitted.results > 0.5)


# Odds Ratio

# Odds Ratio é a chance de um evento acontecer.

exp(cbind(OR=coef(LogModel), confint(LogModel)))


# Árvore de Decisão

# Para plotar a árvore de decisão será usada três variáveis, “Contract”, “tenure_group” e “PaperlessBilling”.

tree <- ctree(Churn ~ Contract+tenure_group+PaperlessBilling, training)
plot(tree, type='simple')


# Matriz de Confusão da Árvore de Decisão

pred_tree <- predict(tree, testing)
print("Confusion Matrix Para Decision Tree"); table(Predicted = pred_tree, Actual = testing$Churn)


# Precisão da árvore de decisão

p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))


# Random Forest 

set.seed(2017)

rfModel <- randomForest(Churn ~ ., data = training)
print(rfModel)
plot(rfModel)

# A previsão é muito boa ao prever "Não". 
# A taxa de erros é muito maior quando se prevê "sim".

# Prevendo valores com dados de teste

pred_rf <- predict(rfModel, testing)

# Confusion Matrix
print("Confusion Matrix Para Random Forest"); table(testing$Churn, pred_rf)

# Taxa de erro para o modelo
plot(rfModel)


# Recursos mais importantes

varImpPlot(rfModel, sort=T, n.var = 10, main = 'Top 10 Feature Importance')
















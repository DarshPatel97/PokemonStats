install.packages("plyr")
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")
install.packages("ggbarplot")
library(ggbarplot)
library(plyr)
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)

pokemon<-read.csv(file = 'C:/Users/Aman/Desktop/Analytics/pokemon.csv')
pokemon
str(pokemon)
head(pokemon)
summary(pokemon)


#Frequency of pokemon types
pokemontype <- table(pokemon$type1)
pokemontype
barplot(pokemontype,
        beside = TRUE,
        main = "Type of pokemons",
        xlab = "Pokemon Types",
        ylab = "Frequency"
)

#Attack of legendary pokemons
ggplot(pokemon, aes(x=is_legendary, y=attack)) +
  geom_boxplot(fill="green") +
  xlab("Legendary Pokemon")+ ylab("Attack")+
  labs(title = 'Attack of legendary pokemons')

#Height vs Weight of pokemons
x<-pokemon$height_m
y<-pokemon$weight_kg
x
x_max<-max(x)
y_max<-max(y)
x_max
y_max
plot(x, y, 
     xlab = "height", 
     ylab = "weight",
     xlim = c(0,15), 
     ylim = c(0,1000), 
     pch = 16)


#Speed of pokemons
ggplot(pokemon, aes(x=type1, y=speed)) +
  geom_boxplot(fill="red") +
  xlab("Pokemon Type")+ ylab("Speed")+
  labs(title = 'Fastest pokemon types')


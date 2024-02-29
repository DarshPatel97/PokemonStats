install.packages("plyr")
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")
install.packages("ggbarplot")
install.packages("tidyr")
library(ggbarplot)
library(plyr)
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)
library(stats)
library(tidyr)

pokemon<-read.csv(file = 'C:/Users/user/Downloads/Pokemon Statistics/pokemon.csv')
pokemon
str(pokemon)
head(pokemon)
summary(pokemon)

#3 Determine strength of pokemons
classfication_count <- table(pokemon$classfication)
print(classfication_count)
barplot(classfication_count,
        beside = TRUE,
        main = "Classification of pokemons",
        xlab = "Classification",
        ylab = "Frequency",
        ylim = c(4,10),
        las=2)


#4 Frequency of pokemon types
pokemontype <- table(pokemon$type1)
pokemontype
barplot(pokemontype,
        beside = TRUE,
        main = "Type of pokemons",
        xlab = "Pokemon Types",
        ylab = "Frequency",
        las=2
)


#5 Identify number of total abilities
all_abilities <- unlist(strsplit(as.character(pokemon$abilities), ","))


#6 Remove leading and trailing white spaces
all_abilities <- trimws(all_abilities)
print(all_abilities)

#7 Count the number of distinct abilities
total_distinct_abilities <- length(unique(all_abilities))

#8 Print the total number of distinct abilities
print(total_distinct_abilities)

#9 Frequency of abilities
ability_frequency <- table(all_abilities)
print(ability_frequency)

sorted_ability_frequency <- sort(ability_frequency, decreasing = TRUE)

top_10_abilities <- head(sorted_ability_frequency, 10)

print(top_10_abilities)

cleaned_ability_names <- gsub("[^[:alnum:][:space:]]", "", names(top_10_abilities))

print(cleaned_ability_names)

barplot(top_10_abilities, 
        main = "Top 10 Abilities Used by Pokémon", 
        xlab = "Abilities", 
        ylab = "Frequency",
        ylim = c(0,30),
        col = "skyblue",
        las = 2,
        names.arg = cleaned_ability_names,
        theme(plot.margin = margin(0, 0, 0.5, 0), "cm"))

#10 which pokemon type are the users of these abilities

result_table <- data.frame(cleaned_ability_names = character(), type1 = character(), stringsAsFactors = FALSE)

for (ability in top_10_abilities) {
  # Filter the data to include only the Pokémon with the ability
  pokemon_with_ability <- pokemon[!is.na(pokemon$cleaned_ability_name),  ]
  
  # Calculate the frequency of Pokémon types for the Pokémon with the ability
  type_frequency <- table(pokemon_with_ability$type1)
  
  # Identify the Pokémon type with the highest frequency
  highest_frequency_type <- names(sort(type_frequency, decreasing = TRUE)[1])
  
  # Add the ability and highest frequency type to the result table
  result_table <- rbind(result_table, data.frame(abilities = ability, type1 = highest_frequency_type))
}

# Print the result table
print(result_table)


#Attack of different pokemons types
ggplot(pokemon, aes(x=reorder(type1, -attack), y=attack)) +
  geom_boxplot(fill="green") +
  xlab("Pokemon Types")+ ylab("Attack Damage")+
  labs(title = 'Attack of legendary pokemons') +
  theme(plot.title = element_text(hjust = 0.5))

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


# Height vs weight 
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
     xlim = c(0,5), 
     ylim = c(0,400), 
     pch = 16)


#Speed of pokemons
ggplot(pokemon, aes(x= reorder(type1, -speed), y=speed)) +
  geom_boxplot(fill="red") +
  xlab("Pokemon Type")+ ylab("Speed")+
  labs(title = 'Fastest pokemon types', hjust = 0.5) +
  theme(plot.title = element_text(hjust = 0.5))


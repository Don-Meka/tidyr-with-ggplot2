pokemon, stats: pokemon_species
growth rate: growth_rates
types: types_names
local_lang: languages

pokemon_species has pokemon, stats, growth_rate_id
growth_rates has growth_rate_id ish
pokemon_types has pokemon types for pokemon species
type_names has types
languages has language names for types

library(ggplot2)
library(dplyr)
library(tidyr)

###loading files####
## pokemon
pokemon_species <- read.csv("~/Emekas Documents/R-Directory/Don Data/pokeapi-master/data/v2/csv/pokemon_species.csv")

## pokemon stats
pokemon_stats <- read.csv("~/Emekas Documents/R-Directory/Don Data/pokeapi-master/data/v2/csv/pokemon_stats.csv")

## pokemon stat names
stat_names <- read.csv("~/Emekas Documents/R-Directory/Don Data/pokeapi-master/data/v2/csv/stat_names.csv")

## pokemon types
pokemon_types <- read.csv("~/Emekas Documents/R-Directory/Don Data/pokeapi-master/data/v2/csv/pokemon_types.csv")

## pokemon type names
type_names <- read.csv("~/Emekas Documents/R-Directory/Don Data/pokeapi-master/data/v2/csv/type_names.csv")

## languages
languages <- read.csv("~/Emekas Documents/R-Directory/Don Data/pokeapi-master/data/v2/csv/languages.csv")

## generations
generations <- read.csv("~/Emekas Documents/R-Directory/Don Data/pokeapi-master/data/v2/csv/generations.csv")

###///


###first part of df###
df <- pokemon_species[,c("id", "identifier", "generation_id", "growth_rate_id")]
## dont need? type <- pokemon_types
####

##stats##
## subsetting stat names
## 9 represents english in the languages dataframe
stat_names2 <- dplyr::filter(stat_names, local_language_id == 9)


## replacing data ish
for (i in 1:length(stat_names2$stat_id)){
  pokemon_stats$stat_id <- gsub(pattern = i, replacement = stat_names2$name[i], x = pokemon_stats$stat_id) 
}




## cleaning pokemon stats df
pokemon_stats2 <- dplyr::filter(pokemon_stats, pokemon_id <= 721)
pokemon_stats2 <- spread(subset(pokemon_stats2, select = -c(effort)), stat_id, base_stat)



df <- cbind(df, pokemon_stats2[,2:7])
##end###


#======
## getting types in der
  ## subsetting type names
  ## 9 represents english in the languages dataframe
  type_names2 <- dplyr::filter(type_names, local_language_id == 9)
  
pokemon_types2 <- dplyr::filter(pokemon_types, pokemon_id <= 721)
pokemon_types2 <- dplyr::filter(pokemon_types2, slot == 1)

for (i in length(type_names2$type_id):1){
  pokemon_types2$type_id <- gsub(pattern = type_names2$type_id[i], replacement = type_names2$name[i], x = pokemon_types2$type_id) 
}

df <- cbind(df, pokemon_types2[2])

summary(df)
head(df)

##////
  df %>%
  gather(-id, -identifier, -generation_id, -growth_rate_id, -type_id, key = "stat", value = "value") %>% 
  ggplot(aes(x = value, y = growth_rate_id, color = type_id, shape = factor(generation_id))) +
  geom_point() +
  facet_wrap(~ stat, scales = "free") +
  theme_bw()

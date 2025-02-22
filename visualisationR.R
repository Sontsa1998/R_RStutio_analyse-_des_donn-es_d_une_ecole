if (!require (ggplot2) & !require(dplyr) & !require(tidyr)) {
  install.packages("ggplot2")
  install.packages("tidyr")
  install.packages("dplyr")
}
library(ggplot2)
library(dplyr)
library(tidyr)


#Importons notre database 
eda_data = read.csv2(file = "all_seasons.csv", sep = ",", dec = "." )
View(eda_data)

#Analysons le jeux de données et analysons les donnnes manquante
is.na(eda_data) #Aucune valeur manquante
sum(colSums(is.na(eda_data)))

#En cas de valeurs manquante, il est primordiale de supprimer les données manquantes
#Supprimons les valeurs manquantes
nrow(eda_data) #(12844 initialement dans notre base de données)
drop_na(eda_data) #Suppression des valeurs manquante
nrow(eda_data) #apres suppresion des valeurs manquante

#Conclusion nous avons pas de valeur manquante dans notre jeux de données

#En cas de valeurs manquante, remplissons les valeurs manquantes en utilisant la fonction Fill
fill(eda_data)


# Statistique avec filtre 
summary(eda_data)

#reduire le dataset pour faire une visualisation 
select_joueur_de_moins_de_20_ans = subset(eda_data, age <= 18)
View(select_joueur_de_moins_de_20_ans)
ggplot(select_joueur_de_moins_de_20_ans) + geom_point(aes(x = player_weight, y = gp), color="green", size=5, alpha=0.3)

#faisons les boxplot
ggplot(select_joueur_de_moins_de_20_ans) + geom_boxplot(aes(x= age, y = player_height), color='blue', fill = "wheat", varwidth = TRUE)

#Geom_bar
ggplot(select_joueur_de_moins_de_20_ans) + geom_bar(aes(x = age), color="cyan", fill="cyan", varwidth=TRUE)

#Geom_col
ggplot(select_joueur_de_moins_de_20_ans) + geom_col(aes(x = gp, y = player_name))


table(eda_data$player_name) #Permet de voir les statistiques
colSums(is.na(eda_data))

View(eda_data)
#Renommons la colonne player_name en name
colnames(eda_data)[2] = "Name"
colnames(eda_data)[8] = "Pays"
View(eda_data)

#creons une nouvelle qui prendra le nombre de point de chaque joueur * son gp
eda_data$Score_total<- eda_data$pts * eda_data$gp 
View(eda_data)

#selectionnons les scores total dont la somme est supérieure a 50 (je veux juste les noms , l'age, les points, les gp ainsi que les scoretotal)

colnames(eda_data)[3] = "Team"
colnames(eda_data)[5] <- "Height"
colnames(eda_data)[6] <- "Weight"
colnames(eda_data)[9] <- "Year"
colnames(eda_data)[10] <- "Round"
colnames(eda_data)[11] <- "Number"
colnames(eda_data)[16] <- "Rating"
colnames(eda_data)[17] <- "PCT"

select_people_with_less_than_10 <-eda_data[eda_data$Score_total < 10, c("Name", "age", "gp", "pts", "Score_total")]  
View(select_people_with_less_than_10)

#calculons la somme des clients regroupé par age et scoretotal
#na.rm=TRUE permet d'ignorer les valeurs manquante
display_result_group_by <- select_people_with_less_than_10 %>% group_by(age) %>% summarise(somme = sum(Score_total, na.rm = TRUE), moyenne = mean(pts, na.rm=TRUE), Total = n())
display_result_group_by

prop.table(table(select_people_with_less_than_10$age))

#Faisons le visuel des points
ggplot(p<-eda_data, aes(x= age, y = college)) + geom_point()
p<-p + labs(
  title = "Fuel Economic in Age per GP",
  subtitle = " Data from 1999 to 2008",
  x = "Engine Dislacement, in liters",
  y = "Highway miles per gallon",
  caption = "MPG data from the ggplot2 package"
)

ggplot(p<- eda_data) + geom_histogram(aes(x = gp))
  
p<- p + aes(y = gp) + labs(title = 'Fuel economic for of cars ', x='Engine displacement, in liters', y='Highway miles per gallon' , caption = 'MPG data from ggplot 2  package')
 
p

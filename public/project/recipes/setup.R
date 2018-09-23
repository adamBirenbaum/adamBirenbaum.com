library(rvest)


url <- "https://www.allrecipes.com/recipes/?grouping=all"

web <- read_html(url)

categ <- html_nodes(web,'.all-categories-col a')

categ <- html_text(categ)

write.csv(categ,"D:/abire/Documents/recipes/categories.csv",row.names = F)



url2 <- "https://simple.wikipedia.org/wiki/List_of_vegetables"

web <- read_html(url2)

veg <- html_nodes(web, "p+ ul li")



a <- html_text(veg)
write.csv(a,"D:/abire/Documents/recipes/vegetable.csv",row.names = F)


url2 <- "https://simple.wikipedia.org/wiki/List_of_fruits"
web <- read_html(url2)
veg <- html_nodes(web, ".column-width a")

veg <- gsub('"',"'",veg,fixed = T)
veg <- gsub(".*wiki/(.*)' title.*$","\\1",veg)

write.csv(veg,"D:/abire/Documents/recipes/fruits.csv",row.names = F)




url2 <- "http://nosetotailapp.com/meat-cuts.php"
web <- read_html(url2)
veg <- html_nodes(web, "br~ a")
veg <- html_text(veg)


write.csv(veg,"D:/abire/Documents/recipes/meats.csv",row.names = F)


url2 <- "https://en.wikipedia.org/wiki/List_of_culinary_herbs_and_spices"
web <- read_html(url2)
veg <- html_nodes(web, ".column-width a")
veg <- html_text(veg)

write.csv(veg,"D:/abire/Documents/recipes/spices.csv",row.names = F)



a <- 1:24
a <- round(a/24, digits = 3)
aa <- paste0(1:24,"/24")

dput(paste0(a," = ",aa))




a <- read.csv("D:/abire/Documents/recipes/dynamic_recipe.csv",stringsAsFactors = F)

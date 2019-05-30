library(httr)
library(magick)
library(lubridate)


key <- "SKZgO0HQLP3BrRqBt8uBNAGne2Ac3piGkeRwnaEF"

format_date <- function(x){
  
  if (nchar(x) == 1){
    return(paste0("0",x))
  }else x
}

re <- GET(paste0("https://api.nasa.gov/EPIC/api/natural/images?api_key=",key))
re <- content(re)

img_name <- sapply(re,function(x) x$image)
img_date <- re[[1]]$date

im_month <- format_date(month(img_date))
im_day <- format_date(day(img_date))
im_year <- year(img_date)

file_format <- "jpg"
img_url <- paste0("https://epic.gsfc.nasa.gov/archive/natural/",im_year,"/",im_month,"/",im_day,"/",file_format,"/",img_name,".",file_format)

direct <- "/var/www/adambirenbaum.com/public/project/earth/"
seq_letters <- letters[1:length(img_url)]

for (i in 1:length(img_url)){
  download.file(img_url[i],destfile = paste0(direct,"img_",seq_letters[i],".",file_format))
  
}

n <- length(img_url)


list.files(path = "/var/www/adambirenbaum.com/public/project/earth", pattern = paste0("*.",file_format), full.names = T) %>% 
  image_read %>% # reads each image file
  image_join() %>% # joins image
  image_animate(fps=5) %>% # animates, can opt for number of loops
  image_write(paste0(direct,"earth.gif"))


date_txt <- paste0(im_month,"-",im_day,"-",im_year)
write.table(date_txt,file = paste0(direct,"date.txt"),row.names = F,col.names = F)


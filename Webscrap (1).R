library(rvest)
library(dplyr)



get_affliation = function (journallink){
  html = read_html(journallink)
  affliation = html %>% html_nodes(".affiliation-name")%>%html_text()
  return(affliation)
  
}

get_keyword = function (keywordlink){
  html = read_html(keywordlink)
  keyword = html %>% html_nodes(".art-keywords")%>%html_text()%>%paste(collapse = ",")
  
  return(keyword)
  
}

get_publicationdate = function (datelink){
  html = read_html(datelink)
  date = html%>%html_nodes("span:nth-child(4)")%>%html_text()
  return(date)
}

get_abstract = function (abstractlink){
  html = read_html(abstractlink)
  abstract = html%>%html_nodes(".art-abstract")%>%html_text()
  
  return(abstract)
}

get_authors = function(authorlink){
  html = read_html(authorlink)
  authors = html%>%html_nodes(".art-authors")%>%html_text()
  
  return(authors)
}

get_title = function(titlelink){
  html = read_html(titlelink)
  
  title = html%>%html_nodes(".title")%>%html_text()
  return(title)
}

dataset = data.frame()

for(page_result in seq(1, 5)){
  link = paste0("https://www.mdpi.com/search?sort=pubdate&page_no=",page_result,"&page_count=50&year_from=2010&year_to=2022&countries=MALAWI&q=Malawi&view=default")
 page = read_html(link)
  
 #link = "https://www.mdpi.com/search?sort=pubdate&page_count=50&q=Malawi&year_from=2010&year_to=2022&featured=&subjects=&journals=&article_types=&countries=MALAWI"
 #page = read_html(link)
 
 title = sapply(links, FUN = get_title)
 authors = sapply(links, FUN = get_authors)
 abstract = sapply(links, FUN = get_abstract)
 links = page %>% html_nodes(".color-grey-dark a") %>% html_text()
 affliation = sapply(links, FUN = get_affliation)
 keyword = sapply(links, FUN = get_keyword)
 date = sapply(links, FUN = get_publicationdate)
 
 dataset = rbind(dataset, data.frame(title,authors,abstract,links,affliation,keyword,date))
 
 }
  





library(rvest)
library(dplyr)

#### Scrapping 10 seasons results of national polish football team
start_nr_url <- 89
start_list <- as.list(seq(from = start_nr_url-2*9, 
                          to = start_nr_url, 
                          by = 2))
url_core <- "http://www.90minut.pl/rep_mecze.php?id_sezon="

result_list <- lapply(start_list, function(nr) {
  url <- paste0(url_core, as.character(nr))
  www <- read_html(url) %>%
    html_nodes("table")
  result_tables <- lapply(www, html_table, fill = TRUE)
  
  result_tables <- result_tables[[4]]
  
  result_tables <- result_tables[3:nrow(result_tables), 2:8] %>%
    select(-X5)
  names(result_tables) <- result_tables[1,]
  result_tables <- result_tables[2:nrow(result_tables),]
  rownames(result_tables) <- seq_len(nrow(result_tables))
  return(result_tables)
})

results <- do.call("rbind", result_list)
names(results) <- c("data","miejsce","rozgrywki","gospodarze","wynik","goscie")
results$wynik <- gsub(" pd\\.k\\. \\d-\\d", "", results$wynik)
results$rywal <- ifelse(results$gospodarze=="Polska", results$goscie, results$gospodarze)
results$gole_zdobyte <- ifelse(results$gospodarze=="Polska", gsub("-\\d+$", "", results$wynik),gsub("^\\d+-", "", results$wynik))
results$gole_stracone <- ifelse(results$gospodarze!="Polska",  gsub("-\\d+$", "", results$wynik),gsub("^\\d+-", "", results$wynik))
results$gole_stracone <- as.numeric(results$gole_stracone)
results$gole_zdobyte <- as.numeric(results$gole_zdobyte)
results$data <- as.Date(results$data, format = "%Y-%m-%d")
results$miesiac <- substr(as.character(results$data), start = 6, stop = 7)
results$rok <- substr(as.character(results$data), start = 1, stop = 4)
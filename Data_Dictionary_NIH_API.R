# Read in libraries
library(httr)
library(jsonlite)
library(xml2)
library(dplyr)
library(tidyverse)
library(purrr)
library(splitstackshape)
library(data.table)

### Cleaning Entity Export Data:
# Read in Entity data from Watson and name it Entities
## Must use reg-ex to put dashes in between strings // API doesn't recognize blank space
## remove c()
## remove hyphen before strings 
Entities <- gsub("-|\\s+|^$", "-", Entities)
Entities <- as.data.frame(Entities)
Entities <- concat.split(Entities, 1)
Entities$Entities_001 <-  gsub("^c\\(|\\)$", "", Entities$Entities_001)
Entities2 <- melt(Entities, id.vars = 1)
Entities2$value <- sub("^[^[:alnum:]]", "", Entities2$value)
Entities2$value <- gsub('"', '', Entities2$value)
Entities2$value[387] <- gsub("[()]", "", Entities2$value[387])

Entities_Clean <- Entities2$value
Entities_Clean <- as.data.frame(Entities_Clean)

# Running API Script:
fetch_data <- function(query_string = 'diabetic foot', UTS_API_KEY = '32de27a7-1e5b-4519-a910-1b36d99c280f', version = 'current') {
  response <- POST('https://utslogin.nlm.nih.gov/cas/v1/api-key', encode='form', body=list(apikey = UTS_API_KEY))
  
  # print out the status_code and content_type
  message(status_code(response), "\n", headers(response)$`content-type`)
  action_uri <- xml_text(xml_find_first(content(response), '//form/@action')); message(action_uri)
  
  # Service Ticket
  response <- POST(action_uri, encode = 'form', body=list(service = 'http://umlsks.nlm.nih.gov'))
  ticket <- content(response, 'text'); message(ticket)
  
  # build search_uri using the paste function for string concatenation
  search_uri <- paste0('https://uts-ws.nlm.nih.gov/rest/search/', version)
  # pass the the query params into httr GET to get the response 
  response <- GET(search_uri, query=list(ticket=ticket, string=query_string))
  ## print out some of the results
  message(search_uri, "\n", status_code(response), "\n", headers(response)$`content-type`)
  fromJSON(content(response, 'text'))
}

# if you have a list of query strings, then
df_list <- lapply(Entities_Clean$Entities_Clean, fetch_data, UTS_API_KEY = "32de27a7-1e5b-4519-a910-1b36d99c280f")
df_list[2]
str(df_list[2])

### Transforming Exported Data from NIH API:  
## (list of nested dfs) into one df
all_entities <- data.table::rbindlist(df_list)
str(all_entities$result[[672]])
all_entities$result[[672]]$ui
all_entities$result[[672]]$rootSource
all_entities$result[[672]]$uri
all_entities$result[[672]]$name
is.list(all_entities)

final_ent <- bind_rows(all_entities, .id = "column_label") %>%
  unnest(cols = "result")
view(final_ent) 
final_ent[1:4] <- list(NULL)
write.csv(final_ent,"~mbroadway/Desktop/data_dictionary_nih.csv", row.names = FALSE)




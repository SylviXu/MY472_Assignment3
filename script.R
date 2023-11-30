library(RSelenium)
library(tidyverse)
library(rvest)
library(xml2)
library(netstat)

# Set up the driver and navigate to the target url.
rD <- rsDriver(browser=c("firefox"), verbose = F, port = 3456L, chromever = NULL) 
driver <- rD[["client"]]
url <- 'https://en.wikipedia.org/wiki/List_of_research_universities_in_the_United_States'
driver$navigate(url)

# Write a function to get tables and urls automatically.
table_scrape <- function() {
  src <- driver$getPageSource() # Get source code of the page
  
  # Get all the tables' html on the Wikipedia page.
  result_html <- read_html(src[[1]]) %>%
    html_elements("table")
  
  # Scrape all the tables.
  result_table <- result_html %>%
    html_table()
  
  # Remove the fifth column of the second table (the R2 table), because there are no values inside.
  result_table[[2]] <- result_table[[2]] %>% select(-5)

  # Create a blank list to store the final table results.
  extracted_table <- list()
  
  # Use a for loop to traverse the first two tables (the R1 and R2) to extract the relevant URL of each institution.
  # Then combine the URL columns into the original tables, and store them respectively into the list created before.
  for (i in c(1,2)) {
    links <- result_html[[i]]%>%
      html_nodes("td:nth-child(1) a") %>%
      html_attr("href")
    links_df <- data.frame(Wikipedia_Link = links)
    new_table <- bind_cols(result_table[[i]], links_df)
    extracted_table[[i]] <- new_table
  }

  
  # Now the item "extracted_table" contains the R1 and R2 tables we need. Here I will rename each table to make them more explicit.
  names(extracted_table) <- c("R1 (Very High Research Activity)", "R2 (High Research Activity)")
  
  # Return the function result.
  return(extracted_table)
}

# Scrape the tables.
table_scrape()

# Exercise b
scrapeAdditionalData <- function(university_data) {
  # Initialize empty lists to store additional data
  coordinates <- vector("list", length = nrow(university_data))
  endowment <- vector("list", length = nrow(university_data))
  total_students <- vector("list", length = nrow(university_data))
  
  # Loop through each university's Wikipedia page
  for (i in 1:nrow(university_data)) {
    # Navigate to the university's Wikipedia page
    university_page <- read_html(paste0("https://en.wikipedia.org",university_data[i, "Wikipedia_Link"]))
    
    # Extracting geographic coordinates
    coordinates[i] <- university_page %>% 
      html_nodes("span.geo-dec") %>%
      html_text() %>%
      first()
    
    # Extracting endowment
    endowment_nodes <- university_page %>%
      html_nodes(".infobox tbody tr:contains('Endowment') td:nth-child(2)")
    
    if (length(endowment_nodes) > 0) {
      endowment[[i]] <- endowment_nodes %>% html_text()
    } else {
      endowment[[i]] <- "NULL"
    }
    clean_endowment[i] <- gsub("\\[\\d+\\]", "", endowment[[i]])
    
    # Extracting total number of students
    undergrad_nodes <- university_page %>%
      html_nodes(".infobox tbody tr:contains('Undergraduates') td:nth-child(2)")
    if (length(undergrad_nodes) > 0) {
      undergrad[[i]] <- undergrad_nodes %>% html_text()
    } else {
      undergrad[[i]] <- "NULL"
    }
    clean_undergrad[i] <- as.numeric(gsub("\\[\\d+\\]", "", gsub(",", "", undergrad[[i]])))
    
    postgrad_nodes <- university_page %>%
      html_nodes(".infobox tbody tr:contains('Postgraduates') td:nth-child(2)")
    if (length(postgrad_nodes) > 0) {
      postgrad[[i]] <- postgrad_nodes %>% html_text()
    } else {
      postgrad[[i]] <- "NULL"
    }
    clean_postgrad[i] <- as.numeric(gsub("\\[\\d+\\]", "", gsub(",", "", postgrad[[i]])))
    
    total_students[i] <- sum(clean_undergrad[i], clean_postgrad[i])
  }
  
  # Add the extracted data to the university dataframe
  university_data$Coordinates <- coordinates
  university_data$Endowment <- clean_endowment
  university_data$Total_Students <- total_students
  
  return(university_data)
}
final_data <- scrapeAdditionalData(extracted_table[[1]])



# Close the browser. 
driver$close()

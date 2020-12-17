# Kevin Craig
# Scrape NC scratch off tickets

# Packages
library(rvest)
library(dplyr)
library(stringr)
library(pdftools)
library(ggplot2)
library(htmltools)
library(htmlwidgets)

rm(list = ls())
options(scipen=999)

# Scrape Odds Data --------------------------------------------------------

# Declare main links for loop
Ticket_Url   <- 'https://nclottery.com/ScratchOff?gt=0'
Ticket_Links <- Ticket_Url  %>% read_html() %>% html_nodes('a') %>% html_attr('href') %>%  str_subset(pattern = "ScratchOffDetail") 

# Lists to populate in loop
Links             <- list()
Data_Title        <- list()
Data_GN           <- list()
Data_Price        <- list()
Data_OverallOds   <- list()
Data_Start        <- list()
Data_End          <- list()
Data_Status       <- list()
Data_Count        <- list()
Data_Odds         <- list()
Data_TotalTickets <- list()
Remove_Elements   <- c('The BIG $PINÂ®')

# Scraping loop
for(i in 1:length(Ticket_Links)){
  # Main Link Scraping
  print(paste0('Reading Link ', Ticket_Links[i], '     Ticket:', i, ' of ', length(Ticket_Links)))
  Link        <- paste0('https://nclottery.com/', Ticket_Links[i]) # Build main link
  Links[[i]]  <- Link 
  Tables_Data <- Link %>% read_html() %>% html_nodes('.TicketData') %>% html_text() # Get data from tables
  Title_Data  <- Link %>% read_html() %>% html_nodes('h1') %>% html_text() # Get header
  
  # Main Link clean info and count tables
  Info        <- str_split(gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", Tables_Data[1], perl=TRUE), '\r') # Clean info table data
  Counts      <- str_split(str_replace_all(Tables_Data[2], "([\t\r])", ""), '\n') # Clean counts table data
  Counts      <- Counts[[1]][Counts[[1]] != ''] # Clean counts table data
  
  # Main link extract info
  Title  <- str_trim(Title_Data[2], 'both') # Keep important info
  GN     <- substr(Ticket_Links[i],nchar(Ticket_Links[i]) - 2, nchar(Ticket_Links[i])) # Game number
  Price  <- as.numeric(gsub('[$,]', '', Info[[1]][4])) # Ticket Price
  Odds   <- substr(Info[[1]][8], 6, nchar(Info[[1]][8])) # Overall Odds
  Start  <- as.Date(Info[[1]][10], format = '%b %d, %Y') # Start Date
  End    <- as.Date(Info[[1]][16], format = '%b %d, %Y') # End Date
  Status <- Info[[1]][14] # Game Status
  
  ### Metadata lists
  Data_Title[[i]]        <- Title
  Data_GN[[i]]           <- GN
  Data_Price[[i]]        <- Price
  Data_OverallOds[[i]]   <- Odds
  Data_Start[[i]]        <- Start
  Data_End[[i]]          <- End
  Data_Status[[i]]       <- Status
  
  # Main link prep counts table - establish temp lists
  Prize_Value <- c() # Prize value temp list
  OG_Count    <- c() # Original count temp list
  New_Count   <- c() # New count temp list
  
  # Main link establish starting points for loop 
  a <- 5 # start loop at element 5 because of headers in table
  b <- 0
  c <- 5 # implications to loop - start loop at element 5 because of headers in table
  
  # Loop through count table entries
  for (x in 1:((length(Counts) - 5) / 3)){ 
    a <- + a + (c-a+1)
    b <- a + 1
    c <- b + 1
    Prize_Value <- c(Prize_Value, Counts[a])
    OG_Count    <- c(OG_Count,    Counts[b])
    New_Count   <- c(New_Count,   Counts[c])
    
  }
  
  ### Main link count info 
  Data_Count[[i]] <- data.frame('GN' = as.numeric(GN),
                                'Prize_Value' = as.numeric(gsub('[$,]', '', Prize_Value)), 
                                'OG_Count'    = as.numeric(gsub('[,]',  '', OG_Count)), 
                                'New_Count'   = as.numeric(gsub('[,]',  '', New_Count)),  
                                 stringsAsFactors = FALSE)
  
  # Sub link scraping
  PDF_Stem <- Link %>% read_html() %>% html_nodes('a') %>% html_attr('href') %>% str_subset(pattern = "Content/Docs/htp")
  PDF_Link <- paste0('https://nclottery.com/', PDF_Stem)
  
  # Sub link clean odds table
  pdf_table <- as.data.frame(pdf_data(PDF_Link)[1])
  if(pdf_table %>% filter(height == 10) %>% nrow() > 0){
    pdf_table <- pdf_table %>% filter(height == 10)
  }else{
    pdf_table <- pdf_table %>% filter(height == 7)
  }
  pdf_table <- pdf_table[6] 
  
  # Sublink segment data vector based on variable
  pdf_table$ID <- seq.int(nrow(pdf_table)) # Add index - divide by three to find splits
  split <- nrow(pdf_table) / 3 # Three columns to divide by
  
  # Sublink map columns to correct variables
  T_Prize <- as.numeric(gsub('[$,]', '', pdf_table[1:split,1]))
  T_Odds  <- as.numeric(gsub('[,]', '', pdf_table[(1+split):(split*2),1]))
  T_Count <- as.numeric(gsub('[,]', '', pdf_table[(1+split*2):(split*3),1]))
  Odds_Table <- data.frame('Prize' = T_Prize,
                           'Odds'  = T_Odds,
                           'Count' = T_Count,
                           stringsAsFactors = FALSE)
  
  ### Sublink odds info
  Data_Odds[[i]] <- Odds_Table
  
  ### Sublink total tickets info
  Data_TotalTickets[i] <- T_Odds[1] * T_Count[1] # Odds of winning top prize times count of top prizes gives total number of tickets
  close(Link)
  close(PDF_Link)
  
}


# Cleaning ----------------------------------------------------------------
# Remove problematic tickets and expired games
Remove_Index <- list()
for(i in 1:length(Data_Count)){
  if (Data_Title[[i]] %in% Remove_Elements | Data_Status[[i]] == 'Ended, Claims Only') # if game is tagged to be removed or game is expired
  { 
    Remove_Index[[i]] <- i # Add element to remove to the tracker
  }
}

# Remove indexes from lists
Remove_Index      <- unlist(Remove_Index[lengths(Remove_Index) > 0L]) # consolidate list of indexs to remove
Links             <- Links[-Remove_Index]
Data_Title        <- Data_Title[-Remove_Index]
Data_GN           <- Data_GN[-Remove_Index]
Data_Price        <- Data_Price[-Remove_Index]
Data_OverallOds   <- Data_OverallOds[-Remove_Index]
Data_Start        <- Data_Start[-Remove_Index]
Data_End          <- Data_End[-Remove_Index]
Data_Status       <- Data_Status[-Remove_Index]
Data_Count        <- Data_Count[-Remove_Index]
Data_Odds         <- Data_Odds[-Remove_Index]
Data_TotalTickets <- Data_TotalTickets[-Remove_Index]


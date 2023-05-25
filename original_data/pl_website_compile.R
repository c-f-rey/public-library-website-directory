# install.packages("rvest")
# install.packages("polite")
# install.packages("pander")

library(tidyverse)
library(rvest)
library(pander)
library(stringr)

#Create character vector for end url of each state directory

state_url_end <- gsub(" ","%20",state.name) #replace spaces between two word states with %20

# scrape library names

lib_name <- lapply(paste0("https://librarytechnology.org/libraries/public.pl?State=", state_url_end), 
                   function (url){
                     url %>% read_html() %>% 
                       html_elements("div") %>% 
                       html_elements("p") %>% 
                       html_element("a:nth-child(1)") %>% 
                       html_text2()
                   })

lib_name <- unlist(lib_name) #unlist into character vector

# scrape https://librarytechnology.org/ webpage id, this will allow us to pull information from each public library page

libtech_id <- lapply(paste0("https://librarytechnology.org/libraries/public.pl?State=", state_url_end), 
                   function (url){
                     url %>% read_html() %>% 
                       html_elements("div") %>% 
                       html_elements("p") %>% 
                       html_element("a:nth-child(1)") %>% 
                       html_attr("href")
                   })

libtech_id <- unlist(libtech_id) #unlist into character vector

libtech_id <- gsub("/library/","",libtech_id) #remove characters so just number ID

#scrape library webiste


lib_website <- lapply(paste0("https://librarytechnology.org/libraries/public.pl?State=", state_url_end), 
                      function (url){
                        url %>% read_html() %>% 
                          html_elements("div") %>% 
                          html_elements("p") %>% 
                          html_element("a:nth-child(2)") %>%
                          html_attr("href")
                      })

lib_website <- unlist(lib_website) #unlist into character vector
  
# scrape library catalog websites

lib_catalog <- lapply(paste0("https://librarytechnology.org/libraries/public.pl?State=", state_url_end), 
                      function (url){
                        url %>% read_html() %>% 
                          html_elements("div") %>% 
                          html_elements("p") %>% 
                          html_element("a:nth-child(3)") %>%
                          html_attr("href")
                      })

lib_catalog <- unlist(lib_catalog) #unlist into character vector

## merge vectors into dataframe

pub_lib_dir <- as.data.frame(cbind(libtech_id , lib_name , lib_website , lib_catalog))
pub_lib_dir<- na.omit(pub_lib_dir) #remove rows with any NA value

#scrape library state name

#update libtech_id from pub_lib_dir

clean_libtech_id <- pull(pub_lib_dir, libtech_id) #pull column into list
clean_libtech_id <- unlist(clean_libtech_id) #unlist into character vector

#ok now scrape library state and city/county

state_id <- lapply(paste0("https://librarytechnology.org/library/", clean_libtech_id), 
                function (url){
                  url %>% read_html() %>% 
                    html_element("h3") %>% 
                    html_text2()
                })

county_state <- unlist(state_id)

#compile items into a dataframe

legend <- data.frame(clean_libtech_id,county_state) # join state/ county info to libtech id
legend <- legend[c("city_county","state")] <- str_split_fixed(legend$county_state,",",2) #separate state and county info into separate columns
legend <- data.frame(clean_libtech_id, legend) #readd libtech id bc i did this weird

pub_lib_dir <- left_join(pub_lib_dir,legend, by = c("libtech_id" = "clean_libtech_id")) #join together

#now we will add PLS data to provide some contextual information
pls_data <- read.csv("https://raw.githubusercontent.com/c-f-rey/pl_web_tracking/main/original_data/PLS_FY19_AE_pud19i.csv", stringsAsFactors = FALSE)

#extract relevant fields from PLS dataset
pls_data_weeded <- pls_data %>% 
  select(STABR , LIBNAME , POPU_LSA , WEBVISIT, LOCALE_MOD, CENTRACT) #Add or remove columns you would like here

#merge datasets
install.packages("fuzzyjoin")
library(fuzzyjoin)
require(fuzzyjoin)
pub_lib_dir_final <- regex_inner_join(pub_lib_dir , pls_data_weeded , by = c("lib_name" = "LIBNAME" , "X2" = "STABR"), ignore_case=TRUE)

#remove redundant columns

pub_lib_dir_final = subset(pub_lib_dir_final, select = -c(X2 , LIBNAME))

#rename columns

pub_lib_dir_final <- pub_lib_dir_final %>% 
  rename(
    city_county = X1 ,
    state = STABR ,
    popu_lsa_2019 = POPU_LSA ,
    web_visits_2019 = WEBVISIT ,
    locale_mod = LOCALE_MOD , 
    cen_tract = CENTRACT
  )
help("write.csv")
write.csv(pub_lib_dir_final , file = "pl_website_directory.csv" , row.names = FALSE)
getwd()

#below is my attempt to scrape vendor information from librarytechnologies.org (Current Automation System, Discovery Interface)

#test_libtech_id <- c("14364" , "12987" , "9061" ,  "14438" , "13009") #creating test character vector, first five libtech ids, so I can run through 5 webpages instead of 7k

###### tried a different method below but it doesn't work!!!
#vendor <- lapply(paste0("https://librarytechnology.org/library/",test_libtech_id), 
#                 function (url){
                   url %>% read_html() %>% 
                     html_elements("div[class='librarydetails']") %>% 
                     html_element("div[class='LTGtable']") %>% 
                     html_element("table:nth-child(2)") %>% 
                     html_element("tbody") %>% 
                     html_element("tr:nth-child(3)") %>% 
                     html_element("td:nth-child(2)") %>% 
                     html_element("a") %>% 
                     html_text()
                 })
######

#pull table by table position. not working because there are a different amoount of tables on different pages

#tech_profiles <- lapply(paste0("https://librarytechnology.org/library/", test_libtech_id),##this pulls the table as a lists, unclear how to convert to dataframe and extract needed values
#                   function(url){
#                   url %>% read_html() %>% 
  #                     html_elements("table") %>%
   #                    .[2] %>% 
    #                    html_table()
     #              })

#convert to data frame to extract values
#tech_profiles_df <- as.data.frame(do.call(cbind,tech_profiles))


#testing for weird outlier
#test <- read_html("https://librarytechnology.org/library/14364") %>% 
 # html_elements("table") %>%
  #.[4] %>% 
  #html_table()



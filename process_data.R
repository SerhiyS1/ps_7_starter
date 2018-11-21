library(tidyverse)
library(fs)
library(stringr)

#Loading in Mr. Schroeder's Data

raw_js <- read_csv("PS7Data.csv")

tidydata <- raw_js %>%
  filter(!(district %in% c("sen", "gov"))) %>%
  mutate(race = paste(state, district, sep = ""),
        dem_adv_actual = (100*(dem_votes - rep_votes)/ (dem_votes + rep_votes + other_votes)))

#Since the readme tells us we want to focus on House races, not Senate or Governor races, I filtered out the 
#Senate and Governor races.  I then used mutate to create a Race variable, one that has the state abbreviation
#and district number.  I then created an Actual Democrat Advantage variable, one which shows the percentage the
#Democrats actually won by.

#Loading in UpShot Data
download.file(url = "https://goo.gl/ZRCBda",
              destfile = "master.zip",
              quiet = TRUE,
              mode = "wb")

unzip("master.zip")

my_list <- dir_ls("2018-live-poll-results-master/data/")

x <- map_dfr(my_list, read_csv, .id = "name") 

x_with_variables <- x %>%
  mutate(state = str_sub(name, 51, 52),
         district = str_sub(name, 53, 54),
         wave = str_sub(name, 56, 56))

#After loading in my data, I wanted to create a state, district, and wave variable.  Using str_sub just as in
#the midterm, I was able to create state, district, and wave variables.  I had to count how many characters
#there were starting with 2018-live-poll-results-master/data/ until the state abbreviation, district #,
#and wave number.

upshot <- x_with_variables %>%
  filter(!(district %in% c("se", "go"))) %>%
  mutate(race = paste(state, district, sep = "")) %>%
  mutate(race = str_to_upper(race)) 
  
#Filtered out Senate and Gov races, and used the same methodology as I did with Mr.Schroeder's data to
#created a Race variable with state abbreviation and district number.

cleanedup <- upshot %>% 
  group_by(name, response, race, wave) %>% 
  summarize(weight = sum(final_weight)) %>% 
  spread(response, weight)

cleanedup[is.na(cleanedup)] <- 0

poll_data <- cleanedup %>%
  mutate(dem_adv_polls = (100*(Dem - Rep) / (Dem + Rep + Und + `3` + `4` + `5`)))

#Now that I had my Race variable made in both my actual election data and polling data, I now had to create my 
#Democratic advantage variable for the polls.  Before I could do that, though, I had to take my data and 
#make it look similar to what we did in Midterm 2.  I grouped by the file name, response, race, and wave, 
#and now my Race variable is associated with the other variables. I then weighted the responses by summing the 
#final weights, giving me the total weight for the Dem, Rep, and Und responses for each race.  Then, I used
#spread, which created columns with the total weights for each response, which I just created. Then, in line
# 54, I used code from my second midterm to turn all NAs into 0s, because I was going to need to add when 
#creating my Democratic advantage in the polls variable.  I then used a simple mutate function to create
#my variable, one that's similar to what we did on the second midterm.  I then checked the data to see if the 
#numbers in my real Dem advantage and polling Dem advantage were similar, and they were.

#Now that I have my data from both the polls and the results, and the variable Race in both, I can start to look
#for patterns or something that I want to investigate.  Since I am a team of one, I don't need any advanced 
#statistics in my report.  Thus, I want to explore how error between the polls and actual results is related 
#to the variables presented in the Upshot data.  I've worked with the educ4 variable in the Upshot data
#previously in class and in problem sets, so I figured I'd work with it again.  During every election, most
#pundits and those interested in politics discuss the education of voters.  There are many trends/patterns that
#political scientists have identified.  For example, those who are highly educated are more likely to vote and 
#more likely to vote Democratic.  Consequently, I wanted to investigate if the error between the results and 
#polls can be attributed to the educational makeup of the surveys.  I need to create variables to find the
#proportion of those interviewed for each level of education.



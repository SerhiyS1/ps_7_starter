library(tidyverse)
library(fs)
library(stringr)
library(readr)

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
  mutate(race = str_to_upper(race)) %>%
  group_by(race) %>%
  mutate(interviews = n()) %>%
  ungroup()
  
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
# 56, I used code from my second midterm to turn all NAs into 0s, because I was going to need to add when 
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
#proportion of those interviewed for each level of education.  First, I went back to the code chunk at line 41,
#and I added two lines, group_by(race) and a mutate to determine my total number of interviews.

somecollege <- upshot %>%
  group_by(race, interviews) %>%
  filter(educ4 == "Some College Educ.") %>%
  mutate(somecollege_per = (100*(n()/interviews))) %>%
  ungroup() %>%
  select(race, somecollege_per) %>%
  filter(!(duplicated(race, fromLast = TRUE)))

fouryear <- upshot %>%
  group_by(race, interviews) %>%
  filter(educ4 == "4-year College Grad.") %>%
  mutate(fouryear_per = (100*(n()/interviews))) %>%
  ungroup() %>%
  select(race, fouryear_per) %>%
  filter(!(duplicated(race, fromLast = TRUE)))

postgrad <- upshot %>%
  group_by(race, interviews) %>%
  filter(educ4 == "Postgraduate Degree") %>%
  mutate(postgrad_per = (100*(n()/interviews))) %>%
  ungroup() %>%
  select(race, postgrad_per) %>%
  filter(!(duplicated(race, fromLast = TRUE)))

highschool <- upshot %>%
  group_by(race, interviews) %>%
  filter(educ4 == "High School Grad. or Less") %>%
  mutate(highschool_per = (100*(n()/interviews))) %>%
  ungroup() %>%
  select(race, highschool_per) %>%
  filter(!(duplicated(race, fromLast = TRUE)))

refused <- upshot %>%
  group_by(race, interviews) %>%
  filter(educ4 == "[DO NOT READ] Don't know/Refused") %>%
  mutate(refused_per = (100*(n()/interviews))) %>%
  ungroup() %>%
  select(race, refused_per) %>%
  filter(!(duplicated(race, fromLast = TRUE)))

#Above, I created variables for each of the responses to educ4, and was able to check the proportion each is
#of the total interviews.  I checked my work by addding up the percenages for a race, and it added up to 100%. 
#Edit: I explain further down below why I had to filter out the duplicates.

joindata <- tidydata %>%
  inner_join(poll_data, by = "race") %>%
  filter(win_party != "UNDECIDED")

#Above, I used an inner join between the results and the polling data.  I also filtered out undecided races
#because I want to have winning party by color in my graphic later.

unique <- joindata %>%
  filter(!(duplicated(race, fromLast = TRUE)))

#I looked at joindata, and there were multiple duplicate rows who differed only by wave.  After some googling,
#I found a very useful link: https://stat.ethz.ch/R-manual/R-devel/library/base/html/duplicated.html 
#Thus, I was able to now have joined data of my results and polls, and have gotten rid of duplicates.  The 
#fromLast part allows me to keep the last wave.  I also ended up using the filtering out of duplicates in
#lines 86 through 124, because otherwise, I would get the percentages for each interview, not for each
#race.

fulldata <- unique %>%
  mutate(error= dem_adv_actual - dem_adv_polls) %>%
  select(race, win_party, error) %>%
  left_join(somecollege, by = "race") %>%
  left_join(fouryear, by = "race") %>%
  left_join(postgrad, by = "race") %>%
  left_join(highschool, by = "race") %>%
  left_join(refused, by = "race") 

#I created my error variable, which is the main investigative point in the problem set.  I then used a series
#of left joins, which added the columns with the percentages of the polled people for each level of 
#education.  I ran into trouble here - the left joins would create literally millions of observations and my 
#RStudio would crash.  My issue was that I didn't ungroup or count(educ4) when making my variables for each 
#response.  The n() in mutate still allowed to me get the proporitons, but ungroup and count made it go back to 
#54 observations.  In addition, a weird issue I have is that to multiply by 10,000 instead of 100 to get a 
#percentage.  

write_rds(fulldata, path = "/Users/serhiysokhan/Desktop/Gov1005/ps_7_starter/fulldata")




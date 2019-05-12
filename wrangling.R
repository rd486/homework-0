library(dslabs)
library(tidyverse)
library(ggplot2)
library(ggrepel)

data("gapminder")
tidy_data <- gapminder %>%
  filter(country %in% c("South Korea","Germany")) %>%
  select(country, year, fertility)
head(tidy_data)
tidy_data %>%
  ggplot(aes(year, fertility, colour = country)) +
  geom_point()

#wide data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <-read_csv(filename)
head(wide_data)

#gather() gather wide data into tidy data
#NOTE it assumes that column names are characters, which won't work for "year"
new_tidy_data <- wide_data %>%
  gather(year, fertility, `1960`:`2015`) 
#1st and 2nd arguments set the column names (alternatively, key = year, value = fertility)...
#...3rd argument specifies the columns to be gathered 
head(new_tidy_data)

#alternatively, specify the column which will NOT be gathered
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE) #convert character to integer
head(new_tidy_data)

#same plot as tidy_data
new_tidy_data %>%
  ggplot(aes(year, fertility, colour = country)) +
  geom_point()

#spread() converts tidy data to wide data
new_wide_data <- new_tidy_data %>%
  spread(year, fertility) 
#1st argument (also: key = year) specifies which variable is used as column names...
#...2nd argument (also : value = fertility) specifies which variable is used to fill out the cells
head(new_wide_data)

#a more complicated example, where multiple variales are encoded in the same column names
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <-read_csv(filename)
select(raw_dat, 1:5)

dat <- raw_dat %>% 
  gather(key, value, -country)
head(dat) #entries in "key" are two values, separated by "_"
#NOTE: "_" is also used in the name "life_expectancy"

#separate() uses "_" as default separator
dat %>%
  separate(key, c("year", "first_variable", "second_variable"), fill ="right") #fill = "right" fills right column with missing values
#1st argument (also: col = key) specifies the column to be separated...
#...2nd argument (also: into = c("","")) specifies new column names

#a better approach would be to merge the extra column
dat %>%
  separate(key, c("year", "variable"), sep = "_", extra ="merge") %>% #extra ="merge" merges the last two variables when there's extra
  spread(variable, value) #we can also use spread to tidy data!
#P.S. unite() is the inverse of separate() and can unite two columns into one

data(murders)
head(murders)
data(polls_us_election_2016)
head(results_us_election_2016)

#left_join()
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + #text repel away from each other & data points & plot edge
  scale_x_continuous(trans = "log2") + 
  scale_y_continuous(trans = "log2") + 
  geom_smooth(method= "lm", se = FALSE) #if se = TRUE, shows confidence interval

#slice()
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 14, 22:23)) %>% 
  select(state, electoral_votes)
tab2

left_join(tab1, tab2)
#alternatively:
#tab1 %>% left_join(tab2)
right_join(tab1, tab2)
#alternatively:
#tab1 %>% right_join(tab2)
inner_join(tab1, tab2) #intersection (of columns)
#alternatively:
#tab1 %>% inner_join(tab2)
full_join(tab1, tab2) #union (of columns)
#alternatively:
#tab1 %>% full_join(tab2)

#semi_join() and anti_join() do not add column from the 2nd table
#semi_join() keeps info available in the 2nd table 
semi_join(tab1, tab2) 
#alternatively:
#tab1 %>% semi_join(tab2)
#anti_join() keeps info which is NOT available in the 2nd table
anti_join(tab1, tab2) 
#alternatively:
#tab1 %>% anti_join(tab2)

tab1 <- tab[1:5,]
tab1
tab2 <- tab[3:7,]
tab2
#dplyr allows intersect() and union() and setequal() to work with data frames
intersect(tab1, tab2) #intersection (of rows)
union(tab1, tab2) #union (of rows)
setequal(tab1, tab2) #tells if two tables have same elements, regardless of orders
#P.S. setdiff() works as well, which only shows rows in 1st table which is not in 2nd table

library(rvest) #for web scraping
url <- "https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state"
h <- read_html(url)
class(h)

#html_nodes() extract all nodes of that type
#html_node() extract the first node of that type
tab <- h %>% html_nodes("table")
tab <- tab[[2]] #selects the second table
tab <- tab %>% html_table #html_table() converts html tables into data frames
class(tab)
head(tab)
#setNames() changes column names
tab <- tab %>% setNames(c("state","population","total","murders","gun_murders","gun_ownership","total_rate","murder_rate","gun_murder_rate"))
head(tab)

#extract other info than table
#(doesn't work, needs to be scrutinised)
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".o-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-ListItemText") %>% html_text()
guacamole <- list(recipe, prep_time, ingredients)
guacamole
#write a function to extract more recipes
get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".o-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-ListItemText") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
}
get_recipe("http://www.foodnetwork.co.uk/article/chocolate-cakes-went-above-and-beyond-all-expectations/beattys-chocolate-cake/2.html")

#String processing
#use double/single quote to define a string
s <- '10"' #single quote is used here because the string already contains "
cat(s) #show the string
#similarly, double quote is used here because the string already contains '
s <- "it's"
cat(s)
#what if the string contains both " and '?
#use \ to escape the quote which is used to define the string
s <- "it's 10\"" #or: s <- 'it\'s 10"'
cat(s)

#stringr package
library(stringr)
#str_detect() 
commas <- function(x) any(str_detect(x, ",")) #define a function to check if a string contains comma(s)
commas(s)
#str_replace_all()
t <- "hello, world"
test_1 <- str_replace_all(t, ",", "")
test_1

#parse_number() in readr package removes non-numeric characters
library(readr)
r <- "123,456"
parse_number(r)
class(r)
r <- as.numeric(r)
class(r)

library(tidyverse)
library(dslabs)
data(reported_heights)
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>%
  head(n=10) #show the first 10 entries

#regular expressions (i.e. regex)
str_detect(reported_heights$height, "cm|inches")
#find strings with digit(s)
pattern1 <- "\\d" #\d means digit, so the first backslash escapes the second backslash
str_detect(reported_heights$height, pattern1)
str_detect(reported_heights$height, "[4-7]") 
#note everything is a character in regex...
#...so [1-30] means 1 through 3 and 0, rather than 1 through 30
#therefore, ...[a-z]: contains any lower case, [A-Z]: contains any upper case...
#...[a-zA-Z] means all letters 

library(htmlwidgets)
#str_view() shows the first match for each string
#str_view_all() shows all the mataches
str_view(reported_heights$height, pattern1)
#^ is the start of a string, $ is the end of a string
#e.g. ^[a-z]: any lower case as the first character
#e.g. [A-Z]$: any upper case as the last character
pattern2 <- "^\\d$" #this refers to strings with exactly one digit
str_view_all(reported_heights$height, pattern2)
#{} means the possible number of times the previous entry repeats
#e.g. [a-z]{3,4}: with exactly 3 or 4 letters
pattern3 <- "\\d{1,2}" #this refers to strings with one or two digits
str_view_all(reported_heights$height, pattern3)
pattern4 <- "^[4-7]'\\d{1,2}\"$"
str_detect(reported_heights$height, pattern4)

#write a function to find entries which didn't report inches, which are...
#...NAs when applying numeric or out of range  
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x)) #avoid warning messages
  ind <- is.na(inches)|inches < smallest|inches > tallest
  ind
}
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
length(problems)

#replace the different ways of representig feet and inches with '...
#...also replace the different ways of representig inches with nothing
pattern5 <- "^[4-7]'\\d{1,2}$" #in comparison with pattern 4, the " at the end is gone
problems %>%
  str_replace("feet|ft|foot","'") %>%
  str_replace("inches|in|''|\"","") %>% #replace inches, in, two single quotes and one double quote with nothing
  str_detect(pattern5) %>%
  sum

#*: zero or more instances
#?: zero or one instance
#+: one or more instance
yes <- c("AB", "A1B", "A11B", "A111B")
data.frame(string = c("AB", "A1B", "A11B", "A111B"),
           none_or_more = str_detect(yes, "A1*B"),
           none_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))

#\s: space
pattern6 <- "^[4-7]\\s*'\\s*\\d{1,2}$" #\\s* permits zero or more space between ' and the numbers
problems %>%
  str_replace("feet|ft|foot","'") %>%
  str_replace("inches|in|''|\"","") %>% #replace inches, in, two single quotes and one double quote with nothing
  str_detect(pattern6) %>%
  sum

#other problematic entries include: x,y; x.y; x y
pattern7 <- "^[4-7],\\d*$" #make sure there is only one digit between 4 and 7 before comma
pattern8 <- "^([4-7]),(\\d*)$" #define groups with ()
#str_extract() only extracts strings matching a pattern, not defined by groups
str_extract(problems, pattern8)
#str_match() extracts the values defined by groups
str_match(problems, pattern8)

pattern9 <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"
#\\1: the 1st group; \\2: the 2nd group; \\3: the 3rd group; ......
str_subset(problems, pattern9) %>%
  str_replace(pattern9, "\\1'\\2") #the implict first argument in str_replace() is problems

#deal with cm
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x)) #avoid warning messages
  ind <- !is.na(inches) &
    ((inches >= smallest&inches <= tallest)|
    (inches/2.54 >= smallest&inches/2.54 <= tallest))
  !ind
}
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

converted <- problems %>%
  str_replace("feet|ft|foot","'") %>%
  str_replace("inches|in|''|\"","") %>% #get ride of inches symbol
  str_replace(pattern9, "\\1'\\2")
index <- str_detect(converted, pattern6) #check how many entries comply with the format x'y
mean(index) #percentage of correct format
converted[!index] #check those entries which are still problematic

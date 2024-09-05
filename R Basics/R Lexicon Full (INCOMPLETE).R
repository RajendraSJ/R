Dear reader use getAnywhere('function')
#To get source code of any function

library(base)
library(beepr)
library(caret)
library(checkmate)
library(chron)
library(cowsay)
library(class)
library(crayon)
library(data.table)
library(datasets)
library(dplyr)
library(DT)
library(fastmatch)
library(fortunes)
library(fun)
library(geosphere)
library(ggplot2)
library(gmp)
library(graphics)
library(grDevices)
library(htmltools)
library(htmlwidgets)
library(janitor)
library(jsonlite)
library(knitr)
library(leaflet)
library(lobstr)
library(lubridate)
library(MASS)
library(methods)
library(microbenchmark)
library(mlr)
library(multicolor)
library(nlme)
library(openxlsx)
library(phonics)
library(praise)
library(progress)
library(progressr)
library(purrr)
library(Rcrawler)
library(rmarkdown)
library(readr)
library(readxl)
library(RODBC)
library(shiny)
library(stats)
library(stringr)
library(stringi)
library(utils)
library(wikifacts)


table(live$State.x )

setnames(live, 'State.x', "State")
setnames(live, c("State_Name.x", "Region.y","State.y"), c('State_Name', "Region", "State2"))
names(live)

live$state <- NULL
live[,c('State', "Region")] <- NULL
live[, State:=NULL]
live[, `:=`(State = NULL,
            Region = NULL)]
abc <- readRDS("F:/Rajendra J/DemogData/Input/")

sum(final$Booksize)/1e7
table(is.na(final$State_Name.x))
colSums(is.na(final))

#gmp library for big calcultations
#Learn Significand, mantissa, ulp, eps, int, float, double

# 1.Introduction to R Programming

Originated in 1993
Created by Robert Gentleman and Ross Ihaka
Latest version 4.2.1
R language comes from language S+ and Scheme which comes from lisp ( List processing)
some more languages like racket and clojure
Hadley Wickham 
R has currently 15000+ package
How to download R?
go to browser and type "CRAN R"
R Studio is a very interactive software for R programming
Also remember to install R tools
#What are packages?
R packages are the functions Written by the R community to be used for everyone
For using a R packages simply install it using 'install.packages()' load it in working session using 'library(package_name'
#You Install once and load whenever you want.

# 2.Some more settings in R options
how to create .RProfile?
file.edit("~/.Rprofile")
How to Restart R session?
options(scipen = 999,stringsAsFactors = FALSE,prompt="RJ>")  #Scientific penalty is added
Learn all the windows and R console Shortcuts
Important Shortcuts
check multiple keybindings

# 3.Numbers part 1

In maths we have in linear and Abstract Algebra
#Abstract Algebra is very important and useful
Complex numbers (a+bi) # Whatever exists in Real field will also exist in Complex field
Wilbert Strong -> book for understanding Algebra
Sheldon Axler -> Linear Algrebra done right
Complex (Real and Imaginary numbers) -> Real (Rational and Irrational numbers) -> Rational Numbers(Integers and fractions) -> Integers(Positive, Negative and Zero) -> Whole numbers -> 
#In R we have Integers and Double Note :->Integers are subset of Doubles
Floats and Double are same thing
You cannot right very big Integer values
When you type a digit like "2" it is saved as double
to save the number as integer you have to write it as "2L"
You can add two numbers using "+"
You can substract two numbers using "-"
You can multiply two numbers using "*"
You can divide two numbers using "/"
You can modulo operator two numbers using "%%"

# 4.Numbers part 2

Assignment operator "<-" can be used using shortcut alt + "-" OR "="
By default R saves as double
# Why R saves as double?
Hint use RUN ?.Machine (Understand it later)
.Machine$integer.max  # To get the Maximum integer that can be saved, it is 2147483647
.Machine$double.xmax  # To get the Maximum double that can be saved, it is  179769313486231570838400602864442228000008602082842266064064680402680408280648240046204888888288080622822420842246006644866884860462806420066668022046626024066662068886808602862886866800048228686262462640668044406484606206082824406288200264266406808068464046840608044222802268424008466606886862062820068082688
R acts weird when operating on very big numbers
So we should be using gmp library
"Everything is a function call in R"
2 == 2 # "==" is used for exact comparison
0.1 == .3/3 gives false which is not correct
sprintf("%1.20f",.3/3)  # Output is 0.09999999999999999167 hence for the incorrect output
.1 == round(.3/3,2)     #rounding can help correct this problem
# Moral of the story Never compare float points using abstraction
Never use Numbers as Primary key for variables , datasets or values
34%%5      # implements mod operator
-34%%5     #Negative mod will give unexpected outcomes
-(34%%5)   #Parenthesis can change the outcome

# 5.Strings and logical in R

Characters are noted using single or double quotes
x <- "This is a string"
x <- 'This is also a string'
typeof(x)   # output will be character
print(x,quote = FALSE)
noquote(x)
cat(x)
cat("This","is","a","string")
format("1234",width = 10,justify = 'left')   #Coverts every input into characters
## %d : Integers
## %f : decimals
## %s : strings
sprintf("%010d",189087)
sprintf("%10.4f",890.564294641)
x <- "this is a slash\c but"  #This will give an error
#LOGICAL  IS DENOTED AS TRUE or FALSE
ONLY capital
predicate functions that return true false as output
conflicts # to check if you have overwritten some variables
# Always write true and false as TRUE and FALSE
as.numeric(TRUE)   #1
as.numeric(FALSE)  #0
#NAs
NA             #type logical
NA_character_  #type Character
NA_complex_    #exists but not used
NA_integer_    #type integer
NA_real_       #type double

library(parallel)

# 6.Scalar vectors in R
#Scalars unit atom
x <- 1
Atomic vectors and list
x <- c(1,"a")  # Cohersion converting numeric to character to retain information in data
X <- c(TRUE,20L,49)
typeof(X)   # Double
X <- c(TRUE , 20L,30L) 
typeof(x)   # Integer
#Indexing
apropos('<-$')
is.atomic
is.vector

# 7. List and Datasets
Scalars, Vectors <- atomic vectors and list
make atomic vectors using 'c' and lists using 'list'
vector('list',10)      #for lists
vector('numeric',10)   #for numeric
vector('character',10) #for Character
Atomic <- Homogenuous <- array -> data.frame
Another data stucture is factor for categorical data
#Lists are hetrogenuous
List_properties <- c("Hetrogenuous","diff length of items","Recursive ( list within list)")
Make list as data frame using data.frame given equal length
tibble
access a column from list using L$[['x']]
Rename list latter using names
dataframe is type of list
df[row,column]
put row, column as either names or no of column in dataframe
alternative use df$x for extracting column
use : for creating sequences
drop = false arguement


# 8. Matrix and Arrays
Similar to tensor
arr <- array(runif(50),dim = c(5,2,5))
arr
arr[row,column,matrix]
Very rare in practical problems
help(matrix)
Factors are categorical, either ordered/ordinal or Unordered/Nominal
Factors are Shown as characters but are saved as numbers so character operations will not work
use ordered = TRUE arguement to set ordinal factor
label = 'salary_'
To convert factors into numbers Always first convert into character then into numeric


#9. Conditions and Looping
ifelse() for vectors and if()else() for scalars
function : scalar
Vectorize(function)
run on Vectors
if(condition{
  else if (condition)
} else {
  y <- (condition)
}
x <- c(1,2,4,5)
y <- vector('character',length(x))
#for loop
iterator = 1
for(it in x){
  if(it>2){
    y[iterator] <- ('gt 2')
    } else {
      y[iterator] <- ('le 2')
  }
  iterator = iterator+1
}
specify length before to keep for loop
while loop
while(iterator <= length(x)){
  if(x[iterator] > 2){
    y[iterator] <- ('gt 2')
  } else {
    y[iterator] <- ('le 2')
  }
  iterator = iterator + 1
} 
y
for making while loop foolproof give it a break arguement


#10. Recycling and Vectorization
c(3,4,7,8,6,9) + 1        #normal addition for all elements
c(3,4,7,8,6,9) + c(4,5)   #Recycling 
Note length of added vector must be factor of length of bigger Vectors
# &&: scalar  and &: vector
x1 <- c(T,F,T,F,T)
x2 <- c(T,T,F,F,F)
x1 & x2     Correct
x1 && x2   Incorrect
| Or  & AND  ! NOT
head(mtcars)       #to view first few rows
view(mtcars)       #to view entire data
colSums(mtcars)    #to get sum of all columns
colMeans(mtcars)   #to get mean of all columns
#Now to get column median we dont have a built in function so we build one ourself.
vmedian <- Vectorize(median)
vmedian(mtcars)
Note : You cannot Vectorize Primitive functions like sum
is.primitive()    #to check if a function is primitive or not.
df <- data.frame(x = c('one','two','three'),y=c('1','2','3'),z=c('one 1','two 2','three 3'))
df
gsub('a','b',c('cat','bat')) #replacement is vector but it is not a vector w.r.t. parameters a and b
vgsub <- Vectorize(gsub)
vgsub(df$x,df$y,df$z)


#11 More on some vectorization and paste
x <- 'a'
y <- 'b'
x1 <- c('a','b')
x2 <- c('d','e')
x3 <- c('a','b','c')
y1 <- c('d','e',NA)
paste(x,y)                #Output "a b"       (vector output)
paste(x,y,sep="")         #Output "ab"        (vector output)
paste(x1,x2,sep="")       #Output  "ad" "be"  (vector output)
paste(x1,x2,sep='-',collapse = '+')    #Output "a-d+b-e" (scalar output)
Note there is no arguement for seperator in paste0 so remember it.
#for unequal vectors
paste(x3,x2,sep='')                    #Output "ad" "be" "cd"
paste(x3,x2,sep='-',collapse = '+')    #Output "a-d+b-e+c-d"
stringr :: str_c(x1,y1,sep = '',collapse = '+')
stringr :: str_c(x1,y1,sep = '')
substring( 'abcdef',1,2)           #Output "ab"
substring( 'abcdef',1:2,1)         #Output "a" ""
substring( 'abcdef',1:6,1:6)           #Output "a" "b" "c" "d" "e" "f"
substring(c('abcdef','ghijkl'),1:6,1:6)           #Output "a" "h" "c" "j" "e" "l"
#substring vectorizes all parameter

# 12 More Functions
#apropos
Finds all functions with specified word like mean
apropos('sum')              #gives all functions with 'sum' in them
apropos('sum$')             ##gives all functions with ending with 'sum'
x = c(1,2,3,4,5)
sum(x)                      #gives sum of elements in a vector/column from a data frame
mean(x)                     #gives mean of elements in a vector/column from a data frame
unique(x)                   #gives count of unique elements in a vector
dim(mtcars)                 #gives dimension of a dataframes/matrices
str(mtcars)                 #gives structure of dataframe i.e. all the columns and their information
summary(mtcars)             #gives summary(min,max,First Quartile,Second Quartile,Third Quartile,mean) of all columns in a dataframe
nrow(mtcars)                #gives count of rows in a data structure or count of observations
ncol(mtcars)                #gives count of columns in a data structure or count of total variables
length(mtcars)              #no.of rows
lengths(mtcars)             #no. of columns
head(mtcars,8)              #displays first few observations
tail(mtcars)                #displays last few observations
x <- c('ab','abc','def','abda')
grep('a',x)                 #gives element no. in the vector that contains requested pattern                 
grep('a',x,value = TRUE)    #gives values in the vector that contains requested pattern
grepl('a',x)                #gives logical output T/F whether the requested pattern in present or not
sub('a','x_',x)             #substitutes the pattern in front of element to the replacement             
gsub('a','x_',x)            #substitutes all the patterns in front/back/middle of element to the requested replacemetnt
startsWith(x,'a')           #Does the string starts with pattern or not| logical output ( Also note startsWith and endsWith are faster than substring)
endsWith(x,'a')             #Does the string ends with pattern or not| logical output

#13 . Functions in R
We can define functions in two ways Anonymous and defined functions
we can create function using function() 
Arguements and paraments.
When we define functions we define it arguements and when we call a function it is called as parameters
f <- function(x){
  x**2
}
# A function has three parts formal, body and Environment
formal is basically the arguements/parameters
body is what the function is basically
environment means in which environment the function was created in.
Each function has its own function inside it and it changes everytime we run the function
# you should write return in a function
A function can run without returning anything
only variable written gives you the value.
assigments dont result in returning values So either write that variable or write return to get something
NOTE :- There is difference between identical and same
x = 20
y = 20L
x == y          #Same
identical(x,y)  #Identical
# functions can be passed as parameters
vect <- c(1,2,6,5,0,7,6,9,7,5,2,4)
mu <- mean(vect)
absdev <- abs(vect - mu)

mad_mean <- function(x) {
  vect <- x
  mu <- mean(vect)
  absdev <- abs(vect - mu)
  mean(absdev)    # mean(absdev) = return(mean(absdev))
}

mad_mean <- function(x,fun) {
  vect <- x
  mu <- mean(vect)
  absdev <- abs(vect - mu)
  return(fun(absdev))    # fun(absdev) = return(fun(absdev))
}

DRY Do Not Repeat Yourself

# Closure

fs <- function(){
  return(x+2)
}
R first finds x in local environment then it trys to find x in global Environment
fs() <- function(){
  x <<- 5         # This double arrow overwrites the value of x in global environment
  return(x+2)
}

#14 . Functions Apply family in R
apply family  := apply  row column,
                 sapply simplified apply,
                 lapply list apply,
                 mapply( Map apply) multivariate apply
                 vapply verified apply,
                 tapply tag apply,
                 eapply environment apply,
                 rapply recursive apply
#apply
for (col in colnames(mtcars)){
  print(mean(mtcars[[col]]))
}
apply(mtcars,MARGIN = 2,mean)   # 1 is for row,2 is for column
head(iris)
str(iris)
for (col in colnames(iris)){
  print(is.numeric(iris[[col]]))
}

apply(iris,MARGIN = 2,is.numeric)   # Gives wrong output as apply converts entire data into an array

# lapply  1) It is the most useful apply.2) Always gives output as list. 3)It is predictable.
lapply(mtcars,mean)  # gives output in a list
un;ist(lapply(mtcars,mean))  # gives output as atomic vector

# vapply
f <- function(x,...){
  return(list("mean" = mean(x, ...),"sum" = sum(x,...)))
}
df <- data.frame(x = c(5,4,1,NA,63,4,9),y = c(8,3,6,5,7,1,NA))
sapply(df,f,na.rm = TRUE)
vapply(df,f,na.rm = TRUE, vector('list',2))  #You can choode how you want your output to be

#15 . Operations on DataFrame (Theory) in R
First you must understand these two functions Merge and Concatenate
Types of merge
Inner
Left  It gives values only that are found in both tables
Right
Full_join
merge(x,y,by.x,by.y,all.x,all.y)  
For Left join keep all.x = T all.y = F
For inner join keep all.x = F all.y = F
For Right join keep all.x = F all.y = T
For outer join keep all.x = T all.y = T
# we must define our left table that is the table that we want to retain all values of it.
Your ID is your by parameter
#concatenate
You can concatenate in two ways row wise or column wise using rbind and cbind

#16 . Operations on DataFrame in R
read.csv is a wrapper of read.table  just keep header = T and sep = ","
read.delim is a wrapper of read.table just keep header = T and sep = "/t")
data = read.table('clipboard', header = T, sep = "/t")  For copying a table from clipboard
#One table must always have only unique keys
Always pay attention to the number of rows.


#17 . Group by and miscellaneous operations in R

#18 . Functional Programming in R

#Reduce
Reduce 
'+'(5,2)  # Prefix notation
5+2       # Infix notation
'+'(2,5,6) # The + is a binary operator and needs two values to work
'+'('+'(2,5),6) # It performs Addition sequentially 
"func(func(func(obj))" This above type of functions means you need help of functioanl programming
Reduce('+',c(2,5,6)) #basic use of reduce function
Reduce('+',c(2,5,6),accumulate = TRUE) # Shows how the reduce function has performed operation stepwise
ir <- head(iris[,1:4])
'+'('+'(ir$Sepal.Length,ir$Sepal.Width),ir$Petal.Length)
Reduce('+',ir[,1:3])
## merge(a,b) given that there is a column with common column name
Normal way to write
m1 <- merge(a,b)
m2 <- merge(m1,b)
m3 <- merge(m2,c)
#Alternative way to write
# merge(merge(merge(a,b),c),d)
OR
Reduce(merge,list(a,b,c,d,e,f)) #Note by default merge gives a inner join if you want left join either use left join from dplyr or simple make a left merge function yourself
merge_left <- function(x,y){
  merge(x,y,all.x = TRUE, all.y = FALSE, ... )
}
#plyr :: join_all##
To find minimum or maximum or rows use pmax and pmin
#########map
mapply = map apply
## lapply for single arguement
## mapply for two arguements
library(geosphere) -> disthaversine(p1,p2,r=6378137)
#do.call
#do.call != Reduce (not equal)
rbind(a,b,c,d,e,f,g)
There is a difference between Hacking and a solution
Hack -> if problem is changed even a little bit the Hack will fail
Solution ->  even if problem is change it might still work
df1 <- data.frame(x= 1:5 , y= letters[1:5])
df2 <- data.frame(x= 2:6 , y= letters[2:6])
df3 <- data.frame(x= 3:7 , y= letters[3:7])
rbind(df1,df2,df3)   # solution 1
do.call(rbind,list(df1,df2,df3)) # solution 2
#We can also create this list using a For loop
NEVER Repeat like above its not efficient
for( i in 1:3){
  lyst[[i]] <- data.frame(x = (1+i):(i+5),y = letters[(i+1):(i+5)])
}
lyst
do.call(rbind,lyst)
library(readxl)
df1 <- readxl :: read_xlsx("PATH",sheet = "Sheet_name")  # dont have a practise of writing codes like this
df2 <- readxl :: read_xlsx("PATH",sheet = "Sheet_name")
sheets <- readxl :: excel_sheets("PATH")
names(lyst) 
for( i in sheets){
  lyst[[i]] <- read_excel('PATH',sheet = i)
}
do.call(rbind,lyst)
#Another Approach
do.call('rbind',lapply(sheets, function(x)read_excel('PATH',sheet = x)))
# Good R codes make a small solution and then extrapolate it.!!!
How to concatenate all columns in R?
paste(ir$Sepal.Length,ir$Sepal.Width,ir$Petal.Length,ir$Petal.Width) #Dont do it like this
do.call(paste0,ir) "ir" = data table
do.call('+',ir[,1:4]) #this will not work
#func(func(func(obj))
Additional languages if you want to learn functon programming ;> clojure, racket


#19 . Regular Expressions (Theory) in R part 1
Should you learn regular expression?
#Solid YES
Data arranging = Data processing
"Regular Expressions" comes under the Theory of Automata
Regular Expressions = Domain Specific Language
SQL is also a Domain Specific Language (DSL)
Almost similar for all languages
You want to identify , remove or extract
#IDENTIFY
grep means global regular expression print
It can give output as Value as well as index
grepl means global regular expression print logical
It gives logical output i.e. whether it matches with pattern or not
#REPLACE
sub
gsub
#EXTRACT
regexpr
regmatches
gregexpr
A String can contain Uppercase,Lowercase,Numeric and Special Characters
For lowercsae [a-z] [] Square bracket defines character class
For uppercase [A-Z]
For uppercase + lowercase [a-zA-Z]
For uppercase + lowercase + numbers [a-zA-Z0-9]
For uppercase + lowercase + numbers + underscore [a-zA-Z0-9_] = \\w
For numbers [0-9] = \\d
+,.,?,{,*,\s,\t
\\W = Not a Word
\\D = Not a Digit
\\S = Not a space
^ = start of a Word (find more)
$ = end of a Word
\b = boundary character
. = any character
+ = One or more characters
* = zero or more characters
? = stop greediness (find more)
() = Capturing groups
{} = Matching with limits


#20 . Regular Expressions in R part 2

x1 <- c('abc123','123abc','567klm','lml567','123','abc','ABC123','132ABC')
Q.1 What are strings contain alphabet followed by number
a)Extended regex  b)PCRE c)directional asis match ##fixed = TRUE
grepl('abc123',x1,fixed = TRUE)  # Proper Result
grepl('abc123',x1,fixed = FALSE)  # Proper Result
grepl('[a-z]+\\d+',x1,fixed = FALSE) #Proper Result
grepl('[a-z]+\\d+',x1,fixed = TRUE) #Non sense Result 
#Because Fixed = TRUE means search for exact pattern it is asis match
grepl('[a-z]+\\d+',x1)  Gives TRUE FALSE as output#fixed = FALSE by default 
grep('[a-z]+\\d+',x1)   Gives Index of match as output
grep('[a-z]+\\d+',x1,value = TRUE) Gives the values that are in output
grep('[a-z]\\d',x1)   #+ is removed 
grep('[a-zA-Z]\\d',x1,value = TRUE)
grep('[a-z]\\d',x1,value = TRUE, ignore.case = TRUE) #Works same as above
grep('(?i)[a-z]\\d',x1,value = TRUE) # (?i) means ignore case
grep('[a-zA-Z]+\\d+',x1,value = TRUE)  # Adding a Plus means one or more than one.
Q.2 What string contain atleast one digit may or may not preceeded by alphabet
grep('[a-zA-Z]*\\d+',x1,value = TRUE)  # Adding a star means zero or more than zero alphabets with some digits
grep('\\s+\\d+',x1,value = TRUE)    #To get space followed by digits
grep('\\s*\\d+',x1,value = TRUE)    #To get pattern if zero or more spaces are followed by digits


#21 . Regular Expressions in R part 3

#Replace all alphabets/special characters with nothing
# Replace all non alphabets/special characters with nothing
# Put a seperator between text and digit

x1 <- c("ab123","abc8990","?(1","*2","-2")

# gsub/sub
string <- c("Bhagat Singh")
sub("\\s","  ",string)
sub("a","A",string)  # only first "a" becomes capital
gsub("a","A",string) # replaces all "a" into "A"
gsub('[a-zA-Z?(*]+',"",x1) # Remove all alphabets
gsub('\\D+','',x1)   # Remove all digits  (for getting non alphabet and special characters)
gsub('\\d+','',x1)   # Remove all non digits  (for getting non alphabet and special characters)
gsub('(\\d+)','/\\1',x1)   # When we put parenthesis it saves the pattern as 1
# in above when there are multiple parenthesis we need to mention paranthesis to take pattern from



#22 . Regular Expressions in R part 4
#23 . Regular Expressions in R part 5

# 24. Problem Solving in coding Part 1

Your code should be readible and generic
Your code should not be extremely specific or extremely generic
Give proper functional names to your variables
#100 functions for 1 task is better than 1 function for 100 tasks
Q1. Find out all the even numbers
x <- c(10,2,8,7,20,90)   # Hint:- Write a function
%% operator is modulo operator
x[ x%%2 == 0]
is_even <- function(number){     #solution      
  return(number[ number%%2 == 0])
}
Q2. Find the last 2 characters
x <- c('ab','bcd','cdef','efgh','fghijkl')
gsub('\\w{2}$',"",x)
gsub('(\\w*?)(\\w{2})$','\\2',x) #Hard Solution
g = gregexpr('\\w{2}$',x)        #Solution 2
unlist(regmatches(x,g))
soln
substr(x,nchar(x)-1,nchar(x))    #Solution 3

extract_last_two <- function(x){
  return(substr(x,nchar(x)-1,nchar(x)))
}

#substr(x,1,2)



Q3 Create a sequence of odd numbers from 1 to 100
seq(1,100,by<-2)                 #Solution 1
(1:100)[(1:100)%%2 = 1]          #Solution 2
odd_extract <- function(highest_value){
  seq = 1:highest_value
  return(seq[seq%%2==1])
}
Q4 Find all the multiples of 3 and 7 less than 100
multiple_extract <- function(highest_number){
  seq=1:highest_number
  return(seq[seq%%3 == 0 & seq%%7 == 0])
}
multiple_extract(100)
x=as.numeric( 1:100)
x[x%%21==0]                     #Solution 2  
is_divisible_by_given_number <- function(x,y,highest_number){
  seqs = 1:highest_number
  divisor = x*y
  return(seqs[seqs%%divisor == 0])
}
is_divisible_by_given_number(14,16,1000)
Q5. Replace all the missing instances with most repeated values( This is called Mode). There is no function in R.( The mode function in R is for storage.mode not for statistical mode)
x <- c('abc',NA,"def","ab",'abd',"ab",'ab','abd','def','abd','ab',NA)
#Determine Mode
table(x)
names(which.max(table(x)))
value <- names(which.max(table(x)))
is.na(x)
x[is.na(x)] <- value
x
mode_replacement(x)
x
Q.6 Replace all the missing values with mean and median
x <- c(10,1,7,NA,NA,7,9,10,10)
x[is.na(x)] <- mean(x,na.rm=)
replace_with_statistic(y,median)
y = c(1,4,6,3,2,9,6,NA,4,7,3,NA,NA)


#25 Problem and solutions part 2
x <- c('This is a paragraph','This can be another one','this is a long string of alphabets','this is small')
Q.7 Count all the words(characters stick together), words are seperated here with spaces.
lengths(strsplit(trimws(x),split = ''))
lengths(strsplit(trimws(x),split = ' '))
g <- gregexpr('\\w+',trimws(x))
lengths(regmatches(x,g))
g <- gregexpr('\\s+',x)
lengths(regmatches(x,g)) + 1
word_count <- function(x){
  return(lengths(strsplit(trimws(x),split = ' ')))
}
word_count(x)
x1 <- c('456abc','abd?678','89abef','890','678def')
Q.8  Find all the numbers and get the maximum of x vector by converting everything to numeric.
max(as.numeric(gsub("\\D+",'',x1)))
stat_fetch <- function(x,func){
  return(func(as.numeric(gsub("\\D+",'',x))))
}
stat_fetch(x1,min)
max('a','b')            #maximum between characters is possible
Q.9 Use mtcars data filter all rows and print the carnames, which has mpg less than average mpg
avg_stat <- mean(iris[['sepal.length']])
mtcars[mtcars[['mpg']] < avg_stat,]
mtcars[['mpg']]  <  avg_stat
mtcars$cars <- rownames(mtcars)
rownames(mtcars) <- NULL
#Use iris data sepal.mean length
comparison <- function(dframe,col,stat){
  avg_stat <- stat(dframe[[col]])
  return(rownames(dframe[(dframe[[col]]) < avg_stat]))
}
Iris <- iris
setDT(Iris)
comparison(Iris,'Sepal.Length',mean)
comparison(iris,'Sepal.Length',median)
Q.10 Given a list like below: sort the list suc that maximum sum element would come on top

alyst = list(x1 = c(1,10),x2 = c(19,-8,-12,8,5,1),x3 = c(1,2,10,11,-89,56,1,7),x4 = c(1,2,3))
summary(alyst)

names(alyst) <- paste0("x",order(-unlist(lapply(alyst, FUN = sum ))))
alyst
nlyst <- vector('list',length(alyst))
names(nlyst) <- sort(names(alyst))
for(item in names(alyst)){
  nlyst[[item]] <- alyst[[item]]
}
rm(alyst)
rm(nlyst)
nlyst

#26 . Problems and Solutions part 3
#27 . Problems and Solutions part 4
#28 . Pipes in R
#29 . Dplyr Intution: Select and Filter in R
#30 . Using dplyr to do group_by, summarise and mutate Theory
#31 . Using dplyr to do group_by, summarise and mutate Practical
#32 . ggplot2 in R
#33 . Histograms and other siblings in R
#34 . Customization and other plots in ggplot2 in R
#35 . Tidy data and ggplot2 in R
#36 . Introduction to data.table in R Part 1
by Matt Dowle     Very fast and efficient library
setDT()
R works on the fundamental of copy and modify (When any operation is applied address location is changed) but 
data.table doesnt create copies i.e. address is same for all saved object
How To check if a data frame or not?
class(df)     # Output should print data.table and data.frame together
##read.csv
##readr::read_csv
##data.table::fread
  
#37 . Introduction to data.table in R Part 2
fread directly imports and convert csv to data table
setDT(data)  # 1 Jump
data <- setDT(data)  #same as above
data <- data.table(data) # 3 Jumps 
data[i,j,k]  # Where Select Group by
data[month == 'mar' and day == 'fri'] this is i operation
we dont need to use $ to mention variables
datq[1:3,] To select first 3 rows
data[,c('X','Y','month')]  # Data frame way
data[,.(X,Y,month)]  # Data Table way
data[,1:3]  # worst way because it is unreadable
#booleans
head(iris[,c(T,T,F,F,F)] # by default if takes non mentioned columns as TRUE
data[,c(T T,T,T),with = FALSE]  #Boolean behaviour is different for dataframe and datatable
data[,.('mean_month' = mean(temp)),.(month)] # This is group by using data.table
data[,list('mean_month' = mean(temp)),.(month)] # This is group by using data.table
data[,.('mean_month' = mean(temp)),.(month)][order(month,day),] # This is order using data.table
# The above method is like Chaining
Use internal variables of data.table




#38 . Introduction to data.table in R Part 3

To create counters
setDT()
df$newcol <- 1:nrows(df)   # dont do this if you are using data.table for adding counter
df[,newcol := .I]
df[,newcol := 1:.N]
df[,newcol := 1:nrows(df)]
# .SD
# .I
# join
# inplace
df[] <- lapply(df,as.character)
df[, names(df) := lapply(.SD,as.character)]
#.EachI
df[df2,.N,on = 'x',by = .EACHI] # merge two data tables and summarise #.EACHI gives summary against each unique in df2( OR in Rows i.e. on)
df[df2,.(mean(y),sum(y),.N),on = 'x',by = .EACHI] # merge two data tables and summarise #.EACHI gives summary against each unique in df2( OR in Rows i.e. on)
df[df2,.(mean(y),mean(z),sum(y),sum(z),.N),on = 'x',by = .EACHI] # merge two data tables and summarise #.EACHI gives summary against each unique in df2( OR in Rows i.e. on)
dt[,mean_mpg := mean(mpg),cyl]   # This is assignment way to use ":="
dt[,':='(mean_mpg = mean(mpg)
         ,sum_mpg = sum(mpg)),cyl]  # This is functional way to use ":="
setDT(dt) <- mtcars
.GRP # This is useful check it later


#39 . Introduction to data.table in R Part 4
unique_key = unique(c(df$x,df2$x))
df[df2[.(unique_key),on = 'x'],on = 'x']

merge(df,df2,all.x=T,all.y=T)  # merge function is available for base, data.frame and data.table
#s3/s4/mlr3
When df and df2 are
x <- c(1,2,3,4,5,6,7,8,9)
summary(x)
class(x) <- 'champak'
summary.champak <- function(x)   # This technique is called as Method dispatch
{
  return(table(x))
}
#S3 dispatch or Polymorphism
# Advanced R and Blue book for S programming
S3, S4, R6, RC  give preference to S3 and R6
# For looking for source codes of generic functions like print insteat search source code of print.default
print.champak <- function(x) {
  cat('champak are : ',unique(x),'\n')
}
print(x)   # champak are :  1 2 3 4 5 6 7 8 9 


#40 . Introduction to data.table in R Part 5
mt <- mtcars
setDT(mt,keep.rownames = TRUE) # For retaining rownames when needed
mt1 <- mt[,.(newcyl = list(cyl)),.(am)]
   am          newcyl
1:  1      6,6,4,4,4,4,...
2:  0      6,8,6,8,4,4,...

# Sometimes when R shows comma it can also mean that the information is stored as list and there is no commas
simply use str to get its structure
setkey()   # learn sometihing about o binary search
setkeyv()  #  

#41 . Basics of stringr and purr in R

To get number of functions in a package
length(ls.str('package:stringr',mode = 'function'))
v1 <- c(1,2,3)
v2 <- c('a','b',NA)
is.na(paste0(v1,v2))
is.na(str_c(v1,v2))
str_view('dsfr89980','\\d+')
str_view_all('dsgr89980dsf83269fj3820','\\d+')
str_extract
str_extract_all
str_replace
str_replace_all
str_count
#purrr is used to get predictable outcomes
map(c(1,2,3),function(x)+1) 
map(c(1,2,3),~.x+1) 

path <- r'(E:\experiment)'
files <- list.files(path = path, pattern = '\\.txt')
read.csv(file.path(path,files[1],check.names = TRUE)
pmap
walk

#42. Meta Programming in R - 1
Meta programming means reading codes as data
Meta programming will improve our understanding of how functions work
Environment -> examples Global Environment
For example you can write a list as
list(a=2,a=10) # You can do this
you cannot assign multiple values to same variable in environment
There is no chronological ordering in environments
@ Environment has binding
e <- new.env()    #To Create new environment
e$a_1 <- 25
e            #does not return variables or value
ls(e)        #returns variables
e[['a_1']]   #returns value of a_1
OR
as.list(e)   #returns variables and their values
# a list can be creating without giving names however environment need proper bindings(Names) to operate.
l <- list(a= 100,b=200)
search()         #Returns all the environment and packages that are loaded in reverse chronological order
list2env(l,envir = .GlobalEnv)
ls()
list2env(l,envir = e)
f <- function(x){
  2*x
}
formals(f)      # Parameters given
body(f)         #
environment(f)  #
f <- function(x){
  print(environment())
  2*x
}
#lets add a constant parameter
f <- function(x,y=10){
  2*x+y
}
typeof(formals)(f)      # Returns both parameters along with value of default parameter
#And consider second function g with second parameter which is a variable
list(x=2,y=2*x)       #This will not work as it is finding x in environment
g <- function(x,y=2*x){
  2*x+y
}
the function works as an alist
typeof(formals) # Closure 
we can change the value or arguements
x is our calling scope or caller environment
Let us check their classes
p1 <- formals(f)
p2 <- formals(g)
names(p1)   # 'x' 'y'
names(p2)   # 'x' 'y'


for (i in names(p1)){                # "name" "numeric"
  print(class(p1[[i]]))
}

for (i in names(p2)){               #   "name" "call"
  print(class(p2[[i]]))
}

a <- 100
x <- "a"
z <- as.name(x)
class(z)       #it is name
Always check the class
eval(z, envir = e)        #NOTE always give arguement envir when using eval, it can give undesired results otherwise
eval(z) may return different values depending on which environment is selected
#eval can be used to evaluate named objects
get(z, envir = e)    # it is similar to eval
quote(2*y) is a call object
and 
class(as.list(body(g)))      # It is also a call object
you can change definition of g using quote
body(g) <- quote(2*x**2)
# call objects can be overwritten
quote is a type of intent function it can replace items
# properties of environment 1) unique 2) order 3) name binded with parent frame
Therefore, a function has : Create environment, Execute environment, Evauate enviornment
function -> default = datatype, non-default = as.name

#43. Meta Programming in R - 2
Scoping Rules
Parent Frame : calling scope
lexical scoping and dynamic scoping
lazy evaluations

Value to be used for variables is first searched in function then in parent environment
f <- function(x) {     #It finds value of x in function and runs
  x
}

f <- function(x){     #It will always give 2 no matter what the value is givien to the parameter
  x <- 2
  x
}

f <- function(x) {   #Unless y is mentioned in the enviornment it will throw an error, so either make y an parameter or mention it in function or atleast in the enviornment
  x+y
  print(parent.env(environment()))
}

g <- function() {
  
  function(x){
    x+y
  }
}

g()(2)      #to run a complex function

## What happens when Global Environment == Calling Environment
## What happens when Global Environment != Calling Environment

f <- function(x){
  y <- 1
  y + g(x)
}
##for above function, is Calling env == global env, NO , It is called Lexical scoping
#calling env == parent.frame()
dynamic scoping means Calling Env == Global environment

#Lazy evaluation

y <- 3
g <- function(x){
  x+y
}


f <- function(x,y = {x +1}){          #First: gets value of x from arguement Second: it will see the Promise of y and fulfil it from arguement 
  y                                   #Third: It will assign new value to x and run remaining function assuming value of y to be previous (x+1) and not latest (x+1)
  x <- 5                              #Note Each promise works only once
  x + y
}

Lazy Evaluation means it will not evaluate values unless object is called.
f <- function(x = {y <- 1;10},y = 2){
  x + y
}
f <- function(x = {y <- 1;10},y = 2){
  y + x
}
#Notice here x+y != y+x

f <- function(a){
  function(b){
    a+b
  }
}
f(1)(1)

func_obj <- vector('list',5)
for (i in 1:5){
  func_obj[[i]] <- f(i)
}
func_obj[[1]](1)
# quote and substitute , parse, deparse

#44. Meta Programming in R - 3
R defaults to lexical lookup instead of dynamic lookup
#Lexical Lookup
#Dynamical Lookup

#45. Meta Programming in R - 4
x <- 10
as.name(x)         gives "10"
quote(X)           gives x or 10 without quotes

#suppose if 
x <- 'y'
y <- 20
eval(as.name(x))    gives 20
class(quote(x))       #Name
identical(quote(x),substitute(x))
#BUT
quote(expression)
substitute(expression,environment)
#if
x <- 10
eval(quote(x))
substitute(x,environment())
e <- new.env()
e$x <- 20
eval(substitute(x,e),envir = globalenv())
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
class(substitute(2+3))      # Call
class(substitute(2))        # numeric
eval(substitute(2 + 3,list('+'= '*')))      # Call
eval(substitute(2 + 3,list( "+" = "-")))    #Check it later it should work but is not working
eval(substitute(x + 1, list('x' = 10,'+' = "*")))
eval(substitute(x + 1, list('x' = 10,'+' = "*")))
Error in eval(substitute(x + 1, list(x = 10, `+` = "*"))) : 
  attempt to apply non-function
eval(substitute(mean(x),list('mean'='sum')))
#NOTICE when you deliberately pass eval(,envir = parent.frame()) it works in calling scope
#            you do not deliberately pass eval(,envir = parent.frame()) then it works in current frame

#46. Meta Programming in R - 5

#we have seen that in global : quote and substitute behave similar but substitute has extra arguement for enivronment
f <- function(x){
  s <- substitute(x)
  print(s)
  print('---Break---')
  q <- quote(x)
  print(q)
}
f(a)
#### quote and substitute behave differently in function
f <- function(x){
  s <- substitute(x)        ##a
  print(s)
  print(eval(s))
  print('---Break---')
  q <- quote(x)             ##x
  print(q)
  print(eval(q))
}
f(a)
Now
f <- function(x){
  q <- quote(x)             ##x
  print(q)
  print(eval(q))
  print('---Break---')
  s <- substitute(x)        ##a
  print(s)
  print(eval(s))
}
### For both above functions it will throw error that object is not found
f1 <- function(x){
  a <- 2
  s <- substitute(x)        ##a
  print(s)
  print(eval(s))
  
}
f1(a)      gives 2  #Since by default envir is parent.frame() but it is finding a in current frame(i.e. in the function)
f2 <- function(x){
  a <- 2
  s <- substitute(x)        ##a
  print(s)
  print(eval(s, envir = parent.frame()))
}
ls()
f2(a)   gives error that object not found # Even though we have given environment as parent.frame() it is searching for 'a' in global environment
Remember the person who made the function did not knew which environment you want to search your values in.

##?Syntax
:: :::	        access variables in a namespace
$ @	            component / slot extraction
[ [[	          indexing
^	              exponentiation (right to left)
- +	            unary minus and plus
:	              sequence operator
%any% |>	      special operators (including %% and %/%)
* /	            multiply, divide
+ -	            (binary) add, subtract
< > <= >= == !=	ordering and comparison
!	              negation
& &&	          and
| ||	          or
~	              as in formulae
-> ->>	        rightwards assignment
<- <<-	        assignment (right to left)
=	              assignment (right to left) 
?               help(unary and binary)


## Find all Functions of libraries below:->

lsf.str("package:base")

# 
# a<- capture.output(ls.str("package:RODBC",all.names = TRUE))
# b<- capture.output(ls.str("package:base",all.names = TRUE))
# 
# class(c) <- list(a,b)
# 
# c <- paste(a,b)
# 
# c <- writeLines(c)
# Final <- writeLines(paste0(allforone),sep = "\n")
# write.table(Final,"D:/Practice/New joinee training/ML/practice.txt",sep = "|")
# 
# 
# Final <- writeLines(a,sep = "\n")
# 
# 
# pack_list <- list('base',
#                   'beepr',
#                   'caret',
#                   'checkmate',
#                   'chron',
#                   'cowsay',
#                   'class',
#                   'crayon',
#                   'data.table',
#                   'datasets',
#                   'dplyr',
#                   'DT',
#                   'fastmatch',
#                   'fortunes',
#                   'fun',
#                   'geosphere',
#                   'ggplot2',
#                   'gmp',
#                   'graphics',
#                   'grDevices',
#                   'htmltools',
#                   'htmlwidgets',
#                   'janitor',
#                   'jsonlite',
#                   'knitr',
#                   'leaflet',
#                   'lobstr',
#                   'lubridate',
#                   'MASS',
#                   'methods',
#                   'microbenchmark',
#                   'mlr',
#                   'multicolor',
#                   'nlme',
#                   'openxlsx',
#                   'phonics',
#                   'praise',
#                   'progress',
#                   'progressr',
#                   'purrr',
#                   'Rcrawler',
#                   'rmarkdown',
#                   'readr',
#                   'readxl',
#                   'RODBC',
#                   'shiny',
#                   'stats',
#                   'stringr',
#                   'stringi',
#                   'utils',
#                   'wikifacts')
# 
# demo <- list(c("base","stats"))
# 
# for (i in demo){
#   a <- 'capture.output(ls.str("package:',,all.names = TRUE))"
#   return(a)
# }
# 
# e <-capture.output(writeLines( "This is a base package, base is an inbuilt package in r .
#        do definetly use Base."))
# gsub(x = e,pattern = "base",replacement = "a1",ignore.case = TRUE)
# 
#        
#        
#        
# a1 <-  toString(capture.outputas.character(writeLines(ls.str("package:base", all.names = TRUE))))
# a2 <-  toString(capture.output(writeLines(ls.str("package:beepr", all.names = TRUE))))                 
# a3 <-  toString(capture.output(writeLines(ls.str("package:caret" ,all.names = TRUE))))                 
# a4 <-  toString(capture.output(writeLines(ls.str("package:checkmate", all.names = TRUE))))                 
# a5 <-  toString(capture.output(writeLines(ls.str("package:chron" ,all.names = TRUE))))                 
# a6 <-  toString(capture.output(writeLines(ls.str("package:cowsay", all.names = TRUE)))) 
# a7 <-  toString(capture.output(writeLines(ls.str("package:class" ,all.names = TRUE)))) 
# a8 <-  toString(capture.output(writeLines(ls.str("package:crayon", all.names = TRUE))))
# a9 <-  toString(capture.output(writeLines(ls.str("package:data.table", all.names = TRUE))))
# a10 <- toString(capture.output(writeLines(ls.str("package:datasets", all.names = TRUE)))) 
# a11 <- toString(capture.output(writeLines(ls.str("package:dplyr", all.names = TRUE))))    
# a12 <- toString(capture.output(writeLines(ls.str("package:DT", all.names = TRUE))))       
# a13 <- toString(capture.output(writeLines(ls.str("package:fastmatch" ,all.names = TRUE))))
# a14 <- toString(capture.output(writeLines(ls.str("package:fortunes", all.names = TRUE)))) 
# a15 <- toString(capture.output(writeLines(ls.str("package:fun", all.names = TRUE))))      
# a16 <- toString(capture.output(writeLines(ls.str("package:geosphere", all.names = TRUE))))
# a17 <- toString(capture.output(writeLines(ls.str("package:ggplot2", all.names = TRUE))))  
# a18 <- toString(capture.output(writeLines(ls.str("package:gmp", all.names = TRUE))))      
# a19 <- toString(capture.output(writeLines(ls.str("package:graphics", all.names = TRUE))))   
# a20 <- toString(capture.output(writeLines(ls.str("package:grDevices", all.names = TRUE))))  
# a21 <- toString(capture.output(writeLines(ls.str("package:htmltools" ,all.names = TRUE)))) 
# a22 <- toString(capture.output(writeLines(ls.str("package:htmlwidgets", all.names = TRUE))))
# a23 <- toString(capture.output(writeLines(ls.str("package:janitor", all.names = TRUE))))    
# a24 <- toString(capture.output(writeLines(ls.str("package:jsonlite" ,all.names = TRUE))))   
# a25 <- toString(capture.output(writeLines(ls.str("package:knitr", all.names = TRUE))))    
# a26 <- toString(capture.output(writeLines(ls.str("package:leaflet", all.names = TRUE))))  
# a27 <- toString(capture.output(writeLines(ls.str("package:lobstr", all.names = TRUE))))   
# a28<-  toString(capture.output(writeLines(ls.str("package:lubridate", all.names = TRUE))))      
# a29 <- toString(capture.output(writeLines(ls.str("package:MASS", all.names = TRUE))))          
# a30 <- toString(capture.output(writeLines(ls.str("package:methods", all.names = TRUE))))       
# a31 <- toString(capture.output(writeLines(ls.str("package:microbenchmark", all.names = TRUE))))
# a32 <- toString(capture.output(writeLines(ls.str("package:mlr" ,all.names = TRUE))))        
# a33<-  toString(capture.output(writeLines(ls.str("package:multicolor" ,all.names = TRUE))))  
# a34 <- toString(capture.output(writeLines(ls.str("package:nlme" ,all.names = TRUE))))       
# a35 <- toString(capture.output(writeLines(ls.str("package:openxlsx", all.names = TRUE))))   
# a36 <- toString(capture.output(writeLines(ls.str("package:phonics", all.names = TRUE))))    
# a37 <- toString(capture.output(writeLines(ls.str("package:praise", all.names = TRUE))))     
# a38 <- toString(capture.output(writeLines(ls.str("package:progress", all.names = TRUE))))   
# a39 <- toString(capture.output(writeLines(ls.str("package:progressr", all.names = TRUE))))  
# a40 <- toString(capture.output(writeLines(ls.str("package:purrr", all.names = TRUE))))      
# a41 <- toString(capture.output(writeLines(ls.str("package:Rcrawler", all.names = TRUE))))   
# a42 <- toString(capture.output(writeLines(ls.str("package:rmarkdown", all.names = TRUE))))  
# a43 <- toString(capture.output(writeLines(ls.str("package:readr", all.names = TRUE))))      
# a44 <- toString(capture.output(writeLines(ls.str("package:readxl", all.names = TRUE))))     
# a45 <- toString(capture.output(writeLines(ls.str("package:RODBC", all.names = TRUE))))      
# a46 <- toString(capture.output(writeLines(ls.str("package:shiny", all.names = TRUE))))      
# a47 <- toString(capture.output(writeLines(ls.str("package:stats", all.names = TRUE))))      
# a48 <- toString(capture.output(writeLines(ls.str("package:stringr", all.names = TRUE))))    
# a49 <- toString(capture.output(writeLines(ls.str("package:stringi", all.names = TRUE))))    
# a50 <- toString(capture.output(writeLines(ls.str("package:utils", all.names = TRUE))))      
# a51 <- toString(capture.output(writeLines(ls.str("package:wikifacts", all.names = TRUE))))
#        
# final <- paste(a1, 
# a2 ,
# a3 ,
# a4 ,
# a5 ,
# a6 ,
# a7 ,
# a8 ,
# a9 ,
# a10,
# a11,
# a12,
# a13,
# a14,
# a15,
# a16,
# a17,
# a18,
# a19,
# a20,
# a21,
# a22,
# a23,
# a24,
# a25,
# a26,
# a27,
# a28,
# a29,
# a30,
# a31,
# a32,
# a33,
# a34,
# a35,
# a36,
# a37,
# a38,
# a39,
# a40,
# a41,
# a42,
# a43,
# a44,
# a45,
# a46,
# a47,
# a48,
# a49,
# a50,
# a51,sep = "|")
#        
# write.table(final,"D:/Practice/New joinee training/ML/R tips and tricks/Function_list.txt")
#        
# u <- c("apple","Mango")
# v <- c("banana","Citron")
# u <- toString(u)
# v <- toString(v)
#        
# y <- paste(u,v,sep= "|")       
# 
# gsub(y,pattern = "(.{1,25})(\\s|$)",replacement = "\\1\n")       
#        
# print(gsub(y,pattern = ", ",replacement = "[\i\n]"))"
# 
# ?getCall
#   
# a1 <- toString(capture.output(writeLines(ls.str("package:base", all.names = TRUE))))
# 
# a2 <- unlist(a1)
       
library(base)
- : function (e1, e2)
-.Date : function (e1, e2)
-.POSIXt : function (e1, e2)  "
! : function (x)  "
!.hexmode : function (a)  "
!.octmode : function (a)  "
!= : function (e1, e2)  "
$ : .Primitive(\"$\") "
$.DLLInfo : function (x, name)  "
$.package_version : function (x, name)  "
$<- : .Primitive(\"$<-\") "
$<-.data.frame : function (x, name, value)  "
%% : function (e1, e2)  "
%*% : function (x, y)  "
%/% : function (e1, e2)  "
%in% : function (x, table)  "
%o% : function (X, Y)  "
%x% : function (X, Y)  "
& : function (e1, e2)  "
&& : .Primitive(\"&&\") "
&.hexmode : function (a, b)  "
&.octmode : function (a, b)  "
( : .Primitive(\"(\") "
* : function (e1, e2)  "
*.difftime : function (e1, e2)  "
...elt : function (n)  "
...length : function ()  "
...names : function ()  "
..deparseOpts :  chr [1:13] \"all\" \"keepInteger\" \"quoteExpressions\" \"showAttributes\" \"useSource\" ..."
..getNamespace : function (name, where)  "
.__H__.cbind : function (..., deparse.level = 1)  "
.__H__.rbind : function (..., deparse.level = 1)  "
.__S3MethodsTable__. : <environment: 0x000001dbe4562208> "
.amatch_bounds : function (x = 0.1)  "
.amatch_costs : function (x = NULL)  "
.ArgsEnv : <environment: 0x000001dbe459d450> "
.AutoloadEnv : <environment: 0x000001dbe483bc88> "
.BaseNamespaceEnv : <environment: namespace:base> "
.bincode : function (x, breaks, right = TRUE, include.lowest = FALSE)  "
.C : function (.NAME, ..., NAOK = FALSE, DUP = TRUE, PACKAGE, ENCODING)  "
.C_R_addTaskCallback : List of 4"
 $ name         : chr \"R_addTaskCallback\""
 $ address      :Class 'RegisteredNativeSymbol' <externalptr> "
 $ dll          :List of 5"
 $ numParameters: int 4"
.C_R_getTaskCallbackNames : List of 4"
 $ name         : chr \"R_getTaskCallbackNames\""
 $ address      :Class 'RegisteredNativeSymbol' <externalptr> "
 $ dll          :List of 5"
 $ numParameters: int 0"
.C_R_removeTaskCallback : List of 4"
 $ name         : chr \"R_removeTaskCallback\""
 $ address      :Class 'RegisteredNativeSymbol' <externalptr> "
 $ dll          :List of 5"
 $ numParameters: int 1"
.cache_class : function (class, extends)  "
.Call : function (.NAME, ..., PACKAGE)  "
.Call.graphics : function (.NAME, ..., PACKAGE)  "
.class2 : function (x)  "
.col : function (dim)  "
.colMeans : function (x, m, n, na.rm = FALSE)  "
.colSums : function (x, m, n, na.rm = FALSE)  "
.Date : function (xx, cl = \"Date\")  "
.decode_numeric_version : function (x)  "
.Defunct : function (new, package = NULL, msg)  "
.deparseOpts : function (control)  "
.Deprecated : function (new, package = NULL, msg, old = as.character(sys.call(sys.parent()))[1L])  "
.detach : function (pos)  "
.Device :  chr \"null device\""
.Devices : Dotted pair list of 1"
 $ : chr \"null device\""
.difftime : function (xx, units, cl = \"difftime\")  "
.doSortWrap : function (vec, decr, nalast, noNA = NA)  "
.doTrace : function (expr, msg)  "
.doWrap : function (vec, decr, nalast, noNA = NA)  "
.dynLibs : function (new)  "
.encode_numeric_version : function (x)  "
.expand_R_libs_env_var : function (x)  "
.External : function (.NAME, ..., PACKAGE)  "
.External.graphics : function (.NAME, ..., PACKAGE)  "
.External2 : function (.NAME, ..., PACKAGE)  "
.F_dchdc :  NULL"
.F_dqrcf : List of 4"
 $ name         : chr \"dqrcf\""
 $ address      :Class 'RegisteredNativeSymbol' <externalptr> "
 $ dll          :List of 5"
 $ numParameters: int 8"
.F_dqrdc2 : List of 4"
 $ name         : chr \"dqrdc2\""
 $ address      :Class 'RegisteredNativeSymbol' <externalptr> "
 $ dll          :List of 5"
 $ numParameters: int 9"
.F_dqrqty : List of 4"
 $ name         : chr \"dqrqty\""
 $ address      :Class 'RegisteredNativeSymbol' <externalptr> "
 $ dll          :List of 5"
 $ numParameters: int 7"
.F_dqrqy : List of 4"
 $ name         : chr \"dqrqy\""
 $ address      :Class 'RegisteredNativeSymbol' <externalptr> "
 $ dll          :List of 5"
 $ numParameters: int 7"
.F_dqrrsd : List of 4"
 $ name         : chr \"dqrrsd\""
 $ address      :Class 'RegisteredNativeSymbol' <externalptr> "
 $ dll          :List of 5"
 $ numParameters: int 7"
.F_dqrxb : List of 4"
 $ name         : chr \"dqrxb\""
 $ address      :Class 'RegisteredNativeSymbol' <externalptr> "
 $ dll          :List of 5"
 $ numParameters: int 7"
.F_dtrco : List of 4"
 $ name         : chr \"dtrco\""
 $ address      :Class 'RegisteredNativeSymbol' <externalptr> "
 $ dll          :List of 5"
 $ numParameters: int 6"
.First.sys : function ()  "
.fixupGFortranStderr : function ()  "
.fixupGFortranStdout : function ()  "
.format.zeros : function (x, zero.print, nx = suppressWarnings(as.numeric(x)), replace = FALSE, warn.non.fitting = TRUE)  "
.Fortran : function (.NAME, ..., NAOK = FALSE, DUP = TRUE, PACKAGE, ENCODING)  "
.GenericArgsEnv : <environment: 0x000001dbe45b86a0> "
.getNamespace : function (name)  "
.getNamespaceInfo : function (ns, which)  "
.getRequiredPackages : function (file = \"DESCRIPTION\", lib.loc = NULL, quietly = FALSE, useImports = FALSE)  "
.getRequiredPackages2 : function (pkgInfo, quietly = FALSE, lib.loc = NULL, useImports = FALSE)  "
.GlobalEnv : <environment: R_GlobalEnv> "
.gt : function (x, i, j)  "
.gtn : function (x, strictly)  "
.handleSimpleError : function (h, msg, call)  "
.Internal : function (call)  "
.isMethodsDispatchOn : function (onOff = NULL)  "
.isOpen : function (srcfile)  "
.kappa_tri : function (z, exact = FALSE, LINPACK = TRUE, norm = NULL, ...)  "
.knownS3Generics :  Named chr [1:54] \"base\" \"base\" \"base\" \"base\" \"base\" \"base\" \"base\" \"base\" \"base\" ..."
.kronecker : function (X, Y, FUN = \"*\", make.dimnames = FALSE, ...)  "
.Last.value :  NULL"
.LC.categories :  chr [1:9] \"LC_ALL\" \"LC_COLLATE\" \"LC_CTYPE\" \"LC_MONETARY\" \"LC_NUMERIC\" \"LC_TIME\" ..."
.leap.seconds :  POSIXct[1:27], format: \"1972-07-01\" \"1973-01-01\" \"1974-01-01\" \"1975-01-01\" \"1976-01-01\" \"1977-01-01\" ..."
.libPaths : function (new, include.site = TRUE)  "
.Library :  chr \"C:/PROGRA~1/R/R-42~1.1/library\""
.Library.site :  chr(0) "
.Machine : List of 28"
 $ double.eps               : num 2.22e-16"
 $ double.neg.eps           : num 1.11e-16"
 $ double.xmin              : num 2.23e-308"
 $ double.xmax              : num 1.8e+308"
 $ double.base              : int 2"
 $ double.digits            : int 53"
 $ double.rounding          : int 5"
 $ double.guard             : int 0"
 $ double.ulp.digits        : int -52"
 $ double.neg.ulp.digits    : int -53"
 $ double.exponent          : int 11"
 $ double.min.exp           : int -1022"
 $ double.max.exp           : int 1024"
 $ integer.max              : int 2147483647"
 $ sizeof.long              : int 4"
 $ sizeof.longlong          : int 8"
 $ sizeof.longdouble        : int 16"
 $ sizeof.pointer           : int 8"
 $ longdouble.eps           : num 1.08e-19"
 $ longdouble.neg.eps       : num 5.42e-20"
 $ longdouble.digits        : int 64"
 $ longdouble.rounding      : int 5"
 $ longdouble.guard         : int 0"
 $ longdouble.ulp.digits    : int -63"
 $ longdouble.neg.ulp.digits: int -64"
 $ longdouble.exponent      : int 15"
 $ longdouble.min.exp       : int -16382"
 $ longdouble.max.exp       : int 16384"
.make_numeric_version : function (x, strict = TRUE, regexp, classes = NULL)  "
.makeMessage : function (..., domain = NULL, appendLF = FALSE)  "
.mapply : function (FUN, dots, MoreArgs)  "
.maskedMsg : function (same, pkg, by)  "
.mergeExportMethods : function (new, ns)  "
.mergeImportMethods : function (impenv, expenv, metaname)  "
.noGenerics :  logi TRUE"
.NotYetImplemented : function ()  "
.NotYetUsed : function (arg, error = TRUE)  "
.Options : Dotted pair list of 175"
 $ prompt                              : chr \"> \""
 $ continue                            : chr \"+ \""
 $ expressions                         : int 5000"
 $ width                               : int 91"
 $ deparse.cutoff                      : int 60"
 $ digits                              : int 3"
 $ echo                                : logi TRUE"
 $ verbose                             : logi FALSE"
 $ check.bounds                        : logi FALSE"
 $ keep.source                         : logi TRUE"
 $ keep.source.pkgs                    : logi FALSE"
 $ keep.parse.data                     : logi TRUE"
 $ keep.parse.data.pkgs                : logi FALSE"
 $ warning.length                      : int 1000"
 $ nwarnings                           : int 50"
 $ OutDec                              : chr \".\""
 $ browserNLdisabled                   : logi FALSE"
 $ CBoundsCheck                        : logi FALSE"
 $ matprod                             : chr \"default\""
 $ PCRE_study                          : logi FALSE"
 $ PCRE_use_JIT                        : logi TRUE"
 $ PCRE_limit_recursion                : logi NA"
 $ warn                                : int 0"
 $ timeout                             : int 60"
 $ encoding                            : chr \"native.enc\""
 $ show.error.messages                 : logi TRUE"
 $ scipen                              : num 0"
 $ max.print                           : int 1000"
 $ add.smooth                          : logi TRUE"
 $ stringsAsFactors                    : logi FALSE"
 $ defaultPackages                     : chr [1:6] \"datasets\" \"utils\" \"grDevices\" \"graphics\" ..."
 $ papersize                           : chr \"a4\""
 $ pager                               :function (files, header, title, delete.file)  "
 $ useFancyQuotes                      : logi TRUE"
 $ pdfviewer                           : chr \"C:/PROGRA~1/R/R-42~1.1/bin/x64/open.exe\""
 $ help_type                           : chr \"html\""
 $ help.try.all.packages               : logi FALSE"
 $ help.search.types                   : chr [1:3] \"vignette\" \"demo\" \"help\""
 $ citation.bibtex.max                 : int 1"
 $ internet.info                       : int 2"
 $ pkgType                             : chr \"both\""
 $ str                                 :List of 7"
 $ demo.ask                            : chr \"default\""
 $ example.ask                         : chr \"default\""
 $ HTTPUserAgent                       : chr \"RStudio Desktop (2022.2.3.492); R (4.2.1 x86_64-w64-mingw32 x86_64 mingw32)\""
 $ menu.graphics                       : logi FALSE"
 $ mailer                              : chr \"mailto\""
 $ install.packages.compile.from.source: chr \"interactive\""
 $ unzip                               : chr \"internal\""
 $ editor                              :function (name, file = \"\", title = file, ...)  "
 $ repos                               : Named chr \"https://cran.rstudio.com/\""
 $ askYesNo                            :function (msg, ...)  "
 $ locatorBell                         : logi TRUE"
 $ device.ask.default                  : logi FALSE"
 $ windowsTimeouts                     : int [1:2] 100 500"
 $ device                              : chr \"RStudioGD\""
 $ contrasts                           : Named chr [1:2] \"contr.treatment\" \"contr.poly\""
 $ na.action                           : chr \"na.omit\""
 $ show.coef.Pvalues                   : logi TRUE"
 $ show.signif.stars                   : logi TRUE"
 $ str.dendrogram.last                 : chr \"`\""
 $ ts.eps                              : num 1e-05"
 $ ts.S.compat                         : logi FALSE"
 $ terminal.manager                    :List of 13"
 $ connectionObserver                  :List of 3"
 $ profvis.prof_output                 : chr \"C:/Users/50045208/AppData/Local/RStudio/profiles-cache\""
 $ profvis.print                       :function (x)  "
 $ profvis.prof_extension              : chr \".Rprof\""
 $ ggvis.renderer                      : chr \"svg\""
 $ buildtools.check                    :function (action)  "
 $ buildtools.with                     :function (code)  "
 $ shiny.launch.browser                :function (url)  "
 $ plumber.docs.callback               :function (url)  "
 $ plumber.swagger.url                 :function (url)  "
 $ reticulate.initialized              :function ()  "
 $ reticulate.repl.initialize          :function ()  "
 $ reticulate.repl.hook                :function (buffer, contents, trimmed)  "
 $ reticulate.repl.busy                :function (busy)  "
 $ reticulate.repl.teardown            :function ()  "
 $ RStudioGD.backend                   : chr \"default\""
 $ RStudioGD.antialias                 : chr \"default\""
 $ browser                             :function (url)  "
 $ viewer                              :function (url, height = NULL)  "
 $ page_viewer                         :function (url, title = \"RStudio Viewer\", self_contained = FALSE)  "
 $ shinygadgets.showdialog             :function (caption, url, width = NULL, height = NULL)  "
 $ askpass                             :function (prompt)  "
 $ asksecret                           :function (name, title = name, prompt = paste(name, \":\", sep = \"\"))  "
 $ restart                             :function (afterRestartCommand = \"\")  "
 $ profvis.keep_output                 : logi TRUE"
 $ rstudio.notebook.executing          : logi FALSE"
 $ deparse.max.lines                   : int 20"
 $ download.file.method                : chr \"libcurl\""
 $ rsconnect.check.certificate         : logi TRUE"
 $ callr.condition_handler_cli_message :function (msg)  "
 $ httr_oob_default                    : logi FALSE"
 $ httr_oauth_cache                    : logi NA"
 $ dplyr.show_progress                 : logi TRUE"
 $ foreachDoparLocal                   : logi TRUE"
 $ ambiguousMethodSelection            :function (cond)  "
  [list output truncated]"
.OptRequireMethods : function ()  "
.packages : function (all.available = FALSE, lib.loc = NULL)  "
.packageStartupMessage : function (message, call = NULL)  "
.Platform : List of 8"
 $ OS.type   : chr \"windows\""
 $ file.sep  : chr \"/\""
 $ dynlib.ext: chr \".dll\""
 $ GUI       : chr \"RStudio\""
 $ endian    : chr \"little\""
 $ pkgType   : chr \"win.binary\""
 $ path.sep  : chr \";\""
 $ r_arch    : chr \"x64\""
.popath :  chr \"C:/Program Files/R/R-4.2.1/library/translations\""
.POSIXct : function (xx, tz = NULL, cl = c(\"POSIXct\", \"POSIXt\"))  "
.POSIXlt : function (xx, tz = NULL, cl = c(\"POSIXlt\", \"POSIXt\"))  "
.pretty : function (x, n = 5L, min.n = n%/%3L, shrink.sml = 0.75, high.u.bias = 1.5, u5.bias = 0.5 + "
    1.5 * high.u.bias, eps.correct = 0L, f.min = 2^-20, bounds = TRUE)  "
.Primitive : function (name)  "
.primTrace : function (obj)  "
.primUntrace : function (obj)  "
.rmpkg : function (pkg)  "
.row : function (dim)  "
.row_names_info : function (x, type = 1L)  "
.rowMeans : function (x, m, n, na.rm = FALSE)  "
.rowNamesDF<- : function (x, make.names = FALSE, value)  "
.rowSums : function (x, m, n, na.rm = FALSE)  "
.S3_methods_table :  chr [1:380, 1:2] \"!\" \"!\" \"$\" \"$\" \"$<-\" \"&\" \"&\" \"*\" \"+\" \"+\" \"-\" \"-\" \"/\" \"[\" \"[\" \"[\" ..."
.S3method : function (generic, class, method)  "
.S3PrimitiveGenerics :  chr [1:30] \"anyNA\" \"as.character\" \"as.complex\" \"as.double\" \"as.environment\" ..."
.Script : function (interpreter, script, args, ...)  "
.set_row_names : function (n)  "
.signalSimpleWarning : function (msg, call)  "
.standard_regexps : function ()  "
.subset : function (x, ...)  "
.subset2 : function (x, ...)  "
.sys.timezone :  chr NA"
.TAOCP1997init : function (seed)  "
.traceback : function (x = NULL, max.lines = getOption(\"traceback.max.lines\", getOption(\"deparse.max.lines\", "
    -1L)))  "
.Traceback : Dotted pair list of 1"
 $ : language ls.str(\"package:stringi\", all.names = T)"
.tryResumeInterrupt : function ()  "
.userHooksEnv : <environment: 0x000001dbe570b798> "
.valid.factor : function (object)  "
/ : function (e1, e2)  "
/.difftime : function (e1, e2)  "
: : .Primitive(\":\") "
:: : function (pkg, name)  "
::: : function (pkg, name)  "
@ : .Primitive(\"@\") "
@<- : .Primitive(\"@<-\") "
[ : .Primitive(\"[\") "
[.AsIs : function (x, i, ...)  "
[.data.frame : function (x, i, j, drop = if (missing(i)) TRUE else length(cols) == 1)  "
[.Date : function (x, ..., drop = TRUE)  "
[.difftime : function (x, ..., drop = TRUE)  "
[.Dlist : function (x, i, ...)  "
[.DLLInfoList : function (x, ...)  "
[.factor : function (x, ..., drop = FALSE)  "
[.hexmode : function (x, i)  "
[.listof : function (x, i, ...)  "
[.noquote : function (x, ...)  "
[.numeric_version : function (x, i, j)  "
[.octmode : function (x, i)  "
[.POSIXct : function (x, ..., drop = TRUE)  "
[.POSIXlt : function (x, i, j, drop = TRUE)  "
[.simple.list : function (x, i, ...)  "
[.table : function (x, i, j, ..., drop = TRUE)  "
[.warnings : function (x, ...)  "
[[ : .Primitive(\"[[\") "
[[.data.frame : function (x, ..., exact = TRUE)  "
[[.Date : function (x, ..., drop = TRUE)  "
[[.factor : function (x, ...)  "
[[.numeric_version : function (x, ..., exact = NA)  "
[[.POSIXct : function (x, ..., drop = TRUE)  "
[[.POSIXlt : function (x, i, drop = TRUE)  "
[[<- : .Primitive(\"[[<-\") "
[[<-.data.frame : function (x, i, j, value)  "
[[<-.factor : function (x, ..., value)  "
[[<-.numeric_version : function (x, ..., value)  "
[[<-.POSIXlt : function (x, i, value)  "
[<- : .Primitive(\"[<-\") "
[<-.data.frame : function (x, i, j, value)  "
[<-.Date : function (x, ..., value)  "
[<-.difftime : function (x, i, value)  "
[<-.factor : function (x, ..., value)  "
[<-.numeric_version : function (x, i, j, value)  "
[<-.POSIXct : function (x, ..., value)  "
[<-.POSIXlt : function (x, i, j, value)  "
^ : function (e1, e2)  "
{ : .Primitive(\"{\") "
| : function (e1, e2)  "
|.hexmode : function (a, b)  "
|.octmode : function (a, b)  "
|| : .Primitive(\"||\") "
~ : .Primitive(\"~\") "
+ : function (e1, e2)  "
+.Date : function (e1, e2)  "
+.POSIXt : function (e1, e2)  "
< : function (e1, e2)  "
<- : .Primitive(\"<-\") "
<<- : .Primitive(\"<<-\") "
<= : function (e1, e2)  "
= : .Primitive(\"=\") "
== : function (e1, e2)  "
> : function (e1, e2)  "
>= : function (e1, e2)  "
abbreviate : function (names.arg, minlength = 4L, use.classes = TRUE, dot = FALSE, strict = FALSE, "
    method = c(\"left.kept\", \"both.sides\"), named = TRUE)  "
abs : function (x)  "
acos : function (x)  "
acosh : function (x)  "
activeBindingFunction : function (sym, env)  "
addNA : function (x, ifany = FALSE)  "
addTaskCallback : function (f, data = NULL, name = character())  "
agrep : function (pattern, x, max.distance = 0.1, costs = NULL, ignore.case = FALSE, value = FALSE, "
    fixed = TRUE, useBytes = FALSE)  "
agrepl : function (pattern, x, max.distance = 0.1, costs = NULL, ignore.case = FALSE, fixed = TRUE, "
    useBytes = FALSE)  "
alist : function (...)  "
all : function (..., na.rm = FALSE)  "
all.equal : function (target, current, ...)  "
all.equal.character : function (target, current, ..., check.attributes = TRUE)  "
all.equal.default : function (target, current, ...)  "
all.equal.environment : function (target, current, all.names = TRUE, evaluate = TRUE, ...)  "
all.equal.envRefClass : function (target, current, ...)  "
all.equal.factor : function (target, current, ..., check.attributes = TRUE)  "
all.equal.formula : function (target, current, ...)  "
all.equal.function : function (target, current, check.environment = TRUE, ...)  "
all.equal.language : function (target, current, ...)  "
all.equal.list : function (target, current, ..., check.attributes = TRUE, use.names = TRUE)  "
all.equal.numeric : function (target, current, tolerance = sqrt(.Machine$double.eps), scale = NULL, countEQ = FALSE, "
    formatFUN = function(err, what) format(err), ..., check.attributes = TRUE)  "
all.equal.POSIXt : function (target, current, ..., tolerance = 0.001, scale, check.tzone = TRUE)  "
all.equal.raw : function (target, current, ..., check.attributes = TRUE)  "
all.names : function (expr, functions = TRUE, max.names = -1L, unique = FALSE)  "
all.vars : function (expr, functions = FALSE, max.names = -1L, unique = TRUE)  "
allowInterrupts : function (expr)  "
any : function (..., na.rm = FALSE)  "
anyDuplicated : function (x, incomparables = FALSE, ...)  "
anyDuplicated.array : function (x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, ...)  "
anyDuplicated.data.frame : function (x, incomparables = FALSE, fromLast = FALSE, ...)  "
anyDuplicated.default : function (x, incomparables = FALSE, fromLast = FALSE, ...)  "
anyDuplicated.matrix : function (x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, ...)  "
anyNA : function (x, recursive = FALSE)  "
anyNA.data.frame : function (x, recursive = FALSE)  "
anyNA.numeric_version : function (x, recursive = FALSE)  "
anyNA.POSIXlt : function (x, recursive = FALSE)  "
aperm : function (a, perm, ...)  "
aperm.default : function (a, perm = NULL, resize = TRUE, ...)  "
aperm.table : function (a, perm = NULL, resize = TRUE, keep.class = TRUE, ...)  "
append : function (x, values, after = length(x))  "
apply : function (X, MARGIN, FUN, ..., simplify = TRUE)  "
Arg : function (z)  "
args : function (name)  "
array : function (data = NA, dim = length(data), dimnames = NULL)  "
arrayInd : function (ind, .dim, .dimnames = NULL, useNames = FALSE)  "
as.array : function (x, ...)  "
as.array.default : function (x, ...)  "
as.call : function (x)  "
as.character : function (x, ...)  "
as.character.condition : function (x, ...)  "
as.character.Date : function (x, ...)  "
as.character.default : function (x, ...)  "
as.character.error : function (x, ...)  "
as.character.factor : function (x, ...)  "
as.character.hexmode : function (x, ...)  "
as.character.numeric_version : function (x, ...)  "
as.character.octmode : function (x, ...)  "
as.character.POSIXt : function (x, ...)  "
as.character.srcref : function (x, useSource = TRUE, to = x, ...)  "
as.complex : function (x, ...)  "
as.data.frame : function (x, row.names = NULL, optional = FALSE, ...)  "
as.data.frame.array : function (x, row.names = NULL, optional = FALSE, ...)  "
as.data.frame.AsIs : function (x, row.names = NULL, optional = FALSE, ...)  "
as.data.frame.character : function (x, ..., stringsAsFactors = FALSE)  "
as.data.frame.complex : function (x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x)))  "
as.data.frame.data.frame : function (x, row.names = NULL, ...)  "
as.data.frame.Date : function (x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x)))  "
as.data.frame.default : function (x, ...)  "
as.data.frame.difftime : function (x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x)))  "
as.data.frame.factor : function (x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x)))  "
as.data.frame.integer : function (x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x)))  "
as.data.frame.list : function (x, row.names = NULL, optional = FALSE, ..., cut.names = FALSE, col.names = names(x), "
    fix.empty.names = TRUE, check.names = !optional, stringsAsFactors = FALSE)  "
as.data.frame.logical : function (x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x)))  "
as.data.frame.matrix : function (x, row.names = NULL, optional = FALSE, make.names = TRUE, ..., stringsAsFactors = FALSE)  "
as.data.frame.model.matrix : function (x, row.names = NULL, optional = FALSE, make.names = TRUE, ...)  "
as.data.frame.noquote : function (x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x)))  "
as.data.frame.numeric : function (x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x)))  "
as.data.frame.numeric_version : function (x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x)))  "
as.data.frame.ordered : function (x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x)))  "
as.data.frame.POSIXct : function (x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x)))  "
as.data.frame.POSIXlt : function (x, row.names = NULL, optional = FALSE, ...)  "
as.data.frame.raw : function (x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x)))  "
as.data.frame.table : function (x, row.names = NULL, ..., responseName = \"Freq\", stringsAsFactors = TRUE, "
    sep = \"\", base = list(LETTERS))  "
as.data.frame.ts : function (x, ...)  "
as.data.frame.vector : function (x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x)))  "
as.Date : function (x, ...)  "
as.Date.character : function (x, format, tryFormats = c(\"%Y-%m-%d\", \"%Y/%m/%d\"), optional = FALSE, ...)  "
as.Date.default : function (x, ...)  "
as.Date.factor : function (x, ...)  "
as.Date.numeric : function (x, origin, ...)  "
as.Date.POSIXct : function (x, tz = \"UTC\", ...)  "
as.Date.POSIXlt : function (x, ...)  "
as.difftime : function (tim, format = \"%X\", units = \"auto\", tz = \"UTC\")  "
as.double : function (x, ...)  "
as.double.difftime : function (x, units = \"auto\", ...)  "
as.double.POSIXlt : function (x, ...)  "
as.environment : function (x)  "
as.expression : function (x, ...)  "
as.expression.default : function (x, ...)  "
as.factor : function (x)  "
as.function : function (x, ...)  "
as.function.default : function (x, envir = parent.frame(), ...)  "
as.hexmode : function (x)  "
as.integer : function (x, ...)  "
as.list : function (x, ...)  "
as.list.data.frame : function (x, ...)  "
as.list.Date : function (x, ...)  "
as.list.default : function (x, ...)  "
as.list.difftime : function (x, ...)  "
as.list.environment : function (x, all.names = FALSE, sorted = FALSE, ...)  "
as.list.factor : function (x, ...)  "
as.list.function : function (x, ...)  "
as.list.numeric_version : function (x, ...)  "
as.list.POSIXct : function (x, ...)  "
as.list.POSIXlt : function (x, ...)  "
as.logical : function (x, ...)  "
as.logical.factor : function (x, ...)  "
as.matrix : function (x, ...)  "
as.matrix.data.frame : function (x, rownames.force = NA, ...)  "
as.matrix.default : function (x, ...)  "
as.matrix.noquote : function (x, ...)  "
as.matrix.POSIXlt : function (x, ...)  "
as.name : function (x)  "
as.null : function (x, ...)  "
as.null.default : function (x, ...)  "
as.numeric : function (x, ...)  "
as.numeric_version : function (x)  "
as.octmode : function (x)  "
as.ordered : function (x)  "
as.package_version : function (x)  "
as.pairlist : function (x)  "
as.POSIXct : function (x, tz = \"\", ...)  "
as.POSIXct.Date : function (x, ...)  "
as.POSIXct.default : function (x, tz = \"\", ...)  "
as.POSIXct.numeric : function (x, tz = \"\", origin, ...)  "
as.POSIXct.POSIXlt : function (x, tz = \"\", ...)  "
as.POSIXlt : function (x, tz = \"\", ...)  "
as.POSIXlt.character : function (x, tz = \"\", format, tryFormats = c(\"%Y-%m-%d %H:%M:%OS\", \"%Y/%m/%d %H:%M:%OS\", "
    \"%Y-%m-%d %H:%M\", \"%Y/%m/%d %H:%M\", \"%Y-%m-%d\", \"%Y/%m/%d\"), optional = FALSE, "
    ...)  "
as.POSIXlt.Date : function (x, ...)  "
as.POSIXlt.default : function (x, tz = \"\", optional = FALSE, ...)  "
as.POSIXlt.factor : function (x, ...)  "
as.POSIXlt.numeric : function (x, tz = \"\", origin, ...)  "
as.POSIXlt.POSIXct : function (x, tz = \"\", ...)  "
as.qr : function (x)  "
as.raw : function (x)  "
as.single : function (x, ...)  "
as.single.default : function (x, ...)  "
as.symbol : function (x)  "
as.table : function (x, ...)  "
as.table.default : function (x, ...)  "
as.vector : function (x, mode = \"any\")  "
as.vector.data.frame : function (x, mode = \"any\")  "
as.vector.factor : function (x, mode = \"any\")  "
as.vector.POSIXlt : function (x, mode = \"any\")  "
asin : function (x)  "
asinh : function (x)  "
asNamespace : function (ns, base.OK = TRUE)  "
asplit : function (x, MARGIN)  "
asS3 : function (object, flag = TRUE, complete = TRUE)  "
asS4 : function (object, flag = TRUE, complete = TRUE)  "
assign : function (x, value, pos = -1, envir = as.environment(pos), inherits = FALSE, immediate = TRUE)  "
atan : function (x)  "
atan2 : function (y, x)  "
atanh : function (x)  "
attach : function (what, pos = 2L, name = deparse1(substitute(what), backtick = FALSE), warn.conflicts = TRUE)  "
attachNamespace : function (ns, pos = 2L, depends = NULL, exclude, include.only)  "
attr : function (x, which, exact = FALSE)  "
attr.all.equal : function (target, current, ..., check.attributes = TRUE, check.names = TRUE)  "
attr<- : function (x, which, value)  "
attributes : function (x)  "
attributes<- : function (x, value)  "
autoload : function (name, package, reset = FALSE, ...)  "
autoloader : function (name, package, ...)  "
backsolve : function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)  "
baseenv : function ()  "
basename : function (path)  "
besselI : function (x, nu, expon.scaled = FALSE)  "
besselJ : function (x, nu)  "
besselK : function (x, nu, expon.scaled = FALSE)  "
besselY : function (x, nu)  "
beta : function (a, b)  "
bindingIsActive : function (sym, env)  "
bindingIsLocked : function (sym, env)  "
bindtextdomain : function (domain, dirname = NULL)  "
bitwAnd : function (a, b)  "
bitwNot : function (a)  "
bitwOr : function (a, b)  "
bitwShiftL : function (a, n)  "
bitwShiftR : function (a, n)  "
bitwXor : function (a, b)  "
body : function (fun = sys.function(sys.parent()))  "
body<- : function (fun, envir = environment(fun), value)  "
bquote : function (expr, where = parent.frame(), splice = FALSE)  "
break : .Primitive(\"break\") "
browser : function (text = \"\", condition = NULL, expr = TRUE, skipCalls = 0L)  "
browserCondition : function (n = 1L)  "
browserSetDebug : function (n = 1L)  "
browserText : function (n = 1L)  "
builtins : function (internal = FALSE)  "
by : function (data, INDICES, FUN, ..., simplify = TRUE)  "
by.data.frame : function (data, INDICES, FUN, ..., simplify = TRUE)  "
by.default : function (data, INDICES, FUN, ..., simplify = TRUE)  "
bzfile : function (description, open = \"\", encoding = getOption(\"encoding\"), compression = 9)  "
c : function (...)  "
c.Date : function (..., recursive = FALSE)  "
c.difftime : function (..., recursive = FALSE)  "
c.factor : function (..., recursive = TRUE)  "
c.noquote : function (..., recursive = FALSE)  "
c.numeric_version : function (..., recursive = FALSE)  "
c.POSIXct : function (..., recursive = FALSE)  "
c.POSIXlt : function (..., recursive = FALSE)  "
c.warnings : function (..., recursive = FALSE)  "
call : function (name, ...)  "
callCC : function (fun)  "
capabilities : function (what = NULL, Xchk = any(nas %in% c(\"X11\", \"jpeg\", \"png\", \"tiff\")))  "
casefold : function (x, upper = FALSE)  "
cat : function (..., file = \"\", sep = \" \", fill = FALSE, labels = NULL, append = FALSE)  "
cbind : function (..., deparse.level = 1)  "
cbind.data.frame : function (..., deparse.level = 1)  "
ceiling : function (x)  "
char.expand : function (input, target, nomatch = stop(\"no match\"))  "
character : function (length = 0L)  "
charmatch : function (x, table, nomatch = NA_integer_)  "
charToRaw : function (x)  "
chartr : function (old, new, x)  "
check_tzones : function (...)  "
chkDots : function (..., which.call = -1, allowed = character(0))  "
chol : function (x, ...)  "
chol.default : function (x, pivot = FALSE, LINPACK = FALSE, tol = -1, ...)  "
chol2inv : function (x, size = NCOL(x), LINPACK = FALSE)  "
choose : function (n, k)  "
class : function (x)  "
class<- : function (x, value)  "
clearPushBack : function (connection)  "
close : function (con, ...)  "
close.connection : function (con, type = \"rw\", ...)  "
close.srcfile : function (con, ...)  "
close.srcfilealias : function (con, ...)  "
closeAllConnections : function ()  "
col : function (x, as.factor = FALSE)  "
colMeans : function (x, na.rm = FALSE, dims = 1L)  "
colnames : function (x, do.NULL = TRUE, prefix = \"col\")  "
colnames<- : function (x, value)  "
colSums : function (x, na.rm = FALSE, dims = 1L)  "
commandArgs : function (trailingOnly = FALSE)  "
comment : function (x)  "
comment<- : function (x, value)  "
complex : function (length.out = 0L, real = numeric(), imaginary = numeric(), modulus = 1, argument = 0)  "
computeRestarts : function (cond = NULL)  "
conditionCall : function (c)  "
conditionCall.condition : function (c)  "
conditionMessage : function (c)  "
conditionMessage.condition : function (c)  "
conflictRules : function (pkg, mask.ok = NULL, exclude = NULL)  "
conflicts : function (where = search(), detail = FALSE)  "
Conj : function (z)  "
contributors : function ()  "
cos : function (x)  "
cosh : function (x)  "
cospi : function (x)  "
crossprod : function (x, y = NULL)  "
Cstack_info : function ()  "
cummax : function (x)  "
cummin : function (x)  "
cumprod : function (x)  "
cumsum : function (x)  "
curlGetHeaders : function (url, redirect = TRUE, verify = TRUE, timeout = 0L, TLS = \"\")  "
cut : function (x, ...)  "
cut.Date : function (x, breaks, labels = NULL, start.on.monday = TRUE, right = FALSE, ...)  "
cut.default : function (x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE, dig.lab = 3L, "
    ordered_result = FALSE, ...)  "
cut.POSIXt : function (x, breaks, labels = NULL, start.on.monday = TRUE, right = FALSE, ...)  "
data.class : function (x)  "
data.frame : function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, "
    stringsAsFactors = FALSE)  "
data.matrix : function (frame, rownames.force = NA)  "
date : function ()  "
debug : function (fun, text = \"\", condition = NULL, signature = NULL)  "
debuggingState : function (on = NULL)  "
debugonce : function (fun, text = \"\", condition = NULL, signature = NULL)  "
default.stringsAsFactors : function ()  "
delayedAssign : function (x, value, eval.env = parent.frame(1), assign.env = parent.frame(1))  "
deparse : function (expr, width.cutoff = 60L, backtick = mode(expr) %in% c(\"call\", \"expression\", "
    \"(\", \"function\"), control = c(\"keepNA\", \"keepInteger\", \"niceNames\", \"showAttributes\"), "
    nlines = -1L)  "
deparse1 : function (expr, collapse = \" \", width.cutoff = 500L, ...)  "
det : function (x, ...)  "
detach : function (name, pos = 2L, unload = FALSE, character.only = FALSE, force = FALSE)  "
determinant : function (x, logarithm = TRUE, ...)  "
determinant.matrix : function (x, logarithm = TRUE, ...)  "
dget : function (file, keep.source = FALSE)  "
diag : function (x = 1, nrow, ncol, names = TRUE)  "
diag<- : function (x, value)  "
diff : function (x, ...)  "
diff.Date : function (x, lag = 1L, differences = 1L, ...)  "
diff.default : function (x, lag = 1L, differences = 1L, ...)  "
diff.difftime : function (x, ...)  "
diff.POSIXt : function (x, lag = 1L, differences = 1L, ...)  "
difftime : function (time1, time2, tz, units = c(\"auto\", \"secs\", \"mins\", \"hours\", \"days\", \"weeks\"))  "
digamma : function (x)  "
dim : function (x)  "
dim.data.frame : function (x)  "
dim<- : function (x, value)  "
dimnames : function (x)  "
dimnames.data.frame : function (x)  "
dimnames<- : function (x, value)  "
dimnames<-.data.frame : function (x, value)  "
dir : function (path = \".\", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, "
    ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)  "
dir.create : function (path, showWarnings = TRUE, recursive = FALSE, mode = \"0777\")  "
dir.exists : function (paths)  "
dirname : function (path)  "
do.call : function (what, args, quote = FALSE, envir = parent.frame())  "
dontCheck : function (x)  "
double : function (length = 0L)  "
dput : function (x, file = \"\", control = c(\"keepNA\", \"keepInteger\", \"niceNames\", \"showAttributes\"))  "
dQuote : function (x, q = getOption(\"useFancyQuotes\"))  "
drop : function (x)  "
droplevels : function (x, ...)  "
droplevels.data.frame : function (x, except = NULL, exclude, ...)  "
droplevels.factor : function (x, exclude = if (anyNA(levels(x))) NULL else NA, ...)  "
dump : function (list, file = \"dumpdata.R\", append = FALSE, control = \"all\", envir = parent.frame(), "
    evaluate = TRUE)  "
duplicated : function (x, incomparables = FALSE, ...)  "
duplicated.array : function (x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, ...)  "
duplicated.data.frame : function (x, incomparables = FALSE, fromLast = FALSE, ...)  "
duplicated.default : function (x, incomparables = FALSE, fromLast = FALSE, nmax = NA, ...)  "
duplicated.matrix : function (x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, ...)  "
duplicated.numeric_version : function (x, incomparables = FALSE, ...)  "
duplicated.POSIXlt : function (x, incomparables = FALSE, ...)  "
duplicated.warnings : function (x, incomparables = FALSE, ...)  "
dyn.load : function (x, local = TRUE, now = TRUE, ...)  "
dyn.unload : function (x)  "
dynGet : function (x, ifnotfound = stop(gettextf(\"%s not found\", sQuote(x)), domain = NA), "
    minframe = 1L, inherits = FALSE)  "
eapply : function (env, FUN, ..., all.names = FALSE, USE.NAMES = TRUE)  "
eigen : function (x, symmetric, only.values = FALSE, EISPACK = FALSE)  "
emptyenv : function ()  "
enc2native : function (x)  "
enc2utf8 : function (x)  "
encodeString : function (x, width = 0L, quote = \"\", na.encode = TRUE, justify = c(\"left\", \"right\", "
    \"centre\", \"none\"))  "
Encoding : function (x)  "
Encoding<- : function (x, value)  "
endsWith : function (x, suffix)  "
enquote : function (cl)  "
env.profile : function (env)  "
environment : function (fun = NULL)  "
environment<- : function (fun, value)  "
environmentIsLocked : function (env)  "
environmentName : function (env)  "
errorCondition : function (message, ..., class = NULL, call = NULL)  "
eval : function (expr, envir = parent.frame(), enclos = if (is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv())  "
eval.parent : function (expr, n = 1)  "
evalq : function (expr, envir = parent.frame(), enclos = if (is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv())  "
exists : function (x, where = -1, envir = if (missing(frame)) as.environment(where) else sys.frame(frame), "
    frame, mode = \"any\", inherits = TRUE)  "
exp : function (x)  "
expand.grid : function (..., KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE)  "
expm1 : function (x)  "
expression : function (...)  "
extSoftVersion : function ()  "
F :  logi FALSE"
factor : function (x = character(), levels, labels = levels, exclude = NA, ordered = is.ordered(x), "
    nmax = NA)  "
factorial : function (x)  "
fifo : function (description, open = \"\", blocking = FALSE, encoding = getOption(\"encoding\"))  "
file : function (description = \"\", open = \"\", blocking = TRUE, encoding = getOption(\"encoding\"), "
    raw = FALSE, method = getOption(\"url.method\", \"default\"))  "
file.access : function (names, mode = 0)  "
file.append : function (file1, file2)  "
file.choose : function (new = FALSE)  "
file.copy : function (from, to, overwrite = recursive, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)  "
file.create : function (..., showWarnings = TRUE)  "
file.exists : function (...)  "
file.info : function (..., extra_cols = TRUE)  "
file.link : function (from, to)  "
file.mode : function (...)  "
file.mtime : function (...)  "
file.path : function (..., fsep = .Platform$file.sep)  "
file.remove : function (...)  "
file.rename : function (from, to)  "
file.show : function (..., header = rep(\"\", nfiles), title = \"R Information\", delete.file = FALSE, "
    pager = getOption(\"pager\"), encoding = \"\")  "
file.size : function (...)  "
file.symlink : function (from, to)  "
Filter : function (f, x)  "
Find : function (f, x, right = FALSE, nomatch = NULL)  "
find.package : function (package = NULL, lib.loc = NULL, quiet = FALSE, verbose = getOption(\"verbose\"))  "
findInterval : function (x, vec, rightmost.closed = FALSE, all.inside = FALSE, left.open = FALSE)  "
findPackageEnv : function (info)  "
findRestart : function (name, cond = NULL)  "
floor : function (x)  "
flush : function (con)  "
flush.connection : function (con)  "
for : .Primitive(\"for\") "
force : function (x)  "
forceAndCall : function (n, FUN, ...)  "
formals : function (fun = sys.function(sys.parent()), envir = parent.frame())  "
formals<- : function (fun, envir = environment(fun), value)  "
format : function (x, ...)  "
format.AsIs : function (x, width = 12, ...)  "
format.data.frame : function (x, ..., justify = \"none\")  "
format.Date : function (x, ...)  "
format.default : function (x, trim = FALSE, digits = NULL, nsmall = 0L, justify = c(\"left\", \"right\", "
    \"centre\", \"none\"), width = NULL, na.encode = TRUE, scientific = NA, big.mark = \"\", "
    big.interval = 3L, small.mark = \"\", small.interval = 5L, decimal.mark = getOption(\"OutDec\"), "
    zero.print = NULL, drop0trailing = FALSE, ...)  "
format.difftime : function (x, ...)  "
format.factor : function (x, ...)  "
format.hexmode : function (x, width = NULL, upper.case = FALSE, ...)  "
format.info : function (x, digits = NULL, nsmall = 0L)  "
format.libraryIQR : function (x, ...)  "
format.numeric_version : function (x, ...)  "
format.octmode : function (x, width = NULL, ...)  "
format.packageInfo : function (x, ...)  "
format.POSIXct : function (x, format = \"\", tz = \"\", usetz = FALSE, ...)  "
format.POSIXlt : function (x, format = \"\", usetz = FALSE, digits = getOption(\"digits.secs\"), ...)  "
format.pval : function (pv, digits = max(1L, getOption(\"digits\") - 2L), eps = .Machine$double.eps, "
    na.form = \"NA\", ...)  "
format.summaryDefault : function (x, digits = max(3L, getOption(\"digits\") - 3L), ...)  "
formatC : function (x, digits = NULL, width = NULL, format = NULL, flag = \"\", mode = NULL, big.mark = \"\", "
    big.interval = 3L, small.mark = \"\", small.interval = 5L, decimal.mark = getOption(\"OutDec\"), "
    preserve.width = \"individual\", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE)  "
formatDL : function (x, y, style = c(\"table\", \"list\"), width = 0.9 * getOption(\"width\"), indent = NULL)  "
forwardsolve : function (l, x, k = ncol(l), upper.tri = FALSE, transpose = FALSE)  "
function : .Primitive(\"function\") "
gamma : function (x)  "
gc : function (verbose = getOption(\"verbose\"), reset = FALSE, full = TRUE)  "
gc.time : function (on = TRUE)  "
gcinfo : function (verbose)  "
gctorture : function (on = TRUE)  "
gctorture2 : function (step, wait = step, inhibit_release = FALSE)  "
get : function (x, pos = -1L, envir = as.environment(pos), mode = \"any\", inherits = TRUE)  "
get0 : function (x, envir = pos.to.env(-1L), mode = \"any\", inherits = TRUE, ifnotfound = NULL)  "
getAllConnections : function ()  "
getCallingDLL : function (f = sys.function(-1), doStop = FALSE)  "
getCallingDLLe : function (e)  "
getConnection : function (what)  "
getDLLRegisteredRoutines : function (dll, addNames = TRUE)  "
getDLLRegisteredRoutines.character : function (dll, addNames = TRUE)  "
getDLLRegisteredRoutines.DLLInfo : function (dll, addNames = TRUE)  "
getElement : function (object, name)  "
geterrmessage : function ()  "
getExportedValue : function (ns, name)  "
getHook : function (hookName)  "
getLoadedDLLs : function ()  "
getNamespace : function (name)  "
getNamespaceExports : function (ns)  "
getNamespaceImports : function (ns)  "
getNamespaceInfo : function (ns, which)  "
getNamespaceName : function (ns)  "
getNamespaceUsers : function (ns)  "
getNamespaceVersion : function (ns)  "
getNativeSymbolInfo : function (name, PACKAGE, unlist = TRUE, withRegistrationInfo = FALSE)  "
getOption : function (x, default = NULL)  "
getRversion : function ()  "
getSrcLines : function (srcfile, first, last)  "
getTaskCallbackNames : function ()  "
gettext : function (..., domain = NULL, trim = TRUE)  "
gettextf : function (fmt, ..., domain = NULL, trim = TRUE)  "
getwd : function ()  "
gl : function (n, k, length = n * k, labels = seq_len(n), ordered = FALSE)  "
globalCallingHandlers : function (...)  "
globalenv : function ()  "
gregexec : function (pattern, text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)  "
gregexpr : function (pattern, text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)  "
grep : function (pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = FALSE, "
    useBytes = FALSE, invert = FALSE)  "
grepl : function (pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)  "
grepRaw : function (pattern, x, offset = 1L, ignore.case = FALSE, value = FALSE, fixed = FALSE, "
    all = FALSE, invert = FALSE)  "
grouping : function (...)  "
gsub : function (pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, "
    useBytes = FALSE)  "
gzcon : function (con, level = 6, allowNonCompressed = TRUE, text = FALSE)  "
gzfile : function (description, open = \"\", encoding = getOption(\"encoding\"), compression = 6)  "
I : function (x)  "
iconv : function (x, from = \"\", to = \"\", sub = NA, mark = TRUE, toRaw = FALSE)  "
iconvlist : function ()  "
icuGetCollate : function (type = c(\"actual\", \"valid\"))  "
icuSetCollate : function (...)  "
identical : function (x, y, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, "
    ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)  "
identity : function (x)  "
if : .Primitive(\"if\") "
ifelse : function (test, yes, no)  "
Im : function (z)  "
importIntoEnv : function (impenv, impnames, expenv, expnames)  "
infoRDS : function (file)  "
inherits : function (x, what, which = FALSE)  "
integer : function (length = 0L)  "
interaction : function (..., drop = FALSE, sep = \".\", lex.order = FALSE)  "
interactive : function ()  "
intersect : function (x, y)  "
intToBits : function (x)  "
intToUtf8 : function (x, multiple = FALSE, allow_surrogate_pairs = FALSE)  "
inverse.rle : function (x, ...)  "
invisible : function (x)  "
invokeRestart : function (r, ...)  "
invokeRestartInteractively : function (r)  "
is.array : function (x)  "
is.atomic : function (x)  "
is.call : function (x)  "
is.character : function (x)  "
is.complex : function (x)  "
is.data.frame : function (x)  "
is.double : function (x)  "
is.element : function (el, set)  "
is.environment : function (x)  "
is.expression : function (x)  "
is.factor : function (x)  "
is.finite : function (x)  "
is.function : function (x)  "
is.infinite : function (x)  "
is.integer : function (x)  "
is.language : function (x)  "
is.list : function (x)  "
is.loaded : function (symbol, PACKAGE = \"\", type = \"\")  "
is.logical : function (x)  "
is.matrix : function (x)  "
is.na : function (x)  "
is.na.data.frame : function (x)  "
is.na.numeric_version : function (x)  "
is.na.POSIXlt : function (x)  "
is.na<- : function (x, value)  "
is.na<-.default : function (x, value)  "
is.na<-.factor : function (x, value)  "
is.na<-.numeric_version : function (x, value)  "
is.name : function (x)  "
is.nan : function (x)  "
is.null : function (x)  "
is.numeric : function (x)  "
is.numeric.Date : function (x)  "
is.numeric.difftime : function (x)  "
is.numeric.POSIXt : function (x)  "
is.numeric_version : function (x)  "
is.object : function (x)  "
is.ordered : function (x)  "
is.package_version : function (x)  "
is.pairlist : function (x)  "
is.primitive : function (x)  "
is.qr : function (x)  "
is.R : function ()  "
is.raw : function (x)  "
is.recursive : function (x)  "
is.single : function (x)  "
is.symbol : function (x)  "
is.table : function (x)  "
is.unsorted : function (x, na.rm = FALSE, strictly = FALSE)  "
is.vector : function (x, mode = \"any\")  "
isa : function (x, what)  "
isatty : function (con)  "
isBaseNamespace : function (ns)  "
isdebugged : function (fun, signature = NULL)  "
isFALSE : function (x)  "
isIncomplete : function (con)  "
isNamespace : function (ns)  "
isNamespaceLoaded : function (name)  "
ISOdate : function (year, month, day, hour = 12, min = 0, sec = 0, tz = \"GMT\")  "
ISOdatetime : function (year, month, day, hour, min, sec, tz = \"\")  "
isOpen : function (con, rw = \"\")  "
isRestart : function (x)  "
isS4 : function (object)  "
isSeekable : function (con)  "
isSymmetric : function (object, ...)  "
isSymmetric.matrix : function (object, tol = 100 * .Machine$double.eps, tol1 = 8 * tol, ...)  "
isTRUE : function (x)  "
jitter : function (x, factor = 1, amount = NULL)  "
julian : function (x, ...)  "
julian.Date : function (x, origin = as.Date(\"1970-01-01\"), ...)  "
julian.POSIXt : function (x, origin = as.POSIXct(\"1970-01-01\", tz = \"GMT\"), ...)  "
kappa : function (z, ...)  "
kappa.default : function (z, exact = FALSE, norm = NULL, method = c(\"qr\", \"direct\"), ...)  "
kappa.lm : function (z, ...)  "
kappa.qr : function (z, ...)  "
kronecker : function (X, Y, FUN = \"*\", make.dimnames = FALSE, ...)  "
l10n_info : function ()  "
La.svd : function (x, nu = min(n, p), nv = min(n, p))  "
La_library : function ()  "
La_version : function ()  "
labels : function (object, ...)  "
labels.default : function (object, ...)  "
lapply : function (X, FUN, ...)  "
lazyLoad : function (filebase, envir = parent.frame(), filter)  "
lazyLoadDBexec : function (filebase, fun, filter)  "
lazyLoadDBfetch : function (key, file, compressed, hook)  "
lbeta : function (a, b)  "
lchoose : function (n, k)  "
length : function (x)  "
length.POSIXlt : function (x)  "
length<- : function (x, value)  "
length<-.Date : function (x, value)  "
length<-.difftime : function (x, value)  "
length<-.factor : function (x, value)  "
length<-.POSIXct : function (x, value)  "
length<-.POSIXlt : function (x, value)  "
lengths : function (x, use.names = TRUE)  "
letters :  chr [1:26] \"a\" \"b\" \"c\" \"d\" \"e\" \"f\" \"g\" \"h\" \"i\" \"j\" \"k\" \"l\" \"m\" \"n\" \"o\" \"p\" \"q\" \"r\" ..."
LETTERS :  chr [1:26] \"A\" \"B\" \"C\" \"D\" \"E\" \"F\" \"G\" \"H\" \"I\" \"J\" \"K\" \"L\" \"M\" \"N\" \"O\" \"P\" \"Q\" \"R\" ..."
levels : function (x)  "
levels.default : function (x)  "
levels<- : function (x, value)  "
levels<-.factor : function (x, value)  "
lfactorial : function (x)  "
lgamma : function (x)  "
libcurlVersion : function ()  "
library : function (package, help, pos = 2, lib.loc = NULL, character.only = FALSE, logical.return = FALSE, "
    warn.conflicts, quietly = FALSE, verbose = getOption(\"verbose\"), mask.ok, exclude, "
    include.only, attach.required = missing(include.only))  "
library.dynam : function (chname, package, lib.loc, verbose = getOption(\"verbose\"), file.ext = .Platform$dynlib.ext, "
    ...)  "
library.dynam.unload : function (chname, libpath, verbose = getOption(\"verbose\"), file.ext = .Platform$dynlib.ext)  "
licence : function ()  "
license : function ()  "
list : function (...)  "
list.dirs : function (path = \".\", full.names = TRUE, recursive = TRUE)  "
list.files : function (path = \".\", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, "
    ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)  "
list2DF : function (x = list(), nrow = 0L)  "
list2env : function (x, envir = NULL, parent = parent.frame(), hash = (length(x) > 100), size = max(29L, "
    length(x)))  "
load : function (file, envir = parent.frame(), verbose = FALSE)  "
loadedNamespaces : function ()  "
loadingNamespaceInfo : function ()  "
loadNamespace : function (package, lib.loc = NULL, keep.source = getOption(\"keep.source.pkgs\"), partial = FALSE, "
    versionCheck = NULL, keep.parse.data = getOption(\"keep.parse.data.pkgs\"))  "
local : function (expr, envir = new.env())  "
lockBinding : function (sym, env)  "
lockEnvironment : function (env, bindings = FALSE)  "
log : function (x, base = exp(1))  "
log10 : function (x)  "
log1p : function (x)  "
log2 : function (x)  "
logb : function (x, base = exp(1))  "
logical : function (length = 0L)  "
lower.tri : function (x, diag = FALSE)  "
ls : function (name, pos = -1L, envir = as.environment(pos), all.names = FALSE, pattern, "
    sorted = TRUE)  "
make.names : function (names, unique = FALSE, allow_ = TRUE)  "
make.unique : function (names, sep = \".\")  "
makeActiveBinding : function (sym, fun, env)  "
Map : function (f, ...)  "
mapply : function (FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)  "
margin.table : function (x, margin = NULL)  "
marginSums : function (x, margin = NULL)  "
mat.or.vec : function (nr, nc)  "
match : function (x, table, nomatch = NA_integer_, incomparables = NULL)  "
match.arg : function (arg, choices, several.ok = FALSE)  "
match.call : function (definition = sys.function(sys.parent()), call = sys.call(sys.parent()), "
    expand.dots = TRUE, envir = parent.frame(2L))  "
match.fun : function (FUN, descend = TRUE)  "
Math.data.frame : function (x, ...)  "
Math.Date : function (x, ...)  "
Math.difftime : function (x, ...)  "
Math.factor : function (x, ...)  "
Math.POSIXt : function (x, ...)  "
matrix : function (data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)  "
max : function (..., na.rm = FALSE)  "
max.col : function (m, ties.method = c(\"random\", \"first\", \"last\"))  "
mean : function (x, ...)  "
mean.Date : function (x, ...)  "
mean.default : function (x, trim = 0, na.rm = FALSE, ...)  "
mean.difftime : function (x, ...)  "
mean.POSIXct : function (x, ...)  "
mean.POSIXlt : function (x, ...)  "
mem.maxNSize : function (nsize = 0)  "
mem.maxVSize : function (vsize = 0)  "
memCompress : function (from, type = c(\"gzip\", \"bzip2\", \"xz\", \"none\"))  "
memDecompress : function (from, type = c(\"unknown\", \"gzip\", \"bzip2\", \"xz\", \"none\"), asChar = FALSE)  "
memory.profile : function ()  "
merge : function (x, y, ...)  "
merge.data.frame : function (x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by, all = FALSE, "
    all.x = all, all.y = all, sort = TRUE, suffixes = c(\".x\", \".y\"), no.dups = TRUE, "
    incomparables = NULL, ...)  "
merge.default : function (x, y, ...)  "
message : function (..., domain = NULL, appendLF = TRUE)  "
mget : function (x, envir = as.environment(-1L), mode = \"any\", ifnotfound, inherits = FALSE)  "
min : function (..., na.rm = FALSE)  "
missing : function (x)  "
Mod : function (z)  "
mode : function (x)  "
mode<- : function (x, value)  "
month.abb :  chr [1:12] \"Jan\" \"Feb\" \"Mar\" \"Apr\" \"May\" \"Jun\" \"Jul\" \"Aug\" \"Sep\" \"Oct\" \"Nov\" \"Dec\""
month.name :  chr [1:12] \"January\" \"February\" \"March\" \"April\" \"May\" \"June\" \"July\" \"August\" ..."
months : function (x, abbreviate)  "
months.Date : function (x, abbreviate = FALSE)  "
months.POSIXt : function (x, abbreviate = FALSE)  "
mostattributes<- : function (x, value)  "
mtfrm : function (x)  "
mtfrm.default : function (x)  "
names : function (x)  "
names.POSIXlt : function (x)  "
names<- : function (x, value)  "
names<-.POSIXlt : function (x, value)  "
namespaceExport : function (ns, vars)  "
namespaceImport : function (self, ..., from = NULL, except = character(0L))  "
namespaceImportClasses : function (self, ns, vars, from = NULL)  "
namespaceImportFrom : function (self, ns, vars, generics, packages, from = \"non-package environment\", except = character(0L))  "
namespaceImportMethods : function (self, ns, vars, from = NULL)  "
nargs : function ()  "
nchar : function (x, type = \"chars\", allowNA = FALSE, keepNA = NA)  "
ncol : function (x)  "
NCOL : function (x)  "
Negate : function (f)  "
new.env : function (hash = TRUE, parent = parent.frame(), size = 29L)  "
next : .Primitive(\"next\") "
NextMethod : function (generic = NULL, object = NULL, ...)  "
ngettext : function (n, msg1, msg2, domain = NULL)  "
nlevels : function (x)  "
noquote : function (obj, right = FALSE)  "
norm : function (x, type = c(\"O\", \"I\", \"F\", \"M\", \"2\"))  "
normalizePath : function (path, winslash = \"\\\", mustWork = NA)  "
nrow : function (x)  "
NROW : function (x)  "
nullfile : function ()  "
numeric : function (length = 0L)  "
numeric_version : function (x, strict = TRUE)  "
numToBits : function (x)  "
numToInts : function (x)  "
nzchar : function (x, keepNA = FALSE)  "
objects : function (name, pos = -1L, envir = as.environment(pos), all.names = FALSE, pattern, "
    sorted = TRUE)  "
oldClass : function (x)  "
oldClass<- : function (x, value)  "
OlsonNames : function (tzdir = NULL)  "
on.exit : function (expr = NULL, add = FALSE, after = TRUE)  "
open : function (con, ...)  "
open.connection : function (con, open = \"r\", blocking = TRUE, ...)  "
open.srcfile : function (con, line, ...)  "
open.srcfilealias : function (con, line, ...)  "
open.srcfilecopy : function (con, line, ...)  "
Ops.data.frame : function (e1, e2 = NULL)  "
Ops.Date : function (e1, e2)  "
Ops.difftime : function (e1, e2)  "
Ops.factor : function (e1, e2)  "
Ops.numeric_version : function (e1, e2)  "
Ops.ordered : function (e1, e2)  "
Ops.POSIXt : function (e1, e2)  "
options : function (...)  "
order : function (..., na.last = TRUE, decreasing = FALSE, method = c(\"auto\", \"shell\", \"radix\"))  "
ordered : function (x, ...)  "
outer : function (X, Y, FUN = \"*\", ...)  "
package_version : function (x, strict = TRUE)  "
packageEvent : function (pkgname, event = c(\"onLoad\", \"attach\", \"detach\", \"onUnload\"))  "
packageHasNamespace : function (package, package.lib)  "
packageNotFoundError : function (package, lib.loc, call = NULL)  "
packageStartupMessage : function (..., domain = NULL, appendLF = TRUE)  "
packBits : function (x, type = c(\"raw\", \"integer\", \"double\"))  "
pairlist : function (...)  "
parent.env : function (env)  "
parent.env<- : function (env, value)  "
parent.frame : function (n = 1)  "
parse : function (file = \"\", n = NULL, text = NULL, prompt = \"?\", keep.source = getOption(\"keep.source\"), "
    srcfile = NULL, encoding = \"unknown\")  "
parseNamespaceFile : function (package, package.lib, mustExist = TRUE)  "
paste : function (..., sep = \" \", collapse = NULL, recycle0 = FALSE)  "
paste0 : function (..., collapse = NULL, recycle0 = FALSE)  "
path.expand : function (path)  "
path.package : function (package = NULL, quiet = FALSE)  "
pcre_config : function ()  "
pi :  num 3.14"
pipe : function (description, open = \"\", encoding = getOption(\"encoding\"))  "
plot : function (x, y, ...)  "
pmatch : function (x, table, nomatch = NA_integer_, duplicates.ok = FALSE)  "
pmax : function (..., na.rm = FALSE)  "
pmax.int : function (..., na.rm = FALSE)  "
pmin : function (..., na.rm = FALSE)  "
pmin.int : function (..., na.rm = FALSE)  "
polyroot : function (z)  "
pos.to.env : function (x)  "
Position : function (f, x, right = FALSE, nomatch = NA_integer_)  "
pretty : function (x, ...)  "
pretty.default : function (x, n = 5L, min.n = n%/%3L, shrink.sml = 0.75, high.u.bias = 1.5, u5.bias = 0.5 + "
    1.5 * high.u.bias, eps.correct = 0L, f.min = 2^-20, ...)  "
prettyNum : function (x, big.mark = \"\", big.interval = 3L, small.mark = \"\", small.interval = 5L, "
    decimal.mark = getOption(\"OutDec\"), input.d.mark = decimal.mark, preserve.width = c(\"common\", "
        \"individual\", \"none\"), zero.print = NULL, replace.zero = FALSE, drop0trailing = FALSE, "
    is.cmplx = NA, ...)  "
print : function (x, ...)  "
print.AsIs : function (x, ...)  "
print.by : function (x, ..., vsep)  "
print.condition : function (x, ...)  "
print.connection : function (x, ...)  "
print.data.frame : function (x, ..., digits = NULL, quote = FALSE, right = TRUE, row.names = TRUE, max = NULL)  "
print.Date : function (x, max = NULL, ...)  "
print.default : function (x, digits = NULL, quote = TRUE, na.print = NULL, print.gap = NULL, right = FALSE, "
    max = NULL, width = NULL, useSource = TRUE, ...)  "
print.difftime : function (x, digits = getOption(\"digits\"), ...)  "
print.Dlist : function (x, ...)  "
print.DLLInfo : function (x, ...)  "
print.DLLInfoList : function (x, ...)  "
print.DLLRegisteredRoutines : function (x, ...)  "
print.eigen : function (x, ...)  "
print.factor : function (x, quote = FALSE, max.levels = NULL, width = getOption(\"width\"), ...)  "
print.function : function (x, useSource = TRUE, ...)  "
print.hexmode : function (x, ...)  "
print.libraryIQR : function (x, ...)  "
print.listof : function (x, ...)  "
print.NativeRoutineList : function (x, ...)  "
print.noquote : function (x, quote = FALSE, right = FALSE, ...)  "
print.numeric_version : function (x, quote = FALSE, ...)  "
print.octmode : function (x, ...)  "
print.packageInfo : function (x, ...)  "
print.POSIXct : function (x, tz = \"\", usetz = TRUE, max = NULL, ...)  "
print.POSIXlt : function (x, tz = \"\", usetz = TRUE, max = NULL, ...)  "
print.proc_time : function (x, ...)  "
print.restart : function (x, ...)  "
print.rle : function (x, digits = getOption(\"digits\"), prefix = \"\", ...)  "
print.simple.list : function (x, ...)  "
print.srcfile : function (x, ...)  "
print.srcref : function (x, useSource = TRUE, ...)  "
print.summary.table : function (x, digits = max(1L, getOption(\"digits\") - 3L), ...)  "
print.summary.warnings : function (x, ...)  "
print.summaryDefault : function (x, digits = max(3L, getOption(\"digits\") - 3L), ...)  "
print.table : function (x, digits = getOption(\"digits\"), quote = FALSE, na.print = \"\", zero.print = \"0\", "
    right = is.numeric(x) || is.complex(x), justify = \"none\", ...)  "
print.warnings : function (x, tags, header = ngettext(n, \"Warning message:\n\", \"Warning messages:\n\"), "
    ...)  "
prmatrix : function (x, rowlab = dn[[1]], collab = dn[[2]], quote = TRUE, right = FALSE, na.print = NULL, "
    ...)  "
proc.time : function ()  "
prod : function (..., na.rm = FALSE)  "
prop.table : function (x, margin = NULL)  "
proportions : function (x, margin = NULL)  "
provideDimnames : function (x, sep = \"\", base = list(LETTERS), unique = TRUE)  "
psigamma : function (x, deriv = 0L)  "
pushBack : function (data, connection, newLine = TRUE, encoding = c(\"\", \"bytes\", \"UTF-8\"))  "
pushBackLength : function (connection)  "
q : function (save = \"default\", status = 0, runLast = TRUE)  "
qr : function (x, ...)  "
qr.coef : function (qr, y)  "
qr.default : function (x, tol = 1e-07, LAPACK = FALSE, ...)  "
qr.fitted : function (qr, y, k = qr$rank)  "
qr.Q : function (qr, complete = FALSE, Dvec)  "
qr.qty : function (qr, y)  "
qr.qy : function (qr, y)  "
qr.R : function (qr, complete = FALSE)  "
qr.resid : function (qr, y)  "
qr.solve : function (a, b, tol = 1e-07)  "
qr.X : function (qr, complete = FALSE, ncol = if (complete) nrow(R) else min(dim(R)))  "
quarters : function (x, abbreviate)  "
quarters.Date : function (x, ...)  "
quarters.POSIXt : function (x, ...)  "
quit : function (save = \"default\", status = 0, runLast = TRUE)  "
quote : function (expr)  "
R.home : function (component = \"home\")  "
R.version : List of 15"
 $ platform      : chr \"x86_64-w64-mingw32\""
 $ arch          : chr \"x86_64\""
 $ os            : chr \"mingw32\""
 $ crt           : chr \"ucrt\""
 $ system        : chr \"x86_64, mingw32\""
 $ status        : chr \"\""
 $ major         : chr \"4\""
 $ minor         : chr \"2.1\""
 $ year          : chr \"2022\""
 $ month         : chr \"06\""
 $ day           : chr \"23\""
 $ svn rev       : chr \"82513\""
 $ language      : chr \"R\""
 $ version.string: chr \"R version 4.2.1 (2022-06-23 ucrt)\""
 $ nickname      : chr \"Funny-Looking Kid\""
R.Version : function ()  "
R.version.string :  chr \"R version 4.2.1 (2022-06-23 ucrt)\""
R_system_version : function (x, strict = TRUE)  "
range : function (..., na.rm = FALSE)  "
range.default : function (..., na.rm = FALSE, finite = FALSE)  "
rank : function (x, na.last = TRUE, ties.method = c(\"average\", \"first\", \"last\", \"random\", "
    \"max\", \"min\"))  "
rapply : function (object, f, classes = \"ANY\", deflt = NULL, how = c(\"unlist\", \"replace\", \"list\"), "
    ...)  "
raw : function (length = 0L)  "
rawConnection : function (object, open = \"r\")  "
rawConnectionValue : function (con)  "
rawShift : function (x, n)  "
rawToBits : function (x)  "
rawToChar : function (x, multiple = FALSE)  "
rbind : function (..., deparse.level = 1)  "
rbind.data.frame : function (..., deparse.level = 1, make.row.names = TRUE, stringsAsFactors = FALSE, "
    factor.exclude = TRUE)  "
rcond : function (x, norm = c(\"O\", \"I\", \"1\"), triangular = FALSE, ...)  "
Re : function (z)  "
read.dcf : function (file, fields = NULL, all = FALSE, keep.white = NULL)  "
readBin : function (con, what, n = 1L, size = NA_integer_, signed = TRUE, endian = .Platform$endian)  "
readChar : function (con, nchars, useBytes = FALSE)  "
readline : function (prompt = \"\")  "
readLines : function (con = stdin(), n = -1L, ok = TRUE, warn = TRUE, encoding = \"unknown\", skipNul = FALSE)  "
readRDS : function (file, refhook = NULL)  "
readRenviron : function (path)  "
Recall : function (...)  "
Reduce : function (f, x, init, right = FALSE, accumulate = FALSE)  "
reg.finalizer : function (e, f, onexit = FALSE)  "
regexec : function (pattern, text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)  "
regexpr : function (pattern, text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)  "
registerS3method : function (genname, class, method, envir = parent.frame())  "
registerS3methods : function (info, package, env)  "
regmatches : function (x, m, invert = FALSE)  "
regmatches<- : function (x, m, invert = FALSE, value)  "
remove : function (..., list = character(), pos = -1, envir = as.environment(pos), inherits = FALSE)  "
removeTaskCallback : function (id)  "
rep : function (x, ...)  "
rep.Date : function (x, ...)  "
rep.difftime : function (x, ...)  "
rep.factor : function (x, ...)  "
rep.int : function (x, times)  "
rep.numeric_version : function (x, ...)  "
rep.POSIXct : function (x, ...)  "
rep.POSIXlt : function (x, ...)  "
rep_len : function (x, length.out)  "
repeat : .Primitive(\"repeat\") "
replace : function (x, list, values)  "
replicate : function (n, expr, simplify = \"array\")  "
require : function (package, lib.loc = NULL, quietly = FALSE, warn.conflicts, character.only = FALSE, "
    mask.ok, exclude, include.only, attach.required = missing(include.only))  "
requireNamespace : function (package, ..., quietly = FALSE)  "
restartDescription : function (r)  "
restartFormals : function (r)  "
retracemem : function (x, previous = NULL)  "
return : .Primitive(\"return\") "
returnValue : function (default = NULL)  "
rev : function (x)  "
rev.default : function (x)  "
rle : function (x)  "
rm : function (..., list = character(), pos = -1, envir = as.environment(pos), inherits = FALSE)  "
RNGkind : function (kind = NULL, normal.kind = NULL, sample.kind = NULL)  "
RNGversion : function (vstr)  "
round : function (x, digits = 0)  "
round.Date : function (x, ...)  "
round.POSIXt : function (x, units = c(\"secs\", \"mins\", \"hours\", \"days\", \"months\", \"years\"))  "
row : function (x, as.factor = FALSE)  "
row.names : function (x)  "
row.names.data.frame : function (x)  "
row.names.default : function (x)  "
row.names<- : function (x, value)  "
row.names<-.data.frame : function (x, value)  "
row.names<-.default : function (x, value)  "
rowMeans : function (x, na.rm = FALSE, dims = 1L)  "
rownames : function (x, do.NULL = TRUE, prefix = \"row\")  "
rownames<- : function (x, value)  "
rowsum : function (x, group, reorder = TRUE, ...)  "
rowsum.data.frame : function (x, group, reorder = TRUE, na.rm = FALSE, ...)  "
rowsum.default : function (x, group, reorder = TRUE, na.rm = FALSE, ...)  "
rowSums : function (x, na.rm = FALSE, dims = 1L)  "
sample : function (x, size, replace = FALSE, prob = NULL)  "
sample.int : function (n, size = n, replace = FALSE, prob = NULL, useHash = (n > 1e+07 && !replace && "
    is.null(prob) && size <= n/2))  "
sapply : function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)  "
save : function (..., list = character(), file = stop(\"'file' must be specified\"), ascii = FALSE, "
    version = NULL, envir = parent.frame(), compress = isTRUE(!ascii), compression_level, "
    eval.promises = TRUE, precheck = TRUE)  "
save.image : function (file = \".RData\", version = NULL, ascii = FALSE, compress = !ascii, safe = TRUE)  "
saveRDS : function (object, file = \"\", ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)  "
scale : function (x, center = TRUE, scale = TRUE)  "
scale.default : function (x, center = TRUE, scale = TRUE)  "
scan : function (file = \"\", what = double(), nmax = -1L, n = -1L, sep = \"\", quote = if (identical(sep, "
    \"\n\")) \"\" else \"'\\"\", dec = \".\", skip = 0L, nlines = 0L, na.strings = \"NA\", flush = FALSE, "
    fill = FALSE, strip.white = FALSE, quiet = FALSE, blank.lines.skip = TRUE, multi.line = TRUE, "
    comment.char = \"\", allowEscapes = FALSE, fileEncoding = \"\", encoding = \"unknown\", "
    text, skipNul = FALSE)  "
search : function ()  "
searchpaths : function ()  "
seek : function (con, ...)  "
seek.connection : function (con, where = NA, origin = \"start\", rw = \"\", ...)  "
seq : function (...)  "
seq.Date : function (from, to, by, length.out = NULL, along.with = NULL, ...)  "
seq.default : function (from = 1, to = 1, by = ((to - from)/(length.out - 1)), length.out = NULL, "
    along.with = NULL, ...)  "
seq.int : function (from, to, by, length.out, along.with, ...)  "
seq.POSIXt : function (from, to, by, length.out = NULL, along.with = NULL, ...)  "
seq_along : function (along.with)  "
seq_len : function (length.out)  "
sequence : function (nvec, ...)  "
sequence.default : function (nvec, from = 1L, by = 1L, ...)  "
serialize : function (object, connection, ascii = FALSE, xdr = TRUE, version = NULL, refhook = NULL)  "
serverSocket : function (port)  "
set.seed : function (seed, kind = NULL, normal.kind = NULL, sample.kind = NULL)  "
setdiff : function (x, y)  "
setequal : function (x, y)  "
setHook : function (hookName, value, action = c(\"append\", \"prepend\", \"replace\"))  "
setNamespaceInfo : function (ns, which, val)  "
setSessionTimeLimit : function (cpu = Inf, elapsed = Inf)  "
setTimeLimit : function (cpu = Inf, elapsed = Inf, transient = FALSE)  "
setwd : function (dir)  "
shell : function (cmd, shell, flag = \"/c\", intern = FALSE, wait = TRUE, translate = FALSE, "
    mustWork = FALSE, ...)  "
shell.exec : function (file)  "
showConnections : function (all = FALSE)  "
shQuote : function (string, type = c(\"sh\", \"csh\", \"cmd\", \"cmd2\"))  "
sign : function (x)  "
signalCondition : function (cond)  "
signif : function (x, digits = 6)  "
simpleCondition : function (message, call = NULL)  "
simpleError : function (message, call = NULL)  "
simpleMessage : function (message, call = NULL)  "
simpleWarning : function (message, call = NULL)  "
simplify2array : function (x, higher = TRUE, except = c(0L, 1L))  "
sin : function (x)  "
single : function (length = 0L)  "
sinh : function (x)  "
sink : function (file = NULL, append = FALSE, type = c(\"output\", \"message\"), split = FALSE)  "
sink.number : function (type = c(\"output\", \"message\"))  "
sinpi : function (x)  "
slice.index : function (x, MARGIN)  "
socketAccept : function (socket, blocking = FALSE, open = \"a+\", encoding = getOption(\"encoding\"), "
    timeout = getOption(\"timeout\"), options = getOption(\"socketOptions\"))  "
socketConnection : function (host = \"localhost\", port, server = FALSE, blocking = FALSE, open = \"a+\", "
    encoding = getOption(\"encoding\"), timeout = getOption(\"timeout\"), options = getOption(\"socketOptions\"))  "
socketSelect : function (socklist, write = FALSE, timeout = NULL)  "
socketTimeout : function (socket, timeout = -1)  "
solve : function (a, b, ...)  "
solve.default : function (a, b, tol = .Machine$double.eps, LINPACK = FALSE, ...)  "
solve.qr : function (a, b, ...)  "
sort : function (x, decreasing = FALSE, ...)  "
sort.default : function (x, decreasing = FALSE, na.last = NA, ...)  "
sort.int : function (x, partial = NULL, na.last = NA, decreasing = FALSE, method = c(\"auto\", "
    \"shell\", \"quick\", \"radix\"), index.return = FALSE)  "
sort.list : function (x, partial = NULL, na.last = TRUE, decreasing = FALSE, method = c(\"auto\", "
    \"shell\", \"quick\", \"radix\"))  "
sort.POSIXlt : function (x, decreasing = FALSE, na.last = NA, ...)  "
source : function (file, local = FALSE, echo = verbose, print.eval = echo, exprs, spaced = use_file, "
    verbose = getOption(\"verbose\"), prompt.echo = getOption(\"prompt\"), max.deparse.length = 150, "
    width.cutoff = 60L, deparseCtrl = \"showAttributes\", chdir = FALSE, encoding = getOption(\"encoding\"), "
    continue.echo = getOption(\"continue\"), skip.echo = 0, keep.source = getOption(\"keep.source\"))  "
split : function (x, f, drop = FALSE, ...)  "
split.data.frame : function (x, f, drop = FALSE, ...)  "
split.Date : function (x, f, drop = FALSE, ...)  "
split.default : function (x, f, drop = FALSE, sep = \".\", lex.order = FALSE, ...)  "
split.POSIXct : function (x, f, drop = FALSE, ...)  "
split<- : function (x, f, drop = FALSE, ..., value)  "
split<-.data.frame : function (x, f, drop = FALSE, ..., value)  "
split<-.default : function (x, f, drop = FALSE, ..., value)  "
sprintf : function (fmt, ...)  "
sqrt : function (x)  "
sQuote : function (x, q = getOption(\"useFancyQuotes\"))  "
srcfile : function (filename, encoding = getOption(\"encoding\"), Enc = \"unknown\")  "
srcfilealias : function (filename, srcfile)  "
srcfilecopy : function (filename, lines, timestamp = Sys.time(), isFile = FALSE)  "
srcref : function (srcfile, lloc)  "
standardGeneric : function (f, fdef)  "
startsWith : function (x, prefix)  "
stderr : function ()  "
stdin : function ()  "
stdout : function ()  "
stop : function (..., call. = TRUE, domain = NULL)  "
stopifnot : function (..., exprs, exprObject, local = TRUE)  "
storage.mode : function (x)  "
storage.mode<- : function (x, value)  "
str2expression : function (text)  "
str2lang : function (s)  "
strftime : function (x, format = \"\", tz = \"\", usetz = FALSE, ...)  "
strptime : function (x, format, tz = \"\")  "
strrep : function (x, times)  "
strsplit : function (x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE)  "
strtoi : function (x, base = 0L)  "
strtrim : function (x, width)  "
structure : function (.Data, ...)  "
strwrap : function (x, width = 0.9 * getOption(\"width\"), indent = 0, exdent = 0, prefix = \"\", "
    simplify = TRUE, initial = prefix)  "
sub : function (pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, "
    useBytes = FALSE)  "
subset : function (x, ...)  "
subset.data.frame : function (x, subset, select, drop = FALSE, ...)  "
subset.default : function (x, subset, ...)  "
subset.matrix : function (x, subset, select, drop = FALSE, ...)  "
substitute : function (expr, env)  "
substr : function (x, start, stop)  "
substr<- : function (x, start, stop, value)  "
substring : function (text, first, last = 1000000L)  "
substring<- : function (text, first, last = 1000000L, value)  "
sum : function (..., na.rm = FALSE)  "
summary : function (object, ...)  "
summary.connection : function (object, ...)  "
summary.data.frame : function (object, maxsum = 7L, digits = max(3L, getOption(\"digits\") - 3L), ...)  "
Summary.data.frame : function (..., na.rm)  "
summary.Date : function (object, digits = 12L, ...)  "
Summary.Date : function (..., na.rm)  "
summary.default : function (object, ..., digits, quantile.type = 7)  "
Summary.difftime : function (..., na.rm)  "
summary.factor : function (object, maxsum = 100L, ...)  "
Summary.factor : function (..., na.rm)  "
summary.matrix : function (object, ...)  "
Summary.numeric_version : function (..., na.rm)  "
Summary.ordered : function (..., na.rm)  "
summary.POSIXct : function (object, digits = 15L, ...)  "
Summary.POSIXct : function (..., na.rm)  "
summary.POSIXlt : function (object, digits = 15, ...)  "
Summary.POSIXlt : function (..., na.rm)  "
summary.proc_time : function (object, ...)  "
summary.srcfile : function (object, ...)  "
summary.srcref : function (object, useSource = FALSE, ...)  "
summary.table : function (object, ...)  "
summary.warnings : function (object, ...)  "
suppressMessages : function (expr, classes = \"message\")  "
suppressPackageStartupMessages : function (expr)  "
suppressWarnings : function (expr, classes = \"warning\")  "
suspendInterrupts : function (expr)  "
svd : function (x, nu = min(n, p), nv = min(n, p), LINPACK = FALSE)  "
sweep : function (x, MARGIN, STATS, FUN = \"-\", check.margin = TRUE, ...)  "
switch : function (EXPR, ...)  "
sys.call : function (which = 0L)  "
sys.calls : function ()  "
Sys.chmod : function (paths, mode = \"0777\", use_umask = TRUE)  "
Sys.Date : function ()  "
sys.frame : function (which = 0L)  "
sys.frames : function ()  "
sys.function : function (which = 0L)  "
Sys.getenv : function (x = NULL, unset = \"\", names = NA)  "
Sys.getlocale : function (category = \"LC_ALL\")  "
Sys.getpid : function ()  "
Sys.glob : function (paths, dirmark = FALSE)  "
Sys.info : function ()  "
Sys.junction : function (from, to)  "
sys.load.image : function (name, quiet)  "
Sys.localeconv : function ()  "
sys.nframe : function ()  "
sys.on.exit : function ()  "
sys.parent : function (n = 1L)  "
sys.parents : function ()  "
Sys.readlink : function (paths)  "
sys.save.image : function (name)  "
Sys.setenv : function (...)  "
Sys.setFileTime : function (path, time)  "
Sys.setLanguage : function (lang, unset = \"en\")  "
Sys.setlocale : function (category = \"LC_ALL\", locale = \"\")  "
Sys.sleep : function (time)  "
sys.source : function (file, envir = baseenv(), chdir = FALSE, keep.source = getOption(\"keep.source.pkgs\"), "
    keep.parse.data = getOption(\"keep.parse.data.pkgs\"), toplevel.env = as.environment(envir))  "
sys.status : function ()  "
Sys.time : function ()  "
Sys.timezone : function (location = TRUE)  "
Sys.umask : function (mode = NA)  "
Sys.unsetenv : function (x)  "
Sys.which : function (names)  "
system : function (command, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE, "
    input = NULL, show.output.on.console = TRUE, minimized = FALSE, invisible = TRUE, "
    timeout = 0)  "
system.file : function (..., package = \"base\", lib.loc = NULL, mustWork = FALSE)  "
system.time : function (expr, gcFirst = TRUE)  "
system2 : function (command, args = character(), stdout = \"\", stderr = \"\", stdin = \"\", input = NULL, "
    env = character(), wait = TRUE, minimized = FALSE, invisible = TRUE, timeout = 0)  "
t : function (x)  "
T :  logi TRUE"
t.data.frame : function (x)  "
t.default : function (x)  "
table : function (..., exclude = if (useNA == \"no\") c(NA, NaN), useNA = c(\"no\", \"ifany\", \"always\"), "
    dnn = list.names(...), deparse.level = 1)  "
tabulate : function (bin, nbins = max(1L, bin, na.rm = TRUE))  "
tan : function (x)  "
tanh : function (x)  "
tanpi : function (x)  "
tapply : function (X, INDEX, FUN = NULL, ..., default = NA, simplify = TRUE)  "
taskCallbackManager : function (handlers = list(), registered = FALSE, verbose = FALSE)  "
tcrossprod : function (x, y = NULL)  "
tempdir : function (check = FALSE)  "
tempfile : function (pattern = \"file\", tmpdir = tempdir(), fileext = \"\")  "
textConnection : function (object, open = \"r\", local = FALSE, name = deparse1(substitute(object)), "
    encoding = c(\"\", \"bytes\", \"UTF-8\"))  "
textConnectionValue : function (con)  "
tolower : function (x)  "
topenv : function (envir = parent.frame(), matchThisEnv = getOption(\"topLevelEnvironment\"))  "
toString : function (x, ...)  "
toString.default : function (x, width = NULL, ...)  "
toupper : function (x)  "
trace : function (what, tracer, exit, at, print, signature, where = topenv(parent.frame()), "
    edit = FALSE)  "
traceback : function (x = NULL, max.lines = getOption(\"traceback.max.lines\", getOption(\"deparse.max.lines\", "
    -1L)))  "
tracemem : function (x)  "
tracingState : function (on = NULL)  "
transform : function (`_data`, ...)  "
transform.data.frame : function (`_data`, ...)  "
transform.default : function (`_data`, ...)  "
trigamma : function (x)  "
trimws : function (x, which = c(\"both\", \"left\", \"right\"), whitespace = \"[ \t\r\n]\")  "
trunc : function (x, ...)  "
trunc.Date : function (x, units = c(\"secs\", \"mins\", \"hours\", \"days\", \"months\", \"years\"), ...)  "
trunc.POSIXt : function (x, units = c(\"secs\", \"mins\", \"hours\", \"days\", \"months\", \"years\"), ...)  "
truncate : function (con, ...)  "
truncate.connection : function (con, ...)  "
try : function (expr, silent = FALSE, outFile = getOption(\"try.outFile\", default = stderr()))  "
tryCatch : function (expr, ..., finally)  "
tryInvokeRestart : function (r, ...)  "
typeof : function (x)  "
unclass : function (x)  "
undebug : function (fun, signature = NULL)  "
union : function (x, y)  "
unique : function (x, incomparables = FALSE, ...)  "
unique.array : function (x, incomparables = FALSE, MARGIN = 1, fromLast = FALSE, ...)  "
unique.data.frame : function (x, incomparables = FALSE, fromLast = FALSE, ...)  "
unique.default : function (x, incomparables = FALSE, fromLast = FALSE, nmax = NA, ...)  "
unique.matrix : function (x, incomparables = FALSE, MARGIN = 1, fromLast = FALSE, ...)  "
unique.numeric_version : function (x, incomparables = FALSE, ...)  "
unique.POSIXlt : function (x, incomparables = FALSE, ...)  "
unique.warnings : function (x, incomparables = FALSE, ...)  "
units : function (x)  "
units.difftime : function (x)  "
units<- : function (x, value)  "
units<-.difftime : function (x, value)  "
unix.time : function (...)  "
unlink : function (x, recursive = FALSE, force = FALSE, expand = TRUE)  "
unlist : function (x, recursive = TRUE, use.names = TRUE)  "
unloadNamespace : function (ns)  "
unlockBinding : function (sym, env)  "
unname : function (obj, force = FALSE)  "
unserialize : function (connection, refhook = NULL)  "
unsplit : function (value, f, drop = FALSE)  "
untrace : function (what, signature = NULL, where = topenv(parent.frame()))  "
untracemem : function (x)  "
unz : function (description, filename, open = \"\", encoding = getOption(\"encoding\"))  "
upper.tri : function (x, diag = FALSE)  "
url : function (description, open = \"\", blocking = TRUE, encoding = getOption(\"encoding\"), "
    method = getOption(\"url.method\", \"default\"), headers = NULL)  "
UseMethod : function (generic, object)  "
utf8ToInt : function (x)  "
validEnc : function (x)  "
validUTF8 : function (x)  "
vapply : function (X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE)  "
vector : function (mode = \"logical\", length = 0L)  "
Vectorize : function (FUN, vectorize.args = arg.names, SIMPLIFY = TRUE, USE.NAMES = TRUE)  "
version : List of 15"
 $ platform      : chr \"x86_64-w64-mingw32\""
 $ arch          : chr \"x86_64\""
 $ os            : chr \"mingw32\""
 $ crt           : chr \"ucrt\""
 $ system        : chr \"x86_64, mingw32\""
 $ status        : chr \"\""
 $ major         : chr \"4\""
 $ minor         : chr \"2.1\""
 $ year          : chr \"2022\""
 $ month         : chr \"06\""
 $ day           : chr \"23\""
 $ svn rev       : chr \"82513\""
 $ language      : chr \"R\""
 $ version.string: chr \"R version 4.2.1 (2022-06-23 ucrt)\""
 $ nickname      : chr \"Funny-Looking Kid\""
warning : function (..., call. = TRUE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL)  "
warningCondition : function (message, ..., class = NULL, call = NULL)  "
warnings : function (...)  "
weekdays : function (x, abbreviate)  "
weekdays.Date : function (x, abbreviate = FALSE)  "
weekdays.POSIXt : function (x, abbreviate = FALSE)  "
which : function (x, arr.ind = FALSE, useNames = TRUE)  "
which.max : function (x)  "
which.min : function (x)  "
while : .Primitive(\"while\") "
with : function (data, expr, ...)  "
with.default : function (data, expr, ...)  "
withAutoprint : function (exprs, evaluated = FALSE, local = parent.frame(), print. = TRUE, echo = TRUE, "
    max.deparse.length = Inf, width.cutoff = max(20, getOption(\"width\")), deparseCtrl = c(\"keepInteger\", "
        \"showAttributes\", \"keepNA\"), ...)  "
withCallingHandlers : function (expr, ...)  "
within : function (data, expr, ...)  "
within.data.frame : function (data, expr, ...)  "
within.list : function (data, expr, keepAttrs = TRUE, ...)  "
withRestarts : function (expr, ...)  "
withVisible : function (x)  "
write : function (x, file = \"data\", ncolumns = if (is.character(x)) 1 else 5, append = FALSE, "
    sep = \" \")  "
write.dcf : function (x, file = \"\", append = FALSE, useBytes = FALSE, indent = 0.1 * getOption(\"width\"), "
    width = 0.9 * getOption(\"width\"), keep.white = NULL)  "
writeBin : function (object, con, size = NA_integer_, endian = .Platform$endian, useBytes = FALSE)  "
writeChar : function (object, con, nchars = nchar(object, type = \"chars\"), eos = \"\", useBytes = FALSE)  "
writeLines : function (text, con = stdout(), sep = \"\n\", useBytes = FALSE)  "
xor : function (x, y)  "
xpdrows.data.frame : function (x, old.rows, new.rows)  "
xtfrm : function (x)  "
xtfrm.AsIs : function (x)  "
xtfrm.data.frame : function (x)  "
xtfrm.Date : function (x)  "
xtfrm.default : function (x)  "
xtfrm.difftime : function (x)  "
xtfrm.factor : function (x)  "
xtfrm.numeric_version : function (x)  "
xtfrm.POSIXct : function (x)  "
xtfrm.POSIXlt : function (x)  "
xzfile : function (description, open = \"\", encoding = getOption(\"encoding\"), compression = 6)  "
zapsmall : function (x, digits = getOption(\"digits\"))  "

library(beepr)
beep : function (sound = 1, expr = NULL)  
beep_on_error : function (expr, sound = 1)  
    
library(caret)
.Depends :  chr [1:2] "ggplot2" "lattice"
anovaScores : function (x, y)  
  avNNet : function (x, ...)  
    bag : function (x, ...)  
      bagControl : function (fit = NULL, predict = NULL, aggregate = NULL, downSample = FALSE, oob = TRUE, 
                             allowParallel = TRUE)  
        bagEarth : function (x, ...)  
          bagEarthStats : function (x)  
            bagFDA : function (x, ...)  
              best : function (x, metric, maximize)  
                BoxCoxTrans : function (y, ...)  
                  calibration : function (x, ...)  
                    caretFuncs : List of 6
$ summary   :function (data, lev = NULL, model = NULL)  
  $ fit       :function (x, y, first, last, ...)  
    $ pred      :function (object, x)  
      $ rank      :function (object, x, y)  
        $ selectSize:function (x, metric, maximize)  
          $ selectVar :function (y, size)  
            caretGA : List of 9
$ fit           :function (x, y, lev = NULL, last = FALSE, ...)  
  $ pred          :function (object, x)  
    $ fitness_intern:function (object, x, y, maximize, p)  
      $ fitness_extern:function (data, lev = NULL, model = NULL)  
        $ initial       :function (vars, popSize, ...)  
          $ selection     :function (population, fitness, r = NULL, q = NULL, ...)  
            $ crossover     :function (population, fitness, parents, ...)  
              $ mutation      :function (population, parent, ...)  
                $ selectIter    :function (x, metric, maximize)  
                  caretSA : List of 8
$ fit           :function (x, y, lev = NULL, last = FALSE, ...)  
  $ pred          :function (object, x)  
    $ fitness_intern:function (object, x, y, maximize, p)  
      $ fitness_extern:function (data, lev = NULL, model = NULL)  
        $ initial       :function (vars, prob = 0.2, ...)  
          $ perturb       :function (x, vars, number = floor(length(x) * 0.01) + 1)  
            $ prob          :function (old, new, iteration = 1)  
              $ selectIter    :function (x, metric, maximize)  
                caretSBF : List of 5
$ summary:function (data, lev = NULL, model = NULL)  
  $ fit    :function (x, y, ...)  
    $ pred   :function (object, x)  
      $ score  :function (x, y)  
        $ filter :function (score, x, y)  
          caretTheme : function ()  
            cforestStats : function (x)  
              checkConditionalX : function (x, y)  
                checkInstall : function (pkg)  
                  checkResamples : function (index, x, y)  
                    class2ind : function (x, drop2nd = FALSE)  
                      classDist : function (x, ...)  
                        cluster : function (x, ...)  
                          compare_models : function (a, b, metric = a$metric[1])  
                            confusionMatrix : function (data, ...)  
                              confusionMatrix.train : function (data, norm = "overall", dnn = c("Prediction", "Reference"), ...)  
                                contr.dummy : function (n, ...)  
                                  contr.ltfr : function (n, contrasts = TRUE, sparse = FALSE)  
                                    createDataPartition : function (y, times = 1, p = 0.5, list = TRUE, groups = min(5, length(y)))  
                                      createFolds : function (y, k = 10, list = TRUE, returnTrain = FALSE)  
                                        createModel : function (x, y, wts, method, tuneValue, obsLevels, pp = NULL, last = FALSE, sampling = NULL, 
                                                                classProbs, ...)  
                                          createMultiFolds : function (y, k = 10, times = 5)  
                                            createResample : function (y, times = 10, list = TRUE)  
                                              createTimeSlices : function (y, initialWindow, horizon = 1, fixedWindow = TRUE, skip = 0)  
                                                ctreeBag : List of 3
$ fit      :function (x, y, ...)  
  $ pred     :function (object, x)  
    $ aggregate:function (x, type = "class")  
      defaultSummary : function (data, lev = NULL, model = NULL)  
        dotPlot : function (x, top = min(20, dim(x$importance)[1]), ...)  
          downSample : function (x, y, list = FALSE, yname = "Class")  
            dummyVars : function (formula, ...)  
              expandParameters : function (fixed, seq)  
                expoTrans : function (y, ...)  
                  extractPrediction : function (models, testX = NULL, testY = NULL, unkX = NULL, unkOnly = !is.null(unkX) & 
                                                  is.null(testX), verbose = FALSE)  
                    extractProb : function (models, testX = NULL, testY = NULL, unkX = NULL, unkOnly = !is.null(unkX) & 
                                              is.null(testX), verbose = FALSE)  
                      F_meas : function (data, ...)  
                        featurePlot : function (x, y, plot = if (is.factor(y)) "strip" else "scatter", labels = c("Feature", 
                                                                                                                  ""), ...)  
                          filterVarImp : function (x, y, nonpara = FALSE, ...)  
                            findCorrelation : function (x, cutoff = 0.9, verbose = FALSE, names = FALSE, exact = ncol(x) < 100)  
                              findLinearCombos : function (x)  
                                flatTable : function (pred, obs)  
                                  gafs : function (x, ...)  
                                    gafs.default : function (x, y, iters = 10, popSize = 50, pcrossover = 0.8, pmutation = 0.1, elite = 0, 
                                                             suggestions = NULL, differences = TRUE, gafsControl = gafsControl(), ...)  
                                      gafs_initial : function (vars, popSize, ...)  
                                        gafs_lrSelection : function (population, fitness, r = NULL, q = NULL, ...)  
                                          gafs_raMutation : function (population, parent, ...)  
                                            gafs_rwSelection : function (population, fitness, ...)  
                                              gafs_spCrossover : function (population, fitness, parents, ...)  
                                                gafs_tourSelection : function (population, fitness, k = 3, ...)  
                                                  gafs_uCrossover : function (population, parents, ...)  
                                                    gafsControl : function (functions = NULL, method = "repeatedcv", metric = NULL, maximize = NULL, 
                                                                            number = ifelse(grepl("cv", method), 10, 25), repeats = ifelse(grepl("cv", method), 
                                                                                                                                           1, 5), verbose = FALSE, returnResamp = "final", p = 0.75, index = NULL, indexOut = NULL, 
                                                                            seeds = NULL, holdout = 0, genParallel = FALSE, allowParallel = TRUE)  
                                                      gamFormula : function (data, smoother = "s", cut = 8, y = "y")  
                                                        gamFuncs : List of 6
$ summary   :function (data, lev = NULL, model = NULL)  
  $ fit       :function (x, y, first, last, ...)  
    $ pred      :function (object, x)  
      $ rank      :function (object, x, y)  
        $ selectSize:function (x, metric, maximize)  
          $ selectVar :function (y, size)  
            gamScores : function (x, y)  
              getModelInfo : function (model = NULL, regex = TRUE, ...)  
                getSamplingInfo : function (method = NULL, regex = TRUE, ...)  
                  getTrainPerf : function (x)  
                    ggplot.gafs : function (data = NULL, mapping = NULL, ..., environment = NULL)  
                      ggplot.safs : function (data = NULL, mapping = NULL, ..., environment = NULL)  
                        groupKFold : function (group, k = length(unique(group)))  
                          hasTerms : function (x)  
                            icr : function (x, ...)  
                              index2vec : function (x, vars, sign = FALSE)  
                                ipredStats : function (x)  
                                  knn3 : function (x, ...)  
                                    knn3Train : function (train, test, cl, k = 1, l = 0, prob = TRUE, use.all = TRUE)  
                                      knnreg : function (x, ...)  
                                        knnregTrain : function (train, test, y, k = 5, use.all = TRUE)  
                                          ldaBag : List of 3
$ fit      :function (x, y, ...)  
  $ pred     :function (object, x)  
    $ aggregate:function (x, type = "class")  
      ldaFuncs : List of 6
$ summary   :function (data, lev = NULL, model = NULL)  
  $ fit       :function (x, y, first, last, ...)  
    $ pred      :function (object, x)  
      $ rank      :function (object, x, y)  
        $ selectSize:function (x, metric, maximize)  
          $ selectVar :function (y, size)  
            ldaSBF : List of 5
$ summary:function (data, lev = NULL, model = NULL)  
  $ fit    :function (x, y, ...)  
    $ pred   :function (object, x)  
      $ score  :function (x, y)  
        $ filter :function (score, x, y)  
          learning_curve_dat : function (dat, outcome = NULL, proportion = (1:10)/10, test_prop = 0, verbose = TRUE, 
                                         ...)  
            lift : function (x, ...)  
              lmFuncs : List of 6
$ summary   :function (data, lev = NULL, model = NULL)  
  $ fit       :function (x, y, first, last, ...)  
    $ pred      :function (object, x)  
      $ rank      :function (object, x, y)  
        $ selectSize:function (x, metric, maximize)  
          $ selectVar :function (y, size)  
            lmSBF : List of 5
$ summary:function (data, lev = NULL, model = NULL)  
  $ fit    :function (x, y, ...)  
    $ pred   :function (object, x)  
      $ score  :function (x, y)  
        $ filter :function (score, x, y)  
          LPH07_1 : function (n = 100, noiseVars = 0, corrVars = 0, corrType = "AR1", corrValue = 0, factors = FALSE, 
                              class = FALSE)  
            LPH07_2 : function (n = 100, noiseVars = 0, corrVars = 0, corrType = "AR1", corrValue = 0)  
              lrFuncs : List of 6
$ summary   :function (data, lev = NULL, model = NULL)  
  $ fit       :function (x, y, first, last, ...)  
    $ pred      :function (object, x)  
      $ rank      :function (object, x, y)  
        $ selectSize:function (x, metric, maximize)  
          $ selectVar :function (y, size)  
            MAE : function (pred, obs, na.rm = FALSE)  
              maxDissim : function (a, b, n = 2, obj = minDiss, useNames = FALSE, randomFrac = 1, verbose = FALSE, 
                                    ...)  
                MeanSD : function (x, exclude = NULL)  
                  minDiss : function (u)  
                    mnLogLoss : function (data, lev = NULL, model = NULL)  
                      modelCor : function (x, metric = x$metric[1], ...)  
                        modelLookup : function (model = NULL)  
                          multiClassSummary : function (data, lev = NULL, model = NULL)  
                            nbBag : List of 3
$ fit      :function (x, y, ...)  
  $ pred     :function (object, x)  
    $ aggregate:function (x, type = "class")  
      nbFuncs : List of 6
$ summary   :function (data, lev = NULL, model = NULL)  
  $ fit       :function (x, y, first, last, ...)  
    $ pred      :function (object, x)  
      $ rank      :function (object, x, y)  
        $ selectSize:function (x, metric, maximize)  
          $ selectVar :function (y, size)  
            nbSBF : List of 6
$ summary:function (data, lev = NULL, model = NULL)  
  $ fit    :function (x, y, ...)  
    $ pred   :function (object, x)  
      $ pred   :function (object, x)  
        $ score  :function (x, y)  
          $ filter :function (score, x, y)  
            nearZeroVar : function (x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, names = FALSE, foreach = FALSE, 
                                    allowParallel = TRUE)  
              negPredValue : function (data, ...)  
                nnetBag : List of 3
$ fit      :function (x, y, ...)  
  $ pred     :function (object, x)  
    $ aggregate:function (x, type = "class")  
      nullModel : function (x, ...)  
        nzv : function (x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, names = FALSE)  
          oneSE : function (x, metric, num, maximize)  
            outcome_conversion : function (x, lv)  
              panel.calibration : function (...)  
                panel.lift : function (x, y, ...)  
                  panel.lift2 : function (x, y, pct = 0, values = NULL, ...)  
                    panel.needle : function (x, y, horizontal = TRUE, pch = if (is.null(groups)) dot.symbol$pch else sup.symbol$pch, 
                                             col = if (is.null(groups)) dot.symbol$col else sup.symbol$col, lty = dot.line$lty, 
                                             lwd = dot.line$lwd, col.line = dot.line$col, levels.fos = NULL, groups = NULL, 
                                             ...)  
                      pcaNNet : function (x, ...)  
                        pickSizeBest : function (x, metric, maximize)  
                          pickSizeTolerance : function (x, metric, tol = 1.5, maximize)  
                            pickVars : function (y, size)  
                              plot.gafs : function (x, metric = x$control$metric["external"], estimate = c("internal", "external"), 
                                                    output = "ggplot", ...)  
                                plot.rfe : function (x, metric = x$metric, ...)  
                                  plot.safs : function (x, metric = x$control$metric["external"], estimate = c("internal", "external"), 
                                                        output = "ggplot", ...)  
                                    plot.train : function (x, plotType = "scatter", metric = x$metric[1], digits = getOption("digits") - 
                                                             3, xTrans = NULL, nameInStrip = FALSE, ...)  
                                      plotClassProbs : function (object, plotType = "histogram", useObjects = FALSE, ...)  
                                        plotObsVsPred : function (object, equalRanges = TRUE, ...)  
                                          plsBag : List of 3
$ fit      :function (x, y, ...)  
  $ pred     :function (object, x)  
    $ aggregate:function (x, type = "class")  
      plsda : function (x, ...)  
        posPredValue : function (data, ...)  
          postResample : function (pred, obs)  
            precision : function (data, ...)  
              predict.bagEarth : function (object, newdata = NULL, type = NULL, ...)  
                predict.gafs : function (object, newdata, ...)  
                  predict.train : function (object, newdata = NULL, type = "raw", na.action = na.omit, ...)  
                    predictionFunction : function (method, modelFit, newdata, preProc = NULL, param = NULL)  
                      predictors : function (x, ...)  
                        preProcess : function (x, ...)  
                          print.train : function (x, printCall = FALSE, details = FALSE, selectCol = FALSE, showSD = FALSE, 
                                                  ...)  
                            probFunction : function (method, modelFit, newdata = NULL, preProc = NULL, param = NULL)  
                              progress : function (x, names, iter, start = TRUE)  
                                prSummary : function (data, lev = NULL, model = NULL)  
                                  R2 : function (pred, obs, formula = "corr", na.rm = FALSE)  
                                    recall : function (data, ...)  
                                      resampleHist : function (object, type = "density", ...)  
                                        resamples : function (x, ...)  
                                          resampleSummary : function (obs, resampled, index = NULL, keepData = TRUE)  
                                            resampleWrapper : function (x, ind)  
                                              rfe : function (x, ...)  
                                                rfeControl : function (functions = NULL, rerank = FALSE, method = "boot", saveDetails = FALSE, 
                                                                       number = ifelse(method %in% c("cv", "repeatedcv"), 10, 25), repeats = ifelse(method %in% 
                                                                                                                                                      c("cv", "repeatedcv"), 1, number), verbose = FALSE, returnResamp = "final", 
                                                                       p = 0.75, index = NULL, indexOut = NULL, timingSamps = 0, seeds = NA, allowParallel = TRUE)  
                                                  rfeIter : function (x, y, testX, testY, sizes, rfeControl = rfeControl(), label = "", seeds = NA, 
                                                                      ...)  
                                                    rfFuncs : List of 6
$ summary   :function (data, lev = NULL, model = NULL)  
  $ fit       :function (x, y, first, last, ...)  
    $ pred      :function (object, x)  
      $ rank      :function (object, x, y)  
        $ selectSize:function (x, metric, maximize)  
          $ selectVar :function (y, size)  
            rfGA : List of 9
$ fit           :function (x, y, lev = NULL, last = FALSE, ...)  
  $ pred          :function (object, x)  
    $ fitness_intern:function (object, x, y, maximize, p)  
      $ fitness_extern:function (data, lev = NULL, model = NULL)  
        $ initial       :function (vars, popSize, ...)  
          $ selection     :function (population, fitness, r = NULL, q = NULL, ...)  
            $ crossover     :function (population, fitness, parents, ...)  
              $ mutation      :function (population, parent, ...)  
                $ selectIter    :function (x, metric, maximize)  
                  rfSA : List of 8
$ fit           :function (x, y, lev = NULL, last = FALSE, ...)  
  $ pred          :function (object, x)  
    $ fitness_intern:function (object, x, y, maximize, p)  
      $ fitness_extern:function (data, lev = NULL, model = NULL)  
        $ initial       :function (vars, prob = 0.2, ...)  
          $ perturb       :function (x, vars, number = floor(length(x) * 0.01) + 1)  
            $ prob          :function (old, new, iteration = 1)  
              $ selectIter    :function (x, metric, maximize)  
                rfSBF : List of 5
$ summary:function (data, lev = NULL, model = NULL)  
  $ fit    :function (x, y, ...)  
    $ pred   :function (object, x)  
      $ score  :function (x, y)  
        $ filter :function (score, x, y)  
          rfStats : function (x)  
            RMSE : function (pred, obs, na.rm = FALSE)  
              safs : function (x, ...)  
                safs_initial : function (vars, prob = 0.2, ...)  
                  safs_perturb : function (x, vars, number = floor(length(x) * 0.01) + 1)  
                    safs_prob : function (old, new, iteration = 1)  
                      safsControl : function (functions = NULL, method = "repeatedcv", metric = NULL, maximize = NULL, 
                                              number = ifelse(grepl("cv", method), 10, 25), repeats = ifelse(grepl("cv", method), 
                                                                                                             1, 5), verbose = FALSE, returnResamp = "final", p = 0.75, index = NULL, indexOut = NULL, 
                                              seeds = NULL, holdout = 0, improve = Inf, allowParallel = TRUE)  
                        sbf : function (x, ...)  
                          sbfControl : function (functions = NULL, method = "boot", saveDetails = FALSE, number = ifelse(method %in% 
                                                                                                                           c("cv", "repeatedcv"), 10, 25), repeats = ifelse(method %in% c("cv", "repeatedcv"), 
                                                                                                                                                                            1, number), verbose = FALSE, returnResamp = "final", p = 0.75, index = NULL, indexOut = NULL, 
                                                 timingSamps = 0, seeds = NA, allowParallel = TRUE, multivariate = FALSE)  
                            sbfIter : function (x, y, testX, testY, testPerf = NULL, sbfControl = sbfControl(), ...)  
                              sensitivity : function (data, ...)  
                                SLC14_1 : function (n = 100, noiseVars = 0, corrVars = 0, corrType = "AR1", corrValue = 0)  
                                  SLC14_2 : function (n = 100, noiseVars = 0, corrVars = 0, corrType = "AR1", corrValue = 0)  
                                    sortImp : function (object, top)  
                                      spatialSign : function (x, ...)  
                                        specificity : function (data, ...)  
                                          splsda : function (x, ...)  
                                            sumDiss : function (u)  
                                              summary.bagEarth : function (object, ...)  
                                                svmBag : List of 3
$ fit      :function (x, y, ...)  
  $ pred     :function (object, x)  
    $ aggregate:function (x, type = "class")  
      thresholder : function (x, threshold, final = TRUE, statistics = "all")  
        tolerance : function (x, metric, tol = 1.5, maximize)  
          train : function (x, ...)  
            trainControl : function (method = "boot", number = ifelse(grepl("cv", method), 10, 25), repeats = ifelse(grepl("[d_]cv$", 
                                                                                                                           method), 1, NA), p = 0.75, search = "grid", initialWindow = NULL, horizon = 1, 
                                     fixedWindow = TRUE, skip = 0, verboseIter = FALSE, returnData = TRUE, returnResamp = "final", 
                                     savePredictions = FALSE, classProbs = FALSE, summaryFunction = defaultSummary, 
                                     selectionFunction = "best", preProcOptions = list(thresh = 0.95, ICAcomp = 3, 
                                                                                       k = 5, freqCut = 95/5, uniqueCut = 10, cutoff = 0.9), sampling = NULL, index = NULL, 
                                     indexOut = NULL, indexFinal = NULL, timingSamps = 0, predictionBounds = rep(FALSE, 
                                                                                                                 2), seeds = NA, adaptive = list(min = 5, alpha = 0.05, method = "gls", complete = TRUE), 
                                     trim = FALSE, allowParallel = TRUE)  
              treebagFuncs : List of 6
$ summary   :function (data, lev = NULL, model = NULL)  
  $ fit       :function (x, y, first, last, ...)  
    $ pred      :function (object, x)  
      $ rank      :function (object, x, y)  
        $ selectSize:function (x, metric, maximize)  
          $ selectVar :function (y, size)  
            treebagGA : List of 9
$ fit           :function (x, y, lev = NULL, last = FALSE, ...)  
  $ pred          :function (object, x)  
    $ fitness_intern:function (object, x, y, maximize, p)  
      $ fitness_extern:function (data, lev = NULL, model = NULL)  
        $ initial       :function (vars, popSize, ...)  
          $ selection     :function (population, fitness, r = NULL, q = NULL, ...)  
            $ crossover     :function (population, fitness, parents, ...)  
              $ mutation      :function (population, parent, ...)  
                $ selectIter    :function (x, metric, maximize)  
                  treebagSA : List of 8
$ fit           :function (x, y, lev = NULL, last = FALSE, ...)  
  $ pred          :function (object, x)  
    $ fitness_intern:function (object, x, y, maximize, p)  
      $ fitness_extern:function (data, lev = NULL, model = NULL)  
        $ initial       :function (vars, prob = 0.2, ...)  
          $ perturb       :function (x, vars, number = floor(length(x) * 0.01) + 1)  
            $ prob          :function (old, new, iteration = 1)  
              $ selectIter    :function (x, metric, maximize)  
                treebagSBF : List of 5
$ summary:function (data, lev = NULL, model = NULL)  
  $ fit    :function (x, y, ...)  
    $ pred   :function (object, x)  
      $ score  :function (x, y)  
        $ filter :function (score, x, y)  
          twoClassSim : function (n = 100, intercept = -5, linearVars = 10, noiseVars = 0, corrVars = 0, corrType = "AR1", 
                                  corrValue = 0, mislabel = 0, ordinal = FALSE)  
            twoClassSummary : function (data, lev = NULL, model = NULL)  
              upSample : function (x, y, list = FALSE, yname = "Class")  
                var_seq : function (p, classification = FALSE, len = 3)  
                  varImp : function (object, ...)  
                    well_numbered : function (prefix, items)  
                      > 
library(checkmate)
"x"
"%??% : function (lhs, rhs)  "
"allMissing : function (x)  "
"anyInfinite : function (x)  "
"anyMissing : function (x)  "
"anyNaN : function (x)  "
"asCount : function (x, na.ok = FALSE, positive = FALSE, tol = sqrt(.Machine$double.eps), .var.name = vname(x))  "
"asInt : function (x, na.ok = FALSE, lower = -Inf, upper = Inf, tol = sqrt(.Machine$double.eps), "
"    .var.name = vname(x))  "
"asInteger : function (x, tol = sqrt(.Machine$double.eps), lower = -Inf, upper = Inf, any.missing = TRUE, "
"    all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, "
"    sorted = FALSE, names = NULL, .var.name = vname(x))  "
"assert : function (..., combine = \"or\", .var.name = NULL, add = NULL)  "
"assert_access : function (x, access = \"\", .var.name = vname(x), add = NULL)  "
"assert_array : function (x, mode = NULL, any.missing = TRUE, d = NULL, min.d = NULL, max.d = NULL, "
"    null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_atomic : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, .var.name = vname(x), add = NULL)  "
"assert_atomic_vector : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, .var.name = vname(x), add = NULL)  "
"assert_character : function (x, n.chars = NULL, min.chars = NULL, max.chars = NULL, pattern = NULL, fixed = NULL, "
"    ignore.case = FALSE, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, "
"    max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, typed.missing = FALSE, "
"    null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_choice : function (x, choices, null.ok = FALSE, fmatch = FALSE, .var.name = vname(x), add = NULL)  "
"assert_class : function (x, classes, ordered = FALSE, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_complex : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE, .var.name = vname(x), "
"    add = NULL)  "
"assert_count : function (x, na.ok = FALSE, positive = FALSE, tol = sqrt(.Machine$double.eps), null.ok = FALSE, "
"    coerce = FALSE, .var.name = vname(x), add = NULL)  "
"assert_data_frame : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_data_table : function (x, key = NULL, index = NULL, types = character(0L), any.missing = TRUE, "
"    all.missing = TRUE, min.rows = NULL, max.rows = NULL, min.cols = NULL, max.cols = NULL, "
"    nrows = NULL, ncols = NULL, row.names = NULL, col.names = NULL, null.ok = FALSE, "
"    .var.name = vname(x), add = NULL)  "
"assert_date : function (x, lower = NULL, upper = NULL, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, null.ok = FALSE, .var.name = vname(x), "
"    add = NULL)  "
"assert_directory : function (x, access = \"\", .var.name = vname(x), add = NULL)  "
"assert_directory_exists : function (x, access = \"\", .var.name = vname(x), add = NULL)  "
"assert_disjunct : function (x, y, fmatch = FALSE, .var.name = vname(x), add = NULL)  "
"assert_double : function (x, lower = -Inf, upper = Inf, finite = FALSE, any.missing = TRUE, all.missing = TRUE, "
"    len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_environment : function (x, contains = character(0L), null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_factor : function (x, levels = NULL, ordered = NA, empty.levels.ok = TRUE, any.missing = TRUE, "
"    all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, n.levels = NULL, "
"    min.levels = NULL, max.levels = NULL, unique = FALSE, names = NULL, null.ok = FALSE, "
"    .var.name = vname(x), add = NULL)  "
"assert_false : function (x, na.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_file : function (x, access = \"\", extension = NULL, .var.name = vname(x), add = NULL)  "
"assert_file_exists : function (x, access = \"\", extension = NULL, .var.name = vname(x), add = NULL)  "
"assert_flag : function (x, na.ok = FALSE, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_formula : function (x, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_function : function (x, args = NULL, ordered = FALSE, nargs = NULL, null.ok = FALSE, .var.name = vname(x), "
"    add = NULL)  "
"assert_int : function (x, na.ok = FALSE, lower = -Inf, upper = Inf, tol = sqrt(.Machine$double.eps), "
"    null.ok = FALSE, coerce = FALSE, .var.name = vname(x), add = NULL)  "
"assert_integer : function (x, lower = -Inf, upper = Inf, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_integerish : function (x, tol = sqrt(.Machine$double.eps), lower = -Inf, upper = Inf, any.missing = TRUE, "
"    all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, "
"    sorted = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE, coerce = FALSE, "
"    .var.name = vname(x), add = NULL)  "
"assert_list : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, names = NULL, null.ok = FALSE, "
"    .var.name = vname(x), add = NULL)  "
"assert_logical : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE, .var.name = vname(x), "
"    add = NULL)  "
"assert_matrix : function (x, mode = NULL, any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_multi_class : function (x, classes, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_named : function (x, type = \"named\", .var.name = vname(x), add = NULL)  "
"assert_names : function (x, type = \"named\", subset.of = NULL, must.include = NULL, permutation.of = NULL, "
"    identical.to = NULL, disjunct.from = NULL, what = \"names\", .var.name = vname(x), "
"    add = NULL)  "
"assert_null : function (x, .var.name = vname(x), add = NULL)  "
"assert_number : function (x, na.ok = FALSE, lower = -Inf, upper = Inf, finite = FALSE, null.ok = FALSE, "
"    .var.name = vname(x), add = NULL)  "
"assert_numeric : function (x, lower = -Inf, upper = Inf, finite = FALSE, any.missing = TRUE, all.missing = TRUE, "
"    len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_os : function (os, add = NULL, .var.name = NULL)  "
"assert_path_for_output : function (x, overwrite = FALSE, extension = NULL, .var.name = vname(x), add = NULL)  "
"assert_posixct : function (x, lower = NULL, upper = NULL, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, null.ok = FALSE, "
"    .var.name = vname(x), add = NULL)  "
"assert_r6 : function (x, classes = NULL, ordered = FALSE, cloneable = NULL, public = NULL, private = NULL, "
"    null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_raw : function (x, len = NULL, min.len = NULL, max.len = NULL, names = NULL, null.ok = FALSE, "
"    .var.name = vname(x), add = NULL)  "
"assert_scalar : function (x, na.ok = FALSE, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_scalar_na : function (x, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_set_equal : function (x, y, ordered = FALSE, fmatch = FALSE, .var.name = vname(x), add = NULL)  "
"assert_string : function (x, na.ok = FALSE, n.chars = NULL, min.chars = NULL, max.chars = NULL, pattern = NULL, "
"    fixed = NULL, ignore.case = FALSE, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_subset : function (x, choices, empty.ok = TRUE, fmatch = FALSE, .var.name = vname(x), add = NULL)  "
"assert_tibble : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_true : function (x, na.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assert_vector : function (x, strict = FALSE, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, "
"    max.len = NULL, unique = FALSE, names = NULL, null.ok = FALSE, .var.name = vname(x), "
"    add = NULL)  "
"assertAccess : function (x, access = \"\", .var.name = vname(x), add = NULL)  "
"assertArray : function (x, mode = NULL, any.missing = TRUE, d = NULL, min.d = NULL, max.d = NULL, "
"    null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertAtomic : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, .var.name = vname(x), add = NULL)  "
"assertAtomicVector : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, .var.name = vname(x), add = NULL)  "
"assertCharacter : function (x, n.chars = NULL, min.chars = NULL, max.chars = NULL, pattern = NULL, fixed = NULL, "
"    ignore.case = FALSE, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, "
"    max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, typed.missing = FALSE, "
"    null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertChoice : function (x, choices, null.ok = FALSE, fmatch = FALSE, .var.name = vname(x), add = NULL)  "
"assertClass : function (x, classes, ordered = FALSE, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertComplex : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE, .var.name = vname(x), "
"    add = NULL)  "
"assertCount : function (x, na.ok = FALSE, positive = FALSE, tol = sqrt(.Machine$double.eps), null.ok = FALSE, "
"    coerce = FALSE, .var.name = vname(x), add = NULL)  "
"assertDataFrame : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertDataTable : function (x, key = NULL, index = NULL, types = character(0L), any.missing = TRUE, "
"    all.missing = TRUE, min.rows = NULL, max.rows = NULL, min.cols = NULL, max.cols = NULL, "
"    nrows = NULL, ncols = NULL, row.names = NULL, col.names = NULL, null.ok = FALSE, "
"    .var.name = vname(x), add = NULL)  "
"assertDate : function (x, lower = NULL, upper = NULL, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, null.ok = FALSE, .var.name = vname(x), "
"    add = NULL)  "
"assertDirectory : function (x, access = \"\", .var.name = vname(x), add = NULL)  "
"assertDirectoryExists : function (x, access = \"\", .var.name = vname(x), add = NULL)  "
"assertDisjunct : function (x, y, fmatch = FALSE, .var.name = vname(x), add = NULL)  "
"assertDouble : function (x, lower = -Inf, upper = Inf, finite = FALSE, any.missing = TRUE, all.missing = TRUE, "
"    len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertEnvironment : function (x, contains = character(0L), null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertFactor : function (x, levels = NULL, ordered = NA, empty.levels.ok = TRUE, any.missing = TRUE, "
"    all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, n.levels = NULL, "
"    min.levels = NULL, max.levels = NULL, unique = FALSE, names = NULL, null.ok = FALSE, "
"    .var.name = vname(x), add = NULL)  "
"assertFALSE : function (x, na.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertFile : function (x, access = \"\", extension = NULL, .var.name = vname(x), add = NULL)  "
"assertFileExists : function (x, access = \"\", extension = NULL, .var.name = vname(x), add = NULL)  "
"assertFlag : function (x, na.ok = FALSE, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertFormula : function (x, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertFunction : function (x, args = NULL, ordered = FALSE, nargs = NULL, null.ok = FALSE, .var.name = vname(x), "
"    add = NULL)  "
"assertInt : function (x, na.ok = FALSE, lower = -Inf, upper = Inf, tol = sqrt(.Machine$double.eps), "
"    null.ok = FALSE, coerce = FALSE, .var.name = vname(x), add = NULL)  "
"assertInteger : function (x, lower = -Inf, upper = Inf, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertIntegerish : function (x, tol = sqrt(.Machine$double.eps), lower = -Inf, upper = Inf, any.missing = TRUE, "
"    all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, "
"    sorted = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE, coerce = FALSE, "
"    .var.name = vname(x), add = NULL)  "
"assertList : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, names = NULL, null.ok = FALSE, "
"    .var.name = vname(x), add = NULL)  "
"assertLogical : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE, .var.name = vname(x), "
"    add = NULL)  "
"assertMatrix : function (x, mode = NULL, any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertMultiClass : function (x, classes, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertNamed : function (x, type = \"named\", .var.name = vname(x), add = NULL)  "
"assertNames : function (x, type = \"named\", subset.of = NULL, must.include = NULL, permutation.of = NULL, "
"    identical.to = NULL, disjunct.from = NULL, what = \"names\", .var.name = vname(x), "
"    add = NULL)  "
"assertNull : function (x, .var.name = vname(x), add = NULL)  "
"assertNumber : function (x, na.ok = FALSE, lower = -Inf, upper = Inf, finite = FALSE, null.ok = FALSE, "
"    .var.name = vname(x), add = NULL)  "
"assertNumeric : function (x, lower = -Inf, upper = Inf, finite = FALSE, any.missing = TRUE, all.missing = TRUE, "
"    len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertOS : function (os, add = NULL, .var.name = NULL)  "
"assertPathForOutput : function (x, overwrite = FALSE, extension = NULL, .var.name = vname(x), add = NULL)  "
"assertPOSIXct : function (x, lower = NULL, upper = NULL, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, null.ok = FALSE, "
"    .var.name = vname(x), add = NULL)  "
"assertR6 : function (x, classes = NULL, ordered = FALSE, cloneable = NULL, public = NULL, private = NULL, "
"    null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertRaw : function (x, len = NULL, min.len = NULL, max.len = NULL, names = NULL, null.ok = FALSE, "
"    .var.name = vname(x), add = NULL)  "
"assertScalar : function (x, na.ok = FALSE, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertScalarNA : function (x, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertSetEqual : function (x, y, ordered = FALSE, fmatch = FALSE, .var.name = vname(x), add = NULL)  "
"assertString : function (x, na.ok = FALSE, n.chars = NULL, min.chars = NULL, max.chars = NULL, pattern = NULL, "
"    fixed = NULL, ignore.case = FALSE, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertSubset : function (x, choices, empty.ok = TRUE, fmatch = FALSE, .var.name = vname(x), add = NULL)  "
"assertTibble : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertTRUE : function (x, na.ok = FALSE, .var.name = vname(x), add = NULL)  "
"assertVector : function (x, strict = FALSE, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, "
"    max.len = NULL, unique = FALSE, names = NULL, null.ok = FALSE, .var.name = vname(x), "
"    add = NULL)  "
"check_access : function (x, access = \"\")  "
"check_array : function (x, mode = NULL, any.missing = TRUE, d = NULL, min.d = NULL, max.d = NULL, "
"    null.ok = FALSE)  "
"check_atomic : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL)  "
"check_atomic_vector : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL)  "
"check_character : function (x, n.chars = NULL, min.chars = NULL, max.chars = NULL, pattern = NULL, fixed = NULL, "
"    ignore.case = FALSE, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, "
"    max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, typed.missing = FALSE, "
"    null.ok = FALSE)  "
"check_choice : function (x, choices, null.ok = FALSE, fmatch = FALSE)  "
"check_class : function (x, classes, ordered = FALSE, null.ok = FALSE)  "
"check_complex : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE)  "
"check_count : function (x, na.ok = FALSE, positive = FALSE, tol = sqrt(.Machine$double.eps), null.ok = FALSE)  "
"check_data_frame : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE)  "
"check_data_table : function (x, key = NULL, index = NULL, types = character(0L), any.missing = TRUE, "
"    all.missing = TRUE, min.rows = NULL, max.rows = NULL, min.cols = NULL, max.cols = NULL, "
"    nrows = NULL, ncols = NULL, row.names = NULL, col.names = NULL, null.ok = FALSE)  "
"check_date : function (x, lower = NULL, upper = NULL, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, null.ok = FALSE)  "
"check_directory_exists : function (x, access = \"\")  "
"check_disjunct : function (x, y, fmatch = FALSE)  "
"check_double : function (x, lower = -Inf, upper = Inf, finite = FALSE, any.missing = TRUE, all.missing = TRUE, "
"    len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE)  "
"check_environment : function (x, contains = character(0L), null.ok = FALSE)  "
"check_factor : function (x, levels = NULL, ordered = NA, empty.levels.ok = TRUE, any.missing = TRUE, "
"    all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, n.levels = NULL, "
"    min.levels = NULL, max.levels = NULL, unique = FALSE, names = NULL, null.ok = FALSE)  "
"check_false : function (x, na.ok = FALSE)  "
"check_file_exists : function (x, access = \"\", extension = NULL)  "
"check_flag : function (x, na.ok = FALSE, null.ok = FALSE)  "
"check_formula : function (x, null.ok = FALSE)  "
"check_function : function (x, args = NULL, ordered = FALSE, nargs = NULL, null.ok = FALSE)  "
"check_int : function (x, na.ok = FALSE, lower = -Inf, upper = Inf, tol = sqrt(.Machine$double.eps), "
"    null.ok = FALSE)  "
"check_integer : function (x, lower = -Inf, upper = Inf, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE)  "
"check_integerish : function (x, tol = sqrt(.Machine$double.eps), lower = -Inf, upper = Inf, any.missing = TRUE, "
"    all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, "
"    sorted = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE)  "
"check_list : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, names = NULL, null.ok = FALSE)  "
"check_logical : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE)  "
"check_matrix : function (x, mode = NULL, any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE)  "
"check_multi_class : function (x, classes, null.ok = FALSE)  "
"check_named : function (x, type = \"named\")  "
"check_names : function (x, type = \"named\", subset.of = NULL, must.include = NULL, permutation.of = NULL, "
"    identical.to = NULL, disjunct.from = NULL, what = \"names\")  "
"check_null : function (x)  "
"check_number : function (x, na.ok = FALSE, lower = -Inf, upper = Inf, finite = FALSE, null.ok = FALSE)  "
"check_numeric : function (x, lower = -Inf, upper = Inf, finite = FALSE, any.missing = TRUE, all.missing = TRUE, "
"    len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE)  "
"check_os : function (os)  "
"check_path_for_output : function (x, overwrite = FALSE, extension = NULL)  "
"check_posixct : function (x, lower = NULL, upper = NULL, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, null.ok = FALSE)  "
"check_r6 : function (x, classes = NULL, ordered = FALSE, cloneable = NULL, public = NULL, private = NULL, "
"    null.ok = FALSE)  "
"check_raw : function (x, len = NULL, min.len = NULL, max.len = NULL, names = NULL, null.ok = FALSE)  "
"check_scalar : function (x, na.ok = FALSE, null.ok = FALSE)  "
"check_scalar_na : function (x, null.ok = FALSE)  "
"check_set_equal : function (x, y, ordered = FALSE, fmatch = FALSE)  "
"check_string : function (x, na.ok = FALSE, n.chars = NULL, min.chars = NULL, max.chars = NULL, pattern = NULL, "
"    fixed = NULL, ignore.case = FALSE, null.ok = FALSE)  "
"check_subset : function (x, choices, empty.ok = TRUE, fmatch = FALSE)  "
"check_tibble : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE)  "
"check_true : function (x, na.ok = FALSE)  "
"check_vector : function (x, strict = FALSE, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, "
"    max.len = NULL, unique = FALSE, names = NULL, null.ok = FALSE)  "
"checkAccess : function (x, access = \"\")  "
"checkArray : function (x, mode = NULL, any.missing = TRUE, d = NULL, min.d = NULL, max.d = NULL, "
"    null.ok = FALSE)  "
"checkAtomic : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL)  "
"checkAtomicVector : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL)  "
"checkCharacter : function (x, n.chars = NULL, min.chars = NULL, max.chars = NULL, pattern = NULL, fixed = NULL, "
"    ignore.case = FALSE, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, "
"    max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, typed.missing = FALSE, "
"    null.ok = FALSE)  "
"checkChoice : function (x, choices, null.ok = FALSE, fmatch = FALSE)  "
"checkClass : function (x, classes, ordered = FALSE, null.ok = FALSE)  "
"checkComplex : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE)  "
"checkCount : function (x, na.ok = FALSE, positive = FALSE, tol = sqrt(.Machine$double.eps), null.ok = FALSE)  "
"checkDataFrame : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE)  "
"checkDataTable : function (x, key = NULL, index = NULL, types = character(0L), any.missing = TRUE, "
"    all.missing = TRUE, min.rows = NULL, max.rows = NULL, min.cols = NULL, max.cols = NULL, "
"    nrows = NULL, ncols = NULL, row.names = NULL, col.names = NULL, null.ok = FALSE)  "
"checkDate : function (x, lower = NULL, upper = NULL, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, null.ok = FALSE)  "
"checkDirectory : function (x, access = \"\")  "
"checkDirectoryExists : function (x, access = \"\")  "
"checkDisjunct : function (x, y, fmatch = FALSE)  "
"checkDouble : function (x, lower = -Inf, upper = Inf, finite = FALSE, any.missing = TRUE, all.missing = TRUE, "
"    len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE)  "
"checkEnvironment : function (x, contains = character(0L), null.ok = FALSE)  "
"checkFactor : function (x, levels = NULL, ordered = NA, empty.levels.ok = TRUE, any.missing = TRUE, "
"    all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, n.levels = NULL, "
"    min.levels = NULL, max.levels = NULL, unique = FALSE, names = NULL, null.ok = FALSE)  "
"checkFALSE : function (x, na.ok = FALSE)  "
"checkFile : function (x, access = \"\", extension = NULL)  "
"checkFileExists : function (x, access = \"\", extension = NULL)  "
"checkFlag : function (x, na.ok = FALSE, null.ok = FALSE)  "
"checkFormula : function (x, null.ok = FALSE)  "
"checkFunction : function (x, args = NULL, ordered = FALSE, nargs = NULL, null.ok = FALSE)  "
"checkInt : function (x, na.ok = FALSE, lower = -Inf, upper = Inf, tol = sqrt(.Machine$double.eps), "
"    null.ok = FALSE)  "
"checkInteger : function (x, lower = -Inf, upper = Inf, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE)  "
"checkIntegerish : function (x, tol = sqrt(.Machine$double.eps), lower = -Inf, upper = Inf, any.missing = TRUE, "
"    all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, "
"    sorted = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE)  "
"checkList : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, names = NULL, null.ok = FALSE)  "
"checkLogical : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE)  "
"checkMatrix : function (x, mode = NULL, any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE)  "
"checkMultiClass : function (x, classes, null.ok = FALSE)  "
"checkNamed : function (x, type = \"named\")  "
"checkNames : function (x, type = \"named\", subset.of = NULL, must.include = NULL, permutation.of = NULL, "
"    identical.to = NULL, disjunct.from = NULL, what = \"names\")  "
"checkNull : function (x)  "
"checkNumber : function (x, na.ok = FALSE, lower = -Inf, upper = Inf, finite = FALSE, null.ok = FALSE)  "
"checkNumeric : function (x, lower = -Inf, upper = Inf, finite = FALSE, any.missing = TRUE, all.missing = TRUE, "
"    len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE)  "
"checkOS : function (os)  "
"checkPathForOutput : function (x, overwrite = FALSE, extension = NULL)  "
"checkPOSIXct : function (x, lower = NULL, upper = NULL, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, null.ok = FALSE)  "
"checkR6 : function (x, classes = NULL, ordered = FALSE, cloneable = NULL, public = NULL, private = NULL, "
"    null.ok = FALSE)  "
"checkRaw : function (x, len = NULL, min.len = NULL, max.len = NULL, names = NULL, null.ok = FALSE)  "
"checkScalar : function (x, na.ok = FALSE, null.ok = FALSE)  "
"checkScalarNA : function (x, null.ok = FALSE)  "
"checkSetEqual : function (x, y, ordered = FALSE, fmatch = FALSE)  "
"checkString : function (x, na.ok = FALSE, n.chars = NULL, min.chars = NULL, max.chars = NULL, pattern = NULL, "
"    fixed = NULL, ignore.case = FALSE, null.ok = FALSE)  "
"checkSubset : function (x, choices, empty.ok = TRUE, fmatch = FALSE)  "
"checkTibble : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE)  "
"checkTRUE : function (x, na.ok = FALSE)  "
"checkVector : function (x, strict = FALSE, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, "
"    max.len = NULL, unique = FALSE, names = NULL, null.ok = FALSE)  "
"expect_access : function (x, access = \"\", info = NULL, label = vname(x))  "
"expect_array : function (x, mode = NULL, any.missing = TRUE, d = NULL, min.d = NULL, max.d = NULL, "
"    null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_atomic : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, info = NULL, label = vname(x))  "
"expect_atomic_vector : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, info = NULL, label = vname(x))  "
"expect_character : function (x, n.chars = NULL, min.chars = NULL, max.chars = NULL, pattern = NULL, fixed = NULL, "
"    ignore.case = FALSE, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, "
"    max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, typed.missing = FALSE, "
"    null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_choice : function (x, choices, null.ok = FALSE, fmatch = FALSE, info = NULL, label = vname(x))  "
"expect_class : function (x, classes, ordered = FALSE, null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_complex : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE, info = NULL, "
"    label = vname(x))  "
"expect_count : function (x, na.ok = FALSE, positive = FALSE, tol = sqrt(.Machine$double.eps), null.ok = FALSE, "
"    info = NULL, label = vname(x))  "
"expect_data_frame : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_data_table : function (x, key = NULL, index = NULL, types = character(0L), any.missing = TRUE, "
"    all.missing = TRUE, min.rows = NULL, max.rows = NULL, min.cols = NULL, max.cols = NULL, "
"    nrows = NULL, ncols = NULL, row.names = NULL, col.names = NULL, null.ok = FALSE, "
"    info = NULL, label = vname(x))  "
"expect_date : function (x, lower = NULL, upper = NULL, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, null.ok = FALSE, info = NULL, "
"    label = vname(x))  "
"expect_directory : function (x, access = \"\", info = NULL, label = vname(x))  "
"expect_directory_exists : function (x, access = \"\", info = NULL, label = vname(x))  "
"expect_disjunct : function (x, y, fmatch = FALSE, info = NULL, label = vname(x))  "
"expect_double : function (x, lower = -Inf, upper = Inf, finite = FALSE, any.missing = TRUE, all.missing = TRUE, "
"    len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_environment : function (x, contains = character(0L), null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_factor : function (x, levels = NULL, ordered = NA, empty.levels.ok = TRUE, any.missing = TRUE, "
"    all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, n.levels = NULL, "
"    min.levels = NULL, max.levels = NULL, unique = FALSE, names = NULL, null.ok = FALSE, "
"    info = NULL, label = vname(x))  "
"expect_file : function (x, access = \"\", extension = NULL, info = NULL, label = vname(x))  "
"expect_file_exists : function (x, access = \"\", extension = NULL, info = NULL, label = vname(x))  "
"expect_flag : function (x, na.ok = FALSE, null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_formula : function (x, null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_function : function (x, args = NULL, ordered = FALSE, nargs = NULL, null.ok = FALSE, info = NULL, "
"    label = vname(x))  "
"expect_int : function (x, na.ok = FALSE, lower = -Inf, upper = Inf, tol = sqrt(.Machine$double.eps), "
"    null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_integer : function (x, lower = -Inf, upper = Inf, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_integerish : function (x, tol = sqrt(.Machine$double.eps), lower = -Inf, upper = Inf, any.missing = TRUE, "
"    all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, "
"    sorted = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE, info = NULL, "
"    label = vname(x))  "
"expect_list : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, names = NULL, null.ok = FALSE, "
"    info = NULL, label = vname(x))  "
"expect_logical : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE, info = NULL, "
"    label = vname(x))  "
"expect_matrix : function (x, mode = NULL, any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_multi_class : function (x, classes, null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_names : function (x, type = \"named\", subset.of = NULL, must.include = NULL, permutation.of = NULL, "
"    identical.to = NULL, disjunct.from = NULL, what = \"names\", info = NULL, label = vname(x))  "
"expect_number : function (x, na.ok = FALSE, lower = -Inf, upper = Inf, finite = FALSE, null.ok = FALSE, "
"    info = NULL, label = vname(x))  "
"expect_numeric : function (x, lower = -Inf, upper = Inf, finite = FALSE, any.missing = TRUE, all.missing = TRUE, "
"    len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_os : function (os, info = NULL, label = NULL)  "
"expect_path_for_output : function (x, overwrite = FALSE, extension = NULL, info = NULL, label = vname(x))  "
"expect_posixct : function (x, lower = NULL, upper = NULL, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, null.ok = FALSE, "
"    info = NULL, label = vname(x))  "
"expect_r6 : function (x, classes = NULL, ordered = FALSE, cloneable = NULL, public = NULL, private = NULL, "
"    null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_raw : function (x, len = NULL, min.len = NULL, max.len = NULL, names = NULL, null.ok = FALSE, "
"    info = NULL, label = vname(x))  "
"expect_scalar : function (x, na.ok = FALSE, null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_scalar_na : function (x, null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_set_equal : function (x, y, ordered = FALSE, fmatch = FALSE, info = NULL, label = vname(x))  "
"expect_string : function (x, na.ok = FALSE, n.chars = NULL, min.chars = NULL, max.chars = NULL, pattern = NULL, "
"    fixed = NULL, ignore.case = FALSE, null.ok = FALSE, info = NULL, label = vname(x))  "
"expect_subset : function (x, choices, empty.ok = TRUE, fmatch = FALSE, info = NULL, label = vname(x))  "
"expect_tibble : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE, info = NULL, label = vname(x))  "
"makeAssertCollection : function ()  "
"makeAssertion : function (x, res, var.name, collection)  "
"makeAssertionFunction : function (check.fun, c.fun = NULL, use.namespace = TRUE, coerce = FALSE, env = parent.frame())  "
"makeExpectation : function (x, res, info, label)  "
"makeExpectationFunction : function (check.fun, c.fun = NULL, use.namespace = FALSE, env = parent.frame())  "
"makeTest : function (res)  "
"makeTestFunction : function (check.fun, c.fun = NULL, env = parent.frame())  "
"matchArg : function (x, choices, several.ok = FALSE, .var.name = vname(x), add = NULL)  "
"qassert : function (x, rules, .var.name = vname(x))  "
"qassertr : function (x, rules, .var.name = vname(x))  "
"qexpect : function (x, rules, info = NULL, label = vname(x))  "
"qexpectr : function (x, rules, info = NULL, label = vname(x))  "
"qtest : function (x, rules)  "
"qtestr : function (x, rules, depth = 1L)  "
"register_test_backend : function (name)  "
"reportAssertions : function (collection)  "
"test_access : function (x, access = \"\")  "
"test_array : function (x, mode = NULL, any.missing = TRUE, d = NULL, min.d = NULL, max.d = NULL, "
"    null.ok = FALSE)  "
"test_atomic : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL)  "
"test_atomic_vector : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL)  "
"test_character : function (x, n.chars = NULL, min.chars = NULL, max.chars = NULL, pattern = NULL, fixed = NULL, "
"    ignore.case = FALSE, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, "
"    max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, typed.missing = FALSE, "
"    null.ok = FALSE)  "
"test_choice : function (x, choices, null.ok = FALSE, fmatch = FALSE)  "
"test_class : function (x, classes, ordered = FALSE, null.ok = FALSE)  "
"test_complex : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE)  "
"test_count : function (x, na.ok = FALSE, positive = FALSE, tol = sqrt(.Machine$double.eps), null.ok = FALSE)  "
"test_data_frame : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE)  "
"test_data_table : function (x, key = NULL, index = NULL, types = character(0L), any.missing = TRUE, "
"    all.missing = TRUE, min.rows = NULL, max.rows = NULL, min.cols = NULL, max.cols = NULL, "
"    nrows = NULL, ncols = NULL, row.names = NULL, col.names = NULL, null.ok = FALSE)  "
"test_date : function (x, lower = NULL, upper = NULL, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, null.ok = FALSE)  "
"test_directory : function (x, access = \"\")  "
"test_directory_exists : function (x, access = \"\")  "
"test_disjunct : function (x, y, fmatch = FALSE)  "
"test_double : function (x, lower = -Inf, upper = Inf, finite = FALSE, any.missing = TRUE, all.missing = TRUE, "
"    len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE)  "
"test_environment : function (x, contains = character(0L), null.ok = FALSE)  "
"test_factor : function (x, levels = NULL, ordered = NA, empty.levels.ok = TRUE, any.missing = TRUE, "
"    all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, n.levels = NULL, "
"    min.levels = NULL, max.levels = NULL, unique = FALSE, names = NULL, null.ok = FALSE)  "
"test_false : function (x, na.ok = FALSE)  "
"test_file_exists : function (x, access = \"\", extension = NULL)  "
"test_flag : function (x, na.ok = FALSE, null.ok = FALSE)  "
"test_formula : function (x, null.ok = FALSE)  "
"test_function : function (x, args = NULL, ordered = FALSE, nargs = NULL, null.ok = FALSE)  "
"test_int : function (x, na.ok = FALSE, lower = -Inf, upper = Inf, tol = sqrt(.Machine$double.eps), "
"    null.ok = FALSE)  "
"test_integer : function (x, lower = -Inf, upper = Inf, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE)  "
"test_integerish : function (x, tol = sqrt(.Machine$double.eps), lower = -Inf, upper = Inf, any.missing = TRUE, "
"    all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, "
"    sorted = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE)  "
"test_list : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, names = NULL, null.ok = FALSE)  "
"test_logical : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE)  "
"test_matrix : function (x, mode = NULL, any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE)  "
"test_multi_class : function (x, classes, null.ok = FALSE)  "
"test_named : function (x, type = \"named\")  "
"test_names : function (x, type = \"named\", subset.of = NULL, must.include = NULL, permutation.of = NULL, "
"    identical.to = NULL, disjunct.from = NULL, what = \"names\")  "
"test_null : function (x)  "
"test_number : function (x, na.ok = FALSE, lower = -Inf, upper = Inf, finite = FALSE, null.ok = FALSE)  "
"test_numeric : function (x, lower = -Inf, upper = Inf, finite = FALSE, any.missing = TRUE, all.missing = TRUE, "
"    len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE)  "
"test_os : function (os)  "
"test_path_for_output : function (x, overwrite = FALSE, extension = NULL)  "
"test_posixct : function (x, lower = NULL, upper = NULL, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, null.ok = FALSE)  "
"test_r6 : function (x, classes = NULL, ordered = FALSE, cloneable = NULL, public = NULL, private = NULL, "
"    null.ok = FALSE)  "
"test_raw : function (x, len = NULL, min.len = NULL, max.len = NULL, names = NULL, null.ok = FALSE)  "
"test_scalar : function (x, na.ok = FALSE, null.ok = FALSE)  "
"test_scalar_na : function (x, null.ok = FALSE)  "
"test_set_equal : function (x, y, ordered = FALSE, fmatch = FALSE)  "
"test_string : function (x, na.ok = FALSE, n.chars = NULL, min.chars = NULL, max.chars = NULL, pattern = NULL, "
"    fixed = NULL, ignore.case = FALSE, null.ok = FALSE)  "
"test_subset : function (x, choices, empty.ok = TRUE, fmatch = FALSE)  "
"test_tibble : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE)  "
"test_true : function (x, na.ok = FALSE)  "
"test_vector : function (x, strict = FALSE, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, "
"    max.len = NULL, unique = FALSE, names = NULL, null.ok = FALSE)  "
"testAccess : function (x, access = \"\")  "
"testArray : function (x, mode = NULL, any.missing = TRUE, d = NULL, min.d = NULL, max.d = NULL, "
"    null.ok = FALSE)  "
"testAtomic : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL)  "
"testAtomicVector : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL)  "
"testCharacter : function (x, n.chars = NULL, min.chars = NULL, max.chars = NULL, pattern = NULL, fixed = NULL, "
"    ignore.case = FALSE, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, "
"    max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, typed.missing = FALSE, "
"    null.ok = FALSE)  "
"testChoice : function (x, choices, null.ok = FALSE, fmatch = FALSE)  "
"testClass : function (x, classes, ordered = FALSE, null.ok = FALSE)  "
"testComplex : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE)  "
"testCount : function (x, na.ok = FALSE, positive = FALSE, tol = sqrt(.Machine$double.eps), null.ok = FALSE)  "
"testDataFrame : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE)  "
"testDataTable : function (x, key = NULL, index = NULL, types = character(0L), any.missing = TRUE, "
"    all.missing = TRUE, min.rows = NULL, max.rows = NULL, min.cols = NULL, max.cols = NULL, "
"    nrows = NULL, ncols = NULL, row.names = NULL, col.names = NULL, null.ok = FALSE)  "
"testDate : function (x, lower = NULL, upper = NULL, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, null.ok = FALSE)  "
"testDirectory : function (x, access = \"\")  "
"testDirectoryExists : function (x, access = \"\")  "
"testDisjunct : function (x, y, fmatch = FALSE)  "
"testDouble : function (x, lower = -Inf, upper = Inf, finite = FALSE, any.missing = TRUE, all.missing = TRUE, "
"    len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE)  "
"testEnvironment : function (x, contains = character(0L), null.ok = FALSE)  "
"testFactor : function (x, levels = NULL, ordered = NA, empty.levels.ok = TRUE, any.missing = TRUE, "
"    all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, n.levels = NULL, "
"    min.levels = NULL, max.levels = NULL, unique = FALSE, names = NULL, null.ok = FALSE)  "
"testFALSE : function (x, na.ok = FALSE)  "
"testFile : function (x, access = \"\", extension = NULL)  "
"testFileExists : function (x, access = \"\", extension = NULL)  "
"testFlag : function (x, na.ok = FALSE, null.ok = FALSE)  "
"testFormula : function (x, null.ok = FALSE)  "
"testFunction : function (x, args = NULL, ordered = FALSE, nargs = NULL, null.ok = FALSE)  "
"testInt : function (x, na.ok = FALSE, lower = -Inf, upper = Inf, tol = sqrt(.Machine$double.eps), "
"    null.ok = FALSE)  "
"testInteger : function (x, lower = -Inf, upper = Inf, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE)  "
"testIntegerish : function (x, tol = sqrt(.Machine$double.eps), lower = -Inf, upper = Inf, any.missing = TRUE, "
"    all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, "
"    sorted = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE)  "
"testList : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, names = NULL, null.ok = FALSE)  "
"testLogical : function (x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, "
"    unique = FALSE, names = NULL, typed.missing = FALSE, null.ok = FALSE)  "
"testMatrix : function (x, mode = NULL, any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE)  "
"testMultiClass : function (x, classes, null.ok = FALSE)  "
"testNamed : function (x, type = \"named\")  "
"testNames : function (x, type = \"named\", subset.of = NULL, must.include = NULL, permutation.of = NULL, "
"    identical.to = NULL, disjunct.from = NULL, what = \"names\")  "
"testNull : function (x)  "
"testNumber : function (x, na.ok = FALSE, lower = -Inf, upper = Inf, finite = FALSE, null.ok = FALSE)  "
"testNumeric : function (x, lower = -Inf, upper = Inf, finite = FALSE, any.missing = TRUE, all.missing = TRUE, "
"    len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL, "
"    typed.missing = FALSE, null.ok = FALSE)  "
"testOS : function (os)  "
"testPathForOutput : function (x, overwrite = FALSE, extension = NULL)  "
"testPOSIXct : function (x, lower = NULL, upper = NULL, any.missing = TRUE, all.missing = TRUE, len = NULL, "
"    min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE, null.ok = FALSE)  "
"testR6 : function (x, classes = NULL, ordered = FALSE, cloneable = NULL, public = NULL, private = NULL, "
"    null.ok = FALSE)  "
"testRaw : function (x, len = NULL, min.len = NULL, max.len = NULL, names = NULL, null.ok = FALSE)  "
"testScalar : function (x, na.ok = FALSE, null.ok = FALSE)  "
"testScalarNA : function (x, null.ok = FALSE)  "
"testSetEqual : function (x, y, ordered = FALSE, fmatch = FALSE)  "
"testString : function (x, na.ok = FALSE, n.chars = NULL, min.chars = NULL, max.chars = NULL, pattern = NULL, "
"    fixed = NULL, ignore.case = FALSE, null.ok = FALSE)  "
"testSubset : function (x, choices, empty.ok = TRUE, fmatch = FALSE)  "
"testTibble : function (x, types = character(0L), any.missing = TRUE, all.missing = TRUE, min.rows = NULL, "
"    max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, "
"    row.names = NULL, col.names = NULL, null.ok = FALSE)  "
"testTRUE : function (x, na.ok = FALSE)  "
"testVector : function (x, strict = FALSE, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, "
"    max.len = NULL, unique = FALSE, names = NULL, null.ok = FALSE)  "
"vname : function (x)  "
"wf : function (x, use.names = TRUE)  "
"wl : function (x, use.names = TRUE)  "

library(chron)
.Holidays :  'dates' Named num [1:6] 01/01/92 05/25/92 07/04/92 09/07/92 11/26/92 ...
as.chron : function (x, ...)  
  as.dates : function (x, ...)  
    as.times : function (x, ...)  
      chron : function (dates. = NULL, times. = NULL, format = c(dates = "m/d/y", times = "h:m:s"), 
                        out.format, origin.)  
        chron_trans : function (format = "%Y-%m-%d", n = 5)  
          dates : function (x, ...)  
            day.of.week : function (month, day, year)  
              days : function (x)  
                hours : function (x)  
                  is.chron : function (x)  
                    is.holiday : function (x, holidays)  
                      is.weekend : function (x)  
                        leap.year : function (y)  
                          minutes : function (x)  
                            month.day.year : function (jul, origin.)  
                              origin : function (x)  
                                origin<- : function (x, value)  
                                  scale_x_chron : function (..., format = "%Y-%m-%d", n = 5)  
                                    scale_y_chron : function (..., format = "%Y-%m-%d", n = 5)  
                                      seconds : function (x)  
                                        seq.dates : function (from, to, by = "days", length., ...)  
                                          times : function (x, ...)  
                                            year.expand : function (y, cut.off = 69, century = c(1900, 2000), ...)  
                                              year.strict : function (...)  
                                                years : function (x)  
                                                  > 
library(cowsay) 
animals :  Named chr [1:44] "\n ----- \n%s \n ------ \n    \\   ^__^ \n     \\  (oo)\\ ________ \n        (__)\\         )\\ /\\ \n         "| __truncated__ ...
endless_horse : function (what = "Hello world!", endless = TRUE, wait = 0.5, what_color = NULL, horse_color = NULL)  
  say : function (what = "Hello world!", by = "cat", type = NULL, what_color = NULL, by_color = NULL, 
                  length = 18, fortune = NULL, ...)  
    > 
library(class)
.Depends :  chr [1:2] "stats" "utils"
batchSOM : function (data, grid = somgrid(), radii, init)  
  condense : function (train, class, store = sample(seq(n), 1), trace = TRUE)  
    knn : function (train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)  
      knn.cv : function (train, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)  
        knn1 : function (train, test, cl)  
          lvq1 : function (x, cl, codebk, niter = 100 * nrow(codebk$x), alpha = 0.03)  
            lvq2 : function (x, cl, codebk, niter = 100 * nrow(codebk$x), alpha = 0.03, win = 0.3)  
              lvq3 : function (x, cl, codebk, niter = 100 * nrow(codebk$x), alpha = 0.03, win = 0.3, epsilon = 0.1)  
                lvqinit : function (x, cl, size, prior, k = 5)  
                  lvqtest : function (codebk, test)  
                    multiedit : function (x, class, k = 1, V = 3, I = 5, trace = TRUE)  
                      olvq1 : function (x, cl, codebk, niter = 40 * nrow(codebk$x), alpha = 0.3)  
                        reduce.nn : function (train, ind, class)  
                          SOM : function (data, grid = somgrid(), rlen = 10000, alpha = seq(0.05, 0, len = rlen), 
                                          radii = seq(4, 1, len = rlen), init)  
                            somgrid : function (xdim = 8, ydim = 6, topo = c("rectangular", "hexagonal"))  
                              > 
library(crayon)
%+% : function (lhs, rhs)  
  bgBlack : function (...)  
    bgBlue : function (...)  
      bgCyan : function (...)  
        bgGreen : function (...)  
          bgMagenta : function (...)  
            bgRed : function (...)  
              bgWhite : function (...)  
                bgYellow : function (...)  
                  black : function (...)  
                    blue : function (...)  
                      blurred : function (...)  
                        bold : function (...)  
                          chr : function (x, ...)  
                            col_align : function (text, width = getOption("width"), align = c("left", "center", "right"), 
                                                  type = "width")  
                              col_nchar : function (x, ...)  
                                col_strsplit : function (x, split, ...)  
                                  col_substr : function (x, start, stop)  
                                    col_substring : function (text, first, last = 1000000L)  
                                      combine_styles : function (...)  
                                        cyan : function (...)  
                                          drop_style : function (style)  
                                            finish : function (x, ...)  
                                              green : function (...)  
                                                has_color : function ()  
                                                  has_hyperlink : function ()  
                                                    has_style : function (string)  
                                                      hidden : function (...)  
                                                        hyperlink : function (text, url)  
                                                          inverse : function (...)  
                                                            italic : function (...)  
                                                              magenta : function (...)  
                                                                make_style : function (..., bg = FALSE, grey = FALSE, colors = num_colors())  
                                                                  num_ansi_colors : function (stream = "auto")  
                                                                    num_colors : function (forget = FALSE)  
                                                                      red : function (...)  
                                                                        reset : function (...)  
                                                                          show_ansi_colors : function (colors = num_colors())  
                                                                            silver : function (...)  
                                                                              strikethrough : function (...)  
                                                                                strip_style : function (string)  
                                                                                  style : function (string, as = NULL, bg = NULL)  
                                                                                    styles : function ()  
                                                                                      underline : function (...)  
                                                                                        white : function (...)  
                                                                                          yellow : function (...)  
                                                                                            > 
library(data.table)
%between% : function (x, y)  
  %chin% : function (x, table)  
    %flike% : function (vector, pattern)  
      %ilike% : function (vector, pattern)  
        %inrange% : function (x, y)  
          %like% : function (vector, pattern)  
            .__C__data.table : Formal class 'classRepresentation' [package "methods"] with 11 slots
.__C__IDate : Formal class 'classRepresentation' [package "methods"] with 11 slots
.__C__ITime : Formal class 'classRepresentation' [package "methods"] with 11 slots
.__T__$:base : <environment: 0x000001dbed9449c8> 
  .__T__$<-:base : <environment: 0x000001dbed948dd8> 
  .__T__[:base : <environment: 0x000001dbed94d208> 
           .__T__[[<-:base : <environment: 0x000001dbed953200> 
                     .__T__[<-:base : <environment: 0x000001dbed958de8> 
                              .BY :  NULL
                            .EACHI :  NULL
                            .GRP :  NULL
                            .I :  NULL
                            .Last.updated :  int 0
                            .N :  NULL
                            .NGRP :  NULL
                            .rbind.data.table : function (..., use.names = TRUE, fill = FALSE, idcol = NULL)  
                              .SD :  NULL
                            := : function (...)  
                              address : function (x)  
                                alloc.col : function (DT, n = getOption("datatable.alloccol"), verbose = getOption("datatable.verbose"))  
                                  as.data.table : function (x, keep.rownames = FALSE, ...)  
                                    as.IDate : function (x, ...)  
                                      as.ITime : function (x, ...)  
                                        as.xts.data.table : function (x, ...)  
                                          between : function (x, lower, upper, incbounds = TRUE, NAbounds = TRUE, check = FALSE)  
                                            chgroup : function (x)  
                                              chmatch : function (x, table, nomatch = NA_integer_)  
                                                chorder : function (x)  
                                                  CJ : function (..., sorted = TRUE, unique = FALSE)  
                                                    copy : function (x)  
                                                      cube : function (x, ...)  
                                                        data.table : function (..., keep.rownames = FALSE, check.names = FALSE, key = NULL, stringsAsFactors = FALSE)  
                                                          dcast : function (data, formula, fun.aggregate = NULL, ..., margins = NULL, subset = NULL, 
                                                                            fill = NULL, value.var = guess(data))  
                                                            dcast.data.table : function (data, formula, fun.aggregate = NULL, sep = "_", ..., margins = NULL, subset = NULL, 
                                                                                         fill = NULL, drop = TRUE, value.var = guess(data), verbose = getOption("datatable.verbose"))  
                                                              fcase : function (..., default = NA)  
                                                                fcoalesce : function (...)  
                                                                  fifelse : function (test, yes, no, na = NA)  
                                                                    fintersect : function (x, y, all = FALSE)  
                                                                      first : function (x, n = 1L, ...)  
                                                                        foverlaps : function (x, y, by.x = if (!is.null(key(x))) key(x) else key(y), by.y = key(y), maxgap = 0L, 
                                                                                              minoverlap = 1L, type = c("any", "within", "start", "end", "equal"), mult = c("all", 
                                                                                                                                                                            "first", "last"), nomatch = getOption("datatable.nomatch", NA), which = FALSE, 
                                                                                              verbose = getOption("datatable.verbose"))  
                                                                          frank : function (x, ..., na.last = TRUE, ties.method = c("average", "first", "last", "random", 
                                                                                                                                    "max", "min", "dense"))  
                                                                            frankv : function (x, cols = seq_along(x), order = 1L, na.last = TRUE, ties.method = c("average", 
                                                                                                                                                                   "first", "last", "random", "max", "min", "dense"))  
                                                                              fread : function (input = "", file = NULL, text = NULL, cmd = NULL, sep = "auto", sep2 = "auto", 
                                                                                                dec = ".", quote = "\"", nrows = Inf, header = "auto", na.strings = getOption("datatable.na.strings", 
                                                                                                                                                                              "NA"), stringsAsFactors = FALSE, verbose = getOption("datatable.verbose", 
                                                                                                                                                                                                                                   FALSE), skip = "__auto__", select = NULL, drop = NULL, colClasses = NULL, 
                                                                                                integer64 = getOption("datatable.integer64", "integer64"), col.names, check.names = FALSE, 
                                                                                                encoding = "unknown", strip.white = TRUE, fill = FALSE, blank.lines.skip = FALSE, 
                                                                                                key = NULL, index = NULL, showProgress = getOption("datatable.showProgress", interactive()), 
                                                                                                data.table = getOption("datatable.fread.datatable", TRUE), nThread = getDTthreads(verbose), 
                                                                                                logical01 = getOption("datatable.logical01", FALSE), keepLeadingZeros = getOption("datatable.keepLeadingZeros", 
                                                                                                                                                                                  FALSE), yaml = FALSE, autostart = NA, tmpdir = tempdir(), tz = "UTC")  
                                                                                frollapply : function (x, n, FUN, ..., fill = NA, align = c("right", "left", "center"))  
                                                                                  frollmean : function (x, n, fill = NA, algo = c("fast", "exact"), align = c("right", "left", "center"), 
                                                                                                        na.rm = FALSE, hasNA = NA, adaptive = FALSE)  
                                                                                    frollsum : function (x, n, fill = NA, algo = c("fast", "exact"), align = c("right", "left", "center"), 
                                                                                                         na.rm = FALSE, hasNA = NA, adaptive = FALSE)  
                                                                                      fsetdiff : function (x, y, all = FALSE)  
                                                                                        fsetequal : function (x, y, all = TRUE)  
                                                                                          fsort : function (x, decreasing = FALSE, na.last = FALSE, internal = FALSE, verbose = FALSE, 
                                                                                                            ...)  
                                                                                            funion : function (x, y, all = FALSE)  
                                                                                              fwrite : function (x, file = "", append = FALSE, quote = "auto", sep = ",", sep2 = c("", "|", 
                                                                                                                                                                                   ""), eol = if (.Platform$OS.type == "windows") "\r\n" else "\n", na = "", dec = ".", 
                                                                                                                 row.names = FALSE, col.names = TRUE, qmethod = c("double", "escape"), logical01 = getOption("datatable.logical01", 
                                                                                                                                                                                                             FALSE), logicalAsInt = logical01, scipen = getOption("scipen", 0L), dateTimeAs = c("ISO", 
                                                                                                                                                                                                                                                                                                "squash", "epoch", "write.csv"), buffMB = 8, nThread = getDTthreads(verbose), 
                                                                                                                 showProgress = getOption("datatable.showProgress", interactive()), compress = c("auto", 
                                                                                                                                                                                                 "none", "gzip"), yaml = FALSE, bom = FALSE, verbose = getOption("datatable.verbose", 
                                                                                                                                                                                                                                                                 FALSE))  
                                                                                                getDTthreads : function (verbose = getOption("datatable.verbose"))  
                                                                                                  getNumericRounding : function ()  
                                                                                                    groupingsets : function (x, ...)  
                                                                                                      haskey : function (x)  
                                                                                                        hour : function (x)  
                                                                                                          IDateTime : function (x, ...)  
                                                                                                            indices : function (x, vectors = FALSE)  
                                                                                                              inrange : function (x, lower, upper, incbounds = TRUE)  
                                                                                                                is.data.table : function (x)  
                                                                                                                  isoweek : function (x)  
                                                                                                                    key : function (x)  
                                                                                                                      key<- : function (x, value)  
                                                                                                                        last : function (x, n = 1L, ...)  
                                                                                                                          like : function (vector, pattern, ignore.case = FALSE, fixed = FALSE)  
                                                                                                                            mday : function (x)  
                                                                                                                              melt : function (data, ..., na.rm = FALSE, value.name = "value")  
                                                                                                                                melt.data.table : function (data, id.vars, measure.vars, variable.name = "variable", value.name = "value", 
                                                                                                                                                            ..., na.rm = FALSE, variable.factor = TRUE, value.factor = FALSE, verbose = getOption("datatable.verbose"))  
                                                                                                                                  merge.data.table : function (x, y, by = NULL, by.x = NULL, by.y = NULL, all = FALSE, all.x = all, all.y = all, 
                                                                                                                                                               sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE, allow.cartesian = getOption("datatable.allow.cartesian"), 
                                                                                                                                                               ...)  
                                                                                                                                    minute : function (x)  
                                                                                                                                      month : function (x)  
                                                                                                                                        nafill : function (x, type = c("const", "locf", "nocb"), fill = NA, nan = NA)  
                                                                                                                                          quarter : function (x)  
                                                                                                                                            rbindlist : function (l, use.names = "check", fill = FALSE, idcol = NULL)  
                                                                                                                                              rleid : function (..., prefix = NULL)  
                                                                                                                                                rleidv : function (x, cols = seq_along(x), prefix = NULL)  
                                                                                                                                                  rollup : function (x, ...)  
                                                                                                                                                    rowid : function (..., prefix = NULL)  
                                                                                                                                                      rowidv : function (x, cols = seq_along(x), prefix = NULL)  
                                                                                                                                                        second : function (x)  
                                                                                                                                                          set : function (x, i = NULL, j, value)  
                                                                                                                                                            setalloccol : function (DT, n = getOption("datatable.alloccol"), verbose = getOption("datatable.verbose"))  
                                                                                                                                                              setattr : function (x, name, value)  
                                                                                                                                                                setcolorder : function (x, neworder = key(x))  
                                                                                                                                                                  setDF : function (x, rownames = NULL)  
                                                                                                                                                                    setDT : function (x, keep.rownames = FALSE, key = NULL, check.names = FALSE)  
                                                                                                                                                                      setDTthreads : function (threads = NULL, restore_after_fork = NULL, percent = NULL, throttle = NULL)  
                                                                                                                                                                        setindex : function (...)  
                                                                                                                                                                          setindexv : function (x, cols, verbose = getOption("datatable.verbose"))  
                                                                                                                                                                            setkey : function (x, ..., verbose = getOption("datatable.verbose"), physical = TRUE)  
                                                                                                                                                                              setkeyv : function (x, cols, verbose = getOption("datatable.verbose"), physical = TRUE)  
                                                                                                                                                                                setnafill : function (x, type = c("const", "locf", "nocb"), fill = NA, nan = NA, cols = seq_along(x))  
                                                                                                                                                                                  setnames : function (x, old, new, skip_absent = FALSE)  
                                                                                                                                                                                    setNumericRounding : function (x)  
                                                                                                                                                                                      setorder : function (x, ..., na.last = FALSE)  
                                                                                                                                                                                        setorderv : function (x, cols = colnames(x), order = 1L, na.last = FALSE)  
                                                                                                                                                                                          shift : function (x, n = 1L, fill = NA, type = c("lag", "lead", "shift"), give.names = FALSE)  
                                                                                                                                                                                            shouldPrint : function (x)  
                                                                                                                                                                                              SJ : function (...)  
                                                                                                                                                                                                tables : function (mb = TRUE, order.col = "NAME", width = 80, env = parent.frame(), silent = FALSE, 
                                                                                                                                                                                                                   index = FALSE)  
                                                                                                                                                                                                  test.data.table : function (script = "tests.Rraw", verbose = FALSE, pkg = ".", silent = FALSE, showProgress = interactive() && 
                                                                                                                                                                                                                                !silent)  
                                                                                                                                                                                                    timetaken : function (started.at)  
                                                                                                                                                                                                      transpose : function (l, fill = NA, ignore.empty = FALSE, keep.names = NULL, make.names = NULL)  
                                                                                                                                                                                                        truelength : function (x)  
                                                                                                                                                                                                          tstrsplit : function (x, ..., fill = NA, type.convert = FALSE, keep, names = FALSE)  
                                                                                                                                                                                                            uniqueN : function (x, by = if (is.list(x)) seq_along(x) else NULL, na.rm = FALSE)  
                                                                                                                                                                                                              update.dev.pkg : function (object = "data.table", repo = "https://Rdatatable.gitlab.io/data.table", 
                                                                                                                                                                                                                                         field = "Revision", type = getOption("pkgType"), lib = NULL, ...)  
                                                                                                                                                                                                                wday : function (x)  
                                                                                                                                                                                                                  week : function (x)  
                                                                                                                                                                                                                    yday : function (x)  
                                                                                                                                                                                                                      year : function (x)  
                                                                                                                                                                                                                        >                                                                                                                                                                             
library(datasets)
ability.cov : List of 3
$ cov   : num [1:6, 1:6] 24.64 5.99 33.52 6.02 20.75 ...
$ center: num [1:6] 0 0 0 0 0 0
$ n.obs : num 112
airmiles :  Time-Series [1:24] from 1937 to 1960: 412 480 683 1052 1385 ...
AirPassengers :  Time-Series [1:144] from 1949 to 1961: 112 118 132 129 121 135 148 148 136 119 ...
airquality : 'data.frame':	153 obs. of  6 variables:
  $ Ozone  : int  41 36 12 18 NA 28 23 19 8 NA ...
$ Solar.R: int  190 118 149 313 NA NA 299 99 19 194 ...
$ Wind   : num  7.4 8 12.6 11.5 14.3 14.9 8.6 13.8 20.1 8.6 ...
$ Temp   : int  67 72 74 62 56 66 65 59 61 69 ...
$ Month  : int  5 5 5 5 5 5 5 5 5 5 ...
$ Day    : int  1 2 3 4 5 6 7 8 9 10 ...
anscombe : 'data.frame':	11 obs. of  8 variables:
  $ x1: num  10 8 13 9 11 14 6 4 12 7 ...
$ x2: num  10 8 13 9 11 14 6 4 12 7 ...
$ x3: num  10 8 13 9 11 14 6 4 12 7 ...
$ x4: num  8 8 8 8 8 8 8 19 8 8 ...
$ y1: num  8.04 6.95 7.58 8.81 8.33 ...
$ y2: num  9.14 8.14 8.74 8.77 9.26 8.1 6.13 3.1 9.13 7.26 ...
$ y3: num  7.46 6.77 12.74 7.11 7.81 ...
$ y4: num  6.58 5.76 7.71 8.84 8.47 7.04 5.25 12.5 5.56 7.91 ...
attenu : 'data.frame':	182 obs. of  5 variables:
  $ event  : num  1 2 2 2 2 2 2 2 2 2 ...
$ mag    : num  7 7.4 7.4 7.4 7.4 7.4 7.4 7.4 7.4 7.4 ...
$ station: Factor w/ 117 levels "1008","1011",..: 24 13 15 68 39 74 22 1 8 55 ...
$ dist   : num  12 148 42 85 107 109 156 224 293 359 ...
$ accel  : num  0.359 0.014 0.196 0.135 0.062 0.054 0.014 0.018 0.01 0.004 ...
attitude : 'data.frame':	30 obs. of  7 variables:
  $ rating    : num  43 63 71 61 81 43 58 71 72 67 ...
$ complaints: num  51 64 70 63 78 55 67 75 82 61 ...
$ privileges: num  30 51 68 45 56 49 42 50 72 45 ...
$ learning  : num  39 54 69 47 66 44 56 55 67 47 ...
$ raises    : num  61 63 76 54 71 54 66 70 71 62 ...
$ critical  : num  92 73 86 84 83 49 68 66 83 80 ...
$ advance   : num  45 47 48 35 47 34 35 41 31 41 ...
austres :  Time-Series [1:89] from 1971 to 1993: 13067 13130 13198 13254 13304 ...
beaver1 : 'data.frame':	114 obs. of  4 variables:
  $ day  : num  346 346 346 346 346 346 346 346 346 346 ...
$ time : num  840 850 900 910 920 930 940 950 1000 1010 ...
$ temp : num  36.3 36.3 36.4 36.4 36.5 ...
$ activ: num  0 0 0 0 0 0 0 0 0 0 ...
beaver2 : 'data.frame':	100 obs. of  4 variables:
  $ day  : num  307 307 307 307 307 307 307 307 307 307 ...
$ time : num  930 940 950 1000 1010 1020 1030 1040 1050 1100 ...
$ temp : num  36.6 36.7 36.9 37.1 37.2 ...
$ activ: num  0 0 0 0 0 0 0 0 0 0 ...
BJsales :  Time-Series [1:150] from 1 to 150: 200 200 199 199 199 ...
BJsales.lead :  Time-Series [1:150] from 1 to 150: 10.01 10.07 10.32 9.75 10.33 ...
BOD : 'data.frame':	6 obs. of  2 variables:
  $ Time  : num  1 2 3 4 5 7
$ demand: num  8.3 10.3 19 16 15.6 19.8
cars : 'data.frame':	50 obs. of  2 variables:
  $ speed: num  4 4 7 7 8 9 10 10 10 11 ...
$ dist : num  2 10 4 22 16 10 18 26 34 17 ...
ChickWeight : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	578 obs. of  4 variables:
  $ weight: num  42 51 59 64 76 93 106 125 149 171 ...
$ Time  : num  0 2 4 6 8 10 12 14 16 18 ...
$ Chick : Ord.factor w/ 50 levels "18"<"16"<"15"<..: 15 15 15 15 15 15 15 15 15 15 ...
$ Diet  : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 1 ...
chickwts : 'data.frame':	71 obs. of  2 variables:
  $ weight: num  179 160 136 227 217 168 108 124 143 140 ...
$ feed  : Factor w/ 6 levels "casein","horsebean",..: 2 2 2 2 2 2 2 2 2 2 ...
co2 :  Time-Series [1:468] from 1959 to 1998: 315 316 316 318 318 ...
CO2 : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	84 obs. of  5 variables:
  $ Plant    : Ord.factor w/ 12 levels "Qn1"<"Qn2"<"Qn3"<..: 1 1 1 1 1 1 1 2 2 2 ...
$ Type     : Factor w/ 2 levels "Quebec","Mississippi": 1 1 1 1 1 1 1 1 1 1 ...
$ Treatment: Factor w/ 2 levels "nonchilled","chilled": 1 1 1 1 1 1 1 1 1 1 ...
$ conc     : num  95 175 250 350 500 675 1000 95 175 250 ...
$ uptake   : num  16 30.4 34.8 37.2 35.3 39.2 39.7 13.6 27.3 37.1 ...
crimtab :  'table' int [1:42, 1:22] 0 0 0 0 0 0 1 0 0 0 ...
discoveries :  Time-Series [1:100] from 1860 to 1959: 5 3 0 2 0 3 2 3 6 1 ...
DNase : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	176 obs. of  3 variables:
  $ Run    : Ord.factor w/ 11 levels "10"<"11"<"9"<..: 4 4 4 4 4 4 4 4 4 4 ...
$ conc   : num  0.0488 0.0488 0.1953 0.1953 0.3906 ...
$ density: num  0.017 0.018 0.121 0.124 0.206 0.215 0.377 0.374 0.614 0.609 ...
esoph : 'data.frame':	88 obs. of  5 variables:
  $ agegp    : Ord.factor w/ 6 levels "25-34"<"35-44"<..: 1 1 1 1 1 1 1 1 1 1 ...
$ alcgp    : Ord.factor w/ 4 levels "0-39g/day"<"40-79"<..: 1 1 1 1 2 2 2 2 3 3 ...
$ tobgp    : Ord.factor w/ 4 levels "0-9g/day"<"10-19"<..: 1 2 3 4 1 2 3 4 1 2 ...
$ ncases   : num  0 0 0 0 0 0 0 0 0 0 ...
$ ncontrols: num  40 10 6 5 27 7 4 7 2 1 ...
euro :  Named num [1:11] 13.76 40.34 1.96 166.39 5.95 ...
euro.cross :  num [1:11, 1:11] 1 0.3411 7.0355 0.0827 2.3143 ...
eurodist :  'dist' num [1:210] 3313 2963 3175 3339 2762 ...
EuStockMarkets :  Time-Series [1:1860, 1:4] from 1991 to 1999: 1629 1614 1607 1621 1618 ...
faithful : 'data.frame':	272 obs. of  2 variables:
  $ eruptions: num  3.6 1.8 3.33 2.28 4.53 ...
$ waiting  : num  79 54 74 62 85 55 88 85 51 85 ...
fdeaths :  Time-Series [1:72] from 1974 to 1980: 901 689 827 677 522 406 441 393 387 582 ...
Formaldehyde : 'data.frame':	6 obs. of  2 variables:
  $ carb  : num  0.1 0.3 0.5 0.6 0.7 0.9
$ optden: num  0.086 0.269 0.446 0.538 0.626 0.782
freeny : 'data.frame':	39 obs. of  5 variables:
  $ y                    : Time-Series  from 1962 to 1972: 8.79 8.79 8.81 8.81 8.91 ...
$ lag.quarterly.revenue: num  8.8 8.79 8.79 8.81 8.81 ...
$ price.index          : num  4.71 4.7 4.69 4.69 4.64 ...
$ income.level         : num  5.82 5.83 5.83 5.84 5.85 ...
$ market.potential     : num  13 13 13 13 13 ...
freeny.x :  num [1:39, 1:4] 8.8 8.79 8.79 8.81 8.81 ...
freeny.y :  Time-Series [1:39] from 1962 to 1972: 8.79 8.79 8.81 8.81 8.91 ...
HairEyeColor :  'table' num [1:4, 1:4, 1:2] 32 53 10 3 11 50 10 30 10 25 ...
Harman23.cor : List of 3
$ cov   : num [1:8, 1:8] 1 0.846 0.805 0.859 0.473 0.398 0.301 0.382 0.846 1 ...
$ center: num [1:8] 0 0 0 0 0 0 0 0
$ n.obs : num 305
Harman74.cor : List of 3
$ cov   : num [1:24, 1:24] 1 0.318 0.403 0.468 0.321 0.335 0.304 0.332 0.326 0.116 ...
$ center: num [1:24] 0 0 0 0 0 0 0 0 0 0 ...
$ n.obs : num 145
Indometh : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	66 obs. of  3 variables:
  $ Subject: Ord.factor w/ 6 levels "1"<"4"<"2"<"5"<..: 1 1 1 1 1 1 1 1 1 1 ...
$ time   : num  0.25 0.5 0.75 1 1.25 2 3 4 5 6 ...
$ conc   : num  1.5 0.94 0.78 0.48 0.37 0.19 0.12 0.11 0.08 0.07 ...
infert : 'data.frame':	248 obs. of  8 variables:
  $ education     : Factor w/ 3 levels "0-5yrs","6-11yrs",..: 1 1 1 1 2 2 2 2 2 2 ...
$ age           : num  26 42 39 34 35 36 23 32 21 28 ...
$ parity        : num  6 1 6 4 3 4 1 2 1 2 ...
$ induced       : num  1 1 2 2 1 2 0 0 0 0 ...
$ case          : num  1 1 1 1 1 1 1 1 1 1 ...
$ spontaneous   : num  2 0 0 0 1 1 0 0 1 0 ...
$ stratum       : int  1 2 3 4 5 6 7 8 9 10 ...
$ pooled.stratum: num  3 1 4 2 32 36 6 22 5 19 ...
InsectSprays : 'data.frame':	72 obs. of  2 variables:
  $ count: num  10 7 20 14 14 12 10 23 17 20 ...
$ spray: Factor w/ 6 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
iris : 'data.frame':	150 obs. of  5 variables:
  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
$ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
$ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
$ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
$ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
iris3 :  num [1:50, 1:4, 1:3] 5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
islands :  Named num [1:48] 11506 5500 16988 2968 16 ...
JohnsonJohnson :  Time-Series [1:84] from 1960 to 1981: 0.71 0.63 0.85 0.44 0.61 0.69 0.92 0.55 0.72 0.77 ...
LakeHuron :  Time-Series [1:98] from 1875 to 1972: 580 582 581 581 580 ...
ldeaths :  Time-Series [1:72] from 1974 to 1980: 3035 2552 2704 2554 2014 ...
lh :  Time-Series [1:48] from 1 to 48: 2.4 2.4 2.4 2.2 2.1 1.5 2.3 2.3 2.5 2 ...
LifeCycleSavings : 'data.frame':	50 obs. of  5 variables:
  $ sr   : num  11.43 12.07 13.17 5.75 12.88 ...
$ pop15: num  29.4 23.3 23.8 41.9 42.2 ...
$ pop75: num  2.87 4.41 4.43 1.67 0.83 2.85 1.34 0.67 1.06 1.14 ...
$ dpi  : num  2330 1508 2108 189 728 ...
$ ddpi : num  2.87 3.93 3.82 0.22 4.56 2.43 2.67 6.51 3.08 2.8 ...
Loblolly : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	84 obs. of  3 variables:
  $ height: num  4.51 10.89 28.72 41.74 52.7 ...
$ age   : num  3 5 10 15 20 25 3 5 10 15 ...
$ Seed  : Ord.factor w/ 14 levels "329"<"327"<"325"<..: 10 10 10 10 10 10 13 13 13 13 ...
longley : 'data.frame':	16 obs. of  7 variables:
  $ GNP.deflator: num  83 88.5 88.2 89.5 96.2 ...
$ GNP         : num  234 259 258 285 329 ...
$ Unemployed  : num  236 232 368 335 210 ...
$ Armed.Forces: num  159 146 162 165 310 ...
$ Population  : num  108 109 110 111 112 ...
$ Year        : int  1947 1948 1949 1950 1951 1952 1953 1954 1955 1956 ...
$ Employed    : num  60.3 61.1 60.2 61.2 63.2 ...
lynx :  Time-Series [1:114] from 1821 to 1934: 269 321 585 871 1475 ...
mdeaths :  Time-Series [1:72] from 1974 to 1980: 2134 1863 1877 1877 1492 ...
morley : 'data.frame':	100 obs. of  3 variables:
  $ Expt : int  1 1 1 1 1 1 1 1 1 1 ...
$ Run  : int  1 2 3 4 5 6 7 8 9 10 ...
$ Speed: int  850 740 900 1070 930 850 950 980 980 880 ...
mtcars : 'data.frame':	32 obs. of  11 variables:
  $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
$ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
$ disp: num  160 160 108 258 360 ...
$ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
$ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
$ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
$ qsec: num  16.5 17 18.6 19.4 17 ...
$ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
$ am  : num  1 1 1 0 0 0 0 0 0 0 ...
$ gear: num  4 4 4 3 3 3 3 4 4 4 ...
$ carb: num  4 4 1 1 2 1 4 2 2 4 ...
nhtemp :  Time-Series [1:60] from 1912 to 1971: 49.9 52.3 49.4 51.1 49.4 47.9 49.8 50.9 49.3 51.9 ...
Nile :  Time-Series [1:100] from 1871 to 1970: 1120 1160 963 1210 1160 1160 813 1230 1370 1140 ...
nottem :  Time-Series [1:240] from 1920 to 1940: 40.6 40.8 44.4 46.7 54.1 58.5 57.7 56.4 54.3 50.5 ...
npk : 'data.frame':	24 obs. of  5 variables:
  $ block: Factor w/ 6 levels "1","2","3","4",..: 1 1 1 1 2 2 2 2 3 3 ...
$ N    : Factor w/ 2 levels "0","1": 1 2 1 2 2 2 1 1 1 2 ...
$ P    : Factor w/ 2 levels "0","1": 2 2 1 1 1 2 1 2 2 2 ...
$ K    : Factor w/ 2 levels "0","1": 2 1 1 2 1 2 2 1 1 2 ...
$ yield: num  49.5 62.8 46.8 57 59.8 58.5 55.5 56 62.8 55.8 ...
occupationalStatus :  'table' int [1:8, 1:8] 50 16 12 11 2 12 0 0 19 40 ...
Orange : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	35 obs. of  3 variables:
  $ Tree         : Ord.factor w/ 5 levels "3"<"1"<"5"<"2"<..: 2 2 2 2 2 2 2 4 4 4 ...
$ age          : num  118 484 664 1004 1231 ...
$ circumference: num  30 58 87 115 120 142 145 33 69 111 ...
OrchardSprays : 'data.frame':	64 obs. of  4 variables:
  $ decrease : num  57 95 8 69 92 90 15 2 84 6 ...
$ rowpos   : num  1 2 3 4 5 6 7 8 1 2 ...
$ colpos   : num  1 1 1 1 1 1 1 1 2 2 ...
$ treatment: Factor w/ 8 levels "A","B","C","D",..: 4 5 2 8 7 6 3 1 3 2 ...
PlantGrowth : 'data.frame':	30 obs. of  2 variables:
  $ weight: num  4.17 5.58 5.18 6.11 4.5 4.61 5.17 4.53 5.33 5.14 ...
$ group : Factor w/ 3 levels "ctrl","trt1",..: 1 1 1 1 1 1 1 1 1 1 ...
precip :  Named num [1:70] 67 54.7 7 48.5 14 17.2 20.7 13 43.4 40.2 ...
presidents :  Time-Series [1:120] from 1945 to 1975: NA 87 82 75 63 50 43 32 35 60 ...
pressure : 'data.frame':	19 obs. of  2 variables:
  $ temperature: num  0 20 40 60 80 100 120 140 160 180 ...
$ pressure   : num  0.0002 0.0012 0.006 0.03 0.09 0.27 0.75 1.85 4.2 8.8 ...
Puromycin : 'data.frame':	23 obs. of  3 variables:
  $ conc : num  0.02 0.02 0.06 0.06 0.11 0.11 0.22 0.22 0.56 0.56 ...
$ rate : num  76 47 97 107 123 139 159 152 191 201 ...
$ state: Factor w/ 2 levels "treated","untreated": 1 1 1 1 1 1 1 1 1 1 ...
quakes : 'data.frame':	1000 obs. of  5 variables:
  $ lat     : num  -20.4 -20.6 -26 -18 -20.4 ...
$ long    : num  182 181 184 182 182 ...
$ depth   : int  562 650 42 626 649 195 82 194 211 622 ...
$ mag     : num  4.8 4.2 5.4 4.1 4 4 4.8 4.4 4.7 4.3 ...
$ stations: int  41 15 43 19 11 12 43 15 35 19 ...
randu : 'data.frame':	400 obs. of  3 variables:
  $ x: num  0.000031 0.044495 0.82244 0.322291 0.393595 ...
$ y: num  0.000183 0.155732 0.873416 0.648545 0.826873 ...
$ z: num  0.000824 0.533939 0.838542 0.990648 0.418881 ...
rivers :  num [1:141] 735 320 325 392 524 ...
rock : 'data.frame':	48 obs. of  4 variables:
  $ area : int  4990 7002 7558 7352 7943 7979 9333 8209 8393 6425 ...
$ peri : num  2792 3893 3931 3869 3949 ...
$ shape: num  0.0903 0.1486 0.1833 0.1171 0.1224 ...
$ perm : num  6.3 6.3 6.3 6.3 17.1 17.1 17.1 17.1 119 119 ...
Seatbelts :  Time-Series [1:192, 1:8] from 1969 to 1985: 107 97 102 87 119 106 110 106 107 134 ...
sleep : 'data.frame':	20 obs. of  3 variables:
  $ extra: num  0.7 -1.6 -0.2 -1.2 -0.1 3.4 3.7 0.8 0 2 ...
$ group: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...
$ ID   : Factor w/ 10 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
stack.loss :  num [1:21] 42 37 37 28 18 18 19 20 15 14 ...
stack.x :  num [1:21, 1:3] 80 80 75 62 62 62 62 62 58 58 ...
stackloss : 'data.frame':	21 obs. of  4 variables:
  $ Air.Flow  : num  80 80 75 62 62 62 62 62 58 58 ...
$ Water.Temp: num  27 27 25 24 22 23 24 24 23 18 ...
$ Acid.Conc.: num  89 88 90 87 87 87 93 93 87 80 ...
$ stack.loss: num  42 37 37 28 18 18 19 20 15 14 ...
state.abb :  chr [1:50] "AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "FL" "GA" "HI" "ID" "IL" "IN" "IA" ...
state.area :  num [1:50] 51609 589757 113909 53104 158693 ...
state.center : List of 2
$ x: num [1:50] -86.8 -127.2 -111.6 -92.3 -119.8 ...
$ y: num [1:50] 32.6 49.2 34.2 34.7 36.5 ...
state.division :  Factor w/ 9 levels "New England",..: 4 9 8 5 9 8 1 3 3 3 ...
state.name :  chr [1:50] "Alabama" "Alaska" "Arizona" "Arkansas" "California" "Colorado" ...
state.region :  Factor w/ 4 levels "Northeast","South",..: 2 4 4 2 4 4 1 2 2 2 ...
state.x77 :  num [1:50, 1:8] 3615 365 2212 2110 21198 ...
sunspot.month :  Time-Series [1:3177] from 1749 to 2014: 58 62.6 70 55.7 85 83.5 94.8 66.3 75.9 75.5 ...
sunspot.year :  Time-Series [1:289] from 1700 to 1988: 5 11 16 23 36 58 29 20 10 8 ...
sunspots :  Time-Series [1:2820] from 1749 to 1984: 58 62.6 70 55.7 85 83.5 94.8 66.3 75.9 75.5 ...
swiss : 'data.frame':	47 obs. of  6 variables:
  $ Fertility       : num  80.2 83.1 92.5 85.8 76.9 76.1 83.8 92.4 82.4 82.9 ...
$ Agriculture     : num  17 45.1 39.7 36.5 43.5 35.3 70.2 67.8 53.3 45.2 ...
$ Examination     : int  15 6 5 12 17 9 16 14 12 16 ...
$ Education       : int  12 9 5 7 15 7 7 8 7 13 ...
$ Catholic        : num  9.96 84.84 93.4 33.77 5.16 ...
$ Infant.Mortality: num  22.2 22.2 20.2 20.3 20.6 26.6 23.6 24.9 21 24.4 ...
Theoph : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	132 obs. of  5 variables:
  $ Subject: Ord.factor w/ 12 levels "6"<"7"<"8"<"11"<..: 11 11 11 11 11 11 11 11 11 11 ...
$ Wt     : num  79.6 79.6 79.6 79.6 79.6 79.6 79.6 79.6 79.6 79.6 ...
$ Dose   : num  4.02 4.02 4.02 4.02 4.02 4.02 4.02 4.02 4.02 4.02 ...
$ Time   : num  0 0.25 0.57 1.12 2.02 ...
$ conc   : num  0.74 2.84 6.57 10.5 9.66 8.58 8.36 7.47 6.89 5.94 ...
Titanic :  'table' num [1:4, 1:2, 1:2, 1:2] 0 0 35 0 0 0 17 0 118 154 ...
ToothGrowth : 'data.frame':	60 obs. of  3 variables:
  $ len : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
$ supp: Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
$ dose: num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
treering :  Time-Series [1:7980] from -6000 to 1979: 1.34 1.08 1.54 1.32 1.41 ...
trees : 'data.frame':	31 obs. of  3 variables:
  $ Girth : num  8.3 8.6 8.8 10.5 10.7 10.8 11 11 11.1 11.2 ...
$ Height: num  70 65 63 72 81 83 66 75 80 75 ...
$ Volume: num  10.3 10.3 10.2 16.4 18.8 19.7 15.6 18.2 22.6 19.9 ...
UCBAdmissions :  'table' num [1:2, 1:2, 1:6] 512 313 89 19 353 207 17 8 120 205 ...
UKDriverDeaths :  Time-Series [1:192] from 1969 to 1985: 1687 1508 1507 1385 1632 ...
UKgas :  Time-Series [1:108] from 1960 to 1987: 160.1 129.7 84.8 120.1 160.1 ...
USAccDeaths :  Time-Series [1:72] from 1973 to 1979: 9007 8106 8928 9137 10017 ...
USArrests : 'data.frame':	50 obs. of  4 variables:
  $ Murder  : num  13.2 10 8.1 8.8 9 7.9 3.3 5.9 15.4 17.4 ...
$ Assault : int  236 263 294 190 276 204 110 238 335 211 ...
$ UrbanPop: int  58 48 80 50 91 78 77 72 80 60 ...
$ Rape    : num  21.2 44.5 31 19.5 40.6 38.7 11.1 15.8 31.9 25.8 ...
UScitiesD :  'dist' int [1:45] 587 1212 701 1936 604 748 2139 2182 543 920 ...
USJudgeRatings : 'data.frame':	43 obs. of  12 variables:
  $ CONT: num  5.7 6.8 7.2 6.8 7.3 6.2 10.6 7 7.3 8.2 ...
$ INTG: num  7.9 8.9 8.1 8.8 6.4 8.8 9 5.9 8.9 7.9 ...
$ DMNR: num  7.7 8.8 7.8 8.5 4.3 8.7 8.9 4.9 8.9 6.7 ...
$ DILG: num  7.3 8.5 7.8 8.8 6.5 8.5 8.7 5.1 8.7 8.1 ...
$ CFMG: num  7.1 7.8 7.5 8.3 6 7.9 8.5 5.4 8.6 7.9 ...
$ DECI: num  7.4 8.1 7.6 8.5 6.2 8 8.5 5.9 8.5 8 ...
$ PREP: num  7.1 8 7.5 8.7 5.7 8.1 8.5 4.8 8.4 7.9 ...
$ FAMI: num  7.1 8 7.5 8.7 5.7 8 8.5 5.1 8.4 8.1 ...
$ ORAL: num  7.1 7.8 7.3 8.4 5.1 8 8.6 4.7 8.4 7.7 ...
$ WRIT: num  7 7.9 7.4 8.5 5.3 8 8.4 4.9 8.5 7.8 ...
$ PHYS: num  8.3 8.5 7.9 8.8 5.5 8.6 9.1 6.8 8.8 8.5 ...
$ RTEN: num  7.8 8.7 7.8 8.7 4.8 8.6 9 5 8.8 7.9 ...
USPersonalExpenditure :  num [1:5, 1:5] 22.2 10.5 3.53 1.04 0.341 44.5 15.5 5.76 1.98 0.974 ...
uspop :  Time-Series [1:19] from 1790 to 1970: 3.93 5.31 7.24 9.64 12.9 17.1 23.2 31.4 39.8 50.2 ...
VADeaths :  num [1:5, 1:4] 11.7 18.1 26.9 41 66 8.7 11.7 20.3 30.9 54.3 ...
volcano :  num [1:87, 1:61] 100 101 102 103 104 105 105 106 107 108 ...
warpbreaks : 'data.frame':	54 obs. of  3 variables:
  $ breaks : num  26 30 54 25 70 52 51 26 67 18 ...
$ wool   : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
$ tension: Factor w/ 3 levels "L","M","H": 1 1 1 1 1 1 1 1 1 2 ...
women : 'data.frame':	15 obs. of  2 variables:
  $ height: num  58 59 60 61 62 63 64 65 66 67 ...
$ weight: num  115 117 120 123 126 129 132 135 139 142 ...
WorldPhones :  num [1:7, 1:7] 45939 60423 64721 68484 71799 ...
WWWusage :  Time-Series [1:100] from 1 to 100: 88 84 85 85 84 85 83 85 88 89 ...
> 
library(dplyr)
%>% : function (lhs, rhs)  
  .data :  list()
across : function (.cols = everything(), .fns = NULL, ..., .names = NULL)  
  add_count : function (x, ..., wt = NULL, sort = FALSE, name = NULL, .drop = deprecated())  
    add_count_ : function (x, vars, wt = NULL, sort = FALSE)  
      add_row : function (.data, ..., .before = NULL, .after = NULL)  
        add_rownames : function (df, var = "rowname")  
          add_tally : function (x, wt = NULL, sort = FALSE, name = NULL)  
            add_tally_ : function (x, wt, sort = FALSE)  
              all_equal : function (target, current, ignore_col_order = TRUE, ignore_row_order = TRUE, convert = FALSE, 
                                    ...)  
                all_of : function (x)  
                  all_vars : function (expr)  
                    anti_join : function (x, y, by = NULL, copy = FALSE, ...)  
                      any_of : function (x, ..., vars = NULL)  
                        any_vars : function (expr)  
                          arrange : function (.data, ..., .by_group = FALSE)  
                            arrange_ : function (.data, ..., .dots = list())  
                              arrange_all : function (.tbl, .funs = list(), ..., .by_group = FALSE)  
                                arrange_at : function (.tbl, .vars, .funs = list(), ..., .by_group = FALSE)  
                                  arrange_if : function (.tbl, .predicate, .funs = list(), ..., .by_group = FALSE)  
                                    as.tbl : function (x, ...)  
                                      as_data_frame : function (x, ...)  
                                        as_label : function (x)  
                                          as_tibble : function (x, ..., .rows = NULL, .name_repair = c("check_unique", "unique", "universal", 
                                                                                                       "minimal"), rownames = pkgconfig::get_config("tibble::rownames", NULL))  
                                            auto_copy : function (x, y, copy = FALSE, ...)  
                                              band_instruments : tibble [3 × 2] (S3: tbl_df/tbl/data.frame)
band_instruments2 : tibble [3 × 2] (S3: tbl_df/tbl/data.frame)
band_members : tibble [3 × 2] (S3: tbl_df/tbl/data.frame)
bench_tbls : function (tbls, op, ..., times = 10)  
  between : function (x, left, right)  
    bind_cols : function (..., .name_repair = c("unique", "universal", "check_unique", "minimal"))  
      bind_rows : function (..., .id = NULL)  
        c_across : function (cols = everything())  
          case_when : function (...)  
            changes : function (x, y)  
              check_dbplyr : function ()  
                coalesce : function (...)  
                  collapse : function (x, ...)  
                    collect : function (x, ...)  
                      combine : function (...)  
                        common_by : function (by = NULL, x, y)  
                          compare_tbls : function (tbls, op, ref = NULL, compare = equal_data_frame, ...)  
                            compare_tbls2 : function (tbls_x, tbls_y, op, ref = NULL, compare = equal_data_frame, ...)  
                              compute : function (x, ...)  
                                contains : function (match, ignore.case = TRUE, vars = NULL)  
                                  copy_to : function (dest, df, name = deparse(substitute(df)), overwrite = FALSE, ...)  
                                    count : function (x, ..., wt = NULL, sort = FALSE, name = NULL)  
                                      count_ : function (x, vars, wt = NULL, sort = FALSE, .drop = group_by_drop_default(x))  
                                        cumall : function (x)  
                                          cumany : function (x)  
                                            cume_dist : function (x)  
                                              cummean : function (x)  
                                                cur_column : function ()  
                                                  cur_data : function ()  
                                                    cur_data_all : function ()  
                                                      cur_group : function ()  
                                                        cur_group_id : function ()  
                                                          cur_group_rows : function ()  
                                                            current_vars : function (...)  
                                                              data_frame : function (...)  
                                                                data_frame_ : function (xs)  
                                                                  db_analyze : function (con, table, ...)  
                                                                    db_begin : function (con, ...)  
                                                                      db_commit : function (con, ...)  
                                                                        db_create_index : function (con, table, columns, name = NULL, unique = FALSE, ...)  
                                                                          db_create_indexes : function (con, table, indexes = NULL, unique = FALSE, ...)  
                                                                            db_create_table : function (con, table, types, temporary = FALSE, ...)  
                                                                              db_data_type : function (con, fields)  
                                                                                db_desc : function (x)  
                                                                                  db_drop_table : function (con, table, force = FALSE, ...)  
                                                                                    db_explain : function (con, sql, ...)  
                                                                                      db_has_table : function (con, table)  
                                                                                        db_insert_into : function (con, table, values, ...)  
                                                                                          db_list_tables : function (con)  
                                                                                            db_query_fields : function (con, sql, ...)  
                                                                                              db_query_rows : function (con, sql, ...)  
                                                                                                db_rollback : function (con, ...)  
                                                                                                  db_save_query : function (con, sql, name, temporary = TRUE, ...)  
                                                                                                    db_write_table : function (con, table, types, values, temporary = FALSE, ...)  
                                                                                                      dense_rank : function (x)  
                                                                                                        desc : function (x)  
                                                                                                          dim_desc : function (x)  
                                                                                                            distinct : function (.data, ..., .keep_all = FALSE)  
                                                                                                              distinct_ : function (.data, ..., .dots, .keep_all = FALSE)  
                                                                                                                distinct_all : function (.tbl, .funs = list(), ..., .keep_all = FALSE)  
                                                                                                                  distinct_at : function (.tbl, .vars, .funs = list(), ..., .keep_all = FALSE)  
                                                                                                                    distinct_if : function (.tbl, .predicate, .funs = list(), ..., .keep_all = FALSE)  
                                                                                                                      distinct_prepare : function (.data, vars, group_vars = character(), .keep_all = FALSE, caller_env = caller_env(2), 
                                                                                                                                                   error_call = caller_env())  
                                                                                                                        do : function (.data, ...)  
                                                                                                                          do_ : function (.data, ..., .dots = list())  
                                                                                                                            dplyr_col_modify : function (data, cols)  
                                                                                                                              dplyr_reconstruct : function (data, template)  
                                                                                                                                dplyr_row_slice : function (data, i, ...)  
                                                                                                                                  ends_with : function (match, ignore.case = TRUE, vars = NULL)  
                                                                                                                                    enexpr : function (arg)  
                                                                                                                                      enexprs : function (..., .named = FALSE, .ignore_empty = c("trailing", "none", "all"), .unquote_names = TRUE, 
                                                                                                                                                          .homonyms = c("keep", "first", "last", "error"), .check_assign = FALSE)  
                                                                                                                                        enquo : function (arg)  
                                                                                                                                          enquos : function (..., .named = FALSE, .ignore_empty = c("trailing", "none", "all"), .unquote_names = TRUE, 
                                                                                                                                                             .homonyms = c("keep", "first", "last", "error"), .check_assign = FALSE)  
                                                                                                                                            ensym : function (arg)  
                                                                                                                                              ensyms : function (..., .named = FALSE, .ignore_empty = c("trailing", "none", "all"), .unquote_names = TRUE, 
                                                                                                                                                                 .homonyms = c("keep", "first", "last", "error"), .check_assign = FALSE)  
                                                                                                                                                eval_tbls : function (tbls, op)  
                                                                                                                                                  eval_tbls2 : function (tbls_x, tbls_y, op)  
                                                                                                                                                    everything : function (vars = NULL)  
                                                                                                                                                      explain : function (x, ...)  
                                                                                                                                                        expr : function (expr)  
                                                                                                                                                          failwith : function (default = NULL, f, quiet = FALSE)  
                                                                                                                                                            filter : function (.data, ..., .preserve = FALSE)  
                                                                                                                                                              filter_ : function (.data, ..., .dots = list())  
                                                                                                                                                                filter_all : function (.tbl, .vars_predicate, .preserve = FALSE)  
                                                                                                                                                                  filter_at : function (.tbl, .vars, .vars_predicate, .preserve = FALSE)  
                                                                                                                                                                    filter_if : function (.tbl, .predicate, .vars_predicate, .preserve = FALSE)  
                                                                                                                                                                      first : function (x, order_by = NULL, default = default_missing(x))  
                                                                                                                                                                        frame_data : function (...)  
                                                                                                                                                                          full_join : function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = FALSE)  
                                                                                                                                                                            funs : function (..., .args = list())  
                                                                                                                                                                              funs_ : function (dots, args = list(), env = base_env())  
                                                                                                                                                                                glimpse : function (x, width = NULL, ...)  
                                                                                                                                                                                  group_by : function (.data, ..., .add = FALSE, .drop = group_by_drop_default(.data))  
                                                                                                                                                                                    group_by_ : function (.data, ..., .dots = list(), add = FALSE)  
                                                                                                                                                                                      group_by_all : function (.tbl, .funs = list(), ..., .add = FALSE, .drop = group_by_drop_default(.tbl))  
                                                                                                                                                                                        group_by_at : function (.tbl, .vars, .funs = list(), ..., .add = FALSE, .drop = group_by_drop_default(.tbl))  
                                                                                                                                                                                          group_by_drop_default : function (.tbl)  
                                                                                                                                                                                            group_by_if : function (.tbl, .predicate, .funs = list(), ..., .add = FALSE, .drop = group_by_drop_default(.tbl))  
                                                                                                                                                                                              group_by_prepare : function (.data, ..., caller_env = caller_env(2), .add = FALSE, .dots = deprecated(), 
                                                                                                                                                                                                                           add = deprecated(), error_call = caller_env())  
                                                                                                                                                                                                group_cols : function (vars = NULL, data = NULL)  
                                                                                                                                                                                                  group_data : function (.data)  
                                                                                                                                                                                                    group_indices : function (.data, ...)  
                                                                                                                                                                                                      group_indices_ : function (.data, ..., .dots = list())  
                                                                                                                                                                                                        group_keys : function (.tbl, ...)  
                                                                                                                                                                                                          group_map : function (.data, .f, ..., .keep = FALSE)  
                                                                                                                                                                                                            group_modify : function (.data, .f, ..., .keep = FALSE)  
                                                                                                                                                                                                              group_nest : function (.tbl, ..., .key = "data", keep = FALSE)  
                                                                                                                                                                                                                group_rows : function (.data)  
                                                                                                                                                                                                                  group_size : function (x)  
                                                                                                                                                                                                                    group_split : function (.tbl, ..., .keep = TRUE)  
                                                                                                                                                                                                                      group_trim : function (.tbl, .drop = group_by_drop_default(.tbl))  
                                                                                                                                                                                                                        group_vars : function (x)  
                                                                                                                                                                                                                          group_walk : function (.data, .f, ...)  
                                                                                                                                                                                                                            grouped_df : function (data, vars, drop = group_by_drop_default(data))  
                                                                                                                                                                                                                              groups : function (x)  
                                                                                                                                                                                                                                id : function (.variables, drop = FALSE)  
                                                                                                                                                                                                                                  ident : function (...)  
                                                                                                                                                                                                                                    if_all : function (.cols = everything(), .fns = NULL, ..., .names = NULL)  
                                                                                                                                                                                                                                      if_any : function (.cols = everything(), .fns = NULL, ..., .names = NULL)  
                                                                                                                                                                                                                                        if_else : function (condition, true, false, missing = NULL)  
                                                                                                                                                                                                                                          inner_join : function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = FALSE)  
                                                                                                                                                                                                                                            intersect : function (x, y, ...)  
                                                                                                                                                                                                                                              is.grouped_df : function (x)  
                                                                                                                                                                                                                                                is.src : function (x)  
                                                                                                                                                                                                                                                  is.tbl : function (x)  
                                                                                                                                                                                                                                                    is_grouped_df : function (x)  
                                                                                                                                                                                                                                                      lag : function (x, n = 1L, default = NA, order_by = NULL, ...)  
                                                                                                                                                                                                                                                        last : function (x, order_by = NULL, default = default_missing(x))  
                                                                                                                                                                                                                                                          last_col : function (offset = 0L, vars = NULL)  
                                                                                                                                                                                                                                                            lead : function (x, n = 1L, default = NA, order_by = NULL, ...)  
                                                                                                                                                                                                                                                              left_join : function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = FALSE)  
                                                                                                                                                                                                                                                                location : function (df)  
                                                                                                                                                                                                                                                                  lst : function (...)  
                                                                                                                                                                                                                                                                    lst_ : function (xs)  
                                                                                                                                                                                                                                                                      make_tbl : function (subclass, ...)  
                                                                                                                                                                                                                                                                        matches : function (match, ignore.case = TRUE, perl = FALSE, vars = NULL)  
                                                                                                                                                                                                                                                                          min_rank : function (x)  
                                                                                                                                                                                                                                                                            mutate : function (.data, ...)  
                                                                                                                                                                                                                                                                              mutate_ : function (.data, ..., .dots = list())  
                                                                                                                                                                                                                                                                                mutate_all : function (.tbl, .funs, ...)  
                                                                                                                                                                                                                                                                                  mutate_at : function (.tbl, .vars, .funs, ..., .cols = NULL)  
                                                                                                                                                                                                                                                                                    mutate_each : function (tbl, funs, ...)  
                                                                                                                                                                                                                                                                                      mutate_each_ : function (tbl, funs, vars)  
                                                                                                                                                                                                                                                                                        mutate_if : function (.tbl, .predicate, .funs, ...)  
                                                                                                                                                                                                                                                                                          n : function ()  
                                                                                                                                                                                                                                                                                            n_distinct : function (..., na.rm = FALSE)  
                                                                                                                                                                                                                                                                                              n_groups : function (x)  
                                                                                                                                                                                                                                                                                                na_if : function (x, y)  
                                                                                                                                                                                                                                                                                                  near : function (x, y, tol = .Machine$double.eps^0.5)  
                                                                                                                                                                                                                                                                                                    nest_by : function (.data, ..., .key = "data", .keep = FALSE)  
                                                                                                                                                                                                                                                                                                      nest_join : function (x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ...)  
                                                                                                                                                                                                                                                                                                        new_grouped_df : function (x, groups, ..., class = character())  
                                                                                                                                                                                                                                                                                                          new_rowwise_df : function (data, group_data = NULL, ..., class = character())  
                                                                                                                                                                                                                                                                                                            nth : function (x, n, order_by = NULL, default = default_missing(x))  
                                                                                                                                                                                                                                                                                                              ntile : function (x = row_number(), n)  
                                                                                                                                                                                                                                                                                                                num_range : function (prefix, range, width = NULL, vars = NULL)  
                                                                                                                                                                                                                                                                                                                  one_of : function (..., .vars = NULL)  
                                                                                                                                                                                                                                                                                                                    order_by : function (order_by, call)  
                                                                                                                                                                                                                                                                                                                      percent_rank : function (x)  
                                                                                                                                                                                                                                                                                                                        progress_estimated : function (n, min_time = 0)  
                                                                                                                                                                                                                                                                                                                          pull : function (.data, var = -1, name = NULL, ...)  
                                                                                                                                                                                                                                                                                                                            quo : function (expr)  
                                                                                                                                                                                                                                                                                                                              quo_name : function (quo)  
                                                                                                                                                                                                                                                                                                                                quos : function (..., .named = FALSE, .ignore_empty = c("trailing", "none", "all"), .unquote_names = TRUE)  
                                                                                                                                                                                                                                                                                                                                  recode : function (.x, ..., .default = NULL, .missing = NULL)  
                                                                                                                                                                                                                                                                                                                                    recode_factor : function (.x, ..., .default = NULL, .missing = NULL, .ordered = FALSE)  
                                                                                                                                                                                                                                                                                                                                      relocate : function (.data, ..., .before = NULL, .after = NULL)  
                                                                                                                                                                                                                                                                                                                                        rename : function (.data, ...)  
                                                                                                                                                                                                                                                                                                                                          rename_ : function (.data, ..., .dots = list())  
                                                                                                                                                                                                                                                                                                                                            rename_all : function (.tbl, .funs = list(), ...)  
                                                                                                                                                                                                                                                                                                                                              rename_at : function (.tbl, .vars, .funs = list(), ...)  
                                                                                                                                                                                                                                                                                                                                                rename_if : function (.tbl, .predicate, .funs = list(), ...)  
                                                                                                                                                                                                                                                                                                                                                  rename_vars : function (vars = chr(), ..., strict = TRUE)  
                                                                                                                                                                                                                                                                                                                                                    rename_vars_ : function (vars, args)  
                                                                                                                                                                                                                                                                                                                                                      rename_with : function (.data, .fn, .cols = everything(), ...)  
                                                                                                                                                                                                                                                                                                                                                        right_join : function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = FALSE)  
                                                                                                                                                                                                                                                                                                                                                          row_number : function (x)  
                                                                                                                                                                                                                                                                                                                                                            rows_append : function (x, y, ..., copy = FALSE, in_place = FALSE)  
                                                                                                                                                                                                                                                                                                                                                              rows_delete : function (x, y, by = NULL, ..., unmatched = c("error", "ignore"), copy = FALSE, in_place = FALSE)  
                                                                                                                                                                                                                                                                                                                                                                rows_insert : function (x, y, by = NULL, ..., conflict = c("error", "ignore"), copy = FALSE, in_place = FALSE)  
                                                                                                                                                                                                                                                                                                                                                                  rows_patch : function (x, y, by = NULL, ..., unmatched = c("error", "ignore"), copy = FALSE, in_place = FALSE)  
                                                                                                                                                                                                                                                                                                                                                                    rows_update : function (x, y, by = NULL, ..., unmatched = c("error", "ignore"), copy = FALSE, in_place = FALSE)  
                                                                                                                                                                                                                                                                                                                                                                      rows_upsert : function (x, y, by = NULL, ..., copy = FALSE, in_place = FALSE)  
                                                                                                                                                                                                                                                                                                                                                                        rowwise : function (data, ...)  
                                                                                                                                                                                                                                                                                                                                                                          same_src : function (x, y)  
                                                                                                                                                                                                                                                                                                                                                                            sample_frac : function (tbl, size = 1, replace = FALSE, weight = NULL, .env = NULL, ...)  
                                                                                                                                                                                                                                                                                                                                                                              sample_n : function (tbl, size, replace = FALSE, weight = NULL, .env = NULL, ...)  
                                                                                                                                                                                                                                                                                                                                                                                select : function (.data, ...)  
                                                                                                                                                                                                                                                                                                                                                                                  select_ : function (.data, ..., .dots = list())  
                                                                                                                                                                                                                                                                                                                                                                                    select_all : function (.tbl, .funs = list(), ...)  
                                                                                                                                                                                                                                                                                                                                                                                      select_at : function (.tbl, .vars, .funs = list(), ...)  
                                                                                                                                                                                                                                                                                                                                                                                        select_if : function (.tbl, .predicate, .funs = list(), ...)  
                                                                                                                                                                                                                                                                                                                                                                                          select_var : function (vars, var = -1)  
                                                                                                                                                                                                                                                                                                                                                                                            select_vars : function (vars = chr(), ..., include = chr(), exclude = chr())  
                                                                                                                                                                                                                                                                                                                                                                                              select_vars_ : function (vars, args, include = chr(), exclude = chr())  
                                                                                                                                                                                                                                                                                                                                                                                                semi_join : function (x, y, by = NULL, copy = FALSE, ...)  
                                                                                                                                                                                                                                                                                                                                                                                                  setdiff : function (x, y, ...)  
                                                                                                                                                                                                                                                                                                                                                                                                    setequal : function (x, y, ...)  
                                                                                                                                                                                                                                                                                                                                                                                                      show_query : function (x, ...)  
                                                                                                                                                                                                                                                                                                                                                                                                        slice : function (.data, ..., .preserve = FALSE)  
                                                                                                                                                                                                                                                                                                                                                                                                          slice_ : function (.data, ..., .dots = list())  
                                                                                                                                                                                                                                                                                                                                                                                                            slice_head : function (.data, ..., n, prop)  
                                                                                                                                                                                                                                                                                                                                                                                                              slice_max : function (.data, order_by, ..., n, prop, with_ties = TRUE)  
                                                                                                                                                                                                                                                                                                                                                                                                                slice_min : function (.data, order_by, ..., n, prop, with_ties = TRUE)  
                                                                                                                                                                                                                                                                                                                                                                                                                  slice_sample : function (.data, ..., n, prop, weight_by = NULL, replace = FALSE)  
                                                                                                                                                                                                                                                                                                                                                                                                                    slice_tail : function (.data, ..., n, prop)  
                                                                                                                                                                                                                                                                                                                                                                                                                      sql : function (...)  
                                                                                                                                                                                                                                                                                                                                                                                                                        sql_escape_ident : function (con, x)  
                                                                                                                                                                                                                                                                                                                                                                                                                          sql_escape_string : function (con, x)  
                                                                                                                                                                                                                                                                                                                                                                                                                            sql_join : function (con, x, y, vars, type = "inner", by = NULL, ...)  
                                                                                                                                                                                                                                                                                                                                                                                                                              sql_select : function (con, select, from, where = NULL, group_by = NULL, having = NULL, order_by = NULL, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                     limit = NULL, distinct = FALSE, ...)  
                                                                                                                                                                                                                                                                                                                                                                                                                                sql_semi_join : function (con, x, y, anti = FALSE, by = NULL, ...)  
                                                                                                                                                                                                                                                                                                                                                                                                                                  sql_set_op : function (con, x, y, method)  
                                                                                                                                                                                                                                                                                                                                                                                                                                    sql_subquery : function (con, from, name = random_table_name(), ...)  
                                                                                                                                                                                                                                                                                                                                                                                                                                      sql_translate_env : function (con)  
                                                                                                                                                                                                                                                                                                                                                                                                                                        src : function (subclass, ...)  
                                                                                                                                                                                                                                                                                                                                                                                                                                          src_df : function (pkg = NULL, env = NULL)  
                                                                                                                                                                                                                                                                                                                                                                                                                                            src_local : function (tbl, pkg = NULL, env = NULL)  
                                                                                                                                                                                                                                                                                                                                                                                                                                              src_mysql : function (dbname, host = NULL, port = 0L, username = "root", password = "", ...)  
                                                                                                                                                                                                                                                                                                                                                                                                                                                src_postgres : function (dbname = NULL, host = NULL, port = NULL, user = NULL, password = NULL, ...)  
                                                                                                                                                                                                                                                                                                                                                                                                                                                  src_sqlite : function (path, create = FALSE)  
                                                                                                                                                                                                                                                                                                                                                                                                                                                    src_tbls : function (x, ...)  
                                                                                                                                                                                                                                                                                                                                                                                                                                                      starts_with : function (match, ignore.case = TRUE, vars = NULL)  
                                                                                                                                                                                                                                                                                                                                                                                                                                                        starwars : tibble [87 × 14] (S3: tbl_df/tbl/data.frame)
storms : tibble [11,859 × 13] (S3: tbl_df/tbl/data.frame)
summarise : function (.data, ..., .groups = NULL)  
  summarise_ : function (.data, ..., .dots = list())  
    summarise_all : function (.tbl, .funs, ...)  
      summarise_at : function (.tbl, .vars, .funs, ..., .cols = NULL)  
        summarise_each : function (tbl, funs, ...)  
          summarise_each_ : function (tbl, funs, vars)  
            summarise_if : function (.tbl, .predicate, .funs, ...)  
              summarize : function (.data, ..., .groups = NULL)  
                summarize_ : function (.data, ..., .dots = list())  
                  summarize_all : function (.tbl, .funs, ...)  
                    summarize_at : function (.tbl, .vars, .funs, ..., .cols = NULL)  
                      summarize_each : function (tbl, funs, ...)  
                        summarize_each_ : function (tbl, funs, vars)  
                          summarize_if : function (.tbl, .predicate, .funs, ...)  
                            sym : function (x)  
                              syms : function (x)  
                                tally : function (x, wt = NULL, sort = FALSE, name = NULL)  
                                  tally_ : function (x, wt, sort = FALSE)  
                                    tbl : function (src, ...)  
                                      tbl_df : function (data)  
                                        tbl_nongroup_vars : function (x)  
                                          tbl_ptype : function (.data)  
                                            tbl_sum : function (x)  
                                              tbl_vars : function (x)  
                                                tibble : function (..., .rows = NULL, .name_repair = c("check_unique", "unique", "universal", 
                                                                                                       "minimal"))  
                                                  top_frac : function (x, n, wt)  
                                                    top_n : function (x, n, wt)  
                                                      transmute : function (.data, ...)  
                                                        transmute_ : function (.data, ..., .dots = list())  
                                                          transmute_all : function (.tbl, .funs, ...)  
                                                            transmute_at : function (.tbl, .vars, .funs, ..., .cols = NULL)  
                                                              transmute_if : function (.tbl, .predicate, .funs, ...)  
                                                                tribble : function (...)  
                                                                  type_sum : function (x)  
                                                                    ungroup : function (x, ...)  
                                                                      union : function (x, y, ...)  
                                                                        union_all : function (x, y, ...)  
                                                                          validate_grouped_df : function (x, check_bounds = FALSE)  
                                                                            validate_rowwise_df : function (x)  
                                                                              vars : function (...)  
                                                                                with_groups : function (.data, .groups, .f, ...)  
                                                                                  with_order : function (order_by, fun, x, ...)  
                                                                                    wrap_dbplyr_obj : function (obj_name)  
                                                                                      > 
library(DT)
%>% : function (lhs, rhs)  
  addRow : function (proxy, data, resetPaging = TRUE)  
    clearSearch : function (proxy)  
      coerceValue : function (val, old)  
        colReorder : function (proxy, order, origOrder = FALSE)  
          datatable : function (data, options = list(), class = "display", callback = JS("return table;"), 
                                rownames, colnames, container, caption = NULL, filter = c("none", "bottom", "top"), 
                                escape = TRUE, style = "auto", width = NULL, height = NULL, elementId = NULL, 
                                fillContainer = getOption("DT.fillContainer", NULL), autoHideNavigation = getOption("DT.autoHideNavigation", 
                                                                                                                    NULL), selection = c("multiple", "single", "none"), extensions = list(), plugins = NULL, 
                                editable = FALSE)  
            dataTableAjax : function (session, data, rownames, filter = dataTablesFilter, outputId)  
              dataTableOutput : function (outputId, width = "100%", height = "auto")  
                dataTableProxy : function (outputId, session = shiny::getDefaultReactiveDomain(), deferUntilFlush = TRUE)  
                  doColumnSearch : function (x, search_string, options = list())  
                    doGlobalSearch : function (data, search_string, options = list())  
                      DTOutput : function (outputId, width = "100%", height = "auto")  
                        editData : function (data, info, proxy = NULL, rownames = TRUE, resetPaging = FALSE, ...)  
                          formatCurrency : function (table, columns, currency = "$", interval = 3, mark = ",", digits = 2, dec.mark = getOption("OutDec"), 
                                                     before = TRUE, zero.print = NULL, rows = NULL)  
                            formatDate : function (table, columns, method = "toDateString", params = NULL, rows = NULL)  
                              formatPercentage : function (table, columns, digits = 0, interval = 3, mark = ",", dec.mark = getOption("OutDec"), 
                                                           zero.print = NULL, rows = NULL)  
                                formatRound : function (table, columns, digits = 2, interval = 3, mark = ",", dec.mark = getOption("OutDec"), 
                                                        zero.print = NULL, rows = NULL)  
                                  formatSignif : function (table, columns, digits = 2, interval = 3, mark = ",", dec.mark = getOption("OutDec"), 
                                                           zero.print = NULL, rows = NULL)  
                                    formatString : function (table, columns, prefix = "", suffix = "", rows = NULL)  
                                      formatStyle : function (table, columns, valueColumns = columns, target = c("cell", "row"), fontWeight = NULL, 
                                                              color = NULL, backgroundColor = NULL, background = NULL, ...)  
                                        hideCols : function (proxy, hide, reset = FALSE)  
                                          JS : function (...)  
                                            reloadData : function (proxy, resetPaging = TRUE, clearSelection = c("all", "none", "row", "column", 
                                                                                                                 "cell"))  
                                              renderDataTable : function (expr, server = TRUE, env = parent.frame(), quoted = FALSE, funcFilter = dataTablesFilter, 
                                                                          ...)  
                                                renderDT : function (expr, server = TRUE, env = parent.frame(), quoted = FALSE, funcFilter = dataTablesFilter, 
                                                                     ...)  
                                                  replaceData : function (proxy, data, ..., resetPaging = TRUE, clearSelection = "all")  
                                                    saveWidget : function (widget, file, selfcontained = TRUE, libdir = NULL, background = "white", 
                                                                           title = class(widget)[[1]], knitrOptions = list())  
                                                      selectCells : function (proxy, selected, ignore.selectable = FALSE)  
                                                        selectColumns : function (proxy, selected, ignore.selectable = FALSE)  
                                                          selectPage : function (proxy, page)  
                                                            selectRows : function (proxy, selected, ignore.selectable = FALSE)  
                                                              showCols : function (proxy, show, reset = FALSE)  
                                                                styleColorBar : function (data, color, angle = 90)  
                                                                  styleEqual : function (levels, values, default = NULL)  
                                                                    styleInterval : function (cuts, values)  
                                                                      styleRow : function (rows, values, default = NULL)  
                                                                        styleValue : function ()  
                                                                          tableFooter : function (names, escape = TRUE)  
                                                                            tableHeader : function (names, escape = TRUE)  
                                                                              updateCaption : function (proxy, caption)  
                                                                                updateFilters : function (proxy, data)  
                                                                                  updateSearch : function (proxy, keywords = list(global = NULL, columns = NULL))  
                                                                                    > 
library(fastmatch)
%fin% : function (x, table)  
  coalesce : function (x)  
    ctapply : function (X, INDEX, FUN, ..., MERGE = c)  
      fmatch : function (x, table, nomatch = NA_integer_, incomparables = NULL)  
        fmatch.hash : function (x, table, nomatch = NA_integer_, incomparables = NULL)  
          > 
library(fortunes) 
fortune : function (which = NULL, fortunes.data = NULL, fixed = TRUE, showMatches = FALSE, author = character(), 
                    ...)  
  read.fortunes : function (file = NULL)  
    > 
library(fun)
alzheimer_test : function (char1 = c("9", "O", "M", "I", "F", "D"), char2 = c("6", "C", "N", "T", "E", 
                                                                              "O"), nr = 10, nc = 30, seed = NULL, ...)  
  gomoku : function (n = 19)  
    htmlspecialchars : function (string)  
      lights_out : function (width = 5, height = 5, steps = 3, cheat = FALSE, col.off = "black", col.on = "white", 
                             col.frame = "lightblue", seed = NULL)  
        mine_sweeper : function (width = 10, height = 10, mines = 20, cheat = FALSE)  
          random_password : function (length = 12, replace = FALSE, extended = TRUE)  
            shutdown : function (wait = 0)  
              sliding_puzzle : function (size = c(3, 3), bg = "lightblue", z = NULL)  
                tag_cloud : function (tagData, htmlOutput = "tagCloud.html", SWFPath = "tagcloud.swf", JSPath = "swfobject.js", 
                                      divId = "tagCloudId", width = 600, height = 400, transparent = FALSE, tcolor = "333333", 
                                      tcolor2 = "009900", hicolor = "ff0000", distr = "true", tspeed = 100, version = 9, 
                                      bgcolor = "ffffff", useXML = FALSE, htmlTitle = "Tag Cloud", noFlashJS, target = NULL, 
                                      scriptOnly = FALSE, encode = FALSE, reserved = FALSE)  
                  tagData : 'data.frame':	45 obs. of  5 variables:
  $ tag    : chr  "2D Kernel Density" "algorithm" "Animation" "AniWiki" ...
$ link   : chr  "http://yihui.name/en/tag/2d-kernel-density/" "http://yihui.name/en/tag/algorithm/" "http://yihui.name/en/tag/animation/" "http://yihui.name/en/tag/aniwiki/" ...
$ count  : num  4 4 44 8 4 4 4 4 4 4 ...
$ color  : chr  "2163bb" "9f0f38" "800130" "7ce1df" ...
$ hicolor: chr  "f0763d" "d825b1" "5b8d6a" "6607b0" ...
tower_of_hanoi : function (n = 7)  
  > 
library(geosphere)
.__T__$:base : <environment: 0x000001dbff8d38e8> 
  .__T__$<-:base : <environment: 0x000001db8008e288> 
  .__T__[:base : <environment: 0x000001db800dc700> 
           .__T__[[:base : <environment: 0x000001db80317c00> 
                     .__T__[[<-:base : <environment: 0x000001db80498320> 
                               .__T__[<-:base : <environment: 0x000001db804de820> 
                                        .__T__areaPolygon:geosphere : <environment: 0x000001db81ddee88> 
                                        .__T__centroid:geosphere : <environment: 0x000001db81e45400> 
                                        .__T__perimeter:geosphere : <environment: 0x000001db81ed6348> 
                                        .__T__span:geosphere : <environment: 0x000001db81fa5240> 
                                        alongTrackDistance : function (p1, p2, p3, r = 6378137)  
                                          antipodal : function (p1, p2, tol = 1e-09)  
                                            antipode : function (p)  
                                              areaPolygon : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      bearing : function (p1, p2, a = 6378137, f = 1/298.257223563)  
                                        bearingRhumb : function (p1, p2)  
                                          centroid : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      daylength : function (lat, doy)  
                                        destPoint : function (p, b, d, a = 6378137, f = 1/298.257223563, ...)  
                                          destPointRhumb : function (p, b, d, r = 6378137)  
                                            dist2gc : function (p1, p2, p3, r = 6378137, sign = FALSE)  
                                              dist2Line : function (p, line, distfun = distGeo)  
                                                distCosine : function (p1, p2, r = 6378137)  
                                                  distGeo : function (p1, p2, a = 6378137, f = 1/298.257223563)  
                                                    distHaversine : function (p1, p2, r = 6378137)  
                                                      distm : function (x, y, fun = distGeo)  
                                                        distMeeus : function (p1, p2, a = 6378137, f = 1/298.257223563)  
                                                          distRhumb : function (p1, p2, r = 6378137)  
                                                            distVincentyEllipsoid : function (p1, p2, a = 6378137, b = 6356752.3142, f = 1/298.257223563)  
                                                              distVincentySphere : function (p1, p2, r = 6378137)  
                                                                finalBearing : function (p1, p2, a = 6378137, f = 1/298.257223563, sphere = FALSE)  
                                                                  gcIntermediate : function (p1, p2, n = 50, breakAtDateLine = FALSE, addStartEnd = FALSE, sp = FALSE, 
                                                                                             sepNA = FALSE)  
                                                                    gcIntersect : function (p1, p2, p3, p4)  
                                                                      gcIntersectBearing : function (p1, brng1, p2, brng2)  
                                                                        gcLat : function (p1, p2, lon)  
                                                                          gcLon : function (p1, p2, lat)  
                                                                            gcMaxLat : function (p1, p2)  
                                                                              geodesic : function (p, azi, d, a = 6378137, f = 1/298.257223563, ...)  
                                                                                geodesic_inverse : function (p1, p2, a = 6378137, f = 1/298.257223563, ...)  
                                                                                  geomean : function (xy, w = NULL)  
                                                                                    greatCircle : function (p1, p2, n = 360, sp = FALSE)  
                                                                                      greatCircleBearing : function (p, brng, n = 360)  
                                                                                        horizon : function (h, r = 6378137)  
                                                                                          lengthLine : function (line)  
                                                                                            makeLine : function (p, interval = 10000, sp = FALSE, ...)  
                                                                                              makePoly : function (p, interval = 10000, sp = FALSE, ...)  
                                                                                                mercator : function (p, inverse = FALSE, r = 6378137)  
                                                                                                  midPoint : function (p1, p2, a = 6378137, f = 1/298.257223563)  
                                                                                                    onGreatCircle : function (p1, p2, p3, tol = 1e-04)  
                                                                                                      perimeter : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      plotArrows : function (p, fraction = 0.9, length = 0.15, first = "", add = FALSE, ...)  
                                        randomCoordinates : function (n)  
                                          refEllipsoids : function ()  
                                            regularCoordinates : function (N)  
                                              span : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      >                                                                                         
library(ggplot2)
"x"
"%+% : function (e1, e2)  "
"%+replace% : function (e1, e2)  "
".data :  list()"
".pt :  num 2.85"
".stroke :  num 3.78"
"aes : function (x, y, ...)  "
"aes_ : function (x, y, ...)  "
"aes_all : function (vars)  "
"aes_auto : function (data = NULL, ...)  "
"aes_q : function (x, y, ...)  "
"aes_string : function (x, y, ...)  "
"after_scale : function (x)  "
"after_stat : function (x)  "
"alpha : function (colour, alpha = NA)  "
"annotate : function (geom, x = NULL, y = NULL, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, "
"    xend = NULL, yend = NULL, ..., na.rm = FALSE)  "
"annotation_custom : function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)  "
"annotation_logticks : function (base = 10, sides = \"bl\", outside = FALSE, scaled = TRUE, short = unit(0.1, "
"    \"cm\"), mid = unit(0.2, \"cm\"), long = unit(0.3, \"cm\"), colour = \"black\", size = 0.5, "
"    linetype = 1, alpha = 1, color = NULL, ...)  "
"annotation_map : function (map, ...)  "
"annotation_raster : function (raster, xmin, xmax, ymin, ymax, interpolate = FALSE)  "
"arrow : function (angle = 30, length = unit(0.25, \"inches\"), ends = \"last\", type = \"open\")  "
"as_label : function (x)  "
"as_labeller : function (x, default = label_value, multi_line = TRUE)  "
"autolayer : function (object, ...)  "
"autoplot : function (object, ...)  "
"AxisSecondary : Classes 'AxisSecondary', 'ggproto', 'gg' <ggproto object: Class AxisSecondary, gg>"
"    axis: NULL"
"    break_info: function"
"    breaks: waiver"
"    create_scale: function"
"    detail: 1000"
"    empty: function"
"    init: function"
"    labels: waiver"
"    make_title: function"
"    mono_test: function"
"    name: waiver"
"    trans: NULL"
"    transform_range: function "
"benchplot : function (x)  "
"binned_scale : function (aesthetics, scale_name, palette, name = waiver(), breaks = waiver(), labels = waiver(), "
"    limits = NULL, rescaler = rescale, oob = squish, expand = waiver(), na.value = NA_real_, "
"    n.breaks = NULL, nice.breaks = TRUE, right = TRUE, trans = \"identity\", show.limits = FALSE, "
"    guide = \"bins\", position = \"left\", super = ScaleBinned)  "
"borders : function (database = \"world\", regions = \".\", fill = NA, colour = \"grey50\", xlim = NULL, "
"    ylim = NULL, ...)  "
"calc_element : function (element, theme, verbose = FALSE, skip_blank = FALSE)  "
"combine_vars : function (data, env = emptyenv(), vars = NULL, drop = TRUE)  "
"continuous_scale : function (aesthetics, scale_name, palette, name = waiver(), breaks = waiver(), minor_breaks = waiver(), "
"    n.breaks = NULL, labels = waiver(), limits = NULL, rescaler = rescale, oob = censor, "
"    expand = waiver(), na.value = NA_real_, trans = \"identity\", guide = \"legend\", "
"    position = \"left\", super = ScaleContinuous)  "
"Coord : Classes 'Coord', 'ggproto', 'gg' <ggproto object: Class Coord, gg>"
"    aspect: function"
"    backtransform_range: function"
"    clip: on"
"    default: FALSE"
"    distance: function"
"    is_free: function"
"    is_linear: function"
"    labels: function"
"    modify_scales: function"
"    range: function"
"    render_axis_h: function"
"    render_axis_v: function"
"    render_bg: function"
"    render_fg: function"
"    setup_data: function"
"    setup_layout: function"
"    setup_panel_guides: function"
"    setup_panel_params: function"
"    setup_params: function"
"    train_panel_guides: function"
"    transform: function "
"coord_cartesian : function (xlim = NULL, ylim = NULL, expand = TRUE, default = FALSE, clip = \"on\")  "
"coord_equal : function (ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = \"on\")  "
"coord_fixed : function (ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = \"on\")  "
"coord_flip : function (xlim = NULL, ylim = NULL, expand = TRUE, clip = \"on\")  "
"coord_map : function (projection = \"mercator\", ..., parameters = NULL, orientation = NULL, xlim = NULL, "
"    ylim = NULL, clip = \"on\")  "
"coord_munch : function (coord, data, range, segment_length = 0.01)  "
"coord_polar : function (theta = \"x\", start = 0, direction = 1, clip = \"on\")  "
"coord_quickmap : function (xlim = NULL, ylim = NULL, expand = TRUE, clip = \"on\")  "
"coord_sf : function (xlim = NULL, ylim = NULL, expand = TRUE, crs = NULL, default_crs = NULL, "
"    datum = sf::st_crs(4326), label_graticule = waiver(), label_axes = waiver(), lims_method = c(\"cross\", "
"        \"box\", \"orthogonal\", \"geometry_bbox\"), ndiscr = 100, default = FALSE, clip = \"on\")  "
"coord_trans : function (x = \"identity\", y = \"identity\", xlim = NULL, ylim = NULL, limx = \"DEPRECATED\", "
"    limy = \"DEPRECATED\", clip = \"on\", expand = TRUE)  "
"CoordCartesian : Classes 'CoordCartesian', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordCartesian, Coord, gg>"
"    aspect: function"
"    backtransform_range: function"
"    clip: on"
"    default: FALSE"
"    distance: function"
"    is_free: function"
"    is_linear: function"
"    labels: function"
"    modify_scales: function"
"    range: function"
"    render_axis_h: function"
"    render_axis_v: function"
"    render_bg: function"
"    render_fg: function"
"    setup_data: function"
"    setup_layout: function"
"    setup_panel_guides: function"
"    setup_panel_params: function"
"    setup_params: function"
"    train_panel_guides: function"
"    transform: function"
"    super:  <ggproto object: Class Coord, gg> "
"CoordFixed : Classes 'CoordFixed', 'CoordCartesian', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordFixed, CoordCartesian, Coord, gg>"
"    aspect: function"
"    backtransform_range: function"
"    clip: on"
"    default: FALSE"
"    distance: function"
"    is_free: function"
"    is_linear: function"
"    labels: function"
"    modify_scales: function"
"    range: function"
"    render_axis_h: function"
"    render_axis_v: function"
"    render_bg: function"
"    render_fg: function"
"    setup_data: function"
"    setup_layout: function"
"    setup_panel_guides: function"
"    setup_panel_params: function"
"    setup_params: function"
"    train_panel_guides: function"
"    transform: function"
"    super:  <ggproto object: Class CoordCartesian, Coord, gg> "
"CoordFlip : Classes 'CoordFlip', 'CoordCartesian', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordFlip, CoordCartesian, Coord, gg>"
"    aspect: function"
"    backtransform_range: function"
"    clip: on"
"    default: FALSE"
"    distance: function"
"    is_free: function"
"    is_linear: function"
"    labels: function"
"    modify_scales: function"
"    range: function"
"    render_axis_h: function"
"    render_axis_v: function"
"    render_bg: function"
"    render_fg: function"
"    setup_data: function"
"    setup_layout: function"
"    setup_panel_guides: function"
"    setup_panel_params: function"
"    setup_params: function"
"    train_panel_guides: function"
"    transform: function"
"    super:  <ggproto object: Class CoordCartesian, Coord, gg> "
"CoordMap : Classes 'CoordMap', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordMap, Coord, gg>"
"    aspect: function"
"    backtransform_range: function"
"    clip: on"
"    default: FALSE"
"    distance: function"
"    is_free: function"
"    is_linear: function"
"    labels: function"
"    modify_scales: function"
"    range: function"
"    render_axis_h: function"
"    render_axis_v: function"
"    render_bg: function"
"    render_fg: function"
"    setup_data: function"
"    setup_layout: function"
"    setup_panel_guides: function"
"    setup_panel_params: function"
"    setup_params: function"
"    train_panel_guides: function"
"    transform: function"
"    super:  <ggproto object: Class Coord, gg> "
"CoordPolar : Classes 'CoordPolar', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordPolar, Coord, gg>"
"    aspect: function"
"    backtransform_range: function"
"    clip: on"
"    default: FALSE"
"    distance: function"
"    is_free: function"
"    is_linear: function"
"    labels: function"
"    modify_scales: function"
"    range: function"
"    render_axis_h: function"
"    render_axis_v: function"
"    render_bg: function"
"    render_fg: function"
"    setup_data: function"
"    setup_layout: function"
"    setup_panel_guides: function"
"    setup_panel_params: function"
"    setup_params: function"
"    train_panel_guides: function"
"    transform: function"
"    super:  <ggproto object: Class Coord, gg> "
"CoordQuickmap : Classes 'CoordQuickmap', 'CoordCartesian', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordQuickmap, CoordCartesian, Coord, gg>"
"    aspect: function"
"    backtransform_range: function"
"    clip: on"
"    default: FALSE"
"    distance: function"
"    is_free: function"
"    is_linear: function"
"    labels: function"
"    modify_scales: function"
"    range: function"
"    render_axis_h: function"
"    render_axis_v: function"
"    render_bg: function"
"    render_fg: function"
"    setup_data: function"
"    setup_layout: function"
"    setup_panel_guides: function"
"    setup_panel_params: function"
"    setup_params: function"
"    train_panel_guides: function"
"    transform: function"
"    super:  <ggproto object: Class CoordCartesian, Coord, gg> "
"CoordSf : Classes 'CoordSf', 'CoordCartesian', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordSf, CoordCartesian, Coord, gg>"
"    aspect: function"
"    backtransform_range: function"
"    clip: on"
"    default: FALSE"
"    determine_crs: function"
"    distance: function"
"    fixup_graticule_labels: function"
"    get_default_crs: function"
"    is_free: function"
"    is_linear: function"
"    labels: function"
"    lims_method: cross"
"    modify_scales: function"
"    params: list"
"    range: function"
"    record_bbox: function"
"    render_axis_h: function"
"    render_axis_v: function"
"    render_bg: function"
"    render_fg: function"
"    setup_data: function"
"    setup_layout: function"
"    setup_panel_guides: function"
"    setup_panel_params: function"
"    setup_params: function"
"    train_panel_guides: function"
"    transform: function"
"    super:  <ggproto object: Class CoordCartesian, Coord, gg> "
"CoordTrans : Classes 'CoordTrans', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordTrans, Coord, gg>"
"    aspect: function"
"    backtransform_range: function"
"    clip: on"
"    default: FALSE"
"    distance: function"
"    is_free: function"
"    is_linear: function"
"    labels: function"
"    modify_scales: function"
"    range: function"
"    render_axis_h: function"
"    render_axis_v: function"
"    render_bg: function"
"    render_fg: function"
"    setup_data: function"
"    setup_layout: function"
"    setup_panel_guides: function"
"    setup_panel_params: function"
"    setup_params: function"
"    train_panel_guides: function"
"    transform: function"
"    super:  <ggproto object: Class Coord, gg> "
"cut_interval : function (x, n = NULL, length = NULL, ...)  "
"cut_number : function (x, n = NULL, ...)  "
"cut_width : function (x, width, center = NULL, boundary = NULL, closed = c(\"right\", \"left\"), ...)  "
"derive : function ()  "
"diamonds : tibble [53,940 × 10] (S3: tbl_df/tbl/data.frame)"
"discrete_scale : function (aesthetics, scale_name, palette, name = waiver(), breaks = waiver(), labels = waiver(), "
"    limits = NULL, expand = waiver(), na.translate = TRUE, na.value = NA, drop = TRUE, "
"    guide = \"legend\", position = \"left\", super = ScaleDiscrete)  "
"draw_key_abline : function (data, params, size)  "
"draw_key_blank : function (data, params, size)  "
"draw_key_boxplot : function (data, params, size)  "
"draw_key_crossbar : function (data, params, size)  "
"draw_key_dotplot : function (data, params, size)  "
"draw_key_label : function (data, params, size)  "
"draw_key_path : function (data, params, size)  "
"draw_key_point : function (data, params, size)  "
"draw_key_pointrange : function (data, params, size)  "
"draw_key_polygon : function (data, params, size)  "
"draw_key_rect : function (data, params, size)  "
"draw_key_smooth : function (data, params, size)  "
"draw_key_text : function (data, params, size)  "
"draw_key_timeseries : function (data, params, size)  "
"draw_key_vline : function (data, params, size)  "
"draw_key_vpath : function (data, params, size)  "
"dup_axis : function (trans = ~., name = derive(), breaks = derive(), labels = derive(), guide = derive())  "
"economics : spec_tbl_df [574 × 6] (S3: spec_tbl_df/tbl_df/tbl/data.frame)"
"economics_long : tibble [2,870 × 4] (S3: tbl_df/tbl/data.frame)"
"el_def : function (class = NULL, inherit = NULL, description = NULL)  "
"element_blank : function ()  "
"element_grob : function (element, ...)  "
"element_line : function (colour = NULL, size = NULL, linetype = NULL, lineend = NULL, color = NULL, "
"    arrow = NULL, inherit.blank = FALSE)  "
"element_rect : function (fill = NULL, colour = NULL, size = NULL, linetype = NULL, color = NULL, "
"    inherit.blank = FALSE)  "
"element_render : function (theme, element, ..., name = NULL)  "
"element_text : function (family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, "
"    angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE)  "
"enexpr : function (arg)  "
"enexprs : function (..., .named = FALSE, .ignore_empty = c(\"trailing\", \"none\", \"all\"), .unquote_names = TRUE, "
"    .homonyms = c(\"keep\", \"first\", \"last\", \"error\"), .check_assign = FALSE)  "
"enquo : function (arg)  "
"enquos : function (..., .named = FALSE, .ignore_empty = c(\"trailing\", \"none\", \"all\"), .unquote_names = TRUE, "
"    .homonyms = c(\"keep\", \"first\", \"last\", \"error\"), .check_assign = FALSE)  "
"ensym : function (arg)  "
"ensyms : function (..., .named = FALSE, .ignore_empty = c(\"trailing\", \"none\", \"all\"), .unquote_names = TRUE, "
"    .homonyms = c(\"keep\", \"first\", \"last\", \"error\"), .check_assign = FALSE)  "
"expand_limits : function (...)  "
"expand_scale : function (mult = 0, add = 0)  "
"expansion : function (mult = 0, add = 0)  "
"expr : function (expr)  "
"Facet : Classes 'Facet', 'ggproto', 'gg' <ggproto object: Class Facet, gg>"
"    compute_layout: function"
"    draw_back: function"
"    draw_front: function"
"    draw_labels: function"
"    draw_panels: function"
"    finish_data: function"
"    init_scales: function"
"    map_data: function"
"    params: list"
"    setup_data: function"
"    setup_params: function"
"    shrink: FALSE"
"    train_scales: function"
"    vars: function "
"facet_grid : function (rows = NULL, cols = NULL, scales = \"fixed\", space = \"fixed\", shrink = TRUE, "
"    labeller = \"label_value\", as.table = TRUE, switch = NULL, drop = TRUE, margins = FALSE, "
"    facets = NULL)  "
"facet_null : function (shrink = TRUE)  "
"facet_wrap : function (facets, nrow = NULL, ncol = NULL, scales = \"fixed\", shrink = TRUE, labeller = \"label_value\", "
"    as.table = TRUE, switch = NULL, drop = TRUE, dir = \"h\", strip.position = \"top\")  "
"FacetGrid : Classes 'FacetGrid', 'Facet', 'ggproto', 'gg' <ggproto object: Class FacetGrid, Facet, gg>"
"    compute_layout: function"
"    draw_back: function"
"    draw_front: function"
"    draw_labels: function"
"    draw_panels: function"
"    finish_data: function"
"    init_scales: function"
"    map_data: function"
"    params: list"
"    setup_data: function"
"    setup_params: function"
"    shrink: TRUE"
"    train_scales: function"
"    vars: function"
"    super:  <ggproto object: Class Facet, gg> "
"FacetNull : Classes 'FacetNull', 'Facet', 'ggproto', 'gg' <ggproto object: Class FacetNull, Facet, gg>"
"    compute_layout: function"
"    draw_back: function"
"    draw_front: function"
"    draw_labels: function"
"    draw_panels: function"
"    finish_data: function"
"    init_scales: function"
"    map_data: function"
"    params: list"
"    setup_data: function"
"    setup_params: function"
"    shrink: TRUE"
"    train_scales: function"
"    vars: function"
"    super:  <ggproto object: Class Facet, gg> "
"FacetWrap : Classes 'FacetWrap', 'Facet', 'ggproto', 'gg' <ggproto object: Class FacetWrap, Facet, gg>"
"    compute_layout: function"
"    draw_back: function"
"    draw_front: function"
"    draw_labels: function"
"    draw_panels: function"
"    finish_data: function"
"    init_scales: function"
"    map_data: function"
"    params: list"
"    setup_data: function"
"    setup_params: function"
"    shrink: TRUE"
"    train_scales: function"
"    vars: function"
"    super:  <ggproto object: Class Facet, gg> "
"faithfuld : tibble [5,625 × 3] (S3: tbl_df/tbl/data.frame)"
"find_panel : function (table)  "
"flip_data : function (data, flip = NULL)  "
"flipped_names : function (flip = FALSE)  "
"fortify : function (model, data, ...)  "
"Geom : Classes 'Geom', 'ggproto', 'gg' <ggproto object: Class Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: "
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function "
"geom_abline : function (mapping = NULL, data = NULL, ..., slope, intercept, na.rm = FALSE, show.legend = NA)  "
"geom_area : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"stack\", na.rm = FALSE, "
"    orientation = NA, show.legend = NA, inherit.aes = TRUE, ..., outline.type = \"upper\")  "
"geom_bar : function (mapping = NULL, data = NULL, stat = \"count\", position = \"stack\", ..., width = NULL, "
"    na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE)  "
"geom_bin_2d : function (mapping = NULL, data = NULL, stat = \"bin2d\", position = \"identity\", ..., "
"    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_bin2d : function (mapping = NULL, data = NULL, stat = \"bin2d\", position = \"identity\", ..., "
"    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_blank : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    show.legend = NA, inherit.aes = TRUE)  "
"geom_boxplot : function (mapping = NULL, data = NULL, stat = \"boxplot\", position = \"dodge2\", ..., "
"    outlier.colour = NULL, outlier.color = NULL, outlier.fill = NULL, outlier.shape = 19, "
"    outlier.size = 1.5, outlier.stroke = 0.5, outlier.alpha = NULL, notch = FALSE, "
"    notchwidth = 0.5, varwidth = FALSE, na.rm = FALSE, orientation = NA, show.legend = NA, "
"    inherit.aes = TRUE)  "
"geom_col : function (mapping = NULL, data = NULL, position = \"stack\", ..., width = NULL, na.rm = FALSE, "
"    show.legend = NA, inherit.aes = TRUE)  "
"geom_contour : function (mapping = NULL, data = NULL, stat = \"contour\", position = \"identity\", ..., "
"    bins = NULL, binwidth = NULL, breaks = NULL, lineend = \"butt\", linejoin = \"round\", "
"    linemitre = 10, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_contour_filled : function (mapping = NULL, data = NULL, stat = \"contour_filled\", position = \"identity\", "
"    ..., bins = NULL, binwidth = NULL, breaks = NULL, na.rm = FALSE, show.legend = NA, "
"    inherit.aes = TRUE)  "
"geom_count : function (mapping = NULL, data = NULL, stat = \"sum\", position = \"identity\", ..., na.rm = FALSE, "
"    show.legend = NA, inherit.aes = TRUE)  "
"geom_crossbar : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    fatten = 2.5, na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE)  "
"geom_curve : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    curvature = 0.5, angle = 90, ncp = 5, arrow = NULL, arrow.fill = NULL, lineend = \"butt\", "
"    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_density : function (mapping = NULL, data = NULL, stat = \"density\", position = \"identity\", ..., "
"    na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE, outline.type = \"upper\")  "
"geom_density_2d : function (mapping = NULL, data = NULL, stat = \"density_2d\", position = \"identity\", "
"    ..., contour_var = \"density\", lineend = \"butt\", linejoin = \"round\", linemitre = 10, "
"    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_density_2d_filled : function (mapping = NULL, data = NULL, stat = \"density_2d_filled\", position = \"identity\", "
"    ..., contour_var = \"density\", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_density2d : function (mapping = NULL, data = NULL, stat = \"density_2d\", position = \"identity\", "
"    ..., contour_var = \"density\", lineend = \"butt\", linejoin = \"round\", linemitre = 10, "
"    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_density2d_filled : function (mapping = NULL, data = NULL, stat = \"density_2d_filled\", position = \"identity\", "
"    ..., contour_var = \"density\", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_dotplot : function (mapping = NULL, data = NULL, position = \"identity\", ..., binwidth = NULL, "
"    binaxis = \"x\", method = \"dotdensity\", binpositions = \"bygroup\", stackdir = \"up\", "
"    stackratio = 1, dotsize = 1, stackgroups = FALSE, origin = NULL, right = TRUE, "
"    width = 0.9, drop = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_errorbar : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE)  "
"geom_errorbarh : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_freqpoly : function (mapping = NULL, data = NULL, stat = \"bin\", position = \"identity\", ..., na.rm = FALSE, "
"    show.legend = NA, inherit.aes = TRUE)  "
"geom_function : function (mapping = NULL, data = NULL, stat = \"function\", position = \"identity\", ..., "
"    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_hex : function (mapping = NULL, data = NULL, stat = \"binhex\", position = \"identity\", ..., "
"    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_histogram : function (mapping = NULL, data = NULL, stat = \"bin\", position = \"stack\", ..., binwidth = NULL, "
"    bins = NULL, na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE)  "
"geom_hline : function (mapping = NULL, data = NULL, ..., yintercept, na.rm = FALSE, show.legend = NA)  "
"geom_jitter : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"jitter\", ..., "
"    width = NULL, height = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_label : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    parse = FALSE, nudge_x = 0, nudge_y = 0, label.padding = unit(0.25, \"lines\"), "
"    label.r = unit(0.15, \"lines\"), label.size = 0.25, na.rm = FALSE, show.legend = NA, "
"    inherit.aes = TRUE)  "
"geom_line : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", na.rm = FALSE, "
"    orientation = NA, show.legend = NA, inherit.aes = TRUE, ...)  "
"geom_linerange : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE)  "
"geom_map : function (mapping = NULL, data = NULL, stat = \"identity\", ..., map, na.rm = FALSE, "
"    show.legend = NA, inherit.aes = TRUE)  "
"geom_path : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    lineend = \"butt\", linejoin = \"round\", linemitre = 10, arrow = NULL, na.rm = FALSE, "
"    show.legend = NA, inherit.aes = TRUE)  "
"geom_point : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_pointrange : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    fatten = 4, na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE)  "
"geom_polygon : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", rule = \"evenodd\", "
"    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_qq : function (mapping = NULL, data = NULL, geom = \"point\", position = \"identity\", ..., "
"    distribution = stats::qnorm, dparams = list(), na.rm = FALSE, show.legend = NA, "
"    inherit.aes = TRUE)  "
"geom_qq_line : function (mapping = NULL, data = NULL, geom = \"path\", position = \"identity\", ..., "
"    distribution = stats::qnorm, dparams = list(), line.p = c(0.25, 0.75), fullrange = FALSE, "
"    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_quantile : function (mapping = NULL, data = NULL, stat = \"quantile\", position = \"identity\", ..., "
"    lineend = \"butt\", linejoin = \"round\", linemitre = 10, na.rm = FALSE, show.legend = NA, "
"    inherit.aes = TRUE)  "
"geom_raster : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    hjust = 0.5, vjust = 0.5, interpolate = FALSE, na.rm = FALSE, show.legend = NA, "
"    inherit.aes = TRUE)  "
"geom_rect : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    linejoin = \"mitre\", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_ribbon : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE, outline.type = \"both\")  "
"geom_rug : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    outside = FALSE, sides = \"bl\", length = unit(0.03, \"npc\"), na.rm = FALSE, show.legend = NA, "
"    inherit.aes = TRUE)  "
"geom_segment : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    arrow = NULL, arrow.fill = NULL, lineend = \"butt\", linejoin = \"round\", na.rm = FALSE, "
"    show.legend = NA, inherit.aes = TRUE)  "
"geom_sf : function (mapping = aes(), data = NULL, stat = \"sf\", position = \"identity\", na.rm = FALSE, "
"    show.legend = NA, inherit.aes = TRUE, ...)  "
"geom_sf_label : function (mapping = aes(), data = NULL, stat = \"sf_coordinates\", position = \"identity\", "
"    ..., parse = FALSE, nudge_x = 0, nudge_y = 0, label.padding = unit(0.25, \"lines\"), "
"    label.r = unit(0.15, \"lines\"), label.size = 0.25, na.rm = FALSE, show.legend = NA, "
"    inherit.aes = TRUE, fun.geometry = NULL)  "
"geom_sf_text : function (mapping = aes(), data = NULL, stat = \"sf_coordinates\", position = \"identity\", "
"    ..., parse = FALSE, nudge_x = 0, nudge_y = 0, check_overlap = FALSE, na.rm = FALSE, "
"    show.legend = NA, inherit.aes = TRUE, fun.geometry = NULL)  "
"geom_smooth : function (mapping = NULL, data = NULL, stat = \"smooth\", position = \"identity\", ..., "
"    method = NULL, formula = NULL, se = TRUE, na.rm = FALSE, orientation = NA, show.legend = NA, "
"    inherit.aes = TRUE)  "
"geom_spoke : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_step : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", direction = \"hv\", "
"    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...)  "
"geom_text : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    parse = FALSE, nudge_x = 0, nudge_y = 0, check_overlap = FALSE, na.rm = FALSE, "
"    show.legend = NA, inherit.aes = TRUE)  "
"geom_tile : function (mapping = NULL, data = NULL, stat = \"identity\", position = \"identity\", ..., "
"    linejoin = \"mitre\", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"geom_violin : function (mapping = NULL, data = NULL, stat = \"ydensity\", position = \"dodge\", ..., "
"    draw_quantiles = NULL, trim = TRUE, scale = \"area\", na.rm = FALSE, orientation = NA, "
"    show.legend = NA, inherit.aes = TRUE)  "
"geom_vline : function (mapping = NULL, data = NULL, ..., xintercept, na.rm = FALSE, show.legend = NA)  "
"GeomAbline : Classes 'GeomAbline', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomAbline, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: slope intercept"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomAnnotationMap : Classes 'GeomAnnotationMap', 'GeomMap', 'GeomPolygon', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomAnnotationMap, GeomMap, GeomPolygon, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: "
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: NULL"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomMap, GeomPolygon, Geom, gg> "
"GeomArea : Classes 'GeomArea', 'GeomRibbon', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomArea, GeomRibbon, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm orientation"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomRibbon, Geom, gg> "
"GeomBar : Classes 'GeomBar', 'GeomRect', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomBar, GeomRect, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm orientation"
"    handle_na: function"
"    non_missing_aes: xmin xmax ymin ymax"
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomRect, Geom, gg> "
"GeomBlank : Classes 'GeomBlank', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomBlank, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: "
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomBoxplot : Classes 'GeomBoxplot', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomBoxplot, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm width orientation"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x|y lower|xlower upper|xupper middle|xmiddle ymin|xmin y ..."
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomCol : Classes 'GeomCol', 'GeomRect', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomCol, GeomRect, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm orientation"
"    handle_na: function"
"    non_missing_aes: xmin xmax ymin ymax"
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomRect, Geom, gg> "
"GeomContour : Classes 'GeomContour', 'GeomPath', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomContour, GeomPath, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomPath, Geom, gg> "
"GeomContourFilled : Classes 'GeomContourFilled', 'GeomPolygon', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomContourFilled, GeomPolygon, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomPolygon, Geom, gg> "
"GeomCrossbar : Classes 'GeomCrossbar', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomCrossbar, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm orientation"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y ymin|xmin ymax|xmax"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomCurve : Classes 'GeomCurve', 'GeomSegment', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomCurve, GeomSegment, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: linetype size shape"
"    optional_aes: "
"    parameters: function"
"    required_aes: x y xend yend"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomSegment, Geom, gg> "
"GeomCustomAnn : Classes 'GeomCustomAnn', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomCustomAnn, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: "
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: "
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomDensity : Classes 'GeomDensity', 'GeomArea', 'GeomRibbon', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomDensity, GeomArea, GeomRibbon, Geom, gg>"
"    aesthetics: function"
"    default_aes: list"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm orientation"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomArea, GeomRibbon, Geom, gg> "
"GeomDensity2d : Classes 'GeomDensity2d', 'GeomPath', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomDensity2d, GeomPath, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomPath, Geom, gg> "
"GeomDensity2dFilled : Classes 'GeomDensity2dFilled', 'GeomPolygon', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomDensity2dFilled, GeomPolygon, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomPolygon, Geom, gg> "
"GeomDotplot : Classes 'GeomDotplot', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomDotplot, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: size shape"
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomErrorbar : Classes 'GeomErrorbar', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomErrorbar, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm orientation"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x|y ymin|xmin ymax|xmax"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomErrorbarh : Classes 'GeomErrorbarh', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomErrorbarh, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: xmin xmax y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomFunction : Classes 'GeomFunction', 'GeomPath', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomFunction, GeomPath, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomPath, Geom, gg> "
"GeomHex : Classes 'GeomHex', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomHex, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomHline : Classes 'GeomHline', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomHline, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: yintercept"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomLabel : Classes 'GeomLabel', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomLabel, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y label"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomLine : Classes 'GeomLine', 'GeomPath', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomLine, GeomPath, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm orientation"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomPath, Geom, gg> "
"GeomLinerange : Classes 'GeomLinerange', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomLinerange, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm orientation"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x|y ymin|xmin ymax|xmax"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomLogticks : Classes 'GeomLogticks', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomLogticks, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: "
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: "
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomMap : Classes 'GeomMap', 'GeomPolygon', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomMap, GeomPolygon, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: map_id"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomPolygon, Geom, gg> "
"GeomPath : Classes 'GeomPath', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomPath, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomPoint : Classes 'GeomPoint', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomPoint, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: size shape colour"
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomPointrange : Classes 'GeomPointrange', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomPointrange, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm orientation"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y ymin|xmin ymax|xmax"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomPolygon : Classes 'GeomPolygon', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomPolygon, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomQuantile : Classes 'GeomQuantile', 'GeomPath', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomQuantile, GeomPath, Geom, gg>"
"    aesthetics: function"
"    default_aes: list"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomPath, Geom, gg> "
"GeomRaster : Classes 'GeomRaster', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomRaster, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: fill xmin xmax ymin ymax"
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomRasterAnn : Classes 'GeomRasterAnn', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomRasterAnn, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: "
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: "
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomRect : Classes 'GeomRect', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomRect, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: xmin xmax ymin ymax"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomRibbon : Classes 'GeomRibbon', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomRibbon, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm orientation"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x|y ymin|xmin ymax|xmax"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomRug : Classes 'GeomRug', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomRug, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: x y"
"    parameters: function"
"    required_aes: "
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomSegment : Classes 'GeomSegment', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomSegment, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: linetype size shape"
"    optional_aes: "
"    parameters: function"
"    required_aes: x y xend yend"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomSf : Classes 'GeomSf', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomSf, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: geometry"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomSmooth : Classes 'GeomSmooth', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomSmooth, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm orientation"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: ymin ymax"
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomSpoke : Classes 'GeomSpoke', 'GeomSegment', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomSpoke, GeomSegment, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: linetype size shape"
"    optional_aes: "
"    parameters: function"
"    required_aes: x y angle radius"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomSegment, Geom, gg> "
"GeomStep : Classes 'GeomStep', 'GeomPath', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomStep, GeomPath, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomPath, Geom, gg> "
"GeomText : Classes 'GeomText', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomText, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y label"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomTile : Classes 'GeomTile', 'GeomRect', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomTile, GeomRect, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class GeomRect, Geom, gg> "
"GeomViolin : Classes 'GeomViolin', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomViolin, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm orientation"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"GeomVline : Classes 'GeomVline', 'Geom', 'ggproto', 'gg' <ggproto object: Class GeomVline, Geom, gg>"
"    aesthetics: function"
"    default_aes: uneval"
"    draw_group: function"
"    draw_key: function"
"    draw_layer: function"
"    draw_panel: function"
"    extra_params: na.rm"
"    handle_na: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: xintercept"
"    setup_data: function"
"    setup_params: function"
"    use_defaults: function"
"    super:  <ggproto object: Class Geom, gg> "
"get_alt_text : function (p, ...)  "
"get_element_tree : function ()  "
"gg_dep : function (version, msg)  "
"ggplot : function (data = NULL, mapping = aes(), ..., environment = parent.frame())  "
"ggplot_add : function (object, plot, object_name)  "
"ggplot_build : function (plot)  "
"ggplot_gtable : function (data)  "
"ggplotGrob : function (x)  "
"ggproto : function (`_class` = NULL, `_inherit` = NULL, ...)  "
"ggproto_parent : function (parent, self)  "
"ggsave : function (filename, plot = last_plot(), device = NULL, path = NULL, scale = 1, width = NA, "
"    height = NA, units = c(\"in\", \"cm\", \"mm\", \"px\"), dpi = 300, limitsize = TRUE, bg = NULL, "
"    ...)  "
"ggtitle : function (label, subtitle = waiver())  "
"guide_axis : function (title = waiver(), check.overlap = FALSE, angle = NULL, n.dodge = 1, order = 0, "
"    position = waiver())  "
"guide_bins : function (title = waiver(), title.position = NULL, title.theme = NULL, title.hjust = NULL, "
"    title.vjust = NULL, label = TRUE, label.position = NULL, label.theme = NULL, label.hjust = NULL, "
"    label.vjust = NULL, keywidth = NULL, keyheight = NULL, axis = TRUE, axis.colour = \"black\", "
"    axis.linewidth = 0.5, axis.arrow = NULL, direction = NULL, default.unit = \"line\", "
"    override.aes = list(), reverse = FALSE, order = 0, show.limits = NULL, ...)  "
"guide_colorbar : function (title = waiver(), title.position = NULL, title.theme = NULL, title.hjust = NULL, "
"    title.vjust = NULL, label = TRUE, label.position = NULL, label.theme = NULL, label.hjust = NULL, "
"    label.vjust = NULL, barwidth = NULL, barheight = NULL, nbin = 300, raster = TRUE, "
"    frame.colour = NULL, frame.linewidth = 0.5, frame.linetype = 1, ticks = TRUE, "
"    ticks.colour = \"white\", ticks.linewidth = 0.5, draw.ulim = TRUE, draw.llim = TRUE, "
"    direction = NULL, default.unit = \"line\", reverse = FALSE, order = 0, available_aes = c(\"colour\", "
"        \"color\", \"fill\"), ...)  "
"guide_colorsteps : function (even.steps = TRUE, show.limits = NULL, ticks = FALSE, ...)  "
"guide_colourbar : function (title = waiver(), title.position = NULL, title.theme = NULL, title.hjust = NULL, "
"    title.vjust = NULL, label = TRUE, label.position = NULL, label.theme = NULL, label.hjust = NULL, "
"    label.vjust = NULL, barwidth = NULL, barheight = NULL, nbin = 300, raster = TRUE, "
"    frame.colour = NULL, frame.linewidth = 0.5, frame.linetype = 1, ticks = TRUE, "
"    ticks.colour = \"white\", ticks.linewidth = 0.5, draw.ulim = TRUE, draw.llim = TRUE, "
"    direction = NULL, default.unit = \"line\", reverse = FALSE, order = 0, available_aes = c(\"colour\", "
"        \"color\", \"fill\"), ...)  "
"guide_coloursteps : function (even.steps = TRUE, show.limits = NULL, ticks = FALSE, ...)  "
"guide_gengrob : function (guide, theme)  "
"guide_geom : function (guide, layers, default_mapping)  "
"guide_legend : function (title = waiver(), title.position = NULL, title.theme = NULL, title.hjust = NULL, "
"    title.vjust = NULL, label = TRUE, label.position = NULL, label.theme = NULL, label.hjust = NULL, "
"    label.vjust = NULL, keywidth = NULL, keyheight = NULL, direction = NULL, default.unit = \"line\", "
"    override.aes = list(), nrow = NULL, ncol = NULL, byrow = FALSE, reverse = FALSE, "
"    order = 0, ...)  "
"guide_merge : function (guide, new_guide)  "
"guide_none : function (title = waiver(), position = waiver())  "
"guide_train : function (guide, scale, aesthetic = NULL)  "
"guide_transform : function (guide, coord, panel_params)  "
"guides : function (...)  "
"has_flipped_aes : function (data, params = list(), main_is_orthogonal = NA, range_is_orthogonal = NA, "
"    group_has_equal = FALSE, ambiguous = FALSE, main_is_continuous = FALSE, main_is_optional = FALSE)  "
"is.Coord : function (x)  "
"is.facet : function (x)  "
"is.ggplot : function (x)  "
"is.ggproto : function (x)  "
"is.theme : function (x)  "
"label_both : function (labels, multi_line = TRUE, sep = \": \")  "
"label_bquote : function (rows = NULL, cols = NULL, default)  "
"label_context : function (labels, multi_line = TRUE, sep = \": \")  "
"label_parsed : function (labels, multi_line = TRUE)  "
"label_value : function (labels, multi_line = TRUE)  "
"label_wrap_gen : function (width = 25, multi_line = TRUE)  "
"labeller : function (..., .rows = NULL, .cols = NULL, keep.as.numeric = NULL, .multi_line = TRUE, "
"    .default = label_value)  "
"labs : function (..., title = waiver(), subtitle = waiver(), caption = waiver(), tag = waiver(), "
"    alt = waiver(), alt_insight = waiver())  "
"last_plot : function ()  "
"layer : function (geom = NULL, stat = NULL, data = NULL, mapping = NULL, position = NULL, "
"    params = list(), inherit.aes = TRUE, check.aes = TRUE, check.param = TRUE, show.legend = NA, "
"    key_glyph = NULL, layer_class = Layer)  "
"layer_data : function (plot, i = 1L)  "
"layer_grob : function (plot, i = 1L)  "
"layer_scales : function (plot, i = 1L, j = 1L)  "
"layer_sf : function (geom = NULL, stat = NULL, data = NULL, mapping = NULL, position = NULL, "
"    params = list(), inherit.aes = TRUE, check.aes = TRUE, check.param = TRUE, show.legend = NA)  "
"Layout : Classes 'Layout', 'ggproto', 'gg' <ggproto object: Class Layout, gg>"
"    coord: NULL"
"    coord_params: list"
"    facet: NULL"
"    facet_params: list"
"    finish_data: function"
"    get_scales: function"
"    layout: NULL"
"    map_position: function"
"    panel_params: NULL"
"    panel_scales_x: NULL"
"    panel_scales_y: NULL"
"    render: function"
"    render_labels: function"
"    reset_scales: function"
"    setup: function"
"    setup_panel_guides: function"
"    setup_panel_params: function"
"    train_position: function"
"    xlabel: function"
"    ylabel: function "
"lims : function (...)  "
"luv_colours : 'data.frame':	657 obs. of  4 variables:"
" $ L  : num  9342 9101 8810 8935 8452 ..."
" $ u  : num  -3.37e-12 -4.75e+02 1.01e+03 1.07e+03 1.01e+03 ..."
" $ v  : num  0 -635 1668 1675 1610 ..."
" $ col: chr  \"white\" \"aliceblue\" \"antiquewhite\" \"antiquewhite1\" ..."
"map_data : function (map, region = \".\", exact = FALSE, ...)  "
"margin : function (t = 0, r = 0, b = 0, l = 0, unit = \"pt\")  "
"max_height : function (grobs, value_only = FALSE)  "
"max_width : function (grobs, value_only = FALSE)  "
"mean_cl_boot : function (x, ...)  "
"mean_cl_normal : function (x, ...)  "
"mean_sdl : function (x, ...)  "
"mean_se : function (x, mult = 1)  "
"median_hilow : function (x, ...)  "
"merge_element : function (new, old)  "
"midwest : tibble [437 × 28] (S3: tbl_df/tbl/data.frame)"
"mpg : tibble [234 × 11] (S3: tbl_df/tbl/data.frame)"
"msleep : tibble [83 × 11] (S3: tbl_df/tbl/data.frame)"
"panel_cols : function (table)  "
"panel_rows : function (table)  "
"Position : Classes 'Position', 'ggproto', 'gg' <ggproto object: Class Position, gg>"
"    compute_layer: function"
"    compute_panel: function"
"    required_aes: "
"    setup_data: function"
"    setup_params: function "
"position_dodge : function (width = NULL, preserve = c(\"total\", \"single\"))  "
"position_dodge2 : function (width = NULL, preserve = c(\"total\", \"single\"), padding = 0.1, reverse = FALSE)  "
"position_fill : function (vjust = 1, reverse = FALSE)  "
"position_identity : function ()  "
"position_jitter : function (width = NULL, height = NULL, seed = NA)  "
"position_jitterdodge : function (jitter.width = NULL, jitter.height = 0, dodge.width = 0.75, seed = NA)  "
"position_nudge : function (x = 0, y = 0)  "
"position_stack : function (vjust = 1, reverse = FALSE)  "
"PositionDodge : Classes 'PositionDodge', 'Position', 'ggproto', 'gg' <ggproto object: Class PositionDodge, Position, gg>"
"    compute_layer: function"
"    compute_panel: function"
"    preserve: total"
"    required_aes: "
"    setup_data: function"
"    setup_params: function"
"    width: NULL"
"    super:  <ggproto object: Class Position, gg> "
"PositionDodge2 : Classes 'PositionDodge2', 'PositionDodge', 'Position', 'ggproto', 'gg' <ggproto object: Class PositionDodge2, PositionDodge, Position, gg>"
"    compute_layer: function"
"    compute_panel: function"
"    padding: 0.1"
"    preserve: total"
"    required_aes: "
"    reverse: FALSE"
"    setup_data: function"
"    setup_params: function"
"    width: NULL"
"    super:  <ggproto object: Class PositionDodge, Position, gg> "
"PositionFill : Classes 'PositionFill', 'PositionStack', 'Position', 'ggproto', 'gg' <ggproto object: Class PositionFill, PositionStack, Position, gg>"
"    compute_layer: function"
"    compute_panel: function"
"    fill: TRUE"
"    required_aes: "
"    reverse: FALSE"
"    setup_data: function"
"    setup_params: function"
"    type: NULL"
"    vjust: 1"
"    super:  <ggproto object: Class PositionStack, Position, gg> "
"PositionIdentity : Classes 'PositionIdentity', 'Position', 'ggproto', 'gg' <ggproto object: Class PositionIdentity, Position, gg>"
"    compute_layer: function"
"    compute_panel: function"
"    required_aes: "
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Position, gg> "
"PositionJitter : Classes 'PositionJitter', 'Position', 'ggproto', 'gg' <ggproto object: Class PositionJitter, Position, gg>"
"    compute_layer: function"
"    compute_panel: function"
"    required_aes: x y"
"    seed: NA"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Position, gg> "
"PositionJitterdodge : Classes 'PositionJitterdodge', 'Position', 'ggproto', 'gg' <ggproto object: Class PositionJitterdodge, Position, gg>"
"    compute_layer: function"
"    compute_panel: function"
"    dodge.width: NULL"
"    jitter.height: NULL"
"    jitter.width: NULL"
"    required_aes: x y"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Position, gg> "
"PositionNudge : Classes 'PositionNudge', 'Position', 'ggproto', 'gg' <ggproto object: Class PositionNudge, Position, gg>"
"    compute_layer: function"
"    compute_panel: function"
"    required_aes: "
"    setup_data: function"
"    setup_params: function"
"    x: 0"
"    y: 0"
"    super:  <ggproto object: Class Position, gg> "
"PositionStack : Classes 'PositionStack', 'Position', 'ggproto', 'gg' <ggproto object: Class PositionStack, Position, gg>"
"    compute_layer: function"
"    compute_panel: function"
"    fill: FALSE"
"    required_aes: "
"    reverse: FALSE"
"    setup_data: function"
"    setup_params: function"
"    type: NULL"
"    vjust: 1"
"    super:  <ggproto object: Class Position, gg> "
"presidential : tibble [11 × 4] (S3: tbl_df/tbl/data.frame)"
"qplot : function (x, y, ..., data, facets = NULL, margins = FALSE, geom = \"auto\", xlim = c(NA, "
"    NA), ylim = c(NA, NA), log = \"\", main = NULL, xlab = NULL, ylab = NULL, asp = NA, "
"    stat = NULL, position = NULL)  "
"quickplot : function (x, y, ..., data, facets = NULL, margins = FALSE, geom = \"auto\", xlim = c(NA, "
"    NA), ylim = c(NA, NA), log = \"\", main = NULL, xlab = NULL, ylab = NULL, asp = NA, "
"    stat = NULL, position = NULL)  "
"quo : function (expr)  "
"quo_name : function (quo)  "
"quos : function (..., .named = FALSE, .ignore_empty = c(\"trailing\", \"none\", \"all\"), .unquote_names = TRUE)  "
"register_theme_elements : function (..., element_tree = NULL, complete = TRUE)  "
"rel : function (x)  "
"remove_missing : function (df, na.rm = FALSE, vars = names(df), name = \"\", finite = FALSE)  "
"render_axes : function (x = NULL, y = NULL, coord, theme, transpose = FALSE)  "
"render_strips : function (x = NULL, y = NULL, labeller, theme)  "
"reset_theme_settings : function (reset_current = TRUE)  "
"resolution : function (x, zero = TRUE)  "
"Scale : Classes 'Scale', 'ggproto', 'gg' <ggproto object: Class Scale, gg>"
"    aesthetics: uneval"
"    axis_order: function"
"    break_info: function"
"    break_positions: function"
"    breaks: waiver"
"    call: NULL"
"    clone: function"
"    dimension: function"
"    expand: waiver"
"    get_breaks: function"
"    get_breaks_minor: function"
"    get_labels: function"
"    get_limits: function"
"    guide: legend"
"    is_discrete: function"
"    is_empty: function"
"    labels: waiver"
"    limits: NULL"
"    make_sec_title: function"
"    make_title: function"
"    map: function"
"    map_df: function"
"    na.value: NA"
"    name: waiver"
"    palette: function"
"    position: left"
"    range: <ggproto object: Class Range, gg>"
"        range: NULL"
"        reset: function"
"        super:  <ggproto object: Class Range, gg>"
"    rescale: function"
"    reset: function"
"    scale_name: NULL"
"    train: function"
"    train_df: function"
"    transform: function"
"    transform_df: function "
"scale_alpha : function (..., range = c(0.1, 1))  "
"scale_alpha_binned : function (..., range = c(0.1, 1))  "
"scale_alpha_continuous : function (..., range = c(0.1, 1))  "
"scale_alpha_date : function (..., range = c(0.1, 1))  "
"scale_alpha_datetime : function (..., range = c(0.1, 1))  "
"scale_alpha_discrete : function (...)  "
"scale_alpha_identity : function (..., guide = \"none\")  "
"scale_alpha_manual : function (..., values, breaks = waiver(), na.value = NA)  "
"scale_alpha_ordinal : function (..., range = c(0.1, 1))  "
"scale_color_binned : function (..., type = getOption(\"ggplot2.binned.colour\"))  "
"scale_color_brewer : function (..., type = \"seq\", palette = 1, direction = 1, aesthetics = \"colour\")  "
"scale_color_continuous : function (..., type = getOption(\"ggplot2.continuous.colour\"))  "
"scale_color_date : function (..., low = \"#132B43\", high = \"#56B1F7\", space = \"Lab\", na.value = \"grey50\", "
"    guide = \"colourbar\")  "
"scale_color_datetime : function (..., low = \"#132B43\", high = \"#56B1F7\", space = \"Lab\", na.value = \"grey50\", "
"    guide = \"colourbar\")  "
"scale_color_discrete : function (..., type = getOption(\"ggplot2.discrete.colour\"))  "
"scale_color_distiller : function (..., type = \"seq\", palette = 1, direction = -1, values = NULL, space = \"Lab\", "
"    na.value = \"grey50\", guide = \"colourbar\", aesthetics = \"colour\")  "
"scale_color_fermenter : function (..., type = \"seq\", palette = 1, direction = -1, na.value = \"grey50\", guide = \"coloursteps\", "
"    aesthetics = \"colour\")  "
"scale_color_gradient : function (..., low = \"#132B43\", high = \"#56B1F7\", space = \"Lab\", na.value = \"grey50\", "
"    guide = \"colourbar\", aesthetics = \"colour\")  "
"scale_color_gradient2 : function (..., low = muted(\"red\"), mid = \"white\", high = muted(\"blue\"), midpoint = 0, "
"    space = \"Lab\", na.value = \"grey50\", guide = \"colourbar\", aesthetics = \"colour\")  "
"scale_color_gradientn : function (..., colours, values = NULL, space = \"Lab\", na.value = \"grey50\", guide = \"colourbar\", "
"    aesthetics = \"colour\", colors)  "
"scale_color_grey : function (..., start = 0.2, end = 0.8, na.value = \"red\", aesthetics = \"colour\")  "
"scale_color_hue : function (..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1, na.value = \"grey50\", "
"    aesthetics = \"colour\")  "
"scale_color_identity : function (..., guide = \"none\", aesthetics = \"colour\")  "
"scale_color_manual : function (..., values, aesthetics = \"colour\", breaks = waiver(), na.value = \"grey50\")  "
"scale_color_ordinal : function (..., type = getOption(\"ggplot2.ordinal.colour\", getOption(\"ggplot2.ordinal.fill\")))  "
"scale_color_steps : function (..., low = \"#132B43\", high = \"#56B1F7\", space = \"Lab\", na.value = \"grey50\", "
"    guide = \"coloursteps\", aesthetics = \"colour\")  "
"scale_color_steps2 : function (..., low = muted(\"red\"), mid = \"white\", high = muted(\"blue\"), midpoint = 0, "
"    space = \"Lab\", na.value = \"grey50\", guide = \"coloursteps\", aesthetics = \"colour\")  "
"scale_color_stepsn : function (..., colours, values = NULL, space = \"Lab\", na.value = \"grey50\", guide = \"coloursteps\", "
"    aesthetics = \"colour\", colors)  "
"scale_color_viridis_b : function (..., alpha = 1, begin = 0, end = 1, direction = 1, option = \"D\", values = NULL, "
"    space = \"Lab\", na.value = \"grey50\", guide = \"coloursteps\", aesthetics = \"colour\")  "
"scale_color_viridis_c : function (..., alpha = 1, begin = 0, end = 1, direction = 1, option = \"D\", values = NULL, "
"    space = \"Lab\", na.value = \"grey50\", guide = \"colourbar\", aesthetics = \"colour\")  "
"scale_color_viridis_d : function (..., alpha = 1, begin = 0, end = 1, direction = 1, option = \"D\", aesthetics = \"colour\")  "
"scale_colour_binned : function (..., type = getOption(\"ggplot2.binned.colour\"))  "
"scale_colour_brewer : function (..., type = \"seq\", palette = 1, direction = 1, aesthetics = \"colour\")  "
"scale_colour_continuous : function (..., type = getOption(\"ggplot2.continuous.colour\"))  "
"scale_colour_date : function (..., low = \"#132B43\", high = \"#56B1F7\", space = \"Lab\", na.value = \"grey50\", "
"    guide = \"colourbar\")  "
"scale_colour_datetime : function (..., low = \"#132B43\", high = \"#56B1F7\", space = \"Lab\", na.value = \"grey50\", "
"    guide = \"colourbar\")  "
"scale_colour_discrete : function (..., type = getOption(\"ggplot2.discrete.colour\"))  "
"scale_colour_distiller : function (..., type = \"seq\", palette = 1, direction = -1, values = NULL, space = \"Lab\", "
"    na.value = \"grey50\", guide = \"colourbar\", aesthetics = \"colour\")  "
"scale_colour_fermenter : function (..., type = \"seq\", palette = 1, direction = -1, na.value = \"grey50\", guide = \"coloursteps\", "
"    aesthetics = \"colour\")  "
"scale_colour_gradient : function (..., low = \"#132B43\", high = \"#56B1F7\", space = \"Lab\", na.value = \"grey50\", "
"    guide = \"colourbar\", aesthetics = \"colour\")  "
"scale_colour_gradient2 : function (..., low = muted(\"red\"), mid = \"white\", high = muted(\"blue\"), midpoint = 0, "
"    space = \"Lab\", na.value = \"grey50\", guide = \"colourbar\", aesthetics = \"colour\")  "
"scale_colour_gradientn : function (..., colours, values = NULL, space = \"Lab\", na.value = \"grey50\", guide = \"colourbar\", "
"    aesthetics = \"colour\", colors)  "
"scale_colour_grey : function (..., start = 0.2, end = 0.8, na.value = \"red\", aesthetics = \"colour\")  "
"scale_colour_hue : function (..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1, na.value = \"grey50\", "
"    aesthetics = \"colour\")  "
"scale_colour_identity : function (..., guide = \"none\", aesthetics = \"colour\")  "
"scale_colour_manual : function (..., values, aesthetics = \"colour\", breaks = waiver(), na.value = \"grey50\")  "
"scale_colour_ordinal : function (..., type = getOption(\"ggplot2.ordinal.colour\", getOption(\"ggplot2.ordinal.fill\")))  "
"scale_colour_steps : function (..., low = \"#132B43\", high = \"#56B1F7\", space = \"Lab\", na.value = \"grey50\", "
"    guide = \"coloursteps\", aesthetics = \"colour\")  "
"scale_colour_steps2 : function (..., low = muted(\"red\"), mid = \"white\", high = muted(\"blue\"), midpoint = 0, "
"    space = \"Lab\", na.value = \"grey50\", guide = \"coloursteps\", aesthetics = \"colour\")  "
"scale_colour_stepsn : function (..., colours, values = NULL, space = \"Lab\", na.value = \"grey50\", guide = \"coloursteps\", "
"    aesthetics = \"colour\", colors)  "
"scale_colour_viridis_b : function (..., alpha = 1, begin = 0, end = 1, direction = 1, option = \"D\", values = NULL, "
"    space = \"Lab\", na.value = \"grey50\", guide = \"coloursteps\", aesthetics = \"colour\")  "
"scale_colour_viridis_c : function (..., alpha = 1, begin = 0, end = 1, direction = 1, option = \"D\", values = NULL, "
"    space = \"Lab\", na.value = \"grey50\", guide = \"colourbar\", aesthetics = \"colour\")  "
"scale_colour_viridis_d : function (..., alpha = 1, begin = 0, end = 1, direction = 1, option = \"D\", aesthetics = \"colour\")  "
"scale_continuous_identity : function (aesthetics, ..., guide = \"none\")  "
"scale_discrete_identity : function (aesthetics, ..., guide = \"none\")  "
"scale_discrete_manual : function (aesthetics, ..., values, breaks = waiver())  "
"scale_fill_binned : function (..., type = getOption(\"ggplot2.binned.fill\"))  "
"scale_fill_brewer : function (..., type = \"seq\", palette = 1, direction = 1, aesthetics = \"fill\")  "
"scale_fill_continuous : function (..., type = getOption(\"ggplot2.continuous.fill\"))  "
"scale_fill_date : function (..., low = \"#132B43\", high = \"#56B1F7\", space = \"Lab\", na.value = \"grey50\", "
"    guide = \"colourbar\")  "
"scale_fill_datetime : function (..., low = \"#132B43\", high = \"#56B1F7\", space = \"Lab\", na.value = \"grey50\", "
"    guide = \"colourbar\")  "
"scale_fill_discrete : function (..., type = getOption(\"ggplot2.discrete.fill\"))  "
"scale_fill_distiller : function (..., type = \"seq\", palette = 1, direction = -1, values = NULL, space = \"Lab\", "
"    na.value = \"grey50\", guide = \"colourbar\", aesthetics = \"fill\")  "
"scale_fill_fermenter : function (..., type = \"seq\", palette = 1, direction = -1, na.value = \"grey50\", guide = \"coloursteps\", "
"    aesthetics = \"fill\")  "
"scale_fill_gradient : function (..., low = \"#132B43\", high = \"#56B1F7\", space = \"Lab\", na.value = \"grey50\", "
"    guide = \"colourbar\", aesthetics = \"fill\")  "
"scale_fill_gradient2 : function (..., low = muted(\"red\"), mid = \"white\", high = muted(\"blue\"), midpoint = 0, "
"    space = \"Lab\", na.value = \"grey50\", guide = \"colourbar\", aesthetics = \"fill\")  "
"scale_fill_gradientn : function (..., colours, values = NULL, space = \"Lab\", na.value = \"grey50\", guide = \"colourbar\", "
"    aesthetics = \"fill\", colors)  "
"scale_fill_grey : function (..., start = 0.2, end = 0.8, na.value = \"red\", aesthetics = \"fill\")  "
"scale_fill_hue : function (..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1, na.value = \"grey50\", "
"    aesthetics = \"fill\")  "
"scale_fill_identity : function (..., guide = \"none\", aesthetics = \"fill\")  "
"scale_fill_manual : function (..., values, aesthetics = \"fill\", breaks = waiver(), na.value = \"grey50\")  "
"scale_fill_ordinal : function (..., type = getOption(\"ggplot2.ordinal.fill\", getOption(\"ggplot2.ordinal.colour\")))  "
"scale_fill_steps : function (..., low = \"#132B43\", high = \"#56B1F7\", space = \"Lab\", na.value = \"grey50\", "
"    guide = \"coloursteps\", aesthetics = \"fill\")  "
"scale_fill_steps2 : function (..., low = muted(\"red\"), mid = \"white\", high = muted(\"blue\"), midpoint = 0, "
"    space = \"Lab\", na.value = \"grey50\", guide = \"coloursteps\", aesthetics = \"fill\")  "
"scale_fill_stepsn : function (..., colours, values = NULL, space = \"Lab\", na.value = \"grey50\", guide = \"coloursteps\", "
"    aesthetics = \"fill\", colors)  "
"scale_fill_viridis_b : function (..., alpha = 1, begin = 0, end = 1, direction = 1, option = \"D\", values = NULL, "
"    space = \"Lab\", na.value = \"grey50\", guide = \"coloursteps\", aesthetics = \"fill\")  "
"scale_fill_viridis_c : function (..., alpha = 1, begin = 0, end = 1, direction = 1, option = \"D\", values = NULL, "
"    space = \"Lab\", na.value = \"grey50\", guide = \"colourbar\", aesthetics = \"fill\")  "
"scale_fill_viridis_d : function (..., alpha = 1, begin = 0, end = 1, direction = 1, option = \"D\", aesthetics = \"fill\")  "
"scale_linetype : function (..., na.value = \"blank\")  "
"scale_linetype_binned : function (..., na.value = \"blank\")  "
"scale_linetype_continuous : function (...)  "
"scale_linetype_discrete : function (..., na.value = \"blank\")  "
"scale_linetype_identity : function (..., guide = \"none\")  "
"scale_linetype_manual : function (..., values, breaks = waiver(), na.value = \"blank\")  "
"scale_radius : function (name = waiver(), breaks = waiver(), labels = waiver(), limits = NULL, range = c(1, "
"    6), trans = \"identity\", guide = \"legend\")  "
"scale_shape : function (..., solid = TRUE)  "
"scale_shape_binned : function (..., solid = TRUE)  "
"scale_shape_continuous : function (...)  "
"scale_shape_discrete : function (..., solid = TRUE)  "
"scale_shape_identity : function (..., guide = \"none\")  "
"scale_shape_manual : function (..., values, breaks = waiver(), na.value = NA)  "
"scale_shape_ordinal : function (...)  "
"scale_size : function (name = waiver(), breaks = waiver(), labels = waiver(), limits = NULL, range = c(1, "
"    6), trans = \"identity\", guide = \"legend\")  "
"scale_size_area : function (..., max_size = 6)  "
"scale_size_binned : function (name = waiver(), breaks = waiver(), labels = waiver(), limits = NULL, range = c(1, "
"    6), n.breaks = NULL, nice.breaks = TRUE, trans = \"identity\", guide = \"bins\")  "
"scale_size_binned_area : function (..., max_size = 6)  "
"scale_size_continuous : function (name = waiver(), breaks = waiver(), labels = waiver(), limits = NULL, range = c(1, "
"    6), trans = \"identity\", guide = \"legend\")  "
"scale_size_date : function (..., range = c(1, 6))  "
"scale_size_datetime : function (..., range = c(1, 6))  "
"scale_size_discrete : function (...)  "
"scale_size_identity : function (..., guide = \"none\")  "
"scale_size_manual : function (..., values, breaks = waiver(), na.value = NA)  "
"scale_size_ordinal : function (..., range = c(2, 6))  "
"scale_type : function (x)  "
"scale_x_binned : function (name = waiver(), n.breaks = 10, nice.breaks = TRUE, breaks = waiver(), labels = waiver(), "
"    limits = NULL, expand = waiver(), oob = squish, na.value = NA_real_, right = TRUE, "
"    show.limits = FALSE, trans = \"identity\", guide = waiver(), position = \"bottom\")  "
"scale_x_continuous : function (name = waiver(), breaks = waiver(), minor_breaks = waiver(), n.breaks = NULL, "
"    labels = waiver(), limits = NULL, expand = waiver(), oob = censor, na.value = NA_real_, "
"    trans = \"identity\", guide = waiver(), position = \"bottom\", sec.axis = waiver())  "
"scale_x_date : function (name = waiver(), breaks = waiver(), date_breaks = waiver(), labels = waiver(), "
"    date_labels = waiver(), minor_breaks = waiver(), date_minor_breaks = waiver(), "
"    limits = NULL, expand = waiver(), oob = censor, guide = waiver(), position = \"bottom\", "
"    sec.axis = waiver())  "
"scale_x_datetime : function (name = waiver(), breaks = waiver(), date_breaks = waiver(), labels = waiver(), "
"    date_labels = waiver(), minor_breaks = waiver(), date_minor_breaks = waiver(), "
"    timezone = NULL, limits = NULL, expand = waiver(), oob = censor, guide = waiver(), "
"    position = \"bottom\", sec.axis = waiver())  "
"scale_x_discrete : function (..., expand = waiver(), guide = waiver(), position = \"bottom\")  "
"scale_x_log10 : function (...)  "
"scale_x_reverse : function (...)  "
"scale_x_sqrt : function (...)  "
"scale_x_time : function (name = waiver(), breaks = waiver(), minor_breaks = waiver(), labels = waiver(), "
"    limits = NULL, expand = waiver(), oob = censor, na.value = NA_real_, guide = waiver(), "
"    position = \"bottom\", sec.axis = waiver())  "
"scale_y_binned : function (name = waiver(), n.breaks = 10, nice.breaks = TRUE, breaks = waiver(), labels = waiver(), "
"    limits = NULL, expand = waiver(), oob = squish, na.value = NA_real_, right = TRUE, "
"    show.limits = FALSE, trans = \"identity\", guide = waiver(), position = \"left\")  "
"scale_y_continuous : function (name = waiver(), breaks = waiver(), minor_breaks = waiver(), n.breaks = NULL, "
"    labels = waiver(), limits = NULL, expand = waiver(), oob = censor, na.value = NA_real_, "
"    trans = \"identity\", guide = waiver(), position = \"left\", sec.axis = waiver())  "
"scale_y_date : function (name = waiver(), breaks = waiver(), date_breaks = waiver(), labels = waiver(), "
"    date_labels = waiver(), minor_breaks = waiver(), date_minor_breaks = waiver(), "
"    limits = NULL, expand = waiver(), oob = censor, guide = waiver(), position = \"left\", "
"    sec.axis = waiver())  "
"scale_y_datetime : function (name = waiver(), breaks = waiver(), date_breaks = waiver(), labels = waiver(), "
"    date_labels = waiver(), minor_breaks = waiver(), date_minor_breaks = waiver(), "
"    timezone = NULL, limits = NULL, expand = waiver(), oob = censor, guide = waiver(), "
"    position = \"left\", sec.axis = waiver())  "
"scale_y_discrete : function (..., expand = waiver(), guide = waiver(), position = \"left\")  "
"scale_y_log10 : function (...)  "
"scale_y_reverse : function (...)  "
"scale_y_sqrt : function (...)  "
"scale_y_time : function (name = waiver(), breaks = waiver(), minor_breaks = waiver(), labels = waiver(), "
"    limits = NULL, expand = waiver(), oob = censor, na.value = NA_real_, guide = waiver(), "
"    position = \"left\", sec.axis = waiver())  "
"ScaleBinned : Classes 'ScaleBinned', 'Scale', 'ggproto', 'gg' <ggproto object: Class ScaleBinned, Scale, gg>"
"    aesthetics: uneval"
"    after.stat: FALSE"
"    axis_order: function"
"    break_info: function"
"    break_positions: function"
"    breaks: waiver"
"    call: NULL"
"    clone: function"
"    dimension: function"
"    expand: waiver"
"    get_breaks: function"
"    get_breaks_minor: function"
"    get_labels: function"
"    get_limits: function"
"    guide: legend"
"    is_discrete: function"
"    is_empty: function"
"    labels: waiver"
"    limits: NULL"
"    make_sec_title: function"
"    make_title: function"
"    map: function"
"    map_df: function"
"    n.breaks: NULL"
"    na.value: NA"
"    name: waiver"
"    nice.breaks: TRUE"
"    oob: function"
"    palette: function"
"    position: left"
"    range: <ggproto object: Class RangeContinuous, Range, gg>"
"        range: NULL"
"        reset: function"
"        train: function"
"        super:  <ggproto object: Class RangeContinuous, Range, gg>"
"    rescale: function"
"    rescaler: function"
"    reset: function"
"    right: TRUE"
"    scale_name: NULL"
"    show.limits: FALSE"
"    train: function"
"    train_df: function"
"    transform: function"
"    transform_df: function"
"    super:  <ggproto object: Class Scale, gg> "
"ScaleBinnedPosition : Classes 'ScaleBinnedPosition', 'ScaleBinned', 'Scale', 'ggproto', 'gg' <ggproto object: Class ScaleBinnedPosition, ScaleBinned, Scale, gg>"
"    aesthetics: uneval"
"    after.stat: FALSE"
"    axis_order: function"
"    break_info: function"
"    break_positions: function"
"    breaks: waiver"
"    call: NULL"
"    clone: function"
"    dimension: function"
"    expand: waiver"
"    get_breaks: function"
"    get_breaks_minor: function"
"    get_labels: function"
"    get_limits: function"
"    guide: legend"
"    is_discrete: function"
"    is_empty: function"
"    labels: waiver"
"    limits: NULL"
"    make_sec_title: function"
"    make_title: function"
"    map: function"
"    map_df: function"
"    n.breaks: NULL"
"    na.value: NA"
"    name: waiver"
"    nice.breaks: TRUE"
"    oob: function"
"    palette: function"
"    position: left"
"    range: <ggproto object: Class RangeContinuous, Range, gg>"
"        range: NULL"
"        reset: function"
"        train: function"
"        super:  <ggproto object: Class RangeContinuous, Range, gg>"
"    rescale: function"
"    rescaler: function"
"    reset: function"
"    right: TRUE"
"    scale_name: NULL"
"    show.limits: FALSE"
"    train: function"
"    train_df: function"
"    transform: function"
"    transform_df: function"
"    super:  <ggproto object: Class ScaleBinned, Scale, gg> "
"ScaleContinuous : Classes 'ScaleContinuous', 'Scale', 'ggproto', 'gg' <ggproto object: Class ScaleContinuous, Scale, gg>"
"    aesthetics: uneval"
"    axis_order: function"
"    break_info: function"
"    break_positions: function"
"    breaks: waiver"
"    call: NULL"
"    clone: function"
"    dimension: function"
"    expand: waiver"
"    get_breaks: function"
"    get_breaks_minor: function"
"    get_labels: function"
"    get_limits: function"
"    guide: legend"
"    is_discrete: function"
"    is_empty: function"
"    labels: waiver"
"    limits: NULL"
"    make_sec_title: function"
"    make_title: function"
"    map: function"
"    map_df: function"
"    minor_breaks: waiver"
"    n.breaks: NULL"
"    na.value: NA"
"    name: waiver"
"    oob: function"
"    palette: function"
"    position: left"
"    print: function"
"    range: <ggproto object: Class RangeContinuous, Range, gg>"
"        range: NULL"
"        reset: function"
"        train: function"
"        super:  <ggproto object: Class RangeContinuous, Range, gg>"
"    rescale: function"
"    rescaler: function"
"    reset: function"
"    scale_name: NULL"
"    train: function"
"    train_df: function"
"    trans: trans"
"    transform: function"
"    transform_df: function"
"    super:  <ggproto object: Class Scale, gg> "
"ScaleContinuousDate : Classes 'ScaleContinuousDate', 'ScaleContinuous', 'Scale', 'ggproto', 'gg' <ggproto object: Class ScaleContinuousDate, ScaleContinuous, Scale, gg>"
"    aesthetics: uneval"
"    axis_order: function"
"    break_info: function"
"    break_positions: function"
"    breaks: waiver"
"    call: NULL"
"    clone: function"
"    dimension: function"
"    expand: waiver"
"    get_breaks: function"
"    get_breaks_minor: function"
"    get_labels: function"
"    get_limits: function"
"    guide: legend"
"    is_discrete: function"
"    is_empty: function"
"    labels: waiver"
"    limits: NULL"
"    make_sec_title: function"
"    make_title: function"
"    map: function"
"    map_df: function"
"    minor_breaks: waiver"
"    n.breaks: NULL"
"    na.value: NA"
"    name: waiver"
"    oob: function"
"    palette: function"
"    position: left"
"    print: function"
"    range: <ggproto object: Class RangeContinuous, Range, gg>"
"        range: NULL"
"        reset: function"
"        train: function"
"        super:  <ggproto object: Class RangeContinuous, Range, gg>"
"    rescale: function"
"    rescaler: function"
"    reset: function"
"    scale_name: NULL"
"    sec_name: function"
"    secondary.axis: waiver"
"    train: function"
"    train_df: function"
"    trans: trans"
"    transform: function"
"    transform_df: function"
"    super:  <ggproto object: Class ScaleContinuous, Scale, gg> "
"ScaleContinuousDatetime : Classes 'ScaleContinuousDatetime', 'ScaleContinuous', 'Scale', 'ggproto', 'gg' <ggproto object: Class ScaleContinuousDatetime, ScaleContinuous, Scale, gg>"
"    aesthetics: uneval"
"    axis_order: function"
"    break_info: function"
"    break_positions: function"
"    breaks: waiver"
"    call: NULL"
"    clone: function"
"    dimension: function"
"    expand: waiver"
"    get_breaks: function"
"    get_breaks_minor: function"
"    get_labels: function"
"    get_limits: function"
"    guide: legend"
"    is_discrete: function"
"    is_empty: function"
"    labels: waiver"
"    limits: NULL"
"    make_sec_title: function"
"    make_title: function"
"    map: function"
"    map_df: function"
"    minor_breaks: waiver"
"    n.breaks: NULL"
"    na.value: NA"
"    name: waiver"
"    oob: function"
"    palette: function"
"    position: left"
"    print: function"
"    range: <ggproto object: Class RangeContinuous, Range, gg>"
"        range: NULL"
"        reset: function"
"        train: function"
"        super:  <ggproto object: Class RangeContinuous, Range, gg>"
"    rescale: function"
"    rescaler: function"
"    reset: function"
"    scale_name: NULL"
"    sec_name: function"
"    secondary.axis: waiver"
"    timezone: NULL"
"    train: function"
"    train_df: function"
"    trans: trans"
"    transform: function"
"    transform_df: function"
"    super:  <ggproto object: Class ScaleContinuous, Scale, gg> "
"ScaleContinuousIdentity : Classes 'ScaleContinuousIdentity', 'ScaleContinuous', 'Scale', 'ggproto', 'gg' <ggproto object: Class ScaleContinuousIdentity, ScaleContinuous, Scale, gg>"
"    aesthetics: uneval"
"    axis_order: function"
"    break_info: function"
"    break_positions: function"
"    breaks: waiver"
"    call: NULL"
"    clone: function"
"    dimension: function"
"    expand: waiver"
"    get_breaks: function"
"    get_breaks_minor: function"
"    get_labels: function"
"    get_limits: function"
"    guide: legend"
"    is_discrete: function"
"    is_empty: function"
"    labels: waiver"
"    limits: NULL"
"    make_sec_title: function"
"    make_title: function"
"    map: function"
"    map_df: function"
"    minor_breaks: waiver"
"    n.breaks: NULL"
"    na.value: NA"
"    name: waiver"
"    oob: function"
"    palette: function"
"    position: left"
"    print: function"
"    range: <ggproto object: Class RangeContinuous, Range, gg>"
"        range: NULL"
"        reset: function"
"        train: function"
"        super:  <ggproto object: Class RangeContinuous, Range, gg>"
"    rescale: function"
"    rescaler: function"
"    reset: function"
"    scale_name: NULL"
"    train: function"
"    train_df: function"
"    trans: trans"
"    transform: function"
"    transform_df: function"
"    super:  <ggproto object: Class ScaleContinuous, Scale, gg> "
"ScaleContinuousPosition : Classes 'ScaleContinuousPosition', 'ScaleContinuous', 'Scale', 'ggproto', 'gg' <ggproto object: Class ScaleContinuousPosition, ScaleContinuous, Scale, gg>"
"    aesthetics: uneval"
"    axis_order: function"
"    break_info: function"
"    break_positions: function"
"    breaks: waiver"
"    call: NULL"
"    clone: function"
"    dimension: function"
"    expand: waiver"
"    get_breaks: function"
"    get_breaks_minor: function"
"    get_labels: function"
"    get_limits: function"
"    guide: legend"
"    is_discrete: function"
"    is_empty: function"
"    labels: waiver"
"    limits: NULL"
"    make_sec_title: function"
"    make_title: function"
"    map: function"
"    map_df: function"
"    minor_breaks: waiver"
"    n.breaks: NULL"
"    na.value: NA"
"    name: waiver"
"    oob: function"
"    palette: function"
"    position: left"
"    print: function"
"    range: <ggproto object: Class RangeContinuous, Range, gg>"
"        range: NULL"
"        reset: function"
"        train: function"
"        super:  <ggproto object: Class RangeContinuous, Range, gg>"
"    rescale: function"
"    rescaler: function"
"    reset: function"
"    scale_name: NULL"
"    sec_name: function"
"    secondary.axis: waiver"
"    train: function"
"    train_df: function"
"    trans: trans"
"    transform: function"
"    transform_df: function"
"    super:  <ggproto object: Class ScaleContinuous, Scale, gg> "
"ScaleDiscrete : Classes 'ScaleDiscrete', 'Scale', 'ggproto', 'gg' <ggproto object: Class ScaleDiscrete, Scale, gg>"
"    aesthetics: uneval"
"    axis_order: function"
"    break_info: function"
"    break_positions: function"
"    breaks: waiver"
"    call: NULL"
"    clone: function"
"    dimension: function"
"    drop: TRUE"
"    expand: waiver"
"    get_breaks: function"
"    get_breaks_minor: function"
"    get_labels: function"
"    get_limits: function"
"    guide: legend"
"    is_discrete: function"
"    is_empty: function"
"    labels: waiver"
"    limits: NULL"
"    make_sec_title: function"
"    make_title: function"
"    map: function"
"    map_df: function"
"    n.breaks.cache: NULL"
"    na.value: NA"
"    name: waiver"
"    palette: function"
"    palette.cache: NULL"
"    position: left"
"    range: <ggproto object: Class Range, gg>"
"        range: NULL"
"        reset: function"
"        super:  <ggproto object: Class Range, gg>"
"    rescale: function"
"    reset: function"
"    scale_name: NULL"
"    train: function"
"    train_df: function"
"    transform: function"
"    transform_df: function"
"    super:  <ggproto object: Class Scale, gg> "
"ScaleDiscreteIdentity : Classes 'ScaleDiscreteIdentity', 'ScaleDiscrete', 'Scale', 'ggproto', 'gg' <ggproto object: Class ScaleDiscreteIdentity, ScaleDiscrete, Scale, gg>"
"    aesthetics: uneval"
"    axis_order: function"
"    break_info: function"
"    break_positions: function"
"    breaks: waiver"
"    call: NULL"
"    clone: function"
"    dimension: function"
"    drop: TRUE"
"    expand: waiver"
"    get_breaks: function"
"    get_breaks_minor: function"
"    get_labels: function"
"    get_limits: function"
"    guide: legend"
"    is_discrete: function"
"    is_empty: function"
"    labels: waiver"
"    limits: NULL"
"    make_sec_title: function"
"    make_title: function"
"    map: function"
"    map_df: function"
"    n.breaks.cache: NULL"
"    na.value: NA"
"    name: waiver"
"    palette: function"
"    palette.cache: NULL"
"    position: left"
"    range: <ggproto object: Class Range, gg>"
"        range: NULL"
"        reset: function"
"        super:  <ggproto object: Class Range, gg>"
"    rescale: function"
"    reset: function"
"    scale_name: NULL"
"    train: function"
"    train_df: function"
"    transform: function"
"    transform_df: function"
"    super:  <ggproto object: Class ScaleDiscrete, Scale, gg> "
"ScaleDiscretePosition : Classes 'ScaleDiscretePosition', 'ScaleDiscrete', 'Scale', 'ggproto', 'gg' <ggproto object: Class ScaleDiscretePosition, ScaleDiscrete, Scale, gg>"
"    aesthetics: uneval"
"    axis_order: function"
"    break_info: function"
"    break_positions: function"
"    breaks: waiver"
"    call: NULL"
"    clone: function"
"    dimension: function"
"    drop: TRUE"
"    expand: waiver"
"    get_breaks: function"
"    get_breaks_minor: function"
"    get_labels: function"
"    get_limits: function"
"    guide: legend"
"    is_discrete: function"
"    is_empty: function"
"    labels: waiver"
"    limits: NULL"
"    make_sec_title: function"
"    make_title: function"
"    map: function"
"    map_df: function"
"    n.breaks.cache: NULL"
"    na.value: NA"
"    name: waiver"
"    palette: function"
"    palette.cache: NULL"
"    position: left"
"    range: <ggproto object: Class Range, gg>"
"        range: NULL"
"        reset: function"
"        super:  <ggproto object: Class Range, gg>"
"    rescale: function"
"    reset: function"
"    scale_name: NULL"
"    train: function"
"    train_df: function"
"    transform: function"
"    transform_df: function"
"    super:  <ggproto object: Class ScaleDiscrete, Scale, gg> "
"seals : tibble [1,155 × 4] (S3: tbl_df/tbl/data.frame)"
"sec_axis : function (trans = NULL, name = waiver(), breaks = waiver(), labels = waiver(), guide = waiver())  "
"set_last_plot : function (value)  "
"sf_transform_xy : function (data, target_crs, source_crs, authority_compliant = FALSE)  "
"should_stop : function (expr)  "
"stage : function (start = NULL, after_stat = NULL, after_scale = NULL)  "
"standardise_aes_names : function (x)  "
"stat : function (x)  "
"Stat : Classes 'Stat', 'ggproto', 'gg' <ggproto object: Class Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: "
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function "
"stat_bin : function (mapping = NULL, data = NULL, geom = \"bar\", position = \"stack\", ..., binwidth = NULL, "
"    bins = NULL, center = NULL, boundary = NULL, breaks = NULL, closed = c(\"right\", "
"        \"left\"), pad = FALSE, na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE)  "
"stat_bin_2d : function (mapping = NULL, data = NULL, geom = \"tile\", position = \"identity\", ..., "
"    bins = 30, binwidth = NULL, drop = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"stat_bin_hex : function (mapping = NULL, data = NULL, geom = \"hex\", position = \"identity\", ..., bins = 30, "
"    binwidth = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"stat_bin2d : function (mapping = NULL, data = NULL, geom = \"tile\", position = \"identity\", ..., "
"    bins = 30, binwidth = NULL, drop = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"stat_binhex : function (mapping = NULL, data = NULL, geom = \"hex\", position = \"identity\", ..., bins = 30, "
"    binwidth = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"stat_boxplot : function (mapping = NULL, data = NULL, geom = \"boxplot\", position = \"dodge2\", ..., "
"    coef = 1.5, na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE)  "
"stat_contour : function (mapping = NULL, data = NULL, geom = \"contour\", position = \"identity\", ..., "
"    bins = NULL, binwidth = NULL, breaks = NULL, na.rm = FALSE, show.legend = NA, "
"    inherit.aes = TRUE)  "
"stat_contour_filled : function (mapping = NULL, data = NULL, geom = \"contour_filled\", position = \"identity\", "
"    ..., bins = NULL, binwidth = NULL, breaks = NULL, na.rm = FALSE, show.legend = NA, "
"    inherit.aes = TRUE)  "
"stat_count : function (mapping = NULL, data = NULL, geom = \"bar\", position = \"stack\", ..., width = NULL, "
"    na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE)  "
"stat_density : function (mapping = NULL, data = NULL, geom = \"area\", position = \"stack\", ..., bw = \"nrd0\", "
"    adjust = 1, kernel = \"gaussian\", n = 512, trim = FALSE, na.rm = FALSE, orientation = NA, "
"    show.legend = NA, inherit.aes = TRUE)  "
"stat_density_2d : function (mapping = NULL, data = NULL, geom = \"density_2d\", position = \"identity\", "
"    ..., contour = TRUE, contour_var = \"density\", n = 100, h = NULL, adjust = c(1, "
"        1), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"stat_density_2d_filled : function (mapping = NULL, data = NULL, geom = \"density_2d_filled\", position = \"identity\", "
"    ..., contour = TRUE, contour_var = \"density\", n = 100, h = NULL, adjust = c(1, "
"        1), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"stat_density2d : function (mapping = NULL, data = NULL, geom = \"density_2d\", position = \"identity\", "
"    ..., contour = TRUE, contour_var = \"density\", n = 100, h = NULL, adjust = c(1, "
"        1), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"stat_density2d_filled : function (mapping = NULL, data = NULL, geom = \"density_2d_filled\", position = \"identity\", "
"    ..., contour = TRUE, contour_var = \"density\", n = 100, h = NULL, adjust = c(1, "
"        1), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"stat_ecdf : function (mapping = NULL, data = NULL, geom = \"step\", position = \"identity\", ..., "
"    n = NULL, pad = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"stat_ellipse : function (mapping = NULL, data = NULL, geom = \"path\", position = \"identity\", ..., "
"    type = \"t\", level = 0.95, segments = 51, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"stat_function : function (mapping = NULL, data = NULL, geom = \"function\", position = \"identity\", ..., "
"    fun, xlim = NULL, n = 101, args = list(), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"stat_identity : function (mapping = NULL, data = NULL, geom = \"point\", position = \"identity\", ..., "
"    show.legend = NA, inherit.aes = TRUE)  "
"stat_qq : function (mapping = NULL, data = NULL, geom = \"point\", position = \"identity\", ..., "
"    distribution = stats::qnorm, dparams = list(), na.rm = FALSE, show.legend = NA, "
"    inherit.aes = TRUE)  "
"stat_qq_line : function (mapping = NULL, data = NULL, geom = \"path\", position = \"identity\", ..., "
"    distribution = stats::qnorm, dparams = list(), line.p = c(0.25, 0.75), fullrange = FALSE, "
"    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"stat_quantile : function (mapping = NULL, data = NULL, geom = \"quantile\", position = \"identity\", ..., "
"    quantiles = c(0.25, 0.5, 0.75), formula = NULL, method = \"rq\", method.args = list(), "
"    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"stat_sf : function (mapping = NULL, data = NULL, geom = \"rect\", position = \"identity\", na.rm = FALSE, "
"    show.legend = NA, inherit.aes = TRUE, ...)  "
"stat_sf_coordinates : function (mapping = aes(), data = NULL, geom = \"point\", position = \"identity\", na.rm = FALSE, "
"    show.legend = NA, inherit.aes = TRUE, fun.geometry = NULL, ...)  "
"stat_smooth : function (mapping = NULL, data = NULL, geom = \"smooth\", position = \"identity\", ..., "
"    method = NULL, formula = NULL, se = TRUE, n = 80, span = 0.75, fullrange = FALSE, "
"    level = 0.95, method.args = list(), na.rm = FALSE, orientation = NA, show.legend = NA, "
"    inherit.aes = TRUE)  "
"stat_spoke : function (...)  "
"stat_sum : function (mapping = NULL, data = NULL, geom = \"point\", position = \"identity\", ..., "
"    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"stat_summary : function (mapping = NULL, data = NULL, geom = \"pointrange\", position = \"identity\", "
"    ..., fun.data = NULL, fun = NULL, fun.max = NULL, fun.min = NULL, fun.args = list(), "
"    na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE, fun.y, "
"    fun.ymin, fun.ymax)  "
"stat_summary_2d : function (mapping = NULL, data = NULL, geom = \"tile\", position = \"identity\", ..., "
"    bins = 30, binwidth = NULL, drop = TRUE, fun = \"mean\", fun.args = list(), na.rm = FALSE, "
"    show.legend = NA, inherit.aes = TRUE)  "
"stat_summary_bin : function (mapping = NULL, data = NULL, geom = \"pointrange\", position = \"identity\", "
"    ..., fun.data = NULL, fun = NULL, fun.max = NULL, fun.min = NULL, fun.args = list(), "
"    bins = 30, binwidth = NULL, breaks = NULL, na.rm = FALSE, orientation = NA, show.legend = NA, "
"    inherit.aes = TRUE, fun.y, fun.ymin, fun.ymax)  "
"stat_summary_hex : function (mapping = NULL, data = NULL, geom = \"hex\", position = \"identity\", ..., bins = 30, "
"    binwidth = NULL, drop = TRUE, fun = \"mean\", fun.args = list(), na.rm = FALSE, "
"    show.legend = NA, inherit.aes = TRUE)  "
"stat_summary2d : function (...)  "
"stat_unique : function (mapping = NULL, data = NULL, geom = \"point\", position = \"identity\", ..., "
"    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)  "
"stat_ydensity : function (mapping = NULL, data = NULL, geom = \"violin\", position = \"dodge\", ..., bw = \"nrd0\", "
"    adjust = 1, kernel = \"gaussian\", trim = TRUE, scale = \"area\", na.rm = FALSE, orientation = NA, "
"    show.legend = NA, inherit.aes = TRUE)  "
"StatBin : Classes 'StatBin', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatBin, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm orientation"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x|y"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatBin2d : Classes 'StatBin2d', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatBin2d, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatBindot : Classes 'StatBindot', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatBindot, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: weight"
"    optional_aes: "
"    parameters: function"
"    required_aes: x"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatBinhex : Classes 'StatBinhex', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatBinhex, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatBoxplot : Classes 'StatBoxplot', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatBoxplot, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm orientation"
"    finish_layer: function"
"    non_missing_aes: weight"
"    optional_aes: "
"    parameters: function"
"    required_aes: y|x"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatContour : Classes 'StatContour', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatContour, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y z"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatContourFilled : Classes 'StatContourFilled', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatContourFilled, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y z"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatCount : Classes 'StatCount', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatCount, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm orientation"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x|y"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatDensity : Classes 'StatDensity', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatDensity, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm orientation"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x|y"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatDensity2d : Classes 'StatDensity2d', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatDensity2d, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    contour_type: lines"
"    default_aes: uneval"
"    extra_params: na.rm contour contour_var bins binwidth breaks"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatDensity2dFilled : Classes 'StatDensity2dFilled', 'StatDensity2d', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatDensity2dFilled, StatDensity2d, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    contour_type: bands"
"    default_aes: uneval"
"    extra_params: na.rm contour contour_var bins binwidth breaks"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class StatDensity2d, Stat, gg> "
"StatEcdf : Classes 'StatEcdf', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatEcdf, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x|y"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatEllipse : Classes 'StatEllipse', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatEllipse, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatFunction : Classes 'StatFunction', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatFunction, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: "
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatIdentity : Classes 'StatIdentity', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatIdentity, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: "
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatQq : Classes 'StatQq', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatQq, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: sample"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatQqLine : Classes 'StatQqLine', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatQqLine, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: sample"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatQuantile : Classes 'StatQuantile', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatQuantile, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatSf : Classes 'StatSf', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatSf, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: geometry"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatSfCoordinates : Classes 'StatSfCoordinates', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatSfCoordinates, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: geometry"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatSmooth : Classes 'StatSmooth', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatSmooth, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm orientation"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatSum : Classes 'StatSum', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatSum, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatSummary : Classes 'StatSummary', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatSummary, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm orientation"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatSummary2d : Classes 'StatSummary2d', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatSummary2d, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y z"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatSummaryBin : Classes 'StatSummaryBin', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatSummaryBin, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm orientation"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatSummaryHex : Classes 'StatSummaryHex', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatSummaryHex, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: x y z"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatUnique : Classes 'StatUnique', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatUnique, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm"
"    finish_layer: function"
"    non_missing_aes: "
"    optional_aes: "
"    parameters: function"
"    required_aes: "
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"StatYdensity : Classes 'StatYdensity', 'Stat', 'ggproto', 'gg' <ggproto object: Class StatYdensity, Stat, gg>"
"    aesthetics: function"
"    compute_group: function"
"    compute_layer: function"
"    compute_panel: function"
"    default_aes: uneval"
"    extra_params: na.rm orientation"
"    finish_layer: function"
"    non_missing_aes: weight"
"    optional_aes: "
"    parameters: function"
"    required_aes: x y"
"    retransform: TRUE"
"    setup_data: function"
"    setup_params: function"
"    super:  <ggproto object: Class Stat, gg> "
"summarise_coord : function (p)  "
"summarise_layers : function (p)  "
"summarise_layout : function (p)  "
"sym : function (x)  "
"syms : function (x)  "
"theme : function (line, rect, text, title, aspect.ratio, axis.title, axis.title.x, axis.title.x.top, "
"    axis.title.x.bottom, axis.title.y, axis.title.y.left, axis.title.y.right, axis.text, "
"    axis.text.x, axis.text.x.top, axis.text.x.bottom, axis.text.y, axis.text.y.left, "
"    axis.text.y.right, axis.ticks, axis.ticks.x, axis.ticks.x.top, axis.ticks.x.bottom, "
"    axis.ticks.y, axis.ticks.y.left, axis.ticks.y.right, axis.ticks.length, axis.ticks.length.x, "
"    axis.ticks.length.x.top, axis.ticks.length.x.bottom, axis.ticks.length.y, axis.ticks.length.y.left, "
"    axis.ticks.length.y.right, axis.line, axis.line.x, axis.line.x.top, axis.line.x.bottom, "
"    axis.line.y, axis.line.y.left, axis.line.y.right, legend.background, legend.margin, "
"    legend.spacing, legend.spacing.x, legend.spacing.y, legend.key, legend.key.size, "
"    legend.key.height, legend.key.width, legend.text, legend.text.align, legend.title, "
"    legend.title.align, legend.position, legend.direction, legend.justification, legend.box, "
"    legend.box.just, legend.box.margin, legend.box.background, legend.box.spacing, "
"    panel.background, panel.border, panel.spacing, panel.spacing.x, panel.spacing.y, "
"    panel.grid, panel.grid.major, panel.grid.minor, panel.grid.major.x, panel.grid.major.y, "
"    panel.grid.minor.x, panel.grid.minor.y, panel.ontop, plot.background, plot.title, "
"    plot.title.position, plot.subtitle, plot.caption, plot.caption.position, plot.tag,  "
"theme_bw : function (base_size = 11, base_family = \"\", base_line_size = base_size/22, base_rect_size = base_size/22)  "
"theme_classic : function (base_size = 11, base_family = \"\", base_line_size = base_size/22, base_rect_size = base_size/22)  "
"theme_dark : function (base_size = 11, base_family = \"\", base_line_size = base_size/22, base_rect_size = base_size/22)  "
"theme_get : function ()  "
"theme_gray : function (base_size = 11, base_family = \"\", base_line_size = base_size/22, base_rect_size = base_size/22)  "
"theme_grey : function (base_size = 11, base_family = \"\", base_line_size = base_size/22, base_rect_size = base_size/22)  "
"theme_light : function (base_size = 11, base_family = \"\", base_line_size = base_size/22, base_rect_size = base_size/22)  "
"theme_linedraw : function (base_size = 11, base_family = \"\", base_line_size = base_size/22, base_rect_size = base_size/22)  "
"theme_minimal : function (base_size = 11, base_family = \"\", base_line_size = base_size/22, base_rect_size = base_size/22)  "
"theme_replace : function (...)  "
"theme_set : function (new)  "
"theme_test : function (base_size = 11, base_family = \"\", base_line_size = base_size/22, base_rect_size = base_size/22)  "
"theme_update : function (...)  "
"theme_void : function (base_size = 11, base_family = \"\", base_line_size = base_size/22, base_rect_size = base_size/22)  "
"transform_position : function (df, trans_x = NULL, trans_y = NULL, ...)  "
"txhousing : tibble [8,602 × 9] (S3: tbl_df/tbl/data.frame)"
"unit : function (x, units, data = NULL)  "
"update_geom_defaults : function (geom, new)  "
"update_labels : function (p, labels)  "
"update_stat_defaults : function (stat, new)  "
"vars : function (...)  "
"waiver : function ()  "
"wrap_dims : function (n, nrow = NULL, ncol = NULL)  "
"xlab : function (label)  "
"xlim : function (...)  "
"ylab : function (label)  "
"ylim : function (...)  "
"zeroGrob : function ()  "

library(gmp)
%*%, ..as.bigz, .__C__bigq, .__C__bigz, .__T__asNumeric:gmp, .__T__which.max:base, .__T__which.min:base, .as.bigz, .as.char.bigz, .sub.bigq, add.bigq, add.bigz, apply, apply.bigq, apply.bigz, apply.default, as.bigq, as.bigz, as.bigz.bigq, as.vector.bigq, as.vector.bigz, asNumeric, BernoulliQ, biginteger_as, biginteger_as_character, c_bigq, c_bigz, chooseZ, crossprod, dbinomQ, denominator, denominator<-, div.bigq, div.bigz, divq.bigz, Eulerian, Eulerian.all, factorialZ, factorize, fibnum, fibnum2, formatN, frexpZ, gcd, gcd.bigz, gcd.default, gcdex, gmpVersion, inv.bigz, is.bigq, is.bigz, is.matrixZQ, is.whole, isprime, lcm.bigz, lcm.default, log.bigz, log10.bigz, log2.bigz, lucnum, lucnum2, matrix, matrix.bigq, matrix.bigz, matrix.default, max.bigq, max.bigz, min.bigq, min.bigz, mod.bigz, modulus, modulus<-, mul.bigq, mul.bigz, NA_bigq_, NA_bigz_, ncol.bigq, ncol.bigz, nextprime, nrow.bigq, nrow.bigz, numerator, numerator<-, outer, pow.bigq, pow.bigz, powm, prod.bigq, prod.bigz, rep.bigq, rep.bigz,
round0, roundQ, sizeinbase, solve.bigq, solve.bigz, Stirling1, Stirling1.all, Stirling2, Stirling2.all, sub.bigq, sub.bigz, sum.bigq, sum.bigz, tcrossprod, urand.bigz, which.max, which.min

library(graphics)
.filled.contour, abline, arrows, assocplot, axis, Axis, axis.Date, axis.POSIXct, axTicks, barplot, barplot.default, box, boxplot, boxplot.default, boxplot.matrix, bxp, cdplot, clip, close.screen, co.intervals, contour, contour.default, coplot, curve, dotchart, erase.screen, filled.contour, fourfoldplot, frame, grconvertX, grconvertY, grid, hist, hist.default, identify, image, image.default, layout, layout.show, lcm, legend, lines, lines.default, locator, matlines, matplot, matpoints, mosaicplot, mtext, pairs, pairs.default, panel.smooth, par, persp, pie, plot, plot.default, plot.design, plot.function, plot.new, plot.window, plot.xy, points, points.default, polygon, polypath, rasterImage, rect, rug, screen, segments, smoothScatter, spineplot, split.screen, stars, stem, strheight, stripchart, strwidth, sunflowerplot, symbols, text, text.default, title, xinch, xspline, xyinch, yinch

library(grDevices)
.axisPars, .clipPath, .defineGroup, .devUp, .linearGradientPattern, .mask, .opIndex, .radialGradientPattern, .ruleIndex, .setClipPath, .setMask, .setPattern, .tilingPattern, .useGroup, adjustcolor, as.graphicsAnnot, as.raster, axisTicks, bitmap, blues9, bmp, boxplot.stats, bringToTop, cairo_pdf, cairo_ps, cairoSymbolFont, check.options, chull, CIDFont, cm, cm.colors, col2rgb, colorConverter, colorRamp, colorRampPalette, colors, colorspaces, colours, contourLines, convertColor, densCols, dev.capabilities, dev.capture, dev.control, dev.copy, dev.copy2eps, dev.copy2pdf, dev.cur, dev.flush, dev.hold, dev.interactive, dev.list, dev.new, dev.next, dev.off, dev.prev, dev.print, dev.set, dev.size, dev2bitmap, devAskNewPage, deviceIsInteractive, embedFonts, extendrange, getGraphicsEvent, getGraphicsEventEnv, graphics.off, gray, gray.colors, grey, grey.colors, grSoftVersion, hcl, hcl.colors, hcl.pals, heat.colors, Hershey, hsv, is.raster, jpeg, make.rgb, msgWindow, n2mfrow, nclass.FD, nclass.scott, nclass.Sturges,
palette, palette.colors, palette.pals, pdf, pdf.options, pdfFonts, pictex, png, postscript, postscriptFonts, ps.options, rainbow, recordGraphics, recordPlot, replayPlot, rgb, rgb2hsv, savePlot, setEPS, setGraphicsEventEnv, setGraphicsEventHandlers, setPS, svg, terrain.colors, tiff, topo.colors, trans3d, Type1Font, win.graph, win.metafile, win.print, windows, windows.options, windowsFont, windowsFonts, x11, X11, xfig, xy.coords, xyTable, xyz.coords

library(htmltools)
a, as.tags, attachDependencies, br, browsable, capturePlot, code, copyDependencyToDir, css, defaultPngDevice, div, doRenderTags, em, extractPreserveChunks, findDependencies, h1, h2, h3, h4, h5, h6, hr, HTML, html_print, htmlDependencies, htmlDependencies<-, htmlDependency, htmlEscape, htmlPreserve, htmlTemplate, img, includeCSS, includeHTML, includeMarkdown, includeScript, includeText, is.browsable, is.singleton, knit_print.html, knit_print.shiny.tag, knit_print.shiny.tag.list, makeDependencyRelative, p, parseCssColors, plotTag, pre, renderDependencies, renderDocument, renderTags, resolveDependencies, restorePreserveChunks, save_html, singleton, span, strong, subtractDependencies, suppressDependencies, surroundSingletons, tag, tagAddRenderHook, tagAppendAttributes, tagAppendChild, tagAppendChildren, tagFunction, tagGetAttribute, tagHasAttribute, tagInsertChildren, tagList, tagQuery, tags, tagSetChildren, takeSingletons, urlEncodePath, validateCssUnit, withTags

library(htmlwidgets)
appendContent, createWidget, getDependency, JS, JSEvals, onRender, onStaticRenderComplete, prependContent, saveWidget, scaffoldWidget, setWidgetIdSeed, shinyRenderWidget, shinyWidgetOutput, sizingPolicy

library(janitor)
%>%, add_totals_col, add_totals_row, adorn_crosstab, adorn_ns, adorn_pct_formatting, adorn_percentages, adorn_rounding, adorn_title, adorn_totals, as_tabyl, chisq.test, clean_names, compare_df_cols, compare_df_cols_same, convert_to_date, convert_to_datetime, convert_to_NA, crosstab, describe_class, excel_numeric_to_date, fisher.test, get_dupes, make_clean_names, remove_constant, remove_empty, remove_empty_cols, remove_empty_rows, round_half_up, round_to_fraction, row_to_names, signif_half_up, tabyl, top_levels, untabyl, use_first_valid_of

library(jsonlite)
.__T__$:base : <environment: 0x000001dbed9449c8> 
  .__T__$<-:base : <environment: 0x000001dbed948dd8> 
  .__T__[:base : <environment: 0x000001dbed94d208> 
           .__T__[[<-:base : <environment: 0x000001dbed953200> 
                     .__T__[<-:base : <environment: 0x000001dbed958de8> 
                              .Depends :  chr "methods"
                            base64_dec : function (input)  
                              base64_enc : function (input)  
                                flatten : function (x, recursive = TRUE)  
                                  fromJSON : function (txt, simplifyVector = TRUE, simplifyDataFrame = simplifyVector, simplifyMatrix = simplifyVector, 
                                                       flatten = FALSE, ...)  
                                    minify : function (txt)  
                                      parse_json : function (json, simplifyVector = FALSE, ...)  
                                        prettify : function (txt, indent = 4)  
                                          rbind_pages : function (pages)  
                                            read_json : function (path, simplifyVector = FALSE, ...)  
                                              serializeJSON : function (x, digits = 8, pretty = FALSE)  
                                                stream_in : function (con, handler = NULL, pagesize = 500, verbose = TRUE, ...)  
                                                  stream_out : function (x, con = stdout(), pagesize = 500, verbose = TRUE, prefix = "", ...)  
                                                    toJSON : function (x, dataframe = c("rows", "columns", "values"), matrix = c("rowmajor", "columnmajor"), 
                                                                       Date = c("ISO8601", "epoch"), POSIXt = c("string", "ISO8601", "epoch", "mongo"), 
                                                                       factor = c("string", "integer"), complex = c("string", "list"), raw = c("base64", 
                                                                                                                                               "hex", "mongo", "int", "js"), null = c("list", "null"), na = c("null", "string"), 
                                                                       auto_unbox = FALSE, digits = 4, pretty = FALSE, force = FALSE, ...)  
                                                      unbox : function (x)  
                                                        unserializeJSON : function (txt)  
                                                          validate : function (txt)  
                                                            write_json : function (x, path, ...)  
                                                              >                                                                         
library(knitr)
all_labels : function (...)  
  all_patterns : List of 8
$ rnw     :List of 7
$ brew    :List of 1
$ tex     :List of 8
$ html    :List of 5
$ md      :List of 4
$ rst     :List of 5
$ asciidoc:List of 6
$ textile :List of 5
all_rcpp_labels : function (...)  
  asis_output : function (x, meta = NULL, cacheable = NA)  
    cache_engines : List of 6
$ get    :function (name, default = FALSE, drop = TRUE)  
  $ set    :function (...)  
    $ delete :function (keys)  
      $ append :function (...)  
        $ merge  :function (values)  
          $ restore:function (target = value)  
            clean_cache : function (clean = FALSE, path = opts_chunk$get("cache.path"))  
              combine_words : function (words, sep = ", ", and = " and ", before = "", after = before, oxford_comma = TRUE)  
                current_input : function (dir = FALSE)  
                  dep_auto : function (path = opts_chunk$get("cache.path"))  
                    dep_prev : function ()  
                      engine_output : function (options, code, out, extra = NULL)  
                        extract_raw_output : function (text, markers = raw_markers)  
                          fig_chunk : function (label, ext = "", number, fig.path = opts_chunk$get("fig.path"))  
                            fig_path : function (suffix = "", options = opts_current$get(), number)  
                              hook_ffmpeg_html : function (x, options)  
                                hook_gifski : function (x, options)  
                                  hook_mogrify : function (before, options, envir)  
                                    hook_movecode : function (x)  
                                      hook_optipng : function (before, options, envir)  
                                        hook_pdfcrop : function (before, options, envir)  
                                          hook_plot_asciidoc : function (x, options)  
                                            hook_plot_custom : function (before, options, envir)  
                                              hook_plot_html : function (x, options)  
                                                hook_plot_md : function (x, options)  
                                                  hook_plot_rst : function (x, options)  
                                                    hook_plot_tex : function (x, options)  
                                                      hook_plot_textile : function (x, options)  
                                                        hook_pngquant : function (before, options, envir)  
                                                          hook_purl : function (before, options, envir)  
                                                            hook_r2swf : function (x, options)  
                                                              hook_scianimator : function (x, options)  
                                                                hooks_asciidoc : function ()  
                                                                  hooks_html : function ()  
                                                                    hooks_jekyll : function (highlight = c("pygments", "prettify", "none"), extra = "")  
                                                                      hooks_latex : function ()  
                                                                        hooks_listings : function (envirs = c("Sinput", "Soutput", "Schunk"))  
                                                                          hooks_markdown : function (strict = FALSE, fence_char = "`")  
                                                                            hooks_rst : function (strict = FALSE)  
                                                                              hooks_sweave : function (envirs = c("Sinput", "Soutput", "Schunk"))  
                                                                                hooks_textile : function ()  
                                                                                  image_uri : function (f)  
                                                                                    imgur_upload : function (file, key = "9f3460e67f308f6")  
                                                                                      include_app : function (url, height = "400px")  
                                                                                        include_graphics : function (path, auto_pdf = getOption("knitr.graphics.auto_pdf", FALSE), dpi = NULL, 
                                                                                                                     rel_path = getOption("knitr.graphics.rel_path", TRUE), error = getOption("knitr.graphics.error", 
                                                                                                                                                                                              TRUE))  
                                                                                          include_url : function (url, height = "400px")  
                                                                                            inline_expr : function (code, syntax)  
                                                                                              is_html_output : function (fmt = pandoc_to(), excludes = NULL)  
                                                                                                is_latex_output : function ()  
                                                                                                  is_low_change : function (p1, p2)  
                                                                                                    kable : function (x, format, digits = getOption("digits"), row.names = NA, col.names = NA, 
                                                                                                                      align, caption = NULL, label = NULL, format.args = list(), escape = TRUE, ...)  
                                                                                                      kables : function (x, format, caption = NULL, label = NULL)  
                                                                                                        knit : function (input, output = NULL, tangle = FALSE, text = NULL, quiet = FALSE, envir = parent.frame(), 
                                                                                                                         encoding = "UTF-8")  
                                                                                                          knit_child : function (..., options = NULL, envir = knit_global())  
                                                                                                            knit_code : List of 6
$ get    :function (name, default = FALSE, drop = TRUE)  
  $ set    :function (...)  
    $ delete :function (keys)  
      $ append :function (...)  
        $ merge  :function (values)  
          $ restore:function (target = value)  
            knit_engines : List of 6
$ get    :function (name, default = FALSE, drop = TRUE)  
  $ set    :function (...)  
    $ delete :function (keys)  
      $ append :function (...)  
        $ merge  :function (values)  
          $ restore:function (target = value)  
            knit_exit : function (append, fully = TRUE)  
              knit_expand : function (file, ..., text = read_utf8(file), delim = c("{{", "}}"))  
                knit_filter : function (ifile, encoding = "UTF-8")  
                  knit_global : function ()  
                    knit_hooks : List of 6
$ get    :function (name, default = FALSE, drop = TRUE)  
  $ set    :function (...)  
    $ delete :function (keys)  
      $ append :function (...)  
        $ merge  :function (values)  
          $ restore:function (target = value)  
            knit_meta : function (class = NULL, clean = TRUE)  
              knit_meta_add : function (meta, label = "")  
                knit_params : function (text, evaluate = TRUE)  
                  knit_params_yaml : function (yaml, evaluate = TRUE)  
                    knit_patterns : List of 6
$ get    :function (name, default = FALSE, drop = TRUE)  
  $ set    :function (...)  
    $ delete :function (keys)  
      $ append :function (...)  
        $ merge  :function (values)  
          $ restore:function (target = value)  
            knit_print : function (x, ...)  
              knit_rd : function (pkg, links = tools::findHTMLlinks(), frame = TRUE)  
                knit_rd_all : function ()  
                  knit_theme : List of 2
$ set:function (theme)  
  $ get:function (theme = NULL)  
    knit_watch : function (input, compile = knit, interval = 1, ...)  
      knit2html : function (input, output = NULL, ..., envir = parent.frame(), text = NULL, quiet = FALSE, 
                            encoding = "UTF-8", force_v1 = getOption("knitr.knit2html.force_v1", FALSE))  
        knit2pandoc : function (input, output = NULL, tangle = FALSE, text = NULL, quiet = FALSE, envir = parent.frame(), 
                                to = "html", pandoc_wrapper = NULL, ..., encoding = "UTF-8")  
          knit2pdf : function (input, output = NULL, compiler = NULL, envir = parent.frame(), quiet = FALSE, 
                               ...)  
            knit2wp : function (input, title = "A post from knitr", ..., envir = parent.frame(), shortcode = FALSE, 
                                action = c("newPost", "editPost", "newPage"), postid, publish = TRUE)  
              load_cache : function (label, object, notfound = "NOT AVAILABLE", path = opts_chunk$get("cache.path"), 
                                     dir = opts_knit$get("output.dir"), envir = NULL, lazy = TRUE)  
                normal_print : function (x, ...)  
                  opts_chunk : List of 6
$ get    :function (name, default = FALSE, drop = TRUE)  
  $ set    :function (...)  
    $ delete :function (keys)  
      $ append :function (...)  
        $ merge  :function (values)  
          $ restore:function (target = value)  
            opts_current : List of 6
$ get    :function (name, default = FALSE, drop = TRUE)  
  $ set    :function (...)  
    $ delete :function (keys)  
      $ append :function (...)  
        $ merge  :function (values)  
          $ restore:function (target = value)  
            opts_hooks : List of 6
$ get    :function (name, default = FALSE, drop = TRUE)  
  $ set    :function (...)  
    $ delete :function (keys)  
      $ append :function (...)  
        $ merge  :function (values)  
          $ restore:function (target = value)  
            opts_knit : List of 6
$ get    :function (name, default = FALSE, drop = TRUE)  
  $ set    :function (...)  
    $ delete :function (keys)  
      $ append :function (...)  
        $ merge  :function (values)  
          $ restore:function (target = value)  
            opts_template : List of 6
$ get    :function (name, default = FALSE, drop = TRUE)  
  $ set    :function (...)  
    $ delete :function (keys)  
      $ append :function (...)  
        $ merge  :function (values)  
          $ restore:function (target = value)  
            pandoc : function (input, format, config = getOption("config.pandoc"), ext = NA)  
              pandoc_from : function ()  
                pandoc_to : function (fmt)  
                  partition_chunk : function (engine, code)  
                    pat_asciidoc : function ()  
                      pat_brew : function ()  
                        pat_html : function ()  
                          pat_md : function ()  
                            pat_rnw : function ()  
                              pat_rst : function ()  
                                pat_tex : function ()  
                                  pat_textile : function ()  
                                    plot_crop : function (x, quiet = TRUE)  
                                      purl : function (..., documentation = 1L)  
                                        rand_seed :  language {  .GlobalEnv$.Random.seed }
raw_block : function (x, type = "latex", ...)  
  raw_html : function (x, ...)  
    raw_latex : function (x, ...)  
      raw_output : function (x, markers = raw_markers, ...)  
        read_chunk : function (path, lines = read_utf8(path), labels = NULL, from = NULL, to = NULL, from.offset = 0L, 
                               to.offset = 0L, roxygen_comments = TRUE)  
          read_demo : function (topic, package = NULL, ...)  
            read_rforge : function (path, project, extra = "")  
              render_asciidoc : function ()  
                render_html : function ()  
                  render_jekyll : function (highlight = c("pygments", "prettify", "none"), extra = "")  
                    render_latex : function ()  
                      render_listings : function ()  
                        render_markdown : function (strict = FALSE, fence_char = "`")  
                          render_rst : function (strict = FALSE)  
                            render_sweave : function ()  
                              render_textile : function ()  
                                restore_raw_output : function (text, chunks, markers = raw_markers)  
                                  rnw2pdf : function (input, output = with_ext(input, "pdf"), compiler = "xelatex", envir = parent.frame(), 
                                                      quiet = FALSE, clean = TRUE, error = FALSE, ...)  
                                    rocco : function (input, ...)  
                                      rst2pdf : function (input, command = "rst2pdf", options = "")  
                                        set_alias : function (...)  
                                          set_header : function (...)  
                                            set_parent : function (parent)  
                                              sew : function (x, options = list(), ...)  
                                                spin : function (hair, knit = TRUE, report = TRUE, text = NULL, envir = parent.frame(), format = c("Rmd", 
                                                                                                                                                   "Rnw", "Rhtml", "Rtex", "Rrst"), doc = "^#+'[ ]?", inline = "^[{][{](.+)[}][}][ ]*$", 
                                                                 comment = c("^[# ]*/[*]", "^.*[*]/ *$"), precious = !knit && is.null(text))  
                                                  spin_child : function (input, format)  
                                                    stitch : function (script, template = system.file("misc", "knitr-template.Rnw", package = "knitr"), 
                                                                       output = NULL, text = NULL, envir = parent.frame())  
                                                      stitch_rhtml : function (..., envir = parent.frame())  
                                                        stitch_rmd : function (..., envir = parent.frame())  
                                                          Sweave2knitr : function (file, output = gsub("[.]([^.]+)$", "-knitr.\\1", file), text = NULL)  
                                                            wrap_rmd : function (file, width = 80, text = NULL, backup)  
                                                              write_bib : function (x = .packages(), file = "", tweak = TRUE, width = NULL, prefix = getOption("knitr.bib.prefix", 
                                                                                                                                                               "R-"), lib.loc = NULL)  
                                                                > 
library(leaflet)
%>% : function (lhs, rhs)  
  addAwesomeMarkers : function (map, lng = NULL, lat = NULL, layerId = NULL, group = NULL, icon = NULL, 
                                popup = NULL, popupOptions = NULL, label = NULL, labelOptions = NULL, options = markerOptions(), 
                                clusterOptions = NULL, clusterId = NULL, data = getMapData(map))  
    addCircleMarkers : function (map, lng = NULL, lat = NULL, radius = 10, layerId = NULL, group = NULL, 
                                 stroke = TRUE, color = "#03F", weight = 5, opacity = 0.5, fill = TRUE, fillColor = color, 
                                 fillOpacity = 0.2, dashArray = NULL, popup = NULL, popupOptions = NULL, label = NULL, 
                                 labelOptions = NULL, options = pathOptions(), clusterOptions = NULL, clusterId = NULL, 
                                 data = getMapData(map))  
      addCircles : function (map, lng = NULL, lat = NULL, radius = 10, layerId = NULL, group = NULL, 
                             stroke = TRUE, color = "#03F", weight = 5, opacity = 0.5, fill = TRUE, fillColor = color, 
                             fillOpacity = 0.2, dashArray = NULL, popup = NULL, popupOptions = NULL, label = NULL, 
                             labelOptions = NULL, options = pathOptions(), highlightOptions = NULL, data = getMapData(map))  
        addControl : function (map, html, position = c("topleft", "topright", "bottomleft", "bottomright"), 
                               layerId = NULL, className = "info legend", data = getMapData(map))  
          addEasyButton : function (map, button)  
            addEasyButtonBar : function (map, ..., position = "topleft", id = NULL)  
              addGeoJSON : function (map, geojson, layerId = NULL, group = NULL, stroke = TRUE, color = "#03F", 
                                     weight = 5, opacity = 0.5, fill = TRUE, fillColor = color, fillOpacity = 0.2, 
                                     dashArray = NULL, smoothFactor = 1, noClip = FALSE, options = pathOptions(), data = getMapData(map))  
                addGraticule : function (map, interval = 20, sphere = FALSE, style = list(color = "#333", weight = 1), 
                                         layerId = NULL, group = NULL, options = pathOptions(pointerEvents = "none", clickable = FALSE))  
                  addLabelOnlyMarkers : function (map, lng = NULL, lat = NULL, layerId = NULL, group = NULL, icon = NULL, 
                                                  label = NULL, labelOptions = NULL, options = markerOptions(), clusterOptions = NULL, 
                                                  clusterId = NULL, data = getMapData(map))  
                    addLayersControl : function (map, baseGroups = character(0), overlayGroups = character(0), position = c("topright", 
                                                                                                                            "bottomright", "bottomleft", "topleft"), options = layersControlOptions(), data = getMapData(map))  
                      addLegend : function (map, position = c("topright", "bottomright", "bottomleft", "topleft"), pal, 
                                            values, na.label = "NA", bins = 7, colors, opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                            title = NULL, className = "info legend", layerId = NULL, group = NULL, data = getMapData(map))  
                        addMapPane : function (map, name, zIndex)  
                          addMarkers : function (map, lng = NULL, lat = NULL, layerId = NULL, group = NULL, icon = NULL, 
                                                 popup = NULL, popupOptions = NULL, label = NULL, labelOptions = NULL, options = markerOptions(), 
                                                 clusterOptions = NULL, clusterId = NULL, data = getMapData(map))  
                            addMeasure : function (map, position = "topright", primaryLengthUnit = "feet", secondaryLengthUnit = NULL, 
                                                   primaryAreaUnit = "acres", secondaryAreaUnit = NULL, activeColor = "#ABE67E", 
                                                   completedColor = "#C8F2BE", popupOptions = list(className = "leaflet-measure-resultpopup", 
                                                                                                   autoPanPadding = c(10, 10)), captureZIndex = 10000, localization = "en", decPoint = ".", 
                                                   thousandsSep = ",")  
                              addMiniMap : function (map, position = "bottomright", width = 150, height = 150, collapsedWidth = 19, 
                                                     collapsedHeight = 19, zoomLevelOffset = -5, zoomLevelFixed = FALSE, centerFixed = FALSE, 
                                                     zoomAnimation = FALSE, toggleDisplay = FALSE, autoToggleDisplay = FALSE, minimized = FALSE, 
                                                     aimingRectOptions = list(color = "#ff7800", weight = 1, clickable = FALSE), shadowRectOptions = list(color = "#000000", 
                                                                                                                                                          weight = 1, clickable = FALSE, opacity = 0, fillOpacity = 0), strings = list(hideText = "Hide MiniMap", 
                                                                                                                                                                                                                                       showText = "Show MiniMap"), tiles = NULL, mapOptions = list())  
                                addPolygons : function (map, lng = NULL, lat = NULL, layerId = NULL, group = NULL, stroke = TRUE, 
                                                        color = "#03F", weight = 5, opacity = 0.5, fill = TRUE, fillColor = color, fillOpacity = 0.2, 
                                                        dashArray = NULL, smoothFactor = 1, noClip = FALSE, popup = NULL, popupOptions = NULL, 
                                                        label = NULL, labelOptions = NULL, options = pathOptions(), highlightOptions = NULL, 
                                                        data = getMapData(map))  
                                  addPolylines : function (map, lng = NULL, lat = NULL, layerId = NULL, group = NULL, stroke = TRUE, 
                                                           color = "#03F", weight = 5, opacity = 0.5, fill = FALSE, fillColor = color, fillOpacity = 0.2, 
                                                           dashArray = NULL, smoothFactor = 1, noClip = FALSE, popup = NULL, popupOptions = NULL, 
                                                           label = NULL, labelOptions = NULL, options = pathOptions(), highlightOptions = NULL, 
                                                           data = getMapData(map))  
                                    addPopups : function (map, lng = NULL, lat = NULL, popup, layerId = NULL, group = NULL, options = popupOptions(), 
                                                          data = getMapData(map))  
                                      addProviderTiles : function (map, provider, layerId = NULL, group = NULL, options = providerTileOptions())  
                                        addRasterImage : function (map, x, colors = if (raster::is.factor(x)) "Set1" else "Spectral", opacity = 1, 
                                                                   attribution = NULL, layerId = NULL, group = NULL, project = TRUE, method = c("auto", 
                                                                                                                                                "bilinear", "ngb"), maxBytes = 4 * 1024 * 1024, data = getMapData(map))  
                                          addRectangles : function (map, lng1, lat1, lng2, lat2, layerId = NULL, group = NULL, stroke = TRUE, 
                                                                    color = "#03F", weight = 5, opacity = 0.5, fill = TRUE, fillColor = color, fillOpacity = 0.2, 
                                                                    dashArray = NULL, smoothFactor = 1, noClip = FALSE, popup = NULL, popupOptions = NULL, 
                                                                    label = NULL, labelOptions = NULL, options = pathOptions(), highlightOptions = NULL, 
                                                                    data = getMapData(map))  
                                            addScaleBar : function (map, position = c("topright", "bottomright", "bottomleft", "topleft"), options = scaleBarOptions())  
                                              addSimpleGraticule : function (map, interval = 20, showOriginLabel = TRUE, redraw = "move", hidden = FALSE, 
                                                                             zoomIntervals = list(), layerId = NULL, group = NULL)  
                                                addTerminator : function (map, resolution = 2, time = NULL, layerId = NULL, group = NULL, options = pathOptions(pointerEvents = "none", 
                                                                                                                                                                clickable = FALSE))  
                                                  addTiles : function (map, urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", 
                                                                       attribution = NULL, layerId = NULL, group = NULL, options = tileOptions(), data = getMapData(map))  
                                                    addTopoJSON : function (map, topojson, layerId = NULL, group = NULL, stroke = TRUE, color = "#03F", 
                                                                            weight = 5, opacity = 0.5, fill = TRUE, fillColor = color, fillOpacity = 0.2, 
                                                                            dashArray = NULL, smoothFactor = 1, noClip = FALSE, options = pathOptions())  
                                                      addWMSTiles : function (map, baseUrl, layerId = NULL, group = NULL, options = WMSTileOptions(), 
                                                                              attribution = NULL, layers = "", data = getMapData(map))  
                                                        atlStorms2005 : Formal class 'SpatialLinesDataFrame' [package "sp"] with 4 slots
awesomeIconList : function (...)  
  awesomeIcons : function (icon = "home", library = "glyphicon", markerColor = "blue", iconColor = "white", 
                           spin = FALSE, extraClasses = NULL, squareMarker = FALSE, iconRotate = 0, fontFamily = "monospace", 
                           text = NULL)  
    breweries91 : Formal class 'SpatialPointsDataFrame' [package "sp"] with 5 slots
clearBounds : function (map)  
  clearControls : function (map)  
    clearGeoJSON : function (map)  
      clearGroup : function (map, group)  
        clearImages : function (map)  
          clearMarkerClusters : function (map)  
            clearMarkers : function (map)  
              clearPopups : function (map)  
                clearShapes : function (map)  
                  clearTiles : function (map)  
                    clearTopoJSON : function (map)  
                      colorBin : function (palette, domain, bins = 7, pretty = TRUE, na.color = "#808080", alpha = FALSE, 
                                           reverse = FALSE, right = FALSE)  
                        colorFactor : function (palette, domain, levels = NULL, ordered = FALSE, na.color = "#808080", alpha = FALSE, 
                                                reverse = FALSE)  
                          colorNumeric : function (palette, domain, na.color = "#808080", alpha = FALSE, reverse = FALSE)  
                            colorQuantile : function (palette, domain, n = 4, probs = seq(0, 1, length.out = n + 1), na.color = "#808080", 
                                                      alpha = FALSE, reverse = FALSE, right = FALSE)  
                              createLeafletMap : function (session, outputId)  
                                derivePoints : function (data, lng = NULL, lat = NULL, missingLng = missing(lng), missingLat = missing(lat), 
                                                         funcName = "f")  
                                  derivePolygons : function (data, lng = NULL, lat = NULL, missingLng = missing(lng), missingLat = missing(lat), 
                                                             funcName = "f")  
                                    dispatch : function (map, funcName, leaflet = stop(paste(funcName, "requires a map proxy object")), 
                                                         leaflet_proxy = stop(paste(funcName, "does not support map proxy objects")))  
                                      easyButton : function (icon = NULL, title = NULL, onClick = NULL, position = "topleft", id = NULL, 
                                                             states = NULL)  
                                        easyButtonState : function (stateName, icon, title, onClick)  
                                          evalFormula : function (list, data)  
                                            expandLimits : function (map, lat, lng)  
                                              expandLimitsBbox : function (map, poly)  
                                                filterNULL : function (x)  
                                                  fitBounds : function (map, lng1, lat1, lng2, lat2, options = list())  
                                                    flyTo : function (map, lng, lat, zoom, options = list())  
                                                      flyToBounds : function (map, lng1, lat1, lng2, lat2, options = list())  
                                                        gadmCHE : Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
getMapData : function (map)  
  groupOptions : function (map, group, zoomLevels = NULL)  
    hideGroup : function (map, group)  
      highlightOptions : function (stroke = NULL, color = NULL, weight = NULL, opacity = NULL, fill = NULL, 
                                   fillColor = NULL, fillOpacity = NULL, dashArray = NULL, bringToFront = NULL, sendToBack = NULL)  
        iconList : function (...)  
          icons : function (iconUrl = NULL, iconRetinaUrl = NULL, iconWidth = NULL, iconHeight = NULL, 
                            iconAnchorX = NULL, iconAnchorY = NULL, shadowUrl = NULL, shadowRetinaUrl = NULL, 
                            shadowWidth = NULL, shadowHeight = NULL, shadowAnchorX = NULL, shadowAnchorY = NULL, 
                            popupAnchorX = NULL, popupAnchorY = NULL, className = NULL)  
            invokeMethod : function (map, data, method, ...)  
              JS : function (...)  
                labelFormat : function (prefix = "", suffix = "", between = " &ndash; ", digits = 3, big.mark = ",", 
                                        transform = identity)  
                  labelOptions : function (interactive = FALSE, clickable = NULL, noHide = NULL, permanent = FALSE, 
                                           className = "", direction = "auto", offset = c(0, 0), opacity = 1, textsize = "10px", 
                                           textOnly = FALSE, style = NULL, zoomAnimation = NULL, sticky = TRUE, ...)  
                    layersControlOptions : function (collapsed = TRUE, autoZIndex = TRUE, ...)  
                      leaflet : function (data = NULL, width = NULL, height = NULL, padding = 0, options = leafletOptions(), 
                                          elementId = NULL, sizingPolicy = leafletSizingPolicy(padding = padding))  
                        leafletCRS : function (crsClass = "L.CRS.EPSG3857", code = NULL, proj4def = NULL, projectedBounds = NULL, 
                                               origin = NULL, transformation = NULL, scales = NULL, resolutions = NULL, bounds = NULL, 
                                               tileSize = NULL)  
                          leafletDependencies : List of 13
$ markerCluster  :function ()  
  $ awesomeMarkers :function ()  
    $ bootstrap      :function ()  
      $ fontawesome    :function ()  
        $ ionicon        :function ()  
          $ omnivore       :function ()  
            $ graticule      :function ()  
              $ simpleGraticule:function ()  
                $ easyButton     :function ()  
                  $ measure        :function ()  
                    $ terminator     :function ()  
                      $ minimap        :function ()  
                        $ providers      :function ()  
                          leafletMap : function (outputId, width, height, initialTileLayer = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", 
                                                 initialTileLayerAttribution = NULL, options = NULL)  
                            leafletOptions : function (minZoom = NULL, maxZoom = NULL, crs = leafletCRS(), worldCopyJump = NULL, 
                                                       preferCanvas = NULL, ...)  
                              leafletOutput : function (outputId, width = "100%", height = 400)  
                                leafletProxy : function (mapId, session = shiny::getDefaultReactiveDomain(), data = NULL, deferUntilFlush = TRUE)  
                                  leafletSizingPolicy : function (defaultWidth = "100%", defaultHeight = 400, padding = 0, browser.fill = TRUE, 
                                                                  ...)  
                                    makeAwesomeIcon : function (icon = "home", library = "glyphicon", markerColor = "blue", iconColor = "white", 
                                                                spin = FALSE, extraClasses = NULL, squareMarker = FALSE, iconRotate = 0, fontFamily = "monospace", 
                                                                text = NULL)  
                                      makeIcon : function (iconUrl = NULL, iconRetinaUrl = NULL, iconWidth = NULL, iconHeight = NULL, 
                                                           iconAnchorX = NULL, iconAnchorY = NULL, shadowUrl = NULL, shadowRetinaUrl = NULL, 
                                                           shadowWidth = NULL, shadowHeight = NULL, shadowAnchorX = NULL, shadowAnchorY = NULL, 
                                                           popupAnchorX = NULL, popupAnchorY = NULL, className = NULL)  
                                        mapOptions : function (map, zoomToLimits = c("always", "first", "never"))  
                                          markerClusterOptions : function (showCoverageOnHover = TRUE, zoomToBoundsOnClick = TRUE, spiderfyOnMaxZoom = TRUE, 
                                                                           removeOutsideVisibleBounds = TRUE, spiderLegPolylineOptions = list(weight = 1.5, 
                                                                                                                                              color = "#222", opacity = 0.5), freezeAtZoom = FALSE, ...)  
                                            markerOptions : function (interactive = TRUE, clickable = NULL, draggable = FALSE, keyboard = TRUE, 
                                                                      title = "", alt = "", zIndexOffset = 0, opacity = 1, riseOnHover = FALSE, riseOffset = 250, 
                                                                      ...)  
                                              pathOptions : function (lineCap = NULL, lineJoin = NULL, clickable = NULL, interactive = TRUE, pointerEvents = NULL, 
                                                                      className = "", ...)  
                                                popupOptions : function (maxWidth = 300, minWidth = 50, maxHeight = NULL, autoPan = TRUE, keepInView = FALSE, 
                                                                         closeButton = TRUE, zoomAnimation = NULL, closeOnClick = NULL, className = "", 
                                                                         ...)  
                                                  previewColors : function (pal, values)  
                                                    projectRasterForLeaflet : function (x, method)  
                                                      providers : List of 164
$ OpenStreetMap                      : chr "OpenStreetMap"
$ OpenStreetMap.Mapnik               : chr "OpenStreetMap.Mapnik"
$ OpenStreetMap.DE                   : chr "OpenStreetMap.DE"
$ OpenStreetMap.CH                   : chr "OpenStreetMap.CH"
$ OpenStreetMap.France               : chr "OpenStreetMap.France"
$ OpenStreetMap.HOT                  : chr "OpenStreetMap.HOT"
$ OpenStreetMap.BZH                  : chr "OpenStreetMap.BZH"
$ OpenSeaMap                         : chr "OpenSeaMap"
$ OpenPtMap                          : chr "OpenPtMap"
$ OpenTopoMap                        : chr "OpenTopoMap"
$ OpenRailwayMap                     : chr "OpenRailwayMap"
$ OpenFireMap                        : chr "OpenFireMap"
$ SafeCast                           : chr "SafeCast"
$ Thunderforest                      : chr "Thunderforest"
$ Thunderforest.OpenCycleMap         : chr "Thunderforest.OpenCycleMap"
$ Thunderforest.Transport            : chr "Thunderforest.Transport"
$ Thunderforest.TransportDark        : chr "Thunderforest.TransportDark"
$ Thunderforest.SpinalMap            : chr "Thunderforest.SpinalMap"
$ Thunderforest.Landscape            : chr "Thunderforest.Landscape"
$ Thunderforest.Outdoors             : chr "Thunderforest.Outdoors"
$ Thunderforest.Pioneer              : chr "Thunderforest.Pioneer"
$ Thunderforest.MobileAtlas          : chr "Thunderforest.MobileAtlas"
$ Thunderforest.Neighbourhood        : chr "Thunderforest.Neighbourhood"
$ OpenMapSurfer                      : chr "OpenMapSurfer"
$ OpenMapSurfer.Roads                : chr "OpenMapSurfer.Roads"
$ OpenMapSurfer.Hybrid               : chr "OpenMapSurfer.Hybrid"
$ OpenMapSurfer.AdminBounds          : chr "OpenMapSurfer.AdminBounds"
$ OpenMapSurfer.ContourLines         : chr "OpenMapSurfer.ContourLines"
$ OpenMapSurfer.Hillshade            : chr "OpenMapSurfer.Hillshade"
$ OpenMapSurfer.ElementsAtRisk       : chr "OpenMapSurfer.ElementsAtRisk"
$ Hydda                              : chr "Hydda"
$ Hydda.Full                         : chr "Hydda.Full"
$ Hydda.Base                         : chr "Hydda.Base"
$ Hydda.RoadsAndLabels               : chr "Hydda.RoadsAndLabels"
$ MapBox                             : chr "MapBox"
$ Stamen                             : chr "Stamen"
$ Stamen.Toner                       : chr "Stamen.Toner"
$ Stamen.TonerBackground             : chr "Stamen.TonerBackground"
$ Stamen.TonerHybrid                 : chr "Stamen.TonerHybrid"
$ Stamen.TonerLines                  : chr "Stamen.TonerLines"
$ Stamen.TonerLabels                 : chr "Stamen.TonerLabels"
$ Stamen.TonerLite                   : chr "Stamen.TonerLite"
$ Stamen.Watercolor                  : chr "Stamen.Watercolor"
$ Stamen.Terrain                     : chr "Stamen.Terrain"
$ Stamen.TerrainBackground           : chr "Stamen.TerrainBackground"
$ Stamen.TerrainLabels               : chr "Stamen.TerrainLabels"
$ Stamen.TopOSMRelief                : chr "Stamen.TopOSMRelief"
$ Stamen.TopOSMFeatures              : chr "Stamen.TopOSMFeatures"
$ TomTom                             : chr "TomTom"
$ TomTom.Basic                       : chr "TomTom.Basic"
$ TomTom.Hybrid                      : chr "TomTom.Hybrid"
$ TomTom.Labels                      : chr "TomTom.Labels"
$ Esri                               : chr "Esri"
$ Esri.WorldStreetMap                : chr "Esri.WorldStreetMap"
$ Esri.DeLorme                       : chr "Esri.DeLorme"
$ Esri.WorldTopoMap                  : chr "Esri.WorldTopoMap"
$ Esri.WorldImagery                  : chr "Esri.WorldImagery"
$ Esri.WorldTerrain                  : chr "Esri.WorldTerrain"
$ Esri.WorldShadedRelief             : chr "Esri.WorldShadedRelief"
$ Esri.WorldPhysical                 : chr "Esri.WorldPhysical"
$ Esri.OceanBasemap                  : chr "Esri.OceanBasemap"
$ Esri.NatGeoWorldMap                : chr "Esri.NatGeoWorldMap"
$ Esri.WorldGrayCanvas               : chr "Esri.WorldGrayCanvas"
$ OpenWeatherMap                     : chr "OpenWeatherMap"
$ OpenWeatherMap.Clouds              : chr "OpenWeatherMap.Clouds"
$ OpenWeatherMap.CloudsClassic       : chr "OpenWeatherMap.CloudsClassic"
$ OpenWeatherMap.Precipitation       : chr "OpenWeatherMap.Precipitation"
$ OpenWeatherMap.PrecipitationClassic: chr "OpenWeatherMap.PrecipitationClassic"
$ OpenWeatherMap.Rain                : chr "OpenWeatherMap.Rain"
$ OpenWeatherMap.RainClassic         : chr "OpenWeatherMap.RainClassic"
$ OpenWeatherMap.Pressure            : chr "OpenWeatherMap.Pressure"
$ OpenWeatherMap.PressureContour     : chr "OpenWeatherMap.PressureContour"
$ OpenWeatherMap.Wind                : chr "OpenWeatherMap.Wind"
$ OpenWeatherMap.Temperature         : chr "OpenWeatherMap.Temperature"
$ OpenWeatherMap.Snow                : chr "OpenWeatherMap.Snow"
$ HERE                               : chr "HERE"
$ HERE.normalDay                     : chr "HERE.normalDay"
$ HERE.normalDayCustom               : chr "HERE.normalDayCustom"
$ HERE.normalDayGrey                 : chr "HERE.normalDayGrey"
$ HERE.normalDayMobile               : chr "HERE.normalDayMobile"
$ HERE.normalDayGreyMobile           : chr "HERE.normalDayGreyMobile"
$ HERE.normalDayTransit              : chr "HERE.normalDayTransit"
$ HERE.normalDayTransitMobile        : chr "HERE.normalDayTransitMobile"
$ HERE.normalDayTraffic              : chr "HERE.normalDayTraffic"
$ HERE.normalNight                   : chr "HERE.normalNight"
$ HERE.normalNightMobile             : chr "HERE.normalNightMobile"
$ HERE.normalNightGrey               : chr "HERE.normalNightGrey"
$ HERE.normalNightGreyMobile         : chr "HERE.normalNightGreyMobile"
$ HERE.normalNightTransit            : chr "HERE.normalNightTransit"
$ HERE.normalNightTransitMobile      : chr "HERE.normalNightTransitMobile"
$ HERE.reducedDay                    : chr "HERE.reducedDay"
$ HERE.reducedNight                  : chr "HERE.reducedNight"
$ HERE.basicMap                      : chr "HERE.basicMap"
$ HERE.mapLabels                     : chr "HERE.mapLabels"
$ HERE.trafficFlow                   : chr "HERE.trafficFlow"
$ HERE.carnavDayGrey                 : chr "HERE.carnavDayGrey"
$ HERE.hybridDay                     : chr "HERE.hybridDay"
$ HERE.hybridDayMobile               : chr "HERE.hybridDayMobile"
$ HERE.hybridDayTransit              : chr "HERE.hybridDayTransit"
[list output truncated]
providers.details : List of 28
$ OpenStreetMap   :List of 3
$ OpenSeaMap      :List of 2
$ OpenPtMap       :List of 2
$ OpenTopoMap     :List of 2
$ OpenRailwayMap  :List of 2
$ OpenFireMap     :List of 2
$ SafeCast        :List of 2
$ Thunderforest   :List of 3
$ OpenMapSurfer   :List of 3
$ Hydda           :List of 3
$ MapBox          :List of 2
$ Stamen          :List of 3
$ TomTom          :List of 3
$ Esri            :List of 3
$ OpenWeatherMap  :List of 3
$ HERE            :List of 3
$ FreeMapSK       :List of 2
$ MtbMap          :List of 2
$ CartoDB         :List of 3
$ HikeBike        :List of 3
$ BasemapAT       :List of 3
$ nlmaps          :List of 3
$ NASAGIBS        :List of 3
$ NLS             :List of 2
$ JusticeMap      :List of 3
$ Wikimedia       :List of 2
$ GeoportailFrance:List of 3
$ OneMapSG        :List of 3
providerTileOptions : function (errorTileUrl = "", noWrap = FALSE, opacity = NULL, zIndex = NULL, updateWhenIdle = NULL, 
                                detectRetina = FALSE, ...)  
  removeControl : function (map, layerId)  
    removeGeoJSON : function (map, layerId)  
      removeImage : function (map, layerId)  
        removeLayersControl : function (map)  
          removeMarker : function (map, layerId)  
            removeMarkerCluster : function (map, layerId)  
              removeMarkerFromCluster : function (map, layerId, clusterId)  
                removeMeasure : function (map)  
                  removePopup : function (map, layerId)  
                    removeScaleBar : function (map)  
                      removeShape : function (map, layerId)  
                        removeTiles : function (map, layerId)  
                          removeTopoJSON : function (map, layerId)  
                            renderLeaflet : function (expr, env = parent.frame(), quoted = FALSE)  
                              safeLabel : function (label, data)  
                                scaleBarOptions : function (maxWidth = 100, metric = TRUE, imperial = TRUE, updateWhenIdle = TRUE)  
                                  setMaxBounds : function (map, lng1, lat1, lng2, lat2)  
                                    setView : function (map, lng, lat, zoom, options = list())  
                                      showGroup : function (map, group)  
                                        tileOptions : function (minZoom = 0, maxZoom = 18, maxNativeZoom = NULL, tileSize = 256, subdomains = "abc", 
                                                                errorTileUrl = "", tms = FALSE, noWrap = FALSE, zoomOffset = 0, zoomReverse = FALSE, 
                                                                opacity = 1, zIndex = 1, unloadInvisibleTiles = NULL, updateWhenIdle = NULL, detectRetina = FALSE, 
                                                                ...)  
                                          validateCoords : function (lng, lat, funcName, warn = TRUE, mode = c("point", "polygon"))  
                                            WMSTileOptions : function (styles = "", format = "image/jpeg", transparent = FALSE, version = "1.1.1", 
                                                                       crs = NULL, ...)  
                                              > 
library(lobstr)
ast : function (x)  
  cst : function ()  
    mem_used : function ()  
      obj_addr : function (x)  
        obj_addrs : function (x)  
          obj_size : function (..., env = parent.frame())  
            obj_sizes : function (..., env = parent.frame())  
              ref : function (..., character = FALSE)  
                sxp : function (x, expand = character(), max_depth = 5L)  
                  tree : function (x, ..., index_unnamed = FALSE, max_depth = 10L, max_length = 1000L, show_environments = TRUE, 
                                   hide_scalar_types = TRUE, val_printer = crayon::blue, class_printer = crayon::silver, 
                                   show_attributes = FALSE, remove_newlines = TRUE, tree_chars = box_chars())  
                    tree_label : function (x, opts)  
                      > 
library(lubridate)
%--% : function (start, end)  
  %m-% : Formal class 'standardGeneric' [package "methods"] with 8 slots
%m+% : Formal class 'standardGeneric' [package "methods"] with 8 slots
%within% : Formal class 'standardGeneric' [package "methods"] with 8 slots
.__C__Duration : Formal class 'classRepresentation' [package "methods"] with 11 slots
.__C__Interval : Formal class 'classRepresentation' [package "methods"] with 11 slots
.__C__Period : Formal class 'classRepresentation' [package "methods"] with 11 slots
.__C__Timespan : Formal class 'classRepresentation' [package "methods"] with 11 slots
.__T__-:base : <environment: 0x000001db806e4aa0> 
  .__T__$:base : <environment: 0x000001db807f0e20> 
  .__T__$<-:base : <environment: 0x000001db8095bdd0> 
  .__T__%%:base : <environment: 0x000001db809b64a8> 
  .__T__%/%:base : <environment: 0x000001db80aabb98> 
  .__T__%m-%:lubridate : <environment: 0x000001db80bb6990> 
  .__T__%m+%:lubridate : <environment: 0x000001db80bdff40> 
  .__T__%within%:lubridate : <environment: 0x000001db80c26730> 
  .__T__*:base : <environment: 0x000001db80c588d0> 
  .__T__/:base : <environment: 0x000001db80d71c00> 
  .__T__[:base : <environment: 0x000001db80ed7b30> 
           .__T__[[:base : <environment: 0x000001db8100e300> 
                     .__T__[[<-:base : <environment: 0x000001db81050648> 
                               .__T__[<-:base : <environment: 0x000001db810bd9b0> 
                                        .__T__+:base : <environment: 0x000001db81170c58> 
                                        .__T__Arith:base : <environment: 0x000001db8130b410> 
                                        .__T__as.character:base : <environment: 0x000001db81be10e8> 
                                        .__T__as.difftime:base : <environment: 0x000001db81c9bad8> 
                                        .__T__as.duration:lubridate : <environment: 0x000001db81ce4ae8> 
                                        .__T__as.interval:lubridate : <environment: 0x000001db81d963c0> 
                                        .__T__as.numeric:base : <environment: 0x000001db81dc24b0> 
                                        .__T__as.period:lubridate : <environment: 0x000001db81e2f7b0> 
                                        .__T__as_date:lubridate : <environment: 0x000001db81eacc30> 
                                        .__T__as_datetime:lubridate : <environment: 0x000001db81efca60> 
                                        .__T__c:base : <environment: 0x000001db81f37398> 
                                        .__T__Compare:methods : <environment: 0x000001db81fc4e70> 
                                        .__T__date<-:lubridate : <environment: 0x000001db82613ca0> 
                                        .__T__day<-:lubridate : <environment: 0x000001db8264c580> 
                                        .__T__format_ISO8601:lubridate : <environment: 0x000001db8268ad18> 
                                        .__T__hour<-:lubridate : <environment: 0x000001db826e6348> 
                                        .__T__minute<-:lubridate : <environment: 0x000001db827864a0> 
                                        .__T__month<-:lubridate : <environment: 0x000001db827c1e20> 
                                        .__T__qday<-:lubridate : <environment: 0x000001db827f0750> 
                                        .__T__reclass_timespan:lubridate : <environment: 0x000001db8281d728> 
                                        .__T__rep:base : <environment: 0x000001db8283bc08> 
                                        .__T__second<-:lubridate : <environment: 0x000001dbf636ee70> 
                                        .__T__show:methods : <environment: 0x000001dbf623e9d8> 
                                        .__T__time_length:lubridate : <environment: 0x000001dbf6184bb0> 
                                        .__T__year<-:lubridate : <environment: 0x000001dbf6166b30> 
                                        .Depends :  chr "methods"
                                      add_with_rollback : function (e1, e2, roll_to_first = FALSE, preserve_hms = TRUE)  
                                        am : function (x)  
                                          Arith : Formal class 'groupGenericFunction' [package "methods"] with 9 slots
                                      as.difftime : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      as.duration : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      as.interval : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      as.period : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      as_date : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      as_datetime : Formal class 'nonstandardGenericFunction' [package "methods"] with 8 slots
                                      ceiling_date : function (x, unit = "seconds", change_on_boundary = NULL, week_start = getOption("lubridate.week.start", 
                                                                                                                                      7))  
                                        Compare : Formal class 'groupGenericFunction' [package "methods"] with 9 slots
                                      cyclic_encoding : function (x, periods, encoders = c("sin", "cos"), week_start = getOption("lubridate.week.start", 
                                                                                                                                 7))  
                                        date : function (x)  
                                          Date : function (length = 0L)  
                                            date_decimal : function (decimal, tz = "UTC")  
                                              date<- : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      day : function (x)  
                                        day<- : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      days : function (x = 1)  
                                        days_in_month : function (x)  
                                          ddays : function (x = 1)  
                                            decimal_date : function (date)  
                                              dhours : function (x = 1)  
                                                dmicroseconds : function (x = 1)  
                                                  dmilliseconds : function (x = 1)  
                                                    dminutes : function (x = 1)  
                                                      dmonths : function (x = 1)  
                                                        dmy : function (..., quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                                          dmy_h : function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                                            dmy_hm : function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                                              dmy_hms : function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                                                dnanoseconds : function (x = 1)  
                                                                  dpicoseconds : function (x = 1)  
                                                                    dseconds : function (x = 1)  
                                                                      dst : function (x)  
                                                                        duration : function (num = NULL, units = "seconds", ...)  
                                                                          dweeks : function (x = 1)  
                                                                            dyears : function (x = 1)  
                                                                              dym : function (..., quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                                                                epiweek : function (x)  
                                                                                  epiyear : function (x)  
                                                                                    fast_strptime : function (x, format, tz = "UTC", lt = TRUE, cutoff_2000 = 68L)  
                                                                                      fit_to_timeline : function (lt, class = "POSIXct", simple = FALSE)  
                                                                                        floor_date : function (x, unit = "seconds", week_start = getOption("lubridate.week.start", 7))  
                                                                                          force_tz : function (time, tzone = "", roll = FALSE)  
                                                                                            force_tzs : function (time, tzones, tzone_out = "UTC", roll = FALSE)  
                                                                                              format_ISO8601 : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      guess_formats : function (x, orders, locale = Sys.getlocale("LC_TIME"), preproc_wday = TRUE, print_matches = FALSE)  
                                        hm : function (..., quiet = FALSE, roll = FALSE)  
                                          hms : function (..., quiet = FALSE, roll = FALSE)  
                                            hour : function (x)  
                                              hour<- : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      hours : function (x = 1)  
                                        int_aligns : function (int1, int2)  
                                          int_diff : function (times)  
                                            int_end : function (int)  
                                              int_end<- : function (int, value)  
                                                int_flip : function (int)  
                                                  int_length : function (int)  
                                                    int_overlaps : function (int1, int2)  
                                                      int_shift : function (int, by)  
                                                        int_standardize : function (int)  
                                                          int_start : function (int)  
                                                            int_start<- : function (int, value)  
                                                              intersect : function (x, y, ...)  
                                                                interval : function (start = NULL, end = NULL, tzone = tz(start))  
                                                                  is.Date : function (x)  
                                                                    is.difftime : function (x)  
                                                                      is.duration : function (x)  
                                                                        is.instant : function (x)  
                                                                          is.interval : function (x)  
                                                                            is.period : function (x)  
                                                                              is.POSIXct : function (x)  
                                                                                is.POSIXlt : function (x)  
                                                                                  is.POSIXt : function (x)  
                                                                                    is.timepoint : function (x)  
                                                                                      is.timespan : function (x)  
                                                                                        isoweek : function (x)  
                                                                                          isoyear : function (x)  
                                                                                            lakers : 'data.frame':	34624 obs. of  13 variables:
                                        $ date     : int  20081028 20081028 20081028 20081028 20081028 20081028 20081028 20081028 20081028 20081028 ...
                                      $ opponent : chr  "POR" "POR" "POR" "POR" ...
                                      $ game_type: chr  "home" "home" "home" "home" ...
                                      $ time     : chr  "12:00" "11:39" "11:37" "11:25" ...
                                      $ period   : int  1 1 1 1 1 1 1 1 1 1 ...
                                      $ etype    : chr  "jump ball" "shot" "rebound" "shot" ...
                                      $ team     : chr  "OFF" "LAL" "LAL" "LAL" ...
                                      $ player   : chr  "" "Pau Gasol" "Vladimir Radmanovic" "Derek Fisher" ...
                                      $ result   : chr  "" "missed" "" "missed" ...
                                      $ points   : int  0 0 0 0 0 2 0 1 0 2 ...
                                      $ type     : chr  "" "hook" "off" "layup" ...
                                      $ x        : int  NA 23 NA 25 NA 25 NA NA NA 36 ...
                                      $ y        : int  NA 13 NA 6 NA 10 NA NA NA 21 ...
                                      leap_year : function (date)  
                                        local_time : function (dt, tz = NULL, units = "secs")  
                                          make_date : function (year = 1970L, month = 1L, day = 1L)  
                                            make_datetime : function (year = 1970L, month = 1L, day = 1L, hour = 0L, min = 0L, sec = 0, tz = "UTC")  
                                              make_difftime : function (num = NULL, units = "auto", ...)  
                                                mday : function (x)  
                                                  mday<- : function (x, value)  
                                                    mdy : function (..., quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                                      mdy_h : function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                                        mdy_hm : function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                                          mdy_hms : function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                                            microseconds : function (x = 1)  
                                                              milliseconds : function (x = 1)  
                                                                minute : function (x)  
                                                                  minute<- : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      minutes : function (x = 1)  
                                        month : function (x, label = FALSE, abbr = TRUE, locale = Sys.getlocale("LC_TIME"))  
                                          month<- : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      ms : function (..., quiet = FALSE, roll = FALSE)  
                                        my : function (..., quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))  
                                          myd : function (..., quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                            NA_Date_ :  Date[1:1], format: NA
                                      NA_POSIXct_ :  POSIXct[1:1], format: NA
                                      nanoseconds : function (x = 1)  
                                        now : function (tzone = "")  
                                          origin :  POSIXct[1:1], format: "1970-01-01"
                                      parse_date_time : function (x, orders, tz = "UTC", truncated = 0, quiet = FALSE, locale = Sys.getlocale("LC_TIME"), 
                                                                  select_formats = .select_formats, exact = FALSE, train = TRUE, drop = FALSE)  
                                        parse_date_time2 : function (x, orders, tz = "UTC", exact = FALSE, lt = FALSE, cutoff_2000 = 68L)  
                                          period : function (num = NULL, units = "second", ...)  
                                            period_to_seconds : function (x)  
                                              picoseconds : function (x = 1)  
                                                pm : function (x)  
                                                  POSIXct : function (length = 0L, tz = "UTC")  
                                                    pretty_dates : function (x, n, ...)  
                                                      qday : function (x)  
                                                        qday<- : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      quarter : function (x, type = "quarter", fiscal_start = 1, with_year = identical(type, "year.quarter"))  
                                        reclass_date : function (new, orig)  
                                          reclass_timespan : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      rollback : function (dates, roll_to_first = FALSE, preserve_hms = TRUE)  
                                        rollbackward : function (dates, roll_to_first = FALSE, preserve_hms = TRUE)  
                                          rollforward : function (dates, roll_to_first = FALSE, preserve_hms = TRUE)  
                                            round_date : function (x, unit = "second", week_start = getOption("lubridate.week.start", 7))  
                                              second : function (x)  
                                                second<- : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      seconds : function (x = 1)  
                                        seconds_to_period : function (x)  
                                          semester : function (x, with_year = FALSE)  
                                            setdiff : function (x, y, ...)  
                                              show : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      stamp : function (x, orders = lubridate_formats, locale = Sys.getlocale("LC_TIME"), quiet = FALSE)  
                                        stamp_date : function (x, locale = Sys.getlocale("LC_TIME"), quiet = FALSE)  
                                          stamp_time : function (x, locale = Sys.getlocale("LC_TIME"), quiet = FALSE)  
                                            time_length : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      today : function (tzone = "")  
                                        tz : function (x)  
                                          tz<- : function (x, value)  
                                            union : function (x, y, ...)  
                                              wday : function (x, label = FALSE, abbr = TRUE, week_start = getOption("lubridate.week.start", 
                                                                                                                     7), locale = Sys.getlocale("LC_TIME"))  
                                                wday<- : function (x, week_start = getOption("lubridate.week.start", 7), value)  
                                                  week : function (x)  
                                                    week<- : function (x, value)  
                                                      weeks : function (x = 1)  
                                                        with_tz : function (time, tzone = "")  
                                                          yday : function (x)  
                                                            yday<- : function (x, value)  
                                                              ydm : function (..., quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                                                ydm_h : function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                                                  ydm_hm : function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                                                    ydm_hms : function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                                                      year : function (x)  
                                                                        year<- : Formal class 'standardGeneric' [package "methods"] with 8 slots
                                      years : function (x = 1)  
                                        ym : function (..., quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))  
                                          ymd : function (..., quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                            ymd_h : function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                              ymd_hm : function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                                ymd_hms : function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)  
                                                  yq : function (..., quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))  
                                                    >                                                                                                                                                                                                                                                                                                                                  
library(MASS)
.Depends :  chr [1:4] "grDevices" "graphics" "stats" "utils"
abbey :  num [1:31] 5.2 6.5 6.9 7 7 7 7.4 8 8 8 ...
accdeaths :  Time-Series [1:72] from 1973 to 1979: 9007 8106 8928 9137 10017 ...
addterm : function (object, ...)  
  Aids2 : 'data.frame':	2843 obs. of  7 variables:
  $ state  : Factor w/ 4 levels "NSW","Other",..: 1 1 1 1 1 1 1 1 1 1 ...
$ sex    : Factor w/ 2 levels "F","M": 2 2 2 2 2 2 2 2 2 2 ...
$ diag   : int  10905 11029 9551 9577 10015 9971 10746 10042 10464 10439 ...
$ death  : int  11081 11096 9983 9654 10290 10344 11135 11069 10956 10873 ...
$ status : Factor w/ 2 levels "A","D": 2 2 2 2 2 2 2 2 2 2 ...
$ T.categ: Factor w/ 8 levels "hs","hsid","id",..: 1 1 1 5 1 1 8 1 1 2 ...
$ age    : int  35 53 42 44 39 36 36 31 26 27 ...
Animals : 'data.frame':	28 obs. of  2 variables:
  $ body : num  1.35 465 36.33 27.66 1.04 ...
$ brain: num  8.1 423 119.5 115 5.5 ...
anorexia : 'data.frame':	72 obs. of  3 variables:
  $ Treat : Factor w/ 3 levels "CBT","Cont","FT": 2 2 2 2 2 2 2 2 2 2 ...
$ Prewt : num  80.7 89.4 91.8 74 78.1 88.3 87.3 75.1 80.6 78.4 ...
$ Postwt: num  80.2 80.1 86.4 86.3 76.1 78.1 75.1 86.7 73.5 84.6 ...
area : function (f, a, b, ..., fa = f(a, ...), fb = f(b, ...), limit = 10, eps = 1e-05)  
  as.fractions : function (x)  
    bacteria : 'data.frame':	220 obs. of  6 variables:
  $ y   : Factor w/ 2 levels "n","y": 2 2 2 2 2 2 1 2 2 2 ...
$ ap  : Factor w/ 2 levels "a","p": 2 2 2 2 1 1 1 1 1 1 ...
$ hilo: Factor w/ 2 levels "hi","lo": 1 1 1 1 1 1 1 1 2 2 ...
$ week: int  0 2 4 11 0 2 6 11 0 2 ...
$ ID  : Factor w/ 50 levels "X01","X02","X03",..: 1 1 1 1 2 2 2 2 3 3 ...
$ trt : Factor w/ 3 levels "placebo","drug",..: 1 1 1 1 3 3 3 3 2 2 ...
bandwidth.nrd : function (x)  
  bcv : function (x, nb = 1000, lower = 0.1 * hmax, upper = hmax)  
    beav1 : 'data.frame':	114 obs. of  4 variables:
  $ day  : int  346 346 346 346 346 346 346 346 346 346 ...
$ time : int  840 850 900 910 920 930 940 950 1000 1010 ...
$ temp : num  36.3 36.3 36.4 36.4 36.5 ...
$ activ: int  0 0 0 0 0 0 0 0 0 0 ...
beav2 : 'data.frame':	100 obs. of  4 variables:
  $ day  : int  307 307 307 307 307 307 307 307 307 307 ...
$ time : int  930 940 950 1000 1010 1020 1030 1040 1050 1100 ...
$ temp : num  36.6 36.7 36.9 37.1 37.2 ...
$ activ: int  0 0 0 0 0 0 0 0 0 0 ...
biopsy : 'data.frame':	699 obs. of  11 variables:
  $ ID   : chr  "1000025" "1002945" "1015425" "1016277" ...
$ V1   : int  5 5 3 6 4 8 1 2 2 4 ...
$ V2   : int  1 4 1 8 1 10 1 1 1 2 ...
$ V3   : int  1 4 1 8 1 10 1 2 1 1 ...
$ V4   : int  1 5 1 1 3 8 1 1 1 1 ...
$ V5   : int  2 7 2 3 2 7 2 2 2 2 ...
$ V6   : int  1 10 2 4 1 10 10 1 1 1 ...
$ V7   : int  3 3 3 3 3 9 3 3 1 2 ...
$ V8   : int  1 2 1 7 1 7 1 1 1 1 ...
$ V9   : int  1 1 1 1 1 1 1 1 5 1 ...
$ class: Factor w/ 2 levels "benign","malignant": 1 1 1 1 1 2 1 1 1 1 ...
birthwt : 'data.frame':	189 obs. of  10 variables:
  $ low  : int  0 0 0 0 0 0 0 0 0 0 ...
$ age  : int  19 33 20 21 18 21 22 17 29 26 ...
$ lwt  : int  182 155 105 108 107 124 118 103 123 113 ...
$ race : int  2 3 1 1 1 3 1 3 1 1 ...
$ smoke: int  0 0 1 1 1 0 0 0 1 1 ...
$ ptl  : int  0 0 0 0 0 0 0 0 0 0 ...
$ ht   : int  0 0 0 0 0 0 0 0 0 0 ...
$ ui   : int  1 0 0 1 1 0 0 0 0 0 ...
$ ftv  : int  0 3 1 2 0 0 1 1 1 0 ...
$ bwt  : int  2523 2551 2557 2594 2600 2622 2637 2637 2663 2665 ...
Boston : 'data.frame':	506 obs. of  14 variables:
  $ crim   : num  0.00632 0.02731 0.02729 0.03237 0.06905 ...
$ zn     : num  18 0 0 0 0 0 12.5 12.5 12.5 12.5 ...
$ indus  : num  2.31 7.07 7.07 2.18 2.18 2.18 7.87 7.87 7.87 7.87 ...
$ chas   : int  0 0 0 0 0 0 0 0 0 0 ...
$ nox    : num  0.538 0.469 0.469 0.458 0.458 0.458 0.524 0.524 0.524 0.524 ...
$ rm     : num  6.58 6.42 7.18 7 7.15 ...
$ age    : num  65.2 78.9 61.1 45.8 54.2 58.7 66.6 96.1 100 85.9 ...
$ dis    : num  4.09 4.97 4.97 6.06 6.06 ...
$ rad    : int  1 2 2 3 3 3 5 5 5 5 ...
$ tax    : num  296 242 242 222 222 222 311 311 311 311 ...
$ ptratio: num  15.3 17.8 17.8 18.7 18.7 18.7 15.2 15.2 15.2 15.2 ...
$ black  : num  397 397 393 395 397 ...
$ lstat  : num  4.98 9.14 4.03 2.94 5.33 ...
$ medv   : num  24 21.6 34.7 33.4 36.2 28.7 22.9 27.1 16.5 18.9 ...
boxcox : function (object, ...)  
  cabbages : 'data.frame':	60 obs. of  4 variables:
  $ Cult  : Factor w/ 2 levels "c39","c52": 1 1 1 1 1 1 1 1 1 1 ...
$ Date  : Factor w/ 3 levels "d16","d20","d21": 1 1 1 1 1 1 1 1 1 1 ...
$ HeadWt: num  2.5 2.2 3.1 4.3 2.5 4.3 3.8 4.3 1.7 3.1 ...
$ VitC  : int  51 55 45 42 53 50 50 52 56 49 ...
caith : 'data.frame':	4 obs. of  5 variables:
  $ fair  : int  326 688 343 98
$ red   : int  38 116 84 48
$ medium: int  241 584 909 403
$ dark  : int  110 188 412 681
$ black : int  3 4 26 85
Cars93 : 'data.frame':	93 obs. of  27 variables:
  $ Manufacturer      : Factor w/ 32 levels "Acura","Audi",..: 1 1 2 2 3 4 4 4 4 5 ...
$ Model             : Factor w/ 93 levels "100","190E","240",..: 49 56 9 1 6 24 54 74 73 35 ...
$ Type              : Factor w/ 6 levels "Compact","Large",..: 4 3 1 3 3 3 2 2 3 2 ...
$ Min.Price         : num  12.9 29.2 25.9 30.8 23.7 14.2 19.9 22.6 26.3 33 ...
$ Price             : num  15.9 33.9 29.1 37.7 30 15.7 20.8 23.7 26.3 34.7 ...
$ Max.Price         : num  18.8 38.7 32.3 44.6 36.2 17.3 21.7 24.9 26.3 36.3 ...
$ MPG.city          : int  25 18 20 19 22 22 19 16 19 16 ...
$ MPG.highway       : int  31 25 26 26 30 31 28 25 27 25 ...
$ AirBags           : Factor w/ 3 levels "Driver & Passenger",..: 3 1 2 1 2 2 2 2 2 2 ...
$ DriveTrain        : Factor w/ 3 levels "4WD","Front",..: 2 2 2 2 3 2 2 3 2 2 ...
$ Cylinders         : Factor w/ 6 levels "3","4","5","6",..: 2 4 4 4 2 2 4 4 4 5 ...
$ EngineSize        : num  1.8 3.2 2.8 2.8 3.5 2.2 3.8 5.7 3.8 4.9 ...
$ Horsepower        : int  140 200 172 172 208 110 170 180 170 200 ...
$ RPM               : int  6300 5500 5500 5500 5700 5200 4800 4000 4800 4100 ...
$ Rev.per.mile      : int  2890 2335 2280 2535 2545 2565 1570 1320 1690 1510 ...
$ Man.trans.avail   : Factor w/ 2 levels "No","Yes": 2 2 2 2 2 1 1 1 1 1 ...
$ Fuel.tank.capacity: num  13.2 18 16.9 21.1 21.1 16.4 18 23 18.8 18 ...
$ Passengers        : int  5 5 5 6 4 6 6 6 5 6 ...
$ Length            : int  177 195 180 193 186 189 200 216 198 206 ...
$ Wheelbase         : int  102 115 102 106 109 105 111 116 108 114 ...
$ Width             : int  68 71 67 70 69 69 74 78 73 73 ...
$ Turn.circle       : int  37 38 37 37 39 41 42 45 41 43 ...
$ Rear.seat.room    : num  26.5 30 28 31 27 28 30.5 30.5 26.5 35 ...
$ Luggage.room      : int  11 15 14 17 13 16 17 21 14 18 ...
$ Weight            : int  2705 3560 3375 3405 3640 2880 3470 4105 3495 3620 ...
$ Origin            : Factor w/ 2 levels "USA","non-USA": 2 2 2 2 2 1 1 1 1 1 ...
$ Make              : Factor w/ 93 levels "Acura Integra",..: 1 2 4 3 5 6 7 9 8 10 ...
cats : 'data.frame':	144 obs. of  3 variables:
  $ Sex: Factor w/ 2 levels "F","M": 1 1 1 1 1 1 1 1 1 1 ...
$ Bwt: num  2 2 2 2.1 2.1 2.1 2.1 2.1 2.1 2.1 ...
$ Hwt: num  7 7.4 9.5 7.2 7.3 7.6 8.1 8.2 8.3 8.5 ...
cement : 'data.frame':	13 obs. of  5 variables:
  $ x1: int  7 1 11 11 7 11 3 1 2 21 ...
$ x2: int  26 29 56 31 52 55 71 31 54 47 ...
$ x3: int  6 15 8 8 6 9 17 22 18 4 ...
$ x4: int  60 52 20 47 33 22 6 44 22 26 ...
$ y : num  78.5 74.3 104.3 87.6 95.9 ...
chem :  num [1:24] 2.9 3.1 3.4 3.4 3.7 3.7 2.8 2.5 2.4 2.4 ...
con2tr : function (obj)  
  contr.sdif : function (n, contrasts = TRUE, sparse = FALSE)  
    coop : 'data.frame':	252 obs. of  4 variables:
  $ Lab : Factor w/ 6 levels "L1","L2","L3",..: 1 1 1 1 1 1 1 1 1 1 ...
$ Spc : Factor w/ 7 levels "S1","S2","S3",..: 1 1 1 1 1 1 2 2 2 2 ...
$ Bat : Factor w/ 3 levels "B1","B2","B3": 1 1 2 2 3 3 1 1 2 2 ...
$ Conc: num  0.29 0.33 0.33 0.32 0.34 0.31 0.13 0.14 0.16 0.11 ...
corresp : function (x, ...)  
  cov.mcd : function (...)  
    cov.mve : function (...)  
      cov.rob : function (x, cor = FALSE, quantile.used = floor((n + p + 1)/2), method = c("mve", 
                                                                                           "mcd", "classical"), nsamp = "best", seed)  
        cov.trob : function (x, wt = rep(1, n), cor = FALSE, center = TRUE, nu = 5, maxit = 25, tol = 0.01)  
          cpus : 'data.frame':	209 obs. of  9 variables:
  $ name   : Factor w/ 209 levels "ADVISOR 32/60",..: 1 3 2 4 5 6 8 9 10 7 ...
$ syct   : int  125 29 29 29 29 26 23 23 23 23 ...
$ mmin   : int  256 8000 8000 8000 8000 8000 16000 16000 16000 32000 ...
$ mmax   : int  6000 32000 32000 32000 16000 32000 32000 32000 64000 64000 ...
$ cach   : int  256 32 32 32 32 64 64 64 64 128 ...
$ chmin  : int  16 8 8 8 8 8 16 16 16 32 ...
$ chmax  : int  128 32 32 32 16 32 32 32 32 64 ...
$ perf   : int  198 269 220 172 132 318 367 489 636 1144 ...
$ estperf: int  199 253 253 253 132 290 381 381 749 1238 ...
crabs : 'data.frame':	200 obs. of  8 variables:
  $ sp   : Factor w/ 2 levels "B","O": 1 1 1 1 1 1 1 1 1 1 ...
$ sex  : Factor w/ 2 levels "F","M": 2 2 2 2 2 2 2 2 2 2 ...
$ index: int  1 2 3 4 5 6 7 8 9 10 ...
$ FL   : num  8.1 8.8 9.2 9.6 9.8 10.8 11.1 11.6 11.8 11.8 ...
$ RW   : num  6.7 7.7 7.8 7.9 8 9 9.9 9.1 9.6 10.5 ...
$ CL   : num  16.1 18.1 19 20.1 20.3 23 23.8 24.5 24.2 25.2 ...
$ CW   : num  19 20.8 22.4 23.1 23 26.5 27.1 28.4 27.8 29.3 ...
$ BD   : num  7 7.4 7.7 8.2 8.2 9.8 9.8 10.4 9.7 10.3 ...
Cushings : 'data.frame':	27 obs. of  3 variables:
  $ Tetrahydrocortisone: num  3.1 3 1.9 3.8 4.1 1.9 8.3 3.8 3.9 7.8 ...
$ Pregnanetriol      : num  11.7 1.3 0.1 0.04 1.1 0.4 1 0.2 0.6 1.2 ...
$ Type               : Factor w/ 4 levels "a","b","c","u": 1 1 1 1 1 1 2 2 2 2 ...
DDT :  num [1:15] 2.79 2.93 3.22 3.78 3.22 3.38 3.18 3.33 3.34 3.06 ...
deaths :  Time-Series [1:72] from 1974 to 1980: 3035 2552 2704 2554 2014 ...
denumerate : function (x)  
  dose.p : function (obj, cf = 1:2, p = 0.5)  
    drivers :  Time-Series [1:192] from 1969 to 1985: 1687 1508 1507 1385 1632 ...
dropterm : function (object, ...)  
  eagles : 'data.frame':	8 obs. of  5 variables:
  $ y: int  17 29 17 20 1 15 0 1
$ n: int  24 29 27 20 12 16 28 4
$ P: Factor w/ 2 levels "L","S": 1 1 1 1 2 2 2 2
$ A: Factor w/ 2 levels "A","I": 1 1 2 2 1 1 2 2
$ V: Factor w/ 2 levels "L","S": 1 2 1 2 1 2 1 2
enlist : function (vec)  
  epil : 'data.frame':	236 obs. of  9 variables:
  $ y      : int  5 3 3 3 3 5 3 3 2 4 ...
$ trt    : Factor w/ 2 levels "placebo","progabide": 1 1 1 1 1 1 1 1 1 1 ...
$ base   : int  11 11 11 11 11 11 11 11 6 6 ...
$ age    : int  31 31 31 31 30 30 30 30 25 25 ...
$ V4     : int  0 0 0 1 0 0 0 1 0 0 ...
$ subject: int  1 1 1 1 2 2 2 2 3 3 ...
$ period : int  1 2 3 4 1 2 3 4 1 2 ...
$ lbase  : num  -0.756 -0.756 -0.756 -0.756 -0.756 ...
$ lage   : num  0.1142 0.1142 0.1142 0.1142 0.0814 ...
eqscplot : function (x, y, ratio = 1, tol = 0.04, uin, ...)  
  farms : 'data.frame':	20 obs. of  4 variables:
  $ Mois  : Factor w/ 4 levels "M1","M2","M4",..: 1 1 2 2 1 1 1 4 3 2 ...
$ Manag : Factor w/ 4 levels "BF","HF","NM",..: 4 1 4 4 2 2 2 2 2 1 ...
$ Use   : Factor w/ 3 levels "U1","U2","U3": 2 2 2 2 1 2 3 3 1 1 ...
$ Manure: Factor w/ 5 levels "C0","C1","C2",..: 5 3 5 5 3 3 4 4 2 2 ...
fbeta : function (x, alpha, beta)  
  fgl : 'data.frame':	214 obs. of  10 variables:
  $ RI  : num  3.01 -0.39 -1.82 -0.34 -0.58 ...
$ Na  : num  13.6 13.9 13.5 13.2 13.3 ...
$ Mg  : num  4.49 3.6 3.55 3.69 3.62 3.61 3.6 3.61 3.58 3.6 ...
$ Al  : num  1.1 1.36 1.54 1.29 1.24 1.62 1.14 1.05 1.37 1.36 ...
$ Si  : num  71.8 72.7 73 72.6 73.1 ...
$ K   : num  0.06 0.48 0.39 0.57 0.55 0.64 0.58 0.57 0.56 0.57 ...
$ Ca  : num  8.75 7.83 7.78 8.22 8.07 8.07 8.17 8.24 8.3 8.4 ...
$ Ba  : num  0 0 0 0 0 0 0 0 0 0 ...
$ Fe  : num  0 0 0 0 0 0.26 0 0 0 0.11 ...
$ type: Factor w/ 6 levels "WinF","WinNF",..: 1 1 1 1 1 1 1 1 1 1 ...
fitdistr : function (x, densfun, start, ...)  
  forbes : 'data.frame':	17 obs. of  2 variables:
  $ bp  : num  194 194 198 198 199 ...
$ pres: num  20.8 20.8 22.4 22.7 23.1 ...
fractions : function (x, cycles = 10, max.denominator = 2000, ...)  
  frequency.polygon : function (x, nclass = nclass.freq(x), xlab = "", ylab = "", ...)  
    GAGurine : 'data.frame':	314 obs. of  2 variables:
  $ Age: num  0 0 0 0 0.01 0.01 0.01 0.01 0.01 0.01 ...
$ GAG: num  23 23.8 16.9 18.6 17.9 25.9 16.5 26.3 26.9 17.9 ...
galaxies :  num [1:82] 9172 9350 9483 9558 9775 ...
gamma.dispersion : function (object, ...)  
  gamma.shape : function (object, ...)  
    gehan : 'data.frame':	42 obs. of  4 variables:
  $ pair : int  1 1 2 2 3 3 4 4 5 5 ...
$ time : int  1 10 22 7 3 32 12 23 8 22 ...
$ cens : int  1 1 1 1 1 0 1 1 1 1 ...
$ treat: Factor w/ 2 levels "6-MP","control": 2 1 2 1 2 1 2 1 2 1 ...
genotype : 'data.frame':	61 obs. of  3 variables:
  $ Litter: Factor w/ 4 levels "A","B","I","J": 1 1 1 1 1 1 1 1 1 1 ...
$ Mother: Factor w/ 4 levels "A","B","I","J": 1 1 1 1 1 2 2 2 3 3 ...
$ Wt    : num  61.5 68.2 64 65 59.7 55 42 60.2 52.5 61.8 ...
geyser : 'data.frame':	299 obs. of  2 variables:
  $ waiting : num  80 71 57 80 75 77 60 86 77 56 ...
$ duration: num  4.02 2.15 4 4 4 ...
gilgais : 'data.frame':	365 obs. of  9 variables:
  $ pH00: num  7 6.7 7.8 8.9 7 8.5 7 7.4 7.4 7.2 ...
$ pH30: num  9.4 9.2 9.3 8.4 8.7 8.1 9 8.4 8.7 8.9 ...
$ pH80: num  7.9 9.2 8 7.8 8.5 8.2 8 8.2 8.1 8.5 ...
$ e00 : int  20 12 11 55 20 90 11 10 23 15 ...
$ e30 : int  37 27 44 290 150 350 44 50 110 89 ...
$ e80 : int  370 80 350 460 270 360 340 270 270 220 ...
$ c00 : int  60 45 20 480 180 1350 55 20 250 75 ...
$ c30 : int  60 38 155 2885 1500 2350 300 550 1225 790 ...
$ c80 : int  505 450 1325 1900 3200 2435 1240 1400 2425 1650 ...
ginv : function (X, tol = sqrt(.Machine$double.eps))  
  glm.convert : function (object)  
    glm.nb : function (formula, data, weights, subset, na.action, start = NULL, etastart, mustart, 
                       control = glm.control(...), method = "glm.fit", model = TRUE, x = FALSE, y = TRUE, 
                       contrasts = NULL, ..., init.theta, link = log)  
      glmmPQL : function (fixed, random, family, data, correlation, weights, control, niter = 10, 
                          verbose = TRUE, ...)  
        hills : 'data.frame':	35 obs. of  3 variables:
  $ dist : num  2.5 6 6 7.5 8 8 16 6 5 6 ...
$ climb: int  650 2500 900 800 3070 2866 7500 800 800 650 ...
$ time : num  16.1 48.4 33.6 45.6 62.3 ...
hist.FD : function (x, prob = TRUE, xlab = deparse(substitute(x)), ...)  
  hist.scott : function (x, prob = TRUE, xlab = deparse(substitute(x)), ...)  
    housing : 'data.frame':	72 obs. of  5 variables:
  $ Sat : Ord.factor w/ 3 levels "Low"<"Medium"<..: 1 2 3 1 2 3 1 2 3 1 ...
$ Infl: Factor w/ 3 levels "Low","Medium",..: 1 1 1 2 2 2 3 3 3 1 ...
$ Type: Factor w/ 4 levels "Tower","Apartment",..: 1 1 1 1 1 1 1 1 1 2 ...
$ Cont: Factor w/ 2 levels "Low","High": 1 1 1 1 1 1 1 1 1 1 ...
$ Freq: int  21 21 28 34 22 36 10 11 36 61 ...
huber : function (y, k = 1.5, tol = 1e-06)  
  hubers : function (y, k = 1.5, mu, s, initmu = median(y), tol = 1e-06)  
    immer : 'data.frame':	30 obs. of  4 variables:
  $ Loc: Factor w/ 6 levels "C","D","GR","M",..: 5 5 5 5 5 6 6 6 6 6 ...
$ Var: Factor w/ 5 levels "M","P","S","T",..: 1 3 5 4 2 1 3 5 4 2 ...
$ Y1 : num  81 105.4 119.7 109.7 98.3 ...
$ Y2 : num  80.7 82.3 80.4 87.2 84.2 ...
Insurance : 'data.frame':	64 obs. of  5 variables:
  $ District: Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 1 ...
$ Group   : Ord.factor w/ 4 levels "<1l"<"1-1.5l"<..: 1 1 1 1 2 2 2 2 3 3 ...
$ Age     : Ord.factor w/ 4 levels "<25"<"25-29"<..: 1 2 3 4 1 2 3 4 1 2 ...
$ Holders : int  197 264 246 1680 284 536 696 3582 133 286 ...
$ Claims  : int  38 35 20 156 63 84 89 400 19 52 ...
is.fractions : function (f)  
  isoMDS : function (d, y = cmdscale(d, k), k = 2, maxit = 50, trace = TRUE, tol = 0.001, p = 2)  
    kde2d : function (x, y, h, n = 25, lims = c(range(x), range(y)))  
      lda : function (x, ...)  
        ldahist : function (data, g, nbins = 25, h, x0 = -h/1000, breaks, xlim = range(breaks), ymax = 0, 
                            width, type = c("histogram", "density", "both"), sep = (type != "density"), col = 5L, 
                            xlab = deparse(substitute(data)), bty = "n", ...)  
          leuk : 'data.frame':	33 obs. of  3 variables:
  $ wbc : int  2300 750 4300 2600 6000 10500 10000 17000 5400 7000 ...
$ ag  : Factor w/ 2 levels "absent","present": 2 2 2 2 2 2 2 2 2 2 ...
$ time: int  65 156 100 134 16 108 121 4 39 143 ...
lm.gls : function (formula, data, W, subset, na.action, inverse = FALSE, method = "qr", model = FALSE, 
                   x = FALSE, y = FALSE, contrasts = NULL, ...)  
  lm.ridge : function (formula, data, subset, na.action, lambda = 0, model = FALSE, x = FALSE, 
                       y = FALSE, contrasts = NULL, ...)  
    lmsreg : function (...)  
      lmwork : function (object)  
        loglm : function (formula, data, subset, na.action, ...)  
          loglm1 : function (formula, data, ...)  
            logtrans : function (object, ...)  
              lqs : function (x, ...)  
                lqs.formula : function (formula, data, ..., method = c("lts", "lqs", "lms", "S", "model.frame"), 
                                        subset, na.action, model = TRUE, x.ret = FALSE, y.ret = FALSE, contrasts = NULL)  
                  ltsreg : function (...)  
                    mammals : 'data.frame':	62 obs. of  2 variables:
  $ body : num  3.38 0.48 1.35 465 36.33 ...
$ brain: num  44.5 15.5 8.1 423 119.5 ...
mca : function (df, nf = 2, abbrev = FALSE)  
  mcycle : 'data.frame':	133 obs. of  2 variables:
  $ times: num  2.4 2.6 3.2 3.6 4 6.2 6.6 6.8 7.8 8.2 ...
$ accel: num  0 -1.3 -2.7 0 -2.7 -2.7 -2.7 -1.3 -2.7 -2.7 ...
Melanoma : 'data.frame':	205 obs. of  7 variables:
  $ time     : int  10 30 35 99 185 204 210 232 232 279 ...
$ status   : int  3 3 2 3 1 1 1 3 1 1 ...
$ sex      : int  1 1 1 0 1 1 1 0 1 0 ...
$ age      : int  76 56 41 71 52 28 77 60 49 68 ...
$ year     : int  1972 1968 1977 1968 1965 1971 1972 1974 1968 1971 ...
$ thickness: num  6.76 0.65 1.34 2.9 12.08 ...
$ ulcer    : int  1 0 0 0 1 1 1 1 1 1 ...
menarche : 'data.frame':	25 obs. of  3 variables:
  $ Age     : num  9.21 10.21 10.58 10.83 11.08 ...
$ Total   : num  376 200 93 120 90 88 105 111 100 93 ...
$ Menarche: num  0 0 0 2 2 5 10 17 16 29 ...
michelson : 'data.frame':	100 obs. of  3 variables:
  $ Speed: int  850 740 900 1070 930 850 950 980 980 880 ...
$ Run  : Factor w/ 20 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
$ Expt : Factor w/ 5 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
minn38 : 'data.frame':	168 obs. of  5 variables:
  $ hs : Factor w/ 3 levels "L","M","U": 1 1 1 1 1 1 1 1 1 1 ...
$ phs: Factor w/ 4 levels "C","E","N","O": 1 1 1 1 1 1 1 3 3 3 ...
$ fol: Factor w/ 7 levels "F1","F2","F3",..: 1 2 3 4 5 6 7 1 2 3 ...
$ sex: Factor w/ 2 levels "F","M": 2 2 2 2 2 2 2 2 2 2 ...
$ f  : int  87 72 52 88 32 14 20 3 6 17 ...
motors : 'data.frame':	40 obs. of  3 variables:
  $ temp: int  150 150 150 150 150 150 150 150 150 150 ...
$ time: int  8064 8064 8064 8064 8064 8064 8064 8064 8064 8064 ...
$ cens: int  0 0 0 0 0 0 0 0 0 0 ...
muscle : 'data.frame':	60 obs. of  3 variables:
  $ Strip : Factor w/ 21 levels "S01","S02","S03",..: 1 1 1 1 2 2 2 2 3 3 ...
$ Conc  : num  1 2 3 4 1 2 3 4 0.25 0.5 ...
$ Length: num  15.8 20.8 22.6 23.8 20.6 26.8 28.4 27 7.2 15.4 ...
mvrnorm : function (n = 1, mu, Sigma, tol = 1e-06, empirical = FALSE, EISPACK = FALSE)  
  nclass.freq : function (x)  
    neg.bin : function (theta = stop("'theta' must be given"))  
      negative.binomial : function (theta = stop("'theta' must be specified"), link = "log")  
        negexp.SSival : function (mCall, data, LHS)  
          newcomb :  num [1:66] 28 -44 29 30 24 28 37 32 36 27 ...
nlschools : 'data.frame':	2287 obs. of  6 variables:
  $ lang : int  46 45 33 46 20 30 30 57 36 36 ...
$ IQ   : num  15 14.5 9.5 11 8 9.5 9.5 13 9.5 11 ...
$ class: Factor w/ 133 levels "180","280","1082",..: 1 1 1 1 1 1 1 1 1 1 ...
$ GS   : int  29 29 29 29 29 29 29 29 29 29 ...
$ SES  : int  23 10 15 23 10 10 23 10 13 15 ...
$ COMB : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
npk : 'data.frame':	24 obs. of  5 variables:
  $ block: Factor w/ 6 levels "1","2","3","4",..: 1 1 1 1 2 2 2 2 3 3 ...
$ N    : Factor w/ 2 levels "0","1": 1 2 1 2 2 2 1 1 1 2 ...
$ P    : Factor w/ 2 levels "0","1": 2 2 1 1 1 2 1 2 2 2 ...
$ K    : Factor w/ 2 levels "0","1": 2 1 1 2 1 2 2 1 1 2 ...
$ yield: num  49.5 62.8 46.8 57 59.8 58.5 55.5 56 62.8 55.8 ...
npr1 : 'data.frame':	104 obs. of  4 variables:
  $ x   : num  8 13.1 13.9 13.4 13.4 ...
$ y   : num  2.38 2.5 3.13 2.63 2.13 2.13 2.5 2.25 3 2.13 ...
$ perm: int  327 3369 4770 938 568 667 2561 2538 1078 1078 ...
$ por : int  33 34 40 35 32 34 32 34 36 34 ...
Null : function (M)  
  oats : 'data.frame':	72 obs. of  4 variables:
  $ B: Factor w/ 6 levels "I","II","III",..: 1 1 1 1 1 1 1 1 1 1 ...
$ V: Factor w/ 3 levels "Golden.rain",..: 3 3 3 3 1 1 1 1 2 2 ...
$ N: Factor w/ 4 levels "0.0cwt","0.2cwt",..: 1 2 3 4 1 2 3 4 1 2 ...
$ Y: int  111 130 157 174 117 114 161 141 105 140 ...
OME : 'data.frame':	1097 obs. of  7 variables:
  $ ID     : int  1 1 1 1 1 1 1 1 1 1 ...
$ Age    : int  30 30 30 30 30 30 30 30 30 30 ...
$ OME    : Factor w/ 3 levels "N/A","high","low": 3 3 3 3 3 3 3 3 3 3 ...
$ Loud   : int  35 35 40 40 45 45 50 50 55 55 ...
$ Noise  : Factor w/ 2 levels "coherent","incoherent": 1 2 1 2 1 2 1 2 1 2 ...
$ Correct: int  1 4 0 1 2 2 3 4 3 2 ...
$ Trials : int  4 5 3 1 4 2 3 4 3 2 ...
painters : 'data.frame':	54 obs. of  5 variables:
  $ Composition: int  10 15 8 12 0 15 8 15 4 17 ...
$ Drawing    : int  8 16 13 16 15 16 17 16 12 18 ...
$ Colour     : int  16 4 16 9 8 4 4 7 10 12 ...
$ Expression : int  3 14 7 8 0 14 8 6 4 18 ...
$ School     : Factor w/ 8 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
parcoord : function (x, col = 1, lty = 1, var.label = FALSE, ...)  
  petrol : 'data.frame':	32 obs. of  6 variables:
  $ No : Factor w/ 10 levels "A","B","C","D",..: 1 1 1 1 2 2 2 3 3 3 ...
$ SG : num  50.8 50.8 50.8 50.8 40.8 40.8 40.8 40 40 40 ...
$ VP : num  8.6 8.6 8.6 8.6 3.5 3.5 3.5 6.1 6.1 6.1 ...
$ V10: int  190 190 190 190 210 210 210 217 217 217 ...
$ EP : int  205 275 345 407 218 273 347 212 272 340 ...
$ Y  : num  12.2 22.3 34.7 45.7 8 13.1 26.6 7.4 18.2 30.4 ...
phones : List of 2
$ year : num [1:24] 50 51 52 53 54 55 56 57 58 59 ...
$ calls: num [1:24] 4.4 4.7 4.7 5.9 6.6 7.3 8.1 8.8 10.6 12 ...
Pima.te : 'data.frame':	332 obs. of  8 variables:
  $ npreg: int  6 1 1 3 2 5 0 1 3 9 ...
$ glu  : int  148 85 89 78 197 166 118 103 126 119 ...
$ bp   : int  72 66 66 50 70 72 84 30 88 80 ...
$ skin : int  35 29 23 32 45 19 47 38 41 35 ...
$ bmi  : num  33.6 26.6 28.1 31 30.5 25.8 45.8 43.3 39.3 29 ...
$ ped  : num  0.627 0.351 0.167 0.248 0.158 0.587 0.551 0.183 0.704 0.263 ...
$ age  : int  50 31 21 26 53 51 31 33 27 29 ...
$ type : Factor w/ 2 levels "No","Yes": 2 1 1 2 2 2 2 1 1 2 ...
Pima.tr : 'data.frame':	200 obs. of  8 variables:
  $ npreg: int  5 7 5 0 0 5 3 1 3 2 ...
$ glu  : int  86 195 77 165 107 97 83 193 142 128 ...
$ bp   : int  68 70 82 76 60 76 58 50 80 78 ...
$ skin : int  28 33 41 43 25 27 31 16 15 37 ...
$ bmi  : num  30.2 25.1 35.8 47.9 26.4 35.6 34.3 25.9 32.4 43.3 ...
$ ped  : num  0.364 0.163 0.156 0.259 0.133 ...
$ age  : int  24 55 35 26 23 52 25 24 63 31 ...
$ type : Factor w/ 2 levels "No","Yes": 1 2 1 1 1 2 1 1 1 2 ...
Pima.tr2 : 'data.frame':	300 obs. of  8 variables:
  $ npreg: int  5 7 5 0 0 5 3 1 3 2 ...
$ glu  : int  86 195 77 165 107 97 83 193 142 128 ...
$ bp   : int  68 70 82 76 60 76 58 50 80 78 ...
$ skin : int  28 33 41 43 25 27 31 16 15 37 ...
$ bmi  : num  30.2 25.1 35.8 47.9 26.4 35.6 34.3 25.9 32.4 43.3 ...
$ ped  : num  0.364 0.163 0.156 0.259 0.133 ...
$ age  : int  24 55 35 26 23 52 25 24 63 31 ...
$ type : Factor w/ 2 levels "No","Yes": 1 2 1 1 1 2 1 1 1 2 ...
polr : function (formula, data, weights, start, ..., subset, na.action, contrasts = NULL, 
                 Hess = FALSE, model = TRUE, method = c("logistic", "probit", "loglog", "cloglog", 
                                                        "cauchit"))  
  psi.bisquare : function (u, c = 4.685, deriv = 0)  
    psi.hampel : function (u, a = 2, b = 4, c = 8, deriv = 0)  
      psi.huber : function (u, k = 1.345, deriv = 0)  
        qda : function (x, ...)  
          quine : 'data.frame':	146 obs. of  5 variables:
  $ Eth : Factor w/ 2 levels "A","N": 1 1 1 1 1 1 1 1 1 1 ...
$ Sex : Factor w/ 2 levels "F","M": 2 2 2 2 2 2 2 2 2 2 ...
$ Age : Factor w/ 4 levels "F0","F1","F2",..: 1 1 1 1 1 1 1 1 2 2 ...
$ Lrn : Factor w/ 2 levels "AL","SL": 2 2 2 1 1 1 1 1 2 2 ...
$ Days: int  2 11 14 5 5 13 20 22 6 6 ...
Rabbit : 'data.frame':	60 obs. of  5 variables:
  $ BPchange : num  0.5 4.5 10 26 37 32 1 1.25 4 12 ...
$ Dose     : num  6.25 12.5 25 50 100 200 6.25 12.5 25 50 ...
$ Run      : Factor w/ 10 levels "C1","C2","C3",..: 1 1 1 1 1 1 2 2 2 2 ...
$ Treatment: Factor w/ 2 levels "Control","MDL": 1 1 1 1 1 1 1 1 1 1 ...
$ Animal   : Factor w/ 5 levels "R1","R2","R3",..: 1 1 1 1 1 1 2 2 2 2 ...
rational : function (x, cycles = 10, max.denominator = 2000, ...)  
  renumerate : function (x)  
    rlm : function (x, ...)  
      rms.curv : function (obj)  
        rnegbin : function (n, mu = n, theta = stop("'theta' must be specified"))  
          road : 'data.frame':	26 obs. of  6 variables:
  $ deaths : int  968 43 588 640 4743 566 325 118 115 1545 ...
$ drivers: int  158 11 91 92 952 109 167 30 35 298 ...
$ popden : num  64 0.4 12 34 100 ...
$ rural  : num  66 5.9 33 73 118 73 5.1 3.4 0 57 ...
$ temp   : int  62 30 64 51 65 42 37 41 44 67 ...
$ fuel   : num  119 6.2 65 74 105 78 95 20 23 216 ...
rotifer : 'data.frame':	20 obs. of  5 variables:
  $ density: num  1.02 1.02 1.02 1.03 1.03 ...
$ pm.y   : int  11 7 10 19 9 21 13 34 10 36 ...
$ pm.tot : int  58 86 76 83 56 73 29 44 31 56 ...
$ kc.y   : int  13 14 30 10 14 35 26 32 22 23 ...
$ kc.tot : int  161 248 234 283 129 161 167 286 117 162 ...
Rubber : 'data.frame':	30 obs. of  3 variables:
  $ loss: int  372 206 175 154 136 112 55 45 221 166 ...
$ hard: int  45 55 61 66 71 71 81 86 53 60 ...
$ tens: int  162 233 232 231 231 237 224 219 203 189 ...
sammon : function (d, y = cmdscale(d, k), k = 2, niter = 100, trace = TRUE, magic = 0.2, tol = 1e-04)  
  select : function (obj)  
    Shepard : function (d, x, p = 2)  
      ships : 'data.frame':	40 obs. of  5 variables:
  $ type     : Factor w/ 5 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 2 2 ...
$ year     : int  60 60 65 65 70 70 75 75 60 60 ...
$ period   : int  60 75 60 75 60 75 60 75 60 75 ...
$ service  : int  127 63 1095 1095 1512 3353 0 2244 44882 17176 ...
$ incidents: int  0 0 3 4 6 18 0 11 39 29 ...
shoes : List of 2
$ A: num [1:10] 13.2 8.2 10.9 14.3 10.7 6.6 9.5 10.8 8.8 13.3
$ B: num [1:10] 14 8.8 11.2 14.2 11.8 6.4 9.8 11.3 9.3 13.6
shrimp :  num [1:18] 32.2 33 30.8 33.8 32.2 33.3 31.7 35.7 32.4 31.2 ...
shuttle : 'data.frame':	256 obs. of  7 variables:
  $ stability: Factor w/ 2 levels "stab","xstab": 2 2 2 2 2 2 2 2 2 2 ...
$ error    : Factor w/ 4 levels "LX","MM","SS",..: 1 1 1 1 1 1 1 1 1 1 ...
$ sign     : Factor w/ 2 levels "nn","pp": 2 2 2 2 2 2 1 1 1 1 ...
$ wind     : Factor w/ 2 levels "head","tail": 1 1 1 2 2 2 1 1 1 2 ...
$ magn     : Factor w/ 4 levels "Light","Medium",..: 1 2 4 1 2 4 1 2 4 1 ...
$ vis      : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
$ use      : Factor w/ 2 levels "auto","noauto": 1 1 1 1 1 1 1 1 1 1 ...
Sitka : 'data.frame':	395 obs. of  4 variables:
  $ size : num  4.51 4.98 5.41 5.9 6.15 4.24 4.2 4.68 4.92 4.96 ...
$ Time : num  152 174 201 227 258 152 174 201 227 258 ...
$ tree : int  1 1 1 1 1 2 2 2 2 2 ...
$ treat: Factor w/ 2 levels "control","ozone": 2 2 2 2 2 2 2 2 2 2 ...
Sitka89 : 'data.frame':	632 obs. of  4 variables:
  $ size : num  6.16 6.18 6.48 6.65 6.87 6.95 6.99 7.04 5.2 5.22 ...
$ Time : num  469 496 528 556 579 613 639 674 469 496 ...
$ tree : int  1 1 1 1 1 1 1 1 2 2 ...
$ treat: Factor w/ 2 levels "control","ozone": 2 2 2 2 2 2 2 2 2 2 ...
Skye : 'data.frame':	23 obs. of  3 variables:
  $ A: int  52 52 47 45 40 37 27 27 23 22 ...
$ F: int  42 44 48 49 50 54 58 54 59 59 ...
$ M: int  6 4 5 6 10 9 15 19 18 19 ...
snails : 'data.frame':	96 obs. of  6 variables:
  $ Species : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
$ Exposure: int  1 1 1 1 1 1 1 1 1 1 ...
$ Rel.Hum : num  60 60 60 65.8 65.8 65.8 70.5 70.5 70.5 75.8 ...
$ Temp    : int  10 15 20 10 15 20 10 15 20 10 ...
$ Deaths  : int  0 0 0 0 0 0 0 0 0 0 ...
$ N       : int  20 20 20 20 20 20 20 20 20 20 ...
SP500 :  num [1:2780] -0.259 -0.865 -0.98 0.45 -1.186 ...
stdres : function (object)  
  steam : 'data.frame':	14 obs. of  2 variables:
  $ Temp : int  0 10 20 30 40 50 60 70 80 85 ...
$ Press: num  4.14 8.52 16.31 32.18 64.62 ...
stepAIC : function (object, scope, scale = 0, direction = c("both", "backward", "forward"), 
                    trace = 1, keep = NULL, steps = 1000, use.start = FALSE, k = 2, ...)  
  stormer : 'data.frame':	23 obs. of  3 variables:
  $ Viscosity: num  14.7 27.5 42 75.7 89.7 ...
$ Wt       : int  20 20 20 20 20 20 20 50 50 50 ...
$ Time     : num  35.6 54.3 75.6 121.2 150.8 ...
studres : function (object)  
  survey : 'data.frame':	237 obs. of  12 variables:
  $ Sex   : Factor w/ 2 levels "Female","Male": 1 2 2 2 2 1 2 1 2 2 ...
$ Wr.Hnd: num  18.5 19.5 18 18.8 20 18 17.7 17 20 18.5 ...
$ NW.Hnd: num  18 20.5 13.3 18.9 20 17.7 17.7 17.3 19.5 18.5 ...
$ W.Hnd : Factor w/ 2 levels "Left","Right": 2 1 2 2 2 2 2 2 2 2 ...
$ Fold  : Factor w/ 3 levels "L on R","Neither",..: 3 3 1 3 2 1 1 3 3 3 ...
$ Pulse : int  92 104 87 NA 35 64 83 74 72 90 ...
$ Clap  : Factor w/ 3 levels "Left","Neither",..: 1 1 2 2 3 3 3 3 3 3 ...
$ Exer  : Factor w/ 3 levels "Freq","None",..: 3 2 2 2 3 3 1 1 3 3 ...
$ Smoke : Factor w/ 4 levels "Heavy","Never",..: 2 4 3 2 2 2 2 2 2 2 ...
$ Height: num  173 178 NA 160 165 ...
$ M.I   : Factor w/ 2 levels "Imperial","Metric": 2 1 NA 2 2 1 1 2 2 2 ...
$ Age   : num  18.2 17.6 16.9 20.3 23.7 ...
synth.te : 'data.frame':	1000 obs. of  3 variables:
  $ xs: num  -0.971 -0.632 -0.774 -0.606 -0.539 ...
$ ys: num  0.429 0.252 0.691 0.176 0.377 ...
$ yc: int  0 0 0 0 0 0 0 0 0 0 ...
synth.tr : 'data.frame':	250 obs. of  3 variables:
  $ xs: num  0.051 -0.748 -0.773 0.218 0.373 ...
$ ys: num  0.161 0.089 0.263 0.127 0.497 ...
$ yc: int  0 0 0 0 0 0 0 0 0 0 ...
theta.md : function (y, mu, dfr, weights, limit = 20, eps = .Machine$double.eps^0.25)  
  theta.ml : function (y, mu, n = sum(weights), weights, limit = 10, eps = .Machine$double.eps^0.25, 
                       trace = FALSE)  
    theta.mm : function (y, mu, dfr, weights, limit = 10, eps = .Machine$double.eps^0.25)  
      topo : 'data.frame':	52 obs. of  3 variables:
  $ x: num  0.3 1.4 2.4 3.6 5.7 1.6 2.9 3.4 3.4 4.8 ...
$ y: num  6.1 6.2 6.1 6.2 6.2 5.2 5.1 5.3 5.7 5.6 ...
$ z: int  870 793 755 690 800 800 730 728 710 780 ...
Traffic : 'data.frame':	184 obs. of  4 variables:
  $ year : int  1961 1961 1961 1961 1961 1961 1961 1961 1961 1961 ...
$ day  : int  1 2 3 4 5 6 7 8 9 10 ...
$ limit: Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
$ y    : int  9 11 9 20 31 26 18 19 18 13 ...
truehist : function (data, nbins = "Scott", h, x0 = -h/1000, breaks, prob = TRUE, xlim = range(breaks), 
                     ymax = max(est), col = "cyan", xlab = deparse(substitute(data)), bty = "n", ...)  
  ucv : function (x, nb = 1000, lower = 0.1 * hmax, upper = hmax)  
    UScereal : 'data.frame':	65 obs. of  11 variables:
  $ mfr      : Factor w/ 6 levels "G","K","N","P",..: 3 2 2 1 2 1 6 4 5 1 ...
$ calories : num  212 212 100 147 110 ...
$ protein  : num  12.12 12.12 8 2.67 2 ...
$ fat      : num  3.03 3.03 0 2.67 0 ...
$ sodium   : num  394 788 280 240 125 ...
$ fibre    : num  30.3 27.3 28 2 1 ...
$ carbo    : num  15.2 21.2 16 14 11 ...
$ sugars   : num  18.2 15.2 0 13.3 14 ...
$ shelf    : int  3 3 3 1 2 3 1 3 2 1 ...
$ potassium: num  848.5 969.7 660 93.3 30 ...
$ vitamins : Factor w/ 3 levels "100%","enriched",..: 2 2 2 2 2 2 2 2 2 2 ...
UScrime : 'data.frame':	47 obs. of  16 variables:
  $ M   : int  151 143 142 136 141 121 127 131 157 140 ...
$ So  : int  1 0 1 0 0 0 1 1 1 0 ...
$ Ed  : int  91 113 89 121 121 110 111 109 90 118 ...
$ Po1 : int  58 103 45 149 109 118 82 115 65 71 ...
$ Po2 : int  56 95 44 141 101 115 79 109 62 68 ...
$ LF  : int  510 583 533 577 591 547 519 542 553 632 ...
$ M.F : int  950 1012 969 994 985 964 982 969 955 1029 ...
$ Pop : int  33 13 18 157 18 25 4 50 39 7 ...
$ NW  : int  301 102 219 80 30 44 139 179 286 15 ...
$ U1  : int  108 96 94 102 91 84 97 79 81 100 ...
$ U2  : int  41 36 33 39 20 29 38 35 28 24 ...
$ GDP : int  394 557 318 673 578 689 620 472 421 526 ...
$ Ineq: int  261 194 250 167 174 126 168 206 239 174 ...
$ Prob: num  0.0846 0.0296 0.0834 0.0158 0.0414 ...
$ Time: num  26.2 25.3 24.3 29.9 21.3 ...
$ y   : int  791 1635 578 1969 1234 682 963 1555 856 705 ...
VA : 'data.frame':	137 obs. of  8 variables:
  $ stime    : num  72 411 228 126 118 10 82 110 314 100 ...
$ status   : num  1 1 1 1 1 1 1 1 1 0 ...
$ treat    : Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...
$ age      : num  69 64 38 63 65 49 69 68 43 70 ...
$ Karn     : num  60 70 60 60 70 20 40 80 50 70 ...
$ diag.time: num  7 5 3 9 11 5 10 29 18 6 ...
$ cell     : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 1 ...
$ prior    : Factor w/ 2 levels "0","10": 1 2 1 2 2 1 2 1 1 1 ...
waders : 'data.frame':	15 obs. of  19 variables:
  $ S1 : int  12 99 197 0 77 19 1023 87 788 82 ...
$ S2 : int  2027 2112 160 17 1948 203 2655 745 2174 350 ...
$ S3 : int  0 9 0 0 0 48 0 1447 0 760 ...
$ S4 : int  0 87 4 3 19 45 18 125 19 197 ...
$ S5 : int  2070 3481 126 50 310 20 320 4330 224 858 ...
$ S6 : int  39 470 17 6 1 433 49 789 178 962 ...
$ S7 : int  219 2063 1 4 1 0 8 228 1 10 ...
$ S8 : int  153 28 32 7 64 0 121 529 423 511 ...
$ S9 : int  0 17 0 0 0 11 9 289 0 251 ...
$ S10: int  15 145 2 1 22 167 82 904 195 987 ...
$ S11: int  51 31 9 2 81 12 48 34 162 191 ...
$ S12: int  8336 1515 477 16 2792 1 3411 1710 2161 34 ...
$ S13: int  2031 1917 1 0 221 0 14 7869 25 87 ...
$ S14: int  14941 17321 548 0 7422 26 9101 2247 1784 417 ...
$ S15: int  19 3378 13 3 10 1790 43 4558 3 4496 ...
$ S16: int  3566 20164 273 69 4519 2916 3230 40880 1254 15835 ...
$ S17: int  0 177 0 1 12 473 587 7166 0 5327 ...
$ S18: int  5 1759 0 0 0 658 10 1632 0 1312 ...
$ S19: int  0 53 0 0 0 55 5 498 0 1020 ...
whiteside : 'data.frame':	56 obs. of  3 variables:
  $ Insul: Factor w/ 2 levels "Before","After": 1 1 1 1 1 1 1 1 1 1 ...
$ Temp : num  -0.8 -0.7 0.4 2.5 2.9 3.2 3.6 3.9 4.2 4.3 ...
$ Gas  : num  7.2 6.9 6.4 6 5.8 5.8 5.6 4.7 5.8 5.2 ...
width.SJ : function (x, nb = 1000, lower = 0.1 * hmax, upper = hmax, method = c("ste", "dpi"))  
  write.matrix : function (x, file = "", sep = " ", blocksize)  
    wtloss : 'data.frame':	52 obs. of  2 variables:
  $ Days  : int  0 4 7 7 11 18 24 30 32 43 ...
$ Weight: num  184 183 180 180 178 ...
> 
library(methods)
"x"
".__C__( : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__.environment : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__.externalptr : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__.name : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__.NULL : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__.Other : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__{ : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__<- : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__activeBindingFunction : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__anova : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__anova.glm : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__anova.glm.null : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__ANY : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__aov : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__array : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__builtin : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__call : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__character : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__classGeneratorFunction : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__className : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__classPrototypeDef : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__classRepresentation : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__ClassUnionRepresentation : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__complex : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__conditionalExtension : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__data.frame : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__data.frameRowLabels : Formal class 'ClassUnionRepresentation' [package \"methods\"] with 11 slots"
".__C__Date : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__defaultBindingFunction : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__density : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__derivedDefaultMethod : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__derivedDefaultMethodWithTrace : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__double : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__dump.frames : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__EmptyMethodsList : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__environment : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__envRefClass : Formal class 'refClassRepresentation' [package \"methods\"] with 15 slots"
".__C__expression : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__externalptr : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__externalRefMethod : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__factor : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__for : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__formula : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__function : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__functionWithTrace : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__genericFunction : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__genericFunctionWithTrace : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__glm : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__glm.null : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__groupGenericFunction : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__groupGenericFunctionWithTrace : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__hsearch : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__if : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__integer : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__integrate : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__internalDispatchMethod : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__language : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__libraryIQR : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__LinearMethodsList : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__list : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__listOfMethods : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__lm : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__localRefClass : Formal class 'refClassRepresentation' [package \"methods\"] with 15 slots"
".__C__logical : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__logLik : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__maov : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__matrix : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__MethodDefinition : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__MethodDefinitionWithTrace : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__MethodSelectionReport : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__MethodsList : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__MethodWithNext : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__MethodWithNextWithTrace : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__missing : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__mlm : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__mtable : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__mts : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__name : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__namedList : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__nonstandardGeneric : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__nonstandardGenericFunction : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__nonstandardGenericWithTrace : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__nonstandardGroupGenericFunction : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__nonStructure : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__NULL : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__numeric : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__ObjectsWithPackage : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__oldClass : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__OptionalFunction : Formal class 'ClassUnionRepresentation' [package \"methods\"] with 11 slots"
".__C__optionalMethod : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__ordered : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__packageInfo : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__packageIQR : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__POSIXct : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__POSIXlt : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__POSIXt : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__PossibleMethod : Formal class 'ClassUnionRepresentation' [package \"methods\"] with 11 slots"
".__C__raw : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__recordedplot : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__refClass : Formal class 'ClassUnionRepresentation' [package \"methods\"] with 11 slots"
".__C__refClassRepresentation : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__refGeneratorSlot : Formal class 'refClassRepresentation' [package \"methods\"] with 15 slots"
".__C__refMethodDef : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__refMethodDefWithTrace : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__refObject : Formal class 'ClassUnionRepresentation' [package \"methods\"] with 11 slots"
".__C__refObjectGenerator : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__repeat : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__rle : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__S3 : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__S4 : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__SClassExtension : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__SealedMethodDefinition : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__signature : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__socket : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__sourceEnvironment : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__special : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__standardGeneric : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__standardGenericWithTrace : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__structure : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__summary.table : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__summaryDefault : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__SuperClassMethod : Formal class 'ClassUnionRepresentation' [package \"methods\"] with 11 slots"
".__C__table : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__traceable : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__ts : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__uninitializedField : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__vector : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__VIRTUAL : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__C__while : Formal class 'classRepresentation' [package \"methods\"] with 11 slots"
".__T__$:base : <environment: 0x000001dbed9449c8> "
".__T__$<-:base : <environment: 0x000001dbed948dd8> "
".__T__[:base : <environment: 0x000001dbed94d208> "
".__T__[[<-:base : <environment: 0x000001dbed953200> "
".__T__[<-:base : <environment: 0x000001dbed958de8> "
".__T__addNextMethod:methods : <environment: 0x000001dbed95aab0> "
".__T__Arith:base : <environment: 0x000001dbed94baf0> "
".__T__body<-:base : <environment: 0x000001dbed95e670> "
".__T__cbind2:methods : <environment: 0x000001dbed950c20> "
".__T__coerce:methods : <environment: 0x000001dbed96c328> "
".__T__coerce<-:methods : <environment: 0x000001dbed970ea8> "
".__T__Compare:methods : <environment: 0x000001dbed96fa88> "
".__T__Complex:base : <environment: 0x000001dbed962508> "
".__T__initialize:methods : <environment: 0x000001dbed973078> "
".__T__kronecker:base : <environment: 0x000001dbed99a7a0> "
".__T__loadMethod:methods : <environment: 0x000001dbed99c698> "
".__T__Logic:base : <environment: 0x000001dbed99ec58> "
".__T__Math:base : <environment: 0x000001dbed99d838> "
".__T__Math2:methods : <environment: 0x000001dbed9a0a70> "
".__T__Ops:base : <environment: 0x000001dbed9abc28> "
".__T__rbind2:methods : <environment: 0x000001dbed9a4240> "
".__T__show:methods : <environment: 0x000001dbed9a9898> "
".__T__slotsFromS3:methods : <environment: 0x000001dbee568080> "
".__T__Summary:base : <environment: 0x000001dbee55ece0> "
".classEnv : function (Class, default = .requirePackage(\"methods\"), mustFind = TRUE)  "
".debugMethod : function (fun, text = \"\", condition = NULL, signature, once = FALSE)  "
".doTracePrint : function (msg = \"\")  "
".EmptyPrimitiveSkeletons : List of 2"
" $ : language f(x)"
" $ : language fgets(x, value = value)"
".hasSlot : function (object, name)  "
".isMethodDebugged : function (fun, signature)  "
".OldClassesList : List of 23"
" $ : chr [1:2] \"anova\" \"data.frame\""
" $ : chr [1:2] \"mlm\" \"lm\""
" $ : chr [1:2] \"aov\" \"lm\""
" $ : chr [1:3] \"maov\" \"mlm\" \"lm\""
" $ : chr [1:2] \"POSIXct\" \"POSIXt\""
" $ : chr [1:2] \"POSIXlt\" \"POSIXt\""
" $ : chr \"Date\""
" $ : chr \"dump.frames\""
" $ : chr [1:3] \"glm.null\" \"glm\" \"lm\""
" $ : chr [1:2] \"anova.glm.null\" \"anova.glm\""
" $ : chr \"hsearch\""
" $ : chr \"integrate\""
" $ : chr \"packageInfo\""
" $ : chr \"libraryIQR\""
" $ : chr \"packageIQR\""
" $ : chr \"mtable\""
" $ : chr [1:2] \"summaryDefault\" \"table\""
" $ : chr \"recordedplot\""
" $ : chr \"socket\""
" $ : chr \"packageIQR\""
" $ : chr \"density\""
" $ : chr \"logLik\""
" $ : chr \"rle\""
".S4methods : function (generic.function, class)  "
".selectSuperClasses : function (ext, dropVirtual = FALSE, namesOnly = TRUE, directOnly = TRUE, simpleOnly = directOnly)  "
".ShortPrimitiveSkeletons : List of 2"
" $ : language f(x, i)"
" $ : language fgets(x, i, value = value)"
".slotNames : function (x)  "
".TraceWithMethods : function (what, tracer = NULL, exit = NULL, at = numeric(), print = TRUE, signature = NULL, "
"    where = .GlobalEnv, edit = FALSE, from = NULL, untrace = FALSE, classMethod = FALSE)  "
".undebugMethod : function (fun, signature)  "
".untracedFunction : function (f)  "
".valueClassTest : function (object, classes, fname)  "
"addNextMethod : Formal class 'standardGeneric' [package \"methods\"] with 8 slots"
"allNames : function (x)  "
"Arith : Formal class 'groupGenericFunction' [package \"methods\"] with 9 slots"
"as : function (object, Class, strict = TRUE, ext = possibleExtends(thisClass, Class))  "
"as<- : function (object, Class, value)  "
"asMethodDefinition : function (def, signature = list(.anyClassName), sealed = FALSE, fdef = def)  "
"assignClassDef : function (Class, def, where = .GlobalEnv, force = FALSE, doSubclasses = is(def, \"ClassUnionRepresentation\"))  "
"assignMethodsMetaData : function (f, value, fdef, where)  "
"balanceMethodsList : function (mlist, args, check = TRUE)  "
"body<- : Formal class 'standardGeneric' [package \"methods\"] with 8 slots"
"cacheGenericsMetaData : function (f, fdef, attach = TRUE, where = topenv(parent.frame()), package, methods)  "
"cacheMetaData : function (where, attach = TRUE, searchWhere = as.environment(where), doCheck = TRUE)  "
"cacheMethod : function (f, sig, def, args = names(sig), fdef, inherited = FALSE)  "
"callGeneric : function (...)  "
"callNextMethod : function (...)  "
"canCoerce : function (object, Class)  "
"cbind2 : Formal class 'standardGeneric' [package \"methods\"] with 8 slots"
"checkAtAssignment : function (cl, name, valueClass)  "
"checkSlotAssignment : function (obj, name, value)  "
"classesToAM : function (classes, includeSubclasses = FALSE, abbreviate = 2)  "
"classLabel : function (Class)  "
"classMetaName : function (name)  "
"className : function (class, package)  "
"coerce : Formal class 'nonstandardGenericFunction' [package \"methods\"] with 8 slots"
"coerce<- : Formal class 'nonstandardGenericFunction' [package \"methods\"] with 8 slots"
"Compare : Formal class 'groupGenericFunction' [package \"methods\"] with 9 slots"
"completeClassDefinition : function (Class, ClassDef = getClassDef(Class), where, doExtends = TRUE)  "
"completeExtends : function (ClassDef, class2, extensionDef, where)  "
"completeSubclasses : function (classDef, class2, extensionDef, where, classDef2 = getClassDef(class2, where))  "
"Complex : Formal class 'groupGenericFunction' [package \"methods\"] with 9 slots"
"conformMethod : function (signature, mnames, fnames, f = \"<unspecified>\", fdef, method)  "
"defaultDumpName : function (generic, signature)  "
"defaultPrototype : function ()  "
"doPrimitiveMethod : function (name, def, call = sys.call(sys.parent()), ev = sys.frame(sys.parent(2)))  "
"dumpMethod : function (f, signature = character(), file = defaultDumpName(f, signature), where = topenv(parent.frame()), "
"    def = getMethod(f, signature, where = where, optional = TRUE))  "
"dumpMethods : function (f, file = \"\", signature = NULL, methods = findMethods(f, where = where), "
"    where = topenv(parent.frame()))  "
"el : function (object, where)  "
"el<- : .Primitive(\"[[<-\") "
"elNamed : function (x, name, mustFind = FALSE)  "
"elNamed<- : function (x, name, value)  "
"empty.dump : function ()  "
"emptyMethodsList : function (mlist, thisClass = \"ANY\", sublist = list())  "
"evalOnLoad : function (expr, where = topenv(parent.frame()), aname = \"\")  "
"evalqOnLoad : function (expr, where = topenv(parent.frame()), aname = \"\")  "
"evalSource : function (source, package = \"\", lock = TRUE, cache = FALSE)  "
"existsFunction : function (f, generic = TRUE, where = topenv(parent.frame()))  "
"existsMethod : function (f, signature = character(), where = topenv(parent.frame()))  "
"extends : function (class1, class2, maybe = TRUE, fullInfo = FALSE)  "
"externalRefMethod : Formal class 'classGeneratorFunction' [package \"methods\"] with 3 slots"
"finalDefaultMethod : function (method)  "
"findClass : function (Class, where = topenv(parent.frame()), unique = \"\")  "
"findFunction : function (f, generic = TRUE, where = topenv(parent.frame()))  "
"findMethod : function (f, signature, where = topenv(parent.frame()))  "
"findMethods : function (f, where, classes = character(), inherited = FALSE, package = \"\")  "
"findMethodSignatures : function (..., target = TRUE, methods = findMethods(...))  "
"findUnique : function (what, message, where = topenv(parent.frame()))  "
"fixPre1.8 : function (names, where = topenv(parent.frame()))  "
"formalArgs : function (def)  "
"functionBody : function (fun = sys.function(sys.parent()))  "
"functionBody<- : function (fun, envir = environment(fun), value)  "
"generic.skeleton : function (name, fdef, fdefault)  "
"getAllSuperClasses : function (ClassDef, simpleOnly = TRUE)  "
"getClass : function (Class, .Force = FALSE, where = .classEnv(Class, topenv(parent.frame()), "
"    FALSE))  "
"getClassDef : function (Class, where = topenv(parent.frame()), package = packageSlot(Class), inherits = TRUE)  "
"getClasses : function (where = .externalCallerEnv(), inherits = missing(where))  "
"getDataPart : function (object, NULL.for.none = FALSE)  "
"getFunction : function (name, generic = TRUE, mustFind = TRUE, where = topenv(parent.frame()))  "
"getGeneric : function (f, mustFind = FALSE, where, package = \"\")  "
"getGenerics : function (where, searchForm = FALSE)  "
"getGroup : function (fdef, recursive = FALSE, where = topenv(parent.frame()))  "
"getGroupMembers : function (group, recursive = FALSE, character = TRUE)  "
"getLoadActions : function (where = topenv(parent.frame()))  "
"getMethod : function (f, signature = character(), where = topenv(parent.frame()), optional = FALSE, "
"    mlist, fdef)  "
"getMethods : function (f, where = topenv(parent.frame()), table = FALSE)  "
"getMethodsForDispatch : function (fdef, inherited = FALSE)  "
"getMethodsMetaData : function (f, where = topenv(parent.frame()))  "
"getPackageName : function (where = topenv(parent.frame()), create = TRUE)  "
"getRefClass : function (Class, where = topenv(parent.frame()))  "
"getSlots : function (x)  "
"getValidity : function (ClassDef)  "
"hasArg : function (name)  "
"hasLoadAction : function (aname, where = topenv(parent.frame()))  "
"hasMethod : function (f, signature = character(), where = .genEnv(f, topenv(parent.frame())))  "
"hasMethods : function (f, where, package = \"\")  "
"implicitGeneric : function (name, where = topenv(parent.frame()), generic = getGeneric(name, where = where))  "
"inheritedSlotNames : function (Class, where = topenv(parent.frame()))  "
"initFieldArgs : function (.Object, classDef, selfEnv, ...)  "
"initialize : Formal class 'nonstandardGenericFunction' [package \"methods\"] with 8 slots"
"initRefFields : function (.Object, classDef, selfEnv, args)  "
"insertClassMethods : function (methods, Class, value, fieldNames, returnAll)  "
"insertMethod : function (mlist, signature, args, def, cacheOnly = FALSE)  "
"insertSource : function (source, package = \"\", functions = allPlainObjects(), methods = (if (missing(functions)) allMethodTables() else NULL), "
"    force = missing(functions) & missing(methods))  "
"is : function (object, class2)  "
"isClass : function (Class, formal = TRUE, where = topenv(parent.frame()))  "
"isClassDef : function (object)  "
"isClassUnion : function (Class)  "
"isGeneric : function (f, where = topenv(parent.frame()), fdef = NULL, getName = FALSE)  "
"isGrammarSymbol : function (symbol)  "
"isGroup : function (f, where = topenv(parent.frame()), fdef = getGeneric(f, where = where))  "
"isRematched : function (definition)  "
"isSealedClass : function (Class, where = topenv(parent.frame()))  "
"isSealedMethod : function (f, signature, fdef = getGeneric(f, FALSE, where = where), where = topenv(parent.frame()))  "
"isVirtualClass : function (Class, where = topenv(parent.frame()))  "
"isXS3Class : function (classDef)  "
"kronecker : Formal class 'standardGeneric' [package \"methods\"] with 8 slots"
"languageEl : function (object, which)  "
"languageEl<- : function (object, which, value)  "
"linearizeMlist : function (mlist, inherited = TRUE)  "
"listFromMethods : function (generic, where, table)  "
"listFromMlist : function (mlist, prefix = list(), sigs. = TRUE, methods. = TRUE)  "
"loadMethod : Formal class 'standardGeneric' [package \"methods\"] with 8 slots"
"Logic : Formal class 'groupGenericFunction' [package \"methods\"] with 9 slots"
"makeClassRepresentation : function (name, slots = list(), superClasses = character(), prototype = NULL, package, "
"    validity = NULL, access = list(), version = .newExternalptr(), sealed = FALSE, "
"    virtual = NA, where)  "
"makeExtends : function (Class, coerce = NULL, test = NULL, replace = NULL, by = character(), package, "
"    slots = getSlots(classDef1), classDef1 = getClass(Class), classDef2)  "
"makeGeneric : function (f, fdef, fdefault = fdef, group = list(), valueClass = character(), package = getPackageName(environment(fdef)), "
"    signature = NULL, genericFunction = NULL, simpleInheritanceOnly = NULL)  "
"makeMethodsList : function (object, level = 1)  "
"makePrototypeFromClassDef : function (slots, ClassDef, extends, where)  "
"makeStandardGeneric : function (f, fdef)  "
"matchSignature : function (signature, fun, where = baseenv())  "
"Math : Formal class 'groupGenericFunction' [package \"methods\"] with 9 slots"
"Math2 : Formal class 'groupGenericFunction' [package \"methods\"] with 9 slots"
"mergeMethods : function (m1, m2, genericLabel = character())  "
"metaNameUndo : function (strings, prefix, searchForm = FALSE)  "
"method.skeleton : function (generic, signature, file, external = FALSE, where = topenv(parent.frame()))  "
"MethodAddCoerce : function (method, argName, thisClass, methodClass)  "
"methodSignatureMatrix : function (object, sigSlots = c(\"target\", \"defined\"))  "
"MethodsList : function (.ArgName, ...)  "
"MethodsListSelect : function (f, env, mlist = NULL, fEnv = if (is(fdef, \"genericFunction\")) environment(fdef) else baseenv(), "
"    finalDefault = finalDefaultMethod(mlist), evalArgs = TRUE, useInherited = TRUE, "
"    fdef = getGeneric(f, where = env), resetAllowed = TRUE)  "
"methodsPackageMetaName : function (prefix, name, package = \"\")  "
"missingArg : function (symbol, envir = parent.frame(), eval = FALSE)  "
"multipleClasses : function (details = FALSE)  "
"new : function (Class, ...)  "
"newBasic : function (Class, ...)  "
"newClassRepresentation : function (...)  "
"newEmptyObject : function ()  "
"Ops : Formal class 'groupGenericFunction' [package \"methods\"] with 9 slots"
"packageSlot : function (object)  "
"packageSlot<- : function (object, value)  "
"possibleExtends : function (class1, class2, ClassDef1 = getClassDef(class1), ClassDef2 = getClassDef(class2, "
"    where = .classEnv(ClassDef1)))  "
"prohibitGeneric : function (name, where = topenv(parent.frame()))  "
"promptClass : function (clName, filename = NULL, type = \"class\", keywords = \"classes\", where = topenv(parent.frame()), "
"    generatorName = clName)  "
"promptMethods : function (f, filename = NULL, methods)  "
"prototype : function (...)  "
"Quote : function (expr)  "
"rbind2 : Formal class 'standardGeneric' [package \"methods\"] with 8 slots"
"reconcilePropertiesAndPrototype : function (name, properties, prototype, superClasses, where)  "
"registerImplicitGenerics : function (what = .ImplicitGenericsTable(where), where = topenv(parent.frame()))  "
"rematchDefinition : function (definition, generic, mnames, fnames, signature)  "
"removeClass : function (Class, where = topenv(parent.frame()))  "
"removeGeneric : function (f, where = topenv(parent.frame()))  "
"removeMethod : function (f, signature = character(), where = topenv(parent.frame()))  "
"removeMethods : function (f, where = topenv(parent.frame()), all = missing(where))  "
"representation : function (...)  "
"requireMethods : function (functions, signature, message = \"\", where = topenv(parent.frame()))  "
"resetClass : function (Class, classDef, where)  "
"resetGeneric : function (f, fdef = getGeneric(f, where = where), mlist = getMethodsForDispatch(fdef), "
"    where = topenv(parent.frame()), deflt = finalDefaultMethod(mlist))  "
"S3Class : function (object)  "
"S3Class<- : function (object, value)  "
"S3Part : function (object, strictS3 = FALSE, S3Class)  "
"S3Part<- : function (object, strictS3 = FALSE, needClass = .S3Class(object), value)  "
"sealClass : function (Class, where = topenv(parent.frame()))  "
"selectMethod : function (f, signature, optional = FALSE, useInherited = TRUE, mlist = if (!is.null(fdef)) getMethodsForDispatch(fdef), "
"    fdef = getGeneric(f, !optional), verbose = FALSE, doCache = FALSE)  "
"selectSuperClasses : function (Class, dropVirtual = FALSE, namesOnly = TRUE, directOnly = TRUE, simpleOnly = directOnly, "
"    where = topenv(parent.frame()))  "
"setAs : function (from, to, def, replace = NULL, where = topenv(parent.frame()))  "
"setClass : function (Class, representation = list(), prototype = NULL, contains = character(), "
"    validity = NULL, access = list(), where = topenv(parent.frame()), version = .newExternalptr(), "
"    sealed = FALSE, package = getPackageName(where), S3methods = FALSE, slots)  "
"setClassUnion : function (name, members = character(), where = topenv(parent.frame()))  "
"setDataPart : function (object, value, check = TRUE)  "
"setGeneric : function (name, def = NULL, group = list(), valueClass = character(), where = topenv(parent.frame()), "
"    package = NULL, signature = NULL, useAsDefault = NULL, genericFunction = NULL, "
"    simpleInheritanceOnly = NULL)  "
"setGenericImplicit : function (name, where = topenv(parent.frame()), restore = TRUE)  "
"setGroupGeneric : function (name, def = NULL, group = list(), valueClass = character(), knownMembers = list(), "
"    package = getPackageName(where), where = topenv(parent.frame()))  "
"setIs : function (class1, class2, test = NULL, coerce = NULL, replace = NULL, by = character(), "
"    where = topenv(parent.frame()), classDef = getClass(class1, TRUE, where = where), "
"    extensionObject = NULL, doComplete = TRUE)  "
"setLoadAction : function (action, aname = \"\", where = topenv(parent.frame()))  "
"setLoadActions : function (..., .where = topenv(parent.frame()))  "
"setMethod : function (f, signature = character(), definition, where = topenv(parent.frame()), "
"    valueClass = NULL, sealed = FALSE)  "
"setOldClass : function (Classes, prototype = NULL, where = topenv(parent.frame()), test = FALSE, "
"    S4Class)  "
"setPackageName : function (pkg, env)  "
"setPrimitiveMethods : function (f, fdef, code, generic, mlist = get(\".Methods\", envir = environment(generic)))  "
"setRefClass : function (Class, fields = character(), contains = character(), methods = list(), where = topenv(parent.frame()), "
"    inheritPackage = FALSE, ...)  "
"setReplaceMethod : function (f, ..., where = topenv(parent.frame()))  "
"setValidity : function (Class, method, where = topenv(parent.frame()))  "
"show : Formal class 'standardGeneric' [package \"methods\"] with 8 slots"
"showClass : function (Class, complete = TRUE, propertiesAreCalled = \"Slots\")  "
"showDefault : function (object, oldMethods = TRUE)  "
"showExtends : function (ext, printTo = stdout())  "
"showMethods : function (f = character(), where = topenv(parent.frame()), classes = NULL, includeDefs = FALSE, "
"    inherited = !includeDefs, showEmpty, printTo = stdout(), fdef = getGeneric(f, "
"        where = where))  "
"showMlist : function (mlist, includeDefs = TRUE, inherited = TRUE, classes = NULL, useArgNames = TRUE, "
"    printTo = stdout())  "
"signature : function (...)  "
"SignatureMethod : function (names, signature, definition)  "
"sigToEnv : function (signature, generic)  "
"slot : function (object, name)  "
"slot<- : function (object, name, check = TRUE, value)  "
"slotNames : function (x)  "
"slotsFromS3 : Formal class 'standardGeneric' [package \"methods\"] with 8 slots"
"substituteDirect : function (object, frame = parent.frame(), cleanFunction = TRUE)  "
"substituteFunctionArgs : function (def, newArgs, args = formalArgs(def), silent = FALSE, functionName = \"a function\")  "
"Summary : Formal class 'groupGenericFunction' [package \"methods\"] with 9 slots"
"superClassDepth : function (ClassDef, soFar = ClassDef@className, simpleOnly = TRUE)  "
"testInheritedMethods : function (f, signatures, test = TRUE, virtual = FALSE, groupMethods = TRUE, where = .GlobalEnv)  "
"testVirtual : function (properties, extends, prototype, where)  "
"tryNew : function (Class, where)  "
"unRematchDefinition : function (definition)  "
"validObject : function (object, test = FALSE, complete = FALSE)  "
"validSlotNames : function (names)  "

library(microbenchmark)
get_nanotime : function ()  
  microbenchmark : function (..., list = NULL, times = 100L, unit = NULL, check = NULL, control = list(), 
                             setup = NULL)  
    microtiming_precision : function (rounds = 100L, warmup = 2^18)  
      > 
library(mlr)
"x"
".Depends :  chr \"ParamHelpers\""
"acc : List of 10"
" $ id        : chr \"acc\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:4] \"classif\" \"classif.multi\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"Accuracy\""
" $ note      : chr \"Defined as: mean(response == truth)\""
" $ aggr      :List of 4"
"addRRMeasure : function (res, measures)  "
"agri.task : List of 6"
" $ type       : chr \"cluster\""
" $ env        :<environment: 0x000001dbfe80d728> "
" $ weights    : NULL"
" $ blocking   : NULL"
" $ coordinates: NULL"
" $ task.desc  :List of 9"
"analyzeFeatSelResult : function (res, reduce = TRUE)  "
"asROCRPrediction : function (pred)  "
"auc : List of 10"
" $ id        : chr \"auc\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:4] \"classif\" \"req.pred\" \"req.truth\" \"req.prob\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"Area under the curve\""
" $ note      : chr \"Integral over the graph that results from computing fpr and tpr for many different thresholds.\""
" $ aggr      :List of 4"
"b632 : List of 4"
" $ id        : chr \"b632\""
" $ name      : chr \".632 Bootstrap\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr [1:2] \"req.train\" \"req.test\""
"b632plus : List of 4"
" $ id        : chr \"b632plus\""
" $ name      : chr \".632 Bootstrap plus\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr [1:2] \"req.train\" \"req.test\""
"bac : List of 10"
" $ id        : chr \"bac\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:4] \"classif\" \"classif.multi\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"Balanced accuracy\""
" $ note      : chr \"For binary tasks, mean of true positive rate and true negative rate.\""
" $ aggr      :List of 4"
"batchmark : function (learners, tasks, resamplings, measures, keep.pred = TRUE, keep.extract = FALSE, "
"    models = FALSE, reg = batchtools::getDefaultRegistry())  "
"bc.task : List of 6"
" $ type       : chr \"classif\""
" $ env        :<environment: 0x000001dbfdf9f988> "
" $ weights    : NULL"
" $ blocking   : NULL"
" $ coordinates: NULL"
" $ task.desc  :List of 13"
"benchmark : function (learners, tasks, resamplings, measures, keep.pred = TRUE, keep.extract = FALSE, "
"    models = FALSE, show.info = getMlrOption(\"show.info\"))  "
"ber : List of 10"
" $ id        : chr \"ber\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:4] \"classif\" \"classif.multi\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num 1"
" $ name      : chr \"Balanced error rate\""
" $ note      : chr \"Mean of misclassification error rates on all individual classes.\""
" $ aggr      :List of 4"
"bh.task : List of 6"
" $ type       : chr \"regr\""
" $ env        :<environment: 0x000001dbfdca2560> "
" $ weights    : NULL"
" $ blocking   : NULL"
" $ coordinates: NULL"
" $ task.desc  :List of 9"
"bootstrapB632 : function (learner, task, iters = 30, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, "
"    ..., show.info = getMlrOption(\"show.info\"))  "
"bootstrapB632plus : function (learner, task, iters = 30, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, "
"    ..., show.info = getMlrOption(\"show.info\"))  "
"bootstrapOOB : function (learner, task, iters = 30, stratify = FALSE, measures, models = FALSE, keep.pred = TRUE, "
"    ..., show.info = getMlrOption(\"show.info\"))  "
"brier : List of 10"
" $ id        : chr \"brier\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:4] \"classif\" \"req.pred\" \"req.truth\" \"req.prob\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num 1"
" $ name      : chr \"Brier score\""
" $ note      : chr \"The Brier score is defined as the quadratic difference between the probability and the value (1,0) for the clas\"| __truncated__"
" $ aggr      :List of 4"
"brier.scaled : List of 10"
" $ id        : chr \"brier.scaled\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:4] \"classif\" \"req.pred\" \"req.truth\" \"req.prob\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"Brier scaled\""
" $ note      : chr \"Brier score scaled to [0,1], see http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3575184/.\""
" $ aggr      :List of 4"
"calculateConfusionMatrix : function (pred, relative = FALSE, sums = FALSE, set = \"both\")  "
"calculateROCMeasures : function (pred)  "
"capLargeValues : function (obj, target = character(0L), cols = NULL, threshold = Inf, impute = threshold, "
"    what = \"abs\")  "
"changeData : function (task, data, costs, weights, coordinates)  "
"checkLearner : function (learner, type = NULL, props = NULL)  "
"checkPredictLearnerOutput : function (learner, model, p)  "
"cindex : List of 10"
" $ id        : chr \"cindex\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"surv\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"Harrell's Concordance index\""
" $ note      : chr \"Fraction of all pairs of subjects whose predicted survival times are correctly ordered among all subjects that \"| __truncated__"
" $ aggr      :List of 4"
"cindex.uno : List of 10"
" $ id        : chr \"cindex.uno\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:5] \"surv\" \"req.pred\" \"req.truth\" \"req.model\" ..."
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args:List of 1"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"Uno's Concordance index\""
" $ note      : chr \"Fraction of all pairs of subjects whose predicted survival times are correctly ordered among all subjects that \"| __truncated__"
" $ aggr      :List of 4"
"configureMlr : function (show.info, on.learner.error, on.learner.warning, on.par.without.desc, on.par.out.of.bounds, "
"    on.measure.not.applicable, show.learner.output, on.error.dump)  "
"convertBMRToRankMatrix : function (bmr, measure = NULL, ties.method = \"average\", aggregation = \"default\")  "
"convertMLBenchObjToTask : function (x, n = 100L, ...)  "
"costiris.task : List of 6"
" $ type       : chr \"costsens\""
" $ env        :<environment: 0x000001dbfcfea918> "
" $ weights    : NULL"
" $ blocking   : NULL"
" $ coordinates: NULL"
" $ task.desc  :List of 11"
"createDummyFeatures : function (obj, target = character(0L), method = \"1-of-n\", cols = NULL)  "
"createSpatialResamplingPlots : function (task = NULL, resample = NULL, crs = NULL, datum = 4326, repetitions = 1, "
"    color.train = \"#0072B5\", color.test = \"#E18727\", point.size = 0.5, axis.text.size = 14, "
"    x.axis.breaks = waiver(), y.axis.breaks = waiver())  "
"crossval : function (learner, task, iters = 10L, stratify = FALSE, measures, models = FALSE, "
"    keep.pred = TRUE, ..., show.info = getMlrOption(\"show.info\"))  "
"cv10 : List of 6"
" $ fixed      : logi FALSE"
" $ blocking.cv: logi FALSE"
" $ id         : chr \"cross-validation\""
" $ iters      : int 10"
" $ predict    : chr \"test\""
" $ stratify   : logi FALSE"
"cv2 : List of 6"
" $ fixed      : logi FALSE"
" $ blocking.cv: logi FALSE"
" $ id         : chr \"cross-validation\""
" $ iters      : int 2"
" $ predict    : chr \"test\""
" $ stratify   : logi FALSE"
"cv3 : List of 6"
" $ fixed      : logi FALSE"
" $ blocking.cv: logi FALSE"
" $ id         : chr \"cross-validation\""
" $ iters      : int 3"
" $ predict    : chr \"test\""
" $ stratify   : logi FALSE"
"cv5 : List of 6"
" $ fixed      : logi FALSE"
" $ blocking.cv: logi FALSE"
" $ id         : chr \"cross-validation\""
" $ iters      : int 5"
" $ predict    : chr \"test\""
" $ stratify   : logi FALSE"
"db : List of 10"
" $ id        : chr \"db\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"cluster\" \"req.pred\" \"req.feats\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Davies-Bouldin cluster separation measure\""
" $ note      : chr \"Ratio of the within cluster scatter, to the between cluster separation, averaged over the clusters. See `?clust\"| __truncated__"
" $ aggr      :List of 4"
"deleteCacheDir : function ()  "
"downsample : function (obj, perc = 1, stratify = FALSE)  "
"dropFeatures : function (task, features)  "
"estimateRelativeOverfitting : function (predish, measures, task, learner = NULL, pred.train = NULL, iter = 1)  "
"estimateResidualVariance : function (x, task, data, target)  "
"expvar : List of 10"
" $ id        : chr \"expvar\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"regr\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"Explained variance\""
" $ note      : chr \"Similar to measure rsq (R-squared). Defined as explained_sum_of_squares / total_sum_of_squares.\""
" $ aggr      :List of 4"
"extractFDABsignal : function (bsignal.knots = 10L, bsignal.df = 3)  "
"extractFDADTWKernel : function (ref.method = \"random\", n.refs = 0.05, refs = NULL, dtwwindow = 0.05)  "
"extractFDAFeatures : function (obj, target = character(0L), feat.methods = list(), ...)  "
"extractFDAFourier : function (trafo.coeff = \"phase\")  "
"extractFDAFPCA : function (rank. = NULL, center = TRUE, scale. = FALSE)  "
"extractFDAMultiResFeatures : function (res.level = 3L, shift = 0.5, seg.lens = NULL)  "
"extractFDATsfeatures : function (scale = TRUE, trim = FALSE, trim_amount = 0.1, parallel = FALSE, na.action = na.pass, "
"    feats = NULL, ...)  "
"extractFDAWavelets : function (filter = \"la8\", boundary = \"periodic\")  "
"f1 : List of 10"
" $ id        : chr \"f1\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"classif\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"F1 measure\""
" $ note      : chr \"Defined as: 2 * tp/ (sum(truth == positive) + sum(response == positive))\""
" $ aggr      :List of 4"
"fdr : List of 10"
" $ id        : chr \"fdr\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"classif\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num 1"
" $ name      : chr \"False discovery rate\""
" $ note      : chr \"Defined as: fp / (tp + fp).\""
" $ aggr      :List of 4"
"featperc : List of 10"
" $ id        : chr \"featperc\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:9] \"classif\" \"classif.multi\" \"multilabel\" \"regr\" ..."
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num 1"
" $ name      : chr \"Percentage of original features used for model\""
" $ note      : chr \"Useful for feature selection.\""
" $ aggr      :List of 4"
"filterFeatures : function (task, method = \"randomForestSRC_importance\", fval = NULL, perc = NULL, abs = NULL, "
"    threshold = NULL, fun = NULL, fun.args = NULL, mandatory.feat = NULL, select.method = NULL, "
"    base.methods = NULL, cache = FALSE, ...)  "
"fixedcv : function (learner, task, horizon = 1L, initial.window = 0.5, skip = 0, measures, models = FALSE, "
"    keep.pred = TRUE, ..., show.info = getMlrOption(\"show.info\"))  "
"fn : List of 10"
" $ id        : chr \"fn\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"classif\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"False negatives\""
" $ note      : chr \"Sum of misclassified observations in the negative class. Also called misses.\""
" $ aggr      :List of 4"
"fnr : List of 10"
" $ id        : chr \"fnr\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"classif\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num 1"
" $ name      : chr \"False negative rate\""
" $ note      : chr \"Percentage of misclassified observations in the negative class.\""
" $ aggr      :List of 4"
"fp : List of 10"
" $ id        : chr \"fp\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"classif\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"False positives\""
" $ note      : chr \"Sum of misclassified observations in the positive class. Also called false alarms.\""
" $ aggr      :List of 4"
"fpr : List of 10"
" $ id        : chr \"fpr\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"classif\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num 1"
" $ name      : chr \"False positive rate\""
" $ note      : chr \"Percentage of misclassified observations in the positive class. Also called false alarm rate or fall-out.\""
" $ aggr      :List of 4"
"friedmanPostHocTestBMR : function (bmr, measure = NULL, p.value = 0.05, aggregation = \"default\")  "
"friedmanTestBMR : function (bmr, measure = NULL, aggregation = \"default\")  "
"fuelsubset.task : List of 6"
" $ type       : chr \"regr\""
" $ env        :<environment: 0x000001dbfa296420> "
" $ weights    : NULL"
" $ blocking   : NULL"
" $ coordinates: NULL"
" $ task.desc  :List of 9"
"G1 : List of 10"
" $ id        : chr \"G1\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"cluster\" \"req.pred\" \"req.feats\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num Inf"
" $ worst     : num 0"
" $ name      : chr \"Calinski-Harabasz pseudo F statistic\""
" $ note      : chr \"Defined as ratio of between-cluster variance to within cluster variance. See `?clusterSim::index.G1`.\""
" $ aggr      :List of 4"
"G2 : List of 10"
" $ id        : chr \"G2\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"cluster\" \"req.pred\" \"req.feats\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"Baker and Hubert adaptation of Goodman-Kruskal's gamma statistic\""
" $ note      : chr \"Defined as: (number of concordant comparisons - number of discordant comparisons) / (number of concordant compa\"| __truncated__"
" $ aggr      :List of 4"
"generateCalibrationData : function (obj, breaks = \"Sturges\", groups = NULL, task.id = NULL)  "
"generateCritDifferencesData : function (bmr, measure = NULL, p.value = 0.05, baseline = NULL, test = \"bd\")  "
"generateFeatureImportanceData : function (task, method = \"permutation.importance\", learner, features = getTaskFeatureNames(task), "
"    interaction = FALSE, measure, contrast = function(x, y) x - y, aggregation = mean, "
"    nmc = 50L, replace = TRUE, local = FALSE, show.info = FALSE)  "
"generateFilterValuesData : function (task, method = \"randomForestSRC_importance\", nselect = getTaskNFeats(task), "
"    ..., more.args = list())  "
"generateHyperParsEffectData : function (tune.result, include.diagnostics = FALSE, trafo = FALSE, partial.dep = FALSE)  "
"generateLearningCurveData : function (learners, task, resampling = NULL, percs = seq(0.1, 1, by = 0.1), measures, "
"    stratify = FALSE, show.info = getMlrOption(\"show.info\"))  "
"generatePartialDependenceData : function (obj, input, features = NULL, interaction = FALSE, derivative = FALSE, individual = FALSE, "
"    fun = mean, bounds = c(qnorm(0.025), qnorm(0.975)), uniform = TRUE, n = c(10, "
"        NA), ...)  "
"generateThreshVsPerfData : function (obj, measures, gridsize = 100L, aggregate = TRUE, task.id = NULL)  "
"getBMRAggrPerformances : function (bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE, drop = FALSE)  "
"getBMRFeatSelResults : function (bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE, drop = FALSE)  "
"getBMRFilteredFeatures : function (bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE, drop = FALSE)  "
"getBMRLearnerIds : function (bmr)  "
"getBMRLearners : function (bmr)  "
"getBMRLearnerShortNames : function (bmr)  "
"getBMRMeasureIds : function (bmr)  "
"getBMRMeasures : function (bmr)  "
"getBMRModels : function (bmr, task.ids = NULL, learner.ids = NULL, drop = FALSE)  "
"getBMRPerformances : function (bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE, drop = FALSE)  "
"getBMRPredictions : function (bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE, drop = FALSE)  "
"getBMRTaskDescriptions : function (bmr)  "
"getBMRTaskDescs : function (bmr)  "
"getBMRTaskIds : function (bmr)  "
"getBMRTuneResults : function (bmr, task.ids = NULL, learner.ids = NULL, as.df = FALSE, drop = FALSE)  "
"getCacheDir : function ()  "
"getCaretParamSet : function (learner, length = 3L, task, discretize = TRUE)  "
"getClassWeightParam : function (learner, lrn.id = NULL)  "
"getConfMatrix : function (pred, relative = FALSE)  "
"getDefaultMeasure : function (x)  "
"getFailureModelDump : function (model)  "
"getFailureModelMsg : function (model)  "
"getFeatSelResult : function (object)  "
"getFeatureImportance : function (object, ...)  "
"getFeatureImportanceLearner : function (.learner, .model, ...)  "
"getFilteredFeatures : function (model)  "
"getFunctionalFeatures : function (object, subset = NULL, features, recode.target = \"no\")  "
"getHomogeneousEnsembleModels : function (model, learner.models = FALSE)  "
"getHyperPars : function (learner, for.fun = c(\"train\", \"predict\", \"both\"))  "
"getLearnerId : function (learner)  "
"getLearnerModel : function (model, more.unwrap = FALSE)  "
"getLearnerNote : function (learner)  "
"getLearnerPackages : function (learner)  "
"getLearnerParamSet : function (learner)  "
"getLearnerParVals : function (learner, for.fun = c(\"train\", \"predict\", \"both\"))  "
"getLearnerPredictType : function (learner)  "
"getLearnerProperties : function (learner)  "
"getLearnerShortName : function (learner)  "
"getLearnerType : function (learner)  "
"getMeasureProperties : function (measure)  "
"getMlrOptions : function ()  "
"getMultilabelBinaryPerformances : function (pred, measures)  "
"getNestedTuneResultsOptPathDf : function (r, trafo = FALSE)  "
"getNestedTuneResultsX : function (r)  "
"getOOBPreds : function (model, task)  "
"getOOBPredsLearner : function (.learner, .model)  "
"getPredictionDump : function (pred)  "
"getPredictionProbabilities : function (pred, cl)  "
"getPredictionResponse : function (pred)  "
"getPredictionSE : function (pred)  "
"getPredictionTaskDesc : function (pred)  "
"getPredictionTruth : function (pred)  "
"getProbabilities : function (pred, cl)  "
"getResamplingIndices : function (object, inner = FALSE)  "
"getRRDump : function (res)  "
"getRRPredictionList : function (res, ...)  "
"getRRPredictions : function (res)  "
"getRRTaskDesc : function (res)  "
"getRRTaskDescription : function (res)  "
"getStackedBaseLearnerPredictions : function (model, newdata = NULL)  "
"getTaskClassLevels : function (x)  "
"getTaskCosts : function (task, subset = NULL)  "
"getTaskData : function (task, subset = NULL, features, target.extra = FALSE, recode.target = \"no\", "
"    functionals.as = \"dfcols\")  "
"getTaskDesc : function (x)  "
"getTaskDescription : function (x)  "
"getTaskFeatureNames : function (task)  "
"getTaskFormula : function (x, target = getTaskTargetNames(x), explicit.features = FALSE, env = parent.frame())  "
"getTaskId : function (x)  "
"getTaskNFeats : function (x)  "
"getTaskSize : function (x)  "
"getTaskTargetNames : function (x)  "
"getTaskTargets : function (task, recode.target = \"no\")  "
"getTaskType : function (x)  "
"getTuneResult : function (object)  "
"getTuneResultOptPath : function (tune.result, as.df = TRUE)  "
"gmean : List of 10"
" $ id        : chr \"gmean\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"classif\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"G-mean\""
" $ note      : chr \"Geometric mean of recall and specificity.\""
" $ aggr      :List of 4"
"gpr : List of 10"
" $ id        : chr \"gpr\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"classif\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"Geometric mean of precision and recall.\""
" $ note      : chr \"Defined as: sqrt(ppv * tpr)\""
" $ aggr      :List of 4"
"growingcv : function (learner, task, horizon = 1, initial.window = 0.5, skip = 0, measures, models = FALSE, "
"    keep.pred = TRUE, ..., show.info = getMlrOption(\"show.info\"))  "
"gunpoint.task : List of 6"
" $ type       : chr \"classif\""
" $ env        :<environment: 0x000001dbf4a2d478> "
" $ weights    : NULL"
" $ blocking   : NULL"
" $ coordinates: NULL"
" $ task.desc  :List of 13"
"hasFunctionalFeatures : function (obj)  "
"hasLearnerProperties : function (learner, props)  "
"hasMeasureProperties : function (measure, props)  "
"hasProperties : function (learner, props)  "
"helpLearner : function (learner)  "
"helpLearnerParam : function (learner, param = NULL)  "
"holdout : function (learner, task, split = 2/3, stratify = FALSE, measures, models = FALSE, "
"    keep.pred = TRUE, ..., show.info = getMlrOption(\"show.info\"))  "
"hout : List of 7"
" $ split      : num 0.667"
" $ id         : chr \"holdout\""
" $ iters      : int 1"
" $ predict    : chr \"test\""
" $ stratify   : logi FALSE"
" $ fixed      : logi FALSE"
" $ blocking.cv: logi FALSE"
"iauc.uno : List of 10"
" $ id        : chr \"iauc.uno\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:5] \"surv\" \"req.pred\" \"req.truth\" \"req.model\" ..."
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args:List of 2"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"Uno's estimator of cumulative AUC for right censored time-to-event data\""
" $ note      : chr \"To set an upper time limit, set argument max.time (defaults to max time in complete task). Implemented in survAUC::AUC.uno.\""
" $ aggr      :List of 4"
"ibrier : List of 10"
" $ id        : chr \"ibrier\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:4] \"surv\" \"req.truth\" \"req.model\" \"req.task\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args:List of 2"
" $ best      : num 0"
" $ worst     : num 1"
" $ name      : chr \"Integrated brier score using Kaplan-Meier estimator for weighting\""
" $ note      : chr \"Only works for methods for which probabilities are provided via pec::predictSurvProb. Currently these are only \"| __truncated__"
" $ aggr      :List of 4"
"impute : function (obj, target = character(0L), classes = list(), cols = list(), dummy.classes = character(0L), "
"    dummy.cols = character(0L), dummy.type = \"factor\", force.dummies = FALSE, impute.new.levels = TRUE, "
"    recode.factor.levels = TRUE)  "
"imputeConstant : function (const)  "
"imputeHist : function (breaks, use.mids = TRUE)  "
"imputeLearner : function (learner, features = NULL)  "
"imputeMax : function (multiplier = 1)  "
"imputeMean : function ()  "
"imputeMedian : function ()  "
"imputeMin : function (multiplier = 1)  "
"imputeMode : function ()  "
"imputeNormal : function (mu = NA_real_, sd = NA_real_)  "
"imputeUniform : function (min = NA_real_, max = NA_real_)  "
"iris.task : List of 6"
" $ type       : chr \"classif\""
" $ env        :<environment: 0x000001dbf2e218a0> "
" $ weights    : NULL"
" $ blocking   : NULL"
" $ coordinates: NULL"
" $ task.desc  :List of 13"
"isFailureModel : function (model)  "
"joinClassLevels : function (task, new.levels)  "
"kappa : List of 10"
" $ id        : chr \"kappa\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:4] \"classif\" \"classif.multi\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num -1"
" $ name      : chr \"Cohen's kappa\""
" $ note      : chr \"Defined as: 1 - (1 - p0) / (1 - pe). With: p0 = 'observed frequency of\n    agreement' and pe = 'expected agrem\"| __truncated__"
" $ aggr      :List of 4"
"kendalltau : List of 10"
" $ id        : chr \"kendalltau\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"regr\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num -1"
" $ name      : chr \"Kendall's tau\""
" $ note      : chr \"Defined as: Kendall's tau correlation between truth and response. Only looks at the order.\n  See Rosset et al.\"| __truncated__"
" $ aggr      :List of 4"
"learnerArgsToControl : function (control, ...)  "
"listFilterEnsembleMethods : function (desc = TRUE)  "
"listFilterMethods : function (desc = TRUE, tasks = FALSE, features = FALSE, include.deprecated = FALSE)  "
"listLearnerProperties : function (type = \"any\")  "
"listLearners : function (obj = NA_character_, properties = character(0L), quiet = TRUE, warn.missing.packages = TRUE, "
"    check.packages = FALSE, create = FALSE)  "
"listMeasureProperties : function ()  "
"listMeasures : function (obj, properties = character(0L), create = FALSE)  "
"listTaskTypes : function ()  "
"logloss : List of 10"
" $ id        : chr \"logloss\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:4] \"classif\" \"classif.multi\" \"req.truth\" \"req.prob\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Logarithmic loss\""
" $ note      : chr \"Defined as: -mean(log(p_i)), where p_i is the predicted probability of the true class of observation i. Inspire\"| __truncated__"
" $ aggr      :List of 4"
"lsr : List of 10"
" $ id        : chr \"lsr\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:4] \"classif\" \"classif.multi\" \"req.truth\" \"req.prob\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num -Inf"
" $ name      : chr \"Logarithmic Scoring Rule\""
" $ note      : chr \"Defined as: mean(log(p_i)), where p_i is the predicted probability of the true class of observation i.\n  This \"| __truncated__"
" $ aggr      :List of 4"
"lung.task : List of 6"
" $ type       : chr \"surv\""
" $ env        :<environment: 0x000001dbeed137b0> "
" $ weights    : NULL"
" $ blocking   : NULL"
" $ coordinates: NULL"
" $ task.desc  :List of 9"
"mae : List of 10"
" $ id        : chr \"mae\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"regr\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Mean of absolute errors\""
" $ note      : chr \"Defined as: mean(abs(response - truth))\""
" $ aggr      :List of 4"
"makeAggregation : function (id, name = id, properties, fun)  "
"makeBaggingWrapper : function (learner, bw.iters = 10L, bw.replace = TRUE, bw.size, bw.feats = 1)  "
"makeBaseWrapper : function (id, type, next.learner, package = character(0L), par.set = makeParamSet(), "
"    par.vals = list(), learner.subclass, model.subclass, cache = FALSE)  "
"makeChainModel : function (next.model, cl)  "
"makeClassificationViaRegressionWrapper : function (learner, predict.type = \"response\")  "
"makeClassifTask : function (id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, "
"    coordinates = NULL, positive = NA_character_, fixup.data = \"warn\", check.data = TRUE)  "
"makeClassifTaskDesc : function (id, data, target, weights, blocking, positive, coordinates)  "
"makeClusterTask : function (id = deparse(substitute(data)), data, weights = NULL, blocking = NULL, coordinates = NULL, "
"    fixup.data = \"warn\", check.data = TRUE)  "
"makeClusterTaskDesc : function (id, data, weights, blocking, coordinates)  "
"makeConstantClassWrapper : function (learner, frac = 0)  "
"makeCostMeasure : function (id = \"costs\", minimize = TRUE, costs, combine = mean, best = NULL, worst = NULL, "
"    name = id, note = \"\")  "
"makeCostSensClassifWrapper : function (learner)  "
"makeCostSensRegrWrapper : function (learner)  "
"makeCostSensTask : function (id = deparse(substitute(data)), data, costs, blocking = NULL, coordinates = NULL, "
"    fixup.data = \"warn\", check.data = TRUE)  "
"makeCostSensTaskDesc : function (id, data, target, blocking, costs, coordinates)  "
"makeCostSensWeightedPairsWrapper : function (learner)  "
"makeCustomResampledMeasure : function (measure.id, aggregation.id, minimize = TRUE, properties = character(0L), "
"    fun, extra.args = list(), best = NULL, worst = NULL, measure.name = measure.id, "
"    aggregation.name = aggregation.id, note = \"\")  "
"makeDownsampleWrapper : function (learner, dw.perc = 1, dw.stratify = FALSE)  "
"makeDummyFeaturesWrapper : function (learner, method = \"1-of-n\", cols = NULL)  "
"makeExtractFDAFeatMethod : function (learn, reextract, args = list(), par.set = NULL)  "
"makeExtractFDAFeatsWrapper : function (learner, feat.methods = list())  "
"makeFeatSelControlExhaustive : function (same.resampling.instance = TRUE, maxit = NA_integer_, max.features = NA_integer_, "
"    tune.threshold = FALSE, tune.threshold.args = list(), log.fun = \"default\")  "
"makeFeatSelControlGA : function (same.resampling.instance = TRUE, impute.val = NULL, maxit = NA_integer_, "
"    max.features = NA_integer_, comma = FALSE, mu = 10L, lambda, crossover.rate = 0.5, "
"    mutation.rate = 0.05, tune.threshold = FALSE, tune.threshold.args = list(), log.fun = \"default\")  "
"makeFeatSelControlRandom : function (same.resampling.instance = TRUE, maxit = 100L, max.features = NA_integer_, "
"    prob = 0.5, tune.threshold = FALSE, tune.threshold.args = list(), log.fun = \"default\")  "
"makeFeatSelControlSequential : function (same.resampling.instance = TRUE, impute.val = NULL, method, alpha = 0.01, "
"    beta = -0.001, maxit = NA_integer_, max.features = NA_integer_, tune.threshold = FALSE, "
"    tune.threshold.args = list(), log.fun = \"default\")  "
"makeFeatSelWrapper : function (learner, resampling, measures, bit.names, bits.to.features, control, show.info = getMlrOption(\"show.info\"))  "
"makeFilter : function (name, desc, pkg, supported.tasks, supported.features, fun)  "
"makeFilterEnsemble : function (name, base.methods, desc, fun)  "
"makeFilterWrapper : function (learner, fw.method = \"randomForestSRC_importance\", fw.base.methods = NULL, "
"    fw.perc = NULL, fw.abs = NULL, fw.threshold = NULL, fw.fun = NULL, fw.fun.args = NULL, "
"    fw.mandatory.feat = NULL, cache = FALSE, ...)  "
"makeFixedHoldoutInstance : function (train.inds, test.inds, size)  "
"makeFunctionalData : function (data, fd.features = NULL, exclude.cols = NULL)  "
"makeImputeMethod : function (learn, impute, args = list())  "
"makeImputeWrapper : function (learner, classes = list(), cols = list(), dummy.classes = character(0L), "
"    dummy.cols = character(0L), dummy.type = \"factor\", force.dummies = FALSE, impute.new.levels = TRUE, "
"    recode.factor.levels = TRUE)  "
"makeLearner : function (cl, id = cl, predict.type = \"response\", predict.threshold = NULL, fix.factors.prediction = FALSE, "
"    ..., par.vals = list(), config = list())  "
"makeLearners : function (cls, ids = NULL, type = NULL, ...)  "
"makeMeasure : function (id, minimize, properties = character(0L), fun, extra.args = list(), aggr = test.mean, "
"    best = NULL, worst = NULL, name = id, note = \"\")  "
"makeModelMultiplexer : function (base.learners)  "
"makeModelMultiplexerParamSet : function (multiplexer, ..., .check = TRUE)  "
"makeMulticlassWrapper : function (learner, mcw.method = \"onevsrest\")  "
"makeMultilabelBinaryRelevanceWrapper : function (learner)  "
"makeMultilabelClassifierChainsWrapper : function (learner, order = NULL)  "
"makeMultilabelDBRWrapper : function (learner)  "
"makeMultilabelNestedStackingWrapper : function (learner, order = NULL, cv.folds = 2)  "
"makeMultilabelStackingWrapper : function (learner, cv.folds = 2)  "
"makeMultilabelTask : function (id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, "
"    coordinates = NULL, fixup.data = \"warn\", check.data = TRUE)  "
"makeMultilabelTaskDesc : function (id, data, target, weights, blocking, coordinates)  "
"makeOverBaggingWrapper : function (learner, obw.iters = 10L, obw.rate = 1, obw.maxcl = \"boot\", obw.cl = NULL)  "
"makeOversampleWrapper : function (learner, osw.rate = 1, osw.cl = NULL)  "
"makePrediction : function (task.desc, row.names, id, truth, predict.type, predict.threshold = NULL, "
"    y, time, error = NA_character_, dump = NULL)  "
"makePreprocWrapper : function (learner, train, predict, par.set = makeParamSet(), par.vals = list())  "
"makePreprocWrapperCaret : function (learner, ...)  "
"makeRegrTask : function (id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, "
"    coordinates = NULL, fixup.data = \"warn\", check.data = TRUE)  "
"makeRegrTaskDesc : function (id, data, target, weights, blocking, coordinates)  "
"makeRemoveConstantFeaturesWrapper : function (learner, perc = 0, dont.rm = character(0L), na.ignore = FALSE, wrap.tol = .Machine$double.eps^0.5)  "
"makeResampleDesc : function (method, predict = \"test\", ..., stratify = FALSE, stratify.cols = NULL, fixed = FALSE, "
"    blocking.cv = FALSE)  "
"makeResampleInstance : function (desc, task, size, ...)  "
"makeRLearner : function ()  "
"makeRLearnerClassif : function (cl, package, par.set, par.vals = list(), properties = character(0L), name = cl, "
"    short.name = cl, note = \"\", class.weights.param = NULL, callees = character(0L))  "
"makeRLearnerCluster : function (cl, package, par.set, par.vals = list(), properties = character(0L), name = cl, "
"    short.name = cl, note = \"\", callees = character(0L))  "
"makeRLearnerCostSens : function (cl, package, par.set, par.vals = list(), properties = character(0L), name = cl, "
"    short.name = cl, note = \"\", callees = character(0L))  "
"makeRLearnerMultilabel : function (cl, package, par.set, par.vals = list(), properties = character(0L), name = cl, "
"    short.name = cl, note = \"\", callees = character(0L))  "
"makeRLearnerRegr : function (cl, package, par.set, par.vals = list(), properties = character(0L), name = cl, "
"    short.name = cl, note = \"\", callees = character(0L))  "
"makeRLearnerSurv : function (cl, package, par.set, par.vals = list(), properties = character(0L), name = cl, "
"    short.name = cl, note = \"\", callees = character(0L))  "
"makeSMOTEWrapper : function (learner, sw.rate = 1, sw.nn = 5L, sw.standardize = TRUE, sw.alt.logic = FALSE)  "
"makeStackedLearner : function (base.learners, super.learner = NULL, predict.type = NULL, method = \"stack.nocv\", "
"    use.feat = FALSE, resampling = NULL, parset = list())  "
"makeSurvTask : function (id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, "
"    coordinates = NULL, fixup.data = \"warn\", check.data = TRUE)  "
"makeSurvTaskDesc : function (id, data, target, weights, blocking, coordinates)  "
"makeTaskDescInternal : function (type, id, data, target, weights, blocking, coordinates)  "
"makeTuneControlCMAES : function (same.resampling.instance = TRUE, impute.val = NULL, start = NULL, tune.threshold = FALSE, "
"    tune.threshold.args = list(), log.fun = \"default\", final.dw.perc = NULL, budget = NULL, "
"    ...)  "
"makeTuneControlDesign : function (same.resampling.instance = TRUE, impute.val = NULL, design = NULL, tune.threshold = FALSE, "
"    tune.threshold.args = list(), log.fun = \"default\")  "
"makeTuneControlGenSA : function (same.resampling.instance = TRUE, impute.val = NULL, start = NULL, tune.threshold = FALSE, "
"    tune.threshold.args = list(), log.fun = \"default\", final.dw.perc = NULL, budget = NULL, "
"    ...)  "
"makeTuneControlGrid : function (same.resampling.instance = TRUE, impute.val = NULL, resolution = 10L, tune.threshold = FALSE, "
"    tune.threshold.args = list(), log.fun = \"default\", final.dw.perc = NULL, budget = NULL)  "
"makeTuneControlIrace : function (impute.val = NULL, n.instances = 100L, show.irace.output = FALSE, tune.threshold = FALSE, "
"    tune.threshold.args = list(), log.fun = \"default\", final.dw.perc = NULL, budget = NULL, "
"    ...)  "
"makeTuneControlMBO : function (same.resampling.instance = TRUE, impute.val = NULL, learner = NULL, mbo.control = NULL, "
"    tune.threshold = FALSE, tune.threshold.args = list(), continue = FALSE, log.fun = \"default\", "
"    final.dw.perc = NULL, budget = NULL, mbo.design = NULL)  "
"makeTuneControlRandom : function (same.resampling.instance = TRUE, maxit = NULL, tune.threshold = FALSE, tune.threshold.args = list(), "
"    log.fun = \"default\", final.dw.perc = NULL, budget = NULL)  "
"makeTuneMultiCritControlGrid : function (same.resampling.instance = TRUE, resolution = 10L, log.fun = \"default\", "
"    final.dw.perc = NULL, budget = NULL)  "
"makeTuneMultiCritControlMBO : function (n.objectives = mbo.control$n.objectives, same.resampling.instance = TRUE, "
"    impute.val = NULL, learner = NULL, mbo.control = NULL, tune.threshold = FALSE, "
"    tune.threshold.args = list(), continue = FALSE, log.fun = \"default\", final.dw.perc = NULL, "
"    budget = NULL, mbo.design = NULL)  "
"makeTuneMultiCritControlNSGA2 : function (same.resampling.instance = TRUE, impute.val = NULL, log.fun = \"default\", "
"    final.dw.perc = NULL, budget = NULL, ...)  "
"makeTuneMultiCritControlRandom : function (same.resampling.instance = TRUE, maxit = 100L, log.fun = \"default\", final.dw.perc = NULL, "
"    budget = NULL)  "
"makeTuneWrapper : function (learner, resampling, measures, par.set, control, show.info = getMlrOption(\"show.info\"))  "
"makeUndersampleWrapper : function (learner, usw.rate = 1, usw.cl = NULL)  "
"makeWeightedClassesWrapper : function (learner, wcw.param = NULL, wcw.weight = 1)  "
"makeWrappedModel : function (learner, learner.model, task.desc, subset, features, factor.levels, time)  "
"mape : List of 10"
" $ id        : chr \"mape\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"regr\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Mean absolute percentage error\""
" $ note      : chr \"Defined as the abs(truth_i - response_i) / truth_i. Won't work if any truth value is equal to zero. In this cas\"| __truncated__"
" $ aggr      :List of 4"
"mcc : List of 10"
" $ id        : chr \"mcc\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"classif\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num -1"
" $ name      : chr \"Matthews correlation coefficient\""
" $ note      : chr \"Defined as (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)), denominator set to 1 if 0\""
" $ aggr      :List of 4"
"mcp : List of 10"
" $ id        : chr \"mcp\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"costsens\" \"req.pred\" \"req.task\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Misclassification penalty\""
" $ note      : chr \"Average difference between costs of oracle and model prediction.\""
" $ aggr      :List of 4"
"meancosts : List of 10"
" $ id        : chr \"meancosts\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"costsens\" \"req.pred\" \"req.task\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Mean costs of the predicted choices\""
" $ note      : chr \"Defined as: mean(y), where y is the vector of costs for the predicted classes.\""
" $ aggr      :List of 4"
"measureACC : function (truth, response)  "
"measureAU1P : function (probabilities, truth)  "
"measureAU1U : function (probabilities, truth)  "
"measureAUC : function (probabilities, truth, negative, positive)  "
"measureAUNP : function (probabilities, truth)  "
"measureAUNU : function (probabilities, truth)  "
"measureBAC : function (truth, response)  "
"measureBER : function (truth, response)  "
"measureBrier : function (probabilities, truth, negative, positive)  "
"measureBrierScaled : function (probabilities, truth, negative, positive)  "
"measureEXPVAR : function (truth, response)  "
"measureF1 : function (truth, response, positive)  "
"measureFDR : function (truth, response, positive)  "
"measureFN : function (truth, response, negative)  "
"measureFNR : function (truth, response, negative, positive)  "
"measureFP : function (truth, response, positive)  "
"measureFPR : function (truth, response, negative, positive)  "
"measureGMEAN : function (truth, response, negative, positive)  "
"measureGPR : function (truth, response, positive)  "
"measureKAPPA : function (truth, response)  "
"measureKendallTau : function (truth, response)  "
"measureLogloss : function (probabilities, truth)  "
"measureLSR : function (probabilities, truth)  "
"measureMAE : function (truth, response)  "
"measureMAPE : function (truth, response)  "
"measureMCC : function (truth, response, negative, positive)  "
"measureMEDAE : function (truth, response)  "
"measureMEDSE : function (truth, response)  "
"measureMMCE : function (truth, response)  "
"measureMSE : function (truth, response)  "
"measureMSLE : function (truth, response)  "
"measureMulticlassBrier : function (probabilities, truth)  "
"measureMultilabelACC : function (truth, response)  "
"measureMultilabelF1 : function (truth, response)  "
"measureMultilabelHamloss : function (truth, response)  "
"measureMultilabelPPV : function (truth, response)  "
"measureMultilabelSubset01 : function (truth, response)  "
"measureMultilabelTPR : function (truth, response)  "
"measureNPV : function (truth, response, negative)  "
"measurePPV : function (truth, response, positive, probabilities = NULL)  "
"measureQSR : function (probabilities, truth)  "
"measureRAE : function (truth, response)  "
"measureRMSE : function (truth, response)  "
"measureRMSLE : function (truth, response)  "
"measureRRSE : function (truth, response)  "
"measureRSQ : function (truth, response)  "
"measureSAE : function (truth, response)  "
"measureSpearmanRho : function (truth, response)  "
"measureSSE : function (truth, response)  "
"measureSSR : function (probabilities, truth)  "
"measureTN : function (truth, response, negative)  "
"measureTNR : function (truth, response, negative)  "
"measureTP : function (truth, response, positive)  "
"measureTPR : function (truth, response, positive)  "
"measureWKAPPA : function (truth, response)  "
"medae : List of 10"
" $ id        : chr \"medae\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"regr\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Median of absolute errors\""
" $ note      : chr \"Defined as: median(abs(response - truth)).\""
" $ aggr      :List of 4"
"medse : List of 10"
" $ id        : chr \"medse\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"regr\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Median of squared errors\""
" $ note      : chr \"Defined as: median((response - truth)^2).\""
" $ aggr      :List of 4"
"mergeBenchmarkResults : function (bmrs)  "
"mergeSmallFactorLevels : function (task, cols = NULL, min.perc = 0.01, new.level = \".merged\")  "
"mmce : List of 10"
" $ id        : chr \"mmce\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:4] \"classif\" \"classif.multi\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num 1"
" $ name      : chr \"Mean misclassification error\""
" $ note      : chr \"Defined as: mean(response != truth)\""
" $ aggr      :List of 4"
"mse : List of 10"
" $ id        : chr \"mse\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"regr\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Mean of squared errors\""
" $ note      : chr \"Defined as: mean((response - truth)^2)\""
" $ aggr      :List of 4"
"msle : List of 10"
" $ id        : chr \"msle\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"regr\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Mean squared logarithmic error\""
" $ note      : chr \"Defined as: mean((log(response + 1, exp(1)) - log(truth + 1, exp(1)))^2).\n  This measure is mostly used for co\"| __truncated__"
" $ aggr      :List of 4"
"mtcars.task : List of 6"
" $ type       : chr \"cluster\""
" $ env        :<environment: 0x000001db85f28958> "
" $ weights    : NULL"
" $ blocking   : NULL"
" $ coordinates: NULL"
" $ task.desc  :List of 9"
"multiclass.au1p : List of 10"
" $ id        : chr \"multiclass.au1p\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:5] \"classif\" \"classif.multi\" \"req.pred\" \"req.truth\" ..."
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0.5"
" $ name      : chr \"Weighted average 1 vs. 1 multiclass AUC\""
" $ note      : chr \"Computes AUC of c(c - 1) binary classifiers while considering the a priori distribution of the classes. See Fer\"| __truncated__"
" $ aggr      :List of 4"
"multiclass.au1u : List of 10"
" $ id        : chr \"multiclass.au1u\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:5] \"classif\" \"classif.multi\" \"req.pred\" \"req.truth\" ..."
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0.5"
" $ name      : chr \"Average 1 vs. 1 multiclass AUC\""
" $ note      : chr \"Computes AUC of c(c - 1) binary classifiers (all possible pairwise combinations) while considering uniform dist\"| __truncated__"
" $ aggr      :List of 4"
"multiclass.aunp : List of 10"
" $ id        : chr \"multiclass.aunp\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:5] \"classif\" \"classif.multi\" \"req.pred\" \"req.truth\" ..."
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0.5"
" $ name      : chr \"Weighted average 1 vs. rest multiclass AUC\""
" $ note      : chr \"Computes the AUC treating a c-dimensional classifier as c two-dimensional classifiers, taking into account the \"| __truncated__"
" $ aggr      :List of 4"
"multiclass.aunu : List of 10"
" $ id        : chr \"multiclass.aunu\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:5] \"classif\" \"classif.multi\" \"req.pred\" \"req.truth\" ..."
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0.5"
" $ name      : chr \"Average 1 vs. rest multiclass AUC\""
" $ note      : chr \"Computes the AUC treating a c-dimensional classifier as c two-dimensional classifiers, where classes are assume\"| __truncated__"
" $ aggr      :List of 4"
"multiclass.brier : List of 10"
" $ id        : chr \"multiclass.brier\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:5] \"classif\" \"classif.multi\" \"req.pred\" \"req.truth\" ..."
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num 2"
" $ name      : chr \"Multiclass Brier score\""
" $ note      : chr \"Defined as: (1/n) sum_i sum_j (y_ij - p_ij)^2, where y_ij = 1 if observation i has class j (else 0), and p_ij i\"| __truncated__"
" $ aggr      :List of 4"
"multilabel.acc : List of 10"
" $ id        : chr \"multilabel.acc\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"multilabel\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"Accuracy (multilabel)\""
" $ note      : chr \"Averaged proportion of correctly predicted labels with respect to the total number of labels for each instance,\"| __truncated__"
" $ aggr      :List of 4"
"multilabel.f1 : List of 10"
" $ id        : chr \"multilabel.f1\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"multilabel\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"F1 measure (multilabel)\""
" $ note      : chr \"Harmonic mean of precision and recall on a per instance basis (Micro-F1), following the\n  definition by Montan\"| __truncated__"
" $ aggr      :List of 4"
"multilabel.hamloss : List of 10"
" $ id        : chr \"multilabel.hamloss\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"multilabel\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num 1"
" $ name      : chr \"Hamming loss\""
" $ note      : chr \"Proportion of labels that are predicted incorrectly, following the definition\n  by Charte and Charte: https://\"| __truncated__"
" $ aggr      :List of 4"
"multilabel.ppv : List of 10"
" $ id        : chr \"multilabel.ppv\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"multilabel\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"Positive predictive value (multilabel)\""
" $ note      : chr \"Also called precision. Averaged ratio of correctly predicted labels for each instance,\n  following the definit\"| __truncated__"
" $ aggr      :List of 4"
"multilabel.subset01 : List of 10"
" $ id        : chr \"multilabel.subset01\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"multilabel\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num 1"
" $ name      : chr \"Subset-0-1 loss\""
" $ note      : chr \"Proportion of observations where the complete multilabel set (all 0-1-labels) is predicted incorrectly,\n  foll\"| __truncated__"
" $ aggr      :List of 4"
"multilabel.tpr : List of 10"
" $ id        : chr \"multilabel.tpr\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"multilabel\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"TPR (multilabel)\""
" $ note      : chr \"Also called recall. Averaged proportion of predicted labels which are relevant for each instance,\n  following \"| __truncated__"
" $ aggr      :List of 4"
"normalizeFeatures : function (obj, target = character(0L), method = \"standardize\", cols = NULL, range = c(0, "
"    1), on.constant = \"quiet\")  "
"npv : List of 10"
" $ id        : chr \"npv\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"classif\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"Negative predictive value\""
" $ note      : chr \"Defined as: tn / (tn + fn).\""
" $ aggr      :List of 4"
"oversample : function (task, rate, cl = NULL)  "
"performance : function (pred, measures, task = NULL, model = NULL, feats = NULL, simpleaggr = FALSE)  "
"phoneme.task : List of 6"
" $ type       : chr \"classif\""
" $ env        :<environment: 0x000001db87209888> "
" $ weights    : NULL"
" $ blocking   : NULL"
" $ coordinates: NULL"
" $ task.desc  :List of 13"
"pid.task : List of 6"
" $ type       : chr \"classif\""
" $ env        :<environment: 0x000001db873d7980> "
" $ weights    : NULL"
" $ blocking   : NULL"
" $ coordinates: NULL"
" $ task.desc  :List of 13"
"plotBMRBoxplots : function (bmr, measure = NULL, style = \"box\", order.lrns = NULL, order.tsks = NULL, "
"    pretty.names = TRUE, facet.wrap.nrow = NULL, facet.wrap.ncol = NULL)  "
"plotBMRRanksAsBarChart : function (bmr, measure = NULL, ties.method = \"average\", aggregation = \"default\", pos = \"stack\", "
"    order.lrns = NULL, order.tsks = NULL, pretty.names = TRUE)  "
"plotBMRSummary : function (bmr, measure = NULL, trafo = \"none\", order.tsks = NULL, pointsize = 4L, "
"    jitter = 0.05, pretty.names = TRUE)  "
"plotCalibration : function (obj, smooth = FALSE, reference = TRUE, rag = TRUE, facet.wrap.nrow = NULL, "
"    facet.wrap.ncol = NULL)  "
"plotCritDifferences : function (obj, baseline = NULL, pretty.names = TRUE)  "
"plotFilterValues : function (fvalues, sort = \"dec\", n.show = nrow(fvalues$data), filter = NULL, feat.type.cols = FALSE)  "
"plotHyperParsEffect : function (hyperpars.effect.data, x = NULL, y = NULL, z = NULL, plot.type = \"scatter\", "
"    loess.smooth = FALSE, facet = NULL, global.only = TRUE, interpolate = NULL, show.experiments = FALSE, "
"    show.interpolated = FALSE, nested.agg = mean, partial.dep.learn = NULL)  "
"plotLearnerPrediction : function (learner, task, features = NULL, measures, cv = 10L, ..., gridsize, pointsize = 2, "
"    prob.alpha = TRUE, se.band = TRUE, err.mark = \"train\", bg.cols = c(\"darkblue\", "
"        \"green\", \"darkred\"), err.col = \"white\", err.size = pointsize, greyscale = FALSE, "
"    pretty.names = TRUE)  "
"plotLearningCurve : function (obj, facet = \"measure\", pretty.names = TRUE, facet.wrap.nrow = NULL, facet.wrap.ncol = NULL)  "
"plotPartialDependence : function (obj, geom = \"line\", facet = NULL, facet.wrap.nrow = NULL, facet.wrap.ncol = NULL, "
"    p = 1, data = NULL)  "
"plotResiduals : function (obj, type = \"scatterplot\", loess.smooth = TRUE, rug = TRUE, pretty.names = TRUE)  "
"plotROCCurves : function (obj, measures, diagonal = TRUE, pretty.names = TRUE, facet.learner = FALSE)  "
"plotThreshVsPerf : function (obj, measures = obj$measures, facet = \"measure\", mark.th = NA_real_, pretty.names = TRUE, "
"    facet.wrap.nrow = NULL, facet.wrap.ncol = NULL)  "
"plotTuneMultiCritResult : function (res, path = TRUE, col = NULL, shape = NULL, pointsize = 2, pretty.names = TRUE)  "
"ppv : List of 10"
" $ id        : chr \"ppv\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"classif\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"Positive predictive value\""
" $ note      : chr \"Defined as: tp / (tp + fp). Also called precision. If the denominator is 0, PPV is set to be either 1 or 0 depe\"| __truncated__"
" $ aggr      :List of 4"
"predictLearner : function (.learner, .model, .newdata, ...)  "
"qsr : List of 10"
" $ id        : chr \"qsr\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:4] \"classif\" \"classif.multi\" \"req.truth\" \"req.prob\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num -1"
" $ name      : chr \"Quadratic Scoring Rule\""
" $ note      : chr \"Defined as: 1 - (1/n) sum_i sum_j (y_ij - p_ij)^2, where y_ij = 1 if observation i has class j (else 0), and p_\"| __truncated__"
" $ aggr      :List of 4"
"rae : List of 10"
" $ id        : chr \"rae\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"regr\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Relative absolute error\""
" $ note      : chr \"Defined as sum_of_absolute_errors / mean_absolute_deviation. Undefined for single instances and when every trut\"| __truncated__"
" $ aggr      :List of 4"
"reduceBatchmarkResults : function (ids = NULL, keep.pred = TRUE, keep.extract = FALSE, show.info = getMlrOption(\"show.info\"), "
"    reg = batchtools::getDefaultRegistry())  "
"reextractFDAFeatures : function (obj, desc, ...)  "
"reimpute : function (obj, desc)  "
"removeConstantFeatures : function (obj, perc = 0, dont.rm = character(0L), na.ignore = FALSE, wrap.tol = .Machine$double.eps^0.5, "
"    show.info = getMlrOption(\"show.info\"), ...)  "
"removeHyperPars : function (learner, ids = character(0L))  "
"repcv : function (learner, task, folds = 10L, reps = 10L, stratify = FALSE, measures, models = FALSE, "
"    keep.pred = TRUE, ..., show.info = getMlrOption(\"show.info\"))  "
"resample : function (learner, task, resampling, measures, weights = NULL, models = FALSE, extract, "
"    keep.pred = TRUE, ..., show.info = getMlrOption(\"show.info\"))  "
"rmse : List of 10"
" $ id        : chr \"rmse\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"regr\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Root mean squared error\""
" $ note      : chr \"The RMSE is aggregated as sqrt(mean(rmse.vals.on.test.sets^2)). If you don't want that, you could also use `test.mean`.\""
" $ aggr      :List of 4"
"rmsle : List of 10"
" $ id        : chr \"rmsle\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"regr\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Root mean squared logarithmic error\""
" $ note      : chr \"Defined as: sqrt(msle). Definition taken from:\n  Definition taken from: https: / /www.kaggle.com / wiki / Root\"| __truncated__"
" $ aggr      :List of 4"
"rrse : List of 10"
" $ id        : chr \"rrse\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"regr\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Root relative squared error\""
" $ note      : chr \"Defined as sqrt (sum_of_squared_errors / total_sum_of_squares). Undefined for single instances and when every t\"| __truncated__"
" $ aggr      :List of 4"
"rsq : List of 10"
" $ id        : chr \"rsq\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"regr\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num -Inf"
" $ name      : chr \"Coefficient of determination\""
" $ note      : chr \"Also called R-squared, which is 1 - residual_sum_of_squares / total_sum_of_squares.\""
" $ aggr      :List of 4"
"sae : List of 10"
" $ id        : chr \"sae\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"regr\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Sum of absolute errors\""
" $ note      : chr \"Defined as: sum(abs(response - truth))\""
" $ aggr      :List of 4"
"selectFeatures : function (learner, task, resampling, measures, bit.names, bits.to.features, control, "
"    show.info = getMlrOption(\"show.info\"))  "
"setAggregation : function (measure, aggr)  "
"setHyperPars : function (learner, ..., par.vals = list())  "
"setHyperPars2 : function (learner, par.vals)  "
"setId : function (learner, id)  "
"setLearnerId : function (learner, id)  "
"setMeasurePars : function (measure, ..., par.vals = list())  "
"setPredictThreshold : function (learner, predict.threshold)  "
"setPredictType : function (learner, predict.type)  "
"setThreshold : function (pred, threshold)  "
"silhouette : List of 10"
" $ id        : chr \"silhouette\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"cluster\" \"req.pred\" \"req.feats\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num Inf"
" $ worst     : num 0"
" $ name      : chr \"Rousseeuw's silhouette internal cluster quality index\""
" $ note      : chr \"Silhouette value of an observation is a measure of how similar an object is to its own cluster compared to othe\"| __truncated__"
" $ aggr      :List of 4"
"simplifyMeasureNames : function (xs)  "
"smote : function (task, rate, nn = 5L, standardize = TRUE, alt.logic = FALSE)  "
"sonar.task : List of 6"
" $ type       : chr \"classif\""
" $ env        :<environment: 0x000001db88608c00> "
" $ weights    : NULL"
" $ blocking   : NULL"
" $ coordinates: NULL"
" $ task.desc  :List of 13"
"spam.task : List of 6"
" $ type       : chr \"classif\""
" $ env        :<environment: 0x000001db886f8978> "
" $ weights    : NULL"
" $ blocking   : NULL"
" $ coordinates: NULL"
" $ task.desc  :List of 13"
"spatial.task : List of 6"
" $ type       : chr \"classif\""
" $ env        :<environment: 0x000001db88a0baa0> "
" $ weights    : NULL"
" $ blocking   : NULL"
" $ coordinates:'data.frame':	751 obs. of  2 variables:"
" $ task.desc  :List of 13"
"spearmanrho : List of 10"
" $ id        : chr \"spearmanrho\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"regr\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num -1"
" $ name      : chr \"Spearman's rho\""
" $ note      : chr \"Defined as: Spearman's rho correlation between truth and response. Only looks at the order.\n  See Rosset et al\"| __truncated__"
" $ aggr      :List of 4"
"sse : List of 10"
" $ id        : chr \"sse\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:3] \"regr\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Sum of squared errors\""
" $ note      : chr \"Defined as: sum((response - truth)^2)\""
" $ aggr      :List of 4"
"ssr : List of 10"
" $ id        : chr \"ssr\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:4] \"classif\" \"classif.multi\" \"req.truth\" \"req.prob\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"Spherical Scoring Rule\""
" $ note      : chr \"Defined as: mean(p_i(sum_j(p_ij))), where p_i is the predicted probability of the true class of observation i a\"| __truncated__"
" $ aggr      :List of 4"
"subsample : function (learner, task, iters = 30, split = 2/3, stratify = FALSE, measures, models = FALSE, "
"    keep.pred = TRUE, ..., show.info = getMlrOption(\"show.info\"))  "
"subsetTask : function (task, subset = NULL, features)  "
"summarizeColumns : function (obj)  "
"summarizeLevels : function (obj, cols = NULL)  "
"test.join : List of 4"
" $ id        : chr \"test.join\""
" $ name      : chr \"Test join\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.test\""
"test.max : List of 4"
" $ id        : chr \"test.max\""
" $ name      : chr \"Test maximum\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.test\""
"test.mean : List of 4"
" $ id        : chr \"test.mean\""
" $ name      : chr \"Test mean\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.test\""
"test.median : List of 4"
" $ id        : chr \"test.median\""
" $ name      : chr \"Test median\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.test\""
"test.min : List of 4"
" $ id        : chr \"test.min\""
" $ name      : chr \"Test minimum\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.test\""
"test.range : List of 4"
" $ id        : chr \"test.range\""
" $ name      : chr \"Test range\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.test\""
"test.rmse : List of 4"
" $ id        : chr \"test.rmse\""
" $ name      : chr \"Test RMSE\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.test\""
"test.sd : List of 4"
" $ id        : chr \"test.sd\""
" $ name      : chr \"Test sd\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.test\""
"test.sum : List of 4"
" $ id        : chr \"test.sum\""
" $ name      : chr \"Test sum\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.test\""
"testgroup.mean : List of 4"
" $ id        : chr \"testgroup.mean\""
" $ name      : chr \"Test group mean\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.test\""
"testgroup.sd : List of 4"
" $ id        : chr \"testgroup.sd\""
" $ name      : chr \"Test group standard deviation\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.test\""
"timeboth : List of 10"
" $ id        : chr \"timeboth\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:9] \"classif\" \"classif.multi\" \"multilabel\" \"regr\" ..."
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"timetrain + timepredict\""
" $ note      : chr \"\""
" $ aggr      :List of 4"
"timepredict : List of 10"
" $ id        : chr \"timepredict\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:8] \"classif\" \"classif.multi\" \"multilabel\" \"regr\" ..."
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Time of predicting test set\""
" $ note      : chr \"\""
" $ aggr      :List of 4"
"timetrain : List of 10"
" $ id        : chr \"timetrain\""
" $ minimize  : logi TRUE"
" $ properties: chr [1:8] \"classif\" \"classif.multi\" \"multilabel\" \"regr\" ..."
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 0"
" $ worst     : num Inf"
" $ name      : chr \"Time of fitting the model\""
" $ note      : chr \"\""
" $ aggr      :List of 4"
"tn : List of 10"
" $ id        : chr \"tn\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"classif\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num Inf"
" $ worst     : num 0"
" $ name      : chr \"True negatives\""
" $ note      : chr \"Sum of correctly classified observations in the negative class. Also called correct rejections.\""
" $ aggr      :List of 4"
"tnr : List of 10"
" $ id        : chr \"tnr\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"classif\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"True negative rate\""
" $ note      : chr \"Percentage of correctly classified observations in the negative class. Also called specificity.\""
" $ aggr      :List of 4"
"tp : List of 10"
" $ id        : chr \"tp\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"classif\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num Inf"
" $ worst     : num 0"
" $ name      : chr \"True positives\""
" $ note      : chr \"Sum of all correctly classified observations in the positive class.\""
" $ aggr      :List of 4"
"tpr : List of 10"
" $ id        : chr \"tpr\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:3] \"classif\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num 0"
" $ name      : chr \"True positive rate\""
" $ note      : chr \"Percentage of correctly classified observations in the positive class. Also called hit rate or recall or sensitivity.\""
" $ aggr      :List of 4"
"train : function (learner, task, subset = NULL, weights = NULL)  "
"train.max : List of 4"
" $ id        : chr \"train.max\""
" $ name      : chr \"Training max\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.train\""
"train.mean : List of 4"
" $ id        : chr \"train.mean\""
" $ name      : chr \"Training mean\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.train\""
"train.median : List of 4"
" $ id        : chr \"train.median\""
" $ name      : chr \"Training median\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.train\""
"train.min : List of 4"
" $ id        : chr \"train.min\""
" $ name      : chr \"Training min\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.train\""
"train.range : List of 4"
" $ id        : chr \"train.range\""
" $ name      : chr \"Training range\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.train\""
"train.rmse : List of 4"
" $ id        : chr \"train.rmse\""
" $ name      : chr \"Training RMSE\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.train\""
"train.sd : List of 4"
" $ id        : chr \"train.sd\""
" $ name      : chr \"Training sd\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.train\""
"train.sum : List of 4"
" $ id        : chr \"train.sum\""
" $ name      : chr \"Training sum\""
" $ fun       :function (task, perf.test, perf.train, measure, group, pred)  "
" $ properties: chr \"req.train\""
"trainLearner : function (.learner, .task, .subset, .weights = NULL, ...)  "
"tuneParams : function (learner, task, resampling, measures, par.set, control, show.info = getMlrOption(\"show.info\"), "
"    resample.fun = resample)  "
"tuneParamsMultiCrit : function (learner, task, resampling, measures, par.set, control, show.info = getMlrOption(\"show.info\"), "
"    resample.fun = resample)  "
"tuneThreshold : function (pred, measure, task, model, nsub = 20L, control = list())  "
"undersample : function (task, rate, cl = NULL)  "
"wkappa : List of 10"
" $ id        : chr \"wkappa\""
" $ minimize  : logi FALSE"
" $ properties: chr [1:4] \"classif\" \"classif.multi\" \"req.pred\" \"req.truth\""
" $ fun       :function (task, model, pred, feats, extra.args)  "
" $ extra.args: list()"
" $ best      : num 1"
" $ worst     : num -1"
" $ name      : chr \"Mean quadratic weighted kappa\""
" $ note      : chr \"Defined as: 1 - sum(weights * conf.mat) / sum(weights * expected.mat),\n    the weight matrix measures seriousn\"| __truncated__"
" $ aggr      :List of 4"
"wpbc.task : List of 6"
" $ type       : chr \"surv\""
" $ env        :<environment: 0x000001db8acf3308> "
" $ weights    : NULL"
" $ blocking   : NULL"
" $ coordinates: NULL"
" $ task.desc  :List of 9"
"yeast.task : List of 6"
" $ type       : chr \"multilabel\""
" $ env        :<environment: 0x000001db8adb4320> "
" $ weights    : NULL"
" $ blocking   : NULL"
" $ coordinates: NULL"
" $ task.desc  :List of 10"


library(multicolor)
%>% : function (lhs, rhs)  
  center_string : function (string, remove_last_break = TRUE, display = FALSE)  
    crawl : function (txt = "hello world!", colors = NULL, recycle_chars = FALSE, direction = "vertical", 
                      pause = 0.05, ...)  
      insert_rainbow : function (clr)  
        multi_color : function (txt = "hello world!", colors = "rainbow", type = "message", direction = "vertical", 
                                recycle_chars = FALSE, add_leading_newline = FALSE, ...)  
          multi_colour : function (txt = "hello world!", colors = "rainbow", type = "message", direction = "vertical", 
                                   recycle_chars = FALSE, add_leading_newline = FALSE, ...)  
            multicolor_logo : function (colors = "random", ...)  
              nix_first_newline : function (s)  
                palettes : List of 4
$ lacroix      : chr [1:10] "#EF7C12" "#F4B95A" "#009F3F" "#8FDA04" ...
$ magma        : chr [1:10] "#51127CE6" "#6B1D81E6" "#852781E6" "#A1307EE6" ...
$ grandbudapest: chr [1:10] "#F1BB7B" "#F59E74" "#F9816D" "#FD6467" ...
$ ghibli       : chr [1:7] "#4D4140" "#596F7E" "#168B98" "#ED5B67" ...
starwars_intro :  chr "It is a period of civil war. Rebel spaceships, striking from a hidden base, have won their first victory agains"| __truncated__
things : List of 45
$ cow         : chr "\n ----- \n%s \n ------ \n    \\   ^__^ \n     \\  (oo)\\ ________ \n        (__)\\         )\\ /\\ \n         "| __truncated__
$ chicken     : chr "\n ----- \n%s \n ------ \n    \\   \n     \\\n         _\n       _/ }\n      `>' \\\n      `|   \\\n       |   "| __truncated__
$ chuck       : chr "\n ----- \n%s \n ------ \n    \\   \n     \\\n                      @@@@@@@@@@@@@@                       \n    "| __truncated__
$ clippy      : chr "\n\n ----- \n%s \n ------ \n    \\   \n     \\   __\n   / \\\n   | |\n   @ @\n  || ||\n  || ||\n  |\\_/|\n  \\___/ GB\n"
$ poop        : chr "\n ----- \n%s \n ------ \n    \\   \n     \\\n     (   )\n  (   ) (\n   ) _   )\n    ( \\_\n  _(_\\ \\)__\n (__"| __truncated__
$ bigcat      : chr "\n ----- \n%s \n ------ \n    \\   \n     \\\n                \\`*-.\n                 )  _`-.\n               "| __truncated__
$ ant         : chr "\n ----- \n%s \n ------ \n    \\   \n     \\\n       '\\__\n      (o )     ___\n      <>(_)(_)(___)\n        < "| __truncated__
$ pumpkin     : chr "\n ----- \n%s \n ------ \n    \\   \n     \\\n                  ___\n               ___)__|_\n          .-*'   "| __truncated__
$ ghost       : chr "\n ----- \n%s \n ------ \n    \\   \n     \\\n     .-.\n    (o o)\n    | O \\\n     \\   \\\n      `~~~' [nosig]\n  "
$ spider      : chr "\n ----- \n%s \n ------ \n    \\   \n     \\\n              |\n              |\n              |\n             _"| __truncated__
$ rabbit      : chr "\n ----- \n%s \n ------ \n    \\   \n     \\\n      ( )_( )\n      (='.'=)\n      (^)_(^) [nosig]\n  "
$ pig         : chr "\n ----- \n%s \n ------ \n    \\   \n     \\\n       _//| .-~~~-.\n     _/oo  }       }-@\n    ('')_  }       |"| __truncated__
$ snowman     : chr "\n ----- \n%s \n ------ \n    \\   \n     \\\n     _[_]_\n      (\")\n  >--( : )--<\n    (__:__) [nosig]\n  "
$ frog        : chr "\n ----- \n%s \n ------ \n    \\   \n     \\\n        (.)_(.)\n     _ (   _   ) _\n    / \\/`-----'\\/ \\\n  __"| __truncated__
$ hypnotoad   : chr "\n ----- \n%s \n ------\n    \\          ,'``.._   ,'``.\n     \\        :,--._:)\\,:,._,.:\n      \\       :`-"| __truncated__
$ shortcat    : chr "\n ----- \n%s \n ------ \n    \\   \n     \\\n    .ﾊ,,ﾊ\n    ( ﾟωﾟ)\n    |つ  つ\n    U \" U\n        [BoingBoing]\n    "
$ longcat     : chr "\n ----- \n%s \n ------ \n    \\   \n     \\\n    .ﾊ,,ﾊ\n    ( ﾟωﾟ)\n    |つ  つ\n%s\n    U \"  U\n        [BoingBoing]\n    "
$ fish        : chr "\n ----- \n%s \n ------ \n    \\   \n     \\\n  ><((((º>  ><((((º>  ><((((º>  ><((((º>  ><((((º>\n      Kiyoko Gotanda\n    "
$ signbunny   : chr "\n -------------- \n%s \n --------------\n(\\__/) ||\n(•ㅅ•) ||\n/   づ\n          [nosig]\n    "
$ facecat     : chr "\n -------------- \n%s \n --------------\n    \\\n      \\\n         /\\ /\\\n         (O o)\n        =(:^:)=\n"| __truncated__
$ behindcat   : chr "\n -------------- \n%s \n --------------\n      \\\n        \\\n          \\\n            |\\___/|\n           "| __truncated__
$ stretchycat : chr "\n -------------- \n%s \n --------------\n    \\\n      \\\n        \\\n                        ,/|         _.-"| __truncated__
$ anxiouscat  : chr "\n -------------- \n%s \n --------------\n    \\\n      \\\n        \\\n        /\\_/\\         _\n       /``  "| __truncated__
$ longtailcat : chr "\n -------------- \n%s \n --------------\n    \\\n      \\\n        \\\n     /\\-/\\\n    /a a  \\             "| __truncated__
$ cat         : chr "\n -------------- \n%s \n --------------\n    \\\n      \\\n        \\\n            |\\___/|\n          ==) ^Y^"| __truncated__
$ trilobite   : chr "\n -------------- \n%s \n --------------\n    \\\n      \\\n        \\\n          _____\n       .'` ,-. `'.\n  "| __truncated__
$ shark       : chr "\n -------------- \n%s \n --------------\n    \\\n      \\\n        \\\n              /\"\"-._\n              ."| __truncated__
$ buffalo     : chr "\n -------------- \n%s \n --------------\n    \\\n      \\\n        \\\n                   _.-````'-,_\n       "| __truncated__
$ grumpycat   : chr "\n -------------- \n%s \n --------------\n    \\\n      \\\n        \\\n      ﾊ _ ﾊ\n      ಠ X ಠ\n  "
$ smallcat    : chr "\n -------------- \n%s \n --------------\n    \\\n     \\\n      \\\n         /\\_/\\\n        ( o.o )\n       "| __truncated__
$ yoda        : chr "\n ----- \n%s \n ------ \n    \\   \n     \\\n                   ____\n                _.' :  `._\n            "| __truncated__
$ mushroom    : chr "\n ----- \n%s \n ------ \n    \\   \n     \\  \n      \\\n                ________\n           __--´      ° `--"| __truncated__
$ endlesshorse: chr "\n ----- \n%s \n ------ \n    \\   \n     \\  \n      \\\n       ,\n    _,,)\\.~,,._\n     (()`  ``)\\))),,_\n "| __truncated__
$ bat         : chr "\n ------------- \n%s \n -------------- \n              \\   \n               \\  \n                \\\n       "| __truncated__
$ bat2        : chr "\n ------------- \n%s \n -------------- \n              \\   \n               \\  \n                \\\n_______"| __truncated__
$ turkey      : chr "\n ------------- \n%s \n -------------- \n              \\   \n               \\  \n                \\\n       "| __truncated__
$ monkey      : chr "\n ------------- \n%s \n -------------- \n              \\   \n               \\  \n                \\\n\n     "| __truncated__
$ daemon      : chr "\n ----- \n%s \n ------ \n    \\   \n     \\  \n      \\\n            ,        ,\n           /(        )`\n    "| __truncated__
$ egret       : chr "\n ----- \n%s \n ------ \n    \\   \n     \\  \n      \\\n       \\   _,\n      -==<' `\n          ) /\n       "| __truncated__
$ duckling    : chr "\n ----- \n%s \n ------ \n    \\   \n     \\  \n      \\\n       \\\n\n        >o  .\n         ===    [ab]\n   "| __truncated__
$ duck        : chr "\n ----- \n%s \n ------ \n    \\   \n     \\  \n      \\\n         __\n        /o \\\n      <=   |         ==\n"| __truncated__
$ owl         : chr "\n ----- \n%s \n ------ \n    \\   \n     \\  \n      \\\n       /\\___/\\\n       {o}{o}|\n       \\ v  /|\n  "| __truncated__
$ squirrel    : chr "             ------ \n          %s \n             ------ \n                 \\   \n                  \\  \n    "| __truncated__
$ squirrel2   : chr "             ------ \n          %s \n             ------ \n                 \\   \n                  \\  \n    "| __truncated__
$ rms         : chr "\n\n ----- \n%s \n ------ \n    \\   \n     \\\n                    @@@@@@ @\n                  @@@@     @@\n  "| __truncated__
triangle_string : function (string, maxlen = 1, step = 1, display = FALSE)  
  > 
library(nlme)
ACF : function (object, maxLag, ...)  
  Alfalfa : Classes ‘nmGroupedData’, ‘groupedData’ and 'data.frame':	72 obs. of  4 variables:
  $ Variety: Factor w/ 3 levels "Cossack","Ladak",..: 2 2 2 2 2 2 2 2 2 2 ...
$ Date   : Factor w/ 4 levels "None","S1","S20",..: 1 2 3 4 1 2 3 4 1 2 ...
$ Block  : Factor w/ 6 levels "1","2","3","4",..: 1 1 1 1 2 2 2 2 3 3 ...
$ Yield  : num  2.17 1.58 2.29 2.23 1.88 1.26 1.6 2.01 1.62 1.22 ...
allCoef : function (..., extract = coef)  
  anova.lme : function (object, ..., test = TRUE, type = c("sequential", "marginal"), adjustSigma = TRUE, 
                        Terms, L, verbose = FALSE)  
    asOneFormula : function (..., omit = c(".", "pi"))  
      Assay : Classes ‘nffGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	60 obs. of  4 variables:
  $ Block  : Ord.factor w/ 2 levels "2"<"1": 2 2 2 2 2 2 2 2 2 2 ...
$ sample : Factor w/ 6 levels "a","b","c","d",..: 1 1 1 1 1 2 2 2 2 2 ...
$ dilut  : Factor w/ 5 levels "1","2","3","4",..: 1 2 3 4 5 1 2 3 4 5 ...
$ logDens: num  -0.2332 -0.0566 0.2111 0.3464 0.5939 ...
asTable : function (object)  
  augPred : function (object, primary = NULL, minimum = min(primary), maximum = max(primary), 
                      length.out = 51, ...)  
    balancedGrouped : function (form, data, labels = NULL, units = NULL)  
      bdf : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	2287 obs. of  28 variables:
  $ schoolNR      : Ord.factor w/ 131 levels "47"<"103"<"2"<..: 119 119 119 119 119 119 119 119 119 119 ...
$ pupilNR       : Factor w/ 2287 levels "17001","17002",..: 1 2 3 4 5 6 7 8 9 10 ...
$ IQ.verb       : num  15 14.5 9.5 11 8 9.5 9.5 13 9.5 11 ...
$ IQ.perf       : num  12.33 10 11 10 6.67 ...
$ sex           : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
$ Minority      : Factor w/ 2 levels "N","Y": 1 2 1 1 1 2 2 1 2 2 ...
$ repeatgr      : Ord.factor w/ 3 levels "0"<"1"<"2": 1 1 1 1 1 1 1 1 2 1 ...
$ aritPRET      : num  14 12 10 13 8 8 7 17 10 14 ...
$ classNR       : num  180 180 180 180 180 180 180 180 180 180 ...
$ aritPOST      : num  24 19 24 26 9 13 13 30 23 22 ...
$ langPRET      : num  36 36 33 29 19 22 20 44 34 31 ...
$ langPOST      : num  46 45 33 46 20 30 30 57 36 36 ...
$ ses           : num  23 10 15 23 10 10 23 10 13 15 ...
$ denomina      : Ord.factor w/ 4 levels "1"<"2"<"3"<"4": 1 1 1 1 1 1 1 1 1 1 ...
$ schoolSES     : num  11 11 11 11 11 11 11 11 11 11 ...
$ satiprin      : num  3.43 3.43 3.43 3.43 3.43 ...
$ natitest      : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
$ meetings      : num  1.7 1.7 1.7 1.7 1.7 1.7 1.7 1.7 1.7 1.7 ...
$ currmeet      : num  1.83 1.83 1.83 1.83 1.83 ...
$ mixedgra      : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
$ percmino      : num  60 60 60 60 60 60 60 60 60 60 ...
$ aritdiff      : num  12 12 12 12 12 12 12 12 12 12 ...
$ homework      : num  2.33 2.33 2.33 2.33 2.33 ...
$ classsiz      : num  29 29 29 29 29 29 29 29 29 29 ...
$ groupsiz      : num  29 29 29 29 29 29 29 29 29 29 ...
$ IQ.ver.cen    : num  3.166 2.666 -2.334 -0.834 -3.834 ...
$ avg.IQ.ver.cen: Named num  -1.51 -1.51 -1.51 -1.51 -1.51 ...
$ grpSiz.cen    : num  5.9 5.9 5.9 5.9 5.9 ...
BodyWeight : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	176 obs. of  4 variables:
  $ weight: num  240 250 255 260 262 258 266 266 265 272 ...
$ Time  : num  1 8 15 22 29 36 43 44 50 57 ...
$ Rat   : Ord.factor w/ 16 levels "2"<"3"<"4"<"1"<..: 4 4 4 4 4 4 4 4 4 4 ...
$ Diet  : Factor w/ 3 levels "1","2","3": 1 1 1 1 1 1 1 1 1 1 ...
Cefamandole : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	84 obs. of  3 variables:
  $ Subject: Ord.factor w/ 6 levels "2"<"1"<"6"<"3"<..: 2 2 2 2 2 2 2 2 2 2 ...
$ Time   : num  10 15 20 30 45 60 75 90 120 150 ...
$ conc   : num  127 80 47.4 39.9 24.8 17.9 11.7 10.9 5.7 2.55 ...
coef<- : function (object, ..., value)  
  coefficients<- : function (object, ..., value)  
    collapse : function (object, ...)  
      compareFits : function (object1, object2, which = 1:ncol(object1))  
        comparePred : function (object1, object2, primary = NULL, minimum = min(primary), maximum = max(primary), 
                                length.out = 51, level = NULL, ...)  
          corAR1 : function (value = 0, form = ~1, fixed = FALSE)  
            corARMA : function (value = double(p + q), form = ~1, p = 0, q = 0, fixed = FALSE)  
              corCAR1 : function (value = 0.2, form = ~1, fixed = FALSE)  
                corCompSymm : function (value = 0, form = ~1, fixed = FALSE)  
                  corExp : function (value = numeric(0), form = ~1, nugget = FALSE, metric = c("euclidean", "maximum", 
                                                                                               "manhattan"), fixed = FALSE)  
                    corFactor : function (object, ...)  
                      corGaus : function (value = numeric(0), form = ~1, nugget = FALSE, metric = c("euclidean", "maximum", 
                                                                                                    "manhattan"), fixed = FALSE)  
                        corIdent : function (form = NULL)  
                          corLin : function (value = numeric(0), form = ~1, nugget = FALSE, metric = c("euclidean", "maximum", 
                                                                                                       "manhattan"), fixed = FALSE)  
                            corMatrix : function (object, ...)  
                              corNatural : function (value = numeric(0), form = ~1, fixed = FALSE)  
                                corRatio : function (value = numeric(0), form = ~1, nugget = FALSE, metric = c("euclidean", "maximum", 
                                                                                                               "manhattan"), fixed = FALSE)  
                                  corSpatial : function (value = numeric(0), form = ~1, nugget = FALSE, type = c("spherical", "exponential", 
                                                                                                                 "gaussian", "linear", "rational"), metric = c("euclidean", "maximum", "manhattan"), 
                                                         fixed = FALSE)  
                                    corSpher : function (value = numeric(0), form = ~1, nugget = FALSE, metric = c("euclidean", "maximum", 
                                                                                                                   "manhattan"), fixed = FALSE)  
                                      corSymm : function (value = numeric(0), form = ~1, fixed = FALSE)  
                                        covariate<- : function (object, value)  
                                          Dialyzer : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	140 obs. of  5 variables:
  $ Subject : Ord.factor w/ 20 levels "10"<"8"<"2"<"6"<..: 9 9 9 9 9 9 9 3 3 3 ...
$ QB      : Factor w/ 2 levels "200","300": 1 1 1 1 1 1 1 1 1 1 ...
$ pressure: num  0.24 0.505 0.995 1.485 2.02 ...
$ rate    : num  0.645 20.115 38.46 44.985 51.765 ...
$ index   : num  1 2 3 4 5 6 7 1 2 3 ...
Dim : function (object, ...)  
  Earthquake : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	182 obs. of  5 variables:
  $ Quake   : Ord.factor w/ 23 levels "20"<"16"<"14"<..: 1 1 1 1 1 1 1 1 1 1 ...
$ Richter : num  5 5 5 5 5 5 5 5 5 5 ...
$ distance: num  7.5 8.8 8.9 9.4 9.7 9.7 10.5 10.5 12 12.2 ...
$ soil    : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
$ accel   : num  0.264 0.263 0.23 0.147 0.286 0.157 0.237 0.133 0.055 0.097 ...
ergoStool : Classes ‘nffGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	36 obs. of  3 variables:
  $ effort : num  12 15 12 10 10 14 13 12 7 14 ...
$ Type   : Factor w/ 4 levels "T1","T2","T3",..: 1 2 3 4 1 2 3 4 1 2 ...
$ Subject: Ord.factor w/ 9 levels "8"<"5"<"4"<"9"<..: 8 8 8 8 9 9 9 9 6 6 ...
Fatigue : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	262 obs. of  3 variables:
  $ Path     : Ord.factor w/ 21 levels "1"<"2"<"3"<"4"<..: 1 1 1 1 1 1 1 1 1 1 ...
$ cycles   : num  0 0.01 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09 ...
$ relLength: num  1 1.06 1.11 1.17 1.24 ...
fdHess : function (pars, fun, ..., .relStep = .Machine$double.eps^(1/3), minAbsPar = 0)  
  fixed.effects : function (object, ...)  
    fixef : function (object, ...)  
      gapply : function (object, which, FUN, form = formula(object), level, groups = getGroups(object, 
                                                                                               form, level), ...)  
        Gasoline : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	32 obs. of  6 variables:
  $ yield   : num  6.9 14.4 7.4 8.5 8 2.8 5 12.2 10 15.2 ...
$ endpoint: num  235 307 212 365 218 235 285 205 267 300 ...
$ Sample  : Ord.factor w/ 10 levels "1"<"2"<"7"<"9"<..: 8 9 5 1 3 4 7 10 6 8 ...
$ API     : num  38.4 40.3 40 31.8 40.8 41.3 38.1 50.8 32.2 38.4 ...
$ vapor   : num  6.1 4.8 6.1 0.2 3.5 1.8 1.2 8.6 5.2 6.1 ...
$ ASTM    : num  220 231 217 316 210 267 274 190 236 220 ...
getCovariate : function (object, form = formula(object), data)  
  getCovariateFormula : function (object)  
    getData : function (object)  
      getGroups : function (object, form = formula(object), level, data, sep = "/")  
        getGroupsFormula : function (object, asList = FALSE, sep = "/")  
          getResponse : function (object, form = formula(object))  
            getResponseFormula : function (object)  
              getVarCov : function (obj, ...)  
                gls : function (model, data = sys.frame(sys.parent()), correlation = NULL, weights = NULL, 
                                subset, method = c("REML", "ML"), na.action = na.fail, control = list(), verbose = FALSE)  
                  glsApVar : function (glsSt, sigma, conLin = attr(glsSt, "conLin"), .relStep = .Machine$double.eps^(1/3), 
                                       minAbsPar = 0, natural = TRUE)  
                    glsControl : function (maxIter = 50L, msMaxIter = 200L, tolerance = 1e-06, msTol = 1e-07, msVerbose = FALSE, 
                                           singular.ok = FALSE, returnObject = FALSE, apVar = TRUE, .relStep = .Machine$double.eps^(1/3), 
                                           opt = c("nlminb", "optim"), optimMethod = "BFGS", minAbsParApVar = 0.05, natural = TRUE, 
                                           sigma = NULL)  
                      glsEstimate : function (object, conLin = attr(object, "conLin"), control = list(singular.ok = FALSE))  
                        glsStruct : function (corStruct = NULL, varStruct = NULL)  
                          Glucose : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	378 obs. of  4 variables:
  $ Subject: Ord.factor w/ 6 levels "6"<"2"<"3"<"5"<..: 5 5 5 5 5 5 5 5 5 5 ...
$ Time   : num  -0.25 0 0.5 1 1.5 2 3 4 5 6 ...
$ conc   : num  4.9 4.5 7.84 5.46 5.08 4.32 3.91 3.99 4.15 4.41 ...
$ Meal   : Ord.factor w/ 6 levels "2am"<"6am"<"10am"<..: 3 3 3 3 3 3 3 3 3 3 ...
Glucose2 : Classes ‘nmGroupedData’, ‘groupedData’ and 'data.frame':	196 obs. of  4 variables:
  $ Subject: Factor w/ 7 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
$ Date   : Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...
$ Time   : num  -1 0 2 4 6 8 10 12 15 18 ...
$ glucose: num  3 3 4.7 6 6.3 4.3 3 2 4.5 3.8 ...
gnls : function (model, data = sys.frame(sys.parent()), params, start, correlation = NULL, 
                 weights = NULL, subset, na.action = na.fail, naPattern, control = list(), verbose = FALSE)  
  gnlsControl : function (maxIter = 50, nlsMaxIter = 7, msMaxIter = 50, minScale = 0.001, tolerance = 1e-06, 
                          nlsTol = 0.001, msTol = 1e-07, returnObject = FALSE, msVerbose = FALSE, apVar = TRUE, 
                          .relStep = .Machine$double.eps^(1/3), opt = c("nlminb", "optim"), optimMethod = "BFGS", 
                          minAbsParApVar = 0.05, sigma = NULL)  
    gnlsStruct : function (corStruct = NULL, varStruct = NULL)  
      groupedData : function (formula, data = NULL, order.groups = TRUE, FUN = function(x) max(x, na.rm = TRUE), 
                              outer = NULL, inner = NULL, labels = NULL, units = NULL)  
        gsummary : function (object, FUN = function(x) mean(x, na.rm = TRUE), omitGroupingFactor = FALSE, 
                             form = formula(object), level, groups = getGroups(object, form, level), invariantsOnly = FALSE, 
                             ...)  
          Gun : Classes ‘nffGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	36 obs. of  4 variables:
  $ rounds  : num  20.2 14.2 22 14.1 23.1 14.1 26.2 18 22.6 14 ...
$ Method  : Factor w/ 2 levels "M1","M2": 1 2 1 2 1 2 1 2 1 2 ...
$ Team    : Ord.factor w/ 9 levels "T1S"<"T3S"<"T2S"<..: 1 1 4 4 7 7 3 3 5 5 ...
$ Physique: Ord.factor w/ 3 levels "Slight"<"Average"<..: 1 1 2 2 3 3 1 1 2 2 ...
IGF : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	237 obs. of  3 variables:
  $ Lot : Ord.factor w/ 10 levels "9"<"6"<"1"<"10"<..: 3 3 3 3 3 3 3 3 3 3 ...
$ age : num  7 7 8 8 13 13 14 14 15 15 ...
$ conc: num  4.9 5.68 5.32 5.5 4.94 5.19 5.18 5.67 5.02 5.88 ...
Initialize : function (object, data, ...)  
  intervals : function (object, level = 0.95, ...)  
    isBalanced : function (object, countOnly = FALSE, level)  
      isInitialized : function (object)  
        LDEsysMat : function (pars, incidence)  
          lme : function (fixed, data = sys.frame(sys.parent()), random, correlation = NULL, weights = NULL, 
                          subset, method = c("REML", "ML"), na.action = na.fail, control = list(), contrasts = NULL, 
                          keep.data = TRUE)  
            lme.formula : function (fixed, data = sys.frame(sys.parent()), random = pdSymm(eval(as.call(fixed[-2]))), 
                                    correlation = NULL, weights = NULL, subset, method = c("REML", "ML"), na.action = na.fail, 
                                    control = list(), contrasts = NULL, keep.data = TRUE)  
              lme.lmList : function (fixed, data = sys.frame(sys.parent()), random, correlation = NULL, weights = NULL, 
                                     subset, method = c("REML", "ML"), na.action = na.fail, control = list(), contrasts = NULL, 
                                     keep.data = TRUE)  
                lmeControl : function (maxIter = 50, msMaxIter = 50, tolerance = 1e-06, niterEM = 25, msMaxEval = 200, 
                                       msTol = 1e-07, msVerbose = FALSE, returnObject = FALSE, gradHess = TRUE, apVar = TRUE, 
                                       .relStep = .Machine$double.eps^(1/3), minAbsParApVar = 0.05, opt = c("nlminb", 
                                                                                                            "optim"), optimMethod = "BFGS", natural = TRUE, sigma = NULL, allow.n.lt.q = FALSE, 
                                       ...)  
                  lmeStruct : function (reStruct, corStruct = NULL, varStruct = NULL)  
                    lmList : function (object, data, level, subset, na.action = na.fail, pool = TRUE, warn.lm = TRUE)  
                      lmList.formula : function (object, data, level, subset, na.action = na.fail, pool = TRUE, warn.lm = TRUE)  
                        logDet : function (object, ...)  
                          Machines : Classes ‘nffGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	54 obs. of  3 variables:
  $ Worker : Ord.factor w/ 6 levels "6"<"2"<"4"<"1"<..: 4 4 4 2 2 2 5 5 5 3 ...
$ Machine: Factor w/ 3 levels "A","B","C": 1 1 1 1 1 1 1 1 1 1 ...
$ score  : num  52 52.8 53.1 51.8 52.8 53.1 60 60.2 58.4 51.1 ...
MathAchieve : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	7185 obs. of  6 variables:
  $ School  : Ord.factor w/ 160 levels "8367"<"8854"<..: 59 59 59 59 59 59 59 59 59 59 ...
$ Minority: Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
$ Sex     : Factor w/ 2 levels "Male","Female": 2 2 1 1 1 1 2 1 2 1 ...
$ SES     : num  -1.528 -0.588 -0.528 -0.668 -0.158 ...
$ MathAch : num  5.88 19.71 20.35 8.78 17.9 ...
$ MEANSES : num  -0.428 -0.428 -0.428 -0.428 -0.428 -0.428 -0.428 -0.428 -0.428 -0.428 ...
MathAchSchool : 'data.frame':	160 obs. of  7 variables:
  $ School : Factor w/ 160 levels "1224","1288",..: 1 2 3 4 5 6 7 8 9 10 ...
$ Size   : num  842 1855 1719 716 455 ...
$ Sector : Factor w/ 2 levels "Public","Catholic": 1 1 1 2 2 1 1 2 2 1 ...
$ PRACAD : num  0.35 0.27 0.32 0.96 0.95 0.25 0.5 0.96 1 0.78 ...
$ DISCLIM: num  1.597 0.174 -0.137 -0.622 -1.694 ...
$ HIMINTY: Factor w/ 2 levels "0","1": 1 1 2 1 2 1 1 1 1 1 ...
$ MEANSES: num  -0.428 0.128 -0.42 0.534 0.351 -0.014 -0.007 0.718 0.569 0.683 ...
matrix<- : function (object, value)  
  Meat : Classes ‘nffGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	30 obs. of  4 variables:
  $ Storage: Ord.factor w/ 6 levels " 0"<" 1"<" 2"<..: 1 2 3 4 5 6 1 3 2 5 ...
$ score  : num  7 17 26 25 33 29 17 27 23 27 ...
$ Block  : Ord.factor w/ 5 levels "II"<"V"<"I"<"III"<..: 3 3 3 3 3 3 1 1 1 1 ...
$ Pair   : Ord.factor w/ 15 levels "II-1"<"II-2"<..: 7 7 8 8 9 9 1 1 2 2 ...
Milk : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	1337 obs. of  4 variables:
  $ protein: num  3.63 3.57 3.47 3.65 3.89 3.73 3.77 3.9 3.78 3.82 ...
$ Time   : num  1 2 3 4 5 6 7 8 9 10 ...
$ Cow    : Ord.factor w/ 79 levels "B04"<"B14"<"B03"<..: 25 25 25 25 25 25 25 25 25 25 ...
$ Diet   : Factor w/ 3 levels "barley","barley+lupins",..: 1 1 1 1 1 1 1 1 1 1 ...
Muscle : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	60 obs. of  3 variables:
  $ Strip : Ord.factor w/ 21 levels "S06"<"S05"<"S04"<..: 6 6 6 6 11 11 11 11 10 10 ...
$ conc  : num  2.2 4.4 6.6 8.8 2.2 4.4 6.6 8.8 0.55 1.1 ...
$ length: num  15.8 20.8 22.6 23.8 20.6 26.8 28.4 27 7.2 15.4 ...
Names : function (object, ...)  
  Names<- : function (object, ..., value)  
    needUpdate : function (object)  
      nfGroupedData : function (formula, data = NULL, order.groups = TRUE, FUN = function(x) max(x, na.rm = TRUE), 
                                outer = NULL, inner = NULL, labels = NULL, units = NULL)  
        Nitrendipene : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	89 obs. of  4 variables:
  $ activity: num  6696 6211 6385 6396 6283 ...
$ NIF     : num  0e+00 0e+00 0e+00 1e-11 1e-11 ...
$ Tissue  : Ord.factor w/ 4 levels "2"<"1"<"3"<"4": 2 2 2 2 2 2 2 2 2 2 ...
$ log.NIF : num  -14 -14 -14 -11 -11 -11 -10 -10 -10 -9 ...
nlme : function (model, data = sys.frame(sys.parent()), fixed, random = fixed, groups, start, 
                 correlation = NULL, weights = NULL, subset, method = c("ML", "REML"), na.action = na.fail, 
                 naPattern, control = list(), verbose = FALSE)  
  nlme.formula : function (model, data = sys.frame(sys.parent()), fixed, random, groups, start, correlation = NULL, 
                           weights = NULL, subset, method = c("ML", "REML"), na.action = na.fail, naPattern, 
                           control = list(), verbose = FALSE)  
    nlme.nlsList : function (model, data = sys.frame(sys.parent()), fixed, random = fixed, groups, start, 
                             correlation = NULL, weights = NULL, subset, method = c("ML", "REML"), na.action = na.fail, 
                             naPattern, control = list(), verbose = FALSE)  
      nlmeControl : function (maxIter = 50, pnlsMaxIter = 7, msMaxIter = 50, minScale = 0.001, tolerance = 1e-05, 
                              niterEM = 25, pnlsTol = 0.001, msTol = 1e-06, returnObject = FALSE, msVerbose = FALSE, 
                              msWarnNoConv = TRUE, gradHess = TRUE, apVar = TRUE, .relStep = .Machine$double.eps^(1/3), 
                              minAbsParApVar = 0.05, opt = c("nlminb", "nlm"), natural = TRUE, sigma = NULL, 
                              ...)  
        nlmeStruct : function (reStruct, corStruct = NULL, varStruct = NULL)  
          nlsList : function (model, data, start, control, level, subset, na.action = na.fail, pool = TRUE, 
                              warn.nls = NA)  
            nlsList.formula : function (model, data, start = NULL, control, level, subset, na.action = na.fail, 
                                        pool = TRUE, warn.nls = NA)  
              nmGroupedData : function (formula, data = NULL, order.groups = TRUE, FUN = function(x) max(x, na.rm = TRUE), 
                                        outer = NULL, inner = NULL, labels = NULL, units = NULL)  
                Oats : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	72 obs. of  4 variables:
  $ Block  : Ord.factor w/ 6 levels "VI"<"V"<"III"<..: 6 6 6 6 6 6 6 6 6 6 ...
$ Variety: Factor w/ 3 levels "Golden Rain",..: 3 3 3 3 1 1 1 1 2 2 ...
$ nitro  : num  0 0.2 0.4 0.6 0 0.2 0.4 0.6 0 0.2 ...
$ yield  : num  111 130 157 174 117 114 161 141 105 140 ...
Orthodont : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	108 obs. of  4 variables:
  $ distance: num  26 25 29 31 21.5 22.5 23 26.5 23 22.5 ...
$ age     : num  8 10 12 14 8 10 12 14 8 10 ...
$ Subject : Ord.factor w/ 27 levels "M16"<"M05"<"M02"<..: 15 15 15 15 3 3 3 3 7 7 ...
$ Sex     : Factor w/ 2 levels "Male","Female": 1 1 1 1 1 1 1 1 1 1 ...
Ovary : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	308 obs. of  3 variables:
  $ Mare     : Ord.factor w/ 11 levels "4"<"2"<"11"<"7"<..: 7 7 7 7 7 7 7 7 7 7 ...
$ Time     : num  -0.1364 -0.0909 -0.0455 0 0.0455 ...
$ follicles: num  20 15 19 16 13 10 12 14 13 20 ...
Oxboys : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	234 obs. of  4 variables:
  $ Subject : Ord.factor w/ 26 levels "10"<"26"<"25"<..: 13 13 13 13 13 13 13 13 13 5 ...
$ age     : num  -1 -0.7479 -0.463 -0.1643 -0.0027 ...
$ height  : num  140 143 145 147 148 ...
$ Occasion: Ord.factor w/ 9 levels "1"<"2"<"3"<"4"<..: 1 2 3 4 5 6 7 8 9 1 ...
Oxide : Classes ‘nmGroupedData’, ‘groupedData’ and 'data.frame':	72 obs. of  5 variables:
  $ Source   : Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...
$ Lot      : Factor w/ 8 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 2 ...
$ Wafer    : Factor w/ 3 levels "1","2","3": 1 1 1 2 2 2 3 3 3 1 ...
$ Site     : Factor w/ 3 levels "1","2","3": 1 2 3 1 2 3 1 2 3 1 ...
$ Thickness: num  2006 1999 2007 1980 1988 ...
PBG : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	60 obs. of  5 variables:
  $ deltaBP  : num  0.5 4.5 10 26 37 32 1 1.25 4 12 ...
$ dose     : num  6.25 12.5 25 50 100 200 6.25 12.5 25 50 ...
$ Run      : Ord.factor w/ 10 levels "T5"<"T4"<"T3"<..: 10 10 10 10 10 10 8 8 8 8 ...
$ Treatment: Factor w/ 2 levels "MDL 72222","Placebo": 2 2 2 2 2 2 2 2 2 2 ...
$ Rabbit   : Ord.factor w/ 5 levels "5"<"3"<"2"<"4"<..: 5 5 5 5 5 5 3 3 3 3 ...
pdBlocked : function (value = numeric(0), form = NULL, nam = NULL, data = parent.frame(), pdClass = "pdSymm")  
  pdCompSymm : function (value = numeric(0), form = NULL, nam = NULL, data = parent.frame())  
    pdConstruct : function (object, value, form, nam, data, ...)  
      pdDiag : function (value = numeric(0), form = NULL, nam = NULL, data = parent.frame())  
        pdFactor : function (object)  
          pdIdent : function (value = numeric(0), form = NULL, nam = NULL, data = parent.frame())  
            pdLogChol : function (value = numeric(0), form = NULL, nam = NULL, data = parent.frame())  
              pdMat : function (value = numeric(0), form = NULL, nam = NULL, data = parent.frame(), pdClass = "pdSymm")  
                pdMatrix : function (object, factor = FALSE)  
                  pdNatural : function (value = numeric(0), form = NULL, nam = NULL, data = parent.frame())  
                    pdSymm : function (value = numeric(0), form = NULL, nam = NULL, data = parent.frame())  
                      Phenobarb : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	744 obs. of  7 variables:
  $ Subject : Ord.factor w/ 59 levels "42"<"28"<"30"<..: 32 32 32 32 32 32 32 32 32 32 ...
$ Wt      : num  1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 ...
$ Apgar   : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 7 7 7 7 7 7 7 7 7 7 ...
$ ApgarInd: Factor w/ 2 levels "< 5",">= 5": 2 2 2 2 2 2 2 2 2 2 ...
$ time    : num  0 2 12.5 24.5 37 48 60.5 72.5 85.3 96.5 ...
$ dose    : num  25 NA 3.5 3.5 3.5 3.5 3.5 3.5 3.5 3.5 ...
$ conc    : num  NA 17.3 NA NA NA NA NA NA NA NA ...
phenoModel : function (Subject, time, dose, lCl, lV)  
  Pixel : Classes ‘nmGroupedData’, ‘groupedData’ and 'data.frame':	102 obs. of  4 variables:
  $ Dog  : Factor w/ 10 levels "1","10","2","3",..: 1 1 1 1 1 1 1 3 3 3 ...
$ Side : Factor w/ 2 levels "L","R": 2 2 2 2 2 2 2 2 2 2 ...
$ day  : num  0 1 2 4 6 10 14 0 1 2 ...
$ pixel: num  1046 1044 1043 1050 1045 ...
plot.lme : function (x, form = resid(., type = "pearson") ~ fitted(.), abline, id = NULL, idLabels = NULL, 
                     idResType = c("pearson", "normalized"), grid, ...)  
  pooledSD : function (object)  
    Quinidine : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	1471 obs. of  14 variables:
  $ Subject   : Ord.factor w/ 136 levels "109"<"70"<"23"<..: 78 78 78 78 78 78 78 78 78 78 ...
$ time      : num  0 3 6 16 24 ...
$ conc      : num  NA NA NA NA NA NA NA 1.4 NA NA ...
$ dose      : num  249 249 249 201 201 201 201 NA 201 201 ...
$ interval  : num  NA NA NA NA NA NA NA NA 6 NA ...
$ Age       : num  60 60 60 60 60 60 60 60 60 60 ...
$ Height    : num  69 69 69 69 69 69 69 69 69 69 ...
$ Weight    : num  106 106 106 106 106 106 106 106 106 106 ...
$ Race      : Factor w/ 3 levels "Caucasian","Latin",..: 1 1 1 1 1 1 1 1 1 1 ...
$ Smoke     : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
$ Ethanol   : Factor w/ 3 levels "none","current",..: 2 2 2 2 2 2 2 2 2 2 ...
$ Heart     : Factor w/ 3 levels "No/Mild","Moderate",..: 2 2 2 2 2 2 2 2 2 2 ...
$ Creatinine: Ord.factor w/ 2 levels "< 50"<">= 50": 2 2 2 2 2 2 2 2 2 2 ...
$ glyco     : num  0.41 0.41 0.41 0.41 0.41 0.41 0.41 0.41 1.11 1.11 ...
quinModel : function (Subject, time, conc, dose, interval, lV, lKa, lCl)  
  Rail : Classes ‘nffGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	18 obs. of  2 variables:
  $ Rail  : Ord.factor w/ 6 levels "2"<"5"<"1"<"6"<..: 3 3 3 1 1 1 5 5 5 6 ...
$ travel: num  55 53 54 26 37 32 78 91 85 92 ...
random.effects : function (object, ...)  
  ranef : function (object, ...)  
    RatPupWeight : Classes ‘nffGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	322 obs. of  5 variables:
  $ weight   : num  6.6 7.4 7.15 7.24 7.1 6.04 6.98 7.05 6.95 6.29 ...
$ sex      : Factor w/ 2 levels "Male","Female": 1 1 1 1 1 1 1 1 2 2 ...
$ Litter   : Ord.factor w/ 27 levels "9"<"8"<"7"<"4"<..: 7 7 7 7 7 7 7 7 7 7 ...
$ Lsize    : num  12 12 12 12 12 12 12 12 12 12 ...
$ Treatment: Ord.factor w/ 3 levels "Control"<"Low"<..: 1 1 1 1 1 1 1 1 1 1 ...
recalc : function (object, conLin, ...)  
  Relaxin : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	198 obs. of  3 variables:
  $ Run : Ord.factor w/ 9 levels "5"<"8"<"9"<"3"<..: 8 8 8 8 8 8 8 8 8 8 ...
$ conc: num  0.085 0.34 0.34 0.34 0.69 0.69 0.69 1.38 1.38 1.38 ...
$ cAMP: num  1.77 3.35 4 6.1 8.4 ...
Remifentanil : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	2107 obs. of  12 variables:
  $ ID     : num  1 1 1 1 1 1 1 1 1 1 ...
$ Subject: Ord.factor w/ 65 levels "30"<"21"<"25"<..: 28 28 28 28 28 28 28 28 28 28 ...
$ Time   : num  0 1.5 2 2.52 3.02 3.63 4.05 5.02 6.02 7.03 ...
$ conc   : num  NA 9.51 11.5 14.1 16.7 17.1 16.8 18.7 14.2 15.8 ...
$ Rate   : num  72 72 72 72 72 ...
$ Amt    : num  108 36 37.4 36 43.9 ...
$ Age    : num  30.6 30.6 30.6 30.6 30.6 ...
$ Sex    : Factor w/ 2 levels "Female","Male": 2 2 2 2 2 2 2 2 2 2 ...
$ Ht     : num  171 171 171 171 171 171 171 171 171 171 ...
$ Wt     : num  72 72 72 72 72 72 72 72 72 72 ...
$ BSA    : num  1.84 1.84 1.84 1.84 1.84 ...
$ LBM    : num  56.5 56.5 56.5 56.5 56.5 ...
reStruct : function (object, pdClass = "pdLogChol", REML = FALSE, data = sys.frame(sys.parent()))  
  simulate.lme : function (object, nsim = 1, seed = as.integer(runif(1, 0, .Machine$integer.max)), 
                           m2, method = c("REML", "ML"), niterEM = c(40, 200), useGen, ...)  
    Soybean : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	412 obs. of  5 variables:
  $ Plot   : Ord.factor w/ 48 levels "1988F4"<"1988F2"<..: 3 3 3 3 3 3 3 3 3 3 ...
$ Variety: Factor w/ 2 levels "F","P": 1 1 1 1 1 1 1 1 1 1 ...
$ Year   : Factor w/ 3 levels "1988","1989",..: 1 1 1 1 1 1 1 1 1 1 ...
$ Time   : num  14 21 28 35 42 49 56 63 70 77 ...
$ weight : num  0.106 0.261 0.666 2.11 3.56 ...
splitFormula : function (form, sep = "/")  
  Spruce : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	1027 obs. of  4 variables:
  $ Tree   : Ord.factor w/ 79 levels "O1T24"<"O1T18"<..: 23 23 23 23 23 23 23 23 23 23 ...
$ days   : num  152 174 201 227 258 469 496 528 556 579 ...
$ logSize: num  4.51 4.98 5.41 5.9 6.15 6.16 6.18 6.48 6.65 6.87 ...
$ plot   : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 1 ...
Tetracycline1 : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	40 obs. of  4 variables:
  $ conc       : num  1.08 1.99 1.46 1.21 1.48 2.5 2.62 1.95 1.19 2.1 ...
$ Time       : num  1 2 3 6 1 2 3 6 1 2 ...
$ Subject    : Ord.factor w/ 5 levels "5"<"3"<"2"<"4"<..: 5 5 5 5 5 5 5 5 3 3 ...
$ Formulation: Factor w/ 2 levels "tetrachel","tetracyn": 1 1 1 1 2 2 2 2 1 1 ...
Tetracycline2 : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	40 obs. of  4 variables:
  $ conc       : num  1.2 1.54 1.28 0.79 1.28 2.25 1.95 1.24 0.96 2.05 ...
$ Time       : num  1 2 3 6 1 2 3 6 1 2 ...
$ Subject    : Ord.factor w/ 5 levels "4"<"5"<"2"<"1"<..: 4 4 4 4 4 4 4 4 3 3 ...
$ Formulation: Factor w/ 2 levels "Berkmycin","tetramycin": 1 1 1 1 2 2 2 2 1 1 ...
varComb : function (...)  
  varConstPower : function (const = numeric(0), power = numeric(0), form = ~fitted(.), fixed = NULL)  
    varConstProp : function (const = numeric(0), prop = numeric(0), form = ~fitted(.), fixed = NULL)  
      VarCorr : function (x, sigma = 1, ...)  
        varExp : function (value = numeric(0), form = ~fitted(.), fixed = NULL)  
          varFixed : function (value = ~1)  
            varFunc : function (object)  
              varIdent : function (value = numeric(0), form = ~1, fixed = NULL)  
                Variogram : function (object, distance, ...)  
                  varPower : function (value = numeric(0), form = ~fitted(.), fixed = NULL)  
                    varWeights : function (object)  
                      Wafer : Classes ‘nmGroupedData’, ‘groupedData’ and 'data.frame':	400 obs. of  4 variables:
  $ Wafer  : Factor w/ 10 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
$ Site   : Factor w/ 8 levels "1","2","3","4",..: 1 1 1 1 1 2 2 2 2 2 ...
$ voltage: num  0.8 1.2 1.6 2 2.4 0.8 1.2 1.6 2 2.4 ...
$ current: num  0.901 3.868 7.641 11.736 15.934 ...
Wheat : Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	48 obs. of  4 variables:
  $ Tray      : Ord.factor w/ 12 levels "3"<"1"<"2"<"4"<..: 2 2 2 2 3 3 3 3 1 1 ...
$ Moisture  : num  10 10 10 10 10 10 10 10 10 10 ...
$ fertilizer: num  2 4 6 8 2 4 6 8 2 4 ...
$ DryMatter : num  3.35 4.32 4.56 5.88 4.04 ...
Wheat2 : Classes ‘nffGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	224 obs. of  5 variables:
  $ Block    : Ord.factor w/ 4 levels "4"<"2"<"3"<"1": 4 4 4 4 4 4 4 4 4 4 ...
$ variety  : Factor w/ 56 levels "ARAPAHOE","BRULE",..: 12 2 50 7 1 14 15 16 4 52 ...
$ yield    : num  29.2 31.6 35 30.1 33 ...
$ latitude : num  4.3 4.3 4.3 4.3 4.3 4.3 4.3 8.6 8.6 8.6 ...
$ longitude: num  19.2 20.4 21.6 22.8 24 25.2 26.4 1.2 2.4 3.6 ...
> 
library(openxlsx)
.__T__$:base : <environment: 0x000001dbf015cf88> 
  .__T__$<-:base : <environment: 0x000001dbed948dd8> 
  .__T__[:base : <environment: 0x000001dbed94d208> 
           .__T__[[<-:base : <environment: 0x000001dbed953200> 
                     .__T__[<-:base : <environment: 0x000001dbed958de8> 
                              activeSheet : function (wb)  
                                activeSheet<- : function (wb, value)  
                                  addCreator : function (wb, Creator)  
                                    addFilter : function (wb, sheet, rows, cols)  
                                      addStyle : function (wb, sheet, style, rows, cols, gridExpand = FALSE, stack = FALSE)  
                                        addWorksheet : function (wb, sheetName, gridLines = openxlsx_getOp("gridLines", TRUE), tabColour = NULL, 
                                                                 zoom = 100, header = openxlsx_getOp("header"), footer = openxlsx_getOp("footer"), 
                                                                 evenHeader = openxlsx_getOp("evenHeader"), evenFooter = openxlsx_getOp("evenFooter"), 
                                                                 firstHeader = openxlsx_getOp("firstHeader"), firstFooter = openxlsx_getOp("firstFooter"), 
                                                                 visible = TRUE, paperSize = openxlsx_getOp("paperSize", 9), orientation = openxlsx_getOp("orientation", 
                                                                                                                                                          "portrait"), vdpi = openxlsx_getOp("vdpi", 300), hdpi = openxlsx_getOp("hdpi", 
                                                                                                                                                                                                                                 300))  
                                          buildWorkbook : function (x, asTable = FALSE, ...)  
                                            cloneWorksheet : function (wb, sheetName, clonedSheet)  
                                              col2int : function (x)  
                                                conditionalFormat : function (wb, sheet, cols, rows, rule = NULL, style = NULL, type = "expression")  
                                                  conditionalFormatting : function (wb, sheet, cols, rows, rule = NULL, style = NULL, type = "expression", ...)  
                                                    convertFromExcelRef : function (col)  
                                                      convertToDate : function (x, origin = "1900-01-01", ...)  
                                                        convertToDateTime : function (x, origin = "1900-01-01", ...)  
                                                          copyWorkbook : function (wb)  
                                                            createComment : function (comment, author = Sys.getenv("USERNAME"), style = NULL, visible = TRUE, 
                                                                                      width = 2, height = 4)  
                                                              createNamedRegion : function (wb, sheet, cols, rows, name, overwrite = FALSE)  
                                                                createStyle : function (fontName = NULL, fontSize = NULL, fontColour = NULL, numFmt = openxlsx_getOp("numFmt", 
                                                                                                                                                                     "GENERAL"), border = NULL, borderColour = openxlsx_getOp("borderColour", "black"), 
                                                                                        borderStyle = openxlsx_getOp("borderStyle", "thin"), bgFill = NULL, fgFill = NULL, 
                                                                                        halign = NULL, valign = NULL, textDecoration = NULL, wrapText = FALSE, textRotation = NULL, 
                                                                                        indent = NULL, locked = NULL, hidden = NULL)  
                                                                  createWorkbook : function (creator = ifelse(.Platform$OS.type == "windows", Sys.getenv("USERNAME"), 
                                                                                                              Sys.getenv("USER")), title = NULL, subject = NULL, category = NULL)  
                                                                    dataValidation : function (wb, sheet, cols, rows, type, operator, value, allowBlank = TRUE, showInputMsg = TRUE, 
                                                                                               showErrorMsg = TRUE)  
                                                                      deleteData : function (wb, sheet, cols, rows, gridExpand = FALSE)  
                                                                        deleteNamedRegion : function (wb, name)  
                                                                          freezePane : function (wb, sheet, firstActiveRow = NULL, firstActiveCol = NULL, firstRow = FALSE, 
                                                                                                 firstCol = FALSE)  
                                                                            getBaseFont : function (wb)  
                                                                              getCellRefs : function (cellCoords)  
                                                                                getCreators : function (wb)  
                                                                                  getDateOrigin : function (xlsxFile)  
                                                                                    getNamedRegions : function (x)  
                                                                                      getSheetNames : function (file)  
                                                                                        getStyles : function (wb)  
                                                                                          getTables : function (wb, sheet)  
                                                                                            groupColumns : function (wb, sheet, cols, hidden = FALSE)  
                                                                                              groupRows : function (wb, sheet, rows, hidden = FALSE)  
                                                                                                insertImage : function (wb, sheet, file, width = 6, height = 3, startRow = 1, startCol = 1, units = "in", 
                                                                                                                        dpi = 300)  
                                                                                                  insertPlot : function (wb, sheet, width = 6, height = 4, xy = NULL, startRow = 1, startCol = 1, 
                                                                                                                         fileType = "png", units = "in", dpi = 300)  
                                                                                                    int2col : function (x)  
                                                                                                      loadWorkbook : function (file, xlsxFile = NULL, isUnzipped = FALSE)  
                                                                                                        makeHyperlinkString : function (sheet, row = 1, col = 1, text = NULL, file = NULL)  
                                                                                                          mergeCells : function (wb, sheet, cols, rows)  
                                                                                                            modifyBaseFont : function (wb, fontSize = 11, fontColour = "black", fontName = "Calibri")  
                                                                                                              op.openxlsx : List of 34
                            $ openxlsx.bandedCols      : logi FALSE
                            $ openxlsx.bandedRows      : logi TRUE
                            $ openxlsx.borderColour    : chr "black"
                            $ openxlsx.borders         : NULL
                            $ openxlsx.borderStyle     : chr "thin"
                            $ openxlsx.compressionLevel: num 9
                            $ openxlsx.creator         : chr ""
                            $ openxlsx.dateFormat      : chr "mm/dd/yyyy"
                            $ openxlsx.datetimeFormat  : chr "yyyy-mm-dd hh:mm:ss"
                            $ openxlsx.hdpi            : num 300
                            $ openxlsx.header          : NULL
                            $ openxlsx.headerStyle     : NULL
                            $ openxlsx.firstColumn     : NULL
                            $ openxlsx.firstFooter     : NULL
                            $ openxlsx.firstHeader     : NULL
                            $ openxlsx.footer          : NULL
                            $ openxlsx.evenFooter      : NULL
                            $ openxlsx.evenHeader      : NULL
                            $ openxlsx.gridLines       : logi TRUE
                            $ openxlsx.keepNA          : logi FALSE
                            $ openxlsx.lastColumn      : NULL
                            $ openxlsx.na.string       : NULL
                            $ openxlsx.maxWidth        : num 250
                            $ openxlsx.minWidth        : num 3
                            $ openxlsx.numFmt          : chr "GENERAL"
                            $ openxlsx.oddFooter       : NULL
                            $ openxlsx.oddHeader       : NULL
                            $ openxlsx.orientation     : chr "portrait"
                            $ openxlsx.paperSize       : num 9
                            $ openxlsx.showGridLines   : logi NA
                            $ openxlsx.tabColour       : NULL
                            $ openxlsx.tableStyle      : chr "TableStyleLight9"
                            $ openxlsx.vdpi            : num 300
                            $ openxlsx.withFilter      : NULL
                            openXL : function (file = NULL)  
                              openxlsx_getOp : function (x, default = NULL)  
                                openxlsx_setOp : function (x, value)  
                                  pageBreak : function (wb, sheet, i, type = "row")  
                                    pageSetup : function (wb, sheet, orientation = NULL, scale = 100, left = 0.7, right = 0.7, top = 0.75, 
                                                          bottom = 0.75, header = 0.3, footer = 0.3, fitToWidth = FALSE, fitToHeight = FALSE, 
                                                          paperSize = NULL, printTitleRows = NULL, printTitleCols = NULL, summaryRow = NULL, 
                                                          summaryCol = NULL)  
                                      protectWorkbook : function (wb, protect = TRUE, password = NULL, lockStructure = FALSE, lockWindows = FALSE, 
                                                                  type = 1L)  
                                        protectWorksheet : function (wb, sheet, protect = TRUE, password = NULL, lockSelectingLockedCells = NULL, 
                                                                     lockSelectingUnlockedCells = NULL, lockFormattingCells = NULL, lockFormattingColumns = NULL, 
                                                                     lockFormattingRows = NULL, lockInsertingColumns = NULL, lockInsertingRows = NULL, 
                                                                     lockInsertingHyperlinks = NULL, lockDeletingColumns = NULL, lockDeletingRows = NULL, 
                                                                     lockSorting = NULL, lockAutoFilter = NULL, lockPivotTables = NULL, lockObjects = NULL, 
                                                                     lockScenarios = NULL)  
                                          read.xlsx : function (xlsxFile, sheet, startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, 
                                                                skipEmptyRows = TRUE, skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE, 
                                                                sep.names = ".", namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)  
                                            readWorkbook : function (xlsxFile, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, 
                                                                     skipEmptyRows = TRUE, skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE, 
                                                                     sep.names = ".", namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)  
                                              removeCellMerge : function (wb, sheet, cols, rows)  
                                                removeColWidths : function (wb, sheet, cols)  
                                                  removeComment : function (wb, sheet, cols, rows, gridExpand = TRUE)  
                                                    removeFilter : function (wb, sheet)  
                                                      removeRowHeights : function (wb, sheet, rows)  
                                                        removeTable : function (wb, sheet, table)  
                                                          removeWorksheet : function (wb, sheet)  
                                                            renameWorksheet : function (wb, sheet, newName)  
                                                              replaceStyle : function (wb, index, newStyle)  
                                                                saveWorkbook : function (wb, file, overwrite = FALSE, returnValue = FALSE)  
                                                                  setColWidths : function (wb, sheet, cols, widths = 8.43, hidden = rep(FALSE, length(cols)), ignoreMergedCells = FALSE)  
                                                                    setFooter : function (wb, text, position = "center")  
                                                                      setHeader : function (wb, text, position = "center")  
                                                                        setHeaderFooter : function (wb, sheet, header = NULL, footer = NULL, evenHeader = NULL, evenFooter = NULL, 
                                                                                                    firstHeader = NULL, firstFooter = NULL)  
                                                                          setLastModifiedBy : function (wb, LastModifiedBy)  
                                                                            setRowHeights : function (wb, sheet, rows, heights)  
                                                                              sheets : function (wb)  
                                                                                sheetVisibility : function (wb)  
                                                                                  sheetVisibility<- : function (wb, value)  
                                                                                    sheetVisible : function (wb)  
                                                                                      sheetVisible<- : function (wb, value)  
                                                                                        showGridLines : function (wb, sheet, showGridLines = FALSE)  
                                                                                          temp_xlsx : function (name = "temp_xlsx")  
                                                                                            ungroupColumns : function (wb, sheet, cols)  
                                                                                              ungroupRows : function (wb, sheet, rows)  
                                                                                                worksheetOrder : function (wb)  
                                                                                                  worksheetOrder<- : function (wb, value)  
                                                                                                    write.xlsx : function (x, file, asTable = FALSE, overwrite = TRUE, ...)  
                                                                                                      writeComment : function (wb, sheet, col, row, comment, xy = NULL)  
                                                                                                        writeData : function (wb, sheet, x, startCol = 1, startRow = 1, array = FALSE, xy = NULL, colNames = TRUE, 
                                                                                                                              rowNames = FALSE, headerStyle = openxlsx_getOp("headerStyle"), borders = openxlsx_getOp("borders", 
                                                                                                                                                                                                                      "none"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", 
                                                                                                                                                                                                                                                                                                                    "thin"), withFilter = openxlsx_getOp("withFilter", FALSE), keepNA = openxlsx_getOp("keepNA", 
                                                                                                                                                                                                                                                                                                                                                                                                       FALSE), na.string = openxlsx_getOp("na.string"), name = NULL, sep = ", ", 
                                                                                                                              col.names, row.names)  
                                                                                                          writeDataTable : function (wb, sheet, x, startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, 
                                                                                                                                     tableStyle = openxlsx_getOp("tableStyle", "TableStyleLight9"), tableName = NULL, 
                                                                                                                                     headerStyle = openxlsx_getOp("headerStyle"), withFilter = openxlsx_getOp("withFilter", 
                                                                                                                                                                                                              TRUE), keepNA = openxlsx_getOp("keepNA", FALSE), na.string = openxlsx_getOp("na.string"), 
                                                                                                                                     sep = ", ", stack = FALSE, firstColumn = openxlsx_getOp("firstColumn", FALSE), 
                                                                                                                                     lastColumn = openxlsx_getOp("lastColumn", FALSE), bandedRows = openxlsx_getOp("bandedRows", 
                                                                                                                                                                                                                   TRUE), bandedCols = openxlsx_getOp("bandedCols", FALSE), col.names, row.names)  
                                                                                                            writeFormula : function (wb, sheet, x, startCol = 1, startRow = 1, array = FALSE, xy = NULL)  
                                                                                                              > 
library(phonics)
caverphone : function (word, maxCodeLen = NULL, modified = FALSE, clean = TRUE)  
  cologne : function (word, maxCodeLen = NULL, clean = TRUE)  
    lein : function (word, maxCodeLen = 4, clean = TRUE)  
      metaphone : function (word, maxCodeLen = 10L, clean = TRUE)  
        mra_compare : function (x, y)  
          mra_encode : function (word, clean = TRUE)  
            nysiis : function (word, maxCodeLen = 6, modified = FALSE, clean = TRUE)  
              onca : function (word, maxCodeLen = 4, clean = TRUE, modified = FALSE, refined = FALSE)  
                phonex : function (word, maxCodeLen = 4, clean = TRUE)  
                  phonics : function (word, method, clean = TRUE)  
                    refinedSoundex : function (word, maxCodeLen = 10L, clean = TRUE)  
                      rogerroot : function (word, maxCodeLen = 5, clean = TRUE)  
                        soundex : function (word, maxCodeLen = 4L, clean = TRUE)  
                          statcan : function (word, maxCodeLen = 4, clean = TRUE)  
                            > 
library(praise)
praise : function (template = "You are ${adjective}!")  
  praise_parts : List of 7
$ adjective    : chr [1:107] "ace" "amazing" "astonishing" "astounding" ...
$ adverb       : chr [1:59] "beautifully" "bravely" "brightly" "calmly" ...
$ adverb_manner: chr [1:59] "beautifully" "bravely" "brightly" "calmly" ...
$ created      : chr [1:17] "assembled" "brewed" "built" "created" ...
$ creating     : chr [1:17] "assembling" "brewing" "building" "creating" ...
$ exclamation  : chr [1:40] "ah" "aha" "ahh" "ahhh" ...
$ rpackage     : chr [1:7] "code" "library (or package?)" "package" "program" ...
> 
library(progress)
progress_bar : Class 'R6ClassGenerator' <progress_bar> object generator
Public:
  finished: FALSE
initialize: function (format = "[:bar] :percent", total = 100, width = getOption("width") - 
                        tick: function (len = 1, tokens = list()) 
                          update: function (ratio, tokens = list()) 
                            message: function (msg, set_width = TRUE) 
                              terminate: function () 
                                clone: function (deep = FALSE) 
                                  Private:
                        first: TRUE
                      supported: NA
                      format: NULL
                      total: NULL
                      current: 0
                      width: NULL
                      chars: list
                      callback: NULL
                      clear: NULL
                      show_after: NULL
                      last_draw: 
                        message_class: NULL
                      start: NULL
                      toupdate: FALSE
                      complete: FALSE
                      spin: NULL
                      has_token: FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FA ...
                      render: function (tokens) 
                        ratio: function () 
                          progress_message: function (..., domain = NULL, appendLF = TRUE) 
                            clear_line: function (width) 
                              cursor_to_start: function () 
                                Parent env: <environment: namespace:progress>
                        Locked objects: TRUE
                      Locked class: FALSE
                      Portable: TRUE 
                      > 
library(progressr)
handler_ascii_alert : function (symbol = "\a", file = stderr(), intrusiveness = getOption("progressr.intrusiveness.auditory", 
                                                                                          5), target = c("terminal", "audio"), ...)  
  handler_beepr : function (initiate = 2L, update = 10L, finish = 11L, intrusiveness = getOption("progressr.intrusiveness.auditory", 
                                                                                                 5), target = "audio", ...)  
    handler_debug : function (interval = getOption("progressr.interval", 0), intrusiveness = getOption("progressr.intrusiveness.debug", 
                                                                                                       0), target = "terminal", uuid = FALSE, ...)  
      handler_filesize : function (file = "default.progress", intrusiveness = getOption("progressr.intrusiveness.file", 
                                                                                        5), target = "file", ...)  
        handler_newline : function (symbol = "\n", file = stderr(), intrusiveness = getOption("progressr.intrusiveness.debug", 
                                                                                              0), target = "terminal", ...)  
          handler_notifier : function (intrusiveness = getOption("progressr.intrusiveness.notifier", 10), target = "gui", 
                                       ...)  
            handler_pbcol : function (adjust = 0, pad = 1L, complete = function(s) crayon::bgBlue(crayon::white(s)), 
                                      incomplete = function(s) crayon::bgCyan(crayon::white(s)), intrusiveness = getOption("progressr.intrusiveness.terminal", 
                                                                                                                           1), target = "terminal", ...)  
              handler_pbmcapply : function (substyle = 3L, style = "ETA", file = stderr(), intrusiveness = getOption("progressr.intrusiveness.terminal", 
                                                                                                                     1), target = "terminal", ...)  
                handler_progress : function (format = ":spin [:bar] :percent :message", show_after = 0, intrusiveness = getOption("progressr.intrusiveness.terminal", 
                                                                                                                                  1), target = "terminal", ...)  
                  handler_rstudio : function (intrusiveness = getOption("progressr.intrusiveness.gui", 1), target = "gui", 
                                              title = function() format(Sys.time(), "Console %X"), ...)  
                    handler_shiny : function (intrusiveness = getOption("progressr.intrusiveness.gui", 1), target = "gui", 
                                              inputs = list(message = NULL, detail = "message"), ...)  
                      handler_tkprogressbar : function (intrusiveness = getOption("progressr.intrusiveness.gui", 1), target = "terminal", 
                                                        ...)  
                        handler_txtprogressbar : function (style = 3L, file = stderr(), intrusiveness = getOption("progressr.intrusiveness.terminal", 
                                                                                                                  1), target = "terminal", ...)  
                          handler_void : function (intrusiveness = 0, target = "void", enable = FALSE, ...)  
                            handler_winprogressbar : function (intrusiveness = getOption("progressr.intrusiveness.gui", 1), target = "gui", 
                                                               ...)  
                              handlers : function (..., append = FALSE, on_missing = c("error", "warning", "ignore"), default = handler_txtprogressbar, 
                                                   global = NULL)  
                                make_progression_handler : function (name, reporter = list(), handler = NULL, enable = getOption("progressr.enable", 
                                                                                                                                 interactive()), enable_after = getOption("progressr.enable_after", 0), times = getOption("progressr.times", 
                                                                                                                                                                                                                          +Inf), interval = getOption("progressr.interval", 0), intrusiveness = 1, clear = getOption("progressr.clear", 
                                                                                                                                                                                                                                                                                                                     TRUE), target = "terminal", ...)  
                                  progress : function (..., call = sys.call())  
                                    progress_aggregator : function (progress)  
                                      progress_progressr : function (...)  
                                        progression : function (message = character(0L), amount = 1, step = NULL, time = progression_time, 
                                                                ..., type = "update", class = NULL, progressor_uuid = NULL, progression_index = NULL, 
                                                                progression_time = Sys.time(), call = NULL, calls = sys.calls(), owner_session_uuid = NULL)  
                                          progressor : function (steps = length(along), along = NULL, offset = 0L, scale = 1L, transform = function(steps) scale * 
                                                                   steps + offset, message = character(0L), label = NA_character_, trace = FALSE, 
                                                                 initiate = TRUE, auto_finish = TRUE, on_exit = !identical(envir, globalenv()), 
                                                                 enable = getOption("progressr.enable", TRUE), envir = parent.frame())  
                                            slow_sum : function (x, delay = getOption("progressr.demo.delay", 1), stdout = FALSE, message = TRUE)  
                                              with_progress : function (expr, handlers = progressr::handlers(), cleanup = TRUE, delay_terminal = NULL, 
                                                                        delay_stdout = NULL, delay_conditions = NULL, interrupts = getOption("progressr.interrupts", 
                                                                                                                                             TRUE), interval = NULL, enable = NULL)  
                                                without_progress : function (expr)  
                                                  withProgressShiny : function (expr, ..., message = NULL, detail = NULL, inputs = list(message = NULL, 
                                                                                                                                        detail = "message"), env = parent.frame(), quoted = FALSE, handlers = c(shiny = handler_shiny, 
                                                                                                                                                                                                                progressr::handlers(default = NULL)))  
                                                    > 
library(purrr)
%@% : function (x, name)  
  %||% : function (x, y)  
    %>% : function (lhs, rhs)  
      accumulate : function (.x, .f, ..., .init, .dir = c("forward", "backward"))  
        accumulate_right : function (.x, .f, ..., .init)  
          accumulate2 : function (.x, .y, .f, ..., .init)  
            array_branch : function (array, margin = NULL)  
              array_tree : function (array, margin = NULL)  
                as_function : function (...)  
                  as_mapper : function (.f, ...)  
                    as_vector : function (.x, .type = NULL)  
                      assign_in : function (x, where, value)  
                        at_depth : function (.x, .depth, .f, ...)  
                          attr_getter : function (attr)  
                            auto_browse : function (.f)  
                              chuck : function (.x, ...)  
                                compact : function (.x, .p = identity)  
                                  compose : function (..., .dir = c("backward", "forward"))  
                                    cross : function (.l, .filter = NULL)  
                                      cross_d : function (...)  
                                        cross_df : function (.l, .filter = NULL)  
                                          cross_n : function (...)  
                                            cross2 : function (.x, .y, .filter = NULL)  
                                              cross3 : function (.x, .y, .z, .filter = NULL)  
                                                detect : function (.x, .f, ..., .dir = c("forward", "backward"), .right = NULL, .default = NULL)  
                                                  detect_index : function (.x, .f, ..., .dir = c("forward", "backward"), .right = NULL)  
                                                    discard : function (.x, .p, ...)  
                                                      done : function (x)  
                                                        every : function (.x, .p, ...)  
                                                          exec : function (.fn, ..., .env = caller_env())  
                                                            flatten : function (.x)  
                                                              flatten_chr : function (.x)  
                                                                flatten_dbl : function (.x)  
                                                                  flatten_df : function (.x, .id = NULL)  
                                                                    flatten_dfc : function (.x)  
                                                                      flatten_dfr : function (.x, .id = NULL)  
                                                                        flatten_int : function (.x)  
                                                                          flatten_lgl : function (.x)  
                                                                            flatten_raw : function (.x)  
                                                                              has_element : function (.x, .y)  
                                                                                head_while : function (.x, .p, ...)  
                                                                                  imap : function (.x, .f, ...)  
                                                                                    imap_chr : function (.x, .f, ...)  
                                                                                      imap_dbl : function (.x, .f, ...)  
                                                                                        imap_dfc : function (.x, .f, ...)  
                                                                                          imap_dfr : function (.x, .f, ..., .id = NULL)  
                                                                                            imap_int : function (.x, .f, ...)  
                                                                                              imap_lgl : function (.x, .f, ...)  
                                                                                                imap_raw : function (.x, .f, ...)  
                                                                                                  imodify : function (.x, .f, ...)  
                                                                                                    insistently : function (f, rate = rate_backoff(), quiet = TRUE)  
                                                                                                      invoke : function (.f, .x = NULL, ..., .env = NULL)  
                                                                                                        invoke_map : function (.f, .x = list(NULL), ..., .env = NULL)  
                                                                                                          invoke_map_chr : function (.f, .x = list(NULL), ..., .env = NULL)  
                                                                                                            invoke_map_dbl : function (.f, .x = list(NULL), ..., .env = NULL)  
                                                                                                              invoke_map_df : function (.f, .x = list(NULL), ..., .env = NULL)  
                                                                                                                invoke_map_dfc : function (.f, .x = list(NULL), ..., .env = NULL)  
                                                                                                                  invoke_map_dfr : function (.f, .x = list(NULL), ..., .env = NULL)  
                                                                                                                    invoke_map_int : function (.f, .x = list(NULL), ..., .env = NULL)  
                                                                                                                      invoke_map_lgl : function (.f, .x = list(NULL), ..., .env = NULL)  
                                                                                                                        invoke_map_raw : function (.f, .x = list(NULL), ..., .env = NULL)  
                                                                                                                          is_atomic : function (x, n = NULL)  
                                                                                                                            is_bare_atomic : function (x, n = NULL)  
                                                                                                                              is_bare_character : function (x, n = NULL)  
                                                                                                                                is_bare_double : function (x, n = NULL)  
                                                                                                                                  is_bare_integer : function (x, n = NULL)  
                                                                                                                                    is_bare_list : function (x, n = NULL)  
                                                                                                                                      is_bare_logical : function (x, n = NULL)  
                                                                                                                                        is_bare_numeric : function (x, n = NULL)  
                                                                                                                                          is_bare_vector : function (x, n = NULL)  
                                                                                                                                            is_character : function (x, n = NULL)  
                                                                                                                                              is_double : function (x, n = NULL, finite = NULL)  
                                                                                                                                                is_empty : function (x)  
                                                                                                                                                  is_formula : function (x, scoped = NULL, lhs = NULL)  
                                                                                                                                                    is_function : function (x)  
                                                                                                                                                      is_integer : function (x, n = NULL)  
                                                                                                                                                        is_list : function (x, n = NULL)  
                                                                                                                                                          is_logical : function (x, n = NULL)  
                                                                                                                                                            is_null : function (x)  
                                                                                                                                                              is_numeric : function (x)  
                                                                                                                                                                is_rate : function (x)  
                                                                                                                                                                  is_scalar_atomic : function (x)  
                                                                                                                                                                    is_scalar_character : function (x)  
                                                                                                                                                                      is_scalar_double : function (x)  
                                                                                                                                                                        is_scalar_integer : function (x)  
                                                                                                                                                                          is_scalar_list : function (x)  
                                                                                                                                                                            is_scalar_logical : function (x)  
                                                                                                                                                                              is_scalar_numeric : function (x)  
                                                                                                                                                                                is_scalar_vector : function (x)  
                                                                                                                                                                                  is_vector : function (x, n = NULL)  
                                                                                                                                                                                    iwalk : function (.x, .f, ...)  
                                                                                                                                                                                      keep : function (.x, .p, ...)  
                                                                                                                                                                                        lift : function (..f, ..., .unnamed = FALSE)  
                                                                                                                                                                                          lift_dl : function (..f, ..., .unnamed = FALSE)  
                                                                                                                                                                                            lift_dv : function (..f, ..., .unnamed = FALSE)  
                                                                                                                                                                                              lift_ld : function (..f, ...)  
                                                                                                                                                                                                lift_lv : function (..f, ...)  
                                                                                                                                                                                                  lift_vd : function (..f, ..., .type)  
                                                                                                                                                                                                    lift_vl : function (..f, ..., .type)  
                                                                                                                                                                                                      list_along : function (x)  
                                                                                                                                                                                                        list_merge : function (.x, ...)  
                                                                                                                                                                                                          list_modify : function (.x, ...)  
                                                                                                                                                                                                            lmap : function (.x, .f, ...)  
                                                                                                                                                                                                              lmap_at : function (.x, .at, .f, ...)  
                                                                                                                                                                                                                lmap_if : function (.x, .p, .f, ..., .else = NULL)  
                                                                                                                                                                                                                  map : function (.x, .f, ...)  
                                                                                                                                                                                                                    map_at : function (.x, .at, .f, ...)  
                                                                                                                                                                                                                      map_call : function (.x, .f, ...)  
                                                                                                                                                                                                                        map_chr : function (.x, .f, ...)  
                                                                                                                                                                                                                          map_dbl : function (.x, .f, ...)  
                                                                                                                                                                                                                            map_depth : function (.x, .depth, .f, ..., .ragged = FALSE)  
                                                                                                                                                                                                                              map_df : function (.x, .f, ..., .id = NULL)  
                                                                                                                                                                                                                                map_dfc : function (.x, .f, ...)  
                                                                                                                                                                                                                                  map_dfr : function (.x, .f, ..., .id = NULL)  
                                                                                                                                                                                                                                    map_if : function (.x, .p, .f, ..., .else = NULL)  
                                                                                                                                                                                                                                      map_int : function (.x, .f, ...)  
                                                                                                                                                                                                                                        map_lgl : function (.x, .f, ...)  
                                                                                                                                                                                                                                          map_raw : function (.x, .f, ...)  
                                                                                                                                                                                                                                            map2 : function (.x, .y, .f, ...)  
                                                                                                                                                                                                                                              map2_chr : function (.x, .y, .f, ...)  
                                                                                                                                                                                                                                                map2_dbl : function (.x, .y, .f, ...)  
                                                                                                                                                                                                                                                  map2_df : function (.x, .y, .f, ..., .id = NULL)  
                                                                                                                                                                                                                                                    map2_dfc : function (.x, .y, .f, ...)  
                                                                                                                                                                                                                                                      map2_dfr : function (.x, .y, .f, ..., .id = NULL)  
                                                                                                                                                                                                                                                        map2_int : function (.x, .y, .f, ...)  
                                                                                                                                                                                                                                                          map2_lgl : function (.x, .y, .f, ...)  
                                                                                                                                                                                                                                                            map2_raw : function (.x, .y, .f, ...)  
                                                                                                                                                                                                                                                              modify : function (.x, .f, ...)  
                                                                                                                                                                                                                                                                modify_at : function (.x, .at, .f, ...)  
                                                                                                                                                                                                                                                                  modify_depth : function (.x, .depth, .f, ..., .ragged = .depth < 0)  
                                                                                                                                                                                                                                                                    modify_if : function (.x, .p, .f, ..., .else = NULL)  
                                                                                                                                                                                                                                                                      modify_in : function (.x, .where, .f, ...)  
                                                                                                                                                                                                                                                                        modify2 : function (.x, .y, .f, ...)  
                                                                                                                                                                                                                                                                          negate : function (.p)  
                                                                                                                                                                                                                                                                            none : function (.x, .p, ...)  
                                                                                                                                                                                                                                                                              partial : function (.f, ..., .env = NULL, .lazy = NULL, .first = NULL)  
                                                                                                                                                                                                                                                                                pluck : function (.x, ..., .default = NULL)  
                                                                                                                                                                                                                                                                                  pluck<- : function (.x, ..., value)  
                                                                                                                                                                                                                                                                                    pmap : function (.l, .f, ...)  
                                                                                                                                                                                                                                                                                      pmap_chr : function (.l, .f, ...)  
                                                                                                                                                                                                                                                                                        pmap_dbl : function (.l, .f, ...)  
                                                                                                                                                                                                                                                                                          pmap_df : function (.l, .f, ..., .id = NULL)  
                                                                                                                                                                                                                                                                                            pmap_dfc : function (.l, .f, ...)  
                                                                                                                                                                                                                                                                                              pmap_dfr : function (.l, .f, ..., .id = NULL)  
                                                                                                                                                                                                                                                                                                pmap_int : function (.l, .f, ...)  
                                                                                                                                                                                                                                                                                                  pmap_lgl : function (.l, .f, ...)  
                                                                                                                                                                                                                                                                                                    pmap_raw : function (.l, .f, ...)  
                                                                                                                                                                                                                                                                                                      possibly : function (.f, otherwise, quiet = TRUE)  
                                                                                                                                                                                                                                                                                                        prepend : function (x, values, before = NULL)  
                                                                                                                                                                                                                                                                                                          pwalk : function (.l, .f, ...)  
                                                                                                                                                                                                                                                                                                            quietly : function (.f)  
                                                                                                                                                                                                                                                                                                              rate_backoff : function (pause_base = 1, pause_cap = 60, pause_min = 1, max_times = 3, jitter = TRUE)  
                                                                                                                                                                                                                                                                                                                rate_delay : function (pause = 1, max_times = Inf)  
                                                                                                                                                                                                                                                                                                                  rate_reset : function (rate)  
                                                                                                                                                                                                                                                                                                                    rate_sleep : function (rate, quiet = TRUE)  
                                                                                                                                                                                                                                                                                                                      rbernoulli : function (n, p = 0.5)  
                                                                                                                                                                                                                                                                                                                        rdunif : function (n, b, a = 1)  
                                                                                                                                                                                                                                                                                                                          reduce : function (.x, .f, ..., .init, .dir = c("forward", "backward"))  
                                                                                                                                                                                                                                                                                                                            reduce_right : function (.x, .f, ..., .init)  
                                                                                                                                                                                                                                                                                                                              reduce2 : function (.x, .y, .f, ..., .init)  
                                                                                                                                                                                                                                                                                                                                reduce2_right : function (.x, .y, .f, ..., .init)  
                                                                                                                                                                                                                                                                                                                                  rep_along : function (along, x)  
                                                                                                                                                                                                                                                                                                                                    rerun : function (.n, ...)  
                                                                                                                                                                                                                                                                                                                                      safely : function (.f, otherwise = NULL, quiet = TRUE)  
                                                                                                                                                                                                                                                                                                                                        set_names : function (x, nm = x, ...)  
                                                                                                                                                                                                                                                                                                                                          simplify : function (.x, .type = NULL)  
                                                                                                                                                                                                                                                                                                                                            simplify_all : function (.x, .type = NULL)  
                                                                                                                                                                                                                                                                                                                                              slowly : function (f, rate = rate_delay(), quiet = TRUE)  
                                                                                                                                                                                                                                                                                                                                                some : function (.x, .p, ...)  
                                                                                                                                                                                                                                                                                                                                                  splice : function (...)  
                                                                                                                                                                                                                                                                                                                                                    tail_while : function (.x, .p, ...)  
                                                                                                                                                                                                                                                                                                                                                      transpose : function (.l, .names = NULL)  
                                                                                                                                                                                                                                                                                                                                                        update_list : function (.x, ...)  
                                                                                                                                                                                                                                                                                                                                                          vec_depth : function (x)  
                                                                                                                                                                                                                                                                                                                                                            walk : function (.x, .f, ...)  
                                                                                                                                                                                                                                                                                                                                                              walk2 : function (.x, .y, .f, ...)  
                                                                                                                                                                                                                                                                                                                                                                when : function (., ...)  
                                                                                                                                                                                                                                                                                                                                                                  zap : function ()  
                                                                                                                                                                                                                                                                                                                                                                    > 
library(Rcrawler)
browser_path : function ()  
  ContentScraper : function (Url, HTmlText, browser, XpathPatterns, CssPatterns, PatternsName, ExcludeXpathPat, 
                             ExcludeCSSPat, ManyPerPattern = FALSE, astext = TRUE, asDataFrame = FALSE, encod)  
    Drv_fetchpage : function (url, browser)  
      Getencoding : function (url)  
        install_browser : function (version = "2.1.1", baseURL = "https://github.com/wch/webshot/releases/download/v0.3.1/")  
          LinkExtractor : function (url, id, lev, IndexErrPages, Useragent, Timeout = 6, use_proxy = NULL, URLlenlimit = 255, 
                                    urlExtfilter, urlregexfilter, encod, urlbotfiler, removeparams, removeAllparams = FALSE, 
                                    ExternalLInks = FALSE, urlsZoneXpath = NULL, Browser, RenderingDelay = 0)  
            LinkNormalization : function (links, current)  
              Linkparameters : function (URL)  
                Linkparamsfilter : function (URL, params, removeAllparams = FALSE)  
                  ListProjects : function (DIR)  
                    LoadHTMLFiles : function (ProjectName, type = "vector", max)  
                      LoginSession : function (Browser, LoginURL, LoginCredentials, cssLoginFields, cssLoginButton, cssRadioToCheck, 
                                               XpathLoginFields, XpathLoginButton, XpathRadioToCheck)  
                        Rcrawler : function (Website, no_cores, no_conn, MaxDepth, DIR, RequestsDelay = 0, Obeyrobots = FALSE, 
                                             Useragent, use_proxy = NULL, Encod, Timeout = 5, URLlenlimit = 255, urlExtfilter, 
                                             dataUrlfilter, crawlUrlfilter, crawlZoneCSSPat = NULL, crawlZoneXPath = NULL, 
                                             ignoreUrlParams, ignoreAllUrlParams = FALSE, KeywordsFilter, KeywordsAccuracy, 
                                             FUNPageFilter, ExtractXpathPat, ExtractCSSPat, PatternsNames, ExcludeXpathPat, 
                                             ExcludeCSSPat, ExtractAsText = TRUE, ManyPerPattern = FALSE, saveOnDisk = TRUE, 
                                             NetworkData = FALSE, NetwExtLinks = FALSE, statslinks = FALSE, Vbrowser = FALSE, 
                                             LoggedSession)  
                          RobotParser : function (website, useragent)  
                            run_browser : function (debugLevel = "DEBUG", timeout = 5000)  
                              stop_browser : function (browser)  
                                > 
library(rmarkdown)
all_output_formats : function (input, output_yaml = NULL)  
  available_templates : function (package = "rmarkdown", full_path = FALSE)  
    beamer_presentation : function (toc = FALSE, slide_level = NULL, number_sections = FALSE, incremental = FALSE, 
                                    fig_width = 10, fig_height = 7, fig_crop = "auto", fig_caption = TRUE, dev = "pdf", 
                                    df_print = "default", theme = "default", colortheme = "default", fonttheme = "default", 
                                    highlight = "default", template = "default", keep_tex = FALSE, keep_md = FALSE, 
                                    latex_engine = "pdflatex", citation_package = c("default", "natbib", "biblatex"), 
                                    self_contained = TRUE, includes = NULL, md_extensions = NULL, pandoc_args = NULL)  
      clean_site : function (input = ".", preview = TRUE, quiet = FALSE, encoding = "UTF-8")  
        context_document : function (toc = FALSE, toc_depth = 2, number_sections = FALSE, fig_width = 6.5, fig_height = 4.5, 
                                     fig_crop = "auto", fig_caption = TRUE, dev = "pdf", df_print = "default", template = NULL, 
                                     keep_tex = FALSE, keep_md = FALSE, citation_package = c("default", "natbib", "biblatex"), 
                                     includes = NULL, md_extensions = NULL, output_extensions = NULL, pandoc_args = NULL, 
                                     context_path = NULL, context_args = NULL, ext = c(".pdf", ".tex"))  
          convert_ipynb : function (input, output = xfun::with_ext(input, "Rmd"))  
            default_output_format : function (input, output_yaml = NULL)  
              default_site_generator : function (input, output_format_filter = NULL, ...)  
                draft : function (file, template, package = NULL, create_dir = "default", edit = TRUE)  
                  find_external_resources : function (input_file, encoding = "UTF-8")  
                    find_pandoc : function (cache = TRUE, dir = NULL, version = NULL)  
                      from_rmarkdown : function (implicit_figures = TRUE, extensions = NULL)  
                        github_document : function (toc = FALSE, toc_depth = 3, number_sections = FALSE, math_method = "webtex", 
                                                    preserve_yaml = FALSE, fig_width = 7, fig_height = 5, dev = "png", df_print = "default", 
                                                    includes = NULL, md_extensions = NULL, hard_line_breaks = TRUE, pandoc_args = NULL, 
                                                    html_preview = TRUE, keep_html = FALSE)  
                          html_dependency_bootstrap : function (theme)  
                            html_dependency_codefolding_lua : function ()  
                              html_dependency_font_awesome : function ()  
                                html_dependency_highlightjs : function (highlight)  
                                  html_dependency_ionicons : function ()  
                                    html_dependency_jquery : function ()  
                                      html_dependency_jqueryui : function ()  
                                        html_dependency_pagedtable : function ()  
                                          html_dependency_tabset : function ()  
                                            html_dependency_tocify : function ()  
                                              html_document : function (toc = FALSE, toc_depth = 3, toc_float = FALSE, number_sections = FALSE, 
                                                                        anchor_sections = FALSE, section_divs = TRUE, fig_width = 7, fig_height = 5, fig_retina = 2, 
                                                                        fig_caption = TRUE, dev = "png", df_print = "default", code_folding = c("none", 
                                                                                                                                                "show", "hide"), code_download = FALSE, self_contained = TRUE, theme = "default", 
                                                                        highlight = "default", highlight_downlit = FALSE, math_method = "default", mathjax = "default", 
                                                                        template = "default", extra_dependencies = NULL, css = NULL, includes = NULL, 
                                                                        keep_md = FALSE, lib_dir = NULL, md_extensions = NULL, pandoc_args = NULL, ...)  
                                                html_document_base : function (theme = NULL, self_contained = TRUE, lib_dir = NULL, math_method = "default", 
                                                                               mathjax = "default", pandoc_args = NULL, template = "default", dependency_resolver = NULL, 
                                                                               copy_resources = FALSE, extra_dependencies = NULL, css = NULL, bootstrap_compatible = FALSE, 
                                                                               ...)  
                                                  html_fragment : function (number_sections = FALSE, section_divs = TRUE, fig_width = 7, fig_height = 5, 
                                                                            fig_retina = 2, fig_caption = TRUE, dev = "png", df_print = "default", mathjax = TRUE, 
                                                                            includes = NULL, keep_md = FALSE, md_extensions = NULL, pandoc_args = NULL, ...)  
                                                    html_notebook : function (toc = FALSE, toc_depth = 3, toc_float = FALSE, number_sections = FALSE, 
                                                                              fig_width = 7, fig_height = 5, fig_retina = 2, fig_caption = TRUE, code_folding = "show", 
                                                                              theme = "default", highlight = "textmate", highlight_downlit = FALSE, math_method = "default", 
                                                                              mathjax = "default", extra_dependencies = NULL, css = NULL, includes = NULL, md_extensions = NULL, 
                                                                              pandoc_args = NULL, output_source = NULL, self_contained = TRUE, ...)  
                                                      html_notebook_metadata : function (iframe = TRUE)  
                                                        html_notebook_output_code : function (code, attributes = list(class = "r"), meta = NULL)  
                                                          html_notebook_output_html : function (html, meta = NULL)  
                                                            html_notebook_output_img : function (path = NULL, bytes = NULL, attributes = NULL, meta = NULL, format = c("png", 
                                                                                                                                                                       "jpeg"))  
                                                              html_notebook_output_png : function (path = NULL, bytes = NULL, attributes = NULL, meta = NULL, format = c("png", 
                                                                                                                                                                         "jpeg"))  
                                                                html_vignette : function (fig_width = 3, fig_height = 3, dev = "png", df_print = "default", css = NULL, 
                                                                                          highlight = "pygments", keep_md = FALSE, readme = FALSE, self_contained = TRUE, 
                                                                                          tabset = FALSE, code_folding = c("none", "show", "hide"), extra_dependencies = NULL, 
                                                                                          pandoc_args = NULL, ...)  
                                                                  includes : function (in_header = NULL, before_body = NULL, after_body = NULL)  
                                                                    includes_to_pandoc_args : function (includes, filter = identity)  
                                                                      ioslides_presentation : function (number_sections = FALSE, logo = NULL, slide_level = 2, incremental = FALSE, 
                                                                                                        fig_width = 7.5, fig_height = 4.5, fig_retina = 2, fig_caption = TRUE, dev = "png", 
                                                                                                        df_print = "default", smart = TRUE, self_contained = TRUE, widescreen = FALSE, 
                                                                                                        smaller = FALSE, transition = "default", math_method = "mathjax", mathjax = "default", 
                                                                                                        analytics = NULL, template = NULL, css = NULL, includes = NULL, keep_md = FALSE, 
                                                                                                        lib_dir = NULL, md_extensions = NULL, pandoc_args = NULL, extra_dependencies = NULL, 
                                                                                                        ...)  
                                                                        knit_params_ask : function (file = NULL, input_lines = NULL, params = NULL, shiny_args = NULL, save_caption = "Save", 
                                                                                                    encoding = "UTF-8")  
                                                                          knitr_options : function (opts_knit = NULL, opts_chunk = NULL, knit_hooks = NULL, opts_hooks = NULL, 
                                                                                                    opts_template = NULL)  
                                                                            knitr_options_html : function (fig_width, fig_height, fig_retina, keep_md, dev = "png")  
                                                                              knitr_options_pdf : function (fig_width, fig_height, fig_crop, dev = "pdf")  
                                                                                latex_dependency : function (name, options = NULL, extra_lines = NULL)  
                                                                                  latex_dependency_tikz : function (libraries, options = NULL, extra_lines = NULL)  
                                                                                    latex_document : function (...)  
                                                                                      latex_fragment : function (...)  
                                                                                        md_document : function (variant = "markdown_strict", preserve_yaml = FALSE, toc = FALSE, toc_depth = 3, 
                                                                                                                number_sections = FALSE, standalone = FALSE, fig_width = 7, fig_height = 5, fig_retina = NULL, 
                                                                                                                dev = "png", df_print = "default", includes = NULL, md_extensions = NULL, pandoc_args = NULL, 
                                                                                                                ext = ".md")  
                                                                                          metadata :  list()
navbar_html : function (navbar)  
  navbar_links_html : function (links)  
    odt_document : function (number_sections = FALSE, fig_width = 5, fig_height = 4, fig_caption = TRUE, 
                             template = "default", reference_odt = "default", includes = NULL, keep_md = FALSE, 
                             md_extensions = NULL, pandoc_args = NULL)  
      output_format : function (knitr, pandoc, keep_md = FALSE, clean_supporting = TRUE, df_print = NULL, 
                                pre_knit = NULL, post_knit = NULL, pre_processor = NULL, intermediates_generator = NULL, 
                                post_processor = NULL, on_exit = NULL, file_scope = NULL, base_format = NULL)  
        output_metadata : List of 6
$ get    :function (name, default = FALSE, drop = TRUE)  
  $ set    :function (...)  
    $ delete :function (keys)  
      $ append :function (...)  
        $ merge  :function (values)  
          $ restore:function (target = value)  
            paged_table : function (x, options = NULL)  
              pandoc_available : function (version = NULL, error = FALSE)  
                pandoc_citeproc_args : function ()  
                  pandoc_citeproc_convert : function (file, type = c("list", "json", "yaml"))  
                    pandoc_convert : function (input, to = NULL, from = NULL, output = NULL, citeproc = FALSE, options = NULL, 
                                               verbose = FALSE, wd = NULL)  
                      pandoc_exec : function ()  
                        pandoc_highlight_args : function (highlight, default = "tango")  
                          pandoc_include_args : function (in_header = NULL, before_body = NULL, after_body = NULL)  
                            pandoc_latex_engine_args : function (latex_engine)  
                              pandoc_lua_filter_args : function (lua_files)  
                                pandoc_metadata_arg : function (name, value)  
                                  pandoc_options : function (to, from = rmarkdown_format(), args = NULL, keep_tex = FALSE, latex_engine = c("pdflatex", 
                                                                                                                                            "lualatex", "xelatex", "tectonic"), ext = NULL, lua_filters = NULL)  
                                    pandoc_path_arg : function (path, backslash = TRUE)  
                                      pandoc_self_contained_html : function (input, output)  
                                        pandoc_template : function (metadata, template, output, verbose = FALSE)  
                                          pandoc_toc_args : function (toc, toc_depth = 3)  
                                            pandoc_variable_arg : function (name, value)  
                                              pandoc_version : function ()  
                                                parse_html_notebook : function (path)  
                                                  pdf_document : function (toc = FALSE, toc_depth = 2, number_sections = FALSE, fig_width = 6.5, fig_height = 4.5, 
                                                                           fig_crop = "auto", fig_caption = TRUE, dev = "pdf", df_print = "default", highlight = "default", 
                                                                           template = "default", keep_tex = FALSE, keep_md = FALSE, latex_engine = "pdflatex", 
                                                                           citation_package = c("default", "natbib", "biblatex"), includes = NULL, md_extensions = NULL, 
                                                                           output_extensions = NULL, pandoc_args = NULL, extra_dependencies = NULL)  
                                                    pkg_file_lua : function (filters = NULL, package = "rmarkdown")  
                                                      powerpoint_presentation : function (toc = FALSE, toc_depth = 2, number_sections = FALSE, incremental = FALSE, 
                                                                                          fig_width = 5, fig_height = 4, fig_caption = TRUE, df_print = "default", keep_md = FALSE, 
                                                                                          md_extensions = NULL, slide_level = NULL, reference_doc = "default", pandoc_args = NULL)  
                                                        publish_site : function (site_dir = ".", site_name = NULL, method = c("rsconnect"), server = NULL, 
                                                                                 account = NULL, render = TRUE, launch_browser = interactive())  
                                                          relative_to : function (dir, file)  
                                                            render : function (input, output_format = NULL, output_file = NULL, output_dir = NULL, output_options = NULL, 
                                                                               output_yaml = NULL, intermediates_dir = NULL, knit_root_dir = NULL, runtime = c("auto", 
                                                                                                                                                               "static", "shiny", "shinyrmd", "shiny_prerendered"), clean = TRUE, params = NULL, 
                                                                               knit_meta = NULL, envir = parent.frame(), run_pandoc = TRUE, quiet = FALSE, encoding = "UTF-8")  
                                                              render_delayed : function (expr)  
                                                                render_site : function (input = ".", output_format = "all", envir = parent.frame(), quiet = FALSE, 
                                                                                        encoding = "UTF-8")  
                                                                  render_supporting_files : function (from, files_dir, rename_to = NULL)  
                                                                    resolve_output_format : function (input, output_format = NULL, output_options = NULL, output_yaml = NULL)  
                                                                      rmarkdown_format : function (extensions = NULL)  
                                                                        rtf_document : function (toc = FALSE, toc_depth = 3, number_sections = FALSE, fig_width = 5, fig_height = 4, 
                                                                                                 keep_md = FALSE, md_extensions = NULL, pandoc_args = NULL)  
                                                                          run : function (file = "index.Rmd", dir = dirname(file), default_file = NULL, auto_reload = TRUE, 
                                                                                          shiny_args = NULL, render_args = NULL)  
                                                                            shiny_prerendered_chunk : function (context, code, singleton = FALSE)  
                                                                              shiny_prerendered_clean : function (input)  
                                                                                shiny_prerendered_server_start_code : function (server_envir)  
                                                                                  site_config : function (input = ".", encoding = "UTF-8")  
                                                                                    site_generator : function (input = ".", output_format = NULL)  
                                                                                      site_resources : function (site_dir, include = NULL, exclude = NULL, recursive = FALSE)  
                                                                                        slidy_presentation : function (number_sections = FALSE, incremental = FALSE, slide_level = NULL, duration = NULL, 
                                                                                                                       footer = NULL, font_adjustment = 0, fig_width = 8, fig_height = 6, fig_retina = 2, 
                                                                                                                       fig_caption = TRUE, dev = "png", df_print = "default", self_contained = TRUE, 
                                                                                                                       highlight = "default", math_method = "default", mathjax = "default", template = "default", 
                                                                                                                       css = NULL, includes = NULL, keep_md = FALSE, lib_dir = NULL, md_extensions = NULL, 
                                                                                                                       pandoc_args = NULL, extra_dependencies = NULL, ...)  
                                                                                          tufte_handout : function (fig_width = 4, fig_height = 2.5, fig_crop = TRUE, dev = "pdf", highlight = "default", 
                                                                                                                    keep_tex = FALSE, citation_package = c("default", "natbib", "biblatex"), includes = NULL, 
                                                                                                                    md_extensions = NULL, pandoc_args = NULL)  
                                                                                            word_document : function (toc = FALSE, toc_depth = 3, number_sections = FALSE, fig_width = 5, fig_height = 4, 
                                                                                                                      fig_caption = TRUE, df_print = "default", highlight = "default", reference_docx = "default", 
                                                                                                                      keep_md = FALSE, md_extensions = NULL, pandoc_args = NULL)  
                                                                                              yaml_front_matter : function (input, encoding = "UTF-8")  
                                                                                                > 
library(readr)
AccumulateCallback : Class 'R6ClassGenerator' <AccumulateCallback> object generator
Inherits from: <ChunkCallback>
Public:
initialize: function (callback, acc = NULL) 
receive: function (data, index) 
result: function () 
clone: function (deep = FALSE) 
Private:
acc: NULL
Parent env: <environment: namespace:readr>
Locked objects: TRUE
Locked class: FALSE
Portable: TRUE 
as.col_spec : function (x)  
ChunkCallback : Class 'R6ClassGenerator' <ChunkCallback> object generator
Public:
initialize: function (callback) 
receive: function (data, index) 
continue: function () 
result: function () 
finally: function () 
clone: function (deep = FALSE) 
Private:
callback: NULL
Parent env: <environment: namespace:readr>
Locked objects: TRUE
Locked class: FALSE
Portable: TRUE 
clipboard : function ()  
col_character : function ()  
col_date : function (format = "")  
col_datetime : function (format = "")  
col_double : function ()  
col_factor : function (levels = NULL, ordered = FALSE, include_na = FALSE)  
col_guess : function ()  
col_integer : function ()  
col_logical : function ()  
col_number : function ()  
col_skip : function ()  
col_time : function (format = "")  
cols : function (..., .default = col_guess())  
cols_condense : function (x)  
cols_only : function (...)  
count_fields : function (file, tokenizer, skip = 0, n_max = -1L)  
DataFrameCallback : Class 'R6ClassGenerator' <DataFrameCallback> object generator
Inherits from: <ChunkCallback>
Public:
initialize: function (callback) 
receive: function (data, index) 
result: function () 
finally: function () 
clone: function (deep = FALSE) 
Private:
results: list
Parent env: <environment: namespace:readr>
Locked objects: TRUE
Locked class: FALSE
Portable: TRUE 
datasource : function (file, skip = 0, skip_empty_rows = FALSE, comment = "", skip_quote = TRUE)  
date_names : function (mon, mon_ab = mon, day, day_ab = day, am_pm = c("AM", "PM"))  
date_names_lang : function (language)  
date_names_langs : function ()  
default_locale : function ()  
edition_get : function ()  
format_csv : function (x, na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", 
"none"), escape = c("double", "backslash", "none"), eol = "\n", quote_escape = deprecated())  
format_csv2 : function (x, na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", 
"none"), escape = c("double", "backslash", "none"), eol = "\n", quote_escape = deprecated())  
format_delim : function (x, delim, na = "NA", append = FALSE, col_names = !append, quote = c("needed", 
"all", "none"), escape = c("double", "backslash", "none"), eol = "\n", quote_escape = deprecated())  
format_tsv : function (x, na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", 
"none"), escape = c("double", "backslash", "none"), eol = "\n", quote_escape = deprecated())  
fwf_cols : function (...)  
fwf_empty : function (file, skip = 0, skip_empty_rows = FALSE, col_names = NULL, comment = "", 
n = 100L)  
fwf_positions : function (start, end = NULL, col_names = NULL)  
fwf_widths : function (widths, col_names = NULL)  
guess_encoding : function (file, n_max = 10000, threshold = 0.2)  
guess_parser : function (x, locale = default_locale(), guess_integer = FALSE, na = c("", "NA"))  
ListCallback : Class 'R6ClassGenerator' <ListCallback> object generator
Inherits from: <ChunkCallback>
Public:
initialize: function (callback) 
receive: function (data, index) 
result: function () 
finally: function () 
clone: function (deep = FALSE) 
Private:
  results: list
Parent env: <environment: namespace:readr>
  Locked objects: TRUE
Locked class: FALSE
Portable: TRUE 
local_edition : function (edition, env = parent.frame())  
locale : function (date_names = "en", date_format = "%AD", time_format = "%AT", decimal_mark = ".", 
grouping_mark = ",", tz = "UTC", encoding = "UTF-8", asciify = FALSE)  
melt_csv : function (file, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", 
comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, progress = show_progress(), 
skip_empty_rows = FALSE)  
melt_csv_chunked : function (file, callback, chunk_size = 10000, locale = default_locale(), na = c("", 
"NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, 
                                   progress = show_progress(), skip_empty_rows = FALSE)  
        melt_csv2 : function (file, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", 
                              comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, progress = show_progress(), 
                              skip_empty_rows = FALSE)  
          melt_csv2_chunked : function (file, callback, chunk_size = 10000, locale = default_locale(), na = c("", 
"NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, 
progress = show_progress(), skip_empty_rows = FALSE)  
melt_delim : function (file, delim, quote = "\"", escape_backslash = FALSE, escape_double = TRUE, 
locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, 
skip = 0, n_max = Inf, progress = show_progress(), skip_empty_rows = FALSE)  
melt_delim_chunked : function (file, callback, chunk_size = 10000, delim, quote = "\"", escape_backslash = FALSE, 
escape_double = TRUE, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, 
comment = "", trim_ws = FALSE, skip = 0, progress = show_progress(), skip_empty_rows = FALSE)  
melt_fwf : function (file, col_positions, locale = default_locale(), na = c("", "NA"), comment = "", 
trim_ws = TRUE, skip = 0, n_max = Inf, progress = show_progress(), skip_empty_rows = FALSE)  
melt_table : function (file, locale = default_locale(), na = "NA", skip = 0, n_max = Inf, guess_max = min(n_max, 
1000), progress = show_progress(), comment = "", skip_empty_rows = FALSE)  
melt_table2 : function (file, locale = default_locale(), na = "NA", skip = 0, n_max = Inf, progress = show_progress(), 
comment = "", skip_empty_rows = FALSE)  
melt_tsv : function (file, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", 
comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, progress = show_progress(), 
skip_empty_rows = FALSE)  
melt_tsv_chunked : function (file, callback, chunk_size = 10000, locale = default_locale(), na = c("", 
"NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, 
progress = show_progress(), skip_empty_rows = FALSE)  
output_column : function (x, name)  
parse_character : function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)  
parse_date : function (x, format = "", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)  
parse_datetime : function (x, format = "", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)  
parse_double : function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)  
parse_factor : function (x, levels = NULL, ordered = FALSE, na = c("", "NA"), locale = default_locale(), 
include_na = TRUE, trim_ws = TRUE)  
parse_guess : function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE, guess_integer = FALSE)  
parse_integer : function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)  
parse_logical : function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)  
parse_number : function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)  
parse_time : function (x, format = "", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)  
parse_vector : function (x, collector, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)  
problems : function (x = .Last.value)  
read_builtin : function (x, package = NULL)  
read_csv : function (file, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, 
locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), name_repair = "unique", 
num_threads = readr_threads(), progress = show_progress(), show_col_types = should_show_types(), 
skip_empty_rows = TRUE, lazy = should_read_lazy())  
read_csv_chunked : function (file, callback, chunk_size = 10000, col_names = TRUE, col_types = NULL, 
locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
trim_ws = TRUE, skip = 0, guess_max = chunk_size, progress = show_progress(), 
show_col_types = should_show_types(), skip_empty_rows = TRUE)  
read_csv2 : function (file, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, 
locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = show_progress(), 
name_repair = "unique", num_threads = readr_threads(), show_col_types = should_show_types(), 
skip_empty_rows = TRUE, lazy = should_read_lazy())  
read_csv2_chunked : function (file, callback, chunk_size = 10000, col_names = TRUE, col_types = NULL, 
locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
trim_ws = TRUE, skip = 0, guess_max = chunk_size, progress = show_progress(), 
show_col_types = should_show_types(), skip_empty_rows = TRUE)  
read_delim : function (file, delim = NULL, quote = "\"", escape_backslash = FALSE, escape_double = TRUE, 
col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = default_locale(), 
na = c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, n_max = Inf, 
guess_max = min(1000, n_max), name_repair = "unique", num_threads = readr_threads(), 
progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE, 
lazy = should_read_lazy())  
read_delim_chunked : function (file, callback, delim = NULL, chunk_size = 10000, quote = "\"", escape_backslash = FALSE, 
escape_double = TRUE, col_names = TRUE, col_types = NULL, locale = default_locale(), 
na = c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, guess_max = chunk_size, 
progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE)  
read_file : function (file, locale = default_locale())  
read_file_raw : function (file)  
read_fwf : function (file, col_positions = fwf_empty(file, skip, n = guess_max), col_types = NULL, 
col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), comment = "", 
trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(n_max, 1000), progress = show_progress(), 
name_repair = "unique", num_threads = readr_threads(), show_col_types = should_show_types(), 
lazy = should_read_lazy(), skip_empty_rows = TRUE)  
read_lines : function (file, skip = 0, skip_empty_rows = FALSE, n_max = Inf, locale = default_locale(), 
na = character(), lazy = should_read_lazy(), num_threads = readr_threads(), progress = show_progress())  
read_lines_chunked : function (file, callback, chunk_size = 10000, skip = 0, locale = default_locale(), 
na = character(), progress = show_progress())  
read_lines_raw : function (file, skip = 0, n_max = -1L, num_threads = readr_threads(), progress = show_progress())  
read_lines_raw_chunked : function (file, callback, chunk_size = 10000, skip = 0, progress = show_progress())  
read_log : function (file, col_names = FALSE, col_types = NULL, trim_ws = TRUE, skip = 0, n_max = Inf, 
show_col_types = should_show_types(), progress = show_progress())  
read_rds : function (file, refhook = NULL)  
read_table : function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), na = "NA", 
skip = 0, n_max = Inf, guess_max = min(n_max, 1000), progress = show_progress(), 
comment = "", show_col_types = should_show_types(), skip_empty_rows = TRUE)  
read_table2 : function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), na = "NA", 
skip = 0, n_max = Inf, guess_max = min(n_max, 1000), progress = show_progress(), 
comment = "", skip_empty_rows = TRUE)  
read_tsv : function (file, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, 
locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = show_progress(), 
name_repair = "unique", num_threads = readr_threads(), show_col_types = should_show_types(), 
skip_empty_rows = TRUE, lazy = should_read_lazy())  
read_tsv_chunked : function (file, callback, chunk_size = 10000, col_names = TRUE, col_types = NULL, 
locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
trim_ws = TRUE, skip = 0, guess_max = chunk_size, progress = show_progress(), 
show_col_types = should_show_types(), skip_empty_rows = TRUE)  
readr_example : function (file = NULL)  
readr_threads : function ()  
should_read_lazy : function ()  
should_show_types : function ()  
show_progress : function ()  
SideEffectChunkCallback : Class 'R6ClassGenerator' <SideEffectChunkCallback> object generator
Inherits from: <ChunkCallback>
  Public:
  initialize: function (callback) 
    receive: function (data, index) 
      continue: function () 
        clone: function (deep = FALSE) 
          Private:
  cancel: FALSE
Parent env: <environment: namespace:readr>
  Locked objects: TRUE
Locked class: FALSE
Portable: TRUE 
spec : function (x)  
  spec_csv : function (file, col_names = TRUE, col_types = list(), col_select = NULL, id = NULL, 
locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, name_repair = "unique", 
num_threads = readr_threads(), progress = show_progress(), show_col_types = should_show_types(), 
skip_empty_rows = TRUE, lazy = should_read_lazy())  
spec_csv2 : function (file, col_names = TRUE, col_types = list(), col_select = NULL, id = NULL, 
locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, progress = show_progress(), 
name_repair = "unique", num_threads = readr_threads(), show_col_types = should_show_types(), 
skip_empty_rows = TRUE, lazy = should_read_lazy())  
spec_delim : function (file, delim = NULL, quote = "\"", escape_backslash = FALSE, escape_double = TRUE, 
col_names = TRUE, col_types = list(), col_select = NULL, id = NULL, locale = default_locale(), 
na = c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, n_max = 0, 
guess_max = 1000, name_repair = "unique", num_threads = readr_threads(), progress = show_progress(), 
show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy())  
spec_table : function (file, col_names = TRUE, col_types = list(), locale = default_locale(), na = "NA", 
skip = 0, n_max = 0, guess_max = 1000, progress = show_progress(), comment = "", 
show_col_types = should_show_types(), skip_empty_rows = TRUE)  
spec_tsv : function (file, col_names = TRUE, col_types = list(), col_select = NULL, id = NULL, 
locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, progress = show_progress(), 
name_repair = "unique", num_threads = readr_threads(), show_col_types = should_show_types(), 
skip_empty_rows = TRUE, lazy = should_read_lazy())  
stop_for_problems : function (x)  
tokenize : function (file, tokenizer = tokenizer_csv(), skip = 0, n_max = -1L)  
tokenizer_csv : function (na = "NA", quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, 
skip_empty_rows = TRUE)  
tokenizer_delim : function (delim, quote = "\"", na = "NA", quoted_na = TRUE, comment = "", trim_ws = TRUE, 
escape_double = TRUE, escape_backslash = FALSE, skip_empty_rows = TRUE)  
tokenizer_fwf : function (begin, end, na = "NA", comment = "", trim_ws = TRUE, skip_empty_rows = TRUE)  
tokenizer_line : function (na = character(), skip_empty_rows = TRUE)  
tokenizer_log : function (trim_ws)  
tokenizer_tsv : function (na = "NA", quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, 
skip_empty_rows = TRUE)  
tokenizer_ws : function (na = "NA", comment = "", skip_empty_rows = TRUE)  
type_convert : function (df, col_types = NULL, na = c("", "NA"), trim_ws = TRUE, locale = default_locale(), 
guess_integer = FALSE)  
with_edition : function (edition, code)  
write_csv : function (x, file, na = "NA", append = FALSE, col_names = !append, quote = c("needed", 
"all", "none"), escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), 
progress = show_progress(), path = deprecated(), quote_escape = deprecated())  
write_csv2 : function (x, file, na = "NA", append = FALSE, col_names = !append, quote = c("needed", 
"all", "none"), escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), 
progress = show_progress(), path = deprecated(), quote_escape = deprecated())  
write_delim : function (x, file, delim = " ", na = "NA", append = FALSE, col_names = !append, quote = c("needed", 
"all", "none"), escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), 
progress = show_progress(), path = deprecated(), quote_escape = deprecated())  
write_excel_csv : function (x, file, na = "NA", append = FALSE, col_names = !append, delim = ",", quote = "all", 
escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), 
progress = show_progress(), path = deprecated(), quote_escape = deprecated())  
write_excel_csv2 : function (x, file, na = "NA", append = FALSE, col_names = !append, delim = ";", quote = "all", 
escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), 
progress = show_progress(), path = deprecated(), quote_escape = deprecated())  
write_file : function (x, file, append = FALSE, path = deprecated())  
write_lines : function (x, file, sep = "\n", na = "NA", append = FALSE, num_threads = readr_threads(), 
path = deprecated())  
write_rds : function (x, file, compress = c("none", "gz", "bz2", "xz"), version = 2, refhook = NULL, 
text = FALSE, path = deprecated(), ...)  
write_tsv : function (x, file, na = "NA", append = FALSE, col_names = !append, quote = "none", 
escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), 
progress = show_progress(), path = deprecated(), quote_escape = deprecated())  

library(readxl)
anchored : function (anchor = "A1", dim = c(1L, 1L), input = NULL, col_names = NULL, byrow = FALSE)  
cell_cols : function (x)  
cell_limits : function (ul = c(NA_integer_, NA_integer_), lr = c(NA_integer_, NA_integer_), sheet = NA_character_)  
cell_rows : function (x)  
excel_format : function (path, guess = TRUE)  
excel_sheets : function (path)  
format_from_ext : function (path)  
format_from_signature : function (path)  
read_excel : function (path, sheet = NULL, range = NULL, col_names = TRUE, col_types = NULL, na = "", 
trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = readxl_progress(), 
.name_repair = "unique")  
read_xls : function (path, sheet = NULL, range = NULL, col_names = TRUE, col_types = NULL, na = "", 
trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = readxl_progress(), 
.name_repair = "unique")  
read_xlsx : function (path, sheet = NULL, range = NULL, col_names = TRUE, col_types = NULL, na = "", 
trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = readxl_progress(), 
.name_repair = "unique")  
readxl_example : function (path = NULL)  
readxl_progress : function ()  

library(RODBC)
etSqlTypeInfo : function (driver)  
odbcClearError : function (channel)  
odbcClose : function (channel)  
odbcCloseAll : function ()  
odbcConnect : function (dsn, uid = "", pwd = "", ...)  
odbcConnectAccess : function (access.file, uid = "", pwd = "", ...)  
odbcConnectAccess2007 : function (access.file, uid = "", pwd = "", ...)  
odbcConnectDbase : function (dbf.file, ...)  
odbcConnectExcel : function (xls.file, readOnly = TRUE, ...)  
odbcConnectExcel2007 : function (xls.file, readOnly = TRUE, ...)  
odbcDataSources : function (type = c("all", "user", "system"))  
odbcDriverConnect : function (connection = "", case = "nochange", believeNRows = TRUE, colQuote, tabQuote = colQuote, 
interpretDot = TRUE, DBMSencoding = "", rows_at_time = 100, readOnlyOptimize = FALSE)  
odbcEndTran : function (channel, commit = TRUE)  
odbcFetchRows : function (channel, max = 0, buffsize = 1000, nullstring = NA_character_, believeNRows = TRUE)  
odbcGetErrMsg : function (channel)  
odbcGetInfo : function (channel)  
odbcQuery : function (channel, query, rows_at_time = attr(channel, "rows_at_time"))  
odbcReConnect : function (channel, ...)  
odbcSetAutoCommit : function (channel, autoCommit = TRUE)  
odbcTables : function (channel, catalog = NULL, schema = NULL, tableName = NULL, tableType = NULL, 
literal = FALSE)  
odbcUpdate : function (channel, query, data, params, test = FALSE, verbose = FALSE, nastring = NULL)  
setSqlTypeInfo : function (driver, value)  
sqlClear : function (channel, sqtable, errors = TRUE)  
sqlColumns : function (channel, sqtable, errors = FALSE, as.is = TRUE, special = FALSE, catalog = NULL, 
schema = NULL, literal = FALSE)  
sqlCopy : function (channel, query, destination, destchannel = channel, verbose = FALSE, errors = TRUE, 
...)  
sqlCopyTable : function (channel, srctable, desttable, destchannel = channel, verbose = FALSE, errors = TRUE)  
sqlDrop : function (channel, sqtable, errors = TRUE)  
sqlFetch : function (channel, sqtable, ..., colnames = FALSE, rownames = TRUE)  
sqlFetchMore : function (channel, ..., colnames = FALSE, rownames = TRUE)  
sqlGetResults : function (channel, as.is = FALSE, errors = FALSE, max = 0, buffsize = 1000, nullstring = NA_character_, 
na.strings = "NA", believeNRows = TRUE, dec = getOption("dec"), stringsAsFactors = FALSE)  
sqlPrimaryKeys : function (channel, sqtable, errors = FALSE, as.is = TRUE, catalog = NULL, schema = NULL)  
sqlQuery : function (channel, query, errors = TRUE, ..., rows_at_time)  
sqlSave : function (channel, dat, tablename = NULL, append = FALSE, rownames = TRUE, colnames = FALSE, 
verbose = FALSE, safer = TRUE, addPK = FALSE, typeInfo, varTypes, fast = TRUE, 
test = FALSE, nastring = NULL)  
sqlTables : function (channel, errors = FALSE, as.is = TRUE, catalog = NULL, schema = NULL, tableName = NULL, 
tableType = NULL, literal = FALSE)  
sqlTypeInfo : function (channel, type = "all", errors = TRUE, as.is = TRUE)  
sqlUpdate : function (channel, dat, tablename = NULL, index = NULL, verbose = FALSE, test = FALSE, 
nastring = NULL, fast = TRUE)  

library(shiny)
..stacktraceoff.. : function (expr)  
..stacktraceon.. : function (expr)  
.Depends :  chr "methods"
a : function (..., .noWS = NULL, .renderHook = NULL)  
absolutePanel : function (..., top = NULL, left = NULL, right = NULL, bottom = NULL, width = NULL, 
height = NULL, draggable = FALSE, fixed = FALSE, cursor = c("auto", "move", "default", 
"inherit"))  
actionButton : function (inputId, label, icon = NULL, width = NULL, ...)  
actionLink : function (inputId, label, icon = NULL, ...)  
addResourcePath : function (prefix, directoryPath)  
animationOptions : function (interval = 1000, loop = FALSE, playButton = NULL, pauseButton = NULL)  
appendTab : function (inputId, tab, select = FALSE, menuName = NULL, session = getDefaultReactiveDomain())  
as.shiny.appobj : function (x)  
basicPage : function (...)  
bindCache : function (x, ..., cache = "app")  
bindEvent : function (x, ..., ignoreNULL = TRUE, ignoreInit = FALSE, once = FALSE, label = NULL)  
bookmarkButton : function (label = "Bookmark...", icon = shiny::icon("link", lib = "glyphicon"), title = "Bookmark this application's state and get a URL for sharing.", 
..., id = "._bookmark_")  
bootstrapLib : function (theme = NULL)  
bootstrapPage : function (..., title = NULL, theme = NULL, lang = NULL)  
br : function (..., .noWS = NULL, .renderHook = NULL)  
browserViewer : function (browser = getOption("browser"))  
brushedPoints : function (df, brush, xvar = NULL, yvar = NULL, panelvar1 = NULL, panelvar2 = NULL, 
allRows = FALSE)  
brushOpts : function (id, fill = "#9cf", stroke = "#036", opacity = 0.25, delay = 300, delayType = c("debounce", 
"throttle"), clip = TRUE, direction = c("xy", "x", "y"), resetOnNew = FALSE)  
callModule : function (module, id, ..., session = getDefaultReactiveDomain())  
captureStackTraces : function (expr)  
checkboxGroupInput : function (inputId, label, choices = NULL, selected = NULL, inline = FALSE, width = NULL, 
choiceNames = NULL, choiceValues = NULL)  
checkboxInput : function (inputId, label, value = FALSE, width = NULL)  
clickOpts : function (id, clip = TRUE)  
code : function (..., .noWS = NULL, .renderHook = NULL)  
column : function (width, ..., offset = 0)  
conditionalPanel : function (condition, ..., ns = NS(NULL))  
conditionStackTrace : function (cond)  
conditionStackTrace<- : function (cond, value)  
createRenderFunction : function (func, transform = function(value, session, name, ...) value, outputFunc = NULL, 
outputArgs = NULL, cacheHint = "auto", cacheWriteHook = NULL, cacheReadHook = NULL)  
createWebDependency : function (dependency, scrubFile = TRUE)  
dataTableOutput : function (outputId)  
dateInput : function (inputId, label, value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", 
startview = "month", weekstart = 0, language = "en", width = NULL, autoclose = TRUE, 
datesdisabled = NULL, daysofweekdisabled = NULL)  
dateRangeInput : function (inputId, label, start = NULL, end = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", 
startview = "month", weekstart = 0, language = "en", separator = " to ", width = NULL, 
autoclose = TRUE)  
dblclickOpts : function (id, clip = TRUE, delay = 400)  
debounce : function (r, millis, priority = 100, domain = getDefaultReactiveDomain())  
devmode : function (devmode = getOption("shiny.devmode", TRUE), verbose = getOption("shiny.devmode.verbose", 
TRUE))  
dialogViewer : function (dialogName, width = 600, height = 600)  
diskCache : function (dir = NULL, max_size = 500 * 1024^2, max_age = Inf, max_n = Inf, evict = c("lru", 
"fifo"), destroy_on_finalize = FALSE, missing = key_missing(), exec_missing = deprecated(), 
logfile = NULL)  
div : function (..., .noWS = NULL, .renderHook = NULL)  
downloadButton : function (outputId, label = "Download", class = NULL, ..., icon = shiny::icon("download"))  
downloadHandler : function (filename, content, contentType = NULL, outputArgs = list())  
downloadLink : function (outputId, label = "Download", class = NULL, ...)  
em : function (..., .noWS = NULL, .renderHook = NULL)  
enableBookmarking : function (store = c("url", "server", "disable"))  
eventReactive : function (eventExpr, valueExpr, event.env = parent.frame(), event.quoted = FALSE, 
value.env = parent.frame(), value.quoted = FALSE, ..., label = NULL, domain = getDefaultReactiveDomain(), 
ignoreNULL = TRUE, ignoreInit = FALSE)  
exportTestValues : function (..., quoted_ = FALSE, env_ = parent.frame(), session_ = getDefaultReactiveDomain())  
exprToFunction : function (expr, env = parent.frame(), quoted = FALSE)  
fileInput : function (inputId, label, multiple = FALSE, accept = NULL, width = NULL, buttonLabel = "Browse...", 
placeholder = "No file selected", capture = NULL)  
fillCol : function (..., flex = 1, width = "100%", height = "100%")  
fillPage : function (..., padding = 0, title = NULL, bootstrap = TRUE, theme = NULL, lang = NULL)  
fillRow : function (..., flex = 1, width = "100%", height = "100%")  
fixedPage : function (..., title = NULL, theme = NULL, lang = NULL)  
fixedPanel : function (..., top = NULL, left = NULL, right = NULL, bottom = NULL, width = NULL, 
height = NULL, draggable = FALSE, cursor = c("auto", "move", "default", "inherit"))  
fixedRow : function (...)  
flowLayout : function (..., cellArgs = list())  
fluidPage : function (..., title = NULL, theme = NULL, lang = NULL)  
fluidRow : function (...)  
freezeReactiveVal : function (x)  
freezeReactiveValue : function (x, name)  
get_devmode_option : function (name, default = NULL, devmode_default = missing_arg(), devmode_message = missing_arg())  
getCurrentOutputInfo : function (session = getDefaultReactiveDomain())  
getCurrentTheme : function ()  
getDefaultReactiveDomain : function ()  
getQueryString : function (session = getDefaultReactiveDomain())  
getShinyOption : function (name, default = NULL)  
getUrlHash : function (session = getDefaultReactiveDomain())  
h1 : function (..., .noWS = NULL, .renderHook = NULL)  
h2 : function (..., .noWS = NULL, .renderHook = NULL)  
h3 : function (..., .noWS = NULL, .renderHook = NULL)  
h4 : function (..., .noWS = NULL, .renderHook = NULL)  
h5 : function (..., .noWS = NULL, .renderHook = NULL)  
h6 : function (..., .noWS = NULL, .renderHook = NULL)  
headerPanel : function (title, windowTitle = title)  
helpText : function (...)  
hideTab : function (inputId, target, session = getDefaultReactiveDomain())  
hoverOpts : function (id, delay = 300, delayType = c("debounce", "throttle"), clip = TRUE, nullOutside = TRUE)  
hr : function (..., .noWS = NULL, .renderHook = NULL)  
HTML : function (text, ..., .noWS = NULL)  
htmlOutput : function (outputId, inline = FALSE, container = if (inline) span else div, ...)  
htmlTemplate : function (filename = NULL, ..., text_ = NULL, document_ = "auto")  
httpResponse : function (status = 200L, content_type = "text/html; charset=UTF-8", content = "", 
headers = list())  
icon : function (name, class = NULL, lib = "font-awesome", ...)  
imageOutput : function (outputId, width = "100%", height = "400px", click = NULL, dblclick = NULL, 
hover = NULL, brush = NULL, inline = FALSE)  
img : function (..., .noWS = NULL, .renderHook = NULL)  
in_devmode : function ()  
includeCSS : function (path, ...)  
includeHTML : function (path)  
includeMarkdown : function (path)  
includeScript : function (path, ...)  
includeText : function (path)  
incProgress : function (amount = 0.1, message = NULL, detail = NULL, session = getDefaultReactiveDomain())  
inputPanel : function (...)  
insertTab : function (inputId, tab, target = NULL, position = c("after", "before"), select = FALSE, 
session = getDefaultReactiveDomain())  
insertUI : function (selector, where = c("beforeBegin", "afterBegin", "beforeEnd", "afterEnd"), 
ui, multiple = FALSE, immediate = FALSE, session = getDefaultReactiveDomain())  
installExprFunction : function (expr, name, eval.env = parent.frame(2), quoted = FALSE, assign.env = parent.frame(1), 
label = sys.call(-1)[[1]], wrappedWithLabel = TRUE, ..stacktraceon = FALSE)  
invalidateLater : function (millis, session = getDefaultReactiveDomain())  
is.key_missing : function (x)  
is.reactive : function (x)  
is.reactivevalues : function (x)  
is.shiny.appobj : function (x)  
is.singleton : function (x)  
isolate : function (expr)  
isRunning : function ()  
isTruthy : function (x)  
key_missing : function ()  
loadSupport : function (appDir = NULL, renv = new.env(parent = globalenv()), globalrenv = globalenv())  
mainPanel : function (..., width = 8)  
makeReactiveBinding : function (symbol, env = parent.frame())  
markdown : function (mds, extensions = TRUE, .noWS = NULL, ...)  
markRenderFunction : function (uiFunc, renderFunc, outputArgs = list(), cacheHint = "auto", cacheWriteHook = NULL, 
cacheReadHook = NULL)  
maskReactiveContext : function (expr)  
memoryCache : function (max_size = 200 * 1024^2, max_age = Inf, max_n = Inf, evict = c("lru", "fifo"), 
missing = key_missing(), exec_missing = deprecated(), logfile = NULL)  
MockShinySession : Class 'R6ClassGenerator' <MockShinySession> object generator
Public:
env: NULL
returned: NULL
singletons: 
clientData: mockclientdata
output: NULL
input: NULL
userData: NULL
progressStack: Stack
token: character
cache: NULL
appcache: NULL
restoreContext: NULL
groups: NULL
user: NULL
options: NULL
initialize : function () 
onFlush: function (fun, once = TRUE) 
onFlushed: function (fun, once = TRUE) 
onEnded: function (sessionEndedCallback) 
isEnded: function () 
isClosed: function () 
close: function () 
cycleStartAction: function (callback) 
fileUrl: function (name, file, contentType = "application/octet-stream") 
setInputs: function (...) 
.scheduleTask: function (millis, callback) 
elapse: function (millis) 
.now: function () 
defineOutput: function (name, func, label) 
getOutput: function (name) 
ns: function (id) 
flushReact: function () 
makeScope: function (namespace) 
setEnv: function (env) 
setReturned: function (value) 
getReturned: function () 
genId: function () 
rootScope: function () 
unhandledError: function (e) 
freezeValue: function (x, name) 
onSessionEnded: function (sessionEndedCallback) 
registerDownload: function (name, filename, contentType, content) 
getCurrentOutputInfo: function () 
clone: function (deep = FALSE) 
Active bindings:
files: function () 
downloads: function () 
closed: function () 
session: function () 
request: function (value) 
Private:
.input: NULL
flushCBs: NULL
flushedCBs: NULL
endedCBs: NULL
timer: NULL
was_closed: FALSE
outs: list
nsPrefix: mock-session
idCounter: 0
file_generators: NULL
currentOutputName: NULL
renderFile: function (name, download) 
flush: function () 
noopWarn: function (name, msg) 
withCurrentOutput: function (name, expr) 
Parent env: <environment: namespace:shiny>
Locked objects: FALSE
Locked class: FALSE
Portable: FALSE 
modalButton : function (label, icon = NULL)  
modalDialog : function (..., title = NULL, footer = modalButton("Dismiss"), size = c("m", "s", "l", 
"xl"), easyClose = FALSE, fade = TRUE)  
moduleServer : function (id, module, session = getDefaultReactiveDomain())  
navbarMenu : function (title, ..., menuName = title, icon = NULL)  
navbarPage : function (title, ..., id = NULL, selected = NULL, position = c("static-top", "fixed-top", 
"fixed-bottom"), header = NULL, footer = NULL, inverse = FALSE, collapsible = FALSE, 
fluid = TRUE, theme = NULL, windowTitle = NA, lang = NULL)  
navlistPanel : function (..., id = NULL, selected = NULL, header = NULL, footer = NULL, well = TRUE, 
fluid = TRUE, widths = c(4, 8))  
nearPoints : function (df, coordinfo, xvar = NULL, yvar = NULL, panelvar1 = NULL, panelvar2 = NULL, 
threshold = 5, maxpoints = NULL, addDist = FALSE, allRows = FALSE)  
need : function (expr, message = paste(label, "must be provided"), label)  
NS : function (namespace, id = NULL)  
ns.sep :  chr "-"
numericInput : function (inputId, label, value, min = NA, max = NA, step = NA, width = NULL)  
observe : function (x, env = parent.frame(), quoted = FALSE, ..., label = NULL, suspended = FALSE, 
priority = 0, domain = getDefaultReactiveDomain(), autoDestroy = TRUE, ..stacktraceon = TRUE)  
observeEvent : function (eventExpr, handlerExpr, event.env = parent.frame(), event.quoted = FALSE, 
handler.env = parent.frame(), handler.quoted = FALSE, ..., label = NULL, suspended = FALSE, 
priority = 0, domain = getDefaultReactiveDomain(), autoDestroy = TRUE, ignoreNULL = TRUE, 
ignoreInit = FALSE, once = FALSE)  
onBookmark : function (fun, session = getDefaultReactiveDomain())  
onBookmarked : function (fun, session = getDefaultReactiveDomain())  
onFlush : function (fun, once = TRUE, session = getDefaultReactiveDomain())  
onFlushed : function (fun, once = TRUE, session = getDefaultReactiveDomain())  
onReactiveDomainEnded : function (domain, callback, failIfNull = FALSE)  
onRestore : function (fun, session = getDefaultReactiveDomain())  
onRestored : function (fun, session = getDefaultReactiveDomain())  
onSessionEnded : function (fun, session = getDefaultReactiveDomain())  
onStop : function (fun, session = getDefaultReactiveDomain())  
outputOptions : function (x, name, ...)  
p : function (..., .noWS = NULL, .renderHook = NULL)  
pageWithSidebar : function (headerPanel, sidebarPanel, mainPanel)  
paneViewer : function (minHeight = NULL)  
parseQueryString : function (str, nested = FALSE)  
passwordInput : function (inputId, label, value = "", width = NULL, placeholder = NULL)  
plotOutput : function (outputId, width = "100%", height = "400px", click = NULL, dblclick = NULL, 
hover = NULL, brush = NULL, inline = FALSE)  
plotPNG : function (func, filename = tempfile(fileext = ".png"), width = 400, height = 400, 
res = 72, ...)  
pre : function (..., .noWS = NULL, .renderHook = NULL)  
prependTab : function (inputId, tab, select = FALSE, menuName = NULL, session = getDefaultReactiveDomain())  
printError : function (cond, full = get_devmode_option("shiny.fullstacktrace", FALSE), offset = getOption("shiny.stacktraceoffset", 
TRUE))  
printStackTrace : function (cond, full = get_devmode_option("shiny.fullstacktrace", FALSE), offset = getOption("shiny.stacktraceoffset", 
TRUE))  
Progress : Class 'R6ClassGenerator' <Progress> object generator
Public:
initialize: function (session = getDefaultReactiveDomain(), min = 0, max = 1, 
set: function (value = NULL, message = NULL, detail = NULL) 
inc: function (amount = 0.1, message = NULL, detail = NULL) 
getMin: function () 
getMax: function () 
getValue: function () 
close: function () 
clone: function (deep = FALSE) 
Private:
session: ShinySession
id: 
min: 
max: 
style: 
value: 
closed: 
Parent env: <environment: namespace:shiny>
Locked objects: TRUE
Locked class: FALSE
Portable: TRUE 
quoToFunction : function (q, label = sys.call(-1)[[1]], ..stacktraceon = FALSE)  
radioButtons : function (inputId, label, choices = NULL, selected = NULL, inline = FALSE, width = NULL, 
choiceNames = NULL, choiceValues = NULL)  
reactive : function (x, env = parent.frame(), quoted = FALSE, ..., label = NULL, domain = getDefaultReactiveDomain(), 
..stacktraceon = TRUE)  
reactiveConsole : function (enabled)  
reactiveFileReader : function (intervalMillis, session, filePath, readFunc, ...)  
reactivePoll : function (intervalMillis, session, checkFunc, valueFunc)  
reactiveTimer : function (intervalMs = 1000, session = getDefaultReactiveDomain())  
reactiveVal : function (value = NULL, label = NULL)  
reactiveValues : function (...)  
reactiveValuesToList : function (x, all.names = FALSE)  
reactlog : function ()  
reactlogReset : function ()  
reactlogShow : function (time = TRUE)  
register_devmode_option : function (name, devmode_message = NULL, devmode_default = NULL)  
registerInputHandler : function (type, fun, force = FALSE)  
registerThemeDependency : function (func)  
removeInputHandler : function (type)  
removeModal : function (session = getDefaultReactiveDomain())  
removeNotification : function (id, session = getDefaultReactiveDomain())  
removeResourcePath : function (prefix)  
removeTab : function (inputId, target, session = getDefaultReactiveDomain())  
removeUI : function (selector, multiple = FALSE, immediate = FALSE, session = getDefaultReactiveDomain())  
renderCachedPlot : function (expr, cacheKeyExpr, sizePolicy = sizeGrowthRatio(width = 400, height = 400, 
growthRate = 1.2), res = 72, cache = "app", ..., alt = "Plot object", outputArgs = list(), 
width = NULL, height = NULL)  
renderDataTable : function (expr, options = NULL, searchDelay = 500, callback = "function(oTable) {}", 
escape = TRUE, env = parent.frame(), quoted = FALSE, outputArgs = list())  
renderImage : function (expr, env = parent.frame(), quoted = FALSE, deleteFile, outputArgs = list())  
renderPlot : function (expr, width = "auto", height = "auto", res = 72, ..., alt = NA, env = parent.frame(), 
quoted = FALSE, execOnResize = FALSE, outputArgs = list())  
renderPrint : function (expr, env = parent.frame(), quoted = FALSE, width = getOption("width"), 
outputArgs = list())  
renderTable : function (expr, striped = FALSE, hover = FALSE, bordered = FALSE, spacing = c("s", 
"xs", "m", "l"), width = "auto", align = NULL, rownames = FALSE, colnames = TRUE, 
digits = NULL, na = "NA", ..., env = parent.frame(), quoted = FALSE, outputArgs = list())  
renderText : function (expr, env = parent.frame(), quoted = FALSE, outputArgs = list(), sep = " ")  
renderUI : function (expr, env = parent.frame(), quoted = FALSE, outputArgs = list())  
repeatable : function (rngfunc, seed = stats::runif(1, 0, .Machine$integer.max))  
req : function (..., cancelOutput = FALSE)  
resourcePaths : function ()  
restoreInput : function (id, default)  
runApp : function (appDir = getwd(), port = getOption("shiny.port"), launch.browser = getOption("shiny.launch.browser", 
interactive()), host = getOption("shiny.host", "127.0.0.1"), workerId = "", quiet = FALSE, 
display.mode = c("auto", "normal", "showcase"), test.mode = getOption("shiny.testmode", 
FALSE))  
runExample : function (example = NA, port = getOption("shiny.port"), launch.browser = getOption("shiny.launch.browser", 
interactive()), host = getOption("shiny.host", "127.0.0.1"), display.mode = c("auto", 
"normal", "showcase"))  
runGadget : function (app, server = NULL, port = getOption("shiny.port"), viewer = paneViewer(), 
stopOnCancel = TRUE)  
runGist : function (gist, destdir = NULL, ...)  
runGitHub : function (repo, username = getOption("github.user"), ref = "HEAD", subdir = NULL, 
destdir = NULL, ...)  
runTests : function (appDir = ".", filter = NULL, assert = TRUE, envir = globalenv())  
runUrl : function (url, filetype = NULL, subdir = NULL, destdir = NULL, ...)  
safeError : function (error)  
selectInput : function (inputId, label, choices, selected = NULL, multiple = FALSE, selectize = TRUE, 
width = NULL, size = NULL)  
selectizeInput : function (inputId, ..., options = NULL, width = NULL)  
serverInfo : function ()  
setBookmarkExclude : function (names = character(0), session = getDefaultReactiveDomain())  
setProgress : function (value = NULL, message = NULL, detail = NULL, session = getDefaultReactiveDomain())  
setSerializer : function (inputId, fun, session = getDefaultReactiveDomain())  
shinyApp : function (ui, server, onStart = NULL, options = list(), uiPattern = "/", enableBookmarking = NULL)  
shinyAppDir : function (appDir, options = list())  
shinyAppFile : function (appFile, options = list())  
shinyAppTemplate : function (path = NULL, examples = "default", dryrun = FALSE)  
shinyOptions : function (...)  
shinyServer : function (func)  
shinyUI : function (ui)  
showBookmarkUrlModal : function (url)  
showModal : function (ui, session = getDefaultReactiveDomain())  
showNotification : function (ui, action = NULL, duration = 5, closeButton = TRUE, id = NULL, type = c("default", 
"message", "warning", "error"), session = getDefaultReactiveDomain())  
showTab : function (inputId, target, select = FALSE, session = getDefaultReactiveDomain())  
sidebarLayout : function (sidebarPanel, mainPanel, position = c("left", "right"), fluid = TRUE)  
sidebarPanel : function (..., width = 4)  
singleton : function (x, value = TRUE)  
sizeGrowthRatio : function (width = 400, height = 400, growthRate = 1.2)  
sliderInput : function (inputId, label, min, max, value, step = NULL, round = FALSE, ticks = TRUE, 
animate = FALSE, width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL, 
timezone = NULL, dragRange = TRUE)  
snapshotExclude : function (x)  
snapshotPreprocessInput : function (inputId, fun, session = getDefaultReactiveDomain())  
snapshotPreprocessOutput : function (x, fun)  
span : function (..., .noWS = NULL, .renderHook = NULL)  
splitLayout : function (..., cellWidths = NULL, cellArgs = list())  
stopApp : function (returnValue = invisible())  
strong : function (..., .noWS = NULL, .renderHook = NULL)  
submitButton : function (text = "Apply Changes", icon = NULL, width = NULL)  
suppressDependencies : function (...)  
tableOutput : function (outputId)  
tabPanel : function (title, ..., value = title, icon = NULL)  
tabPanelBody : function (value, ..., icon = NULL)  
tabsetPanel : function (..., id = NULL, selected = NULL, type = c("tabs", "pills", "hidden"), header = NULL, 
footer = NULL)  
tag : function (`_tag_name`, varArgs, .noWS = NULL, .renderHook = NULL)  
tagAppendAttributes : function (tag, ..., .cssSelector = NULL)  
tagAppendChild : function (tag, child, .cssSelector = NULL)  
tagAppendChildren : function (tag, ..., .cssSelector = NULL, list = NULL)  
tagGetAttribute : function (tag, attr)  
tagHasAttribute : function (tag, attr)  
tagList : function (...)  
tags : List of 181
$ a                  :function (..., .noWS = NULL, .renderHook = NULL)  
$ abbr               :function (..., .noWS = NULL, .renderHook = NULL)  
$ address            :function (..., .noWS = NULL, .renderHook = NULL)  
$ animate            :function (..., .noWS = NULL, .renderHook = NULL)  
$ animateMotion      :function (..., .noWS = NULL, .renderHook = NULL)  
$ animateTransform   :function (..., .noWS = NULL, .renderHook = NULL)  
$ area               :function (..., .noWS = NULL, .renderHook = NULL)  
$ article            :function (..., .noWS = NULL, .renderHook = NULL)  
$ aside              :function (..., .noWS = NULL, .renderHook = NULL)  
$ audio              :function (..., .noWS = NULL, .renderHook = NULL)  
$ b                  :function (..., .noWS = NULL, .renderHook = NULL)  
$ base               :function (..., .noWS = NULL, .renderHook = NULL)  
$ bdi                :function (..., .noWS = NULL, .renderHook = NULL)  
$ bdo                :function (..., .noWS = NULL, .renderHook = NULL)  
$ blockquote         :function (..., .noWS = NULL, .renderHook = NULL)  
$ body               :function (..., .noWS = NULL, .renderHook = NULL)  
$ br                 :function (..., .noWS = NULL, .renderHook = NULL)  
$ button             :function (..., .noWS = NULL, .renderHook = NULL)  
$ canvas             :function (..., .noWS = NULL, .renderHook = NULL)  
$ caption            :function (..., .noWS = NULL, .renderHook = NULL)  
$ circle             :function (..., .noWS = NULL, .renderHook = NULL)  
$ cite               :function (..., .noWS = NULL, .renderHook = NULL)  
$ clipPath           :function (..., .noWS = NULL, .renderHook = NULL)  
$ code               :function (..., .noWS = NULL, .renderHook = NULL)  
$ col                :function (..., .noWS = NULL, .renderHook = NULL)  
$ colgroup           :function (..., .noWS = NULL, .renderHook = NULL)  
$ color-profile      :function (..., .noWS = NULL, .renderHook = NULL)  
$ command            :function (..., .noWS = NULL, .renderHook = NULL)  
$ data               :function (..., .noWS = NULL, .renderHook = NULL)  
$ datalist           :function (..., .noWS = NULL, .renderHook = NULL)  
$ dd                 :function (..., .noWS = NULL, .renderHook = NULL)  
$ defs               :function (..., .noWS = NULL, .renderHook = NULL)  
$ del                :function (..., .noWS = NULL, .renderHook = NULL)  
$ desc               :function (..., .noWS = NULL, .renderHook = NULL)  
$ details            :function (..., .noWS = NULL, .renderHook = NULL)  
$ dfn                :function (..., .noWS = NULL, .renderHook = NULL)  
$ dialog             :function (..., .noWS = NULL, .renderHook = NULL)  
$ discard            :function (..., .noWS = NULL, .renderHook = NULL)  
$ div                :function (..., .noWS = NULL, .renderHook = NULL)  
$ dl                 :function (..., .noWS = NULL, .renderHook = NULL)  
$ dt                 :function (..., .noWS = NULL, .renderHook = NULL)  
$ ellipse            :function (..., .noWS = NULL, .renderHook = NULL)  
$ em                 :function (..., .noWS = NULL, .renderHook = NULL)  
$ embed              :function (..., .noWS = NULL, .renderHook = NULL)  
$ eventsource        :function (..., .noWS = NULL, .renderHook = NULL)  
$ feBlend            :function (..., .noWS = NULL, .renderHook = NULL)  
$ feColorMatrix      :function (..., .noWS = NULL, .renderHook = NULL)  
$ feComponentTransfer:function (..., .noWS = NULL, .renderHook = NULL)  
$ feComposite        :function (..., .noWS = NULL, .renderHook = NULL)  
$ feConvolveMatrix   :function (..., .noWS = NULL, .renderHook = NULL)  
$ feDiffuseLighting  :function (..., .noWS = NULL, .renderHook = NULL)  
$ feDisplacementMap  :function (..., .noWS = NULL, .renderHook = NULL)  
$ feDistantLight     :function (..., .noWS = NULL, .renderHook = NULL)  
$ feDropShadow       :function (..., .noWS = NULL, .renderHook = NULL)  
$ feFlood            :function (..., .noWS = NULL, .renderHook = NULL)  
$ feFuncA            :function (..., .noWS = NULL, .renderHook = NULL)  
$ feFuncB            :function (..., .noWS = NULL, .renderHook = NULL)  
$ feFuncG            :function (..., .noWS = NULL, .renderHook = NULL)  
$ feFuncR            :function (..., .noWS = NULL, .renderHook = NULL)  
$ feGaussianBlur     :function (..., .noWS = NULL, .renderHook = NULL)  
$ feImage            :function (..., .noWS = NULL, .renderHook = NULL)  
$ feMerge            :function (..., .noWS = NULL, .renderHook = NULL)  
$ feMergeNode        :function (..., .noWS = NULL, .renderHook = NULL)  
$ feMorphology       :function (..., .noWS = NULL, .renderHook = NULL)  
$ feOffset           :function (..., .noWS = NULL, .renderHook = NULL)  
$ fePointLight       :function (..., .noWS = NULL, .renderHook = NULL)  
$ feSpecularLighting :function (..., .noWS = NULL, .renderHook = NULL)  
$ feSpotLight        :function (..., .noWS = NULL, .renderHook = NULL)  
$ feTile             :function (..., .noWS = NULL, .renderHook = NULL)  
$ feTurbulence       :function (..., .noWS = NULL, .renderHook = NULL)  
$ fieldset           :function (..., .noWS = NULL, .renderHook = NULL)  
$ figcaption         :function (..., .noWS = NULL, .renderHook = NULL)  
$ figure             :function (..., .noWS = NULL, .renderHook = NULL)  
$ filter             :function (..., .noWS = NULL, .renderHook = NULL)  
$ footer             :function (..., .noWS = NULL, .renderHook = NULL)  
$ foreignObject      :function (..., .noWS = NULL, .renderHook = NULL)  
$ form               :function (..., .noWS = NULL, .renderHook = NULL)  
$ g                  :function (..., .noWS = NULL, .renderHook = NULL)  
$ h1                 :function (..., .noWS = NULL, .renderHook = NULL)  
$ h2                 :function (..., .noWS = NULL, .renderHook = NULL)  
$ h3                 :function (..., .noWS = NULL, .renderHook = NULL)  
$ h4                 :function (..., .noWS = NULL, .renderHook = NULL)  
$ h5                 :function (..., .noWS = NULL, .renderHook = NULL)  
$ h6                 :function (..., .noWS = NULL, .renderHook = NULL)  
$ hatch              :function (..., .noWS = NULL, .renderHook = NULL)  
$ hatchpath          :function (..., .noWS = NULL, .renderHook = NULL)  
$ head               :function (..., .noWS = NULL, .renderHook = NULL)  
$ header             :function (..., .noWS = NULL, .renderHook = NULL)  
$ hgroup             :function (..., .noWS = NULL, .renderHook = NULL)  
$ hr                 :function (..., .noWS = NULL, .renderHook = NULL)  
$ html               :function (..., .noWS = NULL, .renderHook = NULL)  
$ i                  :function (..., .noWS = NULL, .renderHook = NULL)  
$ iframe             :function (..., .noWS = NULL, .renderHook = NULL)  
$ image              :function (..., .noWS = NULL, .renderHook = NULL)  
$ img                :function (..., .noWS = NULL, .renderHook = NULL)  
$ input              :function (..., .noWS = NULL, .renderHook = NULL)  
$ ins                :function (..., .noWS = NULL, .renderHook = NULL)  
$ kbd                :function (..., .noWS = NULL, .renderHook = NULL)  
$ keygen             :function (..., .noWS = NULL, .renderHook = NULL)  
[list output truncated]
tagSetChildren : function (tag, ..., .cssSelector = NULL, list = NULL)  
testServer : function (app = NULL, expr, args = list(), session = MockShinySession$new())  
textAreaInput : function (inputId, label, value = "", width = NULL, height = NULL, cols = NULL, rows = NULL, 
placeholder = NULL, resize = NULL)  
textInput : function (inputId, label, value = "", width = NULL, placeholder = NULL)  
textOutput : function (outputId, container = if (inline) span else div, inline = FALSE)  
throttle : function (r, millis, priority = 100, domain = getDefaultReactiveDomain())  
titlePanel : function (title, windowTitle = title)  
uiOutput : function (outputId, inline = FALSE, container = if (inline) span else div, ...)  
updateActionButton : function (session = getDefaultReactiveDomain(), inputId, label = NULL, icon = NULL)  
updateActionLink : function (session = getDefaultReactiveDomain(), inputId, label = NULL, icon = NULL)  
updateCheckboxGroupInput : function (session = getDefaultReactiveDomain(), inputId, label = NULL, choices = NULL, 
selected = NULL, inline = FALSE, choiceNames = NULL, choiceValues = NULL)  
updateCheckboxInput : function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL)  
updateDateInput : function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL, 
min = NULL, max = NULL)  
updateDateRangeInput : function (session = getDefaultReactiveDomain(), inputId, label = NULL, start = NULL, 
end = NULL, min = NULL, max = NULL)  
updateNavbarPage : function (session = getDefaultReactiveDomain(), inputId, selected = NULL)  
updateNavlistPanel : function (session = getDefaultReactiveDomain(), inputId, selected = NULL)  
updateNumericInput : function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL, 
min = NULL, max = NULL, step = NULL)  
updateQueryString : function (queryString, mode = c("replace", "push"), session = getDefaultReactiveDomain())  
updateRadioButtons : function (session = getDefaultReactiveDomain(), inputId, label = NULL, choices = NULL, 
selected = NULL, inline = FALSE, choiceNames = NULL, choiceValues = NULL)  
updateSelectInput : function (session = getDefaultReactiveDomain(), inputId, label = NULL, choices = NULL, 
selected = NULL)  
updateSelectizeInput : function (session = getDefaultReactiveDomain(), inputId, label = NULL, choices = NULL, 
selected = NULL, options = list(), server = FALSE)  
updateSliderInput : function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL, 
min = NULL, max = NULL, step = NULL, timeFormat = NULL, timezone = NULL)  
updateTabsetPanel : function (session = getDefaultReactiveDomain(), inputId, selected = NULL)  
updateTextAreaInput : function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL, 
placeholder = NULL)  
updateTextInput : function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL, 
placeholder = NULL)  
updateVarSelectInput : function (session = getDefaultReactiveDomain(), inputId, label = NULL, data = NULL, 
selected = NULL)  
updateVarSelectizeInput : function (session = getDefaultReactiveDomain(), inputId, label = NULL, data = NULL, 
selected = NULL, options = list(), server = FALSE)  
urlModal : function (url, title = "Bookmarked application link", subtitle = NULL)  
validate : function (..., errorClass = character(0))  
validateCssUnit : function (x)  
varSelectInput : function (inputId, label, data, selected = NULL, multiple = FALSE, selectize = TRUE, 
width = NULL, size = NULL)  
varSelectizeInput : function (inputId, ..., options = NULL, width = NULL)  
verbatimTextOutput : function (outputId, placeholder = FALSE)  
verticalLayout : function (..., fluid = TRUE)  
wellPanel : function (...)  
with_devmode : function (devmode, code, verbose = getOption("shiny.devmode.verbose", TRUE))  
withLogErrors : function (expr, full = get_devmode_option("shiny.fullstacktrace", FALSE), offset = getOption("shiny.stacktraceoffset", 
TRUE))  
withMathJax : function (...)  
withProgress : function (expr, min = 0, max = 1, value = min + (max - min) * 0.1, message = NULL, 
detail = NULL, style = getShinyOption("progress.style", default = "notification"), 
session = getDefaultReactiveDomain(), env = parent.frame(), quoted = FALSE)  
withReactiveDomain : function (domain, expr)  
withTags : function (code, .noWS = NULL)  
                                                                                                          > 
library(stats)
.checkMFClasses : function (cl, m, ordNotOK = FALSE)  
.getXlevels : function (Terms, m)  
.lm.fit : function (x, y, tol = 1e-07)  
.MFclass : function (x)  
.nknots.smspl : function (n)  
.preformat.ts : function (x, calendar, ...)  
.vcov.aliased : function (aliased, vc, complete = TRUE)  
acf : function (x, lag.max = NULL, type = c("correlation", "covariance", "partial"), plot = TRUE, 
na.action = na.fail, demean = TRUE, ...)  
acf2AR : function (acf)  
add.scope : function (terms1, terms2)  
add1 : function (object, scope, ...)  
addmargins : function (A, margin = seq_along(dim(A)), FUN = sum, quiet = FALSE)  
aggregate : function (x, ...)  
aggregate.data.frame : function (x, by, FUN, ..., simplify = TRUE, drop = TRUE)  
aggregate.ts : function (x, nfrequency = 1, FUN = sum, ndeltat = 1, ts.eps = getOption("ts.eps"), 
...)  
AIC : function (object, ..., k = 2)  
alias : function (object, ...)  
anova : function (object, ...)  
ansari.test : function (x, ...)  
aov : function (formula, data = NULL, projections = FALSE, qr = TRUE, contrasts = NULL, 
...)  
approx : function (x, y = NULL, xout, method = "linear", n = 50, yleft, yright, rule = 1, f = 0, 
ties = mean, na.rm = TRUE)  
approxfun : function (x, y = NULL, method = "linear", yleft, yright, rule = 1, f = 0, ties = mean, 
na.rm = TRUE)  
ar : function (x, aic = TRUE, order.max = NULL, method = c("yule-walker", "burg", "ols", 
"mle", "yw"), na.action = na.fail, series = deparse1(substitute(x)), ...)  
ar.burg : function (x, ...)  
ar.mle : function (x, aic = TRUE, order.max = NULL, na.action = na.fail, demean = TRUE, series = NULL, 
...)  
ar.ols : function (x, aic = TRUE, order.max = NULL, na.action = na.fail, demean = TRUE, intercept = demean, 
series = NULL, ...)  
ar.yw : function (x, ...)  
arima : function (x, order = c(0L, 0L, 0L), seasonal = list(order = c(0L, 0L, 0L), period = NA), 
xreg = NULL, include.mean = TRUE, transform.pars = TRUE, fixed = NULL, init = NULL, 
method = c("CSS-ML", "ML", "CSS"), n.cond, SSinit = c("Gardner1980", "Rossignol2011"), 
optim.method = "BFGS", optim.control = list(), kappa = 1e+06)  
arima.sim : function (model, n, rand.gen = rnorm, innov = rand.gen(n, ...), n.start = NA, start.innov = rand.gen(n.start, 
...), ...)  
arima0 : function (x, order = c(0, 0, 0), seasonal = list(order = c(0, 0, 0), period = NA), 
xreg = NULL, include.mean = TRUE, delta = 0.01, transform.pars = TRUE, fixed = NULL, 
init = NULL, method = c("ML", "CSS"), n.cond, optim.control = list())  
arima0.diag : function (...)  
ARMAacf : function (ar = numeric(), ma = numeric(), lag.max = r, pacf = FALSE)  
ARMAtoMA : function (ar = numeric(), ma = numeric(), lag.max)  
as.dendrogram : function (object, ...)  
as.dist : function (m, diag = FALSE, upper = FALSE)  
as.formula : function (object, env = parent.frame())  
as.hclust : function (x, ...)  
as.stepfun : function (x, ...)  
as.ts : function (x, ...)  
asOneSidedFormula : function (object)  
ave : function (x, ..., FUN = mean)  
bandwidth.kernel : function (k)  
bartlett.test : function (x, ...)  
BIC : function (object, ...)  
binom.test : function (x, n, p = 0.5, alternative = c("two.sided", "less", "greater"), conf.level = 0.95)  
binomial : function (link = "logit")  
biplot : function (x, ...)  
Box.test : function (x, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)  
bw.bcv : function (x, nb = 1000L, lower = 0.1 * hmax, upper = hmax, tol = 0.1 * lower)  
bw.nrd : function (x)  
bw.nrd0 : function (x)  
bw.SJ : function (x, nb = 1000L, lower = 0.1 * hmax, upper = hmax, method = c("ste", "dpi"), 
tol = 0.1 * lower)  
bw.ucv : function (x, nb = 1000L, lower = 0.1 * hmax, upper = hmax, tol = 0.1 * lower)  
C : function (object, contr, how.many, ...)  
cancor : function (x, y, xcenter = TRUE, ycenter = TRUE)  
case.names : function (object, ...)  
ccf : function (x, y, lag.max = NULL, type = c("correlation", "covariance"), plot = TRUE, 
na.action = na.fail, ...)  
chisq.test : function (x, y = NULL, correct = TRUE, p = rep(1/length(x), length(x)), rescale.p = FALSE, 
simulate.p.value = FALSE, B = 2000)  
cmdscale : function (d, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE, list. = eig || add || 
x.ret)  
coef : function (object, ...)  
coefficients : function (object, ...)  
complete.cases : function (...)  
confint : function (object, parm, level = 0.95, ...)  
confint.default : function (object, parm, level = 0.95, ...)  
confint.lm : function (object, parm, level = 0.95, ...)  
constrOptim : function (theta, f, grad, ui, ci, mu = 1e-04, control = list(), method = if (is.null(grad)) "Nelder-Mead" else "BFGS", 
outer.iterations = 100, outer.eps = 1e-05, ..., hessian = FALSE)  
contr.helmert : function (n, contrasts = TRUE, sparse = FALSE)  
contr.poly : function (n, scores = 1:n, contrasts = TRUE, sparse = FALSE)  
contr.SAS : function (n, contrasts = TRUE, sparse = FALSE)  
contr.sum : function (n, contrasts = TRUE, sparse = FALSE)  
contr.treatment : function (n, base = 1, contrasts = TRUE, sparse = FALSE)  
contrasts : function (x, contrasts = TRUE, sparse = FALSE)  
contrasts<- : function (x, how.many = NULL, value)  
convolve : function (x, y, conj = TRUE, type = c("circular", "open", "filter"))  
cooks.distance : function (model, ...)  
cophenetic : function (x)  
cor : function (x, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))  
cor.test : function (x, ...)  
cov : function (x, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))  
cov.wt : function (x, wt = rep(1/nrow(x), nrow(x)), cor = FALSE, center = TRUE, method = c("unbiased", 
"ML"))  
cov2cor : function (V)  
covratio : function (model, infl = lm.influence(model, do.coef = FALSE), res = weighted.residuals(model))  
cpgram : function (ts, taper = 0.1, main = paste("Series: ", deparse1(substitute(ts))), ci.col = "blue")  
cutree : function (tree, k = NULL, h = NULL)  
cycle : function (x, ...)  
D : function (expr, name)  
dbeta : function (x, shape1, shape2, ncp = 0, log = FALSE)  
dbinom : function (x, size, prob, log = FALSE)  
dcauchy : function (x, location = 0, scale = 1, log = FALSE)  
dchisq : function (x, df, ncp = 0, log = FALSE)  
decompose : function (x, type = c("additive", "multiplicative"), filter = NULL)  
delete.response : function (termobj)  
deltat : function (x, ...)  
dendrapply : function (X, FUN, ...)  
density : function (x, ...)  
density.default : function (x, bw = "nrd0", adjust = 1, kernel = c("gaussian", "epanechnikov", "rectangular", 
"triangular", "biweight", "cosine", "optcosine"), weights = NULL, window = kernel, 
width, give.Rkern = FALSE, subdensity = FALSE, n = 512, from, to, cut = 3, na.rm = FALSE, 
...)  
deriv : function (expr, ...)  
deriv3 : function (expr, ...)  
deviance : function (object, ...)  
dexp : function (x, rate = 1, log = FALSE)  
df : function (x, df1, df2, ncp, log = FALSE)  
df.kernel : function (k)  
df.residual : function (object, ...)  
DF2formula : function (x, env = parent.frame())  
dfbeta : function (model, ...)  
dfbetas : function (model, ...)  
dffits : function (model, infl = lm.influence(model, do.coef = FALSE), res = weighted.residuals(model))  
dgamma : function (x, shape, rate = 1, scale = 1/rate, log = FALSE)  
dgeom : function (x, prob, log = FALSE)  
dhyper : function (x, m, n, k, log = FALSE)  
diffinv : function (x, ...)  
dist : function (x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)  
dlnorm : function (x, meanlog = 0, sdlog = 1, log = FALSE)  
dlogis : function (x, location = 0, scale = 1, log = FALSE)  
dmultinom : function (x, size = NULL, prob, log = FALSE)  
dnbinom : function (x, size, prob, mu, log = FALSE)  
dnorm : function (x, mean = 0, sd = 1, log = FALSE)  
dpois : function (x, lambda, log = FALSE)  
drop.scope : function (terms1, terms2)  
drop.terms : function (termobj, dropx = NULL, keep.response = FALSE)  
drop1 : function (object, scope, ...)  
dsignrank : function (x, n, log = FALSE)  
dt : function (x, df, ncp, log = FALSE)  
dummy.coef : function (object, ...)  
dummy.coef.lm : function (object, use.na = FALSE, ...)  
dunif : function (x, min = 0, max = 1, log = FALSE)  
dweibull : function (x, shape, scale = 1, log = FALSE)  
dwilcox : function (x, m, n, log = FALSE)  
ecdf : function (x)  
eff.aovlist : function (aovlist)  
effects : function (object, ...)  
embed : function (x, dimension = 1)  
end : function (x, ...)  
estVar : function (object, ...)  
expand.model.frame : function (model, extras, envir = environment(formula(model)), na.expand = FALSE)  
extractAIC : function (fit, scale, k = 2, ...)  
factanal : function (x, factors, data = NULL, covmat = NULL, n.obs = NA, subset, na.action, start = NULL, 
scores = c("none", "regression", "Bartlett"), rotation = "varimax", control = NULL, 
...)  
factor.scope : function (factor, scope)  
family : function (object, ...)  
fft : function (z, inverse = FALSE)  
filter : function (x, filter, method = c("convolution", "recursive"), sides = 2L, circular = FALSE, 
init = NULL)  
fisher.test : function (x, y = NULL, workspace = 2e+05, hybrid = FALSE, hybridPars = c(expect = 5, 
percent = 80, Emin = 1), control = list(), or = 1, alternative = "two.sided", 
conf.int = TRUE, conf.level = 0.95, simulate.p.value = FALSE, B = 2000)  
fitted : function (object, ...)  
fitted.values : function (object, ...)  
fivenum : function (x, na.rm = TRUE)  
fligner.test : function (x, ...)  
formula : function (x, ...)  
frequency : function (x, ...)  
friedman.test : function (y, ...)  
ftable : function (x, ...)  
Gamma : function (link = "inverse")  
gaussian : function (link = "identity")  
get_all_vars : function (formula, data = NULL, ...)  
getCall : function (x, ...)  
getInitial : function (object, data, ...)  
glm : function (formula, family = gaussian, data, weights, subset, na.action, start = NULL, 
etastart, mustart, offset, control = list(...), model = TRUE, method = "glm.fit", 
x = FALSE, y = TRUE, singular.ok = TRUE, contrasts = NULL, ...)  
glm.control : function (epsilon = 1e-08, maxit = 25, trace = FALSE)  
glm.fit : function (x, y, weights = rep.int(1, nobs), start = NULL, etastart = NULL, mustart = NULL, 
offset = rep.int(0, nobs), family = gaussian(), control = list(), intercept = TRUE, 
singular.ok = TRUE)  
hasTsp : function (x)  
hat : function (x, intercept = TRUE)  
hatvalues : function (model, ...)  
hclust : function (d, method = "complete", members = NULL)  
heatmap : function (x, Rowv = NULL, Colv = if (symm) "Rowv" else NULL, distfun = dist, hclustfun = hclust, 
reorderfun = function(d, w) reorder(d, w), add.expr, symm = FALSE, revC = identical(Colv, 
"Rowv"), scale = c("row", "column", "none"), na.rm = TRUE, margins = c(5, 
5), ColSideColors, RowSideColors, cexRow = 0.2 + 1/log10(nr), cexCol = 0.2 + 
1/log10(nc), labRow = NULL, labCol = NULL, main = NULL, xlab = NULL, ylab = NULL, 
keep.dendro = FALSE, verbose = getOption("verbose"), ...)  
HoltWinters : function (x, alpha = NULL, beta = NULL, gamma = NULL, seasonal = c("additive", "multiplicative"), 
start.periods = 2, l.start = NULL, b.start = NULL, s.start = NULL, optim.start = c(alpha = 0.3, 
beta = 0.1, gamma = 0.1), optim.control = list())  
influence : function (model, ...)  
influence.measures : function (model, infl = influence(model))  
integrate : function (f, lower, upper, ..., subdivisions = 100L, rel.tol = .Machine$double.eps^0.25, 
abs.tol = rel.tol, stop.on.error = TRUE, keep.xy = FALSE, aux = NULL)  
interaction.plot : function (x.factor, trace.factor, response, fun = mean, type = c("l", "p", "b", "o", 
"c"), legend = TRUE, trace.label = deparse1(substitute(trace.factor)), fixed = FALSE, 
xlab = deparse1(substitute(x.factor)), ylab = ylabel, ylim = range(cells, na.rm = TRUE), 
lty = nc:1, col = 1, pch = c(1L:9, 0, letters), xpd = NULL, leg.bg = par("bg"), 
leg.bty = "n", xtick = FALSE, xaxt = par("xaxt"), axes = TRUE, ...)  
inverse.gaussian : function (link = "1/mu^2")  
IQR : function (x, na.rm = FALSE, type = 7)  
is.empty.model : function (x)  
is.leaf : function (object)  
is.mts : function (x)  
is.stepfun : function (x)  
is.ts : function (x)  
is.tskernel : function (k)  
isoreg : function (x, y = NULL)  
KalmanForecast : function (n.ahead = 10L, mod, update = FALSE)  
KalmanLike : function (y, mod, nit = 0L, update = FALSE)  
KalmanRun : function (y, mod, nit = 0L, update = FALSE)  
KalmanSmooth : function (y, mod, nit = 0L)  
kernapply : function (x, ...)  
kernel : function (coef, m = 2, r, name = "unknown")  
kmeans : function (x, centers, iter.max = 10L, nstart = 1L, algorithm = c("Hartigan-Wong", 
"Lloyd", "Forgy", "MacQueen"), trace = FALSE)  
knots : function (Fn, ...)  
kruskal.test : function (x, ...)  
ks.test : function (x, ...)  
ksmooth : function (x, y, kernel = c("box", "normal"), bandwidth = 0.5, range.x = range(x), 
n.points = max(100L, length(x)), x.points)  
lag : function (x, ...)  
lag.plot : function (x, lags = 1, layout = NULL, set.lags = 1L:lags, main = NULL, asp = 1, diag = TRUE, 
diag.col = "gray", type = "p", oma = NULL, ask = NULL, do.lines = (n <= 150), 
labels = do.lines, ...)  
line : function (x, y = NULL, iter = 1)  
lm : function (formula, data, subset, weights, na.action, method = "qr", model = TRUE, 
x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, contrasts = NULL, offset, 
...)  
lm.fit : function (x, y, offset = NULL, method = "qr", tol = 1e-07, singular.ok = TRUE, ...)  
lm.influence : function (model, do.coef = TRUE)  
lm.wfit : function (x, y, w, offset = NULL, method = "qr", tol = 1e-07, singular.ok = TRUE, 
...)  
loadings : function (x, ...)  
loess : function (formula, data, weights, subset, na.action, model = FALSE, span = 0.75, enp.target, 
degree = 2L, parametric = FALSE, drop.square = FALSE, normalize = TRUE, family = c("gaussian", 
"symmetric"), method = c("loess", "model.frame"), control = loess.control(...), 
...)  
loess.control : function (surface = c("interpolate", "direct"), statistics = c("approximate", "exact", 
"none"), trace.hat = c("exact", "approximate"), cell = 0.2, iterations = 4L, iterTrace = FALSE, 
...)  
loess.smooth : function (x, y, span = 2/3, degree = 1, family = c("symmetric", "gaussian"), evaluation = 50, 
...)  
logLik : function (object, ...)  
loglin : function (table, margin, start = rep(1, length(table)), fit = FALSE, eps = 0.1, iter = 20L, 
param = FALSE, print = TRUE)  
lowess : function (x, y = NULL, f = 2/3, iter = 3L, delta = 0.01 * diff(range(x)))  
ls.diag : function (ls.out)  
ls.print : function (ls.out, digits = 4L, print.it = TRUE)  
lsfit : function (x, y, wt = NULL, intercept = TRUE, tolerance = 1e-07, yname = NULL)  
mad : function (x, center = median(x), constant = 1.4826, na.rm = FALSE, low = FALSE, high = FALSE)  
mahalanobis : function (x, center, cov, inverted = FALSE, ...)  
make.link : function (link)  
makeARIMA : function (phi, theta, Delta, kappa = 1e+06, SSinit = c("Gardner1980", "Rossignol2011"), 
tol = .Machine$double.eps)  
makepredictcall : function (var, call)  
manova : function (...)  
mantelhaen.test : function (x, y = NULL, z = NULL, alternative = c("two.sided", "less", "greater"), 
correct = TRUE, exact = FALSE, conf.level = 0.95)  
mauchly.test : function (object, ...)  
mcnemar.test : function (x, y = NULL, correct = TRUE)  
median : function (x, na.rm = FALSE, ...)  
median.default : function (x, na.rm = FALSE, ...)  
medpolish : function (x, eps = 0.01, maxiter = 10L, trace.iter = TRUE, na.rm = FALSE)  
model.extract : function (frame, component)  
model.frame : function (formula, ...)  
model.frame.default : function (formula, data = NULL, subset = NULL, na.action = na.fail, drop.unused.levels = FALSE, 
xlev = NULL, ...)  
model.matrix : function (object, ...)  
model.matrix.default : function (object, data = environment(object), contrasts.arg = NULL, xlev = NULL, ...)  
model.matrix.lm : function (object, ...)  
model.offset : function (x)  
model.response : function (data, type = "any")  
model.tables : function (x, ...)  
model.weights : function (x)  
monthplot : function (x, ...)  
mood.test : function (x, ...)  
mvfft : function (z, inverse = FALSE)  
na.action : function (object, ...)  
na.contiguous : function (object, ...)  
na.exclude : function (object, ...)  
na.fail : function (object, ...)  
na.omit : function (object, ...)  
na.pass : function (object, ...)  
napredict : function (omit, x, ...)  
naprint : function (x, ...)  
naresid : function (omit, x, ...)  
nextn : function (n, factors = c(2, 3, 5))  
nlm : function (f, p, ..., hessian = FALSE, typsize = rep(1, length(p)), fscale = 1, print.level = 0, 
ndigit = 12, gradtol = 1e-06, stepmax = max(1000 * sqrt(sum((p/typsize)^2)), 1000), 
steptol = 1e-06, iterlim = 100, check.analyticals = TRUE)  
nlminb : function (start, objective, gradient = NULL, hessian = NULL, ..., scale = 1, control = list(), 
lower = -Inf, upper = Inf)  
nls : function (formula, data = parent.frame(), start, control = nls.control(), algorithm = c("default", 
"plinear", "port"), trace = FALSE, subset, weights, na.action, model = FALSE, 
lower = -Inf, upper = Inf, ...)  
nls.control : function (maxiter = 50, tol = 1e-05, minFactor = 1/1024, printEval = FALSE, warnOnly = FALSE, 
scaleOffset = 0, nDcentral = FALSE)  
NLSstAsymptotic : function (xy)  
NLSstClosestX : function (xy, yval)  
NLSstLfAsymptote : function (xy)  
NLSstRtAsymptote : function (xy)  
nobs : function (object, ...)  
numericDeriv : function (expr, theta, rho = parent.frame(), dir = 1, eps = .Machine$double.eps^(1/if (central) 3 else 2), 
central = FALSE)  
offset : function (object)  
oneway.test : function (formula, data, subset, na.action, var.equal = FALSE)  
optim : function (par, fn, gr = NULL, ..., method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", 
"SANN", "Brent"), lower = -Inf, upper = Inf, control = list(), hessian = FALSE)  
optimHess : function (par, fn, gr = NULL, ..., control = list())  
optimise : function (f, interval, ..., lower = min(interval), upper = max(interval), maximum = FALSE, 
tol = .Machine$double.eps^0.25)  
optimize : function (f, interval, ..., lower = min(interval), upper = max(interval), maximum = FALSE, 
tol = .Machine$double.eps^0.25)  
order.dendrogram : function (x)  
p.adjust : function (p, method = p.adjust.methods, n = length(p))  
p.adjust.methods :  chr [1:8] "holm" "hochberg" "hommel" "bonferroni" "BH" "BY" "fdr" "none"
pacf : function (x, lag.max, plot, na.action, ...)  
Pair : function (x, y)  
pairwise.prop.test : function (x, n, p.adjust.method = p.adjust.methods, ...)  
pairwise.t.test : function (x, g, p.adjust.method = p.adjust.methods, pool.sd = !paired, paired = FALSE, 
alternative = c("two.sided", "less", "greater"), ...)  
pairwise.table : function (compare.levels, level.names, p.adjust.method)  
pairwise.wilcox.test : function (x, g, p.adjust.method = p.adjust.methods, paired = FALSE, ...)  
pbeta : function (q, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p = FALSE)  
pbinom : function (q, size, prob, lower.tail = TRUE, log.p = FALSE)  
pbirthday : function (n, classes = 365, coincident = 2)  
pcauchy : function (q, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)  
pchisq : function (q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)  
pexp : function (q, rate = 1, lower.tail = TRUE, log.p = FALSE)  
pf : function (q, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE)  
pgamma : function (q, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, log.p = FALSE)  
pgeom : function (q, prob, lower.tail = TRUE, log.p = FALSE)  
phyper : function (q, m, n, k, lower.tail = TRUE, log.p = FALSE)  
plclust : function (tree, hang = 0.1, unit = FALSE, level = FALSE, hmin = 0, square = TRUE, 
labels = NULL, plot. = TRUE, axes = TRUE, frame.plot = FALSE, ann = TRUE, main = "", 
sub = NULL, xlab = NULL, ylab = "Height")  
plnorm : function (q, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE)  
plogis : function (q, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)  
plot.ecdf : function (x, ..., ylab = "Fn(x)", verticals = FALSE, col.01line = "gray70", pch = 19)  
plot.spec.coherency : function (x, ci = 0.95, xlab = "frequency", ylab = "squared coherency", ylim = c(0, 
1), type = "l", main = NULL, ci.col = "blue", ci.lty = 3, ...)  
plot.spec.phase : function (x, ci = 0.95, xlab = "frequency", ylab = "phase", ylim = c(-pi, pi), type = "l", 
main = NULL, ci.col = "blue", ci.lty = 3, ...)  
plot.stepfun : function (x, xval, xlim, ylim = range(c(y, Fn.kn)), xlab = "x", ylab = "f(x)", main = NULL, 
add = FALSE, verticals = TRUE, do.points = (n < 1000), pch = par("pch"), col = par("col"), 
col.points = col, cex.points = par("cex"), col.hor = col, col.vert = col, lty = par("lty"), 
lwd = par("lwd"), ...)  
plot.ts : function (x, y = NULL, plot.type = c("multiple", "single"), xy.labels, xy.lines, panel = lines, 
nc, yax.flip = FALSE, mar.multi = c(0, 5.1, 0, if (yax.flip) 5.1 else 2.1), oma.multi = c(6, 
0, 5, 0), axes = TRUE, ...)  
pnbinom : function (q, size, prob, mu, lower.tail = TRUE, log.p = FALSE)  
pnorm : function (q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  
poisson : function (link = "log")  
poisson.test : function (x, T = 1, r = 1, alternative = c("two.sided", "less", "greater"), conf.level = 0.95)  
poly : function (x, ..., degree = 1, coefs = NULL, raw = FALSE, simple = FALSE)  
polym : function (..., degree = 1, coefs = NULL, raw = FALSE)  
power : function (lambda = 1)  
power.anova.test : function (groups = NULL, n = NULL, between.var = NULL, within.var = NULL, sig.level = 0.05, 
power = NULL)  
power.prop.test : function (n = NULL, p1 = NULL, p2 = NULL, sig.level = 0.05, power = NULL, alternative = c("two.sided", 
"one.sided"), strict = FALSE, tol = .Machine$double.eps^0.25)  
power.t.test : function (n = NULL, delta = NULL, sd = 1, sig.level = 0.05, power = NULL, type = c("two.sample", 
"one.sample", "paired"), alternative = c("two.sided", "one.sided"), strict = FALSE, 
tol = .Machine$double.eps^0.25)  
PP.test : function (x, lshort = TRUE)  
ppoints : function (n, a = if (n <= 10) 3/8 else 1/2)  
ppois : function (q, lambda, lower.tail = TRUE, log.p = FALSE)  
ppr : function (x, ...)  
prcomp : function (x, ...)  
predict : function (object, ...)  
predict.glm : function (object, newdata = NULL, type = c("link", "response", "terms"), se.fit = FALSE, 
dispersion = NULL, terms = NULL, na.action = na.pass, ...)  
predict.lm : function (object, newdata, se.fit = FALSE, scale = NULL, df = Inf, interval = c("none", 
"confidence", "prediction"), level = 0.95, type = c("response", "terms"), terms = NULL, 
na.action = na.pass, pred.var = res.var/weights, weights = 1, ...)  
preplot : function (object, ...)  
princomp : function (x, ...)  
printCoefmat : function (x, digits = max(3L, getOption("digits") - 2L), signif.stars = getOption("show.signif.stars"), 
signif.legend = signif.stars, dig.tst = max(1L, min(5L, digits - 1L)), cs.ind = 1:k, 
tst.ind = k + 1, zap.ind = integer(), P.values = NULL, has.Pvalue = nc >= 4L && 
length(cn <- colnames(x)) && substr(cn[nc], 1L, 3L) %in% c("Pr(", "p-v"), 
eps.Pvalue = .Machine$double.eps, na.print = "NA", quote = FALSE, right = TRUE, 
...)  
profile : function (fitted, ...)  
proj : function (object, ...)  
promax : function (x, m = 4)  
prop.test : function (x, n, p = NULL, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, 
correct = TRUE)  
prop.trend.test : function (x, n, score = seq_along(x))  
psignrank : function (q, n, lower.tail = TRUE, log.p = FALSE)  
psmirnov : function (q, sizes, z = NULL, two.sided = TRUE, exact = TRUE, simulate = FALSE, B = 2000, 
lower.tail = TRUE, log.p = FALSE)  
pt : function (q, df, ncp, lower.tail = TRUE, log.p = FALSE)  
ptukey : function (q, nmeans, df, nranges = 1, lower.tail = TRUE, log.p = FALSE)  
punif : function (q, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)  
pweibull : function (q, shape, scale = 1, lower.tail = TRUE, log.p = FALSE)  
pwilcox : function (q, m, n, lower.tail = TRUE, log.p = FALSE)  
qbeta : function (p, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p = FALSE)  
qbinom : function (p, size, prob, lower.tail = TRUE, log.p = FALSE)  
qbirthday : function (prob = 0.5, classes = 365, coincident = 2)  
qcauchy : function (p, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)  
qchisq : function (p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)  
qexp : function (p, rate = 1, lower.tail = TRUE, log.p = FALSE)  
qf : function (p, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE)  
qgamma : function (p, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, log.p = FALSE)  
qgeom : function (p, prob, lower.tail = TRUE, log.p = FALSE)  
qhyper : function (p, m, n, k, lower.tail = TRUE, log.p = FALSE)  
qlnorm : function (p, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE)  
qlogis : function (p, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)  
qnbinom : function (p, size, prob, mu, lower.tail = TRUE, log.p = FALSE)  
qnorm : function (p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  
qpois : function (p, lambda, lower.tail = TRUE, log.p = FALSE)  
qqline : function (y, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7, 
...)  
qqnorm : function (y, ...)  
qqplot : function (x, y, plot.it = TRUE, xlab = deparse1(substitute(x)), ylab = deparse1(substitute(y)), 
...)  
qsignrank : function (p, n, lower.tail = TRUE, log.p = FALSE)  
qsmirnov : function (p, sizes, z = NULL, two.sided = TRUE, exact = TRUE, simulate = FALSE, B = 2000)  
qt : function (p, df, ncp, lower.tail = TRUE, log.p = FALSE)  
qtukey : function (p, nmeans, df, nranges = 1, lower.tail = TRUE, log.p = FALSE)  
quade.test : function (y, ...)  
quantile : function (x, ...)  
quasi : function (link = "identity", variance = "constant")  
quasibinomial : function (link = "logit")  
quasipoisson : function (link = "log")  
qunif : function (p, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)  
qweibull : function (p, shape, scale = 1, lower.tail = TRUE, log.p = FALSE)  
qwilcox : function (p, m, n, lower.tail = TRUE, log.p = FALSE)  
r2dtable : function (n, r, c)  
rbeta : function (n, shape1, shape2, ncp = 0)  
rbinom : function (n, size, prob)  
rcauchy : function (n, location = 0, scale = 1)  
rchisq : function (n, df, ncp = 0)  
read.ftable : function (file, sep = "", quote = "\"", row.var.names, col.vars, skip = 0)  
rect.hclust : function (tree, k = NULL, which = NULL, x = NULL, h = NULL, border = 2, cluster = NULL)  
reformulate : function (termlabels, response = NULL, intercept = TRUE, env = parent.frame())  
relevel : function (x, ref, ...)  
reorder : function (x, ...)  
replications : function (formula, data = NULL, na.action)  
reshape : function (data, varying = NULL, v.names = NULL, timevar = "time", idvar = "id", ids = 1L:NROW(data), 
times = seq_along(varying[[1L]]), drop = NULL, direction, new.row.names = NULL, 
sep = ".", split = if (sep == "") {
list(regexp = "[A-Za-z][0-9]", include = TRUE)
} else {
list(regexp = sep, include = FALSE, fixed = TRUE)
})  
resid : function (object, ...)  
residuals : function (object, ...)  
residuals.glm : function (object, type = c("deviance", "pearson", "working", "response", "partial"), 
...)  
residuals.lm : function (object, type = c("working", "response", "deviance", "pearson", "partial"), 
...)  
rexp : function (n, rate = 1)  
rf : function (n, df1, df2, ncp)  
rgamma : function (n, shape, rate = 1, scale = 1/rate)  
rgeom : function (n, prob)  
rhyper : function (nn, m, n, k)  
rlnorm : function (n, meanlog = 0, sdlog = 1)  
rlogis : function (n, location = 0, scale = 1)  
rmultinom : function (n, size, prob)  
rnbinom : function (n, size, prob, mu)  
rnorm : function (n, mean = 0, sd = 1)  
rpois : function (n, lambda)  
rsignrank : function (nn, n)  
rsmirnov : function (n, sizes, z = NULL, two.sided = TRUE)  
rstandard : function (model, ...)  
rstudent : function (model, ...)  
rt : function (n, df, ncp)  
runif : function (n, min = 0, max = 1)  
runmed : function (x, k, endrule = c("median", "keep", "constant"), algorithm = NULL, na.action = c("+Big_alternate", 
"-Big_alternate", "na.omit", "fail"), print.level = 0)  
rweibull : function (n, shape, scale = 1)  
rwilcox : function (nn, m, n)  
rWishart : function (n, df, Sigma)  
scatter.smooth : function (x, y = NULL, span = 2/3, degree = 1, family = c("symmetric", "gaussian"), 
xlab = NULL, ylab = NULL, ylim = range(y, pred$y, na.rm = TRUE), evaluation = 50, 
..., lpars = list())  
screeplot : function (x, ...)  
sd : function (x, na.rm = FALSE)  
se.contrast : function (object, ...)  
selfStart : function (model, initial, parameters, template)  
setNames : function (object = nm, nm)  
shapiro.test : function (x)  
sigma : function (object, ...)  
simulate : function (object, nsim = 1, seed = NULL, ...)  
smooth : function (x, kind = c("3RS3R", "3RSS", "3RSR", "3R", "3", "S"), twiceit = FALSE, endrule = c("Tukey", 
"copy"), do.ends = FALSE)  
smooth.spline : function (x, y = NULL, w = NULL, df, spar = NULL, lambda = NULL, cv = FALSE, all.knots = FALSE, 
nknots = .nknots.smspl, keep.data = TRUE, df.offset = 0, penalty = 1, control.spar = list(), 
tol = 1e-06 * IQR(x), keep.stuff = FALSE)  
smoothEnds : function (y, k = 3)  
sortedXyData : function (x, y, data)  
spec.ar : function (x, n.freq, order = NULL, plot = TRUE, na.action = na.fail, method = "yule-walker", 
...)  
spec.pgram : function (x, spans = NULL, kernel = NULL, taper = 0.1, pad = 0, fast = TRUE, demean = FALSE, 
detrend = TRUE, plot = TRUE, na.action = na.fail, ...)  
spec.taper : function (x, p = 0.1)  
spectrum : function (x, ..., method = c("pgram", "ar"))  
spline : function (x, y = NULL, n = 3 * length(x), method = "fmm", xmin = min(x), xmax = max(x), 
xout, ties = mean)  
splinefun : function (x, y = NULL, method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"), 
ties = mean)  
splinefunH : function (x, y, m)  
SSasymp : function (input, Asym, R0, lrc)  
SSasympOff : function (input, Asym, lrc, c0)  
SSasympOrig : function (input, Asym, lrc)  
SSbiexp : function (input, A1, lrc1, A2, lrc2)  
SSD : function (object, ...)  
SSfol : function (Dose, input, lKe, lKa, lCl)  
SSfpl : function (input, A, B, xmid, scal)  
SSgompertz : function (x, Asym, b2, b3)  
SSlogis : function (input, Asym, xmid, scal)  
SSmicmen : function (input, Vm, K)  
SSweibull : function (x, Asym, Drop, lrc, pwr)  
start : function (x, ...)  
stat.anova : function (table, test = c("Rao", "LRT", "Chisq", "F", "Cp"), scale, df.scale, n)  
step : function (object, scope, scale = 0, direction = c("both", "backward", "forward"), 
trace = 1, keep = NULL, steps = 1000, k = 2, ...)  
stepfun : function (x, y, f = as.numeric(right), ties = "ordered", right = FALSE)  
stl : function (x, s.window, s.degree = 0, t.window = NULL, t.degree = 1, l.window = nextodd(period), 
l.degree = t.degree, s.jump = ceiling(s.window/10), t.jump = ceiling(t.window/10), 
l.jump = ceiling(l.window/10), robust = FALSE, inner = if (robust) 1 else 2, outer = if (robust) 15 else 0, 
na.action = na.fail)  
StructTS : function (x, type = c("level", "trend", "BSM"), init = NULL, fixed = NULL, optim.control = NULL)  
summary.aov : function (object, intercept = FALSE, split, expand.split = TRUE, keep.zero.df = TRUE, 
...)  
summary.glm : function (object, dispersion = NULL, correlation = FALSE, symbolic.cor = FALSE, ...)  
summary.lm : function (object, correlation = FALSE, symbolic.cor = FALSE, ...)  
summary.manova : function (object, test = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"), intercept = FALSE, 
tol = 1e-07, ...)  
summary.stepfun : function (object, ...)  
supsmu : function (x, y, wt = rep(1, n), span = "cv", periodic = FALSE, bass = 0, trace = FALSE)  
symnum : function (x, cutpoints = c(0.3, 0.6, 0.8, 0.9, 0.95), symbols = if (numeric.x) c(" ", 
".", ",", "+", "*", "B") else c(".", "|"), legend = length(symbols) >= 3, na = "?", 
eps = 1e-05, numeric.x = is.numeric(x), corr = missing(cutpoints) && numeric.x, 
show.max = if (corr) "1", show.min = NULL, abbr.colnames = has.colnames, lower.triangular = corr && 
is.numeric(x) && is.matrix(x), diag.lower.tri = corr && !is.null(show.max))  
t.test : function (x, ...)  
termplot : function (model, data = NULL, envir = environment(formula(model)), partial.resid = FALSE, 
rug = FALSE, terms = NULL, se = FALSE, xlabs = NULL, ylabs = NULL, main = NULL, 
col.term = 2, lwd.term = 1.5, col.se = "orange", lty.se = 2, lwd.se = 1, col.res = "gray", 
cex = 1, pch = par("pch"), col.smth = "darkred", lty.smth = 2, span.smth = 2/3, 
ask = dev.interactive() && nb.fig < n.tms, use.factor.levels = TRUE, smooth = NULL, 
ylim = "common", plot = TRUE, transform.x = FALSE, ...)  
terms : function (x, ...)  
terms.formula : function (x, specials = NULL, abb = NULL, data = NULL, neg.out = TRUE, keep.order = FALSE, 
simplify = FALSE, ..., allowDotAsName = FALSE)  
time : function (x, ...)  
toeplitz : function (x)  
ts : function (data = NA, start = 1, end = numeric(), frequency = 1, deltat = 1, ts.eps = getOption("ts.eps"), 
class = if (nseries > 1) c("mts", "ts", "matrix") else "ts", names = if (!is.null(dimnames(data))) colnames(data) else paste("Series", 
seq(nseries)))  
ts.intersect : function (..., dframe = FALSE)  
ts.plot : function (..., gpars = list())  
ts.union : function (..., dframe = FALSE)  
tsdiag : function (object, gof.lag, ...)  
tsp : function (x)  
tsp<- : function (x, value)  
tsSmooth : function (object, ...)  
TukeyHSD : function (x, which, ordered = FALSE, conf.level = 0.95, ...)  
uniroot : function (f, interval, ..., lower = min(interval), upper = max(interval), f.lower = f(lower, 
...), f.upper = f(upper, ...), extendInt = c("no", "yes", "downX", "upX"), check.conv = FALSE, 
tol = .Machine$double.eps^0.25, maxiter = 1000, trace = 0)  
update : function (object, ...)  
update.default : function (object, formula., ..., evaluate = TRUE)  
update.formula : function (old, new, ...)  
var : function (x, y = NULL, na.rm = FALSE, use)  
var.test : function (x, ...)  
variable.names : function (object, ...)  
varimax : function (x, normalize = TRUE, eps = 1e-05)  
vcov : function (object, ...)  
weighted.mean : function (x, w, ...)  
weighted.residuals : function (obj, drop0 = TRUE)  
weights : function (object, ...)  
wilcox.test : function (x, ...)  
window : function (x, ...)  
window<- : function (x, ..., value)  
write.ftable : function (x, file = "", quote = TRUE, append = FALSE, digits = getOption("digits"), 
sep = " ", ...)  
xtabs : function (formula = ~., data = parent.frame(), subset, sparse = FALSE, na.action, 
addNA = FALSE, exclude = if (!addNA) c(NA, NaN), drop.unused.levels = FALSE)  
                                                                                                                                                                                                                                                                                                                                                                                                                > 
library(stringr)
%>% : function (lhs, rhs)  
boundary : function (type = c("character", "line_break", "sentence", "word"), skip_word_none = NA, 
...)  
coll : function (pattern, ignore_case = FALSE, locale = "en", ...)  
fixed : function (pattern, ignore_case = FALSE)  
fruit :  chr [1:80] "apple" "apricot" "avocado" "banana" "bell pepper" "bilberry" "blackberry" ...
invert_match : function (loc)  
regex : function (pattern, ignore_case = FALSE, multiline = FALSE, comments = FALSE, dotall = FALSE, 
...)  
sentences :  chr [1:720] "The birch canoe slid on the smooth planks." ...
str_c : function (..., sep = "", collapse = NULL)  
str_conv : function (string, encoding)  
str_count : function (string, pattern = "")  
str_detect : function (string, pattern, negate = FALSE)  
str_dup : function (string, times)  
str_ends : function (string, pattern, negate = FALSE)  
str_extract : function (string, pattern)  
str_extract_all : function (string, pattern, simplify = FALSE)  
str_flatten : function (string, collapse = "")  
str_glue : function (..., .sep = "", .envir = parent.frame())  
str_glue_data : function (.x, ..., .sep = "", .envir = parent.frame(), .na = "NA")  
str_interp : function (string, env = parent.frame())  
str_length : function (string)  
str_locate : function (string, pattern)  
str_locate_all : function (string, pattern)  
str_match : function (string, pattern)  
str_match_all : function (string, pattern)  
str_order : function (x, decreasing = FALSE, na_last = TRUE, locale = "en", numeric = FALSE, ...)  
str_pad : function (string, width, side = c("left", "right", "both"), pad = " ")  
str_remove : function (string, pattern)  
str_remove_all : function (string, pattern)  
str_replace : function (string, pattern, replacement)  
str_replace_all : function (string, pattern, replacement)  
str_replace_na : function (string, replacement = "NA")  
str_sort : function (x, decreasing = FALSE, na_last = TRUE, locale = "en", numeric = FALSE, ...)  
str_split : function (string, pattern, n = Inf, simplify = FALSE)  
str_split_fixed : function (string, pattern, n)  
str_squish : function (string)  
str_starts : function (string, pattern, negate = FALSE)  
str_sub : function (string, start = 1L, end = -1L)  
str_sub<- : function (string, start = 1L, end = -1L, omit_na = FALSE, value)  
str_subset : function (string, pattern, negate = FALSE)  
str_to_lower : function (string, locale = "en")  
str_to_sentence : function (string, locale = "en")  
str_to_title : function (string, locale = "en")  
str_to_upper : function (string, locale = "en")  
str_trim : function (string, side = c("both", "left", "right"))  
str_trunc : function (string, width, side = c("right", "left", "center"), ellipsis = "...")  
str_view : function (string, pattern, match = NA)  
str_view_all : function (string, pattern, match = NA)  
str_which : function (string, pattern, negate = FALSE)  
str_wrap : function (string, width = 80, indent = 0, exdent = 0)  
word : function (string, start = 1L, end = start, sep = fixed(" "))  
words :  chr [1:980] "a" "able" "about" "absolute" "accept" "account" "achieve" "across" "act" ...

library(stringi)
%s!=% : function (e1, e2)  
%s!==% : function (e1, e2)  
%s$% : function (e1, e2)  
%s*% : function (e1, e2)  
%s+% : function (e1, e2)  
%s<% : function (e1, e2)  
%s<=% : function (e1, e2)  
%s==% : function (e1, e2)  
%s===% : function (e1, e2)  
%s>% : function (e1, e2)  
%s>=% : function (e1, e2)  
%stri!=% : function (e1, e2)  
%stri!==% : function (e1, e2)  
%stri$% : function (e1, e2)  
%stri*% : function (e1, e2)  
%stri+% : function (e1, e2)  
%stri<% : function (e1, e2)  
%stri<=% : function (e1, e2)  
%stri==% : function (e1, e2)  
%stri===% : function (e1, e2)  
%stri>% : function (e1, e2)  
%stri>=% : function (e1, e2)  
stri_c : function (..., sep = "", collapse = NULL, ignore_null = FALSE)  
stri_c_list : function (x, sep = "", collapse = NULL)  
stri_cmp : function (e1, e2, ..., opts_collator = NULL)  
stri_cmp_eq : function (e1, e2)  
stri_cmp_equiv : function (e1, e2, ..., opts_collator = NULL)  
stri_cmp_ge : function (e1, e2, ..., opts_collator = NULL)  
stri_cmp_gt : function (e1, e2, ..., opts_collator = NULL)  
stri_cmp_le : function (e1, e2, ..., opts_collator = NULL)  
stri_cmp_lt : function (e1, e2, ..., opts_collator = NULL)  
stri_cmp_neq : function (e1, e2)  
stri_cmp_nequiv : function (e1, e2, ..., opts_collator = NULL)  
stri_coll : function (locale = NULL, strength = 3L, alternate_shifted = FALSE, french = FALSE, 
uppercase_first = NA, case_level = FALSE, normalization = FALSE, normalisation = normalization, 
numeric = FALSE, ...)  
stri_compare : function (e1, e2, ..., opts_collator = NULL)  
stri_conv : function (str, from = NULL, to = NULL, to_raw = FALSE)  
stri_count : function (str, ..., regex, fixed, coll, charclass)  
stri_count_boundaries : function (str, ..., opts_brkiter = NULL)  
stri_count_charclass : function (str, pattern)  
stri_count_coll : function (str, pattern, ..., opts_collator = NULL)  
stri_count_fixed : function (str, pattern, ..., opts_fixed = NULL)  
stri_count_regex : function (str, pattern, ..., opts_regex = NULL)  
stri_count_words : function (str, locale = NULL)  
stri_datetime_add : function (time, value = 1L, units = "seconds", tz = NULL, locale = NULL)  
stri_datetime_add<- : function (time, units = "seconds", tz = NULL, locale = NULL, value)  
stri_datetime_create : function (year, month, day, hour = 12L, minute = 0L, second = 0, lenient = FALSE, 
tz = NULL, locale = NULL)  
stri_datetime_fields : function (time, tz = attr(time, "tzone"), locale = NULL)  
stri_datetime_format : function (time, format = "uuuu-MM-dd HH:mm:ss", tz = NULL, locale = NULL)  
stri_datetime_fstr : function (x, ignore_special = TRUE)  
stri_datetime_now : function ()  
stri_datetime_parse : function (str, format = "uuuu-MM-dd HH:mm:ss", lenient = FALSE, tz = NULL, locale = NULL)  
stri_datetime_symbols : function (locale = NULL, context = "standalone", width = "wide")  
stri_detect : function (str, ..., regex, fixed, coll, charclass)  
stri_detect_charclass : function (str, pattern, negate = FALSE, max_count = -1)  
stri_detect_coll : function (str, pattern, negate = FALSE, max_count = -1, ..., opts_collator = NULL)  
stri_detect_fixed : function (str, pattern, negate = FALSE, max_count = -1, ..., opts_fixed = NULL)  
stri_detect_regex : function (str, pattern, negate = FALSE, max_count = -1, ..., opts_regex = NULL)  
stri_dup : function (str, times)  
stri_duplicated : function (str, from_last = FALSE, fromLast = from_last, ..., opts_collator = NULL)  
stri_duplicated_any : function (str, from_last = FALSE, fromLast = from_last, ..., opts_collator = NULL)  
stri_enc_detect : function (str, filter_angle_brackets = FALSE)  
stri_enc_detect2 : function (str, locale = NULL)  
stri_enc_fromutf32 : function (vec)  
stri_enc_get : function ()  
stri_enc_info : function (enc = NULL)  
stri_enc_isascii : function (str)  
stri_enc_isutf16be : function (str)  
stri_enc_isutf16le : function (str)  
stri_enc_isutf32be : function (str)  
stri_enc_isutf32le : function (str)  
stri_enc_isutf8 : function (str)  
stri_enc_list : function (simplify = TRUE)  
stri_enc_mark : function (str)  
stri_enc_set : function (enc)  
stri_enc_toascii : function (str)  
stri_enc_tonative : function (str)  
stri_enc_toutf32 : function (str)  
stri_enc_toutf8 : function (str, is_unknown_8bit = FALSE, validate = FALSE)  
stri_encode : function (str, from = NULL, to = NULL, to_raw = FALSE)  
stri_endswith : function (str, ..., fixed, coll, charclass)  
stri_endswith_charclass : function (str, pattern, to = -1L, negate = FALSE)  
stri_endswith_coll : function (str, pattern, to = -1L, negate = FALSE, ..., opts_collator = NULL)  
stri_endswith_fixed : function (str, pattern, to = -1L, negate = FALSE, ..., opts_fixed = NULL)  
stri_escape_unicode : function (str)  
stri_extract : function (str, ..., regex, fixed, coll, charclass, mode = c("first", "all", "last"))  
stri_extract_all : function (str, ..., regex, fixed, coll, charclass)  
stri_extract_all_boundaries : function (str, simplify = FALSE, omit_no_match = FALSE, ..., opts_brkiter = NULL)  
stri_extract_all_charclass : function (str, pattern, merge = TRUE, simplify = FALSE, omit_no_match = FALSE)  
stri_extract_all_coll : function (str, pattern, simplify = FALSE, omit_no_match = FALSE, ..., opts_collator = NULL)  
stri_extract_all_fixed : function (str, pattern, simplify = FALSE, omit_no_match = FALSE, ..., opts_fixed = NULL)  
stri_extract_all_regex : function (str, pattern, simplify = FALSE, omit_no_match = FALSE, ..., opts_regex = NULL)  
stri_extract_all_words : function (str, simplify = FALSE, omit_no_match = FALSE, locale = NULL)  
stri_extract_first : function (str, ..., regex, fixed, coll, charclass)  
stri_extract_first_boundaries : function (str, ..., opts_brkiter = NULL)  
stri_extract_first_charclass : function (str, pattern)  
stri_extract_first_coll : function (str, pattern, ..., opts_collator = NULL)  
stri_extract_first_fixed : function (str, pattern, ..., opts_fixed = NULL)  
stri_extract_first_regex : function (str, pattern, ..., opts_regex = NULL)  
stri_extract_first_words : function (str, locale = NULL)  
stri_extract_last : function (str, ..., regex, fixed, coll, charclass)  
stri_extract_last_boundaries : function (str, ..., opts_brkiter = NULL)  
stri_extract_last_charclass : function (str, pattern)  
stri_extract_last_coll : function (str, pattern, ..., opts_collator = NULL)  
stri_extract_last_fixed : function (str, pattern, ..., opts_fixed = NULL)  
stri_extract_last_regex : function (str, pattern, ..., opts_regex = NULL)  
stri_extract_last_words : function (str, locale = NULL)  
stri_flatten : function (str, collapse = "", na_empty = FALSE, omit_empty = FALSE)  
stri_info : function (short = FALSE)  
stri_isempty : function (str)  
stri_join : function (..., sep = "", collapse = NULL, ignore_null = FALSE)  
stri_join_list : function (x, sep = "", collapse = NULL)  
stri_length : function (str)  
stri_list2matrix : function (x, byrow = FALSE, fill = NA_character_, n_min = 0, by_row = byrow)  
stri_locale_get : function ()  
stri_locale_info : function (locale = NULL)  
stri_locale_list : function ()  
stri_locale_set : function (locale)  
stri_locate : function (str, ..., regex, fixed, coll, charclass, mode = c("first", "all", "last"))  
stri_locate_all : function (str, ..., regex, fixed, coll, charclass)  
stri_locate_all_boundaries : function (str, omit_no_match = FALSE, get_length = FALSE, ..., opts_brkiter = NULL)  
stri_locate_all_charclass : function (str, pattern, merge = TRUE, omit_no_match = FALSE, get_length = FALSE)  
stri_locate_all_coll : function (str, pattern, omit_no_match = FALSE, get_length = FALSE, ..., opts_collator = NULL)  
stri_locate_all_fixed : function (str, pattern, omit_no_match = FALSE, get_length = FALSE, ..., opts_fixed = NULL)  
stri_locate_all_regex : function (str, pattern, omit_no_match = FALSE, capture_groups = FALSE, get_length = FALSE, 
..., opts_regex = NULL)  
stri_locate_all_words : function (str, omit_no_match = FALSE, locale = NULL, get_length = FALSE)  
stri_locate_first : function (str, ..., regex, fixed, coll, charclass)  
stri_locate_first_boundaries : function (str, get_length = FALSE, ..., opts_brkiter = NULL)  
stri_locate_first_charclass : function (str, pattern, get_length = FALSE)  
stri_locate_first_coll : function (str, pattern, get_length = FALSE, ..., opts_collator = NULL)  
stri_locate_first_fixed : function (str, pattern, get_length = FALSE, ..., opts_fixed = NULL)  
stri_locate_first_regex : function (str, pattern, capture_groups = FALSE, get_length = FALSE, ..., opts_regex = NULL)  
stri_locate_first_words : function (str, locale = NULL, get_length = FALSE)  
stri_locate_last : function (str, ..., regex, fixed, coll, charclass)  
stri_locate_last_boundaries : function (str, get_length = FALSE, ..., opts_brkiter = NULL)  
stri_locate_last_charclass : function (str, pattern, get_length = FALSE)  
stri_locate_last_coll : function (str, pattern, get_length = FALSE, ..., opts_collator = NULL)  
stri_locate_last_fixed : function (str, pattern, get_length = FALSE, ..., opts_fixed = NULL)  
stri_locate_last_regex : function (str, pattern, capture_groups = FALSE, get_length = FALSE, ..., opts_regex = NULL)  
stri_locate_last_words : function (str, locale = NULL, get_length = FALSE)  
stri_match : function (str, ..., regex, mode = c("first", "all", "last"))  
stri_match_all : function (str, ..., regex)  
stri_match_all_regex : function (str, pattern, omit_no_match = FALSE, cg_missing = NA_character_, ..., opts_regex = NULL)  
stri_match_first : function (str, ..., regex)  
stri_match_first_regex : function (str, pattern, cg_missing = NA_character_, ..., opts_regex = NULL)  
stri_match_last : function (str, ..., regex)  
stri_match_last_regex : function (str, pattern, cg_missing = NA_character_, ..., opts_regex = NULL)  
stri_na2empty : function (x)  
stri_numbytes : function (str)  
stri_omit_empty : function (x, na_empty = FALSE)  
stri_omit_empty_na : function (x)  
stri_omit_na : function (x)  
stri_opts_brkiter : function (type, locale, skip_word_none, skip_word_number, skip_word_letter, skip_word_kana, 
skip_word_ideo, skip_line_soft, skip_line_hard, skip_sentence_term, skip_sentence_sep, 
...)  
stri_opts_collator : function (locale = NULL, strength = 3L, alternate_shifted = FALSE, french = FALSE, 
uppercase_first = NA, case_level = FALSE, normalization = FALSE, normalisation = normalization, 
numeric = FALSE, ...)  
stri_opts_fixed : function (case_insensitive = FALSE, overlap = FALSE, ...)  
stri_opts_regex : function (case_insensitive, comments, dotall, dot_all = dotall, literal, multiline, 
multi_line = multiline, unix_lines, uword, error_on_unknown_escapes, time_limit = 0L, 
stack_limit = 0L, ...)  
stri_order : function (str, decreasing = FALSE, na_last = TRUE, ..., opts_collator = NULL)  
stri_pad : function (str, width = floor(0.9 * getOption("width")), side = c("left", "right", 
"both"), pad = " ", use_length = FALSE)  
stri_pad_both : function (str, width = floor(0.9 * getOption("width")), pad = " ", use_length = FALSE)  
stri_pad_left : function (str, width = floor(0.9 * getOption("width")), pad = " ", use_length = FALSE)  
stri_pad_right : function (str, width = floor(0.9 * getOption("width")), pad = " ", use_length = FALSE)  
stri_paste : function (..., sep = "", collapse = NULL, ignore_null = FALSE)  
stri_paste_list : function (x, sep = "", collapse = NULL)  
stri_printf : function (format, ..., file = "", sep = "\n", append = FALSE, na_string = "NA", inf_string = "Inf", 
nan_string = "NaN", use_length = FALSE)  
stri_rand_lipsum : function (n_paragraphs, start_lipsum = TRUE, nparagraphs = n_paragraphs)  
stri_rand_shuffle : function (str)  
stri_rand_strings : function (n, length, pattern = "[A-Za-z0-9]")  
stri_rank : function (str, ..., opts_collator = NULL)  
stri_read_lines : function (con, encoding = NULL, fname = con, fallback_encoding = NULL)  
stri_read_raw : function (con, fname = con)  
stri_remove_empty : function (x, na_empty = FALSE)  
stri_remove_empty_na : function (x)  
stri_remove_na : function (x)  
stri_replace : function (str, replacement, ..., regex, fixed, coll, charclass, mode = c("first", 
"all", "last"))  
stri_replace_all : function (str, replacement, ..., regex, fixed, coll, charclass)  
stri_replace_all_charclass : function (str, pattern, replacement, merge = FALSE, vectorize_all = TRUE, vectorise_all = vectorize_all)  
stri_replace_all_coll : function (str, pattern, replacement, vectorize_all = TRUE, vectorise_all = vectorize_all, 
..., opts_collator = NULL)  
stri_replace_all_fixed : function (str, pattern, replacement, vectorize_all = TRUE, vectorise_all = vectorize_all, 
..., opts_fixed = NULL)  
stri_replace_all_regex : function (str, pattern, replacement, vectorize_all = TRUE, vectorise_all = vectorize_all, 
..., opts_regex = NULL)  
stri_replace_first : function (str, replacement, ..., regex, fixed, coll, charclass)  
stri_replace_first_charclass : function (str, pattern, replacement)  
stri_replace_first_coll : function (str, pattern, replacement, ..., opts_collator = NULL)  
stri_replace_first_fixed : function (str, pattern, replacement, ..., opts_fixed = NULL)  
stri_replace_first_regex : function (str, pattern, replacement, ..., opts_regex = NULL)  
stri_replace_last : function (str, replacement, ..., regex, fixed, coll, charclass)  
stri_replace_last_charclass : function (str, pattern, replacement)  
stri_replace_last_coll : function (str, pattern, replacement, ..., opts_collator = NULL)  
stri_replace_last_fixed : function (str, pattern, replacement, ..., opts_fixed = NULL)  
stri_replace_last_regex : function (str, pattern, replacement, ..., opts_regex = NULL)  
stri_replace_na : function (str, replacement = "NA")  
stri_replace_rstr : function (x)  
stri_reverse : function (str)  
stri_sort : function (str, decreasing = FALSE, na_last = NA, ..., opts_collator = NULL)  
stri_sort_key : function (str, ..., opts_collator = NULL)  
stri_split : function (str, ..., regex, fixed, coll, charclass)  
stri_split_boundaries : function (str, n = -1L, tokens_only = FALSE, simplify = FALSE, ..., opts_brkiter = NULL)  
stri_split_charclass : function (str, pattern, n = -1L, omit_empty = FALSE, tokens_only = FALSE, simplify = FALSE)  
stri_split_coll : function (str, pattern, n = -1L, omit_empty = FALSE, tokens_only = FALSE, simplify = FALSE, 
..., opts_collator = NULL)  
stri_split_fixed : function (str, pattern, n = -1L, omit_empty = FALSE, tokens_only = FALSE, simplify = FALSE, 
..., opts_fixed = NULL)  
stri_split_lines : function (str, omit_empty = FALSE)  
stri_split_lines1 : function (str)  
stri_split_regex : function (str, pattern, n = -1L, omit_empty = FALSE, tokens_only = FALSE, simplify = FALSE, 
..., opts_regex = NULL)  
stri_sprintf : function (format, ..., na_string = NA_character_, inf_string = "Inf", nan_string = "NaN", 
use_length = FALSE)  
stri_startswith : function (str, ..., fixed, coll, charclass)  
stri_startswith_charclass : function (str, pattern, from = 1L, negate = FALSE)  
stri_startswith_coll : function (str, pattern, from = 1L, negate = FALSE, ..., opts_collator = NULL)  
stri_startswith_fixed : function (str, pattern, from = 1L, negate = FALSE, ..., opts_fixed = NULL)  
stri_stats_general : function (str)  
stri_stats_latex : function (str)  
stri_string_format : function (format, ..., na_string = NA_character_, inf_string = "Inf", nan_string = "NaN", 
use_length = FALSE)  
stri_sub : function (str, from = 1L, to = -1L, length, use_matrix = TRUE, ignore_negative_length = FALSE)  
stri_sub_all : function (str, from = list(1L), to = list(-1L), length, use_matrix = TRUE, ignore_negative_length = TRUE)  
stri_sub_all_replace : function (..., replacement, value = replacement)  
stri_sub_all<- : function (str, from = list(1L), to = list(-1L), length, omit_na = FALSE, use_matrix = TRUE, 
value)  
stri_sub_replace : function (..., replacement, value = replacement)  
stri_sub_replace_all : function (..., replacement, value = replacement)  
stri_sub<- : function (str, from = 1L, to = -1L, length, omit_na = FALSE, use_matrix = TRUE, value)  
stri_subset : function (str, ..., regex, fixed, coll, charclass)  
stri_subset_charclass : function (str, pattern, omit_na = FALSE, negate = FALSE)  
stri_subset_charclass<- : function (str, pattern, negate = FALSE, value)  
stri_subset_coll : function (str, pattern, omit_na = FALSE, negate = FALSE, ..., opts_collator = NULL)  
stri_subset_coll<- : function (str, pattern, negate = FALSE, ..., opts_collator = NULL, value)  
stri_subset_fixed : function (str, pattern, omit_na = FALSE, negate = FALSE, ..., opts_fixed = NULL)  
stri_subset_fixed<- : function (str, pattern, negate = FALSE, ..., opts_fixed = NULL, value)  
stri_subset_regex : function (str, pattern, omit_na = FALSE, negate = FALSE, ..., opts_regex = NULL)  
stri_subset_regex<- : function (str, pattern, negate = FALSE, ..., opts_regex = NULL, value)  
stri_subset<- : function (str, ..., regex, fixed, coll, charclass, value)  
stri_timezone_get : function ()  
stri_timezone_info : function (tz = NULL, locale = NULL, display_type = "long")  
stri_timezone_list : function (region = NA_character_, offset = NA_integer_)  
stri_timezone_set : function (tz)  
stri_trans_casefold : function (str)  
stri_trans_char : function (str, pattern, replacement)  
stri_trans_general : function (str, id, rules = FALSE, forward = TRUE)  
stri_trans_isnfc : function (str)  
stri_trans_isnfd : function (str)  
stri_trans_isnfkc : function (str)  
stri_trans_isnfkc_casefold : function (str)  
stri_trans_isnfkd : function (str)  
stri_trans_list : function ()  
stri_trans_nfc : function (str)  
stri_trans_nfd : function (str)  
stri_trans_nfkc : function (str)  
stri_trans_nfkc_casefold : function (str)  
stri_trans_nfkd : function (str)  
stri_trans_tolower : function (str, locale = NULL)  
stri_trans_totitle : function (str, ..., opts_brkiter = NULL)  
stri_trans_toupper : function (str, locale = NULL)  
stri_trim : function (str, side = c("both", "left", "right"), pattern = "\\P{Wspace}", negate = FALSE)  
stri_trim_both : function (str, pattern = "\\P{Wspace}", negate = FALSE)  
stri_trim_left : function (str, pattern = "\\P{Wspace}", negate = FALSE)  
stri_trim_right : function (str, pattern = "\\P{Wspace}", negate = FALSE)  
stri_unescape_unicode : function (str)  
stri_unique : function (str, ..., opts_collator = NULL)  
stri_width : function (str)  
stri_wrap : function (str, width = floor(0.9 * getOption("width")), cost_exponent = 2, simplify = TRUE, 
normalize = TRUE, normalise = normalize, indent = 0, exdent = 0, prefix = "", 
initial = prefix, whitespace_only = FALSE, use_length = FALSE, locale = NULL)  
stri_write_lines : function (str, con, encoding = "UTF-8", sep = ifelse(.Platform$OS.type == "windows", 
"\r\n", "\n"), fname = con)  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                > 
library(utils)
.DollarNames : function (x, pattern)  
.romans :  Named int [1:13] 1000 900 500 400 100 90 50 40 10 9 ...
.RtangleCodeLabel : function (chunk)  
.S3methods : function (generic.function, class, envir = parent.frame())  
? : function (e1, e2)  
adist : function (x, y = NULL, costs = NULL, counts = FALSE, fixed = TRUE, partial = !fixed, 
ignore.case = FALSE, useBytes = FALSE)  
alarm : function ()  
apropos : function (what, where = FALSE, ignore.case = TRUE, mode = "any")  
aregexec : function (pattern, text, max.distance = 0.1, costs = NULL, ignore.case = FALSE, fixed = FALSE, 
useBytes = FALSE)  
argsAnywhere : function (x)  
arrangeWindows : function (action = c("vertical", "horizontal", "cascade", "minimize", "restore"), 
windows, preserve = TRUE, outer = FALSE)  
as.person : function (x)  
as.personList : function (x)  
as.relistable : function (x)  
as.roman : function (x)  
asDateBuilt : function (built)  
askYesNo : function (msg, default = TRUE, prompts = getOption("askYesNo", gettext(c("Yes", "No", 
"Cancel"))), ...)  
aspell : function (files, filter, control = list(), encoding = "unknown", program = NULL, dictionaries = character())  
aspell_package_C_files : function (dir, ignore = character(), control = list(), program = NULL, dictionaries = character())  
aspell_package_R_files : function (dir, ignore = character(), control = list(), program = NULL, dictionaries = character())  
aspell_package_Rd_files : function (dir, drop = c("\\author", "\\references"), control = list(), program = NULL, 
dictionaries = character())  
aspell_package_vignettes : function (dir, control = list(), program = NULL, dictionaries = character())  
aspell_write_personal_dictionary_file : function (x, out, language = "en", program = NULL)  
assignInMyNamespace : function (x, value)  
assignInNamespace : function (x, value, ns, pos = -1, envir = as.environment(pos))  
available.packages : function (contriburl = contrib.url(repos, type), method, fields = NULL, type = getOption("pkgType"), 
filters = NULL, repos = getOption("repos"), ignore_repo_cache = FALSE, max_repo_cache_age, 
quiet = TRUE, ...)  
bibentry : function (bibtype, textVersion = NULL, header = NULL, footer = NULL, key = NULL, ..., 
other = list(), mheader = NULL, mfooter = NULL)  
browseEnv : function (envir = .GlobalEnv, pattern, excludepatt = "^last\\.warning", html = .Platform$GUI != 
"AQUA", expanded = TRUE, properties = NULL, main = NULL, debugMe = FALSE)  
browseURL : function (url, browser = getOption("browser"), encodeIfNeeded = FALSE)  
browseVignettes : function (package = NULL, lib.loc = NULL, all = TRUE)  
bug.report : function (...)  
capture.output : function (..., file = NULL, append = FALSE, type = c("output", "message"), split = FALSE)  
changedFiles : function (before, after, path = before$path, timestamp = before$timestamp, check.file.info = c("size", 
"isdir", "mode", "mtime"), md5sum = before$md5sum, digest = before$digest, full.names = before$full.names, 
...)  
charClass : function (x, class)  
checkCRAN : function (method)  
choose.dir : function (default = "", caption = "Select folder")  
choose.files : function (default = "", caption = "Select files", multi = TRUE, filters = Filters, 
index = nrow(Filters))  
chooseBioCmirror : function (graphics = getOption("menu.graphics"), ind = NULL, local.only = FALSE)  
chooseCRANmirror : function (graphics = getOption("menu.graphics"), ind = NULL, local.only = FALSE)  
citation : function (package = "base", lib.loc = NULL, auto = NULL)  
cite : function (keys, bib, ...)  
citeNatbib : function (keys, bib, textual = FALSE, before = NULL, after = NULL, mode = c("authoryear", 
"numbers", "super"), abbreviate = TRUE, longnamesfirst = TRUE, bibpunct = c("(", 
")", ";", "a", "", ","), previous)  
citEntry : function (entry, textVersion, header = NULL, footer = NULL, ...)  
citFooter : function (...)  
citHeader : function (...)  
close.socket : function (socket, ...)  
clrhash : function (h)  
combn : function (x, m, FUN = NULL, simplify = TRUE, ...)  
compareVersion : function (a, b)  
contrib.url : function (repos, type = getOption("pkgType"))  
count.fields : function (file, sep = "", quote = "\"'", skip = 0, blank.lines.skip = TRUE, comment.char = "#")  
create.post : function (instructions = character(), description = "post", subject = "", method = getOption("mailer"), 
address = "the relevant mailing list", ccaddress = getOption("ccaddress", ""), 
filename = "R.post", info = character())  
data : function (..., list = character(), package = NULL, lib.loc = NULL, verbose = getOption("verbose"), 
envir = .GlobalEnv, overwrite = TRUE)  
data.entry : function (..., Modes = NULL, Names = NULL)  
dataentry : function (data, modes)  
de : function (..., Modes = list(), Names = NULL)  
de.ncols : function (inlist)  
de.restore : function (inlist, ncols, coltypes, argnames, args)  
de.setup : function (ilist, list.names, incols)  
debugcall : function (call, once = FALSE)  
debugger : function (dump = last.dump)  
demo : function (topic, package = NULL, lib.loc = NULL, character.only = FALSE, verbose = getOption("verbose"), 
type = c("console", "html"), echo = TRUE, ask = getOption("demo.ask"), encoding = getOption("encoding"))  
DLL.version : function (path)  
download.file : function (url, destfile, method, quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"), 
headers = NULL, ...)  
download.packages : function (pkgs, destdir, available = NULL, repos = getOption("repos"), contriburl = contrib.url(repos, 
type), method, type = getOption("pkgType"), ...)  
dump.frames : function (dumpto = "last.dump", to.file = FALSE, include.GlobalEnv = FALSE)  
edit : function (name, ...)  
emacs : function (name = NULL, file = "")  
example : function (topic, package = NULL, lib.loc = NULL, character.only = FALSE, give.lines = FALSE, 
local = FALSE, type = c("console", "html"), echo = TRUE, verbose = getOption("verbose"), 
setRNG = FALSE, ask = getOption("example.ask"), prompt.prefix = abbreviate(topic, 
6), run.dontrun = FALSE, run.donttest = interactive())  
file.edit : function (...)  
file_test : function (op, x, y)  
fileSnapshot : function (path = ".", file.info = TRUE, timestamp = NULL, md5sum = FALSE, digest = NULL, 
full.names = length(path) > 1, ...)  
Filters :  chr [1:12, 1:2] "R or S files (*.R,*.q,*.ssc,*.S)" "Enhanced metafiles (*.emf)" ...
find : function (what, mode = "any", numeric = FALSE, simple.words = TRUE)  
findCRANmirror : function (type = c("src", "web"))  
findLineNum : function (srcfile, line, nameonly = TRUE, envir = parent.frame(), lastenv)  
fix : function (x, ...)  
fixInNamespace : function (x, ns, pos = -1, envir = as.environment(pos), ...)  
flush.console : function ()  
formatOL : function (x, type = "arabic", offset = 0, start = 1, width = 0.9 * getOption("width"))  
formatUL : function (x, label = "*", offset = 0, width = 0.9 * getOption("width"))  
getAnywhere : function (x)  
getClipboardFormats : function (numeric = FALSE)  
getCRANmirrors : function (all = FALSE, local.only = FALSE)  
getFromNamespace : function (x, ns, pos = -1, envir = as.environment(pos))  
gethash : function (h, key, nomatch = NULL)  
getIdentification : function ()  
getParseData : function (x, includeText = NA)  
getParseText : function (parseData, id)  
getS3method : function (f, class, optional = FALSE, envir = parent.frame())  
getSrcDirectory : function (x, unique = TRUE)  
getSrcFilename : function (x, full.names = FALSE, unique = TRUE)  
getSrcLocation : function (x, which = c("line", "column", "byte", "parse"), first = TRUE)  
getSrcref : function (x)  
getTxtProgressBar : function (pb)  
getWindowsHandle : function (which = "Console")  
getWindowsHandles : function (which = "R", pattern = "", minimized = FALSE)  
getWindowTitle : function ()  
getWinProgressBar : function (pb)  
glob2rx : function (pattern, trim.head = FALSE, trim.tail = TRUE)  
globalVariables : function (names, package, add = TRUE)  
hashtab : function (type = c("identical", "address"), size)  
hasName : function (x, name)  
head : function (x, ...)  
head.matrix : function (x, n = 6L, ...)  
help : function (topic, package = NULL, lib.loc = NULL, verbose = getOption("verbose"), try.all.packages = getOption("help.try.all.packages"), 
help_type = getOption("help_type"))  
help.request : function (...)  
help.search : function (pattern, fields = c("alias", "concept", "title"), apropos, keyword, whatis, 
ignore.case = TRUE, package = NULL, lib.loc = NULL, help.db = getOption("help.db"), 
verbose = getOption("verbose"), rebuild = FALSE, agrep = NULL, use_UTF8 = FALSE, 
types = getOption("help.search.types"))  
help.start : function (update = FALSE, gui = "irrelevant", browser = getOption("browser"), remote = NULL)  
history : function (...)  
hsearch_db : function (package = NULL, lib.loc = NULL, types = getOption("help.search.types"), 
verbose = getOption("verbose"), rebuild = FALSE, use_UTF8 = FALSE)  
hsearch_db_concepts : function (db = hsearch_db())  
hsearch_db_keywords : function (db = hsearch_db())  
install.packages : function (...)  
installed.packages : function (lib.loc = NULL, priority = NULL, noCache = FALSE, fields = NULL, subarch = .Platform$r_arch, 
...)  
is.hashtab : function (x)  
is.relistable : function (x)  
isS3method : function (method, f, class, envir = parent.frame())  
isS3stdGeneric : function (f)  
limitedLabels : function (value, maxwidth = getOption("width") - 5L)  
loadhistory : function (...)  
loadRconsole : function (file = choose.files(file.path(Sys.getenv("R_USER"), "Rconsole")))  
localeToCharset : function (locale = Sys.getlocale("LC_CTYPE"))  
ls.str : function (pos = -1, name, envir, all.names = FALSE, pattern, mode = "any")  
lsf.str : function (pos = -1, envir, ...)  
maintainer : function (pkg)  
make.packages.html : function (lib.loc = .libPaths(), temp = FALSE, verbose = TRUE, docdir = R.home("doc"))  
make.socket : function (host = "localhost", port, fail = TRUE, server = FALSE)  
makeRweaveLatexCodeRunner : function (evalFunc = RweaveEvalWithOpt)  
maphash : function (h, FUN)  
memory.limit : function (size = NA)  
memory.size : function (max = FALSE)  
menu : function (choices, graphics = FALSE, title = NULL)  
methods : function (generic.function, class)  
mirror2html : function (mirrors = NULL, file = "mirrors.html", head = "mirrors-head.html", foot = "mirrors-foot.html")  
modifyList : function (x, val, keep.null = FALSE)  
new.packages : function (lib.loc = NULL, repos = getOption("repos"), contriburl = contrib.url(repos, 
type), instPkgs = installed.packages(lib.loc = lib.loc, ...), method, available = NULL, 
ask = FALSE, ..., type = getOption("pkgType"))  
news : function (query, package = "R", lib.loc = NULL, format = NULL, reader = NULL, db = NULL)  
numhash : function (h)  
object.size : function (x)  
old.packages : function (lib.loc = NULL, repos = getOption("repos"), contriburl = contrib.url(repos, 
type), instPkgs = installed.packages(lib.loc = lib.loc, ...), method, available = NULL, 
checkBuilt = FALSE, ..., type = getOption("pkgType"))  
osVersion :  chr "Windows 10 x64 (build 19044)"
package.skeleton : function (name = "anRpackage", list = character(), environment = .GlobalEnv, path = ".", 
                             force = FALSE, code_files = character(), encoding = "unknown")  
  packageDate : function (pkg, lib.loc = NULL, date.fields = c("Date", "Packaged", "Date/Publication", 
"Built"), tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%D", "%m/%d/%y"), desc = packageDescription(pkg, 
lib.loc = lib.loc, fields = date.fields))  
packageDescription : function (pkg, lib.loc = NULL, fields = NULL, drop = TRUE, encoding = "")  
packageName : function (env = parent.frame())  
packageStatus : function (lib.loc = NULL, repositories = NULL, method, type = getOption("pkgType"), 
...)  
packageVersion : function (pkg, lib.loc = NULL)  
page : function (x, method = c("dput", "print"), ...)  
person : function (given = NULL, family = NULL, middle = NULL, email = NULL, role = NULL, comment = NULL, 
first = NULL, last = NULL)  
personList : function (...)  
pico : function (name = NULL, file = "")  
process.events : function ()  
prompt : function (object, filename = NULL, name = NULL, ...)  
promptData : function (object, filename = NULL, name = NULL)  
promptImport : function (object, filename = NULL, name = NULL, importedFrom = NULL, importPage = name, 
...)  
promptPackage : function (package, lib.loc = NULL, filename = NULL, name = NULL, final = FALSE)  
rc.getOption : function (name)  
rc.options : function (...)  
rc.settings : function (ops, ns, args, dots, func, ipck, S3, data, help, argdb, fuzzy, quotes, files)  
rc.status : function ()  
read.csv : function (file, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", 
...)  
read.csv2 : function (file, header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "", 
...)  
read.delim : function (file, header = TRUE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "", 
...)  
read.delim2 : function (file, header = TRUE, sep = "\t", quote = "\"", dec = ",", fill = TRUE, comment.char = "", 
...)  
read.DIF : function (file, header = FALSE, dec = ".", numerals = c("allow.loss", "warn.loss", 
"no.loss"), row.names, col.names, as.is = !stringsAsFactors, na.strings = "NA", 
colClasses = NA, nrows = -1, skip = 0, check.names = TRUE, blank.lines.skip = TRUE, 
stringsAsFactors = FALSE, transpose = FALSE, fileEncoding = "")  
read.fortran : function (file, format, ..., as.is = TRUE, colClasses = NA)  
read.fwf : function (file, widths, header = FALSE, sep = "\t", skip = 0L, row.names, col.names, 
n = -1L, buffersize = 2000, fileEncoding = "", ...)  
read.socket : function (socket, maxlen = 256L, loop = FALSE)  
read.table : function (file, header = FALSE, sep = "", quote = "\"'", dec = ".", numerals = c("allow.loss", 
"warn.loss", "no.loss"), row.names, col.names, as.is = !stringsAsFactors, na.strings = "NA", 
colClasses = NA, nrows = -1, skip = 0, check.names = TRUE, fill = !blank.lines.skip, 
strip.white = FALSE, blank.lines.skip = TRUE, comment.char = "#", allowEscapes = FALSE, 
flush = FALSE, stringsAsFactors = FALSE, fileEncoding = "", encoding = "unknown", 
text, skipNul = FALSE)  
readCitationFile : function (file, meta = NULL)  
readClipboard : function (format = 1L, raw = FALSE)  
readRegistry : function (key, hive = c("HLM", "HCR", "HCU", "HU", "HCC", "HPD"), maxdepth = 1, view = c("default", 
"32-bit", "64-bit"))  
recover : function ()  
relist : function (flesh, skeleton = attr(flesh, "skeleton"))  
remhash : function (h, key)  
remove.packages : function (...)  
removeSource : function (fn)  
Rprof : function (...)  
Rprofmem : function (filename = "Rprofmem.out", append = FALSE, threshold = 0)  
RShowDoc : function (what, type = c("pdf", "html", "txt"), package)  
RSiteSearch : function (string, restrict = c("functions", "descriptions", "news", "Rfunctions", 
"Rmanuals", "READMEs", "views", "vignettes"), format, sortby = c("score", "date:late", 
"date:early", "subject", "subject:descending", "size", "size:descending"), matchesPerPage = 20, 
words = c("all", "any"))  
rtags : function (path = ".", pattern = "\\.[RrSs]$", recursive = FALSE, src = list.files(path = path, 
pattern = pattern, full.names = TRUE, recursive = recursive), keep.re = NULL, 
ofile = "", append = FALSE, verbose = getOption("verbose"), type = c("etags", 
"ctags"))  
Rtangle : function ()  
RtangleFinish : function (object, error = FALSE)  
RtangleRuncode : function (object, chunk, options)  
RtangleSetup : function (file, syntax, output = NULL, annotate = TRUE, split = FALSE, quiet = FALSE, 
drop.evalFALSE = FALSE, ...)  
RtangleWritedoc : function (object, chunk)  
RweaveChunkPrefix : function (options)  
RweaveEvalWithOpt : function (expr, options)  
RweaveLatex : function ()  
RweaveLatexFinish : function (object, error = FALSE)  
RweaveLatexOptions : function (options)  
RweaveLatexSetup : function (file, syntax, output = NULL, quiet = FALSE, debug = FALSE, stylepath, ...)  
RweaveLatexWritedoc : function (object, chunk)  
RweaveTryStop : function (err, options)  
savehistory : function (...)  
select.list : function (choices, preselect = NULL, multiple = FALSE, title = NULL, graphics = getOption("menu.graphics"))  
sessionInfo : function (package = NULL)  
setBreakpoint : function (srcfile, line, nameonly = TRUE, envir = parent.frame(), lastenv, verbose = TRUE, 
tracer, print = FALSE, clear = FALSE, ...)  
sethash : function (h, key, value)  
setInternet2 : function (use = TRUE)  
setRepositories : function (graphics = getOption("menu.graphics"), ind = NULL, addURLs = character())  
setStatusBar : function (text)  
setTxtProgressBar : function (pb, value, title = NULL, label = NULL)  
setWindowTitle : function (suffix, title = paste(getIdentification(), suffix))  
setWinProgressBar : function (pb, value, title = NULL, label = NULL)  
shortPathName : function (path)  
stack : function (x, ...)  
Stangle : function (file, driver = Rtangle(), syntax = getOption("SweaveSyntax"), encoding = "", 
...)  
str : function (object, ...)  
strcapture : function (pattern, x, proto, perl = FALSE, useBytes = FALSE)  
strOptions : function (strict.width = "no", digits.d = 3L, vec.len = 4L, list.len = 99L, deparse.lines = NULL, 
drop.deparse.attr = TRUE, formatNum = function(x, ...) format(x, trim = TRUE, 
drop0trailing = TRUE, ...))  
summaryRprof : function (filename = "Rprof.out", chunksize = 5000, memory = c("none", "both", "tseries", 
"stats"), lines = c("hide", "show", "both"), index = 2, diff = TRUE, exclude = NULL, 
basenames = 1)  
suppressForeignCheck : function (names, package, add = TRUE)  
Sweave : function (file, driver = RweaveLatex(), syntax = getOption("SweaveSyntax"), encoding = "", 
...)  
SweaveHooks : function (options, run = FALSE, envir = .GlobalEnv)  
SweaveSyntaxLatex : List of 9
 $ doc       : chr "^[[:space:]]*\\\\end\\{Scode\\}"
 $ code      : chr "^[[:space:]]*\\\\begin\\{Scode\\}\\{?([^}]*)\\}?.*"
 $ coderef   : chr "^[[:space:]]*\\\\Scoderef\\{([^}]*)\\}.*"
 $ docopt    : chr "^[[:space:]]*\\\\SweaveOpts\\{([^}]*)\\}"
 $ docexpr   : chr "\\\\Sexpr\\{([^}]*)\\}"
 $ extension : chr "\\.[rsRS]tex$"
 $ syntaxname: chr "^[[:space:]]*\\\\SweaveSyntax\\{([^}]*)\\}"
 $ input     : chr "^[[:space:]]*\\\\SweaveInput\\{([^}]*)\\}"
 $ trans     :List of 8
SweaveSyntaxNoweb : List of 9
 $ doc       : chr "^@"
 $ code      : chr "^<<(.*)>>=.*"
 $ coderef   : chr "^<<(.*)>>.*"
 $ docopt    : chr "^[[:space:]]*\\\\SweaveOpts\\{([^}]*)\\}"
 $ docexpr   : chr "\\\\Sexpr\\{([^}]*)\\}"
 $ extension : chr "\\.[rsRS]?nw$"
 $ syntaxname: chr "^[[:space:]]*\\\\SweaveSyntax\\{([^}]*)\\}"
 $ input     : chr "^[[:space:]]*\\\\SweaveInput\\{([^}]*)\\}"
 $ trans     :List of 8
SweaveSyntConv : function (file, syntax, output = NULL)  
tail : function (x, ...)  
tail.matrix : function (x, n = 6L, keepnums = TRUE, addrownums, ...)  
tar : function (tarfile, files = NULL, compression = c("none", "gzip", "bzip2", "xz"), compression_level = 6, 
tar = Sys.getenv("tar"), extra_flags = "")  
timestamp : function (...)  
toBibtex : function (object, ...)  
toLatex : function (object, ...)  
txtProgressBar : function (min = 0, max = 1, initial = 0, char = "=", width = NA, title, label, style = 1, 
file = "")  
type.convert : function (x, ...)  
typhash : function (h)  
undebugcall : function (call)  
unstack : function (x, ...)  
untar : function (tarfile, files = NULL, list = FALSE, exdir = ".", compressed = NA, extras = NULL, 
verbose = FALSE, restore_times = TRUE, support_old_tars = Sys.getenv("R_SUPPORT_OLD_TARS", 
FALSE), tar = Sys.getenv("TAR"))  
unzip : function (zipfile, files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, 
exdir = ".", unzip = "internal", setTimes = FALSE)  
update.packages : function (lib.loc = NULL, repos = getOption("repos"), contriburl = contrib.url(repos, 
type), method, instlib = NULL, ask = TRUE, available = NULL, oldPkgs = NULL, ..., 
checkBuilt = FALSE, type = getOption("pkgType"))  
upgrade : function (object, ...)  
url.show : function (url, title = url, file = tempfile(), delete.file = TRUE, method, ...)  
URLdecode : function (URL)  
URLencode : function (URL, reserved = FALSE, repeated = FALSE)  
vi : function (name = NULL, file = "")  
View : function (...)  
vignette : function (topic, package = NULL, lib.loc = NULL, all = TRUE)  
warnErrList : function (x, warn = TRUE, errValue = NULL)  
win.version : function ()  
winDialog : function (type = c("ok", "okcancel", "yesno", "yesnocancel"), message)  
winDialogString : function (message, default)  
winMenuAdd : function (...)  
winMenuAddItem : function (...)  
winMenuDel : function (...)  
winMenuDelItem : function (...)  
winMenuItems : function (...)  
winMenuNames : function (...)  
winProgressBar : function (title = "R progress bar", label = "", min = 0, max = 1, initial = 0, width = 300L)  
write.csv : function (...)  
write.csv2 : function (...)  
write.socket : function (socket, string)  
write.table : function (x, file = "", append = FALSE, quote = TRUE, sep = " ", eol = "\n", na = "NA", 
dec = ".", row.names = TRUE, col.names = TRUE, qmethod = c("escape", "double"), 
fileEncoding = "")  
writeClipboard : function (str, format = 1L)  
xedit : function (name = NULL, file = "")  
xemacs : function (name = NULL, file = "")  
zip : function (zipfile, files, flags = "-r9X", extras = "", zip = Sys.getenv("R_ZIPCMD", "zip"))  
zip.unpack : function (zipname, dest)  

library(wikifacts)
wiki_define     : function (term = NULL, sentences = 5L)  
wiki_didyouknow : function (n_facts = 1L, date = sample(seq(as.Date("2015-01-01"), Sys.Date() - 1, by = "day"), 1), bare_fact = FALSE)  
wiki_inthenews  : function (n_facts = 1L, date = sample(seq(as.Date("2015-01-01"), Sys.Date() - 1, by = "day"), 1), bare_fact = FALSE)  
wiki_onthisday  : function (n_facts = 1L, date = sample(seq(as.Date("2015-01-01"), Sys.Date() - 1, by = "day"), 1), bare_fact = FALSE)  
wiki_query      : function (qry)  
wiki_randomfact : function (n_facts = 1L, fact = c("any", "didyouknow", "onthisday", "inthenews"), bare_fact = FALSE, repeats = TRUE)  
wiki_search     : function (term = NULL, browser = getOption("browser"))
  
  list = list(a,b,c)
ab <- " "
a <- "a"
b <- "b"
c <- "c"
ab <- paste(ab,a,sep = "|") a1 <- capture.output(as.character(writeLines(stacker(list = list1))))

list <- c("wiki_define","wiki_didyouknow")wikifacts::

list1 <- capture.output(writeLines(ls.str("package:base", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:beepr", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
# list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
# list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
# list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
# list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
# list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
# list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
# list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
# list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
# list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
# list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
# list1 <- capture.output(writeLines(ls.str("package:stats", all.names = TRUE)),split = TRUE)
list1 <- capture.output(writeLines(ls.str("package:microbenchmark", all.names = TRUE)),split = TRUE)

  
stacker <- function(list) {
   for (i in list){
  ab <- ""
  a <- writeLines(as.character(getAnywhere(i)))
  ab <- paste(ab,a,sep = "|")
  
 }
}
list <- list("wiki_define",
             "wiki_didyouknow","wiki_inthenews","wiki_onthisday","wiki_query","wiki_randomfact","wiki_search")

library(wikifacts)
stacker(list)

invisblelist <- print(ls("package:stats"))

ls.str("package:dplyr")
str("package:dplyr")


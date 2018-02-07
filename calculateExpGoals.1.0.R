### Install (if not available already) required libraries ###

if (!'data.table' %in% installed.packages()) {install.packages('data.table')}
if (!'ggplot2' %in% installed.packages()) {install.packages('ggplot2')}
if (!'scales' %in% installed.packages()) {install.packages('scales')}

### Load required libraries ###

library(data.table)
library(RPostgreSQL)
library(ggplot2)
library(scales)
library(boot)

#### Database connection #### 

source('dbConnect.R')

#### Custom variables #### 



#### Custom function ####

# cross-join 

CJ.table <- function(X,Y){
    setkey(X[,c(k=1,.SD)],k)[Y[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]}

# Get coefficients

getCoeff <- function(data.set, indices){
    single.mod <- glm(Goal ~ degree_angle + goaldist + Header + Penalty_attack + 0
                      , family = binomial(link = logit), data = data.set[indices, ])
    coef(single.mod)
}

# Get bootstrap interval for each variable

GetBootInterval <- function(index){
    boot.ci(coeff.bootstrap, index = index, type = 'perc')$percent[4:5]
}

#### 1. Data Input ####

#### 1.1 Working data ####

strQuery <- scan(paste('./data/in/Imp.Shots.txt', sep = ''), character(0), quote = NULL, sep = "\n")
Imp.Shots <- data.table(dbGetQuery(con, paste(strQuery, collapse = ' ', sep = '')))

#### 1.2 Manual data ####

#### 2. Data wrangling ####

Imp.Shots[type == 'Goal', Goal := 1]
Imp.Shots[is.na(Goal), Goal := 0]

Imp.Shots[is.na(attacktype), attacktype := 'Positional attack']

Proc.Shots <- cbind(Imp.Shots, model.matrix(~ attacktype - 1, Imp.Shots)); Proc.Shots[, attacktype := NULL]
Proc.Shots <- cbind(Proc.Shots, model.matrix(~ bodypart - 1, Proc.Shots)); Proc.Shots[, bodypart := NULL]
setnames(Proc.Shots, gsub('attacktype', '', gsub('bodypart', '', gsub(' ', '_', gsub('-', '_', names(Proc.Shots))))))
Proc.Shots[, ':='(Foot = as.factor(Left_foot + Right_foot),
                  Corner_attack = as.factor(Corner_attack),
                  Counter_attack = as.factor(Counter_attack),
                  Free_kick_attack = as.factor(Free_kick_attack),
                  Penalty_attack = as.factor(Penalty_attack),
                  Positional_attack = as.factor(Positional_attack),
                  Body = as.factor(Body),
                  Header = as.factor(Header))]

#### 3. Modelling ####

# Standard single model

glm.mod <- glm(Goal ~ degree_angle + goaldist + Header + Penalty_attack + 0
                  , family = binomial(link = logit), data = Proc.Shots)

# Parametric intervals

std.interval <- confint(glm.mod)

# Bootstrap intervals

coeff.bootstrap <- boot(Proc.Shots, getCoeff, R = 1000)
bootstrap.interval <- sapply(1:nrow(std.interval), GetBootInterval)

#### 4. Viz ####


#### 4. Data Export ####


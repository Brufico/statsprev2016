#' ---
#' title: 'Sonia Cosmetique'
#' author: Bruno Fischer Colonimos
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' output:
#'         pdf_document:
#'             number_sections: yes
#'             latex_engine: pdflatex
#' urlcolor: blue
#' fontsize: 11pt
#' geometry: top= 0.8in, left= 0.8in, right= 0.8in, bottom = 0.8in, footskip = 0.3in
#'---
#'

#+ libs, include = FALSE
library(knitr)
library(dplyr)
library(ggplot2)
library(GGally)
library(caret)


#' Données: lecture et partitionnement
#' ===================================


#+ data, echo = FALSE
datadir <- "data"
dname  <- "SoniaCosmetique.csv"
scos <- read.csv(file.path(datadir, dname),
                 header = TRUE,
                 # colClasses = c(NULL, numeric, numeric, numeric, numeric),
                 row.names = 1,
                 sep = ";",
                 comment.char = "",
                 na.strings = ""
                 )
colnames(scos) <- c("appreciation", "texture", "parfum", "presentation")

proportiontest <- 0.60
set.seed(222)
inTrain <- createDataPartition(y=scos$appreciation,
                               p=proportiontest, list=FALSE)
training <- scos[inTrain,]
testing <- scos[-inTrain,]

dimtrain <- dim(training)

#' les données sont lues et partitionnées en un
#' échantillon d'apprentissage et un échantillon de test (proportion test = `r proportiontest`)
#' dimension de l'échantillon d'apprentissage: `r dimtrain[1]`



#'
#' Exploration
#' ===========
#'
pairs()



#'
#' choix du Modele et training
#' ===========================
# start
m1 <- lm(appreciation ~., data = training)
summary(m1)

#elim presentation
m2 <- lm(appreciation ~ texture + parfum , data = training)
summary(m2)



# diagnostic
# ==========

#+ def, echo = FALSE
plotmod <- function(lmodel, which) {
        # set size
        if (missing(which)) {
                dimgr <-  c(2,2)
        }else if (length(which) > 3){
                dimgr <-  c(2,2)
        }else if (length(which) < 2){
                dimgr <-  c(1,1)
        } else {
                dimgr <-  c(1,2)
        }
        # make the title
        first <- as.character(formula(lmodel))[2]
        last <- as.character(formula(lmodel))[3]
        title <- paste(first, "~", last)
        # plot
        op <- par("mfrow")
        par(mfrow = dimgr)
        if (missing(which)) {
                plot(lmodel)
        }else {
                plot(lmodel, which)
        }
        mtext( title , side = 3, line = -1.5, outer = TRUE)
        par(mfrow = op)
}

#+ diagmod, fig.height = 3
plotmod(m2, which = 1:2)

#' le modèle semble ok.


#+ eq, echo = FALSE

# function to make math equation
matheq <- function(modl, data, meanround = 0, digits=3, eqlimit = "$$", centered = FALSE) {
        dgts <- digits
        responsevar <- as.character(formula(modl))[2]
        variables <- attributes(modl$terms)$term.labels
        cf <- coefficients(modl)
        data <-  data[variables]
        moys <- sapply(data, mean)
        rmoys <- round(moys, meanround)

        if (centered){
                sprod <- sum(cf[-1] * rmoys)
                itc <- cf[1] + sprod
        }else {
                itc <- cf[1]
        }

        varform <- sapply((1:length(variables)),
                       function(i) {
                               if(centered) {
                                     strng <- paste0(format(cf[i+1], digits = dgts),
                                                     " \\cdot ",
                                                     "(", variables[i], " - ",  rmoys[i], ")")
                               }else {
                                       strng <- paste0(format(cf[i+1], digits = dgts),
                                                       " \\cdot ",
                                                       variables[i])
                               }
                               if (i == length(variables)) {
                                       paste0(strng, eqlimit)
                               }else {strng}
                       } )

        headform <- paste0(eqlimit, responsevar, " = ", format(itc,digits = dgts))
        eq  <-  paste(c(headform , varform), collapse =" + ")
        eq
}


eq1 <- matheq(m2, training, centered = FALSE)
eq2 <- matheq(m2, training, centered = TRUE)



#' **Equation**:
#' `r eq1`
#'Ou bien
#' `r eq2`

#+ withcaret, echo=FALSE
modelFit <- train(appreciation ~ texture + parfum , data=training, method="lm")
modelFit


#'
#' Prédiction
#' ===========

#' application a l'échantillon de test:

#+ apptest, echo = FALSE
# review in-sample
predictions <- predict(modelFit,newdata=training)
mn <- mean(training$appreciation, na.rm = TRUE)
InRMSE <- sqrt(sum((training$appreciation - predictions)^2)/(nrow(training)))
InsampleR2 <- 1 - sum((training$appreciation - predictions)^2)/sum((training$appreciation - mn)^2)

# compute out-of-sample
predictions <- predict(modelFit,newdata=testing)
mn <- mean(testing$appreciation, na.rm = TRUE)
OutRMSE <- sqrt(sum((testing$appreciation - predictions)^2)/(nrow(testing)))
OutsampleR2 <- 1 - sum((testing$appreciation - predictions)^2)/sum((testing$appreciation - mn)^2)

#' In-sample $R^2$ = `r InsampleR2` , Out-of-sample $R^2$ = `r OutsampleR2`
#' In-sample RMSE = `r InRMSE` , Out-of-sample RMSE = `r OutRMSE`

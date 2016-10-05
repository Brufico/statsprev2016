#' ---
#' title: 'Sonia Mode'
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


#' Données: lecture
#' ===================================

#+ data, echo = FALSE
datadir <- "data"
dname  <- "SoniaMode.csv"
smod <- read.csv(file.path(datadir, dname),
                 header = TRUE,
                 # colClasses = c(NULL, numeric, numeric, numeric, numeric),
                 row.names = 1,
                 sep = ";",
                 comment.char = "",
                 na.strings = ""
)
colnames(smod)
colnames(smod) <- c("machat", "ristourne", "age", "anciennete", "paiement")

#' Données: exploration
#' ===================================

ggpairs(data = smod)

#'
#' choix du Modele et training
#' ===========================
# start
m1 <- lm(machat ~., data = smod)
summary(m1)

#elim paiementvisa
m2 <- lm(machat ~ ristourne + age + anciennete , data = smod)
summary(m2)

#elim paiementvisa
m3 <- lm(machat ~ ristourne + age  , data = smod)
summary(m3)


#elim paiementvisa
m4 <- lm(machat ~ ristourne  , data = smod)
summary(m4)



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
                dimgr <-  c(1,3)
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
plotmod(m4, which = 1:2)


#' addition de paiement

m4 <- lm(machat ~ ristourne + paiement , data = smod)
summary(m4)

m5 <- lm(machat ~ ristourne + anciennete , data = smod)
summary(m5)

m6 <- lm(machat ~ ristourne + anciennete + paiement, data = smod)
summary(m6)# non

m7 <- lm(machat ~ ristourne + anciennete + age, data = smod)
summary(m7)# non

anova(m5,m7) # non


#' on choisit m5. Diag
plotmod(m5, which = 1:3)

# equation
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


eq1 <- matheq(m5, smod, centered = FALSE)
eq2 <- matheq(m5, smod, centered = TRUE)

#' Equation de régression:
#' `r eq1`
#' ou
#' `r eq2`

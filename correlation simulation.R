#' ---
#' title: 'corrélation accidentelle'
#' author: Bruno Fischer Colonimos
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' output:
#'         pdf_document:
#'             number_sections: yes
#'             latex_engine: lualatex
#' urlcolor: blue
#' fontsize: 11pt
#' geometry: top= 0.8in, left= 0.8in, right= 0.8in, bottom = 0.8in, footskip = 0.3in
#'---


# Simulation

set.seed(655)
n <- 10 # nombre de points de chaque série

B <- 1000 #nombre de simulations

#' Samples
samp <- runif(n = n * B, min = 0, max = 1)
length(samp)
samp <- as.data.frame(matrix(samp, nrow=n))
dim(samp)

samp2 <- runif(n = n * B, min = 0, max = 1)
length(samp2)
samp2 <- as.data.frame(matrix(samp2, nrow=n))

dim(samp)
dim(samp2)

corij <- function(i , j) {cor(samp[[i]], samp2[[j]])}

lcor <- sapply(1:B,
               function(i){
                       sapply(1:B,
                              function(j) {corij(i,j)})
               } )
lvcor <- as.vector(lcor)

# proportion de corrélations > 0.95
crit <- 0.99
p <- mean(lvcor> crit)
# nombre de corrélations > 0.95
s <- sum(lvcor> crit)
max(lvcor)

#' ---
#' title: 'Ajustement + corrélation Sonia 5.1'
#' author: Bruno Fischer Colonimos
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' output:
#'         pdf_document:
#'             number_sections: yes
#' urlcolor: blue
#' fontsize: 11pt
#' geometry: top= 0.8in, left= 0.8in, right= 0.8in, bottom = 0.8in, footskip = 0.3in
#'---

# libraries

#+ libs, include = FALSE

## load libraries
library(knitr)
library(dplyr)
library(ggplot2)


#'
#' Data
#' =====
#'


dt <- read.table(
        text =
"ventes_produit prix_produit prix_produit_concurrent budget_pub_produit
7.38 6.42 6.33 9.17
8.51 6.25 6.67 11.25
9.52 6.17 7.17 12.08
7.50 6.17 6.17 9.17
9.33 6.00 6.42 11.67
8.28 6.00 6.33 10.83
8.75 6.00 6.25 11.25
7.87 6.33 6.42 8.75
7.10 6.33 6.08 8.75
8.00 6.42 6.67 10.00"
, sep = " ", header = TRUE)



#'
#' Graphe
#'
#+ pairs, fig.width = 8, fig.height = 4, echo = FALSE

ps <- function(x, y) {panel.smooth(x, y, col.smooth = "blue", span = 1)}
pairs(dt,panel = ps)




#'
#' Matrice de corrélations
#' =======================
#'

#+ correlmat, echo = FALSE
MC <- cor(dt)
diag(MC) <- NA
kable(MC, digits = 2)

#' **conclusion**:  La variable la plus corrélée aux ventes est budget.pub.produit. Toutefois:
#'
#' * prix.produit est négativement corrélée aux ventes  et prix.produit.concurrent est positivement corrélée aux ventes
#' * Lest deux variables sont fortement corrélées au budget pub...



#'
#' Pentes des régressions simples
#' ============================
#'
#+ regsimples, echo = FALSE
coefsimple <- function(vname) {
        fmla <- as.formula(paste0("ventes_produit", "~", vname) )
        cf <- lm(fmla, data=dt)$coefficients
        cf[2]
}


dcsimple <- as.data.frame(lapply(FUN=coefsimple, X = colnames(dt)[-1] ))
colnames(dcsimple) <- colnames(dt)[-1]
rownames(dcsimple) <- c("coefficient")

kable(dcsimple, digits = 2)

#' interprétation des pentes==> ...
#'



#'
#' régressions multiple
#' ====================
#'
#' modele 1 : avec toutes les variables
#' ------------------------------------

mod1 <- lm(ventes_produit ~ prix_produit + prix_produit_concurrent + budget_pub_produit, data = dt)
summary(mod1)

#' **==> Aucun des coefficients n'est significativement =/= 0**
#'

#' Backwards elimination :
#' -----------------------

#' ### directement: modèle 2=sans  budget.pub.produit
#'
mod2 <- lm(ventes_produit ~ prix_produit + prix_produit_concurrent, data = dt)
summary(mod2)


# definition
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

#' Le modèles est ok:
#'
#' **Diagnostics plots**

#+Dplots, echo = FALSE, fig.height = 3
plotmod(mod2, which=c(1,2))


#+coefeq, echo = FALSE,
cf  <- round(coefficients(mod2),2)
# equation
eq <- paste0("$" , "Ventes = ", cf[1] , " + " , cf[2] ," \\cdot ", "prix_produit", " + " ,
             cf[3] , " \\cdot ", "prix_produit_concurrent", "$")


#' **Conclusion1**: on trouve l'équation suivante: `r eq`
#'
#' $$Ventes = `r cf[1]` + `r cf[2]` \cdot prix.produit + `r cf[3]` \cdot prix.produit.concurrent $$
#'

#+ calcmoy, echo = FALSE
moys <- sapply(X = dt, mean)
moys <- round(moys, 2)

#' ==> interprétation avec changement d'origine vers les valeurs moyennes des prédicteurs
#+ calcmoy2, echo = FALSE
ncf <- c(cf[1] + cf[2] * moys[2] + cf[3] * moys[3],
         cf[2], cf[3])

#' $$Ventes = `r ncf[1]` + `r cf[2]` \cdot (prix_produit - `r moys[2]`) + `r cf[3]` \cdot (prix_produit_concurrent - `r moys[3]`)$$





#'
#' Autre élimination: Si on commence par supprimer prix_produit.concurrent
#' --------------------------------------------------------------------
#'
mod3 <- lm(ventes_produit ~ prix_produit + budget_pub_produit, data = dt)
summary(mod3)

#' Plus-prix non significatif ==> elimination du prix:

mod3X <- lm(ventes_produit ~ budget_pub_produit, data = dt)
summary(mod3X)
plotmod(mod3X)

cf  <- round(coefficients(mod3X),2)

eq <- paste0("Ventes = ", cf[1] , " + " , cf[2] , "budget_pub_produit")

#' **Conclusion2**: on trouve l'équation suivante:  **`r eq`**
#'
#' ==> interprétation




#'
#'
#' Une autre approche (TD ajustement non linéaire)
#' ===============================================

#' **transformation**: variable --> log(variable)

dt2 <- mutate(dt, lvente = log10(ventes_produit),
              lprix = log10(prix_produit),
              lprixconc = log10(prix_produit_concurrent),
              lpub = log10(budget_pub_produit)) %>%
        select(lvente, lprix, lprixconc, lpub)

#' avec tous les prédicteurs
modB0 <- lm(lvente ~ lprix + lprixconc + lpub, data = dt2)
summary(modB0)
plotmod(modB0)




# apres elimination de lpub
#+ modB1
modB1 <- lm(lvente ~ lprix + lprixconc , data = dt2)
summary(modB1)
plotmod(modB1)

#+ modB1coeffs, echo = FALSE
cf <- coefficients(modB1)
ecf <- exp(cf)
eq <- paste0("Ventes = ", format(ecf[1], digits = 2), " x " , "prix_produit^", round(cf[2],2),  " x " ,
             "prix_produit_concurrent^", round(cf[3],2) )

#'
#' **Conclusion3**: on trouve l'équation suivante:  **`r eq`**
#'
#' $$Ventes = `r format(ecf[1], digits = 2)` \cdot prix_produit^{`r round(cf[2],2)`} \cdot prix_produit_concurrent^{`r round(cf[3],2)`}$$
#'

#' moyennes
moy <- round(apply(dt, 2, mean),2)
moy

#' ==> interprétation avec changement d'origine: transformer l'équation avec Prix/prix moyen... etc





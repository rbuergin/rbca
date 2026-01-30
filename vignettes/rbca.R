## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup--------------------------------------------------------------------

library(rbca)



## ----example table, echo = FALSE----------------------------------------------
library(knitr)
library(kableExtra)



# Create table data
fare <- c("$1.55", "$1.85", "$2.15")
df <- data.frame(
  `Fare per trip` = fare,
  `10 min` = c(100, 92, 75),
  `20 min` = c(95, 85, 70),
  `30 min` = c(80, 60, 50),
  check.names = FALSE
)

# Build the table with a vertical line after column 1
kable(df, format = "html", align = "c", escape = FALSE) %>%
  add_header_above(c(" " = 1, "Average waiting time" = 3)) %>%
  column_spec(1, border_right = TRUE) %>% 
  kable_styling(full_width = FALSE, position = "center")


## ----functions table, echo = FALSE--------------------------------------------
# Example data frame
function.table <- data.frame(
  Function = c("[rbcam()](#rbcam)", "[rbcam.coef()](#rbcam.coef)", "[rbcam.pwu()](#rbcam.pwu)",
               "[rbcam.pwu.plot()](#rbcam.pwu.plot)", "[rbcam.rimp()](#rbcam.rimp)",
               "[rbcam.rimp.plot()](#rbcam.rimp.plot)", "[rbcam.predict()](#rbcam.predict)",
               "[rbcam.msep()](#rbcam.msep)", "[rbcam.emsep()](#rbcam.emsep)",
               "[rbcam.rsquared()](#rbcam.rsquared)", "[rbcam.kmeans()](#rbcam.kmeans)",
               "[rbcam.kmeans.withinss()](#rbcam.kmeans.withinss)",
               "[rbcam.lrtest()](#rbcam.lrtest)", "[rbcam.step()](#rbcam.step)"),
  Description = c("Model function",
                  "Extracts model coefficients",
                  "part-worth utility functions",
                  "Visualises the part-worth utility functions",
                  "Calculates RIMP of the attributes",
                  "Visualises the RIMP",
                  "Prediction of new ratings",
                  "Mean-Squared-Error (MSE) for validation data",
                  "expected MSE of predictions",
                  "(adjusted) $R^2$ of the model",
                  "Cluster analysis of part-worth utility functions",
                  "Quadratic sum of residuals within the cluster for definitions of number of clusters",
                  "Compares models with the likelihood ratio test",
                  "Stepwise model selection process")
)

kable(function.table, caption = "")


## ----data example-------------------------------------------------------------
data(tea)
head(tea)


## ----rbcam--------------------------------------------------------------------
tea.m <- rbcam(
  formula = rating ~ price + variety + kind + aroma,
  data = tea,
  var.level.1 = "respondent",
  var.level.2 = NULL,
  contrasts.factor = "contr.treatment")


## ----rbcam.coef---------------------------------------------------------------
rbcam.coef(object = tea.m, level = 3)

rbcam.coef(object = tea.m, level = 1)[1:5,]


## ----rbcam.pwu----------------------------------------------------------------
rbcam.pwu(object = tea.m, level = 3)

rbcam.pwu(object = tea.m, level = 1)$price[1:5,]


## ----rbcam.pwu.plot, warning = FALSE, fig.width=7-----------------------------
rbcam.pwu.plot(object = tea.m, term = "price", normalized = TRUE, show.level.1 = TRUE)
# with jitter
rbcam.pwu.plot(object = tea.m, term = "price", normalized = TRUE, show.level.1 = TRUE,
             jitter = TRUE, jitter.width = 0.5)
# adding random groups for each respondent
set.seed(123)
tea$group <- as.factor(rep(sample(1:3, size = length(unique(tea$respondent)),replace = TRUE),
                           each = 13))

tea.group.m <- rbcam(
  formula = rating ~ price + variety + kind + aroma,
  data = tea,
  var.level.2 = "group")
rbcam.pwu.plot(object = tea.group.m, term = "kind", show.level.2 = TRUE, label.level.2 = TRUE)



## ----rbcam.rimp---------------------------------------------------------------
rbcam.rimp(object = tea.m, level = 3)

rbcam.rimp(object = tea.m, level = 1)[1:5,]


## ----rbcam.rimp.plot, warning = FALSE, fig.width=7----------------------------
rbcam.rimp.plot(object = tea.m, show.level.1 = TRUE)


## ----rbcam.predict------------------------------------------------------------
newdata <- data.frame(price = "medium", variety = "red", kind = "bags", aroma = "no")

rbcam.predict(object = tea.m, newdata = newdata)


## ----rbcam.msep---------------------------------------------------------------
tea.m.w1 <- rbcam(
  formula = rating ~ price + variety + kind + aroma,
  data = tea[tea$profile != "1", ],
  var.level.1 = "respondent",
  contrasts.factor = "contr.treatment")

rbcam.msep(object.list = tea.m.w1, newdata = tea[tea$profile == "1", ])


## ----rbcam.emsep--------------------------------------------------------------
tea.m.price <- rbcam(
  formula = rating ~ price,
  var.level.1 = "respondent",
  data = tea)
tea.m.variety <- rbcam(
  formula = rating ~ variety,
  var.level.1 = "respondent",
  data = tea)
tea.m.kind <- rbcam(
  formula = rating ~ kind,
  var.level.1 = "respondent",
  data = tea)
tea.m.aroma <- rbcam(
  formula = rating ~ aroma,
  var.level.1 = "respondent",
  data = tea)
tea.m.int <- rbcam(
  formula = rating ~ price + variety + kind + aroma + variety * kind,
  data = tea,
  var.level.1 = "respondent")
emsep <- rbcam.emsep(
  object.list = list(
    "IA, only price" = tea.m.price,
    "IA, only variety" = tea.m.variety,
    "IA, only kind" = tea.m.kind,
    "IA, only aroma" = tea.m.aroma,
    "IA, additive" = tea.m,
    "IA, additive + variety * kind" = tea.m.int),
  object.ref = tea.m.int,
  level = 1)
emsep


## ----rbcam.rsquared-----------------------------------------------------------
rbcam.rsquared(object = tea.m, level = 1, adjusted = FALSE)

rbcam.rsquared(object = tea.m, level = 1)


## ----rbcam.kmeans-------------------------------------------------------------
tea.kmeans <- rbcam.kmeans(object = tea.m, centers = 3)
tea.kmeans$cluster


## ----rbcam.kmeans.withinss, warning = FALSE, fig.width=7, results='hide'------
km.withinss <- rbcam.kmeans.withinss(object = tea.m, centers.seq = 1:40)


## ----rbcam.kmeans.withinss2---------------------------------------------------
# Compute kmeans with the defined 5 centers
tea.kmeans.opt <- rbcam.kmeans(object = tea.m, centers = 5)

tea.kmeans.opt$cluster


## ----rbcam.kmeans.add---------------------------------------------------------
tea$cluster <- factor(x = factor(tea.kmeans.opt$cluster[as.integer(tea$respondent)]))
table(tea$cluster)
tea.m.ca <- rbcam(
  formula = rating ~ price + variety + kind + aroma,
  data = tea,
  var.level.1 = "respondent",
  var.level.2 = "cluster",
  contrasts.factor = "contr.treatment")


## ----rbcam.kmeans.add.pwu, warning = FALSE, fig.width=7-----------------------
rbcam.pwu.plot(
  object = tea.m.ca, term = "price",
  show.level.1 = TRUE, show.level.2 = TRUE)
rbcam.pwu.plot(
  object = tea.m.ca, term = "variety",
  show.level.1 = TRUE, show.level.2 = TRUE)
rbcam.pwu.plot(
  object = tea.m.ca, term = "kind",
  show.level.1 = TRUE, show.level.2 = TRUE)
rbcam.pwu.plot(
  object = tea.m.ca, term = "aroma",
  show.level.1 = TRUE, show.level.2 = TRUE)


## ----rbcam.lrtest-------------------------------------------------------------
# model without kind
tea.m2 <- rbcam(
  formula = rating ~ price + variety + aroma,
  data = tea,
  var.level.1 = "respondent",
  var.level.2 = NULL,
  contrasts.factor = "contr.treatment")

rbcam.lrtest(object1 = tea.m2, object2 = tea.m)


## ----rbcam.step---------------------------------------------------------------
tea.m <- rbcam(
  formula = rating ~ price + variety + kind + aroma,
  data = tea)

tea.m0 <- rbcam(
  formula = rating ~ kind,
  data = tea)

# Stepwise selection using both directions
rbcam.step(object = tea.m, scope = formula(rating ~ price + variety + kind + aroma +
                                           kind:aroma))

# Backward direction
rbcam.step(object = tea.m)

# Forward direction with scope
rbcam.step(object = tea.m0, scope = formula(tea.m$mlist.level.3), direction = "forward")


## ----rbcam.step.emsep---------------------------------------------------------
# Backward direction with criterion emsep
rbcam.step(tea.m, direction = "backward", criterion = "emsep")

# Forward direction with scope
rbcam.step(tea.m0, scope = formula(rating ~ price + variety + kind + aroma),
         direction = "forward", criterion = "emsep")


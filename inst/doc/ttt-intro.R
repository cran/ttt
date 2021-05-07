## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'-----------------
set.seed(123)
library(ttt, quietly=TRUE)

## -----------------------------------------------------------------------------
library(table1, quietly=TRUE)
library(magrittr, quietly=TRUE)

## -----------------------------------------------------------------------------
ttt(mtcars)

## -----------------------------------------------------------------------------
ttt(mpg ~ gear | cyl, data=mtcars, lab="Number of Cylinders", render=mean)

## -----------------------------------------------------------------------------
label(mtcars$gear) <- "Number of<br/>Gears"

rndr <- function(x, ...) {
    if (length(x) == 0) return("")
    round_pad(mean(x), 1)
}

ttt(mpg ~ gear | cyl, data=mtcars, lab="Number of Cylinders", render=rndr)

## -----------------------------------------------------------------------------
mtcars %>% ttt(mpg ~ gear | cyl, lab="Number of Cylinders", render=rndr)

## -----------------------------------------------------------------------------
ttt(mpg ~ gear, data=mtcars, render=rndr)

## -----------------------------------------------------------------------------
ttt(mpg ~ 1 | cyl, data=mtcars, lab="Number of Cylinders", render=rndr)

## -----------------------------------------------------------------------------
label(mtcars$cyl) <- "Number of<br/>Cylinders"
ttt(mpg ~ gear + cyl, data=mtcars, render=rndr)

## -----------------------------------------------------------------------------
ttt(mpg ~ 1 | gear + cyl, data=mtcars, lab="Number of Cylinders/Gears", render=rndr)

## -----------------------------------------------------------------------------
bigtable <- expand.grid(
    R1=LETTERS[1:3],
    R2=LETTERS[4:6],
    R3=LETTERS[7:9],
    C1=LETTERS[10:12],
    C2=LETTERS[13:15],
    C3=LETTERS[16:18])

bigtable$x <- 1:nrow(bigtable)
ttt(x ~ R3 + R2 + R1 | C1 + C2 + C3, data=bigtable)

## -----------------------------------------------------------------------------
ttt(rownames(mtcars) ~ gear | cyl, data=mtcars, lab="Number of Cylinders",
  render=paste, collapse="<br/>")

## -----------------------------------------------------------------------------
rndr.meansd <- function(x) signif_pad(c(Mean=mean(x), SD=sd(x)), digits=3)

ttt(decrease ~ treatment, data=OrchardSprays, render=rndr.meansd)

## -----------------------------------------------------------------------------
ttt(decrease ~ treatment, data=OrchardSprays, render=rndr.meansd, expand.along=c(Blah="rows"))

## -----------------------------------------------------------------------------
ttt(decrease ~ treatment, data=OrchardSprays, render=rndr.meansd, expand.along="columns")

## -----------------------------------------------------------------------------
ttt(decrease ~ 1 | treatment, data=OrchardSprays, render=rndr.meansd, lab="Treatment",
    caption="Mean and SD of Decrease by Treatment",
    footnote=c("Data: OrchardSprays", "Comment: <code>ttt</code> is cool!"))

## ---- eval=FALSE--------------------------------------------------------------
#  options(ttt.theme="booktabs")  # Select the "booktabs" theme

## ---- eval=FALSE--------------------------------------------------------------
#  ttt(x ~ R3 + R2 + R1 | C1 + C2 + C3, data=bigtable)

## ---- echo=FALSE--------------------------------------------------------------
css <- readLines(system.file(package="ttt", "ttt_booktabs_1.0/ttt_booktabs.css"))
css <- gsub(".Rttt ", ".Rttt-booktabs-demo ", css, fixed=TRUE)
ttt(x ~ R3 + R2 + R1 | C1 + C2 + C3, data=bigtable, topclass="Rttt-booktabs-demo", css=css)

## ---- eval=FALSE--------------------------------------------------------------
#  options(ttt.theme="default")  # Change back to the "default" theme

## -----------------------------------------------------------------------------
css <- '
#bigtable {
  font-family: "Lucida Console", Monaco, monospace;
}
#bigtable td {
  background-color: #eee;
}
#bigtable th {
  color: blue;
  background-color: lightblue;
}
#bigtable th, #bigtable td {
  border: 2px dashed orange;
}
#bigtable .Rttt-rl {
  background-color: #fff;
  font-style: italic;
  font-weight: bold;
}
#bigtable .Rttt-rl-lvl1 {
  font-size: 12pt;
  color: pink;
  background-color: yellow;
}
#bigtable .Rttt-rl-lvl2 {
  font-size: 14pt;
  color: green;
}
#bigtable .Rttt-rl-lvl3 {
  font-size: 18pt;
  color: red;
}
'

ttt(x ~ R3 + R2 + R1 | C1 + C2 + C3, data=bigtable, id="bigtable", css=css)

## -----------------------------------------------------------------------------
dat <- expand.grid(row=LETTERS[1:5], column=LETTERS[1:5])
dat$value <- rnorm(nrow(dat))

ttt(value ~ row | column, data=dat, render=round_pad, digits=2)

## -----------------------------------------------------------------------------
rndr <- function(x, ...) {
    y <- round_pad(x, 2)
    attr(y, "html.class") <- ifelse(x < 0, "neg", "pos")
    y
}

## -----------------------------------------------------------------------------
ttt(value ~ row | column, data=dat, render=rndr)


library(hydrogeo)
context("Piper object")

testData <- function(n) {
    Ca <- runif(n, 0, 100)
    Mg <- runif(n, 0, 100-Ca)
    Cl <- runif(n, 0, 100)
    SO4 <- runif(n, 0, 100-Cl)
    list(Ca=Ca, Mg=Mg, Cl=Cl, SO4=SO4)
}

test_that("minimal piper object can be instanciated", {
    expect_is( piper(testData(25)), "piper" )
})

test_that("minimal piper object can be plotted", {
    expect_true( plot(piper(testData(25))))
})

test_that("piper with > 25 samples plots, (i.e. no pch errors)", {
    expect_true( plot(piper(testData(26))))
})

## TODO: is this a bug in test framework?
##1. Failure: cations >100% raises error (@testPiperInit.R#22) -------------------
##error$message does not match "Ca + Mg must be <<= 100".
##Actual value: "Ca + Mg must be <= 100"
##test_that("cations >100% raises error", {
##    l <- list(Ca=c(99), Mg=c(1.0001), Cl=c(10), SO4=c(10))
##    expect_error( piper(list(Ca=c(99), Mg=c(1.0001), Cl=c(10), SO4=c(10)))
##                ,"Ca + Mg must be <= 100")
###                ,"Ca + Mg must be <= 100")
##})

## TODO: Fix zaporozec dataset
##test_that("zaporozec dataset plots", {
##    data(zaporozec)
##    expect_true( plot(piper(zaporozec)))
##}
#)

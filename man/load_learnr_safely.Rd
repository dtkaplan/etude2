% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_learnr_safely.R
\name{load_learnr_safely}
\alias{load_learnr_safely}
\title{Load the learnr package safely}
\usage{
load_learnr_safely()
}
\description{
When rendering `{learnr}` tutorials, we generally want to
load the `{learnr}` package. But if we are rendering the document
as ordinary HTML or PDF, `library('learnr')` will throw an error.
This function allows `{learnr}` to be loaded in appropriate circumstances
and not otherwise. NOTE: It does **not** load `{learnr}` if it's not safe.
}

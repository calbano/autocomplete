#!/usr/bin/env Rscript

library(argparse)
source('autocomplete.R')

parser <- ArgumentParser()
parser$add_argument("query", nargs = 1, help = "Prefix to search")
parser$add_argument("file", nargs = 1, help = "File to be searched")
parser$add_argument("n", type = 'integer', nargs = 1, help = "Prefix to search")

args <- parser$parse_args()
filename <- read_terms(args$file)

autocomplete(args$query, filename, args$n)
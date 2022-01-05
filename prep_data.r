#!/usr/bin/env Rscript --vanilla

# This script prepares data for downstream analyses for the COVID-19 Animal 
# Movement Project. It is intended to be the first step in the workflow after
# construction of the database.  
#
# Major tasks accomplished:
#   * Annotate events with study periods (lockdown, pre lockdown, etc.)
#   * Basic track cleaning (remove outliers, etc.)
#   * Write new table back to db with clean data from relevant periods only
#
# TODO: everything.
# TODO: Correct docopts/setup

# ==== Setup ====

'
Join annotations to movement data.
Usage:
join_annos.r <db> <out> <ctf> [-b]
join_annos.r (-h | --help)
Parameters:
  dat: path to input csv file. 
  out: path to output directory.
  ctf: path to control file
Options:
-h --help     Show this screen.
-v --version     Show version.
-b --rollback   Rollback transaction if set to true.
' -> doc

#---- Input Parameters ----#
if(interactive()) {
  library(here)
  
  .wd <- '~/projects/niche_scratch/'
  .rollback <- TRUE
  rd <- here::here
  
  .outPF <- file.path(.wd,'analysis/cranes_anno.csv')
  .dbPF <- file.path(.wd,'data/anno_move.db')
  .ctfPF <- file.path(.wd, "analysis/cranes/ctfs/anno_vars.csv")
  
} else {
  library(docopt)
  library(rprojroot)
  
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .rollback <- as.logical(ag$rollback)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  source(rd('src/funs/input_parse.r'))
  
  .outPF <- makePath(ag$out)
  .dbPF <- makePath(ag$db)
  .ctfPF <- makePath(ag$ctf)
}

#---- Initialize Environment ----#
t0 <- Sys.time()

source(rd('src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
  }))
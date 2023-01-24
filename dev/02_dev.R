# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "stockDas", with_test = TRUE) # stockDas é o módulo para apresentar o dashboard do estoque no body (corpo) Início
golem::add_module(name = "cadastroDash", with_test = TRUE) # cadastroDas é o módulo do Dashboard no tab inicio body que apresenta os botões de cadastros (fornecedor, fazenda, ração, alevino)
golem::add_module(name = "tabInicio", with_test = TRUE) # Módulo para aba início
golem::add_module(name = "tabInicio", with_test = TRUE) # Módulo para aba início

## Add helper functions ----
## Creates fct_* and utils_*
### fct
golem::add_fct("tabInicio", with_test = TRUE) # Tab (aba) início
golem::add_fct("cadastroDash", with_test = TRUE) # Dashboard de cadastros (fornecedor, ração alevino, fazenda) presente no tabInicio body
golem::add_fct("tabFornecedor", with_test = TRUE) # Tab (aba) Cadastro fornecedor
golem::add_fct("tabRacao", with_test = TRUE) # Tab (aba) Ração
golem::add_fct("tabAlevino", with_test = TRUE) # Tab (aba) Cadastro Alevino
golem::add_fct("tabFazenda", with_test = TRUE) # Tab (aba) Cadastro Fazenda

### utils
golem::add_utils("db", with_test = TRUE) # Pequenas funções de conexão do Banco de dados (DataBase -db)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("AdmRacao")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

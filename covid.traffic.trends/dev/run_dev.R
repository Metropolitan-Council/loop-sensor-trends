# Set options here
options(shiny.launch.browser = TRUE,
        scipen = 9999,
        golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
# golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

rmarkdown::render("./ABOUT.Rmd", output_format = "github_document", # -----
                  output_file = "ABOUT.md", output_dir = "./inst/app/www",
                  params = list(actor_id = "roten",
                                data_date = "2020-03-26",
                                sha = system("git rev-parse --short HEAD",
                                             intern=TRUE)))


# Document and reload your package ----
golem::document_and_reload()

# Run the application
run_app()

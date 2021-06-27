library(rmarkdown)

# HTML
# rmarkdown::render("thesis/01_introduction.Rmd", output_format = "html_document")
rmarkdown::render("thesis/02_literature_review.Rmd", output_format = "html_document")
rmarkdown::render("thesis/03_dataset_description.Rmd", output_format = "html_document")
rmarkdown::render("thesis/04_methods_description.Rmd", output_format = "html_document")
rmarkdown::render("thesis/05_results.Rmd", output_format = "html_document")
# rmarkdown::render("thesis/06_summary.Rmd", output_format = "html_document")

# WORD
# rmarkdown::render("thesis/01_introduction.Rmd", output_format = "word_document")
rmarkdown::render("thesis/02_literature_review.Rmd", output_format = "word_document")
rmarkdown::render("thesis/03_dataset_description.Rmd", output_format = "word_document")
rmarkdown::render("thesis/04_methods_description.Rmd", output_format = "word_document")
rmarkdown::render("thesis/05_results.Rmd", output_format = "word_document")
# rmarkdown::render("thesis/06_summary.Rmd", output_format = "word_document")
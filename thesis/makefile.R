library(rmarkdown)

include_code <- FALSE


rmarkdown::render("thesis/00_thesis.Rmd",output_dir = 'thesis/versions/', output_file = 'thesis_init.docx', params = list(echo=include_code))

x# HTML
# rmarkdown::render("thesis/01_introduction.Rmd", output_format = "html_document", params = list(echo=include_code))
rmarkdown::render("thesis/02_literature_review.Rmd", output_format = "html_document", params = list(echo=include_code))
rmarkdown::render("thesis/03_dataset_description.Rmd", output_format = "html_document", params = list(echo=include_code))
rmarkdown::render("thesis/04_methods_description.Rmd", output_format = "html_document", params = list(echo=include_code))
rmarkdown::render("thesis/05_results.Rmd", output_format = "html_document", params = list(echo=include_code))
# rmarkdown::render("thesis/06_summary.Rmd", output_format = "html_document", params = list(echo=include_code))

# WORD
# rmarkdown::render("thesis/01_introduction.Rmd", output_format = "word_document", params = list(echo=include_code))
rmarkdown::render("thesis/02_literature_review.Rmd", output_format = "word_document", params = list(echo=include_code))
rmarkdown::render("thesis/03_dataset_description.Rmd", output_format = "word_document", params = list(echo=include_code))
rmarkdown::render("thesis/04_methods_description.Rmd", output_format = "word_document", params = list(echo=include_code))
rmarkdown::render("thesis/05_results.Rmd", output_format = "word_document", params = list(echo=include_code))
# rmarkdown::render("thesis/06_summary.Rmd", output_format = "word_document", params = list(echo=include_code))
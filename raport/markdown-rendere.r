
library(rmarkdown)

convert_to_docx <- function(filename) {
    rmarkdown::render(filename, output_format = 'word_document')
    print("word document generated")
}

convert_to_html <- function(filename) {
    rmarkdown::render(filename, output_format = 'html_document')
    print("html document generated")
}


convert_all <- function(filename) {
    convert_to_docx(filename)
    convert_to_html(filename)
}


filename <- "./raport.Rmd"
convert_all(filename)

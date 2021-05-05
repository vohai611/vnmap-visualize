# render company's revenue and labor force report --------------------------------
rmarkdown::render("Analyzing-vn-companu-labor.Rmd",
                  output_file = "final-result/Analyzing-company-labor",envir = new.env())
markdown::rpubsUpload(title = "first temp upload",
                      htmlFile = "final-result/Analyzing-company-labor.html")



# render education report -------------------------------------------------

rmarkdown::render("Vietnam-education-report/Vietnam-education-report.Rmd",
                  output_file = here("final-result/education-report"), envir = new.env())


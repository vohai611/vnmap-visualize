###render final result
rmarkdown::render("Analyzing-vn-companu-labor.Rmd",output_file = "final-result/Analyzing-company-labor")
markdown::rpubsUpload(title = "first temp upload",
                      htmlFile = "final-result/Analyzing-company-labor.html")
#

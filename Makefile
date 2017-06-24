#!usr/bin/make -f
# All commands are run as R functions rather than shell commands so that it will work easily on any Windows machine, even if the Windows machine isn't properly set up with all the right tools

all: README.md

clean:
	Rscript -e 'suppressWarnings(file.remove("README.md", "vignettes/ggExtra.md"))'

.PHONY: all clean
.DELETE_ON_ERROR:
.SECONDARY:

README.md : vignettes/ggExtra.Rmd
#	echo "Rendering the ggExtra vignette"
	Rscript -e 'rmarkdown::render("vignettes/ggExtra.Rmd", output_format = "md_document")'
#	echo "Correcting image paths"
#	sed -i -- 's,../inst,inst,g' vignettes/ggExtra.md
	Rscript -e 'file <- gsub("\\.\\./inst", "inst", readLines("vignettes/ggExtra.md")); writeLines(file, "vignettes/ggExtra.md")'
	Rscript -e 'dir.create("inst/vignette_files/", showWarnings = FALSE);file.copy("vignettes/ggExtra_files/", "inst/vignette_files/", overwrite = TRUE, recursive = TRUE)'
	Rscript -e 'file <- gsub("ggExtra_files", "inst/vignette_files/ggExtra_files", readLines("vignettes/ggExtra.md")); writeLines(file, "vignettes/ggExtra.md")'
#	echo "Copying output to README.md"
#	cp vignettes/ggExtra.md README.md
	Rscript -e 'file.copy("vignettes/ggExtra.md", "README.md", overwrite = TRUE)'
	Rscript -e 'suppressWarnings(file.remove("vignettes/ggExtra.md"))'

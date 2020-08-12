OUTPUTDIR=public
SSH_TARGET=cloud:/home/andrew/sites/econf20.classes/public_html

.PHONY : all clean serve build deploy thumb_slides

all:  thumb_slides build


# Automatic thumbnails ----------------------------------------------------
TO_THUMB = $(wildcard static/slides/*.pdf)
THUMB_TARGETS = $(addsuffix .png,$(basename $(TO_THUMB)))

%.png: %.pdf
	convert -thumbnail 1000 -background white -units PixelsPerInch -density 144 $<[0] $@

thumb_slides: $(THUMB_TARGETS)


# Site building -----------------------------------------------------------
clean:
	rm -rf public/

build: thumb_slides
	Rscript -e "blogdown::build_site()"

serve: build
	Rscript -e "blogdown::serve_site(port=4321)"

deploy: build
	rsync -Prvzc --exclude='.DS_Store' --exclude='.Rproj.user/' --delete $(OUTPUTDIR)/ $(SSH_TARGET)

.PHONY: build
default: build

clean:
	rm -r ../posts ../res ../about.html ../archive.html ../index.html

build:
	emacsclient --eval "(blog-publish nil t)"

rebuild:
	emacsclient --eval "(blog-publish t t)"

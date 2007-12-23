
.PHONY : docs

docs : README.html

%.html : %
	pandoc -s -S --toc -c hinotify.css $< -o $@

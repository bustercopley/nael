info:
	emacs --batch \
		"--eval=(require 'ox-texinfo)" \
		"--eval=(find-file \"README.org\")" \
		"--eval=(org-texinfo-export-to-info)"
	rm nael.texi

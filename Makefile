SRC = mode-info.el mi-util.el mi-index.el mi-elisp.el mi-perl.el mi-libc.el mi-ruby.el

EMACS = emacs
FLAGS = -q -no-site-file -batch -eval '(setq mode-info-index-directory "'`pwd`'" load-path (cons "'`pwd`'" load-path))'

# EMACS = mule
# FLAGS = -q -no-site-file -batch -eval '(setq mode-info-index-directory "'`pwd`'" load-path (cons "'`pwd`'" (cons "/usr/share/mule/19.34/site-lisp/apel" load-path)))'

.SUFFIXES: .elc .el

.el.elc:
	$(EMACS) $(FLAGS) -f batch-byte-compile $*.el

default: $(SRC:.el=.elc)

clean:
	rm -f *.elc *~

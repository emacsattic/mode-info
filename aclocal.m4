AC_DEFUN(AC_EMACS_LISP, [
elisp="$2"
if test -z "$3"; then
	AC_MSG_CHECKING(for $1)
fi
AC_CACHE_VAL(EMACS_cv_SYS_$1,[
	OUTPUT=./conftest-$$
	echo ${EMACS}' -batch -eval '\''(let ((x '"${elisp}"')) (write-region (if (stringp x) (princ x) (prin1-to-string x)) nil "'${OUTPUT}'" nil 5))'\' >& AC_FD_CC 2>&1
	eval ${EMACS}' -batch -eval '\''(let ((x '"${elisp}"')) (write-region (if (stringp x) (princ x) (prin1-to-string x)) nil "'${OUTPUT}'" nil 5))'\' >& AC_FD_CC 2>&1
	retval="`cat ${OUTPUT}`"
	echo "=> ${retval}" >& AC_FD_CC 2>&1
	rm -f ${OUTPUT}
	EMACS_cv_SYS_$1="${retval}"
])
$1="${EMACS_cv_SYS_$1}"
if test -z "$3"; then
	AC_MSG_RESULT($$1)
fi
])

AC_DEFUN(AC_PATH_EMACS,
 [dnl Check for Emacsen.

  dnl Apparently, if you run a shell window in Emacs, it sets the EMACS
  dnl environment variable to 't'.  Lets undo the damage.
  test x${EMACS} = xt && EMACS=

  dnl Ignore cache.
  unset ac_cv_prog_EMACS; unset EMACS_cv_SYS_flavor;

  AC_ARG_WITH(emacs,
   [  --with-emacs=EMACS      compile with EMACS [EMACS=emacs, xemacs, mule...]],
   [if test "${withval}" = yes -o -z "${withval}"; then
      AC_PATH_PROGS(EMACS, emacs xemacs mule, emacs)
    else
      AC_PATH_PROG(EMACS, ${withval}, ${withval}, emacs)
    fi])
  AC_ARG_WITH(xemacs,
   [  --with-xemacs=XEMACS    compile with XEMACS [XEMACS=xemacs]],
   [if test x$withval = xyes -o x$withval = x; then
      AC_PATH_PROG(EMACS, xemacs, xemacs, xemacs)
    else
      AC_PATH_PROG(EMACS, $withval, $withval, xemacs)
    fi])
  test -z "${EMACS}" && AC_PATH_PROGS(EMACS, emacs xemacs mule, emacs)
  AC_SUBST(EMACS)
  
  AC_MSG_CHECKING([what a flavor does ${EMACS} have])
  AC_EMACS_LISP(flavor,
    (cond ((featurep (quote xemacs)) \"XEmacs\")\
          ((and (boundp (quote emacs-major-version))\
                (>= emacs-major-version 21))\
           \"FSF Emacs 21\")\
          ((boundp (quote MULE)) \"MULE\")\
          (t \"FSF Emacs\")),
    noecho)
  case "${flavor}" in
  XEmacs)
    EMACS_FLAVOR=xemacs;;
  MULE)
    EMACS_FLAVOR=mule;;
  "FSF Emacs 21")
    EMACS_FLAVOR=emacs21;;
  *)
    EMACS_FLAVOR=emacs;;
  esac
  AC_MSG_RESULT(${flavor})])

AC_DEFUN(AC_PATH_LISPDIR, [
  if test ${EMACS_FLAVOR} = emacs21; then
	tribe=emacs
  else
	tribe=${EMACS_FLAVOR}
  fi
  if test ${prefix} = NONE; then
	AC_MSG_CHECKING([prefix for ${EMACS}])
	AC_EMACS_LISP(prefix,(expand-file-name \"..\" invocation-directory),noecho)
	prefix=${EMACS_cv_SYS_prefix}
	AC_MSG_RESULT(${prefix})
  fi
  AC_ARG_WITH(lispdir,
    [  --with-lispdir=DIR      where lisp files should go],
    lispdir="${withval}")
  AC_MSG_CHECKING([where lisp files should go])
  if test -z "${lispdir}"; then
    dnl Set the default value.
    theprefix=${prefix}
    if test x${theprefix} = xNONE; then
	theprefix=${ac_default_prefix}
    fi
    lispdir="\$(datadir)/${tribe}/site-lisp/mode-info"
    for thedir in share lib; do
	potential=
	if test -d ${theprefix}/${thedir}/${tribe}/site-lisp; then
	   lispdir="\$(prefix)/${thedir}/${tribe}/site-lisp/mode-info"
	   break
	fi
    done
  fi
  AC_MSG_RESULT(${lispdir})
  AC_SUBST(lispdir)])

AC_DEFUN(AC_PATH_INDEXDIR,
 [dnl Examin index directory.

  dnl Ignore cache.
  unset EMACS_cv_SYS_indexdir;

  if test ${EMACS_FLAVOR} = xemacs -o ${EMACS_FLAVOR} = emacs21; then
    AC_ARG_WITH(indexdir,
     [  --with-indexdir=DIR     directory for indices [\$(data-directory)/mode-info]],
      INDEXDIR="${withval}")
    AC_MSG_CHECKING([where index files should go])
    if test -z "${INDEXDIR}"; then
      dnl Set the default value.
      AC_EMACS_LISP(indexdir,
        (let ((prefix \"${prefix}\")\
	      (default (expand-file-name \"mode-info\" data-directory)))\
	  (if (and prefix\
		   (progn\
		     (setq prefix (file-name-as-directory prefix))\
		     (eq 0 (string-match (regexp-quote prefix) default))))\
	      (replace-match \"\$(prefix)/\" nil nil default)\
	    default)),
	${prefix},noecho)
      INDEXDIR=${EMACS_cv_SYS_indexdir}
    fi
    AC_MSG_RESULT(${INDEXDIR})
  else
    INDEXDIR=NONE
  fi
  AC_SUBST(INDEXDIR)])

AC_DEFUN(AC_ADD_LOAD_PATH,
 [dnl Check for additional load path.
  AC_ARG_WITH(addpath,
   [  --with-addpath=PATHs    specify additional PATHs for load-path
                          use colons to separate directory names],
   [AC_MSG_CHECKING([where to find the additional elisp libraries])
      ADDITIONAL_LOAD_PATH="${withval}"
      AC_MSG_RESULT(${ADDITIONAL_LOAD_PATH})],
    ADDITIONAL_LOAD_PATH=NONE)
  AC_SUBST(ADDITIONAL_LOAD_PATH)])

AC_DEFUN(AC_ADD_INFO_PATH,
 [dnl Check for additional Info path.
  AC_ARG_WITH(info-addpath,
   [  --with-info-addpath=PATHs
                          specify additional PATHs for Info
                          use colons to separate directory names],
   [AC_MSG_CHECKING([where to find the additional elisp libraries])
      ADDITIONAL_INFO_PATH="${withval}"
      AC_MSG_RESULT(${ADDITIONAL_INFO_PATH})],
    ADDITIONAL_INFO_PATH=NONE)
  AC_SUBST(ADDITIONAL_INFO_PATH)])

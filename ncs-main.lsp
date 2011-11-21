(vl-load-com)

;;; <LISPDOC>
;;; <SUBR>(main-initialize-app)</SUBR>
;;; <DESC>Initialize main application</DESC>
;;; <ARG>reinit - reinitialize config (bool)</ARG>
;;; <RET>path to config and blacklist file as list (("CONFIG" config) ("BLACKLIST" blacklist))</RET>
;;; </LISPDOC>
(defun main-initialize-app (reinit / )
  (if reinit
    (file-reinitialize-config)
    (file-initialize-config)))

;;; <LISPDOC>
;;; <SUBR>(main-str-list-from-string regexp_object str init_list)</SUBR>
;;; <DESC>Get standards from string</DESC>
;;; <ARG>regexp_object - VBScript.Regexp pointer</ARG>
;;; <ARG>str - string to search</ARG>
;;; <ARG>init_list - initialization list from (main-initialize-app) subroutine</ARG>
;;; <RET>list of found standard strings in ("INDEX" "NUMBER") format</RET>
;;; </LISPDOC>
(defun main-std-list-from-string (regexp_object str init_list / config regexp stdlist)
  (setq config (file-read-config (file-get init_list "CONFIG"))
	regexp (file-get-value config "RegExp")
	stdlist (regexp-execute-to-plain-list regexp_object regexp str)
	whitelist (file-read-list (file-get init_list "WHITELIST")))  
  (if stdlist
    (mapcar
      (function
	(lambda (x)
	  (string-make-pair
	    (string-remove-with-whitelist-total x whitelist))))
      stdlist)))

;;; <LISPDOC>
;;; <SUBR>(main-std-list-clean garbage_list init_list)</SUBR>
;;; <DESC>Remove blacklist values from list</DESC>
;;; <ARG>garbage_list - list to clean ("INDEX" "NUMBER")</ARG>
;;; <ARG>init_list - initialization list from (main-initialize-app) subroutine</ARG>
;;; <RET>cleared list</RET>
;;; </LISPDOC>
(defun main-std-list-clean (garbage_list init_list / cleaned_list blacklist config)
  (setq blacklist (file-read-list (file-get init_list "BLACKLIST"))
        config (file-read-config (file-get init_list "CONFIG")))
  (list-remove-duplicates
    (if (file-get-value config "BlackListExactMatch")
      (vl-remove-if
	   (function
	     (lambda (x)
	       (member (car x) blacklist)))
	   garbage_list)
      (vl-remove-if
        (function
          (lambda (garbage)
            (vl-member-if
              (function
                (lambda (bool)
                  (= bool T)))
              (mapcar
                (function
                  (lambda (black)
                    (wcmatch (strcat (car garbage) (cadr garbage)) (strcat "*" black "*"))))blacklist))))
        garbage_list))))
	   
;;; <LISPDOC>
;;; <SUBR>(main-std-complete-list init_list)</SUBR>
;;; <DESC>Get standards from entire drawing database</DESC>
;;; <ARG>init_list - initialization list from (main-initialize-app) subroutine</ARG>
;;; <RET>list of found standard strings in ("INDEX" "NUMBER") format</RET>
;;; </LISPDOC>
(defun main-std-complete-list (init_list / regexp stdlist templist str)
  (setq regexp (regexp-regapp))
  (foreach str (acad-extract-all-text regexp)
    (if (setq templist (main-std-list-from-string regexp str init_list))
      (setq stdlist (append templist stdlist))))
  (main-std-list-clean stdlist init_list))

;;; <LISPDOC>
;;; <SUBR>(main-std-check standard)</SUBR>
;;; <DESC>Check standard in NormaCS database using API</DESC>
;;; <ARG>standard - standard structure as ("INDEX" "NUMBER")</ARG>
;;; <RET>list of found docs as ("Standard" NormaCS.Application.Document)</RET>
;;; </LISPDOC>
(defun main-std-check (standard / ncs ndocs)
  (setq ncs (ncs-find-object (ncs-regapp)))
  (ncs-find-fill-attr ncs 'Index (car standard))
  (ncs-find-fill-attr ncs 'Number (cadr standard))
  (setq ndocs (list (list-join-to-string standard " ") (ncs-execute-find ncs))))

;;; <LISPDOC>
;;; <SUBR>(main-std-complete-check init_list)</SUBR>
;;; <DESC>List of all found documents</DESC>
;;; <ARG>init_list - initialization list from (main-initialize-app) subroutine</ARG>
;;; <RET>list of found standards as NormaCS-ACAD structure (see (ncs-doc-structure))</RET>
;;; </LISPDOC>
(defun main-std-complete-check (init_list / stdlist stdstr std)
  (foreach std (main-std-complete-list init_list)
    (setq stdstr (main-std-check std))
    (if (cadr stdstr)
      (foreach doc (cadr stdstr)
	(setq stdlist (append (list (ncs-doc-structure (car stdstr) doc)) stdlist)))
      (setq stdlist (append (list (ncs-doc-nil-structure (car stdstr))) stdlist ))))
  stdlist)

;;; <LISPDOC>
;;; <SUBR>(main-html-tr std)</SUBR>
;;; <DESC>Prepare html in-tr string</DESC>
;;; <ARG>std - NormaCS-ACAD structure</ARG>
;;; <RET>HTML TR string</RET>
;;; </LISPDOC>
(defun main-html-tr (std / class test_value outstring)
  (setq class (if (ncs-doc-structure-item "IsActual" std) "new" "old")
        outstring
         (strcat
           (html-table-td
             (ncs-doc-structure-item "DWGName" std) class)
           (if (setq test_value (ncs-doc-structure-item "FoundName" std))
             (html-table-td
               (html-a-href test_value (ncs-doc-structure-item "URL" std)) class)
             (html-table-td "Не найден" class))
           (html-table-td
             (ncs-isactual-to-string std) class)
           (if (car(setq test_value (ncs-doc-structure-item "Replaced" std)))
             (html-table-td
               (ncs-replacement-href-list-to-string
                 (ncs-replacement-to-href-list test_value)) class)
             (html-table-td "&nbsp;" class))))
  (html-table-tr outstring))

;;; <LISPDOC>
;;; <SUBR>(main-html-report stdlist)</SUBR>
;;; <DESC>Create html report file</DESC>
;;; <ARG>stdlist - NormaCS-ACAD structured list</ARG>
;;; <RET>full filepath</RET>
;;; </LISPDOC>
(defun main-html-report (stdlist / fh filename)
  (setq fh (open (setq filename (file-mktemp)) "w"))
  (write-line (html-header (acad-dwgname)) fh)
  (write-line (html-table-start) fh)
  (foreach item stdlist
    (write-line (main-html-tr item) fh))
  (write-line (html-table-finish) fh)
  (write-line (html-footer) fh)
  (close fh)
  filename)

;;; <LISPDOC>
;;; <SUBR>(main-ncs-acad-check)</SUBR>
;;; <DESC>Main subroutine \
;;; Search for standards, check them and make html report</DESC>
;;; <RET>nil</RET>
;;; </LISPDOC>
(defun main-ncs-acad-check ( / file shell)
  (setq file
         (main-html-report
           (main-std-complete-check
             (main-initialize-app nil)))
        shell (file-shell-regapp))
  (file-shell-execute shell file)
  nil)

;;; <LISPDOC>
;;; <SUBR>(main-about)</SUBR>
;;; <DESC>print about info</DESC>
;;; <RET>Nothing</RET>
;;; </LISPDOC>
(defun main-about ()
  (princ "\n==============================================\n")
  (princ "| NormaCS Standard Checker                   |\n")
  (princ "| Aleksei Teplykh <teplykhak@gmail.com> 2011 |\n")
  (princ "|                                            |\n")
  (princ "| C:NCS-ACAD-CHECK                           |\n")
  (princ "==============================================\n")
  (princ))

;;; <LISPDOC>
;;; <SUBR>C:NCS-ACAD-CHECK</SUBR>
;;; <DESC>Register main sub as a command</DESC>
;;; <RET>Nothing</RET>
;;; </LISPDOC>
(defun C:NCS-ACAD-CHECK ()
  (main-ncs-acad-check))

(main-about)
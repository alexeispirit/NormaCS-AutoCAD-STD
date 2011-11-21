;;; <LISPDOC>
;;; <SUBR>(ncs-regapp)</SUBR>
;;; <DESC>Get NormaCS.Application pointer</DESC>
;;; <RET>NormaCS.Application pointer</RET>
;;; </LISPDOC>
(defun ncs-regapp ( / )
  (vla-getinterfaceobject (vlax-get-acad-object) "NormaCS.Application"))

;;; <LISPDOC>
;;; <SUBR>(ncs-find-object ncsapp)</SUBR>
;;; <DESC>Get Application.Find pointer</DESC>
;;; <ARG>ncsapp	- NormaCS.Application pointer</ARG>
;;; <RET>Application.Find pointer</RET>
;;; </LISPDOC>
(defun ncs-find-object (ncsapp)
  (if ncsapp
    (vlax-get ncsapp 'Find)))

;;; <LISPDOC>
;;; <SUBR>(ncs-find-fill-attr ncs_find attr value)</SUBR>
;;; <DESC>Fill Application.Find object with attribute</DESC>
;;; <ARG>ncs_find - Application.Find pointer</ARG>
;;; <ARG>attr - Attribute to fill</ARG>
;;; <ARG>value - Value to fill attribute</ARG>
;;; <RET>nil</RET>
;;; </LISPDOC>
(defun ncs-find-fill-attr (ncs_find attr value)
  (if ncs_find
    (vl-catch-all-apply 'vlax-put (list ncs_find attr value))))

;;; <LISPDOC>
;;; <SUBR>(ncs-execute-find ncs_find)</SUBR>
;;; <DESC>Execute Find object and reset it</DESC>
;;; <ARG>ncs_find - Application.Find pointer</ARG>
;;; <RET>list of Documents</RET>
;;; </LISPDOC>
(defun ncs-execute-find (ncs_find / docs_list)
  (if ncs_find
    (progn
      (vlax-invoke ncs_find 'Execute)
      (vlax-for item (vlax-get ncs_find 'Documents)
	(setq docs_list (cons item docs_list)))      
      (vlax-invoke ncs_find 'Reset)))
  (if docs_list docs_list))

;;; <LISPDOC>
;;; <SUBR>(ncs-doc-isactual doc)</SUBR>
;;; <DESC>Test Document actuality</DESC>
;;; <ARG>doc - Application.Document to test</ARG>
;;; <RET>T if actual \
;;; nil otherwise</RET>
;;; </LISPDOC>
(defun ncs-doc-isactual (doc / isactual)
  (if doc
    (setq isactual (vl-catch-all-apply 'vlax-get (list doc 'IsActual))))
  (if (and isactual (/= isactual acFalse))
    T
    nil))

;;; <LISPDOC>
;;; <SUBR>(ncs-doc-replacement doc)</SUBR>
;;; <DESC>Find Document replacements</DESC>
;;; <ARG>doc - Application.Document</ARG>
;;; <RET>list of Documents</RET>
;;; </LISPDOC>
(defun ncs-doc-replacement (doc / replacements doc_list)
  (if doc
    (setq replacements (vl-catch-all-apply 'vlax-get (list doc 'ReplacedBy))))
  (while (/= 0 (vla-get-count replacements))
    (vlax-for item replacements
      (setq doc_list (cons (vlax-get item 'Document) doc_list)))
    (setq replacements (vl-catch-all-apply 'vlax-get (list (car doc_list) 'ReplacedBy))))
  doc_list)
	    
;;; <LISPDOC>
;;; <SUBR>(ncs-doc-url-structure doc)</SUBR>
;;; <DESC>Make structure Designation - URL</DESC>
;;; <ARG>doc - Application.Document</ARG>
;;; <RET>list ("Designation" "URL")</RET>
;;; </LISPDOC>
(defun ncs-doc-url-structure (doc)
  (if doc
    (list
      (vlax-get doc 'Designation)
      (vlax-get doc 'URL))))

;;; <LISPDOC>
;;; <SUBR>(ncs-doc-structure standard doc)</SUBR>
;;; <DESC>Make document structure</DESC>
;;; <ARG>standard - string of standard found in drawing</ARG>
;;; <ARG>doc - Application.Document</ARG>
;;; <RET>list as NormaCS-ACAD structure \
;;; (("DWGName" ...) \
;;;  ("FoundName" ...) \
;;;  ("IsActual" ...) \
;;;  ("Replaced" (("Designation" "URL")))
;;;  ("URL" ...))</RET>
;;; </LISPDOC>
(defun ncs-doc-structure (standard doc)
  (list
    (list "DWGName" standard)
    (list "FoundName" (vlax-get doc 'Designation))
    (list "IsActual" (ncs-doc-isactual doc))
    (list "URL" (vlax-get doc 'URL))
    (list "Replaced"
	      (mapcar
		(function
		  (lambda (item)
		    (ncs-doc-url-structure item)))
		(ncs-doc-replacement doc)))))

;;; <LISPDOC>
;;; <SUBR>(ncs-doc-nil-structure standard)</SUBR>
;;; <DESC>Make document structure for not found document</DESC>
;;; <ARG>standard - string of standard found in drawing</ARG>
;;; <RET>list as NormaCS-ACAD structure \
;;; (("DWGName" ...) \
;;;  ("FoundName" ...) \
;;;  ("IsActual" ...) \
;;;  ("Replaced" (("Designation" "URL")))
;;;  ("URL" ...))</RET>
;;; </LISPDOC>
(defun ncs-doc-nil-structure (standard)
  (list
    (list "DWGName" standard)
    (list "FoundName" nil)
    (list "IsActual" nil)
    (list "URL" nil)
    (list "Replaced" (list nil))))

;;; <LISPDOC>
;;; <SUBR>(ncs-doc-structure-item key id)</SUBR>
;;; <DESC>Get NormaCS-ACAD structure item</DESC>
;;; <ARG>std - NormaCS-ACAD structure</ARG>
;;; <ARG>key - item to assoc</ARG>
;;; <RET>value by key</RET>
;;; </LISPDOC>
(defun ncs-doc-structure-item (key std)
  (cadr (assoc key std)))

;;; <LISPDOC>
;;; <SUBR>(ncs-is-actual-to-string std)</SUBR>
;;; <DESC>Turn IsActual attribute to string</DESC>
;;; <ARG>std - NormaCS-ACAD structure</ARG>
;;; <RET>"No" - if actual \
;;;	"Yes" - otherwise</RET>
;;; </LISPDOC>
(defun ncs-isactual-to-string (std)
  (if (ncs-doc-structure-item "IsActual" std)
    "Да" "Нет"))

;;; <LISPDOC>
;;; <SUBR>(ncs-replacement-to-html-href doc_url)</SUBR>
;;; <DESC>Prepare replacement html url</DESC>
;;; <ARG>doc_url - ncs-doc-url-structure</ARG>
;;; <RET>href string</RET>
;;; </LISPDOC>
(defun ncs-replacement-to-html-href (doc_url)
  (if doc_url
    (html-a-href (car doc_url) (cadr doc_url))))

;;; <LISPDOC>
;;; <SUBR>(ncs-replacement-to-href-list replist)</SUBR>
;;; <DESC>Prepare replacement list of hrefs</DESC>
;;; <ARG>replist - replacement list</ARG>
;;; <RET>list of hrefs</RET>
;;; </LISPDOC>
(defun ncs-replacement-to-href-list (replist / replacement hreflist)
  (if replist
    (foreach replacement replist
      (setq hreflist (append (list (ncs-replacement-to-html-href replacement)) hreflist)))))

;;; <LISPDOC>
;;; <SUBR>(ncs-replacement-href-to-string hreflist)</SUBR>
;;; <DESC>Convert replacement href list to string</DESC>
;;; <ARG>hreflist - href list</ARG>
;;; <RET>string of joined hrefs</RET>
;;; </LISPDOC>
(defun ncs-replacement-href-list-to-string (hreflist)
  (list-join-to-string hreflist ","))
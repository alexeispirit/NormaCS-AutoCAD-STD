(vl-load-com) ;remove after main

;;; <LISPDOC>
;;; <SUBR>(acad-object)</SUBR>
;;; <DESC>Get AutoCAD.Application object pointer</DESC>
;;; <RET>AutoCAD.Application pointer</RET>
;;; </LISPDOC>
(defun acad-object ()
  (vlax-get-acad-object))

;;; <LISPDOC>
;;; <SUBR>(acad-actdoc)</SUBR>
;;; <DESC>Get ActiveDocument pointer</DESC>
;;; <RET>ActiveDocument pointer</RET>
;;; </LISPDOC>
(defun acad-actdoc ()
  (vla-get-activedocument (acad-object)))

;;; <LISPDOC>
;;; <SUBR>(acad-dwgname)</SUBR>
;;; <DESC>Get activedocument drawing name</DESC>
;;; <RET>Current document name</RET>
;;; </LISPDOC>
(defun acad-dwgname ()
  (vlax-get (acad-actdoc) 'Name))

;;; <LISPDOC>
;;; <SUBR>(acad-dump)</SUBR>
;;; <DESC>Dump vla-object</DESC>
;;; <RET>nil</RET>
;;; </LISPDOC>
(defun acad-dump (vla_object)
  (vlax-dump-object vla_object T))

;;; <LISPDOC>
;;; <SUBR>(acad-vlasel)</SUBR>
;;; <DESC>Get vla-object from entsel</DESC>
;;; <RET>vla-object</RET>
;;; </LISPDOC>
(defun acad-vlasel ()
  (vlax-ename->vla-object (car (entsel))))

;;; <LISPDOC>
;;; <SUBR>(acad-ent2vla entity)</SUBR>
;;; <DESC>Get vla-object from entity</DESC>
;;; <ARG>entity - entity to convert</ARG>
;;; <RET>vla-object</RET>
;;; </LISPDOC>
(defun acad-ent2vla (entity)
  (vlax-ename->vla-object entity))

;;; <LISPDOC>
;;; <SUBR>(acad-dxf-entities)</SUBR>
;;; <DESC>Standard AutoCAD entities list</DESC>
;;; <RET>List of AutoCAD Entities (DXF Code 0)</RET>
;;; </LISPDOC>
(defun acad-dxf-entities ()
  (list "3DFACE" "3DSOLID" "ARC" "ATTDEF" "ATTRIB" "BODY" "CIRCLE" "DIMENSION" "ELLIPSE" "HATCH"
	"HELIX" "IMAGE" "INSERT" "LEADER" "LIGHT" "LINE" "LWPOLYLINE" "MLINE" "MULTILEADER" "MLEADERSTYLE"
	"MTEXT" "OLEFRAME" "OLE2FRAME" "POINT" "POLYLINE" "RAY" "REGION" "SECTION" "SEQEND" "SHAPE"
	"SOLID" "SPLINE" "SUBDIVISION" "SUN" "SURFACE" "ACAD_TABLE" "TEXT" "TOLERANCE" "TRACE"
	"DWFUNDERLAY" "DGNUNDERLAY" "VERTEX" "VIEWPORT" "WIPEOUT" "XLINE"))

;;; <LISPDOC>
;;; <SUBR>(acad-dxf-to-enttypes lst)</SUBR>
;;; <DESC>Convert standard AutoCAD entities list \
;;; to entity types list (0 . "ENTITY")</DESC>
;;; <ARG>lst - list of autocad dxf entities</ARG>
;;; <RET>List of AutoCAD entity types (DXF Code 0)</RET>
;;; </LISPDOC>
(defun acad-dxf-to-enttypes (lst)
  (if lst
    (cons (cons 0 (car lst)) (acad-dxf-to-sscodes (cdr lst)))))

;;; <LISPDOC>
;;; <SUBR>(acad-enttypes-to-ssfilter lst)</SUBR>
;;; <DESC>Add ssget filter options to list</DESC>
;;; <ARG>lst - list of autocad entities types (0 . "ENTITY")</ARG>
;;; <RET>ssfilter list</RET>
;;; </LISPDOC>
(defun acad-enttypes-to-ssfilter (lst)
  (if lst
    (setq lst (cons (cons -4 "<OR") lst)
	  lst (cons (cons -4 "<NOT") lst)
	  lst (append lst (list (cons -4 "OR>")))
	  lst (append lst (list (cons -4 "NOT>"))))))
    
;;; <LISPDOC>
;;; <SUBR>(acad-selset-textstring-entities)</SUBR>
;;; <DESC>Selection set for textstring entities</DESC>
;;; <RET>selection set</RET>
;;; </LISPDOC>
(defun acad-selset-textstring-entities ()
  (ssget "_X" '((0 . "ATTDEF,ATTRIB,LEADER,MLEADER,TEXT,MTEXT"))))

;;; <LISPDOC>
;;; <SUBR>(acad-selset-textoverride-entities)</SUBR>
;;; <DESC>Selection set for textoverride entities</DESC>
;;; <RET>selection set</RET>
;;; </LISPDOC>
(defun acad-selset-textoverride-entities ()
  (ssget "_X" '((0 . "DIMENSION"))))

;;; <LISPDOC>
;;; <SUBR>(acad-selset-table-entities)</SUBR>
;;; <DESC>Selection set for table entities</DESC>
;;; <RET>selection set</RET>
;;; </LISPDOC>
(defun acad-selset-table-entities ()
  (ssget "_X" '((0 . "ACAD_TABLE"))))

;;; <LISPDOC>
;;; <SUBR>(acad-selset-insert-entities)</SUBR>
;;; <DESC>Selection set for insert entities</DESC>
;;; <RET>selection set</RET>
;;; </LISPDOC>
(defun acad-selset-insert-entities ()
  (ssget "_X" '((0 . "INSERT")(66 . 1))))

;;; <LISPDOC>
;;; <SUBR>(acad-selset-proxy-entities)</SUBR>
;;; <DESC>Selection set for non-standard autocad entities</DESC>
;;; <RET>selection set</RET>
;;; </LISPDOC>
(defun acad-selset-proxy-entities ()
  (ssget "_X" (acad-enttypes-to-ssfilter (acad-dxf-to-enttypes (acad-dxf-entities)))))

;;; <LISPDOC>
;;; <SUBR>(acad-extract-textstring vla_object regexp_object)</SUBR>
;;; <DESC>Extract textstring property from entity</DESC>
;;; <ARG>vla_object - acad vla-object</ARG>
;;; <ARG>regexp - VBScript.RegExp pointer</ARG>
;;; <RET>extracted string</RET>
;;; </LISPDOC>
(defun acad-extract-textstring (vla_object regexp_object / textstring)
  (setq textstring (vl-catch-all-apply 'vlax-get (list vla_object 'TextString)))
  (if (= (type textstring) 'STR)
    (string-remove-format regexp_object textstring)
    ""))

;;; <LISPDOC>
;;; <SUBR>(acad-extract-textoverride vla_object regexp_object)</SUBR>
;;; <DESC>Extract textoverride property from entity</DESC>
;;; <ARG>vla_object - acad vla-object</ARG>
;;; <ARG>regexp - VBScript.RegExp pointer</ARG>
;;; <RET>extracted string</RET>
;;; </LISPDOC>
(defun acad-extract-textoverride (vla_object regexp_object / textstring)
  (setq textstring (vl-catch-all-apply 'vlax-get (list vla_object 'TextOverride)))
  (if (= (type textstring) 'STR)
    (string-remove-format regexp_object textstring)
    ""))


;;; <LISPDOC>
;;; <SUBR>(acad-extract-attributes vla_object regexp_object)</SUBR>
;;; <DESC>Extract attributes from insert entity</DESC>
;;; <ARG>vla_object - acad vla-object</ARG>
;;; <ARG>regexp - VBScript.RegExp pointer</ARG>
;;; <RET>list of extracted string</RET>
;;; </LISPDOC>
(defun acad-extract-attributes (vla_object regexp_object / strlist attrvalue)
  (foreach item (vlax-invoke vla_object 'GetAttributes)
    (if (not (string-is-null-or-empty (setq attrvalue (vlax-get item 'TextString))))
      (setq strlist (append (list (string-remove-format regexp_object attrvalue)) strlist))))
  strlist)

;;; <LISPDOC>
;;; <SUBR>(acad-extract-table-text vla_object regexp_object)</SUBR>
;;; <DESC>Extract text from table entity</DESC>
;;; <ARG>vla_object - acad vla-object</ARG>
;;; <ARG>regexp - VBScript.RegExp pointer</ARG>
;;; <RET>list of extracted string</RET>
;;; </LISPDOC>
(defun acad-extract-table-text (vla_object regexp_object / rows cols strlist cellvalue cur_row cur_col)
  (setq rows (vlax-get vla_object 'Rows)
        cols (vlax-get vla_object 'Columns))
  (repeat (setq cur_row rows)
    (repeat (setq cur_col cols)
      (if (not (string-is-null-or-empty (setq cellvalue (vlax-invoke vla_object 'GetCellValue (1- cur_row) (1- cur_col)))))
        (setq strlist (append (list (string-remove-format regexp_object cellvalue)) strlist)))
      (setq cur_col (1- cur_col)))
    (setq cur_row (1- cur_row)))
  strlist)

;;; <LISPDOC>
;;; <SUBR>(acad-textstring-objectname-list)</SUBR>
;;; <DESC>List of textstring elements</DESC>
;;; <RET>list of TextString objectNames</RET>
;;; </LISPDOC>
(defun acad-textstring-objectname-list ()
  (list
    "AcDbMText"
    "AcDbText"
    "AcDbMLeader"
    "AcDbAttributeDefinition"))

;;; <LISPDOC>
;;; <SUBR>(acad-textoverride-objectname-list)</SUBR>
;;; <DESC>List of textoverride elements</DESC>
;;; <RET>list of TextOverride objectNames</RET>
;;; </LISPDOC>
(defun acad-textoverride-objectname-list ()
  (list
    "AcDbRotatedDimension"
    "AcDbAlignedDimension"
    "AcDb2LineAngularDimension"
    "AcDb3PointAngularDimension"
    "AcDbArcDimension"
    "AcDbRadialDimension"
    "AcDbDiametricDimension"
    "AcDbOrdinateDimension"))

;;; <LISPDOC>
;;; <SUBR>(acad-table-objectname-list)</SUBR>
;;; <DESC>List of table elements</DESC>
;;; <RET>list of Table objectNames</RET>
;;; </LISPDOC>
(defun acad-table-objectname-list ()
  (list
    "AcDbTable"))

;;; <LISPDOC>
;;; <SUBR>(acad-block-is-anonymous vla_block)</SUBR>
;;; <DESC>Check anonymous block (*)</DESC>
;;; <ARG>blk - vla-object block</ARG>
;;; <RET>T if anonymous \ nil otherwise</RET>
;;; </LISPDOC>
(defun acad-block-is-anonymous (vla_block)
  (if (= "*" (substr (vlax-get vla_block 'Name) 1 1))
    T
    nil))

;;; <LISPDOC>
;;; <SUBR>(acad-extract-block-text regexp_object)</SUBR>
;;; <DESC>Search text in all blocks</DESC>
;;; <ARG>regexp - VBScript.RegExp pointer</ARG>
;;; <RET>list of extracted strings</RET>
;;; </LISPDOC>
(defun acad-extract-block-text (regexp_object / blocks value object_name strlist blk item)
  (vlax-for blk (vla-get-blocks (acad-actdoc))
    (if (not (acad-block-is-anonymous blk))
      (vlax-for item blk
        (setq object_name (vlax-get item 'ObjectName))
        (cond
          ((member object_name (acad-textstring-objectname-list))
           (if (not (string-is-null-or-empty (setq value (acad-extract-textstring item regexp_object))))
             (setq strlist (append (list value) strlist))))
          ((member object_name (acad-textoverride-objectname-list))
           (if (not (string-is-null-or-empty (setq value (acad-extract-textoverride item regexp_object))))
             (setq strlist (append (list value) strlist))))
          ((member object_name (acad-table-objectname-list))
           (if (setq value (acad-extract-table-text item regexp_object))
             (setq strlist (append value strlist))))))))
  strlist)

;;; <LISPDOC>
;;; <SUBR>(acad-extract-group-text regexp_object ss_subr extr_subr isstring)</SUBR>
;;; <DESC>Search for text in specified entities</DESC>
;;; <ARG>regexp_object - VBScript.RegExp pointer</ARG>
;;; <ARG>ss_subr - selset subr to use</ARG>
;;; <ARG>extr_subr - extact subr to use</ARG>
;;; <ARG>isstring - bool</ARG>
;;; <RET>list of extracted strings</RET>
;;; </LISPDOC>
(defun acad-extract-group-text (regexp_object ss_subr extr_subr isstring / selset k vla_object strlist value)
  (setq selset (eval (list ss_subr)))
  (if selset
    (progn
      (setq k (sslength selset))
      (repeat k
	(setq k (1- k)
	      vla_object (acad-ent2vla (ssname selset k)))
	(if isstring
	  (if (not (string-is-null-or-empty
		     (setq value (eval (list extr_subr vla_object regexp_object)))))
	    (setq strlist (append (list value) strlist)))
	  (if (setq value (eval (list extr_subr vla_object regexp_object)))
	    (setq strlist (append value strlist)))))))
  strlist)  

;;; <LISPDOC>
;;; <SUBR>(acad-extract-database-text regexp_object)</SUBR>
;;; <DESC>Search text in entire database entities besides blocks</DESC>
;;; <ARG>regexp - VBScript.RegExp pointer</ARG>
;;; <RET>list of extracted strings
;;; </LISPDOC>
(defun acad-extract-database-text (regexp_object / strlist)
  (setq strlist
	 (append
	   (acad-extract-group-text regexp_object acad-selset-textstring-entities acad-extract-textstring T) strlist)
	strlist
	 (append
	   (acad-extract-group-text regexp_object acad-selset-textoverride-entities acad-extract-textoverride T) strlist)
	strlist
	 (append
	   (acad-extract-group-text regexp_object acad-selset-insert-entities acad-extract-attributes nil) strlist)
	strlist
	 (append
	   (acad-extract-group-text regexp_object acad-selset-table-entities acad-extract-table-text nil) strlist)))

;;; <LISPDOC>
;;; <SUBR>(acad-extract-all-text regexp_object)</SUBR>
;;; <DESC>Extract all text from drawing</DESC>
;;; <ARG>regexp_object - VBScript.RegExp pointer</ARG>
;;; <RET>list of extracted text strings</RET>
;;; </LISPDOC>
(defun acad-extract-all-text (regexp_object / strlist)
  (setq strlist (append (acad-extract-database-text regexp_object) strlist)
	strlist (append (acad-extract-block-text regexp_object) strlist)))
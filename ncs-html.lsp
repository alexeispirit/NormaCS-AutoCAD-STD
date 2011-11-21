(vl-load-com)

;;; <LISPDOC>
;;; <SUBR>(html-doctype)</SUBR>
;;; <DESC>Define HTML Doctype</DESC>
;;; <RET>HTML Doctype string</RET>
;;; </LISPDOC>
(defun html-doctype ()
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">")

;;; <LISPDOC>
;;; <SUBR>(html-css)</SUBR>
;;; <DESC>Prepare css section</DESC>
;;; <RET>HTML CSS string</RET>
;;; </LISPDOC>
(defun html-css ()
  (strcat
    "<style type='text/css'>"
    "body   {font-family: Arial, Helvetica; font-size: 10pt; margin-left: 10; margin-right: 10 }"
    "table  {border-collapse: collapse; font-size: 10pt; border: 1px solid black; align: left}"
    "td {padding: 1pt 3pt 2pt 3pt; border: 1px solid black}"
    "td.old {color: #990000}"
    "td.new {color: #006600}"
    "th {font-weight: bold; align: center; border: 1px solid black}"
    "h1 {font-weight: bold; font-size: 14pt}"
    "</style>"))

;;; <LISPDOC>
;;; <SUBR>(html-title)</SUBR>
;;; <DESC>Prepare html title</DESC>
;;; <RET>HTML Title string</RET>
;;; </LISPDOC>
(defun html-title ()
  "<title>NormaCS check standards</title>")

;;; <LISPDOC>
;;; <SUBR>(html-h1 dwgname)</SUBR>
;;; <DESC>Prepare html h1</DESC>
;;; <ARG>dwgname - string</ARG>
;;; <RET>HTML h1 string</RET>
;;; </LISPDOC>
(defun html-h1 (dwgname)
  (strcat
    "<h1>NormaCS checked standards ("
    dwgname
    ")</h1>"))

;;; <LISPDOC>
;;; <SUBR>(html-header dwgname)</SUBR>
;;; <DESC>Prepare html header</DESC>
;;; <ARG>dwgname - string</ARG>
;;; <RET>HTML Header string</RET>
;;; </LISPDOC>
(defun html-header (dwgname)
  (strcat
    (html-doctype)
    "<html>"
    "<head>"
    (html-title)
    (html-css)
    "</head>"
    "<body>"
    (html-h1 dwgname)))

;;; <LISPDOC>
;;; <SUBR>(html-footer)</SUBR>
;;; <DESC>Prepare html footer</DESC>
;;; <RET>HTML footer string</RET>
;;; </LISPDOC>
(defun html-footer ()
  (strcat
    "</body>"
    "</html>"))

;;; <LISPDOC>
;;; <SUBR>(html-table-start)</SUBR>
;;; <DESC>Prepare html table (header)</DESC>
;;; <RET>HTML Table start tags string</RET>
;;; </LISPDOC>
(defun html-table-start ()
  (strcat
    "<table>"
    "<tr>"
    "<th width=200px>Имя в чертеже</th>"
    "<th width=200px>Имя в системе</th>"
    "<th width=70px>Актуален</th>"
    "<th width=200px>Заменен</th>"
    "</tr>"))

;;; <LISPDOC>
;;; <SUBR>(html-table-finish)</SUBR>
;;; <DESC>Prepare html table (footer)</DESC>
;;; <RET>HTML Table finish tags string</RET>
;;; </LISPDOC>
(defun html-table-finish ()
  "</table>")

;;; <LISPDOC>
;;; <SUBR>(html-table-tr str)</SUBR>
;;; <DESC>Prepare html table row</DESC>
;;; <ARG>str - string to tag</ARG>
;;; <RET>HTML Table tr string</RET>
;;; </LISPDOC>
(defun html-table-tr (str)
  (strcat
    "<tr>"
    str
    "</tr>"))

;;; <LISPDOC>
;;; <SUBR>(html-table-td str class)</SUBR>
;;; <DESC>Prepare html table cell</DESC>
;;; <ARG>str - string to tag</ARG>
;;; <ARG>class - cell css class</ARG>
;;; <RET>HTML Table td string</RET>
;;; </LISPDOC>
(defun html-table-td (str class)
  (strcat
    "<td class="
    class
    ">"
    str
    "</td>"))

;;; <LISPDOC>
;;; <SUBR>(html-a-href str href)</SUBR>
;;; <DESC>Prepare html a href item</DESC>
;;; <ARG>str - description string</ARG>
;;; <ARG>href - url string</ARG>
;;; <RET>HTML Href string</RET>
;;; </LISPDOC>
(defun html-a-href (str href)
  (strcat
    "<a href="
    href
    ">"
    str
    "</a>"))
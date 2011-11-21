(vl-load-com) ; remove after main

;;; <LISPDOC>
;;; <SUBR>(file-read-config file)</SUBR>
;;; <DESC>Read configuration file to list</DESC>
;;; <ARG>file - file to read</ARG>
;;; <RET>list of config values (("Key" "Value") ...)</RET>
;;; </LISPDOC>
(defun file-read-config (file / fh line conf_list)
  (setq fh (open (findfile file) "r"))
  (setq conf_list (list ))
  (while (setq line (read-line fh))
    (setq conf_list (cons (read line) conf_list)))
  (close fh)
  (reverse conf_list))

;;; <LISPDOC>
;;; <SUBR>(file-get-value config key)</SUBR>
;;; <DESC>Get value from config by key</DESC>
;;; <ARG>config - config list</ARG>
;;; <ARG>key - key to search</ARG>
;;; <RET>value or nil</RET>
;;; </LISPDOC>
(defun file-get-value (config key / pair value)
  (if config
    (progn
      (setq pair (assoc key config))
      (if pair (setq value (cadr pair)))))
  (if value
    value
    nil))

;;; <LISPDOC>
;;; <SUBR>(file-create-if-none filename str)</SUBR>
;;; <DESC>Create file with string if file not found</DESC>
;;; <ARG>filename - file to test and create</ARG>
;;; <ARG>str - string to write in file</ARG>
;;; <RET>nil</RET>
;;; </LISPDOC>
(defun file-create-if-none (filename str / fh)
  (if (not (findfile filename))
    (progn
      (setq fh (open filename "w"))
      (write-line str fh)
      (close fh))))

;;; <LISPDOC>
;;; <SUBR>(file-prepare-config-content</SUBR>
;;; <DESC>Prepare string for config file</DESC>
;;; <RET>config string</RET>
;;; </LISPDOC>
(defun file-prepare-config-content (/)
  (strcat "(\"RegExp\" \"([À-ßà-ÿ ]{2,9} [0-9.-]+)\")\n"
          "(\"BlackListExactMatch\" nil)"))

;;; <LISPDOC>
;;; <SUBR>(file-config-folder)</SUBR>
;;; <DESC>Set configuration files folder</DESC>
;;; <RET>Path to config files</RET>
;;; </LISPDOC>
(defun file-config-folder   (/)
  (strcat (getenv "appdata") "\\ncs-acad"))

;;; <LISPDOC>
;;; <SUBR>(file-config-path filename)</SUBR>
;;; <DESC>Set config flie name</DESC>
;;; <ARG>filename - filename</ARG>
;;; <RET>Path to config file</RET>
;;; </LISPDOC>
(defun file-config-path (filename /)
  (strcat (file-config-folder) "\\" filename))

;;; <LISPDOC>
;;; <SUBR>(file-config-filenames-list filename)</SUBR>
;;; <DESC>List of config files</DESC>
;;; <RET>List of config files path</RET>
;;; </LISPDOC>
(defun file-config-filenames-list (/)
  (list
    (file-config-path "ncs.conf")
    (file-config-path "ncs.blacklist")
    (file-config-path "ncs.whitelist")))    

;;; <LISPDOC>
;;; <SUBR>(file-initialize-config)</SUBR>
;;; <DESC>Initialize config</DESC>
;;; <RET>path to config, blacklist and whitelist files \
;;; as list (("CONFIG" config) ("BLACKLIST" blacklist) ("WHITELIST" whitelist)</RET>
;;; </LISPDOC>
(defun file-initialize-config ( / apppath config_name blacklist_name whitelist_name fh)
  (setq apppath (file-config-folder)
    	config_name (file-config-path "ncs.conf")
        blacklist_name (file-config-path "ncs.blacklist")
	whitelist_name (file-config-path "ncs.whitelist"))
  (if (not (findfile apppath))
    (vl-mkdir apppath))
  (file-create-if-none config_name (file-prepare-config-content))
  (file-create-if-none blacklist_name " ")
  (file-create-if-none whitelist_name "ÃÎÑÒ\nÑÍèÏ")
  (list
    (list "CONFIG" config_name)
    (list "BLACKLIST" blacklist_name)
    (list "WHITELIST" whitelist_name)))

;;; <LISPDOC>
;;; <SUBR>(file-reinitialize-config)</SUBR>
;;; <DESC>Reinitialize config</DESC>
;;; <RET>path to config, blacklist and whitelist files \
;;; as list (("CONFIG" config) ("BLACKLIST" blacklist) ("WHITELIST" whitelist)</RET>
;;; </LISPDOC>
(defun file-reinitialize-config ( / )
  (foreach file (file-config-filenames-list)
    (vl-file-delete file))
  (file-initialize-config))

;;; <LISPDOC>
;;; <SUBR>(file-get initialize_list file_type)</SUBR>
;;; <DESC>Get config file path from initlist</DESC>
;;; <ARG>initialize_list - list after file-initialize-config subroutine</ARG>
;;; <ARG>file_type - CONFIG/BLACKLIST/WHITELIST</ARG>
;;; <RET>path to config file</RET>
;;; </LISPDOC>
(defun file-get (initialize_list file_type /)
  (if (and initialize_list file_type)
    (cadr (assoc file_type initialize_list))))

;;; <LISPDOC>
;;; <SUBR>(file-read-list file)</SUBR>
;;; <DESC>Read blacklist/whitelist filetypes</DESC>
;;; <ARG>file - file to read</ARG>
;;; <RET>list of lines</RET>
;;; </LISPDOC>
(defun file-read-list (file / fh line bwlist)
  (setq fh (open (findfile file) "r"))
  (while (setq line (read-line fh))
    (if (and
	  (not (string-is-null-or-empty line))
	  (not (string-is-a-comment line)))
      (setq bwlist (append (list line) bwlist))))
  (close fh)
  bwlist)

;;; <LISPDOC>
;;; <SUBR>(file-mktemp)</SUBR>
;;; <DESC>Create tempfile</DESC>
;;; <RET>full filename</RET>
;;; </LISPDOC>
(defun file-mktemp ()
  (vl-filename-mktemp "ncsa" (getenv "TEMP") ".html"))

;;; <LISPDOC>
;;; <SUBR>(file-shell-regapp)</SUBR>
;;; <DESC>Create Shell.Application pointer</DESC>
;;; <RET>Shell.Application pointer</RET>
;;; </LISPDOC>
(defun file-shell-regapp ()
  (vla-getinterfaceobject (vlax-get-acad-object) "Shell.Application"))

;;; <LISPDOC>
;;; <SUBR>(file-shell-execute shell filename)</SUBR>
;;; <DESC>Execute Shell command</DESC>
;;; <ARG>shell - Shell.Application pointer</ARG>
;;; <ARG>filename - filename to execute</ARG>
;;; <RET>nil</RET>
;;; </LISPDOC>
(defun file-shell-execute (shell filename)
  (vlax-invoke shell 'ShellExecute filename))

;;; <LISPDOC>
;;; <SUBR>(file-netload filename)</SUBR>
;;; <DESC>Load .Net assembly into autocad</DESC>
;;; <ARG>filename - path to dll file</ARG>
;;; <RET>T or nil</RET>
;;; </LISPDOC>
(defun file-netload (filename)
  (if (findfile filename)
    (progn
      (vl-cmdf "_netload" filename)
      T)
    nil))

;;; <LISPDOC>
;;; <SUBR>(file-check-subr subname)</SUBR>
;;; <DESC>Check whether autolisp subroutine loaded or not</DESC>
;;; <ARG>subname - name of subroutine (SYM)</ARG>
;;; <RET>T or nil</RET>
;;; </LISPDOC>
(defun file-check-subr (subname)
  (if (member subname (atoms-family 0))
    T
    nil))
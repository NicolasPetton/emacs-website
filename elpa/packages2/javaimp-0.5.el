;;; javaimp.el --- Add and reorder Java import statements in Maven projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>
;; Version: 0.5
;; Keywords: java, maven, programming

;;; Commentary:

;; Allows to manage Java import statements in Maven projects.
;;
;;   Quick start: customize `javaimp-import-group-alist', `javaimp-jdk-home'
;; and call `javaimp-maven-visit-root', then in a Java buffer visiting a
;; file under that module or one of its submodules call
;; `javaimp-organize-imports' or `javaimp-add-import'.  `javaimp-add-import'
;; will provide you a helpful completion, and the default value (the one
;; you'll get if you hit `M-n' in the minibuffer) is the symbol under point,
;; so usually it's enough to hit `M-n', then add some starting letters of a
;; package and hit `TAB'.  The module does not add all needed imports
;; automatically!  It only helps you to quickly add imports when stepping
;; through compilation errors.
;;
;; If Maven failed, you can see its output in the buffer named by
;; `javaimp-debug-buf-name' (default is "*javaimp-debug*").
;;
;; Contents of jar files and Maven project structures (pom.xml) are cached,
;; so usually only first command should take a considerable amount of time
;; to complete.  When it is detected that a particular jar or pom.xml file's
;; timestamp changed, it is re-read and cache is updated.
;;
;; Details on variables.
;; 
;;   `javaimp-import-group-alist' defines the order of import statement
;; groups.  By default java.* and javax.* imports are assigned an order of
;; 10, which is low, so it puts those imports at the beginning.  Your
;; project's imports typically should come after, so the sample config below
;; sets 80 for them.
;; 
;;  `javaimp-jdk-home' is a path for JDK.  It is used to scan JDK jars.
;; Usually you will need to set this.
;;
;;  `javaimp-additional-source-dirs' is a list specifying directories where
;; additional (e.g. generated) source files reside.  Each directory is a
;; relative path from ${project.build.directory} project property value.
;; 
;;  `javaimp-mvn-program' defines path of the `mvn' program.  Use if it's
;; not on `exec-path'.
;;
;;  `javaimp-cygpath-program' defines path of the `cygpath' program (applies
;; to Cygwin only, of course).  Use if it's not on `exec-path'.
;;
;;  `javaimp-jar-program' defines path of the `jar' program.  Use if it's
;; not on `exec-path'.
;;  
;; Details on commands.
;;
;;   `javaimp-maven-visit-root' is the first command you should issue to
;; use this module.  It reads the pom structure recursively and records
;; which files belong to which module.  Maven help:effective-pom command is
;; used to do that.
;;
;;   `javaimp-organize-imports' groups import statement and writes those
;; group according to the value of `javaimp-import-group-alist'.  Imports
;; which are not matched by any regexp in that variable are assigned a
;; default order defined by `javaimp-import-default-order' (50 by default).
;;
;; Sample setup (put this into your .emacs):
;; 
;; (require 'javaimp)
;; 
;; (add-to-list 'javaimp-import-group-alist '("\\`\\(ru\\.yota\\.\\|tv\\.okko\\.\\)" . 80))
;; 
;; (setq javaimp-jdk-home (getenv "JAVA_HOME"))
;; (setq javaimp-include-current-project-classes t)
;; (setq javaimp-additional-source-dirs '("generated-sources/thrift"))
;; 
;; (add-hook 'java-mode-hook
;; 	  (lambda ()
;; 	    (local-set-key "\C-ci" 'javaimp-add-import)
;; 	    (local-set-key "\C-co" 'javaimp-organize-imports)))
;; 
;; 
;; TODO:
;; 
;; Support adding static imports by giving a prefix argument to
;; `javaimp-add-import'.
;;  
;; Use functions `cygwin-convert-file-name-from-windows' and
;; `cygwin-convert-file-name-to-windows' when they are available instead of
;; calling `cygpath'.  See
;; https://cygwin.com/ml/cygwin/2013-03/msg00228.html.


;;; Code:


;;; User options

(defgroup javaimp ()
  "Add and reorder Java import statements in Maven projects.")

(defcustom javaimp-import-group-alist '(("\\`javax?\\." . 10))
  "Specifies how to group classes and how to order resulting
groups in the imports list.

Each element should be of the form `(CLASSNAME-REGEXP . ORDER)'
where `CLASSNAME-REGEXP' is a regexp matching the fully qualified
class name.

The order of classes which were not matched is defined by
`javaimp-import-default-order'.")

(defcustom javaimp-import-default-order 50
  "Defines the order of classes which were not matched by
`javaimp-import-group-alist'")

(defcustom javaimp-jdk-home nil
  "Path to the JDK")

(defcustom javaimp-additional-source-dirs nil
  "List of directories where additional (e.g. generated)
source files reside.

Each directory is a relative path from ${project.build.directory} project
property value.

Typically you would check documentation for a Maven plugin, look
at the parameter's default value there and add it to this list.

E.g. \"${project.build.directory}/generated-sources/<plugin_name>\"
becomes \"generated-sources/<plugin_name>\" (note the absence
of the leading slash.

Custom values set in plugin configuration in pom.xml are not
supported yet.")

(defcustom javaimp-mvn-program "mvn"
  "Path to the `mvn' program")

(defcustom javaimp-cygpath-program "cygpath"
  "Path to the `cygpath' program")

(defcustom javaimp-jar-program "jar"
  "Path to the `jar' program")

(defcustom javaimp-include-current-project-classes t
  "If non-nil, current project's classes are included into completion
alternatives.

Only top-level classes are included.")


;;; Variables and constants

(defvar javaimp-maven-root-modules nil
  "Loaded root Maven modules")

(defvar javaimp-jar-classes-cache nil
  "Jar classes cache")

(defconst javaimp-debug-buf-name "*javaimp-debug*")


;;; Dealing with XML

(defun javaimp-xml-child-list (xml-tree child-name)
  "Returns list of children of XML-TREE filtered by CHILD-NAME"
  (let (result)
    (dolist (child (cddr xml-tree) result)
      (when (and (listp child)
		 (eq (car child) child-name))
	(push child result)))))

(defun javaimp-xml-child (name el)
  "Returns a child of EL named by symbol NAME"
  (assq name (cddr el)))

(defun javaimp-xml-first-child (el)
  "Returns a first child of EL"
  (car (cddr el)))


;; A module is represented as a list of the form `(ARTIFACT POM-FILE
;; SOURCE-DIR TEST-SOURCE-DIR BUILD-DIR POM-FILE-MOD-TS PARENT PARENT-TS)'.

(defsubst javaimp-make-mod (artifact pom-file source-dir
				     test-source-dir build-dir
				     pom-file-mod-ts jars-list
				     parent parent-ts)
  (list artifact pom-file source-dir test-source-dir build-dir
	pom-file-mod-ts jars-list parent parent-ts))

(defsubst javaimp-get-mod-artifact (module)
  (nth 0 module))

(defsubst javaimp-get-mod-pom-file (module)
  (nth 1 module))

(defsubst javaimp-get-mod-source-dir (module)
  (nth 2 module))

(defsubst javaimp-get-mod-test-source-dir (module)
  (nth 3 module))

(defsubst javaimp-get-mod-build-dir (module)
  (nth 4 module))

(defsubst javaimp-get-mod-pom-mod-ts (module)
  (nth 5 module))
(defsubst javaimp-set-mod-pom-mod-ts (module value)
  (setcar (nthcdr 5 module) value))

(defsubst javaimp-get-mod-pom-deps (module)
  (nth 6 module))
(defsubst javaimp-set-mod-pom-deps (module value)
  (setcar (nthcdr 6 module) value))

(defsubst javaimp-get-mod-parent (module)
  (nth 7 module))
(defsubst javaimp-set-mod-parent (module value)
  (setcar (nthcdr 7 module) value))

(defsubst javaimp-get-mod-parent-ts (module)
  (nth 8 module))
(defsubst javaimp-set-mod-parent-ts (module value)
  (setcar (nthcdr 8 module) value))


;; An artifact is represented as a list: (GROUP-ID ARTIFACT-ID VERSION).

;; FIXME: use cl-defstruct!

(defun javaimp-make-artifact (group-id artifact-id version)
  (list group-id artifact-id version))

(defun javaimp-artifact-group-id (artifact)
  (car artifact))

(defun javaimp-artifact-artifact-id (artifact)
  (cadr artifact))

(defun javaimp-artifact-version (artifact)
  (nth 2 artifact))

(defun javaimp-artifact-to-string (artifact)
  (format "%s:%s:%s"
	  (javaimp-artifact-artifact-id artifact)
	  (javaimp-artifact-group-id artifact)
	  (javaimp-artifact-version artifact))) ;FIXME: `artifact' is not a function!

(defun javaimp-parse-artifact (artifact)
  (apply #'javaimp-make-artifact (split-string artifact ":")))



;; A jar is represented as follows: `(JAR-PATH JAR-MOD-TS . CLASSES-LIST).

(defsubst javaimp-make-jar (jar-path jar-mod-ts classes-list)
  (cons jar-path (cons jar-mod-ts classes-list)))

(defsubst javaimp-get-jar-path (jar)
  (car jar))

(defsubst javaimp-get-jar-mod-ts (jar)
  (cadr jar))

(defsubst javaimp-set-jar-mod-ts (jar value)
  (setcar (cdr jar) value))

(defsubst javaimp-get-jar-classes-list (jar)
  (cddr jar))

(defsubst javaimp-set-jar-classes-list (jar value)
  (setcdr (cdr jar) value))


;;; Loading maven projects tree

;;;###autoload
(defun javaimp-maven-visit-root (path)
  "Loads all modules starting from root module identified by
PATH.  PATH should point to a directory."
  (interactive "DVisit maven root project: ")
  (let ((root-pom (expand-file-name
		   (concat (file-name-as-directory path) "pom.xml")))
	modules existing-module)
    (unless (file-readable-p root-pom)
      (error "Cannot read root pom: %s" root-pom))
    (setq modules (javaimp-maven-load-module-tree root-pom))
    ;; if a root module with such path is already loaded, replace its
    ;; modules
    (setq existing-module (assoc root-pom javaimp-maven-root-modules))
    (if existing-module
	(setcdr existing-module modules)
      (push (cons root-pom modules) javaimp-maven-root-modules))
    (message "Loaded modules for %s" path)))

(defun javaimp-get-projects (xml-tree)
  (cond ((assq 'projects xml-tree)
	 (javaimp-xml-child-list (assq 'projects xml-tree) 'project))
	((assq 'project xml-tree)
	 (list (assq 'project xml-tree)))
	(t
	 (error "Cannot find projects in mvn output"))))

(defun javaimp-maven-load-module-tree (pom)
  "Returns an alist of all Maven modules in a hierarchy starting
with POM"
  (message "Loading root pom %s..." pom)
  (javaimp-call-mvn
   pom "help:effective-pom"
   (lambda ()
     (let (xml-start-pos xml-end-pos)
       ;; find where we should start parsing XML
       (goto-char (point-min))
       (re-search-forward "<\\?xml\\|<projects?>")
       (setq xml-start-pos (match-beginning 0))
       ;; determine the start tag
       (goto-char (point-min))
       (re-search-forward "<\\(projects?\\)")
       ;; find closing tag which is also the end of the region to parse
       (search-forward (concat "</" (match-string 1) ">"))
       (setq xml-end-pos (match-end 0))
       ;; parse
       (let ((artifact-pomfile-alist
	      (javaimp-build-artifact-pomfile-alist (list pom)))
	     (children (javaimp-get-projects
			(xml-parse-region xml-start-pos xml-end-pos))))
	 (javaimp-maven-build-children children artifact-pomfile-alist))))))

(defun javaimp-make-artifact-from-xml (node)
  (javaimp-make-artifact
   (javaimp-xml-first-child (javaimp-xml-child 'groupId node))
   (javaimp-xml-first-child (javaimp-xml-child 'artifactId node))
   (javaimp-xml-first-child (javaimp-xml-child 'version node))))

(defun javaimp-get-pom-file-path-lax (artifact artifact-pomfile-alist)
  (assoc-default
   artifact artifact-pomfile-alist
   (lambda (tested target)
     (or (equal target tested)
	 (equal (javaimp-artifact-artifact-id target)
		(javaimp-artifact-artifact-id tested))))))

(defun javaimp-maven-build-children (projects artifact-pomfile-alist)
  (let (result)
    (dolist (proj projects result)
      (let* ((artifact (javaimp-make-artifact-from-xml proj))
	     (pom-file-path (javaimp-get-pom-file-path-lax
			     artifact artifact-pomfile-alist))
	     (build (javaimp-xml-child 'build proj))
	     (source-dir (javaimp-xml-first-child
			  (javaimp-xml-child 'sourceDirectory build))) 
	     (test-source-dir (javaimp-xml-first-child
			       (javaimp-xml-child 'testSourceDirectory
						  build)))
	     (build-dir (javaimp-xml-first-child
			 (javaimp-xml-child 'directory build)))
	     (parent (javaimp-make-artifact-from-xml
		      (javaimp-xml-child 'parent proj))))
	(push (javaimp-make-mod 
	       artifact
	       pom-file-path
	       (file-name-as-directory
		(if (eq system-type 'cygwin) 
		    (car (process-lines javaimp-cygpath-program "-u"
					source-dir))
		  source-dir))
	       (file-name-as-directory
		(if (eq system-type 'cygwin) 
		    (car (process-lines javaimp-cygpath-program "-u" 
					test-source-dir))
		  test-source-dir))
	       (file-name-as-directory
		(if (eq system-type 'cygwin) 
		    (car (process-lines javaimp-cygpath-program "-u" 
					build-dir))
		  build-dir))
	       nil nil parent nil)
	      result)))))

(defun javaimp-build-artifact-pomfile-alist (pom-file-list)
  "Recursively builds an alist where each element is of the
form (\"ARTIFACT\" . \"POM-FILE-PATH\"). This is needed because
there is no pom file path in the output of `mvn
help:effective-pom'.  Each pom file path in POM-FILE-LIST should
be in platform's default format."
  (when pom-file-list
    (let ((pom-file (car pom-file-list))
	  xml-tree project)
      (message "Saving artifact id -> pom file mapping for %s" pom-file)
      (with-temp-buffer
	(insert-file-contents pom-file)
	(setq xml-tree (xml-parse-region (point-min) (point-max))))
      (setq project (if (assq 'top xml-tree)
			(assq 'project (cddr (assq 'top xml-tree)))
		      (assq 'project xml-tree)))
      (cons
       ;; this pom
       (cons (javaimp-make-artifact-from-xml project) pom-file)
       (append
	;; submodules
	(javaimp-build-artifact-pomfile-alist
	 (mapcar (lambda (submodule)
		   (expand-file-name
		    (concat
		     ;; this pom's path
		     (file-name-directory pom-file)
		     ;; relative submodule directory
		     (file-name-as-directory
		      (let ((submodule-path (car (cddr submodule))))
			(if (eq system-type 'cygwin)
			    (car (process-lines javaimp-cygpath-program "-u" 
						submodule-path))
			  submodule-path)))
		     ;; well-known file name
		     "pom.xml")))
		 (javaimp-xml-child-list (assq 'modules (cddr project)) 'module)))
	;; rest items
	(javaimp-build-artifact-pomfile-alist (cdr pom-file-list)))))))

(defun javaimp-call-mvn (pom-file target handler)
  "Runs Maven target TARGET on POM-FILE, then calls HANDLER in
the temporary buffer and returns its result"
  (message "Calling \"mvn %s\" on pom: %s" target pom-file)
  (with-temp-buffer
    (let* ((pom-file (if (eq system-type 'cygwin) 
			 (car (process-lines javaimp-cygpath-program 
					     "-m" pom-file))
		       pom-file))
	   (status
	    ;; FIXME on GNU/Linux Maven strangely outputs ^M chars. Check
	    ;; also jar output with the same var binding below.
	    (let ((coding-system-for-read (when (eq system-type 'cygwin) 'utf-8-dos)))
	      (process-file javaimp-mvn-program nil t nil "-f" pom-file target)))
	   (output-buf (current-buffer)))
      (with-current-buffer (get-buffer-create javaimp-debug-buf-name)
	(erase-buffer)
	(insert-buffer-substring output-buf))
      (unless (and (numberp status) (= status 0))
	(error "Maven target \"%s\" failed with status \"%s\""
	       target status))
      (funcall handler))))


;;; Reading and caching dependencies

(defun javaimp-maven-fetch-module-deps (module)
  "Returns list of dependency jars for MODULE"
  (javaimp-call-mvn
   (javaimp-get-mod-pom-file module) "dependency:build-classpath"
   (lambda ()
     (let (deps-line)
       (goto-char (point-min))
       (search-forward "Dependencies classpath:")
       (forward-line 1)
       (setq deps-line (thing-at-point 'line))
       (when (eq system-type 'cygwin)
	 (setq deps-line (car (process-lines javaimp-cygpath-program 
					     "-up" 
					     deps-line))))
       (split-string deps-line (concat "[" path-separator "\n" "]+") t)))))

(defun javaimp-get-file-ts (file)
  (nth 5 (file-attributes file)))

(defun javaimp-any-file-ts-updated (files)
  (if (null files)
      nil
    (let ((curr-ts (javaimp-get-file-ts (car (car files))))
	  (last-ts (cdr (car files))))
      (or (null last-ts)		; reading for the first time?
	  (not (equal (float-time curr-ts) (float-time last-ts)))
	  (javaimp-any-file-ts-updated (cdr files))))))

(defun javaimp-get-dep-jars-cached (module parent)
  "Returns a list of dependency jar file paths for a MODULE.
Both MODULE and PARENT poms are checked for updates because
PARENT pom may have some versions which are inherited by the
MODULE."
  (when (javaimp-any-file-ts-updated
	 (remq nil (list (cons (javaimp-get-mod-pom-file module)
			       (javaimp-get-mod-pom-mod-ts module))
			 (when parent
			   (cons
			    (javaimp-get-mod-pom-file parent)
			    ;; here we check the saved parent ts because it
			    ;; matters what version we had when we were
			    ;; reloading this pom the last time
			    (javaimp-get-mod-parent-ts module))))))
    ;; (re-)fetch dependencies
    (javaimp-set-mod-pom-deps
     module (javaimp-maven-fetch-module-deps module))
    ;; update timestamps
    (javaimp-set-mod-pom-mod-ts
     module (javaimp-get-file-ts (javaimp-get-mod-pom-file module)))
    (when parent
      (javaimp-set-mod-parent-ts
       module (javaimp-get-file-ts (javaimp-get-mod-pom-file parent)))))
  (javaimp-get-mod-pom-deps module))

(defun javaimp-get-jdk-jars ()
  "Returns list of jars from the jre/lib subdirectory of the JDK
directory"
  (when javaimp-jdk-home
    (directory-files (concat (file-name-as-directory javaimp-jdk-home)
			     (file-name-as-directory "jre/lib"))
		     t "\\.jar$")))

(defun javaimp-get-jar-classes-cached (jar)
  (let ((current-jar-mod-ts
	 (nth 5 (file-attributes (javaimp-get-jar-path jar)))))
    (unless (equal (float-time (javaimp-get-jar-mod-ts jar))
		   (float-time current-jar-mod-ts))
      (javaimp-set-jar-classes-list jar (javaimp-fetch-jar-classes jar))
      (javaimp-set-jar-mod-ts jar current-jar-mod-ts))
    (javaimp-get-jar-classes-list jar)))

(defun javaimp-fetch-jar-classes (jar)
  (let ((jar-file (javaimp-get-jar-path jar))
	result)
    (message "Reading classes in jar: %s" jar-file)
    (with-temp-buffer
      (let ((jar-file (if (eq system-type 'cygwin) 
			  (car (process-lines javaimp-cygpath-program 
					      "-m" jar-file))
			jar-file))
	    (coding-system-for-read (when (eq system-type 'cygwin) 'utf-8-dos)))
	(process-file javaimp-jar-program nil t nil "-tf" jar-file))
      (goto-char (point-min))
      (while (re-search-forward "^\\(.+\\)\\.class$" nil t)
	(push (replace-regexp-in-string "[/$]" "." (match-string 1))
	      result))
      result)))

(defun javaimp-collect-jar-classes (jar-paths)
  (let (result jar)
    (dolist (jar-path jar-paths result)
      (setq jar (assoc jar-path javaimp-jar-classes-cache))
      (unless jar
	(setq jar (javaimp-make-jar jar-path nil nil))
	(push jar javaimp-jar-classes-cache))
      (setq result (append (javaimp-get-jar-classes-cached jar) result)))))

(defun javaimp-get-module-from-root (roots predicate)
  (if (null roots)
      nil
    (let ((result (javaimp-get-module (cdr (car roots)) predicate)))
      (or result
	  (javaimp-get-module-from-root (cdr roots) predicate)))))

(defun javaimp-get-module (modules predicate)
  (cond ((null modules)
	 nil)
	((funcall predicate (car modules))
	 (car modules))
	(t
	 (javaimp-get-module (cdr modules) predicate))))

(defun javaimp-get-module-by-file (file)
  (javaimp-get-module-from-root
   javaimp-maven-root-modules
   (lambda (mod)
     (or (string-prefix-p (javaimp-get-mod-source-dir mod) file)
	 (string-prefix-p (javaimp-get-mod-test-source-dir mod) file)))))

(defun javaimp-get-module-by-artifact (artifact)
  (javaimp-get-module-from-root
   javaimp-maven-root-modules
   (lambda (mod)
     (equal (javaimp-get-mod-artifact mod) artifact))))


;;; Adding and organizing imports

;;;###autoload
(defun javaimp-add-import (classname)
  "Imports CLASSNAME in the current file.  Interactively,
performs class name completion based on the current module's
dependencies, JDK jars and top-level classes in the current
module."
  (interactive
   (let* ((file (expand-file-name
		 (or buffer-file-name
		     (error "Buffer is not visiting a file!"))))
	  (module (or (javaimp-get-module-by-file file)
		      (error "Cannot determine module for file: %s" file)))
	  (parent (javaimp-get-module-by-artifact
		   (javaimp-get-mod-parent module))))
     (list (completing-read
	    "Import: "
	    (append
	     (javaimp-collect-jar-classes
	      (append (javaimp-get-dep-jars-cached module parent)
	      	      (javaimp-get-jdk-jars)))
	     (and javaimp-include-current-project-classes
		  (javaimp-get-module-classes module)))
	    nil t nil nil (symbol-name (symbol-at-point))))))
  (javaimp-organize-imports classname))

(defun javaimp-get-module-classes (module)
  "Scans current project and returns a list of top-level classes in both the
source directory and test source directory"
  (let ((src-dir (javaimp-get-mod-source-dir module))
	(test-src-dir (javaimp-get-mod-test-source-dir module))
	(build-dir (javaimp-get-mod-build-dir module)))
    (append
     (and javaimp-additional-source-dirs
	  (seq-mapcat
	   (lambda (rel-dir)
	     (let ((dir (file-name-as-directory (concat build-dir rel-dir))))
	       (and (file-accessible-directory-p dir)
		    (javaimp-get-directory-classes dir nil))))
	   javaimp-additional-source-dirs))
     (and (file-accessible-directory-p test-src-dir)
	  (javaimp-get-directory-classes test-src-dir nil))
     (and (file-accessible-directory-p src-dir)
	  (javaimp-get-directory-classes src-dir nil)))))

(defun javaimp-get-directory-classes (dir prefix)
  "Returns the list of classes found in the directory DIR.  PREFIX is the
initial package prefix."
  (let (result)
    ;; traverse subdirectories
    (dolist (file (directory-files-and-attributes dir nil nil t))
      (if (and (eq (cadr file) t)
	       (not (or (string= (car file) ".")
			(string= (car file) ".."))))
	  (setq result
		(append (javaimp-get-directory-classes 
			 (concat dir (file-name-as-directory (car file)))
			 (concat prefix (car file) "."))
			result))))
    ;; add .java files in the current directory
    (dolist (file (directory-files-and-attributes dir nil "\\.java\\'" t))
      (unless (cadr file)
	(push (concat prefix (file-name-sans-extension (car file))) result)))
    result))

(defun javaimp-add-to-import-groups (new-class groups)
  "Subroutine of `javaimp-organize-imports'"
  (let* ((order (or (assoc-default new-class javaimp-import-group-alist
				   'string-match)
		    javaimp-import-default-order))
	 (group (assoc order groups)))
    (if group
	(progn
	  ;; add only if this class is not already there
	  (unless (member new-class (cdr group))
	    (setcdr group (cons new-class (cdr group))))
	  groups)
      (cons (cons order (list new-class)) groups))))

(defun javaimp-insert-import-groups (groups static-p)
  "Inserts all imports in GROUPS.  Non-nil STATIC-P means that
  all imports are static."
  (when groups
    (dolist (group (sort groups (lambda (g1 g2)
				  (< (car g1) (car g2)))))
      (dolist (class (sort (cdr group) 'string<))
	(insert (concat "import " (when static-p "static ") class ";\n")))
      (insert ?\n))
    ;; remove newline after the last group
    (delete-char -1)))

;;;###autoload
(defun javaimp-organize-imports (&rest new-classes)
  "Groups and orders import statements in the current buffer.  Groups are
formed and ordered according to `javaimp-import-group-alist'.  Classes within a
single group are ordered in a lexicographic order. Optional NEW-CLASSES
argument is a list of additional classes to import."
  (interactive)
  (barf-if-buffer-read-only)
  (save-excursion
    (let (import-groups static-import-groups old-imports-start)
      ;; existing imports
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\s-*import\\s-+\\(static\\s-+\\)?\\([._[:word:]]+\\)"
	      nil t)
	(if (null (match-string 1))
	    (setq import-groups
		  (javaimp-add-to-import-groups (match-string 2)
						import-groups))
	  (setq static-import-groups
		(javaimp-add-to-import-groups (match-string 2)
					      static-import-groups)))
	(beginning-of-line)
	(unless old-imports-start (setq old-imports-start (point)))
	(delete-region (point) (line-beginning-position 2))
	;; delete whatever was between import statements
	(when (/= (point) old-imports-start)
	  (delete-region old-imports-start (point))))
      ;; new imports
      (dolist (class new-classes)
	(setq import-groups (javaimp-add-to-import-groups class import-groups)))
      ;; insert all
      (if (or import-groups static-import-groups)
	  (progn
	    ;; prepare the position
	    (cond (old-imports-start
		   ;; when there were any imports, do not touch blank lines
		   ;; before imports
		   (goto-char old-imports-start))
		  ((re-search-forward "^\\s-*package\\s-" nil t)
		   ;; when there is a package statement, insert one or two
		   ;; blank lines after it
		   (when (= (forward-line) 1) (insert ?\n)) ;; last line?
		   (insert ?\n))
		  (t
		   ;; otherwise, start at the bob, insert one empty line
		   ;; after point
		   (goto-char (point-min))
		   (insert ?\n)
		   (backward-char)))
	    (javaimp-insert-import-groups import-groups nil)
	    (and import-groups static-import-groups (insert ?\n))
	    (javaimp-insert-import-groups static-import-groups t))
	(message "Nothing to organize")))))

;;;###autoload
(defun javaimp-invalidate-jar-classes-cache ()
  "Resets jar classes cache (debugging only)"
  (interactive)
  (setq javaimp-jar-classes-cache nil))

;;;###autoload
(defun javaimp-forget-all-visited-modules ()
  "Resets `javaimp-maven-root-modules' (debugging only)"
  (interactive)
  (setq javaimp-maven-root-modules nil))

;;;###autoload
(defun javaimp-reset ()
  "Resets all data (debugging only)"
  (interactive)
  (javaimp-forget-all-visited-modules)
  (javaimp-invalidate-jar-classes-cache))

;;;; ChangeLog:

;; 2015-04-02  Filipp Gunbin  <fgunbin@fastmail.fm>
;; 
;; 	packages/javaimp/javaimp.el: Support additional source directories.
;; 
;; 	* packages/javaimp/javaimp.el (javaimp-additional-source-dirs): New
;; 	defcustom.
;; 	(javaimp-make-mod): Add build-dir.
;; 	(javaimp-get-mod-build-dir): New defun.
;; 	(javaimp-maven-build-children): Use module build directory
;; 	(javaimp-get-module-classes): Add classes in 'additional-source-dirs' to
;; 	project classes.
;; 
;; 2015-03-30  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/javaimp/javaimp.el: Fix some compiler warnings
;; 
;; 2015-03-11  Filipp Gunbin  <fgunbin@fastmail.fm>
;; 
;; 	packages/javaimp/javaimp.el: use line-beginning-position instead of
;; 	forward-line
;; 
;; 2015-03-10  Filipp Gunbin  <fgunbin@fastmail.fm>
;; 
;; 	packages/javaimp/javaimp.el: replace kill-line with delete-region
;; 
;; 2014-12-08  Filipp Gunbin  <fgunbin@fastmail.fm>
;; 
;; 	minor fixes
;; 
;; 2014-10-23  Filipp Gunbin  <fgunbin@fastmail.fm>
;; 
;; 	packages/javaimp/javaimp.el: parent pom check fix
;; 
;; 2014-10-22  Filipp Gunbin  <fgunbin@fastmail.fm>
;; 
;; 	packages/javaimp/javaimp.el: added check for changes in parent pom
;; 
;; 2014-10-02  Filipp Gunbin  <fgunbin@fastmail.fm>
;; 
;; 	javaimp: doc fixes
;; 
;; 2014-09-30  Filipp Gunbin  <fgunbin@fastmail.fm>
;; 
;; 	javaimp: version 0.5
;; 
;; 2014-09-30  Filipp Gunbin  <fgunbin@fastmail.fm>
;; 
;; 	javaimp: fix for cases when maven doesn't add xml declaration
;; 
;; 2014-08-28  Filipp Gunbin  <fgunbin@fastmail.fm>
;; 
;; 	javaimp: fixed lexical-binding var placement
;; 
;; 2014-08-27  Filipp Gunbin  <fgunbin@fastmail.fm>
;; 
;; 	javaimp: converted to lexical binding, doc fixes, minor code fixes
;; 
;; 2014-07-10  Filipp Gunbin  <fgunbin@fastmail.fm>
;; 
;; 	* javaimp: New package.
;; 


(provide 'javaimp)

;;; javaimp.el ends here

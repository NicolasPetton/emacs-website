Allows to manage Java import statements in Maven projects.

  Quick start: customize `javaimp-import-group-alist', `javaimp-jdk-home'
and call `javaimp-maven-visit-root', then in a Java buffer visiting a
file under that module or one of its submodules call
`javaimp-organize-imports' or `javaimp-add-import'.  `javaimp-add-import'
will provide you a helpful completion, and the default value (the one
you'll get if you hit `M-n' in the minibuffer) is the symbol under point,
so usually it's enough to hit `M-n', then add some starting letters of a
package and hit `TAB'.  The module does not add all needed imports
automatically!  It only helps you to quickly add imports when stepping
through compilation errors.

If Maven failed, you can see its output in the buffer named by
`javaimp-debug-buf-name' (default is "*javaimp-debug*").

Contents of jar files and Maven project structures (pom.xml) are cached,
so usually only first command should take a considerable amount of time
to complete.  When it is detected that a particular jar or pom.xml file's
timestamp changed, it is re-read and cache is updated.

Details on variables.

  `javaimp-import-group-alist' defines the order of import statement
groups.  By default java.* and javax.* imports are assigned an order of
10, which is low, so it puts those imports at the beginning.  Your
project's imports typically should come after, so the sample config below
sets 80 for them.

 `javaimp-jdk-home' is a path for JDK.  It is used to scan JDK jars.
Usually you will need to set this.

 `javaimp-additional-source-dirs' is a list specifying directories where
additional (e.g. generated) source files reside.  Each directory is a
relative path from ${project.build.directory} project property value.

 `javaimp-mvn-program' defines path of the `mvn' program.  Use if it's
not on `exec-path'.

 `javaimp-cygpath-program' defines path of the `cygpath' program (applies
to Cygwin only, of course).  Use if it's not on `exec-path'.

 `javaimp-jar-program' defines path of the `jar' program.  Use if it's
not on `exec-path'.
 
Details on commands.

  `javaimp-maven-visit-root' is the first command you should issue to
use this module.  It reads the pom structure recursively and records
which files belong to which module.  Maven help:effective-pom command is
used to do that.

  `javaimp-organize-imports' groups import statement and writes those
group according to the value of `javaimp-import-group-alist'.  Imports
which are not matched by any regexp in that variable are assigned a
default order defined by `javaimp-import-default-order' (50 by default).

Sample setup (put this into your .emacs):

(require 'javaimp)

(add-to-list 'javaimp-import-group-alist '("\\`\\(ru\\.yota\\.\\|tv\\.okko\\.\\)" . 80))

(setq javaimp-jdk-home (getenv "JAVA_HOME"))
(setq javaimp-include-current-project-classes t)
(setq javaimp-additional-source-dirs '("generated-sources/thrift"))

(add-hook 'java-mode-hook
	  (lambda ()
	    (local-set-key "\C-ci" 'javaimp-add-import)
	    (local-set-key "\C-co" 'javaimp-organize-imports)))


TODO:

Support adding static imports by giving a prefix argument to
`javaimp-add-import'.
 
Use functions `cygwin-convert-file-name-from-windows' and
`cygwin-convert-file-name-to-windows' when they are available instead of
calling `cygpath'.  See
https://cygwin.com/ml/cygwin/2013-03/msg00228.html.
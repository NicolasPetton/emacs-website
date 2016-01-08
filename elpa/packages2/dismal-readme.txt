                             dismal-mode
                             Release 1.3 or later

                           Frank E. Ritter
                    <Frank.Ritter@nottingham.ac.uk>
<http://www.psychology.nottingham.ac.uk/staff/ritter/papers/dismal/dismal.html>

                              8-Nov-97

INTRODUCTION
------------

This is an introduction for dismal (Dis' Mode Ain't lotus), a major
mode in GNU-emacs that implements a spreadsheet (!), including how to
load, run, and use dismal and dismal spreadsheets.  Dismal provides
the basic spreadsheet functions.  Because it is based on GNU-Emacs it
offers several relatively novel features for a spreadsheet.  It is
designed to be keystroke driven rather than mouse/menu driven
(although it can be menu driven).  It is extensible, so that users can
write their own commands and functions, for example, to allow a
function cell to write to several nearby cells.  A ruler can be put up
that reflects the semantics of column names past the ones
automatically provided as letters.

It has some useful functions that implement the keystroke level model
of Card, Moran and Newell.  These were reported at CHI:

  Nichols, S., & Ritter, F. E. (1994).  A theoretically motivated tool
  for automatically generating command aliases.  Proceedings of CHI
  '95. 393-400.
  (http://www.psychology.nottingham.ac.uk/aigr/research/cmat/Chi95.html). 

For futher information, please see the dismal-manual.* files
accompanying the release, or 
http://www.psychology.nottingham.ac.uk/staff/ritter/papers/dismal/dismal.html


If you would like to contribute, there remain numerous problems with
dismal (noted in the todo list in dismal.el), but I use it at least
weekly and now know of at 20 users outside of Nottingham.

OBTAINING AND INSTALLING THE PACKAGE
------------------------------------

dismal-mode is available via anonymous ftp from host
(ftp.nottingham.ac.uk, but many machines don't know it, so you can use
the numbers 128.243.40.43, which is a specific ftp machine) in the
directory "pub/lpzfr" (From within ftp only the part of the tree
rooted at /usr/ftp is visible) and updated less frequently from the
Elisp archives at The OSU and
centro.soar.cs.cmu.edu:/afs/cs/project/soar/public/Soar6/.

There is a mirror site at ftp://cs.nyu.edu/pub/local/fox/dismal that
may be updated nightly.

There is a mirror site at
ftp://vpsyc.psychology.nottingham.ac.uk/pub/ritter/dismal that I
maintain.

The file is named "pub/lpzfr/dismal-VERSION.tar.gz" (so use
binary mode by issuing the "binary" command), where version gives you
the version of dismal that you are getting.

If you would like help with using anonymous ftp, feel free to send me
a message and I will explain the procedure.

To install the system:

1) Find a directory where you wish to install the package.  (At the
   University of Nottingham we currently put it in ~soar/emacs/utils/dismal.)
   Transfer dismal.tar.gz to your desired directory,
   cd to that directory, and do 

   gunzip dismal.tar.gz | tar xvf -

2) Execute "make" in that directory.  This will compile the .el files.  If
   GNU emacs at your site is not called "emacs", but something else (e.g.,
   some places use "gmacs"), then compile the .el files using the alternative
   command "make EMACS=xxxx" where "xxxx" is the name of your emacs program.
   There are a lot of compiler warnings generated in 19.28.  You may
   also have to change the compiler name in the makefile.

3) Please read the dismal.info files.  They are the manual.

4) At U-N, where dismal is installed in /psyc/lang/soar/utils/dismal/VERSION,
   users can set up their Emacs to use dismal by adding the following to
   their ~/.emacs files: 

   (load "/psyc/lang/soar/utils/dismal/VERSION/dismal-mode-defaults")

   To use dismal at your site, change the pathname to
   correspond to the location of the dismal files at your site.  More
   information about installing and using dismal are contained in the 
   online help, available within dismal as C-h m, 
   or in the info files themselves (dismal-manual.tex and
   dismal-manual.txt)
   

FILES
-----

The files included in the package are:

    COPYING              dismal.el
    Makefile             dismal-simple-menus.el
    README               dismal-metacolumn.el
    REFCARD              dismal-mouse-x.el
    dismal-manual.txt    dismal-mode-defaults.el
          

   *vectors.el
   *heaps.el            *rmatrix.el
   *simple-menu.el


The last group of files marked with "*" are not part of dismal proper,
but dismal depends on them.  It should be possible for later releases
of dismal to replace them with smarter and faster code.  Some of these
files are, in fact, gone already.

MISCELLANEOUS
-------------

Some features which are not fully implemented yet, but will eventually
be, are:

    use of Emacs 19's features (defsubs)
    the use of sparse matrices to represent the spreadsheet

Version 1.1 makes some use of defsubst and of fonts and menus

There are too many bugs or missing features to list them here.
Interested users should consult the dismal.el file.

If you would like to hear about updates and other such things or to
help maintain it, please let me know and I'll put you on the dismal
users mailing list.

<Frank.Ritter@nottingham.ac.uk>



(require (lib "xml.ss" "xml"))

(empty-tag-shorthand html-empty-tags)

(define title "PLT Anonymous CVS")

(define page
  `(html
    
    (head
     (title ,title))
    
    (body 
     ((bgcolor "white"))

     "This page has moved to "
     (a ((href "http://www.plt-scheme.org/anoncvs/"))
	"http://www.plt-scheme.org/anoncvs/")
     ".")))

#|

(define (plt-software name dir)
  `(a ((href ,(format "http://www.plt-scheme.org/software/~a"
		      dir)))
      ,name))

(define (section s)
  `((p)
    (table ((width "100%")) 
	   (tr (td ((bgcolor "#ccccff")) (font ((size "+1")) (b ,s)))))
    (p)))

(define (subsection s)
  `((p) (strong ,s) (p)))

(define (tt s)
  `(font ((color "green")) (tt ,s)))

(define (submod n s)
  `(,(tt n)
    ": "
    nbsp nbsp
    ,s))

(define page
  `(html
    
    (head
     (title ,title))
    
    (body 
     ((bgcolor "white"))

     (h1
      (img ((src "logo.gif")
	    (align "center")
	    (alt "[PLT logo]")))
      ,title)

     "The anonymous CVS server provides access to the source code for
PLT software (DrScheme, MzScheme, etc.) as it is being developed.  The
server is useful for those who want access to the latest and greatest
version of PLT software, either to take advantage of new features or
to help test the new code."

(p)

"To use the anonymous CVS server, you will need"

(ul
 (li "Experience using the regular PLT release.") 

 (li (a ((href "http://www.sourcegear.com/CVS"))
	"CVS client")
     " software and "
     (a ((href "http://www.loria.fr/cgi-bin/molli/wilma.cgi/doc"))
	"the know-how to use it")
     ".")

 (li "One of the platforms with "
     (a ((href "http://download.plt-scheme.org/binaries/pre-release/"))
	"pre-built binaries")
     ", or a C/C++ compiler:"
     (ul 
      (li "Unix: most any compiler")
      (li "Windows: MSVC or Cygwin (MzScheme only)")
      (li "Mac: CodeWarrior")))

 (li "Patience and proficiency for using unstable Scheme and C code."))

"If you lack any of these, consider downloading compiled binaries
or source code via the regular download page for "
,(plt-software "DrScheme" "drscheme/") ", "
,(plt-software "MzScheme" "mzscheme/") ", "
"or " ,(plt-software "other PLT software" "") "."

(p)

;------------------------------------------------------------

,@(section "Using the Repository")

(ul
 (li "Repository: " ,(tt "anoncvs.plt-scheme.org:/cvs"))
 (li "Pserver user: " ,(tt "anonymous"))
 (li "Password: " (em "empty"))
 (li "Module: " ,(tt "plt"))
 (li "Tag for reasonably stable code: " ,(tt "exp")
     nbsp nbsp nbsp nbsp nbsp
     "Tag for version 103 tree (old): " ,(tt "v103")))

"The following commands will checkout the latest reasonably stable version
of all repository files to your machine:"

(pre "  cvs -d :pserver:anonymous@anoncvs.plt-scheme.org:/cvs login")
(pre "  <hit Enter at the password prompt>")
(pre "  cvs -d :pserver:anonymous@anoncvs.plt-scheme.org:/cvs checkout -r exp plt")

(table (tr (td ((bgcolor "yellow")) (b "Always use the " nbsp (tt
"exp") nbsp " or " nbsp (tt "v103") nbsp " tag when checking out code."))))

(p)

"The " (tt "exp") " or " (tt "v103") 
" tag ensures that you get a reasonably stable set
of files. Versions more recent than the " (tt "exp") "-tagged version
are likely to be in the midst of changes, and therefore inconsistent."

(p)

"When updating with " (tt "cvs update") ", don't forget to use " (tt "-d") " to check for newly added directories.
(Use " (tt "-P") " to avoid now-empty directories.)"

;------------------------------------------------------------

,@(section "Repository Overview")

"The " (tt "plt") " module contains the following submodules
(directories):"

(ul
 (li ,@(submod "bin" "Unix scripts"))
 (li ,@(submod "collects" "Scheme code, organized into collections"))
 (li ,@(submod "man" "Unix man pages"))
 (li ,@(submod "notes" "release notes"))
 (li ,@(submod "src" "C code")
     (ul
      (li ,@(submod "mac" "Mac code"))
      (li ,@(submod "mred" "MrEd code"))
      (li ,@(submod "mzscheme" "MzScheme code"))
      (li ,@(submod "mysterx" "Windows MysterX code"))
      (li ,@(submod "srpersist" "SrPersist code"))
      (li ,@(submod "starter" "Windows program-launcher code"))
      (li ,@(submod "worksp" "Windows MSVC makefiles and projects"))
      (li ,@(submod "wxcommon" "MrEd code"))
      (li ,@(submod "wxmac" "Mac MrEd code"))
      (li ,@(submod "wxwindow" "Windows MrEd code"))
      (li ,@(submod "wxxt" "X MrEd code"))))
 (li ,@(submod "teachpack" "teaching support"))
 (li ,@(submod "tests" "some test suites")))

"Of course, when building under Windows (for example), the directories
for Unix and Mac aren't needed. Similarly, the MrEd or MysterX
directories are not needed to build only MzScheme."

(p)

"Note that documentation is " (strong "not") " available via the
repository. See below for information on obtaining and reading
pre-release documentation."

;------------------------------------------------------------

,@(section "Building and Using the Code")

,@(subsection "Building C Source")

"Compilation information is provided by README files in various " (tt
"plt/src") " directories. Start with " (tt "plt/src/README") "."

,@(subsection "Downloading Pre-Built Binaries")

"If a binary for your platform is available from "
(a ((href "http://download.plt-scheme.org/binaries/pre-release/"))
   "the pre-built binaries page")
", then you can download the binaries instead of building them.
The pre-built binaries are updated nightly by building with the "
(tt "exp") "-tagged code."

(p)

"For installation instructions, see the bottom of the pre-built 
download page for your platform."


,@(subsection "Updating Collection Files")

"After building and/or installing the binaries,
byte-code-compile the
Scheme sources by running Setup PLT (" (tt "setup-plt") " in
Unix). Whenever you update Scheme code in the " (tt "plt/collects") "
directory, run Setup PLT with the " (tt "-c") " or " (tt "--clean") " flag, 
in case a
module's exports have changed."

;------------------------------------------------------------

,@(section "Getting Updated Documentation")

"Neither the documentation nor its source is available via the CVS
server. Instead, Help Desk provides a mechanism for downloading
pre-release documentation from a special web site."

(p)

"Help Desk enables the special pre-release download feature when it
discovers a " (tt "CVS") " directory within the " (tt
"plt/collects/help") " directory."

(p)

"When using Help Desk the first time, you will get a page that says
\"SOMETHING IS WRONG\". The page provides a link for downloading the
main help pages from a web site. After downloading and installing the
main help pages, restart Help Desk. Afterwards, when you attempt to
read other parts documentation, you will get a download page for
installing new documentation (just like the regular release, except
that it downloads from a special pre-release site)."

(p)

"To update installed documentation, Help Desk provides a "
"\"refresh all manuals\""
" link on the \"Installed Manuals\" page. To reach the "
"\"Installed Manuals\" page, click the \"Manuals\" link on "
" Help Desk's initial page."

;------------------------------------------------------------

,@(section "Version Numbering")

"The version numbers for MzScheme and MrEd are always kept in sync. A
pre-release version number has the form N.M, where N is one less than the version
number for the next regular release, and M indicates the Mth " (tt
"exp") "-tagged version before the N release. For example, 200.8 means
\"the 8th checkpoint leading up to the release of version 201\"."

(p)

"The Scheme code implementing DrScheme evolves at its own pace, and it
has its own pre-release number, indicated by a suffix of the for dN
after the MrEd version number. Thus, DrScheme version 200.8d3 is the
3rd checkpoint of DrScheme running on MrEd version 200.8."

(p)

"A few other subsystems have their own version numbers, but not all of
them."

(p)

"The change logs can be found in " (tt "HISTORY") " files within " (tt
"plt/notes") ". For example, " (tt "plt/notes/mzscheme/HISTORY") "
records the changes made to MzScheme. Relatively detailed notes are
kept between regular releases, but then collapsed when the release
occurs."

)))

|#

(write-xml/content (xexpr->xml page))
(newline)

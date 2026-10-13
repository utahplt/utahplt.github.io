#lang racket/base
(require pkg-build
	 racket/file
	 file/untgz)

;; ----------------------------------------
;; Build packages

(define test-env
  (list (cons "DISPLAY" ":1")
	(cons "PLT_PKG_BUILD_SERVICE" "1")))

(parameterize ([current-directory "work"])
 (build-pkgs 
  #:snapshot-url "http://www.cs.utah.edu/plt/popl16/"
  #:pkg-catalogs (list "http://www.cs.utah.edu/plt/popl16/pkg-build-scopes-2015-10/archive/catalog/")

  #:installer-platform-name "{1} Racket | {3} Linux | {9} 64-bit x86_64 natipkg; built on Ubuntu 12.04 (Precise)"
  
  #:extra-packages '("main-distribution-test")

  #:vms (list
	 (vbox-vm
	  #:name "Ubuntu 14.04"
	  #:host "192.168.56.107"
	  #:env test-env
	  #:minimal-variant (vbox-vm #:name "Ubuntu 14.04 minimal"
				     #:host "192.168.56.117")))
  
  #:steps (steps-in 'download 'site)))

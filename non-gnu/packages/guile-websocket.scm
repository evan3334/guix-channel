(define-module (non-gnu packages guile-websocket)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages autotools))

(define-public guile-websocket
  (package
   (name "guile-websocket")
   (version "0.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "git://dthompson.us/guile-websocket.git")
                  (commit "d58d696")))
	    (file-name (git-file-name name version))
            (sha256
             (base32
              "10r8384frlyrljjdyzilrh8hzi60v9hisx4fxjs7rmg9g01cs77k"))))
   (build-system gnu-build-system)
   (arguments
    '(#:make-flags
      '("GUILE_AUTO_COMPILE=0")
      #:phases
      (modify-phases %standard-phases
	(add-after 'unpack 'update-guile-version
	  (lambda _
	    (substitute* "configure.ac"
	      (("2.0.9") "3.0.0"))
	    (substitute* "Makefile.am"
	      (("2.0") "3.0"))
	    #t))
	(add-after 'update-guile-version 'bootstrap
	  (lambda _ (zero? (system* "sh" "bootstrap")))))))
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)))
   (inputs
    `(("guile" ,guile-next)))
   (synopsis "Websocket server/client for Guile")
   (description "Guile-websocket provides an implementation of the
WebSocket protocol as defined by RFC 6455.")
   (home-page "https://git.dthompson.us/guile-websocket.git")
   (license license:lgpl3+)))

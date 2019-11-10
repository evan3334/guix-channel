(define-module (non-gnu packages td)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls)
  #:use-module ((guix licenses) #:prefix license:))

(define-public tdlib
  (package
   (name "tdlib")
   (version "1.5.0")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://github.com/tdlib/td.git")
		  (commit "b6a483d")))
	    (sha256
	     (base32
	      "1rqxdvzlryqln5jzj35cwz1fjwy4s8xq97p0wdnpzbfjpcalvrm5"))))
   (build-system cmake-build-system)
   (arguments '(#:tests? #f)) ;; disable tests, as many of them require things
                              ;; like internet access
   (inputs `(("openssl" ,openssl)
	     ("zlib" ,zlib)))
   (native-inputs `(("gperf" ,gperf)))
   (synopsis "Cross-platform library for building Telegram clients")
   (description "TDLib (Telegram Database library) is a cross-platform library
for building Telegram clients. It can be easily used from almost any programming
language.")
   (home-page "https://core.telegram.org/tdlib")
   (license license:boost1.0)))

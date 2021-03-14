(define-module (non-gnu packages c++-libs)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:))

(define-public argh
  (package
    (name "argh")
    (version "1.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/adishavit/argh.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "132vwg53bhwha4aqk3p0xmilwgli3488bn2wfjy15c5ha0w5md1s"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* _
                      (invoke "./argh_tests")
                      #t)))))
    (home-page "https://github.com/adishavit/argh/")
    (synopsis "Argh! A minimalist argument handler.")
    (description #f)
    (license license:bsd-3)))

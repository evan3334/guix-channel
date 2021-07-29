(define-module (updates packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-esup
  (let ((commit "5169dd7fc8765a7377b0ab93aa63b7f0f934689a")
        (revision "2"))
    (package
      (name "emacs-esup")
      (version (git-version "0.7.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jschaf/esup")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0mn9pffw7kzdzwv3jkhygdkmlqax9fsrbjznbck90ydiv095fmp6"))
         (patches (search-patches "emacs-esup-fix-error.patch"))))
      (build-system emacs-build-system)
      (native-inputs
       `(("emacs-noflet" ,emacs-noflet)
         ("emacs-undercover" ,emacs-undercover)
         ("emacs-buttercup" ,emacs-buttercup)))
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)))
      (arguments
     `(#:tests? #t
       #:test-command '("buttercup" "-L" ".")))
      (home-page "https://github.com/jschaf/esup")
      (synopsis "Emacs start up profiler")
      (description "Benchmark Emacs Startup time without ever leaving
your Emacs.")
      (license license:gpl2+))))

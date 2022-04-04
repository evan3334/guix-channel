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

(define-public emacs-lsp-java
  (let ((version "3.1")
        (commit "0968038b9aea52ef3bf499e597cf3800d913c059")
        (revision "1"))
    (package
      (name "emacs-lsp-java")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/emacs-lsp/lsp-java")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1rpssrv1avbfq47h23qiymzhzddcxgs77diwq3mavqkxkqrkj3vz"))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-dap-mode
             emacs-lsp-mode
             emacs-markdown-mode
             emacs-dash
             emacs-f
             emacs-ht
             emacs-request
             emacs-treemacs))
      (home-page "https://github.com/emacs-lsp/lsp-java/")
      (synopsis "Java support for lsp-mode")
      (description "Emacs Java IDE using Eclipse JDT Language Server.")
      (license license:gpl3+))))

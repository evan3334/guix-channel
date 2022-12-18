(define-module (updates packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages lua)
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

(define-public emacs-lua-mode
  (let ((version "20210802")
        (commit "d17a00ca50aee197cd017d573b83367eb241cc44")
        (revision "1"))
    (package
      (name "emacs-lua-mode")
      (version (git-version version revision commit))
      (home-page "https://github.com/immerrr/lua-mode/")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ycaz2jqms9pkfb052g64hdjd59zwshrww59rap0mqsmn5j3kfpw"))
         (snippet '(begin (delete-file "test/test-strings-and-comments.el")))))
      (build-system emacs-build-system)
      (arguments
       `(#:tests? #f))
      (native-inputs
       (list emacs-buttercup lua))
      (synopsis "Major mode for lua")
      (description
       "This Emacs package provides a mode for @uref{https://www.lua.org/,
Lua programming language}.")
      (license license:gpl3+))))

(define-public emacs-hledger-mode
  (let ((commit "07baa57017e1b7703560363a6e1948b630a5158d")
        (revision "1"))
    (package
      (name "emacs-hledger-mode")
      (version (git-version "20221116.2203" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/narendraj9/hledger-mode")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0ps6w2a060662c36nmjh0z9ln1gccgl9cvgynrw8m5811inxw4km"))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-popup
             emacs-async
             emacs-htmlize))
      (arguments
       '(#:include '("^[^/]+.el$")
         #:exclude '()))
      (home-page "https://github.com/narendraj9/hledger-mode")
      (synopsis "Mode for writing journal entries for hledger")
      (description
       "This major mode for Emacs enables writing and managing hledger
journal files.  It generates some useful reports along with some financial
ratios that can help you keep a check on your financial health for users of
the plaintext accounting system hledger.")
      (license license:gpl3))))

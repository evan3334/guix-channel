(define-module (non-gnu packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages vulkan)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-ein
  (package
    (name "emacs-ein")
    (version "20220513.1959")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://melpa.org/packages/ein-"
             version
             ".tar"))
       (sha256
        (base32
         "1a2lnfk43zp7bwyxsbz3dm6d554dwdkq2d6grfc365fqxvvcw4bl"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-websocket" ,emacs-websocket)
       ("emacs-anaphora" ,emacs-anaphora)
       ("emacs-request" ,emacs-request)
       ("emacs-deferred" ,emacs-deferred)
       ("emacs-polymode" ,emacs-polymode)
       ("emacs-dash" ,emacs-dash)
       ("emacs-with-editor" ,emacs-with-editor)))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page
     "https://github.com/millejoh/emacs-ipython-notebook")
    (synopsis "Jupyter notebook client in Emacs")
    (description
     "Emacs IPython Notebook (EIN) connects to instances of jupyter notebook.")
    (license license:gpl3+)))

(define-public emacs-company-glsl
  (package
    (name "emacs-company-glsl")
    (version "20210109.1403")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://melpa.org/packages/company-glsl-"
             version
             ".el"))
       (sha256
        (base32 "115x2fxw7g1y9vdv56wqnbn4481453qv2ai2s7cd4zyci7bagwbk"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-glslang-path
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let* ((glslang-path (assoc-ref inputs "glslang")))
                        (substitute* "company-glsl.el"
                          (("call-process \"" all)
                           (string-append all glslang-path "/")))))))))
    (propagated-inputs
     `(("emacs-company" ,emacs-company)
       ("emacs-glsl-mode" ,emacs-glsl-mode)))
    (inputs
     `(("glslang" ,glslang)))
    (home-page "https://github.com/guidoschmidt/company-glsl")
    (synopsis "Company completion support for GLSL")
    (description
     "Provides a company completion backend glslangValidator and filtered
lists provided by `glsl-mode'.")
    (license license:gpl3+)))

(define-public emacs-lsp-haskell
  (package
    (name "emacs-lsp-haskell")
    (version "20220809.2129")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emacs-lsp/lsp-haskell.git")
                    (commit "485c1148ce4d27030bb95b21c7289809294e7d31")))
              (sha256
               (base32
                "0ygyvam8h59bhx785rwf4hs30d95xk5kb48inr1gs4313qc2lil2"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-lsp-mode))
    (home-page "https://github.com/emacs-lsp/lsp-haskell")
    (synopsis "Haskell support for lsp-mode")
    (description "Haskell specific adapter for LSP mode")
    (license license:gpl3+)))

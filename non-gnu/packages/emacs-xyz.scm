(define-module (non-gnu packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages ocaml)
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

(define-public emacs-flycheck-hledger
  (package
    (name "emacs-flycheck-hledger")
    (version "20220715.1115")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/DamienCassou/flycheck-hledger.git")
                    (commit "88c275563c8a1fbc9c4dead285d53d1e7c266665")))
              (sha256
               (base32
                "0nihm80nfydiv0x199kqvf62qgkwxr7wrfqqgfav663qimbjmhz8"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-flycheck))
    (home-page "https://github.com/DamienCassou/flycheck-hledger/")
    (synopsis "Flycheck module to check hledger journals")
    (description
     "This package is a flycheck checker for hledger files.")
    (license license:gpl3+)))

(define-public emacs-gradle-mode
  (package
   (name "emacs-gradle-mode")
   (version "20150313.1905")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/jacobono/emacs-gradle-mode.git")
                  (commit "e4d665d5784ecda7ddfba015f07c69be3cfc45f2")))
            (sha256
             (base32
              "0xs2278gamzg0710bm1fkhjh1p75m2l1jcl98ldhyjhvaf9d0ysc"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-s))
   (home-page "https://github.com/jacobono/emacs-gradle-mode")
   (synopsis "Gradle integration with Emacs' compile")
   (description
    "Gradle integration into Emacs, through compile-mode.  see documentation on
https://github.com/jacobono/emacs-gradle-mode")
   (license #f)))

(define-public emacs-flycheck-ocaml
  (package
    (name "emacs-flycheck-ocaml")
    (version "20220730.542")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/flycheck/flycheck-ocaml.git")
                    (commit "77f8ddbd9bfc3a11957ac7ec7e45d5fa9179b192")))
              (sha256
               (base32
                "0ndqd5s43la6nyrzff7w4d7kb7ya77i0givi8p8cik4r8nfxwjnd"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-flycheck ocaml-merlin emacs-let-alist))
    (home-page "https://github.com/flycheck/flycheck-ocaml")
    (synopsis "Flycheck: OCaml support")
    (description
     "This Flycheck extension provides a new `ocaml-merlin syntax checker which uses
Merlin Mode (see URL `https://github.com/ocaml/merlin') to check OCaml buffers
for errors. # Setup Add the following to your init file: (with-eval-after-load
merlin ;; Disable Merlin's own error checking (setq merlin-error-after-save nil)
;; Enable Flycheck checker (flycheck-ocaml-setup)) (add-hook tuareg-mode-hook
#'merlin-mode) # Usage Just use Flycheck as usual in Tuareg Mode buffers.
Flycheck will automatically use the new `ocaml-merlin` syntax checker if Merlin
Mode is enabled and Merlin's own error checking (`merlin-error-after-save`) is
disabled.  If you enable Merlin's error checking with `M-x
merlin-toggle-view-errors` Flycheck will not use the `ocaml-merlin` syntax
checker anymore, to avoid duplicate and redundant error reporting.")
    (license #f)))

(define-public emacs-lsp-pyright
  (package
    (name "emacs-lsp-pyright")
    (version "20230225.1118")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emacs-lsp/lsp-pyright.git")
                    (commit "54a2acddfdd7c3d31cb804a042305a3c6e60cf81")))
              (sha256
               (base32
                "1256q00zsh4q4p3qx5jwih1j7j7nfgmwvv9m0bn6j588wj97aiy2"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-lsp-mode emacs-dash emacs-ht))
    (home-page "https://github.com/emacs-lsp/lsp-pyright")
    (synopsis "Python LSP client using Pyright")
    (description "Pyright language server.")
    (license license:gpl3+)))

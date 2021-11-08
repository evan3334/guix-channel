(define-module (non-gnu packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages vulkan)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-ein
  (package
    (name "emacs-ein")
    (version "20210522.1036")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://melpa.org/packages/ein-"
             version
             ".tar"))
       (sha256
        (base32
         "11v5pw42xsd7a2cnx77b6bj534mpi8yj0dj1qx0pq2ky9vi40pva"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-websocket" ,emacs-websocket)
       ("emacs-anaphora" ,emacs-anaphora)
       ("emacs-request" ,emacs-request)
       ("emacs-deferred" ,emacs-deferred)
       ("emacs-polymode" ,emacs-polymode)
       ("emacs-dash" ,emacs-dash)
       ("emacs-with-editor" ,emacs-with-editor)))
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

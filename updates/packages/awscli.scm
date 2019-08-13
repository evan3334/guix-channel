(define-module (updates packages awscli)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages groff)
  #:use-module ((guix licenses) #:prefix license:))

(define-public awscli
  (package
   (name "awscli")
   (version "1.16.216")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri name version))
     (sha256
      (base32
          "08mz6nx8zvy8mrwbasss4ji8cgxdf3c2sln6bwkbnw3nlr7rvw1c"))))
   (build-system python-build-system)
   (arguments
    ;; FIXME: The 'pypi' release does not contain tests.
    '(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'fix-reference-to-groff
          (lambda _
            (substitute* "awscli/help.py"
              (("if not self._exists_on_path\\('groff'\\):") "")
              (("raise ExecutableNotFoundError\\('groff'\\)") "")
              (("cmdline = \\['groff'")
               (string-append "cmdline = ['" (which "groff") "'")))
            #t)))))
   (propagated-inputs
    `(("python-colorama" ,python-colorama)
      ("python-botocore" ,python-botocore)
      ("python-s3transfer" ,python-s3transfer)
      ("python-docutils" ,python-docutils)
      ("python-pyyaml" ,python-pyyaml)
      ("python-rsa" ,python-rsa)))
   (inputs
    `(("groff" ,groff)))
   (home-page "https://aws.amazon.com/cli/")
   (synopsis "Command line client for AWS")
   (description "AWS CLI provides a unified command line interface to the
Amazon Web Services (AWS) API.")
   (license license:asl2.0)))

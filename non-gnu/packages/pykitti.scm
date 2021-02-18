(define-module (non-gnu packages pykitti)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science))

(define-public python-pykitti
  (package
    (name "python-pykitti")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pykitti" version))
       (sha256
        (base32
         "0bdrc7iwipx6yin4khm4b3ggghrax3ika9h5igjr5fvkf4f42mfa"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-pillow" ,python-pillow)))
    (arguments `(#:tests? #f))
    (home-page
     "https://github.com/utiasSTARS/pykitti")
    (synopsis
     "A minimal set of tools for working with the KITTI dataset in Python")
    (description
     "A minimal set of tools for working with the KITTI dataset in Python")
    (license license:expat)))

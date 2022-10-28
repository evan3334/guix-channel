(define-module (non-gnu packages python)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt))

(define-public python-lsp-jsonrpc
  (package
    (name "python-lsp-jsonrpc")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-lsp-jsonrpc" version))
        (sha256
          (base32 "1gb0fsamxndhplx25v8m0b3k7aknzy454fpa0qsqsqnv6c3igv3v"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-ujson" ,python-ujson)))
    (native-inputs
      `(("python-coverage" ,python-coverage)
        ("python-pycodestyle" ,python-pycodestyle)
        ("python-pyflakes" ,python-pyflakes)
        ("python-pylint" ,python-pylint)
        ("python-pytest" ,python-pytest)
        ("python-pytest-cov" ,python-pytest-cov)))
    (home-page "https://github.com/python-lsp/python-lsp-jsonrpc")
    (synopsis "JSON RPC 2.0 server library")
    (description "JSON RPC 2.0 server library")
    (license license:expat)))

(define-public python-lsp-server
  (package
    (name "python-lsp-server")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-lsp-server" version))
       (sha256
        (base32 "1qz07i140rrgbwzpx1m1kg8fn6bdwx0l1wdmipnilvq7zzsqyglc"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-jedi" ,python-jedi)
       ("python-lsp-jsonrpc" ,python-lsp-jsonrpc)
       ("python-pluggy" ,python-pluggy)
       ("python-setuptools" ,python-setuptools)
       ("python-ujson" ,python-ujson)
       ("python-autopep8" ,python-autopep8)
       ("python-flake8" ,python-flake8)
       ("python-mccabe" ,python-mccabe)
       ("python-pycodestyle" ,python-pycodestyle)
       ("python-pydocstyle" ,python-pydocstyle)
       ("python-pyflakes" ,python-pyflakes)
       ("python-pylint" ,python-pylint)
       ("python-rope" ,python-rope)
       ("python-yapf" ,python-yapf)))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-flaky" ,python-flaky)
       ("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-pylint" ,python-pylint)
       ("python-pyqt5" ,python-qtpy)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)))
    (home-page "https://github.com/python-lsp/python-lsp-server")
    (synopsis "Python Language Server for the Language Server Protocol")
    (description "Python Language Server for the Language Server Protocol")
    (license license:expat)))

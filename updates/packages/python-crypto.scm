(define-module (updates packages python-crypto)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz))

(define-public python-txtorcon-23
  (package
    (name "python-txtorcon")
    (version "23.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "txtorcon" version))
        (sha256
         (base32
          "09a3k4g90pvs0q006ighka7xic39nnnk9bfrka23g4b8cynzy982"))))
    (build-system python-build-system)
    (arguments
      ;; The tests fail immediately due to a missing file. Reported upstream:
      ;; <https://github.com/meejah/txtorcon/issues/330>
     `(#:tests? #f))
    (propagated-inputs
     (list python-automat
           python-idna
           python-incremental
           python-pyopenssl
           python-service-identity
           python-twisted
           python-zope-interface))
    (home-page "https://github.com/meejah/txtorcon")
    (synopsis "Twisted-based Tor controller client")
    (description "This package provides a Twisted-based Tor controller client,
with state-tracking and configuration abstractions.")
    (license license:expat)))

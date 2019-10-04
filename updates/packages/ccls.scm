(define-module (updates packages ccls)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages web)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages llvm))

(define-public ccls-tagged
  (package
   (name "ccls")
   (version "0.20190823.3")
   (home-page "https://github.com/MaskRay/ccls")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/MaskRay/ccls/archive/"
                  version ".tar.gz"))
            (sha256
             (base32
              "0l4rhfb2z5p60759r6b5inj6kbhydgb80y5g7cwmjxzja6308kfv"))
            (file-name (string-append name "-" version ".tar.gz"))))
   (build-system cmake-build-system)
   (arguments
    `(#:configure-flags
      (list "-DUSE_SYSTEM_RAPIDJSON=ON"
            (string-append "-DCMAKE_CXX_FLAGS='-isystem "
                           (assoc-ref %build-inputs "gcc-toolchain")
                           "/include/c++'"))
      #:tests? #f))
   (native-inputs
    `(("rapidjson" ,rapidjson)
      ("gcc-toolchain" ,gcc-toolchain-7)))
   (inputs
    `(("clang" ,clang)
      ("ncurses" ,ncurses)))
   (synopsis "C/C++/ObjC language server.")
   (description "C/C++/ObjC language server.")
   (license license:expat)))

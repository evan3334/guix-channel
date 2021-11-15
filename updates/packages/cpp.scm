(define-module (updates packages cpp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages web))

(define-public ccls-next
  (package
    (name "ccls")
    (version "0.20210330")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MaskRay/ccls")
             (commit version)))
       (sha256
        (base32 "0zzdn7c7a244djqwcsd7rvgclcdacyf9d0vkxpfspl83k2554alf"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       ;; set CCLS_VERSION variable so version information is available to ccls
       ;; at runtime
       #:configure-flags (list ,(string-append "-DCCLS_VERSION=" version))))
    (inputs
     `(("rapidjson" ,rapidjson)))
    (native-inputs
     `(("clang" ,clang-10)
       ("llvm" ,llvm-10)))
    (home-page "https://github.com/MaskRay/ccls")
    (synopsis "C/C++/Objective-C language server")
    (description
     "@code{ccls} is a server implementing the Language Server Protocol (LSP)
for C, C++ and Objective-C languages.  It uses @code{clang} to perform static
code analysis and supports cross references, hierarchies, completion and
syntax highlighting.  @code{ccls} is derived from @code{cquery} which is not
maintained anymore.")
    (license license:asl2.0)))

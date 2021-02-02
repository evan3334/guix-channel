(define-module (updates packages olive)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages video)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages version-control))

(define-public pystring
  (package
    (name "pystring")
    (version "1.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/imageworks/pystring")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1w31pjiyshqgk6zd6m3ab3xfgb0ribi77r6fwrry2aw8w1adjknf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         ;; pystring does not have a configure script
         (delete 'configure)
         ;; Makefile attempts to install to /usr/lib; change this to the
         ;; proper output path
         (add-before 'install 'fix-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (substitute* "Makefile"
                 (("LIBDIR = /usr/lib")
                  (string-append "LIBDIR = " lib)))
               (mkdir-p lib)
               #t)))
         ;; Makefile does not install the header files for the library;
         ;; install them to an "include" directory under the proper output
         ;; path
         (add-after 'install 'install-header
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (inc (string-append out "/include/pystring")))
               (mkdir-p inc)
               (copy-file "pystring.h"
                          (string-append inc "/pystring.h"))
               #t))))))
    (native-inputs `(("libtool" ,libtool)))
    (home-page "https://github.com/imageworks/pystring")
    (synopsis "C++ functions matching the interface and behavior of Python
string methods")
    (description "Pystring is a collection of C++ functions which match the
interface and behavior of python's string class methods using std::string.
Implemented in C++, it does not require or make use of a python interpreter.
It provides convenience and familiarity for common string operations not
included in the standard C++ library.  It's also useful in environments where
both C++ and python are used.")
    (license license:bsd-3)))

(define-public opencolorio-2
  (package
    (name "opencolorio")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AcademySoftwareFoundation/OpenColorIO")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0888fca8wa5zdc6f7lmh1wi7ljw75ql0rlzaslk2zffd08ij0s38"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DOCIO_INSTALL_EXT_PACKAGES=NONE"
             (string-append "-Dpystring_ROOT="
                            (assoc-ref %build-inputs "pystring")))))
    (native-inputs
     `(("git" ,git)
       ("pkg-config" ,pkg-config)
       ("python" ,python)))
    (inputs
     `(("lcms" ,lcms)
       ("openexr" ,openexr)
       ("tinyxml" ,tinyxml)
       ("expat" ,expat)
       ("pybind11" ,pybind11)
       ("yaml-cpp" ,yaml-cpp)
       ("ilmbase" ,ilmbase)
       ("pystring" ,pystring)))
    (home-page "https://opencolorio.org")
    (synopsis "Color management for visual effects and animation")
    (description
     "OpenColorIO, or OCIO, is a complete color management solution geared
towards motion picture production, with an emphasis on visual effects and
computer animation.  It provides a straightforward and consistent user
experience across all supporting applications while allowing for sophisticated
back-end configuration options suitable for high-end production usage.

OCIO is compatible with the @acronym{ACES, Academy Color Encoding
Specification} and is @acronym{LUT, look-up table}-format agnostic, supporting
many popular formats.")
    (license (list license:zlib         ; src/OpenColorIO/md5
                   license:bsd-3))))    ; the rest

(define-public olive-editor
  (let ((version "0.2.0")
		(revision "1")
		(commit "19eabf283062ed0d046b8ce8dee8a14af7c6de31"))
    (package
      (name "olive-editor")
      (version (git-version version revision commit))
      (source (origin
				(method git-fetch)
				(uri (git-reference
					  (url "https://github.com/olive-editor/olive")
					  (commit commit)))
				(file-name (git-file-name name version))
				(sha256
				 (base32
				  "092nkhw9dws6969312n5fg05m5yx7nv0c77svbnr15hxl5bn3gkx"))))
      (build-system cmake-build-system)
      (arguments `(#:tests? #f))
      (inputs `(("qtbase" ,qtbase)
				("qtmultimedia" ,qtmultimedia)
				("qttools" ,qttools)
				("qtsvg" ,qtsvg)
				("ffmpeg" ,ffmpeg)
				("opencolorio" ,opencolorio-2)
				("openimageio" ,openimageio)
				("openexr" ,openexr)))
      (home-page "https://olivevideoeditor.org")
      (synopsis "Free non-linear video editor")
      (description #f)
      (license license:gpl3+))))

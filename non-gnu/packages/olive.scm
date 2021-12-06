(define-module (non-gnu packages olive)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
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
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web))

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

(define-public imath
  (package
    (name "imath")
    (version "3.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/AcademySoftwareFoundation/imath")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04c9y6lni2lyxyaapbwnwrp3mdb1s2kmlrslkrxh1vlj1yiz4g24"))))
    (build-system cmake-build-system)
    (arguments `(#:configure-flags '("-DPYTHON=ON")))
    (home-page "https://github.com/AcademySoftwareFoundation/Imath")
    (synopsis "C++ and Python library of 2D and 3D vector, matrix, and math operations for
computer graphics")
    (description "Imath is a basic, light-weight, and efficient C++ representation of 2D and 3D
vectors and matrices and other simple but useful mathematical objects, functions, and data
types common in computer graphics applications, including the “half” 16-bit floating-point
type.  Imath also includes optional python bindings for all types and functions, including
optimized implementations of vector and matrix arrays.")
    (license license:bsd-3)))

(define-public openexr-3
  (package
    (name "openexr")
    (version "3.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/AcademySoftwareFoundation/openexr")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pcd3rqdkl07fscnkxbxpy0h0qdzy5w5dl5k5mk36k6nizy2z8gx"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-test-tmpdir
             (lambda _
               (let* ((new-tmpdir (string-append (getcwd) "/test-tmp"))
                      (test-dir (string-append (getcwd) "/src/test/"))
                      (get-tmpdir-h (lambda (dir) (string-append test-dir dir "/tmpDir.h")))
                      (replace-files (list (get-tmpdir-h "OpenEXRFuzzTest")
                                           (get-tmpdir-h "OpenEXRTest")
                                           (get-tmpdir-h "OpenEXRUtilTest"))))
                 (mkdir-p new-tmpdir)
                 (map (lambda (file)
                        (substitute* file
                          (("/var/tmp") new-tmpdir)))
                      replace-files)
                 #t))))))
    (inputs `(("zlib" ,zlib)
              ("imath" ,imath)))
    (home-page "https://www.openexr.com")
    (synopsis "High-dynamic range file format library")
    (description "OpenEXR is a high dynamic-range (HDR) image file format developed for use in
computer imaging applications.  The IlmImf C++ libraries support storage of the \"EXR\" file
format for storing 16-bit floating-point images.")
    (license license:bsd-3)))

(define-public optional-lite
  (package
    (name "optional-lite")
    (version "3.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/martinmoene/optional-lite")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17g8jcwyc3skf8nr68yr0mbp4q2hankgidgdgjfkyl03f5ysy64v"))))
    (build-system cmake-build-system)
    (home-page "https://github.com/martinmoene/optional-lite")
    (synopsis "Header-only library providing a C++17-like optional type for C++98, C++11, and later")
    (description "optional lite is a single-file header-only library to represent optional
(nullable) objects and pass them by value.  The library aims to provide a C++17-like optional for use with C++98 and later.")
    (license license:boost1.0)))

(define-public cpp11-any
  (let ((version "0.0.0")
	(revision "1")
	(commit "f67bd5f8bbf7eb628bf38206d4ac5cb22438e6bb"))
    (package
      (name "cpp11-any")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/thelink2012/any")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ch6aq363dqx6bx7yjg4lkq346sy2acw1fnamg4jdqs4z8rqz9jz"))))
      (build-system cmake-build-system)
      (home-page "https://github.com/thelink2012/any")
      (synopsis "Implementation of std::experimental::any, including small object optimization, for C++11 compilers")
      (description "This is a implementation of N4562 std::experimental::any (merged into
C++17) for C++11 compilers.

It contains a small object optimization for objects with a size of up to 2 words (such as int,
float and std::shared_ptr). Storing those objects in the container will not trigger a dynamic
allocation.")
      (license license:boost1.0))))

;; TODO: actually get opentimelineio to work (had a lot of issues with bundled dependencies;
;; version of rapidjson that it wants is not the one packaged in Guix)

;;*  (define-public opentimelineio
;;*    (package
;;*      (name "opentimelineio")
;;*      (version "0.13")
;;*      (source (origin
;;*                (method git-fetch)
;;*                (uri (git-reference
;;*                      (url "https://github.com/PixarAnimationStudios/OpenTimelineIO")
;;*                      (commit (string-append "v" version))
;;*                      (recursive? #t)))
;;*                (file-name (git-file-name name version))
;;*                (sha256
;;*                 (base32
;;*                  "136an91rd4ympsg363rgd5h0a54ps072pjd9hbmrad685rcjmmzn"))
;;*                (modules '((guix build utils)))
;;*                (snippet
;;*                 '(begin
;;*                    (for-each delete-file-recursively
;;*                              '("src/deps/optional-lite"
;;*                                "src/deps/pybind11"
;;*                                "src/deps/rapidjson"
;;*                                "src/macosx-build"))
;;*                    #t))))
;;*      (build-system cmake-build-system)
;;*      (arguments
;;*       `(#:phases
;;*         (modify-phases %standard-phases
;;*           (add-after 'unpack 'fix-bundled-deps
;;*             (lambda* (#:key inputs #:allow-other-keys)
;;*               (let ((rapidjson (assoc-ref inputs "rapidjson"))
;;*                     (optional-lite (assoc-ref inputs "optional-lite")))
;;*                 (substitute* "src/opentimelineio/CMakeLists.txt"
;;*                   (("\\$\\{PROJECT_SOURCE_DIR\\}/src/deps/rapidjson") rapidjson)
;;*                   (("\\$\\{PROJECT_SOURCE_DIR\\}/src/deps/optional-lite") optional-lite))
;;*                 (substitute* "src/deps/CMakeLists.txt"
;;*                   (("add_subdirectory\\(pybind11\\)") "")
;;*                   (("DEPS_SUBMODULES any optional-lite pybind11 rapidjson")
;;*                    "DEPS_SUBMODULES any")
;;*                   (("optional-lite/")
;;*                    (string-append optional-lite "/")))
;;*                 #t))))))
;;*      (inputs `(("pyside2" ,python-pyside-2)
;;*                ("pybind11" ,pybind11)
;;*                ("rapidjson" ,rapidjson)
;;*                ("optional-lite" ,optional-lite)))
;;*      (home-page "https://opentimeline.io")
;;*      (synopsis "API and interchange format for editorial timeline information")
;;*      (description "OpenTimelineIO is an interchange format and API for editorial cut
;;*  information.  OTIO is not a container format for media, rather it contains information about
;;*  the order and length of cuts and references to external media.

;;*  OTIO includes both a file format and an API for manipulating that format.  It also includes a
;;*  plugin architecture for writing adapters to convert from/to existing editorial timeline
;;*  formats.  It also implements a dependency-less library for dealing strictly with time,
;;*  opentime.")
;;*      (license `((license:fsf-free "file://LICENSE.txt" "Modified Apache 2.0 License")
;;*                 license:boost1.0)))) ;; "any" library in src/deps is under BSL1.0

(define-public olive-editor
  (let ((version "0.2.0")
	(revision "2")
	(commit "2a80b858f9ecc58b23d5c5cecbc5058b0444acb0"))
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
		  "06izg7yb595swak2iki655g7ha35hhd8pnq0p6qnc8l4j6l8khvd"))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags '("-DCMAKE_CXX_FLAGS=-Wno-error=shadow")))
      (inputs `(("qtbase" ,qtbase-5)
		("qtmultimedia" ,qtmultimedia)
		("qttools" ,qttools)
		("qtsvg" ,qtsvg)
		("ffmpeg" ,ffmpeg)
		("opencolorio" ,opencolorio-2)
		("openimageio" ,openimageio)
                ("zlib" ,zlib)
                ("imath" ,imath)
		("openexr" ,openexr-3)
                ("ilmbase" ,ilmbase)))
      (native-inputs `(("pkg-config" ,pkg-config)))
      (home-page "https://olivevideoeditor.org")
      (synopsis "Free non-linear video editor")
      (description #f)
      (license license:gpl3+))))

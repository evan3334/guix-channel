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
    (synopsis "C++ and Python library of 2D and 3D vector, matrix, and math operations for computer graphics")
    (description "Imath is a basic, light-weight, and efficient C++ representation of 2D and 3D
vectors and matrices and other simple but useful mathematical objects, functions, and data
types common in computer graphics applications, including the “half” 16-bit floating-point
type.  Imath also includes optional python bindings for all types and functions, including optimized implementations of vector and matrix arrays.")
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
computer imaging applications.  The IlmImf C++ libraries support storage of the \"EXR\" file format for storing 16-bit floating-point images.")
    (license license:bsd-3)))

(define-public olive-editor
  (let ((version "0.2.0")
		(revision "1")
		(commit "2f4e9809ceeba5f43583e6c923774d990c49be59"))
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
				  "1sfzibjrmwr1b00a26zjkiq2wa3cbibq7bs0sq3a9z10h489w166"))))
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

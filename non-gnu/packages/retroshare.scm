(define-module (non-gnu packages retroshare)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xorg))

(define-public libupnp-1.6
  (package
    (name "libupnp")
    (version "1.6.25")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/pupnp/pupnp/releases/download"
                          "/release-" version "/libupnp-" version ".tar.bz2"))
      (sha256
       (base32
        "0hzsd7rvfa87b4hxg9yj4xhdfxx9sp09r9sqdl3mqhvmcyw018y5"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (build-system gnu-build-system)
    (arguments
     ;; The tests require a network device capable of multicasting which is
     ;; not available in the build environment. See
     ;; https://lists.gnu.org/archive/html/guix-devel/2015-01/msg00312.html.
     `(#:tests? #f
       #:configure-flags '("--disable-static")))
    (home-page "http://pupnp.sourceforge.net")
    (synopsis "Portable SDK for UPnP Devices")
    (description
     "The portable SDK for UPnP Devices (libupnp) provides developers with an
API and code for building control points, devices, and bridges that are
compliant with Version 1.0 of the Universal Plug and Play Device Architecture
Specification and support several operating systems like Linux, *BSD, Solaris
and others.")
    (license license:bsd-3)))

(define-public retroshare
  (let ((version-str "0.6.5")
	(commit "d7ecc26"))
    (package
      (name "retroshare")
      (version version-str)
      (source
       (origin
	 (method git-fetch)
	 (uri (git-reference
	       (url "https://github.com/RetroShare/RetroShare.git")
	       (commit (string-append "v" version-str))))
	 (file-name (git-file-name name version-str))
	 (sha256
	  (base32
	   "0sw6s75n6vz376bjphzf494qw10b1wm429qrklylb7prsrzpk8df"))))
      (build-system gnu-build-system)
      (native-inputs `(("git" ,git)
		       ("qttools" ,qttools)
		       ("pkg-config" ,pkg-config)))
      (inputs `(("qtbase" ,qtbase)
		("qtmultimedia" ,qtmultimedia)
		("qtx11extras" ,qtx11extras)
		("openssl" ,openssl)
		("zlib" ,zlib)
		("libupnp" ,libupnp-1.6)
		("speex" ,speex)
		("sqlcipher" ,sqlcipher)
		("libmicrohttpd" ,libmicrohttpd)
		("rapidjson" ,rapidjson)
		("xscrnsaver" ,libxscrnsaver)))
      (arguments
       `(#:phases
	 (modify-phases %standard-phases
	   (replace 'configure
	     (lambda* (#:key inputs outputs #:allow-other-keys)
	       (let* ((out (assoc-ref outputs "out"))
		      (lib (string-append out "/lib"))
		      (version-parts (string-split ,version-str #\.))
		      (major (list-ref version-parts 0))
		      (minor (list-ref version-parts 1))
		      (mini (list-ref version-parts 2))
		      (extra (string-append "-" ,commit)))
		      ;;*  (qtbase (assoc-ref inputs "qtbase"))
		      ;;*  (qmake (string-append qtbase "/bin/qmake"))
		      ;;*  (bash-minimal (assoc-ref inputs "bash"))
		      ;;*  (bash (string-append bash-minimal "/bin/bash")))
		 (invoke "qmake" (string-append "PREFIX=" out)
			 (string-append "LIB_DIR=" lib)
			 "CONFIG-=debug"
			 "CONFIG+=release"
			 (string-append "RS_MAJOR_VERSION=" major)
			 (string-append "RS_MINOR_VERSION=" minor)
			 (string-append "RS_MINI_VERSION=" mini)
			 (string-append "RS_EXTRA_VERSION=" extra))
		 #t)))
	   (add-before 'configure 'fix-patched-libupnp
	     (lambda _
	       (substitute* "libretroshare/src/libretroshare.pro"
		 (("DEFINES \\*= PATCHED_LIBUPNP") ""))
	       #t)))))
      (synopsis "Secure, decentralized communication platform")
      (description "RetroShare is a decentralized, private, secure, cross-platform,
communication toolkit.  RetroShare provides file sharing, chat, messages, forums, channels and
more.")
      (home-page "https://retroshare.cc")
      (license (license:fsf-free
		"https://raw.githubusercontent.com/RetroShare/RetroShare/master/.reuse/dep5"
		"Parts of the package are under different free software licenses.")))))

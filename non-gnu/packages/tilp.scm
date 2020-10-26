(define-module (non-gnu packages tilp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages autogen)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config))

(define-public libticonv
  (package
   (name "libticonv")
   (version "1.1.5")
   (source (origin
			(method url-fetch)
			(uri "https://www.ticalc.org/pub/unix/tilibs.tar.gz")
			(file-name (string-append name "-" version ".tar.gz"))
			(sha256
			 (base32
			  "07cfwwlidgx4fx88whnlch6y1342x16h15lkvkkdlp2y26sn2yxg"))))
   (build-system gnu-build-system)
   (arguments
	`(#:configure-flags '("--enable-iconv")
	  #:phases
	  (modify-phases %standard-phases
		;; For all of the TiLP libraries, the source tarball is actually some
		;; kind of "meta-tarball" that contains the source tarballs for all of
		;; the TiLP libraries. This alternate unpack phase will unpack the
		;; "meta-tarball" first, then unpack the tarball we want.
		(replace 'unpack
		  (lambda* (#:key source #:allow-other-keys)
			(let* ((basename (string-append ,name "-" ,version))
				   (tarball-name (string-append basename ".tar.bz2")))
			  (invoke "tar" "-x" (string-append "tilibs2/" tarball-name)
					  "-f" source "--strip-components=1")
			  (invoke "tar" "xf" tarball-name)
			  (delete-file tarball-name)
			  (chdir basename)
			  #t))))))
   (native-inputs `(("tar" ,tar)
					("autoconf" ,autoconf)
					("automake" ,automake)
					("libtool" ,libtool)
					("pkg-config" ,pkg-config)))
   (inputs `(("glib" ,glib)))
   (home-page "http://lpg.ticalc.org/prj_tilp/")
   (synopsis "Linking program for Texas Instruments graphing calculators")
   (description "TiLP is a linking program for Texas Instruments' graphing calculators. TiLP works on several platforms (UNIX, Windows, Macintosh). It can handle any TI calculator (from TI73 to V200) with any link cable.")
   (license license:gpl2+)))

(define-public libticables
  (package
    (name "libticables")
    (version "1.3.5")
    (source (origin
			(method url-fetch)
			(uri "https://www.ticalc.org/pub/unix/tilibs.tar.gz")
			(file-name (string-append name "-" version ".tar.gz"))
			(sha256
			 (base32
			  "07cfwwlidgx4fx88whnlch6y1342x16h15lkvkkdlp2y26sn2yxg"))))
    (build-system gnu-build-system)
	(arguments
	 `(#:configure-flags '("--enable-libusb10")
	   #:phases
	   (modify-phases %standard-phases
		 ;; For all of the TiLP libraries, the source tarball is actually some
		 ;; kind of "meta-tarball" that contains the source tarballs for all of
		 ;; the TiLP libraries. This alternate unpack phase will unpack the
		 ;; "meta-tarball" first, then unpack the tarball we want.
		 (replace 'unpack
		   (lambda* (#:key source #:allow-other-keys)
			 (let* ((basename (string-append ,name "2-" ,version))
					(tarball-name (string-append basename ".tar.bz2")))
			   (invoke "tar" "-x" (string-append "tilibs2/" tarball-name)
					   "-f" source "--strip-components=1")
			   (invoke "tar" "xf" tarball-name)
			   (delete-file tarball-name)
			   (chdir basename)
			   #t))))))
	(native-inputs `(("tar" ,tar)
					 ("autoconf" ,autoconf)
					 ("automake" ,automake)
					 ("libtool" ,libtool)
					 ("pkg-config" ,pkg-config)
					 ("gettext" ,gnu-gettext)))
    (inputs `(("glib" ,glib)
			  ("libusb" ,libusb)))
	(home-page "http://lpg.ticalc.org/prj_tilp/")
	(synopsis "Linking program for Texas Instruments graphing calculators")
	(description "TiLP is a linking program for Texas Instruments' graphing calculators. TiLP works on several platforms (UNIX, Windows, Macintosh). It can handle any TI calculator (from TI73 to V200) with any link cable.")
    (license license:gpl2+)))

(define-public libtifiles
  (package
    (name "libtifiles")
    (version "1.1.7")
	(source (origin
			  (method url-fetch)
			  (uri "https://www.ticalc.org/pub/unix/tilibs.tar.gz")
			  (file-name (string-append name "-" version ".tar.gz"))
			  (sha256
			   (base32
				"07cfwwlidgx4fx88whnlch6y1342x16h15lkvkkdlp2y26sn2yxg"))))
    (build-system gnu-build-system)
	(arguments
	 `(#:phases
	   (modify-phases %standard-phases
		 ;; For all of the TiLP libraries, the source tarball is actually some
		 ;; kind of "meta-tarball" that contains the source tarballs for all of
		 ;; the TiLP libraries. This alternate unpack phase will unpack the
		 ;; "meta-tarball" first, then unpack the tarball we want.
		 (replace 'unpack
		   (lambda* (#:key source #:allow-other-keys)
			 (let* ((basename (string-append ,name "2-" ,version))
					(tarball-name (string-append basename ".tar.bz2")))
			   (invoke "tar" "-x" (string-append "tilibs2/" tarball-name)
					   "-f" source "--strip-components=1")
			   (invoke "tar" "xf" tarball-name)
			   (delete-file tarball-name)
			   (chdir basename)
			   #t))))))
	(propagated-inputs `(("libarchive" ,libarchive)))
	(native-inputs `(("tar" ,tar)
					 ("autoconf" ,autoconf)
					 ("automake" ,automake)
					 ("libtool" ,libtool)
					 ("pkg-config" ,pkg-config)
					 ("gettext" ,gnu-gettext)))
	(inputs `(("glib" ,glib)
			  ("libticonv" ,libticonv)))
	(home-page "http://lpg.ticalc.org/prj_tilp/")
	(synopsis "Linking program for Texas Instruments graphing calculators")
	(description "TiLP is a linking program for Texas Instruments' graphing calculators. TiLP works on several platforms (UNIX, Windows, Macintosh). It can handle any TI calculator (from TI73 to V200) with any link cable.")
    (license license:gpl2+)))

(define-public libticalcs
  (package
    (name "libticalcs")
    (version "1.1.9")
	(source (origin
			  (method url-fetch)
			  (uri "https://www.ticalc.org/pub/unix/tilibs.tar.gz")
			  (file-name (string-append name "-" version ".tar.gz"))
			  (sha256
			   (base32
				"07cfwwlidgx4fx88whnlch6y1342x16h15lkvkkdlp2y26sn2yxg"))))
    (build-system gnu-build-system)
	(arguments
	 `(#:configure-flags '()
	   #:phases
	   (modify-phases %standard-phases
		 ;; For all of the TiLP libraries, the source tarball is actually some
		 ;; kind of "meta-tarball" that contains the source tarballs for all of
		 ;; the TiLP libraries. This alternate unpack phase will unpack the
		 ;; "meta-tarball" first, then unpack the tarball we want.
		 (replace 'unpack
		   (lambda* (#:key source #:allow-other-keys)
			 (let* ((basename (string-append ,name "2-" ,version))
					(tarball-name (string-append basename ".tar.bz2")))
			   (invoke "tar" "-x" (string-append "tilibs2/" tarball-name)
					   "-f" source "--strip-components=1")
			   (invoke "tar" "xf" tarball-name)
			   (delete-file tarball-name)
			   (chdir basename)
			   #t))))))
	(native-inputs `(("tar" ,tar)
					 ("autoconf" ,autoconf)
					 ("automake" ,automake)
					 ("libtool" ,libtool)
					 ("pkg-config" ,pkg-config)
					 ("gettext" ,gnu-gettext)))
	(inputs `(("glib" ,glib)
			  ("libticables" ,libticables)
			  ("libticonv" ,libticonv)
			  ("libtifiles" ,libtifiles)))
	(home-page "http://lpg.ticalc.org/prj_tilp/")
	(synopsis "Linking program for Texas Instruments graphing calculators")
	(description "TiLP is a linking program for Texas Instruments' graphing calculators. TiLP works on several platforms (UNIX, Windows, Macintosh). It can handle any TI calculator (from TI73 to V200) with any link cable.")
    (license license:gpl2+)))

(define-public tilp
  (package
    (name "tilp")
    (version "1.18")
    (source (origin
			  (method url-fetch)
			  (uri "https://www.ticalc.org/pub/unix/tilp.tar.gz")
			  (file-name (string-append name "-" version ".tar.gz"))
			  (sha256
			   (base32
				"1mww2pjzvlbnjp2z57qf465nilfjmqi451marhc9ikmvzpvk9a3b"))
			  (patches (search-patches "tilp-remove-kde.patch"))))
    (build-system gnu-build-system)
	(arguments
	 `(#:configure-flags '()
	   ;;*  #:phases
	   ;;*  (modify-phases %standard-phases
	   ;;*  	 (add-before 'bootstrap 'patch-autoconf
	   ;;*  	   (lambda _
	   ;;*  		 (substitute* "configure.ac"
	   ;;*  		   (("AC_PATH_KDE") "# AC_PATH_KDE"))
	   ;;*  		 #t)))))
	   ))
	(native-inputs `(("autoconf" ,autoconf)
					 ("automake" ,automake)
					 ("autogen" ,autogen)
					 ("libtool" ,libtool)
					 ("intltool" ,intltool)
					 ("pkg-config" ,pkg-config)
					 ("gettext" ,gnu-gettext)))
	(inputs `(("libticables" ,libticables)
			  ("libticonv" ,libticonv)
			  ("libtifiles" ,libtifiles)
			  ("libticalcs" ,libticalcs)
			  ("glib" ,glib)
			  ("gtk+" ,gtk+)
			  ("libglade" ,libglade)))
	(home-page "http://lpg.ticalc.org/prj_tilp/")
	(synopsis "Linking program for Texas Instruments graphing calculators")
	(description "TiLP is a linking program for Texas Instruments' graphing calculators. TiLP works on several platforms (UNIX, Windows, Macintosh). It can handle any TI calculator (from TI73 to V200) with any link cable.")
    (license license:gpl2+)))

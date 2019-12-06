(define-module (non-gnu packages matrix)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages qt))

;; -------------------------
;; Nheko-Reborn dependencies
;; -------------------------

;; libolm
(define-public libolm-3.1.4
  (package
   (name "libolm")
   (version "3.1.4")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://gitlab.matrix.org/matrix-org/olm.git")
		  (commit "67535953")))
	    (sha256
	     (base32
	      "06s7rw4a9vn35wzz7chxn54mp0sjgbpv2bzz9lq0g4hnzw33cjbi"))))
   (build-system cmake-build-system)
   (arguments '(#:tests? #f)) ;; FIXME: add an actual check phase
   (synopsis "Implementation of the olm and megolm cryptographic ratchets")
   (description "An implementation of the Double Ratchet cryptographic ratchet 
described by https://whispersystems.org/docs/specifications/doubleratchet/, written 
in C and C++11 and exposed as a C API.")
   (home-page "https://gitlab.matrix.org/matrix-org/olm")
   (license license:asl2.0)))

(define-public libolm-2.2.2
  (package
   (name "libolm")
   (version "2.2.2")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://gitlab.matrix.org/matrix-org/olm.git")
		  (commit "77eaaa3d")))
	    (sha256
	     (base32
	      "0l51xhc4qailvbdg799znxsrnk8xp0glj4vhsrza4fgz1a2sdqyh"))))
   (build-system gnu-build-system)
   (arguments '(#:tests?
		#f       ;; FIXME: add an actual check phase
		#:phases
		(modify-phases %standard-phases
		  (delete 'configure))
		#:make-flags
		`(,(string-append "PREFIX="
				  (assoc-ref %outputs "out")))))
   (synopsis "Implementation of the olm and megolm cryptographic ratchets")
   (description "An implementation of the Double Ratchet cryptographic ratchet 
described by https://whispersystems.org/docs/specifications/doubleratchet/, written 
in C and C++11 and exposed as a C API.")
   (home-page "https://gitlab.matrix.org/matrix-org/olm")
   (license license:asl2.0)))

;; mtxclient
(define-public mtxclient
  (package
   (name "mtxclient")
   (version "0.2.1")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://github.com/Nheko-Reborn/mtxclient.git")
		  (commit "975ce89")))
	    (sha256
	     (base32
	      "0pycznrvj57ff6gbwfn1xj943d2dr4vadl79hii1z16gn0nzxpmj"))))
   (build-system cmake-build-system)
   (arguments '(#:tests? #f ;; FIXME: add an actual check phase
		#:build-type
		"Debug")) 
   (native-inputs `(("pkg-config" ,pkg-config)))
   (inputs `(("boost" ,boost)
	     ("openssl" ,openssl)
	     ("gtest" ,googletest)
	     ("libsodium" ,libsodium)
	     ("libolm" ,libolm-2.2.2)
	     ("zlib" ,zlib)
	     ("nlohmann_json" ,json-modern-cxx)
	     ("spdlog" ,spdlog)))
   (synopsis "Client API library for Matrix, built on top of Boost.Asio")
   (description "Client API library for the Matrix protocol, built on top of Boost.Asio.")
   (home-page "https://github.com/Nheko-Reborn/mtxclient")
   (license license:expat)))

;; tweeny
(define-public tweeny
  (package
    (name "tweeny")
    (version "3")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/mobius3/tweeny.git")
		    (commit "a8e49e7")))
	      (sha256
	       (base32
		"1adm4c17pi7xf3kf6sjyxibz5rdg1ka236p72xsm6js4j9gzlbp4"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))
    (synopsis "A modern C++ tweening library")
    (description "Tweeny is an inbetweening library designed for the creation of complex
animations for games and other beautiful interactive software. It leverages features of modern
C++ to empower developers with an intuitive API for declaring tweenings of any type of value,
as long as they support arithmetic operations.")
    (home-page "https://mobius3.github.io/tweeny/")
    (license license:expat)))

;; lmdbxx
(define-public lmdbxx
  (package
    (name "lmdbxx")
    (version "0.9.14.1")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/drycpp/lmdbxx.git")
		    (commit "0b43ca8")))
	      (sha256
	       (base32
		"1whsc5cybf9rmgyaj6qjji03fv5jbgcgygp956s3835b9f9cjg1n"))))
    (build-system gnu-build-system)
    (inputs `(("lmdb" ,lmdb)))
    (arguments '(#:make-flags
		 `(,(string-append "PREFIX="
				   (assoc-ref %outputs "out")))
		 #:phases
		 (modify-phases %standard-phases
		   (delete 'configure))))
    (synopsis "C++11 wrapper for the LMDB embedded B+ tree database library")
    (description "This is a comprehensive C++ wrapper for the LMDB embedded database library,
offering both an error-checked procedural interface and an object-oriented resource interface
with RAII semantics.")
    (home-page "http://http://lmdbxx.sourceforge.net/")
    (license license:unlicense)))

(define-public nheko
  (package
    (name "nheko")
    (version "0.6.4")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/Nheko-Reborn/nheko.git")
		    (commit "1b34b53")))
	      (sha256
	       (base32
		"19dkc98l1q4070v6mli4ybqn0ip0za607w39hjf0x8rqdxq45iwm"))))
    (build-system cmake-build-system)
    (inputs `(("qt-base" ,qtbase)
	      ("qt-svg" ,qtsvg)
	      ("qt-multimedia" ,qtmultimedia)
	      ("qt-tools" ,qttools)
	      ("lmdb" ,lmdb)
	      ("lmdbxx" ,lmdbxx)
	      ("boost" ,boost)
	      ("zlib" ,zlib)
	      ("openssl" ,openssl)
	      ("matrixclient" ,mtxclient)
	      ("libolm" ,libolm-2.2.2)
	      ("spdlog" ,spdlog)
	      ("nlohmann_json" ,json-modern-cxx)
	      ("cmark" ,cmark)
	      ("tweeny" ,tweeny)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments '(#:tests? #f ;; FIXME: add an actual check phase
		 #:build-type
		 "Release"
		 #:configure-flags
		 `(,(string-append "-DLMDBXX_INCLUDE_DIR="
				   (assoc-ref %build-inputs "lmdbxx")
				   "/include"))))
    (synopsis "Desktop client for Matrix using Qt and C++14.")
    (description "Desktop client for Matrix using Qt and C++14. The motivation behind the
project is to provide a native desktop app for Matrix that feels more like a mainstream chat
app (Riot, Telegram etc) and less like an IRC client.")
    (home-page "https://github.com/Nheko-Reborn/nheko")
    (license license:gpl3)))

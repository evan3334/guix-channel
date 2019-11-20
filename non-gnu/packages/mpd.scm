(define-module (non-gnu packages mpd)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:prefix license:))

(define-public ashuffle
  (package
   (name "ashuffle")
   (version "2.0.2")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://github.com/joshkunz/ashuffle.git")
		  (commit "6bea04b")))
	    (sha256
	     (base32
	      "11aa95cg0yca2m2d00sar6wr14g3lc7cfm9bin1h7lk7asdm8azp"))))
   (build-system meson-build-system)
   (inputs `(("libmpdclient" ,libmpdclient)))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (synopsis "Automatic library-wide shuffle for mpd")
   (description "ashuffle is an application for automatically shuffling your MPD
library in a similar way to a more standard music player's \"shuffle library\"
feature. ashuffle works like any other MPD client, and can be used alongside
your other MPD frontends.")
   (home-page "https://github.com/joshkunz/ashuffle")
   (license license:expat)))

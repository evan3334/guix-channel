(define-module (non-gnu packages sdr)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config))

(define-public rtl-sdr
  (package
   (name "rtl-sdr")
   (version "0.6.0")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "git://git.osmocom.org/rtl-sdr.git")
		  (commit "1f0eafe")))
	    (sha256
	     (base32
	      "0lmvsnb4xw4hmz6zs0z5ilsah5hjz29g1s0050n59fllskqr3b8k"))))
   (build-system cmake-build-system)
   (arguments '(#:tests? #f)) ;; No tests for this package
   (inputs `(("libusb" ,libusb)))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (synopsis "Software to turn the RTL2832U into a SDR receiver")
   (description "Turns your Realtek RTL2832 based DVB dongle into a SDR receiver.")
   (home-page "https://osmocom.org/projects/rtl-sdr/wiki")
   (license license:gpl2)))

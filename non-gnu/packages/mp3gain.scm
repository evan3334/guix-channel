(define-module (non-gnu packages mp3gain)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages compression)
  #:use-module (ice-9 string-fun))

(define-public mp3gain
  (package
   (name "mp3gain")
   (version "1.6.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "https://sourceforge.net/projects/mp3gain/files/mp3gain/"
	   version "/mp3gain-"
	   (string-replace-substring version "." "_")
	   "-src.zip/download"))
     (file-name (string-append name "-" version ".zip"))
     (sha256
      (base32
       "0varr6y7k8zarr56b42r0ad9g3brhn5vv3xjg1c0v19jxwr4gh2w"))))
   (build-system gnu-build-system)
   (arguments
    '(#:make-flags
      '("CC=gcc")
      ;; No tests
      #:tests? #f 
      #:phases
      (modify-phases
	  %standard-phases
	;; The zip file is weird and contains all the sources right in the root
	;; directory. Make a directory and unpack the sources there instead.
	(replace 'unpack
	  (lambda* (#:key source #:allow-other-keys)
	    (let ((basename (string-drop-right source 4)))
	      (mkdir basename)
	      (chdir basename)
	      (invoke "unzip" source))
	    #t))
	;; Replace hardcoded installation path
	(add-before 'install 'fix-install-path
	  (lambda* (#:key outputs #:allow-other-keys)
	    (let* ((output-path (assoc-ref outputs "out"))
		   (bin-path (string-append output-path "/bin")))
	      (mkdir-p bin-path)
	      (substitute* "Makefile"
		(("/usr/local/bin") bin-path)))
	    #t))
	;; No configure step
	(delete 'configure))))
   (native-inputs `(("unzip" ,unzip)))
   (inputs `(("mpg123" ,mpg123)))
   (synopsis "Analyzes and adjusts the volume of MP3 files")
   (description "MP3Gain analyzes and losslessly adjusts mp3 files to a \
specified target volume. It does not simply do peak amplitude normalization. \
Instead, it performs statistical analysis to determine how loud the file \
actually sounds to the human ear.")
   (home-page "https://sourceforge.net/projects/mp3gain")
   (license license:lgpl2.1)))


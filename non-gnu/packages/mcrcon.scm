(define-module (non-gnu packages mcrcon)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public mcrcon
  (package
    (name "mcrcon")
    (version "0.7.1")
    (source (origin
			  (method git-fetch)
			  (uri (git-reference
					(url "https://github.com/Tiiffi/mcrcon")
					(commit (string-append "v" version))))
			  (file-name (git-file-name name version))
			  (sha256
			   (base32
				"004h1glagyw1mic1k461bky0w2fxdfhrhcqjzr9wp6gvyy9s8bix"))))
    (build-system gnu-build-system)
	(arguments
	 `(#:tests? #f
	   #:phases
	   (modify-phases %standard-phases
		 (delete 'configure)
		 (add-before 'build 'setenv
		   (lambda* (#:key outputs #:allow-other-keys)
			 (let ((out (assoc-ref outputs "out")))
			   (setenv "PREFIX" out)
			   #t))))))
    (home-page "https://github.com/Tiiffi/mcrcon")
    (synopsis "Rcon client for Minecraft")
    (description "mcrcon is a console based Minecraft rcon client for remote administration and
server maintenance scripts.")
    (license license:zlib)))

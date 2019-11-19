(define-module (updates packages stumpwm)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages texinfo)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-19))

(define-public stumpwm-master
  (let ((commit "5adc827")
        (revision "2"))
    (package
     (name "stumpwm")
     (version (git-version "18.11" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/stumpwm/stumpwm.git")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "1kw1s0xw3ggaz7cfs9rh7v1cai4xx7c7cigiwdss5vv5b7vs0zmj"))))
     (build-system asdf-build-system/sbcl)
     (native-inputs `(("fiasco" ,sbcl-fiasco)
		      ("texinfo" ,texinfo)))
     (inputs `(("cl-ppcre" ,sbcl-cl-ppcre)
	       ("clx" ,sbcl-clx-master)
	       ("alexandria" ,sbcl-alexandria)))
     (outputs '("out" "lib"))
     (arguments
      '(#:asd-system-name "stumpwm"
	#:phases
	(modify-phases
	 %standard-phases
	 (add-after 'create-symlinks 'build-program
		    (lambda* (#:key outputs #:allow-other-keys)
		      (build-program
		       (string-append (assoc-ref outputs "out") "/bin/stumpwm")
		       outputs
		       #:entry-program '((stumpwm:stumpwm) 0))))
	 (add-after 'build-program 'create-desktop-file
		    (lambda* (#:key outputs #:allow-other-keys)
		      (let* ((out (assoc-ref outputs "out"))
			     (xsessions (string-append out "/share/xsessions")))
			(mkdir-p xsessions)
			(call-with-output-file
			    (string-append xsessions "/stumpwm-master.desktop")
			  (lambda (file)
			    (format file
				    "[Desktop Entry]~@
                                                   Name=stumpwm-master~@
                                                   Comment=The Stump Window Manager~@
                                                   Exec=~a/bin/stumpwm~@
                                                   TryExec=~@*~a/bin/stumpwm~@
                                                   Icon=~@
                                                   Type=Application~%"
				    out)))
			#t)))
	 (add-after 'install 'install-manual
		    (lambda* (#:key outputs #:allow-other-keys)
		      ;; The proper way to the manual is bootstrapping a full autotools
		      ;; build system and running ‘./configure && make stumpwm.info’ to
		      ;; do some macro substitution.  We can get away with much less.
		      (let* ((out  (assoc-ref outputs "out"))
			     (info (string-append out "/share/info")))
			(invoke "makeinfo" "stumpwm.texi.in")
			(install-file "stumpwm.info" info)
			#t))))))
     (synopsis "Window manager written in Common Lisp")
     (description "Stumpwm is a window manager written entirely in Common Lisp.
It attempts to be highly customizable while relying entirely on the keyboard
for input.  These design decisions reflect the growing popularity of
productive, customizable lisp based systems.")
     (home-page "https://github.com/stumpwm/stumpwm")
     (license license:gpl2+)
     (properties `((cl-source-variant . ,(delay cl-stumpwm)))))))

(define-public stumpwm-system
  (package
   (inherit stumpwm-master)
   (name "stumpwm-system")))
  
(define-public sbcl-clx-master
  (deprecated-package "sbcl-clx" sbcl-clx))

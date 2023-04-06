(define-module (non-gnu packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph))

(define-public python-tzdata
  (package
    (name "python-tzdata")
    (version "2023.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "tzdata" version))
              (sha256
               (base32
                "0fksj1gcr54yqvwiplnb9bg775k5v82vxcdxjm7hvjsawl41xvqi"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest
                         python-pytest-subtests))
    (home-page "https://github.com/python/tzdata")
    (synopsis "Provider of IANA time zone data")
    (description "Provider of IANA time zone data")
    (license #f)))

(define-public python-discord.py
  (package
    (name "python-discord.py")
    (version "2.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Rapptz/discord.py")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y2b7vpqycfr9f9r8vikks0lhxqgdxddrs19kmp1j8lx2hfvswnm"))))
    (build-system python-build-system)
    (arguments `(#:phases
                 (modify-phases %standard-phases
                   (replace 'check
                     (lambda* _
                       (invoke (which "pytest") "tests")))
                   (add-after 'unpack 'patch-deps
                     (lambda* (#:key inputs #:allow-other-keys)
                       (let* ((ffmpeg-exe (which "ffmpeg"))
                              (opus-path (assoc-ref inputs "opus"))
                              (opus-lib (string-append opus-path "/lib/libopus.so")))
                         (substitute* "discord/opus.py"
                           (("ctypes.util.find_library\\('opus'\\)")
                            (string-append "\"" opus-lib "\"")))
                         (substitute* "discord/player.py"
                           (("(executable: str = )'ffmpeg'," all variable)
                            (string-append variable "\"" ffmpeg-exe "\",")))))))))
    (propagated-inputs (list python-aiodns
                             python-aiohttp
                             python-brotli
                             python-cchardet ; Remove when Python upgrades to 3.10
                             python-pynacl
                             python-tzdata))
    (inputs (list opus ffmpeg))
    (native-inputs (list python-coverage
                         python-pytest
                         python-pytest-asyncio
                         python-pytest-cov
                         python-pytest-mock
                         python-typing-extensions))
    (home-page "https://github.com/Rapptz/discord.py")
    (synopsis "A Python wrapper for the Discord API")
    (description "This package provides a Python wrapper for the Discord API")
    (license license:expat)))

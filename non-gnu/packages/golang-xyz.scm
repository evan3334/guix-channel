(define-module (non-gnu packages golang-xyz)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang))

(define-public protoc-gen-go
  (package
    (name "protoc-gen-go")
    (version "1.27.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://go.googlesource.com/protobuf")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0aszb7cv8fq1m8akgd4kjyg5q7g5z9fdqnry6057ygq9r8r2yif2"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "google.golang.org/protobuf/cmd/protoc-gen-go"
       #:unpack-path "google.golang.org/protobuf"))
    (home-page "https://google.golang.org/protobuf")
    (synopsis "Go support for Protocol Buffers")
    (description
      "This project hosts the Go implementation for
@url{https://developers.google.com/protocol-buffers,protocol buffers}, which is a
language-neutral, platform-neutral, extensible mechanism for serializing
structured data.  The protocol buffer language is a language for specifying the
schema for structured data.  This schema is compiled into language specific
bindings.  This project provides both a tool to generate Go code for the
protocol buffer language, and also the runtime implementation to handle
serialization of messages in Go.  See the
@url{https://developers.google.com/protocol-buffers/docs/overview,protocol buffer developer guide}
for more information about protocol buffers themselves.")
    (license license:bsd-3)))

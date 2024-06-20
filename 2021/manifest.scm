(use-modules (guix packages)
             (guix git-download)
             (gnu packages lisp-xyz))

(define sbcl-april-master
  (let ((commit "14971869399f3b6098aff3d5ef31b73205a68d83")
        (revision "1"))
    (package
      (inherit sbcl-april)
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/phantomics/april")
                      (commit commit)))
                (file-name (git-file-name "cl-april" version))
                (sha256
                 (base32
                  "18jggcz2bsg3jnxh7hjalqq1rhvarbvhirrirsv7igdhv7fzib0m"))
                (modules '((guix build utils)))
                (snippet '(begin
                            ;; Remove bundled Apache-relicensed MaxPC.
                            (delete-file-recursively "maxpc-apache")
                            ;; Ensure references are to upstream MaxPC.
                            (substitute* "vex/vex.asd"
                              (("maxpc-apache")
                               "maxpc"))))))
      (arguments
       `(#:asd-systems '("april"
                         "april-lib.dfns.array"
                         "april-lib.dfns.string"
                         "april-lib.dfns.power"
                         "april-lib.dfns.tree"
                         "april-lib.dfns.graph"
                         "april-lib.dfns.numeric")))
      (inputs (cons (list "sbcl-serapeum" sbcl-serapeum)
                    (cons (list "sbcl-cl-unicode" sbcl-cl-unicode)
                          (package-inputs sbcl-april)))))))

(concatenate-manifests (list (specifications->manifest '("sbcl"))
                             (packages->manifest (list sbcl-april-master))))

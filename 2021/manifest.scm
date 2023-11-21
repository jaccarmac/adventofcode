(use-modules (guix packages)
             (guix git-download)
             (gnu packages lisp-xyz))

(define sbcl-april-master
  (let ((commit "c9a79a1034c2aa27b4987d87cbb434162cb5c150")
        (revision "1"))
    (package
      (inherit sbcl-april)
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jaccarmac/april")
                      (commit commit)))
                (file-name (git-file-name "cl-april" version))
                (sha256
                 (base32
                  "0rxm1fvfbnfsgzkdvxzxdmv5qcz1irwq921dhh4wnfl58q5rs1n7"))
                (modules '((guix build utils)))
                (snippet '(begin
                            ;; Remove bundled Apache-relicensed MaxPC.
                            (delete-file-recursively "maxpc-apache")
                            ;; Ensure references are to upstream MaxPC.
                            (substitute* "vex/vex.asd"
                              (("maxpc-apache")
                               "maxpc"))))))
      (inputs (cons (list "sbcl-serapeum" sbcl-serapeum)
                    (cons (list "sbcl-cl-unicode" sbcl-cl-unicode)
                          (package-inputs sbcl-april)))))))

(concatenate-manifests (list (specifications->manifest '("sbcl"))
                             (packages->manifest (list sbcl-april-master))))

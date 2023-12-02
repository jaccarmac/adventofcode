(use-modules (guix packages)
             (guix git-download)
             (gnu packages lisp-xyz))

(define sbcl-april-master
  (let ((commit "9ed8b6ffa03fbe28abfcd946a898084e1036fdbd")
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
                  "1kll35yv1c9jfxr23bm0dgm3n4n10xbw6bns404116p0339wy9ki"))
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

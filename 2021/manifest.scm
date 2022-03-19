(use-modules (guix packages)
             (guix git-download)
             (gnu packages lisp-xyz))

(define sbcl-april-master
  (let ((commit "0dda9997cdb7b839c45843175187d9e64401dfdf")
        (revision "1"))
    (package
      (inherit sbcl-april)
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/phantomics/april")
               (commit commit)))
         (file-name (git-file-name "cl-april" version))
         (sha256
          (base32 "0wznbmmi3r5iczlhvzf8j3pmmk3l4nx0ckhaqa8z07z79z5c2phs"))
         (modules '((guix build utils)))
         (snippet '(begin
                     ;; Remove bundled Apache-relicensed MaxPC.
                     (delete-file-recursively "maxpc-apache")
                     ;; Ensure references are to upstream MaxPC.
                     (substitute* "vex/vex.asd"
                       (("maxpc-apache") "maxpc"))))))
      (inputs(cons (list "sbcl-cl-unicode" sbcl-cl-unicode)
                   (package-inputs sbcl-april))))))

(concatenate-manifests
 (list
  (specifications->manifest '("sbcl"))
  (packages->manifest (list sbcl-april-master))))

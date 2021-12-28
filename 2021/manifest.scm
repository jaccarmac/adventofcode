(use-modules (guix packages)
             (guix git-download)
             (gnu packages lisp-xyz))

(define sbcl-april-master
  (let ((commit "401fbfc8e77a1c3c78b8920a210c8b54e56e3fc7")
        (revision "1"))
    (package
      (inherit sbcl-april)
      (version (git-version "0.9.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/phantomics/april")
               (commit commit)))
         (file-name (git-file-name "cl-april" version))
         (sha256
          (base32 "1lq5x8062qrpgfdagalb787h162j2ivzm3hahj4cl083fbkkqp7b"))
         (modules '((guix build utils)))
         (snippet '(begin
                     ;; Remove bundled Apache-relicensed MaxPC.
                     (delete-file-recursively "maxpc-apache")
                     ;; Ensure references are to upstream MaxPC.
                     (substitute* "vex/vex.asd"
                       (("maxpc-apache") "maxpc")))))))))

(packages->manifest
 (list
  (specification->package "sbcl")
  sbcl-april-master))

(use-modules (guix packages)
             (guix git-download)
             (gnu packages lisp-xyz))

(define sbcl-april-master
  (let ((commit "c219c6b3b6d7b9cf0ef8dce1646d5ac53f215d34")
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
          (base32 "1cc76n99mj840r1qmc0w16da15rv7adb54f27bc7gv7prlghchim"))
         (modules '((guix build utils)))
         (snippet '(begin
                     ;; Remove bundled Apache-relicensed MaxPC.
                     (delete-file-recursively "maxpc-apache")
                     ;; Ensure references are to upstream MaxPC.
                     (substitute* "vex/vex.asd"
                       (("maxpc-apache") "maxpc")))))))))

(concatenate-manifests
 (list
  (specifications->manifest '("sbcl"))
  (packages->manifest (list sbcl-april-master))))

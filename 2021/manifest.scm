(use-modules (guix packages)
             (guix git-download)
             (gnu packages lisp-xyz))

(define sbcl-april-master
  (let ((commit "615aa21081fe155f56aeba7140c876d176c80a09")
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
          (base32 "00j5ghxb01rc110nim7yf02afc5mbnb36f0jxr7mbpl48s80yjh4"))
         (modules '((guix build utils)))
         (snippet '(begin
                     ;; Remove bundled Apache-relicensed MaxPC.
                     (delete-file-recursively "maxpc-apache")
                     ;; Ensure references are to upstream MaxPC.
                     (substitute* "vex/vex.asd"
                       (("maxpc-apache") "maxpc"))))))
      (inputs (cons (list "sbcl-serapeum" sbcl-serapeum) (cons (list "sbcl-cl-unicode" sbcl-cl-unicode)
                                                               (package-inputs sbcl-april)))))))

(concatenate-manifests
 (list
  (specifications->manifest '("sbcl"))
  (packages->manifest (list sbcl-april-master))))

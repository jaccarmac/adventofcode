(use-modules (guix packages)
             (guix git-download)
             (gnu packages lisp-xyz))

(define sbcl-april-master
  (let ((commit "fb60dd3db197f335303a3de26bf71a9daa3d2384")
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
          (base32 "0rhzl14ihl2ywglhnwa72nyyk0nas7ys4lp6iq61adkl1sym32bc"))
         (modules '((guix build utils)))
         (snippet '(begin
                     ;; Remove bundled Apache-relicensed MaxPC.
                     (delete-file-recursively "maxpc-apache")
                     ;; Ensure references are to upstream MaxPC.
                     (substitute* "vex/vex.asd"
                       (("maxpc-apache") "maxpc"))
                     ;; Remove dependency on Quicklisp.
                     (substitute* "april.asd"
                       (("\"quicklisp\"") ""))
                     (substitute* "package.lisp"
                       (("#:quicklisp") ""))
                     ;; Remove the use of Quicklisp.
                     (substitute* "utilities.lisp"
                       (("\\(defun install-demos" all)
                        (string-append "#+quicklisp " all)))))))
      (inputs(cons (list "sbcl-cl-unicode" sbcl-cl-unicode)
                   (package-inputs sbcl-april))))))

(concatenate-manifests
 (list
  (specifications->manifest '("sbcl"))
  (packages->manifest (list sbcl-april-master))))

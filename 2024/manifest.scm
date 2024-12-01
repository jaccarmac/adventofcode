(use-modules (guix packages)
             (guix download)
             (guix git-download)
             (gnu packages perl6)
             (gnu packages perl))

(define rakudo-latest
  (package
    (inherit rakudo)
    (version "2024.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/rakudo/rakudo/releases/download/" version
             "/rakudo-" version ".tar.gz"))
       (sha256
        (base32 "0l4hxiagyvhn3xldli4sy2w6ibv2qgxb6llifq6qfg1w96lnmszw"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "3rdparty"))))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'remove-calls-to-git
                    (lambda _
                      (invoke "perl" "-ni" "-e"
                              "print if not /^BEGIN {/ .. /^}/" "Configure.pl")))
                  (add-after 'remove-calls-to-git 'fix-paths
                    (lambda _
                      (substitute* "tools/templates/Makefile-common-macros.in"
                        (("NQP_CONFIG_DIR = .*")
                         (string-append "NQP_CONFIG_DIR = "
                                        (assoc-ref %build-inputs
                                                   "nqp-configure")
                                        "/lib/perl5/site_perl/"
                                        ,(package-version perl) "\n")))))
                  ;; These tests pass when run manually.
                  (add-after 'fix-paths 'disable-failing-tests
                    (lambda _
                      (substitute* "t/09-moar/01-profilers.t"
                        (("^plan 12;\n")
                         "plan 10;\n")
                        (("^ok \\$htmlpath\\.IO\\.f, .*")
                         "")
                        (("^ok \\(try \\$htmlpath\\.IO\\.s .*")
                         ""))
                      (substitute* "t/02-rakudo/repl.t"
                                   (("^# https://github\\.com/Raku/old-issue-tracker/issues/5444\n")
                                    "todo 'works outside the Guix build';"))
                      (substitute* "t/05-messages/03-errors.t"
                                   (("^# https://github\\.com/Raku/old-issue-tracker/issues/6683\n")
                                    "todo 'works outside the Guix build';"))))
                  (add-after 'patch-source-shebangs 'patch-more-shebangs
                    (lambda _
                      (substitute* '("src/core.c/Proc.rakumod"
                                     "t/spec/S29-os/system.t"
                                     "tools/build/create-js-runner.pl"
                                     "tools/build/create-jvm-runner.pl")
                        (("/bin/sh")
                         (which "sh")))))
                  (replace 'configure
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out"))
                            (nqp (assoc-ref inputs "nqp")))
                        (invoke "perl"
                                "Configure.pl"
                                "--backend=moar"
                                "--with-nqp"
                                (string-append nqp "/bin/nqp")
                                "--prefix"
                                out))))
                  ;; This is the recommended tool for distro maintainers to install Raku
                  ;; modules systemwide.  See: https://github.com/ugexe/zef/issues/117
                  (add-after 'install 'install-dist-tool
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (dest (string-append out "/share/perl6/tools")))
                        (install-file "tools/install-dist.raku" dest)
                        (substitute* (string-append dest "/install-dist.raku")
                          (("/usr/bin/env raku")
                           (string-append out "/bin/raku")))))))))
    (native-inputs (modify-inputs (package-native-inputs rakudo)
                     (replace "nqp-configure" nqp-configure-latest)))
    (inputs (modify-inputs (package-inputs rakudo)
              (replace "nqp" nqp-latest)
              (replace "moarvm" moarvm-latest)))
    (native-search-paths
     (list (search-path-specification
            (variable "RAKULIB")
            (separator ",")
            (files '("share/perl6/lib" "share/perl6/site/lib"
                     "share/perl6/vendor/lib")))))))

(define moarvm-latest
  (package
    (inherit moarvm)
    (version "2024.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://moarvm.org/releases/MoarVM-" version
                           ".tar.gz"))
       (sha256
        (base32 "1jbj4fhak5w4vg43sgk4i6jb6g06xbv8jajvnwwi027algpzwp05"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; (delete-file-recursively "3rdparty/dynasm") ; JIT
                   (delete-file-recursively "3rdparty/dyncall")
                   (delete-file-recursively "3rdparty/freebsd")
                   (delete-file-recursively "3rdparty/libatomicops")
                   (delete-file-recursively "3rdparty/libuv")
                   (delete-file-recursively "3rdparty/libtommath")
                   (delete-file-recursively "3rdparty/msinttypes")))))))

(define nqp-configure-latest
  (let ((commit "d1437267bac250ee59cdb0073cf545628dbedc1d"))
    (package
      (inherit nqp-configure)
      (version "2024.10")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Raku/nqp-configure")
               (commit commit)))
         (file-name (git-file-name "nqp-configure" version))
         (sha256
          (base32 "1vc1q11kjb964jal9dhgf5vwp371a3rfw7gj987n33kzli7a10n0")))))))

(define nqp-latest
  (package
    (inherit nqp)
    (version "2024.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Raku/nqp/releases/download/"
                           version "/nqp-" version ".tar.gz"))
       (sha256
        (base32 "10plyl6fwbjp6nj9k5n8km4w6w2rvr8q7bw053vn9ylimwjfml8z"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "3rdparty"))))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-paths
                    (lambda _
                      (substitute* "tools/build/gen-version.pl"
                        (("catfile\\(\\$libdir, 'MAST', \\$_\\)")
                         (string-append "catfile('"
                                        (assoc-ref %build-inputs "moarvm")
                                        "/share/nqp/lib" "', 'MAST', $_)")))))
                  (add-after 'patch-source-shebangs 'patch-more-shebangs
                    (lambda _
                      (substitute* '("t/nqp/111-spawnprocasync.t"
                                     "t/nqp/113-run-command.t"
                                     "tools/build/gen-js-cross-runner.pl"
                                     "tools/build/gen-js-runner.pl"
                                     "tools/build/install-js-runner.pl"
                                     "tools/build/install-jvm-runner.pl.in")
                        (("/bin/sh")
                         (which "sh")))))
                  (replace 'configure
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out"))
                            (moar (assoc-ref inputs "moarvm")))
                        (invoke "perl"
                                "Configure.pl"
                                "--backends=moar"
                                "--with-moar"
                                (string-append moar "/bin/moar")
                                "--prefix"
                                out)))))))
    (native-inputs (modify-inputs (package-native-inputs nqp)
                     (replace "nqp-configure" nqp-configure-latest)))
    (inputs (modify-inputs (package-inputs nqp)
              (replace "moarvm" moarvm-latest)))))

(packages->manifest (list rakudo-latest))

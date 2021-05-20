;; ~~~please see copyright notice in ./COPYING

;;;;;;;;;; WARNING: ;;;;;;;;;;
;;
;; This SRFI is generally a side effecting one, so tests often depend
;; on the state left by previous ones if they worked.
;;
;; If you're sufficiently daring, this test suite runs fine as root,
;; and some things can only be tested if run as root.  The only caveat
;; is that before you try to run it as a normal user again, you must
;; manually delete the tmp-containing-dir
;;

(define-library (srfi 205 test)
  (export run-tests)

  (import (scheme base)

          (chibi)
          (rename (only (chibi ast) errno integer->error-string gc) (integer->error-string error-string))
          (only (chibi process) exit) ;; for printf style debugging
          (chibi test)

          (only (srfi 1) alist-cons)
          (only (srfi 69) make-hash-table hash-table-set! hash-table-ref)
          (srfi 151) ;; bitwise operators
          (srfi 205)

          (srfi 205 posix-error)
          )

  (include-shared "205")
  (include-shared "aux")

  (include "common.scm")

  (begin

    ;; Inverse of test-error, mutated from test-not, only errors if an
    ;; exception is raised

    (define-syntax test-not-error
      (syntax-rules ()
        ((_ expr) (test-assert (begin expr #t)))
        ((_ name expr) (test-assert name (begin expr #t)))))

    (define the-error #f)

    (define input-port-dev-zero (open-input-file "/dev/zero")) ;; not a terminal port

    (define (run-tests)

      (test-group "srfi-205: POSIX Terminal Fundamentals"

#|
        (test-group "Prologue"

          ) ;; end prologue
|#

        (test-group "Errors"

          ;; tests from old SRFI 198 implementation

          (test #f (posix-error? 1))

          (set! the-error (make-posix-error 1))
          (test 'error (posix-error-error-set the-error))
          (test #f (posix-error-number the-error))
          (test #f (posix-error-name the-error))
          (test 'make-posix-error (posix-error-scheme-procedure the-error))
          (test #f (posix-error-posix-interface the-error))
          (test "Malformed call to make-posix-error, not a list; see data for details" (posix-error-message the-error))
          (test '((arguments . 1)) (posix-error-data the-error))

          (set! the-error (make-posix-error '(1)))
          (test 'error (posix-error-error-set the-error))
          (test #f (posix-error-number the-error))
          (test #f (posix-error-name the-error))
          (test 'make-posix-error (posix-error-scheme-procedure the-error))
          (test #f (posix-error-posix-interface the-error))
          (test "Malformed call to make-posix-error, not an alist; see data for details" (posix-error-message the-error))
          (test '((arguments 1)) (posix-error-data the-error))

          (set! the-error (make-posix-error '((1 . 1))))
          (test 'error (posix-error-error-set the-error))
          (test #f (posix-error-number the-error))
          (test #f (posix-error-name the-error))
          (test 'make-posix-error (posix-error-scheme-procedure the-error))
          (test #f (posix-error-posix-interface the-error))
          (test "Malformed call to make-posix-error, first key must be a symbol; see data for details" (posix-error-message the-error))
          (test '((arguments (1 . 1))) (posix-error-data the-error))

          (set! the-error (make-posix-error '((foo . 1))))
          (test 'error (posix-error-error-set the-error))
          (test #f (posix-error-number the-error))
          (test #f (posix-error-name the-error))
          (test 'make-posix-error (posix-error-scheme-procedure the-error))
          (test #f (posix-error-posix-interface the-error))
          (test "Malformed call to make-posix-error, missing error-set; see data for details" (posix-error-message the-error))
          (test '((arguments (foo . 1))) (posix-error-data the-error))

          (set! the-error (make-posix-error '((error-set . error))))
          (test 'error (posix-error-error-set the-error))
          (test #f (posix-error-number the-error))
          (test #f (posix-error-name the-error))
          (test #f (posix-error-scheme-procedure the-error))
          (test #f (posix-error-posix-interface the-error))
          (test #f (posix-error-message the-error))
          (test #f (posix-error-data the-error))

          ;; Make sure the error raising code isn't malfunctioning and raising a different error
          (test-error ((with-exception-handler
                        (lambda (exception) (set! the-error exception))
                        (lambda () (raise-posix-error '((error-set . error)))))))
          (test-assert (posix-error? the-error))

          ;; Make a "real" error
          (set! the-error (make-posix-error '((error-set . errno)
                                              (errno-number . 2)
                                              (errno-name . ENOENT)
                                              (scheme-procedure . open-file)
                                              (posix-interface . open)
                                              (message . "open-file called open: ENOENT: No such file or directory")
                                              (data . ((arguments . ("not-a-valid-filename" 0 428))
                                                       (heritage . "SRFI 205"))))))

          (test 'errno (posix-error-error-set the-error))
          (test 2 (posix-error-number the-error))
          (test 'ENOENT (posix-error-name the-error))
          (test 'open-file (posix-error-scheme-procedure the-error))
          (test 'open (posix-error-posix-interface the-error))
          (test "open-file called open: ENOENT: No such file or directory" (posix-error-message the-error))
          (test '("not-a-valid-filename" 0 428) (cdr (assq 'arguments (posix-error-data the-error))))
          (test "SRFI 205" (cdr (assq 'heritage (posix-error-data the-error))))


          ;; tests using the above
          (test 0 (errno))
          (test-not-error (set-errno E2BIG))
          (set-errno E2BIG)
          (test E2BIG (errno))
          (test-assert (string? (errno-string (errno))))
          (test-assert (string? (errno-string E2BIG)))
          (set-errno E2BIG)
          (test-assert (equal? (errno-string (errno)) (errno-string E2BIG)))

          ;; Make sure the error raising code isn't malfunctioning and raising a different error
          (test-error ((with-exception-handler
                        (lambda (exception) (set! the-error exception))
                        (lambda () (errno-error 2 'test-of-errno-error-procedure-symbol 'test-of-errno-error-syscall-symbol 1 2 3 4)))))
          (test-assert (posix-error? the-error))
          (test 'errno (posix-error-error-set the-error))
          (test 2 (posix-error-number the-error))
          (test 'ENOENT (posix-error-name the-error))
          (test 'test-of-errno-error-procedure-symbol (posix-error-scheme-procedure the-error))
          (test 'test-of-errno-error-syscall-symbol (posix-error-posix-interface the-error))
          (test "test-of-errno-error-procedure-symbol called test-of-errno-error-syscall-symbol: ENOENT: No such file or directory"
                (posix-error-message the-error))
          (test '(1 2 3 4) (cdr (assq 'arguments (posix-error-data the-error))))

#| ~~~
          ;; Make sure the error raising code works for a real error
          (test-error ((with-exception-handler
                        (lambda (exception) (set! the-error exception))
                        (lambda () (open-file bogus-path textual-input 0)))))
          (test-assert (posix-error? the-error))
          (test 'errno (posix-error-error-set the-error))
          (test 2 (posix-error-number the-error))
          (test 'ENOENT (posix-error-name the-error))
          (test 'open-file (posix-error-scheme-procedure the-error))
          (test 'open (posix-error-posix-interface the-error))
          (test "open-file called open: ENOENT: No such file or directory"
                (posix-error-message the-error))
          (test (list bogus-path open/read #o666) (cdr (assq 'arguments (posix-error-data the-error))))
|#

          (test-error ((with-exception-handler
                        (lambda (exception) (set! the-error exception))
                        (lambda () (sanity-check-error "Sanity check error test message" 'test-of-errno-error-procedure-symbol 1 2 3 4)))))
          (test-assert (posix-error? the-error))
          (test 'sanity-check (posix-error-error-set the-error))
          (test #f (posix-error-number the-error))
          (test #f (posix-error-name the-error))
          (test 'test-of-errno-error-procedure-symbol (posix-error-scheme-procedure the-error))
          (test #f (posix-error-posix-interface the-error))
          (test "test-of-errno-error-procedure-symbol: Sanity check error test message"
                (posix-error-message the-error))
          (test '(1 2 3 4) (cdr (assq 'arguments (posix-error-data the-error))))


          ) ;; end errors


#|
        (test-group "Safe terminal I/O primitives"

          ) ;; end Safe terminal I/O primitives
|#


#|
        (test-group "Low-level terminal manipulation"

          ) ;; end Low-level terminal manipulation
|#


        (test-group "Miscellaneous procedures"

          (test-error (terminal-file-name "foo"))
          (test-error (terminal-file-name (open-input-string "plover")))
          (test-error (terminal-file-name input-port-dev-zero))
          (test-assert (string? (terminal-file-name (current-input-port))))

          ) ;; end


        (test-group "Epilogue: ~~~cleanup, force a gc"

          (close-port input-port-dev-zero)

          (test-not-error (gc)) ;; see if we blow up

          ) ;; end epilogue

        ))))

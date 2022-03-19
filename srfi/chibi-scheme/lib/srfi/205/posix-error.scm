;; please see copyright notice in ./COPYING

(define-record-type Posix-Interface-Error
    (make-posix-interface-error-record error-set errno-number errno-name scheme-procedure posix-interface message data)
    posix-error?
  (error-set posix-error-error-set)
  (errno-number posix-error-number)
  (errno-name posix-error-name)
  (scheme-procedure posix-error-scheme-procedure)
  (posix-interface posix-error-posix-interface)
  (message posix-error-message)
  (data posix-error-data))


(define (make-malformed-posix-error-alist original-argument the-message)
  (alist-cons 'message
              the-message
              (alist-cons 'data
                          (alist-cons 'arguments original-argument '())
                          '((error-set . error)
                            (scheme-procedure . make-posix-error)))))

(define (alist-or-#f the-key the-alist)
  (let ((the-sub-alist (assq the-key the-alist)))
    (if the-sub-alist
        (cdr the-sub-alist)
        the-sub-alist)))

(define (make-posix-error the-alist)
  (cond ((not (pair? the-alist))
         (make-posix-error (make-malformed-posix-error-alist the-alist "Malformed call to make-posix-error, not a list; see data for details")))
        ((not (pair? (car the-alist)))
         (make-posix-error (make-malformed-posix-error-alist the-alist "Malformed call to make-posix-error, not an alist; see data for details")))
        ((not (symbol? (caar the-alist)))
         (make-posix-error (make-malformed-posix-error-alist the-alist "Malformed call to make-posix-error, first key must be a symbol; see data for details")))
        (else (let ((the-error-set (assq 'error-set the-alist)))
                (if (not the-error-set)
                    (make-posix-error (make-malformed-posix-error-alist the-alist "Malformed call to make-posix-error, missing error-set; see data for details"))
                    (make-posix-interface-error-record (cdr the-error-set)
                                                         (alist-or-#f 'errno-number the-alist)
                                                         (alist-or-#f 'errno-name the-alist)
                                                         (alist-or-#f 'scheme-procedure the-alist)
                                                         (alist-or-#f 'posix-interface the-alist)
                                                         (alist-or-#f 'message the-alist)
                                                         (alist-or-#f 'data the-alist)))))))

(define (raise-posix-error the-alist)
  (raise (make-posix-error the-alist)))

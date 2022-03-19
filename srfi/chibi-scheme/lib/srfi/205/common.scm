;; please see copyright notice in ./COPYING

;; Common code that's included by both 170.sld and test.sld

;;; 3.1  Errors

(define errno-map (make-hash-table))

(map (lambda (errno-number errno-symbol) (hash-table-set! errno-map errno-number errno-symbol))
     (list
      E2BIG EACCES EADDRINUSE EADDRNOTAVAIL
      EAFNOSUPPORT EAGAIN EALREADY EBADF
      EBADMSG EBUSY ECANCELED ECHILD
      ECONNABORTED ECONNREFUSED ECONNRESET
      EDEADLK EDESTADDRREQ EDOM EDQUOT
      EEXIST EFAULT EFBIG EHOSTUNREACH
      EIDRM EILSEQ EINPROGRESS EINTR EINVAL
      EIO EISCONN EISDIR ELOOP EMFILE
      EMLINK EMSGSIZE ENAMETOOLONG ENETDOWN
      ENETRESET ENETUNREACH ENFILE ENOBUFS
      ENODEV ENOENT ENOEXEC ENOLCK ENOMEM
      ENOMSG ENOPROTOOPT ENOSPC ENOSYS
      ENOTCONN ENOTDIR ENOTEMPTY ENOTRECOVERABLE
      ENOTSOCK ENOTSUP ENOTTY ENXIO
      EOPNOTSUPP EOVERFLOW EOWNERDEAD EPERM
      EPIPE EPROTO EPROTONOSUPPORT EPROTOTYPE
      ERANGE EROFS ESPIPE ESRCH ESTALE
      ETIMEDOUT ETXTBSY EWOULDBLOCK EXDEV

      (cond-expand ((not openbsd)
                    EMULTIHOP ENOLINK
                    ;; STREAMS:
                    ENODATA ENOSTR ENOSR ETIME)))

     (list
      'E2BIG 'EACCES 'EADDRINUSE 'EADDRNOTAVAIL
      'EAFNOSUPPORT 'EAGAIN 'EALREADY 'EBADF
      'EBADMSG 'EBUSY 'ECANCELED 'ECHILD
      'ECONNABORTED 'ECONNREFUSED 'ECONNRESET
      'EDEADLK 'EDESTADDRREQ 'EDOM 'EDQUOT
      'EEXIST 'EFAULT 'EFBIG 'EHOSTUNREACH
      'EIDRM 'EILSEQ 'EINPROGRESS 'EINTR 'EINVAL
      'EIO 'EISCONN 'EISDIR 'ELOOP 'EMFILE
      'EMLINK 'EMSGSIZE 'ENAMETOOLONG 'ENETDOWN
      'ENETRESET 'ENETUNREACH 'ENFILE 'ENOBUFS
      'ENODEV 'ENOENT 'ENOEXEC 'ENOLCK 'ENOMEM
      'ENOMSG 'ENOPROTOOPT 'ENOSPC 'ENOSYS
      'ENOTCONN 'ENOTDIR 'ENOTEMPTY 'ENOTRECOVERABLE
      'ENOTSOCK 'ENOTSUP 'ENOTTY 'ENXIO
      'EOPNOTSUPP 'EOVERFLOW 'EOWNERDEAD 'EPERM
      'EPIPE 'EPROTO 'EPROTONOSUPPORT 'EPROTOTYPE
      'ERANGE 'EROFS 'ESPIPE 'ESRCH 'ESTALE
      'ETIMEDOUT 'ETXTBSY 'EWOULDBLOCK 'EXDEV

      (cond-expand ((not openbsd)
                    'EMULTIHOP 'ENOLINK
                    ;; STREAMS:
                    'ENODATA 'ENOSTR 'ENOSR 'ETIME))
      ))

(define (errno-string errno)
  (if (not (exact-integer? errno))
      (sanity-check-error "errno-string requires an exact integer" 'errno-string errno))
  (%strerror errno))

(define (errno-symbol errno)
  (if (not (exact-integer? errno))
      (sanity-check-error "errno-symbol requires an exact integer" 'errno-symbol errno))
  (hash-table-ref errno-map errno))

(define (errno-error errno procedure-symbol syscall-symbol . data)
  (raise-posix-error
   (alist-cons 'message
               (string-append (symbol->string procedure-symbol)
                              " called "
                              (symbol->string syscall-symbol)
                              ": "
                              (symbol->string (errno-symbol errno))
                              ": "
                              (errno-string errno))
               (alist-cons 'scheme-procedure
                           procedure-symbol
                           (alist-cons 'posix-interface
                                       syscall-symbol
                                       (alist-cons 'data
                                                   (list (cons 'arguments data))
                                                   (alist-cons 'errno-number
                                                               errno
                                                               (alist-cons 'errno-name
                                                                           (errno-symbol errno)
                                                                           '((error-set . errno))))))))))


(define (sanity-check-error message procedure-symbol . data)
  (raise-posix-error
   (alist-cons 'message
               (string-append (symbol->string procedure-symbol) ": " message)
               (alist-cons 'scheme-procedure
                           procedure-symbol
                           (alist-cons 'data
                                       (list (cons 'arguments data))
                                       '((error-set . sanity-check)))))))


;; This suffers from the problems discussed in SRFI 199
;; (https://srfi-email.schemers.org/srfi-199/), it needs to be done at
;; the C level, because Chibi Scheme may set errno in the middle of
;; the loop.

(define (retry-if-EINTR the-lambda)
  (let loop ((ret (the-lambda)))
    (if ret
        ret
        (if (equal? EINTR (errno))
            (loop (the-lambda))
            ret))))

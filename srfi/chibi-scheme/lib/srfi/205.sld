;; ~~~please see copyright notice in 205/COPYING

(define-library (srfi 205)
  (export

   ;; 3.1  Errors

   posix-error? posix-error-name posix-error-message
   ;; these are extra and allowed additions to the official SRFI namespace:
   posix-error-error-set
   posix-error-number posix-error-scheme-procedure
   posix-error-posix-interface posix-error-data



   ;; useful for debuging

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
   errno set-errno
   errno-string errno-symbol



   ;; with-raw-mode
   ;; with-rare-mode
   ;; with-cooked-mode
   ;; without-echo

   ;; stty
   ;; gtty

   terminal-file-name
   terminal/stop-output terminal/start-output
   terminal/stop-input terminal/start-input
   terminal-flow-control
   ;; terminal-wait
   ;; terminal-discard
   ;; terminal-send-break

   )

#|
  (cond-expand ((not openbsd)
    (export


    )))
|#

  (cond-expand
   (chibi
    (import
     (scheme base)

     (chibi)
     (rename (only (chibi ast) errno integer->error-string) (integer->error-string error-string))
     (only (chibi stty) ioctl) ;; ~~~

     (only (srfi 1) alist-cons)
     (only (srfi 69) make-hash-table hash-table-set! hash-table-ref)
     (srfi 151) ;; bitwise operators

     (only (srfi 205 posix-error) posix-error?
                                  posix-error-error-set
                                  posix-error-name posix-error-message
                                  posix-error-number posix-error-scheme-procedure
                                  posix-error-posix-interface posix-error-data
                                  raise-posix-error)
     )

    (include-shared "205/205")
    (include-shared "205/aux")))

  (include "205/common.scm")
  (include "205/205.scm")
  )

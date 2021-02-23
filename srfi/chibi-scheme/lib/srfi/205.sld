;; ~~~please see copyright notice in 205/COPYING

(define-library (srfi 205)
  (export

   ;; 3.1  Errors

   posix-error? posix-error-name posix-error-message
   ;; these are extra and allowed additions to the official SRFI namespace:
   posix-error-error-set
   posix-error-number posix-error-scheme-procedure
   posix-error-posix-interface posix-error-data


#|
   ;; useful for debuging

   errno/E2BIG errno/EACCES errno/EADDRINUSE errno/EADDRNOTAVAIL
   errno/EAFNOSUPPORT errno/EAGAIN errno/EALREADY errno/EBADF
   errno/EBADMSG errno/EBUSY errno/ECANCELED errno/ECHILD
   errno/ECONNABORTED errno/ECONNREFUSED errno/ECONNRESET
   errno/EDEADLK errno/EDESTADDRREQ errno/EDOM errno/EDQUOT
   errno/EEXIST errno/EFAULT errno/EFBIG errno/EHOSTUNREACH
   errno/EIDRM errno/EILSEQ errno/EINPROGRESS errno/EINTR errno/EINVAL
   errno/EIO errno/EISCONN errno/EISDIR errno/ELOOP errno/EMFILE
   errno/EMLINK errno/EMSGSIZE errno/ENAMETOOLONG errno/ENETDOWN
   errno/ENETRESET errno/ENETUNREACH errno/ENFILE errno/ENOBUFS
   errno/ENODEV errno/ENOENT errno/ENOEXEC errno/ENOLCK errno/ENOMEM
   errno/ENOMSG errno/ENOPROTOOPT errno/ENOSPC errno/ENOSYS
   errno/ENOTCONN errno/ENOTDIR errno/ENOTEMPTY errno/ENOTRECOVERABLE
   errno/ENOTSOCK errno/ENOTSUP errno/ENOTTY errno/ENXIO
   errno/EOPNOTSUPP errno/EOVERFLOW errno/EOWNERDEAD errno/EPERM
   errno/EPIPE errno/EPROTO errno/EPROTONOSUPPORT errno/EPROTOTYPE
   errno/ERANGE errno/EROFS errno/ESPIPE errno/ESRCH errno/ESTALE
   errno/ETIMEDOUT errno/ETXTBSY errno/EWOULDBLOCK errno/EXDEV
|#

   ;; with-raw-mode
   ;; with-rare-mode
   ;; with-cooked-mode
   ;; without-echo

   ;; stty
   ;; gtty

   ;; terminal-file-name
   ;; terminal-flow-control
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

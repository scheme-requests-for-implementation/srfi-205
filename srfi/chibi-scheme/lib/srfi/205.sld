;; please see copyright notice in 205/COPYING

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

#| brkint no-brkint
   clocal no-clocal
   cmspar no-cmspar
   cread no-cread
   crtscts no-crtscts
   cs5 cs6 cs7 cs8
   cstopb no-cstopb
   echo no-echo
   echoctl no-echoctl
   echoe no-echoe
   echok no-echok
   echonl no-echonl
   hup no-hup
   hupcl no-hupcl
   icanon no-icanon
   icrnl no-icrnl
   iexten no-iexten
   ignbrk no-ignbrk
   igncr no-igncr
   ignpar no-ignpar
   imaxbel no-imaxbel
   inlcr no-inlcr
   inpck no-inpck
   isig no-isig
   istrip no-istrip
   iuclc no-iuclc
   ixany no-ixany
   ixoff no-ixoff
   ixon no-ixon
   noflsh no-noflsh
   now drain flush
   ocrnl no-ocrnl
   olcuc no-olcuc
   onlcr no-onlcr
   onlret no-onlret
   onocr no-onocr
   opost no-opost
   parenb no-parenb
   parmrk no-parmrk
   parodd no-parodd
   tostop no-tostop
   xcase no-xcase
   |#
   ;; get
   ;; state-object
   ;; sane

   ;; gtty

   terminal-file-name
   terminal/stop-output terminal/start-output
   terminal/stop-input terminal/start-input
   terminal-flow-control
   terminal-wait
   terminal/discard-input terminal/discard-output terminal/discard-both
   terminal-discard
   terminal-send-break
   terminal-dimensions

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
     (only (chibi stty) ioctl)

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
    (include-shared "205/aux")
    (include-shared "205/terminal-dimensions")))

  (include "205/common.scm")
  (include "205/205.scm")
  (include "205/terminal-dimensions.scm")
  )

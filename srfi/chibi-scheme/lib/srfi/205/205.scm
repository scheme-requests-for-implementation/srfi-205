;; please see copyright notice in ./COPYING

#|
;; The following was a quick and dirty implimentation of the highest
;; procedures WITHOUT the use of stty; how messy and sprawling they'd
;; have to be even after cleanup is a primary motivation for this SRFI

;; All terminal procedures except for terminal? will be moved to a new
;; SRFI; this working code is left here for it.

;; all prefactory with- and without- errno-errors need a more
;; specific error indicator, that's why they're not combined

(define (with-raw-mode input-port output-port min time proc)
  (if (not (and (port? input-port) (port? output-port)))
      (sanity-check-error "first two arguments must ports" 'with-raw-mode input-port output-port min time proc))
  (if (not (and (terminal? input-port) (terminal? output-port)))
      (sanity-check-error "first two argument must be a terminal port" 'with-raw-mode input-port output-port min time proc))
  (if (not (and (input-port? input-port) (output-port? output-port)))
      (sanity-check-error "first two arguments must be an input and output ports, respectively" 'with-raw-mode input-port output-port min time proc))
  (if (not (exact-integer? min))
      (sanity-check-error "third argument must be an exact integer" 'with-raw-mode input-port output-port min time proc))
  (if (not (exact-integer? time))
      (sanity-check-error "fourth argument must be an exact integer" 'with-raw-mode input-port output-port min time proc))

  (let* ((initial-input-termios (%tcgetattr input-port))
         (initial-output-termios (%tcgetattr output-port))
         (new-input-termios (%tcgetattr input-port)) ;; because of tagging, how to copy is not obvious
         (new-output-termios (%tcgetattr output-port)) ;; because of tagging, how to copy is not obvious
         (reset-terminal (lambda ()
                           (let ((input-return (retry-if-EINTR (lambda () (%tcsetattr input-port TCSAFLUSH initial-input-termios))))) ;; still try resetting output
                             (if (not (and (retry-if-EINTR (lambda () (%tcsetattr output-port TCSAFLUSH initial-output-termios))) input-return))
                                 (errno-error (errno) 'with-raw-mode 'tcsetattr input-port output-port min time proc))))) ;; might as well exit the procedure
         ;; set all for *both* ports???
         (the-lflags (bitwise-ior ECHO ICANON IEXTEN ISIG))
         (the-iflags (bitwise-ior BRKINT ICRNL INPCK ISTRIP IXON))
         (the-and-cflags (bitwise-ior CSIZE PARENB))
         (the-ior-cflags CS8)
         (the-oflags OPOST))

    (if (or (not initial-input-termios) (not new-input-termios) (not initial-output-termios) (not new-output-termios))
        (sanity-check-error "failure to get or set termios data" 'with-raw-mode input-port output-port min time proc))

    (term-attrs-lflag-set! new-input-termios
                           (bitwise-and (term-attrs-lflag new-input-termios) (bitwise-not the-lflags)))
    (term-attrs-iflag-set! new-input-termios
                           (bitwise-and (term-attrs-iflag new-input-termios) (bitwise-not the-iflags)))
    (term-attrs-cflag-set! new-input-termios
                           (bitwise-and (term-attrs-cflag new-input-termios) (bitwise-not the-and-cflags)))
    (term-attrs-cflag-set! new-input-termios
                           (bitwise-ior (term-attrs-cflag new-input-termios) the-ior-cflags))
    (term-attrs-oflag-set! new-input-termios
                           (bitwise-and (term-attrs-oflag new-input-termios) (bitwise-not the-oflags)))
    (term-attrs-cc-element-set! new-input-termios min VMIN) ;; ought to transpose array index and value to put in it
    (term-attrs-cc-element-set! new-input-termios time VTIME) ;; ought to transpose array index and value to put in it

    (term-attrs-lflag-set! new-output-termios
                           (bitwise-and (term-attrs-lflag new-output-termios) (bitwise-not the-lflags)))
    (term-attrs-iflag-set! new-output-termios
                           (bitwise-and (term-attrs-iflag new-output-termios) (bitwise-not the-iflags)))
    (term-attrs-cflag-set! new-output-termios
                           (bitwise-and (term-attrs-cflag new-output-termios) (bitwise-not the-and-cflags)))
    (term-attrs-cflag-set! new-output-termios
                           (bitwise-ior (term-attrs-cflag new-output-termios) the-ior-cflags))
    (term-attrs-oflag-set! new-output-termios
                           (bitwise-and (term-attrs-oflag new-output-termios) (bitwise-not the-oflags)))
    (dynamic-wind
        (lambda ()      ;; set output port first since input port is the same + VMIN and VTIME, we're probably doing duplicate tcsetattrs at the OS level
          (if (not (and (retry-if-EINTR (lambda () (%tcsetattr output-port TCSAFLUSH new-output-termios)))
                        (retry-if-EINTR (lambda () (%tcsetattr input-port TCSAFLUSH new-input-termios)))))
              (errno-error (errno) 'with-raw-mode 'tcsetattr input-port output-port min time proc)

              ;; For historical reasons, tcsetattr returns 0 if *any*
              ;; of the attribute changes took, so we must check to
              ;; see if all have been set
              (let ((real-new-input-termios (%tcgetattr input-port))
                    (real-new-output-termios (%tcgetattr output-port)))
                (if (not (and real-new-input-termios real-new-output-termios))
                    (begin (reset-terminal)
                           (errno-error (errno) 'with-raw-mode 'tcsetattr input-port output-port min time proc))
                    (if (not (and (equal? (term-attrs-lflag new-input-termios) (term-attrs-lflag real-new-input-termios))
                                  (equal? (term-attrs-iflag new-input-termios) (term-attrs-iflag real-new-input-termios))
                                  (equal? (term-attrs-cflag new-input-termios) (term-attrs-cflag real-new-input-termios))
                                  (equal? (term-attrs-oflag new-input-termios) (term-attrs-oflag real-new-input-termios))
                                  (equal? min (term-attrs-cc-element real-new-input-termios VMIN))
                                  (equal? time (term-attrs-cc-element real-new-input-termios VTIME))

                                  (equal? (term-attrs-lflag new-output-termios) (term-attrs-lflag real-new-output-termios))
                                  (equal? (term-attrs-iflag new-output-termios) (term-attrs-iflag real-new-output-termios))
                                  (equal? (term-attrs-cflag new-output-termios) (term-attrs-cflag real-new-output-termios))
                                  (equal? (term-attrs-oflag new-output-termios) (term-attrs-oflag real-new-output-termios))))
                        (begin (reset-terminal)
                               (sanity-check-error "a termios update failed" 'with-raw-mode input-port output-port min time proc)))))))
        (lambda () (proc input-port output-port))
        (lambda ()
          (reset-terminal)))))

(define (with-rare-mode input-port output-port proc)
  (if (not (and (port? input-port) (port? output-port)))
      (sanity-check-error "first two arguments must be ports" 'with-rare-mode input-port output-port proc))
  (if (not (and (terminal? input-port) (terminal? output-port)))
      (sanity-check-error "first two arguments must be a terminal ports" 'with-rare-mode input-port output-port proc))
  (if (not (and (input-port? input-port) (output-port? output-port)))
      (sanity-check-error "first two arguments must be an input and output ports, respectively" 'with-rare-mode input-port output-port proc))

  (let* ((initial-input-termios (%tcgetattr input-port))
         (initial-output-termios (%tcgetattr output-port))
         (new-input-termios (%tcgetattr input-port)) ;; because of tagging, how to copy is not obvious
         (new-output-termios (%tcgetattr output-port)) ;; because of tagging, how to copy is not obvious
         (reset-terminal (lambda ()
                           (let ((input-return (retry-if-EINTR (lambda () (%tcsetattr input-port TCSAFLUSH initial-input-termios))))) ;; still try resetting output
                             (if (not (and (retry-if-EINTR (lambda () (%tcsetattr output-port TCSAFLUSH initial-output-termios))) input-return))
                                 (errno-error (errno) 'with-rare-mode 'tcsetattr input-port output-port proc))))) ;; might as well exit the procedure
         (the-lflags (bitwise-ior ICANON ECHO))) ;; set for *both* ports???

    (if (or (not initial-input-termios) (not new-input-termios) (not initial-output-termios) (not new-output-termios))
        (sanity-check-error "failure to get or set termios data" 'with-rare-mode input-port output-port proc))

    (term-attrs-lflag-set! new-input-termios
                           (bitwise-and (term-attrs-lflag new-input-termios) (bitwise-not the-lflags)))
    (term-attrs-cc-element-set! new-input-termios 1 VMIN) ;;  ought to transpose array index and value to put in it
    (term-attrs-cc-element-set! new-input-termios 0 VTIME) ;;  ought to transpose array index and value to put in it
    (term-attrs-lflag-set! new-output-termios
                           (bitwise-and (term-attrs-lflag new-output-termios) (bitwise-not the-lflags)))
    (dynamic-wind
        (lambda ()      ;; set output port first since input port is the same + VMIN and VTIME, we're probably doing duplicate tcsetattrs at the OS level
          (if (not (and (retry-if-EINTR (lambda () (%tcsetattr output-port TCSAFLUSH new-output-termios)))
                        (retry-if-EINTR (lambda () (%tcsetattr input-port TCSAFLUSH new-input-termios)))))
              (errno-error (errno) 'with-rare-mode 'tcsetattr input-port output-port proc)

              ;; For historical reasons, tcsetattr returns 0 if *any*
              ;; of the attribute changes took, so we must check to
              ;; see if all have been set
              (let ((real-new-input-termios (%tcgetattr input-port))
                    (real-new-output-termios (%tcgetattr output-port)))
                (if (not (and real-new-input-termios real-new-output-termios))
                    (begin (reset-terminal)
                           (errno-error (errno) 'with-rare-mode 'tcgetattr input-port output-port proc))
                    (if (not (and (equal? 0 (bitwise-and (term-attrs-lflag real-new-input-termios) the-lflags))
                                  (equal? 1 (term-attrs-cc-element real-new-input-termios VMIN))
                                  (equal? 0 (term-attrs-cc-element real-new-input-termios VTIME))
                                  (equal? 0 (bitwise-and (term-attrs-lflag real-new-output-termios) the-lflags))))
                        (begin (reset-terminal)
                               (sanity-check-error "a termios update failed" 'with-rare-mode input-port output-port proc)))))))
        (lambda () (proc input-port output-port))
        (lambda ()
          (reset-terminal)))))

(define (without-echo input-port output-port proc)
  (if (not (and (port? input-port) (port? output-port)))
      (sanity-check-error "first two arguments must be ports" 'without-echo input-port output-port proc))
  (if (not (and (terminal? input-port) (terminal? output-port)))
      (sanity-check-error "first two arguments must be terminal ports" 'without-echo input-port output-port proc))
  (if (not (and (input-port? input-port) (output-port? output-port)))
      (sanity-check-error "first two arguments must be an input and output ports, respectively" 'without-echo input-port output-port proc))

  (let* ((initial-output-termios (%tcgetattr output-port))
         (new-output-termios (%tcgetattr output-port)) ;; because of tagging, how to copy is not obvious
         (reset-terminal (lambda ()
                           (if (not (retry-if-EINTR (lambda () (%tcsetattr output-port TCSAFLUSH initial-output-termios))))
                               (errno-error (errno) 'without-echo 'tcsetattr output-port proc)))) ;; might as well exit the procedure
         (the-lflags (bitwise-ior ECHO ECHOE ECHOK ECHONL)))

    (if (or (not initial-output-termios) (not new-output-termios))
        (sanity-check-error "failure to get or set termios data" 'without-echo output-port proc))
    (term-attrs-lflag-set! new-output-termios
                           (bitwise-and (term-attrs-lflag new-output-termios) (bitwise-not the-lflags)))
    (dynamic-wind
        (lambda ()
          (if (not (retry-if-EINTR (lambda () (%tcsetattr output-port TCSAFLUSH new-output-termios))))
              (errno-error (errno) 'without-echo 'tcsetattr output-port proc)

              ;; For historical reasons, tcsetattr returns 0 if *any*
              ;; of the attribute changes took, so we must check to
              ;; see if all have been set
              (let ((real-new-output-termios (%tcgetattr output-port)))
                (if (not real-new-output-termios)
                    (begin (reset-terminal)
                           (errno-error (errno) 'without-echo 'tcgetattr output-port proc))
                    (if (not (equal? 0 (bitwise-and (term-attrs-lflag real-new-output-termios) the-lflags)))
                        (begin (reset-terminal)
                               (sanity-check-error "a termios update failed" 'without-echo output-port proc)))))))
        (lambda () (proc input-port output-port))
        (lambda ()
          (reset-terminal)))))
|#

;;; Low-level terminal manipulation

#|
BRKINT
CLOCAL
CREAD
CSTOPB
ECHO
ECHOE
ECHOK
ECHONL
HUP
HUPCL
ICANON
ICRNL
IEXTEN
IGNBRK
IGNCR
IGNPAR
INLCR
INPCK
ISIG
ISTRIP
IXANY
IXOFF
IXON
NOFLSH
OCRNL
ONLCR
ONLRET
ONOCR
OPOST
PARENB
PARMRK
PARODD
TABS
TOSTOP

;; Only one from the following groups are allowed:

CS5 CS6 CS7 CS8
|#

;; HUP = HUPCL

(define-record-type Terminal-State-Object
  (make-terminal-state-object termios)
  terminal-state-object?
  (termios get-termios))

#|no-brkint
clocal
no-clocal
cmspar
no-cmspar
cread
no-cread
crtscts
no-crtscts
cs5
cs6
cs7
cs8
cstopb
no-cstopb
echo
no-echo
echoctl
no-echoctl
echoe
no-echoe
echok
no-echok
echonl
no-echonl
hup
no-hup
hupcl
no-hupcl
icanon
no-icanon
icrnl
no-icrnl
iexten
no-iexten
ignbrk
no-ignbrk
igncr
no-igncr
ignpar
no-ignpar
imaxbel
no-imaxbel
inlcr
no-inlcr
inpck
no-inpck
isig
no-isig
istrip
no-istrip
iuclc
no-iuclc
ixany
no-ixany
ixoff
no-ixoff
ixon
no-ixon
noflsh
no-noflsh
now
drain
flush
ocrnl
no-ocrnl
olcuc
no-olcuc
onlcr
no-onlcr
onlret
no-onlret
onocr
no-onocr
opost
no-opost
parenb
no-parenb
parmrk
no-parmrk
parodd
no-parodd
tostop
no-tostop
xcase
no-xcase
|#



;;; Miscellaneous procedures

(define (terminal-file-name the-port)
  (if (not (port? the-port))
      (sanity-check-error "argument must be a port" 'terminal-file-name the-port))
  (let ((the-fd (port-fileno the-port)))
    (if (not (exact-integer? the-fd))
        (sanity-check-error "argument must be a port associated with a file descriptor" 'terminal-file-name the-port))
    (let ((the-filename (%ttyname_r the-fd)))
      (if (not the-filename)
          (errno-error (errno) 'terminal-file-name 'ttyname_r the-port)
          the-filename))))

(define (terminal-flow-control the-port the-action)
  (if (not (port? the-port))
      (sanity-check-error "first argument must be a port" 'terminal-flow-control the-port the-action))
  (let ((the-fd (port-fileno the-port)))
    (if (not (exact-integer? the-fd))
        (sanity-check-error "first argument must be a port associated with a file descriptor" 'terminal-flow-control the-port the-action))
    (if (not (exact-integer? the-action))
        (sanity-check-error "second argument must be an action that is an exact integer" 'terminal-flow-control the-port the-action))
    (if (not (%tcflow the-fd the-action))
             (errno-error (errno) 'terminal-flow-control 'tcflow the-port the-action))))

(define (terminal-wait the-port)
  (if (not (port? the-port))
      (sanity-check-error "argument must be a port" 'terminal-wait the-port))
  (let ((the-fd (port-fileno the-port)))
    (if (not (exact-integer? the-fd))
        (sanity-check-error "argument must be a port associated with a file descriptor" 'terminal-wait the-port))
    (if (not (retry-if-EINTR (lambda () (%tcdrain the-fd))))
        (errno-error (errno) 'terminal-wait 'tcdrain the-port))))

(define (terminal-discard the-port the-action)
  (if (not (port? the-port))
      (sanity-check-error "first argument must be a port" 'terminal-discard the-port the-action))
  (let ((the-fd (port-fileno the-port)))
    (if (not (exact-integer? the-fd))
        (sanity-check-error "first argument must be a port associated with a file descriptor" 'terminal-discard the-port the-action))
    (if (not (exact-integer? the-action))
        (sanity-check-error "second argument must be an action that is an exact integer" 'terminal-discard the-port the-action))
    (if (not (%tcflush the-fd the-action))
             (errno-error (errno) 'terminal-discard 'tcflush the-port the-action))))

(define (terminal-send-break the-port the-duration)
  (if (not (port? the-port))
      (sanity-check-error "first argument must be a port" 'terminal-send-break the-port the-duration))
  (let ((the-fd (port-fileno the-port)))
    (if (not (exact-integer? the-fd))
        (sanity-check-error "first argument must be a port associated with a file descriptor" 'terminal-send-break the-port the-duration))
    (if (not (exact-integer? the-duration))
        (sanity-check-error "second argument must be a duration that is an exact integer" 'terminal-send-break the-port the-duration))
    (if (not (%tcsendbreak the-fd the-duration))
             (errno-error (errno) 'terminal-send-break 'tcsendbreak the-port the-duration))))

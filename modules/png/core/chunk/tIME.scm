(define-module (png core chunk tIME)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:tIME>
            png-chunk:tIME-year
            png-chunk:tIME-month
            png-chunk:tIME-day
            png-chunk:tIME-hour
            png-chunk:tIME-minute
            png-chunk:tIME-second
            png-chunk-decode-tIME))


;; tIME chunk layout:
;;
;;   Year:   2 bytes (complete; for example, 1995, not 95)
;;   Month:  1 byte (1-12)
;;   Day:    1 byte (1-31)
;;   Hour:   1 byte (0-23)
;;   Minute: 1 byte (0-59)
;;   Second: 1 byte (0-60)    (yes, 60, for leap seconds; not 61,
;;                             a common error)
(define %tIME-chunk-length 7)



(define-class <png-chunk:tIME> (<png-chunk>)
  (year
   #:init-value   0
   #:init-keyword #:year
   #:getter       png-chunk:tIME-year)

  (month
   #:init-value   0
   #:init-keyword #:month
   #:getter       png-chunk:tIME-month)

  (day
   #:init-value   0
   #:init-keyword #:day
   #:getter       png-chunk:tIME-day)

  (hour
   #:init-value   0
   #:init-keyword #:hour
   #:getter       png-chunk:tIME-hour)

  (minute
   #:init-value   0
   #:init-keyword #:minute
   #:getter       png-chunk:tIME-minute)

  (second
   #:init-value   0
   #:init-keyword #:second
   #:getter       png-chunk:tIME-second))

(define-method (initialize (chunk <png-chunk:tIME>) initargs)
  (next-method)
  (slot-set! chunk 'type 'tIME))



(define-method (%display (chunk <png-chunk:tIME>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:tIME ~a: ~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d ~a>"
            (list-ref type 2)
            (png-chunk:tIME-year chunk)
            (png-chunk:tIME-month chunk)
            (png-chunk:tIME-day chunk)
            (png-chunk:tIME-hour chunk)
            (png-chunk:tIME-minute chunk)
            (png-chunk:tIME-second chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:tIME>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:tIME>) (port <port>))
  (%display chunk port))



(define-method (png-chunk-decode-tIME (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))
    (make <png-chunk:tIME>
      #:length length
      #:type   type
      #:data   data
      #:crc    crc
      #:year   (vector->int16 (bytevector-copy/part data 0 2))
      #:month  (bytevector-u8-ref data 2)
      #:day    (bytevector-u8-ref data 3)
      #:hour   (bytevector-u8-ref data 4)
      #:minute (bytevector-u8-ref data 5)
      #:second (bytevector-u8-ref data 6))))

(define-method (png-chunk-encode (chunk <png-chunk:tIME>))
  (let* ((year   (png-chunk:tIME-year chunk))
         (month  (png-chunk:tIME-month chunk))
         (day    (png-chunk:tIME-day chunk))
         (hour   (png-chunk:tIME-hour chunk))
         (minute (png-chunk:tIME-minute chunk))
         (second (png-chunk:tIME-second chunk))
         (data   (make-bytevector %tIME-chunk-length 0))
         (encoded-chunk (make <png-chunk>
                          #:type   'tIME
                          #:data   data
                          #:length %tIME-chunk-length)))
    (bytevector-copy! (int16->bytevector year)
                      0
                      data
                      0
                      2)
    (bytevector-u8-set! data 2 month)
    (bytevector-u8-set! data 3 day)
    (bytevector-u8-set! data 4 hour)
    (bytevector-u8-set! data 5 minute)
    (bytevector-u8-set! data 6 second)
    (png-chunk-crc-update! encoded-chunk)
    encoded-chunk))

(define-module (png core chunk-time)
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
            data->png-chunk:tIME))



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



(define-method (%display (chunk <png-chunk:tIME>) (port <port>))
  (let ((type (vector->chunk-type (png-chunk-type chunk))))
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



(define-method (data->png-chunk:tIME (data   <bytevector>)
                                     (type   <bytevector>)
                                     (length <number>)
                                     (crc    <bytevector>))
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
    #:second (bytevector-u8-ref data 6)))

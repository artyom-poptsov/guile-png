;;; Generated by Guile-SMC 0.6.0
;;; <https://github.com/artyom-poptsov/guile-smc>


(define-module
  (png fsm context)
  #:use-module
  (smc core common)
  #:use-module
  (smc context common)
  #:use-module
  (smc context u8)
  #:use-module
  (smc context char)
  #:use-module
  (smc core config)
  #:use-module
  (smc core log)
  #:use-module
  (smc context oop generic)
  #:use-module
  (smc context oop port)
  #:use-module
  (smc context oop char)
  #:use-module
  (smc context oop u8)
  #:re-export
  (<u8-context>
    u8-context?
    u8-context-pre-action
    u8-context-event-source
    u8-context-syntax-error
    u8-context-log-error
    u8-context-log-warning
    u8-context-log-info
    u8-context-log-debug
    <char-context>
    char-context?
    context-row-number
    context-row-number-set!
    context-row-number-update!
    context-col-number
    context-col-number-set!
    context-col-number-update!
    char-context-update-counters!
    char-context-event-source
    char-context-pre-action
    throw-syntax-error
    context-log-error
    context-log-warning
    context-log-info
    context-log-debug
    <port-context>
    port-context?
    context-port
    <context>
    context?
    context-debug-mode?
    context-debug-mode-set!
    context-counter
    context-counter-set!
    context-counter-update!
    context-stanza
    context-stanza/reversed
    context-stanza-set!
    context-buffer
    context-buffer/reversed
    context-buffer-set!
    context-result
    context-result-set!
    context-result/reversed
    buffer-empty?
    stanza-empty?
    result-empty?
    pop-buffer
    pop-stanza
    pop-result
    clear-buffer
    clear-stanza
    clear-result
    update-counter
    push-event-to-buffer
    push-event-to-stanza
    push-event-to-result
    push-buffer-to-stanza
    push-stanza-to-result
    u8:eof-object?
    u8:newline?
    u8:ascii?
    u8:hex-digit?
    u8:symbol?
    u8:punctuation?
    u8:blank?
    u8:whitespace?
    u8:printing?
    u8:graphic?
    u8:letter+digit?
    u8:digit?
    u8:upper-case?
    u8:lower-case?
    u8:letter?
    u8:del?
    u8:tilde?
    u8:right-curly-bracket?
    u8:vertical-line?
    u8:left-curly-bracket?
    u8:letter-z?
    u8:letter-y?
    u8:letter-x?
    u8:letter-w?
    u8:letter-v?
    u8:letter-u?
    u8:letter-t?
    u8:letter-s?
    u8:letter-r?
    u8:letter-q?
    u8:letter-p?
    u8:letter-o?
    u8:letter-n?
    u8:letter-m?
    u8:letter-l?
    u8:letter-k?
    u8:letter-j?
    u8:letter-i?
    u8:letter-h?
    u8:letter-g?
    u8:letter-f?
    u8:letter-e?
    u8:letter-d?
    u8:letter-c?
    u8:letter-b?
    u8:letter-a?
    u8:grave-accent?
    u8:low-line?
    u8:circumflex-accent?
    u8:right-square-bracket?
    u8:reverse-solidus?
    u8:left-square-bracket?
    u8:letter-Z?
    u8:letter-Y?
    u8:letter-X?
    u8:letter-W?
    u8:letter-V?
    u8:letter-U?
    u8:letter-T?
    u8:letter-S?
    u8:letter-R?
    u8:letter-Q?
    u8:letter-P?
    u8:letter-O?
    u8:letter-N?
    u8:letter-M?
    u8:letter-L?
    u8:letter-K?
    u8:letter-J?
    u8:letter-I?
    u8:letter-H?
    u8:letter-G?
    u8:letter-F?
    u8:letter-E?
    u8:letter-D?
    u8:letter-C?
    u8:letter-B?
    u8:letter-A?
    u8:at-symbol?
    u8:question-mark?
    u8:more-than-sign?
    u8:equals-sign?
    u8:less-than-sign?
    u8:semicolon?
    u8:colon?
    u8:digit-nine?
    u8:digit-eight?
    u8:digit-seven?
    u8:digit-six?
    u8:digit-five?
    u8:digit-four?
    u8:digit-three?
    u8:digit-two?
    u8:digit-one?
    u8:digit-zero?
    u8:solidus?
    u8:full-stop?
    u8:hyphen-minus?
    u8:comma?
    u8:plus-sign?
    u8:asterisk?
    u8:right-parenthesis?
    u8:left-parenthesis?
    u8:single-quote?
    u8:ampersand?
    u8:percent-sign?
    u8:dollar-sign?
    u8:number-sign?
    u8:double-quote?
    u8:exclamation-mark?
    u8:space?
    u8:us?
    u8:rs?
    u8:gs?
    u8:fs?
    u8:esc?
    u8:sub?
    u8:em?
    u8:can?
    u8:etb?
    u8:syn?
    u8:nak?
    u8:dc4?
    u8:dc3?
    u8:dc2?
    u8:dc1?
    u8:dle?
    u8:si?
    u8:so?
    u8:cr?
    u8:ff?
    u8:vt?
    u8:lf?
    u8:tab?
    u8:bs?
    u8:bel?
    u8:ack?
    u8:enq?
    u8:eot?
    u8:etx?
    u8:stx?
    u8:soh?
    u8:nul?
    u8:make-char-guard
    u8:make-charset-guard
    char:eof-object?
    char:newline?
    char:ascii?
    char:hex-digit?
    char:symbol?
    char:punctuation?
    char:blank?
    char:whitespace?
    char:printing?
    char:graphic?
    char:letter+digit?
    char:digit?
    char:upper-case?
    char:lower-case?
    char:letter?
    char:del?
    char:tilde?
    char:right-curly-bracket?
    char:vertical-line?
    char:left-curly-bracket?
    char:letter-z?
    char:letter-y?
    char:letter-x?
    char:letter-w?
    char:letter-v?
    char:letter-u?
    char:letter-t?
    char:letter-s?
    char:letter-r?
    char:letter-q?
    char:letter-p?
    char:letter-o?
    char:letter-n?
    char:letter-m?
    char:letter-l?
    char:letter-k?
    char:letter-j?
    char:letter-i?
    char:letter-h?
    char:letter-g?
    char:letter-f?
    char:letter-e?
    char:letter-d?
    char:letter-c?
    char:letter-b?
    char:letter-a?
    char:grave-accent?
    char:low-line?
    char:circumflex-accent?
    char:right-square-bracket?
    char:reverse-solidus?
    char:left-square-bracket?
    char:letter-Z?
    char:letter-Y?
    char:letter-X?
    char:letter-W?
    char:letter-V?
    char:letter-U?
    char:letter-T?
    char:letter-S?
    char:letter-R?
    char:letter-Q?
    char:letter-P?
    char:letter-O?
    char:letter-N?
    char:letter-M?
    char:letter-L?
    char:letter-K?
    char:letter-J?
    char:letter-I?
    char:letter-H?
    char:letter-G?
    char:letter-F?
    char:letter-E?
    char:letter-D?
    char:letter-C?
    char:letter-B?
    char:letter-A?
    char:at-symbol?
    char:question-mark?
    char:more-than-sign?
    char:equals-sign?
    char:less-than-sign?
    char:semicolon?
    char:colon?
    char:digit-nine?
    char:digit-eight?
    char:digit-seven?
    char:digit-six?
    char:digit-five?
    char:digit-four?
    char:digit-three?
    char:digit-two?
    char:digit-one?
    char:digit-zero?
    char:solidus?
    char:full-stop?
    char:hyphen-minus?
    char:comma?
    char:plus-sign?
    char:asterisk?
    char:right-parenthesis?
    char:left-parenthesis?
    char:single-quote?
    char:ampersand?
    char:percent-sign?
    char:dollar-sign?
    char:number-sign?
    char:double-quote?
    char:exclamation-mark?
    char:space?
    char:us?
    char:rs?
    char:gs?
    char:fs?
    char:esc?
    char:sub?
    char:em?
    char:can?
    char:etb?
    char:syn?
    char:nak?
    char:dc4?
    char:dc3?
    char:dc2?
    char:dc1?
    char:dle?
    char:si?
    char:so?
    char:cr?
    char:ff?
    char:vt?
    char:lf?
    char:tab?
    char:bs?
    char:bel?
    char:ack?
    char:enq?
    char:eot?
    char:etx?
    char:stx?
    char:soh?
    char:nul?
    char:make-guard
    char:make-charset-guard
    #{guard:#t}#
    action:no-op
    <precise-logger>
    precise-logger?
    <system-log>
    system-log?
    <null-log>
    null-log?
    <precise-port-log>
    precise-port-log?
    <stderr-log>
    stderr-log?
    smc-log-init!
    smc-log
    log-add-handler!
    log-remove-handler!
    log-clear-handlers!
    log-error
    log-warning
    log-info
    log-debug
    log-use-stderr!
    log-level-enabled?
    %precise-log-formatter
    %precise-log-helper
    define-method-with-docs
    object-address/hex-string
    safe-module-ref
    safe-module-list-ref
    %logger-binary))

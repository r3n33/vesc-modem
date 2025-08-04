@const-start

; TODO: Set CAOPEN TIMEOUT "AT+CACFG=\"TIMEOUT\",0,%d\r\n"

(defun modem-tcp-is-connected () {
    (var cid (if (ix (rest-args) 0) (ix (rest-args) 0) 0))

    (modem-cmd "AT+CASTATE?")
    (if (not (modem-wait-ok 0.5))
        (puts "Error getting tcp socket state")
    )

    (eq (ix (assoc modem-state 'tcp-cid-states) cid) 'connected)
})

; <server> <= 64 bytes; IP or hostname
; <port> ex. 80, 8080, 443
; <cid> optional TCP/UDP Identifier (0-12); defaults to 0
; <pdp-idx> optional Index of PDP connection; defaults to 0
(defunret modem-tcp-open (server port) {
    (var cid (if (ix (rest-args) 0) (ix (rest-args) 0) 0))
    (var pdp-idx (if (ix (rest-args) 1) (ix (rest-args) 1) 0))

    (if (not-eq (ix (assoc modem-state 'tcp-cid-states) cid) 'closed)
        (modem-tcp-close cid))

    (prepare-state)

    ; AT+CAOPEN=<cid>,<pdp_index>,<conn_type>,<server>,<port>[,<recv_mode>]
    ; TODO: May want to utilize recv_mode 1?
    ; 0 = The received data can only be read manually using AT+CARECV=<cid>
    ; 1 = After receiving the data, it will automatically report URC:
    ;     +CAURC: "recv",<id>,<length>[,<remoteIP>,<remote_port>]<CR><LF><data>
    (var cmd (str-merge "AT+CAOPEN=" (str-from-n cid)"," (str-from-n pdp-idx)",\"TCP\"," server "," (str-from-n port) "\r\n"))
    (if dev-debug-modem (puts cmd))
    (uart-write cmd)

    (modem-wait-ok 3.0) ; TODO: Optional timeout parameter
    ; TODO: Received: "\r\nR1951.04\r\n\r\nNO CARRIER\r\n" when PDP was INACTIVE

    (if (eq (ix (assoc modem-state 'tcp-cid-states) cid) 'connected)
        (progn
            (if dev-debug-modem (print "Connected"))
            (return true)))

    nil
})

; <cid> optional TCP/UDP Identifier (0-12); defaults to 0
(defun modem-tcp-close () {
    (var cid (if (ix (rest-args) 0) (ix (rest-args) 0) 0))

    (prepare-state)

    (uart-write (str-merge "AT+CACLOSE=" (str-from-n cid) "\r\n"))

    (if (modem-wait-ok 1.0)
        (progn
            (if dev-debug-modem (print "Disconnected"))
            (setassoc modem-state 'tcp-cid-states (setix (assoc modem-state 'tcp-cid-states) cid 'closed))
            true)
        (progn
            (if dev-debug-modem (print (list "CACLOSE TIMEOUT" (assoc modem-state 'cme-error))))
            (setassoc modem-state 'tcp-cid-states (setix (assoc modem-state 'tcp-cid-states) cid 'error))
            false)
    )
})

; <arr> data to send
; <len> optional length of data to send
; <cid> optional TCP/UDP Identifier (0-12); defaults to 0
(defunret modem-tcp-send (arr) {
    (var len (if (ix (rest-args) 0) (ix (rest-args) 0) (str-len arr)))
    (var cid (if (ix (rest-args) 1) (ix (rest-args) 1) 0))

    (mutex-lock modem-mutex)
    (prepare-state)

    (uart-write (str-merge "AT+CASEND=" (str-from-n cid) "," (str-from-n len) "\r\n"))

    ; Wait for ">" response
    (var start (systime))
    (var tout 5.0)
    (loopwhile (not (assoc modem-state 'data-input-rdy))
        (progn
            (if (> (secs-since start) tout) (progn
                (if dev-debug-modem (print "Wait for > timeout! Unable to send"))
                (mutex-unlock modem-mutex)
                (return nil)))
            (sleep 0.05)))

    (uart-write arr)
    (var ret (modem-wait-ok 1.0))

    (mutex-unlock modem-mutex)

    ret
})

; <len> Number of bytes to read
; <tout> optional Timeout in seconds; defaults to 1.0
; <as-str> optional Add a null byte to returned data (default true)
; <cid> optional TCP/UDP Identifier (0-12); defaults to 0
(defun modem-tcp-recv (len) {
    (var tout (if (ix (rest-args) 0) (ix (rest-args) 0) 1.0))
    (var as-str (rest-args 1))
    (var cid (if (ix (rest-args) 2) (ix (rest-args) 2) 0))

    (mutex-lock modem-mutex)

    (prepare-state)
    (setassoc modem-state 'carecv-data (bufcreate 0))

    ; Split carecv requests and combine output to not overflow uart buffer
    (var start (systime))
    (var socket-recv-pos 0)
    (loopwhile (< socket-recv-pos len) {
        (setassoc modem-state 'carecv-busy true)

        (var read-len (if (> len modem-rx-len ) modem-rx-len  len))
        (var res (uart-write (str-merge "AT+CARECV=" (str-from-n cid) "," (str-from-n read-len) "\r\n")))

        (loopwhile (assoc modem-state 'carecv-busy) {
            (if (> (secs-since start) tout) {
                (if dev-debug-modem (print (list "+CARECV is stuck; uart-write result was" res)))
                (break)
            })
            (sleep 0.05)
        })

        (setq socket-recv-pos (buflen (assoc modem-state 'carecv-data)))
        (if dev-debug-modem (print (list "socket-recv-pos" socket-recv-pos "/" len)))

        (if (> (secs-since start) tout) {
            (if dev-debug-modem (print (list "+CARECV timeout, returning")))
            (break)
        })
    })

    (if as-str (append-buf (assoc modem-state 'carecv-data) [0 0] 1))

    (mutex-unlock modem-mutex)

    (if dev-debug-modem (puts "modem-tcp-recv complete"))

    (assoc modem-state 'carecv-data)
})

; <cid> optional TCP/UDP Identifier (0-12); defaults to 0
(defunret modem-tcp-status () {
    (var cid (if (ix (rest-args) 0) (ix (rest-args) 0) 0))
    ; "\r\nOK\r\n"
    ; "\r\n+CASTATE: 0,1\r\n\r\nOK\r\n"
    (uart-write "AT+CASTATE?\r\n")

    (if (> cid 12) (return 'invalid-cid))

    (modem-wait-ok 1.0)

    (ix (assoc modem-state 'tcp-cid-states) cid)
})

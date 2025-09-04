@const-start

(defun http-type-to-str (type) (match type
    (get "1")
    (put "2")
    (post "3")
    (patch "4")
    (head "5")
))

; NOTE: This function needs exclusive access to the modem.
;       In a threaded program guard with modem-mutex
(defunret modem-http-request (server-address endpoint request-type) {
    (var content-type (rest-args 0))
    (var content (rest-args 1))
    (var content-len
        (if (rest-args 2)
            (rest-args 2)
            (if (eq (type-of content) 'type-array)
                (str-len content)
                nil)))

    (if dev-debug-modem (puts (str-merge "modem-http-request: " server-address endpoint " <" (to-str request-type))))

    (prepare-shconn)
    (modem-cmd (str-merge "AT+SHCONF=\"URL\",\"" server-address "\""))
    (modem-cmd "AT+SHCONF=\"BODYLEN\",1024")
    (modem-cmd "AT+SHCONF=\"HEADERLEN\",350")
    (modem-cmd "AT+SHCONN" 5)
    (modem-cmd "AT+SHSTATE?")
    (if (not (is-sh-connected)) {
        (puts "Connection failed")
        (return 'connect-failed)
    })

    ; Set header content
    (modem-cmd "AT+SHCHEAD") ; Clear header
    (if (not-eq content-type nil)
        (modem-cmd (str-merge "AT+SHAHEAD=\"Content-Type\",\"" content-type "\"")))
    (modem-cmd "AT+SHAHEAD=\"User-Agent\",\"curl/7.47.0\"")
    (modem-cmd "AT+SHAHEAD=\"Cache-control\",\"no-cache\"")
    (modem-cmd "AT+SHAHEAD=\"Connection\",\"keep-alive\"")
    (modem-cmd "AT+SHAHEAD=\"Accept\",\"*/*\"")

    ; Set body content
    (modem-cmd "AT+SHCPARA") ; Clear body content
    (if (and content content-len) {
        (modem-cmd (str-merge "AT+SHBOD=" (str-from-n content-len) ",3000")) ; Allow 3 seconds to input content
        (if (modem-wait 'data-input-rdy 3) {
            (uart-write content)
            (modem-wait-ok 1) ; Wait up to 1 second for OK
        } (puts "error: modem-http-request data-input-rdy timeout"))
    })
    (modem-cmd (str-merge "AT+SHREQ=\"" endpoint "\"," (http-type-to-str request-type)) 1.0)

    (modem-wait 'shreq-bytes 10.0)
})

(defun modem-http-read (pos len) {
    (var tout (if (rest-args 0) (rest-args 0) 1.0))
    (var as-str (rest-args 1))

    (setassoc modem-state 'shread-data nil)
    (modem-cmd (str-merge "AT+SHREAD=" (str-from-n pos) "," (str-from-n len)) 1.0)
    (modem-wait 'shread-data tout)

    (if as-str (append-buf (assoc modem-state 'shread-data) [0 0] 1))
    (assoc modem-state 'shread-data)
})

(defun modem-http-disconnect () {
    (var res (modem-cmd "AT+SHDISC"))
    (if res (setassoc modem-state 'shstate 0))
    res
})

(defun modem-http-post (address endpoint content) {
    (var len (if (rest-args 0) (rest-args 0) (str-len content)))
    (var content-type (if (rest-args 1) (rest-args 1) "application/json"))

    (mutex-lock modem-mutex)
    (setassoc modem-state 'shread-data nil)

    (var resp (bufcreate 0))
    (var bytes (modem-http-request
        address
        endpoint
        'post
        content-type
        content
        len
    ))

    (if (eq (type-of bytes) 'type-symbol) {
        (puts (str-merge "modem-http-post: error: " (to-str bytes)))
    } {
        (var pos 0)
        (loopwhile (< pos bytes) {
            (var bytes-remaining (- bytes pos))
            (var len (if (> bytes-remaining modem-rx-len) modem-rx-len bytes-remaining))
            (var buf (modem-http-read (- bytes bytes-remaining) len 1 true))
            (if buf {
                (append-buf resp buf len)
                (setq pos (+ pos (str-len buf)))
            })
        })
    })

    (modem-http-disconnect)

    (mutex-unlock modem-mutex)
    (list (assoc modem-state 'shreq-code) resp)
})

(defun modem-http-get (address endpoint) {
    (mutex-lock modem-mutex)

    (var resp (bufcreate 0))
    (var bytes (modem-http-request address endpoint 'get))

    (if (eq (type-of bytes) 'type-i) {
        (var pos 0)
        (loopwhile (< pos bytes) {
            (var bytes-remaining (- bytes pos))
            (var len (if (> bytes-remaining modem-rx-len) modem-rx-len bytes-remaining))
            (var buf (modem-http-read (- bytes bytes-remaining) len 1 true))
            (if buf {
                (append-buf resp buf len)
                (setq pos (+ pos (str-len buf)))
            })
        })
    })

    (modem-http-disconnect)
    (mutex-unlock modem-mutex)
    (list (assoc modem-state 'shreq-code) resp)
})

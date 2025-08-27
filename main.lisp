@const-start

(def dev-modem-gnss-enable nil) ; TODO: Enabling GNSS will suspend use of GPRS

(def dev-include-modem-examples t)

(import "modem/hw-link.lisp" 'code-hw-specific)
(read-eval-program code-hw-specific)

(import "modem/modem.lisp" 'code-modem)
(read-eval-program code-modem)
(import "modem/rx-thread.lisp" 'rx-thread)
(read-eval-program rx-thread)
(import "modem/tcp-socket.lisp" 'tcp-socket)
(read-eval-program tcp-socket)
(import "modem/tcp-hub.lisp" 'tcp-hub)
(read-eval-program tcp-hub)
(import "modem/file-system.lisp" 'file-system)
(read-eval-program file-system)
(import "modem/gnss.lisp" 'modem-gnss)
(read-eval-program modem-gnss)
(import "modem/mqtt.lisp" 'code-mqtt)
(read-eval-program code-mqtt)
(import "modem/http.lisp" 'modem-http)
(read-eval-program modem-http)
(import "modem/thingsboard.lisp" 'code-things)
(read-eval-program code-things)

(if dev-include-modem-examples {
    (import "examples/file-system.lisp" 'file-system-examples)
    (read-eval-program file-system-examples)

    (import "examples/gnss.lisp" 'gnss-examples)
    (read-eval-program gnss-examples)

    (import "examples/http.lisp" 'http-examples)
    (read-eval-program http-examples)

    (import "examples/thingsboard.lisp" 'thingsboard-examples)
    (read-eval-program thingsboard-examples)

    (import "examples/tcp-socket.lisp" 'tcp-socket-examples)
    (read-eval-program tcp-socket-examples)
})

(start-rx-thd) ; The RX thread processes incoming data from the modem

(modem-debug true)
(if (init-modem)
    (progn
        (modem-debug false)
        (modem-cmd "ATE0") ; Disable ECHO
        (modem-cmd "AT+CMEE=2") ; Verbose errors
        (apn-load)
        (auto-activation)
        (modem-ntp-sync)
        (prepare-https)

        (modem-tcp-close) ; Closing connection in case it was left open while debugging

        (if dev-modem-gnss-enable
            (modem-gnss-enable)
            (modem-gnss-disable)
        )

        (print-rssi (modem-rssi))
    )
    (loopwhile t {
        (puts "init-modem failed. please enable debugging and try again")
        (sleep 5)
    })
)

(if dev-include-modem-examples {
    (example-save-file)
    (example-read-file)

    (example-gnss-update)

    (example-http-get)
    (example-https-post)

    (example-tcp-socket)

    (example-tcp-hub-connect)
    (sleep 5)
    (example-tcp-hub-disconnect)

    (example-thingsboard-mqtt-publish)
    (example-thingsboard-mqtt-subscribe)
    (sleep 5)
    (example-thingsboard-mqtt-unsubscribe)
})

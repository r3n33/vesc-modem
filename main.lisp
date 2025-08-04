@const-start

(def dev-modem-gnss-enable nil) ; TODO: Enabling GNSS will suspend use of GPRS

(def dev-include-modem-tests t)
(def dev-perform-modem-tests nil)

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
(import "modem/file-download.lisp" 'file-download)
(read-eval-program file-download)
(import "modem/gnss.lisp" 'modem-gnss)
(read-eval-program modem-gnss)
(import "modem/mqtt.lisp" 'code-mqtt)
(read-eval-program code-mqtt)

(if dev-include-modem-tests {
    (import "modem/modem-tests.lisp" 'modem-tests)
    (read-eval-program modem-tests)
    (def dev-debug-modem t)
    ;(def dev-debug-modem nil)
})

(import "thingsboard.lisp" 'code-things)
(read-eval-program code-things)

(start-rx-thd)
(if (init-modem) {
    (modem-cmd "ATE0") ; Disable ECHO
    (modem-cmd "AT+CMEE=2") ; Verbose errors
    (apn-load)
    (auto-activation)
    (modem-ntp-sync)
    (prepare-https)

    (modem-tcp-close) ; Closing connection in case it was left open while debugging

    (if dev-modem-gnss-enable (modem-gnss-enable))

    (print-rssi (modem-rssi))

    (if dev-perform-modem-tests {
        (test-http) ; HTTP SH Command Examples
        (test-https) ; HTTPS SH Command Examples
        (test-tcp-socket) ; TCP Socket Example

        (modem-save-file "https://raw.githubusercontent.com/vedderb/vesc_express/refs/heads/main/README.md" "vesc.md" 'blocking) ; Download file to flash example
        (puts (str-from-n (modem-file-size "vesc.md") "Downloaded file size: %d bytes"))

        (print-file-contents "vesc.md") ; Read file from flash example

        (if dev-modem-gnss-enable {
            (modem-gnss-update)
            (print modem-gnss-state)
            (gnss-lat-lon)
            (gnss-height)
            (gnss-speed)
            (gnss-date-time)
        })

        (print-modem-state)

        ;(spawn things-test-mqtt)
        ;(def run-test nil)

        ;(mqtt-set-callback mqtt-example-cb)
        ;(mqtt-subscribe "v1/devices/me/attributes")
        ;(mqtt-unsubscribe "v1/devices/me/attributes")
    })

    ;(spawn activate-tcp-hub)
    ;(exit-tcp-hub)
})

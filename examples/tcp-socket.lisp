@const-start

(defun example-tcp-socket () {
    (modem-tcp-close)
    (modem-tcp-open "httpbin.org" 80)

    (if (modem-tcp-send "GET /get?user=jack&password=123 HTTP/1.1\r\nHost: httpbin.org\r\n\r\n")
        (if (modem-wait-tcp-data 0 10.0)
            ; Read some data from the socket
            (puts (modem-tcp-recv 512 0.5))))

    (modem-tcp-close)
})

(defun example-tcp-hub-connect () {
    (spawn activate-tcp-hub)
})

(defun example-tcp-hub-disconnect () {
    (exit-tcp-hub)
})

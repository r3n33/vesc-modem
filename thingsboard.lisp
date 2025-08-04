@const-start

(defun thing-make-set-req (host token fields) {
        (var str-content (str-merge "{" (str-join fields ", ") "}"))
        (str-merge
            "POST /api/v1/" token "/telemetry HTTP/1.1\r\n"
            "Host: " host "\r\n"
            "Content-Type:application/json\r\n"
            "Content-Length: " (str-from-n (str-len str-content)) "\r\n\r\n"
            str-content
        )
})

(defun thing-make-get-req (host token) {
        (str-merge
            ;"GET /api/v1/" token "/attributes/updates?timeout=20000 HTTP/1.1\r\n" ; Wait until updates
            "GET /api/v1/" token "/attributes HTTP/1.1\r\n" ; Get all attributes
            "Host: " host "\r\n"
            "Content-Type:application/json\r\n\r\n"
            ;"connection: Close\r\n\r\n"
        )
})

(defun thing-send-field-mqtt (host topic token) {
    (if (eq (mqtt-state) 'disconnected)
        (mqtt-connect host token)
    )

    (var fields (rest-args))
    (var str-content (str-merge "{" (str-join fields ", ") "}"))
    (mqtt-publish topic str-content)

    (print "Done!")
})

(defun thing-send-field-esp (host token) {
        (def socket (tcp-connect host 80))

        (var str-to-send (thing-make-set-req host token (rest-args)))
        ;(puts str-to-send)
        (tcp-send socket str-to-send)
        (puts (tcp-recv socket 200))
        (tcp-close socket)
        (print "Done!")
})

(defun thing-send-field (host token) {
        (modem-tcp-close)
        (modem-tcp-open host 80)

        (var str-to-send (thing-make-set-req host token (rest-args)))
        ;(puts str-to-send)
        (print (modem-tcp-send str-to-send))

        (modem-tcp-close)
        (print "Done!")
})

(defun thing-get-attr-esp (host token) {
        (def socket (tcp-connect host 80))

        (var str-to-send (thing-make-get-req host token))
        (puts str-to-send)
        (tcp-send socket str-to-send)
        (map (fn (x) (print x)) (str-split (tcp-recv socket 600) "\n"))
        (tcp-close socket)

        ; TODO: Parse response!

        (print "Done!")
})

(defun thing-get-attr (host token) {
        (modem-tcp-close)
        (modem-tcp-open host 80)

        (var str-to-send (thing-make-get-req host token))
        (puts str-to-send)
        (print (modem-tcp-send str-to-send))

        (if (modem-wait-tcp-data 0 10.0)
            (map (fn (x) (print x)) (str-split (modem-tcp-recv 400 10.0) "\n"))
        )

        (modem-tcp-close)

        ; TODO: Parse response!

        (print "Done!")
})

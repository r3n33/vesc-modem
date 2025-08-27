@const-start

; TODO: Make these available via QML
(def thingsboard-token "hlu91xostd9d4clqfhfj")
(def thingsboard-host "things.vedder.se")
(def thingsboard-publish-topic "v1/devices/me/telemetry")
(def thingsboard-subscribe-topic "v1/devices/me/attributes")

(defun example-thingsboard-mqtt-publish () {
    (var pos (gnss-lat-lon))
    (thing-send-field-mqtt thingsboard-host thingsboard-publish-topic thingsboard-token
        (str-merge "lat:" (str-from-n (ix pos 0)))
        (str-merge "lon:" (str-from-n (ix pos 1)))
        (str-merge "temperature:" (str-from-n (/ (mod (rand) 400) 10.0)))
    )
})

(defun example-thingsboard-mqtt-subscribe () {
    (mqtt-set-callback example-mqtt-callback)
    (mqtt-subscribe thingsboard-subscribe-topic)
    ; All messages published to subscribe-topic will be sent to the callback
})

(defun example-thingsboard-mqtt-unsubscribe () {
    (mqtt-unsubscribe thingsboard-subscribe-topic)
})

(defun example-mqtt-callback (topic msg) {
    (if dev-debug-modem (print (list "example-mqtt-callback" topic msg)))

    (match topic
        ("v1/devices/me/attributes" (puts (str-merge topic " = " msg)))
    )
})

(defun example-thingsboard-http () {
    (thing-send-field thingsboard-host thingsboard-token "temperature:27")
    (thing-send-field thingsboard-host thingsboard-token "lat:58.074422" "lon:13.645292")

    (thing-send-field-esp thingsboard-host thingsboard-token "lat:58.074222" "lon:13.646292")

    (thing-get-attr-esp thingsboard-host thingsboard-token)
    (thing-get-attr thingsboard-host thingsboard-token)
})

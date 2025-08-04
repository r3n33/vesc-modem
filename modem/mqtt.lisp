@const-start

; TODO: The topic and message are the LWT and need to be tested
;;(modem-cmd "AT+SMCONF=\"TOPIC\",\"v1/devices/me/telemetry\"") ; Set thingsboard telemetry topic
;;(modem-cmd (str-merge "AT+SMCONF=\"MESSAGE\",\"{\\\"temperature\\\":40}\""))

; <status>
;   0 MQTT disconnect state
;   1 MQTT on-line state
;   2 MQTT on-line state and SP(Session Present) flag is set
; (modem-cmd "AT+SMSTATE?")

; TODO: Something hangs when this is executed
; (modem-cmd "AT+SMCONF?")

; (mqtt-connect "things.vedder.se" vl-token)
; (mqtt-publish "v1/devices/me/telemetry" "{\"temperature\":42}")
; (mqtt-disconnect)

(defun mqtt-state () {
    (setassoc modem-state 'mqtt-state 'unknown)
    (modem-cmd "AT+SMSTATE?" 5)
    (assoc modem-state 'mqtt-state)
})

(defun mqtt-disconnect () {
    (modem-cmd "AT+SMDISC")
})

(defun mqtt-connect (address username) {
    (var mqtt-port (if (rest-args 0) (rest-args 0) 1883))

    ; TODO: Check state and close if already connected

    (modem-cmd (str-merge "AT+SMCONF=\"URL\",\"" address "\"," (str-from-n mqtt-port))) ; Setup server URL
    (modem-cmd "AT+SMCONF=\"KEEPTIME\",60") ; Hold connection time (0-65535 seconds)
    (modem-cmd "AT+SMCONF=\"CLEANSS\",1") ; Clear session
    ;(modem-cmd "AT+SMCONF=\"ASYNCMODE\",0")
    ;(modem-cmd "AT+SMCONF=\"ASYNCMODE\",1")
    ;(modem-cmd "AT+SMCONF=\"QOS\",0")
    ;(modem-cmd "AT+SMCONF=\"RETAIN\",0")
    (modem-cmd (str-merge "AT+SMCONF=\"USERNAME\",\"" username "\""))
    (modem-cmd "AT+SMCONN" 5)
})

(defunret mqtt-publish (topic message) {
    (var len (if (rest-args 0) (rest-args 0) (str-len message)))

    (mutex-lock modem-mutex)

    (prepare-state)

    ; AT+SMPUB=<topic>,<content length>,<qos>,<retain><CR>
    ;   message is entered
    ;   Quit edit mode if message length equals to <content length>.
    (modem-cmd (str-merge "AT+SMPUB=\"" topic "\"," (str-from-n len)",1,1"))

    ; Wait for ">" response
    (var start (systime))
    (var tout 5.0)
    (loopwhile (not (assoc modem-state 'data-input-rdy))
        (progn
            (if (> (secs-since start) tout) (progn
                (if dev-debug-modem (print "Wait for > timeout! Unable to publish"))
                (mutex-unlock modem-mutex)
                (return nil)))
            (sleep 0.05)))

    (uart-write message)
    (var ret (modem-wait-ok 1.0))

    (mutex-unlock modem-mutex)

    ret
})

(defun mqtt-set-callback (cb) {
    (setassoc modem-state 'mqtt-callback cb)
})

; <topic> Up to 128 bytes in length
; <qos> optional QOS level
    ; 'at-most-once
    ; 'at-least-once
    ; 'only-once
(defun mqtt-subscribe (topic) {
    (var qos (if (rest-args 0) (rest-args 0) 'at-least-once))

    (prepare-state)

    (modem-cmd (str-merge "AT+SMSUB=\"" topic "\"," (match qos
        (at-most-once "0")
        (at-least-once "1")
        (only-once "2")
        (_ (progn
            (puts "Invalid mqtt-subscribe qos")
            "1"
        ))
    )))
})

(defun mqtt-unsubscribe (topic) {
    (prepare-state)
    (modem-cmd (str-merge "AT+SMUNSUB=\"" topic "\""))
})

(defun mqtt-example-cb (topic msg) {
    (if dev-debug-modem (print (list "mqtt-example-cb" topic msg)))

    (match topic
        ("v1/devices/me/attributes" (puts msg))
    )
})

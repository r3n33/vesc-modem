@const-start

(defunret test-http () {
    (puts "Testing HTTP Connection")
    (prepare-shconn)

    (modem-cmd "AT+SHCONF=\"URL\",\"http://httpbin.org\"")
    (modem-cmd "AT+SHCONF=\"BODYLEN\",1024")
    (modem-cmd "AT+SHCONF=\"HEADERLEN\",350")
    (modem-cmd "AT+SHCONN" 5)
    (modem-cmd "AT+SHSTATE?")
    (if (not (is-sh-connected)) {
        (puts "Connection failed")
        (return 'connect-failed)
    })

    (modem-cmd "AT+SHCHEAD")
    (modem-cmd "AT+SHAHEAD=\"User-Agent\",\"curl/7.47.0\"")
    (modem-cmd "AT+SHAHEAD=\"Cache-control\",\"no-cache\"")
    (modem-cmd "AT+SHAHEAD=\"Connection\",\"keep-alive\"")
    (modem-cmd "AT+SHAHEAD=\"Accept\",\"*/*\"")

    (modem-cmd "AT+SHREQ=\"/get?user=jack&password=123\",1" 1.0)

    (def bytes (modem-wait 'shreq-bytes 10.0))
    (if (not-eq bytes nil)
        (modem-cmd (str-merge "AT+SHREAD=0," bytes) 1.0 (+ (to-i bytes) 25) true))

    (modem-wait 'shread-data 5.0)
    (puts (assoc modem-state 'shread-data))
    (setassoc modem-state 'shread-data nil) ; Done with data
    (if (modem-cmd "AT+SHDISC")
        (setassoc modem-state 'shstate 0))
})

(defun test-http-get () {
    (var bytes (modem-http-request "http://httpbin.org" "/get?user=jack&password=123" 'get))
    (var pos 0)
    (loopwhile (< pos bytes) {
        (var bytes-remaining (- bytes pos))
        (var len (if (> bytes-remaining modem-rx-len) modem-rx-len bytes-remaining))
        (var buf (modem-http-read (- bytes bytes-remaining) len 1 true))
        (if buf {
            (puts buf)
            (setq pos (+ pos (str-len buf)))
        })
    })

    (modem-http-disconnect)
})

(defunret test-https () {
    (puts "Testing HTTPS Connection")
    (prepare-shconn)
    (prepare-https)
    (modem-cmd "AT+SHCONF=\"URL\",\"https://httpbin.org\"")
    (modem-cmd "AT+SHCONF=\"BODYLEN\",1024")
    (modem-cmd "AT+SHCONF=\"HEADERLEN\",350")
    (modem-cmd "AT+SHCONN" 45)
    (modem-cmd "AT+SHSTATE?")
    (if (not (is-sh-connected)) {
        (puts "Connection failed")
        (return 'connect-failed)
    })

    (modem-cmd "AT+SHCHEAD")
    (modem-cmd "AT+SHAHEAD=\"Content-Type\",\"application/x-www-form-urlencoded\"")
    (modem-cmd "AT+SHAHEAD=\"Cache-control\",\"no-cache\"")
    (modem-cmd "AT+SHAHEAD=\"Connection\",\"keep-alive\"")
    (modem-cmd "AT+SHAHEAD=\"Accept\",\"*/*\"")

    (modem-cmd "AT+SHCPARA")
    (modem-cmd "AT+SHPARA=\"product\",\"apple\"")
    (modem-cmd "AT+SHPARA=\"price\",\"1\"")

    (modem-cmd "AT+SHREQ=\"/post\",3" 10.0)
    (def bytes (modem-wait 'shreq-bytes 10.0))
    (if (not-eq bytes nil)
        (modem-cmd (str-merge "AT+SHREAD=0," bytes) 1.0 (+ (to-i bytes) 25) true))

    (modem-wait 'shread-data 5.0)
    (puts (assoc modem-state 'shread-data))
    (setassoc modem-state 'shread-data nil) ; Done with data
    (if (modem-cmd "AT+SHDISC")
        (setassoc modem-state 'shstate 0))
})

(defun test-tcp-socket () {
    (puts "Testing TCP Socket")
    (modem-tcp-close)
    (modem-tcp-open "httpbin.org" 80)
    (modem-tcp-status)
    (sleep 1.0) ; TODO: Not sure why some delay is needed before AT+CASEND will work reliably
    (if (modem-tcp-send "GET /get?user=jack&password=123 HTTP/1.1\r\nHost: httpbin.org\r\n\r\n")
        (if (modem-wait-tcp-data 0 10.0)
            ; Read some data from the socket
            (puts (modem-tcp-recv 512 0.5))))

    (modem-tcp-close)
})

(def vl-token "R8Ldt1tHVe7CVn83Ls9C") ; Benjamin Test
(if false {
        (thing-send-field "things.vedder.se" vl-token "temperature:27")
        (thing-send-field "things.vedder.se" vl-token "lat:58.074422" "lon:13.645292")

        (thing-send-field-esp "things.vedder.se" vl-token "lat:58.074222" "lon:13.646292")

        (thing-get-attr-esp "things.vedder.se" vl-token)
        (thing-get-attr "things.vedder.se" vl-token)
})

(defun things-test-mqtt () {
    (def vl-token "hlu91xostd9d4clqfhfj") ; Renee Test
    (def run-test t)
    (loopwhile run-test {
        (var pos (gnss-lat-lon))
        (thing-send-field-mqtt "things.vedder.se" "v1/devices/me/telemetry" vl-token
            (str-merge "lat:" (str-from-n (ix pos 0)))
            (str-merge "lon:" (str-from-n (ix pos 1)))
            (str-merge "temperature:" (str-from-n (/ (mod (rand) 400) 10.0)))
        )
        (sleep 1)
    })
})
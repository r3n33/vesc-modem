(def dev-debug-modem nil)

(def modem-state (list
    (cons 'found-ok false)
    (cons 'cme-error false)

    (cons 'shstate 0)
    (cons 'shreq-code nil)
    (cons 'shreq-bytes nil)

    (cons 'shread-bytes nil)
    (cons 'shread-data nil)

    (cons 'pdp0-active false)
    (cons 'pdp1-active false)
    (cons 'pdp2-active false)
    (cons 'pdp3-active false)

    (cons 'signal-rssi 0)
    (cons 'signal-ber 0)

    ; 'closed
    ; 'connected
    ; 'listening (server mode)
    ; 'has-data
    ; 'error
    ; 'unknown
    (cons 'tcp-cid-states (list
        'closed
        'closed
        'closed
        'closed
        'closed
        'closed
        'closed
        'closed
        'closed
        'closed
        'closed
        'closed
        'closed
    ))
    (cons 'data-input-rdy false) ; Modem has provided '>' prompt for data input
    (cons 'carecv-busy false)
    (cons 'carecv-data false)

    (cons 'fs-file-size nil)
    ; 'idle
    ; 'downloading
    (cons 'fs-dl-status 'idle)
    (cons 'fs-dl-size nil)
    (cons 'fs-dl-rx-bytes nil)
    (cons 'fs-dl-code nil)
    (cons 'fs-rfile-data nil)

    (cons 'time-synced nil) ; Set to true with NTP or GNSS time update

    (cons 'mqtt-state 'unknown)
    (cons 'mqtt-callback nil)
))

(def modem-mutex (mutex-create))

(def modem-apn "iot.1nce.net")

@const-start

(def cmd-tout 0.25)

(defun modem-off () {
    (if (is-modem-on) (toggle-modem-pwr))
})

(defun modem-on () {
    (if (not (is-modem-on)) (toggle-modem-pwr))
})

(defun modem-clear-buffers () {
    (setassoc modem-state 'carecv-data nil)
    (setassoc modem-state 'fs-rfile-data nil)
    (setassoc modem-state 'shread-data nil)
})

(defun print-modem-state () (progn
    (puts "** Modem State **")
    (puts (str-merge "Time Synchronized: " (to-str (assoc modem-state 'time-synced))))
    (puts (str-merge "PDP 0 Active: " (to-str (assoc modem-state 'pdp0-active))))
    (puts (str-merge "PDP 1 Active: " (to-str (assoc modem-state 'pdp1-active))))
    (puts (str-merge "PDP 2 Active: " (to-str (assoc modem-state 'pdp2-active))))
    (puts (str-merge "PDP 3 Active: " (to-str (assoc modem-state 'pdp3-active))))
    (puts "")
    (puts (str-merge "Found OK: " (to-str (assoc modem-state 'found-ok))))
    (puts (str-merge "CME Error: " (to-str (assoc modem-state 'cme-error))))
    (puts (str-merge "RSSI: " (to-str (assoc modem-state 'signal-rssi))))
    (puts (str-merge "BER: " (to-str (assoc modem-state 'signal-ber))))
    (puts "")
    (puts (str-merge "SH State: " (to-str (assoc modem-state 'shstate))))
    (puts (str-merge "SH Req Code: " (to-str (assoc modem-state 'shreq-code))))
    (puts (str-merge "SH Req Bytes: " (to-str (assoc modem-state 'shreq-bytes))))
    (puts (str-merge "SH Read Bytes: " (to-str (assoc modem-state 'shread-bytes))))
    (puts "")
    (puts (str-merge "Data Input Rdy: " (to-str (assoc modem-state 'data-input-rdy))))
    (puts (str-merge "TCP Recv Busy: " (to-str (assoc modem-state 'carecv-busy))))
    ; TODO: MQTT state

    (print-tcp-states)
))

(defun print-tcp-states () (progn
    (looprange i 0 13 (progn
        (puts (str-merge "TCP Socket " (str-from-n i) ": " (to-str (ix (assoc modem-state 'tcp-cid-states) i))))
    ))
))

(defun print-file-contents (file-name) {
    (var size (modem-file-size file-name))
    (if size {
        (var pos 0)
        (loopwhile (< pos size) {
            (modem-read-file file-name pos)
            (puts (assoc modem-state 'fs-rfile-data))
            (setq pos (+ pos modem-rx-len))
        })
    })
})

; Send a command to the modem and wait for an "OK" response
; <tout> optional timeout, defaults to cmd-tout
(defun modem-cmd (cmd) (progn
    (var tout (if (ix (rest-args) 0) (ix (rest-args) 0) cmd-tout))
    (prepare-state)
    (if dev-debug-modem (puts (str-merge "Sending: " cmd)))
    (uart-write (str-merge cmd "\r\n"))
    (modem-wait-ok tout)
))

; PDN Auto-activation
(defun auto-activation () {
    (setq modem-auto-reactivate nil)
    (modem-cmd "AT+CPIN?")
    (modem-cmd (str-merge "AT+CGDCONT=1,\"IP\",\"" modem-apn "\""))
    (modem-cmd "AT+CSQ")
    (modem-cmd "AT+CGATT=1" 75)
    (modem-cmd "AT+CGATT?") ; TODO: Check result
    (modem-cmd "AT+COPS?")
    (modem-cmd "AT+CGNAPN")
    (modem-cmd (str-merge "AT+CNCFG=0,1,\"" modem-apn "\""))
    ; Deactivate PDP Context
    (modem-cmd "AT+CNACT=0,0")
    (modem-cmd "AT+CNACT=1,0")
    ; Activate PDP Context
    (modem-cmd "AT+CNACT=0,1")
    (modem-cmd "AT+CNACT=1,1")
    (modem-cmd "AT+CNACT?")
    (modem-cmd "AT+CGACT=0,1")
    (modem-cmd "AT+CGACT=1,1")
    (setq modem-auto-reactivate t)
})

; Ensure SH is disconnected before making a new SHCONN
(defun prepare-shconn () {
    (modem-cmd "AT+SHSTATE?")
    (if (is-sh-connected) {
        (puts "Disconnecting zombie SHCONN")
        (modem-cmd "AT+SHDISC")
    })
})

; Less than ideal configuration used to establish SSL connections
(defun prepare-https () {
    ; TODO: Check/Set Clock or ignore RTC time
        ; (modem-gnss-update)
        ; (modem-ntp-sync)
        (modem-cmd "AT+CSSLCFG=\"ignorertctime\",1,1")
    (modem-cmd "AT+CSSLCFG=\"sslversion\",1,3")
    ;AT+CSSLCFG="sni",1,"domain.com" ; Server Name Indication - declare which domain cert were looking for
    (modem-cmd "AT+CSSLCFG=\"sni\",1,0")
    (modem-cmd "AT+SHSSL=1,\"\"")
})

(defun modem-exit-data-mode () {
    ; Exit transparent transport mode
    (sleep 1.5)
    (var exit-cmd (buf-resize "+++" -1))
    (uart-write exit-cmd)
    (sleep 1.5)
})

(defun print-rssi (rssi) {
    (cond
        ((< rssi -110) (puts (str-from-n rssi "Poor modem signal strength %d dBm")))
        ((< rssi -84) (puts (str-from-n rssi "Fair modem signal strength %d dBm")))
        ((< rssi -70) (puts (str-from-n rssi "Good modem signal strength %d dBm")))
        ((< rssi -51) (puts (str-from-n rssi "Excellent modem signal strength %d dBm")))
        (t (puts "Error getting signal strength"))
    )
})

(defun modem-rssi () {
    (modem-cmd "AT+CSQ")
    (var rssi (assoc modem-state 'signal-rssi))
    (cond
        ((= rssi 0) -115)
        ((= rssi 1) -111)
        ((and (>= rssi 2) (<= rssi 30))
            ; Linear map -110 dBm to -54 dBm for RSSI 2 to 30
            (+ -110 (/ (* (- rssi 2)  (- 110 54)) (- 30 2))))
        ((= rssi 31) -52)
        (t 0)
    )
})

(defun modem-ntp-sync () {
    (var server-url (if (rest-args 0) (rest-args 0) "0.pool.ntp.org"))

    ; "+CNTP: 1,\"2025/07/03,17:10:18\"\r\n"
    (modem-cmd (str-merge "AT+CNTP=\"" server-url "\""))
    (modem-cmd "AT+CNTP")
    (modem-wait 'time-synced 10)
})

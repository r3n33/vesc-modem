(def thd-id nil) ; To track the current rx thread context id
(def run-rx-thd t) ; To keep rx thread running
(def modem-rx-active t) ; To disable uart-read in rx thread

(def modem-auto-reactivate nil) ; To signal if connection should reactivate

@const-start

(defun str-to-i28 (str) (to-i (str-to-i str)))

; TODO: Replace all (to-i (str-to-i <value>)) with (str-to-i28 <value>)

(defun prepare-state () (progn
    ; TODO: There may be more state variables that would benefit from a reset here
    (setassoc modem-state 'found-ok false)
    (setassoc modem-state 'cme-error false)
    (setassoc modem-state 'data-input-rdy false)
    (setassoc modem-state 'shreq-bytes nil)

    (if (and modem-auto-reactivate
            (or (not (assoc modem-state 'pdp0-active))
                (not (assoc modem-state 'pdp1-active))))
        (progn
            (puts "Modem reactivating PDP contexts")
            (auto-activation)))
))

(defun modem-wait-ok (tout) {
    (var start (systime))
    (loopwhile (< (secs-since start) tout) {
        (if (assoc modem-state 'found-ok) (break))
        (if (assoc modem-state 'cme-error) (break))
        (sleep 0.05)
    })
    (assoc modem-state 'found-ok)
})

; Wait for any nil <sym> to update
(defun modem-wait (sym tout) {
    (var start (systime))
    (loopwhile (< (secs-since start) tout) {
        (if (assoc modem-state sym) (break))
        (if (assoc modem-state 'cme-error) (break))
        (sleep 0.05)
    })
    (assoc modem-state sym)
})

(defun modem-wait-tcp-data (cid tout) {
    (var res nil)
    (var start (systime))
    (loopwhile (< (secs-since start) tout) {
        (if (eq (ix (assoc modem-state 'tcp-cid-states) cid) 'has-data)
            (progn
                (setq res true)
                (break)
            ))
        (sleep 0.05)
    })
    res
})

(defun is-sh-connected () {
    (= (assoc modem-state 'shstate) 1)
})

; TODO: Cleanup use of def and evalulate sanity of logic...
(defunret parse-key-vals (str) {
    (if (not-eq (type-of str) 'type-array) {
        (return 'invalid-parse-value)
    })

    (var res (if (rest-args 0) (ix (rest-args) 0) nil))

    (var pos (str-find str "+"))
    (if (= pos -1) {
    ;(puts "No + remaining")
        (return res)
    })

    (var pos2 (str-find str ": "))
    (var key (str-part str (+ pos 1) (- pos2 pos 1)))
    ;(print (list "a key" key pos pos2))

    (var pos3 (str-find str "\r\n" pos2))
    (if (= pos3 -1) {
    ;(print "missing \r\n")
        (setq pos3 (buflen str))
    })
    (setq pos2 (+ pos2 2)) ; advance position by ": "
    (var val (str-part str pos2 (- pos3 pos2)))
    ;(print (list "value" val pos2 pos3))

    (var vals (if (> (str-find val ",") -1) (str-split val ",") val))

    (if (eq res nil) {
        (setq res (cons key val))
    } {
        (setq res (cons res (list (cons key val))))
    })

    (if (> (str-len str) (+ pos3 3)) {
    ;(print (list "  **keep parsing" (str-len str) pos3))
        (parse-key-vals (str-part str (+ pos3 2)) res)
    })
    res
})

(defun shstate-helper (resp) {
    ; +SHSTATE: <status>
    (var vars (str-split resp ","))
    (var status (str-to-i28 (ix vars 0)))
    (if dev-debug-modem (print (list "Found shstate" status)))
    (setassoc modem-state 'shstate status)
})

(defun shreq-helper (resp) {
    ; +SHREQ: <url>,<type>
    (var vars (str-split resp ","))
    (var req-type (ix vars 0))
    (var http-code (str-to-i28 (ix vars 1)))
    (var req-bytes (str-to-i28 (ix vars 2)))
    (var ret (list req-type http-code req-bytes))
    (if dev-debug-modem (print (list "Found shreq" ret)))
    (setassoc modem-state 'shreq-code http-code)
    (setassoc modem-state 'shreq-bytes req-bytes)
})

(defun app-pdp-helper (resp) {
    ; +APP PDP: <pdpidx>,ACTIVE
    (var vars (str-split resp ","))
    (var pdp-idx (ix vars 0))
    (var pdp-state (ix vars 1))

    (var ret (list pdp-idx pdp-state))
    (var sym (str2sym (str-merge "pdp" pdp-idx "-active")))
    (if dev-debug-modem (print (list "Found app pdp" ret pdp-idx pdp-state sym)))

    (setassoc modem-state sym (if (eq pdp-state "ACTIVE") true false))
})

(defun cme-error-helper (resp) {
    ; +CME ERROR: <err>
    (var vars (str-split resp ","))
    (var err-str (ix vars 0))
    (setassoc modem-state 'cme-error true)
    (if dev-debug-modem (print (list "Found cme error" err-str)))
})

(defun shread-helper (resp) {
    ; +SHREAD: <data_len>\r\n<data>
    (var vars (str-split resp ","))
    (var data-len (ix vars 0))
    (setassoc modem-state 'shread-bytes data-len)
    (if dev-debug-modem (print (list "Found shread" data-len)))

    (var buf (bufcreate (str-to-i data-len)))
    (var len (uart-read buf (buflen buf) nil nil 1.0))
    (if (> len 0) (setassoc modem-state 'shread-data buf))
    (if dev-debug-modem (trap (puts (assoc modem-state 'shread-data))))
    (if dev-debug-modem (print (list "Read" (to-i len) "bytes from shread")))
})

(defun csq-helper (resp) {
    ; +CSQ: <rssi>,<ber>
    (var vars (str-split resp ","))
    (var csq-rssi (ix vars 0))
    (var csq-ber (ix vars 1))

    (setassoc modem-state 'signal-rssi (to-i (str-to-i csq-rssi)))
    (setassoc modem-state 'signal-ber (to-i (str-to-i csq-ber)))
})

(defun castate-helper (resp) {
    ; <state> 0 = Closed by remote server or internal error
    ;         1 = Connected to remote server
    ;         2 = Listening (server mode)
    ; +CASTATE: <cid>,<state>
    (var vars (str-split resp ","))
    (if (ix vars 1) (progn
        (var cid (to-i (str-to-i (ix vars 0))))
        (var state (to-i (str-to-i (ix vars 1))))

        (if dev-debug-modem (print (list "found castate" cid state)))

        (setassoc modem-state 'tcp-cid-states
            (setix (assoc modem-state 'tcp-cid-states) cid (match state
                (0 'closed)
                (1 'connected)
                (2 'listening)
                (_ (progn
                    (print (list "unexpected castate" state))
                    'unknown
                ))
            )))
    ))
})

(defun caopen-helper (resp) {
    ; +CAOPEN: <cid>,<result>
    ; <result> 0 = Success
    ;          1 = Socket error
    ;          2 = No memory
    ;          3 = Connection limit
    ;          4 = Parameter invalid
    ;          6 = Invalid IP address
    ;          7 = Not support the function
    ;          12 = Can’t bind the port
    ;          13 = Can’t listen the port
    ;          20 = Can’t resolv the host
    ;          21 = Network not active
    ;          23 = Remote refuse
    ;          24 = Certificate’s time expired
    ;          25 = Certificate’s common name does not match
    ;          26 = Certificate’s common name does not match and time expired
    ;          27 = Connect failed
    (var vars (str-split resp ","))
    (var cid (to-i (str-to-i (ix vars 0))))
    (var result (to-i (str-to-i (ix vars 0))))
    (setassoc modem-state 'tcp-cid-states
        (setix (assoc modem-state 'tcp-cid-states) cid (match result
            (0 'connected)
            (_ (progn
                (print (list "caopen error: cid / result" cid result))
                'error
            ))
        )))
    (if dev-debug-modem (print (list "found caopen" cid result (ix (assoc modem-state 'tcp-cid-states) cid))))
})

(defun cadataind-helper (resp) {
    ; If <recv_mode>=0, After open a connection successfully, if module receives data,
    ; it will report "+CADATAIND: <cid>" to remind user to read data.
    (var vars (str-split resp ","))
    (var cid (to-i (str-to-i (ix vars 0))))
    (setassoc modem-state 'tcp-cid-states
        (setix (assoc modem-state 'tcp-cid-states) cid 'has-data))
    (if dev-debug-modem (print (list "found cadataind for cid" cid)))
})

(defun append-buf (buf new-buf len) {
    (var bufpos (buflen buf))
    (buf-resize buf len)
    (bufcpy buf bufpos new-buf 0 len)
})

; TODO: Revisit shread-helper and consider combining functionality
(defun carecv-helper (carecv-pos buf buf-len) {
    ; +CARECV: [<remote IP>,<remote port>,]<recvlen>,<data>
    ; <remote IP> and <remote port> will show if AT+CASRIP=1
    (var pos-comma (str-find buf "," carecv-pos))
    (var recv-len (str-to-i (str-part buf (+ carecv-pos 9) (- pos-comma carecv-pos))))

    (if dev-debug-modem (puts (str-merge (str-from-n recv-len "Found +CARECV: %d; within ") (str-from-n buf-len "%d bytes"))))

    (if dev-debug-modem (print (list "carecv" recv-len (str-part buf 0 buf-len))))

    (var bytes-read 0)

    (if (= recv-len 0) {
        ; We have no data to read
        ; TODO: we have no more data...
    } {
        ; Length of data already received after "," of CARECV response
        (var data-start (+ pos-comma 1))
        (var data-len (- buf-len pos-comma 1))

        ; Limit data length to provided recv-len
        (if (> data-len recv-len) {
            (setq data-len recv-len)
        })

        ; Get data that may already be received
        (var data-bytes (bufcreate data-len))
        (bufcpy data-bytes 0 buf data-start data-len)
        (if dev-debug-modem (print (list data-len "carecv <" data-bytes)))
        ; Do something with data received
        (append-buf (assoc modem-state 'carecv-data) data-bytes data-len)

        (setq bytes-read (buflen data-bytes))

        (if (> recv-len bytes-read) {
            ; Get data that has not yet been received over uart but is ready from modem
            (loopwhile (< bytes-read recv-len) {
                (var max-read modem-rx-len)

                (var to-read (- recv-len bytes-read))
                (if (> to-read max-read) (setq to-read max-read))

                (var buf (bufcreate to-read))
                (var len (uart-read buf (buflen buf) nil nil 1.0))
                (if (> len 0) (progn
                    (setq bytes-read (+ bytes-read len))
                    (append-buf (assoc modem-state 'carecv-data) buf len)

                    (if dev-debug-modem (print (list len "carecv <<" buf)))
                ) (if dev-debug-modem (print (list "carecv << len was" len "retrying"))))
            })
            (if dev-debug-modem (print (list "...carecv finished all" recv-len "bytes... moving on...")))
        })
    })

    (setassoc modem-state 'carecv-busy nil)
})

(defun httptofs-helper (resp) {
    ; Download File to AP File System
    ; +HTTPTOFS: <StatusCode>,<DataLen> ; <-- Write result of HTTPTOFS
    ; +HTTPTOFS: <status>,<url>,<file_path> ; <-- Read result of HTTPTOFS
    (var vars (str-split resp ","))
    (match (length vars)
        (2 (progn
            (var code (to-i (str-to-i (ix vars 0))))
            (var data-len (to-i (str-to-i (ix vars 0))))
            (setassoc modem-state 'fs-dl-code code)
            (setassoc modem-state 'fs-dl-size data-len)
        ))
        (3 (progn
            (var status (to-i (str-to-i (ix vars 0))))
            (setassoc modem-state 'fs-dl-status status)
        ))
        (_ (print (list "unexpected httptofs response length" (length vars))))
    )
})

(defun httptofsrl-helper (resp) {
    ; State of Download File to AP File System
    ; +HTTPTOFSRL: <status>,<curlen>,<totallen>
    ; <status> 0 = Idle
    ;          1 = Downloading
    (var vars (str-split resp ","))
    (var status (ix vars 0))
    (var cur-len (to-i (str-to-i (ix vars 1))))
    (var total-len (to-i (str-to-i (ix vars 2))))
    (setassoc modem-state 'fs-dl-status (match status
        ("0" 'idle)
        ("1" 'downloading)
        (_ (print (list "unknown httptofsrl status" status)))
    ))
    (setassoc modem-state 'fs-dl-rx-bytes cur-len)
    (setassoc modem-state 'fs-dl-size total-len)
})

(defun cfsgfis-helper (resp) {
    ; Get File Size
    ; +CFSGFIS: <bytes>
    (var vars (str-split resp ","))
    (var bytes (to-i (str-to-i (ix vars 0))))
    (setassoc modem-state 'fs-file-size bytes)
})

(defun cfsrfile-helper (resp) {
    ; Read file from flash
    ; +CFSRFILE: <bytes-read>\r\n
    (var vars (str-split resp ","))
    (var bytes (str-to-i28 (ix vars 0)))

    (if dev-debug-modem (puts (str-from-n bytes "CFSRFILE has %d bytes")))

    (setassoc modem-state 'fs-rfile-data (bufcreate 0))
    (var pos 0)
    (loopwhile (< pos bytes) {
        (var to-read (- bytes pos))
        (if (> to-read modem-rx-len) (setq to-read modem-rx-len))

        (var buf (bufcreate to-read))
        (var len (uart-read buf (buflen buf) nil nil 1.0))
        (if (> len 0) (progn
            (append-buf (assoc modem-state 'fs-rfile-data) buf len)
            (setq pos (+ pos len))

            (if dev-debug-modem (print (list len "cfsrfile <<" buf)))
        ) (if dev-debug-modem (print (list "cfsrfile << len was" len "retrying"))))
    })
})

(defun cntp-helper (resp) {
    ; Sync time to NTP server
    ; +CNTP: <result>,<date-time-string>
    (var vars (str-split resp ","))
    (var result (str-to-i28 (ix vars 0)))
    (var date (ix vars 1))
    (var time (ix vars 2))

    (if dev-debug-modem (print (list "cntp-helper" result date time)))

    (if (= result 1)
        (progn
            (setassoc modem-state 'time-synced t)
            (puts "NTP Time Sync Successful"))
        (puts (str-merge "NTP Time Sync Error: " (ix vars 0)))
    )
})

(defun cgnsinf-helper (resp) {
    ; +CGNSINF:
    ; 0 <GNSS run status>
    ; 1 <Fix status>
    ; 2 <UTC date & Time>
    ; 3 <Latitude>
    ; 4 <Longitude>
    ; 5 <MSL Altitude>
    ; 6 <Speed Over Ground>
    ; 7 <Course Over Ground>
    ; 8 <Fix Mode>
    ; 9 <Reserved1>
    ; 10 <HDOP>
    ; 11 <PDOP>
    ; 12 <VDOP>
    ; 13 <Reserved2>
    ; 14 <GNSS Satellites in View>
    ; 15 <Reserved3>
    ; 16 <HPA>
    ; 17 <VPA>
    (var vars (str-split resp ","))

    (if dev-debug-modem (print (list (length vars) "cgnsinf" vars)))

    ; Parse data if GNSS is enabled
    (if (eq (ix vars 0) "1") {
        (var fix-status (ix vars 1))
        ; ...
        (var latitude (ix vars 3))
        (var longitude (ix vars 4))
        (var altitude (ix vars 5))
        (var speed (ix vars 6))
        (var heading (ix vars 7))
        (var fix-mode (ix vars 8))
        ; ...
        (var hdop (ix vars 10))
        ; ...
        (var sats (ix vars 14))

        (if (not-eq fix-status [0]) (setassoc modem-gnss-state 'has-fix (eq fix-status "1")))
        (if (not-eq fix-mode [0]) (setassoc modem-gnss-state 'fix-mode (str-to-i28 fix-mode)))
        (if (not-eq latitude [0]) (setassoc modem-gnss-state 'latitude (str-to-f latitude)))
        (if (not-eq longitude [0]) (setassoc modem-gnss-state 'longitude (str-to-f longitude)))
        (if (not-eq altitude [0]) (setassoc modem-gnss-state 'altitude (str-to-f altitude)))
        (if (not-eq speed [0]) (setassoc modem-gnss-state 'speed (str-to-f speed)))
        (if (not-eq heading [0]) (setassoc modem-gnss-state 'heading (str-to-f heading)))
        (if (not-eq hdop [0]) (setassoc modem-gnss-state 'hdop (str-to-f hdop)))
        (if (not-eq sats [0]) (setassoc modem-gnss-state 'sats-in-view (str-to-i28 sats)))
    })
})

(defun smstate-helper (resp) {
    ; +SMSTATE: <state>
    (var vars (str-split resp ","))
    (var state (str-to-i28 (ix vars 0)))
    (match state
        (0 (setassoc modem-state 'mqtt-state 'disconnected))
        (1 (setassoc modem-state 'mqtt-state 'connected))
        (2 (setassoc modem-state 'mqtt-state 'connected-sp))
        (_ (puts "Error parsing SMSTATE"))
    )
})

(defun smsub-helper (resp) {
    ; +SMSUB: <topic>,<data>
    (var vars (str-split resp ","))
    (var topic (str-replace (ix vars 0) "\"" ""))
    (var data (ix vars 1))
    (if (assoc modem-state 'mqtt-callback)
        ((assoc modem-state 'mqtt-callback) topic data))
})

(defun kv-helper (kv) {
    ;(print (list "kv-helper" kv))
    (var key (car kv))
    (var value (cdr kv))
    (match key
        ; TODO: +CNACT: 0,1,"10.185.135.247" ; +CNACT: 2,0,"0.0.0.0"
        ; TODO: +CAURC: \"incoming full\"
        ; TODO: +CAURC: buffer full,0

        ("SHREQ" (shreq-helper value))
        ("SHSTATE" (shstate-helper value))
        ("APP PDP" (app-pdp-helper value))
        ; " <- for TODO
        ; TODO: LispBM syntax parser for VSCode does not like the space in APP PDP nor CME ERROR
        ("CME ERROR" (cme-error-helper value))
        ("SHREAD" (shread-helper value))

        ("CSQ" (csq-helper value))

        ("CASTATE" (castate-helper value))
        ("CAOPEN" (caopen-helper value))
        ("CADATAIND" (cadataind-helper value))

        ("HTTPTOFS" (httptofs-helper value))
        ("HTTPTOFSRL" (httptofsrl-helper value))
        ("CFSGFIS" (cfsgfis-helper value))

        ("CFSRFILE" (cfsrfile-helper value))

        ("CNTP" (cntp-helper value))

        ("CGNSINF" (cgnsinf-helper value))

        ("SMSTATE" (smstate-helper value))
        ("SMSUB" (smsub-helper value))

        (_ nil)
    )
})

(defun modem-data-helper (data len) {
    (if (> (str-find data "OK") -1) (progn
        (if dev-debug-modem (puts "Found OK"))
        (setassoc modem-state 'found-ok true)))

    ; AT+CASEND= will result in > when ready to write data
    (if (> (str-find data ">") -1) (progn
        (if dev-debug-modem (puts "Found >"))
        (setassoc modem-state 'data-input-rdy true)
    ))

    ; AT+CARECV= may return binary data therefore a special handler is needed
    (var found-carecv (str-find data "+CARECV: "))
    (if (> found-carecv -1) (carecv-helper found-carecv data len))

    (if (= found-carecv -1) {
        ; All ASCII responses that begin with + and contain : <value(,s)>
        (var kv (parse-key-vals data))
        (if kv (kv-helper kv))
    })
})

(defun modem-rx-thd () {
    (var buf-len modem-rx-len)
    (var buf (bufcreate buf-len))
    (var rx-ts (systime))
    (var new-line 10)
    (loopwhile run-rx-thd {
        (if modem-rx-active {
            ; (uart-read array num optOffset optStopAt optTimeout)
            (var len (uart-read buf (buflen buf) nil new-line 0.05))
            (if (> len 0) (progn
                (if dev-debug-modem {
                    (var buf-trunc (str-part buf 0 len))
                    (print (list (str-from-n (secs-since rx-ts) "< %.2fs <") buf-trunc))
                })
                (setq rx-ts (systime))

                (trap (modem-data-helper buf len))

                (bufclear buf)
            ))
        } (sleep 0.1))
    })
    (puts "modem-rx-thd terminated")
    (setq thd-id nil)
})

(defun stop-rx-thd () (setq run-rx-thd nil))

(defun start-rx-thd () {
    (if (not thd-id)
        (progn
            (setq run-rx-thd t)
            (setq modem-rx-active t)
            (puts "Starting modem rx thread")
            (setq thd-id (spawn modem-rx-thd)))
        (progn
            (puts "Killing previous rx thread")
            (setq run-rx-thd false)
            (loopwhile thd-id (sleep 0.1))
            (start-rx-thd)))
})

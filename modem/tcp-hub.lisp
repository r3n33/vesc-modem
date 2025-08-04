(def tcp-hub-tx-cnt 0)
(def tcp-hub-rx-cnt 0)
(def tcp-hub-active nil)

@const-start

(defun hex-str (bytes)
    (apply str-merge (map (fn (x) (str-from-n x "%02X")) bytes)))

(defun tcp-hub-event-handler () {
    (set-mailbox-size 69)
    (loopwhile t
        (recv
            ((event-cmds-data-tx (? data)) {
                (if dev-debug-modem (print (list (buflen data) "cmds-proc >" data)))
                (modem-tcp-send data (buflen data))
                (setq tcp-hub-tx-cnt (+ tcp-hub-tx-cnt 1))
            })
            (_ nil)))
})

(defun exit-tcp-hub () {
    ; Disable the commands processor
    (cmds-start-stop false)
    (event-enable 'event-cmds-data-tx 0)

    ; Stop tcp hub
    (setq tcp-hub-active nil)

    ; Close the connection
    (modem-tcp-close)

    (puts "TCP Hub connection closed")
})

(defun activate-tcp-hub () (progn
    (if tcp-hub-active {
        (puts "TCP Hub is already active, restarting connection")
        (exit-tcp-hub)
    })
    (setq dev-debug-modem nil) ; Disabling debug output or else we may stream REPL to cellular client
    (setq tcp-hub-active t)
    (trap (modem-tcp-close)) ; Attempting to close connection in case new firmware was flashed with an open socket
    (modem-tcp-open "veschub.vedder.se" 65101)
    (modem-tcp-status)
    (sleep 1.0)
    (var tcp-username (hex-str (get-mac-addr)))
    (var tcp-password (hex-str (get-mac-addr)))
    (if (modem-tcp-send (str-merge "VESC:" tcp-username ":" tcp-password "\n"))
        (progn
            (puts "Connected to TCP Hub")
            (puts (str-merge "Login credentials: " tcp-username ", " tcp-password))
            (event-register-handler (spawn tcp-hub-event-handler))
            (event-enable 'event-cmds-data-tx 1)
            (cmds-start-stop true)

            (loopwhile tcp-hub-active (progn
                (if (modem-wait-tcp-data 0 1.0)
                    (progn
                        (var bytes (modem-tcp-recv modem-rx-len 0.05 false))
                        (cmds-proc bytes) ; Send data to command processor
                        (if (> (buflen bytes) 0)
                            (setq tcp-hub-rx-cnt (+ tcp-hub-rx-cnt 1)))))))
        )
        (progn
            (puts "Error connecting to TCP Hub")
            (setq tcp-hub-active nil)
        )
    )
))

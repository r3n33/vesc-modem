@const-start

;(defun modem-fs-size () {
;    (modem-cmd "AT+CFSGFRS?") ; Get the Size of File System
;})

(defun modem-file-size (file-name) {
    ; AT+CFSGFIS ; Get File Size
    (setassoc modem-state 'fs-file-size nil)
    (modem-cmd "AT+CFSINIT")
    (if (modem-cmd (str-merge "AT+CFSGFIS=0," file-name) 5) {
        (if dev-debug-modem (print (list "saved file size" (assoc modem-state 'fs-file-size))))
    })
    (modem-cmd "AT+CFSTERM")
    (assoc modem-state 'fs-file-size)
})

; Wait for file download to complete
(defun modem-wait-file () {
    (var start (systime))
    (modem-cmd "AT+HTTPTOFSRL?") ; Check if we downloading before waiting
    (loopwhile (eq (assoc modem-state 'fs-dl-status) 'downloading) {
        (modem-cmd "AT+HTTPTOFSRL?")
        (if dev-debug-modem (print (list "dl"
            (assoc modem-state 'fs-dl-code)
            (assoc modem-state 'fs-dl-rx-bytes)
            "/"
            (assoc modem-state 'fs-dl-size)
        )))
        (sleep 1.0)
    })
    (var dl-secs (secs-since start))
    (puts (str-from-n dl-secs "Download complete in %.2f seconds"))
    (puts (str-from-n (/ (assoc modem-state 'fs-dl-size) dl-secs) "Downloaded %.0f bytes/second"))
})

(defunret modem-save-file (url file-name) {
    (var blocking (ix (rest-args) 0))
    (prepare-https)

    (setassoc modem-state 'fs-dl-status 'idle)
    (setassoc modem-state 'fs-dl-size nil)
    (setassoc modem-state 'fs-dl-size nil)
    (setassoc modem-state 'fs-dl-code nil)

    (if (not (modem-cmd (str-merge "AT+HTTPTOFS=\"" url "\",\"/custapp/" file-name "\"") 1.0))
        (return 'cmd-failed))

    (modem-cmd "AT+HTTPTOFSRL?")

    (if blocking
        (modem-wait-file)
        true
    )
})

(defun modem-del-file (file-name) {
    ; TODO: Make it so
    ; AT+CFSDFILE ; Delete the File from the Flash
})

(defun modem-mv-file (file-name-old file-name-new) {
    ; TODO: Make it so
    ; AT+CFSREN ; Rename a File
})

(defun modem-read-file (file-name) {
    (var start-pos (if (rest-args 0) (rest-args 0) 0))
    (var read-len (if (rest-args 1) (rest-args 1) modem-rx-len))
    (var tout 5)

    ; AT+CFSRFILE=<index>,<filename>,<mode>,<readbytes>,<position>
    ; <index> 0 = "/custapp/"
    ;         1 = "/fota/"
    ;         2 = "/datatx/"
    ;         3 = "/customer/"
    ; <filename> limit 230 bytes
    ; <mode> 0 = Read data at the beginning of the file
    ;        1 = Read data at the <position> of the file
    ; <readbytes> Limit for read command is 10240 bytes at a time
    ; <position> Starting position for read

    (modem-cmd "AT+CFSINIT")
    (modem-cmd (str-merge "AT+CFSRFILE=0," file-name ",1," (str-from-n read-len) "," (str-from-n start-pos)) tout)
    (modem-cmd "AT+CFSTERM")
})

;(modem-save-file "https://raw.githubusercontent.com/vedderb/vesc_express/refs/heads/main/README.md" "vesc.md" 'blocking)

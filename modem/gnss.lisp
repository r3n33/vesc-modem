(def modem-gnss-state (list
    (cons 'has-fix false)
    (cons 'latitude nil)
    (cons 'longitude nil)
    (cons 'altitude nil) ; Meters
    (cons 'speed nil) ; Km/hour
    (cons 'heading nil)
    (cons 'fix-mode 0)
    (cons 'sats-in-view nil)
    (cons 'hdop nil)
))

@const-start

(defun modem-gnss-update () {
    (if (modem-cmd "AT+CGNSINF") {
        ; (set-pos-time lat lon height speed hdop msToday year month day)
        (set-pos-time
            (assoc modem-gnss-state 'latitude)
            (assoc modem-gnss-state 'longitude)
            (assoc modem-gnss-state 'altitude)
            (assoc modem-gnss-state 'speed)
            (assoc modem-gnss-state 'heading)
            nil ; TODO: Parse and fill in date-time
            nil
            nil
            nil
        )
    })
})

; TODO: Testing XTRA commands
; TODO: New output on power on
; ("< 0.21s <" "\r\n")
; ("< 0.00s <" "DST: 1\r\n")
; ("< 0.00s <" "\r\n")
; ("< 0.00s <" "*PSUTTZ: 25/07/21,15:13:23\",\"-20\",1\r\n")
(defun modem-gnss-xtra () {
    (modem-cmd "AT+CLTS=1") ; Time Sync
    (modem-cmd "AT+HTTPTOFS=\"http://iot2.xtracloud.net/xtra3gr_72h.bin\",\"/customer/Xtra3.bin\"") ; Download XTRA
    ; TODO: Wait for +HTTPTOFS: 200,...
    (modem-cmd "AT+CGNSCPY") ; Copy XTRA file
    (modem-cmd "AT+CGNSXTRA") ; Validate XTRA data
    (modem-cmd "AT+CGNSXTRA=1") ; Enable XTRA
    (modem-cmd "AT+CGNSCOLD")
})

;(modem-cmd "AT+CGNSPWR?")
;(modem-cmd "AT+SGNSCFG?")
;(modem-cmd "AT+CGNSINF")
;(modem-cmd "AT+CGNSXTRA?")
;(modem-cmd "AT+SGNSCMD=1,0") ; Turn on GNSS and get location information once ; +SGNSCMD:
;(modem-cmd "AT+SGNSCMD=1,1")
;(modem-cmd "AT+SGNSCMD=2,1000,0,1")
;(modem-cmd "AT+SGNSCMD=0") ; Turn off GNSS

;(modem-cmd "AT+CGNSMOD?")
;(modem-cmd "AT+CGNSMOD=1,1,0,0,0") ; Set GNSS Work Mode

(defun modem-gnss-enable () {
    ; Turn on GNSS power supply
    (if (modem-cmd "AT+CGNSPWR=1")
        (puts "GNSS Enabled")
        (puts "Error enabling GNSS"))
})

(defun modem-gnss-disable () {
    ; Turn off GNSS power supply
    (if (modem-cmd "AT+CGNSPWR=0")
        (puts "GNSS Disabled")
        (puts "Error disabling GNSS"))
})

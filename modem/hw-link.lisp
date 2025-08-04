@const-start

(def modem-rx-len 512)

; Helper function to make a list populated with val
(defun mklist (len val) (map (fn (x) val) (range len)))

; RGB LED
(def led-count 2)
@const-end
(def led-strip (rgbled-buffer led-count 0 1))
(def led-colors (mklist led-count 0))
(def led-brightness 1.0)
@const-start

(defun is-modem-on () {
    (var modem-status (= (gpio-read 3) 1))
    (puts (if modem-status "Modem is ON" "Modem is OFF"))
    modem-status
})

(defun toggle-modem-pwr () {
    (var was-on (is-modem-on))
    (puts "Pressing modem pwr key")
    (gpio-write 2 1)
    (sleep 2)
    (gpio-write 2 0)
    (puts "Releasing modem pwr key")
    (sleep 6) ; Give modem a few seconds to power on/off
    (is-modem-on)
})

(defun modem-ready () {
    ; Check for AT OK
    (var res (modem-cmd "AT"))
    res
})

(defun modem-uart-start (baud) {
    (sleep 1)
    (uart-start 0 20 21 baud)
})

(defun modem-fast-baud () {
    (modem-cmd "AT+IPR=921600")
    (sleep 0.1)
    (modem-uart-start 912600)
    (sleep 0.1)
    ; Check for AT OK
    (var res (modem-cmd "AT"))
    (puts (if res "Baudrate is now 921600" "Updating baudrate failed"))
    res
})

(defun modem-set-led (idx color) {
    (setix led-colors idx color)
    (rgbled-color led-strip 0 led-colors led-brightness)
    (rgbled-update led-strip)
})

(defun init-modem () {
    (var res t)
    (gpio-configure 2 'pin-mode-out) ; Modem Power
    (gpio-write 2 0)
    (gpio-configure 3 'pin-mode-in) ; Modem Status

    (sleep 0.5) ; TODO: Sleep a moment or else (gpio-read 3) will return true even if the modem is off

    (gpio-configure 6 'pin-mode-out) ; CAN Standby
    (gpio-write 6 0) ; Low to enable

    (gpio-configure 7 'pin-mode-out) ; LED / GNSS Enable
    (gpio-write 7 1) ; High to enable

    (rgbled-init 8) ; Init RGB LED on pin 8
    (loopwhile-thd 100 t { ; Blink LED 0
        (modem-set-led 0 (color-make 0 0 255))
        (sleep 0.25)
        (modem-set-led 0 (color-make 0 0 0))
        (sleep 0.75)
    })

    (modem-uart-start 912600) ; Try fast baud first

    (if (not (is-modem-on))
        (toggle-modem-pwr) ; Power On
        (puts "Modem already powered on"))

    ; Try to upgrade to fast baud if modem is not ready
    (if (not (modem-ready)) {
        (modem-uart-start 115200)
        (sleep 0.1)
        (if (not (modem-ready))
            (setq res nil)
            (setq res (modem-fast-baud)))
    })

    res
})

(defun modem-sleep (seconds) {
    (gpio-write 7 0) ; led gnss off
    (gpio-write 6 1) ; can off

    (if (is-modem-on)
        (toggle-modem-pwr)) ; modem off
    
    (sleep-deep seconds) ; esp sleep
})

(defun apn-load () {
    (var f (f-open "apn.cfg" "r"))
    (if f {
        (setq modem-apn (f-readline f 64))
        (f-close f)
        (send-data modem-apn)
    })
})

(defun apn-save (apn) {
    (var f (f-open "apn.cfg" "w"))
    (if f {
        (f-write f apn)
        (f-close f)
    })
})

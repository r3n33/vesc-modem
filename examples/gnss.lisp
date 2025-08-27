@const-start

(defun example-gnss-update () {
    (if dev-modem-gnss-enable {
        (modem-gnss-update)
        (puts (to-str modem-gnss-state))
    })
})

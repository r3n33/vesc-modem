@const-start

; Download file to modem's flash storage using modem's HTTP application
(defun example-save-file () {
    (if (modem-save-file "https://raw.githubusercontent.com/vedderb/vesc_express/refs/heads/main/README.md" "vesc.md" 'blocking)
        (puts (str-from-n (modem-file-size "vesc.md") "Downloaded file size: %d bytes"))
    )
})

; Read file from modem's flash storage
(defun example-read-file () {
    (var file "vesc.md")
    (var size (modem-file-size file))
    (if size {
        (var len-max 64)
        (var pos 0)
        (loopwhile (< pos size) {
            (var read-len (if (> (- size pos) len-max) len-max (- size pos)))
            (puts (modem-read-file file pos read-len))
            (setq pos (+ pos read-len))
        })
    })
})

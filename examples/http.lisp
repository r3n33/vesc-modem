@const-start

(defun example-http-get () {
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

(defun example-https-post () {
    (prepare-https)
    (var address "https://httpbin.org")
    (var endpoint "/post")
    (var json (str-merge "{ \"name\": \"Jack\", \"age\": 27 }"))
    (modem-http-post address endpoint json)
})

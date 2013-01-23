(require 'url-http)

(defvar airplay->host nil)
(defvar airplay->port 7000)

(defun airplay/net:query (args)
  (mapconcat
   (lambda (x)
     (concat (url-hexify-string (car x)) "=" (url-hexify-string (cdr x))))
   args "&"))

(defun airplay/protocol:make-path (path query)
  (format "http://%s:%s%s?%s"
          airplay->host airplay->port path (airplay/net:query query)))

(defun airplay/protocol:make-text-parameters (args)
  (concat
   (mapconcat
    (lambda (x)
      (concat (car x) ": " (cdr x))) args "\n")
   "\n"))

(defun airplay/net:retrieve (method path &optional query body callee)
  (let ((url-request-method method)
        (url-request-data body)
        (url (airplay/protocol:make-path path query))
        (cb (or callee (lambda (status)))))
    (url-retrieve url cb)))

(defun airplay/protocol:get (path &optional query callee)
  (airplay/net:retrieve "GET" path query nil callee))

(defun airplay/protocol:post (path &optional query data callee)
  (airplay/net:retrieve "POST" path query data callee))

(defun airplay/protocol:put (path &optional query data callee)
  (airplay/net:retrieve "PUT" path query data callee))

(defun airplay/api:send_image (imagefile)
  (let ((path "/photo") data buf)
    (setq buf (find-file-noselect imagefile t t))
    (setq data (with-current-buffer buf (buffer-string)))
    (kill-buffer buf)
    (airplay/protocol:put path nil data)))

(defun airplay/api:send_video (video_location)
  (airplay/protocol:post "/play" nil
                         (airplay/protocol:make-text-parameters
                          `(("Content-Location" . ,video_location)
                            ("Start-Position" . "0.0")))))

(defun airplay/api:stop ()
  (airplay/protocol:post "/stop"))

(defun airplay/api:server_info ()
  (airplay/protocol:get "/server-info" nil
                        (lambda (status)
                          (xml-parse-region (point-min) (point-max)))))

(provide 'airplay)

;; (setq airplay->host "192.168.0.3")
;; (airplay/api:server_info)
;; (airplay/api:stop)
;; (airplay/api:send_video "http://ia600409.us.archive.org/27/items/MIT18.01JF07/ocw-18.01-f07-lec01_300k.mp4")
;; (airplay/api:send_image "~/Desktop/jobs.jpg")


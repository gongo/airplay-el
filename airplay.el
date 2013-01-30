(require 'url-http)
(require 'xml)
(require 'dns)

(defvar airplay->host nil)
(defvar airplay->port 7000)

(defconst airplay->log-buffer "*airplay log*")

(defconst airplay->image-transitions
  '(
    :none        "None"
    :slide_left  "SlideLeft"
    :slide_right "SlideRight"
    :dissolve    "Dissolve"
    ))

(defun airplay/debug-log (fmt &rest args)
  (with-current-buffer (get-buffer-create airplay->log-buffer)
    (insert (apply 'format fmt args))))

(defun airplay/net:browse ()
  "Return IP Address of _airplay._tcp service type device.
If not found device, return nil."
  (with-temp-buffer
    (let ((process (make-network-process :name "mdns"
                                         :coding 'binary
                                         :buffer (current-buffer)
                                         :host "224.0.0.251"
                                         :service 5353
                                         :type 'datagram))
          (send-text (dns-write
                      `((id ,(random 65000))
                        (opcode query)
                        (queries (("_airplay._tcp.local" (type PTR)))))))
          response)
      (process-send-string process send-text)
      (when (zerop (buffer-size))
        (accept-process-output process 5)) ;; wait 5sec
      (setq response (buffer-string))
      (delete-process process)
      (if (zerop (length response)) nil
        (dns-get 'data (car (dns-get 'additionals (dns-read response))))))))

(defun airplay/protocol:make-query (args)
  (mapconcat
   (lambda (x)
     (concat (url-hexify-string (car x)) "=" (url-hexify-string (cdr x))))
   args "&"))

(defun airplay/protocol:make-path (path query)
  (unless airplay->host
    (setq airplay->host (airplay/net:browse)))
  (format "http://%s:%s%s?%s"
          airplay->host airplay->port path (airplay/protocol:make-query query)))

(defun airplay/protocol:make-text-parameters (args)
  (concat
   (mapconcat (lambda (x) (concat (car x) ": " (cdr x))) args "\n")
   "\n"))

(defun airplay/protocol:parse-text-parameters ()
  "\
Parse string in current buffer.
Returns the text/parameters list.

eg.

  (buffer-string)
  ;; => \"duration: 83.124794\\nposition: 14.467000\\n\"

  (airplay/protocol:parse-text-parameters)
  ;; => ((\"duration\" . \"0.000000\") (\"position\" . \"0.000000\"))
"
  (let ((params '()))
    (save-excursion
      (goto-char (point-min))
      (perform-replace "\\`\\(?:\\\s-\\|\n\\)+\\|\\(?:\\\s-\\|\n\\)+\\'" "" nil t nil)
      (goto-char (point-min))
      (while (re-search-forward "^\\([^:\\\s ]+\\)[\\\s ]*:[\\\s ]*\\([^\\\s ]+\\)$" nil t)
        (let ((name (match-string 1)) (body (match-string 2)))
          (setq params (append params `((,name . ,body))))
          (forward-line))))
    params))

(defun airplay/protocol:parse-xml ()
  "Parse string in current buffer.
Returns the XML list. see `xml-parse-region'"
  (xml-parse-region (point-min) (point-max)))

(defun* airplay/net:send (method path &rest settings
                                 &key
                                 (header nil)
                                 (query  nil)
                                 (body   nil))
  (let* ((query  (plist-get settings :query))
         (body   (plist-get settings :body))
         (header (plist-get settings :header))
         (url-request-method method)
         (url-request-data body)
         (url-request-extra-headers header)
         (url (airplay/protocol:make-path path query)))
    (lexical-let ((method method) (path path) (url url))
      (url-retrieve
       url
       (lambda (s)
         (url-http-parse-headers)
         (unless (eq 200 url-http-response-status)
           (airplay/debug-log
            "airplat/net:send [method: %s] [path: %s] [response:%d]\n"
            method path url-http-response-status)))))))

(defun* airplay/net:receive (method path &rest settings
                                    &key
                                    (query  nil)
                                    (response-type nil))
  (let* ((query  (plist-get settings :query))
         (type   (plist-get settings :response-type))
         (parser (cond
                  ((eq type 'xml) 'airplay/protocol:parse-xml)
                  ((eq type 'params) 'airplay/protocol:parse-text-parameters)
                  (t (lambda () (buffer-string)))))
         (url-request-method method)
         (url (airplay/protocol:make-path path query))
         response)
    (with-current-buffer (url-retrieve-synchronously url)
      (url-http-parse-headers)
      (if (eq 200 url-http-response-status)
          (save-excursion
            (delete-region (point-min) url-http-end-of-headers)
            (goto-char (point-min))
            (setq response (funcall parser)))
        (airplay/debug-log
         "airplat/net:receive [method: %s] [path: %s] [response:%d]\n"
         method path url-http-response-status))
      (kill-buffer (current-buffer)))
    response))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTTP Method                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun airplay/protocol:get (path &rest args)
  (apply 'airplay/net:receive "GET" path args))

(defun airplay/protocol:post (path &rest args)
  (apply 'airplay/net:send "POST" path args))

(defun airplay/protocol:put (path &rest args)
  (apply 'airplay/net:send "PUT" path args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User API                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun airplay:send_image (image_file &optional transition)
  (let* ((transition (or transition :none))
         (transition_val (or (plist-get airplay->image-transitions transition)
                             (plist-get airplay->image-transitions :none))))
    (airplay/protocol:put "/photo"
                          :header `(("X-Apple-Transition" . ,transition_val))
                          :body (with-temp-buffer
                                  (insert-file-contents-literally image_file)
                                  (buffer-string)))))

(defun airplay:stop ()
  (airplay/protocol:post "/stop"))

(defun airplay:server_info ()
  (airplay/protocol:get "/server-info"
                        :response-type 'xml))

(defun airplay:send_video (video_location)
  (airplay/protocol:post "/play"
                         :body (airplay/protocol:make-text-parameters
                                `(("Content-Location" . ,video_location)
                                  ("Start-Position"   . "0.0")))))

(defun airplay:set_scrub (position)
  (airplay/protocol:post "/scrub"
                         :query `(("position" . ,position))))

(defun airplay:get_scrub ()
  (airplay/protocol:get "/scrub"
                        :response-type 'params))

(defun airplay:scrub (&optional position)
  (if position (airplay:set_scrub position) (airplay:get_scrub)))

(defun airplay:playback-info ()
  (airplay/protocol:get "/playback-info"
                        :response-type 'xml))

(provide 'airplay)

;; (airplay:server_info)
;; (airplay:stop)
;; (airplay:send_video "http://ia600409.us.archive.org/27/items/MIT18.01JF07/ocw-18.01-f07-lec01_300k.mp4")
;; (airplay:send_image "~/Desktop/jobs.jpg")
;; (airplay:send_image "~/Desktop/jobs.jpg" :none)
;; (airplay:send_image "~/Desktop/jobs.jpg" :slide_left)
;; (airplay:send_image "~/Desktop/jobs.jpg" :slide_right)
;; (airplay:send_image "~/Desktop/jobs.jpg" :dissolve)
;; (airplay:scrub)
;; (airplay:scrub "3082")
;; (airplay:playback-info)


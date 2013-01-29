(require 'url-http)
(require 'xml)
(require 'dns)

(defvar airplay->host nil)
(defvar airplay->port 7000)

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

(defun airplay/net:query (args)
  (mapconcat
   (lambda (x)
     (concat (url-hexify-string (car x)) "=" (url-hexify-string (cdr x))))
   args "&"))

(defun airplay/protocol:make-path (path query)
  (unless airplay->host
    (setq airplay->host (airplay/net:browse)))
  (format "http://%s:%s%s?%s"
          airplay->host airplay->port path (airplay/net:query query)))

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

(defun* airplay/net:retrieve (method path &rest settings
                                     &key
                                     (query  nil)
                                     (body   nil)
                                     (callee nil)
                                     (response-type nil))
  (let* ((query  (plist-get settings :query))
         (body   (plist-get settings :body))
         (callee (plist-get settings :callee))
         (type   (plist-get settings :response-type))
         (parser (cond
                  ((eq type 'xml) 'airplay/protocol:parse-xml)
                  ((eq type 'params) 'airplay/protocol:parse-text-parameters)
                  (t (lambda () (buffer-string)))))
         (url-request-method method)
         (url-request-data body)
         (url (airplay/protocol:make-path path query)))
    (lexical-let ((callee (or callee (lambda (s m))))
                  (parser parser))
      (url-retrieve
       url
       (lambda (s)
         (url-http-parse-response)
         (url-http-parse-headers)
         (let ((status  url-http-response-status)
               response)
           (save-excursion
             (delete-region (point-min) url-http-end-of-headers)
             (goto-char (point-min))
             (setq response (funcall parser))
             (funcall callee status response))))))))

(defun airplay/protocol:get (path &rest args)
  (apply 'airplay/net:retrieve "GET" path args))

(defun airplay/protocol:post (path &rest args)
  (apply 'airplay/net:retrieve "POST" path args))

(defun airplay/protocol:put (path &rest args)
  (apply 'airplay/net:retrieve "PUT" path args))

(defun airplay:send_image (image_file)
  (airplay/protocol:put "/photo"
                        :body (with-temp-buffer
                                (insert-file-contents-literally image_file)
                                (buffer-string))))

(defun airplay:stop ()
  (airplay/protocol:post "/stop"))

(defun airplay:server_info ()
  (airplay/protocol:get "/server-info"
                        :callee (lambda (status response) (buffer-string))
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
                        :response-type 'params
                        :callee (lambda (status response)
                                  (message (buffer-string)))))

(defun airplay:scrub (&optional position)
  (if position (airplay:set_scrub position) (airplay:get_scrub)))

(defun airplay:playback-info ()
  (airplay/protocol:get "/playback-info"
                        :response-type 'xml
                        :callee (lambda (status response)
                                  (message (buffer-string)))))

(provide 'airplay)

;; (airplay:server_info)
;; (airplay:stop)
;; (airplay:send_video "http://ia600409.us.archive.org/27/items/MIT18.01JF07/ocw-18.01-f07-lec01_300k.mp4")
;; (airplay:send_image "~/Desktop/jobs.jpg")
;; (airplay:scrub)
;; (airplay:scrub "2999")
;; (airplay:playback-info)

;;; airplay.el --- Airplay bindings to Emacs

;; Copyright (C) 2013 by Wataru MIYAGUNI

;; Author: Wataru MIYAGUNI <gonngo@gmail.com>
;; URL: https://github.com/gongo/airplay-el
;; Version: 0.01
;; Package-Requires: ((request "20130110.2144"))

;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; A client for AirPlay Server.

;;; Usage:

;;
;; (require 'airplay)
;;
;; browsing Apple TV in LAN
;;
;;   (airplay/device:browse) ;; => ("192.168.0.10" . 7000)
;;                           ;; if cannot find , (nil . nil)
;;
;;   If want to specify, following code.
;;
;;   (setq airplay->host "192.168.0.10")
;;   (setq airplay->port 7000)
;;
;; View picture file (at local machine)
;;
;;   (airplay/image:view "~/Desktop/jobs.jpg")
;;   (airplay/image:view "~/Desktop/jobs.jpg" :slide_left)
;;   (airplay/image:view "~/Desktop/jobs.jpg" :slide_right)
;;   (airplay/image:view "~/Desktop/jobs.jpg" :dissolve)
;;
;; Play movie file (via HTTP)
;;
;;   (airplay/video:view "https://dl.dropbox.com/u/2532139/IMG_0381XXX.m4v")
;;
;; If want to stop picture or movie
;;
;;   (airplay:stop)
;;
;; Other API
;;
;;   (airplay/video:scrub)
;;     ;; => (message "38.011/90.000")
;;
;;   (airplay/video:scrub "20")
;;     ;; => seek to 20 seconds in playing video.
;;
;;   (airplay:playback-info)
;;     ;; => (message "Playing now!!") or (message "Not playing...")
;;
;; Advance.
;;
;;   (deferred:$
;;     (deferred:next
;;       (lambda ()
;;         (airplay/image:view "~/Desktop/jobs.jpg" :slide_left)))
;;     (deferred:wait 1000)
;;     (deferred:nextc it
;;       (lambda (s)
;;         (airplay/image:view "~/Desktop/jobs.jpg" :slide_right)))
;;     (deferred:wait 2000)
;;     (deferred:nextc it
;;       (lambda (s)
;;         (airplay:stop))))
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'xml)
(require 'dns)
(require 'request)
(require 'request-deferred)

(defvar airplay->host nil)
(defvar airplay->port 7000)

(defconst airplay->log-buffer "*airplay log*")

(defconst airplay/image->transitions
  '(
    :none        "None"
    :slide_left  "SlideLeft"
    :slide_right "SlideRight"
    :dissolve    "Dissolve"
    ))

(defun airplay/debug-log (fmt &rest args)
  (with-current-buffer (get-buffer-create airplay->log-buffer)
    (insert (apply 'format fmt args))))

(defun airplay/device:browse ()
  "Return IP Address and port of _airplay._tcp service type device.
If not found device, return (nil . nil)."
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
      (if (zerop (length response)) (cons nil nil)
        (let* ((dns_response (dns-get 'additionals (dns-read response)))
               (address (dns-get 'data (car dns_response)))
               (port (dns-get 'port (dns-get 'data (car (last dns_response))))))
          `(,address . ,port))))))

(defun airplay/net:request (method path &rest args)
  (let ((request-backend 'url-retrieve))
    (apply 'request-deferred (airplay/net:--make-url path) :type method args)))

(defun airplay/net:--make-query (args)
  (mapconcat
   (lambda (x)
     (concat (url-hexify-string (car x)) "=" (url-hexify-string (cdr x))))
   args "&"))

(defun airplay/net:--make-url (path)
  (unless airplay->host
    (let ((device (airplay/device:browse)))
      (setq airplay->host (car device))
      (setq airplay->port (cdr device))))
  (format "http://%s:%s/%s" airplay->host airplay->port path))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTTP Method                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun airplay/protocol:get (path &rest args)
  (apply 'airplay/net:request "GET" path args))

(defun airplay/protocol:post (path &rest args)
  (apply 'airplay/net:request "POST" path args))

(defun airplay/protocol:put (path &rest args)
  (apply 'airplay/net:request "PUT" path args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Request/Response Content Type Maker/Parser   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun airplay/protocol:parse-plist-xml ()
  "Parse string in current buffer.
Assumes \"Apple//DTD PLIST\" ( http://www.apple.com/DTDs/PropertyList-1.0.dtd ) format.
Returns the XML list."
  (let ((tree (xml-parse-region (point-min) (point-max))))
    (airplay/protocol:--parse-plist-xml
     (assoc 'plist tree))))

(defun airplay/protocol:--parse-plist-xml (top)
  (let ((kvs (xml-node-children (remove-if 'stringp (assoc 'dict top))))
        key value kset vset (dict '()))
    (while kvs
      (setq kset (pop kvs))
      (setq vset (pop kvs))

      (setq key (car (xml-node-children kset)))
      (setq value
            (if (eq (xml-node-name vset) 'array)
                (airplay/protocol:--parse-plist-xml (xml-node-children vset))
              (car (xml-node-children vset))))
      (setq dict (append dict `((,key . ,value)))))
    dict))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User API                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun airplay/image:view (image_file &optional transition)
  (let* ((transition (or transition :none))
         (transition_val (or (plist-get airplay/image->transitions transition)
                             (plist-get airplay/image->transitions :none))))
    (airplay/protocol:put
     "photo"
     :headers `(("X-Apple-Transition" . ,transition_val))
     :data (with-temp-buffer
             (insert-file-contents-literally image_file)
             (buffer-string)))))

(defun airplay:stop ()
  (airplay/protocol:post "stop"))

(defun airplay/video:view (video_location)
  (airplay/protocol:post
   "play"
   :data (airplay/protocol:make-text-parameters
          `(("Content-Location" . ,video_location)
            ("Start-Position"   . "0.0")))))

(defun airplay:set_scrub (position)
  (airplay/protocol:post
   "scrub"
   :params `(("position" . ,position))))

(defun airplay:get_scrub ()
  (airplay/protocol:get
   "scrub"
   :parser 'airplay/protocol:parse-text-parameters
   :success (function*
             (lambda (&key data &allow-other-keys)
               (let ((position (cdr (assoc "position" data)))
                     (duration (cdr (assoc "duration" data))))
                 (message (format "%s/%s" position duration)))))))

(defun airplay/video:scrub (&optional position)
  (if position (airplay:set_scrub position) (airplay:get_scrub)))

(defun airplay:playback-info (&optional callback)
  (lexical-let
      ((callback (or callback
                     (lambda (data)
                       (if (null data)
                           (message "Not playing...")
                         (message "Playing now!"))))))
    (airplay/protocol:get
     "playback-info"
     :parser 'airplay/protocol:parse-plist-xml
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (funcall callback data))))))

(provide 'airplay)

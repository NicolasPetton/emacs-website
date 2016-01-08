;;; osc.el --- Open Sound Control protocol library

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Version: 0.1
;; Keywords: comm, processes, multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; OpenSound Control ("OSC") is a protocol for communication among
;; computers, sound synthesizers, and other multimedia devices that is
;; optimized for modern networking technology and has been used in many
;; application areas.

;; This package implements low-level functionality for OSC clients and servers.
;; In particular:
;; * `osc-make-client' and `osc-make-server' can be used to create process objects.
;; * `osc-send-message' encodes and sends OSC messages from a client process.
;; * `osc-server-set-handler' can be used to change handlers for particular
;;   OSC paths on a server process object on the fly.

;; BUGS/TODO:
;;
;; * Timetags and binary blobs are not supported yet.

;; Usage:
;;
;; Client: (setq my-client (osc-make-client "localhost" 7770))
;;         (osc-send-message my-client "/osc/path" 1.5 1.0 5 "done")
;;         (delete-process my-client)
;;
;; Server: (setq my-server (osc-make-server "localhost" 7770
;;          (lambda (path &rest args)
;;            (message "OSC %s: %S" path args))))

;;; Code:

(require 'cl-lib)

(defun osc-insert-string (string)
  (insert string 0 (make-string (- 3 (% (length string) 4)) 0)))

(defun osc-insert-float32 (value)
  (let (s (e 0) f)
    (cond
     ((string= (format "%f" value) (format "%f" -0.0))
      (setq s 1 f 0))
     ((string= (format "%f" value) (format "%f" 0.0))
      (setq s 0 f 0))
     ((= value 1.0e+INF)
      (setq s 0 e 255 f (1- (expt 2 23))))
     ((= value -1.0e+INF)
      (setq s 1 e 255 f (1- (expt 2 23))))
     ((string= (format "%f" value) (format "%f" 0.0e+NaN))
      (setq s 0 e 255 f 1))
     (t
      (setq s (if (>= value 0.0)
		  (progn (setq f value) 0)
		(setq f (* -1 value)) 1))
      (while (>= (* f (expt 2.0 e)) 2.0) (setq e (1- e)))
      (if (= e 0) (while (< (* f (expt 2.0 e)) 1.0) (setq e (1+ e))))
      (setq f (round (* (1- (* f (expt 2.0 e))) (expt 2 23)))
	    e (+ (* -1 e) 127))))
    (insert (+ (lsh s 7) (lsh (logand e #XFE) -1))
	    (+ (lsh (logand e #X01) 7) (lsh (logand f #X7F0000) -16))
	    (lsh (logand f #XFF00) -8)
	    (logand f #XFF))))

(defun osc-insert-int32 (value)
  (let (bytes)
    (dotimes (i 4)
      (push (% value 256) bytes)
      (setq value (/ value 256)))
    (dolist (byte bytes)
      (insert byte))))

;;;###autoload
(defun osc-make-client (host port)
  "Create an OSC client process which talks to HOST and PORT."
  (make-network-process
   :name "OSCclient"
   :coding 'binary
   :host host
   :service port
   :type 'datagram))

;;;###autoload
(defun osc-send-message (client path &rest args)
  "Send an OSC message from CLIENT to the specified PATH with ARGS."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (osc-insert-string path)
    (osc-insert-string
     (apply 'concat "," (mapcar (lambda (arg)
				  (cond
				   ((floatp arg) "f")
				   ((integerp arg) "i")
				   ((stringp arg) "s")
				   (t (error "Invalid argument: %S" arg))))
				args)))
    (dolist (arg args)
      (cond
       ((floatp arg) (osc-insert-float32 arg))
       ((integerp arg) (osc-insert-int32 arg))
       ((stringp arg) (osc-insert-string arg))))
    (process-send-string client (buffer-string))))

(defun osc-read-string ()
  (let ((pos (point)) string)
    (while (not (= (following-char) 0)) (forward-char 1))
    (setq string (buffer-substring-no-properties pos (point)))
    (forward-char (- 4 (% (length string) 4)))
    string))

(defun osc-read-int32 ()
  (let ((value 0))
    (dotimes (i 4)
      (setq value (logior (* value 256) (following-char)))
      (forward-char 1))
    value))

(defun osc-read-float32 ()
  (let ((s (lsh (logand (following-char) #X80) -7))
	(e (+ (lsh (logand (following-char) #X7F) 1)
	      (lsh (logand (progn (forward-char) (following-char)) #X80) -7)))
	(f (+ (lsh (logand (following-char) #X7F) 16)
	      (lsh (progn (forward-char) (following-char)) 8)
	      (prog1 (progn (forward-char) (following-char)) (forward-char)))))
    (cond
     ((and (= e 0) (= f 0))
      (* 0.0 (expt -1 s)))
     ((and (= e 255) (or (= f (1- (expt 2 23))) (= f 0)))
      (* 1.0e+INF (expt -1 s)))
     ((and (= e 255) (not (or (= f 0) (= f (1- (expt 2 23))))))
      0.0e+NaN)
     (t
      (* (expt -1 s)
	 (expt 2.0 (- e 127))
	 (1+ (/ f (expt 2.0 23))))))))

(defun osc-server-set-handler (server path handler)
  "Set HANDLER for PATH on SERVER.
IF HANDLER is nil, remove previously defined handler and fallback to
the generic handler for SERVER."
  (let* ((handlers (plist-get (process-plist server) :handlers))
	 (slot (assoc-string path handlers)))
    (if slot
	(setcdr slot handler)
      (plist-put
       (process-plist server)
       :handlers (nconc (list (cons path handler)) handlers)))))

(defun osc-server-get-handler (server path)
  (or (cdr (assoc path (plist-get (process-plist server) :handlers)))
      (plist-get (process-plist server) :generic)))

(defun osc-filter (proc string)
  (when (= (% (length string) 4) 0)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert string)
      (goto-char (point-min))
      (let ((path (osc-read-string)))
	(if (not (string= path "#bundle"))
	    (when (looking-at ",")
	      (save-excursion
		(apply (osc-server-get-handler proc path)
		       path
		       (mapcar
			(lambda (type)
			  (cl-case type
			    (?f (osc-read-float32))
			    (?i (osc-read-int32))
			    (?s (osc-read-string))))
			(string-to-list (substring (osc-read-string) 1))))))
	  (forward-char 8) ;skip 64-bit timetag
	  (while (not (eobp))
	    (let ((size (osc-read-int32)))
	      (osc-filter proc
			  (buffer-substring
			   (point) (progn (forward-char size) (point)))))))))))

;;;###autoload
(defun osc-make-server (host port default-handler)
  "Create an OSC server which listens on HOST and PORT.
DEFAULT-HANDLER is a function with arguments (path &rest args) which is called
when a new OSC message arrives.  See `osc-server-set-handler' for more
fine grained control.
A process object is returned which can be dicarded with `delete-process'."
  (make-network-process
   :name "OSCserver"
   :filter #'osc-filter
   :host host
   :service port
   :server t
   :type 'datagram
   :plist (list :generic default-handler)))

(defun osc--test-transport-equality (value)
  "Test if transporting a certain VALUE via OSC results in equality.
This is mostly for testing the implementation robustness."
  (let* ((osc-test-value value)
	 (osc-test-func (cond ((or (floatp value) (integerp value)) '=)
			      ((stringp value) 'string=)))
	 (osc-test-done nil)
	 (osc-test-ok nil)
	 (server (osc-make-server
		  "localhost" t
		  (lambda (_path v)
		    (setq osc-test-done t
			  osc-test-ok (list v (funcall osc-test-func
						       osc-test-value v))))))
	 (client (osc-make-client
		  (nth 0 (process-contact server)) (nth 1 (process-contact server)))))
    (osc-send-message client
		      "/test" osc-test-value)
    (while (not osc-test-done)
      (accept-process-output server 0 500))
    (delete-process server) (delete-process client)
    osc-test-ok))

;;;; ChangeLog:

;; 2014-05-26  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/osc/osc.el (osc-filter): Fix up old `cl' name.
;; 	(osc--test-transport-equality): Mark `path' as unused.
;; 
;; 2014-05-24  Mario Lang	<mlang@delysid.org>
;; 
;; 	Add osc.el.
;; 


(provide 'osc)
;;; osc.el ends here

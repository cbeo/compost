;;; UTILITIES

(in-package #:compost)

(defun pw-digest (str)
  (flexi-streams:octets-to-string 
   (ironclad:digest-sequence
    :sha256
    (flexi-streams:string-to-octets str :external-format :utf-8))
   :external-format :latin1))

(defun make-uid (&optional (string ""))
  (remove #\=  (base64:string-to-base64-string 
                (format nil "~a~a"
                        (gensym string)
                        (get-universal-time)))))

(defun split-string (delimiter string)
  (labels ((rec (acc start)
             (if-let (found (search delimiter string :start2 start))
               (rec (cons (subseq string start found) acc)
                    (+ found (length delimiter)))
               (reverse (cons (subseq string start) acc)))))
    (rec nil 0)))



(let ((timezone-names nil))
  (defun timezone-names ()
    (if timezone-names timezone-names
        (setf timezone-names
              (sort  (loop :for k :being :the :hash-keys :of local-time::*location-name->timezone* 
                           :collect k)
                     #'string<)))))

(defun timestring (universal-time zone)
  "Given a UNIVERSAL-TIME and a string naming a timezone, return a
  nice looking string  representing the time."
  (local-time:format-timestring
   nil
   (local-time:universal-to-timestamp universal-time)
   :format '(:long-weekday ", " :long-month " " :day " at " :hour ":" :min)
   :timezone (local-time:find-timezone-by-location-name zone)))

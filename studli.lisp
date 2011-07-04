(defpackage :studli
  (:use :cl :drakma :cl-ppcre :lisp-unit))

(in-package :studli)

(defparameter *testing* t)

(defparameter *username* "")

(defparameter *password* "")

(defparameter *url* "https://studip.uni-passau.de/studip/")

(defparameter *rc-file* "~/.studlirc")

(load *rc-file*)

(defparameter *cookie* (make-instance 'cookie-jar))

;; init-session: string string -> boolean
;; initiate login to studip with username :username, password
;; :password, report success
(defun init-session (&key username password)
  (let ((response (do-init-request)))
    (cond
      ((scan "eingeloggt" response) (http-request (concatenate 'string *url* "index.php")
                                                  :cookie-jar *cookie*))
      (t
       (multiple-value-bind (regex matches)
           (scan-to-strings "id=\"login_ticket\" value=\"(\\S+)?\"" response)
         (declare (ignore regex))
         (let ((login-ticket (elt matches 0)))
           (http-request (concatenate 'string *url* "index.php")
                         :method :post
                         :parameters `(("username" . ,username)
                                       ("password" . ,password)
                                       ("login_ticket" . ,login-ticket))
                         :cookie-jar *cookie*)))))))

(define-test init-test
  (assert-true (scan "Startseite f&uuml;r Studierende bei Stud.IP" (init-session :username *username* :password *password*)))
  (do-logout)
  (assert-true (scan "Bitte identifizieren Sie sich" (init-session :username *username* :password "bogus"))))
                      
;; do-init-request: -> string
;; returns login page of studip
(defun do-init-request ()
  (http-request (concatenate 'string *url* "login.php")
                :cookie-jar *cookie*))

(define-test init-request
  (assert-true (scan "login_ticket" (do-init-request))))

;; do-logout: -> boolean
;; perform logout on page, return success
(defun do-logout ()
  (not (null (scan "abgemeldet" (http-request (concatenate 'string *url* "logout.php")
                                              :cookie-jar *cookie*)))))

(define-test logout-test
  (do-logout)
  (assert-true (scan "login_ticket" (do-init-request))))

;; do-seminars-request: -> string
;; return page of seminars as string
(defun do-seminars-request ()
  (http-request (concatenate 'string *url* "meine_seminare.php")
                :cookie-jar *cookie*))

(define-test seminars-request-test
  (init-session :username *username* :password *password*)
  (assert-true (scan "Meine Veranstaltungen" (do-seminars-request))))

;; list-all-seminars: -> (listof (string . string))
;; return all seminars in a list
(defun list-all-seminars ()
  (nreverse (get-all-groups-from-scan "(?m)<a href=\"(seminar.*?)\"\\s*>.*?<font size=.*?>(.*?)</font>" (do-seminars-request))))

;; seminar-download-links: -> (listof (string . string))
;; return all seminars in a list with its name and its url for the
;; download section
(defun seminar-download-links ()
  (nreverse (get-all-groups-from-scan "(?m)<a href=\"seminar.*?\"\\s*>.*?<font size=.*?>(.*?)</font>.*?(seminar_main.\\S*?seminarFolders)" (do-seminars-request))))

;; get-all-groups-from-scan: string string -> (listof (array string))
;; compute all possibilities matching regex in str, and return their
;; grouping
(defun get-all-groups-from-scan (regex str)
  (let ((result '()))
    (do-register-groups (first second) (regex str)
      (push (cons first second) result))
    result))
;; select-seminar: -> string
;; reads in a selection of a seminar and returns its page
(defun select-seminar ()
  (select :function #'list-all-seminars
          :url-pos #'car
          :str-pos #'cdr))

;; select-seminar-download: -> string
;; reads in a selection of a seminar and returns download section of
;; it
(defun select-seminar-download ()
  (select :function #'seminar-download-links
          :url-pos #'cdr
          :str-pos #'car))

;; list-news: -> (listof (string . string))
;; return title and url of all news
(defun list-news ()
  (nreverse (get-all-groups-from-scan "(?m)<td class=\"printhead\".*?(seminar_main.php.*?)\".*?>(.*?)</a>" (select-seminar))))

;; select-seminar-news: -> string
;; return title and text of a selected news
(defun select-seminar-news ()
  (select :function #'list-news
          :url-pos #'car
          :str-pos #'cdr))

;; display-seminar-news: -> void
;; EFFECT: display content of selected news
(defun display-seminar-news ()
  (regex-replace-all "<br.*?>" (elt (nth-value 1 (scan-to-strings "(?m)class=\"printcontent\".*?<br>(.*?)</td>" (select-seminar-news))) 0) "
"))

;; select-seminar-details: -> string
;; return html-page of selected seminar
(defun select-seminar-details ()
  (select-seminar)
  (http-request (concatenate 'string *url* "details.php")
                :cookie-jar *cookie*))

;; cleanup-html: string -> string
;; removes all html specific content from strand returns only text
(defun cleanup-html (str)
  (regex-replace-all "\\s\\s+"
                     (regex-replace-all "(<br.*?>|<tr>|<li>)"
                                        (regex-replace-all "(<td.*?>|</td>|</tr>|<table.*?>|</table>|<font.*?>|</font>|<a.*?>|</a>|<img.*?>|<div.*?>|</div>|<b>|</b>|<i>|</i>|<blockquote>|</blockquote>|<script.*?>|</script>|<!--.*?-->|<ul>|</ul>|</li>)" str "")
                                        (format nil "~%"))
                     " "))
;; get-seminar-details: -> string
;; return details of selected seminar as a string without html
;; specific content
(defun get-seminar-details ()
  (cleanup-html (scan-to-strings "(?m)<table.*</table>" (substitute #\  #\Newline (select-seminar-details)))))

(defun select (&key function url-pos str-pos)
  (let ((selection (funcall function)))
    (print-selection selection :str-pos str-pos)
    (format t "Select an action: ")
    (let ((selected (read)))
      (cond
        ((numberp selected) (http-request (concatenate 'string *url* (funcall url-pos (elt selection (1- selected))))
                                          :cookie-jar *cookie*))
        (t (format t "It needs to be a number. You typed: ~a~%" selected)
           (select :function function :url-pos url-pos :str-pos str-pos))))))

;; print-selection: (listof (string . string)) (:str-pos function) -> void
;; EFFECT: print possibilities from selection with accessor :str-pos
(defun print-selection (selection &key str-pos)
  (loop for name in selection
     and j = 1 then (1+ j)
     do (format t "~a: ~a~%" (funcall str-pos name) j)))

;; do-change-password: String String -> void
;; EFFECT: change old-password for username to new-password, also
;; change the corresponding studlirc file, read old-password
(defun do-change-password (new-password)
  (progn
    (change-rc-parameter "*password*" new-password)
    (change-password-request *username* new-password *password*)
    (defparameter *password* new-password)))

;; change-rc-parameter: String String -> void
;; EFFECT: change studlirc to have the new or replaced value in
;; parameter
(defun change-rc-parameter (parameter value)
  (labels ((escape-earmuffs (str)
             (regex-replace-all "\\*" str "\\\\*")))
    (alexandria:write-string-into-file
     (regex-replace (format nil "(~A)\\s+.*?\\)" (escape-earmuffs parameter)) (alexandria:read-file-into-string *rc-file*) (format nil "\\1 ~w)" value))
     *rc-file*
     :if-exists :supersede)))

;; change-password-request: String String String -> void
;; make a request to change your password on studip for the username
;; from old to new
(defun change-password-request (username new old)
  (http-request "https://www.rz.uni-passau.de/cgi-bin/setpass"
                :parameters `(("user" . ,username)
                              ("opass" . ,old)
                              ("npass" . ,new)
                              ("npass2" . ,new)
                              ("change" . "true"))
                :method :post
                :cookie-jar *cookie*))

(when *testing*
  (run-tests))


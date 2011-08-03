;; (dolist (i '(drakma cl-ppcre lisp-unit alexandria html-entities cl-fad))
;;   (ql:quickload i))

(defpackage :studli
  (:use :cl :drakma :cl-ppcre :lisp-unit :cl-fad))

(in-package :studli)

(defparameter *testing* t)

(defparameter *username* "")

(defparameter *password* "")

(defparameter *list-num-first-seminars* 5)

(defparameter *url* "https://studip.uni-passau.de/studip/")

(defparameter *rc-file* "~/.studlirc")

(defparameter *seminars* nil)

(when (file-exists-p *rc-file*)
  (load *rc-file*))

(defparameter *cookie* (make-instance 'cookie-jar))

;;; studip-http-request: String -> String
;;; make an http-request to the studip-website concatenated with the suffix
(defun studip-http-request (suffix)
  (http-request (concatenate 'string *url* suffix)
                :cookie-jar *cookie*))

;;; index-request: -> String
;;; return the html of the index-page of studip
(defun index-request ()
  (studip-http-request "index.php"))

;; init-session: string string -> boolean
;; initiate login to studip with username :username, password
;; :password, report success
(defun init-session (&key username password)
  (let ((response (do-init-request)))
    (cond
      ((already-logged-in? response) (index-request))
      (t (init-login-request :username username
                             :password password
                             :response response)))))

;;; already-logged-in?: String -> Boolean
;;; check if the response to the index-page thinks you're logged in
(defun already-logged-in? (response)
  (scan "eingeloggt" response))

;;; init-login-request: String String -> String
;;; make a login-request with username and password and return the
;;; index page
(defun init-login-request (&key username password response)
  (http-login-request username password (search-for-login-ticket response)))

;;; search-for-login-ticket: String -> String
;;; search in an html response the login-ticket to log in to the
;;; studip system
(defun search-for-login-ticket (response)
  (multiple-value-bind (regex matches)
      (scan-to-strings "id=\"login_ticket\" value=\"(\\S+)?\"" response)
    (declare (ignore regex))
    (elt matches 0)))

;;; http-login-request: String String String -> String
;;; make a login-request with authentication and sending the
;;; login-ticket and return the index page
(defun http-login-request (username password login-ticket)
  (http-request (concatenate 'string *url* "index.php")
                :method :post
                :parameters `(("username" . ,username)
                              ("password" . ,password)
                              ("login_ticket" . ,login-ticket))
                :cookie-jar *cookie*))

(define-test init-test
  (assert-true (scan "Startseite f&uuml;r Studierende bei Stud.IP"
                     (init-session :username *username* :password *password*)))
  (do-logout)
  (assert-true (scan "Bitte identifizieren Sie sich"
                     (init-session :username *username* :password "bogus"))))
                      
;; do-init-request: -> string
;; returns login page of studip
(defun do-init-request ()
  (studip-http-request "login.php"))

(define-test init-request
  (do-logout)
  (assert-true (scan "login_ticket" (do-init-request))))

;; do-logout: -> boolean
;; perform logout on page, return success
(defun do-logout ()
  (not (null (scan "abgemeldet" (studip-http-request "logout.php")))))

(define-test logout-test
  (do-logout)
  (assert-true (scan "login_ticket" (do-init-request))))

;; do-seminars-request: -> string
;; return page of seminars as string
(defun do-seminars-request ()
  (studip-http-request "meine_seminare.php"))

(define-test seminars-request-test
  (init-session :username *username* :password *password*)
  (assert-true (scan "Meine Veranstaltungen" (do-seminars-request))))

;; list-desired-seminars: -> (listof (string . string))
;; return all seminars in a list
(defun list-desired-seminars ()
  (if *seminars*
      *seminars*
      (request-new-seminar-list)))

;;; request-new-seminar-list: -> (listof (String . String))
;;; EFFECT: set *seminar* to new list
;;; request new seminar list and take the desired amount of it
(defun request-new-seminar-list ()
  (setf *seminars*
        (take *list-num-first-seminars*
              (list-all-seminars))))

;;; list-all-seminars: -> (Pair (listof String) (listof String))
;;; returns all Seminar names and their corresponding paths
(defun list-all-seminars ()
  (nreverse (get-all-groups-from-scan
             "(?m)<a href=\"(seminar.*?)\"\\s*>.*?<font size=.*?>(.*?)</font>"
             (do-seminars-request))))

;; seminar-download-links: -> (listof (string . string))
;; return all seminars in a list with its name and its url for the
;; download section
(defun seminar-download-links ()
  (nreverse (get-all-groups-from-scan
             "(?m)<a href=\"seminar.*?\"\\s*>.*?<font size=.*?>(.*?)</font>.*?(seminar_main.\\S*?seminarFolders)"
             (do-seminars-request))))

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
  (select :function #'list-desired-seminars
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
  (nreverse (get-all-groups-from-scan
             "(?s)<td class=\"printhead\".*?(seminar_main.php.*?)\".*?>(.*?)</a>"
             (select-seminar))))

;; select-seminar-news: -> string
;; return title and text of a selected news
(defun select-seminar-news ()
  (select :function #'list-news
          :url-pos #'car
          :str-pos #'cdr))

;; get-seminar-news: -> String
;; return a specific news from news of a seminar
(defun get-seminar-news ()
  (regex-replace-all "<br.*?>"
                     (elt (nth-value 1
                                     (scan-to-strings "(?s)class=\"printcontent\".*?<br>(.*?)</td>"
                                                      (select-seminar-news))
                                     )
                          0)
                     "
"))

;; select-seminar-details: -> string
;; return html-page of selected seminar
(defun select-seminar-details ()
  (select-seminar)
  (studip-http-request "details.php"))

;; cleanup-html: string -> string
;; removes all html specific content from str and returns only text by
;; substituting the equivalent ascii-string
(defun cleanup-html (str)
  (regex-replace-all "(?s)(<br.*?>|<tr>|<li>)"
                     (regex-replace-all "(?s)(<td.*?>|</td>|</tr>|<table.*?>|</table>|<font.*?>|</font>|<a.*?>|</a>|<img.*?>|<div.*?>|</div>|<b>|</b>|<i>|</i>|<blockquote>|</blockquote>|<script.*?>|</script>|<!--.*?-->|<ul>|</ul>|</li>|\\s\\s+)"
                                        str "")
                     (format nil "~%")))


;; get-seminar-details: -> string
;; return details of selected seminar as a string without html
;; specific content
(defun get-seminar-details ()
  (html-entities:decode-entities
   (cleanup-html
    (scan-to-strings "(?s)<table.*</table>"
                     (select-seminar-details)))))

;;; print-seminar-details: -> void
(defun print-seminar-details ()
  (format t "~%~a" (get-seminar-details)))

;;; select: (-> Pair) (Pair -> String) (Pair -> String) -> String
(defun select (&key function url-pos str-pos)
  (let ((selection (funcall function)))
    (print-selection selection :str-pos str-pos)
    (select-an-action selection url-pos)))

;;; select-an-action: Pair (Pair -> String) -> String
;;; select an action by number from a selection
(defun select-an-action (selection url-pos)
  (format t "Select an action: ")
  (let* ((to-select (read-a-number))
         (selected (elt selection (1- to-select)))
         (url-suffix (funcall url-pos selected)))
    (studip-http-request url-suffix)))

;;; read-a-number: &optional String -> Number
;;; get a number from the user print optional String before allowing input
(defun read-a-number (&optional (say "Please provide a number: "))
  (let ((number (read)))
    (cond
      ((numberp number) number)
      (t (progn
           (format t "~&~a" say)
           (read-a-number))))))

;; print-selection: (listof (string . string)) (:str-pos function) -> void
;; EFFECT: print possibilities from selection with accessor :str-pos
(defun print-selection (selection &key str-pos)
  (loop for name in selection
     and j = 1 then (1+ j)
     do (format t "~a: ~a~%" (html-entities:decode-entities (funcall str-pos name)) j)))

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
     (regex-replace (format nil "(~A)\\s+.*?\\)" (escape-earmuffs parameter))
                    (alexandria:read-file-into-string *rc-file*)
                    (format nil "\\1 ~w)" value))
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

;;; take: Number (listof A) -> (listof A)
;;; it gives the first num items in list back
(defun take (num list)
  (cond
    ((or (<= num 0) (null list)) nil)
    (t (cons (car list)
           (take (1- num) (cdr list))))))

(define-test take-test
  (let ((alist (list 'a 'b 'c))
        (numlist (list 1 2 3)))
    (assert-equal (list 'a) (take 1 alist))
    (assert-equal (list 'a 'b) (take 2 alist))
    (assert-equal alist (take 3 alist))
    (assert-equal alist (take 100 alist))
    (assert-equal nil (take 5 '()))
    (assert-equal nil (take 0 '()))
    (assert-equal nil (take -1 '()))
    (assert-equal nil (take 0 alist))
    (assert-equal nil (take -5 alist))
    (assert-equal numlist (take 5 numlist))))

(when *testing*
  (run-tests))

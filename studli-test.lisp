(defpackage :studli-test
  (:use :cl :asdf :lisp-unit :studli))

(in-package :studli-test)

(define-test init-test
  (assert-true (scan "Startseite f&uuml;r Studierende bei Stud.IP"
                     (init-session :username *username* :password *password*)))
  (do-logout)
  (assert-true (scan "Bitte identifizieren Sie sich"
                     (init-session :username *username* :password "bogus"))))


(define-test init-request
  (do-logout)
  (assert-true (scan "login_ticket" (do-init-request))))


(define-test logout-test
  (do-logout)
  (assert-true (scan "login_ticket" (do-init-request))))


(define-test seminars-request-test
  (init-session :username *username* :password *password*)
  (assert-true (scan "Meine Veranstaltungen" (do-seminars-request))))


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

(run-tests)

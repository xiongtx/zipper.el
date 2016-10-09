(require 'cl-lib)
(require 'zipper)


;; Test data
(defvar data '((1 2) (3 (4 5) 6) (7 8)))
(defvar z (zipper data))


;; Conversion to and from zipper

(ert-deftest zipper-roundtrip-test ()
  "Tests zipping and unzipping tree."
  (should (equal data (zipper-root z))))


;; Navigation

(ert-deftest zipper-top-test ()
  "Tests detecting zipper top."
  (should (zipper-top-p z)))

(ert-deftest zipper-down-test ()
  "Tests moving down."
  (let ((zd (zipper-down z)))
    (should (equal (cl-first data) (node zd)))
    (should (equal nil (left (path zd))))
    (should (equal nil (ppath (path zd))))
    (should (equal (cdr data) (right (path zd))))))

(ert-deftest zipper-up-test ()
  "Tests moving up."
  (let ((zd (zipper-down z)))
    (should (equal data (node (zipper-up zd))))))

(ert-deftest zipper-right-test ()
  "Tests moving right."
  (should (equal nil (zipper-right z)))
  (let* ((zd (zipper-down z))
         (zdr (zipper-right zd)))
    (should (equal (cl-second data) (node zdr)))
    (should (equal (list (cl-first data)) (left (path zdr))))
    (should (equal (list (cl-third data)) (right (path zdr))))))

(ert-deftest zipper-rightmost-test ()
  "Tests moving rightmost."
  (let ((zd (zipper-down z)))
    (should (equal (car (last data)) (node (zipper-rightmost zd))))
    (should (equal (zipper-rightmost zd)
                   (zipper-rightmost (zipper-rightmost zd))))))

(ert-deftest zipper-rights ()
  "Tests getting right siblings."
  (let ((zd (zipper-down z)))
    (should (equal (cdr data) (zipper-rights zd)))))

(ert-deftest zipper-left-test ()
  "Tests moving left."
  (should (equal nil (zipper-left z)))
  (let* ((zd (zipper-down z))
         (zdrm (zipper-rightmost zd)))
    (should (equal nil (zipper-left zd)))
    (should (equal (car ) (zipper-left zdrm)))))

(ert-deftest zipper-leftmost-test ()
  "Tests moving leftmost."
  (let* ((zd (zipper-down z))
         (zdrm (zipper-rightmost zd)))
    (should (equal (cl-first data) (node (zipper-leftmost zdrm))))
    (should (equal (zipper-leftmost zdrm)
                   (zipper-leftmost (zipper-leftmost zdrm))))))

(ert-deftest zipper-lefts-test ()
  "Tests getting left siblings."
  (let* ((zd (zipper-down z))
         (zdrm (zipper-rightmost zd)))
    (should (equal (butlast data) (zipper-lefts zdrm)))))


;; Insert and edit

(ert-deftest zipper-insert-left-test ()
  "Tests inserting left."
  (let* ((zd (zipper-down z))
         (zdil (zipper-insert-left zd 'x)))
    (should (equal 'x (cl-first (left (path zdil)))))))

(ert-deftest zipper-insert-right-test ()
  "Tests inserting left."
  (let* ((zd (zipper-down z))
         (zdir (zipper-insert-right zd 'x)))
    (should (equal 'x (cl-first (right (path zdir)))))))

(ert-deftest zipper-replace-test ()
  "Tests replacing node."
  (let* ((zd (zipper-down z))
         (zdrp (zipper-replace zd '(x y z))))
    (should (equal '(x y z) (node zdrp)))
    (should (equal (path zd) (path zdrp)))))

(ert-deftest zipper-edit-test ()
  "Tests editing node."
  (let* ((zd (zipper-down z))
         (zde (zipper-edit zd #'append '(x y z))))
    (should (equal (append (node zd) '(x y z)) (node zde)))
    (should (equal (path zd) (path zde)))))

(ert-deftest zipper-insert-child-test ()
  "Tests inserting to children."
  (let ((zd (zipper-down z)))
    (should (equal (cons 'x (cl-first data))
                   (node (zipper-insert-child zd 'x))))))

(ert-deftest zipper-append-child-test ()
  "Tests appending to children."
  (let ((zd (zipper-down z)))
    (should (equal (append (cl-first data) (list 'x))
                   (node (zipper-append-child zd 'x))))))


;; Depth-first walk

(ert-deftest zipper-next-test ()
  "Tests going to the next loc."
  (let* ((zd (zipper-down z))
         (zdd (zipper-down (zipper-down z)))
         (zdr (zipper-right (zipper-down z))))
    (should (equal zd (zipper-next z)))
    (should (equal zdd (zipper-next zd)))))

(ert-deftest zipper-prev-test ()
  "Tests going to the previous loc."
  (let* ((zd (zipper-down z))
         (zdr (zipper-right (zipper-down z)))
         (zdd (zipper-down (zipper-down z)))
         (zddrm (zipper-rightmost zdd)))
    (should (equal zddrm (zipper-prev zdr)))
    (should (equal zd (zipper-prev zdd)))))


;; Remove

(ert-deftest zipper-remove-test ()
  "Tests removing node."
  (let* ((zd (zipper-down z)))
    (should (equal (cdr data) (node (zipper-remove zd))))))







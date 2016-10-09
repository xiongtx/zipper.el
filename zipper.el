;;; zipper.el --- Zipper data structure -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Tianxiang Xiong

;; Author: Tianxiang Xiong <tianxiang.xiong@gmail.com>
;; Keywords: lisp, data
;; Version: 0.1.0

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

;; An implementation of the zipper data structure, as described by Gerard Huet
;; [fn:1], based on Clojure's `clojure.zip' [fn:2] and Daniel Martin's
;; `cl-zipper' [fn:3].

;; Here, a `node' is a tree (list or atom), and a path is a `zipper-path'
;; object representing the path to the root. This vocabulary is closer to that
;; of `clojure.zip' than Huet's paper.

;; [fn:1]: https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf
;; [fn:2]: https://github.com/clojure/clojure/blob/master/src/clj/clojure/zip.clj
;; [fn:3]: https://github.com/danielfm/cl-zipper

;;; Code:

(require 'cl-lib)


;; Classes

(defclass zipper-path ()
  ((left
    :accessor left
    :initarg :left
    :documentation "Left siblings.")
   (ppath
    :accessor ppath
    :initarg :ppath
    :documentation "Path to root.")
   (right
    :accessor right
    :initarg :right
    :documentation "Right siblings.")))

(defclass zipper-loc ()  
  ((node
    :accessor node
    :initarg :node
    :documentation "Current tree")
   (path
    :accessor path
    :initarg :path
    :documentation "Path to root")))

(defun zipper--make-path (left ppath right)
  "Create a `zipper-path' object."
  (make-instance 'zipper-path :left left :ppath ppath :right right))

(defun zipper--make-loc (node path)
  "Create a `zipper-loc' object."
  (make-instance 'zipper-loc :node node :path path))

(defun zipper--make-loc-path (node left ppath right)
  "Create a `zipper-loc' object with a new path."
  (zipper--make-loc node (zipper--make-path left ppath right)))


;; Conversion to and from zipper

(defun zipper (root)
  "Create a zipper for ROOT."
  (zipper--make-loc root nil))

(defun zipper-root (loc)
  "Zips all the way up and returns the root node, reflecting any
changes."
  (let ((up (zipper-up loc)))
    (if up
        (zipper-root up)
      (node loc))))


;; Utils

(defmacro with-zipper-loc (loc &rest body)  
  "Binds the variables `node', `path', `left', `ppath', and
`right' for LOC."
  (declare (indent defun))
  `(let* ((node (node ,loc))
          (path (path ,loc))
          (left (condition-case nil (left path)  
                  (cl-no-applicable-method nil)))
          (ppath (condition-case nil (ppath path)
                   (cl-no-applicable-method nil)))
          (right (condition-case nil (right path)  
                   (cl-no-applicable-method nil))))
     ,@body))

(defun zipper-branch-p (loc)
  "Returns true if the node at LOC is a branch."
  (with-zipper-loc loc
    (consp node)))


;; Navigation

(defun zipper-top-p (loc)
  "Returns true if LOC is at the top."
  (null (path loc)))

(defun zipper-down (loc)
  "Returns the loc of the leftmost child of the node at this loc,
or nil if none."
  (with-zipper-loc loc
    (when (sequencep node)
      (zipper--make-loc-path (car node)
                             nil
                             path
                             (cdr node)))))

(defun zipper-up (loc)
  "Returns the loc of the parent of the node at this loc, or nil
if at the top."
  (with-zipper-loc loc
    (unless (zipper-top-p loc)
      (zipper--make-loc (append (reverse left)
                                (cons node right))
                        (ppath path)))))

(defun zipper-right (loc)
  "Returns the loc of the right sibling of the node at this loc, 
or nil."
  (with-zipper-loc loc
    (when (and path right)
      (zipper--make-loc-path (car right)
                             (cons node left)
                             (ppath path)
                             (cdr right)))))

(defun zipper-rightmost (loc)
  "Returns the loc of the rightmost sibling of the node at this
loc, or self."
  (with-zipper-loc loc
    (if (and path right)
        (zipper--make-loc-path (car (last right))
                               (append (butlast right) (list node) left)
                               ppath
                               nil)
      loc)))

(defun zipper-rights (loc)
  "Returns a list of the right siblings of this loc"
  (with-zipper-loc loc
    (when path right)))

(defun zipper-left (loc)
  "Returns the loc of the left sibling of the node at this loc,
or nil."
  (with-zipper-loc loc
    (when (and path left)
      (zipper--make-loc-path (car left)
                             (cdr left)
                             ppath
                             (cons node right)))))

(defun zipper-leftmost (loc)
  "Returns the loc of the leftmost sibling of the node at this
loc, or self."
  (with-zipper-loc loc
    (if left
        (zipper--make-loc-path (car (last left))
                               nil
                               ppath
                               (append (butlast left) (list node) right))
      loc)))

(defun zipper-lefts (loc)
  "Returns a list of the left siblings of this loc."
  (with-zipper-loc loc
    (when path (reverse left))))

(defun zipper-nodes-path (loc)
  "Returns the list of nodes leading up to LOC."
  (let ((up (zipper-up loc)))
    (when up
      (append (zipper-path up) (list (node up))))))


;; Insert and edit

(defun zipper-insert-left (loc item)
  "Inserts ITEM as the left sibling of the node at this LOC,
without moving."
  (with-zipper-loc loc
    (if (zipper-top-p loc)
        (error "Insert left at top")
      (zipper--make-loc-path node
                             (cons item left)
                             ppath
                             right))))

(defun zipper-insert-right (loc item)
  "Inserts ITEM as the right sibling of the node at this LOC,
without moving."
  (with-zipper-loc loc
    (if (zipper-top-p loc)
        (error "Insert right at top")
      (zipper--make-loc-path node
                             left
                             ppath
                             (cons item right)))))

(defun zipper-replace (loc n)
  "Replaces the node at this LOC with node N, without moving."
  (with-zipper-loc loc
    (zipper-make-loc n path)))

(defun zipper-edit (loc f &rest args)
  "Replaces the node at this LOC with the value of (f node args)."
  (with-zipper-loc loc
    (zipper-replace loc (apply f node args))))

(defun zipper-insert-child (loc item)
  "Inserts ITEM as the leftmost child of the node at this LOC,
without moving."
  (with-zipper-loc loc
    (zipper-replace loc (cons item node))))

(defun zipper-append-child (loc item)
  "Inserts ITEM as the rightmost child of the node at this LOC,
without moving."
  (with-zipper-loc loc
    (zipper-replace loc (append node (list item)))))


;; Depth-first walk

(defun zipper-end-p (loc)
  "Returns true if LOC represents the end of a depth-first
walk."
  (equal :end (path loc)))

(defun zipper--up-right (loc)
  "Returns the loc that is up and to the right of LOC.

If no such loc exists, recurse until root. If root is reached,
return a special loc with indicator of end of depth-first walk."
  (let ((up (zipper-up loc)))
    (if up
        (or (zipper-right loc)
            (zipper--up-right up))
      (zipper--make-loc (node loc) :end))))

(defun zipper-next (loc)
  "Moves to the next loc in the hierarchy, depth-first. 

When reaching the end, returns a distinguished loc. 

If already at the end, stays there."
  (if (zipper-end-p loc)
      loc
    (or (and (zipper-branch-p loc)
             (zipper-down loc))
        (zipper-right loc)
        (zipper--up-right loc))))

(defun zipper--downmost-rightmost (loc)
  "Returns the loc that is as far down and to the right as
possible from LOC, or self."
  (let ((dloc (and (zipper-branch-p loc)
                   (zipper-down loc))))
    (if dloc
        (zipper--downmost-rightmost (zipper-rightmost dloc))
      loc)))

(defun zipper-prev (loc)
  "Moves to the previous loc in the hierarchy, depth-first.

If already at the root, return nil."
  (let ((lloc (zipper-left loc)))
    (if lloc
        (zipper--downmost-rightmost lloc)
      (zipper-up loc))))


;; Remove

(defun zipper-remove (loc)
  "Removes the node at LOC, returning the loc that would have
preceded it in a depth-first walk."
  (with-zipper-loc loc
    (if (zipper-top-p loc)
        (error "Remove at top")
      (if (> (length left) 0)
          (zipper--downmost-rightmost
           (zipper--make-loc-path (car left)
                                  (cdr left)
                                  ppath
                                  right))
        (zipper--make-loc right ppath)))))


(provide 'zipper)
;;; zipper.el ends here

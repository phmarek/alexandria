(in-package :alexandria-2)


(defun subseq* (sequence start &optional end)
  "Like SUBSEQ, but limits END to the length."
  (subseq sequence start
          (if end
              (min end (length sequence)))))


(defun map-permutations* (chunk list &key (sets 2) (min-length 0))
  "Splits LIST into all possible combinations of SETS sets
  with each at least MIN-LENGTH elements,
  calling CHUNK with the sets as &REST arguments.

  The order of the elements within the sets is undefined;
  CHUNK must not modify the incoming lists.

  This is a recursive function; stack usage is proportional to (LENGTH LIST).

  There will be no duplicate calls, ie. no (A B) and (B A) calls for sets A and B."
  (let ((split-up-sets (make-list sets)))
        (labels 
            ((rec (c cons)
               (if cons
                   (destructuring-bind (current . next) cons
                     ;; To avoid duplicate results (sets A B and B A),
                     ;; we require the first elements to be in the first few sets only
                     (let ((next-c (1+ c)))
                       (dotimes (i (min next-c sets))
                         (push current (elt split-up-sets i))
                         (rec next-c next)
                         (pop (elt split-up-sets i)))))
                   (let ((lengths (mapcar #'length split-up-sets)))
                     (when (or (= min-length 0)
                               (every (lambda (x) (>= x min-length))
                                      lengths))
                       ;; ... and that all empty sets must be at the end.
                       (let ((first-nil (position          0 lengths))
                             (last-arg  (position-if #'plusp lengths :from-end t)))
                         (when (> (or first-nil sets) last-arg)
                           (apply chunk split-up-sets))))))))
      (rec 0 list))))

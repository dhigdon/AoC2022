;;; Advent of Code 2022  Day 13 - Distress Signal

;; A macro for a "three-way short circuit evaluator"
(defmacro test (a b)
  `(ecase ,a
     (:accept :accept)
     (:reject :reject)
     (:continue ,b)))

(defun accept (left right)
  "Returns T if left and right are in the right order,
  and NIL otherwise. Right order-ness is determined by
  the rules below"
  (cond
    ;; Take care of null parameters
    ((null left) (if (null right) :continue :accept))
    ((null right) :reject)

    ;; Run the rules
    (t (let ((l (first left))
             (r (first right)))
         (test (cond ((and (listp l) (listp r))
                      (accept l r))

                     ((and (numberp l) (numberp r))
                      (cond ((= l r) :continue)
                            ((< l r) :accept)
                            ((> l r) :reject)))

                     ((and (listp l) (numberp r))
                      (accept l (list r)))

                     ((and (numberp l) (listp r))
                      (accept (list l) r)))

               ;; If the cond above continued,
               ;; then we attempt to accept the rest of the data
               (accept (rest left) (rest right)))))))

(defun accept-order (a b)
  "Ordering function for use with sort"
  (eq (accept a b) :accept))

(defun part1 (fname)
  "Solves part 1 - count indices of all passing pairs"
  (let ((lines (with-open-file (f (pathname fname))
                 (loop for left  = (read f nil :eof)
                       and right = (read f nil :eof)
                       and n upfrom 1
                       until (or (eq left :eof) (eq right :eof))
                       collect (list n (accept left right))))))
    (loop for r in lines
          when (eq (second r) :accept)
          summing (first r))))

(defun part1a (fname)
  "Solves part 1 - count indices of all passing pairs"
  ;; Version of part1 that does not need to accumulate lines.
  ;; It can solve the whole problem in one loop statement.
  (with-open-file (f (pathname fname))
    (loop for left = (read f nil :eof)
          and right = (read f nil :eof)
          and n upfrom 1
          until (or (eq left :eof) (eq right :eof))
          when (accept-order left right)
          summing n)))

(defparameter *start* '((2)))
(defparameter *finish* '((6)))

(defun part2 (fname)
  "Solves part 2 - sort with start/finish markers inserted,
  and multiply the final indices of start and finish markers"
  ;; Unlike part1a, we DO need to store the signal lines
  ;; so that we can sort them.
  (let* ((lines (with-open-file (f (pathname fname))
                  (loop for code = (read f nil :eof)
                        until (eq code :eof)
                        collect code)))

         ;; Add the start and finish tags
         (signal (append (list *start* *finish*) lines))

         ;; result contains a list of (<index> <line>) lists
         (result (loop for r in (sort signal #'accept-order) 
                       and n upfrom 1
                       collect (list n r)))

         ;; The default test works here because we use the 
         ;; same lists. Otherwise, we'd need #'equal
         (start  (find *start* result :key #'second))
         (finish (find *finish* result :key #'second)))

    ;; We should always find *start* and *finish*!
    (* (first start) (first finish))))

(defun part2a (fname)
  "Solves part 2. Find product of start and finish marker line numbers."
  ;; start-pos and finish-pos are the number of data lines
  ;; that occur before each "tag".
  ;; start-pos needs +1 to account for its own position
  ;; finish-pos needs +2 to account for it and *start*
  (with-open-file (f (pathname fname))
    (loop for line = (read f nil :eof)
          until (eq line :eof)
          counting (accept-order line *start*) into start-pos
          counting (accept-order line *finish*) into finish-pos
          finally (return (* (+ 1 start-pos)
                             (+ 2 finish-pos))))))


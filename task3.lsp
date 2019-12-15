(defun collection ()
    (list 1 0 0 0 1 0 1 1 1 0)
)

(defun one (item)
    (list 1 item)
)

(defun reducer(result item)
    (if (= (car(cdr(car result))) (car(cdr item)))
        (cons (list (+ (car (car result)) 1) (car(cdr item))) (cdr result))
        (cons item result)
    )
)

(defun expander_backer(item)
    (if (= (car item ) 1)
        (car (cdr item))
        item
    )
)

(defun compressor (lst)
    (map 'list #'expander_backer (cdr (reverse (reduce #'reducer (map 'list #'one lst) :initial-value (list (list 0 -9999))))))
)


(defun list-of (cnt item)
    (if (= cnt 0)
        `()
        (cons item (list-of (- cnt 1) item))
    )
)

(defun decredf (all i)
    (append 
        (if (consp i)
            (list-of (nth 0 i) (nth 1 i))
            (list i)
        )
        all
    )
)

(defun backer (lst)
    (reverse(reduce #'decredf lst :initial-value (list)))
)

(compressor (collection))
(backer (compressor (collection)))
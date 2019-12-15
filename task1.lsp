(defun collection ()
`(5,6,6,1,3,4,7,2,8,9))

(defun _insert (coll res value index current)
    (if (= current index)
        (append res (cons value coll ))
        (_insert 
            (cdr coll) 
            (reverse (cons (car coll) (reverse res)))
            value
            index
            (+ current 1)
        )  
    )
)

(defun insert (coll value index)
    (_insert coll `() value index 0)
)

(defun _del (coll res index current)
    (if (= current index)
        (append res (cdr coll ))
        (_del
            (cdr coll) 
            (reverse (cons (car coll) (reverse res)))
            index
            (+ current 1)
        )  
    )
)

(defun del (coll index)
    (_del coll `() index 0)
)

(defun ranger (coll value current)
    (if (= (car coll) value)
        current
        (ranger (cdr coll) value (+ current 1))
    )
)
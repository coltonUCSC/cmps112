(define (temp filename) filename)

(define (temp2 filename) (let ((x 5)) x) (let ((filename "wow\n")) filename))

(define (temp3 a b) (+ a b))

(define *function-table* (make-hash))
(define *label-table* (make-hash))
(define *variable-table* (make-hash))

(define (f-get key) 
	(hash-ref *function-table* key))
(define (f-set key value) 
	(hash-set! *function-table* key value))
(define (l-get key) 
	(hash-ref *label-table* key))
(define (l-set key value) 
	(hash-set! *label-table* key value))
(define (v-get key) 
	(hash-ref *variable-table* key))
(define (v-set key value) 
	(hash-set! *variable-table* key value))

(f-set "meme" "dank")
(f-set "dank" "meme")
(display (f-get "meme"))
(display (f-get "dank"))

(dim (val 100))

;(display (temp "meme\n"))
;(display (temp2 "meme2\n"))
;(define a 5)
;(set! a '(5 9))
;(define (testset arg arg2) (let (arg) ) 
;(= a (let ((a '(5 8))) a))
;(let ((a (5 8))) (list a))
;(display (let ((a '(5 8))) a))
;(display a)
;(display "\n")


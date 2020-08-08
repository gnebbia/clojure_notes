#-(and) "
   
P50 (***) Huffman code.

    First of all, consult a good book on discrete mathematics or
    algorithms for a detailed description of Huffman codes!
   
    We suppose a set of symbols with their frequencies, given as a
    list of fr(S,F) terms. Example:
    [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our
    objective is to construct a list hc(S,C) terms, where C is the
    Huffman code word for the symbol S. In our example, the result
    could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'),
    hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. The task shall
    be performed by the predicate huffman/2 defined as follows:
   
    % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs
"



#-(and) "

The algorithm described in:
\"A Method for the Construction of Minimum-Redundancy Codes\"
David A. Huffman, Procedings of the I.R.E.
<http://compression.ru/download/articles/huff/huffman_1952_minimum-redundancy-codes.pdf>
is:

compute huffman code:
  input:   message ensemble (set of (message, probability)).
           base D.
  output:  code ensemble (set of (message, code)).
  algorithm:

    1- sort the message ensemble by decreasing probability.

    2- N is the cardinal of the message ensemble (number of different
       messages).

    3- compute the integer n_0 such as 2<=n_0<=D and (N-n_0)/(D-1) is integer.

    4- select the n_0 least probable messages, and assign them each a
       digit code.

    5- substitute the selected messages by a composite message summing
       their probability, and re-order it.

    6- while there remains more than one message, do steps thru 8.

    7-    select D least probable messages, and assign them each a
          digit code.

    8-    substitute the selected messages by a composite message
          summing their probability, and re-order it.

    9- the code of each message is given by the concatenation of the
       code digits of the aggregate they've been put in.

"


(defun make-code (base type digits)
  "
Returns a STRING or VECTOR representing the code made of the
concatenation of the digits in the DIGITS list.

BASE
    is the base of the code.  When TYPE is STRING, BASE is limited to 36.

TYPE
    is either STRING or VECTOR, indicates the type of the codes.
    If BASE is two, then a bit-vector are used, otherwise vectors
    of unsigned-bytes.

DIGITS
    is a list of digits (integers between 0 and (1- BASE)).

"
  (check-type base integer)
  (ecase type
    ((string)
     (assert (<= 2 base 36) (base) "When TYPE is STRING, BASE must be between 2 and 36")
     (make-array (length digits)
                 :element-type 'base-char
                 :initial-contents (mapcar (function digit-char) digits)))
    ((vector)
     (make-array (length digits)
                 :element-type (if (= 2 base)
                                   'bit
                                   `(unsigned-byte ,base))
                 :initial-contents digits))))


(defstruct composite
  "
While building the Huffman codes, the symbols are gathered in a
composite symbol tree.  Each node maintains the code digit, the
frequency of all the messages its aggregates, and the list of its
subnodes.
"
  symbols frequency code)


(defun collect-codes (composite digits)
  "
Returns an a-list of all the leaf nodes in the composite tree, ie. the
symbols, with their code (reversed list of digits).
"
  (mapcan (lambda (symbol)
            (if (composite-p (first (composite-symbols symbol)))
                (collect-codes symbol (cons (composite-code symbol) digits))
                (list (cons (first (composite-symbols symbol))
                            (cons (composite-code symbol) digits)))))
          (composite-symbols composite)))


(defun huffman (symbols &key (base 2) (type 'string))
  "
Builds the Huffman code ensemble for the given message ensemble.
Returns an a-list (symbol . code).

SYMBOLS
    is an a-list (symbol . frequency)

BASE
    is the base of the code.  When TYPE is STRING, BASE is limited to 36.

TYPE
    is either STRING or VECTOR, indicates the type of the codes.
    If BASE is two, then a bit-vector are used, otherwise vectors
    of unsigned-bytes.

"
  (let* ((symbols (sort (mapcar (lambda (symbol)
                                  (make-composite :symbols (list (car symbol))
                                                  :frequency (cdr symbol)))
                                symbols)
                        (function <) :key (function composite-frequency)))
         (n       (length symbols))
         (n0      (if (= 2 base)
                      2
                      ;; (n-n0)/(d-1)  is integer <=> (r-n0)/(d-1) is integer
                      ;; with r being the remainder of n / (d-1).
                      (let ((r (mod n (1- base))))
                        (case r
                          (0         (1- base))
                          (1         base)
                          (otherwise r))))))
    (assert (zerop (mod (- n n0) (1- base))))
    (if (<= n base)
        (loop  ; Less than base symbols, we just assign all of them a single-digit code.
           :for symbol :in selection
           :for code :from 0
           :collect (cons (first (composite-symbols symbol))
                          (make-code base type (list code))))
        (loop ; more than BASE symbols. Let's apply Huffman's algorithm.
           :for selection-size = n0 :then base
           :for selection = (subseq symbols 0 selection-size)
           :for composite = (make-composite :symbols selection
                                            :frequency (reduce (function +) selection
                                                               :key (function composite-frequency)))
           :do (progn
                 (loop                  ; codify the selection.
                    :for symbol :in selection
                    :for code :from (1- (length selection)) :downto 0
                                        ; to keep the same numbering as in Huffman's paper.
                                        ; We could as well just use  :for code :from 0
                    :do (setf (composite-code symbol) code))
                 (setf symbols (merge 'list (list composite) (nthcdr selection-size symbols)
                                      (function <) :key (function composite-frequency))))
           :while (rest symbols)
           :finally (return (mapcar (lambda (symbol)
                                      (cons (car symbol) (make-code base type (reverse (cdr symbol)))))
                                    (collect-codes (first symbols) '())))))))


;; (huffman '((a . 45) (b . 13) (c . 12) (d . 16) (e . 9) (f . 5)))

;; --> ((A . "1") (C . "011") (B . "010") (F . "0011") (E . "0010") (D . "000"))
;; 
;; (huffman '((a . 45) (b . 13) (c . 12) (d . 16) (e . 9) (f . 5)) :type 'vector)
;; --> ((A . #*1) (C . #*011) (B . #*010) (F . #*0011) (E . #*0010) (D . #*000))
;;
;; (huffman '((a . 45) (b . 13) (c . 12) (d . 16) (e . 9) (f . 5)) :type 'string :base 3)
;; --> ((D . "2") (C . "12") (B . "11") (F . "101") (E . "100") (A . "0"))


(defparameter *example-1*
  '((1 . 0.20)
    (2 . 0.18)
    (3 . 0.10)
    (4 . 0.10)
    (5 . 0.10)
    (6 . 0.06)
    (7 . 0.06)
    (8 . 0.04)
    (9 . 0.04)
    (10 . 0.04)
    (11 . 0.04)
    (12 . 0.03)
    (13 . 0.01)))

;; (map nil (function print)
;;      (sort (huffman *example-1* :base 2 :type 'string)
;;            (function <) :key (function car)))
;; ( 1 . "10") 
;; ( 2 . "000") 
;; ( 3 . "111") 
;; ( 4 . "110") 
;; ( 5 . "011") 
;; ( 6 . "00100") ; 6 and 7 have the same probability, so this result is valid,
;; ( 7 . "0101")  ; it depends on the way sort orders them. 
;; ( 8 . "00110") 
;; ( 9 . "01001") 
;; (10 . "01000") 
;; (11 . "00101") 
;; (12 . "001110") 
;; (13 . "001111") 


(defparameter *example-2*
  '((1 . 0.22)
    (2 . 0.20)
    (3 . 0.18)
    (4 . 0.15)
    (5 . 0.10)
    (6 . 0.08)
    (7 . 0.05)
    (8 . 0.02)))

;; (map nil (function print)
;;      (sort (huffman *example-2* :base 4 :type 'string)
;;            (function <) :key (function car)))
;; (1 . "1") 
;; (2 . "2") 
;; (3 . "3") 
;; (4 . "00") 
;; (5 . "01") 
;; (6 . "02") 
;; (7 . "030") 
;; (8 . "031") 

;;;; THE END ;;;;

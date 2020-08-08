#-(and) "

P16 (**) Drop every N'th element from a list.
    Example:
    * (drop '(a b c d e f g h i k) 3)
    (A B D E G H K)
"



;; From: Dan Becker <db19606@gmail.com>
;; Subject: list looping
;; Newsgroups: comp.lang.lisp
;; Date: Tue, 21 Dec 2010 14:06:58 -0800 (PST)
;; Organization: http://groups.google.com
;; Message-ID: <9d846d29-3d63-4037-9410-86fdcabc8e93@i18g2000yqn.googlegroups.com>
;; 
;; For exercise I'm running through the problems from here:
;; 
;; http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
;; 
;; For some of these, I'm wanting to write code that loops through a list
;; in parallel with updating some other variable, so I end up with
;; functions like this:

(defun drop (list n)
  "returns result of dropping every nth element of list"
  (do ((l list (cdr l))
       (i 1 (1+ i))
       (result nil))
      ((null l) (nreverse result))
    (unless (= (mod i n) 0)
      (push (car l) result))))

;; I know there's also an approach using LOOP that I think I like better:

(defun drop (list n)
  "returns result of dropping every nth element of list"
  (loop
     for elem in list
     for i from 1
       unless (= (mod i n) 0)
       collect elem))



;; From: tar@sevak.isi.edu (Thomas A. Russ)
;; Subject: Re: list looping
;; Newsgroups: comp.lang.lisp
;; Date: 21 Dec 2010 18:26:13 -0800
;; Organization: USC Information Sciences Institute
;; Message-ID: <ymifwtq4k6y.fsf@blackcat.isi.edu>
;; 
;; Another choice that doesn't use MOD:

(defun drop (list n)
  (let ((result nil))
     (loop while list
	   do (loop repeat (1- n)
		    while list
                    do (push (pop list) result))
	      (pop list))
     (nreverse result)))


;; This would be cleaner if one knew that the list length was a multiple of
;; the drop count.
;; -- 
;; Thomas A. Russ,  USC/Information Sciences Institute



;; From: Ariel Badichi <vermilionrush@gmail.com>
;; Subject: Re: list looping
;; Newsgroups: comp.lang.lisp
;; Date: Wed, 22 Dec 2010 05:13:05 +0200
;; Organization: A noiseless patient Spider
;; Message-ID: <87ei9ah54u.fsf@gmail.com>
;;
;; Here's a "cute" attempt:

(defun drop (sequence n &aux (i 0))
  "Remove every Nth element of SEQUENCE."
  (remove-if (lambda (item) (divisible-by-p (incf i) n)) sequence))

(defun divisible-by-p (m n)
  "Return true if M is divisible by N, and false otherwise."
  (zerop (mod m n)))

;; The issues with this DROP are that it (i) assumes REMOVE-IF calls the
;; predicate with elements in order, which I'm not sure is guaranteed by
;; the Standard (ii) has a side-effecting predicate (iii) does not declare
;; ITEM as ignored (iv) uses &AUX.
;; 
;; Ariel


;; From: kenny <kentilton@gmail.com>
;; Subject: Re: list looping
;; Newsgroups: comp.lang.lisp
;; Date: Wed, 22 Dec 2010 02:34:41 -0800 (PST)
;; Organization: http://groups.google.com
;; Message-ID: <3ffa4a93-ed93-4eab-b36d-6ac69aa52fad@w17g2000yqh.googlegroups.com>
;;
;; Oh, we can get that with the loop version;

(defun drop (list n)
 (loop
    :for elt :in list
    :for i :from 1
    :for die-elt-die! = (zerop (mod i n))
    :unless die-elt-die!
    :collect elt))

;; out-out-damned-elt! would work as well.
;; 
;; hth,
;;
;; kt



;; From: Pascal Costanza <pc@p-cos.net>
;; Subject: Re: list looping
;; Newsgroups: comp.lang.lisp
;; Date: Wed, 22 Dec 2010 15:56:47 +0100
;; Message-ID: <8nehtfFgrpU1@mid.individual.net>
;; 
;; Here is my suggestion:

(defun drop (list n &aux (m (1- n)))
  (loop
     :for a = list :then (cdr b)
     :for b = (nthcdr m a)
     :if b :nconc (ldiff a b)
     :else :nconc a :and :do (loop-finish)))


;; I tend to use &aux when I need a variable that is only a 'minor
;; variation' of one or more of the parameters (where 'minor' is a highly
;; subjective judgment).
;; 
;; 
;; Pascal
;; 
;; -- 
;; My website: http://p-cos.net
;; Common Lisp Document Repository: http://cdr.eurolisp.org
;; Closer to MOP & ContextL: http://common-lisp.net/project/closer/


#|

From: Daniel Torrido <danieltorridoverdeu@gmail.com>
Subject: Re: Exercises please
Newsgroups: comp.lang.lisp
Date: Tue, 27 Jan 2015 21:02:57 +0100 (7 hours, 52 minutes, 56 seconds ago)
Organization: None (http://example.com)
Message-ID: <87k308dma6.fsf@example.com>

Helmut Jarausch <hjarausch@gmail.com> writes:

> Am Dienstag, 27. Januar 2015 15:40:06 UTC+1 schrieb informatimago:
> Many thanks Pascal, that is exactly what I was looking for.
>
>> and my solutions (still incomplete): 
>> http://www.informatimago.com/develop/lisp/l99/index.html
> Some links are broken, e.g., p91-p99.
> Is that what you mean by "incomplete"?

 About p91: P91 (**) Knight's tour
    Another famous problem is this one: How can a knight jump on an NxN
    chessboard in such a way that it visits every square exactly once?

I seem to recall that someone proved that the following heuristic solves the
problem, move the knight always to the square that has fewer neigbours,
a neigbour of a square is a square that is not visited and that you can
move to it in one move. That algorithms don't use backtracking. The
origin of the 99 problems is for teaching programming in Prolog where
backtracking allow you to formulate solution to problems easily. 

|#


;; therefore, let's implement both algorithms.

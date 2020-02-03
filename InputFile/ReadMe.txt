/////////ESECUZIONI PER LISP


;; Mettere length = 5
(execution-loop ’(state :acc 0 :pc 0 :mem (100 902 100 902 0 0) :in () :out () :flag noflag))	
Error: Invalid memory length


;; Mettere length = 5
(execution-loop ’(state :acc 0 :pc 0 :mem (100 902 100 902 0) :in () :out () :flag noflag))	
(100 200)


;; Mettere length = 9
(execution-loop ’(state :acc 0 :pc 0 :mem (101 202 902 102 103 104 105 902 0) :in () :out () :flag noflag))	
(300 511)


;; Mettere length = 100
(execution-loop ’(state :acc 0 :pc 0 :mem (678 902 754 189 271 153 208 887 125 239 130 381 327 901 113 139 901 256 575 570 151 291 392 218 202 242 901 234 177 535 902 155 668 161 902 315 901 264 901 741 356 164 278 106 902 901 377 177 607 865 385 166 902 901 549 545 902 901 901 862 324 901 858 901 902 745 242 901 338 897 306 902 635 212 901 294 322 253 901 103 902 126 289 187 902 184 902 657 901 257 144 901 176 901 712 646 193 901 601 274) :in (5 2 6 7 8) :out () :flag noflag))
Error: Input list is empty


;; Mettere length = 100
(execution-loop ’(state :acc 0 :pc 0 :mem (901 370 901 371 901 372 901 373 901 374 578 375 571 270 823 577 375 570 376 571 370 576 371 572 271 834 577 375 571 376 572 371 576 372 573 272 845 577 375 572 376 573 372 576 373 574 273 856 577 375 573 376 574 373 576 374 575 759 610 570 902 571 902 572 902 573 902 574 902 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) :in (5 2 6 7 8) :out () :flag noflag))
(2 5 6 7 8)


------------------- LMC.lisp --------------------

(lmc-run "C:\\Users\\riccardo\\Desktop\\TestProgetto\\exec.lmc" '(901 902 705 600 0 4 5 6 7 8 9 0))
The Output list is: 
(4 5 6 7 8 9 0)


(lmc-run "C:\\Users\\riccardo\\Desktop\\TestProgetto\\reverse.lmc" '(1 2 3 4 5 6 0 1 2))
The Output list is: 
(6 5 4 3 2 1)


(lmc-run "C:\\Users\\riccardo\\Desktop\\TestProgetto\\multiplication.lmc" '(10 4))
The Output list is: 
(40)


(lmc-run "C:\\Users\\riccardo\\Desktop\\TestProgetto\\test1.txt" '(10 4 37 24 26))
The Output list is: 
(4 10 24 26 37)


(lmc-run "C:\\Users\\riccardo\\Desktop\\TestProgetto\\test2.txt" '(10))
The Output list is: 
(10 9 8 7 6 5 4 3 2 1 0)


(lmc-run "C:\\Users\\riccardo\\Desktop\\TestProgetto\\test3.txt" '(100 20))
The Output list is: 
(5)


(lmc-run "C:\\Users\\riccardo\\Desktop\\TestProgetto\\test4.txt" '(20))
The Output list is: 
(400)


(lmc-run "C:\\Users\\riccardo\\Desktop\\TestProgetto\\test5.txt" '(9))
The Output list is: 
(0 1 1 2 3 5 8)


(lmc-run "C:\\Users\\riccardo\\Desktop\\TestProgetto\\test6.txt" '())
The Output list is: 
NIL
#| (STATE :ACC 0 :PC 1 :MEM (132 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ...) :IN NIL :OUT NIL :FLAG NOFLAG)
(HALTED-STATE :ACC 0 :PC 1 :MEM (132 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ...) :IN NIL :OUT NIL :FLAG NOFLAG) |#


(lmc-run "C:\\Users\\riccardo\\Desktop\\TestProgetto\\TriangularNumber.txt" '(15))
The Output list is: 
(5)
(lmc-run "C:\\Users\\riccardo\\Desktop\\TestProgetto\\TriangularNumber.txt" '(7))
The Output list is: 
(0)


(lmc-run "C:\\Users\\riccardo\\Desktop\\TestProgetto\\looping.lmc" '())
The Output list is: 
;; Lista di 10000 elementi (numeri da 0 a 99 per 100 volte)
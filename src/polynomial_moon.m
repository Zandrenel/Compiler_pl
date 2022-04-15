   align   
   entry   
   j   scope24
stop   hlt   
scope19       %,Function,"evaluate",Scope,"POLYNOMIAL"

   jr   r15
scope20       %,Function,"evaluate",Scope,"QUADRATIC"

   jr   r15
scope21       %,Function,"build",Scope,"QUADRATIC"

   jr   r15
scope22       %,Function,"build",Scope,"LINEAR"



   jr   r15
scope23       %,Function,"evaluate",Scope,"LINEAR"

   jr   r15
scope24       %,Free Function,"main"


   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symbol56(r0),r1
loop7       


   sub   r1,r1,r1
   addi   r1,r1,10
   sw   symb151(r0),r1
   lw   r1,symbol56(r0)
   lw   r2,symb151(r0)
   cle   r3,r1,r2
   sw   symb150(r0),r3
   lw   r1,symb150(r0)
   bnz   r1,while7

   lw   r1,symbol56(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1



   lw   r1,symbol56(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1


   j   loop7
while7       
   j   stop
buf   res   20
symbol47   res   8
symbol48   res   8
symb146   res   4
symb147   res   4
symb148   res   4
symb149   res   4
symbol49   res   40
symbol50   res   8
symbol51   res   8
symbol52   res   16
symbol53   res   8
symbol54   res   16
symbol55   res   40
symbol56   res   4
symb151   res   4
symb150   res   4

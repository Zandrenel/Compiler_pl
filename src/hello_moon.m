   align   
   entry   
   j   scope3
stop   hlt   
scope3       %,Free Function,"main"

   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb59(r0),r1
   sub   r1,r1,r1
   addi   r1,r1,4
   sw   symb60(r0),r1
   lw   r1,symb59(r0)
   lw   r2,symb60(r0)
   add   r3,r1,r2
   sw   symb58(r0),r3
   lw   r1,symb58(r0)
   sw   symbol15(r0),r1

   lw   r1,symbol15(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1
   j   stop
buf   res   20
symb59   res   4
symb60   res   4
symb58   res   4
symbol15   res   4

   align   
   entry   
   j   scope2
stop   hlt   
scope2       %,Function Label,"main"

   sub   r1,r1,r1
   addi   r1,r1,8
   sw   symbol4(r0),r1

   lw   r1,symbol4(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1




   sub   r1,r1,r1
   addi   r1,r1,10
   sw   symb13(r0),r1
   lw   r1,symbol5(r0)
   lw   r2,symb13(r0)
   add   r3,r1,r2
   sw   symb12(r0),r3
   lw   r1,symbol4(r0)
   lw   r2,symb12(r0)
   cgt   r3,r1,r2
   sw   symb14(r0),r3
   lw   r1,symb14(r0)
   jl r15,putint
   bnz   r1,if4
   j   if6
if4        


   sub   r1,r1,r1
   addi   r1,r1,10
   sw   symb16(r0),r1
   lw   r1,symbol4(r0)
   lw   r2,symb16(r0)
   add   r3,r1,r2
   sw   symb15(r0),r3
   lw   r1,symb15(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1
   j   if6
if5        


   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb18(r0),r1
   lw   r1,symbol4(r0)
   lw   r2,symb18(r0)
   add   r3,r1,r2
   sw   symb17(r0),r3
   lw   r1,symb17(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1
if6        

   sub   r1,r1,r1
   addi   r1,r1,0
   sw   symbol6(r0),r1
loop2       


   sub   r1,r1,r1
   addi   r1,r1,10
   sw   symb20(r0),r1
   lw   r1,symbol6(r0)
   lw   r2,symb20(r0)
   cle   r3,r1,r2
   sw   symb19(r0),r3
   lw   r1,symb19(r0)
   bnz   r1,while2

   lw   r1,symbol6(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1


   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb22(r0),r1
   lw   r1,symbol6(r0)
   lw   r2,symb22(r0)
   add   r3,r1,r2
   sw   symb21(r0),r3
   lw   r1,symb21(r0)
   sw   symbol6(r0),r1

   lw   r1,symbol6(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1


   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb22(r0),r1
   lw   r1,symbol6(r0)
   lw   r2,symb22(r0)
   add   r3,r1,r2
   sw   symb21(r0),r3
   lw   r1,symb21(r0)
   sw   symbol6(r0),r1
   j   loop2
while2       
   j   stop
buf   res   20
symbol4   res   4
symbol5   res   4
symb13   res   4
symb12   res   4
symb14   res   4
symb16   res   4
symb15   res   4
symb18   res   4
symb17   res   4
symbol6   res   4
symb20   res   4
symb19   res   4
symb22   res   4
symb21   res   4

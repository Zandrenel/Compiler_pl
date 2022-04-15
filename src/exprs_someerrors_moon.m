   align   
   entry   
   j   scope15
stop   hlt   
scope15       %,Free Function,"main"

   sub   r1,r1,r1
   addi   r1,r1,21
   sw   symbol40(r0),r1


   sub   r1,r1,r1
   addi   r1,r1,4
   sw   symb125(r0),r1
   lw   r1,symbol40(r0)
   lw   r2,symb125(r0)
   add   r3,r1,r2
   sw   symb124(r0),r3
   lw   r1,symb124(r0)
   sw   symbol41(r0),r1


   sub   r1,r1,r1
   addi   r1,r1,4
   sw   symb127(r0),r1
   lw   r1,symbol40(r0)
   lw   r2,symb127(r0)
   add   r3,r1,r2
   sw   symb126(r0),r3
   lw   r1,symb126(r0)
   sw   symbol42(r0),r1



   sub   r1,r1,r1
   addi   r1,r1,3
   sw   symb132(r0),r1

   sub   r1,r1,r1
   addi   r1,r1,2
   sw   symb129(r0),r1
   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb130(r0),r1
   lw   r1,symb129(r0)
   lw   r2,symb130(r0)
   sub   r3,r1,r2
   sw   symb128(r0),r3
   lw   r1,symb132(r0)
   lw   r2,symb128(r0)
   sub   r3,r1,r2
   sw   symb131(r0),r3
   lw   r1,symbol40(r0)
   lw   r2,symb131(r0)
   add   r3,r1,r2
   sw   symb133(r0),r3
   lw   r1,symb133(r0)
   sw   symbol43(r0),r1



   lw   r1,symbol44(r0)
   lw   r2,symbol44(r0)
   add   r3,r1,r2
   sw   symb134(r0),r3
   lw   r1,symb134(r0)
   sw   symbol45(r0),r1

   lw   r1,symbol46(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1

   lw   r1,symbol44(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1

   lw   r1,symbol45(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1

   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb136(r0),r1
   sub   r1,r1,r1
   addi   r1,r1,3
   sw   symb137(r0),r1
   lw   r1,symb136(r0)
   lw   r2,symb137(r0)
   ceq   r3,r1,r2
   sw   symb135(r0),r3
   lw   r1,symb135(r0)
   bnz   r1,if13
   j   if15
if13        

   lw   r1,symbol45(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1
   j   if15
if14        
if15        
loop6       


   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb139(r0),r1
   sub   r1,r1,r1
   addi   r1,r1,3
   sw   symb140(r0),r1
   lw   r1,symb139(r0)
   lw   r2,symb140(r0)
   add   r3,r1,r2
   sw   symb138(r0),r3

   lw   r1,symb138(r0)
   lw   r2,symbol40(r0)
   cge   r3,r1,r2
   sw   symb141(r0),r3
   lw   r1,symb141(r0)
   bnz   r1,while6

   lw   r1,symbol40(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1


   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb143(r0),r1
   lw   r1,symbol40(r0)
   lw   r2,symb143(r0)
   sub   r3,r1,r2
   sw   symb142(r0),r3
   lw   r1,symb142(r0)
   sw   symbol40(r0),r1

   lw   r1,symbol40(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1


   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb143(r0),r1
   lw   r1,symbol40(r0)
   lw   r2,symb143(r0)
   sub   r3,r1,r2
   sw   symb142(r0),r3
   lw   r1,symb142(r0)
   sw   symbol40(r0),r1
   j   loop6
while6       


   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb145(r0),r1
   lw   r1,symbol45(r0)
   lw   r2,symb145(r0)
   add   r3,r1,r2
   sw   symb144(r0),r3
   lw   r1,symb144(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1
   j   stop
buf   res   20
symbol40   res   4
symb125   res   4
symb124   res   4
symbol41   res   4
symb127   res   4
symb126   res   4
symbol42   res   4
symb129   res   4
symb130   res   4
symb128   res   4
symb132   res   4
symb131   res   4
symb133   res   4
symbol43   res   4
symbol44   res   4
symb134   res   4
symbol45   res   4
symbol46   res   4
symb136   res   4
symb137   res   4
symb135   res   4
symb139   res   4
symb140   res   4
symb138   res   4
symb141   res   4
symb143   res   4
symb142   res   4
symb145   res   4
symb144   res   4

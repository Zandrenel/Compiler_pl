   align   
   entry   
   j   scope4
stop   hlt   
scope4       %,Free Function,"main"

   sub   r1,r1,r1
   addi   r1,r1,21
   sw   symbol16(r0),r1


   sub   r1,r1,r1
   addi   r1,r1,4
   sw   symb62(r0),r1
   lw   r1,symbol16(r0)
   lw   r2,symb62(r0)
   add   r3,r1,r2
   sw   symb61(r0),r3
   lw   r1,symb61(r0)
   sw   symbol17(r0),r1


   sub   r1,r1,r1
   addi   r1,r1,4
   sw   symb64(r0),r1
   lw   r1,symbol16(r0)
   lw   r2,symb64(r0)
   add   r3,r1,r2
   sw   symb63(r0),r3
   lw   r1,symb63(r0)
   sw   symbol18(r0),r1



   sub   r1,r1,r1
   addi   r1,r1,4
   sw   symb66(r0),r1
   lw   r1,symbol16(r0)
   lw   r2,symb66(r0)
   add   r3,r1,r2
   sw   symb65(r0),r3
   sub   r1,r1,r1
   addi   r1,r1,2
   sw   symb68(r0),r1
   lw   r1,symb65(r0)
   lw   r2,symb68(r0)
   mul   r3,r1,r2
   sw   symb67(r0),r3
   lw   r1,symb67(r0)
   sw   symbol19(r0),r1



   sub   r1,r1,r1
   addi   r1,r1,3
   sw   symb73(r0),r1

   sub   r1,r1,r1
   addi   r1,r1,2
   sw   symb70(r0),r1
   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb71(r0),r1
   lw   r1,symb70(r0)
   lw   r2,symb71(r0)
   sub   r3,r1,r2
   sw   symb69(r0),r3
   lw   r1,symb73(r0)
   lw   r2,symb69(r0)
   sub   r3,r1,r2
   sw   symb72(r0),r3
   lw   r1,symbol16(r0)
   lw   r2,symb72(r0)
   add   r3,r1,r2
   sw   symb74(r0),r3
   lw   r1,symb74(r0)
   sw   symbol20(r0),r1



   lw   r1,symbol19(r0)
   lw   r2,symbol19(r0)
   add   r3,r1,r2
   sw   symb75(r0),r3
   lw   r1,symb75(r0)
   sw   symbol21(r0),r1


   sub   r1,r1,r1
   addi   r1,r1,7
   sw   symb77(r0),r1
   sub   r1,r1,r1
   addi   r1,r1,2
   sw   symb78(r0),r1
   lw   r1,symb77(r0)
   lw   r2,symb78(r0)
   sub   r3,r1,r2
   sw   symb76(r0),r3

   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb83(r0),r1

   sub   r1,r1,r1
   addi   r1,r1,2
   sw   symb80(r0),r1
   sub   r1,r1,r1
   addi   r1,r1,9
   sw   symb81(r0),r1
   lw   r1,symb80(r0)
   lw   r2,symb81(r0)
   mul   r3,r1,r2
   sw   symb79(r0),r3
   lw   r1,symb83(r0)
   lw   r2,symb79(r0)
   sub   r3,r1,r2
   sw   symb82(r0),r3
   lw   r1,symb76(r0)
   lw   r2,symb82(r0)
   mul   r3,r1,r2
   sw   symb84(r0),r3
   lw   r1,symb84(r0)
   sw   symbol22(r0),r1

   lw   r1,symbol22(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1

   lw   r1,symbol19(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1

   lw   r1,symbol21(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1

   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb86(r0),r1
   sub   r1,r1,r1
   addi   r1,r1,3
   sw   symb87(r0),r1
   lw   r1,symb86(r0)
   lw   r2,symb87(r0)
   ceq   r3,r1,r2
   sw   symb85(r0),r3
   lw   r1,symb85(r0)
   bnz   r1,if7
   j   if9
if7        

   lw   r1,symbol21(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1
   j   if9
if8        
if9        
loop3       


   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb89(r0),r1
   sub   r1,r1,r1
   addi   r1,r1,3
   sw   symb90(r0),r1
   lw   r1,symb89(r0)
   lw   r2,symb90(r0)
   add   r3,r1,r2
   sw   symb88(r0),r3

   lw   r1,symb88(r0)
   lw   r2,symbol16(r0)
   cge   r3,r1,r2
   sw   symb91(r0),r3
   lw   r1,symb91(r0)
   bnz   r1,while3

   lw   r1,symbol16(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1


   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb93(r0),r1
   lw   r1,symbol16(r0)
   lw   r2,symb93(r0)
   sub   r3,r1,r2
   sw   symb92(r0),r3
   lw   r1,symb92(r0)
   sw   symbol16(r0),r1

   lw   r1,symbol16(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1


   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb93(r0),r1
   lw   r1,symbol16(r0)
   lw   r2,symb93(r0)
   sub   r3,r1,r2
   sw   symb92(r0),r3
   lw   r1,symb92(r0)
   sw   symbol16(r0),r1
   j   loop3
while3       


   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb95(r0),r1
   lw   r1,symbol21(r0)
   lw   r2,symb95(r0)
   add   r3,r1,r2
   sw   symb94(r0),r3
   lw   r1,symb94(r0)
   jl   r15,putint
   sub   r1,r1,r1
   addi   r1,r1,10
   putc   r1
   j   stop
buf   res   20
symbol16   res   4
symb62   res   4
symb61   res   4
symbol17   res   4
symb64   res   4
symb63   res   4
symbol18   res   4
symb66   res   4
symb65   res   4
symb68   res   4
symb67   res   4
symbol19   res   4
symb70   res   4
symb71   res   4
symb69   res   4
symb73   res   4
symb72   res   4
symb74   res   4
symbol20   res   4
symb75   res   4
symbol21   res   4
symb77   res   4
symb78   res   4
symb76   res   4
symb80   res   4
symb81   res   4
symb79   res   4
symb83   res   4
symb82   res   4
symb84   res   4
symbol22   res   4
symb86   res   4
symb87   res   4
symb85   res   4
symb89   res   4
symb90   res   4
symb88   res   4
symb91   res   4
symb93   res   4
symb92   res   4
symb95   res   4
symb94   res   4

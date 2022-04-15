   align   
   entry   
   j   scope6
stop   hlt   
scope6       

   sub   r1,r1,r1
   addi   r1,r1,21
   sw   symbol24(r0),r1


   sub   r1,r1,r1
   addi   r1,r1,4
   sw   symb294(r0),r1
   lw   r1,symbol24(r0)
   lw   r2,symb294(r0)
   add   r3,r1,r2
   sw   symb293(r0),r3
   lw   r1,symb293(r0)
   sw   symbol25(r0),r1

   lw   r1,symbol25(r0)
   jl   r15,putint


   sub   r1,r1,r1
   addi   r1,r1,4
   sw   symb297(r0),r1
   lw   r1,symbol24(r0)
   lw   r2,symb297(r0)
   add   r3,r1,r2
   sw   symb296(r0),r3
   lw   r1,symb296(r0)
   sw   symbol26(r0),r1



   sub   r1,r1,r1
   addi   r1,r1,3
   sw   symb304(r0),r1

   sub   r1,r1,r1
   addi   r1,r1,2
   sw   symb300(r0),r1
   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb301(r0),r1
   lw   r1,symb300(r0)
   lw   r2,symb301(r0)
   add   r3,r1,r2
   sw   symb299(r0),r3
   lw   r1,symb304(r0)
   lw   r2,symb299(r0)
   add   r3,r1,r2
   sw   symb303(r0),r3
   lw   r1,symbol24(r0)
   lw   r2,symb303(r0)
   add   r3,r1,r2
   sw   symb306(r0),r3
   lw   r1,symb306(r0)
   sw   symbol27(r0),r1



   lw   r1,symbol28(r0)
   lw   r2,symbol28(r0)
   add   r3,r1,r2
   sw   symb308(r0),r3
   lw   r1,symb308(r0)
   sw   symbol29(r0),r1


   sub   r1,r1,r1
   addi   r1,r1,7
   sw   symb311(r0),r1
   sub   r1,r1,r1
   addi   r1,r1,2
   sw   symb312(r0),r1
   lw   r1,symb311(r0)
   lw   r2,symb312(r0)
   add   r3,r1,r2
   sw   symb310(r0),r3

   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb318(r0),r1

   sub   r1,r1,r1
   addi   r1,r1,2
   sw   symb314(r0),r1
   sub   r1,r1,r1
   addi   r1,r1,9
   sw   symb315(r0),r1
   lw   r1,symb314(r0)
   lw   r2,symb315(r0)
   add   r3,r1,r2
   sw   symb313(r0),r3
   lw   r1,symb318(r0)
   lw   r2,symb313(r0)
   add   r3,r1,r2
   sw   symb317(r0),r3
   lw   r1,symb310(r0)
   lw   r2,symb317(r0)
   add   r3,r1,r2
   sw   symb319(r0),r3
   lw   r1,symb319(r0)
   sw   symbol30(r0),r1

   lw   r1,symbol29(r0)
   jl   r15,putint

   lw   r1,symbol24(r0)
   jl   r15,putint


   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symb348(r0),r1
   lw   r1,symbol24(r0)
   lw   r2,symb348(r0)
   add   r3,r1,r2
   sw   symb347(r0),r3
   lw   r1,symb347(r0)
   sw   symbol24(r0),r1
   j   stop
buf   res   20
symbol24   res   4
symb294   res   4
symb293   res   4
symbol25   res   4
symb297   res   4
symb296   res   4
symbol26   res   4
symb300   res   4
symb301   res   4
symb299   res   4
symb304   res   4
symb303   res   4
symb306   res   4
symbol27   res   4
symbol28   res   4
symb308   res   4
symbol29   res   4
symb311   res   4
symb312   res   4
symb310   res   4
symb314   res   4
symb315   res   4
symb313   res   4
symb318   res   4
symb317   res   4
symb319   res   4
symbol30   res   4
symb321   res   4
symb322   res   4
symb320   res   4
symb325   res   4
symb326   res   4
symb324   res   4
symb327   res   4
symb330   res   4
symb329   res   4
symb333   res   4
symb332   res   4
symb336   res   4
symb335   res   4
symb339   res   4
symb338   res   4
symb342   res   4
symb341   res   4
symb345   res   4
symb344   res   4
symb348   res   4
symb347   res   4

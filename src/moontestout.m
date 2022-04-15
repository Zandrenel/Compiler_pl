   align   
   entry   
   j   scope11
stop   hlt   
scope6       

scope7       

scope8       

scope9       



scope10       

scope11       


   sub   r1,r1,r1
   addi   r1,r1,1
   sw   symbol16(r0),r1

   lw   r1,symbol16(r0)
   jl   r15,putint


   j   stop
buf   res   20
symbol7   res   8
symbol8   res   8
symb119   res   4
symb120   res   4
symb121   res   4
symb122   res   4
symbol9   res   40
symbol10   res   8
symbol11   res   8
symbol12   res   16
symbol13   res   8
symbol14   res   16
symbol15   res   40
symbol16   res   4
symb124   res   4
symb123   res   4

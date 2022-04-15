align
entry   
addi r14,r0,topaddr
j main
stop   hlt
% each expression to assign a variable

main
sub r1,r1,r1
addi r1,r1,3
sw a(r0),r1

% assign b = 9
sub r1,r1,r1
addi r1,r1,9
sw b(r0),r1

% assign c = 3
sub r1,r1,r1
addi r1,r1,3
sw c(r0),r1

% mult statement
lw r1,b(r0)
lw r2,c(r0)
mul r3,r1,r2 
sw t8(r0),r3



% add satement
   lw r1,a(r0)
   lw r2,t8(r0) 
   add r3,r1,r2 
   sw t9(r0),r3

   % write statement
   lw r1,t9(r0)
   jl r15,putint
   
   j stop
   % end of main

   % reserve all variable space at one spot at the end
   
   a res 4
   t8 res 4
   t9 res 4
   c res 4
   b res 4
   d res 4
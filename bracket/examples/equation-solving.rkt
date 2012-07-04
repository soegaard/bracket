#lang bracket
DeclareVars(x,a,b,c);

Solve(2*x=1,x);                   % x = 1/2
Substitute(2*x=1,Solve(2*x=1,x)); % 1=1 

Solve(2*x+3=1,x); % x = -1

Solve(a*x+b=c,x); % x = (c-b)/a

solution:=Solve(a*x+b+x=c,x);
solution;
% Check solution
Expand(Substitute(a*x+b+x,solution)); % This actually simplifies to c
% ... unfortunately not automatically.



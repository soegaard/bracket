#lang bracket
% Compute sqrt(a) using Newton iteration.
a:=9;
% The function f has sqrt(a) as root.
f(x):=x^2-a;
% The deriviative f'(x) is needed.
df(x):=2*x;
% Given a number x close to sqrt(a) compute a number even closer.
next(x):=x-f(x)/df(x);
% Iterate and save temporary values.
x0:=1.0;
x1:=next(x0);
x2:=next(x1);
x3:=next(x2);
x4:=next(x3);
x5:=next(x4);
x6:=next(x5);
x7:=next(x6);
% The list of the first approximations.
{x0,x1,x2,x3,x4,x5,x6,x7};

% The function NestList(f,expr,n) returns the first n elements of {x,f(x), f(f(x)), ...}
NestList(f,expr,n):=if(n=0, List(expr), Cons(expr, NestList(f,f(expr),n-1)));
% Use it!
NestList(next,x0,7);
% NestList also works symbolically.
DeclareVars(g,x);
NestList(g,x,5);
% We can get the first approximations as symbolical expressions. Not pretty.
NestList(next,x,2);


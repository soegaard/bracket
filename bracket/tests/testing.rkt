#lang bracket
DeclareVars(x,y,z,x0,y0,z0,w);

{2,3}*{4,5};
2*{3,4};
{3,4}*5;

Fold(f,b,xs):=foldl(f,b,rest(xs));
sum_list(xs):=Fold(Plus,0,xs);
RRange(i,j):=if(i>=j,{},Cons(i,Range(i+1,j)));

sum(f,x,i,j):=sum_list(Map((λk.Substitute(f,x=k)),Range(i,j)));
sum(x^2,x,1,4);

dot(v,w):=sum_list(v*w);
norm(v):=√(dot(v,v));
proj(v,w):=(dot(v,w)/(norm(w))^2)*w;
line(point,normal):=dot((point-{x,y}),normal);
a:={x,y};
b:={z,w};
bug_in_next_two_lines:=2;
a*b;     
dot(a,b);
norm(a);
norm(b);
proj(a,b);
line({x0,y0},{z,w});
x:=0;
x=0;
If(x=0,1,2);
If(x=0,1,2);
If(x=42,1,2);
If(3+z,1,2)
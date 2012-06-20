#lang mpl
DeclareVars(x,y,z,v,v1,v2,w,w1,w2,a,b,c);

sumlist(xs):=Apply(Plus,xs);
dot(v,w):=sumlist(v*w);
norm(v):=Sqrt(dot(v,v));
proj(v,w):=(dot(v,w)/norm(w))*w;

dot({a,b},{x,y});
norm({a,b});
(dot({a,b},{x,y})/norm({x,y})) * {x,y};
(dot({a,b,c},{x,y,z})/norm({x,y,z})) * {x,y,z};
proj({a,b},{x,y});

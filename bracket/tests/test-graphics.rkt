#lang bracket
DeclareVars(x);
Graphics({Disk()});
Graphics({Disk({0,0})});
Graphics({Disk({0,0},5)});

Graphics({Red, Disk({0,0},6), Blue , Disk({8,0},6), Green, Disk({4,4},6)});

Graphics({Hue(0),Point({0,0}),Hue(1/3),Point({3,0}),Hue(2/3),Point({6,0})});

Graphics({Line({10,10},{20,20},{20,30},{30,30}),
              Point({10,10})});
Graphics(AppendStar(Map((λx.{Hue(x/(2*pi)),Point({8*Sin(2*x),8*Cos(3*x)})}),Range(0,2*pi,1/128))))

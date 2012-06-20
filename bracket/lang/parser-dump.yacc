%token NUMBER
%token STRING
%token IDENTIFIER
%token IDENTIFIEROP
%token IDENTIFIER:=
%left  '-' '+'
%left  '*' '/'
%right  'OB'
%right  '^'
%left  '='
%right  'NEG'
%left  'SEMI'
%start (start)
%%
start: exp 
| 
;
args: exp 
| exp 'COMMA' args 
| 
;
ids: 
| IDENTIFIER ids 
;
parenthensis-exp: 'OP' exp 'CP' 
;
atom: NUMBER 
| IDENTIFIER %prec IDENTIFIER
| STRING 
| parenthensis-exp 
;
construction-exp: 'OC' args 'CC' 
| 'OP' 'LAMBDA' ids 'PERIOD' exp 'CP' 
| atom 
;
application-exp: IDENTIFIEROP args 'CP' 
| application-exp 'OP' args 'CP' %prec OP
| application-exp 'ODB' exp 'CB' 'CB' 
| construction-exp 
;
power-exp: application-exp '^' power-exp %prec ^
| application-exp 
;
sqrt-exp: 'SQRT' sqrt-exp 
| power-exp 
;
negation-exp: '-' negation-exp 
| sqrt-exp 
;
multiplication-exp: multiplication-exp '*' negation-exp %prec *
| multiplication-exp '/' negation-exp %prec /
| negation-exp 
;
addition-exp: addition-exp '-' multiplication-exp %prec -
| addition-exp '+' multiplication-exp %prec +
| multiplication-exp 
;
order-exp: addition-exp 'LESS-EQUAL' addition-exp %prec =
| addition-exp '<' addition-exp %prec =
| addition-exp 'GREATER-EQUAL' addition-exp %prec =
| addition-exp '>' addition-exp %prec =
| addition-exp 'NOT-EQUAL' addition-exp %prec =
| addition-exp '=' addition-exp %prec =
| addition-exp 
;
logical-negation-exp: 'NEG' logical-negation-exp %prec NEG
| order-exp 
;
assignment-exp: IDENTIFIER:= assignment-exp 
| IDENTIFIEROP args 'CP' ':=' assignment-exp 
| logical-negation-exp 
;
compound-exp: compound-exp 'SEMI' assignment-exp 
| assignment-exp 
;
exp: compound-exp 
;
%%

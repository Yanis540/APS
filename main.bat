cd APS0 
: lancer make (soyez CERTAIN que YACC_ML = ocamlyacc)
./prologTerm ./Samples/prog0.aps
: g0(L),type_def(L,functionRec(id(yanis),T,[(n,int)], num(1)),G).
: type_def([],functionRec(id(yanis),T,[(n,int)],if n> 0  num(1) app(id(mul),[id(n),app(id(yanis),[app(id(sub),[id(n),num(1)])]),T ),G).
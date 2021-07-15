%define lr.type canonical-lr
%start root

%%
root: 'a' A 'b' B
;
A: 'b' | B 'c'
;
B: 'c' | 'a' A
;
%%

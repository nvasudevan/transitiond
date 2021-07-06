%define lr.type canonical-lr
%start root

%%
root: 'a' A 'b' B
;
A: 'b' | 'c' B
;
B: 'c' | 'a' A
;
%%

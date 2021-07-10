%define lr.type canonical-lr
%start root

%%
root: 'a' A 'c' | 'd'
;
A: 'b' |
;
%%
%define lr.type canonical-lr
%start root

%%
root: 'a' A 'b'
;
A: 'b' | 'c' A
;
%%

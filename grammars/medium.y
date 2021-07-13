%define lr.type canonical-lr
%start root

%%

root: H 'q'
;
H: P 'c' J | 'f'
;
P: V 'q' | 'f'
;
J: 'n' | L 'r'
;
V: 'a' | 'c' H
;
L: 'o' | I 'f'
;
I: 'c' | V 'q'
;
%%

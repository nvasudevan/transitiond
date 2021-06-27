%define lr.type canonical-lr
%start S
%%
S: F B 'x' | G B 'y'
;
F: 'a'
;
G: 'a'
;
B: 'b' 'b'
;
%%
%define lr.type canonical-lr
%start root

%%

root: H N 'q' 'u' | J N
;
H: P 'c' J 'y' V | J | 'f' 'j'
;
N: L 'u' | 'l'
;
P: I V 'q' | I 'a' | 'f' U 'y' 'r'
;
J: 'n' K U | L 'r' K I
;
V: 'a' 'l' Z 'q' 'n' | U 'c' I H
;
L: 'o' | 'j' U I N 'f' | 'c'
;
I: 'c' 'y' Z | V 'j' 'q' | 'k' 'f' 'r'
;
U: 'y' 'c' Z O 'o'
;
K: L
;
Z: 'f' | 'n' 'f'
;
O: 'a' 'q' 'r' | J Z U 'c' | L
;


%%
.so letter.tmac \" US letter
.so math.tmac
.so es.tmac
.nr PS 11
.nr VS 13
.ds CH
.
.
.CD
.sp -1i
FS-0330. Física II. Tarea no. 7
Gustavo Calvo C11437
.CE
.
.
.
.NH
Sifón (20 puntos)
.LP
El sifón es un aparato que permite extraer líquido de un recipiente sin volcarlo.
En la figura se muestra el esquema para un sifón. Para que el agua fluya de un
punto $A$ adentro del recipiente a un punto más bajo $C$ por fuera, es necesario
que el tubo esté inicialmente lleno. Luego el líquido seguirá fluyendo hasta
que el nivel descienda por debajo de la boca del tubo en $A$. El líquido tiene
una densidad $rho$ y una viscosidad despreciable.
.
.defcolor red rgb #f7dbd4
.PS
X: circle rad 0.00 invis
line down 1.5i
line right 1.2i
line up 1.5i
Y: circle rad 0.00 invis
move to X
move down 0.89i
P: move right 0.6i
box width 1.18 ht 1.2 at P color "red"
move to Y
move left 0.45i
move down 0.825i
A: circle rad 0.00 invis "\h'-2'\v'-0.6'$A$"
line up 1.2i
V: circle rad 0.00 invis
J: move right 0.55
B: move up 0.55i "\v'-0.6'$B$"
move down 0.55i
U: move right 0.56i
arc from U to V
line down 2.5i from U
C: circle rad 0.00 invis "\h'-3'\v'0.6'$C$"
move to A
move right 0.1i
A: circle rad 0.00 invis
line up 1.2i
V: circle rad 0.00 invis
J: move right 0.275
B: move up 0.55i
move down 0.55i
U: move right 0.64i
arc from U to V
line down 2.5i from U
C: circle rad 0.00 invis
move to A
move left 0.05i
A: circle rad 0.00 invis
line up 1.2i thickness 6.5 color "red"
V: circle rad 0.00 invis
J: move right 0.275
B: move up 0.55i
move down 0.55i
U: move right 0.738i
arc from U to V thickness 6.5 color "red"
line down 2.8i from U thickness 6.5 color "red"
C: circle rad 0.00 invis
move to A
move right 0.5i
arrow up 1.65i "\v'-1.5'\h'1.5'$h sub 1$"
arrow down 1.12i
arrow down 0.54i
arrow up 0.54i "\v'-0.1'\h'1.5'$d$"
arrow down 1.84i
arrow up 1.32i "\v'-0.2'\h'1.5'$h sub 2$"
.PE
.
.
.
.LP
\fB(a) (6 puntos)\fP Encuentre la velocidad $vel$ con que el lı́quido sale del tubo en $C$.
.EQ
p sub A + cancel { 1 smallover 2 rho vel sub A sup 2 } +          rho g h sub 2 ~~=~~
p sub C +          1 smallover 2 rho vel sub C sup 2   + cancel { rho g h sub C }
.EN
.EQ
p sub A = l[ p sub atm + rho g d r] ~~and~~ p sub C = l[ p sub atm r]
~~~->~~~
l[ cancel { p sub atm } + rho g d r]
+ rho g h sub 2 ~~=~~
left [ cancel { p sub atm } right ]
+ 1 smallover 2 rho vel sub C sup 2
.EN
.EQ
{ sqrt { 2g l( d+ h sub 2 r) } = vel sub C }
.EN
.EQ
~
.EN
.
.LP
\fB(b) (6 puntos)\fP Encuentre la presión del líquido en el punto más alto del tubo, $B$.
.EQ
p sub B ~~=~~ p sub atm -
pile { size -2 { pile { roman "Presión" above roman "columna A" } } above box { rho g h sub 1 } above ~ }
-
pile { size -2 { pile { roman "Presión" above roman "columna C" } } above box { rho g l( h sub 1 + d + h sub 2 r) } above ~ }
        ~~=~~ p sub atm - rho g l( 2h sub 1 + d + h sub 2 r)
.EN
.
.LP
\fB(c) (8 puntos)\fP Encuentre la mayor altura $h sub 1$ a la que el sifón puede elevar el agua.
.LP
Justifique claramente su respuesta.
.QP
Dado que la presión debe ser mayor a cero, se puede confeccionar la siguiente inecuación en la que
$d + h sub 2$ igual a cero maximiza la altura conservando las características de sifón.
.QE
.EQ "*Expresión obtenida en (b)"
p sub B = 101325 Pa - 9800 l( 2h sub 1 + d + h sub 2 r) Pa ~~>~~ 0
.EN
.EQ
101325 / { 2 ~.~ 9800 } - cancel { { l( d + h sub 2 r) } / 2 } ~~>~~
h sub 1 ~~~->~~~ 5.17 m ~~>~~ h sub 1
.EN
.LP
.
.
.
.2C
.NH
Coeficientes de descarga (25 puntos)
.LP
Un fluido ideal e incompresible está sujeto a un campo gravitacional constante $-g z hat$.
El fluido está contenido en un tanque cuya superficie superior está abierta a la atmóstera.
Considere el tubo re-entrante horizontal que se muestra en la figura.
El tubo tiene un área transversal $A$. El fluido que sale por el tubo forma un chorro
que se angosta hasta alcanzar un área transversal $a$, tal que $a < A$.
.PS
X: circle rad 0.00 invis
line down 1i
line right 1.2i
line up 0.15i
line up 0.15i invis
A: line up 0.7i
Y: circle rad 0.00 invis
move to X
move down 0.89i
P: move right 0.6i
P: move up 0.45i
box width 1.18 ht 0.5 at P color "red"
line left 0.35i from A
line down 0.15i invis
line right 0.35i
P: move to P
P: move down 0.485i
box width 1.18 ht 0.128 at P color "red"
P: move to P
P: move up 0.15i
P: move left 0.175i
box width 0.83 ht 0.2 at P color "red"
"\h'1'$p sub 1$"
P: move to P
I: move right 0.4i
I: move up 0.06i
for i = 0 to 9 do {
    move down 0.01i 
    I: move right 0.001i 
    circle rad 0.02 at I color "red"
}
move up 0.05i
move left 0.15i
for i = 0 to 9 do {
    move down 0.005i 
    I: move right 0.001i 
    circle rad 0.02 at I color "red"
}
for i = 0 to 9 do {
    move down 0.001i 
    I: move right 0.001i 
    circle rad 0.02 at I color "red"
}
move left 0.4i
for i = 0 to 9 do {
    I: move right 0.001i 
    circle rad 0.02 at I color "red"
}
move left 0.4i
for i = 0 to 9 do {
    I: move right 0.001i 
    circle rad 0.02 at I color "red"
}
move up 0.03i
move left 0.2i
for i = 0 to 4 do {
    I: move right 0.001i 
    circle rad 0.04 at I color "red"
}
"\h'7'\v'0.2'$-> a$"
"\h'-4'$A ->$"
.PE
.EQ
~
.EN
.
.LP
\fB(a) (3 puntos)\fP Encuentre la tasa a la que fluye moméntum a través de la
superficie a en donde el chorro alcanza su grosor mínimo. Exprese su respuesta
en términos de $a$ y de la velocidad $vel$ del chorro en ese punto.
.EQ
m dot = rho ~.~ vel ~.~ a
.EN
.EQ
p = m ~.~ vel ~~~->~~~ p dot = m dot vel + cancel { m vel dot }
.EN
.EQ
p dot = rho vel sup 2 a = 10 sup 3 vel sup 2 a ~.~ N
.EN
.
.EQ
~
.EN
\fB(b) (3 puntos)\fP Explique claramente por qué, si el tanque es mucho más
grande que el tubo, podemos tomar que todo el fluido que está en contacto con
las paredes del tanque está uniformemente en reposo.
.QP
El volumen $V$ expulsado por el orificio después de un tiempo $t$ está
directamente relacionado con el cambio de volumen $V$ dentro del envase.
Dado que $V (t) = vel sub 0 t . A = vel sub 1 t . a$, si se toma que A>>a y por lo
tanto $a smallover A approx 0$, entonces
$vel sub 0 = vel sub 1 . a smallover A approx 0$.
Siendo $vel sub 0$ la velocidad en cualquier punto no cercano al orificio,
es decir, que el fluido se encuentra uniformemente en reposo cuando cumple
estas condiciones.
.QE
.EQ
~
.EN
.
.LP
\fB(c) (3 puntos)\fP Aplique el teorema de Bernoulli para relacionar $vel$
con la presión $p sub 1$ del fluido en reposo junto a la pared vertical
opuesta a la boca del tubo (i.e., a la misma altura del tubo).
.EQ
p sub 1 = P sub atm + rho g h
.EN
.EQ
cancel {P sub 1} sup {P sub atm} + cancel {1 smallover 2 rho vel sub 1 sup 2} + rho g h sub 1 =
cancel {P sub 2} sup {P sub atm} + 1 smallover 2 rho vel sub 2 sup 2 + cancel {rho g h sub 2}
.EN
.EQ
p sub 1 =
P sub atm + 1 smallover 2 rho vel sub 2 sup 2
.EN
.EQ
~
.EN
.
.LP
\fB(d) (6 puntos)\fP Usando los resultados previos y las leyes de Newton
aplicadas a un elemento de fluido que sale por el tubo, demuestre que
$a smallover A = 1 smallover 2$
.EQ
F = [ p sub 1 ] ~.~ A = [ p sub 2 ] ~.~ a = p dot
.EN
.EQ
left [ 1 smallover 2 cancel {rho vel sup 2} right ] ~.~ A =
left [ cancel {rho vel sup 2} right ] ~.~ a
.EN
.EQ
a / A = 1 / 2
.EN
.EQ
~
.EN
.
.LP
Ahora suponga que en lugar de un tubo re-entrante tenemos un agujero
circular sencillo de área $A$ sobre la pared vertical del tanque, como
se muestra en la segunda figura.
.EQ
~
.EN
.
.LP
\fB(e) (3 puntos)\fP Explique por qué la conclusión de la parte (b)
ya no se sostiene. ¿Qué ocurre con la presión del fluido en contacto
con las paredes del tanque en las cercanías del agujero?
.
.QP
Al acercarse al agujero la velocidad del fluido aumenta de manera que
no se puede considerar que esté en reposo, además, la presión se acerca
a presión atmosférica de manera que ya no se pueden realizar las mismas
suposiciones.
.QE
.EQ
~
.EN
.
.LP
\fB(f) (4 puntos)\fP En este caso, ¿espera que $a/A$ sea igual,
mayor o menor que $1/2$? Justifique claramente su respuesta.
.
.QP
En este caso, debería ser igual ya que por continuidad la relación
entre velocidad y área debe ser igual para $A$ y $a$, por lo tanto,
es igual dado que solamente involucra las velocidades.
.QE
.
.1C
.LP
\fB(g) (3 puntos)\fP Basado en los resultados previos,
¿cuál configuración el tanque se vaciará más rápidamente,
si los dos tanques tienen las mismas dimensiones y si la
altura inicial del fluido es la misma? ¿Cómo podemos minimizar
el tiempo de descarga, dada un área $A$ del agujero sobre la
pared externa del tanque?
.QP
Deberían vaciarse al mismo tiempo ya que la velocidad de salida
depende de la presión ejercida por una columna vertical, en la que
una variación horizontal de la forma del agujero no tiene afectación.
.QE
.QP
Con base en lo anterior, la altura de la columna de agua puede aumentarse
al reducir el área transversal del recipiente. De esta manera, manteniendo
el volumen se puede aumentar la velocidad de salida sin modificar otra propiedad.
.QE
.
.bp
.NH
Flujo de Poiseuille (25 puntos)
.LP
Un líquido con viscosidad $eta$ tiene un flujo laminar estacionario en un tubo
cilíndrico horizontal de radio $R$ y longitud $L$, como se muestra en la figura.
.EQ
~
.EN
.
.LP
\fB(a) (6 puntos)\fP Considere un cilindro arbitrario de fluido de radio $r$.
Demuestre que la fuerza viscosa $F$ sobre el cilindro, debida a la capa circundante, es
$F = - eta ( 2 pi r L ) dv / dr$
.EQ
F ~~~=~~~ - eta A dv / dr ~~~=~~~
- eta left ( 2 pi r ~.~ L right ) dv / dr
.EN
.EQ
~
.EN
.
.LP
\fB(b) (3 puntos)\fP Demuestre que la fuerza $F$ que empuja al cilindro
a lo largo del tubo es $F' = ( pi r sup 2 ) Delta p$
.QP
La presión $p$ en cualquier punto empuja el líquido. Para que pueda mover al
cilindro debe aplicar esa presión sobre un área, en este caso la superficie
perpendicular a la aplicación de la fuerza resultante $F'$. Dado que el cilindro
también posee inercia, hay una diferencia de presión entre cualquier punto $p$ y la
presión en la superficie perpendicular al cilindro es $Delta p$.
La fuerza que se ejerce sobre el cilindro debería ser $Delta p ~.~ A$.
.QE
.EQ
F' = ( pi r sup 2 ) Delta p
.EN
.EQ
~
.EN
.
.LP
\fB(c) (8 puntos)\fP Utilice la condición de equilibrio para obtener una expresión
para $d vel$ en términos de $dr$. Integre este resultado para hallar la velocidad
del flujo como función de $r$. Muestre que concuerda con el resultado que
vimos en clase [la Ec. (18) en el cap. 18 del libro de texto].
.EQ
F = F' = eta left ( 2 pi r ~.~ L right ) {d vel} / dr = ( pi r sup 2 ) Delta p
.EN
.EQ
d vel = { Delta p } / { 2 eta L } r dr ~~~->~~~
int d vel = int { Delta p } / { 2 eta L } r dr ~~~=~~~
{ Delta p } / { 4 eta L } r sup 2 ~~~=~~~ vel
.EN
.EQ
~
.EN
.
.LP
\fB(d) (8 puntos)\fP Halle una expresión para el flujo de masa por un anillo
entre los $r$ y $r + dr$. Integre el resultado para hallar el flujo de masa
total por el tubo, verificando el resultado visto en clase
[la Ec. (20) en el cap. 18 del libro de texto].
.EQ
m dot = rho ~.~ vel ~.~ dA ~~~=~~~
rho ~.~
left ( {r sup 2 Delta p } / { 4 eta L } right )
~.~ left ( 2 pi r dr right )
.EN
.EQ
m dot = { rho pi r sup 3 Delta p} / {2 eta L} dr
.EN
.EQ
m = int m dot = { rho pi r sup 4 Delta p} / {8 eta L}
.EN
.EQ
~
.EN
.EQ
~
.EN
.EQ
~
.EN
.
.
.
.NH
Número de Reynolds (10 puntos)
.LP
Usando el número de Reynolds, estime la máxima velocidad a la que puede fluir
la sangre en el cuerpo humano, manteniendo un flujo laminar.
Considere una arteria como un tubo cilíndrico de radio $3,8 mm$.
La viscosidad de la sangre a $37 deg C$ es
$eta = 4,0 times 10 sup {-3} N . s /m sup 2$.
La densidad de la sangre entera es $rho = 1,06 times 10 sup 3 kg/m sup 3$.
Justifique claramente su razonamiento. Comente si esta respuesta
le parece razonable.
.EQ
Re = { rho ~ vel ~ 2r } / { eta }
.EN
.EQ
vel = { Re ~.~ eta } / { rho ~.~ r } =
vel = { 2000 ~.~ 0,004 } / { 1060 ~.~ 2 ~.~ 0,0038 } = 0.99 m/s sup 2
.EN

Dissipative and Turnpike Properties in Controlled Stochastic Systems with Lévy Jumps
====================================================================================

Propiedades disipativas y de tipo «turnpike» en sistemas estocásticos controlados con saltos de Lévy

This article investigates a type of stability in stochastic-controlled systems driven by Lévy jumps. An optimal control problem is 
formulated and the relationships between dissipativeness and turnpike properties for these systems are examined, using the stochastic 
version of Pontryagin’s maximum principle, as well as the polynomial theory of Routh-Hurwitz, Khasminskii and Riccati. An example of 
such a system, consisting of a Lotka-Volterra model, is presented and a simulation of the results is performed, showing that the 
turnpike property is indeed satisﬁed in the controls, states, and co-states.

Este artículo investiga un tipo de estabilidad en sistemas controlados estocásticamente, sujetos a saltos de Lévy. Se formula un 
problema de control óptimo y se examinan las relaciones entre la disipatividad y las propiedades de tipo «turnpike» (comportamiento 
asintótico hacia una trayectoria óptima) en dichos sistemas, empleando la versión estocástica del principio del máximo de Pontryagin, 
así como la teoría polinómica de Routh-Hurwitz, Khasminskii y Riccati. Se presenta un ejemplo de este tipo de sistema, basado en un 
modelo de Lotka-Volterra, y se lleva a cabo una simulación de los resultados, la cual demuestra que, efectivamente, se cumple la 
propiedad «turnpike» tanto en los controles como en los estados y coestados.

**1. Introduction**

Two important aspects of controlled and stochastic dynamical systems are the subject of this article: Turnpike-type stability, on the 
one hand, and dissipativity property, on the other. The ﬁrst property consists of a type of stability around a state of equilibrium, 
characterized by remaining close to that state for the second third of the total time considered. The optimal trajectory, the optimal 
control, and the corresponding adjoint vector remain exponentially close to a steady state. The second property consists of the system 
conserving its internal energy for most of the time. Dissipativity theory is a system-theoretic concept that provides a powerful 
framework for the analysis and control design of open dynamical systems based on generalized system energy considerations. In 
particular, dissipativity theory deals with the study of the properties of dynamical systems related to the conservation, dissipation, 
and transport of mass and energy. For a dissipative dynamic system, the stored energy is at most equal to the sum of the initial energy 
stored in the system and the total energy supplied externally to the system. This property is a characteristic of the Turnpike theory 
which was introduced in 1958 in mathematical economics and recently has been applied in Control Theory in [1].

Este artículo aborda dos aspectos importantes de los sistemas dinámicos controlados y estocásticos: la estabilidad de tipo *Turnpike* 
(o de «camino principal»), por un lado, y la propiedad de disipatividad, por otro. La primera consiste en un tipo de estabilidad en 
torno a un estado de equilibrio, caracterizada por permanecer cerca de dicho estado durante el segundo tercio del tiempo total 
considerado. Tanto la trayectoria óptima y el control óptimo como el vector adjunto correspondiente permanecen exponencialmente 
próximos a un estado estacionario. La segunda propiedad consiste en que el sistema conserva su energía interna durante la mayor parte 
del tiempo. La teoría de la disipatividad es un concepto propio de la teoría de sistemas que proporciona un marco sólido para el 
análisis y el diseño de control de sistemas dinámicos abiertos, basándose en consideraciones sobre la energía generalizada del sistema. 
En particular, esta teoría se ocupa del estudio de las propiedades de los sistemas dinámicos relacionadas con la conservación, la 
disipación y el transporte de masa y energía. En un sistema dinámico disipativo, la energía almacenada es, como máximo, igual a la suma 
de la energía inicial almacenada en el sistema y la energía total suministrada al mismo desde el exterior. Esta propiedad es una 
característica de la teoría *Turnpike*, la cual fue introducida en 1958 en el ámbito de la economía matemática y, recientemente, ha 
sido aplicada a la teoría del control en [1].


It is very common that when studying a question that has arisen from applications, an optimal control problem must be solved (see for 
example [2], [3], [4]). Several articles have been written on the relationship between turnpike property and dissipativity in control 
systems. In [5], the authors investigate the relationship between strict dissipativity and the exponential turnpike property for a 
nonlinear control system. They demonstrate that, under mild assumptions, the turnpike property implies dissipativity and that, for 
discretetime systems, dissipativity implies the turnpike property. The paper [6] is one of the ﬁrst to address the turnpike property 
from an optimal control perspective, analyzing the relationship between the strict dissipativity property and a speciﬁc exponential 
turnpike property. The authors consider deterministic, for ﬁnite-dimensional linear quadratic optimal discrete-time control systems, 
Their main result is that the exponential turnpike property implies strict dissipativity. In [7], the authors analyze the connection 
between Turnpike behavior and strict dissipative properties for ﬁnite-dimensional linear quadratic optimal discrete-time control 
systems, under a weak stabilization assumption. In [8], we analyze the Turnpike property of a stochastic-controlled Lotka-Volterra 
system for only two species and with white noise. In [9], we considered a stochastic Lotka-Volterra model for a predatortwo-prey 
system, driven by white noise processes and Lévy jumps, and demonstrated the boundedness and exponential stability of its solutions 
using Lyapunov function techniques. Finally, in [10], we demonstrate the Turnpike property for a stochastic-controlled Lotka-Volterra 
model of a predator-two prey system with only white noise. Now, in the present work, we study the Turnpike property for general 
controlled stochastic systems driven by Lévy jumps and its relationship to the dissipativity property.

Es muy frecuente que, al abordar una cuestión surgida de aplicaciones prácticas, sea necesario resolver un problema de control óptimo 
(véanse, por ejemplo, [2], [3], [4]). Se han escrito diversos artículos sobre la relación entre la propiedad *turnpike* (o propiedad de 
comportamiento asintótico) y la disipatividad en sistemas de control. En [5], los autores investigan la relación entre la disipatividad 
estricta y la propiedad *turnpike* exponencial en sistemas de control no lineales. Demuestran que, bajo supuestos poco restrictivos, la 
propiedad *turnpike* implica disipatividad y que, para sistemas en tiempo discreto, la disipatividad implica la propiedad *turnpike*. 
El artículo [6] es uno de los primeros en abordar la propiedad *turnpike* desde la perspectiva del control óptimo, analizando la 
relación entre la propiedad de disipatividad estricta y una propiedad *turnpike* exponencial específica. Los autores consideran 
sistemas de control óptimo lineal-cuadráticos en tiempo discreto y de dimensión finita; su resultado principal es que la propiedad 
*turnpike* exponencial implica disipatividad estricta. En [7], los autores analizan la conexión entre el comportamiento *turnpike* y 
las propiedades de disipatividad estricta en sistemas de control óptimo lineal-cuadráticos en tiempo discreto y de dimensión finita, 
bajo un supuesto de estabilización débil. En [8], analizamos la propiedad *turnpike* de un sistema de Lotka-Volterra con control 
estocástico, que involucra dos especies y está sujeto a ruido blanco. En [9], consideramos un modelo estocástico de Lotka-Volterra para 
un sistema de un depredador y dos presas, afectado por procesos de ruido blanco y saltos de Lévy, y demostramos la acotación y la 
estabilidad exponencial de sus soluciones mediante técnicas de funciones de Lyapunov. Finalmente, en [10], demostramos la propiedad 
*turnpike* para un modelo estocástico de Lotka-Volterra —con control y un depredador frente a dos presas— que solo presenta ruido 
blanco. En el presente trabajo, estudiamos la propiedad *turnpike* para sistemas estocásticos controlados generales, sujetos a saltos 
de Lévy, así como su relación con la propiedad de disipatividad.


From now on, we will use the notation x t interchangeably with x(t), or u t with u(t), and so on. We consider a controlled jump 
diﬀusion process given by the following non-linear stochastic ordinary diﬀerential equation system:

.. math::

   dx = f(t, x(t), u(t))dt + g(t, x(t), u(t))dW(t) 

     + x(t)u(t) \int_{R^3} \gamma (t, x(t−), z ˜N(dt, dz), ∫ R3 

(1)

where x(t) ∈ L F 2 (0, T; R 3 ), T ∈ R, x(t−) denotes the left hand limit of x at time t,

f : L F 2 (0, T; R 3 ) × L F 2 (0, T; R 3 ) −→ R 3 , g : L F 2 (0, T; R 3 ) ×

L F 2 (0, T; R 3 ) −→ R 3 × R 3 , γ : L F 2 (0, T; R 3 ) ×

L F 2 (0, T; R 3 ) −→ R 3 , u : R → R 3 is a measurable and bounded function called the control, which belongs to a region control U 
and it is an adapted and cadlag function (continuous on the right and limit on the left), and W(t) = W t = (W 1 (t), W 2 (t), W 3 (t)) 
is a 3-dimensional stochastic standard Brownian process, deﬁned in the probability space, (Ω, F t , p), where { F t , 0 ≤ t ≤ T } 
is the natural ﬁlter of the process W t . the control u t is contained in the Banach space

L F 2 (0, T; R 3 ) = { x t | x t is a stochastic process, R 3 − valuated,

|| x t|| 2 < ∞ } ,

ﬁnally we will say that x t ∈ L F 2 (0, T; R 3 ) and || x || is the Euclidean norm and that | A | = ∑ i=1 n ∑ j=1 m || A ij A ji | 
1/2 The Lévy process is generated through the counting process

µ t = { N t , t ≥ 0 } ,N t = N t (N 1 (dt, dz), N 2 (dt, dz), N 3 (dt, dz))

which we will call the Poisson process, which represents the number of times an event occurs up to time t. This satisﬁes:

a) N(0) = 0

b) N(t+s)−N(s), which represents the number of events that occur between time t+s and constitute a Poisson distribution, that is, 
P[N(t) = n] = (λt) n e −λt . We can give the following n! process as

N(t, U) = N(t, U, w) = # { ∆ µ n (s) ∈ U : 0 ≤ s ≤ t } ,

where U ⊂ R 3 . We can state that, for each t ≥ 0, w ∈ U ﬁxed N(t, U) is a measure in B ⊂ R 3 and is denoted as N(t, dz), for each 
w ﬁxed, we have that N(b, U, w)−N(a, U, w) deﬁnes a measure and is denoted as N(dt, dz) = ∑ 0 ≤ s ≤ t 1 U ∆ µ s , which 
represents the number of jumps of size ∆ µ n that occur before time t. We deﬁne the function ν(U) = E[N(1, U)], which is also a 
measure in B called the Levy measure of {µ t } and ﬁnally, we deﬁne the measure

¯N(dt,

dz)

=

N(dt,

dz)

−

ν(dz)dt,

and with this, we deﬁne the integral

T γ (s, z(−), w)¯N(dt, dz). ∫ 0 ∫ R3 

Considering the stochastic diﬀerential system (1), we assume the following hypothesis related with the Lipschitz and linear growth 
conditions in the x variable, for f(x, t, u), g(x, t, u) and h : R 3 → R 3 the jump coeﬃcient or Poisson’s process coeﬃcient, 
deﬁned by h(x, t, u) = γ (t, x, z)¯N(dt, dz). ∫ R3  (H1) There exist constants κ 1 < ∞ and κ 2 < ∞ such that f(x, t, u), g(x, t, u) 
and h(x, t, u) satisfy:

a) ||γ (t, x, z) || 2 ¯N(dt, dz) ≤ κ 1 (1 + || x || 2 ), ∫ R3  2 2 || f(x, t, u) || ≤ κ 1 (1 + || x || ),

2 2 || g(x, t, u) || ≤ κ 1 (1 + || x || ),

b) ||γ (t, x, z) − γ (t, y, z) || 2 ¯N(dt, dz) ≤ κ 2 || x − y || 2 , ∫ R3 

2 2 || f(x, t, u) − f(y, t, u) || ≤ κ 2 || x − y || ,

2 2 || g(x, t, u) − g(y, t, u) || ≤ κ 2 || x − y || .

(H2) There exists κ 3 < ∞ , such that ∀t ∈ R : || u(t) || ≤ κ 3 .

(H3) There exist c

∈

R and M

⊂

R 3 , such that :

sup |γ

sup

(s,

x(s−),

|

z)

<

.

∞

x ∈ M 0 ≤| z |≤ c

Our interest lies in the stability of the solutions to a stochastic optimal control problem, the Turnpike property for this problem and 
its relation to the dissipative property.

2 Stochastic Optimal Control Problem with Jumps

We establish the following Stochastic Optimal Control Problem with jumps (SOCPJ):

To ﬁnd the control u(t) = (u 1 (t), u 2 (t), u 3 (t)) ∈ U in the system

dx = f(t, x(t), u(t))dt + g(t, x(t), u(t))dW(t),

(2)

+ x(t)u(t)

∫ R3 

γ

(t, x(t−), z)˜N(dt, dz),

x(0) = (x 10 , x 20 , x 30 ), x(T) = (x 11 , x 21 , x 31 ),

which minimize the following expected cost functional in the Lagrange form:

1 T J(u t ) = E f 0 (x t , u t ) dt , a.s. { 2 ∫ 0 }

(3)

where f 0 : L F 2 (0, T; R 3 ) × L F 2 (0, T; R 3 ) −→ R, f 0 (x t , u t ) =

xi 2  (t) + u i 2 (t) . From now on we will assume that ∑ i=1 3 ( ) hypotheses (H1) and (H2) are satisﬁed, to guarantee the existence 
and uniqueness of the solutions of (2)

Deﬁnition 1. The control u ∗ (t) = (u 1 ∗ (t), u 2 ∗ (t), u 3 ∗ (t)) ∈ U satisfying (2) and (3) is said to be an optimal control 
if it satisﬁes J(u ∗ (·)) = min u(·) J(u(·)). The corresponding state x ∗ (t) is called optimal state and (x ∗ (t), u ∗ (t)) is 
called the optimal pair.

Associated to the Optimal Control SOCPJ (2) and to the generalized Hamiltonian function H(x(t), p(t), q(t), u(t)),

H = ] − f 0 (t, x, u) p(t), f(t, x, u)⊤  + tr[q(t)g(t, x, u)⊤  ⟨ ⟩

3 + ∑ x i (t)p i (t)u i (t) γ i (t, x i (t−), z)N(dt, dz), (4) i=1 ∫ R

the stochastic maximum principle [11] characterizes the optimal control and proclaims the existence of two adjoint processes p(t) and 
q(t) that satisfy:

∂H ∂H ∂H dx T = dt + dW(t) + dN(t), ∂p ∂q ∂h ∂H dp T = dt + q(t)dW(t) ∂x +u(t)p(t) γ (t, x(t−), z)N(dt, dx). ∫ R3 

Then, the adjoint equations corresponding to the processes p(t) are the following:

dp(t) = − { ⟨ f x ⟩ ∑ ⟨ gx j (t, x, u) ⊤ , p(t) + (t, x(t), u(t)) ⊤ , q j (t) ⟩ ( ∫ 0 ⟨ ∂x ⟩ ) j=1 T ∂H −(f 0 (t, x(t), u(t))) 
x = E ( ( H(x, p, q, u) − H ∗ (t) − (x(t) − x ∗ (t)), ∂x (t) ) dt ) dt + q(t), dW(t) } 〈 〉 ∫ 0 ⟨ ⟩

+u(t)p(t) γ (t, x(t−), z)N(dt, dz), ∫ R3 

T −E f 0 (t, x, u) − f 0 (t, x ∗ , u ∗ )dt ( ∫ 0 )

p(T) = (p 11 , p 21 , p 31 ) ⊤ ,

(5) T ∂H = E ( H(x, p, q, u) − H ∗ (t) − (x(t) − x ∗ (t)), (t) dt ( ∫ 0 ⟨ ∂x ⟩ ) )

(7)

and, according to the necessary conditions of stochastic maximum principle ∂u i ∂H (x, p, q, u) = 0, i = 1, 2, 3, we obtain

u i (t) = −A i p i (t)x i (t)+p i (t)x i (t) γ i (t, x i (t−), z)N(dt, dz). ∫ R

(6) In this way, we have suﬃcient conditions for optimality [12]:

Theorem 1. The control u ∗ (t) = (u 1 ∗ (t), u 2 ∗ (t), u 3 ∗ (t)) given by (6) is the solution of the Stochastic Optimal Control 
Problem with Jumps SOCPJ (2).

Proof. Let u ∗ (t) be the control given by (6) and let x ∗ (t) be the corresponding state of (1) associated to u ∗ (t). Let u(t) ∈ 
U be any arbitrary control satisfying the system (1). We deﬁne the Hamiltonian corresponding to (4) by H ∗ (t) = H(x ∗ (t), p(t), 
q(t), u ∗ (t)). Now, we consider the product 〈 p(T), x(T) − x ∗ (T) 〉 . In the calculation of the diﬀerential of stochastic 
processes, by applying Itˆo property with

umps, we have:

d

⟨

p(T),

x(T)

−

x∗ 

⟩

(T)

=

⟨

p(T),

d(x(T)

−

x∗ 

⟩

(T))

∗ + q(t)(g(x) − g(x ))dt

+ ⟨ (x(T) − x ∗ (T)), dp(T) ⟩

γ (t, x(t−), z)¯N(dt, dz)), ∫ R3  which gives by integration, taking the expected value and using (4) and (3):

+ p(t)u(t)

E ⟨ p(T), x(T) − x ∗ (T) ⟩ − ⟨ p(0), x i (0) − x ∗ (0) ⟩ ( )

T T = E ⟨ p(t), d(x(t) − x ∗ (t)) ⟩ + ⟨ d(x(t) − x ∗ (t)), dp(t) ⟩ ( ∫ 0 ∫ 0

T + q(t) (t)) g(x(t)) g((x∗  dt ∫ 0 ( )

+(x(t) − x ∗ (t))p(t)u(t) γ (t, x(t−), z)¯N(dt, dz)) ∫ R 3 )

T = E ⟨ p(t), f(x(t)) − f(x ∗ (t)) ⟩ dt ( 0

∫

T + q(t) (t)) g(x(t)) g(x∗  dt ∫ 0 ( ) +(x(t) − x ∗ (t))p(t)u(t) γ (t, x(t−), z)¯N(dt, dz)) ∫ R3  T ∗ ∂H − x(t) − x ∗ (t), 
(t) dt ∫ 0 ⟨ ∂x ⟩ )

T = E (t, x, u) − (H ∗ (t) − f 0 (t, x ∗ , u ∗ )) H(x, p, q, u) f0  dt ( ∫ 0 ( ) )

3 T ∂H −E H(x, p, q, u) − H ∗ (t) − (x(t) − x ∗ (t)), (t) dt dp(t) = − { ⟨ f x ⟩ ∑ ⟨ gx j (t, x, u) ⊤ , p(t) + (t, x(t), 
u(t)) ⊤ , q j (t) ⟩ ( ∫ 0 ⟨ ∂x ⟩ ) j=1 T ∂H −(f 0 (t, x(t), u(t))) x = E ( ( H(x, p, q, u) − H ∗ (t) − (x(t) − x ∗ (t)), ∂x 
(t) ) dt ) dt + q(t), dW(t) } 〈 〉 ∫ 0 ⟨ ⟩

+u(t)p(t) γ (t, x(t−), z)N(dt, dz), ∫ R3 

T −E f 0 (t, x, u) − f 0 (t, x ∗ , u ∗ )dt ( ∫ 0 )

p(T) = (p 11 , p 21 , p 31 ) ⊤ ,

(5) T ∂H = E ( H(x, p, q, u) − H ∗ (t) − (x(t) − x ∗ (t)), (t) dt ( ∫ 0 ⟨ ∂x ⟩ ) )

J(u)

−

J(u

.

The conditions of transversality in the maximum principle [13] and equations (7) imply that

T E , ( ∫ 0 )

H(x(t),

p(t),

q(t),

u(t))−H∗ 

⟨

(t)−

x(t)

−

x∗ 

(t),

Hx 

⟩

(t)

dt

its equal to J(u) − J(u ∗ ) and the convexity of H(·, p, q, ·) in

x(t), u(t), given by (4), implies:

T (H(x(t), p(t), q(t), u(t)) − H ∗ (t))dt ∫ 0

(8)

T ≤ ⟨ (t), H x (t) ⟩ dt. ∫ 0

x(t)

−

x∗ 

Finally,

from

(2)

and

(8)

we

deduce:

J(u

∗

)

≤

J(u)

∀u(t)

,

which proves that u ∗ (t) is the optimal control and the proof is completed.

In this section we begin by analyzing some suﬃcient conditions, related to Lyapunov functions, that guarantee the dissipative property 
of the system (1). Next, we will show that, under certain conditions related to a Lyapunov function, strict dissipativeness implies the 
turnpike property only for the state function x(t), and ﬁnally we will show that strict dissipativeness implies the turnpike property 
for the system (1), under conditions related to the spectral abscissa of a Riccati equation relative to the system (1).

Deﬁnition 2. We will say that the Control Problem SOCPJ (2) satisﬁes the Turnpike property, if there exist constants C 1 , C 2 and T 
, t 0 , such that for all t ∈ [t 0 , T], the following inequality holds:


E || x(t) − ¯x(t) || 2 + E || u(t) − ¯u(t) || 2

So, we have,

+E || p(t) − ¯p(t) || 2 ≤ C 1 e −C 2 (t−t 0 ) . (9)

(14)

¯f(x 0 , u T ) = f 0 (x 0 , u T ) − f 0 (¯x, ¯u)

We introduce a Lyapunov function [9], which is an Ito-Lévy process V (x t ) ∈ C 2,1 (R 3 × R + ; R + ) and also recall the deﬁnition 
of the linear operator or diﬀusion operator L : R + × R n → R, acting on V (x t ):

LV (x t ) = ) + ), f(t, x, u) (xt  (xt  ∂t ⟨ ∂x ⟩ 1 2 V ∂ + (t, x) (x t )g(t, x) trace g⊤  ( ∂x 2 ) 2

(16)

J(x 0 , u T ) ≤ TE(f 0 (¯x, ¯u)) + δ

resulting in

+x(t)u(t) { V ( x t− + γ (x, t) ) ∫ | z | <c

(17)

¯J(x t , u T ) ≤ TE(f 0 (¯x, ¯u)) + δ

∂V −V (x t− − γ i (x, t)) ) } ν(dz), (xt  ∂x

Now, from deﬁnition (3) of dissipativity,

where c ∈ (0, ∞) is the maximum jump size.

Deﬁnition 3. We will say that the control problem SOCPJ (2) is strictly dissipative stochastically at equilibrium (¯x(t), ¯p(t), 
¯u(t)), if there exists a bounded function V :

L F 2 (0, T; R n ) × L F 2 (0, T; R n ) −→ R, V ∈ C 2 (L F 2 (0, T; R n )),

called storage, and a strictly increasing, continuous and bounded function r 0 : R −→ R, called the dissipation rate, t such that the 
stochastic process { V (x t )− ∫ 0 r 0 ( || x s − ¯x s || )ds : t ≥ 0 } is a martingale such that:

T E ) ) + E ( f 0 (x s , u s ) V (xT  V (x0  ( ) ≤ ( ∫ 0

−f 0 (x 0 , u 0 ) − r 0 ( || x s − ¯x s || ) ) ds (10) . )

Now, we have the next relationship between the dissipative property and the turnpike property, whose demonstration is inspired by [7].

Theorem 2. Suppose that the SOCPJ control problem (2) is strictly dissipative at equilibrium (¯x(t), ¯u(t)) and that the functions V (x 
t ) and r 0 ( || x t − ¯x 0 || ) in deﬁnition (3) satisfy

a) E || x t − ¯x || 2 ≤ EV (x t ),

b) E V (x t ) ( || x s − ¯x 0 || ), C1  Er0  ( ) ≤

for some C 1 > 0, then the SOCPJ control problem (2) has the turnpike property for the state x(t): there exist constants C 2 , C 3 such 
that for all T > t 0 , and t ∈ [t 0 , T], the following inequality holds:

E || x(t) − ¯x(t) || 2 ≤ C 2 e −C 3 (t−t 0 ) .

(11)

Proof. We deﬁne the function ¯f(x 0 , u t ):

¯f(x t , u t ) = f 0 (x t , u t ) − f 0 (¯x, ¯u)

(12)

and also, we deﬁne the following functional

T ¯J(x t , u t ) = min u t ,0 ≤ t ≤ T E ¯f(x s , u s ) ds ∫ 0

(13)

So, we have,

¯f(x 0 , u T ) = f 0 (x 0 , u T ) − f 0 (¯x, ¯u)

and, integrating and taking the minimum, we obtain

¯J(x t , u T ) = 2J(x 0 , u T ) − TE(f 0 (¯x, ¯u))

(15)

By the other hand, from (13) there exists δ > 0 such that

J(x 0 , u T ) ≤ TE(f 0 (¯x, ¯u)) + δ

resulting in

+x(t)u(t) { V ( x t− + γ (x, t) ) ∫ | z | <c

(17)

¯J(x t , u T ) ≤ TE(f 0 (¯x, ¯u)) + δ

∂V −V (x t− − γ i (x, t)) ) } ν(dz), (xt  ∂x

Now, from deﬁnition (3) of dissipativity,

EV (x T ) ≤ V (x 0 ) + E f 0 (x s , u s ))ds ( ∫ 0

−f 0 (x 0 , u 0 ) − r 0 ( || x s − ¯x s || ) ds, )

then

T EV (x T ) ≤ V (x 0 ) + E f 0 (x s , u s ) ds ( ∫ 0 ) T −E(Tf 0 (x 0 , u 0 )) − E r 0 ( || x s − ¯x s || ) ds , ( ∫ 0 )

and (17) implies

T EV (x T ) ≤ V (x 0 ) + δ − E r 0 ( || x s − ¯x s || ) ds ( ∫ 0 )

Since V (x t ) is bounded and by hypothesis (b), there exists a positive constant L > 0, such that

T EV (x T ) ≤ L + δ + E( −C 1 V (x s ))ds ∫ 0

also, by the Gronwall inequality, there exists a positive constant C 3 > 0, such that

EV (x T ) ≤ (L + δ)C 1 e−C 3 T 

E || x T (t) − ¯x(t) || 2 ≤ C 2 e −C3t ,

dZ(t) =

ˆ MZ(t)dt + QdW(t)

ˆ

for C 2 = (L + δ)C 1 , obtaining the turnpike property for the state x(t).

Now, in order to show a sort of converse of the previous theorem, we will consider certain techniques used in [1] and [10]:

We consider a perturbation of variables x(t), u(t), p(t), q(t) as follows, x T (t) = ¯x(t) + δx(t), u T (t) = ¯u(t) + δu(t), p T (t) = 
¯p(t) + δp(t), q T (t) = ¯q(t) + ψ (t)δq(t) + x(t)u(t) ∫ R 3 γ (t, x, z)¯N(dt, dz) δq(t).

By using the Hamiltonian perturbed and the stochastic Maximum Principle with jumps [11], [14], we obtain, extending the idea in [10] 
for the system considered here:



From: ohmega@cbv.net
Newsgroups: cult.cbv.discuss
Message-ID: <67BC2AC4.F4C0@cbv.net>
Date: 19 Jul 19106 17:21:07
X-Organization: Cult of the Bound Variable
Subject: Ray Tracing


Dearest friends,

I have discovered the most amazing application of the Computing
Device! It is a simulator that replicates the human viewing
interaction of geometry. I call it a "Ray Tracer" because it operates
along a ray.

== Ray Tracing ==

Suppose the eye is looking along a one-dimensional ray, and can see a
series of n zero-dimensional surfaces. Each surface has four qualities:

 * D, the direction it faces (either Towards or Away from the eye)
 * R, its reflectance
 * T, its translucence
 * E, its emission

We call these surfaces S1, S2, ... Sn, where S1 is closest to the eye.
Adjacent to each surface are two ray segments; the ones pointing
towards the eye are called Li and the ones facing away are called Ri.
The rays adjacent to Sj that are closer to the eye are called L(j-1)
and R(j-1); the two farther away are called Lj and Rj.

       L0        L1        L2              Ln
  eye <=====> * <=====> * <=====> * ... * <=====> (empty space)
          R0  S1    R1  S2    R2  S3    Sn    Rn 

Each ray segment has an intensity value determined by a set of
equations. Ray Tracing consists of determining the value of the ray
L0, which is the intensity that the eye sees.

Intensity can take on three values: All, Medium, and None. These are
also the values that the R, T, and E components of surfaces can take
on. The following tables define the operations + and * on these
values.

   +   | None   Medium   All
------------------------------
None   | None   Medium   All
Medium | Medium All      All
All    | All    All      All

   *   | None   Medium   All
------------------------------
None   | None   None     None
Medium | None   Medium   Medium
All    | None   Medium   All


The rays satisfy the following equations:

Ln = None
R0 = None

For i such that 0 <= i < n:
Li = if S(i + 1).D = Towards
     then (S(i + 1).R * Ri) +
          (S(i + 1).T * L(i + 1)) +
          S(i + 1).E
     else L(i + 1)

For i such that 0 < i <= n:
Ri = if Si.D = Away
     then (Si.R * Li) +
          (Si.T * R(i - 1)) +
          Si.E
     else R(i - 1)

== Representation ==

The ray-tracer consists of a module "main" with a single input along
the direction N, which is the sequence of surfaces. Its output is
a single intensity value, the darkest correct value of L0.

Intensity values are represented as follows:
None    =   Inl ()
Medium  =   Inr Inl ()
All     =   Inr Inr Inl ()

Surface orientations are represented as follows:
Towards =   Inl ()
Away    =   Inr ()

A single surface with components D, R, T and E is represented 
as follows:

(D, (R, (T, E)))

The list of surfaces S1..Sn is represented as follows:

Inl(S1, Inl(S2, Inl(S3, ... Inl(Sn, Inr()) ... )))

---------------------------------------------
 Bill Ohmega      "Hell is other programming
ohmega@cbv.net     languages." -- Sartran
---------------------------------------------

datatype 'a Inl = Inl of 'a
datatype 'a Inr = Inr of 'a
datatype bool = False of unit Inl
              | True of unit Inr
datatype intensity = None of unit Inl
                   | Medium of unit Inl Inr
                   | All of unit Inl Inr Inr
datatype orientation = Towards of unit Inl
                     | Away of unit Inr
type surface = (* D *) orientation * ((* R *) intensity * ((* T *) intensity * (* E *) intensity))
datatype 'a list = Cons of ('a * 'a list) Inl
                 | Nil of unit Inr
fun add x y = case x of
                  None (Inl _) => y
                | Medium (Inr (Inl _)) => (case y of
                                                None (Inl _) => Medium (Inr (Inl ()))
                                              | Medium (Inr _) => All (Inr (Inr (Inl ())))
                                              | All (Inr _) => All (Inr (Inr (Inl ())))
                                           )
                | All (Inr (Inr _)) => All (Inr (Inr (Inl ())))
(*
rm rt_add.2d
/bin/umodem rt_add.2d STOP
,.......|..................................,
:A      | *=================*              :
---+    +>!send[(W,E),(W,S)]!+             :
:  v      *=================*v             :
:*=============*| *===========*            :
:!case N of E,S!#>!send[(N,E)]!-------------
:*=============*| *===========*            :
:   |           |                          :
:   v           |                          :
:*=============*| *=======================*:
:!case N of S,E!#>!send[(Inr Inr Inl(),E)]!-
:*=============*v *=======================*:
:++*=============*  *===================*  :
:+>!case N of E,S!->!send[(Inr Inl(),E)]!---
:  *=============*  *===================*  :
:        | *=======================*       :
:        +>!send[(Inr Inr Inl(),E)]!--------
:          *=======================*       :
,..........................................,
,..............................,
:main   *===================*  :
:       !send[(Inr Inl(),E)]!+ :
:       *===================*v :
:*===================*  *=====*:
:!send[(Inr Inl(),E)]!->!use A!-
:*===================*  *=====*:
,..............................,STOP
2d rt_add.2d
 *)
fun mul x y = case x of
                  None (Inl _) => None (Inl ())
                | Medium (Inr (Inl _)) => (case y of
                                               None (Inl _) => None (Inl ())
                                             | Medium (Inr _) => Medium (Inr (Inl ()))
                                             | All (Inr _) => Medium (Inr (Inl ()))
                                          )
                | All (Inr (Inr _)) => y
(*
rm rt_mul.2d
/bin/umodem rt_mul.2d STOP
,.................|..................,
:M *=============*| *===============*:
-->!case W of E,S!#>!send[(Inl(),E)]!-
:  *=============*v *===============*:
:   | *=================*            :
:   | !send[(N,S),(N,E)]!+           :
:   v *=================*v           :
:*=============*| *===========*      :
:!case N of S,E!#>!send[(N,E)]!-------
:*=============*v *===========*      :
:++*=============*  *===============*:
:+>!case N of E,S!->!send[(Inl(),E)]!-
:  *=============*  *===============*:
:   | *===================*          :
:   +>!send[(Inr Inl(),E)]!-----------
:     *===================*          :
,....................................,
,..............................,
:main   *===================*  :
:       !send[(Inr Inl(),E)]!+ :
:       *===================*v :
:*===================*  *=====*:
:!send[(Inr Inl(),E)]!->!use M!-
:*===================*  *=====*:
,..............................,STOP
2d rt_mul.2d
*)
fun zip (xs : 'a list) (ys : 'b list) : ('a * 'b) list
    = case xs of
          Nil (Inr ()) => Nil (Inr ())
        | Cons (Inl (x, xs')) => case ys of
                                     Nil (Inr ()) => Nil (Inr ())
                                   | Cons (Inl (y, ys')) => let val zs = zip xs' ys'
                                                            in Cons (Inl ((x, y), zs))
                                                            end
(*
ys xs
rm rt_zip.2d
/bin/umodem rt_zip.2d STOP
,..|.............................................,
:Z | *=============*  *===============*          :
:  +>!case W of S,E!->!send[(Inr(),E)]!-----------
:    *=============*  *===============*          :
:     |                *=============*           :
:     v              +>!case W of S,E!+          :
: *=================*| *=============*v          :
->!send[(N,S),(W,E)]!+  |       *===============*:
: *=================*   |       !send[(Inr(),E)]!-
:    |              +---#------+*===============*:
:    v    +---------+   v      ++                :
:*=======*|         *=======*   v                :
:!split N!+         !split N!+ *=====*           :
:*=======*        ++*=======*+>!use Z!+          :
:++               v+-+         *=====*v          :
:| *===============*   *==================*      :
:+>!send[((W,N),E)]!-->!send[(Inl(W,N),E)]!-------
:  *===============*   *==================*      :
,................................................,
STOP
2d rt_zip.2d
*)
fun head (xs : intensity list) : intensity = case xs of
                                                 Nil (Inr _) => None (Inl ()) (* impossible *)
                                               | Cons (Inl (x, _)) => x
(*
rm rt_head.2d
/bin/umodem rt_head.2d STOP
,.....|...........................,
:H    v            *=======*      :
: *=============*+>!split W!+     :
->!case N of E,S!+ *=======*v     :
: *=============*  ++*===========*:
:  |               +>!send[(W,E)]!-
:  |                 *===========*:
:  +-------------------------------
,.................................,
STOP
2d rt_head.2d
*)
fun tail (xs : 'a list) : 'a list = case xs of
                                        Nil (Inr ()) => Nil (Inr ()) (* impossible *)
                                      | Cons (Inl (x, xs')) => xs'
(*
rm rt_tail.2d
/bin/umodem rt_tail.2d STOP
,..|.............................,
:T v              *=======*      :
:*=============*+>!split W!+     :
:!case N of E,S!+ *=======*v     :
:*=============*  ++*===========*:
: |               +>!send[(N,E)]!-
: |                 *===========*:
: +-------------------------------
,................................,
STOP
2d rt_tail.2d
*)
fun scanr (f : 'a -> 'b -> 'b) (z : 'b) (xs : 'a list)
    = case xs of
          Nil (Inr ()) => Cons (Inl (z, Nil (Inr ())))
        | Cons (Inl (x, xs')) => let val ys = scanr f z xs'
                                 in case ys of
                                        Nil (Inr ()) => Nil (Inr ()) (* impossible *)
                                      | Cons (Inl (y, _)) => Cons (Inl (f x y, ys))
                                 end
fun L init (rs : (intensity * surface) list) : intensity list
    = scanr (fn (r, s) => fn l' => let val (D, (R, (T, E))) = s
                                   in case D of
                                          Towards (Inl ()) => add (mul R r) (add (mul T l') E)
                                        | Away (Inr ()) => l'
                                   end) init rs
fun LL x l' = let val (r, s) = x
                  val (D, (R, (T, E))) = s
              in case D of
                     Towards (Inl ()) => add (mul R r) (add (mul T l') E)
                   | Away (Inr ()) => l'
              end
(*
x    l'
rm rt_LL.2d
/bin/umodem rt_LL.2d STOP
,....|..........................................,
:l   | *=================*                      :
---+ +>!send[(W,S),(W,E)]!------+               :
:  v   *=================*      |               :
:*=======*| *=======*  *=======*| *=======*     :
:!split N!#>!split W!->!split W!#>!split W!-+   :
:*=======*| *=======*  *=======*| *=======* |   :
:       +-#----#----+    |     ++   |       |   :
:+--------+    |  +-+    v   +-+    v       |   :
:|             v  | *=====*  | *=====*      |   :
:|*=============* +>!use M!+ +>!use M!+     |   :
:|!case N of E,S!+  *=====*v   *=====*|     v   :
:|*=============*| *===========*     ++  *=====*:
:|  |            +>!send[(N,S)]!     v +>!use A!-
:|  v              *===========**=====*| *=====*:
:| *===========*              +>!use A!+        :
:+>!send[(W,E)]!+               *=====*         :
:  *===========*+--------------------------------
,...............................................,
STOP
2d rt_LL.2d
*)
fun L z (xs : (intensity * surface) list) : intensity list
    = case xs of
          Nil (Inr ()) => Cons (Inl (z, Nil (Inr ())))
        | Cons (Inl (x, xs')) => let val ys = L z xs'
                                 in case ys of
                                        Nil (Inr ()) => Nil (Inr ()) (* impossible *)
                                      | Cons (Inl (l', _)) => Cons (Inl (LL x l', ys))
                                 end
(*
xs  z
rm rt_L.2d
/bin/umodem rt_L.2d STOP
,...|.....................................................,
:L  | *=================*+-+                              :
--+ +>!send[(W,S),(W,E)]!+ v                              :
: v   *=================* *======================*        :
:*=============*|       +>!send[(Inl(N,Inr()),E)]!---------
:!case N of S,E!#-------+ *======================*        :
:*=============*|                          *=============*:
: |             v    *=================* +>!case W of S,E!-
: v         *=====*+>!send[(W,E),(W,S)]!-+ *=============*:
:*=======*+>!use L!+ *=================*+---+             :
:!split N!+ *=====*                +----#----+            :
:*=======*                              v    v            :
: |                              *=======*  *===========* :
: |                              !split N!->!send[(N,S)]! :
: |                              *=======*  *===========* :
: |                              ++          |            :
: |                              v           v            :
: |                         *=====*  *==================* :
: +------------------------>!use l!->!send[(Inl(W,N),E)]!--
:                           *=====*  *==================* :
,.........................................................,
STOP
2d rt_L.2d
*)
fun scanl (f : 'b -> 'a -> 'b) (z : 'b) (xs : 'a list) : 'b list
    = let val ys = case xs of
                       Nil (Inr ()) => Nil (Inr ())
                     | Cons (Inl (x, xs')) => scanl f (f z x) xs'
      in Cons (Inl (z, ys))
      end
fun R init (ls : (intensity * surface) list) : intensity list
    = scanl (fn r' => fn (l, s) => let val (D, (R, (T, E))) = s
                                   in case D of
                                          Towards (Inl ()) => r'
                                        | Away (Inr ()) => add (mul R l) (add (mul T r') E)
                                   end) init ls
fun RR r' x = let val (l, s) = x
                  val (D, (R, (T, E))) = s
              in case D of
                     Towards (Inl ()) => r'
                   | Away (Inr ()) => add (mul R l) (add (mul T r') E)
              end
(*
x    r'
rm rt_RR.2d
/bin/umodem rt_RR.2d STOP
,....|..........................................,
:r   | *=================*                      :
---+ +>!send[(W,S),(W,E)]!------+               :
:  v   *=================*      |               :
:*=======*| *=======*  *=======*| *=======*     :
:!split N!#>!split W!->!split W!#>!split W!-+   :
:*=======*| *=======*  *=======*| *=======* |   :
:       +-#----#----+    |     ++   |       |   :
:+--------+    |  +-+    v   +-+    v       |   :
:|             v  | *=====*  | *=====*      |   :
:|*=============* +>!use M!+ +>!use M!+     |   :
:|!case N of S,E!+  *=====*v   *=====*|     v   :
:|*=============*| *===========*     ++  *=====*:
:|  |            +>!send[(N,S)]!     v +>!use A!-
:|  v              *===========**=====*| *=====*:
:| *===========*              +>!use A!+        :
:+>!send[(W,E)]!+               *=====*         :
:  *===========*+--------------------------------
,...............................................,
STOP
2d rt_RR.2d
*)
fun R z (xs : (intensity * surface) list) : intensity list
    = let val ys = case xs of
                       Nil (Inr ()) => Nil (Inr ())
                     | Cons (Inl (x, xs')) => R (RR z x) xs'
      in Cons (Inl (z, ys))
      end
(*
z  xs
rm rt_R.2d
/bin/umodem rt_R.2d STOP
,..|................................................,
:R |           *=================*                  :
---#---------->!send[(W,S),(W,E)]!+                 :
:  v           *=================*v                 :
:*=============*| *======================*          :
:!case N of S,E!#>!send[(Inl(N,Inr()),E)]!-----------
:*=============*| *======================*          :
: | *=======*  ++                                   :
: +>!split W!--#-----+                              :
:   *=======*  v     v                              :
:       | *=====*  *=====*  *======================*:
:       +>!use r!->!use R!->!send[(Inl(W,Inr()),E)]!-
:         *=====*  *=====*  *======================*:
,...................................................,
STOP
2d rt_R.2d
*)
fun eqIntensity (x : intensity) (y : intensity)
    = case x of
          None (Inl _) => (case y of
                               None (Inl _) => True (Inr ())
                             | _ => False (Inl ())
                          )
        | Medium (Inr (Inl _)) => (case y of
                                       None (Inl _) => False (Inl ())
                                     | Medium (Inr (Inl _)) => True (Inr ())
                                     | All (Inr (Inr _)) => False (Inl ())
                                  )
        | All (Inr (Inr _)) => (case y of
                                    None (Inl _) => False (Inl ())
                                  | Medium (Inr (Inl _)) => False (Inl ())
                                  | All (Inr (Inr _)) => True (Inr ())
                               )
(*
rm rt_eqIntensity.2d
/bin/umodem rt_eqIntensity.2d STOP
,.........|...........................................,
:E        | *=================*                       :
---+      +>!send[(W,E),(W,S)]!+   *===============*  :
:  v        *=================*v +>!send[(Inr(),E)]!---
:*=============*| *=============*| *===============*  :
:!case N of E,S!#>!case N of E,S!+  *===============* :
:*=============*v *=============* +>!send[(Inl(),E)]!--
: |        *=================* +--+ *===============* :
: |        !send[(N,E),(N,S)]!+    *===============*  :
: v        *=================*v  +>!send[(Inl(),E)]!---
:*=============*| *=============*| *===============*  :
:!case N of E,S!#>!case N of E,S!+   *===============*:
:*=============*| *=============*  +>!send[(Inr(),E)]!-
: |             | ++*=============*++*===============*:
: |             | +>!case W of E,S!-+*===============*:
: |             |   *=============*+>!send[(Inl(),E)]!-
: |             v                +-+ *===============*:
:++*=============*                   *===============*:
:+>!case N of E,S!------------------>!send[(Inl(),E)]!-
:  *=============*                   *===============*:
:               +--------------------------------------
,.....................................................,
STOP
2d rt_eqIntensity.2d
*)
fun eqList (xs : intensity list) (ys : intensity list)
    = case xs of
          Nil (Inr _) => (case ys of
                              Nil (Inr _) => True (Inr ())
                            | Cons (Inl _) => False (Inl ())
                         )
        | Cons (Inl (x, xs')) => (case ys of
                                      Nil (Inr _) => False (Inl ())
                                    | Cons (Inl (y, ys')) => case eqIntensity x y of
                                                                 False (Inl _) => False (Inl ())
                                                               | True (Inr _) => eqList xs' ys'
                                 )
(*
rm rt_eqList.2d
/bin/umodem rt_eqList.2d STOP
,..|...............................................,
:Q |          *=================*                  :
---#--------->!send[(W,E),(W,S)]!+                 :
:  v          *=================*v                 :
:*=============*         |      *===========*      :
:!case N of S,E!---------#----->!send[(N,E)]!-------
:*=============*         v      *===========*      :
:++*=================*  *=============*            :
:+>!send[(W,S),(W,E)]!->!case N of S,E!+           :
:  *=================*  *=============*v           :
: +-+                    |        *===============*:
: | *=======*    +-------+        !send[(Inl(),E)]!-
: +>!split W!----#--------------+ *===============*:
:   *=======*    v              |                  :
:+---+    *=======*        +--+ +------------+     :
:|        !split N!--------+  v              v     :
:|        *=======*          *===========*  *=====*:
:|      +--+*=============*+>!send[(N,E)]!->!use Q!-
:|      v +>!case W of S,E!+ *===========*  *=====*:
:| *=====*| *=============*  *===============*     :
:+>!use E!+              +-->!send[(Inl(),E)]!------
:  *=====*                   *===============*     :
,..................................................,
STOP
2d rt_eqList.2d
*)
fun buildListOfNone (xs : 'a list) : intensity list
    = case xs of
          Nil (Inr _) => Cons (Inl (None (Inl ()), Nil (Inr ())))
        | Cons (Inl (_, xs')) => Cons (Inl (None (Inl ()), buildListOfNone xs'))
(*
rm rt_build.2d
/bin/umodem rt_build.2d STOP
,...............................................,
:B *=============*  *==========================*:
-->!case W of S,E!->!send[(Inl(Inl(),Inr()),E)]!-
:  *=============*  *==========================*:
:   |       *=====*                             :
:   v     +>!use B!+                            :
:*=======*| *=====*v                            :
:!split N!+  *======================*           :
:*=======* +>!send[(Inl(Inl(),N),E)]!------------
:       +--+ *======================*           :
,...............................................,
STOP
2d rt_build.2d
*)
fun and_ x y = case x of
                   True (Inr _) => y
                 | False (Inl _) => False (Inl ())
(*
rm rt_and.2d
/bin/umodem rt_and.2d STOP
,...................|...........,
:D                  v           :
: *=============*  *===========*:
->!case W of S,E!->!send[(N,E)]!-
: *=============*  *===========*:
:            | *===============*:
:            +>!send[(Inl(),E)]!-
:              *===============*:
,...............................,
STOP
2d rt_and.2d
*)
fun fix s (l, r) = let val l' = L (None (Inl ())) (zip r s)
                       val r' = R (None (Inl ())) (zip (tail l) s)
                       val terminate = and_ (eqList l l') (eqList r r')
                   in case terminate of
                          True (Inr _) => head l'
                        | False (Inl _) => fix s (l', r')
                   end
(*
s  (l,r)
rm rt_fix.2d
/bin/umodem rt_fix.2d STOP
,..|............................................................................................,
:X | *=================*   *=========================*                                          :
:  +>!send[(W,S),(W,E)]!+  !send[(Inl(),S),(Inl(),E)]!+                                         :
:    *=================*|  *=========================*|                                         :
:          | *=======* +#--+       |                  |                                         :
--+        +>!split W!-+|  v       |                  |                                         :
: v          *=======*+-+ *=====*  |     +------------+                                         :
:*=================*|+#-->!use Z!--#--+  v                                                      :
:!send[(N,E),(N,S)]!#+|   *=====* ++  | *=====*  *=================*                            :
:*=================*v | +-+     +-+   +>!use L!->!send[(W,S),(W,E)]!+                           :
: |            *=====*| | v     |+-+    *=====*  *=================*|                           :
: |            !use T!#-+*=====*|| v                          |     v                   +-+     :
: v            *=====*|+>!use Z!#+*=====*  *=================*| *=====================* | v     :
:*=================*  || *=====*+>!use R!->!send[(W,S),(W,E)]!#>!send[(N,S),((N,W),E)]!-+*=====*:
:!send[(N,E),(N,S)]!--#+          *=====*  *=================*| *=====================*+>!use X!-
:*=================*+-#-----------------------#---------------#-----------------#-----+| *=====*:
:                 +-+ v           +-----------#---------------+                 |     |+-------+:
:                   *=======*+----#--------+  v                                 |     v        |:
:                   !split N!+    v        | *=====*             *=============*| *===========*|:
:                   *=======**=====*       +>!use Q!-------+   +>!case W of E,S!#>!send[(N,E)]!+:
:                          +>!use Q!+        *=====*       v   | *=============*v *===========* :
:                            *=====*|                   *=====*|            | *=====*           :
:                                   +------------------>!use D!+            +>!use H!------------
:                                                       *=====*               *=====*           :
,...............................................................................................,
STOP
2d rt_fix.2d
*)
fun main (* N *) s = let val init = buildListOfNone s
                     in fix s (init, init)
                     end
(*
,.....|...........................................,
:main v               *=====*  *===============*  :
:*=================*+>!use B!->!send[((W,W),E)]!+ :
:!send[(N,E),(N,S)]!+ *=====*  *===============*v :
:*=================*                       *=====*:
:                 +----------------------->!use X!-
:                                          *=====*:
,.................................................,
*)
local val Towards' = Towards (Inl ())
      val Away' = Away (Inr ())
      val None' = None (Inl ())
      val Med' = Medium (Inr (Inl ()))
      val All' = All (Inr (Inr (Inl ())))
      fun fromList xs = List.foldr (fn ((d,r,t,e),xs) => Cons (Inl ((d,(r,(t,e))),xs))) (Nil (Inr ())) xs
in
val input0 = fromList [(Towards', Med',  Med',  None')
                      ,(Towards', None', Med',  None')
                      ,(Towards', Med',  Med',  None')
                      ,(Towards', All',  Med',  None')
                      ,(Away',    Med',  Med',  None')
                      ,(Towards', All',  Med',  All')
                      ,(Away',    Med',  Med',  All')
                      ,(Towards', None', Med',  None')
                      ,(Away',    None', Med',  None')
                      ,(Away',    None', Med',  None')
                      ,(Away',    None', Med',  None')
                      ,(Towards', None', Med',  Med')
                      ,(Towards', Med',  All',  None')
                      ,(Towards', None', Med',  None')
                      ,(Towards', Med',  Med',  All')
                      ,(Away',    Med',  Med',  Med')
                      ,(Towards', None', Med',  All')
                      ]
val input1 = fromList [(Towards', None', Med',  None')
                      ,(Away',    None', Med',  All')
                      ,(Away',    None', Med',  None')
                      ,(Away',    Med',  Med',  Med')
                      ,(Away',    None', Med',  Med')
                      ,(Towards', Med',  Med',  None')
                      ,(Towards', None', None', None')
                      ,(Away',    None', Med',  Med')
                      ,(Away',    Med',  Med',  Med')
                      ,(Away',    None', Med',  None')
                      ,(Towards', All',  Med',  None')
                      ,(Away',    Med',  Med',  None')
                      ,(Towards', Med',  Med',  None')
                      ,(Away',    All',  Med',  None')
                      ,(Towards', None', None', Med')
                      ,(Away',    Med',  Med',  None')
                      ,(Towards', All',  Med',  None')
                      ,(Towards', None', Med',  None')
                      ,(Away',    None', Med',  None')
                      ,(Towards', None', Med',  None')
                      ]
end;
case main input0 of
    None (Inl ()) => print "None\n"
  | Medium (Inr (Inl ())) => print "Med\n"
  | All (Inr (Inr (Inl ()))) => print "All\n";

(*
rm raytrace.2d
/bin/umodem raytrace.2d STOP
,.......|..................................,
:A      | *=================*              :
---+    +>!send[(W,E),(W,S)]!+             :
:  v      *=================*v             :
:*=============*| *===========*            :
:!case N of E,S!#>!send[(N,E)]!-------------
:*=============*| *===========*            :
:   |           |                          :
:   v           |                          :
:*=============*| *=======================*:
:!case N of S,E!#>!send[(Inr Inr Inl(),E)]!-
:*=============*v *=======================*:
:++*=============*  *===================*  :
:+>!case N of E,S!->!send[(Inr Inl(),E)]!---
:  *=============*  *===================*  :
:        | *=======================*       :
:        +>!send[(Inr Inr Inl(),E)]!--------
:          *=======================*       :
,..........................................,
,.................|..................,
:M *=============*| *===============*:
-->!case W of E,S!#>!send[(Inl(),E)]!-
:  *=============*v *===============*:
:   | *=================*            :
:   | !send[(N,S),(N,E)]!+           :
:   v *=================*v           :
:*=============*| *===========*      :
:!case N of S,E!#>!send[(N,E)]!-------
:*=============*v *===========*      :
:++*=============*  *===============*:
:+>!case N of E,S!->!send[(Inl(),E)]!-
:  *=============*  *===============*:
:   | *===================*          :
:   +>!send[(Inr Inl(),E)]!-----------
:     *===================*          :
,....................................,
,..|.............................................,
:Z | *=============*  *===============*          :
:  +>!case W of S,E!->!send[(Inr(),E)]!-----------
:    *=============*  *===============*          :
:     |                *=============*           :
:     v              +>!case W of S,E!+          :
: *=================*| *=============*v          :
->!send[(N,S),(W,E)]!+  |       *===============*:
: *=================*   |       !send[(Inr(),E)]!-
:    |              +---#------+*===============*:
:    v    +---------+   v      ++                :
:*=======*|         *=======*   v                :
:!split N!+         !split N!+ *=====*           :
:*=======*        ++*=======*+>!use Z!+          :
:++               v+-+         *=====*v          :
:| *===============*   *==================*      :
:+>!send[((W,N),E)]!-->!send[(Inl(W,N),E)]!-------
:  *===============*   *==================*      :
,................................................,
,.....|...........................,
:H    v            *=======*      :
: *=============*+>!split W!+     :
->!case N of E,S!+ *=======*v     :
: *=============*  ++*===========*:
:  |               +>!send[(W,E)]!-
:  |                 *===========*:
:  +-------------------------------
,.................................,
,..|.............................,
:T v              *=======*      :
:*=============*+>!split W!+     :
:!case N of E,S!+ *=======*v     :
:*=============*  ++*===========*:
: |               +>!send[(N,E)]!-
: |                 *===========*:
: +-------------------------------
,................................,
,....|..........................................,
:l   | *=================*                      :
---+ +>!send[(W,S),(W,E)]!------+               :
:  v   *=================*      |               :
:*=======*| *=======*  *=======*| *=======*     :
:!split N!#>!split W!->!split W!#>!split W!-+   :
:*=======*| *=======*  *=======*| *=======* |   :
:       +-#----#----+    |     ++   |       |   :
:+--------+    |  +-+    v   +-+    v       |   :
:|             v  | *=====*  | *=====*      |   :
:|*=============* +>!use M!+ +>!use M!+     |   :
:|!case N of E,S!+  *=====*v   *=====*|     v   :
:|*=============*| *===========*     ++  *=====*:
:|  |            +>!send[(N,S)]!     v +>!use A!-
:|  v              *===========**=====*| *=====*:
:| *===========*              +>!use A!+        :
:+>!send[(W,E)]!+               *=====*         :
:  *===========*+--------------------------------
,...............................................,
,...|.....................................................,
:L  | *=================*+-+                              :
--+ +>!send[(W,S),(W,E)]!+ v                              :
: v   *=================* *======================*        :
:*=============*|       +>!send[(Inl(N,Inr()),E)]!---------
:!case N of S,E!#-------+ *======================*        :
:*=============*|                          *=============*:
: |             v    *=================* +>!case W of S,E!-
: v         *=====*+>!send[(W,E),(W,S)]!-+ *=============*:
:*=======*+>!use L!+ *=================*+---+             :
:!split N!+ *=====*                +----#----+            :
:*=======*                              v    v            :
: |                              *=======*  *===========* :
: |                              !split N!->!send[(N,S)]! :
: |                              *=======*  *===========* :
: |                              ++          |            :
: |                              v           v            :
: |                         *=====*  *==================* :
: +------------------------>!use l!->!send[(Inl(W,N),E)]!--
:                           *=====*  *==================* :
,.........................................................,
,....|..........................................,
:r   | *=================*                      :
---+ +>!send[(W,S),(W,E)]!------+               :
:  v   *=================*      |               :
:*=======*| *=======*  *=======*| *=======*     :
:!split N!#>!split W!->!split W!#>!split W!-+   :
:*=======*| *=======*  *=======*| *=======* |   :
:       +-#----#----+    |     ++   |       |   :
:+--------+    |  +-+    v   +-+    v       |   :
:|             v  | *=====*  | *=====*      |   :
:|*=============* +>!use M!+ +>!use M!+     |   :
:|!case N of S,E!+  *=====*v   *=====*|     v   :
:|*=============*| *===========*     ++  *=====*:
:|  |            +>!send[(N,S)]!     v +>!use A!-
:|  v              *===========**=====*| *=====*:
:| *===========*              +>!use A!+        :
:+>!send[(W,E)]!+               *=====*         :
:  *===========*+--------------------------------
,...............................................,
,..|................................................,
:R |           *=================*                  :
---#---------->!send[(W,S),(W,E)]!+                 :
:  v           *=================*v                 :
:*=============*| *======================*          :
:!case N of S,E!#>!send[(Inl(N,Inr()),E)]!-----------
:*=============*| *======================*          :
: | *=======*  ++                                   :
: +>!split W!--#-----+                              :
:   *=======*  v     v                              :
:       | *=====*  *=====*  *======================*:
:       +>!use r!->!use R!->!send[(Inl(W,Inr()),E)]!-
:         *=====*  *=====*  *======================*:
,...................................................,
,.........|...........................................,
:E        | *=================*                       :
---+      +>!send[(W,E),(W,S)]!+   *===============*  :
:  v        *=================*v +>!send[(Inr(),E)]!---
:*=============*| *=============*| *===============*  :
:!case N of E,S!#>!case N of E,S!+  *===============* :
:*=============*v *=============* +>!send[(Inl(),E)]!--
: |        *=================* +--+ *===============* :
: |        !send[(N,E),(N,S)]!+    *===============*  :
: v        *=================*v  +>!send[(Inl(),E)]!---
:*=============*| *=============*| *===============*  :
:!case N of E,S!#>!case N of E,S!+   *===============*:
:*=============*| *=============*  +>!send[(Inr(),E)]!-
: |             | ++*=============*++*===============*:
: |             | +>!case W of E,S!-+*===============*:
: |             |   *=============*+>!send[(Inl(),E)]!-
: |             v                +-+ *===============*:
:++*=============*                   *===============*:
:+>!case N of E,S!------------------>!send[(Inl(),E)]!-
:  *=============*                   *===============*:
:               +--------------------------------------
,.....................................................,
,..|...............................................,
:Q |          *=================*                  :
---#--------->!send[(W,E),(W,S)]!+                 :
:  v          *=================*v                 :
:*=============*         |      *===========*      :
:!case N of S,E!---------#----->!send[(N,E)]!-------
:*=============*         v      *===========*      :
:++*=================*  *=============*            :
:+>!send[(W,S),(W,E)]!->!case N of S,E!+           :
:  *=================*  *=============*v           :
: +-+                    |        *===============*:
: | *=======*    +-------+        !send[(Inl(),E)]!-
: +>!split W!----#--------------+ *===============*:
:   *=======*    v              |                  :
:+---+    *=======*        +--+ +------------+     :
:|        !split N!--------+  v              v     :
:|        *=======*          *===========*  *=====*:
:|      +--+*=============*+>!send[(N,E)]!->!use Q!-
:|      v +>!case W of S,E!+ *===========*  *=====*:
:| *=====*| *=============*  *===============*     :
:+>!use E!+              +-->!send[(Inl(),E)]!------
:  *=====*                   *===============*     :
,..................................................,
,...............................................,
:B *=============*  *==========================*:
-->!case W of S,E!->!send[(Inl(Inl(),Inr()),E)]!-
:  *=============*  *==========================*:
:   |       *=====*                             :
:   v     +>!use B!+                            :
:*=======*| *=====*v                            :
:!split N!+  *======================*           :
:*=======* +>!send[(Inl(Inl(),N),E)]!------------
:       +--+ *======================*           :
,...............................................,
,...................|...........,
:D                  v           :
: *=============*  *===========*:
->!case W of S,E!->!send[(N,E)]!-
: *=============*  *===========*:
:            | *===============*:
:            +>!send[(Inl(),E)]!-
:              *===============*:
,...............................,
,..|............................................................................................,
:X | *=================*   *=========================*                                          :
:  +>!send[(W,S),(W,E)]!+  !send[(Inl(),S),(Inl(),E)]!+                                         :
:    *=================*|  *=========================*|                                         :
:          | *=======* +#--+       |                  |                                         :
--+        +>!split W!-+|  v       |                  |                                         :
: v          *=======*+-+ *=====*  |     +------------+                                         :
:*=================*|+#-->!use Z!--#--+  v                                                      :
:!send[(N,E),(N,S)]!#+|   *=====* ++  | *=====*  *=================*                            :
:*=================*v | +-+     +-+   +>!use L!->!send[(W,S),(W,E)]!+                           :
: |            *=====*| | v     |+-+    *=====*  *=================*|                           :
: |            !use T!#-+*=====*|| v                          |     v                   +-+     :
: v            *=====*|+>!use Z!#+*=====*  *=================*| *=====================* | v     :
:*=================*  || *=====*+>!use R!->!send[(W,S),(W,E)]!#>!send[(N,S),((N,W),E)]!-+*=====*:
:!send[(N,E),(N,S)]!--#+          *=====*  *=================*| *=====================*+>!use X!-
:*=================*+-#-----------------------#---------------#-----------------#-----+| *=====*:
:                 +-+ v           +-----------#---------------+                 |     |+-------+:
:                   *=======*+----#--------+  v                                 |     v        |:
:                   !split N!+    v        | *=====*             *=============*| *===========*|:
:                   *=======**=====*       +>!use Q!-------+   +>!case W of E,S!#>!send[(N,E)]!+:
:                          +>!use Q!+        *=====*       v   | *=============*v *===========* :
:                            *=====*|                   *=====*|            | *=====*           :
:                                   +------------------>!use D!+            +>!use H!------------
:                                                       *=====*               *=====*           :
,...............................................................................................,
,.....|...........................................,
:main v               *=====*  *===============*  :
:*=================*+>!use B!->!send[((W,W),E)]!+ :
:!send[(N,E),(N,S)]!+ *=====*  *===============*v :
:*=================*                       *=====*:
:                 +----------------------->!use X!-
:                                          *=====*:
,.................................................,STOP
verify raytrace raytrace.2d
*)

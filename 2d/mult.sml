datatype num = Inl of num | Inr of unit
fun plus x y = case y of
                   Inl y' => Inl (plus x y')
                 | Inr () => x
(*
rm plus2.2d
/bin/umodem plus2.2d STOP
,......................|................,
:P *=================* |                :
-->!send[(W,S),(W,E)]!-#---------+      :
:  *=================* v         v      :
:       | *=============*  *===========*:
:       | !case N of S,E!->!send[(N,E)]!-
:       | *=============*  *===========*:
:       |       |                       :
:       |       v                       :
:       |    *=====*  *===============* :
:       +--->!use P!->!send[(Inl W,E)]!--
:            *=====*  *===============* :
,.......................................,
,..............................................................,
:main                                                          :
:                                                              :
:  *================================================*          :
:  !send [(Inl Inl Inl Inr (),E),(Inl Inl Inr (),S)]!--+       :
:  *================================================*  |       :
:                   |                                  v       :
:                   |                             *=====*      :
:                   +---------------------------->!use P!-------
:                                                 *=====*      :
,..............................................................,
STOP
2d plus2.2d
---
rm plus2.2d
/bin/umodem plus2.2d STOP
,.....................|................,
:P *=================*|                :
-->!send[(W,S),(W,E)]!#----------+     :
:  *=================*v          v     :
:      | *=============*  *===========*:
:      | !case N of S,E!->!send[(N,E)]!-
:      v *=============*  *===========*:
:*===============*  |                  :
:!send[(Inl N,S)]!  v                  :
:*===============**=====*              :
:       +-------->!use P!---------------
:                 *=====*              :
,......................................,
,..............................................................,
:main                                                          :
:                                                              :
:  *================================================*          :
:  !send [(Inl Inl Inl Inr (),E),(Inl Inl Inr (),S)]!--+       :
:  *================================================*  |       :
:                   |                                  v       :
:                   |                             *=====*      :
:                   +---------------------------->!use P!-------
:                                                 *=====*      :
,..............................................................,STOP
2d plus2.2d
*)
fun mult x y = case y of
                   Inl y' => plus x (mult x y')
                 | Inr () => Inr ()
(*
rm mult.2d
/bin/umodem mult.2d STOP
,......................|................,
:P *=================* |                :
-->!send[(W,S),(W,E)]!-#---------+      :
:  *=================* v         v      :
:       | *=============*  *===========*:
:       | !case N of S,E!->!send[(N,E)]!-
:       | *=============*  *===========*:
:       |       |                       :
:       |       v                       :
:       |    *=====*  *===============* :
:       +--->!use P!->!send[(Inl W,E)]!--
:            *=====*  *===============* :
,.......................................,
,.............|..............................,
:mult         v                              :
:          *=============*  *===============*:
:          !case N of S,E!->!send[(Inr W,E)]!-
:          *=============*  *===============*:
:                       |                    :
:                       v                    :
: *=================*  *========*            :
->!send[(W,S),(W,E)]!->!use mult!---+        :
: *=================*  *========*   v        :
:              |                 *=====*     :
:              +---------------->!use P!------
:                                *=====*     :
,............................................,
STOP
verify mult mult.2d
---
rm mult.2d
/bin/umodem mult.2d STOP
,......................|................,
:P *=================* |                :
-->!send[(W,S),(W,E)]!-#---------+      :
:  *=================* v         v      :
:       | *=============*  *===========*:
:       | !case N of S,E!->!send[(N,E)]!-
:       | *=============*  *===========*:
:       |       |                       :
:       |       v                       :
:       |    *=====*  *===============* :
:       +--->!use P!->!send[(Inl W,E)]!--
:            *=====*  *===============* :
,.......................................,
,.............|............................,
:mult         v                            :
:        *=============*  *===============*:
:        !case N of S,E!->!send[(Inr W,E)]!-
:        *=============*  *===============*:
: *=================* |                    :
->!send[(W,S),(W,E)]!-#-------------+      :
: *=================* v             v      :
:              |   *========*    *=====*   :
:              +-->!use mult!--->!use P!----
:                  *========*    *=====*   :
,..........................................,
STOP
verify mult mult.2d
---
rm mult.2d
/bin/umodem mult.2d STOP
,.....................|................,
:P *=================*|                :
-->!send[(W,S),(W,E)]!#----------+     :
:  *=================*v          v     :
:      | *=============*  *===========*:
:      | !case N of S,E!->!send[(N,E)]!-
:      v *=============*  *===========*:
:*===============*  |                  :
:!send[(Inl N,S)]!  v                  :
:*===============**=====*              :
:       +-------->!use P!---------------
:                 *=====*              :
,......................................,
,............|............................,
:mult        v                            :
:       *=============*  *===============*:
:       !case N of S,E!->!send[(Inr W,E)]!-
:       *=============*  *===============*:
: *=================*|                    :
->!send[(W,S),(W,E)]!#-------------+      :
: *=================*v             v      :
:              |   *========*   *=====*   :
:              +-->!use mult!-->!use P!----
:                  *========*   *=====*   :
,.........................................,STOP
verify mult mult.2d
---
rm mult.2d
/bin/umodem mult.2d STOP
,.....................|................,
:P *=================*|                :
-->!send[(W,S),(W,E)]!#----------+     :
:  *=================*v          v     :
:      | *=============*  *===========*:
:      | !case N of S,E!->!send[(N,E)]!-
:      v *=============*  *===========*:
:*===============*  |                  :
:!send[(Inl N,S)]!  v                  :
:*===============**=====*              :
:       +-------->!use P!---------------
:                 *=====*              :
,......................................,
,.....|...................................,
:mult | *=============*  *===============*:
--+   +>!case W of S,E!->!send[(Inr W,E)]!-
: v     *=============*  *===============*:
:*=================*|                     :
:!send[(N,S),(N,E)]!#----+                :
:*=================*v    v                :
:       |   *========*  *=====*           :
:       +-->!use mult!->!use P!------------
:           *========*  *=====*           :
,.........................................,STOP
verify mult mult.2d
program area=1032
---
rm mult.2d
/bin/umodem mult.2d STOP
,.....................|................,
:P *=================*|                :
-->!send[(W,S),(W,E)]!#----------+     :
:  *=================*v          v     :
:      | *=============*  *===========*:
:      | !case N of S,E!->!send[(N,E)]!-
:      v *=============*++*===========*:
:*===============*    +-+v             :
:!send[(Inl N,S)]!      *=====*        :
:*===============* +--->!use P!---------
:       +----------+    *=====*        :
,......................................,
,.....|...................................,
:mult | *=============*  *===============*:
--+   +>!case W of S,E!->!send[(Inr W,E)]!-
: v     *=============*  *===============*:
:*=================*|                     :
:!send[(N,S),(N,E)]!#----+                :
:*=================*v    v                :
:       |   *========*  *=====*           :
:       +-->!use mult!->!use P!------------
:           *========*  *=====*           :
,.........................................,STOP
verify mult mult.2d
program area=989
---
rm mult.2d
/bin/umodem mult.2d STOP
,..................|..................,
:P                 | *=============*  :
--+                +>!case W of S,E!+ :
: v                  *=============*v :
:*=================*   | *===========*:
:!send[(N,S),(N,E)]!---#>!send[(W,E)]!-
:*=================*   v *===========*:
:++*===============*  *=====*         :
:+>!send[(Inl W,E)]!->!use P!----------
:  *===============*  *=====*         :
,.....................................,
,.....|...................................,
:mult | *=============*  *===============*:
--+   +>!case W of S,E!->!send[(Inr W,E)]!-
: v     *=============*  *===============*:
:*=================*|                     :
:!send[(N,S),(N,E)]!#----+                :
:*=================*v    v                :
:       |   *========*  *=====*           :
:       +-->!use mult!->!use P!------------
:           *========*  *=====*           :
,.........................................,STOP
verify mult mult.2d
program area=946
39*11 & 43*11
43*22=946
---
rm mult.2d
/bin/umodem mult.2d STOP
,..................|..................,,.....|...................................,
:P                 | *=============*  ::mult | *=============*  *===============*:
--+                +>!case W of S,E!+ :--+   +>!case W of S,E!->!send[(Inr W,E)]!-
: v                  *=============*v :: v     *=============*  *===============*:
:*=================*   | *===========*::*=================*|                     :
:!send[(N,S),(N,E)]!---#>!send[(W,E)]!-:!send[(N,S),(N,E)]!#----+                :
:*=================*   v *===========*::*=================*v    v                :
:++*===============*  *=====*         ::       |   *========*  *=====*           :
:+>!send[(Inl W,E)]!->!use P!----------:       +-->!use mult!->!use P!------------
:  *===============*  *=====*         ::           *========*  *=====*           :
,.....................................,,.........................................,STOP
verify mult mult.2d
program area=902
*)

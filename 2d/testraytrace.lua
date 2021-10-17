local None = "Inl()"
local Medium = "Inr Inl()"
local Med = Medium
local All = "Inr Inr Inl()"
local Towards = "Inl()"
local Away = "Inr()"
local function list(t)
  local s = "Inr()"
  for i=#t,1,-1 do
    s = "Inl("..t[i]..","..s..")"
  end
  return s
end
local function surface(D,R,T,E)
  return "("..D..",("..R..",("..T..","..E..")))"
end
local function table_rep(x,n)
  local t = {}
  for i = 1,n do
    t[i] = x
  end
  return t
end

--
-- Add/Mul
--
local code = [[
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
]]
local code = [[
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
]]
local inputN = {
  "Inl()",
  "Inr Inl()",
  "Inr Inr Inl()",
}
local inputW = {
  "Inl()",
  "Inr Inl()",
  "Inr Inr Inl()",
}

--
-- Zip
--
local code = [[
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
]]
-- xs
local inputN = {
  "Inr()",
  "Inl((),Inr())",
  "Inl((),Inl((),Inl((),Inr())))",
}
-- ys
local inputW = {
  "Inr()",
  "Inl(Inl(),Inr())",
  "Inl(Inl(),Inl(Inl(),Inl(Inl(),Inr())))",
}

--
-- Head
--
local code = [[
,.....|...........................,
:H    v            *=======*      :
: *=============*+>!split W!+     :
->!case N of E,S!+ *=======*v     :
: *=============*  ++*===========*:
:  |               +>!send[(W,E)]!-
:  |                 *===========*:
:  +-------------------------------
,.................................,
]]
-- xs
local inputN = {
  "Inl((),Inr())",
  "Inl((Inl(),Inr()),Inr())",
  "Inl(Inr(),Inl((),Inl((),Inr())))",
}
-- dummy
local inputW = {
  "()",
}

--
-- And
--
local code = [[
,...................|...........,
:D                  v           :
: *=============*  *===========*:
->!case W of S,E!->!send[(N,E)]!-
: *=============*  *===========*:
:            | *===============*:
:            +>!send[(Inl(),E)]!-
:              *===============*:
,...............................,
]]
local inputN = {
  "Inl()", -- false
  "Inr()", -- true
}
local inputW = {
  "Inl()", -- false
  "Inr()", -- true
}

--
-- EqIntensity
--
local code = [[
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
]]
local inputN = {
  None,
  Medium,
  All,
}
local inputW = {
  None,
  Medium,
  All,
}

--
-- EqList
--
local code = [[
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
]]
local inputN = {
  list{},
  list{None,Medium,All},
  list{Medium,None,All,None},
  list{Medium,None,All,All},
}
local inputW = {
  list{},
  list{None,Medium,All},
  list{Medium,None,All,None},
  list{Medium,None,All,All},
}

--
-- Tail
--
local code = [[
,...|.............................,
:T  v              *=======*      :
: *=============*+>!split W!+     :
->!case N of E,S!+ *=======*v     :
: *=============*  ++*===========*:
:  |               +>!send[(N,E)]!-
:  |                 *===========*:
:  +-------------------------------
,.................................,
]]
-- xs
local inputN = {
  list{"()"},
  list{"(Inl(),Inr())"},
  list{"Inr()","()","()"},
}
-- dummy
local inputW = {
  "()",
}

--
-- fix
--
local code = [[
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
,..|............................................,
:R | *=================*  *=================*   :
---#>!send[(W,S),(W,E)]!->!send[(W,S),(W,E)]!+  :
:  v *=================*  *=================*|  :
:*=============*|          |                 |  :
:!case N of S,E!#+         v                 |  :
:*=============*|+-+ *======================*|  :
: | *=======*  ++  +>!send[(Inl(N,Inr()),E)]!#---
: +>!split W!--#----+*======================*|  :
:   *=======*  v    v                        v  :
:       | *=====*  *=====*  *==================*:
:       +>!use r!->!use R!->!send[(Inl(N,W),E)]!-
:         *=====*  *=====*  *==================*:
,...............................................,
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
,..|................................................................................................,
:X | *=================*   *=========================*                                              :
:  +>!send[(W,S),(W,E)]!+  !send[(Inl(),S),(Inl(),E)]!+                                             :
:    *=================*|  *=========================*|                                             :
:          | *=======* +#--+       |                  |                                             :
--+        +>!split W!-+|  v       |                  |                                             :
: v          *=======*+-+ *=====*  |     +------------+                                             :
:*=================*|+#-->!use Z!--#--+  v                                                          :
:!send[(N,E),(N,S)]!#+|   *=====* ++  | *=====*  *=================*                                :
:*=================*v | +-+     +-+   +>!use L!->!send[(W,S),(W,E)]!+                               :
: |            *=====*| | v     |+-+    *=====*  *=================*|                               :
: |            !use T!#-+*=====*|| v                          |     v                       +-+     :
: v            *=====*|+>!use Z!#+*=====*  *=================*| *=========================* | v     :
:*=================*  || *=====*+>!use R!->!send[(W,S),(W,E)]!#>!send[((N,W),S),((N,W),E)]!-+*=====*:
:!send[(N,E),(N,S)]!--#+          *=====*  *=================*| *=========================*+>!use X!-
:*=================*+-#-----------------------#---------------#-----------------#---------+| *=====*:
:                 +-+ v           +-----------#---------------+                 |         |+-------+:
:                   *=======*+----#--------+  v                                 |         v        |:
:                   !split N!+    v        | *=====*             *=============*| *===========*    |:
:                   *=======**=====*       +>!use Q!-------+   +>!case W of E,S!#>!send[(N,E)]!----+:
:                          +>!use Q!+        *=====*       v   | *=============*v *===========*     :
:                            *=====*|                   *=====*|            | *===========*         :
:                                   +------------------>!use D!+            +>!send[(N,E)]!----------
:                                                       *=====*               *===========*         :
,...................................................................................................,
]]
-- (l,r), s
local inputs = {
  {"("..list(table_rep(None,18))..","..list(table_rep(None,18))..")",
  list{surface(Towards,Med,Med,None),
       surface(Towards,None,Med,None),
       surface(Towards,Med,Med,None),
       surface(Towards,All,Med,None),
       surface(Away,Med,Med,None),
       surface(Towards,All,Med,All),
       surface(Away,Med,Med,All),
       surface(Towards,None,Med,None),
       surface(Away,None,Med,None),
       surface(Away,None,Med,None),
       surface(Away,None,Med,None),
       surface(Towards,None,Med,Med),
       surface(Towards,Med,All,None),
       surface(Towards,None,Med,None),
       surface(Towards,Med,Med,All),
       surface(Away,Med,Med,Med),
       surface(Towards,None,Med,All),
  },
  }
}

local p = io.popen("../aarch64/run_jit --discard-initial-output --input ohmega --input bidirectional ../umix.um", "w")
-- for _,N in ipairs(inputN) do
-- for _,W in ipairs(inputW) do
for _,t in ipairs(inputs) do
  local N = t[1]
  local W = t[2]
  p:write("rm test.2d\n")
  p:write("/bin/umodem test.2d STOP\n")
  p:write(code)
  local width = math.max(#N, #W) + 23
  local test = {}
  table.insert(test, ","..string.rep(".", width-2)..",\n")
  table.insert(test, ":main"..string.rep(" ", width-6)..":\n")
  table.insert(test, ":*"..string.rep("=", #N+10).."*"..string.rep(" ", width-#N-14)..":\n")
  table.insert(test, ":!send[("..N..",E)]!"..string.rep("-", width-#N-16).."+ :\n")
  table.insert(test, ":*"..string.rep("=", #N+10).."*"..string.rep(" ", width-#N-16).."v :\n")
  table.insert(test, ":*"..string.rep("=", #W+10).."*"..string.rep(" ", width-#W-21).."*=====*:\n")
  table.insert(test, ":!send[("..W..",E)]!"..string.rep("-", width-#W-22)..">!use X!-\n")
  table.insert(test, ":*"..string.rep("=", #W+10).."*"..string.rep(" ", width-#W-21).."*=====*:\n")
  table.insert(test, ","..string.rep(".", width-2)..",\n")
  p:write(table.concat(test,""))
  p:write("STOP2d test.2d\n")
end
p:close()
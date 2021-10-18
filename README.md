# My solutions to ICFP Programming Contest 2006 "The Cult of the Bound Variable"

<http://boundvariable.org/>

## The Universal Machine

* [run.c](run.c)
    * Simple VM using `switch`.
* [run_threaded.c](run_threaded.c)
    * Threaded VM using computed goto.
* [run_threaded_switch.c](run_threaded_switch.c)
    * Threaded (?) VM using `switch`.
* [run_directthreaded.c](run_directthreaded.c)
    * Direct-threaded VM using computed goto.
* [run_directthreaded_tailcall.c](run_directthreaded_tailcall.c)
    * Direct-threaded VM using tail calls.
* [aarch64/run_jit.c](aarch64/run_jit.c)
    * JIT-compiling VM for AArch64.

参考：

* [YARV Maniacs 【第 3 回】 命令ディスパッチの高速化](https://magazine.rubyist.net/articles/0008/0008-YarvManiacs.html)
* [JITあれこれ | κeenのHappy Hacκing Blog](https://keens.github.io/blog/2018/12/01/jitarekore/)
* [WASM3の末尾呼び出しVMがかしこい - Qiita](https://qiita.com/okuoku/items/ed52f48ce871f429759e)

## codex.umz

* [extract.lua](extract.lua)
    * Extract `umix.um` from `codex.umz`'s output. Use as `make umix.um`.
* [extract-CBV.hs](extract-CBV.hs)
    * Extract CBV's logo as a PNG image.

![](CBV.png)

## QVICKBASIC (BASIC)

* [hack3.bas](hack3.bas)
    * A program to obtain passwords for `ftd`, `ohmega`, and `howie`.

## Adventure (ADVTR)

See [adventure/README.md](adventure/README.md).

## 2D (CIRCS)

* [2d/mult.sml](2d/mult.sml)
* [2d/reverse.sml](2d/reverse.sml)
* [2d/raytrace.sml](2d/raytrace.sml)

## Antomaton (ANTWO)

* [antomaton/puzzle1sol.txt](antomaton/puzzle1sol.txt)
* [antomaton/puzzle2sol.txt](antomaton/puzzle2sol.txt)
* [antomaton/puzzle3sol.txt](antomaton/puzzle3sol.txt)
* [antomaton/puzzle5sol.txt](antomaton/puzzle5sol.txt)
* [antomaton/puzzle15sol.txt](antomaton/puzzle15sol.txt)

## Balance (BLNCE)

See [balance/README.md](balance/README.md).

## Black Knots (BLACK)

See [blackknots/README.md](blackknots/README.md).

## Advise / O'Cult (ADVIS)

* [ocult/arith.adv](ocult/arith.adv)
* [ocult/xml.adv](ocult/xml.adv)

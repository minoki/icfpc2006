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

参考：

* [YARV Maniacs 【第 3 回】 命令ディスパッチの高速化](https://magazine.rubyist.net/articles/0008/0008-YarvManiacs.html)
* [JITあれこれ | κeenのHappy Hacκing Blog](https://keens.github.io/blog/2018/12/01/jitarekore/)
* [WASM3の末尾呼び出しVMがかしこい - Qiita](https://qiita.com/okuoku/items/ed52f48ce871f429759e)

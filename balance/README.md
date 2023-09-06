* [x] stop (10 / 10)
* [x] stop1 (10 / 10)
* [x] stop127 (20 / 20)
* [x] stop128 (15 / 15)
* [ ] copymem (0 / 200)
* [ ] copyreg (0 / 200)
* [x] swapmem (27 / 40)
* [x] swapreg (50 / 50)
* [x] swapreg2 (50 / 50)
* [x] addmem (40 / 40)
* [x] addmem2 (48 / 50)
* [ ] multmem (0 / 200)
* [ ] fillmem (0 / 200)
* [ ] clearreg (0 / 100)

# stop

```
PHYSICS 16
SCIENCE 0
```

```
/bin/umodem stop.bal STOP
70
00
STOP
certify stop stop.bal
```

# stop1

```
PHYSICS 1
PHYSICS 1
SCIENCE 0
```

```
/bin/umodem stop1.bal STOP
61
61
00
STOP
certify stop1 stop1.bal
```

# stop1 / stop127 / stop128

```
SCIENCE 0
PHYSICS 1
```

```
/bin/umodem stop1.bal STOP
00
61
STOP
certify stop1 stop1.bal
```

# stop128

```
PHYSICS (-16)
SCIENCE 0
```

```
/bin/umodem stop128.bal STOP
70
00
STOP
certify stop128 stop128.bal
```

# swapreg

```
PHYSICS (-16)
PHYSICS 3
SCIENCE 0
```

```
/bin/umodem swapreg.bal STOP
70
63
00
STOP
certify swapreg swapreg.bal
```

# swapreg2

```
PHYSICS 2
PHYSICS (-13)
PHYSICS 1
PHYSICS 13
SCIENCE 0
```

```
/bin/umodem swapreg2.bal STOP
62
73
61
6d
00
STOP
certify swapreg2 swapreg2.bal
```

# addmem / addmem2

```
PHYSICS 1
PHYSICS (-3)
PHYSICS (-1)
PHYSICS 1
MATH 2 0 2
SCIENCE 0
```

```
/bin/umodem addmem.bal STOP
61
7d
7f
61
22
00
STOP
certify addmem addmem.bal
certify addmem2 addmem.bal
```

score: 31 (addmem), 48 (addmem2)

Shorter code for addmem:

```
PHYSICS 2
PHYSICS (-4)
MATH 0 0 3
SCIENCE 0
```

```
/bin/umodem addmem.bal STOP
62
7c
23
00
STOP
certify addmem addmem.bal
```

score: 40 (addmem)

# swapmem

```
PHYSICS 1
PHYSICS (-13)
PHYSICS (-1)
PHYSICS (-4)
LOGIC 0 1 2
PHYSICS (-15)
PHYSICS (-2)
LOGIC 1 0 1
PHYSICS (-16)
PHYSICS (-1)
LOGIC 0 0 2
SCIENCE 0
```

```
/bin/umodem swapmem.bal STOP
61
73
7f
7c
46
71
7e
51
70
7f
42
00
STOP
certify swapmem swapmem.bal
```

score: 27

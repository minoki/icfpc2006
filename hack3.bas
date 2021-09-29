rm hack3.bas
/bin/umodem hack3.bas STOP
LXV      IF ARGS() > I THEN GOTO LXXXV
LXX      PRINT "usage: ./hack.exe username"
LXXV     PRINT CHR(X)
LXXX     END
XCV      DIM username AS STRING
C        username = ARG(II)
CX       DIM pwdcount AS INTEGER
CXV      pwdcount = LIII
CXX      DIM words(pwdcount) AS STRING
CXXV     words(I) = "airplane"
CXXX     words(II) = "alphabet"
CXXXV    words(III) = "aviator"
CXL      words(IV) = "bidirectional"
CXLV     words(V) = "changeme"
CL       words(VI) = "creosote"
CLV      words(VII) = "cyclone"
CLX      words(VIII) = "december"
CLXV     words(IX) = "dolphin"
CLXX     words(X) = "elephant"
CLXXV    words(XI) = "ersatz"
CLXXX    words(XII) = "falderal"
CLXXXV   words(XIII) = "functional"
CXC      words(XIV) = "future"
CXCV     words(XV) = "guitar"
CC       words(XVI) = "gymnast"
CCV      words(XVII) = "hello"
CCX      words(XVIII) = "imbroglio"
CCXV     words(XIX) = "january"
CCXX     words(XX) = "joshua"
CCXXV    words(XXI) = "kernel"
CCXXX    words(XXII) = "kingfish"
CCXXXV   words(XXIII) = "(\b.bb)(\v.vv)"
CCXL     words(XXIV) = "millennium"
CCXLV    words(XXV) = "monday"
CCL      words(XXVI) = "nemesis"
CCLV     words(XXVII) = "oatmeal"
CCLX     words(XXVIII) = "october"
CCLXV    words(XXIX) = "paladin"
CCLXX    words(XXX) = "pass"
CCLXXV   words(XXXI) = "password"
CCLXXX   words(XXXII) = "penguin"
CCLXXXV  words(XXXIII) = "polynomial"
CCXC     words(XXXIV) = "popcorn"
CCXCV    words(XXXV) = "qwerty"
CCC      words(XXXVI) = "sailor"
CCCV     words(XXXVII) = "swordfish"
CCCX     words(XXXVIII) = "symmetry"
CCCXV    words(XXXIX) = "system"
CCCXX    words(XL) = "tattoo"
CCCXXV   words(XLI) = "thursday"
CCCXXX   words(XLII) = "tinman"
CCCXXXV  words(XLIII) = "topography"
CCCXL    words(XLIV) = "unicorn"
CCCXLV   words(XLV) = "vader"
CCCL     words(XLVI) = "vampire"
CCCLV    words(XLVII) = "viper"
CCCLX    words(XLVIII) = "warez"
CCCLXV   words(XLIX) = "xanadu"
CCCLXX   words(L) = "xyzzy"
CCCLXXV  words(LI) = "zephyr"
CCCLXXX  words(LII) = "zeppelin"
CCCLXXXV words(LIII) = "zxcvbnm"
CCCXCV   PRINT "attempting hack with " + pwdcount + " passwords " + CHR(X)
CD       DIM i AS INTEGER
CDV      i = I
CDX      IF CHECKPASS(username, words(i)) THEN GOTO CDXXX
CDXV     i = i + I
CDXX     IF i > pwdcount THEN GOTO CDXLV
CDXXV    GOTO CDX
CDXXX    PRINT "found match!! for user " + username + CHR(X)
CDXXXV   PRINT "password: " + words(i) + CHR(X)
CDXL     END
CDXLV    PRINT "no simple matches for user " + username + CHR(X)
CDXC     i = I
CDXCV    DIM j AS INTEGER
D        IF i > pwdcount THEN GOTO DLV
DV       j = XLVIII
DVI      DIM k AS INTEGER
DVII     k = XLVIII
DX       IF CHECKPASS(username, words(i) + CHR(j) + CHR(k)) THEN GOTO DXL
DXI      k = k + I
DXII     IF k < LVIII THEN GOTO DX
DXV      j = j + I
DXVI     IF j < LVIII THEN GOTO DVII
DXXX     i = i + I
DXXXV    GOTO D
DXL      PRINT "found match!! for user " + username + CHR(X)
DXLV     PRINT "password: " + words(i) + CHR(j) + CHR(k) + CHR(X)
DL       END
DLV      PRINT "no match for user " + username + CHR(X)
DLX      END
STOP
/bin/qbasic hack3.bas

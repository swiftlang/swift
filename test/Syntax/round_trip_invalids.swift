// To know about setup, see `tokens_invalids.swift`.

// RUN: cat %s > %t
// RUN: cat %t | sed 's/'$(echo -ne "\x5a1")'/'$(echo -ne "\xc2")'/g' > %t.sed
// RUN: cp -f %t.sed %t
// RUN: cat %t | sed 's/'$(echo -ne "\x5a2")'/'$(echo -ne "\xcc\x82")'/g' > %t.sed
// RUN: cp -f %t.sed %t
// RUN: cat %t | sed 's/'$(echo -ne "\x5a3")'/'$(echo -ne "\xe2\x80\x9d")'/g' > %t.sed
// RUN: cp -f %t.sed %t
// RUN: cat %t | sed 's/'$(echo -ne "\x5a4")'/'$(echo -ne "\xe2\x80\x9c")'/g' > %t.sed
// RUN: cp -f %t.sed %t
// RUN: cat %t | sed 's/'$(echo -ne "\x5a5")'/'$(echo -ne "\xe1\x9a\x80")'/g' > %t.sed
// RUN: cp -f %t.sed %t

// RUN: %round-trip-syntax-test --swift-syntax-test %swift-syntax-test --file %t

x
Z1 x
Z2
Z3
Z4
Z4 abcdef Z3
Z5 x

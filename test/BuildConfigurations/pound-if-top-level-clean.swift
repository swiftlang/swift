// RUN: %swift -D FOO %s -verify -parse

var x = 0

#if FOO
x = 1
#else
x = 2
#endif


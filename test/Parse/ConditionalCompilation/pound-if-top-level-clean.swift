// RUN: %target-typecheck-verify-swift -D FOO

var x = 0

#if FOO
x = 1
#else
x = 2
#endif


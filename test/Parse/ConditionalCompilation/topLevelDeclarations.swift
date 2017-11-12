// RUN: %target-typecheck-verify-swift -D FOO

// This test is meant to ensure that mixes of top-level declarations, statements
// and expressions within #if blocks parse correctly
var x = 0
#if FOO
class C{}
x = x+1
#endif

#if FOO
x = x+1
class D{}
#endif

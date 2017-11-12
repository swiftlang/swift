// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// <rdar://problem/13995785> We were using the wrong type metadata pointer for the empty
// tuple.

func dup<T>(_ x: T) -> (T, T) {
  return (x, x)
}

func drop<T>(_ x: T) {}

var x = ()
drop(dup(x))
print("ok") // CHECK: ok

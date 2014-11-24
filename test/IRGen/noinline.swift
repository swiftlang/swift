// RUN: %swift -target x86_64-apple-macosx10.9 %s -O -emit-ir | FileCheck %s
// XFAIL: linux

public var x = 0

@inline(never) func foo() {
  x = 1
}

foo()

// CHECK: call {{.*foo}}


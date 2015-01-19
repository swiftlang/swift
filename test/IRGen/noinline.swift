// RUN: %target-swift-frontend %s -O -emit-ir | FileCheck %s

public var x = 0

@inline(never) func foo() {
  x = 1
}

foo()

// CHECK: call {{.*foo}}


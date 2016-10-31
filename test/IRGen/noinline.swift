// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -O -emit-ir | %FileCheck %s

public var x = 0

@inline(never) func foo() {
  x = 1
}

foo()

// CHECK: call {{.*foo}}


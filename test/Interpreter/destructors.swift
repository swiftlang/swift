// RUN: %target-run-simple-swift | FileCheck %s

class C {

  deinit {
    println("boom! roasted")
  }
}

class D : C {

  deinit {
    println("i can't decide between a fat joke and a dumb joke")
  }
}

C()
// CHECK: boom! roasted
D()
// CHECK-NEXT: i can't decide between a fat joke and a dumb joke
// CHECK-NEXT: boom! roasted


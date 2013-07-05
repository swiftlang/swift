// RUN: %swift -i %s | FileCheck %s

class C {
  constructor() {}

  destructor {
    println("boom! roasted")
  }
}

class D : C {
  destructor {
    println("i can't decide between a fat joke and a dumb joke")
  }
}

C()
// CHECK: boom! roasted
D()
// CHECK-NEXT: i can't decide between a fat joke and a dumb joke
// CHECK-NEXT: boom! roasted


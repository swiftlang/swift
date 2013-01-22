// RUN: %swift -sil-i %s | FileCheck %s

class C {
  constructor() {}

  destructor {
    println("boom! roasted")
  }
}

new C()
// CHECK: boom! roasted

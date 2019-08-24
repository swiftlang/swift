// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

class C {

  deinit {
    print("boom! roasted")
  }
}

class D : C {

  deinit {
    print("i can't decide between a fat joke and a dumb joke")
  }
}

@inline(never)
func createC() -> C {
  return C()
}

@inline(never)
func createD() -> C {
  return D()
}

// CHECK: boom! roasted
createC()
// CHECK-NEXT: i can't decide between a fat joke and a dumb joke
// CHECK-NEXT: boom! roasted
createD()


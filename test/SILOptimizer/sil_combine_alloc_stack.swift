// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

protocol E {
  func f() -> Bool
}

final class K {
  deinit {
    print("deinit")
  }
}


struct X : E {
  var x: K
  func f() -> Bool { return true }
}

func g<T>(_ x : T) -> Bool {
  if let y = x as? E { return y.f() }
  return false
}

// CHECK that there is no use-after-free in this function.
@inline(never)
func foo(_ x: X) -> Bool {
  return g(x)
}

@inline(never)
func testit() {
  let x = X(x: K())
  _ = foo(x)
  print(x)
}

// CHECK: X(x: a.K)
// CHECK: deinit
testit()



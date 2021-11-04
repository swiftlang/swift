// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-lexical-lifetimes -Xfrontend -enable-copy-propagation) | %FileCheck %s

// REQUIRES: executable_test

// =============================================================================
// = Declarations                                                           {{ =
// =============================================================================

class C {
  init(_ d: D) {
    self.d = d
  }
  weak var d: D?
  func foo(_ string: String) {
    d?.cWillFoo(self, string)
  }
}
class D {
  func cWillFoo(_ c: C, _ string: String) {
    print(#function, string)
  }
}

// =============================================================================
// = Declarations                                                           }} =
// =============================================================================

// =============================================================================
// = Tests                                                                  {{ =
// =============================================================================

func test_localLetKeepsObjectAliveBeyondCallToClassWithWeakReference() {
  let d = D()
  let c = C(d)
  // CHECK: cWillFoo{{.*}} test_localLetKeepsObjectAliveBeyondCallToClassWithWeakReference
  c.foo(#function)
}

func test_localVarKeepsObjectAliveBeyondCallToClassWithWeakReference() {
  var d = D()
  let c = C(d)
  // CHECK: cWillFoo{{.*}} test_localVarKeepsObjectAliveBeyondCallToClassWithWeakReference
  c.foo(#function)
}

// =============================================================================
// = Tests                                                                  }} =
// =============================================================================

func run() {
  test_localLetKeepsObjectAliveBeyondCallToClassWithWeakReference()
  test_localVarKeepsObjectAliveBeyondCallToClassWithWeakReference()
}

run()


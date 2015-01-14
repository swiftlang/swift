// Counterpart to local_types_1.swift, used to test that two partial modules,
// each with their own local types, merge their local types with the same
// discriminators without loss of information necessary for debugging.

public func singleNestingFunc3() {
  // Identically-named nominal types in this function
  // should not collide with those in globalfunc() - they should be
  // uniqued with the hash of this file's basename.
  struct S {
    let i: Int
    func foo() {}
  }
  class C {
    let i = 2
    func foo() {}
  }
  enum E {
    case I(Int)
    func foo() {}
  }
}

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-function-caller-generator Test %t/test.swift > %t/out.swift
// RUN: %diff %t/out.swift %t/out.swift.expected

// When a base type declares a function that a derived type does not, a caller
// is still emitted for calling that (inherited) method on the derived type.
// When the function is declared in both with the same signature, an additional
// caller is emitted that invokes the base implementation via `super` from an
// extension of the derived type. This is transitive along the inheritance
// chain. `super` is only valid in a class, so struct types only get the
// inherited-method callers.

//--- test.swift
class BaseClass {
  func nonFinalShared(x: Int) -> Int
  final func finalBaseOnly() -> Int
  func baseOnly() -> Int
}

class DerivedClass : BaseClass {
  func nonFinalShared(x: Int) -> Int
  func derivedOnly() -> Int
}

class LeafClass : DerivedClass {
  func nonFinalShared(x: Int) -> Int
}

struct BaseStruct {
  func structShared() -> Int
  func structBaseOnly() -> Int
}

struct DerivedStruct : BaseStruct {
  func structShared() -> Int
}

//--- out.swift.expected
import Test


extension BaseClass {
  func call_nonFinalShared(x: Int) -> Int {
    return nonFinalShared(x: x)
  }
  func call_finalBaseOnly() -> Int {
    return finalBaseOnly()
  }
  func call_baseOnly() -> Int {
    return baseOnly()
  }
}

extension DerivedClass {
  func call_nonFinalShared(x: Int) -> Int {
    return nonFinalShared(x: x)
  }
  func call_derivedOnly() -> Int {
    return derivedOnly()
  }
}

extension LeafClass {
  func call_nonFinalShared(x: Int) -> Int {
    return nonFinalShared(x: x)
  }
}

extension BaseStruct {
  func call_structShared() -> Int {
    return structShared()
  }
  func call_structBaseOnly() -> Int {
    return structBaseOnly()
  }
}

extension DerivedStruct {
  func call_structShared() -> Int {
    return structShared()
  }
}

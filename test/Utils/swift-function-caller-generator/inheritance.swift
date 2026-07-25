// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-function-caller-generator Test %t/test.swift > %t/out.swift
// RUN: %diff %t/out.swift %t/out.swift.expected

//--- test.swift
class BaseClass {
  func nonFinalShared(x: Int) -> Int
  final func finalBaseOnly() -> Int
  func baseOnly() -> Int
}

class DerivedClass : BaseClass {
  override func nonFinalShared(x: Int) -> Int
  func derivedOnly() -> Int
}

class LeafClass : DerivedClass {
  override func nonFinalShared(x: Int) -> Int
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
  func call_nonFinalShared_DerivedClass(x: Int) -> Int {
    return nonFinalShared(x: x)
  }
  func call_derivedOnly() -> Int {
    return derivedOnly()
  }
}

extension LeafClass {
  func call_nonFinalShared_LeafClass(x: Int) -> Int {
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

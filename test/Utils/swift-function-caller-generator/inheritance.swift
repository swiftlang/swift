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
  final func call_nonFinalShared_BaseClass(x: Int) -> Int {
    return nonFinalShared(x: x)
  }
  final func call_finalBaseOnly_BaseClass() -> Int {
    return finalBaseOnly()
  }
  final func call_baseOnly_BaseClass() -> Int {
    return baseOnly()
  }
}

extension DerivedClass {
  final func call_nonFinalShared_DerivedClass(x: Int) -> Int {
    return nonFinalShared(x: x)
  }
  final func call_derivedOnly_DerivedClass() -> Int {
    return derivedOnly()
  }
  final func call_nonFinalShared_BaseClass_super(x: Int) -> Int {
    return super.nonFinalShared(x: x)
  }
  final func call_finalBaseOnly_BaseClass_super() -> Int {
    return super.finalBaseOnly()
  }
  final func call_baseOnly_BaseClass_super() -> Int {
    return super.baseOnly()
  }
}

extension LeafClass {
  final func call_nonFinalShared_LeafClass(x: Int) -> Int {
    return nonFinalShared(x: x)
  }
  final func call_nonFinalShared_DerivedClass_super(x: Int) -> Int {
    return super.nonFinalShared(x: x)
  }
  final func call_derivedOnly_DerivedClass_super() -> Int {
    return super.derivedOnly()
  }
}

extension BaseStruct {
  func call_structShared_BaseStruct() -> Int {
    return structShared()
  }
  func call_structBaseOnly_BaseStruct() -> Int {
    return structBaseOnly()
  }
}

extension DerivedStruct {
  func call_structShared_DerivedStruct() -> Int {
    return structShared()
  }
}

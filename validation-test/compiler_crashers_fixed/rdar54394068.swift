// RUN: %target-swift-frontend -typecheck -verify %s

protocol A {
  associatedtype BType: B where BType.AType == Self
  associatedtype CType: C where CType.AType == Self

  var b: BType { get }
  var c: CType { get set }

  func bGetter() -> BType
  mutating func cSetter(_ newC: CType)

  subscript (b: BType) -> CType { get set }
}

protocol B {
  associatedtype AType: A
}

protocol C {
  associatedtype AType: A
}

struct AImpl: A {
  typealias BType = BImpl
  typealias CType = CImpl

  let b: BImpl
  var c: CImpl

  func bGetter() -> BImpl {
    return b
  }

  mutating func cSetter(_ newC: CImpl) {
    c = newC
  }

  subscript(b: BImpl) -> CImpl {
    get {
      return c
    }
    set {
      c = newValue
    }
  }
}

struct BImpl: B {
  typealias AType = AImpl
}

struct CImpl: C {
  typealias AType = AImpl
}

// RUN: %target-typecheck-verify-swift

protocol Runcible {
  associatedtype Runcee
}

class Mince {
  init() {}
}

class Spoon : Runcible {
  init() {}

  typealias Runcee = Mince
}

class Owl<T:Runcible> {
  init() {}

  func eat(_ what: T.Runcee, with: T) { }
}

func owl1() -> Owl<Spoon> {
  return Owl<Spoon>()
}

func owl2() -> Owl<Spoon> {
  return Owl()
}

func owl3() {
  Owl<Spoon>().eat(Mince(), with:Spoon())
}

// https://github.com/apple/swift/issues/43341
// Can't access associated types through class-constrained generic parameters
func spoon<S: Spoon>(_ s: S) {
  let _: S.Runcee?
}

// https://github.com/apple/swift/issues/46726

protocol SameTypedDefault {
    associatedtype X
    associatedtype Y
    static var x: X { get }
    static var y: Y { get }
}
extension SameTypedDefault where Y == X {
    static var x: X {
        return y
    }
}

struct UsesSameTypedDefault: SameTypedDefault {
    static var y: Int {
        return 0
    }
}

protocol XReqt {}
protocol YReqt {}

protocol SameTypedDefaultWithReqts {
    associatedtype X: XReqt // expected-note{{protocol requires nested type 'X'}}
    associatedtype Y: YReqt // expected-note{{protocol requires nested type 'Y'}}
    static var x: X { get }
    static var y: Y { get }
}
extension SameTypedDefaultWithReqts where Y == X {
    static var x: X {
        return y
    }
}

struct XYType: XReqt, YReqt {}
struct YType: YReqt {}

struct UsesSameTypedDefaultWithReqts: SameTypedDefaultWithReqts {
    static var y: XYType { return XYType() }
}

// expected-note@+2 {{add stubs for conformance}}
// expected-error@+1{{does not conform}}
struct UsesSameTypedDefaultWithoutSatisfyingReqts: SameTypedDefaultWithReqts {
    static var y: YType { return YType() }
}

protocol SameTypedDefaultBaseWithReqts {
    associatedtype X: XReqt // expected-note{{protocol requires nested type 'X'}}
    static var x: X { get }
}
protocol SameTypedDefaultDerivedWithReqts: SameTypedDefaultBaseWithReqts {
    associatedtype Y: YReqt
    static var y: Y { get }
}

extension SameTypedDefaultDerivedWithReqts where Y == X {
    static var x: X {
        return y
    }
}

struct UsesSameTypedDefaultDerivedWithReqts: SameTypedDefaultDerivedWithReqts {
    static var y: XYType { return XYType() }
}

// expected-note@+2 {{add stubs for conformance}}
// expected-error@+1{{does not conform}}
struct UsesSameTypedDefaultDerivedWithoutSatisfyingReqts: SameTypedDefaultDerivedWithReqts {
    static var y: YType { return YType() }
}

// https://github.com/apple/swift/issues/54624

protocol P1_54624 {
  associatedtype Assoc // expected-note {{'Assoc' declared here}}
}

enum E_54624 {}

protocol P2_54624: P1_54624 where Assoc == E_54624 {
  associatedtype Assoc: E_54624 // expected-error {{type 'Self.Assoc' constrained to non-protocol, non-class type 'E_54624'}}
  // expected-warning@-1 {{redeclaration of associated type 'Assoc' from protocol 'P1_54624' is better expressed as a 'where' clause on the protocol}}
}

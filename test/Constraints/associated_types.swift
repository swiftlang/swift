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

// "Can't access associated types through class-constrained generic parameters"
// (https://bugs.swift.org/browse/SR-726)
func spoon<S: Spoon>(_ s: S) {
  let _: S.Runcee?
}

// SR-4143

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
    associatedtype X: XReqt // expected-note{{}}
    associatedtype Y: YReqt // expected-note{{}}
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

// expected-error@+1{{does not conform}}
struct UsesSameTypedDefaultWithoutSatisfyingReqts: SameTypedDefaultWithReqts {
    static var y: YType { return YType() }
}

protocol SameTypedDefaultBaseWithReqts {
    associatedtype X: XReqt // expected-note{{}}
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

// expected-error@+1{{does not conform}}
struct UsesSameTypedDefaultDerivedWithoutSatisfyingReqts: SameTypedDefaultDerivedWithReqts {
    static var y: YType { return YType() }
}


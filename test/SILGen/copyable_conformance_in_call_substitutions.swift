// RUN: %target-swift-frontend -emit-silgen -verify -swift-version 6 %s

func checkFunctionCall<T, Arg0>(_ lhs: T, calling f: (T, Arg0) -> Bool, _ arg: Arg0) -> Bool {
    return f(lhs, arg)
}

protocol Eq: ~Copyable {
  static func ==(_ a: borrowing Self, _ b: borrowing Self) -> Bool
}

extension Eq {
   static func ==(_ a: borrowing Self, _ b: borrowing Self) -> Bool { true }
}

extension Eq {
    func isEqual(to rhs: any Eq) -> Bool { (rhs as? Self)! == self }
}

struct S: Eq { var a: Int }

func test() {
    let lhs: any Eq = S(a: 1)
    let rhs: any Eq = S(a: 1)
    _ = checkFunctionCall(lhs, calling: { $0.isEqual(to: $1) }, rhs)
}

// RUN: %target-typecheck-verify-swift

typealias T1 = Int
typealias T2 = Float
typealias T3 = Bool

protocol P {
  associatedtype R
  static var foo: (T1, (R) -> T2) { get }
}

extension P {
  static func bind() -> (T1, (R) -> T3) {
    return (1, { _ in true })
  }
}

struct S: P {
  typealias R = T3

  static let foo: (T1, (R) -> T2) = bind()
  // expected-error@-1 {{cannot convert value of type '(T1, (S.R) -> T3)' (aka '(Int, (Bool) -> Bool)') to specified type '(T1, (S.R) -> T2)' (aka '(Int, (Bool) -> Float)')}}
}

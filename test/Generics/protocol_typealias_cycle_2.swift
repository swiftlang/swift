// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype T where T == Self
  static func f() -> T
}

extension P {
  typealias T = Self
}

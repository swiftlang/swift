// RUN: %target-typecheck-verify-swift

protocol C {
  associatedtype T : Collection where T.Element == Self
}

protocol V : C, RawRepresentable where RawValue == String {}

protocol P {
  associatedtype A: V
}

extension P {
  func foo<U: Collection>(_ args: U) -> String where U.Element == A {
    return args.reduce("", { $1.rawValue }) // Ok
  }
}

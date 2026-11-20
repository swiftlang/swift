// rdar://141296118 (https://github.com/swiftlang/swift/issues/78113)
// RUN: %target-typecheck-verify-swift -swift-version 6

func takingAnOptionalClosure(closure: (() -> Void)?) {}
func someFunc() {}

func someOtherFunc() {
  let a = false ? someFunc : nil
  takingAnOptionalClosure(closure: a)
  takingAnOptionalClosure(closure: false ? someFunc : nil) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

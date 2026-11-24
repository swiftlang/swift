// rdar://141296118 (https://github.com/swiftlang/swift/issues/78113)
// RUN: %target-typecheck-verify-swift -language-mode 5 -verify-additional-prefix swift5-
// RUN: %target-typecheck-verify-swift -swift-version 6 -verify-additional-prefix swift6-

func takingAnOptionalClosure(closure: (() -> Void)?) {}
func someFunc() {}

func someOtherFunc() {
  let a = false ? someFunc : nil
  takingAnOptionalClosure(closure: a)
  takingAnOptionalClosure(closure: false ? someFunc : nil) // expected-swift6-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

func stepForward(_ input: Int) -> Int {
  return input + 1
}

func stepBackward(_ input: Int) -> Int {
  return input - 1
}

func chooseStepFunction(backward: Bool) -> (Int) -> Int {
  backward ? stepBackward : stepForward // expected-swift6-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

func test(f: @escaping @Sendable (Int) -> Int) {
  let _: (Int) -> Int = true ? f : f // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

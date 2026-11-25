// rdar://82407752 (https://github.com/swiftlang/swift/issues/57437)
// RUN: %target-typecheck-verify-swift

struct S {
  var str: String
}

func test(s: S?) {
  let _: Int? = s?.str // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}

  func f(_: Int?) {}
  f(s?.str) // expected-error {{cannot convert value of type 'String?' to expected argument type 'Int?'}}
}

// rdar://189079546 (https://github.com/swiftlang/swift/issues/92889)
// RUN: %target-typecheck-verify-swift

func f(a: Int?) {
  if true || { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    a?.description
  } // expected-error {{expected '{' after 'if' condition}}
}

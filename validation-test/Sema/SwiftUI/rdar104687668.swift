// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15
// REQUIRES: OS=macosx

import SwiftUI

struct A {
  func bar() -> Bool { true }
}

struct B: View {
  var body: some View { fatalError() }
}

struct C<T: View>: View {
  init(@ViewBuilder _: () -> T, _: () -> Void) {}
  var body: some View { fatalError() }
}

func foo<C: Collection, R>(_: C, @ViewBuilder _: (C.Element) -> R) -> R {
  fatalError()
}

let arr: [A] = []

@ViewBuilder
func blah() -> some View {
  foo(arr) { a in
    let b = a.bar()
    C {
      if b { B() }
    } _: {
      if b { () } else { () }
    }
  }
}

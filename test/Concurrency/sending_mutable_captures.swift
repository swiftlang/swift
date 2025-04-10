// RUN: %target-swift-frontend -emit-sil -swift-version 6 %s -o /dev/null -verify

func foo() {
  var x: X? = X()
  // expected-error@+2{{sending value of non-Sendable type '() -> Void' risks causing data races}}
  // expected-note@+1{{Passing value of non-Sendable type '() -> Void' as a 'sending' argument to global function 'user' risks causing races in between local and caller code}}
  user {
    x?.bar()
    x = nil
  }
  x = nil // expected-note{{access can happen concurrently}}
}

func user(_ block: sending @escaping @isolated(any) () -> Void) {

}

final class X: Sendable {
  func bar() {}
}

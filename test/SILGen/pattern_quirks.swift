// RUN: %target-swift-emit-silgen -verify %s

struct S {
  var x: () -> Void
}

enum E {
  case a((x: S, y: S))
  case b(x: S, y: S) // expected-note {{'b(x:y:)' declared here}}
  case c(S)
}

// Make sure we can SILGen this without crashing.
struct R {
  func bar(_ x: (x: S, y: S)) {}
  func baz(_ x: S) {}

  func foo(_ x: E) {
    switch x {
    case .a(y: let y):
      self.bar(y)
    case .b(y: let y): // expected-warning {{enum case 'b' has 2 associated values; matching them as a tuple is deprecated}}
      self.bar(y)
    case .c(y: let y):
      self.baz(y)
    }
  }
}

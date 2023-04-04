// RUN: %target-swift-frontend -typecheck %s
// RUN: %target-typecheck-verify-swift -enable-lexical-borrow-scopes=false

public struct MO : ~Copyable { // expected-error {{noncopyable types require lexical borrow scopes (add -enable-lexical-borrow-scopes=true)}}
  let x = 0
}

public func whatever(_ mo: borrowing MO) {}

// RUN: %target-swift-frontend -typecheck -verify %s

class Base {
  init(noExplicitOwnership: String) {} // expected-note * {{}}
  init(consume: consuming String) {} // expected-note * {{}}
  init(borrow: borrowing String) {} // expected-note * {{}}}
  
  func noExplicitOwnership(arg: String) {} // expected-note * {{}}
  func borrow(arg: borrowing String) {} // expected-note * {{}}}
  func consume(arg: consuming String) {} // expected-note * {{}}}
}

class ExplicitImplicit: Base {
  // OK to override parameters with the same ownership spelled explicitly.
  override init(noExplicitOwnership: consuming String) { }
  override init(consume: String) { }
  
  override func noExplicitOwnership(arg: borrowing String) {}
  override func borrow(arg: String) {}
}

class ImplicitMismatch: Base {
  // Error if explicit ownership does not match default implicit ownership of
  // base.
  override init(noExplicitOwnership: borrowing String) { } // expected-error{{does not override a designated initializer}}
  override func noExplicitOwnership(arg: consuming String) { } // expected-error{{does not override any method}}
  
  // Error if default implicit ownership does not match explicit ownership of
  // base.
  override init(borrow: String) { } // expected-error{{does not override a designated initializer}}
  override func consume(arg: String) { } // expected-error{{does not override any method}}
}

class ExplicitMismatch: Base {
  override init(consume: borrowing String) { } // expected-error{{does not override a designated initializer}}
  override init(borrow: consuming String) { } // expected-error{{does not override a designated initializer}}
  
  override func consume(arg: borrowing String) { } // expected-error{{does not override any method}}
  override func borrow(arg: consuming String) { } // expected-error{{does not override any method}}
}

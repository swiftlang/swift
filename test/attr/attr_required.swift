// RUN: %target-typecheck-verify-swift

// -------------------------------------------------------------------------
// Restrictions on where required can appear
// -------------------------------------------------------------------------
required class AC { } // expected-error{{'required' may only be used on 'init' declarations}}

class C {
  required init(string s: String) { } // expected-note{{'required' initializer is declared in superclass here}}

  required var s: String // expected-error{{'required' may only be used on 'init' declarations}}

  required func f() { } // expected-error{{'required' may only be used on 'init' declarations}}
}

struct S {
  required init() { } // expected-error{{'required' initializer in non-class type 'S'}}
}

enum E {
  required init() { } // expected-error{{'required' initializer in non-class type 'E'}}
}

extension C {
  required convenience init(string2 s: String) { self.init(string:s) } // expected-error{{'required' initializer must be declared directly in class 'C' (not in an extension)}}
}

// -------------------------------------------------------------------------
// Subclass requirements for required
// -------------------------------------------------------------------------
class C2 : C { // okay: implicitly defined
}

class C2b : C {
  init() {}
} // expected-error{{'required' initializer 'init(string:)' must be provided by subclass of 'C'}}{{1-1=\n  required init(string s: String) {\n      fatalError("init(string:) has not been implemented")\n  }\n}}

class C3 : C {
  required init(string s: String) { } // expected-note{{overridden required initializer is here}}
}

class C4 : C3 {
  // implicitly required
  init(string s: String) { }
  // expected-error @-1{{'required' modifier must be present on all overrides of a required initializer}}
  // expected-note @-2{{'required' initializer is declared in superclass here}}
}

class C5 : C4 {
}

class C5b : C4 {
  init() {}
  // expected-error{{'required' initializer 'init(string:)' must be provided by subclass of 'C4'}}
  func wibble() { }
}

class C6 : C {
  init() { super.init(string: "") }
  required convenience init(string: String) { self.init() }
}


class Foo {
  required init() { }
  // expected-note@-1{{'required' initializer is declared in superclass here}}
  // expected-note@-2{{overridden required initializer is here}}
}

class Bar : Foo {
}

class Baz : Bar {
  init(i: Int) { super.init() }
} // expected-error{{'required' initializer 'init()' must be provided by subclass of 'Bar'}}

class Baz2 : Bar {
  init() { super.init() } // expected-error {{'required' modifier must be present on all overrides of a required initializer}}
}


class HasRequiredConvenienceInit {
  init() {}
  required convenience init(conveniently: Int) { self.init() } // expected-note {{here}}
}

class InheritsAllInits: HasRequiredConvenienceInit {}

class InheritsConvenienceInits: HasRequiredConvenienceInit {
  override init() {}
}

class DoesNotInheritConvenienceInits: HasRequiredConvenienceInit {
  init(value: Int) { super.init() }
} // expected-error {{'required' initializer 'init(conveniently:)' must be provided by subclass of 'HasRequiredConvenienceInit'}}

class ProvidesNewConvenienceInit: HasRequiredConvenienceInit {
  init(value: Int) { super.init() }
  required convenience init(conveniently: Int) { self.init(value: 1) }
}

class ProvidesNewDesignatedInit: HasRequiredConvenienceInit {
  required init(conveniently: Int) { super.init() }
}

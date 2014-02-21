// RUN: %swift -parse %s -verify

// -------------------------------------------------------------------------
// Restrictions on where @abstract can appear
// -------------------------------------------------------------------------
@abstract class AC { } // expected-error{{'abstract' attribute can only be applied to an initializer}}

class C {
  @abstract init withString(s: String) { } // expected-note{{'abstract' initializer with type '(withString: String)' not overridden}}

  @abstract var s: String // expected-error{{'abstract' attribute can only be applied to an initializer}}

  @abstract func f() { } // expected-error{{'abstract' attribute can only be applied to an initializer}}
}

struct S {
  @abstract init() { } // expected-error{{'abstract' initializer in non-class type 'S'}}
}

enum E {
  @abstract init() { } // expected-error{{'abstract' initializer in non-class type 'E'}}
}

extension C {
  @abstract init withString(s: String) { } // expected-error{{'abstract' initializer must be declared directly in class 'C' (not in an extension)}}
}

// -------------------------------------------------------------------------
// Subclass requirements for @abstract
// -------------------------------------------------------------------------
class C2 : C { // expected-error{{class 'C2' does not implement its superclass's abstract members}}
}

class C3 : C {
  @abstract init withString(s: String) { }
}

class C4 : C3 {
  // implicitly @abstract
  init withString(s: String) { } // expected-note{{'abstract' initializer with type '(withString: String)' not overridden}}
}

class C5 : C4 { // expected-error{{class 'C5' does not implement its superclass's abstract members}}
}




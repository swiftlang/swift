// RUN: %swift -parse %s -verify

// -------------------------------------------------------------------------
// Restrictions on where required can appear
// -------------------------------------------------------------------------
required class AC { } // expected-error{{'required' may only be used on 'init' declarations}}

class C {
  required init(string s: String) { } // expected-note{{'required' initializer 'init(string:)' not overridden}}

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

class C2b : C { // expected-error{{class 'C2b' does not implement its superclass's required members}}
  init() {}
}

class C3 : C {
  required init(string s: String) { } // expected-note{{overridden required initializer is here}}
}

class C4 : C3 {
  // implicitly required
  init(string s: String) { } // expected-error{{override of required initializer missing 'required' modifier}}{{3-3=required }}
  // expected-note @-1{{'required' initializer 'init(string:)' not overridden}}
}

class C5 : C4 {
}

class C5b : C4 { // expected-error{{class 'C5b' does not implement its superclass's required members}}
  init() {}
}

class Foo {
  required init() { }
}

class Bar : Foo {
  
}

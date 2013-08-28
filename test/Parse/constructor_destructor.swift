// RUN: %swift %s -verify

struct FooStructConstructorA {
  constructor
} // expected-error {{expected '(' for constructor parameters}}

struct FooStructConstructorB {
  constructor()
} // expected-error {{expected '{' for constructor}}

struct FooStructConstructorC {
  // FIXME: bad diagnostics.
  constructor {} // expected-error {{expected '(' for constructor parameters}} expected-error {{consecutive declarations on a line must be separated by ';'}} expected-error {{expected declaration}}
}


struct FooStructDestructorA {
  destructor
} // expected-error {{expected '{' for destructor}}

struct FooStructDestructorB {
  // FIXME: bad diagnostics.
  destructor() // expected-error {{consecutive declarations on a line must be separated by ';'}} expected-error {{expected '{' for destructor}} expected-error {{expected declaration}}
}

struct FooStructDestructorC {
  // FIXME: bad diagnostics.
  destructor() {} // expected-error {{consecutive declarations on a line must be separated by ';'}} expected-error {{expected '{' for destructor}} expected-error {{expected declaration}}
}


constructor() {} // expected-error {{'constructor' functions may only be declared within a type}}
// FIXME: bad diagnostics.
constructor {} // expected-error {{'constructor' functions may only be declared within a type}} expected-error {{expected '(' for constructor parameters}} expected-error {{braced block of statements is an unused closure}}

destructor {} // expected-error {{'destructor' functions may only be declared within a class}}
destructor() // expected-error {{expected '{' for destructor}}
destructor() {} // expected-error {{expected '{' for destructor}} expected-error {{braced block of statements is an unused closure}}

struct BarStruct {
  constructor() {}
  destructor {} // expected-error {{'destructor' functions may only be declared within a class}}
}

extension BarStruct {
  constructor(x : Int) {}

  // When/if we allow 'var' in extensions, then we should also allow dtors
  destructor {} // expected-error {{'destructor' functions may only be declared within a class}}
}

union BarUnion {
  constructor() {}
  destructor {} // expected-error {{'destructor' functions may only be declared within a class}}
}

extension BarUnion {
  constructor(x : Int) {}
  destructor {} // expected-error {{'destructor' functions may only be declared within a class}}
}

class BarClass {
  constructor() {}
  destructor {}
}

extension BarClass {
  constructor(x : Int) {}
  destructor {} // expected-error {{'destructor' functions may only be declared within a class}}
}

protocol BarProtocol {
  constructor() {} // FIXME
  destructor {} // expected-error {{'destructor' functions may only be declared within a class}}
}

extension BarProtocol { // expected-error {{protocol 'BarProtocol' cannot be extended}}
  constructor(x : Int) {}
  destructor {} // expected-error {{'destructor' functions may only be declared within a class}}
}

func fooFunc() {
  constructor() {} // expected-error {{'constructor' functions may only be declared within a type}}
  destructor {} // expected-error {{'destructor' functions may only be declared within a class}}
}

func barFunc() {
  { () -> () in
    constructor() {} // expected-error {{'constructor' functions may only be declared within a type}}
    return
  } ()

  // FIXME: bad diagnostics.
  { () -> () in // expected-error {{'() -> ()' is not convertible to '()'}}
    destructor {} // expected-error {{'destructor' functions may only be declared within a class}}
    return
  } () // expected-error {{consecutive statements on a line must be separated by ';'}}
}

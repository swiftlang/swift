// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

struct MyProps {
  var prop1 : Int {
    get async { }
  }

  var prop2 : Int {
    get throws { }
  }

  var prop3 : Int {
    get async throws { }
  }

  var prop1mut : Int {
    mutating get async { }
  }

  var prop2mut : Int {
    mutating get throws { }
  }

  var prop3mut : Int {
    mutating get async throws { }
  }
}

struct X1 {
  subscript(_ i : Int) -> Int {
      get async {}
    }
}

class X2 {
  subscript(_ i : Int) -> Int {
      get throws {}
    }
}

struct X3 {
  subscript(_ i : Int) -> Int {
      get async throws {}
    }
}

struct BadSubscript1 {
  subscript(_ i : Int) -> Int {
      get async throws {}
      set {} // expected-error {{'set' accessor is not allowed on property with 'get' accessor that is 'async' or 'throws'}}
    }
}

struct BadSubscript2 {
  subscript(_ i : Int) -> Int {
      get throws {}

      // expected-error@+2 {{'set' accessor is not allowed on property with 'get' accessor that is 'async' or 'throws'}}
      // expected-error@+1 {{'set' accessor cannot have specifier 'throws'}}
      set throws {}
    }
}

struct S {
  var prop2 : Int {
    mutating get async throws { 0 }
    nonmutating set {} // expected-error {{'set' accessor is not allowed on property with 'get' accessor that is 'async' or 'throws'}}
  }
}

var prop3 : Bool {
  // expected-error@+2 {{'_read' accessor is not allowed on property with 'get' accessor that is 'async' or 'throws'}}
  // expected-error@+1 {{variable cannot provide both a 'read' accessor and a getter}}
  _read { yield prop3 }

  // expected-note@+2 {{getter defined here}}
  // expected-note@+1 2 {{previous definition of getter here}}
  get throws { false }
  get async { true } // expected-error{{variable already has a getter}}

  get {} // expected-error{{variable already has a getter}}
}

enum E {
  private(set) var prop4 : Double {
    set {} // expected-error {{'set' accessor is not allowed on property with 'get' accessor that is 'async' or 'throws'}}
    get async throws { 1.1 }
    _modify { yield &prop4 } // expected-error {{'_modify' accessor is not allowed on property with 'get' accessor that is 'async' or 'throws'}}
  }
}

protocol P {
  associatedtype T
  var prop1 : T { get async throws }
  var prop2 : T { get async throws set } // expected-error {{'set' accessor is not allowed on property with 'get' accessor that is 'async' or 'throws'}}
  var prop3 : T { get throws set } // expected-error {{'set' accessor is not allowed on property with 'get' accessor that is 'async' or 'throws'}}
  var prop4 : T { get async }
  var prop5 : T { mutating get async throws }
  var prop6 : T { mutating get throws }
  var prop7 : T { mutating get async nonmutating set } // expected-error {{'set' accessor is not allowed on property with 'get' accessor that is 'async' or 'throws'}}
}

///////////////////
// invalid syntax

var bad1 : Int {
  get rethrows { 0 }  // expected-error{{only function declarations may be marked 'rethrows'; did you mean 'throws'?}}

  // expected-error@+1 {{'set' accessor is not allowed on property with 'get' accessor that is 'async' or 'throws'}}
  set rethrows { }   // expected-error{{'set' accessor cannot have specifier 'rethrows'}}
}

var bad2 : Int {
  get reasync { 0 }  // expected-error{{expected '{' to start getter definition}}

  set reasync { }
}

var bad3 : Int {
  _read async { yield 0 } // expected-error{{'_read' accessor cannot have specifier 'async'}}
  set(theValue) async { } // expected-error{{'set' accessor cannot have specifier 'async'}}
}


var bad4 : Int = 0 {
  // expected-error@+4 {{'willSet' accessor cannot have specifier 'throws'}}
  // expected-error@+3 {{'willSet' accessor cannot have specifier 'async'}}
  // expected-error@+2 {{'willSet' accessor cannot have specifier 'rethrows'}}
  // expected-error@+1 {{'willSet' accessor cannot have specifier 'reasync'}}
  willSet(theValue) reasync rethrows async throws {}

  // expected-error@+2 {{expected '{' to start 'didSet' definition}}
  // expected-error@+1 {{'didSet' accessor cannot have specifier 'throws'}}
  didSet throws bogus {}
}

var bad5 : Int {
  get bogus rethrows {} // expected-error{{expected '{' to start getter definition}}
}

var bad6 : Int {
  // expected-error@+2{{expected '{' to start getter definition}}
  // expected-error@+1 {{only function declarations may be marked 'rethrows'; did you mean 'throws'?}}
  get rethrows -> Int { 0 }
}

var bad7 : Double {
  get throws async { 3.14 } // expected-error {{'async' must precede 'throws'}}
}

var bad8 : Double {
  get {}
  // expected-error@+2 {{'_modify' accessor cannot have specifier 'async'}}
  // expected-error@+1 {{'_modify' accessor cannot have specifier 'throws'}}
  _modify throws async { yield &bad8 }
}

protocol BadP {
  var prop2 : Int { get bogus rethrows set } // expected-error{{expected get or set in a protocol property}}

  // expected-error@+2 {{only function declarations may be marked 'rethrows'; did you mean 'throws'?}}
  // expected-error@+1 {{expected get or set in a protocol property}}
  var prop3 : Int { get rethrows bogus set }

  // expected-error@+1 {{expected get or set in a protocol property}}
  var prop4 : Int { get reasync bogus set }

  var prop5 : Int { get throws async } // expected-error {{'async' must precede 'throws'}}
}

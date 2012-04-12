// RUN: %swift %s -verify

struct X { }
var _x : X;

// Basic parsing
var x1 : X {
  get { return _x; }
  set { _x = value; }  
}

var x2 : X {
  set (val) { _x = val; }
  get { return _x; }
}

var x3 : X {
  get { return _x; }
}

// Parsing problems
// FIXME: Redundant error below
var x4 { // expected-error{{variable with getter/setter must have an explicit type}} expected-error{{cannot infer a type for this pattern}}
  get { return _x; }
}

var (x5 : int) { // expected-error{{getter/setter can only be defined for a single variable}}
  get { return _x; }
}

var x6 : X {
  get { return _x; } // expected-note{{previous definition of getter is here}}
  set { _x = value; } // expected-note{{previous definition of setter is here}}
  get { return _x; } // expected-error{{duplicate definition of getter for 'x6'}}
  set (val) { _x = val; } // expected-error{{duplicate definition of setter for 'x6'}}
}

var x7 : X {
    get blah wibble // expected-error{{expected '{' to start getter definition}}
}

var x8 : X {
    set blah wibble // expected-error{{expected '{' to start setter definition}}
}

var x9 : X {
    set ( ) {} // expected-error{{empty parameter list is unnecessary for setter}}
    get ( ) {} // expected-error{{empty parameter list is unnecessary for getter}}
}

var x10 : X {
    set ( : ) {} // expected-error{{expected the name of the setter value}}
    get {}
    wonka // expected-error{{expected 'get' or 'set' to define variable access}}
}

var x11 : X {
    set {} // expected-error{{variable with a setter must also have a getter}}
}

var x12 : X { // expected-error{{variable with getter/setter cannot have an initializer}}
    get {}
} = X() 

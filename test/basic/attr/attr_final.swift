// RUN: %swift %s -verify

class Super {
  @final var i: Int { get { return 5 } } // expected-note{{overridden declaration is here}}
  @final func foo() { } // expected-note{{overridden declaration is here}}
  @final subscript (i: Int) -> Int { // expected-note{{overridden declaration is here}}
    get { 
      return i
    }
  }
}

class Sub : Super {
  @override var i: Int { get { return 5 } } // expected-error{{declaration overrides a 'final' declaration}}
  @override func foo() { }  // expected-error{{declaration overrides a 'final' declaration}}
  @override subscript (i: Int) -> Int {  // expected-error{{declaration overrides a 'final' declaration}}
    get { 
      return i
    }
  }
}


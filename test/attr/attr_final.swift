// RUN: %swift %s -verify

class InvalidInversion {
  @!final var i: Int = 0 // expected-error {{attribute may not be inverted}}
}

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
  override var i: Int { get { return 5 } } // expected-error{{var overrides a 'final' var}}
  override func foo() { }  // expected-error{{instance method overrides a 'final' instance method}}
  override subscript (i: Int) -> Int {  // expected-error{{subscript overrides a 'final' subscript}}
    get { 
      return i
    }
  }

  @final init() {} // expected-error {{@final may only be applied to classes, properties, methods, and subscripts}}
}


struct SomeStruct {
  @final func f() {} // expected-error {{only classes and class members may be marked with the @final attribute}}
}

struct SomeEnum {
  @final func f() {}  // expected-error {{only classes and class members may be marked with the @final attribute}}
}

extension Super {
  @final func someClassMethod() {} // ok
  
  var prop : Int {
  @final willSet { }   // expected-error {{@final cannot be applied to accessors, it must be put on the var}}
  }
}

@final func global_function() {}  // expected-error {{only classes and class members may be marked with the @final attribute}}

@final
class Super2 {
  var i: Int { get { return 5 } } // expected-note{{overridden declaration is here}}
  func foo() { } // expected-note{{overridden declaration is here}}
  subscript (i: Int) -> Int { // expected-note{{overridden declaration is here}}
    get {
      return i
    }
  }
}

class Sub2 : Super2 { //// expected-error{{inheritance from a final class 'Super2'}}
  override var i: Int { get { return 5 } } // expected-error{{var overrides a 'final' var}}
  override func foo() { }  // expected-error{{instance method overrides a 'final' instance method}}
  override subscript (i: Int) -> Int {  // expected-error{{subscript overrides a 'final' subscript}}
    get { 
      return i
    }
  }

  @final init() {} // expected-error {{@final may only be applied to classes, properties, methods, and subscripts}}
}


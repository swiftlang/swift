// RUN: %swift -parse %s -verify

class A {
  func f0() { }
  func f1() { } // expected-note{{overridden declaration is here}}

  var v1: Int
  var v2: Int // expected-note{{overridden declaration is here}}
  var v4: String

  subscript (i: Int) -> String {
    get {
      return "hello"
    }

    set {
    }
  }

  subscript (d: Double) -> String { // expected-note{{overridden declaration is here}}
    get {
      return "hello"
    }

    set {
    }
  }
}

class B : A {
  @override func f0() { }
  func f1() { } // expected-error{{overriding declaration requires an 'override' attribute}}{{3-3=@override }}
  @override func f2() { } // expected-error{{method does not override any method from its superclass}}

  @override var v1: Int
  var v2: Int // expected-error{{overriding declaration requires an 'override' attribute}}
  @override var v3: Int // FIXME: should complain about @override not overriding
  @override var v4: Int // expected-error{{cannot overload a declaration from a superclass}}

  @override subscript (i: Int) -> String {
    get {
      return "hello"
    }

    set {
    }
  }

  subscript (d: Double) -> String { // expected-error{{overriding declaration requires an 'override' attribute}}
    get {
      return "hello"
    }

    set {
    }
  }

  @override subscript (f: Float) -> String { // FIXME: should complain about @override not overriding
    get {
      return "hello"
    }

    set {
    }
  }

  @override init() { } // expected-error{{'override' attribute on a non-overridable declaration}}
  @override deinit() { } // expected-error{{'override' attribute on a non-overridable declaration}}
  @override typealias Inner = Int // expected-error{{invalid attributes specified for typealias}}
}

struct S {
  @override func f() { } // expected-error{{'override' attribute is only available on class members}}
}

enum E {
  @override func f() { } // expected-error{{'override' attribute is only available on class members}}
}

protocol P {
  @override func f() { } // expected-error{{'override' attribute is only available on class members}}
}

@override func f() { } // expected-error{{'override' attribute is only available on class members}}

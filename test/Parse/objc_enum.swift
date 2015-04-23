// RUN: %target-parse-verify-swift

@objc enum Foo: Int {
  case Zim, Zang, Zung
}

@objc enum Generic<T>: Int { // expected-error{{'@objc' enum cannot be generic}}
  case Zim, Zang, Zung
}

@objc(EnumRuntimeName) enum RuntimeNamed: Int { // expected-error{{'@objc' enum cannot have a name}}
  case Zim, Zang, Zung
}

@objc enum NoRawType { // expected-error{{'@objc' enum must declare an integer raw type}}
  case Zim, Zang, Zung
}

@objc enum NonIntegerRawType: Float { // expected-error{{'@objc' enum raw type 'Float' is not an integer type}}
  // expected-error@-1{{type 'NonIntegerRawType' does not conform to protocol 'RawRepresentable'}}
  case Zim = 1.0, Zang = 1.5, Zung = 2.0
}

enum NonObjCEnum: Int {
  case Zim, Zang, Zung
}

class Bar {
  @objc func foo(x: Foo) {}
  @objc func nonObjC(x: NonObjCEnum) {} //expected-error{{type of the parameter cannot be represented in Objective-C}} //expected-note{{non-'@objc' enums cannot be represented in Objective-C}}
}

// RUN: %target-typecheck-verify-swift -enable-objc-interop

@objc enum Foo: Int32 {
  case Zim, Zang, Zung
}

@objc enum Generic<T>: Int32 { // expected-error{{'@objc' enum cannot be generic}} {{1-7=}}
  case Zim, Zang, Zung
}

@objc(EnumRuntimeName) enum RuntimeNamed: Int32 {
  case Zim, Zang, Zung
}

@objc enum NoRawType { // expected-error{{'@objc' enum must declare an integer raw type}}
  case Zim, Zang, Zung
}

@objc enum NonIntegerRawType: Float { // expected-error{{'@objc' enum raw type 'Float' is not an integer type}}
  case Zim = 1.0, Zang = 1.5, Zung = 2.0
}

enum NonObjCEnum: Int {
  case Zim, Zang, Zung
}

class Bar {
  @objc func foo(x: Foo) {}
  @objc func nonObjC(x: NonObjCEnum) {} //expected-error{{type of the parameter cannot be represented in Objective-C}} //expected-note{{non-'@objc' enums cannot be represented in Objective-C}}
}

// <rdar://problem/23681566> @objc enums with payloads rejected with no source location info
@objc enum r23681566 : Int32 {  // expected-error {{'r23681566' declares raw type 'Int32', but does not conform to RawRepresentable and conformance could not be synthesized}} expected-note {{declared raw type 'Int32' here}} expected-note {{do you want to add protocol stubs?}}
  case Foo(progress: Int)     // expected-error {{enum with raw type cannot have cases with arguments}}
}


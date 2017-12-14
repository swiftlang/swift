// RUN: %target-typecheck-verify-swift -swift-version 4
//
// REQUIRES: objc_interop

class MixedKeywordsAndAttributes { // expected-note{{in declaration of 'MixedKeywordsAndAttributes'}}
  // expected-error@+1 {{expected declaration}} expected-error@+1 {{consecutive declarations on a line must be separated by ';'}} {{11-11=;}}
  override @objc func f1() {}
}

@objc class ObjCClass {}
struct SwiftStruct { }

class A {
  @objc subscript (a: ObjCClass) -> ObjCClass { // expected-note{{overridden declaration here has type '(ObjCClass) -> ObjCClass'}}
    get { return ObjCClass() }
  }

  func bar() { } // expected-note{{add '@objc' to make this declaration overridable}}{{3-3=@objc }}
}

extension A {
  func foo() { } // expected-note{{add '@objc' to make this declaration overridable}}{{3-3=@objc }}
  func wibble(_: SwiftStruct) { } // expected-note{{overridden declaration is here}}
}

class B : A {
  // Objective-C
  @objc subscript (a: ObjCClass) -> AnyObject { // expected-error{{overriding keyed subscript with incompatible type '(ObjCClass) -> AnyObject'}}
    get { return self }
  }

  override func foo() { } // expected-error{{overriding non-@objc declarations from extensions is not supported}}

  override func wibble(_: SwiftStruct) { } // expected-error{{overriding declarations in extensions is not supported}}
}

extension B {
  override func bar() { } // expected-error{{overriding non-@objc declarations from extensions is not supported}}
}

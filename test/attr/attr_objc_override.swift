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
  @objc subscript (a: ObjCClass) -> ObjCClass {
    get { return ObjCClass() }  // expected-note{{subscript getter declared here}}
  }

  func bar() { } // expected-note{{add '@objc' to make this declaration overridable}}{{38:3-3=@objc }}
}

extension A {
  func foo() { } // expected-note{{add '@objc' to make this declaration overridable}}{{32:3-3=@objc }}
  func wibble(_: SwiftStruct) { } // expected-note{{overridden declaration is here}}
}

class B : A {
  // Objective-C
  @objc subscript (a: ObjCClass) -> AnyObject {
    get { return self }  // expected-error{{subscript getter with Objective-C selector 'objectForKeyedSubscript:' conflicts with subscript getter from superclass 'A'}}
  }

  override func foo() { } // expected-error{{non-@objc instance method 'foo()' is declared in extension of 'A' and cannot be overridden}}

  override func wibble(_: SwiftStruct) { } // expected-error{{instance method 'wibble' is declared in extension of 'A' and cannot be overridden}}
}

extension B {
  override func bar() { } // expected-error{{non-@objc instance method 'bar()' declared in 'A' cannot be overridden from extension}}
}

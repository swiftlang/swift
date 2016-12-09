// RUN: %target-typecheck-verify-swift
//
// REQUIRES: objc_interop

class MixedKeywordsAndAttributes { // expected-note{{in declaration of 'MixedKeywordsAndAttributes'}}
  // expected-error@+1 {{expected declaration}} expected-error@+1 {{consecutive declarations on a line must be separated by ';'}} {{11-11=;}}
  override @objc func f1() {}
}

@objc class ObjCClass {}

class A {
  @objc subscript (a: ObjCClass) -> ObjCClass { // expected-note{{overridden declaration here has type '(ObjCClass) -> ObjCClass'}}
    get { return ObjCClass() }
  }
}

class B : A {
  // Objective-C
  @objc subscript (a: ObjCClass) -> AnyObject { // expected-error{{overriding keyed subscript with incompatible type '(ObjCClass) -> AnyObject'}}
    get { return self }
  }
}

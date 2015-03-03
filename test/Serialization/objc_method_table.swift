// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -disable-objc-attr-requires-foundation-module -o %t %S/Inputs/objc_method_decls.swift
// RUN: llvm-bcanalyzer %t/objc_method_decls.swiftmodule | FileCheck %s
// RUN: %target-swift-frontend -parse -disable-objc-attr-requires-foundation-module -I %t %s -verify

import objc_method_decls

class B : A {
  @objc func a1() { } // expected-error{{method 'a1()' with Objective-C selector 'a1' conflicts with method 'f1()' from superclass 'A'}}

  @objc init(int: Int) { // expected-error{{initializer 'init(int:)' with Objective-C selector 'initWithInt:' conflicts with method 'f2' from superclass 'A'}}
    self.foo = int 
    super.init()
  }

  @objc var foo: Int // expected-error{{setter for 'foo' with Objective-C selector 'setFoo:' conflicts with method 'f3' from superclass 'A'}}

  @objc subscript (i: Int) -> AnyObject {
    get { return self } // expected-error{{subscript getter with Objective-C selector 'objectAtIndexedSubscript:' conflicts with method 'f4' from superclass 'A'}}
    set { }
  }
}

// Make sure the OBJC_METHODS table is present.
// CHECK: OBJC_METHODS

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-ast-typechecked -source-filename %s -function-definitions=true -prefer-type-repr=false -print-implicit-attrs=true -explode-pattern-binding-decls=true | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import CoreGraphics

// CHECK-LABEL: extension CGColor
extension CGColor {
  // CHECK-LABEL: {{^}} var red: CGFloat
  var red: CGFloat { return 0 }
  // CHECK-LABEL: {{^}} func asCMYK() -> CGColor
  func asCMYK() -> CGColor { return self }
  // CHECK-LABEL: {{^}} subscript(x: CGColor) -> CGFloat
  subscript(x: CGColor) -> CGFloat { return 0 }

  @objc var blue: CGFloat { return 0 } // expected-error{{method cannot be marked @objc because Core Foundation types are not classes in Objective-C}}
  @objc func asHSV() -> CGColor { return self } // expected-error{{method cannot be marked @objc because Core Foundation types are not classes in Objective-C}}
  @objc subscript(x: Int) -> CGFloat { return 0 } // expected-error{{method cannot be marked @objc because Core Foundation types are not classes in Objective-C}}
}

@objc protocol Foo {
  func foo() // expected-note{{satisfying requirement for instance method 'foo()' in protocol 'Foo'}}
}

// CHECK-LABEL: extension CGColor : Foo
extension CGColor: Foo { // expected-error{{Core Foundation class 'CGColor' cannot conform to @objc protocol 'Foo' because Core Foundation types are not classes in Objective-C}}
  // CHECK-LABEL: {{^}} func foo()
  func foo() {} // expected-error{{method cannot be an implementation of an @objc requirement because Core Foundation types are not classes in Objective-C}}
}


// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache -target x86_64-apple-macosx10.9 %s
// RUN: rm -rf %t/clang-module-cache
// RUN: %swift-ide-test %clang-importer-sdk -print-ast-typechecked -source-filename %s -module-cache-path %t/clang-module-cache -target x86_64-apple-macosx10.9 -function-definitions=true -prefer-type-repr=false -print-implicit-attrs=true -explode-pattern-binding-decls=true | FileCheck %s

import CoreGraphics

// CHECK-LABEL: extension CGColor
extension CGColor {
  // CHECK-LABEL: {{^}} final var red: CGFloat
  var red: CGFloat { return 0 }
  // CHECK-LABEL: {{^}} final func asCMYK() -> CGColor
  func asCMYK() -> CGColor { return self }
  // CHECK-LABEL: {{^}} final subscript (x: CGColor) -> CGFloat
  subscript(x: CGColor) -> CGFloat { return 0 }

  @objc var blue: CGFloat { return 0 } // expected-error{{method cannot be marked @objc because Core Foundation types are not classes in Objective-C}}
  @objc func asHSV() -> CGColor { return self } // expected-error{{method cannot be marked @objc because Core Foundation types are not classes in Objective-C}}
  @objc subscript(x: Int) -> CGFloat { return 0 } // expected-error{{method cannot be marked @objc because Core Foundation types are not classes in Objective-C}}
}

@objc protocol Foo { func foo() }

// CHECK-LABEL: extension CGColor : Foo
extension CGColor: Foo { // expected-error{{Core Foundation class 'CGColor' cannot conform to @objc protocol 'Foo' because Core Foundation types are not classes in Objective-C}}
  // CHECK-LABEL: {{^}} final func foo()
  func foo() {}
}


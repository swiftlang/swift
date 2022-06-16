// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Foo.swiftinterface) %s -disable-objc-attr-requires-foundation-module -module-name Foo
// RUN: %FileCheck %s < %t/Foo.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/Foo.swiftinterface) -module-name Foo

// Test the interface we generate for @IBOutlet private(set) properties is
// consumable.

@objc public class MyType {}

open class Bar {
	// CHECK: @objc @IBOutlet weak public var foo: Foo.MyType! {
	// CHECK-NEXT: get
	// CHECK-NEXT: }
	@IBOutlet public private(set) weak var foo: MyType!
}

// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -enable-library-evolution -disable-objc-attr-requires-foundation-module -module-name Foo -emit-module-interface-path %t/Foo.swiftinterface %s
// RUN: %FileCheck %s -input-file %t/Foo.swiftinterface
// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo.swiftinterface -o %t/Foo.swiftmodule -module-name Foo

// Test the interface we generate for @IBOutlet private(set) properties is
// consumable.

@objc public class MyType {}

open class Bar {
	// CHECK: @objc @IBOutlet weak public var foo: Foo.MyType! {
	// CHECK-NEXT: get
	// CHECK-NEXT: }
	@IBOutlet public private(set) weak var foo: MyType!
}

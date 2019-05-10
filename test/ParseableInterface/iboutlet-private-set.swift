// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -enable-library-evolution -disable-objc-attr-requires-foundation-module -emit-parseable-module-interface-path %t/Foo.swiftinterface %s
// RUN: %FileCheck %s -input-file %t/Foo.swiftinterface
// RUN: %target-swift-frontend -build-module-from-parseable-interface %t/Foo.swiftinterface -o %t/Foo.swiftmodule

// Test the interface we generate for @IBOutlet private(set) properties is
// consumable.

@objc public class MyType {}

open class Bar {
	// CHECK: @objc @IBOutlet weak public var foo: MyType! {
	// CHECK-NEXT: get
	// CHECK-NEXT: }
	@IBOutlet public private(set) weak var foo: MyType!
}

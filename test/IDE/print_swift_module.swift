// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module-path %t/print_swift_module.swiftmodule -emit-module-doc -emit-module-doc-path %t/print_swift_module.swiftdoc %s
// RUN: %target-swift-ide-test -print-module -print-interface -no-empty-line-between-members -module-to-print=print_swift_module -I %t -source-filename=%s > %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK1 < %t.syn.txt

public protocol P1 {
	/// foo1 comment from P1
	func foo1()
	/// foo2 comment from P1
	func foo2()
}
public class C1 : P1 {
	public func foo1() {
	}
	/// foo2 comment from C1
	public func foo2() {
	}
}

// CHECK1: 			public class C1 : P1 {
// CHECK1-NEXT:		/// foo1 comment from P1
// CHECK1-NEXT:  	public func foo1()
// CHECK1-NEXT:  	/// foo2 comment from C1
// CHECK1-NEXT:  	public func foo2()
// CHECK1-NEXT: }

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s \
// RUN: -emit-sil -package-name somepkg \
// RUN: -O -wmo -enable-library-evolution \
// RUN: -experimental-package-cmo \
// RUN: -experimental-allow-non-resilient-access \
// RUN: -Xllvm -sil-print-around=mandatory-inlining \
// RUN: -Xllvm -sil-print-debuginfo 2>&1 | %FileCheck %s --check-prefix=CHECK-MI

// RUN: %target-swift-frontend %s \
// RUN: -emit-sil -package-name somepkg \
// RUN: -O -wmo -enable-library-evolution \
// RUN: -experimental-package-cmo \
// RUN: -experimental-allow-non-resilient-access \
// RUN: -Xllvm -sil-print-around=inline \
// RUN: -Xllvm -sil-print-debuginfo 2>&1 | %FileCheck %s --check-prefix=CHECK-PI

// RUN: %target-swift-frontend %s \
// RUN: -emit-module -package-name somepkg \
// RUN: -module-name Lib -emit-module-path %t/Lib.swiftmodule \
// RUN: -O -wmo -enable-library-evolution \
// RUN: -experimental-package-cmo \
// RUN: -experimental-allow-non-resilient-access \
// RUN: -Xllvm -sil-print-around=cmo \
// RUN: -Xllvm -sil-print-debuginfo 2>&1 | %FileCheck %s --check-prefix=CHECK-PCMO


// [serialized]
@inlinable
public func inl_pub(_ arg: Int) -> Int {
	let u = ufi(arg)	// inlined
	let p = pub(arg)    // ref
	return u + p + arg
}

// [serialized_for_package]
public func pub(_ arg: Int) -> Int {
	return arg + pkg(arg) // inlined	
}

// [serialized_for_package]
public func leaf(_ arg: Int) -> Int {
	return arg + 17 // inlined	
}

// [serialized_for_package]
package func pkg(_ arg: Int) -> Int {
	let u = ufi(arg)	// inlined
	let l = leaf(arg)	// inlined
	return u + l + arg
}

package enum PkgEnum: UInt {
	case on, off
	package init(_ arg: Bool) {
		self = arg ? .on : .off
	}
}
package extension PkgEnum: Equatable {}

// serialized_for_package
@usableFromInline
func ufi(_ arg: Int) -> Int {
	return arg + leaf(arg) // inlined
}

// not serialized
func f(_ arg: Int) -> Int {
	return arg + g() + leaf(arg) + inl_pub(arg) // all inlined
}

// not serialized
package func callPkgUseInternal(_ arg: Int) -> Int {
	return arg + pkgUseInternal(arg) // inlined
}

// not serialized
package func pkgUseInternal(_ arg: Int) -> Int {
	return arg + g(arg) // inlined
}

// not serialized
fileprivate g(_ arg: Int) -> Int {
	return arg + 1234
} 

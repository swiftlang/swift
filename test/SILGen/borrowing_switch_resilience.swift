// RUN: %target-swift-emit-silgen -enable-library-evolution %s | %FileCheck %s
@_silgen_name("use")
public func use<T: ~Copyable>(_: borrowing T)

public struct NCL: ~Copyable { var x: AnyObject }

public enum MultiNCL: ~Copyable {
	case foo(NCL)
	case bar(NCL)
}

// Local use of the resilient type can take advantage of knowledge of its
// layout and do value projections.
// CHECK-LABEL: sil{{.*}} @$s{{.*}}testNCL_privateImpl
public func testNCL_privateImpl(foo: borrowing MultiNCL) {
	// CHECK: switch_enum %
	switch foo {
	case .foo(let x):
		use(x)
	case .bar(let x):
		use(x)
	}
}

// Inlinable code can be used out-of-module, so has to use the most general
// access pattern, using `unchecked_borrow_enum_data_addr` to project the
// enum with scratch space.
// CHECK-LABEL: sil{{.*}} @$s{{.*}}testNCL_inlinableImpl
@inlinable
public func testNCL_inlinableImpl(foo: borrowing MultiNCL) {
	switch foo {
	// CHECK: switch_enum_addr [[ENUM:%.*]], case #MultiNCL.foo!enumelt: [[BB_FOO:bb[0-9]+]], 
	case .foo(let x):
	// CHECK: [[BB_FOO]]:
	// CHECK: [[SCRATCH:%.*]] = alloc_stack $MultiNCL
	// CHECK: [[PAYLOAD:%.*]] = unchecked_borrow_enum_data_addr [[ENUM]], #MultiNCL.foo!enumelt in [[SCRATCH]]
	// CHECK: apply
	// CHECK: dealloc_stack [[SCRATCH]]
		use(x)
	case .bar(let x):
		use(x)
	@unknown default:
		break
	}
}

public struct NCA: ~Copyable { var x: Any }

public enum MultiNCA: ~Copyable {
	case foo(NCA)
	case bar(NCA)
}

// Local use of the resilient type can take advantage of knowledge of its
// layout and do in-place projections knowing the layout cannot use spare bit
// packing.
// CHECK-LABEL: sil{{.*}} @$s{{.*}}testNCA_privateImpl
public func testNCA_privateImpl(foo: borrowing MultiNCA) {
	switch foo {
	// CHECK: switch_enum_addr [[ENUM:%.*]], case #MultiNCA.foo!enumelt: [[BB_FOO:bb[0-9]+]], 
	case .foo(let x):
	// CHECK: [[BB_FOO]]:
	// CHECK: [[PAYLOAD:%.*]] = unchecked_inplace_enum_data_addr [[ENUM]], #MultiNCA.foo!enumelt
		use(x)
	case .bar(let x):
		use(x)
	}
}

// Inlinable code can be used out-of-module, so has to use the most general
// access pattern, using `unchecked_borrow_enum_data_addr` to project the
// enum with scratch space.
@inlinable
public func testNCA_inlinableImpl(foo: borrowing MultiNCA) {
	switch foo {
	// CHECK: switch_enum_addr [[ENUM:%.*]], case #MultiNCA.foo!enumelt: [[BB_FOO:bb[0-9]+]], 
	case .foo(let x):
	// CHECK: [[BB_FOO]]:
	// CHECK: [[SCRATCH:%.*]] = alloc_stack $MultiNCA
	// CHECK: [[PAYLOAD:%.*]] = unchecked_borrow_enum_data_addr [[ENUM]], #MultiNCA.foo!enumelt in [[SCRATCH]]
	// CHECK: apply
	// CHECK: dealloc_stack [[SCRATCH]]
		use(x)
	case .bar(let x):
		use(x)
	@unknown default:
		break
	}
}

public struct CL: Copyable { var x: AnyObject }

public enum MultiCL: ~Copyable {
	case foo(CL)
	case bar(CL)
}

public func testCL_privateImpl(foo: borrowing MultiCL) {
	switch foo {
	case .foo(let x):
		use(x)
	case .bar(let x):
		use(x)
	}
}

@inlinable
public func testCL_inlinableImpl(foo: borrowing MultiCL) {
	switch foo {
	case .foo(let x):
		use(x)
	case .bar(let x):
		use(x)
	@unknown default:
		break
	}
}

public struct CA: Copyable { var x: Any }

public enum MultiCA: ~Copyable {
	case foo(CA)
	case bar(CA)
}

public func testCA_privateImpl(foo: borrowing MultiCA) {
	switch foo {
	case .foo(let x):
		use(x)
	case .bar(let x):
		use(x)
	}
}

@inlinable
public func testCA_inlinableImpl(foo: borrowing MultiCA) {
	switch foo {
	case .foo(let x):
		use(x)
	case .bar(let x):
		use(x)
	@unknown default:
		break
	}
}

internal enum Bar<T>: ~Copyable {
	case foo(T)
	case bar(T)
}

func testNondestructiveBecauseGeneric<T>(foo: borrowing Bar<T>) {
	switch foo {
	case .foo(let x):
		use(x)
	case .bar(let x):
		use(x)
	}
}

func testNondestructiveBecauseGenericInstanceA(foo: borrowing Bar<Any>) {
	switch foo {
	case .foo(let x):
		use(x)
	case .bar(let x):
		use(x)
	}
}

func testNondestructiveBecauseGenericInstanceL(foo: borrowing Bar<AnyObject>) {
	switch foo {
	case .foo(let x):
		use(x)
	case .bar(let x):
		use(x)
	}
}


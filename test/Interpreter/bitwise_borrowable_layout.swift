// RUN: %empty-directory(%t)
// RUN: %target-build-swift -enable-library-evolution -enable-experimental-feature RawLayout -enable-experimental-feature AddressableTypes -emit-module-path %t/BitwiseBorrowableLayoutResilientTypes.swiftmodule -parse-as-library %S/Inputs/BitwiseBorrowableLayoutResilientTypes.swift
// RUN: %target-build-swift -enable-library-evolution -enable-experimental-feature RawLayout -enable-experimental-feature AddressableTypes -parse-as-library -c -o %t/BitwiseBorrowableLayoutResilientTypes.o %S/Inputs/BitwiseBorrowableLayoutResilientTypes.swift
// RUN: %target-build-swift -enable-experimental-feature RawLayout -enable-experimental-feature AddressableTypes -I %t -o %t/a.out %s %t/BitwiseBorrowableLayoutResilientTypes.o
// RUN: %target-run %t/a.out | %FileCheck %s

// again, using compact value witnesses:

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -enable-layout-string-value-witnesses -Xfrontend -enable-layout-string-value-witnesses-instantiation -enable-library-evolution -enable-experimental-feature RawLayout -enable-experimental-feature AddressableTypes -emit-module-path %t/BitwiseBorrowableLayoutResilientTypes.swiftmodule -parse-as-library %S/Inputs/BitwiseBorrowableLayoutResilientTypes.swift
// RUN: %target-build-swift -Xfrontend -enable-layout-string-value-witnesses -Xfrontend -enable-layout-string-value-witnesses-instantiation -enable-library-evolution -enable-experimental-feature RawLayout -enable-experimental-feature AddressableTypes -parse-as-library -c -o %t/BitwiseBorrowableLayoutResilientTypes.o %S/Inputs/BitwiseBorrowableLayoutResilientTypes.swift
// RUN: %target-build-swift -Xfrontend -enable-layout-string-value-witnesses -Xfrontend -enable-layout-string-value-witnesses-instantiation -enable-experimental-feature RawLayout -enable-experimental-feature AddressableTypes -I %t -o %t/a.out %s %t/BitwiseBorrowableLayoutResilientTypes.o
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_RawLayout
// REQUIRES: swift_feature_AddressableTypes
// UNSUPPORTED: use_os_stdlib

import BitwiseBorrowableLayoutResilientTypes

struct NonBitwiseTakable {
	weak var x: AnyObject?
}

// note that `_rawLayout` also makes the type addressable-for-dependencies
@_rawLayout(like: Int)
struct NonBitwiseBorrowable: ~Copyable {}

@_addressableForDependencies
struct AFD { var x: Int }

struct TrivialButFor<T: ~Copyable>: ~Copyable {
	var x: Int
	var y: T
}

struct TrivialButForResilientAFD {
	var x: Int
	var y: ResilientAFD
}

struct TrivialButForResilientNonBitwiseTakable: ~Copyable {
	var x: Int
	var y: ResilientNonBitwiseTakable
}

enum TrivialEnumButFor<T: ~Copyable>: ~Copyable {
	case x(Int)
	case y(T)
}

enum TrivialEnumButForResilientAFD {
	case x(Int)
	case y(ResilientAFD)
}

enum TrivialEnumButForResilientNonBitwiseTakable {
	case x(Int)
	case y(ResilientNonBitwiseTakable)
}

enum SPEnumButFor<T: ~Copyable>: ~Copyable {
	case x
	case y(T)
}

enum SPEnumButForResilientAFD {
	case x
	case y(ResilientAFD)
}

enum SPEnumButForResilientNonBitwiseTakable {
	case x
	case y(ResilientNonBitwiseTakable)
}

// This function is intentionally testing runtime behavior, so we inspect
// the raw value witness flags instead of using compiler builtins to check
// type traits. 
// The unsafe pointer chasing here ought to be opaque to the compiler, but
// just as a hedge against any future inlining or constant-folding capabilities
// let's prevent inlining or other optimizations as much as we can here.
@inline(never)
@_optimize(none)
func valueWitnessFlags(for type: any ~Copyable.Type) -> UInt {
	let metadataPtr = unsafeBitCast(type, to: UnsafePointer<UnsafeRawPointer>.self)
	let vwPtr = metadataPtr[-1].assumingMemoryBound(to: UInt.self)
	return vwPtr[10]
}

func isBitwiseBorrowable(_ type: any ~Copyable.Type) -> Bool {
	// 0x0010_0000: not bitwise takable
	// 0x0100_0000: not bitwise borrowable
	// both flags must be zero for a type to be considered bitwise-borrowable
	return valueWitnessFlags(for: type) & 0x0110_0000 == 0
}

func isAddressableForDependencies(_ type: any ~Copyable.Type) -> Bool {
	// 0x0200_0000: addressable for dependencies
	return valueWitnessFlags(for: type) & 0x0200_0000 != 0
}

// CHECK: start
print("start")

// CHECK-NEXT: true
print(isBitwiseBorrowable(TrivialButFor<Int>.self))
// CHECK-NEXT: false
print(isBitwiseBorrowable(TrivialButFor<NonBitwiseTakable>.self))
// CHECK-NEXT: false
print(isBitwiseBorrowable(TrivialButFor<NonBitwiseBorrowable>.self))
// CHECK-NEXT: true
print(isBitwiseBorrowable(TrivialButFor<AFD>.self))
// CHECK-NEXT: true
print(isBitwiseBorrowable(TrivialButFor<ResilientAFD>.self))
// CHECK-NEXT: false
print(isBitwiseBorrowable(TrivialButFor<ResilientNonBitwiseTakable>.self))
// CHECK-NEXT: true
print(isBitwiseBorrowable(TrivialButForResilientAFD.self))
// CHECK-NEXT: false
print(isBitwiseBorrowable(TrivialButForResilientNonBitwiseTakable.self))

// CHECK-NEXT: true
print(isBitwiseBorrowable(TrivialEnumButFor<Int>.self))
// CHECK-NEXT: false
print(isBitwiseBorrowable(TrivialEnumButFor<NonBitwiseTakable>.self))
// CHECK-NEXT: false
print(isBitwiseBorrowable(TrivialEnumButFor<NonBitwiseBorrowable>.self))
// CHECK-NEXT: true
print(isBitwiseBorrowable(TrivialEnumButFor<AFD>.self))
// CHECK-NEXT: true
print(isBitwiseBorrowable(TrivialEnumButFor<ResilientAFD>.self))
// CHECK-NEXT: false
print(isBitwiseBorrowable(TrivialEnumButFor<ResilientNonBitwiseTakable>.self))
// CHECK-NEXT: true
print(isBitwiseBorrowable(TrivialEnumButForResilientAFD.self))
// CHECK-NEXT: false
print(isBitwiseBorrowable(TrivialEnumButForResilientNonBitwiseTakable.self))

// CHECK-NEXT: true
print(isBitwiseBorrowable(SPEnumButFor<Int>.self))
// CHECK-NEXT: false
print(isBitwiseBorrowable(SPEnumButFor<NonBitwiseTakable>.self))
// CHECK-NEXT: false
print(isBitwiseBorrowable(SPEnumButFor<NonBitwiseBorrowable>.self))
// CHECK-NEXT: true
print(isBitwiseBorrowable(SPEnumButFor<AFD>.self))
// CHECK-NEXT: true
print(isBitwiseBorrowable(SPEnumButFor<ResilientAFD>.self))
// CHECK-NEXT: false
print(isBitwiseBorrowable(SPEnumButFor<ResilientNonBitwiseTakable>.self))
// CHECK-NEXT: true
print(isBitwiseBorrowable(SPEnumButForResilientAFD.self))
// CHECK-NEXT: false
print(isBitwiseBorrowable(SPEnumButForResilientNonBitwiseTakable.self))

// CHECK-NEXT: false
print(isAddressableForDependencies(TrivialButFor<Int>.self))
// CHECK-NEXT: false
print(isAddressableForDependencies(TrivialButFor<NonBitwiseTakable>.self))
// CHECK-NEXT: true
print(isAddressableForDependencies(TrivialButFor<NonBitwiseBorrowable>.self))
// CHECK-NEXT: true
print(isAddressableForDependencies(TrivialButFor<AFD>.self))
// CHECK-NEXT: true
print(isAddressableForDependencies(TrivialButFor<ResilientAFD>.self))
// CHECK-NEXT: true
print(isAddressableForDependencies(TrivialButForResilientAFD.self))

// CHECK-NEXT: false
print(isAddressableForDependencies(TrivialEnumButFor<Int>.self))
// CHECK-NEXT: false
print(isAddressableForDependencies(TrivialEnumButFor<NonBitwiseTakable>.self))
// CHECK-NEXT: true
print(isAddressableForDependencies(TrivialEnumButFor<NonBitwiseBorrowable>.self))
// CHECK-NEXT: true
print(isAddressableForDependencies(TrivialEnumButFor<AFD>.self))
// CHECK-NEXT: true
print(isAddressableForDependencies(TrivialEnumButFor<ResilientAFD>.self))
// CHECK-NEXT: true
print(isAddressableForDependencies(TrivialEnumButForResilientAFD.self))

// CHECK-NEXT: false
print(isAddressableForDependencies(SPEnumButFor<Int>.self))
// CHECK-NEXT: false
print(isAddressableForDependencies(SPEnumButFor<NonBitwiseTakable>.self))
// CHECK-NEXT: true
print(isAddressableForDependencies(SPEnumButFor<NonBitwiseBorrowable>.self))
// CHECK-NEXT: true
print(isAddressableForDependencies(SPEnumButFor<AFD>.self))
// CHECK-NEXT: true
print(isAddressableForDependencies(SPEnumButFor<ResilientAFD>.self))
// CHECK-NEXT: true
print(isAddressableForDependencies(SPEnumButForResilientAFD.self))

// CHECK-NEXT: done
print("done")


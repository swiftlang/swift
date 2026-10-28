// RUN: %target-swift-frontend %s -emit-ir -disable-availability-checking | %FileCheck %s
// RUN: %target-swift-frontend %s -O -emit-ir -disable-availability-checking | %FileCheck %s --check-prefix=CHECK-OPT

// REQUIRES: synchronization

// Raw-layout storage (e.g. `_Cell`) has interior mutability: it may be mutated
// through pointers derived from its address while it is borrowed, so borrowed
// pointers to it must not be marked `noalias`. Inout access is still exclusive.

import Synchronization

// CHECK-LABEL: define{{.*}} swiftcc void @"$s18raw_layout_noalias8borrowedyy15Synchronization5_CellVySiGF"(ptr {{(align [0-9]+ )?}}captures(none) dereferenceable({{[0-9]+}}) %0)
public func borrowed(_ c: borrowing _Cell<Int>) {}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s18raw_layout_noalias6mutateyy15Synchronization5_CellVySiGzF"(ptr noalias {{(align [0-9]+ )?}}captures(none) dereferenceable({{[0-9]+}}) %0)
public func mutate(_ c: inout _Cell<Int>) {}

// Types nesting raw-layout storage are interior mutable too.
public struct HasCell: ~Copyable { var x: Int; var c: _Cell<Int> }
public struct HasAtomic: ~Copyable { let a: Atomic<Int> }
public struct Nested: ~Copyable { var inner: HasCell; var y: Int }
public struct GenericHasCell<T>: ~Copyable { var c: _Cell<T> }
public enum EnumHasCell: ~Copyable { case a(_Cell<Int>), b }

// CHECK-LABEL: define{{.*}} swiftcc void @"$s18raw_layout_noalias9atomicArgyy15Synchronization6AtomicVySiGF"(ptr {{(align [0-9]+ )?}}captures(none) dereferenceable({{[0-9]+}}) %0)
public func atomicArg(_ a: borrowing Atomic<Int>) {}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s18raw_layout_noalias7hasCellyyAA03HasE0VF"(ptr {{(align [0-9]+ )?}}captures(none) dereferenceable({{[0-9]+}}) %0)
public func hasCell(_ s: borrowing HasCell) {}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s18raw_layout_noalias9hasAtomicyyAA03HasE0VF"(ptr {{(align [0-9]+ )?}}captures(none) dereferenceable({{[0-9]+}}) %0)
public func hasAtomic(_ s: borrowing HasAtomic) {}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s18raw_layout_noalias6nestedyyAA6NestedVF"(ptr {{(align [0-9]+ )?}}captures(none) dereferenceable({{[0-9]+}}) %0)
public func nested(_ s: borrowing Nested) {}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s18raw_layout_noalias14genericHasCellyyAA07GenericeF0VySiGF"(ptr {{(align [0-9]+ )?}}captures(none) dereferenceable({{[0-9]+}}) %0)
public func genericHasCell(_ s: borrowing GenericHasCell<Int>) {}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s18raw_layout_noalias11enumHasCellyyAA04EnumeF0OF"(ptr {{(align [0-9]+ )?}}captures(none) dereferenceable({{[0-9]+}}) %0)
public func enumHasCell(_ s: borrowing EnumHasCell) {}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s18raw_layout_noalias12mutateNestedyyAA0E0VzF"(ptr noalias {{(align [0-9]+ )?}}captures(none) dereferenceable({{[0-9]+}}) %0)
public func mutateNested(_ s: inout Nested) {}

// Non-raw-layout indirect parameters keep `noalias`.
// CHECK-LABEL: define{{.*}} swiftcc void @"$s18raw_layout_noalias7genericyyxlF"(ptr noalias %0, ptr %T)
public func generic<T>(_ x: borrowing T) {}

// A closure may write to the cell while `readTwice` holds a borrow of it; the
// second load must not be forwarded from the first.
@inline(never)
public func readTwice(_ c: borrowing _Cell<Int>, _ body: () -> Void) -> (Int, Int) {
  let a = unsafe c._address.pointee
  body()
  let b = unsafe c._address.pointee
  return (a, b)
}

// CHECK-OPT-LABEL: define{{.*}} swiftcc {{.*}} @"$s18raw_layout_noalias4testSi_SityF"()
// CHECK-OPT-NOT:     ret { i{{32|64}}, i{{32|64}} } { i{{32|64}} 0, i{{32|64}} 0 }
// CHECK-OPT:         ret
public func test() -> (Int, Int) {
  let c = _Cell<Int>(0)
  return readTwice(c) { unsafe c._address.pointee += 1 }
}

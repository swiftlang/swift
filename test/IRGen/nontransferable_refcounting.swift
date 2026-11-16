// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir %s -swift-version 6 -enable-experimental-feature NontransferableTypes -parse-as-library -module-name m -o %t/out.ll
// RUN: %FileCheck %s < %t/out.ll
// RUN: %FileCheck %s --check-prefix=METADATA < %t/out.ll
// RUN: %FileCheck %s --check-prefix=WITNESS < %t/out.ll

// REQUIRES: swift_feature_NontransferableTypes

public final class NTC: ~Transferable {}
public final class C {}

public struct Single: ~Transferable {
  var c: NTC
}

public struct Mixed: ~Transferable {
  var c: NTC
  var d: C
}

// CHECK-LABEL: define {{.*}} @"$s1m6copyNTyAA3NTCCADF"(
// CHECK:   call ptr @swift_nonatomic_retain(
// CHECK-NOT: call ptr @swift_retain(
// CHECK: }
public func copyNT(_ c: NTC) -> NTC { c }

// CHECK-LABEL: define {{.*}} @"$s1m16copyTransferableyAA1CCADF"(
// CHECK:   call ptr @swift_retain(
// CHECK-NOT: swift_nonatomic_
// CHECK: }
public func copyTransferable(_ c: C) -> C { c }

// Only the reference to the non-'Transferable' class is non-atomic.
// CHECK-LABEL: define {{.*}} @"$s1m9copyMixedyAA0B0VADF"(
// CHECK:   call ptr @swift_nonatomic_retain(
// CHECK:   call ptr @swift_retain(
// CHECK: }
public func copyMixed(_ m: Mixed) -> Mixed { m }

// CHECK-LABEL: define {{.*}} @"$s1m11copyUnownedyAA3NTCCADF"(
// CHECK-NOT: call {{.*}} @swift_{{(unowned)?}}{{[rR]}}etain
// CHECK:   call ptr @swift_nonatomic_unownedRetain(
// CHECK:   call ptr @swift_nonatomic_unownedRetainStrong(
// CHECK:   call void @swift_nonatomic_unownedRelease(
// CHECK: }
public func copyUnowned(_ c: NTC) -> NTC {
  unowned let u = c
  return u
}

public class ConditionalC<T: ~Transferable>: ~Transferable {}

extension ConditionalC: Transferable where T: Transferable {}

public final class NTSubclass: ConditionalC<NTC> {}
public final class TransferableSubclass: ConditionalC<C> {}

// CHECK-LABEL: define {{.*}} @"$s1m17copyConditionalNTyAA0B1CCyAA3NTCCGAGF"(
// CHECK:   call ptr @swift_nonatomic_retain(
// CHECK-NOT: call ptr @swift_retain(
// CHECK: }
public func copyConditionalNT(_ c: ConditionalC<NTC>) -> ConditionalC<NTC> { c }

// CHECK-LABEL: define {{.*}} @"$s1m16copyConditionalTyAA0B1CCyAA1CCGAGF"(
// CHECK:   call ptr @swift_retain(
// CHECK-NOT: swift_nonatomic_
// CHECK: }
public func copyConditionalT(_ c: ConditionalC<C>) -> ConditionalC<C> { c }

// 'T' could be bound to a 'Transferable' type.
// CHECK-LABEL: define {{.*}} @"$s1m22copyConditionalGenericyAA0B1CCyxGAERi2_zlF"(
// CHECK:   call ptr @swift_retain(
// CHECK-NOT: swift_nonatomic_
// CHECK: }
public func copyConditionalGeneric<T: ~Transferable>(
  _ c: ConditionalC<T>
) -> ConditionalC<T> { c }

// CHECK-LABEL: define {{.*}} @"$s1m14copyNTSubclassyAA0B0CADF"(
// CHECK:   call ptr @swift_nonatomic_retain(
// CHECK-NOT: call ptr @swift_retain(
// CHECK: }
public func copyNTSubclass(_ c: NTSubclass) -> NTSubclass { c }

// CHECK-LABEL: define {{.*}} @"$s1m24copyTransferableSubclassyAA0bC0CADF"(
// CHECK:   call ptr @swift_retain(
// CHECK-NOT: swift_nonatomic_
// CHECK: }
public func copyTransferableSubclass(
  _ c: TransferableSubclass
) -> TransferableSubclass { c }

// Generic code copies values through the value witnesses of their types.
public func copyGeneric<T: ~Transferable>(_ x: borrowing T) -> T { copy x }
public func callGeneric(_ c: NTC) -> NTC { copyGeneric(c) }

// The payload of an indirect case is in a box, which is counted atomically.
public enum Tree<T>: ~Transferable {
  case leaf(T)
  indirect case node(NTC, Tree, Tree)
}

public func copyTree<T>(_ t: Tree<T>) -> Tree<T> { t }

// The metadata of a non-'Transferable' class and of a type that's laid out as
// a reference to one don't use the value witnesses of 'Builtin.NativeObject'
// or the standard witnesses for a single strong reference.

// METADATA: @"$s1m3NTCCWV" = internal constant %swift.vwtable { ptr @"$s1m3NTCCwCP", ptr @"$s1m3NTCCwxx", ptr @"$s1m3NTCCwcp", ptr @"$s1m3NTCCwca",
// METADATA: @"$s1m3NTCCMf" = internal global {{.*}} @"$s1m3NTCCWV"
// METADATA: @"$s1m1CCMf" = internal global {{.*}} @"$sBoWV"
// METADATA: @"$s1m6SingleVWV" = internal constant %swift.vwtable { ptr @"$s1m6SingleVwCP", ptr @"$s1m6SingleVwxx", ptr @"$s1m6SingleVwcp", ptr @"$s1m6SingleVwca",
// METADATA: @"$s1m6SingleVMf" = internal constant {{.*}} @"$s1m6SingleVWV"

// WITNESS-LABEL: define {{.*}} @"$s1m3NTCCwxx"(
// WITNESS:   call void @swift_nonatomic_release(
// WITNESS: }
// WITNESS-LABEL: define {{.*}} @"$s1m3NTCCwcp"(
// WITNESS:   call ptr @swift_nonatomic_retain(
// WITNESS: }
// WITNESS-LABEL: define {{.*}} @"$s1m6SingleVwxx"(
// WITNESS:   call void @swift_nonatomic_release(
// WITNESS: }
// WITNESS-LABEL: define {{.*}} @"$s1m6SingleVwcp"(
// WITNESS:   call ptr @swift_nonatomic_retain(
// WITNESS: }

// Only the reference to the non-'Transferable' class is non-atomic.
// WITNESS-LABEL: define {{.*}} @"$s1m5MixedVwcp"(
// WITNESS:   call ptr @swift_nonatomic_retain(
// WITNESS:   call ptr @swift_retain(
// WITNESS: }

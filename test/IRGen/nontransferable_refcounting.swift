// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir %s -swift-version 6 -enable-experimental-feature NontransferableTypes -parse-as-library -module-name m -o %t/out.ll
// RUN: %FileCheck %s < %t/out.ll
// RUN: %FileCheck %s --check-prefix=METADATA < %t/out.ll
// RUN: %FileCheck %s --check-prefix=WITNESS < %t/out.ll
// RUN: %FileCheck %s --check-prefix=HELPER < %t/out.ll

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

// The context of a @nontransferable function is never shared with a
// 'Transferable' function, so it is confined too.
// CHECK-LABEL: define {{.*}} @"$s1m19copyNontransferableyyyYdcyyYdcF"(
// CHECK:   call ptr @swift_nonatomic_retain(
// CHECK: }
public func copyNontransferable(
  _ f: @escaping @nontransferable () -> Void
) -> @nontransferable () -> Void { f }

// CHECK-LABEL: define {{.*}} @"$s1m19dropNontransferableyyyyYdcnF"(
// CHECK:   call void @swift_nonatomic_release(
// CHECK: }
public func dropNontransferable(_ f: consuming @escaping @nontransferable () -> Void) {}

// CHECK-LABEL: define {{.*}} @"$s1m24copyTransferableFunctionyyycyycF"(
// CHECK:   call ptr @swift_retain(
// CHECK-NOT: swift_nonatomic_
// CHECK: }
public func copyTransferableFunction(_ f: @escaping () -> Void) -> () -> Void { f }

// The thunk forms a context of its own that holds the 'Transferable'
// function, whose context is still shared.
// CHECK-LABEL: define {{.*}} @"$s1m24convertToNontransferableyyyYdcyycF"(
// CHECK:   call ptr @swift_retain(
// CHECK:   call noalias ptr @swift_allocObject(
// CHECK: }
public func convertToNontransferable(
  _ f: @escaping () -> Void
) -> @nontransferable () -> Void { f }

// The context of a @nontransferable closure is retained and released
// non-atomically, so a single captured instance can only be its context if it
// is confined as well.
// CHECK-LABEL: define {{.*}} @"$s1m22captureTransferableRefyyyYdcAA1CCF"(
// CHECK:   call noalias ptr @swift_allocObject(
// CHECK: }
public func captureTransferableRef(_ c: C) -> @nontransferable () -> Void {
  { _ = c }
}

// CHECK-LABEL: define {{.*}} @"$s1m12captureNTRefyyyYdcAA3NTCCF"(
// CHECK-NOT: swift_allocObject
// CHECK: }
public func captureNTRef(_ c: NTC) -> @nontransferable () -> Void {
  { _ = c }
}

// A box that holds a non-'Transferable' value can only be referenced by
// values that aren't 'Transferable' either, so it is confined too.
// CHECK-LABEL: define {{.*}} @"$s1m9captureNTyyYdcyF"(
// CHECK:   [[BOX:%.*]] = call noalias ptr @swift_allocObject(
// CHECK:   call ptr @swift_nonatomic_retain(ptr returned [[BOX]])
// CHECK:   call void @swift_nonatomic_release(ptr [[BOX]])
// CHECK: }
public func captureNT() -> @nontransferable () -> Void {
  var local = NTC()
  return { local = NTC(); _ = local }
}

// The box of an 'Int' could be shared with a 'Transferable' closure, so the
// closure gets a context of its own instead of using the box as its context.
// CHECK-LABEL: define {{.*}} @"$s1m10captureIntyyYdcyF"(
// CHECK:   [[BOX:%.*]] = call noalias ptr @swift_allocObject(
// CHECK:   call ptr @swift_retain(ptr returned [[BOX]])
// CHECK:   call noalias ptr @swift_allocObject(
// CHECK:   call void @swift_release(ptr [[BOX]])
// CHECK: }
public func captureInt() -> @nontransferable () -> Void {
  var i = 0
  return { i += 1 }
}

public indirect enum NTTree: ~Transferable {
  case leaf(NTC)
  case node(NTTree, NTTree)
}

public indirect enum TTree {
  case leaf(C)
  case node(TTree, TTree)
}

// The payload of an indirect case is a reference to a box.
// CHECK-LABEL: define {{.*}} @"$s1m10copyNTTreeyAA0B0OADF"(
// CHECK:   call ptr @swift_nonatomic_retain(
// CHECK-NOT: call ptr @swift_retain(
// CHECK: }
public func copyNTTree(_ t: NTTree) -> NTTree { t }

// CHECK-LABEL: define {{.*}} @"$s1m9copyTTreeyAA0B0OADF"(
// CHECK:   call ptr @swift_retain(
// CHECK-NOT: swift_nonatomic_
// CHECK: }
public func copyTTree(_ t: TTree) -> TTree { t }

// A box is only referenced by values of its enum type, so it's confined
// whenever that type can never be 'Transferable', whatever its payload is.
public enum NTIndirectInt: ~Transferable {
  indirect case value(Int)
}

// CHECK-LABEL: define {{.*}} @"$s1m17copyNTIndirectIntyAA0bC0OADF"(
// CHECK:   call ptr @swift_nonatomic_retain(
// CHECK-NOT: call ptr @swift_retain(
// CHECK: }
public func copyNTIndirectInt(_ e: NTIndirectInt) -> NTIndirectInt { e }

public enum ConditionalTree<T: ~Transferable>: ~Transferable {
  case leaf
  indirect case node(T)
}

extension ConditionalTree: Transferable where T: Transferable {}

// FIXME: The box could be non-atomic, but all instances of a generic enum whose
// IR type doesn't depend on its arguments share the type info of the generic
// enum, and some of them are 'Transferable'.
// CHECK-LABEL: define {{.*}} @"$s1m21copyConditionalTreeNTyAA0bC0OyAA3NTCCGAGF"(
// CHECK:   call ptr @swift_retain(
// CHECK-NOT: swift_nonatomic_
// CHECK: }
public func copyConditionalTreeNT(
  _ t: ConditionalTree<NTC>
) -> ConditionalTree<NTC> { t }

// CHECK-LABEL: define {{.*}} @"$s1m20copyConditionalTreeTyAA0bC0OyAA1CCGAGF"(
// CHECK:   call ptr @swift_retain(
// CHECK-NOT: swift_nonatomic_
// CHECK: }
public func copyConditionalTreeT(
  _ t: ConditionalTree<C>
) -> ConditionalTree<C> { t }

// 'T' could be bound to a 'Transferable' type.
// CHECK-LABEL: define {{.*}} @"$s1m26copyConditionalTreeGenericyAA0bC0OyxGAERi2_zlF"(
// CHECK:   call ptr @swift_retain(
// CHECK-NOT: swift_nonatomic_
// CHECK: }
public func copyConditionalTreeGeneric<T: ~Transferable>(
  _ t: borrowing ConditionalTree<T>
) -> ConditionalTree<T> { copy t }

// An enum that is laid out as a nullable reference to a non-'Transferable'
// instance.
public enum MaybeNT: ~Transferable {
  case none
  case some(NTC)
}

// CHECK-LABEL: define {{.*}} @"$s1m11copyMaybeNTyAA0bC0OADF"(
// CHECK:   call ptr @swift_nonatomic_retain(
// CHECK-NOT: call ptr @swift_retain(
// CHECK: }
public func copyMaybeNT(_ m: MaybeNT) -> MaybeNT { m }

// The same operations apply to the payload of every case, so they can only be
// non-atomic if all payloads are confined.
public enum EitherNTOrT: ~Transferable {
  case nt(NTC)
  case t(C)
}

// CHECK-LABEL: define {{.*}} @"$s1m15copyEitherNTOrTyAA0bC1TOADF"(
// CHECK:   call ptr @swift_retain(
// CHECK-NOT: swift_nonatomic_
// CHECK: }
public func copyEitherNTOrT(_ e: EitherNTOrT) -> EitherNTOrT { e }

// Generic code copies values through the value witnesses of their types.
public func copyGeneric<T: ~Transferable>(_ x: borrowing T) -> T { copy x }
public func callGeneric(_ c: NTC) -> NTC { copyGeneric(c) }

public enum Tree<T>: ~Transferable {
  case leaf(T)
  indirect case node(NTC, Tree, Tree)
}

public func copyTree<T>(_ t: Tree<T>) -> Tree<T> { t }

// The metadata of a non-'Transferable' class and of a type that's laid out as
// a reference to one don't use the value witnesses of 'Builtin.NativeObject',
// but share non-atomic witnesses for a single strong reference.

// METADATA: @"$s1m3NTCCWV" = internal constant %swift.vwtable { ptr @__swift_initWithCopy_nonatomic_strong, ptr @__swift_destroy_nonatomic_strong, ptr @__swift_initWithCopy_nonatomic_strong, ptr @__swift_assignWithCopy_nonatomic_strong, ptr @__swift_memcpy8_8, ptr @__swift_assignWithTake_nonatomic_strong,
// METADATA: @"$s1m3NTCCMf" = internal global {{.*}} @"$s1m3NTCCWV"
// METADATA: @"$s1m1CCMf" = internal global {{.*}} @"$sBoWV"
// METADATA: @"$s1m6SingleVWV" = internal constant %swift.vwtable { ptr @__swift_initWithCopy_nonatomic_strong, ptr @__swift_destroy_nonatomic_strong, ptr @__swift_initWithCopy_nonatomic_strong, ptr @__swift_assignWithCopy_nonatomic_strong, ptr @__swift_memcpy8_8, ptr @__swift_assignWithTake_nonatomic_strong,
// METADATA: @"$s1m6SingleVMf" = internal constant {{.*}} @"$s1m6SingleVWV"

// HELPER-LABEL: define {{.*}} @__swift_initWithCopy_nonatomic_strong(
// HELPER:   call ptr @swift_nonatomic_retain(
// HELPER-NOT: call {{.*}} @swift_{{(retain|release)}}(
// HELPER: }
// HELPER-LABEL: define {{.*}} @__swift_destroy_nonatomic_strong(
// HELPER:   call void @swift_nonatomic_release(
// HELPER: }
// HELPER-LABEL: define {{.*}} @__swift_assignWithCopy_nonatomic_strong(
// HELPER:   call ptr @swift_nonatomic_retain(
// HELPER:   call void @swift_nonatomic_release(
// HELPER-NOT: call {{.*}} @swift_{{(retain|release)}}(
// HELPER: }
// HELPER-LABEL: define {{.*}} @__swift_assignWithTake_nonatomic_strong(
// HELPER:   call void @swift_nonatomic_release(
// HELPER-NOT: call {{.*}} @swift_{{(retain|release)}}(
// HELPER: }

// Only the reference to the non-'Transferable' class is non-atomic.
// WITNESS-LABEL: define {{.*}} @"$s1m5MixedVwcp"(
// WITNESS:   call ptr @swift_nonatomic_retain(
// WITNESS:   call ptr @swift_retain(
// WITNESS: }

// The runtime instantiates these for 'Transferable' types as well.
// WITNESS-LABEL: define {{.*}} @"$s1m15ConditionalTreeOwxx"(
// WITNESS:   call void @swift_release(
// WITNESS-NOT: swift_nonatomic_
// WITNESS: }
// WITNESS-LABEL: define {{.*}} @"$s1m15ConditionalTreeOwcp"(
// WITNESS:   call ptr @swift_retain(
// WITNESS-NOT: swift_nonatomic_
// WITNESS: }

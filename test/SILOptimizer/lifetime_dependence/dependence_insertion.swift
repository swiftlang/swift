// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -Xllvm -sil-print-after=lifetime-dependence-insertion \
// RUN:   -enable-builtin-module \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -define-availability "Span 0.1:macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, visionOS 9999" \
// RUN:   -enable-address-dependencies \
// RUN:   -enable-experimental-feature LifetimeDependence \
// RUN:   -enable-experimental-feature AddressableParameters \
// RUN:   -enable-experimental-feature AddressableTypes \
// RUN:   -o /dev/null 2>&1 | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_AddressableParameters
// REQUIRES: swift_feature_AddressableTypes

import Builtin

@unsafe
@_unsafeNonescapableResult
@_alwaysEmitIntoClient
@_transparent
@lifetime(borrow source)
internal func _overrideLifetime<
  T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable
>(
  _ dependent: consuming T, borrowing source: borrowing U
) -> T {
  dependent
}

struct BV : ~Escapable {
  let p: UnsafeRawPointer
  let i: Int

  @lifetime(borrow p)
  init(_ p: UnsafeRawPointer, _ i: Int) {
    self.p = p
    self.i = i
  }
}

struct NC : ~Copyable {
  let p: UnsafeRawPointer
  let i: Int

  // Requires a borrow.
  @lifetime(borrow self)
  borrowing func getBV() -> BV {
    BV(p, i)
  }
}

public class C {
  var i: Int = 42
}

@available(Span 0.1, *)
public typealias IntSpan = Span<Int>

@available(Span 0.1, *)
protocol IntSpanable {
  @lifetime(borrow self)
  func getIntSpan() -> IntSpan
}

@available(Span 0.1, *)
public struct Holder: IntSpanable {
  let c: C
  var p: UnsafePointer<Int>

  init(_ c: C) {
    self.c = c
    self.p = UnsafePointer<Int>(bitPattern: 0)!
    withUnsafePointer(to: c.i) {
      self.p = $0
    }
  }

  @lifetime(borrow self)
  func getIntSpan() -> IntSpan {
    let span = unsafe Span(_unsafeStart: p, count: 1)
    return unsafe _overrideLifetime(span, borrowing: self)
  }
}

@available(Span 0.1, *)
@_addressableForDependencies
public struct InlineHolder: IntSpanable {
  var i: Int
  var c: C

  init(_ c: C) {
    self.c = c
    self.i = c.i
  }

  @lifetime(borrow self)
  func getIntSpan() -> IntSpan {
    let a = Builtin.addressOfBorrow(self)
    let address = unsafe UnsafePointer<Int>(a)
    let span = unsafe Span(_unsafeStart: address, count: 1)
    return unsafe _overrideLifetime(span, borrowing: self)
  }
}

@_silgen_name("use")
func use(_ o : borrowing BV)

// =============================================================================
// Basic pointer dependencies
// =============================================================================

// CHECK-LABEL: sil hidden [ossa] @$s4test13bv_borrow_var1p1iySV_SitF : $@convention(thin) (UnsafeRawPointer, Int) -> () {
// CHECK: [[A:%.*]] = begin_access [read] [unknown] %{{.*}} : $*NC
// CHECK: [[U:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[A]] : $*NC 
// CHECK: [[L:%.*]] = load [copy] [[U]] : $*NC
// CHECK:   [[R:%.*]] = apply %{{.*}}([[L]]) : $@convention(method) (@guaranteed NC) -> @lifetime(borrow 0) @owned BV
// CHECK:   [[M:%.*]] = mark_dependence [unresolved] [[R]] : $BV on [[A]] : $*NC
// CHECK:   end_access [[A]] : $*NC
// CHECK:   [[MV:%.*]] = move_value [var_decl] [[M]] : $BV
// CHECK:   %{{.*}} = apply %{{.*}}([[MV]]) : $@convention(thin) (@guaranteed BV) -> ()
// CHECK-LABEL: } // end sil function '$s4test13bv_borrow_var1p1iySV_SitF'
func bv_borrow_var(p: UnsafeRawPointer, i: Int) {
  var nc = NC(p: p, i: i)
  let bv = nc.getBV()
  use(bv)
}

// LifetimeDependence.Scope needs to see through typed-to-raw pointer conversion.
//
// CHECK-LABEL: sil hidden [ossa] @$s4test18bv_pointer_convert1pAA2BVVSPySiG_tF : $@convention(thin) (UnsafePointer<Int>) -> @lifetime(borrow 0) @owned BV {
// CHECK: bb0(%0 : $UnsafePointer<Int>):
// CHECK: apply %{{.*}}<UnsafePointer<Int>, UnsafeRawPointer>([[RAW:%.*]], %{{.*}}) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : _Pointer, τ_0_1 : _Pointer> (@in_guaranteed τ_0_0) -> @out τ_0_1
// CHECK: [[RAW:%.*]] = load [trivial] %6 : $*UnsafeRawPointer
// CHECK: [[BV:%.*]] = apply %13([[RAW]], {{.*}}) : $@convention(method) (UnsafeRawPointer, Int, @thin BV.Type) -> @lifetime(borrow 0) @owned BV
// CHECK: [[MD:%.*]] = mark_dependence [unresolved] [[BV]] : $BV on %0 : $UnsafePointer<Int>
// CHECK: return [[MD]] : $BV
// CHECK-LABEL: } // end sil function '$s4test18bv_pointer_convert1pAA2BVVSPySiG_tF'
@lifetime(borrow p)
func bv_pointer_convert(p: UnsafePointer<Int>) -> BV {
  BV(p, 0)
}

// =============================================================================
// @_addressableForDependencies
// =============================================================================

// 'some Spanable' is AddressableForDependencies.
@available(Span 0.1, *)
@lifetime(borrow spanable)
func getIntSpan(_ spanable: some IntSpanable) -> IntSpan {
  spanable.getIntSpan()
}

// 'Holder' is not AddressableForDependencies.
// 'span' depends on the value of the borrowed argument.
@available(Span 0.1, *)
public func getHolderSpan(_ holder: borrowing Holder) -> IntSpan {
  let span = getIntSpan(holder)
  return span
}

@available(Span 0.1, *)
public func testHolderSpan() -> Int {
  let c = C()
  let holder = Holder(consume c)
  let span = getHolderSpan(holder)
  return span[0]
}

// 'InlineHolder' is AddressableForDependencies.
// 'span' depends on the address of the borrowed argument.
@available(Span 0.1, *)
public func getInlineHolderSpan(_ holder: borrowing InlineHolder) -> IntSpan {
  let span = getIntSpan(holder)
  return span
}

@available(Span 0.1, *)
public func testInlineHolderSpan() -> Int {
  let c = C()
  let holder = InlineHolder(consume c)
  let span = getInlineHolderSpan(holder)
  return span[0]
}

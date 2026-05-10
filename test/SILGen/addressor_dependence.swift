// RUN: %target-swift-emit-silgen %s | %FileCheck %s

public class C {}

public struct BoxC {
  let storage: AnyObject?
  let pointer: UnsafePointer<C>

  subscript() -> C {
    unsafeAddress {
      pointer
    }
  }
}

// The addressor result must explicitly dependend on the apply's 'self' arg.
//
// CHECK-LABEL: sil [ossa] @$s20addressor_dependence21testAddressorLifetime3boxAA1CCAA4BoxCVn_tF : $@convention(thin) (@owned BoxC) -> @owned C {
// CHECK: bb0(%0 : @noImplicitCopy @_eagerMove @owned $BoxC):
// CHECK: [[MV:%.*]] = moveonlywrapper_to_copyable [guaranteed]
// CHECK: [[APPLY:%.*]] = apply %{{.*}}([[MV]]) : $@convention(method) (@guaranteed BoxC) -> UnsafePointer<C>
// CHECK: [[P:%.*]] = struct_extract [[APPLY]], #UnsafePointer._rawValue
// CHECK: [[A:%.*]] = pointer_to_address [[P]] to [strict] $*C
// CHECK: [[MD:%.*]] = mark_dependence [unresolved] [[A]] on [[MV]]
// CHECK: begin_access [read] [unsafe] [[MD]]
// CHECK-LABEL: } // end sil function '$s20addressor_dependence21testAddressorLifetime3boxAA1CCAA4BoxCVn_tF'
public func testAddressorLifetime(box: consuming BoxC) -> C {
  box[]
}

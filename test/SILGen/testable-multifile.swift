// This test is paired with testable-multifile-other.swift.

// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/TestableMultifileHelper.swift -enable-testing -o %t

// RUN: %target-swift-frontend -emit-silgen -I %t %s %S/testable-multifile-other.swift -module-name main | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -I %t %S/testable-multifile-other.swift %s -module-name main | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -I %t -primary-file %s %S/testable-multifile-other.swift -module-name main | %FileCheck %s

// Just make sure we don't crash later on.
// RUN: %target-swift-frontend -emit-ir -I %t -primary-file %s %S/testable-multifile-other.swift -module-name main -o /dev/null
// RUN: %target-swift-frontend -emit-ir -I %t -O -primary-file %s %S/testable-multifile-other.swift -module-name main -o /dev/null

@testable import TestableMultifileHelper

public protocol Fooable {
  func foo()
}

struct FooImpl: Fooable, HasDefaultFoo {}
public struct PublicFooImpl: Fooable, HasDefaultFoo {}

// CHECK-LABEL: sil{{.*}} @_TTWV4main7FooImplS_7FooableS_FS1_3foofT_T_ : $@convention(witness_method) (@in_guaranteed FooImpl) -> () {
// CHECK: function_ref @_TFE23TestableMultifileHelperPS_13HasDefaultFoo3foofT_T_
// CHECK: {{^}$}}

// CHECK-LABEL: sil{{.*}} @_TTWV4main13PublicFooImplS_7FooableS_FS1_3foofT_T_ : $@convention(witness_method) (@in_guaranteed PublicFooImpl) -> () {
// CHECK: function_ref @_TFE23TestableMultifileHelperPS_13HasDefaultFoo3foofT_T_
// CHECK: {{^}$}}

private class PrivateSub: Base {
  fileprivate override func foo() {}
}
class Sub: Base {
  internal override func foo() {}
}
public class PublicSub: Base {
  public override func foo() {}
}

// CHECK-LABEL: sil_vtable PrivateSub {
// CHECK-NEXT:   #Base.foo!1: _TFC4mainP33_F1525133BD493492AD72BF10FBCB1C5210PrivateSub3foofT_T_
// CHECK-NEXT:   #Base.init!initializer.1: _TFC4mainP33_F1525133BD493492AD72BF10FBCB1C5210PrivateSubcfT_S0_
// CHECK-NEXT:   #PrivateSub.deinit!deallocator: _TFC4mainP33_F1525133BD493492AD72BF10FBCB1C5210PrivateSubD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable Sub {
// CHECK-NEXT:   #Base.foo!1: _TFC4main3Sub3foofT_T_
// CHECK-NEXT:   #Base.init!initializer.1: _TFC4main3SubcfT_S0_
// CHECK-NEXT:   #Sub.deinit!deallocator: _TFC4main3SubD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable PublicSub {
// CHECK-NEXT:   #Base.foo!1: _TFC4main9PublicSub3foofT_T_
// CHECK-NEXT:   #Base.init!initializer.1: _TFC4main9PublicSubcfT_S0_
// CHECK-NEXT:   #PublicSub.deinit!deallocator: _TFC4main9PublicSubD
// CHECK-NEXT: }



// CHECK-LABEL: sil_witness_table FooImpl: Fooable module main {
// CHECK-NEXT:  method #Fooable.foo!1: @_TTWV4main7FooImplS_7FooableS_FS1_3foofT_T_
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table [fragile] PublicFooImpl: Fooable module main {
// CHECK-NEXT:  method #Fooable.foo!1: @_TTWV4main13PublicFooImplS_7FooableS_FS1_3foofT_T_
// CHECK-NEXT: }

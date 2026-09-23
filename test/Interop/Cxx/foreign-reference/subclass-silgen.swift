// RUN: %target-swift-emit-silgen %s -I %S%{fs-sep}Inputs -cxx-interoperability-mode=default -enable-experimental-feature ForeignReferenceTypeSubclassing -target %target-swift-5.8-abi-triple | %FileCheck %s

// REQUIRES: swift_feature_ForeignReferenceTypeSubclassing

import InheritFRTSubclassing

public final class FieldSub: SharedConstructed {
  public let x: Int64
  public init(x: Int64, a: Int32) {
    self.x = x
    super.init(a)
  }
}

public final class DefaultSub: SubclassableShared {
  public let y: Int64 = 7
}

// Allocation happens in the allocating entry point, and constructs nothing: the
// base subobject is still uninitialized when the initializer body starts.
// CHECK-LABEL: sil{{.*}} @$s{{.*}}8FieldSubC1x1aACs5Int64V_s5Int32VtcfC
// CHECK:         alloc_ref $FieldSub
// CHECK-NOT:     builtin "initializeForeignReferenceSubclass"
// CHECK:         return

// The initializer body stores the Swift stored property first, then constructs
// the base subobject, forwarding the `super.init` argument to it.
// CHECK-LABEL: sil{{.*}} @$s{{.*}}8FieldSubC1x1aACs5Int64V_s5Int32Vtcfc
// CHECK:         ref_element_addr {{.*}} #FieldSub.x
// CHECK:         [[SELF:%.*]] = load [take]
// CHECK:         [[SUPER:%.*]] = upcast [[SELF]] to $SharedConstructed
// CHECK:         [[CTOR:%.*]] = function_ref @$sSo17SharedConstructedVyABs5Int32VcfCTo : $@convention(c) (Int32) -> @owned SharedConstructed
// CHECK:         [[SUB:%.*]] = unchecked_ref_cast [[SUPER]] to $FieldSub
// CHECK:         [[NEW:%.*]] = builtin "initializeForeignReferenceSubclass"<FieldSub>([[SUB]], [[CTOR]], {{%[0-9]+}}) : $FieldSub
// CHECK:         [[NEWSUPER:%.*]] = upcast [[NEW]] to $SharedConstructed
// CHECK:         [[NEWSUB:%.*]] = unchecked_ref_cast [[NEWSUPER]] to $FieldSub
// CHECK:         store [[NEWSUB]] to [init]

// The destroying destructor does not chain to the C++ base's destructor: the
// imported foreign reference type has no Swift deinit, and uses the release
// operation instead.
// CHECK-LABEL: sil{{.*}} @$s{{.*}}8FieldSubCfd
// CHECK-NOT:     function_ref @$s{{.*}}17SharedConstructed
// CHECK:         unchecked_ref_cast
// CHECK:         return

// A synthesized default initializer gets the same treatment.
// CHECK-LABEL: sil{{.*}} @$s{{.*}}10DefaultSubCACycfc
// CHECK:         ref_element_addr {{.*}} #DefaultSub.y
// CHECK:         [[DSELF:%.*]] = load [take]
// CHECK:         [[DSUPER:%.*]] = upcast [[DSELF]] to $SubclassableShared
// CHECK:         [[DCTOR:%.*]] = function_ref @$sSo18SubclassableSharedVABycfCTo : $@convention(c) () -> SubclassableShared
// CHECK:         [[DSUB:%.*]] = unchecked_ref_cast [[DSUPER]] to $DefaultSub
// CHECK:         [[DNEW:%.*]] = builtin "initializeForeignReferenceSubclass"<DefaultSub>([[DSUB]], [[DCTOR]]) : $DefaultSub
// CHECK:         [[DNEWSUPER:%.*]] = upcast [[DNEW]] to $SubclassableShared
// CHECK:         [[DNEWSUB:%.*]] = unchecked_ref_cast [[DNEWSUPER]] to $DefaultSub
// CHECK:         store [[DNEWSUB]] to [init]

// The `super.init` arguments may read stored properties of `self`.
public final class PassesOwnField: SharedConstructed {
  public let x: Int32
  public init(x: Int32) {
    self.x = x
    super.init(self.x)
  }
}
// CHECK-LABEL: sil{{.*}} @$s{{.*}}14PassesOwnFieldC1xACs5Int32V_tcfc
// CHECK:         assign
// CHECK:         [[SELF:%.*]] = load [take]
// CHECK:         [[SUPER:%.*]] = upcast [[SELF]] to $SharedConstructed
// CHECK:         [[BORROW:%.*]] = begin_borrow [[SUPER]]
// CHECK:         [[BORROWSUB:%.*]] = unchecked_ref_cast [[BORROW]] to $PassesOwnField
// CHECK:         [[XADDR:%.*]] = ref_element_addr [[BORROWSUB]], #PassesOwnField.x
// CHECK:         [[X:%.*]] = load [trivial] [[XADDR]]
// CHECK:         end_borrow [[BORROW]]
// CHECK:         [[CTOR:%.*]] = function_ref @$sSo17SharedConstructedVyABs5Int32VcfCTo
// CHECK:         [[SUB:%.*]] = unchecked_ref_cast [[SUPER]] to $PassesOwnField
// CHECK:         [[NEW:%.*]] = builtin "initializeForeignReferenceSubclass"<PassesOwnField>([[SUB]], [[CTOR]], [[X]]) : $PassesOwnField
// CHECK:         store {{%.*}} to [init]

public final class ConstRefArgSub: ReferenceConstructed {
  public let t: Int64 = 1
  public init(_ i: Int32) { super.init(i) }
}
// CHECK-LABEL: sil{{.*}} @$s{{.*}}14ConstRefArgSubCyACs5Int32Vcfc
// CHECK:         [[TEMP:%.*]] = alloc_stack $Int32
// CHECK:         [[ARG:%.*]] = store_borrow %0 to [[TEMP]]
// CHECK:         [[CTOR:%.*]] = function_ref @$sSo20ReferenceConstructedVyABs5Int32VcfCTo : $@convention(c) (@in_guaranteed Int32) -> @owned ReferenceConstructed
// CHECK:         [[SUB:%.*]] = unchecked_ref_cast {{%.*}} to $ConstRefArgSub
// CHECK:         builtin "initializeForeignReferenceSubclass"<ConstRefArgSub>([[SUB]], [[CTOR]], [[ARG]]) : $ConstRefArgSub
// CHECK:         dealloc_stack [[TEMP]]

public final class InoutArgSub: ReferenceConstructed {
  public let t: Int64 = 1
  public init(_ i: inout Int32) { super.init(&i, 0) }
}
// CHECK-LABEL: sil{{.*}} @$s{{.*}}11InoutArgSubCyACs5Int32Vzcfc
// CHECK:         [[ACCESS:%.*]] = begin_access [modify] [unknown] %0
// CHECK:         [[CTOR:%.*]] = function_ref @$sSo20ReferenceConstructedVyABs5Int32Vz_ADtcfCTo : $@convention(c) (@inout Int32, Int32) -> @owned ReferenceConstructed
// CHECK:         builtin "initializeForeignReferenceSubclass"<InoutArgSub>({{%.*}}, [[CTOR]], [[ACCESS]], {{%.*}}) : $InoutArgSub
// CHECK:         end_access [[ACCESS]]

public final class ByValueArgSub: ReferenceConstructed {
  public let t: Int64 = 1
  public init(_ arg: NonTrivialArg) { super.init(arg, 0) }
}
// CHECK-LABEL: sil{{.*}} @$s{{.*}}13ByValueArgSubCyACSo010NonTrivialD0Vcfc
// CHECK:         [[TEMP:%.*]] = alloc_stack $NonTrivialArg
// CHECK:         copy_addr %0 to [init] [[TEMP]]
// CHECK:         [[CTOR:%.*]] = function_ref @$sSo20ReferenceConstructedVyABSo13NonTrivialArgV_SitcfCTo : $@convention(c) (@in_cxx NonTrivialArg, Int) -> @owned ReferenceConstructed
// CHECK:         builtin "initializeForeignReferenceSubclass"<ByValueArgSub>({{%.*}}, [[CTOR]], [[TEMP]], {{%.*}}) : $ByValueArgSub
// CHECK-NEXT:    destroy_addr [[TEMP]]
// CHECK-NEXT:    dealloc_stack [[TEMP]]

public final class SelfNonTrivialArgSub: ReferenceConstructed {
  public let arg: NonTrivialArg
  public init(_ arg: NonTrivialArg) {
    self.arg = arg
    super.init(self.arg, CChar(0))
  }
}
// CHECK-LABEL: sil{{.*}} @$s{{.*}}20SelfNonTrivialArgSubCyACSo0cdE0Vcfc
// CHECK:         [[SELF:%.*]] = load [take]
// CHECK:         [[SUPER:%.*]] = upcast [[SELF]] to $ReferenceConstructed
// CHECK:         [[BORROW:%.*]] = begin_borrow [[SUPER]]
// CHECK:         [[ARGADDR:%.*]] = ref_element_addr {{%.*}}, #SelfNonTrivialArgSub.arg
// CHECK:         [[TEMP:%.*]] = alloc_stack $NonTrivialArg
// CHECK:         copy_addr [[ARGADDR]] to [init] [[TEMP]]
// CHECK:         end_borrow [[BORROW]]
// CHECK:         [[CTOR:%.*]] = function_ref @$sSo20ReferenceConstructedVyABSo13NonTrivialArgV_s4Int8VtcfCTo : $@convention(c) (@in_guaranteed NonTrivialArg, Int8) -> @owned ReferenceConstructed
// CHECK:         [[SUB:%.*]] = unchecked_ref_cast [[SUPER]] to $SelfNonTrivialArgSub
// CHECK:         builtin "initializeForeignReferenceSubclass"<SelfNonTrivialArgSub>([[SUB]], [[CTOR]], [[TEMP]], {{%.*}}) : $SelfNonTrivialArgSub
// CHECK-NEXT:    destroy_addr [[TEMP]]

public final class PicksDoubleSub: OverloadedConstructed {
  public let t: Int64 = 1
  public init() { super.init(2.0) }
}
// CHECK-LABEL: sil{{.*}} @$s{{.*}}14PicksDoubleSubCACycfc
// CHECK:         [[CTOR:%.*]] = function_ref @$sSo21OverloadedConstructedVyABSdcfCTo : $@convention(c) (Double) -> @owned OverloadedConstructed
// CHECK:         builtin "initializeForeignReferenceSubclass"<PicksDoubleSub>({{%.*}}, [[CTOR]], {{%.*}}) : $PicksDoubleSub

// A `self.init` delegation to a C++ constructor, e.g. from a convenience
// initializer in a Swift extension of the base, is an ordinary factory call.
extension SharedConstructed {
  public convenience init(twice a: Int32) {
    self.init(a * 2)
  }
}
// CHECK-LABEL: sil{{.*}} @$sSo17SharedConstructedV{{.*}}5twiceABs5Int32V_tcfC
// CHECK-NOT:     builtin "initializeForeignReferenceSubclass"
// CHECK:         [[CTOR:%.*]] = function_ref @$sSo17SharedConstructedVyABs5Int32VcfCTo : $@convention(c) (Int32) -> @owned SharedConstructed
// CHECK-NOT:     builtin "initializeForeignReferenceSubclass"
// CHECK:         apply [[CTOR]]
// CHECK-NOT:     builtin "initializeForeignReferenceSubclass"
// CHECK:         return

// RUN: %target-swift-emit-silgen %s -I %S%{fs-sep}Inputs -cxx-interoperability-mode=default -enable-experimental-feature ForeignReferenceTypeSubclassing -target %target-swift-5.8-abi-triple | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi

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
// CHECK-NOT:     builtin "initializeForeignReferenceSubclassVTables"
// CHECK:         return

// The initializer body stores the Swift stored property first, then calls the
// initializing entry point of the C++ constructor, which constructs the base
// subobject of `self` in place and returns `self`, and then points the
// v-tables of `self` at the v-tables of the subclass.
// CHECK-LABEL: sil{{.*}} @$s{{.*}}8FieldSubC1x1aACs5Int64V_s5Int32Vtcfc
// CHECK:         ref_element_addr {{.*}} #FieldSub.x
// CHECK:         [[SELF:%.*]] = load [take]
// CHECK:         [[SUPER:%.*]] = upcast [[SELF]] to $SharedConstructed
// CHECK:         [[CTOR:%.*]] = function_ref @$sSo17SharedConstructedVyABs5Int32VcfcTo : $@convention(cxx_method) (Int32, @guaranteed SharedConstructed) -> @owned SharedConstructed
// CHECK-NOT:     begin_borrow [[SUPER]]
// CHECK:         [[NEWSUPER:%.*]] = apply [[CTOR]](%1, [[SUPER]])
// CHECK-NEXT:    end_lifetime [[SUPER]]
// CHECK-NEXT:    [[NEWSUB:%.*]] = unchecked_ref_cast [[NEWSUPER]] to $FieldSub
// CHECK-NEXT:    builtin "initializeForeignReferenceSubclassVTables"<FieldSub>([[NEWSUB]]) : $()
// CHECK-NEXT:    store [[NEWSUB]] to [init]

// The initializing entry point has no symbol of its own yet: it must not use
// the symbol of the static factory that the initializer was imported from.
// CHECK:       sil {{.*}}[clang SharedConstructed.init] @$sSo17SharedConstructedVyABs5Int32VcfcTo : $@convention(cxx_method) (Int32, @guaranteed SharedConstructed) -> @owned SharedConstructed

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
// CHECK:         [[DCTOR:%.*]] = function_ref @$sSo18SubclassableSharedVABycfcTo : $@convention(cxx_method) (@guaranteed SubclassableShared) -> SubclassableShared
// CHECK-NOT:     begin_borrow [[DSUPER]]
// CHECK:         [[DNEWSUPER:%.*]] = apply [[DCTOR]]([[DSUPER]])
// CHECK-NEXT:    end_lifetime [[DSUPER]]
// CHECK-NEXT:    [[DNEWSUB:%.*]] = unchecked_ref_cast [[DNEWSUPER]] to $DefaultSub
// CHECK-NEXT:    builtin "initializeForeignReferenceSubclassVTables"<DefaultSub>([[DNEWSUB]]) : $()
// CHECK-NOT:     end_borrow
// CHECK-NEXT:    store [[DNEWSUB]] to [init]

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
// CHECK:         [[CTOR:%.*]] = function_ref @$sSo17SharedConstructedVyABs5Int32VcfcTo
// CHECK-NOT:     begin_borrow [[SUPER]]
// CHECK:         [[NEWSUPER:%.*]] = apply [[CTOR]]([[X]], [[SUPER]])
// CHECK:         [[SUB:%.*]] = unchecked_ref_cast [[NEWSUPER]] to $PassesOwnField
// CHECK:         builtin "initializeForeignReferenceSubclassVTables"<PassesOwnField>([[SUB]]) : $()
// CHECK:         store [[SUB]] to [init]

public final class ConstRefArgSub: ReferenceConstructed {
  public let t: Int64 = 1
  public init(_ i: Int32) { super.init(i) }
}
// CHECK-LABEL: sil{{.*}} @$s{{.*}}14ConstRefArgSubCyACs5Int32Vcfc
// CHECK:         [[TEMP:%.*]] = alloc_stack $Int32
// CHECK:         [[ARG:%.*]] = store_borrow %0 to [[TEMP]]
// CHECK:         [[CTOR:%.*]] = function_ref @$sSo20ReferenceConstructedVyABs5Int32VcfcTo : $@convention(cxx_method) (@in_guaranteed Int32, @guaranteed ReferenceConstructed) -> @owned ReferenceConstructed
// CHECK:         [[NEWSUPER:%.*]] = apply [[CTOR]]([[ARG]], {{%.*}})
// CHECK:         [[SUB:%.*]] = unchecked_ref_cast [[NEWSUPER]] to $ConstRefArgSub
// CHECK:         builtin "initializeForeignReferenceSubclassVTables"<ConstRefArgSub>([[SUB]]) : $()
// CHECK:         dealloc_stack [[TEMP]]

public final class InoutArgSub: ReferenceConstructed {
  public let t: Int64 = 1
  public init(_ i: inout Int32) { super.init(&i, 0) }
}
// CHECK-LABEL: sil{{.*}} @$s{{.*}}11InoutArgSubCyACs5Int32Vzcfc
// CHECK:         [[ACCESS:%.*]] = begin_access [modify] [unknown] %0
// CHECK:         [[CTOR:%.*]] = function_ref @$sSo20ReferenceConstructedVyABs5Int32Vz_ADtcfcTo : $@convention(cxx_method) (@inout Int32, Int32, @guaranteed ReferenceConstructed) -> @owned ReferenceConstructed
// CHECK:         apply [[CTOR]]([[ACCESS]], {{%.*}}, {{%.*}})
// CHECK-NEXT:    end_access [[ACCESS]]

public final class ByValueArgSub: ReferenceConstructed {
  public let t: Int64 = 1
  public init(_ arg: NonTrivialArg) { super.init(arg, CLong(0)) }
}
// CHECK-LABEL: sil{{.*}} @$s{{.*}}13ByValueArgSubCyACSo010NonTrivialD0Vcfc
// CHECK:         [[TEMP:%.*]] = alloc_stack $NonTrivialArg
// CHECK:         copy_addr %0 to [init] [[TEMP]]
// CHECK-SYSV:    [[CTOR:%.*]] = function_ref @$sSo20ReferenceConstructedVyABSo13NonTrivialArgV_SitcfcTo : $@convention(cxx_method) (@in_cxx NonTrivialArg, Int, @guaranteed ReferenceConstructed) -> @owned ReferenceConstructed
// CHECK-WIN:     [[CTOR:%.*]] = function_ref @$sSo20ReferenceConstructedVyABSo13NonTrivialArgV_s5Int32VtcfcTo : $@convention(cxx_method) (@in NonTrivialArg, Int32, @guaranteed ReferenceConstructed) -> @owned ReferenceConstructed
// CHECK:         [[NEWSUPER:%.*]] = apply [[CTOR]]([[TEMP]], {{%.*}}, {{%.*}})
// CHECK-NOT:     end_borrow
// CHECK-NEXT:    dealloc_stack [[TEMP]]
// CHECK-NEXT:    end_lifetime
// CHECK-NEXT:    unchecked_ref_cast [[NEWSUPER]] to $ByValueArgSub

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
// The stored property is passed to the C++ reference parameter in place, so
// that a reference the base keeps stays valid.
// CHECK:         [[BORROW:%.*]] = begin_borrow [[SUPER]]
// CHECK:         [[ARGADDR:%.*]] = ref_element_addr {{%.*}}, #SelfNonTrivialArgSub.arg
// CHECK-NOT:     alloc_stack $NonTrivialArg
// CHECK-NOT:     copy_addr
// CHECK-NOT:     end_borrow [[BORROW]]
// CHECK:         [[CTOR:%.*]] = function_ref @$sSo20ReferenceConstructedVyABSo13NonTrivialArgV_s4Int8VtcfcTo : $@convention(cxx_method) (@in_guaranteed NonTrivialArg, Int8, @guaranteed ReferenceConstructed) -> @owned ReferenceConstructed
// CHECK-NOT:     begin_borrow [[SUPER]]
// CHECK:         [[NEWSUPER:%.*]] = apply [[CTOR]]([[ARGADDR]], {{%.*}}, [[SUPER]])
// CHECK-NEXT:    end_borrow [[BORROW]]
// CHECK-NEXT:    end_lifetime [[SUPER]]
// CHECK-NEXT:    [[SUB:%.*]] = unchecked_ref_cast [[NEWSUPER]] to $SelfNonTrivialArgSub
// CHECK-NEXT:    builtin "initializeForeignReferenceSubclassVTables"<SelfNonTrivialArgSub>([[SUB]]) : $()

public final class ArgHolder {
  public let arg: NonTrivialArg
  public init(arg: NonTrivialArg) { self.arg = arg }
}

// A stored property of another object is passed in place as well.
public final class OtherNonTrivialArgSub: ReferenceConstructed {
  public let t: Int64 = 1
  public init(_ holder: ArgHolder) {
    super.init(holder.arg, CChar(0))
  }
}
// CHECK-LABEL: sil{{.*}} @$s{{.*}}21OtherNonTrivialArgSubCyAcA0E6HolderCcfc
// CHECK:         [[SUPER:%.*]] = upcast {{%.*}} to $ReferenceConstructed
// CHECK:         [[HOLDER:%.*]] = begin_borrow %0
// CHECK:         [[ARGADDR:%.*]] = ref_element_addr [[HOLDER]], #ArgHolder.arg
// CHECK-NOT:     alloc_stack $NonTrivialArg
// CHECK-NOT:     copy_addr
// CHECK:         [[CTOR:%.*]] = function_ref @$sSo20ReferenceConstructedVyABSo13NonTrivialArgV_s4Int8VtcfcTo
// CHECK:         apply [[CTOR]]([[ARGADDR]], {{%.*}}, [[SUPER]])

public final class PicksDoubleSub: OverloadedConstructed {
  public let t: Int64 = 1
  public init() { super.init(2.0) }
}
// CHECK-LABEL: sil{{.*}} @$s{{.*}}14PicksDoubleSubCACycfc
// CHECK:         [[CTOR:%.*]] = function_ref @$sSo21OverloadedConstructedVyABSdcfcTo : $@convention(cxx_method) (Double, @guaranteed OverloadedConstructed) -> @owned OverloadedConstructed
// CHECK:         apply [[CTOR]]({{%.*}}, {{%.*}})

// A `self.init` delegation to a C++ constructor, e.g. from a convenience
// initializer in a Swift extension of the base, is an ordinary factory call.
extension SharedConstructed {
  public convenience init(twice a: Int32) {
    self.init(a * 2)
  }
}
// CHECK-LABEL: sil{{.*}} @$sSo17SharedConstructedV{{.*}}5twiceABs5Int32V_tcfC
// CHECK-NOT:     builtin "initializeForeignReferenceSubclassVTables"
// CHECK:         [[CTOR:%.*]] = function_ref @$sSo17SharedConstructedVyABs5Int32VcfCTo : $@convention(c) (Int32) -> @owned SharedConstructed
// CHECK-NOT:     builtin "initializeForeignReferenceSubclassVTables"
// CHECK:         apply [[CTOR]]
// CHECK-NOT:     builtin "initializeForeignReferenceSubclassVTables"
// CHECK:         return

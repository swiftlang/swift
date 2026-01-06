// RUN: %target-swift-emit-silgen -enable-experimental-feature NoImplicitCopy -enable-library-evolution %s | %FileCheck %s
// RUN: %target-swift-emit-sil -O -sil-verify-all -enable-experimental-feature NoImplicitCopy -enable-library-evolution %s

// REQUIRES: swift_feature_NoImplicitCopy

////////////////////////
// MARK: Declarations //
////////////////////////

public struct EmptyStruct : ~Copyable {}
public struct NonEmptyStruct : ~Copyable {
    var e = EmptyStruct()
}
public class CopyableKlass {
    var s = NonEmptyStruct()

    let letStruct = NonEmptyStruct()
}

public func borrowVal(_ x: borrowing EmptyStruct) {}

//////////////////////
// MARK: DeinitTest //
//////////////////////

// CHECK-LABEL: sil [ossa] @$s26moveonly_library_evolution10DeinitTestVfD : $@convention(method) (@in DeinitTest) -> () {
// CHECK: bb0([[ARG:%.*]] : $*DeinitTest):
// CHECK:   [[TMP:%[^,]+]] = alloc_stack
// CHECK:   [[MUNV:%[^,]+]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TMP]]
// CHECK:   copy_addr [take] [[ARG]] to [init] [[MUNV]]
// CHECK:   drop_deinit [[MUNV]]
// CHECK: } // end sil function '$s26moveonly_library_evolution10DeinitTestVfD'
public struct DeinitTest : ~Copyable {
    deinit {
    }
}

//////////////////////////////////////////
// MARK: Caller Argument Spilling Tests //
//////////////////////////////////////////

// CHECK-LABEL: sil [ossa] @$s26moveonly_library_evolution29callerArgumentSpillingTestArgyyAA13CopyableKlassCF : $@convention(thin) (@guaranteed CopyableKlass) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $CopyableKlass):
// CHECK:    [[ADDR:%.*]] = ref_element_addr [[ARG]]
// CHECK:    [[MARKED_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ADDR]]
// CHECK:    [[GEP:%.*]] = struct_element_addr [[MARKED_ADDR]]
// CHECK:    apply {{%.*}}([[GEP]]) : $@convention(thin) (@in_guaranteed EmptyStruct) -> ()
// CHECK: } // end sil function '$s26moveonly_library_evolution29callerArgumentSpillingTestArgyyAA13CopyableKlassCF'
public func callerArgumentSpillingTestArg(_ x: CopyableKlass) {
    borrowVal(x.letStruct.e)
}

/////////////////////////////////////
// MARK: UsableFromInline in Class //
/////////////////////////////////////

public class CopyableKlass2 {
    public init() {}
}

@frozen
public struct E : ~Copyable {
    var x = CopyableKlass2()
}

public class UsableFromInlineTestKlass {
    // Read accessor
    //
    // CHECK-LABEL: sil [ossa] @$s26moveonly_library_evolution25UsableFromInlineTestKlassC1eAA1EVvr : $@yield_once @convention(method) (@guaranteed UsableFromInlineTestKlass) -> @yields @guaranteed E {
    // CHECK: bb0([[ARG:%.*]] : @guaranteed
    // CHECK:   [[FIELD:%.*]] = ref_element_addr [[ARG]]
    // CHECK:   [[ACCESS:%.*]] = begin_access [read] [dynamic] [[FIELD]]
    // CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
    // CHECK:   [[LOAD:%.*]] = load [copy] [[MARK]]
    // CHECK:   yield [[LOAD]]
    // CHECK: } // end sil function '$s26moveonly_library_evolution25UsableFromInlineTestKlassC1eAA1EVvr'

    // Setter
    // CHECK-LABEL: sil [ossa] @$s26moveonly_library_evolution25UsableFromInlineTestKlassC1eAA1EVvs : $@convention(method) (@owned E, @guaranteed UsableFromInlineTestKlass) -> () {
    // CHECK: bb0([[NEW_VALUE:%.*]] : @owned $E, [[SELF:%.*]] : @guaranteed
    // CHECK:  [[VALUE:%.*]] = alloc_box ${ let E }
    // CHECK:  [[PROJECT:%.*]] = project_box [[VALUE]]
    // CHECK:  store [[NEW_VALUE]] to [init] [[PROJECT]]
    // CHECK:  [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
    // CHECK:  [[LOAD:%.*]] = load [copy] [[MARK]]
    // CHECK:  [[REF:%.*]] = ref_element_addr [[SELF]]
    // CHECK:  [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[REF]]
    // CHECK:  [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
    // CHECK:  assign [[LOAD]] to [[MARK]]
    // CHECK: } // end sil function '$s26moveonly_library_evolution25UsableFromInlineTestKlassC1eAA1EVvs'

    // Modify
    // CHECK-LABEL: sil [ossa] @$s26moveonly_library_evolution25UsableFromInlineTestKlassC1eAA1EVvM : $@yield_once @convention(method) (@guaranteed UsableFromInlineTestKlass) -> @yields @inout E {
    // CHECK: bb0([[ARG:%.*]] : @guaranteed
    // CHECK:   [[FIELD:%.*]] = ref_element_addr [[ARG]]
    // CHECK:   [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[FIELD]]
    // CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
    // CHECK:   yield [[MARK]]
    // CHECK: } // end sil function '$s26moveonly_library_evolution25UsableFromInlineTestKlassC1eAA1EVvM'
    @usableFromInline
    var e = E()
}


func useUsableFromInlineTestKlass() {
    let k = UsableFromInlineTestKlass()
    k.e = E()
}

// rdar://142690658 (In ~Copyable public struct, an init with COW type param causes compiler error)
//
// CHECK-LABEL: sil hidden [ossa] @$s26moveonly_library_evolution19NonCopyableWithInitV1sACSS_tcfC :
// CHECK-SAME: $@convention(method) (@owned String, @thin NonCopyableWithInit.Type) -> @out NonCopyableWithInit {
// CHECK: bb0(%0 : $*NonCopyableWithInit, %1 : @owned $String, %2 : $@thin NonCopyableWithInit.Type):
// CHECK:   [[BOX:%.*]] = project_box %{{.*}}, 0
// CHECK:   store %{{.*}} to [init] [[BOX]]
// CHECK:   [[MD:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[BOX]]
// CHECK:   copy_addr [[MD]] to [init] %0
public struct NonCopyableWithInit: ~Copyable {
  init(s: String) {}
}

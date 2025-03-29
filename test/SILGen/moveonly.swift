// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

//////////////////
// Declarations //
//////////////////

public final class CopyableKlass {
    var fd = FD()
}

public struct FD: ~Copyable {
    var copyableKlass = CopyableKlass()
}

public struct NonTrivialStruct2: ~Copyable {
    var fd = FD()
    var copyableKlass = CopyableKlass()
}

public struct NonTrivialStruct: ~Copyable {
    var fd = FD()
    var nonTrivialStruct2 = NonTrivialStruct2()
    var copyableKlass = CopyableKlass()
    var nonTrivialCopyableStruct = NonTrivialCopyableStruct()
}

public struct NonTrivialCopyableStruct2 {
    var copyableKlass = CopyableKlass()
    var copyableKlass2 = CopyableKlass()
}

public struct NonTrivialCopyableStruct {
    var copyableKlass = CopyableKlass()
    var nonTrivialCopyableStruct2 = NonTrivialCopyableStruct2()
}

public enum NonTrivialEnum: ~Copyable {
    case first
    case second(CopyableKlass)
    case third(NonTrivialStruct)
}

public struct AddressOnlyGeneric<T> : ~Copyable {
    var t: T
}

public protocol P {
}
extension CopyableKlass : P {}

public struct AddressOnlyProtocol: ~Copyable {
    var t: P = CopyableKlass()

    func nonMutatingFunc() {}
    mutating func mutatingFunc() {}
}

var varGlobal = NonTrivialStruct()
let letGlobal = NonTrivialStruct()

public func borrowVal(_ e : borrowing NonTrivialEnum) {}
public func borrowVal(_ e : borrowing FD) {}
public func borrowVal(_ k: borrowing CopyableKlass) {}
public func borrowVal(_ k: borrowing NonTrivialCopyableStruct) {}
public func borrowVal(_ k: borrowing NonTrivialCopyableStruct2) {}
public func borrowVal(_ s: borrowing NonTrivialStruct) {}
public func borrowVal(_ s: borrowing NonTrivialStruct2) {}
public func borrowVal<T>(_ s: borrowing AddressOnlyGeneric<T>) {}
public func borrowVal(_ s: borrowing AddressOnlyProtocol) {}

public func consumeVal(_ e : __owned NonTrivialEnum) {}
public func consumeVal(_ e : __owned FD) {}
public func consumeVal(_ k: __owned CopyableKlass) {}
public func consumeVal(_ k: __owned NonTrivialCopyableStruct) {}
public func consumeVal(_ k: __owned NonTrivialCopyableStruct2) {}
public func consumeVal(_ s: __owned NonTrivialStruct) {}
public func consumeVal(_ s: __owned NonTrivialStruct2) {}
public func consumeVal<T>(_ s: __owned AddressOnlyGeneric<T>) {}
public func consumeVal(_ s: __owned AddressOnlyProtocol) {}

var bool: Bool { false }

///////////
// Tests //
///////////

//===---
// Function Arguments
//

// CHECK-LABEL: sil [ossa] @$s8moveonly19useNonTrivialStructyyAA0cdE0VF : $@convention(thin) (@guaranteed NonTrivialStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $NonTrivialStruct):
// CHECK:   [[COPIED_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPIED_ARG]]
// CHECK: } // end sil function '$s8moveonly19useNonTrivialStructyyAA0cdE0VF'
public func useNonTrivialStruct(_ s: borrowing NonTrivialStruct) {
    borrowVal(s)
    let s2 = s
    let k = s.fd
    let _ = k
    borrowVal(s)
    let _ = s2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly24useNonTrivialOwnedStructyyAA0cdF0VnF : $@convention(thin) (@owned NonTrivialStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : @owned $NonTrivialStruct):
// CHECK:   [[BOX:%.*]] = alloc_box
// CHECK:   [[PROJECT:%.*]] = project_box [[BOX]]
// CHECK:   store [[ARG]] to [init] [[PROJECT]]
// CHECK:   mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// CHECK: } // end sil function '$s8moveonly24useNonTrivialOwnedStructyyAA0cdF0VnF'
public func useNonTrivialOwnedStruct(_ s: __owned NonTrivialStruct) {
    borrowVal(s)
    let s2 = s
    let k = s.fd
    let _ = k
    borrowVal(s)
    let _ = s2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly17useNonTrivialEnumyyAA0cdE0OF : $@convention(thin) (@guaranteed NonTrivialEnum) -> () {
public func useNonTrivialEnum(_ s: borrowing NonTrivialEnum) {
    borrowVal(s)
    let s2 = s
    switch consume s {
    case _:
        break
    }
    borrowVal(s)
    let _ = s2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly22useNonTrivialOwnedEnumyyAA0cdF0OnF : $@convention(thin) (@owned NonTrivialEnum) -> () {
public func useNonTrivialOwnedEnum(_ s: __owned NonTrivialEnum) {
    borrowVal(s)
    let s2 = s
    switch consume s {
    case _:
        break
    }
    borrowVal(s)
    let _ = s2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly21useAddressOnlyGenericyyAA0cdE0VyxGhlF : $@convention(thin) <T> (@in_guaranteed AddressOnlyGeneric<T>) -> () {
// CHECK: bb0([[ARG:%.*]] :
// CHECK:   mark_unresolved_non_copyable_value [no_consume_or_assign] [[ARG]]
// CHECK: } // end sil function '$s8moveonly21useAddressOnlyGenericyyAA0cdE0VyxGhlF'
public func useAddressOnlyGeneric<T>(_ s: __shared AddressOnlyGeneric<T>) {
    borrowVal(s)
    let s2 = s
    let k = s.t
    let _ = k
    borrowVal(s)
    let _ = s2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly26useOwnedAddressOnlyGenericyyAA0deF0VyxGnlF : $@convention(thin) <T> (@in AddressOnlyGeneric<T>) -> () {
// CHECK: bb0([[ARG:%.*]] :
// CHECK:   mark_unresolved_non_copyable_value [consumable_and_assignable] [[ARG]]
// CHECK: } // end sil function '$s8moveonly26useOwnedAddressOnlyGenericyyAA0deF0VyxGnlF'
public func useOwnedAddressOnlyGeneric<T>(_ s: __owned AddressOnlyGeneric<T>) {
    borrowVal(s)
    let s2 = s
    let k = s.t
    let _ = k
    borrowVal(s)
    let _ = s2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly22useAddressOnlyProtocolyyAA0cdE0VhF : $@convention(thin) (@in_guaranteed AddressOnlyProtocol) -> () {
// CHECK: bb0([[ARG:%.*]] :
// CHECK:   mark_unresolved_non_copyable_value [no_consume_or_assign] [[ARG]]
// CHECK: } // end sil function '$s8moveonly22useAddressOnlyProtocolyyAA0cdE0VhF'
public func useAddressOnlyProtocol(_ s: __shared AddressOnlyProtocol) {
    borrowVal(s)
    let s2 = s
    let k = s.t
    let _ = k
    borrowVal(s)
    let _ = s2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly27useOwnedAddressOnlyProtocolyyAA0deF0VnF : $@convention(thin) (@in AddressOnlyProtocol) -> () {
// CHECK: bb0([[ARG:%.*]] :
// CHECK:   mark_unresolved_non_copyable_value [consumable_and_assignable] [[ARG]]
// CHECK: } // end sil function '$s8moveonly27useOwnedAddressOnlyProtocolyyAA0deF0VnF'
public func useOwnedAddressOnlyProtocol(_ s: __owned AddressOnlyProtocol) {
    borrowVal(s)
    let s2 = s
    let k = s.t
    let _ = k
    borrowVal(s)
    let _ = s2
}

//===---
// Self in Init
//

//===---
// Self in Methods
//

extension NonTrivialStruct {
    // CHECK-LABEL: sil hidden [ossa] @$s8moveonly16NonTrivialStructV13testNoUseSelfyyF : $@convention(method) (@guaranteed NonTrivialStruct) -> () {
    // CHECK: bb0([[ARG:%.*]] : @guaranteed $NonTrivialStruct):
    // CHECK:   [[COPIED_ARG:%.*]] = copy_value [[ARG]]
    // CHECK:   mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPIED_ARG]]
    // CHECK: } // end sil function '$s8moveonly16NonTrivialStructV13testNoUseSelfyyF'
    func testNoUseSelf() {
        let x = self
        let _ = x
    }
}

extension NonTrivialEnum {
    // CHECK-LABEL: sil hidden [ossa] @$s8moveonly14NonTrivialEnumO13testNoUseSelfyyF : $@convention(method) (@guaranteed NonTrivialEnum) -> () {
    // CHECK: bb0([[ARG:%.*]] : @guaranteed $NonTrivialEnum):
    // CHECK:   [[COPIED_ARG:%.*]] = copy_value [[ARG]]
    // CHECK:   mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPIED_ARG]]
    // CHECK: } // end sil function '$s8moveonly14NonTrivialEnumO13testNoUseSelfyyF'
    func testNoUseSelf() {
        let x = self
        let _ = x
    }
}

extension AddressOnlyGeneric {
    // CHECK-LABEL: sil hidden [ossa] @$s8moveonly18AddressOnlyGenericV13testNoUseSelfyyF : $@convention(method) <T> (@in_guaranteed AddressOnlyGeneric<T>) -> () {
    // CHECK: bb0([[ARG_IN:%.*]] :
    // CHECK:   [[ARG:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ARG_IN]] :
    //
    // CHECK:   [[ALLOC_X:%.*]] = alloc_box $<τ_0_0> { let AddressOnlyGeneric<τ_0_0> } <T>, let, name "x"
    // CHECK:   [[X:%.*]] = begin_borrow [lexical] [var_decl] [[ALLOC_X]]
    // CHECK:   [[PROJECT_X:%.*]] = project_box [[X]]
    // CHECK:   copy_addr [[ARG]] to [init] [[PROJECT_X]]
    // CHECK:   [[MARKED_X:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT_X]]
    // CHECK:   [[BLACKHOLE_ADDR:%.*]] = alloc_stack $AddressOnlyGeneric<T>
    // CHECK:   copy_addr [[MARKED_X]] to [init] [[BLACKHOLE_ADDR]]
    // CHECK:   destroy_addr [[BLACKHOLE_ADDR]]
    // CHECK:   dealloc_stack [[BLACKHOLE_ADDR]]
    //
    // CHECK:   [[ALLOC_Y:%.*]] = alloc_box $<τ_0_0> { let AddressOnlyGeneric<τ_0_0> } <T>, let, name "y"
    // CHECK:   [[Y:%.*]] = begin_borrow [lexical] [var_decl] [[ALLOC_Y]]
    // CHECK:   [[PROJECT_Y:%.*]] = project_box [[Y]]
    // CHECK:   copy_addr [[ARG]] to [init] [[PROJECT_Y]]
    // CHECK:   [[MARKED_Y:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT_Y]]
    // CHECK:   [[BLACKHOLE_ADDR:%.*]] = alloc_stack $AddressOnlyGeneric<T>
    // CHECK:   copy_addr [[MARKED_Y]] to [init] [[BLACKHOLE_ADDR]]
    // CHECK:   destroy_addr [[BLACKHOLE_ADDR]]
    // CHECK:   dealloc_stack [[BLACKHOLE_ADDR]]
    //
    // CHECK: } // end sil function '$s8moveonly18AddressOnlyGenericV13testNoUseSelfyyF'
    func testNoUseSelf() {
        let x = self
        let _ = x
        let y = self
        let _ = y
    }
}

extension AddressOnlyProtocol {
    // CHECK-LABEL: sil hidden [ossa] @$s8moveonly19AddressOnlyProtocolV13testNoUseSelfyyF : $@convention(method) (@in_guaranteed AddressOnlyProtocol) -> () {
    // CHECK: bb0([[ARG_IN:%.*]] :
    // CHECK:   [[ARG:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ARG_IN]] :
    //
    // CHECK:   [[ALLOC_X:%.*]] = alloc_box ${ let AddressOnlyProtocol }, let, name "x"
    // CHECK:   [[X:%.*]] = begin_borrow [lexical] [var_decl] [[ALLOC_X]]
    // CHECK:   [[PROJECT_X:%.*]] = project_box [[X]]
    // CHECK:   copy_addr [[ARG]] to [init] [[PROJECT_X]]
    // CHECK:   [[MARKED_X:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT_X]]
    // CHECK:   [[BLACKHOLE_ADDR:%.*]] = alloc_stack $AddressOnlyProtocol
    // CHECK:   copy_addr [[MARKED_X]] to [init] [[BLACKHOLE_ADDR]]
    // CHECK:   destroy_addr [[BLACKHOLE_ADDR]]
    // CHECK:   dealloc_stack [[BLACKHOLE_ADDR]]
    //
    // CHECK:   [[ALLOC_Y:%.*]] = alloc_box ${ let AddressOnlyProtocol }, let, name "y"
    // CHECK:   [[Y:%.*]] = begin_borrow [lexical] [var_decl] [[ALLOC_Y]]
    // CHECK:   [[PROJECT_Y:%.*]] = project_box [[Y]]
    // CHECK:   copy_addr [[ARG]] to [init] [[PROJECT_Y]]
    // CHECK:   [[MARKED_Y:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT_Y]]
    // CHECK:   [[BLACKHOLE_ADDR:%.*]] = alloc_stack $AddressOnlyProtocol
    // CHECK:   copy_addr [[MARKED_Y]] to [init] [[BLACKHOLE_ADDR]]
    // CHECK:   destroy_addr [[BLACKHOLE_ADDR]]
    // CHECK:   dealloc_stack [[BLACKHOLE_ADDR]]
    // CHECK: } // end sil function '$s8moveonly19AddressOnlyProtocolV13testNoUseSelfyyF'
    func testNoUseSelf() {
        let x = self
        let _ = x
        let y = self
        let _ = y
    }
}

///////////////////////////////
// Black Hole Initialization //
///////////////////////////////

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly27blackHoleLetInitialization1yyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROWED_BOX:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly2FDVACycfC :
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: store [[X]] to [init] [[PROJECT]]
//
// CHECK: [[MARKED:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// CHECK: [[VALUE:%.*]] = load [copy] [[MARKED]]
// CHECK: move_value [[VALUE]]
// CHECK: } // end sil function '$s8moveonly27blackHoleLetInitialization1yyF'
func blackHoleLetInitialization1() {
    let x = FD()
    let _ = x
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly27blackHoleLetInitialization2yyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROWED_BOX:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly2FDVACycfC :
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: store [[X]] to [init] [[PROJECT]]
//
// CHECK: [[MARKED:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// CHECK: [[VALUE:%.*]] = load [copy] [[MARKED]]
// CHECK: move_value [[VALUE]]
// CHECK: } // end sil function '$s8moveonly27blackHoleLetInitialization2yyF'
func blackHoleLetInitialization2() {
    let x = FD()
    var _ = x
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly27blackHoleVarInitialization1yyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BOX_BORROW:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX_BORROW]]
// CHECK: store {{%.*}} to [init] [[PROJECT_BOX]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly2FDVACycfC :
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: assign [[X]] to [[MARKED_ADDR]]
//
// CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[READ]]
// CHECK: [[LD:%.*]] = load [copy] [[MARKED_ADDR]]
// CHECK: [[CONSUME:%.*]] = move_value [[LD]]
// CHECK: } // end sil function '$s8moveonly27blackHoleVarInitialization1yyF'
func blackHoleVarInitialization1() {
    var x = FD()
    x = FD()
    let _ = x
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly27blackHoleVarInitialization2yyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BOX_BORROW:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX_BORROW]]
// CHECK: store {{%.*}} to [init] [[PROJECT_BOX]]
//
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly2FDVACycfC :
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: assign [[X]] to [[MARKED_ADDR]]
//
// CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[READ]]
// CHECK: [[LD:%.*]] = load [copy] [[MARKED_ADDR]]
// CHECK: [[CONSUME:%.*]] = move_value [[LD]]
// CHECK: } // end sil function '$s8moveonly27blackHoleVarInitialization2yyF'
func blackHoleVarInitialization2() {
    var x = FD()
    x = FD()
    var _ = x
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly27blackHoleVarInitialization3yyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BOX_BORROW:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX_BORROW]]
// CHECK: store {{%.*}} to [init] [[PROJECT_BOX]]
//
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly2FDVACycfC :
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: assign [[X]] to [[MARKED_ADDR]]
//
// CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[READ]]
// CHECK: [[LD:%.*]] = load [copy] [[MARKED_ADDR]]
// CHECK: [[CONSUME:%.*]] = move_value [[LD]]
// CHECK: } // end sil function '$s8moveonly27blackHoleVarInitialization3yyF'
func blackHoleVarInitialization3() {
    var x = FD()
    x = FD()
    _ = x
}

////////////////////////////////
// Borrow Function Call Tests //
////////////////////////////////

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly24borrowObjectFunctionCallyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
//
// CHECK: [[PROJECT:%.*]] = project_box [[BORROW_BOX]]
// CHECK: store {{%.*}} to [init] [[PROJECT]]
//
// CHECK: [[CLS:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// CHECK: [[BORROW:%.*]] = load_borrow [[CLS]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA2FDVF :
// CHECK: apply [[FN]]([[BORROW]])
// CHECK: end_borrow [[BORROW]]
// CHECK: } // end sil function '$s8moveonly24borrowObjectFunctionCallyyF'
func borrowObjectFunctionCall() {
    let k = FD()
    borrowVal(k)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly29moveOnlyStructNonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROWED_BOX:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK: store {{%.*}} to [init] [[PROJECT]]
//
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable]
// CHECK: assign {{%.*}} to [[MARKED_ADDR]]
//
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[BORROW:%.*]] = load [copy] [[MARKED_ADDR]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA16NonTrivialStructVF :
// CHECK: apply [[FN]]([[BORROW]])
// CHECK: destroy_value [[BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly29moveOnlyStructNonConsumingUseyyF'
func moveOnlyStructNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly018moveOnlyStructMovecD15NonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical] [var_decl]
// CHECK: [[PROJECT:%.*]] = project_box [[BORROW_BOX]]
//
// CHECK: store
// CHECK: assign
//
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[MARKED_ADDR]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialStruct2
// CHECK:   [[BORROW:%.*]] = load [copy] [[GEP]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA17NonTrivialStruct2VF : $@convention(thin) (@guaranteed NonTrivialStruct2) -> ()
// CHECK:   apply [[FN]]([[BORROW]])
// CHECK:   destroy_value [[BORROW]]
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly018moveOnlyStructMovecD15NonConsumingUseyyF'
func moveOnlyStructMoveOnlyStructNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.nonTrivialStruct2)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly018moveOnlyStructMovecD28CopyableKlassNonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical] [var_decl]
// CHECK: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]

// CHECK: store
// CHECK: assign
//
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[MARKED_ADDR]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialStruct2
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialStruct2, #NonTrivialStruct2.copyableKlass
// CHECK:   [[COPY:%.*]] = load [copy] [[GEP2]] : $*CopyableKlass
// CHECK:   end_access [[ACCESS]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA13CopyableKlassCF : $@convention(thin) (@guaranteed CopyableKlass) -> ()
// CHECK:   apply [[FN]]([[COPY]])
// CHECK:   destroy_value [[COPY]] : $CopyableKlass
// CHECK: } // end sil function '$s8moveonly018moveOnlyStructMovecD28CopyableKlassNonConsumingUseyyF'
func moveOnlyStructMoveOnlyStructCopyableKlassNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.nonTrivialStruct2.copyableKlass)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly021moveOnlyStructSetMoveC5FieldyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box ${ var NonTrivialStruct }
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT1:%.*]] = project_box [[BORROW_BOX]]
// CHECK: store
//
// CHECK: [[BOX2:%.*]] = alloc_box ${ let NonTrivialStruct2 }
// CHECK: [[BORROW_BOX2:%.*]] = begin_borrow [lexical] [var_decl] [[BOX2]]
// CHECK: [[PROJECT2:%.*]] = project_box [[BORROW_BOX2]]
// CHECK: store
//
// CHECK:   [[MARKED_ADDR2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT2]] : $*NonTrivialStruct2
// CHECK:   [[CONSUMED_LET:%.*]] = load [copy] [[MARKED_ADDR2]] : $*NonTrivialStruct2
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT1]]
// CHECK:   [[MARKED_ADDR1:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[MARKED_ADDR1]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialStruct2
// CHECK:   assign [[CONSUMED_LET]] to [[GEP]] : $*NonTrivialStruct2
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly021moveOnlyStructSetMoveC5FieldyyF'
func moveOnlyStructSetMoveOnlyField() {
    var k = NonTrivialStruct()
    let new = NonTrivialStruct2()
    k.nonTrivialStruct2 = new
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly022moveOnlyStructCopyableD15NonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical] [var_decl]
// CHECK: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
//
// CHECK: store
// CHECK: assign
//
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[MARKED_ADDR]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialCopyableStruct
// CHECK:   [[COPY:%.*]] = load [copy] [[GEP]] : $*NonTrivialCopyableStruct
// CHECK:   end_access [[ACCESS]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA24NonTrivialCopyableStructVF :
// CHECK:   apply [[FN]]([[COPY]])
// CHECK:   destroy_value [[COPY]] : $NonTrivialCopyableStruct
// CHECK: } // end sil function '$s8moveonly022moveOnlyStructCopyableD15NonConsumingUseyyF'
func moveOnlyStructCopyableStructNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.nonTrivialCopyableStruct)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly022moveOnlyStructCopyabledE20KlassNonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical] [var_decl]
// CHECK: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
//
// CHECK: store
// CHECK: assign
//
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[MARKED_ADDR]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialCopyableStruct
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialCopyableStruct, #NonTrivialCopyableStruct.copyableKlass
// CHECK:   [[COPY:%.*]] = load [copy] [[GEP2]] : $*CopyableKlass
// CHECK:   end_access [[ACCESS]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA13CopyableKlassCF :
// CHECK:   apply [[FN]]([[COPY]])
// CHECK:   destroy_value [[COPY]] : $CopyableKlass
// CHECK: } // end sil function '$s8moveonly022moveOnlyStructCopyabledE20KlassNonConsumingUseyyF'
func moveOnlyStructCopyableStructCopyableKlassNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.nonTrivialCopyableStruct.copyableKlass)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly022moveOnlyStructCopyabledeD15NonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical] [var_decl]
// CHECK: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
//
// CHECK: store
// CHECK: assign
//
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[MARKED_ADDR]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialCopyableStruct
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialCopyableStruct, #NonTrivialCopyableStruct.nonTrivialCopyableStruct2
// CHECK:   [[COPY:%.*]] = load [copy] [[GEP2]] : $*NonTrivialCopyableStruct2
// CHECK:   end_access [[ACCESS]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA25NonTrivialCopyableStruct2VF :
// CHECK:   apply [[FN]]([[COPY]])
// CHECK:   destroy_value [[COPY]] : $NonTrivialCopyableStruct2
// CHECK: } // end sil function '$s8moveonly022moveOnlyStructCopyabledeD15NonConsumingUseyyF'
func moveOnlyStructCopyableStructCopyableStructNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.nonTrivialCopyableStruct.nonTrivialCopyableStruct2)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly022moveOnlyStructCopyablededE20KlassNonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical] [var_decl]
// CHECK: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
//
// CHECK: store
// CHECK: assign
//
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[MARKED_ADDR]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialCopyableStruct
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialCopyableStruct, #NonTrivialCopyableStruct.nonTrivialCopyableStruct2
// CHECK:   [[GEP3:%.*]] = struct_element_addr [[GEP2]] : $*NonTrivialCopyableStruct2, #NonTrivialCopyableStruct2.copyableKlass
// CHECK:   [[COPY:%.*]] = load [copy] [[GEP3]] : $*CopyableKlass
// CHECK:   end_access [[ACCESS]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA13CopyableKlassCF :
// CHECK:   apply [[FN]]([[COPY]])
// CHECK:   destroy_value [[COPY]] : $CopyableKlass
// CHECK: } // end sil function '$s8moveonly022moveOnlyStructCopyablededE20KlassNonConsumingUseyyF'
func moveOnlyStructCopyableStructCopyableStructCopyableKlassNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.nonTrivialCopyableStruct.nonTrivialCopyableStruct2.copyableKlass)
}

// We fail here b/c we are accessing through a class.
//
// CHECK-LABEL: sil hidden [ossa] @$s8moveonly022moveOnlyStructCopyabledede9KlassMovecF15NonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical] [var_decl]
// CHECK: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
//
// CHECK: store
// CHECK: assign
//
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[MARKED_ADDR]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialCopyableStruct
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialCopyableStruct, #NonTrivialCopyableStruct.nonTrivialCopyableStruct2
// CHECK:   [[GEP3:%.*]] = struct_element_addr [[GEP2]] : $*NonTrivialCopyableStruct2, #NonTrivialCopyableStruct2.copyableKlass
// CHECK:   [[COPYABLE_KLASS:%.*]] = load [copy] [[GEP3]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   [[BORROWED_COPYABLE_KLASS:%.*]] = begin_borrow [[COPYABLE_KLASS]]
// CHECK:   [[FIELD:%.*]] = ref_element_addr [[BORROWED_COPYABLE_KLASS]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [dynamic] [[FIELD]]
// CHECK:   [[ACCESS_MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK:   [[BORROWED_MOVEONLY_KLASS:%.*]] = load [copy] [[ACCESS_MARK]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA2FDVF :
// CHECK:   apply [[FN]]([[BORROWED_MOVEONLY_KLASS]])
// CHECK:   destroy_value [[BORROWED_MOVEONLY_KLASS]]
// CHECK:   destroy_value [[COPYABLE_KLASS]]
// CHECK: } // end sil function '$s8moveonly022moveOnlyStructCopyabledede9KlassMovecF15NonConsumingUseyyF'
func moveOnlyStructCopyableStructCopyableStructCopyableKlassMoveOnlyKlassNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.nonTrivialCopyableStruct.nonTrivialCopyableStruct2.copyableKlass.fd)
}

//////////////////////
// Assignment Tests //
//////////////////////

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly19assignCopyableKlassyyAA0cD0CF : $@convention(thin) (@guaranteed CopyableKlass) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed
// CHECK:   [[REF:%.*]] = ref_element_addr [[ARG]]
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[REF]]
// CHECK:   [[MARKED_ACCESS:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK:   assign {{%.*}} to [[MARKED_ACCESS]]
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly19assignCopyableKlassyyAA0cD0CF'
func assignCopyableKlass(_ x: CopyableKlass) {
    x.fd = FD()
}

///////////////////////
// Enum Switch Tests //
///////////////////////

enum EnumSwitchTests {
    enum E2: ~Copyable {
        case lhs(CopyableKlass)
        case rhs(FD)
    }

    enum E: ~Copyable {
        case first(NonTrivialStruct2)
        case second(NonTrivialStruct)
        case third(CopyableKlass)
        case fourth(E2)
    }
}

func consumeVal(_ e: __owned EnumSwitchTests.E2) {}

var booleanGuard: Bool { false }
var booleanGuard2: Bool { false }

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly15enumSwitchTest1yyAA04EnumC5TestsO1EOF : $@convention(thin) (@guaranteed EnumSwitchTests.E) -> () {
func enumSwitchTest1(_ e: borrowing EnumSwitchTests.E) {
    switch consume e {
    case .first:
        break
    case .second(let x):
        borrowVal(x)
        break
    case .third(let y):
        borrowVal(y)
        break
    case .fourth where booleanGuard:
        break
    case .fourth(.lhs(let lhs)):
        borrowVal(lhs)
        break
    case .fourth(.rhs(let rhs)):
        consumeVal(rhs)
        break
    }
}

//////////////////////
// Global Addr Test //
//////////////////////

// Make sure that we emit a new global_addr for each use.
// CHECK-LABEL: sil hidden [ossa] @$s8moveonly16testGlobalBorrowyyF : $@convention(thin) () -> () {
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[GLOBAL]]
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[LOADED_VAL:%.*]] = load [copy] [[MARKED_GLOBAL]] : $*NonTrivialStruct
// CHECK: apply {{%.*}}([[LOADED_VAL]])
// CHECK: destroy_value [[LOADED_VAL]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[GLOBAL]]
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARKED_GLOBAL]]
// CHECK: [[LOADED_VAL:%.*]] = load [copy] [[GEP]] : $*NonTrivialStruct2
// CHECK: apply {{%.*}}([[LOADED_VAL]])
// CHECK: destroy_value [[LOADED_VAL]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9letGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[GLOBAL]]
// CHECK: [[LOADED_VAL:%.*]] = load [copy] [[MARKED_GLOBAL]] : $*NonTrivialStruct
// CHECK: apply {{%.*}}([[LOADED_VAL]])
// CHECK: destroy_value [[LOADED_VAL]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9letGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[GLOBAL]]
// CHECK: [[LOADED_VAL:%.*]] = load_borrow [[MARKED_GLOBAL]] : $*NonTrivialStruct
// CHECK: [[LOADED_GEP:%.*]] = struct_extract [[LOADED_VAL]] : $NonTrivialStruct, #NonTrivialStruct.nonTrivialStruct2
// CHECK: apply {{%.*}}([[LOADED_GEP]])
// CHECK: end_borrow [[LOADED_VAL]]
// CHECK: } // end sil function '$s8moveonly16testGlobalBorrowyyF'
func testGlobalBorrow() {
    borrowVal(varGlobal)
    borrowVal(varGlobal.nonTrivialStruct2)
    borrowVal(letGlobal)
    borrowVal(letGlobal.nonTrivialStruct2)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly17testGlobalConsumeyyF : $@convention(thin) () -> () {
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[ACCESS:%.*]] = begin_access [deinit] [dynamic] [[GLOBAL]]
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[LOADED_VAL:%.*]] = load [take]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[ACCESS:%.*]] = begin_access [deinit] [dynamic] [[GLOBAL]]
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARKED_GLOBAL]]
// CHECK: [[LOADED_VAL:%.*]] = load [take] [[GEP]]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9letGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[GLOBAL]]
// CHECK: [[LOADED_VAL:%.*]] = load [copy] [[MARKED_GLOBAL]]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9letGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[GLOBAL]]
// CHECK: [[LOADED_VAL:%.*]] = load_borrow [[MARKED_GLOBAL]]
// CHECK: [[LOADED_GEP:%.*]] = struct_extract [[LOADED_VAL]]
// CHECK: [[LOADED_GEP_COPY:%.*]] = copy_value [[LOADED_GEP]]
// CHECK: apply {{%.*}}([[LOADED_GEP_COPY]])
// CHECK: end_borrow [[LOADED_VAL]]
//
// CHECK: } // end sil function '$s8moveonly17testGlobalConsumeyyF'
func testGlobalConsume() {
    consumeVal(varGlobal)
    consumeVal(varGlobal.nonTrivialStruct2)
    consumeVal(letGlobal)
    consumeVal(letGlobal.nonTrivialStruct2)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly16testGlobalAssignyyF : $@convention(thin) () -> () {
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL]]
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: assign {{%.*}} to [[MARKED_GLOBAL]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL]]
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: assign {{%.*}} to [[MARKED_GLOBAL]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL]]
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARKED_GLOBAL]]
// CHECK: assign {{%.*}} to [[GEP]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL]]
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARKED_GLOBAL]]
// CHECK: assign {{%.*}} to [[GEP]]
// CHECK: end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly16testGlobalAssignyyF'
func testGlobalAssign() {
    varGlobal = NonTrivialStruct()
    varGlobal = NonTrivialStruct()
    varGlobal.nonTrivialStruct2 = NonTrivialStruct2()
    varGlobal.nonTrivialStruct2 = NonTrivialStruct2()
}

/////////////////////////////////
// MARK: Closure Capture Tests //
/////////////////////////////////

// Make sure that we insert a mark_unresolved_non_copyable_value on the capture value.
// CHECK-LABEL: sil hidden [ossa] @$s8moveonly49checkMarkUnresolvedNonCopyableValueInstOnCaptured1xyAA2FDVn_tF : $@convention(thin) (@owned FD) -> () {
// CHECK: bb0([[ARG:%.*]] : @owned
// CHECK:   [[BOX:%.*]] = alloc_box
// CHECK:   [[PROJECT:%.*]] = project_box [[BOX]]
// CHECK:   store [[ARG]] to [init] [[PROJECT]]
//
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly49checkMarkUnresolvedNonCopyableValueInstOnCaptured1xyAA2FDVn_tFyyXEfU_ : $@convention(thin) @substituted <τ_0_0> (@guaranteed FD) -> @out τ_0_0 for <()>
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// CHECK:   [[VALUE:%.*]] = load [copy] [[MARK]]
// CHECK:   [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[FN]]([[VALUE]])
// CHECK: } // end sil function '$s8moveonly49checkMarkUnresolvedNonCopyableValueInstOnCaptured1xyAA2FDVn_tF'
func checkMarkUnresolvedNonCopyableValueInstOnCaptured(x: __owned FD) {
    func clodger<T>(_: () -> T) {}
    clodger({ consumeVal(x) })
}

//////////////////
// Empty Struct //
//////////////////

struct EmptyStruct: ~Copyable {
  // Make sure we explicitly initialize empty struct as appropriate despite the
  // fact we do not have any fields.
  //
  // CHECK-LABEL: sil hidden [ossa] @$s8moveonly11EmptyStructVACycfC : $@convention(method) (@thin EmptyStruct.Type) -> @owned EmptyStruct {
  // CHECK: [[BOX:%.*]] = alloc_box ${ var EmptyStruct }, var, name "self"
  // CHECK: [[MARKED_UNINIT:%.*]] = mark_uninitialized [rootself] [[BOX]]
  // CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[MARKED_UNINIT]]
  // CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
  // CHECK: [[STRUCT:%.*]] = struct $EmptyStruct ()
  // CHECK: store [[STRUCT]] to [init] [[PROJECT]]
  // CHECK: [[MV_CHECK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[PROJECT]]
  // CHECK: [[LOADED_VALUE:%.*]] = load [copy] [[MV_CHECK]]
  // CHECK: destroy_value [[MARKED_UNINIT]]
  // CHECK: return [[LOADED_VALUE]]
  // CHECK: } // end sil function '$s8moveonly11EmptyStructVACycfC'
  init() {
  }
}

///////////////////////////////
// Conditionally Initialized //
///////////////////////////////

// CHECK: sil hidden [ossa] @$s8moveonly31testConditionallyInitializedLetyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box ${ let NonTrivialStruct }, let, name "x"
// CHECK: [[MARK_UNINIT:%.*]] = mark_uninitialized [var] [[BOX]]
// CHECK: [[BORROW:%.*]] = begin_borrow [lexical] [var_decl] [[MARK_UNINIT]]
// CHECK: [[PROJECT:%.*]] = project_box [[BORROW]]
// CHECK: cond_br {{%.*}}, [[LHS_BB:bb[0-9]+]], [[RHS_BB:bb[0-9]+]]
//
// CHECK: [[LHS_BB]]:
// CHECK:   [[MARKED:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[PROJECT]]
// CHECK:   assign {{%.*}} to [[MARKED]]
// CHECK:   br [[CONT_BB:bb[0-9]+]]
//
// CHECK: [[RHS_BB]]:
// CHECK:   [[MARKED:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[PROJECT]]
// CHECK:   assign {{%.*}} to [[MARKED]]
// CHECK:   br [[CONT_BB:bb[0-9]+]]
//
// CHECK: [[CONT_BB]]:
// CHECK:   [[MARKED:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// CHECK:   [[BORROW_LOAD:%.*]] = load [copy] [[MARKED]]
// CHECK:   apply {{%.*}}([[BORROW_LOAD]])
// CHECK:   destroy_value [[BORROW_LOAD]]
// CHECK:   [[MARKED:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[PROJECT]]
// CHECK:   [[TAKE_LOAD:%.*]] = load [take] [[MARKED]]
// CHECK:   apply {{%.*}}([[TAKE_LOAD]])
// CHECK: } // end sil function '$s8moveonly31testConditionallyInitializedLetyyF'
func testConditionallyInitializedLet() {
    let x: NonTrivialStruct

    if bool {
        x = NonTrivialStruct()
    } else {
        x = NonTrivialStruct()
    }

    borrowVal(x)
    consumeVal(x)
}

////////////////////////
// MARK: Setter Tests //
////////////////////////

struct AddressOnlySetterTester : ~Copyable {
    var a: AddressOnlyProtocol {
        get { fatalError() }

        // CHECK-LABEL: sil hidden [ossa] @$s8moveonly23AddressOnlySetterTesterV1aAA0bC8ProtocolVvs : $@convention(method) (@in AddressOnlyProtocol, @inout AddressOnlySetterTester) -> () {
        // CHECK: bb0([[IN_ARG:%.*]] : $*AddressOnlyProtocol, [[SELF_INOUT_ARG:%.*]] : $*AddressOnlySetterTester):
        // CHECK: mark_unresolved_non_copyable_value [consumable_and_assignable] [[IN_ARG]]
        // CHECK: mark_unresolved_non_copyable_value [consumable_and_assignable] [[SELF_INOUT_ARG]]
        // CHECK: } // end sil function '$s8moveonly23AddressOnlySetterTesterV1aAA0bC8ProtocolVvs'
        set { fatalError() }
    }
}

public class NonFinalClassTest {
    // CHECK: sil hidden [transparent] [ossa] @$s8moveonly17NonFinalClassTestC1xAA19AddressOnlyProtocolVvs : $@convention(method) (@in AddressOnlyProtocol, @guaranteed NonFinalClassTest) -> () {
    // CHECK: bb0([[INPUT:%.*]] : $*AddressOnlyProtocol, [[SELF:%.*]] : @guaranteed
    // CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[INPUT]]
    // CHECK:   [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
    // CHECK:   copy_addr [[MARK]] to [init] [[TEMP]]
    // CHECK:   [[REF:%.*]] = ref_element_addr [[SELF]]
    // CHECK:   [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[REF]]
    // CHECK:   [[MARK2:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
    // CHECK:   copy_addr [take] [[TEMP]] to [[MARK2]]
    // CHECK:   end_access [[ACCESS]]
    // CHECK:   dealloc_stack [[TEMP]]
    // CHECK:   destroy_addr [[MARK]]
    // CHECK: } // end sil function '$s8moveonly17NonFinalClassTestC1xAA19AddressOnlyProtocolVvs'
    var x: AddressOnlyProtocol
    init(y: consuming AddressOnlyProtocol) { self.x = y }
}

/////////////////////
// MARK: Subscript //
/////////////////////

// MARK: Getter Only

public struct LoadableSubscriptGetOnlyTester : ~Copyable {
    subscript(_ i: Int) -> AddressOnlyProtocol {
        get {
            fatalError()
        }
    }
}

// CHECK-LABEL: sil [ossa] @$s8moveonly047testSubscriptGetOnly_BaseLoadable_ResultAddressE4_VaryyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box $
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
//
// The get call
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [unchecked] [[MARK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[TEMP_MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[TEMP_MARK]], {{%.*}}, [[LOAD_BORROW]])
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: apply {{%.*}}([[TEMP_MARK]])
// CHECK: destroy_addr [[TEMP_MARK]]
//
// Test the assignment
// CHECK: [[M2_BOX:%.*]] = alloc_box ${
// CHECK: [[M2_BORROW:%.*]] = begin_borrow [lexical] [var_decl] [[M2_BOX]]
// CHECK: [[M2_PROJECT:%.*]] = project_box [[M2_BORROW]]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[LOAD:%.*]] = load_borrow [unchecked] [[MARK]]
// CHECK: apply {{%.*}}([[M2_PROJECT]], {{%.*}}, [[LOAD]])
// CHECK: end_borrow [[LOAD]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[M2_MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[M2_PROJECT]]
// CHECK: end_borrow [[M2_BORROW]]
// CHECK: destroy_value [[M2_BOX]]
// CHECK: } // end sil function '$s8moveonly047testSubscriptGetOnly_BaseLoadable_ResultAddressE4_VaryyF'
public func testSubscriptGetOnly_BaseLoadable_ResultAddressOnly_Var() {
    var m = LoadableSubscriptGetOnlyTester()
    m = LoadableSubscriptGetOnlyTester()
    m[0].nonMutatingFunc()
    let m2 = m[0]
    _ = m2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly047testSubscriptGetOnly_BaseLoadable_ResultAddressE4_LetyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box $
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
//
// The get call
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [[MARK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[TEMP_MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[TEMP_MARK]], {{%.*}}, [[LOAD_BORROW]])
// CHECK: apply {{%.*}}([[TEMP_MARK]])
// CHECK: destroy_addr [[TEMP_MARK]]
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: } // end sil function '$s8moveonly047testSubscriptGetOnly_BaseLoadable_ResultAddressE4_LetyyF'
public func testSubscriptGetOnly_BaseLoadable_ResultAddressOnly_Let() {
    let m = LoadableSubscriptGetOnlyTester()
    m[0].nonMutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly047testSubscriptGetOnly_BaseLoadable_ResultAddressE6_InOut1myAA0gcdE6TesterVz_tF : $@convention(thin) (@inout LoadableSubscriptGetOnlyTester) -> () {
// CHECK: bb0([[ARG:%.*]] : $*
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[ARG]]
//
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARK]]
// CHECK:   [[LOAD_BORROW:%.*]] = load_borrow [unchecked] [[ACCESS]]
// CHECK:   [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK:   [[TEMP_MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK:   apply {{%.*}}([[TEMP_MARK]], {{%.*}}, [[LOAD_BORROW]])
// CHECK:   end_borrow [[LOAD_BORROW]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   apply {{%.*}}([[TEMP_MARK]])
// CHECK:   destroy_addr [[TEMP_MARK]]
// CHECK: } // end sil function '$s8moveonly047testSubscriptGetOnly_BaseLoadable_ResultAddressE6_InOut1myAA0gcdE6TesterVz_tF'
public func testSubscriptGetOnly_BaseLoadable_ResultAddressOnly_InOut(m: inout LoadableSubscriptGetOnlyTester) {
    m[0].nonMutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly047testSubscriptGetOnly_BaseLoadable_ResultAddressE7_GlobalyyF : $@convention(thin) () -> () {
// CHECK: [[GLOBAL_ADDR:%.*]] = global_addr @$s8moveonly36globalLoadableSubscriptGetOnlyTesterAA0cdefG0Vvp :
//
// The get call
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[GLOBAL_ADDR]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [unchecked] [[MARK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[TEMP_MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[TEMP_MARK]], {{%.*}}, [[LOAD_BORROW]])
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: apply {{%.*}}([[TEMP_MARK]])
// CHECK: destroy_addr [[TEMP_MARK]]
// CHECK: } // end sil function '$s8moveonly047testSubscriptGetOnly_BaseLoadable_ResultAddressE7_GlobalyyF'
var globalLoadableSubscriptGetOnlyTester = LoadableSubscriptGetOnlyTester()
public func testSubscriptGetOnly_BaseLoadable_ResultAddressOnly_Global() {
    globalLoadableSubscriptGetOnlyTester[0].nonMutatingFunc()
}

// Make sure that we get the same behavior when we access through another noncopyable struct.
public struct LoadableSubscriptGetOnlyTesterNonCopyableStructParent : ~Copyable {
    var tester = LoadableSubscriptGetOnlyTester()
    var computedTester: LoadableSubscriptGetOnlyTester { fatalError() }
}

// CHECK-LABEL: sil [ossa] @$s8moveonly077testSubscriptGetOnlyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressE4_VaryyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box $
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
//
// The first get call
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARK]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [unchecked] [[GEP]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[TEMP_MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[TEMP_MARK]], {{%.*}}, [[LOAD_BORROW]])
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: apply {{%.*}}([[TEMP_MARK]])
// CHECK: destroy_addr [[TEMP_MARK]]
//
// The second get call.
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [unchecked] [[MARK]]
// CHECK: [[VALUE:%.*]] = apply {{%.*}}([[LOAD_BORROW]])
//
// CHECK: [[BORROWED_VALUE:%.*]] = begin_borrow [[VALUE]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[TEMP_MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[TEMP_MARK]], {{%.*}}, [[BORROWED_VALUE]])
// CHECK: end_borrow [[BORROWED_VALUE]]
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: apply {{%.*}}([[TEMP_MARK]])
// CHECK: destroy_addr [[TEMP_MARK]]
// CHECK: destroy_value [[VALUE]]
// } // end sil function '$s8moveonly077testSubscriptGetOnlyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressE4_VaryyF'
public func testSubscriptGetOnlyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_Var() {
    var m = LoadableSubscriptGetOnlyTesterNonCopyableStructParent()
    m = LoadableSubscriptGetOnlyTesterNonCopyableStructParent()
    m.tester[0].nonMutatingFunc()
    m.computedTester[0].nonMutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly077testSubscriptGetOnlyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressE4_LetyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box ${ let L
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
//
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// CHECK: [[LOAD:%.*]] = load_borrow [[MARK]]
// CHECK: [[EXT:%.*]] = struct_extract [[LOAD]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[TEMP_MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[TEMP_MARK]], {{%.*}}, [[EXT]])
// CHECK: apply {{%.*}}([[TEMP_MARK]])
// CHECK: destroy_addr [[TEMP_MARK]]
// CHECK: end_borrow [[LOAD]]
// CHECK: } // end sil function '$s8moveonly077testSubscriptGetOnlyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressE4_LetyyF'
public func testSubscriptGetOnlyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_Let() {
    let m = LoadableSubscriptGetOnlyTesterNonCopyableStructParent()
    m.tester[0].nonMutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly077testSubscriptGetOnlyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressE6_InOut1myAA0lcde6TesterghjI0Vz_tF : $@convention(thin) (@inout LoadableSubscriptGetOnlyTesterNonCopyableStructParent) -> () {
// CHECK: bb0([[ARG:%.*]] :
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[ARG]]
//
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARK]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[ACCESS]]
// CHECK:   [[LOAD:%.*]] = load_borrow [unchecked] [[GEP]]
// CHECK:   [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK:   [[TEMP_MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK:   apply {{%.*}}([[TEMP_MARK]], {{%.*}}, [[LOAD]])
// CHECK:   end_borrow [[LOAD]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   apply {{%.*}}([[TEMP_MARK]])
// CHECK:   destroy_addr [[TEMP_MARK]]
// CHECK: } // end sil function '$s8moveonly077testSubscriptGetOnlyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressE6_InOut1myAA0lcde6TesterghjI0Vz_tF'
public func testSubscriptGetOnlyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_InOut(m: inout LoadableSubscriptGetOnlyTesterNonCopyableStructParent) {
    m.tester[0].nonMutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly077testSubscriptGetOnlyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressE7_GlobalyyF : $@convention(thin) () -> () {
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly59globalLoadableSubscriptGetOnlyTesterNonCopyableStructParentAA0cdefghijK0Vvp
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [dynamic] [[GLOBAL]]
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[MARK]]
// CHECK:   [[LOAD:%.*]] = load_borrow [unchecked] [[GEP]]
// CHECK:   [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK:   [[TEMP_MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK:   apply {{%.*}}([[TEMP_MARK]], {{%.*}}, [[LOAD]])
// CHECK:   end_borrow [[LOAD]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   apply {{%.*}}([[TEMP_MARK]])
// CHECK:   destroy_addr [[TEMP_MARK]]
// CHECK: } // end sil function '$s8moveonly077testSubscriptGetOnlyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressE7_GlobalyyF'
var globalLoadableSubscriptGetOnlyTesterNonCopyableStructParent = LoadableSubscriptGetOnlyTesterNonCopyableStructParent()
public func testSubscriptGetOnlyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_Global() {
    globalLoadableSubscriptGetOnlyTesterNonCopyableStructParent.tester[0].nonMutatingFunc()
}

public class LoadableSubscriptGetOnlyTesterClassParent {
    var tester = LoadableSubscriptGetOnlyTester()
    var computedTester: LoadableSubscriptGetOnlyTester { fatalError() }
    var testerParent = LoadableSubscriptGetOnlyTesterNonCopyableStructParent()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly065testSubscriptGetOnlyThroughParentClass_BaseLoadable_ResultAddressE4_VaryyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box $
// CHECK: [[BORROW:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BORROW]]
//
// First read.
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT_CP:%.*]] = copy_value [[CORO_RESULT_ORIG]]
// CHECK: [[CORO_RESULT_MK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_CP]]
// CHECK: [[CORO_RESULT:%.*]] = begin_borrow [[CORO_RESULT_MK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $LoadableSubscriptGetOnlyTester
// CHECK: [[TEMP_MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[TEMP]]
// CHECK: [[TEMP_MARK_BORROW:%.*]] = store_borrow [[CORO_RESULT]] to [[TEMP_MARK]]
// CHECK: [[LOAD:%.*]] = load_borrow [unchecked] [[TEMP_MARK_BORROW]]
// CHECK: [[TEMP2:%.*]] = alloc_stack $
// CHECK: [[TEMP2_MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP2]]
// CHECK: apply {{%.*}}([[TEMP2_MARK]], {{%.*}}, [[LOAD]])
// CHECK: end_borrow [[LOAD]]
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: apply {{%.*}}([[TEMP2_MARK]])
// CHECK: destroy_addr [[TEMP2_MARK]]
//
// Second read.
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT_CP:%.*]] = copy_value [[CORO_RESULT_ORIG]]
// CHECK: [[CORO_RESULT_MK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_CP]]
// CHECK: [[CORO_RESULT:%.*]] = begin_borrow [[CORO_RESULT_MK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $LoadableSubscriptGetOnlyTester
// CHECK: [[TEMP_MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[TEMP]]
// CHECK: [[TEMP_MARK_BORROW:%.*]] = store_borrow [[CORO_RESULT]] to [[TEMP_MARK]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[TEMP_MARK_BORROW]]
// CHECK: [[LOAD:%.*]] = load_borrow [unchecked] [[GEP]]
// CHECK: [[TEMP2:%.*]] = alloc_stack $
// CHECK: [[TEMP2_MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP2]]
// CHECK: apply {{%.*}}([[TEMP2_MARK]], {{%.*}}, [[LOAD]])
// CHECK: end_borrow [[LOAD]]
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: apply {{%.*}}([[TEMP2_MARK]])
// CHECK: destroy_addr [[TEMP2_MARK]]
//
// Third read.
//
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[LOAD:%.*]] = load_borrow [unchecked] [[ACCESS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[LOAD]])
// CHECK: [[CORO_RESULT_CP:%.*]] = copy_value [[CORO_RESULT_ORIG]]
// CHECK: [[CORO_RESULT_MK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_CP]]
// CHECK: [[CORO_RESULT:%.*]] = begin_borrow [[CORO_RESULT_MK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $
// CHECK: [[TEMP_MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[TEMP_MARK]], {{%.*}}, [[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[LOAD]]
// CHECK: apply {{%.*}}([[TEMP_MARK]])
// CHECK: destroy_addr [[TEMP_MARK]]

// CHECK: } // end sil function '$s8moveonly065testSubscriptGetOnlyThroughParentClass_BaseLoadable_ResultAddressE4_VaryyF'
public func testSubscriptGetOnlyThroughParentClass_BaseLoadable_ResultAddressOnly_Var() {
    var m = LoadableSubscriptGetOnlyTesterClassParent()
    m = LoadableSubscriptGetOnlyTesterClassParent()
    m.tester[0].nonMutatingFunc()
    m.testerParent.tester[0].nonMutatingFunc()
    m.computedTester[0].nonMutatingFunc()
}

// MARK: Getter + Setter.
// This is different since adding a setter changes how we codegen.

public struct LoadableSubscriptGetSetTester : ~Copyable {
    subscript(_ i: Int) -> AddressOnlyProtocol {
        get {
            fatalError()
        }
        set {
            fatalError()
        }
    }
}

// CHECK-LABEL: sil [ossa] @$s8moveonly54testSubscriptGetSet_BaseLoadable_ResultAddressOnly_VaryyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box $
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
//
// The get call
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [[MARK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[TEMP_MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[TEMP_MARK]], {{%.*}}, [[LOAD_BORROW]])
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: apply {{%.*}}([[TEMP_MARK]])
// CHECK: destroy_addr [[TEMP_MARK]]
//
// The assignment:
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}) : $@convention(method) (@thin AddressOnlyProtocol.Type) -> @out AddressOnlyProtocol
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[MARK]])
// CHECK: end_access [[ACCESS]]
//
// The mutating function call.
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [[MARK]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD_BORROW]])
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[MARK]])
// CHECK: end_access [[ACCESS]]
//
// CHECK: } // end sil function '$s8moveonly54testSubscriptGetSet_BaseLoadable_ResultAddressOnly_VaryyF'
public func testSubscriptGetSet_BaseLoadable_ResultAddressOnly_Var() {
    var m = LoadableSubscriptGetSetTester()
    m = LoadableSubscriptGetSetTester()
    m[0].nonMutatingFunc()
    m[0] = AddressOnlyProtocol()
    m[0].mutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly54testSubscriptGetSet_BaseLoadable_ResultAddressOnly_LetyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box $
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
//
// The get call
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [[MARK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD_BORROW]])
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: } // end sil function '$s8moveonly54testSubscriptGetSet_BaseLoadable_ResultAddressOnly_LetyyF'
public func testSubscriptGetSet_BaseLoadable_ResultAddressOnly_Let() {
    let m = LoadableSubscriptGetSetTester()
    m[0].nonMutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly56testSubscriptGetSet_BaseLoadable_ResultAddressOnly_InOut1myAA0gcdE6TesterVz_tF : $@convention(thin) (@inout LoadableSubscriptGetSetTester) -> () {
// CHECK: bb0([[ARG:%.*]] : $*
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[ARG]]
//
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARK]]
// CHECK:   [[LOAD_BORROW:%.*]] = load_borrow [[ACCESS]]
// CHECK:   [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK:   [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK:   apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD_BORROW]])
// CHECK:   end_borrow [[LOAD_BORROW]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   apply {{%.*}}([[MARK_TEMP]])
// CHECK:   destroy_addr [[MARK_TEMP]]
//
// The assignment:
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}) : $@convention(method) (@thin AddressOnlyProtocol.Type) -> @out AddressOnlyProtocol
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[MARK]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[ACCESS]])
// CHECK: end_access [[ACCESS]]
//
// The mutating function call.
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[MARK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [[ACCESS]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD_BORROW]])
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[ACCESS]])
// CHECK: end_access [[ACCESS]]
//
// CHECK: } // end sil function '$s8moveonly56testSubscriptGetSet_BaseLoadable_ResultAddressOnly_InOut1myAA0gcdE6TesterVz_tF'
public func testSubscriptGetSet_BaseLoadable_ResultAddressOnly_InOut(m: inout LoadableSubscriptGetSetTester) {
    m[0].nonMutatingFunc()
    m[0] = AddressOnlyProtocol()
    m[0].mutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly57testSubscriptGetSet_BaseLoadable_ResultAddressOnly_GlobalyyF : $@convention(thin) () -> () {
// CHECK: [[GLOBAL_ADDR:%.*]] = global_addr @$s8moveonly35globalLoadableSubscriptGetSetTesterAA0cdefG0Vvp
//
// The get call
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[GLOBAL_ADDR]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [[MARK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD_BORROW]])
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
//
// The assignment:
// CHECK: [[GLOBAL_ADDR:%.*]] = global_addr @$s8moveonly35globalLoadableSubscriptGetSetTesterAA0cdefG0Vvp
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}) : $@convention(method) (@thin AddressOnlyProtocol.Type) -> @out AddressOnlyProtocol
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL_ADDR]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[MARK]])
// CHECK: end_access [[ACCESS]]
//
// The mutating function call.
// CHECK: [[GLOBAL_ADDR:%.*]] = global_addr @$s8moveonly35globalLoadableSubscriptGetSetTesterAA0cdefG0Vvp
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL_ADDR]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [[MARK]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD_BORROW]])
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[MARK]])
// CHECK: end_access [[ACCESS]]
//
// CHECK: } // end sil function '$s8moveonly57testSubscriptGetSet_BaseLoadable_ResultAddressOnly_GlobalyyF'
var globalLoadableSubscriptGetSetTester = LoadableSubscriptGetSetTester()
public func testSubscriptGetSet_BaseLoadable_ResultAddressOnly_Global() {
    globalLoadableSubscriptGetSetTester[0].nonMutatingFunc()
    globalLoadableSubscriptGetSetTester[0] = AddressOnlyProtocol()
    globalLoadableSubscriptGetSetTester[0].mutatingFunc()
}

// Make sure that we get the same behavior when we access through another noncopyable struct.
public struct LoadableSubscriptGetSetTesterNonCopyableStructParent : ~Copyable {
    var tester = LoadableSubscriptGetSetTester()
    var computedTester: LoadableSubscriptGetSetTester { fatalError() }
}

// CHECK-LABEL: sil [ossa] @$s8moveonly84testSubscriptGetSetThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_VaryyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box $
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
//
// The first get call
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARK]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [[GEP]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD_BORROW]])
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
//
// The mutating call.
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: [[LOAD:%.*]] = load_borrow [[GEP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD]])
// CHECK: end_borrow [[LOAD]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[GEP]])
// CHECK: end_access [[ACCESS]]
//
// The second get call.
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [unchecked] [[MARK]]
// CHECK: [[VALUE:%.*]] = apply {{%.*}}([[LOAD_BORROW]])
// CHECK: [[BORROWED_VALUE:%.*]] = begin_borrow [[VALUE]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[BORROWED_VALUE]])
// CHECK: end_borrow [[BORROWED_VALUE]]
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
// CHECK: destroy_value [[VALUE]]
// } // end sil function '$s8moveonly077testSubscriptGetSetThroughNonCopyableParentStruct_BaseLoadable_ResultAddressE4_VaryyF'
public func testSubscriptGetSetThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_Var() {
    var m = LoadableSubscriptGetSetTesterNonCopyableStructParent()
    m = LoadableSubscriptGetSetTesterNonCopyableStructParent()
    m.tester[0].nonMutatingFunc()
    m.tester[0].mutatingFunc()
    m.computedTester[0].nonMutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly84testSubscriptGetSetThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_LetyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box ${ let L
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
//
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// CHECK: [[LOAD:%.*]] = load_borrow [[MARK]]
// CHECK: [[EXT:%.*]] = struct_extract [[LOAD]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[EXT]])
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
// CHECK: end_borrow [[LOAD]]
// CHECK: } // end sil function '$s8moveonly84testSubscriptGetSetThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_LetyyF'
public func testSubscriptGetSetThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_Let() {
    let m = LoadableSubscriptGetSetTesterNonCopyableStructParent()
    m.tester[0].nonMutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly86testSubscriptGetSetThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_InOut1myAA0lcde6TesterghjI0Vz_tF : $@convention(thin) (@inout LoadableSubscriptGetSetTesterNonCopyableStructParent) -> () {
// CHECK: bb0([[ARG:%.*]] :
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[ARG]]
//
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARK]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[ACCESS]]
// CHECK:   [[LOAD:%.*]] = load_borrow [[GEP]]
// CHECK:   [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK:   [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK:   apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD]])
// CHECK:   end_borrow [[LOAD]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   apply {{%.*}}([[MARK_TEMP]])
// CHECK:   destroy_addr [[MARK_TEMP]]
//
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [unknown] [[MARK]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[ACCESS]]
// CHECK:   [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK:   [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK:   [[LOAD:%.*]] = load_borrow [[GEP]]
// CHECK:   apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD]])
// CHECK:   end_borrow [[LOAD]]
// CHECK:   apply {{%.*}}([[MARK_TEMP]])
// CHECK:   apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[GEP]])
// CHECK: } // end sil function '$s8moveonly86testSubscriptGetSetThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_InOut1myAA0lcde6TesterghjI0Vz_tF'
public func testSubscriptGetSetThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_InOut(m: inout LoadableSubscriptGetSetTesterNonCopyableStructParent) {
    m.tester[0].nonMutatingFunc()
    m.tester[0].mutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly87testSubscriptGetSetThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_GlobalyyF : $@convention(thin) () -> () {
// CHECK:   [[GLOBAL:%.*]] = global_addr @$s8moveonly58globalLoadableSubscriptGetSetTesterNonCopyableStructParentAA0cdefghijK0Vvp
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [dynamic] [[GLOBAL]]
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[MARK]]
// CHECK:   [[LOAD:%.*]] = load_borrow [[GEP]]
// CHECK:   [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK:   [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK:   apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD]])
// CHECK:   end_borrow [[LOAD]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   apply {{%.*}}([[MARK_TEMP]])
// CHECK:   destroy_addr [[MARK_TEMP]]
//
// CHECK:   [[GLOBAL:%.*]] = global_addr @$s8moveonly58globalLoadableSubscriptGetSetTesterNonCopyableStructParentAA0cdefghijK0Vvp
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL]]
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[MARK]]
// CHECK:   [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK:   [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK:   [[LOAD:%.*]] = load_borrow [[GEP]]
// CHECK:   apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD]])
// CHECK:   end_borrow [[LOAD]]
// CHECK:   apply {{%.*}}([[MARK_TEMP]])
// CHECK:   apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[GEP]])
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly87testSubscriptGetSetThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_GlobalyyF'
var globalLoadableSubscriptGetSetTesterNonCopyableStructParent = LoadableSubscriptGetSetTesterNonCopyableStructParent()
public func testSubscriptGetSetThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_Global() {
    globalLoadableSubscriptGetSetTesterNonCopyableStructParent.tester[0].nonMutatingFunc()
    globalLoadableSubscriptGetSetTesterNonCopyableStructParent.tester[0].mutatingFunc()
}

public class LoadableSubscriptGetSetTesterClassParent {
    var tester = LoadableSubscriptGetSetTester()
    var computedTester: LoadableSubscriptGetSetTester { fatalError() }
    var computedTester2: LoadableSubscriptGetSetTester {
        get { fatalError() }
        set { fatalError() }
    }
    var testerParent = LoadableSubscriptGetSetTesterNonCopyableStructParent()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly72testSubscriptGetSetThroughParentClass_BaseLoadable_ResultAddressOnly_VaryyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box $
// CHECK: [[BORROW:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BORROW]]
//
// First read.
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT_CP:%.*]] = copy_value [[CORO_RESULT_ORIG]]
// CHECK: [[CORO_RESULT_MK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_CP]]
// CHECK: [[CORO_RESULT:%.*]] = begin_borrow [[CORO_RESULT_MK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[BORROW_COPYABLE_CLASS]]
// CHECK: destroy_value [[COPYABLE_CLASS]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
//
// First mutation.
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: [[TEMP:%.*]] = alloc_stack $
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: [[LOAD:%.*]] = load_borrow [[CORO_RESULT]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD]])
// CHECK: end_borrow [[LOAD]]
// Mutating Func
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// Setter
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
//
// Second read.
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT_CP:%.*]] = copy_value [[CORO_RESULT_ORIG]]
// CHECK: [[CORO_RESULT_MK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_CP]]
// CHECK: [[CORO_RESULT:%.*]] = begin_borrow [[CORO_RESULT_MK]]
// CHECK: [[EXT:%.*]] = struct_extract [[CORO_RESULT]]
// CHECK: [[TEMP:%.*]] = alloc_stack $
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[EXT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[BORROW_COPYABLE_CLASS]]
// CHECK: destroy_value [[COPYABLE_CLASS]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
//
// Second mutate
//
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[CORO_RESULT]]
// CHECK: [[TEMP:%.*]] = alloc_stack $
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: [[LOAD:%.*]] = load_borrow [[GEP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD]])
// CHECK: end_borrow [[LOAD]]
// Mutating func
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// Setter func
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[GEP]])
// CHECK: end_apply [[CORO_TOKEN]]
//
// Third read.
//
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[CLASS:%.*]] = load_borrow [unchecked] [[ACCESS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[CLASS]])
// CHECK: [[CORO_RESULT_CP:%.*]] = copy_value [[CORO_RESULT_ORIG]]
// CHECK: [[CORO_RESULT_MK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_CP]]
// CHECK: [[CORO_RESULT:%.*]] = begin_borrow [[CORO_RESULT_MK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[CLASS]]
// CHECK: end_access [[ACCESS]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
//
// First read
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT_CP:%.*]] = copy_value [[CORO_RESULT_ORIG]]
// CHECK: [[CORO_RESULT_MK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_CP]]
// CHECK: [[CORO_RESULT:%.*]] = begin_borrow [[CORO_RESULT_MK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// Getter
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
//
// Mutation
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: [[TEMP:%.*]] = alloc_stack $
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: [[LOAD:%.*]] = load_borrow [[CORO_RESULT]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD]])
// CHECK: end_borrow [[LOAD]]
// Mutating Func
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// Setter
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: } // end sil function '$s8moveonly72testSubscriptGetSetThroughParentClass_BaseLoadable_ResultAddressOnly_VaryyF'
public func testSubscriptGetSetThroughParentClass_BaseLoadable_ResultAddressOnly_Var() {
    var m = LoadableSubscriptGetSetTesterClassParent()
    m = LoadableSubscriptGetSetTesterClassParent()
    m.tester[0].nonMutatingFunc()
    m.tester[0].mutatingFunc()
    m.testerParent.tester[0].nonMutatingFunc()
    m.testerParent.tester[0].mutatingFunc()
    m.computedTester[0].nonMutatingFunc()
    m.computedTester2[0].nonMutatingFunc()
    m.computedTester2[0].mutatingFunc()
}

// MARK: _read and _modify

public struct LoadableSubscriptReadModifyTester : ~Copyable {
    subscript(_ i: Int) -> AddressOnlyProtocol {
        _read {
            fatalError()
        }
        _modify {
            fatalError()
        }
    }
}

// CHECK-LABEL: sil [ossa] @$s8moveonly58testSubscriptReadModify_BaseLoadable_ResultAddressOnly_VaryyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box $
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
//
// The read call
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[LOAD:%.*]] = load_borrow [[MARK]]
// Eventually, we need this end_apply to be around the nonMutatingFunc.
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[LOAD]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[LOAD]]
// CHECK: end_access [[ACCESS]]
//
// The assignment:
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}) : $@convention(method) (@thin AddressOnlyProtocol.Type) -> @out AddressOnlyProtocol
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[MARK]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: copy_addr [take] [[MARK_TEMP]] to [[CORO_RESULT]]
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_access [[ACCESS]]
//
// The mutating function call.
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[MARK]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: } // end sil function '$s8moveonly58testSubscriptReadModify_BaseLoadable_ResultAddressOnly_VaryyF'
public func testSubscriptReadModify_BaseLoadable_ResultAddressOnly_Var() {
    var m = LoadableSubscriptReadModifyTester()
    m = LoadableSubscriptReadModifyTester()
    m[0].nonMutatingFunc()
    m[0] = AddressOnlyProtocol()
    m[0].mutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly58testSubscriptReadModify_BaseLoadable_ResultAddressOnly_LetyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box $
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
//
// The get call
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [[MARK]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[LOAD_BORROW]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: } // end sil function '$s8moveonly58testSubscriptReadModify_BaseLoadable_ResultAddressOnly_LetyyF'
public func testSubscriptReadModify_BaseLoadable_ResultAddressOnly_Let() {
    let m = LoadableSubscriptReadModifyTester()
    m[0].nonMutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly60testSubscriptReadModify_BaseLoadable_ResultAddressOnly_InOut1myAA0gcdE6TesterVz_tF : $@convention(thin) (@inout LoadableSubscriptReadModifyTester) -> () {
// CHECK: bb0([[ARG:%.*]] : $*
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[ARG]]
//
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARK]]
// CHECK:   [[LOAD_BORROW:%.*]] = load_borrow [[ACCESS]]
// CHECK:   ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[LOAD_BORROW]])
// CHECK:   [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_ORIG]]
// CHECK:   apply {{%.*}}([[CORO_RESULT]])
// CHECK:   end_apply [[CORO_TOKEN]]
// CHECK:   end_borrow [[LOAD_BORROW]]
// CHECK:   end_access [[ACCESS]]
//
// The assignment:
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[MARK]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[ACCESS]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: copy_addr [take] [[MARK_TEMP]] to [[CORO_RESULT]]
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_access [[ACCESS]]
//
// The mutating function call.
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[MARK]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[ACCESS]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: } // end sil function '$s8moveonly60testSubscriptReadModify_BaseLoadable_ResultAddressOnly_InOut1myAA0gcdE6TesterVz_tF'
public func testSubscriptReadModify_BaseLoadable_ResultAddressOnly_InOut(m: inout LoadableSubscriptReadModifyTester) {
    m[0].nonMutatingFunc()
    m[0] = AddressOnlyProtocol()
    m[0].mutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly61testSubscriptReadModify_BaseLoadable_ResultAddressOnly_GlobalyyF : $@convention(thin) () -> () {
// CHECK: [[GLOBAL_ADDR:%.*]] = global_addr @$s8moveonly39globalLoadableSubscriptReadModifyTesterAA0cdefG0Vvp :
//
// The get call
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[GLOBAL_ADDR]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [[MARK]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[LOAD_BORROW]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: end_access [[ACCESS]]
//
// The assignment:
// CHECK: [[GLOBAL_ADDR:%.*]] = global_addr @$s8moveonly39globalLoadableSubscriptReadModifyTesterAA0cdefG0Vvp :
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}) : $@convention(method) (@thin AddressOnlyProtocol.Type) -> @out AddressOnlyProtocol
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL_ADDR]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[MARK]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: copy_addr [take] [[MARK_TEMP]] to [[CORO_RESULT]]
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_access [[ACCESS]]
//
// The mutating function call.
// CHECK: [[GLOBAL_ADDR:%.*]] = global_addr @$s8moveonly39globalLoadableSubscriptReadModifyTesterAA0cdefG0Vvp :
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL_ADDR]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[MARK]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: } // end sil function '$s8moveonly61testSubscriptReadModify_BaseLoadable_ResultAddressOnly_GlobalyyF'
var globalLoadableSubscriptReadModifyTester = LoadableSubscriptReadModifyTester()
public func testSubscriptReadModify_BaseLoadable_ResultAddressOnly_Global() {
    globalLoadableSubscriptReadModifyTester[0].nonMutatingFunc()
    globalLoadableSubscriptReadModifyTester[0] = AddressOnlyProtocol()
    globalLoadableSubscriptReadModifyTester[0].mutatingFunc()
}

// Make sure that we get the same behavior when we access through another noncopyable struct.
public struct LoadableSubscriptReadModifyTesterNonCopyableStructParent : ~Copyable {
    var tester = LoadableSubscriptReadModifyTester()
    var computedTester: LoadableSubscriptReadModifyTester { fatalError() }
}

// CHECK-LABEL: sil [ossa] @$s8moveonly88testSubscriptReadModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_VaryyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box $
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
//
// The first get call
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARK]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [[GEP]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[LOAD_BORROW]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: end_access [[ACCESS]]
//
// The mutating call.
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARK]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[GEP]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_access [[ACCESS]]
//
// The second get call.
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [unchecked] [[MARK]]
// CHECK: [[VALUE:%.*]] = apply {{%.*}}([[LOAD_BORROW]])
// CHECK: [[BORROWED_VALUE:%.*]] = begin_borrow [[VALUE]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[BORROWED_VALUE]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[BORROWED_VALUE]]
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: end_access [[ACCESS]]
// } // end sil function '$s8moveonly88testSubscriptReadModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_VaryyF'
public func testSubscriptReadModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_Var() {
    var m = LoadableSubscriptReadModifyTesterNonCopyableStructParent()
    m = LoadableSubscriptReadModifyTesterNonCopyableStructParent()
    m.tester[0].nonMutatingFunc()
    m.tester[0].mutatingFunc()
    m.computedTester[0].nonMutatingFunc()
}


// CHECK-LABEL: sil [ossa] @$s8moveonly88testSubscriptReadModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_LetyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box ${ let L
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
//
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// CHECK: [[LOAD:%.*]] = load_borrow [[MARK]]
// CHECK: [[EXT:%.*]] = struct_extract [[LOAD]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[EXT]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[LOAD]]
// CHECK: } // end sil function '$s8moveonly88testSubscriptReadModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_LetyyF'
public func testSubscriptReadModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_Let() {
    let m = LoadableSubscriptReadModifyTesterNonCopyableStructParent()
    m.tester[0].nonMutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly90testSubscriptReadModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_InOut1myAA0lcde6TesterghjI0Vz_tF : $@convention(thin) (@inout LoadableSubscriptReadModifyTesterNonCopyableStructParent) -> () {
// CHECK: bb0([[ARG:%.*]] :
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[ARG]]
//
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARK]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[ACCESS]]
// CHECK:   [[LOAD:%.*]] = load_borrow [[GEP]]
// CHECK:   ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[LOAD]])
// CHECK:   [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_ORIG]]
// CHECK:   apply {{%.*}}([[CORO_RESULT]])
// CHECK:   end_apply [[CORO_TOKEN]]
// CHECK:   end_borrow [[LOAD]]
// CHECK:   end_access [[ACCESS]]
//
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [unknown] [[MARK]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[ACCESS]]
// CHECK:   ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[GEP]])
// CHECK:   [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK:   apply {{%.*}}([[CORO_RESULT]])
// CHECK:   end_apply [[CORO_TOKEN]]
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly90testSubscriptReadModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_InOut1myAA0lcde6TesterghjI0Vz_tF'
public func testSubscriptReadModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_InOut(m: inout LoadableSubscriptReadModifyTesterNonCopyableStructParent) {
    m.tester[0].nonMutatingFunc()
    m.tester[0].mutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly91testSubscriptReadModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_GlobalyyF : $@convention(thin) () -> () {
// CHECK:   [[GLOBAL:%.*]] = global_addr @$s8moveonly62globalLoadableSubscriptReadModifyTesterNonCopyableStructParentAA0cdefghijK0Vvp :
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [dynamic] [[GLOBAL]]
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[MARK]]
// CHECK:   [[LOAD:%.*]] = load_borrow [[GEP]]
// CHECK:   ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[LOAD]])
// CHECK:   [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_ORIG]]
// CHECK:   apply {{%.*}}([[CORO_RESULT]])
// CHECK:   end_apply [[CORO_TOKEN]]
// CHECK:   end_borrow [[LOAD]]
// CHECK:   end_access [[ACCESS]]
//
// CHECK:   [[GLOBAL:%.*]] = global_addr @$s8moveonly62globalLoadableSubscriptReadModifyTesterNonCopyableStructParentAA0cdefghijK0Vvp :
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL]]
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[MARK]]
// CHECK:   ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[GEP]])
// CHECK:   [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK:   apply {{%.*}}([[CORO_RESULT]])
// CHECK:   end_apply [[CORO_TOKEN]]
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly91testSubscriptReadModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_GlobalyyF'
var globalLoadableSubscriptReadModifyTesterNonCopyableStructParent = LoadableSubscriptReadModifyTesterNonCopyableStructParent()
public func testSubscriptReadModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_Global() {
    globalLoadableSubscriptReadModifyTesterNonCopyableStructParent.tester[0].nonMutatingFunc()
    globalLoadableSubscriptReadModifyTesterNonCopyableStructParent.tester[0].mutatingFunc()
}

public class LoadableSubscriptReadModifyTesterClassParent {
    var tester = LoadableSubscriptReadModifyTester()
    var computedTester: LoadableSubscriptReadModifyTester { fatalError() }
    var computedTester2: LoadableSubscriptReadModifyTester {
        get { fatalError() }
        set { fatalError() }
    }
    var testerParent = LoadableSubscriptReadModifyTesterNonCopyableStructParent()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly76testSubscriptReadModifyThroughParentClass_BaseLoadable_ResultAddressOnly_VaryyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box $
// CHECK: [[BORROW:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BORROW]]
//
// First read.
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT_CP:%.*]] = copy_value [[CORO_RESULT_ORIG]]
// CHECK: [[CORO_RESULT_MK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_CP]]
// CHECK: [[CORO_RESULT:%.*]] = begin_borrow [[CORO_RESULT_MK]]
// CHECK: ([[CORO_RESULT_2_ORIG:%.*]], [[CORO_TOKEN_2:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[CORO_RESULT]])
// CHECK: [[CORO_RESULT_2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_2_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT_2]])
// CHECK: end_apply [[CORO_TOKEN_2]]
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[BORROW_COPYABLE_CLASS]]
// CHECK: destroy_value [[COPYABLE_CLASS]]
//
// First mutation.
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: ([[CORO_RESULT_2_ORIG:%.*]], [[CORO_TOKEN_2:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[CORO_RESULT]])
// CHECK: [[CORO_RESULT_2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_2_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT_2]])
// CHECK: end_apply [[CORO_TOKEN_2]]
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[BORROW_COPYABLE_CLASS]]
// CHECK: destroy_value [[COPYABLE_CLASS]]
//
// Second read.
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT_CP:%.*]] = copy_value [[CORO_RESULT_ORIG]]
// CHECK: [[CORO_RESULT_MK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_CP]]
// CHECK: [[CORO_RESULT:%.*]] = begin_borrow [[CORO_RESULT_MK]]
// CHECK: [[EXT:%.*]] = struct_extract [[CORO_RESULT]]
// CHECK: ([[CORO_RESULT_ORIG_2:%.*]], [[CORO_TOKEN_2:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[EXT]])
// CHECK: [[CORO_RESULT_2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_ORIG_2]]
// CHECK: apply {{%.*}}([[CORO_RESULT_2]])
// CHECK: end_apply [[CORO_TOKEN_2]]
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[BORROW_COPYABLE_CLASS]]
// CHECK: destroy_value [[COPYABLE_CLASS]]
//
// Second mutate
//
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[CORO_RESULT]]
// CHECK: ([[CORO_RESULT_2_ORIG:%.*]], [[CORO_TOKEN_2:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[GEP]])
// CHECK: [[CORO_RESULT_2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_2_ORIG]]
// Mutating func
// CHECK: apply {{%.*}}([[CORO_RESULT_2]])
// CHECK: end_apply [[CORO_TOKEN_2]]
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[BORROW_COPYABLE_CLASS]]
// CHECK: destroy_value [[COPYABLE_CLASS]]
//
// Third read.
//
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[CLASS:%.*]] = load_borrow [unchecked] [[ACCESS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[CLASS]])
// CHECK: [[CORO_RESULT_CP:%.*]] = copy_value [[CORO_RESULT_ORIG]]
// CHECK: [[CORO_RESULT_MK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_CP]]
// CHECK: [[CORO_RESULT:%.*]] = begin_borrow [[CORO_RESULT_MK]]
// CHECK: ([[CORO_RESULT2_ORIG:%.*]], [[CORO_TOKEN2:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[CORO_RESULT]])
// CHECK: [[CORO_RESULT2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT2_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT2]])
// CHECK: end_apply [[CORO_TOKEN2]]
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[BORROW]]
// CHECK: end_access [[ACCESS]]
//
// First read
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT_CP:%.*]] = copy_value [[CORO_RESULT_ORIG]]
// CHECK: [[CORO_RESULT_MK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_CP]]
// CHECK: [[CORO_RESULT:%.*]] = begin_borrow [[CORO_RESULT_MK]]
// CHECK: ([[CORO_RESULT_2_ORIG:%.*]], [[CORO_TOKEN_2:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[CORO_RESULT]])
// CHECK: [[CORO_RESULT_2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_2_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT_2]])
// CHECK: end_apply [[CORO_TOKEN_2]]
// CHECK: end_apply [[CORO_TOKEN]]
//
// Mutation
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: ([[CORO_RESULT_2_ORIG:%.*]], [[CORO_TOKEN_2:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[CORO_RESULT]])
// CHECK: [[CORO_RESULT_2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_2_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT_2]])
// CHECK: end_apply [[CORO_TOKEN_2]]
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: } // end sil function '$s8moveonly76testSubscriptReadModifyThroughParentClass_BaseLoadable_ResultAddressOnly_VaryyF'
public func testSubscriptReadModifyThroughParentClass_BaseLoadable_ResultAddressOnly_Var() {
    var m = LoadableSubscriptReadModifyTesterClassParent()
    m = LoadableSubscriptReadModifyTesterClassParent()
    m.tester[0].nonMutatingFunc()
    m.tester[0].mutatingFunc()
    m.testerParent.tester[0].nonMutatingFunc()
    m.testerParent.tester[0].mutatingFunc()
    m.computedTester[0].nonMutatingFunc()
    m.computedTester2[0].nonMutatingFunc()
    m.computedTester2[0].mutatingFunc()
}

// MARK: get and _modify

public struct LoadableSubscriptGetModifyTester : ~Copyable {
    subscript(_ i: Int) -> AddressOnlyProtocol {
        get {
            fatalError()
        }
        _modify {
            fatalError()
        }
    }
}

// CHECK-LABEL: sil [ossa] @$s8moveonly57testSubscriptGetModify_BaseLoadable_ResultAddressOnly_VaryyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box $
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
//
// The get call
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [[MARK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD_BORROW]])
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
//
// The assignment:
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}) : $@convention(method) (@thin AddressOnlyProtocol.Type) -> @out AddressOnlyProtocol
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[MARK]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: copy_addr [take] [[MARK_TEMP]] to [[CORO_RESULT]]
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_access [[ACCESS]]
//
// The mutating function call.
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[MARK]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: } // end sil function '$s8moveonly57testSubscriptGetModify_BaseLoadable_ResultAddressOnly_VaryyF'
public func testSubscriptGetModify_BaseLoadable_ResultAddressOnly_Var() {
    var m = LoadableSubscriptGetModifyTester()
    m = LoadableSubscriptGetModifyTester()
    m[0].nonMutatingFunc()
    m[0] = AddressOnlyProtocol()
    m[0].mutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly57testSubscriptGetModify_BaseLoadable_ResultAddressOnly_LetyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box $
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
//
// The get call
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [[MARK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD_BORROW]])
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: } // end sil function '$s8moveonly57testSubscriptGetModify_BaseLoadable_ResultAddressOnly_LetyyF'
public func testSubscriptGetModify_BaseLoadable_ResultAddressOnly_Let() {
    let m = LoadableSubscriptGetModifyTester()
    m[0].nonMutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly59testSubscriptGetModify_BaseLoadable_ResultAddressOnly_InOut1myAA0gcdE6TesterVz_tF : $@convention(thin) (@inout LoadableSubscriptGetModifyTester) -> () {
// CHECK: bb0([[ARG:%.*]] : $*
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[ARG]]
//
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARK]]
// CHECK:   [[LOAD_BORROW:%.*]] = load_borrow [[ACCESS]]
// CHECK:   [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK:   [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK:   apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD_BORROW]])
// CHECK:   end_borrow [[LOAD_BORROW]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   apply {{%.*}}([[MARK_TEMP]])
// CHECK:   destroy_addr [[MARK_TEMP]]
//
// The assignment:
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}) : $@convention(method) (@thin AddressOnlyProtocol.Type) -> @out AddressOnlyProtocol
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[MARK]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[ACCESS]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: copy_addr [take] [[MARK_TEMP]] to [[CORO_RESULT]]
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_access [[ACCESS]]
//
// The mutating function call.
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[MARK]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[ACCESS]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: } // end sil function '$s8moveonly59testSubscriptGetModify_BaseLoadable_ResultAddressOnly_InOut1myAA0gcdE6TesterVz_tF'
public func testSubscriptGetModify_BaseLoadable_ResultAddressOnly_InOut(m: inout LoadableSubscriptGetModifyTester) {
    m[0].nonMutatingFunc()
    m[0] = AddressOnlyProtocol()
    m[0].mutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly60testSubscriptGetModify_BaseLoadable_ResultAddressOnly_GlobalyyF : $@convention(thin) () -> () {
// CHECK: [[GLOBAL_ADDR:%.*]] = global_addr @$s8moveonly38globalLoadableSubscriptGetModifyTesterAA0cdefG0Vvp :
//
// The get call
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[GLOBAL_ADDR]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [[MARK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD_BORROW]])
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
//
// The assignment:
// CHECK: [[GLOBAL_ADDR:%.*]] = global_addr @$s8moveonly38globalLoadableSubscriptGetModifyTesterAA0cdefG0Vvp :
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}) : $@convention(method) (@thin AddressOnlyProtocol.Type) -> @out AddressOnlyProtocol
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL_ADDR]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[MARK]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: copy_addr [take] [[MARK_TEMP]] to [[CORO_RESULT]]
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_access [[ACCESS]]
//
// The mutating function call.
// CHECK: [[GLOBAL_ADDR:%.*]] = global_addr @$s8moveonly38globalLoadableSubscriptGetModifyTesterAA0cdefG0Vvp :
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL_ADDR]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[MARK]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: } // end sil function '$s8moveonly60testSubscriptGetModify_BaseLoadable_ResultAddressOnly_GlobalyyF'
var globalLoadableSubscriptGetModifyTester = LoadableSubscriptGetModifyTester()
public func testSubscriptGetModify_BaseLoadable_ResultAddressOnly_Global() {
    globalLoadableSubscriptGetModifyTester[0].nonMutatingFunc()
    globalLoadableSubscriptGetModifyTester[0] = AddressOnlyProtocol()
    globalLoadableSubscriptGetModifyTester[0].mutatingFunc()
}

// Make sure that we get the same behavior when we access through another noncopyable struct.
public struct LoadableSubscriptGetModifyTesterNonCopyableStructParent : ~Copyable {
    var tester = LoadableSubscriptGetModifyTester()
    var computedTester: LoadableSubscriptGetModifyTester { fatalError() }
}

// CHECK-LABEL: sil [ossa] @$s8moveonly87testSubscriptGetModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_VaryyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box $
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
//
// The first get call
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARK]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [[GEP]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD_BORROW]])
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
//
// The mutating call.
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARK]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[GEP]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_access [[ACCESS]]
//
// The second get call.
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK: [[LOAD_BORROW:%.*]] = load_borrow [unchecked] [[MARK]]
// CHECK: [[VALUE:%.*]] = apply {{%.*}}([[LOAD_BORROW]])
// CHECK: [[BORROWED_VALUE:%.*]] = begin_borrow [[VALUE]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[BORROWED_VALUE]])
// CHECK: end_borrow [[BORROWED_VALUE]]
// CHECK: end_borrow [[LOAD_BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
// CHECK: destroy_value [[VALUE]]
// } // end sil function '$s8moveonly077testSubscriptGetModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressE4_VaryyF'
public func testSubscriptGetModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_Var() {
    var m = LoadableSubscriptGetModifyTesterNonCopyableStructParent()
    m = LoadableSubscriptGetModifyTesterNonCopyableStructParent()
    m.tester[0].nonMutatingFunc()
    m.tester[0].mutatingFunc()
    m.computedTester[0].nonMutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly87testSubscriptGetModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_LetyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box ${ let L
// CHECK: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
//
// CHECK: [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// CHECK: [[LOAD:%.*]] = load_borrow [[MARK]]
// CHECK: [[EXT:%.*]] = struct_extract [[LOAD]]
// CHECK: [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[EXT]])
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
// CHECK: end_borrow [[LOAD]]
// CHECK: } // end sil function '$s8moveonly87testSubscriptGetModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_LetyyF'
public func testSubscriptGetModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_Let() {
    let m = LoadableSubscriptGetModifyTesterNonCopyableStructParent()
    m.tester[0].nonMutatingFunc()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly90testSubscriptGetModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_GlobalyyF : $@convention(thin) () -> () {
// CHECK:   [[GLOBAL:%.*]] = global_addr @$s8moveonly61globalLoadableSubscriptGetModifyTesterNonCopyableStructParentAA0cdefghijK0Vvp :
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [dynamic] [[GLOBAL]]
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[MARK]]
// CHECK:   [[LOAD:%.*]] = load_borrow [[GEP]]
// CHECK:   [[TEMP:%.*]] = alloc_stack $AddressOnlyProtocol
// CHECK:   [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK:   apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[LOAD]])
// CHECK:   end_borrow [[LOAD]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   apply {{%.*}}([[MARK_TEMP]])
// CHECK:   destroy_addr [[MARK_TEMP]]
//
// CHECK:   [[GLOBAL:%.*]] = global_addr @$s8moveonly61globalLoadableSubscriptGetModifyTesterNonCopyableStructParentAA0cdefghijK0Vvp :
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL]]
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[MARK]]
// CHECK:   ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[GEP]])
// CHECK:   [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK:   apply {{%.*}}([[CORO_RESULT]])
// CHECK:   end_apply [[CORO_TOKEN]]
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly90testSubscriptGetModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_GlobalyyF'
var globalLoadableSubscriptGetModifyTesterNonCopyableStructParent = LoadableSubscriptGetModifyTesterNonCopyableStructParent()
public func testSubscriptGetModifyThroughNonCopyableParentStruct_BaseLoadable_ResultAddressOnly_Global() {
    globalLoadableSubscriptGetModifyTesterNonCopyableStructParent.tester[0].nonMutatingFunc()
    globalLoadableSubscriptGetModifyTesterNonCopyableStructParent.tester[0].mutatingFunc()
}

public class LoadableSubscriptGetModifyTesterClassParent {
    var tester = LoadableSubscriptGetModifyTester()
    var computedTester: LoadableSubscriptGetModifyTester { fatalError() }
    var computedTester2: LoadableSubscriptGetModifyTester {
        get { fatalError() }
        set { fatalError() }
    }
    var testerParent = LoadableSubscriptGetModifyTesterNonCopyableStructParent()
}

// CHECK-LABEL: sil [ossa] @$s8moveonly75testSubscriptGetModifyThroughParentClass_BaseLoadable_ResultAddressOnly_VaryyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box $
// CHECK: [[BORROW:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK: [[PROJECT:%.*]] = project_box [[BORROW]]
//
// First read.
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT_CP:%.*]] = copy_value [[CORO_RESULT_ORIG]]
// CHECK: [[CORO_RESULT_MK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_CP]]
// CHECK: [[CORO_RESULT:%.*]] = begin_borrow [[CORO_RESULT_MK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[BORROW_COPYABLE_CLASS]]
// CHECK: destroy_value [[COPYABLE_CLASS]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
//
// First mutation.
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: ([[CORO_RESULT_2_ORIG:%.*]], [[CORO_TOKEN_2:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[CORO_RESULT]])
// CHECK: [[CORO_RESULT_2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_2_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT_2]])
// CHECK: end_apply [[CORO_TOKEN_2]]
// CHECK: end_apply [[CORO_TOKEN]]
//
// Second read.
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT_CP:%.*]] = copy_value [[CORO_RESULT_ORIG]]
// CHECK: [[CORO_RESULT_MK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_CP]]
// CHECK: [[CORO_RESULT:%.*]] = begin_borrow [[CORO_RESULT_MK]]
// CHECK: [[EXT:%.*]] = struct_extract [[CORO_RESULT]]
// CHECK: [[TEMP:%.*]] = alloc_stack $
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[EXT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[BORROW_COPYABLE_CLASS]]
// CHECK: destroy_value [[COPYABLE_CLASS]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
//
// Second mutate
//
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[CORO_RESULT]]
// CHECK: ([[CORO_RESULT_2_ORIG:%.*]], [[CORO_TOKEN_2:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[GEP]])
// CHECK: [[CORO_RESULT_2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_2_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT_2]])
// CHECK: end_apply [[CORO_TOKEN_2]]
// CHECK: end_apply [[CORO_TOKEN]]
//
// Third read.
//
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[CLASS:%.*]] = load_borrow [unchecked] [[ACCESS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[CLASS]])
// CHECK: [[CORO_RESULT_CP:%.*]] = copy_value [[CORO_RESULT_ORIG]]
// CHECK: [[CORO_RESULT_MK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_CP]]
// CHECK: [[CORO_RESULT:%.*]] = begin_borrow [[CORO_RESULT_MK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: end_borrow [[CLASS]]
// CHECK: end_access [[ACCESS]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
//
// First read
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT_CP:%.*]] = copy_value [[CORO_RESULT_ORIG]]
// CHECK: [[CORO_RESULT_MK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CORO_RESULT_CP]]
// CHECK: [[CORO_RESULT:%.*]] = begin_borrow [[CORO_RESULT_MK]]
// CHECK: [[TEMP:%.*]] = alloc_stack $
// CHECK: [[MARK_TEMP:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[TEMP]]
// Getter
// CHECK: apply {{%.*}}([[MARK_TEMP]], {{%.*}}, [[CORO_RESULT]])
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: apply {{%.*}}([[MARK_TEMP]])
// CHECK: destroy_addr [[MARK_TEMP]]
//
// Mutation
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[COPYABLE_CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[BORROW_COPYABLE_CLASS:%.*]] = begin_borrow [[COPYABLE_CLASS]]
// CHECK: ([[CORO_RESULT_ORIG:%.*]], [[CORO_TOKEN:%.*]]) = begin_apply {{%.*}}([[BORROW_COPYABLE_CLASS]])
// CHECK: [[CORO_RESULT:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_ORIG]]
// CHECK: ([[CORO_RESULT_2_ORIG:%.*]], [[CORO_TOKEN_2:%.*]]) = begin_apply {{%.*}}({{%.*}}, [[CORO_RESULT]])
// CHECK: [[CORO_RESULT_2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CORO_RESULT_2_ORIG]]
// CHECK: apply {{%.*}}([[CORO_RESULT_2]])
// CHECK: end_apply [[CORO_TOKEN_2]]
// CHECK: end_apply [[CORO_TOKEN]]
// CHECK: } // end sil function '$s8moveonly75testSubscriptGetModifyThroughParentClass_BaseLoadable_ResultAddressOnly_VaryyF'
public func testSubscriptGetModifyThroughParentClass_BaseLoadable_ResultAddressOnly_Var() {
    var m = LoadableSubscriptGetModifyTesterClassParent()
    m = LoadableSubscriptGetModifyTesterClassParent()
    m.tester[0].nonMutatingFunc()
    m.tester[0].mutatingFunc()
    m.testerParent.tester[0].nonMutatingFunc()
    m.testerParent.tester[0].mutatingFunc()
    m.computedTester[0].nonMutatingFunc()
    m.computedTester2[0].nonMutatingFunc()
    m.computedTester2[0].mutatingFunc()
}

//////////////////////////////
// MARK: Capture Self Tests //
//////////////////////////////

func testSelfCaptureHandledCorrectly() {
    struct E {
        var a: [Int] = []
    }

    struct Test : ~Copyable {
        var e: E

        // Test that we capture inits by address.
        //
        // CHECK-LABEL: sil private [ossa] @$s8moveonly31testSelfCaptureHandledCorrectlyyyF4TestL_VADycfC : $@convention(method) (@thin Test.Type) -> @owned Test {
        // CHECK: [[BOX:%.*]] = alloc_box ${ var Test }
        // CHECK: [[MARK_UNINIT:%.*]] = mark_uninitialized [rootself] [[BOX]]
        // CHECK: [[BORROW:%.*]] = begin_borrow [lexical] [var_decl] [[MARK_UNINIT]]
        // CHECK: [[PROJECT:%.*]] = project_box [[BORROW]]
        // CHECK: apply {{%.*}}([[PROJECT]])
        // CHECK: } // end sil function '$s8moveonly31testSelfCaptureHandledCorrectlyyyF4TestL_VADycfC'
        init() {
            e = E()
            func capture() {
                let e = self.e
            }
            capture()
        }

        // CHECK-LABEL: sil private [ossa] @$s8moveonly31testSelfCaptureHandledCorrectlyyyF4TestL_V22captureByLocalFunctionyyF : $@convention(method) (@guaranteed Test) -> () {
        // CHECK: bb0([[ARG:%.*]] : @guaranteed $Test):
        // CHECK:   [[COPY:%.*]] = copy_value [[ARG]]
        // CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY]]
        // CHECK:   [[BORROW:%.*]] = begin_borrow [[MARK]]
        // CHECK:   apply {{%.*}}([[BORROW]])
        // CHECK:   end_borrow [[BORROW]]
        // CHECK:   destroy_value [[MARK]]
        // CHECK: } // end sil function '$s8moveonly31testSelfCaptureHandledCorrectlyyyF4TestL_V22captureByLocalFunctionyyF'
        func captureByLocalFunction() {
            func capture() {
                let e = self.e
            }
            capture()
        }

        // CHECK-LABEL: sil private [ossa] @$s8moveonly31testSelfCaptureHandledCorrectlyyyF4TestL_V17captureByLocalLetyyF : $@convention(method) (@guaranteed Test) -> () {
        // CHECK: bb0([[ARG:%.*]] : @guaranteed $Test):
        // CHECK:   [[COPY:%.*]] = copy_value [[ARG]]
        // CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY]]
        // CHECK:   [[COPY2:%.*]] = copy_value [[MARK]]
        // CHECK:   [[PAI:%.*]] = partial_apply [callee_guaranteed] {{%.*}}([[COPY2]])
        // CHECK:   destroy_value [[MARK]]
        // CHECK: } // end sil function '$s8moveonly31testSelfCaptureHandledCorrectlyyyF4TestL_V17captureByLocalLetyyF'
        func captureByLocalLet() {
            let f = {
                let e = self.e
            }

            f()
        }

        // CHECK-LABEL: sil private [ossa] @$s8moveonly31testSelfCaptureHandledCorrectlyyyF4TestL_V17captureByLocalVaryyF : $@convention(method) (@guaranteed Test) -> () {
        // CHECK: bb0([[ARG:%.*]] : @guaranteed $Test):
        // CHECK:   [[COPY:%.*]] = copy_value [[ARG]]
        // CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY]]
        // CHECK:   [[COPY2:%.*]] = copy_value [[MARK]]
        // CHECK:   [[PAI:%.*]] = partial_apply [callee_guaranteed] {{%.*}}([[COPY2]])
        // CHECK:   destroy_value [[MARK]]
        // CHECK: } // end sil function '$s8moveonly31testSelfCaptureHandledCorrectlyyyF4TestL_V17captureByLocalVaryyF'
        func captureByLocalVar() {
            var f = {}
            f = {
                let e = self.e
            }
            f()
        }

        // CHECK-LABEL: sil private [ossa] @$s8moveonly31testSelfCaptureHandledCorrectlyyyF4TestL_V27captureByNonEscapingClosureyyF : $@convention(method) (@guaranteed Test) -> () {
        // CHECK: bb0([[ARG:%.*]] : @guaranteed $Test):
        // CHECK:   [[COPY:%.*]] = copy_value [[ARG]]
        // CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY]]
        // CHECK:   [[COPY2:%.*]] = copy_value [[MARK]]
        // CHECK:   [[PAI:%.*]] = partial_apply [callee_guaranteed] {{%.*}}([[COPY2]])
        // CHECK:   destroy_value [[MARK]]
        // CHECK: } // end sil function '$s8moveonly31testSelfCaptureHandledCorrectlyyyF4TestL_V27captureByNonEscapingClosureyyF'
        func captureByNonEscapingClosure() {
            func useClosure(_ f: () -> ()) {}
            useClosure {
                let e = self.e
            }
        }
    }
}

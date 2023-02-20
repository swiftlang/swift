// RUN: %target-swift-emit-silgen -enable-experimental-move-only %s | %FileCheck %s

//////////////////
// Declarations //
//////////////////

public final class CopyableKlass {
    var fd = FD()
}

@_moveOnly
public struct FD {
    var copyableKlass = CopyableKlass()
}

@_moveOnly
public struct NonTrivialStruct2 {
    var fd = FD()
    var copyableKlass = CopyableKlass()
}

@_moveOnly
public struct NonTrivialStruct {
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

@_moveOnly
public enum NonTrivialEnum {
    case first
    case second(CopyableKlass)
    case third(NonTrivialStruct)
}

var varGlobal = NonTrivialStruct()
let letGlobal = NonTrivialStruct()

public func borrowVal(_ e : __shared NonTrivialEnum) {}
public func borrowVal(_ e : __shared FD) {}
public func borrowVal(_ k: __shared CopyableKlass) {}
public func borrowVal(_ k: __shared NonTrivialCopyableStruct) {}
public func borrowVal(_ k: __shared NonTrivialCopyableStruct2) {}
public func borrowVal(_ s: __shared NonTrivialStruct) {}
public func borrowVal(_ s: __shared NonTrivialStruct2) {}

public func consumeVal(_ e : __owned NonTrivialEnum) {}
public func consumeVal(_ e : __owned FD) {}
public func consumeVal(_ k: __owned CopyableKlass) {}
public func consumeVal(_ k: __owned NonTrivialCopyableStruct) {}
public func consumeVal(_ k: __owned NonTrivialCopyableStruct2) {}
public func consumeVal(_ s: __owned NonTrivialStruct) {}
public func consumeVal(_ s: __owned NonTrivialStruct2) {}

///////////
// Tests //
///////////

//===---
// Function Arguments
//

// CHECK-LABEL: sil [ossa] @$s8moveonly19useNonTrivialStructyyAA0cdE0VhF : $@convention(thin) (@guaranteed NonTrivialStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $NonTrivialStruct):
// CHECK:   [[COPIED_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   mark_must_check [no_consume_or_assign] [[COPIED_ARG]]
// CHECK: } // end sil function '$s8moveonly19useNonTrivialStructyyAA0cdE0VhF'
public func useNonTrivialStruct(_ s: __shared NonTrivialStruct) {
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
// CHECK:   [[PROJECT:%.*]] = project_box [[BOX]]
// CHECK:   mark_must_check [assignable_but_not_consumable] [[PROJECT]]
// CHECK: } // end sil function '$s8moveonly24useNonTrivialOwnedStructyyAA0cdF0VnF'
public func useNonTrivialOwnedStruct(_ s: __owned NonTrivialStruct) {
    borrowVal(s)
    let s2 = s
    let k = s.fd
    let _ = k
    borrowVal(s)
    let _ = s2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly17useNonTrivialEnumyyAA0cdE0OhF : $@convention(thin) (@guaranteed NonTrivialEnum) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $NonTrivialEnum):
// CHECK:   [[COPIED_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   mark_must_check [no_consume_or_assign] [[COPIED_ARG]]
// CHECK: } // end sil function '$s8moveonly17useNonTrivialEnumyyAA0cdE0OhF'
public func useNonTrivialEnum(_ s: __shared NonTrivialEnum) {
    borrowVal(s)
    let s2 = s
    switch s {
    case _:
        break
    }
    borrowVal(s)
    let _ = s2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly22useNonTrivialOwnedEnumyyAA0cdF0OnF : $@convention(thin) (@owned NonTrivialEnum) -> () {
// CHECK: bb0([[ARG:%.*]] : @owned $NonTrivialEnum):
// CHECK:   [[BOX:%.*]] = alloc_box
// CHECK:   [[PROJECT:%.*]] = project_box [[BOX]]
// CHECK:   store [[ARG]] to [init] [[PROJECT]]
// CHECK:   [[PROJECT:%.*]] = project_box [[BOX]]
// CHECK:   mark_must_check [assignable_but_not_consumable] [[PROJECT]]
// CHECK: } // end sil function '$s8moveonly22useNonTrivialOwnedEnumyyAA0cdF0OnF'
public func useNonTrivialOwnedEnum(_ s: __owned NonTrivialEnum) {
    borrowVal(s)
    let s2 = s
    switch s {
    case _:
        break
    }
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
    // CHECK:   mark_must_check [no_consume_or_assign] [[COPIED_ARG]]
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
    // CHECK:   mark_must_check [no_consume_or_assign] [[COPIED_ARG]]
    // CHECK: } // end sil function '$s8moveonly14NonTrivialEnumO13testNoUseSelfyyF'
    func testNoUseSelf() {
        let x = self
        let _ = x
    }
}

///////////////////////////////
// Black Hole Initialization //
///////////////////////////////

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly27blackHoleLetInitialization1yyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROWED_BOX:%.*]] = begin_borrow [lexical] [[BOX]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly2FDVACycfC :
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK: store [[X]] to [init] [[PROJECT]]
//
// CHECK: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK: [[MARKED:%.*]] = mark_must_check [assignable_but_not_consumable] [[PROJECT]]
// CHECK: [[VALUE:%.*]] = load [copy] [[MARKED]]
// CHECK: move_value [[VALUE]]
// CHECK: } // end sil function '$s8moveonly27blackHoleLetInitialization1yyF'
func blackHoleLetInitialization1() {
    let x = FD()
    let _ = x
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly27blackHoleLetInitialization2yyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROWED_BOX:%.*]] = begin_borrow [lexical] [[BOX]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly2FDVACycfC :
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK: store [[X]] to [init] [[PROJECT]]
//
// CHECK: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK: [[MARKED:%.*]] = mark_must_check [assignable_but_not_consumable] [[PROJECT]]
// CHECK: [[VALUE:%.*]] = load [copy] [[MARKED]]
// CHECK: move_value [[VALUE]]
// CHECK: } // end sil function '$s8moveonly27blackHoleLetInitialization2yyF'
func blackHoleLetInitialization2() {
    let x = FD()
    var _ = x
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly27blackHoleVarInitialization1yyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BOX_BORROW:%.*]] = begin_borrow [lexical] [[BOX]]
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX_BORROW]]
// CHECK: store {{%.*}} to [init] [[PROJECT_BOX]]
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX_BORROW]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly2FDVACycfC :
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: assign [[X]] to [[MARKED_ADDR]]
//
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX_BORROW]]
// CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[READ]]
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
// CHECK: [[BOX_BORROW:%.*]] = begin_borrow [lexical] [[BOX]]
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX_BORROW]]
// CHECK: store {{%.*}} to [init] [[PROJECT_BOX]]
//
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX_BORROW]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly2FDVACycfC :
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: assign [[X]] to [[MARKED_ADDR]]
//
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX_BORROW]]
// CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[READ]]
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
// CHECK: [[BOX_BORROW:%.*]] = begin_borrow [lexical] [[BOX]]
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX_BORROW]]
// CHECK: store {{%.*}} to [init] [[PROJECT_BOX]]
//
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX_BORROW]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly2FDVACycfC :
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: assign [[X]] to [[MARKED_ADDR]]
//
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX_BORROW]]
// CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[READ]]
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
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical] [[BOX]]
//
// CHECK: [[PROJECT:%.*]] = project_box [[BORROW_BOX]]
// CHECK: store {{%.*}} to [init] [[PROJECT]]
//
// CHECK: [[PROJECT:%.*]] = project_box [[BORROW_BOX]]
// CHECK: [[CLS:%.*]] = mark_must_check [assignable_but_not_consumable] [[PROJECT]]
// CHECK: [[BORROW:%.*]] = load_borrow [[CLS]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA2FDVhF :
// CHECK: apply [[FN]]([[BORROW]])
// CHECK: end_borrow [[BORROW]]
// CHECK: } // end sil function '$s8moveonly24borrowObjectFunctionCallyyF'
func borrowObjectFunctionCall() {
    let k = FD()
    borrowVal(k)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly29moveOnlyStructNonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROWED_BOX:%.*]] = begin_borrow [lexical] [[BOX]]
//
// TODO: We should have a begin_access [init] here probably.
// CHECK: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK: store {{%.*}} to [init] [[PROJECT]]
//
// CHECK: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable]
// CHECK: assign {{%.*}} to [[MARKED_ADDR]]
//
// CHECK: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[BORROW:%.*]] = load_borrow [[MARKED_ADDR]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA16NonTrivialStructVhF :
// CHECK: apply [[FN]]([[BORROW]])
// CHECK: end_borrow [[BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly29moveOnlyStructNonConsumingUseyyF'
func moveOnlyStructNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly018moveOnlyStructMovecD15NonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical]
//
// CHECK: project_box
// CHECK: store
// CHECK: project_box
// CHECK: assign
//
// CHECK:   [[PROJECT:%.*]] = project_box [[BORROW_BOX]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[MARKED_ADDR]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialStruct2
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA17NonTrivialStruct2VhF : $@convention(thin) (@guaranteed NonTrivialStruct2) -> ()
// CHECK:   apply [[FN]]([[BORROW]])
// CHECK:   end_borrow [[BORROW]]
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly018moveOnlyStructMovecD15NonConsumingUseyyF'
func moveOnlyStructMoveOnlyStructNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.nonTrivialStruct2)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly018moveOnlyStructMovecD28CopyableKlassNonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical]
//
// CHECK: project_box
// CHECK: store
// CHECK: project_box
// CHECK: assign
//
// CHECK:   [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[MARKED_ADDR]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialStruct2
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialStruct2, #NonTrivialStruct2.copyableKlass
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP2]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA13CopyableKlassChF : $@convention(thin) (@guaranteed CopyableKlass) -> ()
// CHECK:   apply [[FN]]([[BORROW]])
// CHECK:   end_borrow [[BORROW]]
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly018moveOnlyStructMovecD28CopyableKlassNonConsumingUseyyF'
func moveOnlyStructMoveOnlyStructCopyableKlassNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.nonTrivialStruct2.copyableKlass)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly42moveOnlyStructCopyableKlassNonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical]
//
// CHECK: project_box
// CHECK: store
// CHECK: project_box
// CHECK: assign
//
// CHECK:   [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[MARKED_ADDR]] : $*NonTrivialStruct, #NonTrivialStruct.copyableKlass
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA13CopyableKlassChF : $@convention(thin) (@guaranteed CopyableKlass) -> ()
// CHECK:   apply [[FN]]([[BORROW]])
// CHECK:   end_borrow [[BORROW]]
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly42moveOnlyStructCopyableKlassNonConsumingUseyyF'
func moveOnlyStructCopyableKlassNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.copyableKlass)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly022moveOnlyStructCopyableD15NonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical]
//
// CHECK: project_box
// CHECK: store
// CHECK: project_box
// CHECK: assign
//
// CHECK:   [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[MARKED_ADDR]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialCopyableStruct
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA24NonTrivialCopyableStructVhF :
// CHECK:   apply [[FN]]([[BORROW]])
// CHECK:   end_borrow [[BORROW]]
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly022moveOnlyStructCopyableD15NonConsumingUseyyF'
func moveOnlyStructCopyableStructNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.nonTrivialCopyableStruct)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly022moveOnlyStructCopyabledE20KlassNonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical]
//
// CHECK: project_box
// CHECK: store
// CHECK: project_box
// CHECK: assign
//
// CHECK:   [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[MARKED_ADDR]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialCopyableStruct
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialCopyableStruct, #NonTrivialCopyableStruct.copyableKlass
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP2]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA13CopyableKlassChF :
// CHECK:   apply [[FN]]([[BORROW]])
// CHECK:   end_borrow [[BORROW]]
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly022moveOnlyStructCopyabledE20KlassNonConsumingUseyyF'
func moveOnlyStructCopyableStructCopyableKlassNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.nonTrivialCopyableStruct.copyableKlass)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly022moveOnlyStructCopyabledeD15NonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical]
//
// CHECK: project_box
// CHECK: store
// CHECK: project_box
// CHECK: assign
//
// CHECK:   [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[MARKED_ADDR]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialCopyableStruct
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialCopyableStruct, #NonTrivialCopyableStruct.nonTrivialCopyableStruct2
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP2]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA25NonTrivialCopyableStruct2VhF :
// CHECK:   apply [[FN]]([[BORROW]])
// CHECK:   end_borrow [[BORROW]]
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly022moveOnlyStructCopyabledeD15NonConsumingUseyyF'
func moveOnlyStructCopyableStructCopyableStructNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.nonTrivialCopyableStruct.nonTrivialCopyableStruct2)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly022moveOnlyStructCopyablededE20KlassNonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical]
//
// CHECK: project_box
// CHECK: store
// CHECK: project_box
// CHECK: assign
//
// CHECK:   [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[MARKED_ADDR]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialCopyableStruct
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialCopyableStruct, #NonTrivialCopyableStruct.nonTrivialCopyableStruct2
// CHECK:   [[GEP3:%.*]] = struct_element_addr [[GEP2]] : $*NonTrivialCopyableStruct2, #NonTrivialCopyableStruct2.copyableKlass
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP3]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA13CopyableKlassChF :
// CHECK:   apply [[FN]]([[BORROW]])
// CHECK:   end_borrow [[BORROW]]
// CHECK:   end_access [[ACCESS]]
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
// CHECK: [[BORROW_BOX:%.*]] = begin_borrow [lexical]
//
// CHECK: project_box
// CHECK: store
// CHECK: project_box
// CHECK: assign
//
// CHECK:   [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[MARKED_ADDR]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialCopyableStruct
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialCopyableStruct, #NonTrivialCopyableStruct.nonTrivialCopyableStruct2
// CHECK:   [[GEP3:%.*]] = struct_element_addr [[GEP2]] : $*NonTrivialCopyableStruct2, #NonTrivialCopyableStruct2.copyableKlass
// CHECK:   [[COPYABLE_KLASS:%.*]] = load [copy] [[GEP3]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   [[BORROWED_COPYABLE_KLASS:%.*]] = begin_borrow [[COPYABLE_KLASS]]
// CHECK:   [[FIELD:%.*]] = ref_element_addr [[BORROWED_COPYABLE_KLASS]]
// CHECK:   [[FIELD_MARK:%.*]] = mark_must_check [no_consume_or_assign] [[FIELD]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [dynamic] [[FIELD_MARK]]
// CHECK:   [[BORROWED_MOVEONLY_KLASS:%.*]] = load_borrow [[ACCESS]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA2FDVhF :
// CHECK:   apply [[FN]]([[BORROWED_MOVEONLY_KLASS]])
// CHECK:   end_borrow [[BORROWED_MOVEONLY_KLASS]]
// CHECK:   destroy_value [[COPYABLE_KLASS]]
// CHECK: } // end sil function '$s8moveonly022moveOnlyStructCopyabledede9KlassMovecF15NonConsumingUseyyF'
func moveOnlyStructCopyableStructCopyableStructCopyableKlassMoveOnlyKlassNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.nonTrivialCopyableStruct.nonTrivialCopyableStruct2.copyableKlass.fd)
}

///////////////////////
// Enum Switch Tests //
///////////////////////

enum EnumSwitchTests {
    @_moveOnly
    enum E2 {
        case lhs(CopyableKlass)
        case rhs(FD)
    }

    @_moveOnly
    enum E {
        case first(NonTrivialStruct2)
        case second(NonTrivialStruct)
        case third(CopyableKlass)
        case fourth(E2)
    }
}

func consumeVal(_ e: __owned EnumSwitchTests.E2) {}

var booleanGuard: Bool { false }
var booleanGuard2: Bool { false }

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly15enumSwitchTest1yyAA04EnumC5TestsO1EOhF : $@convention(thin) (@guaranteed EnumSwitchTests.E) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   [[MARKED_VALUE:%.*]] = mark_must_check [no_consume_or_assign] [[COPY_ARG]]
// CHECK:   [[BORROWED_VALUE:%.*]] = begin_borrow [[MARKED_VALUE]]
// CHECK:   switch_enum [[BORROWED_VALUE]] : $EnumSwitchTests.E, case #EnumSwitchTests.E.first!enumelt: [[BB_E_1:bb[0-9]+]], case #EnumSwitchTests.E.second!enumelt: [[BB_E_2:bb[0-9]+]], case #EnumSwitchTests.E.third!enumelt: [[BB_E_3:bb[0-9]+]], case #EnumSwitchTests.E.fourth!enumelt: [[BB_E_4:bb[0-9]+]]
//
// CHECK: [[BB_E_1]]([[BBARG:%.*]] : @guaranteed
// CHECK:   end_borrow [[BORROWED_VALUE]]
// CHECK:   br [[BB_CONT:bb[0-9]+]]
//
// CHECK: [[BB_E_2]]([[BBARG:%.*]] : @guaranteed
// CHECK:   [[NEW_BOX:%.*]] = alloc_box
// CHECK:   [[NEW_BOX_BORROW:%.*]] = begin_borrow [lexical] [[NEW_BOX]]
// CHECK:   [[NEW_BOX_PROJECT:%.*]] = project_box [[NEW_BOX_BORROW]]
// CHECK:   [[BBARG_COPY:%.*]] = copy_value [[BBARG]]
// CHECK:   store [[BBARG_COPY]] to [init] [[NEW_BOX_PROJECT]]
// CHECK:   end_borrow [[BORROWED_VALUE]]
// CHECK:   br [[BB_CONT]]
//
// This case is copyable
// CHECK: [[BB_E_3]]([[BBARG:%.*]] : @guaranteed
// CHECK:   [[BBARG_COPY:%.*]] = copy_value [[BBARG]]
// CHECK:   begin_borrow [lexical] [[BBARG_COPY]]
// CHECK:   end_borrow [[BORROWED_VALUE]]
// CHECK:   br [[BB_CONT]]
//
// This is a guard case.
// CHECK: [[BB_E_4]]([[BBARG:%.*]] : @guaranteed
// CHECK:   cond_br {{%.*}}, [[BB_GUARD_1:bb[0-9]+]], [[BB_GUARD_2:bb[0-9]+]]
//
// CHECK: [[BB_GUARD_1]]:
// CHECK:   end_borrow [[BORROWED_VALUE]]
// CHECK:   br [[BB_CONT]]
//
// CHECK: [[BB_GUARD_2]]:
// CHECK:   switch_enum [[BBARG]] : $EnumSwitchTests.E2, case #EnumSwitchTests.E2.lhs!enumelt: [[BB_E2_LHS:bb[0-9]+]], case #EnumSwitchTests.E2.rhs!enumelt: [[BB_E2_RHS:bb[0-9]+]]
//
// Copyable case
// CHECK: [[BB_E2_LHS]]([[BBARG:%.*]] : @guaranteed
// CHECK:   [[BBARG_COPY:%.*]] = copy_value [[BBARG]]
// CHECK:   begin_borrow [lexical] [[BBARG_COPY]]
// CHECK:   end_borrow [[BORROWED_VALUE]]
// CHECK:   br [[BB_CONT]]
//
// Move only case.
// CHECK: [[BB_E2_RHS]]([[BBARG:%.*]] : @guaranteed
// CHECK:   [[NEW_BOX:%.*]] = alloc_box
// CHECK:   [[NEW_BOX_BORROW:%.*]] = begin_borrow [lexical] [[NEW_BOX]]
// CHECK:   [[NEW_BOX_PROJECT:%.*]] = project_box [[NEW_BOX_BORROW]]
// CHECK:   [[BBARG_COPY:%.*]] = copy_value [[BBARG]]
// CHECK:   store [[BBARG_COPY]] to [init] [[NEW_BOX_PROJECT]]
// CHECK:   end_borrow [[BORROWED_VALUE]]
// CHECK:   br [[BB_CONT]]
//
// CHECK: [[BB_CONT]]:
// CHECK:   destroy_value [[MARKED_VALUE]]
// CHECK: } // end sil function '$s8moveonly15enumSwitchTest1yyAA04EnumC5TestsO1EOhF'
func enumSwitchTest1(_ e: __shared EnumSwitchTests.E) {
    switch e {
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
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[LOADED_VAL:%.*]] = load_borrow [[MARKED_GLOBAL]]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
// CHECK: end_borrow [[LOADED_VAL]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[GLOBAL]]
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARKED_GLOBAL]]
// CHECK: [[LOADED_VAL:%.*]] = load_borrow [[GEP]]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
// CHECK: end_borrow [[LOADED_VAL]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9letGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[GLOBAL]]
// CHECK: [[LOADED_VAL:%.*]] = load [copy] [[MARKED_GLOBAL]]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
// CHECK: destroy_value [[LOADED_VAL]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9letGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[GLOBAL]]
// CHECK: [[LOADED_VAL:%.*]] = load [copy] [[MARKED_GLOBAL]]
// CHECK: [[LOADED_BORROWED_VAL:%.*]] = begin_borrow [[LOADED_VAL]]
// CHECK: [[LOADED_GEP:%.*]] = struct_extract [[LOADED_BORROWED_VAL]]
// CHECK: [[LOADED_GEP_COPY:%.*]] = copy_value [[LOADED_GEP]]
// CHECK: end_borrow [[LOADED_BORROWED_VAL]]
// CHECK: destroy_value [[LOADED_VAL]]
// CHECK: apply {{%.*}}([[LOADED_GEP_COPY]])
// CHECK: destroy_value [[LOADED_GEP_COPY]]
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
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[LOADED_VAL:%.*]] = load [take]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[ACCESS:%.*]] = begin_access [deinit] [dynamic] [[GLOBAL]]
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARKED_GLOBAL]]
// CHECK: [[LOADED_VAL:%.*]] = load [take] [[GEP]]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9letGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[GLOBAL]]
// CHECK: [[LOADED_VAL:%.*]] = load [copy] [[MARKED_GLOBAL]]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9letGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[GLOBAL]]
// CHECK: [[LOADED_VAL:%.*]] = load [copy] [[MARKED_GLOBAL]]
// CHECK: [[LOADED_BORROWED_VAL:%.*]] = begin_borrow [[LOADED_VAL]]
// CHECK: [[LOADED_GEP:%.*]] = struct_extract [[LOADED_BORROWED_VAL]]
// CHECK: [[LOADED_GEP_COPY:%.*]] = copy_value [[LOADED_GEP]]
// CHECK: end_borrow [[LOADED_BORROWED_VAL]]
// CHECK: destroy_value [[LOADED_VAL]]
// CHECK: apply {{%.*}}([[LOADED_GEP_COPY]])
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
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: assign {{%.*}} to [[MARKED_GLOBAL]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL]]
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: assign {{%.*}} to [[MARKED_GLOBAL]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL]]
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARKED_GLOBAL]]
// CHECK: assign {{%.*}} to [[GEP]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL]]
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
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

// Make sure that we insert a mark_must_check on the capture value.
// CHECK-LABEL: sil hidden [ossa] @$s8moveonly28checkMarkMustCheckOnCaptured1xyAA2FDVn_tF : $@convention(thin) (@owned FD) -> () {
// CHECK: bb0([[ARG:%.*]] : @owned
// CHECK:   [[BOX:%.*]] = alloc_box
// CHECK:   [[PROJECT:%.*]] = project_box [[BOX]]
// CHECK:   store [[ARG]] to [init] [[PROJECT]]
//
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly28checkMarkMustCheckOnCaptured1xyAA2FDVn_tFyyXEfU_ : $@convention(thin) @substituted <τ_0_0> (@guaranteed FD) -> @out τ_0_0 for <()>
// CHECK:   [[PROJECT:%.*]] = project_box [[BOX]]
// CHECK:   [[MARK:%.*]] = mark_must_check [assignable_but_not_consumable] [[PROJECT]]
// CHECK:   [[VALUE:%.*]] = load [copy] [[MARK]]
// CHECK:   [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[FN]]([[VALUE]])
// CHECK: } // end sil function '$s8moveonly28checkMarkMustCheckOnCaptured1xyAA2FDVn_tF'
func checkMarkMustCheckOnCaptured(x: __owned FD) {
    func clodger<T>(_: () -> T) {}
    clodger({ consumeVal(x) })
}

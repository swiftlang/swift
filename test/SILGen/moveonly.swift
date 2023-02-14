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

public func borrowVal(_ e : NonTrivialEnum) {}
public func borrowVal(_ e : FD) {}
public func borrowVal(_ k: CopyableKlass) {}
public func borrowVal(_ k: NonTrivialCopyableStruct) {}
public func borrowVal(_ k: NonTrivialCopyableStruct2) {}
public func borrowVal(_ s: NonTrivialStruct) {}
public func borrowVal(_ s: NonTrivialStruct2) {}

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

// CHECK-LABEL: sil [ossa] @$s8moveonly19useNonTrivialStructyyAA0cdE0VF : $@convention(thin) (@guaranteed NonTrivialStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $NonTrivialStruct):
// CHECK:   [[COPIED_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   mark_must_check [no_consume_or_assign] [[COPIED_ARG]]
// CHECK: } // end sil function '$s8moveonly19useNonTrivialStructyyAA0cdE0VF'
public func useNonTrivialStruct(_ s: NonTrivialStruct) {
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
// CHECK:   mark_must_check [consumable_and_assignable] [[PROJECT]]
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
// CHECK: bb0([[ARG:%.*]] : @guaranteed $NonTrivialEnum):
// CHECK:   [[COPIED_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   mark_must_check [no_consume_or_assign] [[COPIED_ARG]]
// CHECK: } // end sil function '$s8moveonly17useNonTrivialEnumyyAA0cdE0OF'
public func useNonTrivialEnum(_ s: NonTrivialEnum) {
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
// CHECK:   mark_must_check [consumable_and_assignable] [[PROJECT]]
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
// CHECK: [[BORROW:%.*]] = begin_borrow [lexical] 
// CHECK: [[PROJECT:%.*]] = project_box [[BORROW]]
// CHECK: [[X_MV_ONLY:%.*]] = mark_must_check [consumable_and_assignable] [[PROJECT]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly2FDVACycfC :
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: store [[X]] to [init] [[X_MV_ONLY]]
// CHECK: [[LOADED_X:%.*]] = load [copy] [[X_MV_ONLY]]
// CHECK: [[X_MV_ONLY_CONSUME:%.*]] = move_value [[LOADED_X]]
// CHECK: } // end sil function '$s8moveonly27blackHoleLetInitialization1yyF'
func blackHoleLetInitialization1() {
    let x = FD()
    let _ = x
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly27blackHoleLetInitialization2yyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BORROW:%.*]] = begin_borrow [lexical] 
// CHECK: [[PROJECT:%.*]] = project_box [[BORROW]]
// CHECK: [[X_MV_ONLY:%.*]] = mark_must_check [consumable_and_assignable] [[PROJECT]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly2FDVACycfC :
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: store [[X]] to [init] [[X_MV_ONLY]]
// CHECK: [[LOADED_X:%.*]] = load [copy] [[X_MV_ONLY]]
// CHECK: [[X_MV_ONLY_CONSUME:%.*]] = move_value [[LOADED_X]]
// CHECK: } // end sil function '$s8moveonly27blackHoleLetInitialization2yyF'
func blackHoleLetInitialization2() {
    let x = FD()
    var _ = x
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly27blackHoleVarInitialization1yyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BOX_BORROW:%.*]] = begin_borrow [lexical] [[BOX]]
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX_BORROW]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable] [[PROJECT_BOX]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly2FDVACycfC :
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: store [[X]] to [init] [[MARKED_ADDR]]
// CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK: [[LD:%.*]] = load [copy] [[READ]]
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
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable] [[PROJECT_BOX]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly2FDVACycfC :
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: store [[X]] to [init] [[MARKED_ADDR]]
// CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK: [[LD:%.*]] = load [copy] [[READ]]
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
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable] [[PROJECT_BOX]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly2FDVACycfC :
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: store [[X]] to [init] [[MARKED_ADDR]]
// CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK: [[LD:%.*]] = load [copy] [[READ]]
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
// CHECK: [[BOX_BORROW:%.*]] = begin_borrow [lexical] [[BOX]]
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX_BORROW]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable] [[PROJECT_BOX]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly2FDVACycfC :
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: store [[X]] to [init] [[MARKED_ADDR]]
// CHECK: [[X_MV_ONLY_BORROW:%.*]] = load_borrow [[MARKED_ADDR]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA2FDVF :
// CHECK: apply [[FN]]([[X_MV_ONLY_BORROW]])
// CHECK: end_borrow [[X_MV_ONLY_BORROW]]
// CHECK: } // end sil function '$s8moveonly24borrowObjectFunctionCallyyF'
func borrowObjectFunctionCall() {
    let k = FD()
    borrowVal(k)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly29moveOnlyStructNonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK: [[BORROW:%.*]] = load_borrow [[ACCESS]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA16NonTrivialStructVF :
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
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[ACCESS]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialStruct2
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA17NonTrivialStruct2VF : $@convention(thin) (@guaranteed NonTrivialStruct2) -> ()
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
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[ACCESS]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialStruct2
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialStruct2, #NonTrivialStruct2.copyableKlass
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP2]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA13CopyableKlassCF : $@convention(thin) (@guaranteed CopyableKlass) -> ()
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
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[ACCESS]] : $*NonTrivialStruct, #NonTrivialStruct.copyableKlass
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA13CopyableKlassCF : $@convention(thin) (@guaranteed CopyableKlass) -> ()
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
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[ACCESS]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialCopyableStruct
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA24NonTrivialCopyableStructVF :
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
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[ACCESS]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialCopyableStruct
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialCopyableStruct, #NonTrivialCopyableStruct.copyableKlass
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP2]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA13CopyableKlassCF :
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
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[ACCESS]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialCopyableStruct
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialCopyableStruct, #NonTrivialCopyableStruct.nonTrivialCopyableStruct2
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP2]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA25NonTrivialCopyableStruct2VF :
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
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[ACCESS]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialCopyableStruct
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialCopyableStruct, #NonTrivialCopyableStruct.nonTrivialCopyableStruct2
// CHECK:   [[GEP3:%.*]] = struct_element_addr [[GEP2]] : $*NonTrivialCopyableStruct2, #NonTrivialCopyableStruct2.copyableKlass
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP3]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA13CopyableKlassCF :
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
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[ACCESS]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialCopyableStruct
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialCopyableStruct, #NonTrivialCopyableStruct.nonTrivialCopyableStruct2
// CHECK:   [[GEP3:%.*]] = struct_element_addr [[GEP2]] : $*NonTrivialCopyableStruct2, #NonTrivialCopyableStruct2.copyableKlass
// CHECK:   [[COPYABLE_KLASS:%.*]] = load [copy] [[GEP3]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   [[BORROWED_COPYABLE_KLASS:%.*]] = begin_borrow [[COPYABLE_KLASS]]
// CHECK:   [[FIELD:%.*]] = ref_element_addr [[BORROWED_COPYABLE_KLASS]]
// CHECK:   [[FIELD_MARK:%.*]] = mark_must_check [no_consume_or_assign] [[FIELD]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [dynamic] [[FIELD_MARK]]
// CHECK:   [[BORROWED_MOVEONLY_KLASS:%.*]] = load_borrow [[ACCESS]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA2FDVF :
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

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly15enumSwitchTest1yyAA04EnumC5TestsO1EOF : $@convention(thin) (@guaranteed EnumSwitchTests.E) -> () {
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
// CHECK:   [[BOX:%.*]] = alloc_box
// CHECK:   [[BORROW:%.*]] = begin_borrow [lexical] 
// CHECK:   [[PROJECT:%.*]] = project_box [[BORROW]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable] [[PROJECT]]
// CHECK:   [[COPIED_VAL:%.*]] = copy_value [[BBARG]]
// CHECK:   store [[COPIED_VAL]] to [init] [[MARKED_ADDR]]
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
// CHECK:   [[BOX:%.*]] = alloc_box
// CHECK:   [[BORROW:%.*]] = begin_borrow [lexical] 
// CHECK:   [[PROJECT:%.*]] = project_box [[BORROW]]
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable] [[PROJECT]]
// CHECK:   [[BBARG_COPY:%.*]] = copy_value [[BBARG]]
// CHECK:   store [[BBARG_COPY]] to [init] [[MARKED_ADDR]]
// CHECK:   br [[BB_CONT]]
//
// CHECK: [[BB_CONT]]:
// CHECK:   destroy_value [[MARKED_VALUE]]
// CHECK: } // end sil function '$s8moveonly15enumSwitchTest1yyAA04EnumC5TestsO1EOF'
func enumSwitchTest1(_ e: EnumSwitchTests.E) {
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
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [no_consume_or_assign] [[GLOBAL]]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[MARKED_GLOBAL]]
// CHECK: [[LOADED_VAL:%.*]] = load_borrow [[ACCESS]]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
// CHECK: end_borrow [[LOADED_VAL]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [no_consume_or_assign] [[GLOBAL]]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[MARKED_GLOBAL]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[ACCESS]]
// CHECK: [[LOADED_VAL:%.*]] = load_borrow [[GEP]]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
// CHECK: end_borrow [[LOADED_VAL]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9letGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [no_consume_or_assign] [[GLOBAL]]
// CHECK: [[LOADED_VAL:%.*]] = load [copy] [[MARKED_GLOBAL]]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
// CHECK: destroy_value [[LOADED_VAL]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9letGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [no_consume_or_assign] [[GLOBAL]]
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
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[GLOBAL]]
// CHECK: [[ACCESS:%.*]] = begin_access [deinit] [dynamic] [[MARKED_GLOBAL]]
// CHECK: [[LOADED_VAL:%.*]] = load [take]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[GLOBAL]]
// CHECK: [[ACCESS:%.*]] = begin_access [deinit] [dynamic] [[MARKED_GLOBAL]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[ACCESS]]
// CHECK: [[LOADED_VAL:%.*]] = load [take] [[GEP]]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9letGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [no_consume_or_assign] [[GLOBAL]]
// CHECK: [[LOADED_VAL:%.*]] = load [copy] [[MARKED_GLOBAL]]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9letGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [no_consume_or_assign] [[GLOBAL]]
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
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[GLOBAL]]
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[MARKED_GLOBAL]]
// CHECK: assign {{%.*}} to [[ACCESS]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[GLOBAL]]
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[MARKED_GLOBAL]]
// CHECK: assign {{%.*}} to [[ACCESS]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[GLOBAL]]
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[MARKED_GLOBAL]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[ACCESS]]
// CHECK: assign {{%.*}} to [[GEP]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: [[GLOBAL:%.*]] = global_addr @$s8moveonly9varGlobalAA16NonTrivialStructVvp :
// CHECK: [[MARKED_GLOBAL:%.*]] = mark_must_check [assignable_but_not_consumable] [[GLOBAL]]
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[MARKED_GLOBAL]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[ACCESS]]
// CHECK: assign {{%.*}} to [[GEP]]
// CHECK: end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly16testGlobalAssignyyF'
func testGlobalAssign() {
    varGlobal = NonTrivialStruct()
    varGlobal = NonTrivialStruct()
    varGlobal.nonTrivialStruct2 = NonTrivialStruct2()
    varGlobal.nonTrivialStruct2 = NonTrivialStruct2()
}

////////////////////////////
// Escaping Closure Tests //
////////////////////////////

// Closure Caller
//
// CHECK-LABEL: sil hidden [ossa] @$s8moveonly20closureVarTestBorrowyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box ${ var NonTrivialStruct }, var, name "x"
// CHECK: [[BORROWED_BOX:%.*]] = begin_borrow [lexical] [[BOX]]
// CHECK: [[PROJECTED_BORROWED_BOX:%.*]] = project_box [[BORROWED_BOX]]
// CHECK: mark_must_check [consumable_and_assignable] [[PROJECTED_BORROWED_BOX]]
// CHECK: } // end sil function '$s8moveonly20closureVarTestBorrowyyF'
//
// Closure Callee
//
// CHECK_LABEL: sil private [ossa] @$s8moveonly20closureVarTestBorrowyyFyycfU_ : $@convention(thin) (@guaranteed { var NonTrivialStruct }) -> () {
// CHECK: bb0([[BOX:%.*]] :
//
// First access.
// CHECK:   [[PROJECT_BOX:%.*]] = project_box [[BOX]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT_BOX]]
// CHECK:   [[MARKED_PROJECT_BOX:%.*]] = mark_must_check [no_consume_or_assign] [[ACCESS]]
// CHECK:   [[LOADED_VAL:%.*]] = load_borrow [[MARKED_PROJECT_BOX]]
// CHECK:   apply {{%.*}}([[LOADED_VAL]])
// CHECK:   end_borrow [[LOADED_VAL]]
// CHECK:   end_access [[ACCESS]]
//
// Second access.
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT_BOX]]
// CHECK:   [[MARKED_PROJECT_BOX:%.*]] = mark_must_check [no_consume_or_assign] [[ACCESS]]
// CHECK:   [[LOADED_VAL:%.*]] = load_borrow [[MARKED_PROJECT_BOX]]
// CHECK:   apply {{%.*}}([[LOADED_VAL]])
// CHECK:   end_borrow [[LOADED_VAL]]
// CHECK:   end_access [[ACCESS]]
//
// GEP Access 1
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT_BOX]]
// CHECK:   [[MARKED_PROJECT_BOX:%.*]] = mark_must_check [no_consume_or_assign] [[ACCESS]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[MARKED_PROJECT_BOX]]
// CHECK:   [[LOADED_VAL:%.*]] = load_borrow [[GEP]]
// CHECK:   apply {{%.*}}([[LOADED_VAL]])
// CHECK:   end_borrow [[LOADED_VAL]]
// CHECK:   end_access [[ACCESS]]
//
// GEP Access 2
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT_BOX]]
// CHECK:   [[MARKED_PROJECT_BOX:%.*]] = mark_must_check [no_consume_or_assign] [[ACCESS]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[MARKED_PROJECT_BOX]]
// CHECK:   [[LOADED_VAL:%.*]] = load_borrow [[GEP]]
// CHECK:   apply {{%.*}}([[LOADED_VAL]])
// CHECK:   end_borrow [[LOADED_VAL]]
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly20closureVarTestBorrowyyFyycfU_'
func closureVarTestBorrow() {
    var x = NonTrivialStruct()
    x = NonTrivialStruct()
    let f = {
        borrowVal(x)
        borrowVal(x)
        borrowVal(x.nonTrivialStruct2)
        borrowVal(x.nonTrivialStruct2)
    }

    _ = f
}

// Closure Caller
//
// CHECK-LABEL: sil hidden [ossa] @$s8moveonly21closureVarTestConsumeyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box ${ var NonTrivialStruct }, var, name "x"
// CHECK: [[BORROWED_BOX:%.*]] = begin_borrow [lexical] [[BOX]]
// CHECK: [[PROJECTED_BORROWED_BOX:%.*]] = project_box [[BORROWED_BOX]]
// CHECK: mark_must_check [consumable_and_assignable] [[PROJECTED_BORROWED_BOX]]
// CHECK: } // end sil function '$s8moveonly21closureVarTestConsumeyyF'
//
// Closure Callee
//
// CHECK-LABEL: sil private [ossa] @$s8moveonly21closureVarTestConsumeyyFyycfU_ : $@convention(thin) (@guaranteed { var NonTrivialStruct }) -> () {
// CHECK: bb0([[BOX:%.*]] :
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX]]
//
// let _ = x
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_BOX_ADDR:%.*]] = mark_must_check [no_consume_or_assign] [[ACCESS]]
// CHECK: [[LOADED_VAL:%.*]] = load [copy] [[MARKED_BOX_ADDR]]
// CHECK: [[MOVED_VALUE:%.*]] = move_value [[LOADED_VAL]]
// CHECK: destroy_value [[MOVED_VALUE]]
//
// let _ = x.nonTrivialStruct2
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_BOX_ADDR:%.*]] = mark_must_check [no_consume_or_assign] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARKED_BOX_ADDR]]
// CHECK: [[LOADED_VAL:%.*]] = load [copy] [[GEP]]
// CHECK: [[MOVED_VALUE:%.*]] = move_value [[LOADED_VAL]]
// CHECK: destroy_value [[MOVED_VALUE]]
//
// consumeVal(x)
// CHECK: [[ACCESS:%.*]] = begin_access [deinit] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_BOX_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[LOADED_VAL:%.*]] = load [take] [[MARKED_BOX_ADDR]]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
//
// consumeVal(x.nonTrivialStruct2)
// CHECK: [[ACCESS:%.*]] = begin_access [deinit] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_BOX_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARKED_BOX_ADDR]]
// CHECK: [[LOADED_VAL:%.*]] = load [take] [[GEP]]
// CHECK: apply {{%.*}}([[LOADED_VAL]])
// CHECK: } // end sil function '$s8moveonly21closureVarTestConsumeyyFyycfU_'
func closureVarTestConsume() {
    var x = NonTrivialStruct()
    x = NonTrivialStruct()
    let f = {
        let _ = x
        let _ = x.nonTrivialStruct2
        consumeVal(x)
        consumeVal(x.nonTrivialStruct2)
    }

    _ = f
}

// Closure Caller
//
// CHECK-LABEL: sil hidden [ossa] @$s8moveonly20closureVarTestAssignyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box ${ var NonTrivialStruct }, var, name "x"
// CHECK: [[BORROWED_BOX:%.*]] = begin_borrow [lexical] [[BOX]]
// CHECK: [[PROJECTED_BORROWED_BOX:%.*]] = project_box [[BORROWED_BOX]]
// CHECK: mark_must_check [consumable_and_assignable] [[PROJECTED_BORROWED_BOX]]
// CHECK: } // end sil function '$s8moveonly20closureVarTestAssignyyF'
//
// Closure Callee
//
// CHECK-LABEL: sil private [ossa] @$s8moveonly20closureVarTestAssignyyFyycfU_ : $@convention(thin) (@guaranteed { var NonTrivialStruct }) -> () {
// CHECK: bb0([[BOX:%.*]] :
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX]]
//
// x = NonTrivialStruct()
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_BOX_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: assign {{%.*}} to [[MARKED_BOX_ADDR]]
// CHECK: end_access [[ACCESS]]
//
// x = NonTrivialStruct()
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_BOX_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: assign {{%.*}} to [[MARKED_BOX_ADDR]]
// CHECK: end_access [[ACCESS]]
//
// x.nonTrivialStruct2 = NonTrivialStruct2()
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_BOX_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARKED_BOX_ADDR]]
// CHECK: assign {{%.*}} to [[GEP]]
// CHECK: end_access [[ACCESS]]
//
// x.nonTrivialStruct2 = NonTrivialStruct2()
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT_BOX]]
// CHECK: [[MARKED_BOX_ADDR:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[MARKED_BOX_ADDR]]
// CHECK: assign {{%.*}} to [[GEP]]
// CHECK: end_access [[ACCESS]]
//
// CHECK: } // end sil function '$s8moveonly20closureVarTestAssignyyFyycfU_'
func closureVarTestAssign() {
    var x = NonTrivialStruct()
    x = NonTrivialStruct()
    let f = {
        x = NonTrivialStruct()
        x = NonTrivialStruct()
        x.nonTrivialStruct2 = NonTrivialStruct2()
        x.nonTrivialStruct2 = NonTrivialStruct2()
    }

    _ = f
}

// Start by just making sure, we mark the box itself as a moveonly var.
// CHECK-LABEL: sil hidden [ossa] @$s8moveonly16closureInClosureyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[BOX:%.*]] = alloc_box
// CHECK:   [[BORROW:%.*]] = begin_borrow [lexical] [[BOX]]
// CHECK:   [[PROJECT:%.*]] = project_box [[BORROW]]
// CHECK:   [[MARKED:%.*]] = mark_must_check [consumable_and_assignable] [[PROJECT]]
// CHECK: } // end sil function '$s8moveonly16closureInClosureyyF'
//
// CHECK-LABEL: sil private [ossa] @$s8moveonly16closureInClosureyyFyycfU_ : $@convention(thin) (@guaranteed { var NonTrivialStruct }) -> () {
// CHECK: bb0([[BOX:%.*]] :
// CHECK:    [[PROJECT:%.*]] = project_box [[BOX]]
//
// let _ = x
// CHECK:    [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:    [[MARKED:%.*]] = mark_must_check [no_consume_or_assign] [[ACCESS]]
// CHECK:    [[LOADED_VAL:%.*]] = load [copy] [[MARKED]]
// CHECK:    move_value [[LOADED_VAL]]
//
// let g = { capture(x) }
// CHECK:   [[BOX_COPY:%.*]] = copy_value [[BOX]]
// CHECK:   partial_apply [callee_guaranteed] {{%.*}}([[BOX_COPY]])
//
// let g2 = { capture(x) }
// CHECK:   [[BOX_COPY:%.*]] = copy_value [[BOX]]
// CHECK:   partial_apply [callee_guaranteed] {{%.*}}([[BOX_COPY]])
// CHECK: } // end sil function '$s8moveonly16closureInClosureyyFyycfU_'
//
// Now lets check g (g2 is the same, so we skip it).
// CHECK-LABEL: sil private [ossa] @$s8moveonly16closureInClosureyyFyycfU_yycfU_ : $@convention(thin) (@guaranteed { var NonTrivialStruct }) -> () {
// CHECK: bb0([[BOX:%.*]] :
// CHECK:    [[PROJECT:%.*]] = project_box [[BOX]]
//
// let _ = x
// CHECK:    [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK:    [[MARKED:%.*]] = mark_must_check [no_consume_or_assign] [[ACCESS]]
// CHECK:    [[LOADED_VAL:%.*]] = load [copy] [[MARKED]]
// CHECK:    move_value [[LOADED_VAL]]
// CHECK: } // end sil function '$s8moveonly16closureInClosureyyFyycfU_yycfU_'
func closureInClosure() {
    var x = NonTrivialStruct()
    x = NonTrivialStruct()
    let f = {
        let _ = x
        let g = {
            let _ = x
        }
        let g2 = {
            let _ = x
        }
    }
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly12closureInOutyyAA16NonTrivialStructVzF : $@convention(thin) (@inout NonTrivialStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : $*NonTrivialStruct):
// CHECK:   [[MARKED:%.*]] = mark_must_check [consumable_and_assignable] [[ARG]]
// CHECK:   partial_apply [callee_guaranteed] {{%.*}}([[MARKED]]) : $@convention(thin) (@inout_aliasable NonTrivialStruct) -> ()
// CHECK: } // end sil function '$s8moveonly12closureInOutyyAA16NonTrivialStructVzF'
//
// CHECK-LABEL: sil private [ossa] @$s8moveonly12closureInOutyyAA16NonTrivialStructVzFyycfU_ : $@convention(thin) (@inout_aliasable NonTrivialStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : @closureCapture $*NonTrivialStruct):
// CHECK:    [[MARKED:%.*]] = mark_must_check [assignable_but_not_consumable] [[ARG]]
// CHECK:    partial_apply [callee_guaranteed] {{%.*}}([[MARKED]]) : $@convention(thin) (@inout_aliasable NonTrivialStruct) -> ()
// CHECK: } // end sil function '$s8moveonly12closureInOutyyAA16NonTrivialStructVzFyycfU_'
//
// CHECK-LABEL: sil private [ossa] @$s8moveonly12closureInOutyyAA16NonTrivialStructVzFyycfU_yycfU_ : $@convention(thin) (@inout_aliasable NonTrivialStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : @closureCapture $*NonTrivialStruct):
// CHECK:    [[MARKED:%.*]] = mark_must_check [assignable_but_not_consumable] [[ARG]]
//
// borrowVal(x2)
// CHECK:    [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED]]
// CHECK:    [[VAL:%.*]] = load_borrow [[ACCESS]]
// CHECK:    apply {{%.*}}([[VAL]])
// CHECK:    end_borrow [[VAL]]
// CHECK:    end_access [[ACCESS]]
//
// consumeVal(x2)
// CHECK:    [[ACCESS:%.*]] = begin_access [deinit] [unknown] [[MARKED]]
// CHECK:    [[VAL:%.*]] = load [take] [[ACCESS]]
// CHECK:    apply {{%.*}}([[VAL]])
// CHECK:    end_access [[ACCESS]]
//
// consumeVal(x2)
// CHECK:    [[ACCESS:%.*]] = begin_access [deinit] [unknown] [[MARKED]]
// CHECK:    [[VAL:%.*]] = load [take] [[ACCESS]]
// CHECK:    apply {{%.*}}([[VAL]])
// CHECK:    end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly12closureInOutyyAA16NonTrivialStructVzFyycfU_yycfU_'
func closureInOut(_ x2: inout NonTrivialStruct) {
    let f = {
        let g = {
            borrowVal(x2)
            consumeVal(x2)
            consumeVal(x2)
        }
        g()
    }
    f()
}
REQUIRES: updating_for_owned_noescape

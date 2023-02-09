// RUN: %target-swift-emit-silgen -enable-experimental-move-only -enable-experimental-feature MoveOnlyClasses %s | %FileCheck %s

//////////////////
// Declarations //
//////////////////

public final class CopyableKlass {
    var k = Klass()
}

@_moveOnly
public class Klass2 {}

@_moveOnly
public final class Klass {
    var int: Int
    var moveOnlyKlass: Klass2
    var copyableKlass: CopyableKlass

    // CHECK-LABEL: sil hidden [ossa] @$s8moveonly5KlassCACycfc : $@convention(method) (@owned Klass) -> @owned Klass {
    // CHECK: bb0([[ARG:%.*]] : @owned $Klass):
    // CHECK:   [[MARK_UNINIT:%.*]] = mark_uninitialized [rootself] [[ARG]]
    // CHECK:   [[MARK_NO_IMP_COPY:%.*]] = mark_must_check [consumable_and_assignable] [[MARK_UNINIT]]
    // CHECK: } // end sil function '$s8moveonly5KlassCACycfc'
    init() {
        moveOnlyKlass = Klass2()
        int = 5
        copyableKlass = CopyableKlass()
    }
}

@_moveOnly
public struct NonTrivialStruct2 {
    var moveOnlyKlass = Klass()
    var copyableKlass = CopyableKlass()
}

@_moveOnly
public struct NonTrivialStruct {
    var moveOnlyKlass = Klass()
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
    case second(Klass)
    case third(NonTrivialStruct)
}

public func borrowVal(_ e : NonTrivialEnum) {}
public func borrowVal(_ k: CopyableKlass) {}
public func borrowVal(_ k: Klass) {}
public func borrowVal(_ k: Klass2) {}
public func borrowVal(_ k: NonTrivialCopyableStruct) {}
public func borrowVal(_ k: NonTrivialCopyableStruct2) {}
public func borrowVal(_ s: NonTrivialStruct) {}
public func borrowVal(_ s: NonTrivialStruct2) {}

public func consumeVal(_ e : __owned NonTrivialEnum) {}
public func consumeVal(_ k: __owned CopyableKlass) {}
public func consumeVal(_ k: __owned Klass) {}
public func consumeVal(_ k: __owned Klass2) {}
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

// CHECK-LABEL: sil [ossa] @$s8moveonly8useKlassyyAA0C0CF : $@convention(thin) (@guaranteed Klass) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Klass):
// CHECK:   [[OWNED_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   [[MARKED_OWNED_ARG:%.*]] = mark_must_check [no_consume_or_assign] [[OWNED_ARG]]
// CHECK: } // end sil function '$s8moveonly8useKlassyyAA0C0CF'
public func useKlass(_ k: Klass) {
    borrowVal(k)
    let k2 = k
    borrowVal(k)
    let _ = k2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly15useKlassConsumeyyAA0C0CnF : $@convention(thin) (@owned Klass) -> () {
// CHECK: bb0([[ARG:%.*]] : @owned $Klass):
// TODO: Is this move_value [lexical] necessary?
// CHECK:   [[LEXICAL_MOVE:%.*]] = move_value [lexical] [[ARG]]
// CHECK:   mark_must_check [consumable_and_assignable] [[LEXICAL_MOVE]]
// CHECK: } // end sil function '$s8moveonly15useKlassConsumeyyAA0C0CnF'
public func useKlassConsume(_ k: __owned Klass) {
    borrowVal(k)
    let k2 = k
    borrowVal(k)
    let _ = k2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly19useNonTrivialStructyyAA0cdE0VF : $@convention(thin) (@guaranteed NonTrivialStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $NonTrivialStruct):
// CHECK:   [[COPIED_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   mark_must_check [no_consume_or_assign] [[COPIED_ARG]]
// CHECK: } // end sil function '$s8moveonly19useNonTrivialStructyyAA0cdE0VF'
public func useNonTrivialStruct(_ s: NonTrivialStruct) {
    borrowVal(s)
    let s2 = s
    let k = s.moveOnlyKlass
    let _ = k
    borrowVal(s)
    let _ = s2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly24useNonTrivialOwnedStructyyAA0cdF0VnF : $@convention(thin) (@owned NonTrivialStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : @owned $NonTrivialStruct):
// CHECK:   [[MOVED_ARG:%.*]] = move_value [lexical] [[ARG]]
// CHECK:   mark_must_check [consumable_and_assignable] [[MOVED_ARG]]
// CHECK: } // end sil function '$s8moveonly24useNonTrivialOwnedStructyyAA0cdF0VnF'
public func useNonTrivialOwnedStruct(_ s: __owned NonTrivialStruct) {
    borrowVal(s)
    let s2 = s
    let k = s.moveOnlyKlass
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
// CHECK:   [[MOVED_ARG:%.*]] = move_value [lexical] [[ARG]]
// CHECK:   mark_must_check [consumable_and_assignable] [[MOVED_ARG]]
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

extension Klass {
    // CHECK-LABEL: sil hidden [ossa] @$s8moveonly5KlassC13testNoUseSelfyyF : $@convention(method) (@guaranteed Klass) -> () {
    // CHECK: bb0([[ARG:%.*]] : @guaranteed $Klass):
    // CHECK:   [[COPIED_ARG:%.*]] = copy_value [[ARG]]
    // CHECK:   mark_must_check [no_consume_or_assign] [[COPIED_ARG]]
    // CHECK: } // end sil function '$s8moveonly5KlassC13testNoUseSelfyyF'
    func testNoUseSelf() {
        let x = self
        let _ = x
    }
}

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
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly5KlassCACycfC : $@convention(method) (@thick Klass.Type) -> @owned Klass
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: [[X_MV_LEXICAL:%.*]] = move_value [lexical] [[X]]
// CHECK: [[X_MV_ONLY:%.*]] = mark_must_check [consumable_and_assignable] [[X_MV_LEXICAL]]
// CHECK: [[X_MV_ONLY_BORROW:%.*]] = begin_borrow [[X_MV_ONLY]]
// CHECK: [[X_MV_ONLY_COPY:%.*]] = copy_value [[X_MV_ONLY_BORROW]]
// CHECK: [[X_MV_ONLY_CONSUME:%.*]] = move_value [[X_MV_ONLY_COPY]]
// CHECK: } // end sil function '$s8moveonly27blackHoleLetInitialization1yyF'
func blackHoleLetInitialization1() {
    let x = Klass()
    let _ = x
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly27blackHoleLetInitialization2yyF : $@convention(thin) () -> () {
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly5KlassCACycfC : $@convention(method) (@thick Klass.Type) -> @owned Klass
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: [[X_MV_LEXICAL:%.*]] = move_value [lexical] [[X]]
// CHECK: [[X_MV_ONLY:%.*]] = mark_must_check [consumable_and_assignable] [[X_MV_LEXICAL]]
// CHECK: [[X_MV_ONLY_BORROW:%.*]] = begin_borrow [[X_MV_ONLY]]
// CHECK: [[X_MV_ONLY_COPY:%.*]] = copy_value [[X_MV_ONLY_BORROW]]
// CHECK: [[X_MV_ONLY_CONSUME:%.*]] = move_value [[X_MV_ONLY_COPY]]
// CHECK: } // end sil function '$s8moveonly27blackHoleLetInitialization2yyF'
func blackHoleLetInitialization2() {
    let x = Klass()
    var _ = x
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly27blackHoleVarInitialization1yyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BOX_BORROW:%.*]] = begin_borrow [lexical] [[BOX]]
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX_BORROW]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable] [[PROJECT_BOX]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly5KlassCACycfC : $@convention(method) (@thick Klass.Type) -> @owned Klass
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: store [[X]] to [init] [[MARKED_ADDR]]
// CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK: [[LD:%.*]] = load [copy] [[READ]]
// CHECK: [[CONSUME:%.*]] = move_value [[LD]]
// CHECK: } // end sil function '$s8moveonly27blackHoleVarInitialization1yyF'
func blackHoleVarInitialization1() {
    var x = Klass()
    x = Klass()
    let _ = x
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly27blackHoleVarInitialization2yyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box
// CHECK: [[BOX_BORROW:%.*]] = begin_borrow [lexical] [[BOX]]
// CHECK: [[PROJECT_BOX:%.*]] = project_box [[BOX_BORROW]]
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable] [[PROJECT_BOX]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly5KlassCACycfC : $@convention(method) (@thick Klass.Type) -> @owned Klass
// CHECK: [[X:%.*]] = apply [[FN]](
// CHECK: store [[X]] to [init] [[MARKED_ADDR]]
// CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK: [[LD:%.*]] = load [copy] [[READ]]
// CHECK: [[CONSUME:%.*]] = move_value [[LD]]
// CHECK: } // end sil function '$s8moveonly27blackHoleVarInitialization2yyF'
func blackHoleVarInitialization2() {
    var x = Klass()
    x = Klass()
    var _ = x
}

////////////////////////////////
// Borrow Function Call Tests //
////////////////////////////////

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly24borrowObjectFunctionCallyyF : $@convention(thin) () -> () {
// CHECK: [[CLS:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK: [[BORROW:%.*]] = begin_borrow [[CLS]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA5KlassCF
// CHECK: apply [[FN]]([[BORROW]])
// CHECK: end_borrow [[BORROW]]
// CHECK: } // end sil function '$s8moveonly24borrowObjectFunctionCallyyF'
func borrowObjectFunctionCall() {
    let k = Klass()
    borrowVal(k)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly25borrowAddressFunctionCallyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[BOX]]
// CHECK: [[BORROW:%.*]] = load_borrow [[ACCESS]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA5KlassCF
// CHECK: apply [[FN]]([[BORROW]])
// CHECK: end_borrow [[BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly25borrowAddressFunctionCallyyF'
func borrowAddressFunctionCall() {
    var k = Klass()
    k = Klass()
    borrowVal(k)
}

// We currently have the wrong behavior here since the class is treated by
// LValue emission as a base. That being said, move only classes are not our
// high order bit here, so I filed a bug. We might be able to do it in the future.
//
// CHECK-LABEL: sil hidden [ossa] @$s8moveonly31klassBorrowAddressFunctionCall2yyF : $@convention(thin) () -> () {
// CHECK: [[MARK:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARK]]
// CHECK: } // end sil function '$s8moveonly31klassBorrowAddressFunctionCall2yyF'
func klassBorrowAddressFunctionCall2() {
    var k = Klass()
    k = Klass()
    borrowVal(k.moveOnlyKlass)
}

// We copy here since we have a class as our base like the above.
//
// CHECK-LABEL: sil hidden [ossa] @$s8moveonly38klassBorrowCopyableAddressFunctionCallyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:  [[CLASS:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK:  [[ACCESS:%.*]] = begin_access [read] [unknown] [[CLASS]]
// CHECK:  [[LOAD:%.*]] = load [copy] [[ACCESS]]
// CHECK:  end_access [[ACCESS]]
// CHECK:  [[BORROW_LOAD:%.*]] = begin_borrow [[LOAD]]
// CHECK:  [[ADDR:%.*]] = ref_element_addr [[BORROW_LOAD]]
// CHECK:  [[ACCESS_ADDR:%.*]] = begin_access [read] [dynamic] [[ADDR]]
// CHECK:  [[BORROWED_COPY_CLASS:%.*]] = load_borrow [[ACCESS_ADDR]]
// CHECK:  apply {{%.*}}([[BORROWED_COPY_CLASS]])
// CHECK: } // end sil function '$s8moveonly38klassBorrowCopyableAddressFunctionCallyyF'
func klassBorrowCopyableAddressFunctionCall() {
    var k = Klass()
    k = Klass()
    borrowVal(k.copyableKlass)
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

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly018moveOnlyStructMoveC20KlassNonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK: [[STRUCT_EXT:%.*]] = struct_element_addr [[ACCESS]]
// CHECK: [[BORROW:%.*]] = load_borrow [[STRUCT_EXT]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA5KlassCF
// CHECK: apply [[FN]]([[BORROW]])
// CHECK: end_borrow [[BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly018moveOnlyStructMoveC20KlassNonConsumingUseyyF'
func moveOnlyStructMoveOnlyKlassNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.moveOnlyKlass)
}

// We fail here b/c we are accessing through a class.
//
// CHECK-LABEL: sil hidden [ossa] @$s8moveonly018moveOnlyStructMovec5KlassecF15NonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK:   [[STRUCT_EXT:%.*]] = struct_element_addr [[ACCESS]]
// CHECK:   [[STRUCT_EXT_COPY:%.*]] = load [copy] [[STRUCT_EXT]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   [[STRUCT_EXT_COPY_BORROW:%.*]] = begin_borrow [[STRUCT_EXT_COPY]]
// CHECK:   [[ELT_ADDR:%.*]] = ref_element_addr [[STRUCT_EXT_COPY_BORROW]]
// CHECK:   [[ACCESS_ELT_ADDR:%.*]] = begin_access [read] [dynamic] [[ELT_ADDR]]
// CHECK:   [[KLS:%.*]] = load_borrow [[ACCESS_ELT_ADDR]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA6Klass2CF
// CHECK:   apply [[FN]]([[KLS]])
// CHECK:   end_borrow [[KLS]]
// CHECK:   destroy_value [[STRUCT_EXT_COPY]]
// CHECK: } // end sil function '$s8moveonly018moveOnlyStructMovec5KlassecF15NonConsumingUseyyF'
func moveOnlyStructMoveOnlyKlassMoveOnlyKlassNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.moveOnlyKlass.moveOnlyKlass)
}

// We fail here b/c we are accessing through a class.
//
// CHECK-LABEL: sil hidden [ossa] @$s8moveonly018moveOnlyStructMovec13KlassCopyableF15NonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK:   [[STRUCT_EXT:%.*]] = struct_element_addr [[ACCESS]]
// CHECK:   [[STRUCT_EXT_COPY:%.*]] = load [copy] [[STRUCT_EXT]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   [[STRUCT_EXT_COPY_BORROW:%.*]] = begin_borrow [[STRUCT_EXT_COPY]]
// CHECK:   [[FIELD:%.*]] = ref_element_addr [[STRUCT_EXT_COPY_BORROW]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [dynamic] [[FIELD]]
// CHECK:   [[BORROWED_KLS:%.*]] = load_borrow [[ACCESS]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA13CopyableKlassCF
// CHECK:   apply [[FN]]([[BORROWED_KLS]])
// CHECK:   end_borrow [[BORROWED_KLS]]
// CHECK:   destroy_value [[STRUCT_EXT_COPY]]
// CHECK: } // end sil function '$s8moveonly018moveOnlyStructMovec13KlassCopyableF15NonConsumingUseyyF'
func moveOnlyStructMoveOnlyKlassCopyableKlassNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.moveOnlyKlass.copyableKlass)
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

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly018moveOnlyStructMovecdeC20KlassNonConsumingUseyyF : $@convention(thin) () -> () {
// CHECK:   [[MARKED_ADDR:%.*]] = mark_must_check [consumable_and_assignable]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[ACCESS]] : $*NonTrivialStruct, #NonTrivialStruct.nonTrivialStruct2
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialStruct2, #NonTrivialStruct2.moveOnlyKlass
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP2]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA5KlassCF : $@convention(thin) (@guaranteed Klass) -> ()
// CHECK:   apply [[FN]]([[BORROW]])
// CHECK:   end_borrow [[BORROW]]
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly018moveOnlyStructMovecdeC20KlassNonConsumingUseyyF'
func moveOnlyStructMoveOnlyStructMoveOnlyKlassNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.nonTrivialStruct2.moveOnlyKlass)
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
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [dynamic] [[FIELD]]
// CHECK:   [[BORROWED_MOVEONLY_KLASS:%.*]] = load_borrow [[ACCESS]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly9borrowValyyAA5KlassCF :
// CHECK:   apply [[FN]]([[BORROWED_MOVEONLY_KLASS]])
// CHECK:   end_borrow [[BORROWED_MOVEONLY_KLASS]]
// CHECK:   destroy_value [[COPYABLE_KLASS]]
// CHECK: } // end sil function '$s8moveonly022moveOnlyStructCopyabledede9KlassMovecF15NonConsumingUseyyF'
func moveOnlyStructCopyableStructCopyableStructCopyableKlassMoveOnlyKlassNonConsumingUse() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    borrowVal(k.nonTrivialCopyableStruct.nonTrivialCopyableStruct2.copyableKlass.k)
}

///////////////////////
// Enum Switch Tests //
///////////////////////

enum EnumSwitchTests {
    @_moveOnly
    enum E2 {
        case lhs(CopyableKlass)
        case rhs(Klass)
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
// CHECK:   [[BBARG_COPY:%.*]] = copy_value [[BBARG]]
// CHECK:   [[NEW_VAL:%.*]] = move_value [lexical] [[BBARG_COPY]]
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
// CHECK:   [[BBARG_COPY:%.*]] = copy_value [[BBARG]]
// CHECK:   move_value [lexical] [[BBARG_COPY]]
// CHECK:   end_borrow [[BORROWED_VALUE]]
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

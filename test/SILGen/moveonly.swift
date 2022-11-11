// RUN: %target-swift-emit-silgen -enable-experimental-move-only %s | %FileCheck %s

//////////////////
// Declarations //
//////////////////

@_moveOnly
public class Klass2 {}

@_moveOnly
public class Klass {
    var intField: Int
    var klsField: Klass2

    // CHECK-LABEL: sil hidden [ossa] @$s8moveonly5KlassCACycfc : $@convention(method) (@owned Klass) -> @owned Klass {
    // CHECK: bb0([[ARG:%.*]] : @owned $Klass):
    // CHECK:   [[MARK_UNINIT:%.*]] = mark_uninitialized [rootself] [[ARG]]
    // CHECK:   [[MARK_NO_IMP_COPY:%.*]] = mark_must_check [no_implicit_copy] [[MARK_UNINIT]]
    // CHECK: } // end sil function '$s8moveonly5KlassCACycfc'
    init() {
        klsField = Klass2()
        intField = 5
    }
}

public func nonConsumingUseKlass(_ k: Klass) {}
public func nonConsumingUseKlass2(_ k: Klass2) {}

@_moveOnly
public struct NonTrivialStruct2 {
    var k = Klass()
}

public func nonConsumingUseNonTrivialStruct2(_ s: NonTrivialStruct2) {}

@_moveOnly
public struct NonTrivialStruct {
    var k = Klass()
    var k2 = NonTrivialStruct2()
}

public func nonConsumingUseNonTrivialStruct(_ s: NonTrivialStruct) {}

@_moveOnly
public enum NonTrivialEnum {
    case first
    case second(Klass)
    case third(NonTrivialStruct)
}

public func nonConsumingUseNonTrivialEnum(_ e : NonTrivialEnum) {}

///////////
// Tests //
///////////

//===---
// Function Arguments
//

// CHECK-LABEL: sil [ossa] @$s8moveonly8useKlassyyAA0C0CF : $@convention(thin) (@guaranteed Klass) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Klass):
// CHECK:   [[OWNED_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   [[MARKED_OWNED_ARG:%.*]] = mark_must_check [no_copy] [[OWNED_ARG]]
// CHECK: } // end sil function '$s8moveonly8useKlassyyAA0C0CF'
public func useKlass(_ k: Klass) {
    nonConsumingUseKlass(k)
    let k2 = k
    nonConsumingUseKlass(k)
    let _ = k2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly15useKlassConsumeyyAA0C0CnF : $@convention(thin) (@owned Klass) -> () {
// CHECK: bb0([[ARG:%.*]] : @owned $Klass):
// TODO: Is this move_value [lexical] necessary?
// CHECK:   [[LEXICAL_MOVE:%.*]] = move_value [lexical] [[ARG]]
// CHECK:   mark_must_check [no_implicit_copy] [[LEXICAL_MOVE]]
// CHECK: } // end sil function '$s8moveonly15useKlassConsumeyyAA0C0CnF'
public func useKlassConsume(_ k: __owned Klass) {
    nonConsumingUseKlass(k)
    let k2 = k
    // NOTE: We should mark the next line as a lifetime extending use.
    nonConsumingUseKlass(k)
    let _ = k2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly19useNonTrivialStructyyAA0cdE0VF : $@convention(thin) (@guaranteed NonTrivialStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $NonTrivialStruct):
// CHECK:   [[COPIED_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   mark_must_check [no_copy] [[COPIED_ARG]]
// CHECK: } // end sil function '$s8moveonly19useNonTrivialStructyyAA0cdE0VF'
public func useNonTrivialStruct(_ s: NonTrivialStruct) {
    nonConsumingUseNonTrivialStruct(s)
    let s2 = s
    let k = s.k
    let _ = k
    nonConsumingUseNonTrivialStruct(s)
    let _ = s2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly24useNonTrivialOwnedStructyyAA0cdF0VnF : $@convention(thin) (@owned NonTrivialStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : @owned $NonTrivialStruct):
// CHECK:   [[MOVED_ARG:%.*]] = move_value [lexical] [[ARG]]
// CHECK:   mark_must_check [no_implicit_copy] [[MOVED_ARG]]
// CHECK: } // end sil function '$s8moveonly24useNonTrivialOwnedStructyyAA0cdF0VnF'
public func useNonTrivialOwnedStruct(_ s: __owned NonTrivialStruct) {
    nonConsumingUseNonTrivialStruct(s)
    let s2 = s
    let k = s.k
    let _ = k
    nonConsumingUseNonTrivialStruct(s)
    let _ = s2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly17useNonTrivialEnumyyAA0cdE0OF : $@convention(thin) (@guaranteed NonTrivialEnum) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $NonTrivialEnum):
// CHECK:   [[COPIED_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   mark_must_check [no_copy] [[COPIED_ARG]]
// CHECK: } // end sil function '$s8moveonly17useNonTrivialEnumyyAA0cdE0OF'
public func useNonTrivialEnum(_ s: NonTrivialEnum) {
    nonConsumingUseNonTrivialEnum(s)
    let s2 = s
    switch s {
    case _:
        break
    }
    nonConsumingUseNonTrivialEnum(s)
    let _ = s2
}

// CHECK-LABEL: sil [ossa] @$s8moveonly22useNonTrivialOwnedEnumyyAA0cdF0OnF : $@convention(thin) (@owned NonTrivialEnum) -> () {
// CHECK: bb0([[ARG:%.*]] : @owned $NonTrivialEnum):
// CHECK:   [[MOVED_ARG:%.*]] = move_value [lexical] [[ARG]]
// CHECK:   mark_must_check [no_implicit_copy] [[MOVED_ARG]]
// CHECK: } // end sil function '$s8moveonly22useNonTrivialOwnedEnumyyAA0cdF0OnF'
public func useNonTrivialOwnedEnum(_ s: __owned NonTrivialEnum) {
    nonConsumingUseNonTrivialEnum(s)
    let s2 = s
    switch s {
    case _:
        break
    }
    nonConsumingUseNonTrivialEnum(s)
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
    // CHECK:   mark_must_check [no_copy] [[COPIED_ARG]]
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
    // CHECK:   mark_must_check [no_copy] [[COPIED_ARG]]
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
    // CHECK:   mark_must_check [no_copy] [[COPIED_ARG]]
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
// CHECK: [[X_MV_ONLY:%.*]] = mark_must_check [no_implicit_copy] [[X_MV_LEXICAL]]
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
// CHECK: [[X_MV_ONLY:%.*]] = mark_must_check [no_implicit_copy] [[X_MV_LEXICAL]]
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
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [no_implicit_copy] [[PROJECT_BOX]]
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
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [no_implicit_copy] [[PROJECT_BOX]]
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
// CHECK: [[CLS:%.*]] = mark_must_check [no_implicit_copy]
// CHECK: [[BORROW:%.*]] = begin_borrow [[CLS]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly20nonConsumingUseKlassyyAA0E0CF
// CHECK: apply [[FN]]([[BORROW]])
// CHECK: end_borrow [[BORROW]]
// CHECK: } // end sil function '$s8moveonly24borrowObjectFunctionCallyyF'
func borrowObjectFunctionCall() {
    let k = Klass()
    nonConsumingUseKlass(k)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly25borrowAddressFunctionCallyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = mark_must_check [no_implicit_copy]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[BOX]]
// CHECK: [[BORROW:%.*]] = load_borrow [[ACCESS]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly20nonConsumingUseKlassyyAA0E0CF
// CHECK: apply [[FN]]([[BORROW]])
// CHECK: end_borrow [[BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly25borrowAddressFunctionCallyyF'
func borrowAddressFunctionCall() {
    var k = Klass()
    k = Klass()
    nonConsumingUseKlass(k)
}

// We currently have the wrong behavior here since the class is treated by
// LValue emission as a base. That being said, move only classes are not our
// high order bit here, so I filed: ...;
//
// CHECK-LABEL: sil hidden [ossa] @$s8moveonly31klassBorrowAddressFunctionCall2yyF : $@convention(thin) () -> () {
// CHECK: [[MARK:%.*]] = mark_must_check [no_implicit_copy]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARK]]
// CHECK: } // end sil function '$s8moveonly31klassBorrowAddressFunctionCall2yyF'
func klassBorrowAddressFunctionCall2() {
    var k = Klass()
    k = Klass()
    nonConsumingUseKlass2(k.klsField)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly31structBorrowObjectFunctionCall1yyF : $@convention(thin) () -> () {
// CHECK: [[VALUE:%.*]] = mark_must_check [no_implicit_copy]
// CHECK: [[BORROW:%.*]] = begin_borrow [[VALUE]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly31nonConsumingUseNonTrivialStructyyAA0efG0VF : $@convention(thin) (@guaranteed NonTrivialStruct) -> ()
// CHECK: apply [[FN]]([[BORROW]])
// CHECK: end_borrow [[BORROW]]
// CHECK: [[BORROW:%.*]] = begin_borrow [[VALUE]]
// CHECK: [[STRUCT_EXT:%.*]] = struct_extract [[BORROW]]
// CHECK: [[STRUCT_EXT_COPY:%.*]] = copy_value [[STRUCT_EXT]]
// TODO: Should we insert a mark_must_check_here?
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly20nonConsumingUseKlassyyAA0E0CF
// CHECK: apply [[FN]]([[STRUCT_EXT_COPY]])
// CHECK: destroy_value [[STRUCT_EXT_COPY]]
// CHECK: end_borrow [[BORROW]]
// CHECK: [[BORROW:%.*]] = begin_borrow [[VALUE]]
// CHECK: [[STRUCT_EXT:%.*]] = struct_extract [[BORROW]]
// CHECK: [[STRUCT_EXT_COPY:%.*]] = copy_value [[STRUCT_EXT]]
// CHECK: [[STRUCT_EXT_COPY_BORROW:%.*]] = begin_borrow [[STRUCT_EXT_COPY]]
// CHECK: [[METHOD:%.*]] = class_method [[STRUCT_EXT_COPY_BORROW]]
// CHECK: [[KLS2:%.*]] = apply [[METHOD]]([[STRUCT_EXT_COPY_BORROW]])
// CHECK: end_borrow [[STRUCT_EXT_COPY_BORROW]]
// CHECK: [[BORROWED_KLS2:%.*]] = begin_borrow [[KLS2]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly21nonConsumingUseKlass2yyAA0E0CF
// CHECK: apply [[FN]]([[BORROWED_KLS2]])
// CHECK: destroy_value [[STRUCT_EXT_COPY]]
// CHECK: destroy_value [[KLS2]]
// CHECK: end_borrow [[BORROW]]
// CHECK: destroy_value [[VALUE]]
// CHECK: } // end sil function '$s8moveonly31structBorrowObjectFunctionCall1yyF'
func structBorrowObjectFunctionCall1() {
    let k = NonTrivialStruct()
    nonConsumingUseNonTrivialStruct(k)
    nonConsumingUseKlass(k.k)
    nonConsumingUseKlass2(k.k.klsField)
}

// CHECK-LABEL: sil hidden [ossa] @$s8moveonly31structBorrowObjectFunctionCall2yyF : $@convention(thin) () -> () {
// CHECK: [[MARKED_ADDR:%.*]] = mark_must_check [no_implicit_copy]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK: [[BORROW:%.*]] = load_borrow [[ACCESS]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly31nonConsumingUseNonTrivialStructyyAA0efG0VF :
// CHECK: apply [[FN]]([[BORROW]])
// CHECK: end_borrow [[BORROW]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK: [[STRUCT_EXT:%.*]] = struct_element_addr [[ACCESS]]
// CHECK: [[BORROW:%.*]] = load_borrow [[STRUCT_EXT]]
// CHECK: [[FN:%.*]] = function_ref @$s8moveonly20nonConsumingUseKlassyyAA0E0CF
// CHECK: apply [[FN]]([[BORROW]])
// CHECK: end_borrow [[BORROW]]
// CHECK: end_access [[ACCESS]]
//
// This case we fail due to the class being a base element. We need to use the
// scope expansion approach to handle this.
//
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK:   [[STRUCT_EXT:%.*]] = struct_element_addr [[ACCESS]]
// CHECK:   [[STRUCT_EXT_COPY:%.*]] = load [copy] [[STRUCT_EXT]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   [[STRUCT_EXT_COPY_BORROW:%.*]] = begin_borrow [[STRUCT_EXT_COPY]]
// CHECK:   [[METHOD:%.*]] = class_method [[STRUCT_EXT_COPY_BORROW]]
// CHECK:   [[KLS:%.*]] = apply [[METHOD]]([[STRUCT_EXT_COPY_BORROW]])
// CHECK:   end_borrow [[STRUCT_EXT_COPY_BORROW]]
// CHECK:   [[BORROWED_KLS:%.*]] = begin_borrow [[KLS]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly21nonConsumingUseKlass2yyAA0E0CF
// CHECK:   apply [[FN]]([[BORROWED_KLS]])
// CHECK:   end_borrow [[BORROWED_KLS]]
// CHECK:   destroy_value [[STRUCT_EXT_COPY]]
// CHECK:   destroy_value [[KLS]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK:   [[GEP:%.*]] = struct_element_addr [[ACCESS]] : $*NonTrivialStruct, #NonTrivialStruct.k2
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly32nonConsumingUseNonTrivialStruct2yyAA0efG0VF : $@convention(thin) (@guaranteed NonTrivialStruct2) -> ()
// CHECK:   apply [[FN]]([[BORROW]])
// CHECK:   end_borrow [[BORROW]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[MARKED_ADDR]]
// CHECK:   [[GEP1:%.*]] = struct_element_addr [[ACCESS]] : $*NonTrivialStruct, #NonTrivialStruct.k2
// CHECK:   [[GEP2:%.*]] = struct_element_addr [[GEP1]] : $*NonTrivialStruct2, #NonTrivialStruct2.k
// CHECK:   [[BORROW:%.*]] = load_borrow [[GEP2]]
// CHECK:   [[FN:%.*]] = function_ref @$s8moveonly20nonConsumingUseKlassyyAA0E0CF : $@convention(thin) (@guaranteed Klass) -> ()
// CHECK:   apply [[FN]]([[BORROW]])
// CHECK:   end_borrow [[BORROW]]
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s8moveonly31structBorrowObjectFunctionCall2yyF'
func structBorrowObjectFunctionCall2() {
    var k = NonTrivialStruct()
    k = NonTrivialStruct()
    nonConsumingUseNonTrivialStruct(k)
    nonConsumingUseKlass(k.k)
    nonConsumingUseKlass2(k.k.klsField)
    nonConsumingUseNonTrivialStruct2(k.k2)
    nonConsumingUseKlass(k.k2.k)
}

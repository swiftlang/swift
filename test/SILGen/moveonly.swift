// RUN: %target-swift-emit-silgen -enable-experimental-move-only %s | %FileCheck %s

//////////////////
// Declarations //
//////////////////

@_moveOnly
public class Klass {
    var intField: Int
    var klsField: Klass?

    // CHECK-LABEL: sil hidden [ossa] @$s8moveonly5KlassCACycfc : $@convention(method) (@owned Klass) -> @owned Klass {
    // CHECK: bb0([[ARG:%.*]] : @owned $Klass):
    // CHECK:   [[MARK_UNINIT:%.*]] = mark_uninitialized [rootself] [[ARG]]
    // CHECK:   [[MARK_NO_IMP_COPY:%.*]] = mark_must_check [no_implicit_copy] [[MARK_UNINIT]]
    // CHECK: } // end sil function '$s8moveonly5KlassCACycfc'
    init() {
        klsField = Klass()
        intField = 5
    }
}

public func nonConsumingUseKlass(_ k: Klass) {}

@_moveOnly
public struct NonTrivialStruct {
    var k = Klass()
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

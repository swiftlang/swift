// RUN: %target-swift-emit-silgen %s | %FileCheck %s

func readwrite(_ : inout String) {}

struct SimpleModify {
  var stored: String

  var modifiable: String {
    get {}
// CHECK-LABEL: sil hidden @$s15modify_accessor12SimpleModifyV10modifiableSSvM
// CHECK-SAME:    : $@yield_once @convention(method) (@inout SimpleModify) -> @yields @inout String {
// CHECK:         [[SELF:%.*]] = begin_access [modify] [unknown] %0 : $*SimpleModify
// CHECK-NEXT:    [[FIELD:%.*]] = struct_element_addr [[SELF]] : $*SimpleModify, #SimpleModify.stored
// CHECK-NEXT:    yield [[FIELD]] : $*String, resume bb1, unwind bb2
// CHECK:       bb1:
// CHECK-NEXT:    end_access [[SELF]] : $*SimpleModify
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
// CHECK:       bb2:
// CHECK-NEXT:    end_access [[SELF]] : $*SimpleModify
// CHECK-NEXT:    unwind
    _modify {
      yield &stored
    }
  }

// CHECK-LABEL: sil hidden @$s15modify_accessor12SimpleModifyV3set6stringySS_tF
// CHECK:         [[VALUE:%.*]] = copy_value %0 : $String
// CHECK-NEXT:    [[SELF:%.*]] = begin_access [modify] [unknown] %1 : $*SimpleModify
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[MODIFYFN:%.*]] = function_ref @$s15modify_accessor12SimpleModifyV10modifiableSSvM
// CHECK-NEXT:    ([[FIELD:%.*]], [[TOKEN:%.*]]) = begin_apply [[MODIFYFN]]([[SELF]])
// CHECK-NEXT:    assign [[VALUE]] to [[FIELD]] : $*String
// CHECK-NEXT:    end_apply [[TOKEN]]
// CHECK-NEXT:    end_access [[SELF]] : $*SimpleModify
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
mutating func set(string: String) {
    modifiable = string
  }

// CHECK-LABEL: sil hidden @$s15modify_accessor12SimpleModifyV0A0yyF
// CHECK:         [[SELF:%.*]] = begin_access [modify] [unknown] %0 : $*SimpleModify
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[MODIFYFN:%.*]] = function_ref @$s15modify_accessor12SimpleModifyV10modifiableSSvM
// CHECK-NEXT:    ([[FIELD:%.*]], [[TOKEN:%.*]]) = begin_apply [[MODIFYFN]]([[SELF]])
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[READWRITE:%.*]] = function_ref @$s15modify_accessor9readwriteyySSzF
// CHECK-NEXT:    apply [[READWRITE]]([[FIELD]])
// CHECK-NEXT:    end_apply [[TOKEN]]
// CHECK-NEXT:    end_access [[SELF]] : $*SimpleModify
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
  mutating func modify() {
    readwrite(&modifiable)
  }
}

class SetterSynthesisFromModify {
  var stored: String = "test"
  var modifiable: String {
    get { return stored }
// CHECK: sil hidden [transparent] @$s15modify_accessor25SetterSynthesisFromModifyC10modifiableSSvs
// CHECK:         [[VALUE_BORROW:%.*]] = begin_borrow %0 : $String
// CHECK-NEXT:    [[VALUE:%.*]] = copy_value [[VALUE_BORROW]] : $String
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[MODIFYFN:%.*]] = function_ref @$s15modify_accessor25SetterSynthesisFromModifyC10modifiableSSvM
// CHECK-NEXT:    ([[FIELD:%.*]], [[TOKEN:%.*]]) = begin_apply [[MODIFYFN]](%1)
// CHECK-NEXT:    assign [[VALUE]] to [[FIELD]] : $*String
// CHECK-NEXT:    end_apply [[TOKEN]]
// CHECK-NEXT:    end_borrow [[VALUE_BORROW]]
// CHECK-NEXT:    destroy_value %0 : $String
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
    _modify {
      yield &stored
    }
  }
}

struct ModifyAndSet {
  var stored: String

  var modifiable: String {
    get { return stored }
    _modify { yield &stored }
    set(value) { stored = value }
  }

// CHECK-LABEL: sil hidden @$s15modify_accessor12ModifyAndSetV3set6stringySS_tF
// CHECK:         [[VALUE:%.*]] = copy_value %0 : $String
// CHECK-NEXT:    [[SELF:%.*]] = begin_access [modify] [unknown] %1 : $*ModifyAndSet
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[SETTERFN:%.*]] = function_ref @$s15modify_accessor12ModifyAndSetV10modifiableSSvs
// CHECK-NEXT:    apply [[SETTERFN]]([[VALUE]], [[SELF]])
// CHECK-NEXT:    end_access [[SELF]] : $*ModifyAndSet
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
  mutating func set(string: String) {
    modifiable = string
  }

  // CHECK-LABEL: sil hidden @$s15modify_accessor12ModifyAndSetV0A0yyF
  // CHECK:         [[SELF:%.*]] = begin_access [modify] [unknown] %0 : $*ModifyAndSet
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    [[MODIFYFN:%.*]] = function_ref @$s15modify_accessor12ModifyAndSetV10modifiableSSvM
  // CHECK-NEXT:    ([[FIELD:%.*]], [[TOKEN:%.*]]) = begin_apply [[MODIFYFN]]([[SELF]])
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    [[READWRITE:%.*]] = function_ref @$s15modify_accessor9readwriteyySSzF
  // CHECK-NEXT:    apply [[READWRITE]]([[FIELD]])
  // CHECK-NEXT:    end_apply [[TOKEN]]
  // CHECK-NEXT:    end_access [[SELF]] : $*ModifyAndSet
  // CHECK-NEXT:    [[RET:%.*]] = tuple ()
  // CHECK-NEXT:    return [[RET]] : $()
  mutating func modify() {
    readwrite(&modifiable)
  }
}

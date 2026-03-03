// RUN: %target-swift-frontend -enable-copy-propagation=requested-passes-only -enable-lexical-lifetimes=false -Xllvm -sil-print-types -emit-sil %s -swift-version 5 -verify | %FileCheck %s

// Ensure that convenience initializers on concrete types can
// delegate to factory initializers defined in protocol
// extensions.

protocol TriviallyConstructible {
  init(lower: Int)
}

extension TriviallyConstructible {
  init(middle: Int) {
    self.init(lower: middle)
  }

  init?(failingMiddle: Int) {
    self.init(lower: failingMiddle)
  }

  init(throwingMiddle: Int) throws {
    self.init(lower: throwingMiddle)
  }
}

class TrivialClass : TriviallyConstructible {
  required init(lower: Int) {}

  // CHECK-LABEL: sil hidden @$s023definite_init_protocol_B012TrivialClassC5upperACSi_tcfC
  // CHECK:     bb0(%0 : $Int, [[SELF_META:%.*]] : $@thick TrivialClass.Type):
  // CHECK-NEXT:  [[SELF_BOX:%.*]] = alloc_stack [var_decl] $TrivialClass
  // CHECK-NEXT:  debug_value
  // CHECK-NEXT:  [[METATYPE:%.*]] = unchecked_trivial_bit_cast [[SELF_META]] {{.*}} to $@thick @dynamic_self TrivialClass.Type
  // CHECK-NEXT:  [[RESULT:%.*]] = alloc_stack $TrivialClass
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[FN:%.*]] = function_ref @$s023definite_init_protocol_B022TriviallyConstructiblePAAE6middlexSi_tcfC
  // CHECK-NEXT:  apply [[FN]]<@dynamic_self TrivialClass>([[RESULT]], %0, [[METATYPE]])
  // CHECK-NEXT:  [[NEW_SELF:%.*]] = load [[RESULT]]
  // TODO: Once we restore arbitrary takes, the strong_retain/destroy_addr pair below will go away.
  // CHECK-NEXT:  strong_retain [[NEW_SELF]]
  // CHECK-NEXT:  store [[NEW_SELF]] to [[SELF_BOX]]
  // CHECK-NEXT:  dealloc_stack [[RESULT]]
  // CHECK-NEXT:  destroy_addr [[SELF_BOX]]
  // CHECK-NEXT:  dealloc_stack [[SELF_BOX]]
  // CHECK-NEXT:  return [[NEW_SELF]]
  convenience init(upper: Int) {
    self.init(middle: upper)
  }

  convenience init?(failingUpper: Int) {
    self.init(failingMiddle: failingUpper)
  }

  convenience init(throwingUpper: Int) throws {
    try self.init(throwingMiddle: throwingUpper)
  }
}

struct TrivialStruct : TriviallyConstructible {
  let x: Int

  init(lower: Int) { self.x = lower }

// CHECK-LABEL: sil hidden @$s023definite_init_protocol_B013TrivialStructV5upperACSi_tcfC
// CHECK:     bb0(%0 : $Int, %1 : $@thin TrivialStruct.Type):
// CHECK-NEXT: [[SELF:%.*]] = alloc_stack [var_decl] $TrivialStruct
// CHECK:      [[SELF_BOX:%.*]] = alloc_stack $TrivialStruct
// CHECK-NEXT: [[METATYPE:%.*]] = metatype $@thick TrivialStruct.Type
// CHECK:      [[FN:%.*]] = function_ref @$s023definite_init_protocol_B022TriviallyConstructiblePAAE6middlexSi_tcfC
// CHECK-NEXT: apply [[FN]]<TrivialStruct>([[SELF_BOX]], %0, [[METATYPE]])
// CHECK-NEXT: [[NEW_SELF:%.*]] = load [[SELF_BOX]]
// CHECK-NEXT: store [[NEW_SELF]] to [[SELF]]
// CHECK-NEXT: dealloc_stack [[SELF_BOX]]
// CHECK-NEXT: dealloc_stack [[SELF]]
// CHECK-NEXT: return [[NEW_SELF]]
  init(upper: Int) {
    self.init(middle: upper)
  }

  init?(failingUpper: Int) {
    self.init(failingMiddle: failingUpper)
  }

  init(throwingUpper: Int) throws {
    try self.init(throwingMiddle: throwingUpper)
  }
}

struct AddressOnlyStruct : TriviallyConstructible {
  let x: Any

  init(lower: Int) { self.x = lower }

// CHECK-LABEL: sil hidden @$s023definite_init_protocol_B017AddressOnlyStructV5upperACSi_tcfC
// CHECK:     bb0(%0 : $*AddressOnlyStruct, %1 : $Int, %2 : $@thin AddressOnlyStruct.Type):
// CHECK-NEXT: [[SELF:%.*]] = alloc_stack [var_decl] $AddressOnlyStruct
// CHECK:      [[SELF_BOX:%.*]] = alloc_stack $AddressOnlyStruct
// CHECK-NEXT: [[METATYPE:%.*]] = metatype $@thick AddressOnlyStruct.Type
// CHECK:      [[FN:%.*]] = function_ref @$s023definite_init_protocol_B022TriviallyConstructiblePAAE6middlexSi_tcfC
// CHECK-NEXT: apply [[FN]]<AddressOnlyStruct>([[SELF_BOX]], %1, [[METATYPE]])
// CHECK-NEXT: copy_addr [take] [[SELF_BOX]] to [init] [[SELF]]
// CHECK-NEXT: dealloc_stack [[SELF_BOX]]
// CHECK-NEXT: copy_addr [[SELF]] to [init] %0
// CHECK-NEXT: destroy_addr [[SELF]]
// CHECK-NEXT: dealloc_stack [[SELF]]
// CHECK-NEXT: [[RESULT:%.*]] = tuple ()
// CHECK-NEXT: return [[RESULT]]
  init(upper: Int) {
    self.init(middle: upper)
  }

  init?(failingUpper: Int) {
    self.init(failingMiddle: failingUpper)
  }

  init(throwingUpper: Int) throws {
    try self.init(throwingMiddle: throwingUpper)
  }
}

enum TrivialEnum : TriviallyConstructible {
  case NotSoTrivial

  init(lower: Int) {
    self = .NotSoTrivial
  }

// CHECK-LABEL: sil hidden @$s023definite_init_protocol_B011TrivialEnumO5upperACSi_tcfC
// CHECK:     bb0(%0 : $Int, %1 : $@thin TrivialEnum.Type):
// CHECK-NEXT: [[SELF:%.*]] = alloc_stack [var_decl] $TrivialEnum
// CHECK:      [[SELF_BOX:%.*]] = alloc_stack $TrivialEnum
// CHECK-NEXT: [[METATYPE:%.*]] = metatype $@thick TrivialEnum.Type
// CHECK:      [[FN:%.*]] = function_ref @$s023definite_init_protocol_B022TriviallyConstructiblePAAE6middlexSi_tcfC
// CHECK-NEXT: apply [[FN]]<TrivialEnum>([[SELF_BOX]], %0, [[METATYPE]])
// CHECK-NEXT: [[NEW_SELF:%.*]] = load [[SELF_BOX]]
// CHECK-NEXT: store [[NEW_SELF]] to [[SELF]]
// CHECK-NEXT: dealloc_stack [[SELF_BOX]]
// CHECK-NEXT: dealloc_stack [[SELF]]
// CHECK-NEXT: return [[NEW_SELF]]
  init(upper: Int) {
    self.init(middle: upper)
  }

  init?(failingUpper: Int) {
    self.init(failingMiddle: failingUpper)
  }

  init(throwingUpper: Int) throws {
    try self.init(throwingMiddle: throwingUpper)
  }
}

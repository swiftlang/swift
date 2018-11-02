// RUN: %target-swift-frontend -emit-sil -enable-sil-ownership %s | %FileCheck %s

enum ValueEnum {
  case a(String)
  case b
  case c

  init() { self = .b }

  init(a: Double) {
    self.init()
    _ = self // okay: self has been initialized by the delegation above
    self = .c
  }

  init(a: Float) {
    self.init()
    self.init() // this is now OK
  }

  init(e: Bool) {
    if e {
      self = ValueEnum()
    } else {
      self.init()
    }
  }

  // CHECK-LABEL: sil hidden @$S25definite_init_value_types9ValueEnumO1xACSb_tcfC : $@convention(method) (Bool, @thin ValueEnum.Type) -> @owned ValueEnum
  // CHECK:      bb0(%0 : $Bool, %1 : $@thin ValueEnum.Type):
  // CHECK-NEXT:   [[STATE:%.*]] = alloc_stack $Builtin.Int1
  // CHECK-NEXT:   [[SELF_BOX:%.*]] = alloc_stack $ValueEnum
  // CHECK-NEXT:   [[INIT_STATE:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK-NEXT:   store [[INIT_STATE]] to [[STATE]]
  // CHECK:        [[BOOL:%.*]] = struct_extract %0 : $Bool, #Bool._value
  // CHECK-NEXT:   cond_br [[BOOL]], bb1, bb2
  // CHECK:      bb1:
  // CHECK-NEXT:   [[METATYPE:%.*]] = metatype $@thin ValueEnum.Type
  // CHECK-NEXT:   [[NEW_SELF:%.*]] = enum $ValueEnum, #ValueEnum.b!enumelt
  // CHECK-NEXT:   [[SELF_ACCESS:%.*]] = begin_access [modify] [static] [[SELF_BOX]]
  // CHECK-NEXT:   [[NEW_STATE:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK-NEXT:   store [[NEW_STATE]] to [[STATE]]
  // CHECK-NEXT:   store [[NEW_SELF]] to [[SELF_ACCESS]]
  // CHECK-NEXT:   end_access [[SELF_ACCESS]]
  // CHECK-NEXT:   br bb2
  // CHECK:      bb2:
  // CHECK-NEXT:   [[METATYPE:%.*]] = metatype $@thin ValueEnum.Type
  // CHECK-NEXT:   [[NEW_SELF:%.*]] = enum $ValueEnum, #ValueEnum.c!enumelt
  // CHECK-NEXT:   [[SELF_ACCESS:%.*]] = begin_access [modify] [static] [[SELF_BOX]]
  // CHECK-NEXT:   [[STATE_VALUE:%.*]] = load [[STATE]]
  // CHECK-NEXT:   cond_br [[STATE_VALUE]], bb3, bb4
  // CHECK:      bb3:
  // CHECK-NEXT:   destroy_addr [[SELF_BOX]]
  // CHECK-NEXT:   br bb4
  // CHECK:      bb4:
  // CHECK-NEXT:   [[NEW_STATE:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK-NEXT:   store [[NEW_STATE]] to [[STATE]]
  // CHECK-NEXT:   store [[NEW_SELF]] to [[SELF_ACCESS]]
  // CHECK-NEXT:   end_access [[SELF_ACCESS]]
  // CHECK-NEXT:   retain_value [[NEW_SELF]]
  // CHECK-NEXT:   destroy_addr [[SELF_BOX]]
  // CHECK-NEXT:   dealloc_stack [[SELF_BOX]]
  // CHECK-NEXT:   dealloc_stack [[STATE]]
  // CHECK-NEXT:   return [[NEW_SELF]]
  init(x: Bool) {
    if x {
      self = .b
    }
    self = .c
  }
}

enum AddressEnum {
  case a(Any)
  case b
  case c

  init() { self = .b }

  init(e: Bool) {
    if e {
      self = AddressEnum()
    } else {
      self.init()
    }
  }

  init(x: Bool) {
    if x {
      self = .b
    }
    self = .c
  }
}

struct EmptyStruct {}

struct ValueStruct {
  var ivar: EmptyStruct

  init() { ivar = EmptyStruct() }

  init(a: Float) {
    self.init()
    self.init()
  }

  init(e: Bool) {
    if e {
      self.init()
    } else {
      self = ValueStruct()
    }
  }
}

struct AddressStruct {
  var ivar: EmptyStruct // expected-note {{'self.ivar' not initialized}}
  var any: Any?

  init() { ivar = EmptyStruct(); any = nil }

  init(e: Bool) {
    if e {
      self = AddressStruct()
    } else {
      self.init()
    }
  }
}

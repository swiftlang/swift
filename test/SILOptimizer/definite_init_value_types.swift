// RUN: %target-swift-frontend -enable-copy-propagation=requested-passes-only -enable-lexical-lifetimes=false -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s

// REQUIRES: swift_in_compiler

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

  // CHECK-LABEL: sil hidden @$s25definite_init_value_types9ValueEnumO1xACSb_tcfC : $@convention(method) (Bool, @thin ValueEnum.Type) -> @owned ValueEnum
  // CHECK:      bb0(%0 : $Bool, %1 : $@thin ValueEnum.Type):
  // CHECK-NEXT:   [[SELF_BOX:%.*]] = alloc_stack [dynamic_lifetime] [var_decl] $ValueEnum
  // CHECK-NEXT:   [[INIT_STATE:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:        [[BOOL:%.*]] = struct_extract %0 : $Bool, #Bool._value
  // CHECK-NEXT:   cond_br [[BOOL]], bb1, bb2
  // CHECK:      bb1:
  // CHECK-NEXT:   [[NEW_SELF:%.*]] = enum $ValueEnum, #ValueEnum.b!enumelt
  // CHECK-NEXT:   [[SELF_ACCESS:%.*]] = begin_access [modify] [static] [[SELF_BOX]]
  // CHECK-NEXT:   [[NEW_STATE:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK-NEXT:   store [[NEW_SELF]] to [[SELF_ACCESS]]
  // CHECK-NEXT:   end_access [[SELF_ACCESS]]
  // CHECK-NEXT:   br bb3
  // CHECK:      bb2:
  // CHECK-NEXT:   br bb3
  // CHECK:      bb3([[STATE_VALUE:%.*]] : $Builtin.Int1):
  // CHECK-NEXT:   [[NEW_SELF:%.*]] = enum $ValueEnum, #ValueEnum.c!enumelt
  // CHECK-NEXT:   [[SELF_ACCESS:%.*]] = begin_access [modify] [static] [[SELF_BOX]]
  // CHECK-NEXT:   cond_br [[STATE_VALUE]], bb4, bb5
  // CHECK:      bb4:
  // CHECK-NEXT:   destroy_addr [[SELF_BOX]]
  // CHECK-NEXT:   br bb6
  // CHECK:      bb5:
  // CHECK-NEXT:   br bb6
  // CHECK:      bb6:
  // CHECK-NEXT:   store [[NEW_SELF]] to [[SELF_ACCESS]]
  // CHECK-NEXT:   end_access [[SELF_ACCESS]]
  // CHECK-NEXT:   destroy_addr [[SELF_BOX]]
  // CHECK-NEXT:   dealloc_stack [[SELF_BOX]]
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

  // Test control statements with boolean literals, like while-true loops
  init(es: EmptyStruct) {
    while true {
      if cond() {
        ivar = es
        break
      }
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

func cond() -> Bool {
  return true
}


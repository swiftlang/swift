// RUN:%target-swift-frontend -emit-silgen %s -verify  -enable-experimental-feature BorrowAndMutateAccessors -enable-library-evolution | %FileCheck %s

// REQUIRES: swift_feature_BorrowAndMutateAccessors

public class Klass {}

func getKlass() -> Klass {
  return Klass()
}

func use(_ t: Klass) {}

public struct NC : ~Copyable {}

func use(_ t: borrowing NC) {}

public struct S {
  var _k: Klass
  var k: Klass {
    borrow {
      return _k
    }
  }
}

public struct Wrapper {
  var _k: Klass
  var _s: S

  var s: S {
    borrow {
      return _s
    }
  }

  var k: Klass {
    borrow {
      return _k
    }
  }

  var nested1: Klass {
    borrow {
      return _s.k
    }
  }

  var nested2: Klass {
    borrow {
      return k
    }
  }

  subscript(index: Int) -> Klass {
    borrow {
      return _k
    }
  }

  var nested_subscript: Klass {
    borrow {
      return self[0]
    }
  }
}

public struct NCS: ~Copyable {
  var _nc: NC

  var nc: NC {
    borrow {
      return _nc
    }
  }
}

public struct NCWrapper: ~Copyable {
  var _nc: NC
  var _s: NCS

  var nc: NC {
    borrow {
      return _nc
    }
  }
  var nested1: NC {
    borrow {
      return _s.nc
    }
  }

  var nested2: NC {
    borrow {
      return nc
    }
  }

  subscript(index: Int) -> NC {
    borrow {
      return _nc
    }
  }

  var nested_subscript: NC {
    borrow {
      return self[0]
    }
  }
}

func test() {
  let w1 = Wrapper(_k: Klass(), _s: S(_k: Klass()))
  use(w1.k)
  var k1 = w1.k
  use(k1)
  k1 = Klass()
  use(k1)
  use(w1.nested1)
  use(w1.nested2)
}

// CHECK-LABEL: sil hidden [ossa] @$s25borrow_accessor_evolution7WrapperV1sAA1SVvb : $@convention(method) (@in_guaranteed Wrapper) -> @guaranteed_address S {
// CHECK: bb0([[REG0:%.*]] : $*Wrapper):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #Wrapper._s
// CHECK:   return [[REG2]]
// CHECK: }

// CHECK-LABEL: sil hidden [ossa] @$s25borrow_accessor_evolution7WrapperV1kAA5KlassCvb : $@convention(method) (@in_guaranteed Wrapper) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : $*Wrapper):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #Wrapper._k
// CHECK:   [[REG3:%.*]] = load_borrow [[REG2]]
// CHECK:   [[REG4:%.*]] = unchecked_ownership_conversion [[REG3]], @guaranteed to @unowned
// CHECK:   [[REG5:%.*]] = unchecked_ownership_conversion [[REG4]], @unowned to @guaranteed
// CHECK:   return [[REG5]]
// CHECK: }

// CHECK-LABEL: sil hidden [ossa] @$s25borrow_accessor_evolution7WrapperV7nested1AA5KlassCvb : $@convention(method) (@in_guaranteed Wrapper) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : $*Wrapper):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #Wrapper._s
// CHECK:   [[REG3:%.*]] = function_ref @$s25borrow_accessor_evolution1SV1kAA5KlassCvb : $@convention(method) (@in_guaranteed S) -> @guaranteed Klass
// CHECK:   [[REG4:%.*]] = apply [[REG3]]([[REG2]]) : $@convention(method) (@in_guaranteed S) -> @guaranteed Klass
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK-LABEL: sil hidden [ossa] @$s25borrow_accessor_evolution7WrapperV7nested2AA5KlassCvb : $@convention(method) (@in_guaranteed Wrapper) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : $*Wrapper):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = function_ref @$s25borrow_accessor_evolution7WrapperV1kAA5KlassCvb : $@convention(method) (@in_guaranteed Wrapper) -> @guaranteed Klass
// CHECK:   [[REG3:%.*]] = apply [[REG2]]([[REG0]]) : $@convention(method) (@in_guaranteed Wrapper) -> @guaranteed Klass
// CHECK:   return [[REG3]]
// CHECK: }

// CHECK-LABEL: sil hidden [ossa] @$s25borrow_accessor_evolution7WrapperVyAA5KlassCSicib : $@convention(method) (Int, @in_guaranteed Wrapper) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : $*Wrapper):
// CHECK:   debug_value [[REG0]], let, name "index", argno 1
// CHECK:   debug_value [[REG1]], let, name "self", argno 2, expr op_deref
// CHECK:   [[REG4:%.*]] = struct_element_addr [[REG1]], #Wrapper._k
// CHECK:   [[REG5:%.*]] = load_borrow [[REG4]]
// CHECK:   [[REG6:%.*]] = unchecked_ownership_conversion [[REG5]], @guaranteed to @unowned
// CHECK:   [[REG7:%.*]] = unchecked_ownership_conversion [[REG6]], @unowned to @guaranteed
// CHECK:   end_borrow [[REG5]]
// CHECK:   return [[REG7]]
// CHECK: }

// CHECK-LABEL: sil hidden [ossa] @$s25borrow_accessor_evolution7WrapperV16nested_subscriptAA5KlassCvb : $@convention(method) (@in_guaranteed Wrapper) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : $*Wrapper):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = integer_literal $Builtin.IntLiteral, 0
// CHECK:   [[REG3:%.*]] = metatype $@thin Int.Type
// CHECK:   [[REG4:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:   [[REG5:%.*]] = apply [[REG4]]([[REG2]], [[REG3]]) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:   [[REG6:%.*]] = function_ref @$s25borrow_accessor_evolution7WrapperVyAA5KlassCSicib : $@convention(method) (Int, @in_guaranteed Wrapper) -> @guaranteed Klass
// CHECK:   [[REG7:%.*]] = apply [[REG6]]([[REG5]], [[REG0]]) : $@convention(method) (Int, @in_guaranteed Wrapper) -> @guaranteed Klass
// CHECK:   return [[REG7]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s25borrow_accessor_evolution3NCSV2ncAA2NCVvb : $@convention(method) (@in_guaranteed NCS) -> @guaranteed_address NC {
// CHECK: bb0([[REG0:%.*]] : $*NCS):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = struct_element_addr [[REG2]], #NCS._nc
// CHECK:   return [[REG3]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s25borrow_accessor_evolution9NCWrapperV2ncAA2NCVvb : $@convention(method) (@in_guaranteed NCWrapper) -> @guaranteed_address NC {
// CHECK: bb0([[REG0:%.*]] : $*NCWrapper):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = struct_element_addr [[REG2]], #NCWrapper._nc
// CHECK:   return [[REG3]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s25borrow_accessor_evolution9NCWrapperV7nested1AA2NCVvb : $@convention(method) (@in_guaranteed NCWrapper) -> @guaranteed_address NC {
// CHECK: bb0([[REG0:%.*]] : $*NCWrapper):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = struct_element_addr [[REG2]], #NCWrapper._s
// CHECK:   [[REG4:%.*]] = function_ref @$s25borrow_accessor_evolution3NCSV2ncAA2NCVvb : $@convention(method) (@in_guaranteed NCS) -> @guaranteed_address NC
// CHECK:   [[REG5:%.*]] = apply [[REG4]]([[REG3]]) : $@convention(method) (@in_guaranteed NCS) -> @guaranteed_address NC
// CHECK:   [[REG6:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG5]]
// CHECK:   return [[REG6]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s25borrow_accessor_evolution9NCWrapperV7nested2AA2NCVvb : $@convention(method) (@in_guaranteed NCWrapper) -> @guaranteed_address NC {
// CHECK: bb0([[REG0:%.*]] : $*NCWrapper):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = function_ref @$s25borrow_accessor_evolution9NCWrapperV2ncAA2NCVvb : $@convention(method) (@in_guaranteed NCWrapper) -> @guaranteed_address NC
// CHECK:   [[REG4:%.*]] = apply [[REG3]]([[REG2]]) : $@convention(method) (@in_guaranteed NCWrapper) -> @guaranteed_address NC
// CHECK:   [[REG5:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG4]]
// CHECK:   return [[REG5]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s25borrow_accessor_evolution9NCWrapperVyAA2NCVSicib : $@convention(method) (Int, @in_guaranteed NCWrapper) -> @guaranteed_address NC {
// CHECK: bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : $*NCWrapper):
// CHECK:   debug_value [[REG0]], let, name "index", argno 1
// CHECK:   debug_value [[REG1]], let, name "self", argno 2, expr op_deref
// CHECK:   [[REG4:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG1]]
// CHECK:   [[REG5:%.*]] = struct_element_addr [[REG4]], #NCWrapper._nc
// CHECK:   return [[REG5]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s25borrow_accessor_evolution9NCWrapperV16nested_subscriptAA2NCVvb : $@convention(method) (@in_guaranteed NCWrapper) -> @guaranteed_address NC {
// CHECK: bb0([[REG0:%.*]] : $*NCWrapper):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = integer_literal $Builtin.IntLiteral, 0
// CHECK:   [[REG4:%.*]] = metatype $@thin Int.Type
// CHECK:   [[REG5:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:   [[REG6:%.*]] = apply [[REG5]]([[REG3]], [[REG4]]) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:   [[REG7:%.*]] = function_ref @$s25borrow_accessor_evolution9NCWrapperVyAA2NCVSicib : $@convention(method) (Int, @in_guaranteed NCWrapper) -> @guaranteed_address NC
// CHECK:   [[REG8:%.*]] = apply [[REG7]]([[REG6]], [[REG2]]) : $@convention(method) (Int, @in_guaranteed NCWrapper) -> @guaranteed_address NC
// CHECK:   [[REG9:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG8]]
// CHECK:   return [[REG9]]
// CHECK: }


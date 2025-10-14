// RUN:%target-swift-frontend -emit-silgen %s -enable-experimental-feature BorrowAndMutateAccessors | %FileCheck %s

// REQUIRES: swift_feature_BorrowAndMutateAccessors

public final class Klass {}

public struct NC : ~Copyable {}

func use(_ t: Klass) {}

func use(_ t: borrowing NC) {}

public struct S {
  var _k: Klass

  var k: Klass {
    borrow {
      return _k
    }
    mutate {
      return &_k
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
    mutate {
      return &_s
    }
  }

  var k: Klass {
    borrow {
      return _k
    }
    mutate {
      return &_k
    }
  }

  var nested_borrow1: Klass {
    borrow {
      return _s.k
    }
    mutate {
      return &s._k
    }
  }

  var nested_borrow2: Klass {
    borrow {
      return k
    }
    mutate {
      return &_k
    }
  }

  subscript(index: Int) -> Klass {
    borrow {
      return _k
    }
    mutate {
      return &_k
    }
  }

  var nested_subscript: Klass {
    borrow {
      return self[0]
    }
    mutate {
      return &self[0]
    }
  }

  var literal: Int {
    borrow {
      return 0
    }
  }
}

public struct SimpleWrapper<T> {
  var _prop: T

  var prop: T {
    borrow {
      return _prop
    }
    mutate {
      return &_prop
    }
  }
}

public struct GenWrapper<T> {
  var _prop: T
  var _w: SimpleWrapper<T>
  var _k: Klass
  var _s: S

  public var prop: T {
    borrow {
      return _prop
    }
    mutate {
      return &_prop
    }
  }

  var s: S {
    borrow {
      return _s
    }
    mutate {
      return &_s
    }
  }

  var k: Klass {
    borrow {
      return _k
    }
    mutate {
      return &_k
    }
  }

  var nested_prop1: T {
    borrow {
      return _w.prop
    }
    mutate {
      return &_w.prop
    }
  }

  var nested_prop2: T {
    borrow {
      return prop
    }
    mutate {
      return &prop
    }
  }

  var nested_k1: Klass {
    borrow {
      return _s.k
    }
    mutate {
      return &_s.k
    }
  }

  var nested_k2: Klass {
    borrow {
      return s.k
    }
    mutate {
      return &s.k
    }
  }

  subscript(index: Int) -> T {
    borrow {
      return _prop
    }
    mutate {
      return &_prop
    }
  }

  var nested_subscript: T {
    borrow {
      return self[0]
    }
    mutate {
      return &self[0]
    }
  }

  var literal: Int {
    borrow {
      return 0
    }
  }
}

public struct NCS: ~Copyable {
  var _nc: NC

  var nc: NC {
    borrow {
      return _nc
    }
    mutate {
      return &_nc
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
    mutate {
      return &_nc
    }
  }

  var nested1: NC {
    borrow {
      return _s.nc
    }
    mutate {
      return &_s.nc
    }
  }

  var nested2: NC {
    borrow {
      return nc
    }
    mutate {
      return &nc
    }
  }

  subscript(index: Int) -> NC {
    borrow {
      return _nc
    }
    mutate {
      return &_nc
    }
  }

  var nested_subscript: NC {
    borrow {
      return self[0]
    }
    mutate {
      return &self[0]
    }
  }

  var literal: Int {
    borrow {
      return 0
    }
  }
}

public struct SimpleNCWrapper<T : ~Copyable> : ~Copyable {
  var _prop: T

  var prop: T {
    borrow {
      return _prop
    }
    mutate {
      return &_prop
    }
  }
}

public struct GenNCWrapper<T : ~Copyable> : ~Copyable {
  var _prop: T
  var _w: SimpleNCWrapper<T>
  var _nc: NC
  var _ncw: NCWrapper

  public var prop: T {
    borrow {
      return _prop
    }
    mutate {
      return &_prop
    }
  }

  var nc: NC {
    borrow {
      return _nc
    }
    mutate {
      return &_nc
    }
  }

  var ncw: NCWrapper {
    borrow {
      return _ncw
    }
    mutate {
      return &_ncw
    }
  }

  var nested_prop1: T {
    borrow {
      return _w.prop
    }
    mutate {
      return &_w.prop
    }
  }

  var nested_prop2: T {
    borrow {
      return prop
    }
    mutate {
      return &prop
    }
  }

  var nested_nc1: NC {
    borrow {
      return _ncw.nc
    }
    mutate {
      return &_ncw.nc
    }
  }

  var nested_nc2: NC {
    borrow {
      return ncw.nc
    }
    mutate {
      return &ncw.nc
    }
  }

  subscript(index: Int) -> T {
    borrow {
      return _prop
    }
    mutate {
      return &_prop
    }
  }

  var nested_subscript: T {
    borrow {
      return self[0]
    }
    mutate {
      return &self[0]
    }
  }

  var literal: Int {
    borrow {
      return 0
    }
  }
}

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperV1sAA1SVvb : $@convention(method) (@guaranteed Wrapper) -> @guaranteed S {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $Wrapper):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1
// CHECK:   [[REG2:%.*]] = struct_extract [[REG0]], #Wrapper._s
// CHECK:   return [[REG2]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperV1sAA1SVvz : $@convention(method) (@inout Wrapper) -> @guaranteed_address S {
// CHECK:bb0([[REG0:%.*]] : $*Wrapper):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = struct_element_addr [[REG0]], #Wrapper._s
// CHECK:  return [[REG2]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperV1kAA5KlassCvb : $@convention(method) (@guaranteed Wrapper) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $Wrapper):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1
// CHECK:   [[REG2:%.*]] = struct_extract [[REG0]], #Wrapper._k
// CHECK:   return [[REG2]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperV1kAA5KlassCvz : $@convention(method) (@inout Wrapper) -> @guaranteed_address Klass {
// CHECK:bb0([[REG0:%.*]] : $*Wrapper):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = struct_element_addr [[REG0]], #Wrapper._k
// CHECK:  return [[REG2]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperV14nested_borrow1AA5KlassCvb : $@convention(method) (@guaranteed Wrapper) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $Wrapper):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1
// CHECK:   [[REG2:%.*]] = struct_extract [[REG0]], #Wrapper._s
// CHECK:   [[REG3:%.*]] = function_ref @$s15borrow_accessor1SV1kAA5KlassCvb : $@convention(method) (@guaranteed S) -> @guaranteed Klass
// CHECK:   [[REG4:%.*]] = apply [[REG3]]([[REG2]]) : $@convention(method) (@guaranteed S) -> @guaranteed Klass
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperV14nested_borrow1AA5KlassCvz : $@convention(method) (@inout Wrapper) -> @guaranteed_address Klass {
// CHECK:bb0([[REG0:%.*]] : $*Wrapper):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = function_ref @$s15borrow_accessor7WrapperV1sAA1SVvz : $@convention(method) (@inout Wrapper) -> @guaranteed_address S
// CHECK:  [[REG3:%.*]] = apply [[REG2]]([[REG0]]) : $@convention(method) (@inout Wrapper) -> @guaranteed_address S
// CHECK:  [[REG4:%.*]] = struct_element_addr [[REG3]], #S._k
// CHECK:  return [[REG4]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperV14nested_borrow2AA5KlassCvb : $@convention(method) (@guaranteed Wrapper) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $Wrapper):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1
// CHECK:   [[REG2:%.*]] = function_ref @$s15borrow_accessor7WrapperV1kAA5KlassCvb : $@convention(method) (@guaranteed Wrapper) -> @guaranteed Klass
// CHECK:   [[REG3:%.*]] = apply [[REG2]]([[REG0]]) : $@convention(method) (@guaranteed Wrapper) -> @guaranteed Klass
// CHECK:   return [[REG3]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperV14nested_borrow2AA5KlassCvz : $@convention(method) (@inout Wrapper) -> @guaranteed_address Klass {
// CHECK:bb0([[REG0:%.*]] : $*Wrapper):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = struct_element_addr [[REG0]], #Wrapper._k
// CHECK:  return [[REG2]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperVyAA5KlassCSicib : $@convention(method) (Int, @guaranteed Wrapper) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : @guaranteed $Wrapper):
// CHECK:   debug_value [[REG0]], let, name "index", argno 1
// CHECK:   debug_value [[REG1]], let, name "self", argno 2
// CHECK:   [[REG4:%.*]] = struct_extract [[REG1]], #Wrapper._k
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperVyAA5KlassCSiciz : $@convention(method) (Int, @inout Wrapper) -> @guaranteed_address Klass {
// CHECK:bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : $*Wrapper):
// CHECK:  debug_value [[REG0]], let, name "index", argno 1
// CHECK:  debug_value [[REG1]], var, name "self", argno 2, expr op_deref
// CHECK:  [[REG4:%.*]] = struct_element_addr [[REG1]], #Wrapper._k
// CHECK:  return [[REG4]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperV16nested_subscriptAA5KlassCvb : $@convention(method) (@guaranteed Wrapper) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $Wrapper):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1
// CHECK:   [[REG2:%.*]] = integer_literal $Builtin.IntLiteral, 0
// CHECK:   [[REG3:%.*]] = metatype $@thin Int.Type
// CHECK:   [[REG4:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:   [[REG5:%.*]] = apply [[REG4]]([[REG2]], [[REG3]]) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:   [[REG6:%.*]] = function_ref @$s15borrow_accessor7WrapperVyAA5KlassCSicib : $@convention(method) (Int, @guaranteed Wrapper) -> @guaranteed Klass
// CHECK:   [[REG7:%.*]] = apply [[REG6]]([[REG5]], [[REG0]]) : $@convention(method) (Int, @guaranteed Wrapper) -> @guaranteed Klass
// CHECK:   return [[REG7]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperV16nested_subscriptAA5KlassCvz : $@convention(method) (@inout Wrapper) -> @guaranteed_address Klass {
// CHECK:bb0([[REG0:%.*]] : $*Wrapper):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = integer_literal $Builtin.IntLiteral, 0
// CHECK:  [[REG3:%.*]] = metatype $@thin Int.Type
// CHECK:  [[REG4:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:  [[REG5:%.*]] = apply [[REG4]]([[REG2]], [[REG3]]) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:  [[REG6:%.*]] = function_ref @$s15borrow_accessor7WrapperVyAA5KlassCSiciz : $@convention(method) (Int, @inout Wrapper) -> @guaranteed_address Klass
// CHECK:  [[REG7:%.*]] = apply [[REG6]]([[REG5]], [[REG0]]) : $@convention(method) (Int, @inout Wrapper) -> @guaranteed_address Klass
// CHECK:  return [[REG7]]
// CHECK: }

// CHECK: sil [ossa] @$s15borrow_accessor10GenWrapperV4propxvb : $@convention(method) <T> (@in_guaranteed GenWrapper<T>) -> @guaranteed_address T {
// CHECK: bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #GenWrapper._prop
// CHECK:   return [[REG2]]
// CHECK: }

// CHECK: sil [ossa] @$s15borrow_accessor10GenWrapperV4propxvz : $@convention(method) <T> (@inout GenWrapper<T>) -> @guaranteed_address T {
// CHECK:bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = struct_element_addr [[REG0]], #GenWrapper._prop
// CHECK:  return [[REG2]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperV1sAA1SVvb : $@convention(method) <T> (@in_guaranteed GenWrapper<T>) -> @guaranteed S {
// CHECK: bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #GenWrapper._s
// CHECK:   [[REG3:%.*]] = load_borrow [[REG2]]
// CHECK:   [[REG4:%.*]] = unchecked_ownership_conversion [[REG3]], @guaranteed to @unowned
// CHECK:   [[REG5:%.*]] = unchecked_ownership_conversion [[REG4]], @unowned to @guaranteed
// CHECK:   end_borrow [[REG3]]
// CHECK:   return [[REG5]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperV1sAA1SVvz : $@convention(method) <T> (@inout GenWrapper<T>) -> @guaranteed_address S {
// CHECK:bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = struct_element_addr [[REG0]], #GenWrapper._s
// CHECK:  return [[REG2]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperV1kAA5KlassCvb : $@convention(method) <T> (@in_guaranteed GenWrapper<T>) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #GenWrapper._k
// CHECK:   [[REG3:%.*]] = load_borrow [[REG2]]
// CHECK:   [[REG4:%.*]] = unchecked_ownership_conversion [[REG3]], @guaranteed to @unowned
// CHECK:   [[REG5:%.*]] = unchecked_ownership_conversion [[REG4]], @unowned to @guaranteed
// CHECK:   end_borrow [[REG3]]
// CHECK:   return [[REG5]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperV1kAA5KlassCvz : $@convention(method) <T> (@inout GenWrapper<T>) -> @guaranteed_address Klass {
// CHECK:bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = struct_element_addr [[REG0]], #GenWrapper._k
// CHECK:  return [[REG2]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperV12nested_prop1xvb : $@convention(method) <T> (@in_guaranteed GenWrapper<T>) -> @guaranteed_address T {
// CHECK: bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #GenWrapper._w
// CHECK:   [[REG3:%.*]] = function_ref @$s15borrow_accessor13SimpleWrapperV4propxvb : $@convention(method) <τ_0_0> (@in_guaranteed SimpleWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[REG4:%.*]] = apply [[REG3]]<T>([[REG2]]) : $@convention(method) <τ_0_0> (@in_guaranteed SimpleWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperV12nested_prop1xvz : $@convention(method) <T> (@inout GenWrapper<T>) -> @guaranteed_address T {
// CHECK:bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = struct_element_addr [[REG0]], #GenWrapper._w
// CHECK:  [[REG3:%.*]] = function_ref @$s15borrow_accessor13SimpleWrapperV4propxvz : $@convention(method) <τ_0_0> (@inout SimpleWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:  [[REG4:%.*]] = apply [[REG3]]<T>([[REG2]]) : $@convention(method) <τ_0_0> (@inout SimpleWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:  return [[REG4]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperV12nested_prop2xvb : $@convention(method) <T> (@in_guaranteed GenWrapper<T>) -> @guaranteed_address T {
// CHECK: bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = function_ref @$s15borrow_accessor10GenWrapperV4propxvb : $@convention(method) <τ_0_0> (@in_guaranteed GenWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[REG3:%.*]] = apply [[REG2]]<T>([[REG0]]) : $@convention(method) <τ_0_0> (@in_guaranteed GenWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   return [[REG3]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperV9nested_k1AA5KlassCvb : $@convention(method) <T> (@in_guaranteed GenWrapper<T>) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #GenWrapper._s
// CHECK:   [[REG3:%.*]] = load_borrow [[REG2]]
// CHECK:   [[REG4:%.*]] = function_ref @$s15borrow_accessor1SV1kAA5KlassCvb : $@convention(method) (@guaranteed S) -> @guaranteed Klass
// CHECK:   [[REG5:%.*]] = apply [[REG4]]([[REG3]]) : $@convention(method) (@guaranteed S) -> @guaranteed Klass
// CHECK:   [[REG6:%.*]] = unchecked_ownership_conversion [[REG5]], @guaranteed to @unowned
// CHECK:   [[REG7:%.*]] = unchecked_ownership_conversion [[REG6]], @unowned to @guaranteed
// CHECK:   end_borrow [[REG3]]
// CHECK:   return [[REG7]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperV9nested_k1AA5KlassCvz : $@convention(method) <T> (@inout GenWrapper<T>) -> @guaranteed_address Klass {
// CHECK:bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = struct_element_addr [[REG0]], #GenWrapper._s
// CHECK:  [[REG3:%.*]] = function_ref @$s15borrow_accessor1SV1kAA5KlassCvz : $@convention(method) (@inout S) -> @guaranteed_address Klass
// CHECK:  [[REG4:%.*]] = apply [[REG3]]([[REG2]]) : $@convention(method) (@inout S) -> @guaranteed_address Klass
// CHECK:  return [[REG4]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperV9nested_k2AA5KlassCvb : $@convention(method) <T> (@in_guaranteed GenWrapper<T>) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = function_ref @$s15borrow_accessor10GenWrapperV1sAA1SVvb : $@convention(method) <τ_0_0> (@in_guaranteed GenWrapper<τ_0_0>) -> @guaranteed S
// CHECK:   [[REG3:%.*]] = apply [[REG2]]<T>([[REG0]]) : $@convention(method) <τ_0_0> (@in_guaranteed GenWrapper<τ_0_0>) -> @guaranteed S
// CHECK:   [[REG4:%.*]] = function_ref @$s15borrow_accessor1SV1kAA5KlassCvb : $@convention(method) (@guaranteed S) -> @guaranteed Klass
// CHECK:   [[REG5:%.*]] = apply [[REG4]]([[REG3]]) : $@convention(method) (@guaranteed S) -> @guaranteed Klass
// CHECK:   return [[REG5]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperV9nested_k2AA5KlassCvz : $@convention(method) <T> (@inout GenWrapper<T>) -> @guaranteed_address Klass {
// CHECK:bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = function_ref @$s15borrow_accessor10GenWrapperV1sAA1SVvz : $@convention(method) <τ_0_0> (@inout GenWrapper<τ_0_0>) -> @guaranteed_address S
// CHECK:  [[REG3:%.*]] = apply [[REG2]]<T>([[REG0]]) : $@convention(method) <τ_0_0> (@inout GenWrapper<τ_0_0>) -> @guaranteed_address S
// CHECK:  [[REG4:%.*]] = function_ref @$s15borrow_accessor1SV1kAA5KlassCvz : $@convention(method) (@inout S) -> @guaranteed_address Klass
// CHECK:  [[REG5:%.*]] = apply [[REG4]]([[REG3]]) : $@convention(method) (@inout S) -> @guaranteed_address Klass
// CHECK:  return [[REG5]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperVyxSicib : $@convention(method) <T> (Int, @in_guaranteed GenWrapper<T>) -> @guaranteed_address T {
// CHECK: bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : $*GenWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "index", argno 1
// CHECK:   debug_value [[REG1]], let, name "self", argno 2, expr op_deref
// CHECK:   [[REG4:%.*]] = struct_element_addr [[REG1]], #GenWrapper._prop
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperVyxSiciz : $@convention(method) <T> (Int, @inout GenWrapper<T>) -> @guaranteed_address T {
// CHECK:bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : $*GenWrapper<T>):
// CHECK:  debug_value [[REG0]], let, name "index", argno 1
// CHECK:  debug_value [[REG1]], var, name "self", argno 2, expr op_deref
// CHECK:  [[REG4:%.*]] = struct_element_addr [[REG1]], #GenWrapper._prop
// CHECK:  return [[REG4]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperV16nested_subscriptxvb : $@convention(method) <T> (@in_guaranteed GenWrapper<T>) -> @guaranteed_address T {
// CHECK: bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = integer_literal $Builtin.IntLiteral, 0
// CHECK:   [[REG3:%.*]] = metatype $@thin Int.Type
// CHECK:   [[REG4:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:   [[REG5:%.*]] = apply [[REG4]]([[REG2]], [[REG3]]) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:   [[REG6:%.*]] = function_ref @$s15borrow_accessor10GenWrapperVyxSicib : $@convention(method) <τ_0_0> (Int, @in_guaranteed GenWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[REG7:%.*]] = apply [[REG6]]<T>([[REG5]], [[REG0]]) : $@convention(method) <τ_0_0> (Int, @in_guaranteed GenWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   return [[REG7]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperV16nested_subscriptxvz : $@convention(method) <T> (@inout GenWrapper<T>) -> @guaranteed_address T {
// CHECK:bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = integer_literal $Builtin.IntLiteral, 0
// CHECK:  [[REG3:%.*]] = metatype $@thin Int.Type
// CHECK:  [[REG4:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:  [[REG5:%.*]] = apply [[REG4]]([[REG2]], [[REG3]]) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:  [[REG6:%.*]] = function_ref @$s15borrow_accessor10GenWrapperVyxSiciz : $@convention(method) <τ_0_0> (Int, @inout GenWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:  [[REG7:%.*]] = apply [[REG6]]<T>([[REG5]], [[REG0]]) : $@convention(method) <τ_0_0> (Int, @inout GenWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:  return [[REG7]]
// CHECK: }

// CHECK: sil [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlE4propxvb : $@convention(method) <T where T : ~Copyable> (@in_guaranteed GenNCWrapper<T>) -> @guaranteed_address T {
// CHECK: bb0([[REG0:%.*]] : $*GenNCWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = struct_element_addr [[REG2]], #GenNCWrapper._prop
// CHECK:   return [[REG3]]
// CHECK: }

// CHECK: sil [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlE4propxvz : $@convention(method) <T where T : ~Copyable> (@inout GenNCWrapper<T>) -> @guaranteed_address T {
// CHECK:bb0([[REG0:%.*]] : $*GenNCWrapper<T>):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG0]]
// CHECK:  [[REG3:%.*]] = struct_element_addr [[REG2]], #GenNCWrapper._prop
// CHECK:  return [[REG3]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlE2ncAA2NCVvb : $@convention(method) <T where T : ~Copyable> (@in_guaranteed GenNCWrapper<T>) -> @guaranteed NC {
// CHECK: bb0([[REG0:%.*]] : $*GenNCWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = struct_element_addr [[REG2]], #GenNCWrapper._nc
// CHECK:   [[REG4:%.*]] = load_borrow [[REG3]]
// CHECK:   [[REG5:%.*]] = unchecked_ownership_conversion [[REG4]], @guaranteed to @unowned
// CHECK:   [[REG6:%.*]] = unchecked_ownership_conversion [[REG5]], @unowned to @guaranteed
// CHECK:   end_borrow [[REG4]]
// CHECK:   return [[REG6]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlE2ncAA2NCVvz : $@convention(method) <T where T : ~Copyable> (@inout GenNCWrapper<T>) -> @guaranteed_address NC {
// CHECK:bb0([[REG0:%.*]] : $*GenNCWrapper<T>):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG0]]
// CHECK:  [[REG3:%.*]] = struct_element_addr [[REG2]], #GenNCWrapper._nc
// CHECK:  return [[REG3]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlE3ncwAA0D0Vvb : $@convention(method) <T where T : ~Copyable> (@in_guaranteed GenNCWrapper<T>) -> @guaranteed NCWrapper {
// CHECK: bb0([[REG0:%.*]] : $*GenNCWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = struct_element_addr [[REG2]], #GenNCWrapper._ncw
// CHECK:   [[REG4:%.*]] = load_borrow [[REG3]]
// CHECK:   [[REG5:%.*]] = unchecked_ownership_conversion [[REG4]], @guaranteed to @unowned
// CHECK:   [[REG6:%.*]] = unchecked_ownership_conversion [[REG5]], @unowned to @guaranteed
// CHECK:   end_borrow [[REG4]]
// CHECK:   return [[REG6]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlE3ncwAA0D0Vvz : $@convention(method) <T where T : ~Copyable> (@inout GenNCWrapper<T>) -> @guaranteed_address NCWrapper {
// CHECK:bb0([[REG0:%.*]] : $*GenNCWrapper<T>):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG0]]
// CHECK:  [[REG3:%.*]] = struct_element_addr [[REG2]], #GenNCWrapper._ncw
// CHECK:  return [[REG3]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlE12nested_prop1xvb : $@convention(method) <T where T : ~Copyable> (@in_guaranteed GenNCWrapper<T>) -> @guaranteed_address T {
// CHECK: bb0([[REG0:%.*]] : $*GenNCWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = struct_element_addr [[REG2]], #GenNCWrapper._w
// CHECK:   [[REG4:%.*]] = function_ref @$s15borrow_accessor15SimpleNCWrapperVAARi_zrlE4propxvb : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed SimpleNCWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[REG5:%.*]] = apply [[REG4]]<T>([[REG3]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed SimpleNCWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[REG6:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG5]]
// CHECK:   return [[REG6]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlE12nested_prop1xvz : $@convention(method) <T where T : ~Copyable> (@inout GenNCWrapper<T>) -> @guaranteed_address T {
// CHECK:bb0([[REG0:%.*]] : $*GenNCWrapper<T>):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG0]]
// CHECK:  [[REG3:%.*]] = struct_element_addr [[REG2]], #GenNCWrapper._w
// CHECK:  [[REG4:%.*]] = function_ref @$s15borrow_accessor15SimpleNCWrapperVAARi_zrlE4propxvz : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@inout SimpleNCWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:  [[REG5:%.*]] = apply [[REG4]]<T>([[REG3]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@inout SimpleNCWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:  [[REG6:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG5]]
// CHECK:  return [[REG6]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlE12nested_prop2xvb : $@convention(method) <T where T : ~Copyable> (@in_guaranteed GenNCWrapper<T>) -> @guaranteed_address T {
// CHECK:bb0([[REG0:%.*]] : $*GenNCWrapper<T>):
// CHECK:  debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:  [[REG3:%.*]] = function_ref @$s15borrow_accessor12GenNCWrapperVAARi_zrlE4propxvb : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed GenNCWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:  [[REG4:%.*]] = apply [[REG3]]<T>([[REG2]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed GenNCWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:  [[REG5:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG4]]
// CHECK:  return [[REG5]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlE12nested_prop2xvz : $@convention(method) <T where T : ~Copyable> (@inout GenNCWrapper<T>) -> @guaranteed_address T {
// CHECK:bb0([[REG0:%.*]] : $*GenNCWrapper<T>):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG0]]
// CHECK:  [[REG3:%.*]] = function_ref @$s15borrow_accessor12GenNCWrapperVAARi_zrlE4propxvz : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@inout GenNCWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:  [[REG4:%.*]] = apply [[REG3]]<T>([[REG2]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@inout GenNCWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:  [[REG5:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG4]]
// CHECK:  return [[REG5]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlE10nested_nc1AA2NCVvb : $@convention(method) <T where T : ~Copyable> (@in_guaranteed GenNCWrapper<T>) -> @guaranteed NC {
// CHECK: bb0([[REG0:%.*]] : $*GenNCWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = struct_element_addr [[REG2]], #GenNCWrapper._ncw
// CHECK:   [[REG4:%.*]] = load_borrow [[REG3]]
// CHECK:   [[REG5:%.*]] = function_ref @$s15borrow_accessor9NCWrapperV2ncAA2NCVvb : $@convention(method) (@guaranteed NCWrapper) -> @guaranteed NC
// CHECK:   [[REG6:%.*]] = apply [[REG5]]([[REG4]]) : $@convention(method) (@guaranteed NCWrapper) -> @guaranteed NC
// CHECK:   [[REG7:%.*]] = unchecked_ownership_conversion [[REG6]], @guaranteed to @unowned
// CHECK:   [[REG8:%.*]] = unchecked_ownership_conversion [[REG7]], @unowned to @guaranteed
// CHECK:   [[REG9:%.*]] = copy_value [[REG8]]
// CHECK:   [[REG10:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG9]]
// CHECK:   [[REG11:%.*]] = begin_borrow [[REG10]]
// CHECK:   end_borrow [[REG4]]
// CHECK:   end_borrow [[REG11]]
// CHECK:   destroy_value [[REG10]]
// CHECK:   return [[REG8]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlE10nested_nc1AA2NCVvz : $@convention(method) <T where T : ~Copyable> (@inout GenNCWrapper<T>) -> @guaranteed_address NC {
// CHECK:bb0([[REG0:%.*]] : $*GenNCWrapper<T>):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG0]]
// CHECK:  [[REG3:%.*]] = struct_element_addr [[REG2]], #GenNCWrapper._ncw
// CHECK:  [[REG4:%.*]] = function_ref @$s15borrow_accessor9NCWrapperV2ncAA2NCVvz : $@convention(method) (@inout NCWrapper) -> @guaranteed_address NC
// CHECK:  [[REG5:%.*]] = apply [[REG4]]([[REG3]]) : $@convention(method) (@inout NCWrapper) -> @guaranteed_address NC
// CHECK:  [[REG6:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG5]]
// CHECK:  return [[REG6]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlE10nested_nc2AA2NCVvb : $@convention(method) <T where T : ~Copyable> (@in_guaranteed GenNCWrapper<T>) -> @guaranteed NC {
// CHECK: bb0([[REG0:%.*]] : $*GenNCWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = function_ref @$s15borrow_accessor12GenNCWrapperVAARi_zrlE3ncwAA0D0Vvb : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed GenNCWrapper<τ_0_0>) -> @guaranteed NCWrapper
// CHECK:   [[REG4:%.*]] = apply [[REG3]]<T>([[REG2]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed GenNCWrapper<τ_0_0>) -> @guaranteed NCWrapper
// CHECK:   [[REG5:%.*]] = copy_value [[REG4]]
// CHECK:   [[REG6:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG5]]
// CHECK:   [[REG7:%.*]] = begin_borrow [[REG6]]
// CHECK:   [[REG8:%.*]] = function_ref @$s15borrow_accessor9NCWrapperV2ncAA2NCVvb : $@convention(method) (@guaranteed NCWrapper) -> @guaranteed NC
// CHECK:   [[REG9:%.*]] = apply [[REG8]]([[REG4]]) : $@convention(method) (@guaranteed NCWrapper) -> @guaranteed NC
// CHECK:   [[REG10:%.*]] = copy_value [[REG9]]
// CHECK:   [[REG11:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG10]]
// CHECK:   [[REG12:%.*]] = begin_borrow [[REG11]]
// CHECK:   end_borrow [[REG12]]
// CHECK:   destroy_value [[REG11]]
// CHECK:   end_borrow [[REG7]]
// CHECK:   destroy_value [[REG6]]
// CHECK:   return [[REG9]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlE10nested_nc2AA2NCVvz : $@convention(method) <T where T : ~Copyable> (@inout GenNCWrapper<T>) -> @guaranteed_address NC {
// CHECK:bb0([[REG0:%.*]] : $*GenNCWrapper<T>):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG0]]
// CHECK:  [[REG3:%.*]] = function_ref @$s15borrow_accessor12GenNCWrapperVAARi_zrlE3ncwAA0D0Vvz : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@inout GenNCWrapper<τ_0_0>) -> @guaranteed_address NCWrapper
// CHECK:  [[REG4:%.*]] = apply [[REG3]]<T>([[REG2]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@inout GenNCWrapper<τ_0_0>) -> @guaranteed_address NCWrapper
// CHECK:  [[REG5:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG4]]
// CHECK:  [[REG6:%.*]] = function_ref @$s15borrow_accessor9NCWrapperV2ncAA2NCVvz : $@convention(method) (@inout NCWrapper) -> @guaranteed_address NC
// CHECK:  [[REG7:%.*]] = apply [[REG6]]([[REG5]]) : $@convention(method) (@inout NCWrapper) -> @guaranteed_address NC
// CHECK:  [[REG8:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG7]]
// CHECK:  return [[REG8]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlEyxSicib : $@convention(method) <T where T : ~Copyable> (Int, @in_guaranteed GenNCWrapper<T>) -> @guaranteed_address T {
// CHECK: bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : $*GenNCWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "index", argno 1
// CHECK:   debug_value [[REG1]], let, name "self", argno 2, expr op_deref
// CHECK:   [[REG4:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG1]]
// CHECK:   [[REG5:%.*]] = struct_element_addr [[REG4]], #GenNCWrapper._prop
// CHECK:   return [[REG5]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlEyxSiciz : $@convention(method) <T where T : ~Copyable> (Int, @inout GenNCWrapper<T>) -> @guaranteed_address T {
// CHECK:bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : $*GenNCWrapper<T>):
// CHECK:  debug_value [[REG0]], let, name "index", argno 1
// CHECK:  debug_value [[REG1]], var, name "self", argno 2, expr op_deref
// CHECK:  [[REG4:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG1]]
// CHECK:  [[REG5:%.*]] = struct_element_addr [[REG4]], #GenNCWrapper._prop
// CHECK:  return [[REG5]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlE16nested_subscriptxvb : $@convention(method) <T where T : ~Copyable> (@in_guaranteed GenNCWrapper<T>) -> @guaranteed_address T {
// CHECK: bb0([[REG0:%.*]] : $*GenNCWrapper<T>):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = integer_literal $Builtin.IntLiteral, 0
// CHECK:   [[REG4:%.*]] = metatype $@thin Int.Type
// CHECK:   [[REG5:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:   [[REG6:%.*]] = apply [[REG5]]([[REG3]], [[REG4]]) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:   [[REG7:%.*]] = function_ref @$s15borrow_accessor12GenNCWrapperVAARi_zrlEyxSicib : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @in_guaranteed GenNCWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[REG8:%.*]] = apply [[REG7]]<T>([[REG6]], [[REG2]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @in_guaranteed GenNCWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[REG9:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG8]]
// CHECK:   return [[REG9]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s15borrow_accessor12GenNCWrapperVAARi_zrlE16nested_subscriptxvz : $@convention(method) <T where T : ~Copyable> (@inout GenNCWrapper<T>) -> @guaranteed_address T {
// CHECK:bb0([[REG0:%.*]] : $*GenNCWrapper<T>):
// CHECK:  debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:  [[REG2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG0]]
// CHECK:  [[REG3:%.*]] = integer_literal $Builtin.IntLiteral, 0
// CHECK:  [[REG4:%.*]] = metatype $@thin Int.Type
// CHECK:  [[REG5:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:  [[REG6:%.*]] = apply [[REG5]]([[REG3]], [[REG4]]) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:  [[REG7:%.*]] = function_ref @$s15borrow_accessor12GenNCWrapperVAARi_zrlEyxSiciz : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @inout GenNCWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:  [[REG8:%.*]] = apply [[REG7]]<T>([[REG6]], [[REG2]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @inout GenNCWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:  [[REG9:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG8]]
// CHECK:  return [[REG9]]
// CHECK: }


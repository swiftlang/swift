// RUN: %target-swift-emit-silgen %s -disable-objc-attr-requires-foundation-module -enable-objc-interop | %FileCheck %s

struct X { }

// Initializer delegation within a struct.
struct S {
  // CHECK-LABEL: sil hidden @$s19init_ref_delegation1SV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thin S.Type) -> S {
  init() {
    // CHECK: bb0([[SELF_META:%[0-9]+]] : $@thin S.Type):
    // CHECK-NEXT:   [[SELF_BOX:%[0-9]+]] = alloc_box ${ var S }
    // CHECK-NEXT:   [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK-NEXT:   [[PB:%.*]] = project_box [[MARKED_SELF_BOX]]
    
    // CHECK-NEXT:   [[X_META:%[0-9]+]] = metatype $@thin X.Type
    // CHECK:   [[X_CTOR:%[0-9]+]] = function_ref @$s19init_ref_delegation1XV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thin X.Type) -> X
    // CHECK-NEXT:   [[X:%[0-9]+]] = apply [[X_CTOR]]([[X_META]]) : $@convention(method) (@thin X.Type) -> X
    // CHECK:   [[S_DELEG_INIT:%[0-9]+]] = function_ref @$s19init_ref_delegation1SV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (X, @thin S.Type) -> S
    // CHECK-NEXT:   [[REPLACEMENT_SELF:%[0-9]+]] = apply [[S_DELEG_INIT]]([[X]], [[SELF_META]]) : $@convention(method) (X, @thin S.Type) -> S
    self.init(x: X())
    // CHECK-NEXT:   assign [[REPLACEMENT_SELF]] to [[PB]] : $*S
    // CHECK-NEXT:   [[SELF_BOX1:%[0-9]+]] = load [trivial] [[PB]] : $*S
    // CHECK-NEXT:   destroy_value [[MARKED_SELF_BOX]] : ${ var S }
    // CHECK-NEXT:   return [[SELF_BOX1]] : $S
  }

  init(x: X) { }
}

// Initializer delegation within an enum
enum E {
  // We don't want the enum to be uninhabited
  case Foo

  // CHECK-LABEL: sil hidden @$s19init_ref_delegation1EO{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thin E.Type) -> E
  init() {
    // CHECK: bb0([[E_META:%[0-9]+]] : $@thin E.Type):
    // CHECK:   [[E_BOX:%[0-9]+]] = alloc_box ${ var E }
    // CHECK:   [[MARKED_E_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[E_BOX]]
    // CHECK:   [[PB:%.*]] = project_box [[MARKED_E_BOX]]

    // CHECK:   [[X_META:%[0-9]+]] = metatype $@thin X.Type
    // CHECK:   [[E_DELEG_INIT:%[0-9]+]] = function_ref @$s19init_ref_delegation1XV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thin X.Type) -> X
    // CHECK:   [[X:%[0-9]+]] = apply [[E_DELEG_INIT]]([[X_META]]) : $@convention(method) (@thin X.Type) -> X
    // CHECK:   [[X_INIT:%[0-9]+]] = function_ref @$s19init_ref_delegation1EO{{[_0-9a-zA-Z]*}}fC : $@convention(method) (X, @thin E.Type) -> E
    // CHECK:   [[S:%[0-9]+]] = apply [[X_INIT]]([[X]], [[E_META]]) : $@convention(method) (X, @thin E.Type) -> E
    // CHECK:   assign [[S:%[0-9]+]] to [[PB]] : $*E
    // CHECK:   [[E_BOX1:%[0-9]+]] = load [trivial] [[PB]] : $*E
    self.init(x: X())
    // CHECK:   destroy_value [[MARKED_E_BOX]] : ${ var E }
    // CHECK:   return [[E_BOX1:%[0-9]+]] : $E
  }

  init(x: X) { }
}

// Initializer delegation to a generic initializer
struct S2 {
  // CHECK-LABEL: sil hidden @$s19init_ref_delegation2S2V{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thin S2.Type) -> S2
  init() {
    // CHECK: bb0([[S2_META:%[0-9]+]] : $@thin S2.Type):
    // CHECK:   [[SELF_BOX:%[0-9]+]] = alloc_box ${ var S2 }
    // CHECK:   [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK:   [[PB:%.*]] = project_box [[MARKED_SELF_BOX]]

    // CHECK:   [[X_META:%[0-9]+]] = metatype $@thin X.Type
    // CHECK:   [[X_INIT:%[0-9]+]] = function_ref @$s19init_ref_delegation1XV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thin X.Type) -> X
    // CHECK:   [[X:%[0-9]+]] = apply [[X_INIT]]([[X_META]]) : $@convention(method) (@thin X.Type) -> X
    // CHECK:   [[X_BOX:%[0-9]+]] = alloc_stack $X
    // CHECK:   store [[X]] to [trivial] [[X_BOX]] : $*X
    // CHECK:   [[S2_DELEG_INIT:%[0-9]+]] = function_ref @$s19init_ref_delegation2S2V{{[_0-9a-zA-Z]*}}fC : $@convention(method) <τ_0_0> (@in τ_0_0, @thin S2.Type) -> S2
    // CHECK:   [[SELF_BOX1:%[0-9]+]] = apply [[S2_DELEG_INIT]]<X>([[X_BOX]], [[S2_META]]) : $@convention(method) <τ_0_0> (@in τ_0_0, @thin S2.Type) -> S2
    // CHECK:   dealloc_stack [[X_BOX]] : $*X
    // CHECK:   assign [[SELF_BOX1]] to [[PB]] : $*S2
    // CHECK:   [[SELF_BOX4:%[0-9]+]] = load [trivial] [[PB]] : $*S2
    self.init(t: X())
    // CHECK:   destroy_value [[MARKED_SELF_BOX]] : ${ var S2 }
    // CHECK:   return [[SELF_BOX4]] : $S2
  }

  init<T>(t: T) { }
}



class C1 {
  var ivar: X

 // CHECK-LABEL: sil hidden @$s19init_ref_delegation2C1C{{[_0-9a-zA-Z]*}}fC
  convenience init(x: X) {
    // CHECK: bb0([[X:%[0-9]+]] : $X, [[SELF_META:%[0-9]+]] : $@thick C1.Type):
    // CHECK:   [[SELF_BOX:%[0-9]+]] = alloc_box ${ var C1 }
    // CHECK:   [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK:   [[PB:%.*]] = project_box [[MARKED_SELF_BOX]]

    // CHECK:   [[DELEG_INIT:%[0-9]+]] = class_method [[SELF_META]] : $@thick C1.Type, #C1.init!allocator.1
    // CHECK:   [[SELFP:%[0-9]+]] = apply [[DELEG_INIT]]([[X]], [[X]], [[SELF_META]])
    // CHECK:   assign [[SELFP]] to [[PB]]
    // CHECK:   [[SELFP:%[0-9]+]] = load [copy] [[PB]] : $*C1
    // CHECK:   destroy_value [[MARKED_SELF_BOX]] : ${ var C1 }
    // CHECK:   return [[SELFP]] : $C1
    self.init(x1: x, x2: x)
  }

  init(x1: X, x2: X) { ivar = x1 }
}

@objc class C2 {
  var ivar: X

  // CHECK-LABEL: sil hidden @$s19init_ref_delegation2C2C{{[_0-9a-zA-Z]*}}fC
  convenience init(x: X) {
    // CHECK: bb0([[X:%[0-9]+]] : $X, [[SELF_META:%[0-9]+]] : $@thick C2.Type):
    // CHECK:   [[SELF_BOX:%[0-9]+]] = alloc_box ${ var C2 }
    // CHECK:   [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK:   [[PB_SELF:%.*]] = project_box [[MARKED_SELF_BOX]]
    // CHECK:   [[DELEG_INIT:%[0-9]+]] = class_method [[SELF_META]] : $@thick C2.Type, #C2.init!allocator.1
    // CHECK:   [[REPLACE_SELF:%[0-9]+]] = apply [[DELEG_INIT]]([[X]], [[X]], [[SELF_META]])
    // CHECK:   assign [[REPLACE_SELF]] to [[PB_SELF]] : $*C2
    // CHECK:   [[VAR_15:%[0-9]+]] = load [copy] [[PB_SELF]] : $*C2
    // CHECK:   destroy_value [[MARKED_SELF_BOX]] : ${ var C2 }
    // CHECK:   return [[VAR_15]] : $C2
    self.init(x1: x, x2: x)
    // CHECK-NOT: sil hidden @$s19init_ref_delegation2C2C{{[_0-9a-zA-Z]*}}fcTo : $@convention(objc_method) (X, @owned C2) -> @owned C2 {
  }

  // CHECK-LABEL: sil hidden @$s19init_ref_delegation2C2C{{[_0-9a-zA-Z]*}}fC : $@convention(method) (X, X, @thick C2.Type) -> @owned C2 {
  // CHECK-NOT:   sil @$s19init_ref_delegation2C2C{{[_0-9a-zA-Z]*}}fcTo : $@convention(objc_method) (X, X, @owned C2) -> @owned C2 {
  init(x1: X, x2: X) { ivar = x1 }
}

var x: X = X()

class C3 {
  var i: Int = 5

  // CHECK-LABEL: sil hidden @$s19init_ref_delegation2C3C{{[_0-9a-zA-Z]*}}fC
  convenience init() {
    // CHECK: mark_uninitialized [delegatingself]
    // CHECK-NOT: integer_literal
    // CHECK: class_method [[SELF:%[0-9]+]] : $@thick C3.Type, #C3.init!allocator.1
    // CHECK-NOT: integer_literal
    // CHECK: return
    self.init(x: x)
  }

  init(x: X) { }
}

// Initializer delegation from a constructor defined in an extension.

class C4 { }

extension C4 {
  convenience init(x1: X) {
    self.init()
  }
  // CHECK: sil hidden @$s19init_ref_delegation2C4C{{[_0-9a-zA-Z]*}}fC
  // CHECK: [[PEER:%[0-9]+]] = function_ref @$s19init_ref_delegation2C4C{{[_0-9a-zA-Z]*}}fC
  // CHECK: apply [[PEER]]([[X:%[0-9]+]], [[META:%[0-9]+]])
  convenience init(x2: X) {
    self.init(x1: x2)
  }
}

// Initializer delegation to a constructor defined in a protocol extension.

protocol Pb {
  init()
}

extension Pb {
  init(d: Int) { }
}

class Sn : Pb {
  required init() { }

  convenience init(d3: Int) {
    self.init(d: d3)
  }
}

// Same as above but for a value type.

struct Cs : Pb {
  init() { }

  init(d3: Int) {
    self.init(d: d3)
  }
}

// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

struct Foo {}
class Bar {}

// CHECK: [[CONCRETE:%.*]] = init_existential_addr [[EXISTENTIAL:%.*]] : $*Any, $Foo.Type
// CHECK: [[METATYPE:%.*]] = metatype $@thick Foo.Type
// CHECK: store [[METATYPE]] to [trivial] [[CONCRETE]] : $*@thick Foo.Type
let x: Any = Foo.self


// CHECK: [[CONCRETE:%.*]] = init_existential_addr [[EXISTENTIAL:%.*]] : $*Any, $() -> ()
// CHECK: [[CLOSURE:%.*]] = function_ref
// CHECK: [[CLOSURE_THICK:%.*]] = thin_to_thick_function [[CLOSURE]]
// CHECK: [[REABSTRACTION_THUNK:%.*]] = function_ref @_T0Ix_ytytIxir_TR
// CHECK: [[CLOSURE_REABSTRACTED:%.*]] = partial_apply [[REABSTRACTION_THUNK]]([[CLOSURE_THICK]])
// CHECK: store [[CLOSURE_REABSTRACTED]] to [init] [[CONCRETE]]
let y: Any = {() -> () in ()}


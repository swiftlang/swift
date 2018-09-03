
// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

struct Foo {}
class Bar {}

// CHECK: [[METATYPE:%.*]] = metatype $@thick Foo.Type
// CHECK: [[CONCRETE:%.*]] = init_existential_addr [[EXISTENTIAL:%.*]] : $*Any, $Foo.Type
// CHECK: store [[METATYPE]] to [trivial] [[CONCRETE]] : $*@thick Foo.Type
let x: Any = Foo.self


// CHECK: [[CLOSURE:%.*]] = function_ref
// CHECK: [[CLOSURE_THICK:%.*]] = thin_to_thick_function [[CLOSURE]]
// CHECK: [[REABSTRACTION_THUNK:%.*]] = function_ref @$SIeg_ytytIegnr_TR
// CHECK: [[CLOSURE_REABSTRACTED:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACTION_THUNK]]([[CLOSURE_THICK]])
// CHECK: [[CONCRETE:%.*]] = init_existential_addr [[EXISTENTIAL:%.*]] : $*Any, $() -> ()
// CHECK: store [[CLOSURE_REABSTRACTED]] to [init] [[CONCRETE]]
let y: Any = {() -> () in ()}


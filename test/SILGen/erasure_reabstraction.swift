
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

struct Foo {}
class Bar {}

// CHECK: [[METATYPE:%.*]] = metatype $@thick Foo.Type
// CHECK: [[CONCRETE:%.*]] = init_existential_addr [[EXISTENTIAL:%.*]] : $*Any, $Foo.Type
// CHECK: store [[METATYPE]] to [trivial] [[CONCRETE]] : $*@thick Foo.Type
let x: Any = Foo.self


// CHECK: [[CLOSURE:%.*]] = function_ref
// CHECK: [[CLOSURE_THICK:%.*]] = thin_to_thick_function [[CLOSURE]]
// CHECK: [[REABSTRACTION_THUNK:%.*]] = function_ref @$sIeg_ytIegr_TR
// CHECK: [[CLOSURE_REABSTRACTED:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACTION_THUNK]]([[CLOSURE_THICK]])
// CHECK: [[CLOSURE_CONV:%.*]] = convert_function [[CLOSURE_REABSTRACTED]]
// CHECK: [[CONCRETE:%.*]] = init_existential_addr [[EXISTENTIAL:%.*]] : $*Any, $() -> ()
// CHECK: store [[CLOSURE_CONV]] to [init] [[CONCRETE]]
let y: Any = {() -> () in ()}


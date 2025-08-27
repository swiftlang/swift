// RUN: %target-swift-emit-silgen -module-name test %s | %FileCheck %s

func returnStringAnyPair() -> (String, Any) {
  return ("hello", "world")
}

//   Test that we handle projections appropriately in emit-into contexts.
// CHECK-LABEL: sil hidden [ossa] @$s4test0A17ProjectIntoReturnypyF :
// CHECK:       bb0(%0 : $*Any):
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[FN:%.*]] = function_ref @$s4test19returnStringAnyPairSS_yptyF :
// CHECK-NEXT:    [[STRING:%.*]] = apply [[FN]](%0)
// CHECK-NEXT:    ignored_use [[STRING]]
// CHECK-NEXT:    destroy_value [[STRING]]
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]]
func testProjectIntoReturn() -> Any {
  return returnStringAnyPair().1
}

func returnAsTuple<each T>(_ args: repeat each T) -> (repeat each T) {
  (repeat each args)
}

//   Test that we handle projections of non-pack elements of tuples.
// CHECK-LABEL: sil hidden [ossa] @$s4test0A14PackProjectionySSxxQpRvzlF :
// CHECK:       bb0(%0 : $*Pack{repeat each T}):
// CHECK:         [[VALUES:%.*]] = alloc_stack [lexical] [var_decl] $(String, repeat each T, String)
//   The result gets expanded, so we emit a pack loop to set up a pack
//   initialization of the elements of values. Eventually we do this call:
// CHECK:         [[FN:%.*]] = function_ref @$s4test13returnAsTupleyxxQp_txxQpRvzlF :
// CHECK-NEXT:    apply [[FN]]<Pack{String, repeat each T, String}>(
//   This copy is unnecessary.
// CHECK:         [[TEMP:%.*]] = alloc_stack $(String, repeat each T, String)
// CHECK-NEXT:    copy_addr [[VALUES]] to [init] [[TEMP]]
// CHECK:         [[INDEX2:%.*]] = scalar_pack_index 2 of
// CHECK-NEXT:    [[ELT2_ADDR:%.*]] = tuple_pack_element_addr [[INDEX2]] of [[TEMP]] as $*String
// CHECK-NEXT:    [[VALUE:%.*]] = load [take] [[ELT2_ADDR]]
// CHECK:         return [[VALUE]]
func testPackProjection<each T>(_ args: repeat each T) -> String {
  let values = returnAsTuple("start", repeat each args, "end")
  return values.2
}

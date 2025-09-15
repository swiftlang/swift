// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name test -Xllvm -sil-full-demangle -swift-version 6 %s | %FileCheck %s

// We weren't adjusting offsets around isolated default arguments
// properly, and it broke when presented with a non-isolated or
// non-defaulted argument between two isolated default arguments.

@MainActor
func main_actor_int_x() -> Int {
  return 0
}

@MainActor
func main_actor_int_y() -> Int {
  return 0
}

@MainActor
func main_actor_int_z() -> Int {
  return 0
}

@MainActor
func main_actor_void() -> () {
}

@MainActor
func main_actor_int_pair() -> (Int, Int) {
  return (0,0)
}

func make_int(isolated isolation: (any Actor)? = #isolation) -> Int {
  return 0
}

// This test breaks because the intermediate argument is `nil`, which
// we treat as non-isolated.
@MainActor
func nonIsolatedDefaultArg(x: Int = main_actor_int_x(),
                           y: Int? = nil,
                           z: Int = main_actor_int_z()) {}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A21NonIsolatedDefaultArgyyYaF :
// CHECK:         [[ARG1:%.*]] = enum $Optional<Int>, #Optional.none
// CHECK:         hop_to_executor {{%.*}} : $MainActor
// CHECK-NEXT:    // function_ref default argument 0 of
// CHECK-NEXT:    [[ARG0FN:%.*]] = function_ref @$s4test21nonIsolatedDefaultArg1x1y1zySi_SiSgSitFfA_
// CHECK-NEXT:    [[ARG0:%.*]] = apply [[ARG0FN]]()
// CHECK-NEXT:    // function_ref default argument 2 of
// CHECK-NEXT:    [[ARG2FN:%.*]] = function_ref @$s4test21nonIsolatedDefaultArg1x1y1zySi_SiSgSitFfA1_
// CHECK-NEXT:    [[ARG2:%.*]] = apply [[ARG2FN]]()
// CHECK-NEXT:    // function_ref test.nonIsolatedDefaultArg
// CHECK-NEXT:    [[FN:%.*]] = function_ref @$s4test21nonIsolatedDefaultArg1x1y1zySi_SiSgSitF :
// CHECK:         apply [[FN]]([[ARG0]], [[ARG1]], [[ARG2]])
func testNonIsolatedDefaultArg() async {
  await nonIsolatedDefaultArg()
}

// This test breaks because the intermediate argument is non-defaulted
// and so gets evaluated in the non-delayed argument pass.
@MainActor
func isolatedDefaultArgs(x: Int = main_actor_int_x(),
                         y: Int = main_actor_int_y(),
                         z: Int = main_actor_int_z()) {}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A13NonDefaultArgyyYaF :
// CHECK:         [[LITERAL_FN:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC :
// CHECK-NEXT:    [[ARG1:%.*]] = apply [[LITERAL_FN]](
// CHECK:         hop_to_executor {{%.*}} : $MainActor
// CHECK-NEXT:    // function_ref default argument 0 of
// CHECK-NEXT:    [[ARG0FN:%.*]] = function_ref @$s4test19isolatedDefaultArgs1x1y1zySi_S2itFfA_
// CHECK-NEXT:    [[ARG0:%.*]] = apply [[ARG0FN]]()
// CHECK-NEXT:    // function_ref default argument 2 of
// CHECK-NEXT:    [[ARG2FN:%.*]] = function_ref @$s4test19isolatedDefaultArgs1x1y1zySi_S2itFfA1_
// CHECK-NEXT:    [[ARG2:%.*]] = apply [[ARG2FN]]()
// CHECK-NEXT:    // function_ref test.isolatedDefaultArgs
// CHECK-NEXT:    [[FN:%.*]] = function_ref @$s4test19isolatedDefaultArgs1x1y1zySi_S2itF :
// CHECK:         apply [[FN]]([[ARG0]], [[ARG1]], [[ARG2]])
func testNonDefaultArg() async {
  await isolatedDefaultArgs(y: 0)
}

// Exercise our handling of isolated default arguments that expand to
// empty or multiple arguments.
@MainActor
func voidIsolatedDefaultArg(x: () = main_actor_void(),
                            y: Int = main_actor_int_y(),
                            z: Int = main_actor_int_z()) {}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A22VoidIsolatedDefaultArgyyYaF :
// CHECK:         [[LITERAL_FN:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC :
// CHECK-NEXT:    [[ARG1:%.*]] = apply [[LITERAL_FN]](
// CHECK:         hop_to_executor {{%.*}} : $MainActor
// CHECK-NEXT:    // function_ref default argument 0 of
// CHECK-NEXT:    [[ARG0FN:%.*]] = function_ref @$s4test22voidIsolatedDefaultArg1x1y1zyyt_S2itFfA_
// CHECK-NEXT:    [[ARG0:%.*]] = apply [[ARG0FN]]()
// CHECK-NEXT:    // function_ref default argument 2 of
// CHECK-NEXT:    [[ARG2FN:%.*]] = function_ref @$s4test22voidIsolatedDefaultArg1x1y1zyyt_S2itFfA1_
// CHECK-NEXT:    [[ARG2:%.*]] = apply [[ARG2FN]]()
// CHECK-NEXT:    // function_ref test.voidIsolatedDefaultArg
// CHECK-NEXT:    [[FN:%.*]] = function_ref @$s4test22voidIsolatedDefaultArg1x1y1zyyt_S2itF :
// CHECK:         apply [[FN]]([[ARG1]], [[ARG2]])
func testVoidIsolatedDefaultArg() async {
  await voidIsolatedDefaultArg(y: 0)
}

@MainActor
func tupleIsolatedDefaultArg(x: (Int,Int) = main_actor_int_pair(),
                             y: Int = main_actor_int_y(),
                             z: Int = main_actor_int_z()) {}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A23TupleIsolatedDefaultArgyyYaF :
// CHECK:         [[LITERAL_FN:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC :
// CHECK-NEXT:    [[ARG1:%.*]] = apply [[LITERAL_FN]](
// CHECK:         hop_to_executor {{%.*}} : $MainActor
// CHECK-NEXT:    // function_ref default argument 0 of
// CHECK-NEXT:    [[ARG0FN:%.*]] = function_ref @$s4test23tupleIsolatedDefaultArg1x1y1zySi_Sit_S2itFfA_
// CHECK-NEXT:    [[ARG0:%.*]] = apply [[ARG0FN]]()
// CHECK-NEXT:    ([[ARG0_0:%.*]], [[ARG0_1:%.*]]) = destructure_tuple [[ARG0]] : $(Int, Int)
// CHECK-NEXT:    // function_ref default argument 2 of
// CHECK-NEXT:    [[ARG2FN:%.*]] = function_ref @$s4test23tupleIsolatedDefaultArg1x1y1zySi_Sit_S2itFfA1_
// CHECK-NEXT:    [[ARG2:%.*]] = apply [[ARG2FN]]()
// CHECK-NEXT:    // function_ref test.tupleIsolatedDefaultArg
// CHECK-NEXT:    [[FN:%.*]] = function_ref @$s4test23tupleIsolatedDefaultArg1x1y1zySi_Sit_S2itF :
// CHECK:         apply [[FN]]([[ARG0_0]], [[ARG0_1]], [[ARG1]], [[ARG2]])
func testTupleIsolatedDefaultArg() async {
  await tupleIsolatedDefaultArg(y: 0)
}

// When a function is caller-isolated, its default argument generators
// should probably also be caller-isolated and forward their isolation
// properly when #isolation is used. Currently, however, that's not what
// we're doing, so test for the current behavior.

nonisolated(nonsending)
func callerIsolatedDefaultArg(x: Int = make_int()) async {}

@MainActor
func useCallerIsolatedDefaultArg() async {
  await callerIsolatedDefaultArg()
}

//   Check that the default argument generator isn't caller-isolated.
// CHECK-LABEL: // default argument 0 of test.callerIsolatedDefaultArg
// CHECK-NEXT:  // Isolation: unspecified
// CHECK-NEXT:  sil hidden [ossa] @$s4test24callerIsolatedDefaultArg1xySi_tYaFfA_ :
// CHECK:       bb0:
//   Check that we provide a nil isolation for #isolation
// CHECK-NEXT:    [[NIL_ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.none
// CHECK-NEXT:    // function_ref test.make_int
// CHECK-NEXT:    [[FN:%.*]] = function_ref @$s4test8make_int8isolatedSiScA_pSg_tF :
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[FN]]([[NIL_ISOLATION]])
// CHECK-NEXT:    return [[RESULT]]

//   Check that we pass the right isolation to the generator.
// CHECK-LABEL: sil hidden [ossa] @$s4test27useCallerIsolatedDefaultArgyyYaF :
//   Get the main actor reference.
// CHECK:         [[GET_MAIN_ACTOR:%.*]] = function_ref @$sScM6sharedScMvgZ :
// CHECK-NEXT:    [[T0:%.*]] = apply [[GET_MAIN_ACTOR]](
// CHECK-NEXT:    [[MAIN_ACTOR:%.*]] = begin_borrow [[T0]]
//   Call the accessor.
// CHECK:         // function_ref default argument 0 of
// CHECK-NEXT:    [[GEN:%.*]] = function_ref @$s4test24callerIsolatedDefaultArg1xySi_tYaFfA_ :
// CHECK-NEXT:    [[ARG:%.*]] = apply [[GEN]]()

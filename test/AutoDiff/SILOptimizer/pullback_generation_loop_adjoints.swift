// RUN: %target-swift-frontend -emit-sil -Xllvm --sil-print-after=differentiation %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -Xllvm --debug-only=differentiation      %s 2>&1 | %FileCheck --check-prefix=DEBUG %s

// Needed for '--debug-only'
// REQUIRES: asserts

import _Differentiation

@differentiable(reverse)
func repeat_while_loop(x: Float) -> Float {
  var result : Float
  repeat {
    result = x + 0
  } while 0 == 1
  return result
}

// DEBUG-LABEL: [AD] Running PullbackCloner on
// DEBUG-NEXT:  // repeat_while_loop
// DEBUG:       [AD] Begin search for adjoints of loop-local active values

// CHECK-LABEL: // pullback of repeat_while_loop(x:)

// DEBUG-NEXT:  [AD] Original bb1 belongs to a loop, original header bb1, pullback header bb3
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping: %[[#AARG0:]] = argument of bb0 : $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#A0:]] = alloc_stack [var_decl] $Float, var, name "result", type $Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, zeroing its adjoint value in loop header:   %[[#A1:]] = apply %[[#A2:]](%[[#AARG0]], %[[#A3:]], %[[#A4:]]) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// DEBUG-NEXT:  [AD] Setting adjoint value for   %[[#A1]] = apply %[[#A2]](%[[#AARG0]], %[[#A3]], %[[#A4]]) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// DEBUG-NEXT:  [AD] No debug variable found.
// DEBUG-NEXT:  [AD] The new adjoint value, replacing the existing one, is: Zero[$Float]
// DEBUG-NEXT:  [AD] The following active value is loop-local, checking if it's adjoint is a projection
// DEBUG-NEXT:  [AD] Adjoint for the following value is a projection, skipping:   %[[#A5:]] = begin_access [modify] [static] %[[#A0]] : $*Float

// CHECK:       bb3(%[[ARG31:[0-9]+]] : $Float, %[[ARG32:[0-9]+]] : $Float, %[[ARG33:[0-9]+]] : @owned $(predecessor: _AD__$s33pullback_generation_loop_adjoints013repeat_while_C01xS2f_tF_bb1__Pred__src_0_wrt_0, @callee_guaranteed (Float) -> Float)):
// CHECK:         (%[[T01:[0-9]+]], %[[T02:[0-9]+]]) = destructure_tuple %[[ARG33]]
// CHECK:         %[[T03:[0-9]+]] = load [trivial] %[[V1:[0-9]+]]

/// Ensure that we do not add adjoint from the previous iteration
/// The incorrect SIL looked like the following:
///   %[[T03]] = load [trivial] %[[V1]]
///   store %[[#B08]] to [trivial] %[[#B13]]                            // <-- we check absence of this
///   store %[[T03]] to [trivial] %[[#B12]]
///   %62 = witness_method $Float, #AdditiveArithmetic."+"
///   %63 = metatype $@thick Float.Type
///   %64 = apply %62<Float>(%[[#]], %[[#B12]], %[[#B13]], %63)
// CHECK-NOT:     store %[[ARG32]] to [trivial] %[[#]]

// CHECK:         %[[T04:[0-9]+]] = witness_method $Float, #AdditiveArithmetic.zero!getter : <Self where Self : AdditiveArithmetic> (Self.Type) -> () -> Self : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:         %[[T05:[0-9]+]] = metatype $@thick Float.Type
// CHECK:         %[[#]] = apply %[[T04]]<Float>(%[[V1]], %[[T05]]) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0

/// It's crucial that we call pullback with T03 which does not contain adjoints from previous iterations
// CHECK:         %[[T07:[0-9]+]] = apply %[[T02]](%[[T03]]) : $@callee_guaranteed (Float) -> Float

// CHECK:         destroy_value %[[T02]]
// CHECK:         %[[T08:[0-9]+]] = alloc_stack $Float
// CHECK:         %[[T09:[0-9]+]] = alloc_stack $Float
// CHECK:         %[[T10:[0-9]+]] = alloc_stack $Float
// CHECK:         store %[[ARG31]] to [trivial] %[[T09]]
// CHECK:         store %[[T07]] to [trivial] %[[T10]]
// CHECK:         %[[T11:[0-9]+]] = witness_method $Float, #AdditiveArithmetic."+" : <Self where Self : AdditiveArithmetic> (Self.Type) -> (Self, Self) -> Self : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> @out τ_0_0
// CHECK:         %[[T12:[0-9]+]] = metatype $@thick Float.Type
// CHECK:         %[[#]] = apply %[[T11]]<Float>(%[[T08]], %[[T10]], %[[T09]], %[[T12]]) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> @out τ_0_0
// CHECK:         destroy_addr %[[T09]]
// CHECK:         destroy_addr %[[T10]]
// CHECK:         dealloc_stack %[[T10]]
// CHECK:         dealloc_stack %[[T09]]
// CHECK:         %[[T14:[0-9]+]] = load [trivial] %[[T08]]
// CHECK:         dealloc_stack %[[T08]]
// CHECK:         debug_value %[[T14]], let, name "x", argno 1
// CHECK:         copy_addr %[[V1]] to %[[V2:[0-9]+]]
// CHECK:         debug_value %[[T14]], let, name "x", argno 1
// CHECK:         copy_addr %[[V1]] to %[[V3:[0-9]+]]
// CHECK:         switch_enum %[[T01]], case #_AD__$s33pullback_generation_loop_adjoints013repeat_while_C01xS2f_tF_bb1__Pred__src_0_wrt_0.bb2!enumelt: bb4, case #_AD__$s33pullback_generation_loop_adjoints013repeat_while_C01xS2f_tF_bb1__Pred__src_0_wrt_0.bb0!enumelt: bb6, forwarding: @owned

// CHECK:       bb4(%[[ARG41:[0-9]+]] : $Builtin.RawPointer):
// CHECK:         %[[T15:[0-9]+]] = pointer_to_address %[[ARG41]] to [strict] $*(predecessor: _AD__$s33pullback_generation_loop_adjoints013repeat_while_C01xS2f_tF_bb2__Pred__src_0_wrt_0)
// CHECK:         %[[T16:[0-9]+]] = load [trivial] %[[T15]]
// CHECK:         br bb5(%[[T14]], %[[T03]], %[[T16]])

// DEBUG-NEXT:  [AD] Original bb2 belongs to a loop, original header bb1, pullback header bb3
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping: %[[#AARG0]] = argument of bb0 : $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#A0]] = alloc_stack [var_decl] $Float, var, name "result", type $Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, but it was already processed, skipping:   %[[#A1]] = apply %[[#A2]](%[[#AARG0]], %[[#A3]], %[[#A4]]) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, but it was already processed, skipping:   %[[#A5]] = begin_access [modify] [static] %[[#A0]] : $*Float

// CHECK:       bb5(%[[ARG51:[0-9]+]] : $Float, %[[ARG52:[0-9]+]] : $Float, %[[ARG53:[0-9]+]] : $(predecessor: _AD__$s33pullback_generation_loop_adjoints013repeat_while_C01xS2f_tF_bb2__Pred__src_0_wrt_0)):

/// Ensure that we do not zero adjoints for non-loop-local active values
// CHECK-NOT:     witness_method $Float, #AdditiveArithmetic.zero!getter

// CHECK:         %[[T17:[0-9]+]] = destructure_tuple %[[ARG53]]
// CHECK:         debug_value %[[ARG51]], let, name "x", argno 1
// CHECK:         copy_addr %[[V2]] to %[[V1]]
// CHECK:         switch_enum %[[T17]], case #_AD__$s33pullback_generation_loop_adjoints013repeat_while_C01xS2f_tF_bb2__Pred__src_0_wrt_0.bb1!enumelt: bb1, forwarding: @owned

// DEBUG-NEXT:  [AD] End search for adjoints of loop-local active values

@differentiable(reverse)
func repeat_while_loop_nested(_ x: Float) -> Float {
  var result = x
  var i = 0
  repeat {
    var temp = x
    var j = 0
    repeat {
      temp = temp * x
      j += 1
    } while j < 2
    result = result * temp
    i += 1
  } while i < 2
  return result
}

// DEBUG-LABEL: [AD] Running PullbackCloner on
// DEBUG-NEXT:  // repeat_while_loop_nested
// DEBUG:       [AD] Begin search for adjoints of loop-local active values

// DEBUG-NEXT:  [AD] Original bb4 belongs to a loop, original header bb1, pullback header bb10
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping: %[[#BARG0:]] = argument of bb0 : $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#B00:]] = alloc_stack [var_decl] $Float, var, name "result", type $Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, checking if it's adjoint is a projection
// DEBUG-NEXT:  [AD] Adjoint for the following value is NOT a projection, zeroing its adjoint buffer in loop header:   %[[#B01:]] = alloc_stack [var_decl] $Float, var, name "temp", type $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#B02:]] = begin_access [read] [static] %[[#B01]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#B03:]] = load [trivial] %[[#B02]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#B04:]] = apply %[[#B05:]](%[[#B03]], %[[#BARG0]], %[[#B06:]]) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#B07:]] = begin_access [modify] [static] %[[#B01]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, checking if it's adjoint is a projection
// DEBUG-NEXT:  [AD] Adjoint for the following value is a projection, skipping:   %[[#B08:]] = begin_access [read] [static] %[[#B00]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, zeroing its adjoint value in loop header:   %[[#B09:]] = load [trivial] %[[#B08]] : $*Float
// DEBUG-NEXT:  [AD] Setting adjoint value for   %[[#B09]] = load [trivial] %[[#B08]] : $*Float
// DEBUG-NEXT:  [AD] No debug variable found.
// DEBUG-NEXT:  [AD] The new adjoint value, replacing the existing one, is: Zero[$Float]
// DEBUG-NEXT:  [AD] The following active value is loop-local, checking if it's adjoint is a projection
// DEBUG-NEXT:  [AD] Adjoint for the following value is a projection, skipping:   %[[#B10:]] = begin_access [read] [static] %[[#B01]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, zeroing its adjoint value in loop header:   %[[#B11:]] = load [trivial] %[[#B10]] : $*Float
// DEBUG-NEXT:  [AD] Setting adjoint value for   %[[#B11]] = load [trivial] %[[#B10]] : $*Float
// DEBUG-NEXT:  [AD] No debug variable found.
// DEBUG-NEXT:  [AD] The new adjoint value, replacing the existing one, is: Zero[$Float]
// DEBUG-NEXT:  [AD] The following active value is loop-local, zeroing its adjoint value in loop header:   %[[#B12:]] = apply %[[#B13:]](%[[#B09]], %[[#B11]], %[[#B14:]]) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// DEBUG-NEXT:  [AD] Setting adjoint value for   %[[#B12]] = apply %[[#B13]](%[[#B09]], %[[#B11]], %[[#B14]]) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// DEBUG-NEXT:  [AD] No debug variable found.
// DEBUG-NEXT:  [AD] The new adjoint value, replacing the existing one, is: Zero[$Float]
// DEBUG-NEXT:  [AD] The following active value is loop-local, checking if it's adjoint is a projection
// DEBUG-NEXT:  [AD] Adjoint for the following value is a projection, skipping:   %[[#B15:]] = begin_access [modify] [static] %[[#B00]] : $*Float

// DEBUG-NEXT:  [AD] Original bb2 belongs to a loop, original header bb2, pullback header bb6
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping: %[[#BARG0]] = argument of bb0 : $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#B00]] = alloc_stack [var_decl] $Float, var, name "result", type $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#B01]] = alloc_stack [var_decl] $Float, var, name "temp", type $Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, checking if it's adjoint is a projection
// DEBUG-NEXT:  [AD] Adjoint for the following value is a projection, skipping:   %[[#B02]] = begin_access [read] [static] %[[#B01]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, zeroing its adjoint value in loop header:   %[[#B03]] = load [trivial] %[[#B02]] : $*Float
// DEBUG-NEXT:  [AD] Setting adjoint value for   %[[#B03]] = load [trivial] %[[#B02]] : $*Float
// DEBUG-NEXT:  [AD] No debug variable found.
// DEBUG-NEXT:  [AD] The new adjoint value, replacing the existing one, is: Zero[$Float]
// DEBUG-NEXT:  [AD] The following active value is loop-local, zeroing its adjoint value in loop header:   %[[#B04]] = apply %[[#B05]](%[[#B03]], %[[#BARG0]], %[[#B06]]) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// DEBUG-NEXT:  [AD] Setting adjoint value for   %[[#B04]] = apply %[[#B05]](%[[#B03]], %[[#BARG0]], %[[#B06]]) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// DEBUG-NEXT:  [AD] No debug variable found.
// DEBUG-NEXT:  [AD] The new adjoint value, replacing the existing one, is: Zero[$Float]
// DEBUG-NEXT:  [AD] The following active value is loop-local, checking if it's adjoint is a projection
// DEBUG-NEXT:  [AD] Adjoint for the following value is a projection, skipping:   %[[#B07]] = begin_access [modify] [static] %[[#B01]] : $*Float

// DEBUG-NEXT:  [AD] Original bb3 belongs to a loop, original header bb2, pullback header bb6
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping: %[[#BARG0]] = argument of bb0 : $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#B00]] = alloc_stack [var_decl] $Float, var, name "result", type $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#B01]] = alloc_stack [var_decl] $Float, var, name "temp", type $Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, but it was already processed, skipping:   %[[#B02]] = begin_access [read] [static] %[[#B01]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, but it was already processed, skipping:   %[[#B03]] = load [trivial] %[[#B02]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, but it was already processed, skipping:   %[[#B04]] = apply %[[#B05]](%[[#B03]], %[[#BARG0]], %[[#B06]]) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, but it was already processed, skipping:   %[[#B07]] = begin_access [modify] [static] %[[#B01]] : $*Float

// DEBUG-NEXT:  [AD] Original bb1 belongs to a loop, original header bb1, pullback header bb10
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping: %[[#BARG0]] = argument of bb0 : $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#B00]] = alloc_stack [var_decl] $Float, var, name "result", type $Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, but it was already processed, skipping:   %[[#B01]] = alloc_stack [var_decl] $Float, var, name "temp", type $Float

// DEBUG-NEXT:  [AD] Original bb5 belongs to a loop, original header bb1, pullback header bb10
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping: %[[#BARG0]] = argument of bb0 : $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#B00]] = alloc_stack [var_decl] $Float, var, name "result", type $Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, but it was already processed, skipping:   %[[#B01]] = alloc_stack [var_decl] $Float, var, name "temp", type $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#B02]] = begin_access [read] [static] %[[#B01]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#B03]] = load [trivial] %[[#B02]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#B04]] = apply %[[#B05]](%[[#B03]], %[[#BARG0]], %[[#B06]]) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#B07]] = begin_access [modify] [static] %[[#B01]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, but it was already processed, skipping:   %[[#B08]] = begin_access [read] [static] %[[#B00]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, but it was already processed, skipping:   %[[#B09]] = load [trivial] %[[#B08]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, but it was already processed, skipping:   %[[#B10]] = begin_access [read] [static] %[[#B01]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, but it was already processed, skipping:   %[[#B11]] = load [trivial] %[[#B10]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, but it was already processed, skipping:   %[[#B12]] = apply %[[#B13]](%[[#B09]], %[[#B11]], %[[#B14]]) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, but it was already processed, skipping:   %[[#B15]] = begin_access [modify] [static] %[[#B00]] : $*Float

// DEBUG-NEXT:  [AD] End search for adjoints of loop-local active values

@differentiable(reverse)
func repeat_while_loop_condition(_ x: Float) -> Float {
  var result = x
  var i = 0
  repeat {
    if i == 2 {
      break
    }
    if i == 0 {
      result = result * x
    } else {
      result = result * result
    }
    i += 1
  } while i < 10
  return result
}

// DEBUG-LABEL: [AD] Running PullbackCloner on
// DEBUG-NEXT:  // repeat_while_loop_condition
// DEBUG:       [AD] Begin search for adjoints of loop-local active values

// DEBUG-NEXT:  [AD] Original bb6 belongs to a loop, original header bb1, pullback header bb10
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping: %[[#CARG0:]] = argument of bb0 : $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#C00:]] = alloc_stack [var_decl] $Float, var, name "result", type $Float

// DEBUG-NEXT:  [AD] Original bb1 belongs to a loop, original header bb1, pullback header bb10
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping: %[[#CARG0]] = argument of bb0 : $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#C00]] = alloc_stack [var_decl] $Float, var, name "result", type $Float

// DEBUG-NEXT:  [AD] Original bb5 belongs to a loop, original header bb1, pullback header bb10
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping: %[[#CARG0]] = argument of bb0 : $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#C00]] = alloc_stack [var_decl] $Float, var, name "result", type $Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, checking if it's adjoint is a projection
// DEBUG-NEXT:  [AD] Adjoint for the following value is a projection, skipping:   %[[#C01:]] = begin_access [read] [static] %[[#C00]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, zeroing its adjoint value in loop header:   %[[#C02:]] = load [trivial] %[[#C01]] : $*Float
// DEBUG-NEXT:  [AD] Setting adjoint value for   %[[#C02]] = load [trivial] %[[#C01]] : $*Float
// DEBUG-NEXT:  [AD] No debug variable found.
// DEBUG-NEXT:  [AD] The new adjoint value, replacing the existing one, is: Zero[$Float]
// DEBUG-NEXT:  [AD] The following active value is loop-local, checking if it's adjoint is a projection
// DEBUG-NEXT:  [AD] Adjoint for the following value is a projection, skipping:   %[[#C03:]] = begin_access [read] [static] %[[#C00]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, zeroing its adjoint value in loop header:   %[[#C04:]] = load [trivial] %[[#C03]] : $*Float
// DEBUG-NEXT:  [AD] Setting adjoint value for   %[[#C04]] = load [trivial] %[[#C03]] : $*Float
// DEBUG-NEXT:  [AD] No debug variable found.
// DEBUG-NEXT:  [AD] The new adjoint value, replacing the existing one, is: Zero[$Float]
// DEBUG-NEXT:  [AD] The following active value is loop-local, zeroing its adjoint value in loop header:   %[[#C05:]] = apply %[[#C06:]](%[[#C02]], %[[#C04]], %[[#C07:]]) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// DEBUG-NEXT:  [AD] Setting adjoint value for   %[[#C05]] = apply %[[#C06]](%[[#C02]], %[[#C04]], %[[#C07]]) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// DEBUG-NEXT:  [AD] No debug variable found.
// DEBUG-NEXT:  [AD] The new adjoint value, replacing the existing one, is: Zero[$Float]
// DEBUG-NEXT:  [AD] The following active value is loop-local, checking if it's adjoint is a projection
// DEBUG-NEXT:  [AD] Adjoint for the following value is a projection, skipping:   %[[#]] = begin_access [modify] [static] %[[#C00]] : $*Float

// DEBUG-NEXT:  [AD] Original bb4 belongs to a loop, original header bb1, pullback header bb10
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping: %[[#CARG0]] = argument of bb0 : $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#C00]] = alloc_stack [var_decl] $Float, var, name "result", type $Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, checking if it's adjoint is a projection
// DEBUG-NEXT:  [AD] Adjoint for the following value is a projection, skipping:   %[[#C08:]] = begin_access [read] [static] %[[#C00]] : $*Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, zeroing its adjoint value in loop header:   %[[#C09:]] = load [trivial] %[[#C08]] : $*Float
// DEBUG-NEXT:  [AD] Setting adjoint value for   %[[#C09]] = load [trivial] %[[#C08]] : $*Float
// DEBUG-NEXT:  [AD] No debug variable found.
// DEBUG-NEXT:  [AD] The new adjoint value, replacing the existing one, is: Zero[$Float]
// DEBUG-NEXT:  [AD] The following active value is loop-local, zeroing its adjoint value in loop header:   %[[#C10:]] = apply %[[#C11:]](%[[#C09]], %[[#CARG0]], %[[#C12:]]) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// DEBUG-NEXT:  [AD] Setting adjoint value for   %[[#C10]] = apply %[[#C11]](%[[#C09]], %[[#CARG0]], %[[#C12]]) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// DEBUG-NEXT:  [AD] No debug variable found.
// DEBUG-NEXT:  [AD] The new adjoint value, replacing the existing one, is: Zero[$Float]
// DEBUG-NEXT:  [AD] The following active value is loop-local, checking if it's adjoint is a projection
// DEBUG-NEXT:  [AD] Adjoint for the following value is a projection, skipping:   %[[#]] = begin_access [modify] [static] %[[#C00]] : $*Float

// DEBUG-NEXT:  [AD] Original bb7 belongs to a loop, original header bb1, pullback header bb10
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping: %[[#CARG0]] = argument of bb0 : $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#C00]] = alloc_stack [var_decl] $Float, var, name "result", type $Float

// DEBUG-NEXT:  [AD] Original bb3 belongs to a loop, original header bb1, pullback header bb10
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping: %[[#CARG0]] = argument of bb0 : $Float
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#C00]] = alloc_stack [var_decl] $Float, var, name "result", type $Float

// DEBUG-NEXT:  [AD] End search for adjoints of loop-local active values

typealias FloatArrayTan = Array<Float>.TangentVector

func identity(_ array: [Float]) -> [Float] {
  var results: [Float] = []
  for i in withoutDerivative(at: array.indices) {
    results += [array[i]]
  }
  return results
}

pullback(at: [1, 2, 3], of: identity)(FloatArrayTan([4, -5, 6]))

// DEBUG-LABEL: [AD] Running PullbackCloner on
// DEBUG-NEXT:  // identity
// DEBUG:       [AD] Begin search for adjoints of loop-local active values

// DEBUG-NEXT:  [AD] Original bb1 belongs to a loop, original header bb1, pullback header bb3
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping: %[[#DARG0:]] = argument of bb0 : $Array<Float>
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#D0:]] = alloc_stack [var_decl] $Array<Float>, var, name "results", type $Array<Float>

// DEBUG-NEXT:  [AD] Original bb2 belongs to a loop, original header bb1, pullback header bb3
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping: %[[#DARG0:]] = argument of bb0 : $Array<Float>
// DEBUG-NEXT:  [AD] The following active value is NOT loop-local, skipping:   %[[#D0]] = alloc_stack [var_decl] $Array<Float>, var, name "results", type $Array<Float>
// DEBUG-NEXT:  [AD] The following active value is loop-local, zeroing its adjoint value in loop header:   %[[#D1:]] = apply %[[#]]<Float>(%[[#]]) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// DEBUG-NEXT:  [AD] Setting adjoint value for   %[[#D:]] = apply %[[#]]<Float>(%[[#]]) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// DEBUG-NEXT:  [AD] No debug variable found.
// DEBUG-NEXT:  [AD] The new adjoint value, replacing the existing one, is: Zero[$Array<Float>.DifferentiableView]
// DEBUG-NEXT:  [AD] The following active value is loop-local, zeroing its adjoint value in loop header: (**%[[#D2:]]**, %[[#D3:]]) = destructure_tuple %[[#D1]] : $(Array<Float>, Builtin.RawPointer)
// DEBUG-NEXT:  [AD] Setting adjoint value for (**%[[#D2]]**, %[[#D3]]) = destructure_tuple %[[#D1]] : $(Array<Float>, Builtin.RawPointer)
// DEBUG-NEXT:  [AD] No debug variable found.
// DEBUG-NEXT:  [AD] The new adjoint value, replacing the existing one, is: Zero[$Array<Float>.DifferentiableView]
// DEBUG-NEXT:  [AD] The following active value is loop-local, checking if it's adjoint is a projection
// DEBUG-NEXT:  [AD] Materializing adjoint for Zero[$Array<Float>.DifferentiableView]
// DEBUG-NEXT:  [AD] Recorded temporary   %[[#]] = load [take] %[[#]] : $*Array<Float>.DifferentiableView
// DEBUG-NEXT:  [AD] Adjoint for the following value is a projection, skipping:   %[[#D4:]] = pointer_to_address %[[#D5:]] : $Builtin.RawPointer to [strict] $*Float
// DEBUG-NEXT:  [AD] The following active value is loop-local, zeroing its adjoint value in loop header:   %[[#D6:]] = apply %[[#]]<Float>(%[[#D2]]) : $@convention(thin) <τ_0_0> (@owned Array<τ_0_0>) -> @owned Array<τ_0_0>
// DEBUG-NEXT:  [AD] Setting adjoint value for   %[[#D6]] = apply %[[#]]<Float>(%[[#D2]]) : $@convention(thin) <τ_0_0> (@owned Array<τ_0_0>) -> @owned Array<τ_0_0>
// DEBUG-NEXT:  [AD] No debug variable found.
// DEBUG-NEXT:  [AD] The new adjoint value, replacing the existing one, is: Zero[$Array<Float>.DifferentiableView]
// DEBUG-NEXT:  [AD] The following active value is loop-local, checking if it's adjoint is a projection
// DEBUG-NEXT:  [AD] Adjoint for the following value is a projection, skipping:   %[[#]] = begin_access [modify] [static] %[[#D0]] : $*Array<Float>

// DEBUG-NEXT:  [AD] End search for adjoints of loop-local active values

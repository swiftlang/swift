// RUN: %target-swift-frontend -enable-builtin-module -Xllvm -sil-print-types -emit-silgen %s | %FileCheck %s

import Builtin

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteAddTest{{.*}} : $@convention(thin) (Builtin.Vec4xInt32, Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xInt32, [[ARG1:%.*]] : $Builtin.Vec4xInt32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_add"<Builtin.Vec4xInt32>([[ARG0]] : $Builtin.Vec4xInt32, [[ARG1]] : $Builtin.Vec4xInt32) : $Builtin.Vec4xInt32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteAddTest{{.*}}'
func concreteAddTest(_ x: Builtin.Vec4xInt32, _ y: Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return Builtin.generic_add(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericAddTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_add"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericAddTest{{.*}}'
func genericAddTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_add(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteAndTest{{.*}} : $@convention(thin) (Builtin.Vec4xInt32, Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xInt32, [[ARG1:%.*]] : $Builtin.Vec4xInt32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_and"<Builtin.Vec4xInt32>([[ARG0]] : $Builtin.Vec4xInt32, [[ARG1]] : $Builtin.Vec4xInt32) : $Builtin.Vec4xInt32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteAndTest{{.*}}'
func concreteAndTest(_ x: Builtin.Vec4xInt32, _ y: Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return Builtin.generic_and(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericAndTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_and"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericAndTest{{.*}}'
func genericAndTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_and(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteAshrTest{{.*}} : $@convention(thin) (Builtin.Vec4xInt32, Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xInt32, [[ARG1:%.*]] : $Builtin.Vec4xInt32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_ashr"<Builtin.Vec4xInt32>([[ARG0]] : $Builtin.Vec4xInt32, [[ARG1]] : $Builtin.Vec4xInt32) : $Builtin.Vec4xInt32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteAshrTest{{.*}}'
func concreteAshrTest(_ x: Builtin.Vec4xInt32, _ y: Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return Builtin.generic_ashr(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericAshrTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_ashr"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericAshrTest{{.*}}'
func genericAshrTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_ashr(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteLshrTest{{.*}} : $@convention(thin) (Builtin.Vec4xInt32, Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xInt32, [[ARG1:%.*]] : $Builtin.Vec4xInt32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_lshr"<Builtin.Vec4xInt32>([[ARG0]] : $Builtin.Vec4xInt32, [[ARG1]] : $Builtin.Vec4xInt32) : $Builtin.Vec4xInt32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteLshrTest{{.*}}'
func concreteLshrTest(_ x: Builtin.Vec4xInt32, _ y: Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return Builtin.generic_lshr(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericLshrTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_lshr"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericLshrTest{{.*}}'
func genericLshrTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_lshr(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteMulTest{{.*}} : $@convention(thin) (Builtin.Vec4xInt32, Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xInt32, [[ARG1:%.*]] : $Builtin.Vec4xInt32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_mul"<Builtin.Vec4xInt32>([[ARG0]] : $Builtin.Vec4xInt32, [[ARG1]] : $Builtin.Vec4xInt32) : $Builtin.Vec4xInt32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteMulTest{{.*}}'
func concreteMulTest(_ x: Builtin.Vec4xInt32, _ y: Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return Builtin.generic_mul(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericMulTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_mul"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericMulTest{{.*}}'
func genericMulTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_mul(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteOrTest{{.*}} : $@convention(thin) (Builtin.Vec4xInt32, Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xInt32, [[ARG1:%.*]] : $Builtin.Vec4xInt32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_or"<Builtin.Vec4xInt32>([[ARG0]] : $Builtin.Vec4xInt32, [[ARG1]] : $Builtin.Vec4xInt32) : $Builtin.Vec4xInt32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteOrTest{{.*}}'
func concreteOrTest(_ x: Builtin.Vec4xInt32, _ y: Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return Builtin.generic_or(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericOrTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_or"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericOrTest{{.*}}'
func genericOrTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_or(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteSdivTest{{.*}} : $@convention(thin) (Builtin.Vec4xInt32, Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xInt32, [[ARG1:%.*]] : $Builtin.Vec4xInt32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_sdiv"<Builtin.Vec4xInt32>([[ARG0]] : $Builtin.Vec4xInt32, [[ARG1]] : $Builtin.Vec4xInt32) : $Builtin.Vec4xInt32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteSdivTest{{.*}}'
func concreteSdivTest(_ x: Builtin.Vec4xInt32, _ y: Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return Builtin.generic_sdiv(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericSdivTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_sdiv"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericSdivTest{{.*}}'
func genericSdivTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_sdiv(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteSdivExactTest{{.*}} : $@convention(thin) (Builtin.Vec4xInt32, Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xInt32, [[ARG1:%.*]] : $Builtin.Vec4xInt32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_sdiv_exact"<Builtin.Vec4xInt32>([[ARG0]] : $Builtin.Vec4xInt32, [[ARG1]] : $Builtin.Vec4xInt32) : $Builtin.Vec4xInt32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteSdivExactTest{{.*}}'
func concreteSdivExactTest(_ x: Builtin.Vec4xInt32, _ y: Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return Builtin.generic_sdiv_exact(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericSdivExactTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_sdiv_exact"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericSdivExactTest{{.*}}'
func genericSdivExactTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_sdiv_exact(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteShlTest{{.*}} : $@convention(thin) (Builtin.Vec4xInt32, Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xInt32, [[ARG1:%.*]] : $Builtin.Vec4xInt32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_shl"<Builtin.Vec4xInt32>([[ARG0]] : $Builtin.Vec4xInt32, [[ARG1]] : $Builtin.Vec4xInt32) : $Builtin.Vec4xInt32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteShlTest{{.*}}'
func concreteShlTest(_ x: Builtin.Vec4xInt32, _ y: Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return Builtin.generic_shl(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericShlTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_shl"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericShlTest{{.*}}'
func genericShlTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_shl(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteSremTest{{.*}} : $@convention(thin) (Builtin.Vec4xInt32, Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xInt32, [[ARG1:%.*]] : $Builtin.Vec4xInt32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_srem"<Builtin.Vec4xInt32>([[ARG0]] : $Builtin.Vec4xInt32, [[ARG1]] : $Builtin.Vec4xInt32) : $Builtin.Vec4xInt32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteSremTest{{.*}}'
func concreteSremTest(_ x: Builtin.Vec4xInt32, _ y: Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return Builtin.generic_srem(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericSremTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_srem"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericSremTest{{.*}}'
func genericSremTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_srem(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteSubTest{{.*}} : $@convention(thin) (Builtin.Vec4xInt32, Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xInt32, [[ARG1:%.*]] : $Builtin.Vec4xInt32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_sub"<Builtin.Vec4xInt32>([[ARG0]] : $Builtin.Vec4xInt32, [[ARG1]] : $Builtin.Vec4xInt32) : $Builtin.Vec4xInt32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteSubTest{{.*}}'
func concreteSubTest(_ x: Builtin.Vec4xInt32, _ y: Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return Builtin.generic_sub(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericSubTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_sub"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericSubTest{{.*}}'
func genericSubTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_sub(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteUdivTest{{.*}} : $@convention(thin) (Builtin.Vec4xInt32, Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xInt32, [[ARG1:%.*]] : $Builtin.Vec4xInt32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_udiv"<Builtin.Vec4xInt32>([[ARG0]] : $Builtin.Vec4xInt32, [[ARG1]] : $Builtin.Vec4xInt32) : $Builtin.Vec4xInt32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteUdivTest{{.*}}'
func concreteUdivTest(_ x: Builtin.Vec4xInt32, _ y: Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return Builtin.generic_udiv(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericUdivTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_udiv"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericUdivTest{{.*}}'
func genericUdivTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_udiv(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteUdivExactTest{{.*}} : $@convention(thin) (Builtin.Vec4xInt32, Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xInt32, [[ARG1:%.*]] : $Builtin.Vec4xInt32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_udiv_exact"<Builtin.Vec4xInt32>([[ARG0]] : $Builtin.Vec4xInt32, [[ARG1]] : $Builtin.Vec4xInt32) : $Builtin.Vec4xInt32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteUdivExactTest{{.*}}'
func concreteUdivExactTest(_ x: Builtin.Vec4xInt32, _ y: Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return Builtin.generic_udiv_exact(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericUdivExactTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_udiv_exact"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericUdivExactTest{{.*}}'
func genericUdivExactTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_udiv_exact(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteXorTest{{.*}} : $@convention(thin) (Builtin.Vec4xInt32, Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xInt32, [[ARG1:%.*]] : $Builtin.Vec4xInt32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_xor"<Builtin.Vec4xInt32>([[ARG0]] : $Builtin.Vec4xInt32, [[ARG1]] : $Builtin.Vec4xInt32) : $Builtin.Vec4xInt32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteXorTest{{.*}}'
func concreteXorTest(_ x: Builtin.Vec4xInt32, _ y: Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return Builtin.generic_xor(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericXorTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_xor"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericXorTest{{.*}}'
func genericXorTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_xor(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteFaddTest{{.*}} : $@convention(thin) (Builtin.Vec4xFPIEEE32, Builtin.Vec4xFPIEEE32) -> Builtin.Vec4xFPIEEE32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xFPIEEE32, [[ARG1:%.*]] : $Builtin.Vec4xFPIEEE32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_fadd"<Builtin.Vec4xFPIEEE32>([[ARG0]] : $Builtin.Vec4xFPIEEE32, [[ARG1]] : $Builtin.Vec4xFPIEEE32) : $Builtin.Vec4xFPIEEE32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteFaddTest{{.*}}'
func concreteFaddTest(_ x: Builtin.Vec4xFPIEEE32, _ y: Builtin.Vec4xFPIEEE32) -> Builtin.Vec4xFPIEEE32 {
  return Builtin.generic_fadd(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericFaddTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_fadd"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericFaddTest{{.*}}'
func genericFaddTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_fadd(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteFdivTest{{.*}} : $@convention(thin) (Builtin.Vec4xFPIEEE32, Builtin.Vec4xFPIEEE32) -> Builtin.Vec4xFPIEEE32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xFPIEEE32, [[ARG1:%.*]] : $Builtin.Vec4xFPIEEE32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_fdiv"<Builtin.Vec4xFPIEEE32>([[ARG0]] : $Builtin.Vec4xFPIEEE32, [[ARG1]] : $Builtin.Vec4xFPIEEE32) : $Builtin.Vec4xFPIEEE32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteFdivTest{{.*}}'
func concreteFdivTest(_ x: Builtin.Vec4xFPIEEE32, _ y: Builtin.Vec4xFPIEEE32) -> Builtin.Vec4xFPIEEE32 {
  return Builtin.generic_fdiv(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericFdivTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_fdiv"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericFdivTest{{.*}}'
func genericFdivTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_fdiv(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteFmulTest{{.*}} : $@convention(thin) (Builtin.Vec4xFPIEEE32, Builtin.Vec4xFPIEEE32) -> Builtin.Vec4xFPIEEE32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xFPIEEE32, [[ARG1:%.*]] : $Builtin.Vec4xFPIEEE32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_fmul"<Builtin.Vec4xFPIEEE32>([[ARG0]] : $Builtin.Vec4xFPIEEE32, [[ARG1]] : $Builtin.Vec4xFPIEEE32) : $Builtin.Vec4xFPIEEE32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteFmulTest{{.*}}'
func concreteFmulTest(_ x: Builtin.Vec4xFPIEEE32, _ y: Builtin.Vec4xFPIEEE32) -> Builtin.Vec4xFPIEEE32 {
  return Builtin.generic_fmul(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericFmulTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_fmul"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericFmulTest{{.*}}'
func genericFmulTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_fmul(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteFremTest{{.*}} : $@convention(thin) (Builtin.Vec4xFPIEEE32, Builtin.Vec4xFPIEEE32) -> Builtin.Vec4xFPIEEE32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xFPIEEE32, [[ARG1:%.*]] : $Builtin.Vec4xFPIEEE32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_frem"<Builtin.Vec4xFPIEEE32>([[ARG0]] : $Builtin.Vec4xFPIEEE32, [[ARG1]] : $Builtin.Vec4xFPIEEE32) : $Builtin.Vec4xFPIEEE32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteFremTest{{.*}}'
func concreteFremTest(_ x: Builtin.Vec4xFPIEEE32, _ y: Builtin.Vec4xFPIEEE32) -> Builtin.Vec4xFPIEEE32 {
  return Builtin.generic_frem(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericFremTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_frem"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericFremTest{{.*}}'
func genericFremTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_frem(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteFsubTest{{.*}} : $@convention(thin) (Builtin.Vec4xFPIEEE32, Builtin.Vec4xFPIEEE32) -> Builtin.Vec4xFPIEEE32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xFPIEEE32, [[ARG1:%.*]] : $Builtin.Vec4xFPIEEE32):
// CHECK:   [[RESULT:%.*]] = builtin "generic_fsub"<Builtin.Vec4xFPIEEE32>([[ARG0]] : $Builtin.Vec4xFPIEEE32, [[ARG1]] : $Builtin.Vec4xFPIEEE32) : $Builtin.Vec4xFPIEEE32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteFsubTest{{.*}}'
func concreteFsubTest(_ x: Builtin.Vec4xFPIEEE32, _ y: Builtin.Vec4xFPIEEE32) -> Builtin.Vec4xFPIEEE32 {
  return Builtin.generic_fsub(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericFsubTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_fsub"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericFsubTest{{.*}}'
func genericFsubTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_fsub(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}concreteUremTest{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Int64, [[ARG1:%.*]] : $Builtin.Int64):
// CHECK:   [[RESULT:%.*]] = builtin "generic_urem"<Builtin.Int64>([[ARG0]] : $Builtin.Int64, [[ARG1]] : $Builtin.Int64) : $Builtin.Int64
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}concreteUremTest{{.*}}'
func concreteUremTest(_ x: Builtin.Int64, _ y: Builtin.Int64) -> Builtin.Int64 {
  return Builtin.generic_urem(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s20polymorphic_builtins{{.*}}genericUremTest{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
// CHECK:   [[STACK_SLOT_0:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG0]] to [init] [[STACK_SLOT_0]]
// CHECK:   [[STACK_SLOT_1:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [init] [[STACK_SLOT_1]]
// CHECK:   builtin "generic_urem"<T>([[RESULT]] : $*T, [[STACK_SLOT_0]] : $*T, [[STACK_SLOT_1]] : $*T) : $()
// CHECK: } // end sil function '$s20polymorphic_builtins{{.*}}genericUremTest{{.*}}'
func genericUremTest<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_urem(x, y)
}

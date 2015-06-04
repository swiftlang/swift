// RUN: %target-swift-frontend -emit-silgen %s > /tmp/x.sil
// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol Foo {
  static func staticFunc()
  func instanceFunc()
}

// CHECK-LABEL: sil hidden @_TF21partial_apply_generic14getStaticFunc1uRq_S_3Foo_FMq_FT_T_
func getStaticFunc1<T: Foo>(t: T.Type) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @_TZFP21partial_apply_generic3Foo10staticFuncuRq_S0__FMq_FT_T_
// CHECK-NEXT: apply [[REF]]<T>(%0)
  return t.staticFunc
// CHECK-NEXT: return
}

// CHECK-LABEL: sil shared @_TZFP21partial_apply_generic3Foo10staticFuncuRq_S0__FMq_FT_T_
// CHECK: [[REF:%.*]] = witness_method $Self, #Foo.staticFunc!1
// CHECK-NEXT: partial_apply [[REF]]<Self>(%0)
// CHECK-NEXT: return

// CHECK-LABEL: sil hidden @_TF21partial_apply_generic14getStaticFunc2uRq_S_3Foo_Fq_FT_T_
func getStaticFunc2<T: Foo>(t: T) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @_TZFP21partial_apply_generic3Foo10staticFuncuRq_S0__FMq_FT_T_
// CHECK: apply [[REF]]<T>
  return T.staticFunc
// CHECK-NEXT: destroy_addr %0 : $*T
// CHECK-NEXT: return
}

// CHECK-LABEL: sil hidden @_TF21partial_apply_generic16getInstanceFunc1uRq_S_3Foo_Fq_FT_T_
func getInstanceFunc1<T: Foo>(t: T) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @_TFP21partial_apply_generic3Foo12instanceFuncuRq_S0__Fq_FT_T_
// CHECK-NEXT: alloc_stack $T
// CHECK-NEXT: copy_addr %0 to [initialization]
// CHECK-NEXT: apply [[REF]]<T>
  return t.instanceFunc
// CHECK-NEXT: dealloc_stack
// CHECK-NEXT: destroy_addr %0 : $*T
// CHECK-NEXT: return
}

// CHECK-LABEL: sil shared @_TFP21partial_apply_generic3Foo12instanceFuncuRq_S0__Fq_FT_T_
// CHECK: [[REF:%.*]] = witness_method $Self, #Foo.instanceFunc!1
// CHECK-NEXT: partial_apply [[REF]]<Self>(%0)
// CHECK-NEXT: return

// CHECK-LABEL: sil hidden @_TF21partial_apply_generic16getInstanceFunc2uRq_S_3Foo_Fq_Fq_FT_T_
func getInstanceFunc2<T: Foo>(t: T) -> (T) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @_TFP21partial_apply_generic3Foo12instanceFuncuRq_S0__Fq_FT_T_
// CHECK-NEXT: partial_apply [[REF]]<T>(
  return T.instanceFunc
// CHECK-NEXT: destroy_addr %0 : $*
// CHECK-NEXT: return
}

// CHECK-LABEL: sil hidden @_TF21partial_apply_generic16getInstanceFunc3uRq_S_3Foo_FMq_Fq_FT_T_
func getInstanceFunc3<T: Foo>(t: T.Type) -> (T) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @_TFP21partial_apply_generic3Foo12instanceFuncuRq_S0__Fq_FT_T_
// CHECK-NEXT: partial_apply [[REF]]<T>(
  return t.instanceFunc
// CHECK-NEXT: return
}


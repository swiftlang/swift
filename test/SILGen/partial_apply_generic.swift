// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

protocol Foo {
  static func staticFunc()
  func instanceFunc()
}

// CHECK-LABEL: sil hidden @_TF21partial_apply_generic14getStaticFunc1
func getStaticFunc1<T: Foo>(t: T.Type) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @_TZFP21partial_apply_generic3Foo10staticFunc
// CHECK-NEXT: apply [[REF]]<T>(%0)
  return t.staticFunc
// CHECK-NEXT: return
}

// CHECK-LABEL: sil shared [thunk] @_TZFP21partial_apply_generic3Foo10staticFunc
// CHECK: [[REF:%.*]] = witness_method $Self, #Foo.staticFunc!1
// CHECK-NEXT: partial_apply [[REF]]<Self>(%0)
// CHECK-NEXT: return

// CHECK-LABEL: sil hidden @_TF21partial_apply_generic14getStaticFunc2
func getStaticFunc2<T: Foo>(t: T) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @_TZFP21partial_apply_generic3Foo10staticFunc
// CHECK: apply [[REF]]<T>
  return T.staticFunc
// CHECK-NEXT: destroy_addr %0 : $*T
// CHECK-NEXT: return
}

// CHECK-LABEL: sil hidden @_TF21partial_apply_generic16getInstanceFunc1
func getInstanceFunc1<T: Foo>(t: T) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @_TFP21partial_apply_generic3Foo12instanceFunc
// CHECK-NEXT: alloc_stack $T
// CHECK-NEXT: copy_addr %0 to [initialization]
// CHECK-NEXT: apply [[REF]]<T>
  return t.instanceFunc
// CHECK-NEXT: dealloc_stack
// CHECK-NEXT: destroy_addr %0 : $*T
// CHECK-NEXT: return
}

// CHECK-LABEL: sil shared [thunk] @_TFP21partial_apply_generic3Foo12instanceFunc
// CHECK: [[REF:%.*]] = witness_method $Self, #Foo.instanceFunc!1
// CHECK-NEXT: partial_apply [[REF]]<Self>(%0)
// CHECK-NEXT: return

// CHECK-LABEL: sil hidden @_TF21partial_apply_generic16getInstanceFunc2
func getInstanceFunc2<T: Foo>(t: T) -> (T) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @_TFP21partial_apply_generic3Foo12instanceFunc
// CHECK-NEXT: partial_apply [[REF]]<T>(
  return T.instanceFunc
// CHECK-NEXT: destroy_addr %0 : $*
// CHECK-NEXT: return
}

// CHECK-LABEL: sil hidden @_TF21partial_apply_generic16getInstanceFunc3
func getInstanceFunc3<T: Foo>(t: T.Type) -> (T) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @_TFP21partial_apply_generic3Foo12instanceFunc
// CHECK-NEXT: partial_apply [[REF]]<T>(
  return t.instanceFunc
// CHECK-NEXT: return
}


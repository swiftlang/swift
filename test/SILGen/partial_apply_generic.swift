// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

protocol Foo {
  static func staticFunc()
  func instanceFunc()
}

// CHECK-LABEL: sil hidden @_T021partial_apply_generic14getStaticFunc1{{[_0-9a-zA-Z]*}}F
func getStaticFunc1<T: Foo>(t: T.Type) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @_T021partial_apply_generic3FooP10staticFunc{{[_0-9a-zA-Z]*}}FZ
// CHECK-NEXT: apply [[REF]]<T>(%0)
  return t.staticFunc
// CHECK-NEXT: return
}

// CHECK-LABEL: sil shared [thunk] @_T021partial_apply_generic3FooP10staticFunc{{[_0-9a-zA-Z]*}}FZ
// CHECK: [[REF:%.*]] = witness_method $Self, #Foo.staticFunc!1
// CHECK-NEXT: partial_apply [[REF]]<Self>(%0)
// CHECK-NEXT: return

// CHECK-LABEL: sil hidden @_T021partial_apply_generic14getStaticFunc2{{[_0-9a-zA-Z]*}}F
func getStaticFunc2<T: Foo>(t: T) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @_T021partial_apply_generic3FooP10staticFunc{{[_0-9a-zA-Z]*}}FZ
// CHECK: apply [[REF]]<T>
  return T.staticFunc
// CHECK-NEXT: destroy_addr %0 : $*T
// CHECK-NEXT: return
}

// CHECK-LABEL: sil hidden @_T021partial_apply_generic16getInstanceFunc1{{[_0-9a-zA-Z]*}}F
func getInstanceFunc1<T: Foo>(t: T) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @_T021partial_apply_generic3FooP12instanceFunc{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT: alloc_stack $T
// CHECK-NEXT: copy_addr %0 to [initialization]
// CHECK-NEXT: apply [[REF]]<T>
  return t.instanceFunc
// CHECK-NEXT: dealloc_stack
// CHECK-NEXT: destroy_addr %0 : $*T
// CHECK-NEXT: return
}

// CHECK-LABEL: sil shared [thunk] @_T021partial_apply_generic3FooP12instanceFunc{{[_0-9a-zA-Z]*}}F
// CHECK: [[REF:%.*]] = witness_method $Self, #Foo.instanceFunc!1
// CHECK-NEXT: partial_apply [[REF]]<Self>(%0)
// CHECK-NEXT: return

// CHECK-LABEL: sil hidden @_T021partial_apply_generic16getInstanceFunc2{{[_0-9a-zA-Z]*}}F
func getInstanceFunc2<T: Foo>(t: T) -> (T) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @_T021partial_apply_generic3FooP12instanceFunc{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT: partial_apply [[REF]]<T>(
  return T.instanceFunc
// CHECK-NEXT: destroy_addr %0 : $*
// CHECK-NEXT: return
}

// CHECK-LABEL: sil hidden @_T021partial_apply_generic16getInstanceFunc3{{[_0-9a-zA-Z]*}}F
func getInstanceFunc3<T: Foo>(t: T.Type) -> (T) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @_T021partial_apply_generic3FooP12instanceFunc{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT: partial_apply [[REF]]<T>(
  return t.instanceFunc
// CHECK-NEXT: return
}


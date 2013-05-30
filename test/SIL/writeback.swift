// RUN: %swift -emit-sil %s | FileCheck %s

struct Foo {
  func foo() {}
}

var x : Foo {
  get: return Foo()
  set:
}

var y : Foo {
  get: return Foo()
  set:
}

var z : Foo {
  get: return Foo()
  set:
}

var readonly : Foo {
  get: return Foo()
}

func bar(x:[byref] Foo) {}
func bas(x:[byref] Foo)(y:[byref] Foo) {}
func zim(x:[byref] Foo)(y:[byref] Foo) -> (z:[byref] Foo) -> () {
  return {|z:[byref] Foo| }
}

// Writeback to value type 'this' argument
x.foo()
// CHECK: [[FOO:%.*]] = function_ref $[cc(method), thin] ((), [byref] Foo) -> (), @_TV9writeback3Foo3foofRS0_FT_T_
// CHECK: [[GET_X:%.*]] = function_ref $[thin] () -> Foo, @_T9writeback1xVS_3Foog
// CHECK: [[X:%.*]] = apply [[GET_X]]() : $[thin] () -> Foo
// CHECK: [[X_TEMP:%.*]] = alloc_var stack $Foo
// CHECK: store [[X]] to [[X_TEMP]]
// CHECK: apply [[FOO]]([[X_TEMP]]) : $[cc(method), thin] ((), [byref] Foo) -> ()
// CHECK: [[X1:%.*]] = load [[X_TEMP]] : $*Foo
// CHECK: [[SET_X:%.*]] = function_ref $[thin] (value : Foo) -> (), @_T9writeback1xVS_3Foos
// CHECK: apply [[SET_X]]([[X1]]) : $[thin] (value : Foo) -> ()
// CHECK: dealloc_var stack [[X_TEMP]] : $*Foo

// Writeback to byref argument
bar(&x)
// CHECK: [[BAR:%.*]] = function_ref $[thin] (x : [byref] Foo) -> (), @_T9writeback3barFT1xRVS_3Foo_T_
// CHECK: [[GET_X:%.*]] = function_ref $[thin] () -> Foo, @_T9writeback1xVS_3Foog
// CHECK: [[X:%.*]] = apply [[GET_X]]() : $[thin] () -> Foo
// CHECK: [[X_TEMP:%.*]] = alloc_var stack $Foo
// CHECK: store [[X]] to [[X_TEMP]] : $*Foo
// CHECK: apply [[BAR]]([[X_TEMP]]) : $[thin] (x : [byref] Foo) -> ()
// CHECK: [[X1:%.*]] = load [[X_TEMP]] : $*Foo
// CHECK: [[SET_X:%.*]] = function_ref $[thin] (value : Foo) -> (), @_T9writeback1xVS_3Foos
// CHECK: apply [[SET_X]]([[X1]]) : $[thin] (value : Foo) -> ()
// CHECK: dealloc_var stack [[X_TEMP]] : $*Foo

// Writeback to curried arguments
bas(&x)(&y)
// CHECK: [[BAS:%.*]] = function_ref $[thin] ((y : [byref] Foo), (x : [byref] Foo)) -> (), @_T9writeback3basfT1xRVS_3Foo_FT1yRS0__T_
// CHECK: [[GET_X:%.*]] = function_ref $[thin] () -> Foo, @_T9writeback1xVS_3Foog
// CHECK: {{%.*}} = apply [[GET_X]]() : $[thin] () -> Foo
// CHECK: [[GET_Y:%.*]] = function_ref $[thin] () -> Foo, @_T9writeback1yVS_3Foog
// CHECK: {{%.*}} = apply [[GET_Y]]() : $[thin] () -> Foo
// CHECK: apply [[BAS]]({{%.*}}, {{%.*}}) : $[thin] ((y : [byref] Foo), (x : [byref] Foo)) -> ()
// CHECK: [[SET_Y:%.*]] = function_ref $[thin] (value : Foo) -> (), @_T9writeback1yVS_3Foos
// CHECK: apply [[SET_Y]]({{%.*}}) : $[thin] (value : Foo) -> ()
// CHECK: [[SET_X:%.*]] = function_ref $[thin] (value : Foo) -> (), @_T9writeback1xVS_3Foos
// CHECK: apply [[SET_X]]({{%.*}}) : $[thin] (value : Foo) -> ()

// Writeback to curried arguments to function that returns a function
zim(&x)(&y)(&z)
// CHECK: [[ZIM:%.*]] = function_ref $[thin] ((y : [byref] Foo), (x : [byref] Foo)) -> (z : [byref] Foo) -> (), @_T9writeback3zimfT1xRVS_3Foo_FT1yRS0__FT1zRS0__T_
// CHECK: [[GET_X:%.*]] = function_ref $[thin] () -> Foo, @_T9writeback1xVS_3Foog
// CHECK: {{%.*}} = apply [[GET_X]]() : $[thin] () -> Foo
// CHECK: [[GET_Y:%.*]] = function_ref $[thin] () -> Foo, @_T9writeback1yVS_3Foog
// CHECK: {{%.*}} = apply [[GET_Y]]() : $[thin] () -> Foo
// CHECK: [[ZIM2:%.*]] = apply [[ZIM]]({{%.*}}, {{%.*}}) : $[thin] ((y : [byref] Foo), (x : [byref] Foo)) -> (z : [byref] Foo) -> ()
// CHECK: [[SET_Y:%.*]] = function_ref $[thin] (value : Foo) -> (), @_T9writeback1yVS_3Foos
// CHECK: apply [[SET_Y]]({{%.*}}) : $[thin] (value : Foo) -> ()
// CHECK: [[SET_X:%.*]] = function_ref $[thin] (value : Foo) -> (), @_T9writeback1xVS_3Foos
// CHECK: apply [[SET_X]]({{%.*}}) : $[thin] (value : Foo) -> ()
// CHECK: [[GET_Z:%.*]] = function_ref $[thin] () -> Foo, @_T9writeback1zVS_3Foog
// CHECK: {{%.*}} = apply [[GET_Z]]() : $[thin] () -> Foo
// CHECK: apply [[ZIM2]]({{%.*}}) : $(z : [byref] Foo) -> ()
// CHECK: [[SET_Z:%.*]] = function_ref $[thin] (value : Foo) -> (), @_T9writeback1zVS_3Foos
// CHECK: apply [[SET_Z]]({{%.*}}) : $[thin] (value : Foo) -> ()

// No writeback for readonly 'this' argument
readonly.foo()
// CHECK: [[FOO:%.*]] = function_ref $[cc(method), thin] ((), [byref] Foo) -> (), @_TV9writeback3Foo3foofRS0_FT_T_
// CHECK: [[GET_READONLY:%.*]] = function_ref $[thin] () -> Foo, @_T9writeback8readonlyVS_3Foog
// CHECK: apply [[FOO]]({{%.*}}) : $[cc(method), thin] ((), [byref] Foo) -> ()
// CHECK-NOT: load
// CHECK-NOT: @_T9writeback8readonlyVS_3Foos

func zang(x:Foo) {}

// No writeback for pass-by-value argument
zang(x)
// CHECK:  function_ref $[thin] (x : Foo) -> (), @_T9writeback4zangFT1xVS_3Foo_T_
// CHECK-NOT: @_T9writeback1xVS_3Foos
zang(readonly)
// CHECK:  function_ref $[thin] (x : Foo) -> (), @_T9writeback4zangFT1xVS_3Foo_T_
// CHECK-NOT: @_T9writeback8readonlyVS_3Foos

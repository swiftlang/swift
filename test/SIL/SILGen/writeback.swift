// RUN: %swift -emit-sil %s | FileCheck %s

struct Foo {
  func foo() {}

  subscript(x:Int) -> Foo {
    get: return Foo()
    set:
  }
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
// CHECK: [[FOO:%.*]] = function_ref @_TV9writeback3Foo3foofRS0_FT_T_ : $[cc(method), thin] ((), [byref] Foo) -> ()
// CHECK: [[GET_X:%.*]] = function_ref @_T9writeback1xVS_3Foog : $[thin] () -> Foo
// CHECK: [[X:%.*]] = apply [[GET_X]]() : $[thin] () -> Foo
// CHECK: [[X_TEMP:%.*]] = alloc_var stack $Foo
// CHECK: store [[X]] to [[X_TEMP]]
// CHECK: apply [[FOO]]([[X_TEMP]]) : $[cc(method), thin] ((), [byref] Foo) -> ()
// CHECK: [[X1:%.*]] = load [[X_TEMP]] : $*Foo
// CHECK: [[SET_X:%.*]] = function_ref @_T9writeback1xVS_3Foos : $[thin] (value : Foo) -> ()
// CHECK: apply [[SET_X]]([[X1]]) : $[thin] (value : Foo) -> ()
// CHECK: dealloc_var stack [[X_TEMP]] : $*Foo

// Writeback to byref argument
bar(&x)
// CHECK: [[BAR:%.*]] = function_ref @_T9writeback3barFT1xRVS_3Foo_T_ : $[thin] (x : [byref] Foo) -> ()
// CHECK: [[GET_X:%.*]] = function_ref @_T9writeback1xVS_3Foog : $[thin] () -> Foo
// CHECK: [[X:%.*]] = apply [[GET_X]]() : $[thin] () -> Foo
// CHECK: [[X_TEMP:%.*]] = alloc_var stack $Foo
// CHECK: store [[X]] to [[X_TEMP]] : $*Foo
// CHECK: apply [[BAR]]([[X_TEMP]]) : $[thin] (x : [byref] Foo) -> ()
// CHECK: [[X1:%.*]] = load [[X_TEMP]] : $*Foo
// CHECK: [[SET_X:%.*]] = function_ref @_T9writeback1xVS_3Foos : $[thin] (value : Foo) -> ()
// CHECK: apply [[SET_X]]([[X1]]) : $[thin] (value : Foo) -> ()
// CHECK: dealloc_var stack [[X_TEMP]] : $*Foo

// Writeback to curried arguments
bas(&x)(&y)
// CHECK: [[BAS:%.*]] = function_ref @_T9writeback3basfT1xRVS_3Foo_FT1yRS0__T_ : $[thin] ((y : [byref] Foo), (x : [byref] Foo)) -> ()
// CHECK: [[GET_X:%.*]] = function_ref @_T9writeback1xVS_3Foog : $[thin] () -> Foo
// CHECK: {{%.*}} = apply [[GET_X]]() : $[thin] () -> Foo
// CHECK: [[GET_Y:%.*]] = function_ref @_T9writeback1yVS_3Foog : $[thin] () -> Foo
// CHECK: {{%.*}} = apply [[GET_Y]]() : $[thin] () -> Foo
// CHECK: apply [[BAS]]({{%.*}}, {{%.*}}) : $[thin] ((y : [byref] Foo), (x : [byref] Foo)) -> ()
// CHECK: [[SET_Y:%.*]] = function_ref @_T9writeback1yVS_3Foos : $[thin] (value : Foo) -> ()
// CHECK: apply [[SET_Y]]({{%.*}}) : $[thin] (value : Foo) -> ()
// CHECK: [[SET_X:%.*]] = function_ref @_T9writeback1xVS_3Foos : $[thin] (value : Foo) -> ()
// CHECK: apply [[SET_X]]({{%.*}}) : $[thin] (value : Foo) -> ()

// Writeback to curried arguments to function that returns a function
zim(&x)(&y)(&z)
// CHECK: [[ZIM:%.*]] = function_ref @_T9writeback3zimfT1xRVS_3Foo_FT1yRS0__FT1zRS0__T_ : $[thin] ((y : [byref] Foo), (x : [byref] Foo)) -> (z : [byref] Foo) -> ()
// CHECK: [[GET_X:%.*]] = function_ref @_T9writeback1xVS_3Foog : $[thin] () -> Foo
// CHECK: {{%.*}} = apply [[GET_X]]() : $[thin] () -> Foo
// CHECK: [[GET_Y:%.*]] = function_ref @_T9writeback1yVS_3Foog : $[thin] () -> Foo
// CHECK: {{%.*}} = apply [[GET_Y]]() : $[thin] () -> Foo
// CHECK: [[ZIM2:%.*]] = apply [[ZIM]]({{%.*}}, {{%.*}}) : $[thin] ((y : [byref] Foo), (x : [byref] Foo)) -> (z : [byref] Foo) -> ()
// CHECK: [[SET_Y:%.*]] = function_ref @_T9writeback1yVS_3Foos : $[thin] (value : Foo) -> ()
// CHECK: apply [[SET_Y]]({{%.*}}) : $[thin] (value : Foo) -> ()
// CHECK: [[SET_X:%.*]] = function_ref @_T9writeback1xVS_3Foos : $[thin] (value : Foo) -> ()
// CHECK: apply [[SET_X]]({{%.*}}) : $[thin] (value : Foo) -> ()
// CHECK: [[GET_Z:%.*]] = function_ref @_T9writeback1zVS_3Foog : $[thin] () -> Foo
// CHECK: {{%.*}} = apply [[GET_Z]]() : $[thin] () -> Foo
// CHECK: apply [[ZIM2]]({{%.*}}) : $(z : [byref] Foo) -> ()
// CHECK: [[SET_Z:%.*]] = function_ref @_T9writeback1zVS_3Foos : $[thin] (value : Foo) -> ()
// CHECK: apply [[SET_Z]]({{%.*}}) : $[thin] (value : Foo) -> ()

// No writeback for readonly 'this' argument
readonly.foo()
// CHECK: [[FOO:%.*]] = function_ref @_TV9writeback3Foo3foofRS0_FT_T_ : $[cc(method), thin] ((), [byref] Foo) -> ()
// CHECK: [[GET_READONLY:%.*]] = function_ref @_T9writeback8readonlyVS_3Foog : $[thin] () -> Foo
// CHECK: apply [[FOO]]({{%.*}}) : $[cc(method), thin] ((), [byref] Foo) -> ()
// CHECK-NOT: load
// CHECK-NOT: @_T9writeback8readonlyVS_3Foos

func zang(x:Foo) {}

// No writeback for pass-by-value argument
zang(x)
// CHECK:  function_ref @_T9writeback4zangFT1xVS_3Foo_T_ : $[thin] (x : Foo) -> ()
// CHECK-NOT: @_T9writeback1xVS_3Foos
zang(readonly)
// CHECK:  function_ref @_T9writeback4zangFT1xVS_3Foo_T_ : $[thin] (x : Foo) -> ()
// CHECK-NOT: @_T9writeback8readonlyVS_3Foos

func zung() -> Int { return 0 }

// Ensure that subscripts are only evaluated once.
bar(&x[zung()])
// CHECK: [[BAR:%.*]] = function_ref @_T9writeback3barFT1xRVS_3Foo_T_ : $[thin] (x : [byref] Foo) -> ()
// CHECK: [[GET_X:%.*]] = function_ref @_T9writeback1xVS_3Foog : $[thin] () -> Foo
// CHECK: [[ZUNG:%.*]] = function_ref @_T9writeback4zungFT_Si : $[thin] () -> Int64
// CHECK: [[INDEX:%.*]] = apply [[ZUNG]]() : $[thin] () -> Int64
// CHECK: [[GET_SUBSCRIPT:%.*]] = function_ref @_TV9writeback3Foo11__subscriptFT1xSi_S0_g : $[cc(method), thin] ((), (x : Int64), [byref] Foo) -> Foo
// CHECK: apply [[GET_SUBSCRIPT]]([[INDEX]], {{%.*}}) : $[cc(method), thin] ((), (x : Int64), [byref] Foo) -> Foo
// CHECK: apply [[BAR]]({{%.*}}) : $[thin] (x : [byref] Foo) -> ()
// CHECK: [[SET_SUBSCRIPT:%.*]] = function_ref @_TV9writeback3Foo11__subscriptFT1xSi_S0_s : $[cc(method), thin] ((value : Foo), (x : Int64), [byref] Foo) -> ()
// CHECK: apply [[SET_SUBSCRIPT]]({{%.*}}, [[INDEX]], {{%.*}}) : $[cc(method), thin] ((value : Foo), (x : Int64), [byref] Foo) -> ()
// CHECK: function_ref @_T9writeback1xVS_3Foos : $[thin] (value : Foo) -> ()

protocol Fungible {}
extension Foo : Fungible {}

var addressOnly : Fungible {
  get: return Foo()
  set:
}

func funge(x:[byref] Fungible) {}

funge(&addressOnly)
// CHECK: [[FUNGE:%.*]] = function_ref @_T9writeback5fungeFT1xRPS_8Fungible__T_ : $[thin] (x : [byref] Fungible) -> ()
// CHECK: [[GET:%.*]] = function_ref @_T9writeback11addressOnlyPS_8Fungible_g : $[thin] () -> Fungible
// CHECK: [[TEMP:%.*]] = alloc_var stack $Fungible
// CHECK: apply [[GET]]([[TEMP]]) : $[thin] () -> Fungible
// CHECK: apply [[FUNGE]]([[TEMP]]) : $[thin] (x : [byref] Fungible) -> ()
// CHECK: [[SET:%.*]] = function_ref @_T9writeback11addressOnlyPS_8Fungible_s : $[thin] (value : Fungible) -> ()
// CHECK: apply [[SET]]([[TEMP]]) : $[thin] (value : Fungible) -> ()
// CHECK: dealloc_var stack [[TEMP]] : $*Fungible

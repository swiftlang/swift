// RUN: %target-swift-emit-silgen -enable-sil-ownership -Xllvm -sil-full-demangle %s | %FileCheck %s

struct Foo {
  mutating           // used to test writeback.
  func foo() {}

  subscript(x: Int) -> Foo {
    get {
      return Foo()
    }
    set {}
  }
}

var x: Foo {
  get {
    return Foo()
  }
  set {}
}

var y: Foo {
  get {
    return Foo()
  }
  set {}
}

var z: Foo {
  get {
    return Foo()
  }
  set {}
}

var readonly: Foo {
  get {
    return Foo()
  }
}

func bar(x x: inout Foo) {}

// Writeback to value type 'self' argument
x.foo()
// CHECK: [[X_TEMP:%.*]] = alloc_stack $Foo
// CHECK: [[GET_X:%.*]] = function_ref @$s9writeback1xAA3FooVvg : $@convention(thin) () -> Foo
// CHECK: [[X:%.*]] = apply [[GET_X]]() : $@convention(thin) () -> Foo
// CHECK: store [[X]] to [trivial] [[X_TEMP]]
// CHECK: [[FOO:%.*]] = function_ref @$s9writeback3FooV3foo{{[_0-9a-zA-Z]*}}F : $@convention(method) (@inout Foo) -> ()
// CHECK: apply [[FOO]]([[X_TEMP]]) : $@convention(method) (@inout Foo) -> ()
// CHECK: [[X1:%.*]] = load [trivial] [[X_TEMP]] : $*Foo
// CHECK: [[SET_X:%.*]] = function_ref @$s9writeback1xAA3FooVvs : $@convention(thin) (Foo) -> ()
// CHECK: apply [[SET_X]]([[X1]]) : $@convention(thin) (Foo) -> ()
// CHECK: dealloc_stack [[X_TEMP]] : $*Foo

// Writeback to inout argument
bar(x: &x)
// CHECK: [[X_TEMP:%.*]] = alloc_stack $Foo
// CHECK: [[GET_X:%.*]] = function_ref @$s9writeback1xAA3FooVvg : $@convention(thin) () -> Foo
// CHECK: [[X:%.*]] = apply [[GET_X]]() : $@convention(thin) () -> Foo
// CHECK: store [[X]] to [trivial] [[X_TEMP]] : $*Foo
// CHECK: [[BAR:%.*]] = function_ref @$s9writeback3bar1xyAA3FooVz_tF : $@convention(thin) (@inout Foo) -> ()
// CHECK: apply [[BAR]]([[X_TEMP]]) : $@convention(thin) (@inout Foo) -> ()
// CHECK: [[X1:%.*]] = load [trivial] [[X_TEMP]] : $*Foo
// CHECK: [[SET_X:%.*]] = function_ref @$s9writeback1xAA3FooVvs : $@convention(thin) (Foo) -> ()
// CHECK: apply [[SET_X]]([[X1]]) : $@convention(thin) (Foo) -> ()
// CHECK: dealloc_stack [[X_TEMP]] : $*Foo

func zang(x x: Foo) {}

// No writeback for pass-by-value argument
zang(x: x)
// CHECK:  function_ref @$s9writeback4zang1xyAA3FooV_tF : $@convention(thin) (Foo) -> ()
// CHECK-NOT: @$s9writeback1xAA3FooVvs
zang(x: readonly)
// CHECK:  function_ref @$s9writeback4zang1xyAA3FooV_tF : $@convention(thin) (Foo) -> ()
// CHECK-NOT: @$s9writeback8readonlyAA3FooVvs

func zung() -> Int { return 0 }

// Ensure that subscripts are only evaluated once.
bar(x: &x[zung()])
// CHECK: [[ZUNG:%.*]] = function_ref @$s9writeback4zungSiyF : $@convention(thin) () -> Int
// CHECK: [[INDEX:%.*]] = apply [[ZUNG]]() : $@convention(thin) () -> Int
// CHECK: [[GET_X:%.*]] = function_ref @$s9writeback1xAA3FooVvg : $@convention(thin) () -> Foo
// CHECK: [[GET_SUBSCRIPT:%.*]] = function_ref @$s9writeback3FooV{{[_0-9a-zA-Z]*}}ig : $@convention(method) (Int, Foo) -> Foo
// CHECK: apply [[GET_SUBSCRIPT]]([[INDEX]], {{%.*}}) : $@convention(method) (Int, Foo) -> Foo
// CHECK: [[BAR:%.*]] = function_ref @$s9writeback3bar1xyAA3FooVz_tF : $@convention(thin) (@inout Foo) -> ()
// CHECK: apply [[BAR]]({{%.*}}) : $@convention(thin) (@inout Foo) -> ()
// CHECK: [[SET_SUBSCRIPT:%.*]] = function_ref @$s9writeback3FooV{{[_0-9a-zA-Z]*}}is : $@convention(method) (Foo, Int, @inout Foo) -> ()
// CHECK: apply [[SET_SUBSCRIPT]]({{%.*}}, [[INDEX]], {{%.*}}) : $@convention(method) (Foo, Int, @inout Foo) -> ()
// CHECK: function_ref @$s9writeback1xAA3FooVvs : $@convention(thin) (Foo) -> ()

protocol Fungible {}
extension Foo : Fungible {}

var addressOnly: Fungible {
  get {
    return Foo()
  }
  set {}
}

func funge(x x: inout Fungible) {}

funge(x: &addressOnly)
// CHECK: [[TEMP:%.*]] = alloc_stack $Fungible
// CHECK: [[GET:%.*]] = function_ref @$s9writeback11addressOnlyAA8Fungible_pvg : $@convention(thin) () -> @out Fungible
// CHECK: apply [[GET]]([[TEMP]]) : $@convention(thin) () -> @out Fungible
// CHECK: [[FUNGE:%.*]] = function_ref @$s9writeback5funge1xyAA8Fungible_pz_tF : $@convention(thin) (@inout Fungible) -> ()
// CHECK: apply [[FUNGE]]([[TEMP]]) : $@convention(thin) (@inout Fungible) -> ()
// CHECK: [[SET:%.*]] = function_ref @$s9writeback11addressOnlyAA8Fungible_pvs : $@convention(thin) (@in Fungible) -> ()
// CHECK: apply [[SET]]([[TEMP]]) : $@convention(thin) (@in Fungible) -> ()
// CHECK: dealloc_stack [[TEMP]] : $*Fungible

// Test that writeback occurs with generic properties.
// <rdar://problem/16525257> 

protocol Runcible {
  associatedtype Frob: Frobable

  var frob: Frob { get set }
}

protocol Frobable {
  associatedtype Anse
  
  var anse: Anse { get set }
}

// CHECK-LABEL: sil hidden @$s9writeback12test_generic{{[_0-9a-zA-Z]*}}F 
// CHECK:         witness_method $Runce, #Runcible.frob!modify.1
// CHECK:         witness_method $Runce.Frob, #Frobable.anse!setter.1
func test_generic<Runce: Runcible>(runce runce: inout Runce, anse: Runce.Frob.Anse) {
  runce.frob.anse = anse
}

// We should *not* write back when referencing decls or members as rvalues.
// <rdar://problem/16530235>
// CHECK-LABEL: sil hidden @$s9writeback15loadAddressOnlyAA8Fungible_pyF : $@convention(thin) () -> @out Fungible {
func loadAddressOnly() -> Fungible {
  // CHECK:       function_ref writeback.addressOnly.getter
  // CHECK-NOT:   function_ref writeback.addressOnly.setter
  return addressOnly
}

// CHECK-LABEL: sil hidden @$s9writeback10loadMember{{[_0-9a-zA-Z]*}}F
// CHECK:         witness_method $Runce, #Runcible.frob!getter.1
// CHECK:         witness_method $Runce.Frob, #Frobable.anse!getter.1
// CHECK-NOT:     witness_method $Runce.Frob, #Frobable.anse!setter.1
// CHECK-NOT:     witness_method $Runce, #Runcible.frob!setter.1
func loadMember<Runce: Runcible>(runce runce: Runce) -> Runce.Frob.Anse {
  return runce.frob.anse
}


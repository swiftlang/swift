// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path %t/clang-module-cache -target x86_64-apple-macosx10.9 -sdk %S/Inputs -I %S/Inputs -enable-source-import -enable-dynamic %s -emit-silgen | FileCheck %s

import Foundation
import gizmo

class Foo: Proto {
  // Not objc or dynamic, so only a vtable entry
  init(native: Int) {}
  func nativeMethod() {}
  var nativeProp: Int
  subscript(#native: Int) -> Int {
    get { return native }
    set {}
  }

  // @objc, so it has an ObjC entry point but can also be dispatched
  // by vtable
  @objc init(objc: Int) {}
  @objc func objcMethod() {}
  @objc var objcProp: Int
  @objc subscript(#objc: Int) -> Int {
    get { return objc }
    set {}
  }

  // dynamic, so it has only an ObjC entry point
  dynamic init(dynamic: Int) {}
  dynamic func dynamicMethod() {}
  dynamic var dynamicProp: Int
  dynamic subscript(#dynamic: Int) -> Int {
    get { return dynamic }
    set {}
  }
}

protocol Proto {
  func nativeMethod()
  var nativeProp: Int { get set }
  subscript(#native: Int) -> Int { get set }

  func objcMethod()
  var objcProp: Int { get set }
  subscript(#objc: Int) -> Int { get set }

  func dynamicMethod()
  var dynamicProp: Int { get set }
  subscript(#dynamic: Int) -> Int { get set }
}

// ObjC entry points for @objc and dynamic entry points

// normal and @objc initializing ctors can be statically dispatched
// CHECK-LABEL: sil @_TFC7dynamic3FooCfMS0_FT6nativeSi_S0_
// CHECK:         function_ref @_TFC7dynamic3FoocfMS0_FT6nativeSi_S0_

// CHECK-LABEL: sil @_TFC7dynamic3FooCfMS0_FT4objcSi_S0_
// CHECK:         function_ref @_TFC7dynamic3FoocfMS0_FT4objcSi_S0_

// CHECK-LABEL: sil @_TToFC7dynamic3FoocfMS0_FT4objcSi_S0_
// CHECK-LABEL: sil @_TToFC7dynamic3Foo10objcMethodfS0_FT_T_
// CHECK-LABEL: sil [transparent] @_TToFC7dynamic3Foog8objcPropSi
// CHECK-LABEL: sil [transparent] @_TToFC7dynamic3Foos8objcPropSi
// CHECK-LABEL: sil @_TToFC7dynamic3Foog9subscriptFT4objcSi_Si
// CHECK-LABEL: sil @_TToFC7dynamic3Foos9subscriptFT4objcSi_Si

// TODO: dynamic initializing ctor must be objc dispatched
// CHECK-LABEL: sil @_TFC7dynamic3FooCfMS0_FT7dynamicSi_S0_
// CHECK:         function_ref @_TFC7dynamic3FoocfMS0_FT7dynamicSi_S0__dynamic
// CHECK-LABEL: sil private [transparent] @_TFC7dynamic3FoocfMS0_FT7dynamicSi_S0__dynamic
// CHECK:         class_method [volatile] %1 : $Foo, #Foo.init!initializer.1.foreign :

// CHECK-LABEL: sil @_TToFC7dynamic3FoocfMS0_FT7dynamicSi_S0_
// CHECK-LABEL: sil @_TToFC7dynamic3Foo13dynamicMethodfS0_FT_T_
// CHECK-LABEL: sil [transparent] @_TToFC7dynamic3Foog11dynamicPropSi
// CHECK-LABEL: sil [transparent] @_TToFC7dynamic3Foos11dynamicPropSi
// CHECK-LABEL: sil @_TToFC7dynamic3Foog9subscriptFT7dynamicSi_Si
// CHECK-LABEL: sil @_TToFC7dynamic3Foos9subscriptFT7dynamicSi_Si

// Protocol witnesses use best appropriate dispatch

// Native witnesses use vtable dispatch:
// CHECK-LABEL: sil @_TTWC7dynamic3FooS_5ProtoFS1_12nativeMethodUS1___fRQPS1_FT_T_
// CHECK:         class_method {{%.*}} : $Foo, #Foo.nativeMethod!1 :
// CHECK-LABEL: sil @_TTWC7dynamic3FooS_5ProtoFS1_g10nativePropSi
// CHECK:         class_method {{%.*}} : $Foo, #Foo.nativeProp!getter.1 :
// CHECK-LABEL: sil @_TTWC7dynamic3FooS_5ProtoFS1_s10nativePropSi
// CHECK:         class_method {{%.*}} : $Foo, #Foo.nativeProp!setter.1 :
// CHECK-LABEL: sil @_TTWC7dynamic3FooS_5ProtoFS1_g9subscriptFT6nativeSi_Si
// CHECK:         class_method {{%.*}} : $Foo, #Foo.subscript!getter.1 :
// CHECK-LABEL: sil @_TTWC7dynamic3FooS_5ProtoFS1_s9subscriptFT6nativeSi_Si
// CHECK:         class_method {{%.*}} : $Foo, #Foo.subscript!setter.1 :

// @objc witnesses use vtable dispatch:
// CHECK-LABEL: sil @_TTWC7dynamic3FooS_5ProtoFS1_10objcMethodUS1___fRQPS1_FT_T_
// CHECK:         class_method {{%.*}} : $Foo, #Foo.objcMethod!1 :
// CHECK-LABEL: sil @_TTWC7dynamic3FooS_5ProtoFS1_g8objcPropSi
// CHECK:         class_method {{%.*}} : $Foo, #Foo.objcProp!getter.1 :
// CHECK-LABEL: sil @_TTWC7dynamic3FooS_5ProtoFS1_s8objcPropSi
// CHECK:         class_method {{%.*}} : $Foo, #Foo.objcProp!setter.1 :
// CHECK-LABEL: sil @_TTWC7dynamic3FooS_5ProtoFS1_g9subscriptFT4objcSi_Si
// CHECK:         class_method {{%.*}} : $Foo, #Foo.subscript!getter.1 :
// CHECK-LABEL: sil @_TTWC7dynamic3FooS_5ProtoFS1_s9subscriptFT4objcSi_Si
// CHECK:         class_method {{%.*}} : $Foo, #Foo.subscript!setter.1 :

// Dynamic witnesses use objc dispatch:
// CHECK-LABEL: sil @_TTWC7dynamic3FooS_5ProtoFS1_13dynamicMethodUS1___fRQPS1_FT_T_
// CHECK:         function_ref @_TFC7dynamic3Foo13dynamicMethodfS0_FT_T__dynamic
// CHECK-LABEL: sil private [transparent] @_TFC7dynamic3Foo13dynamicMethodfS0_FT_T__dynamic
// CHECK:         class_method [volatile] {{%.*}} : $Foo, #Foo.dynamicMethod!1.foreign :

// CHECK-LABEL: sil @_TTWC7dynamic3FooS_5ProtoFS1_g11dynamicPropSi
// CHECK:         function_ref @_TFC7dynamic3Foog11dynamicPropSi_dynamic
// CHECK-LABEL: sil private [transparent] @_TFC7dynamic3Foog11dynamicPropSi_dynamic
// CHECK:         class_method [volatile] {{%.*}} : $Foo, #Foo.dynamicProp!getter.1.foreign :

// CHECK-LABEL: sil @_TTWC7dynamic3FooS_5ProtoFS1_s11dynamicPropSi
// CHECK:         function_ref @_TFC7dynamic3Foos11dynamicPropSi_dynamic
// CHECK-LABEL: sil private [transparent] @_TFC7dynamic3Foos11dynamicPropSi_dynamic
// CHECK:         class_method [volatile] {{%.*}} : $Foo, #Foo.dynamicProp!setter.1.foreign :

// CHECK-LABEL: sil @_TTWC7dynamic3FooS_5ProtoFS1_g9subscriptFT7dynamicSi_Si
// CHECK:         function_ref @_TFC7dynamic3Foog9subscriptFT7dynamicSi_Si_dynamic
// CHECK-LABEL: sil private [transparent] @_TFC7dynamic3Foog9subscriptFT7dynamicSi_Si_dynamic
// CHECK:         class_method [volatile] {{%.*}} : $Foo, #Foo.subscript!getter.1.foreign :

// CHECK-LABEL: sil @_TTWC7dynamic3FooS_5ProtoFS1_s9subscriptFT7dynamicSi_Si
// CHECK:         function_ref @_TFC7dynamic3Foos9subscriptFT7dynamicSi_Si_dynamic
// CHECK-LABEL: sil private [transparent] @_TFC7dynamic3Foos9subscriptFT7dynamicSi_Si_dynamic
// CHECK:         class_method [volatile] {{%.*}} : $Foo, #Foo.subscript!setter.1.foreign :

// CHECK-LABEL: sil @_TF7dynamic20nativeMethodDispatchFT_T_ : $@thin () -> ()
func nativeMethodDispatch() {
  // CHECK: function_ref @_TFC7dynamic3FooCfMS0_FT6nativeSi_S0_
  let c = Foo(native: 0)
  // CHECK: class_method {{%.*}} : $Foo, #Foo.nativeMethod!1 :
  c.nativeMethod()
  // CHECK: class_method {{%.*}} : $Foo, #Foo.nativeProp!getter.1 :
  let x = c.nativeProp
  // CHECK: class_method {{%.*}} : $Foo, #Foo.nativeProp!setter.1 :
  c.nativeProp = x
  // CHECK: class_method {{%.*}} : $Foo, #Foo.subscript!getter.1 :
  let y = c[native: 0]
  // CHECK: class_method {{%.*}} : $Foo, #Foo.subscript!setter.1 :
  c[native: 0] = y
}

// CHECK-LABEL: sil @_TF7dynamic18objcMethodDispatchFT_T_ : $@thin () -> ()
func objcMethodDispatch() {
  // CHECK: function_ref @_TFC7dynamic3FooCfMS0_FT4objcSi_S0_
  let c = Foo(objc: 0)
  // CHECK: class_method {{%.*}} : $Foo, #Foo.objcMethod!1 :
  c.objcMethod()
  // CHECK: class_method {{%.*}} : $Foo, #Foo.objcProp!getter.1 :
  let x = c.objcProp
  // CHECK: class_method {{%.*}} : $Foo, #Foo.objcProp!setter.1 :
  c.objcProp = x
  // CHECK: class_method {{%.*}} : $Foo, #Foo.subscript!getter.1 :
  let y = c[objc: 0]
  // CHECK: class_method {{%.*}} : $Foo, #Foo.subscript!setter.1 :
  c[objc: 0] = y
}

// CHECK-LABEL: sil @_TF7dynamic21dynamicMethodDispatchFT_T_ : $@thin () -> ()
func dynamicMethodDispatch() {
  // CHECK: function_ref @_TFC7dynamic3FooCfMS0_FT7dynamicSi_S0_
  let c = Foo(dynamic: 0)
  // CHECK: class_method [volatile] {{%.*}} : $Foo, #Foo.dynamicMethod!1.foreign 
  c.dynamicMethod()
  // CHECK: class_method [volatile] {{%.*}} : $Foo, #Foo.dynamicProp!getter.1.foreign
  let x = c.dynamicProp
  // CHECK: class_method [volatile] {{%.*}} : $Foo, #Foo.dynamicProp!setter.1.foreign
  c.dynamicProp = x
  // CHECK: class_method [volatile] {{%.*}} : $Foo, #Foo.subscript!getter.1.foreign
  let y = c[dynamic: 0]
  // CHECK: class_method [volatile] {{%.*}} : $Foo, #Foo.subscript!setter.1.foreign
  c[dynamic: 0] = y
}

// CHECK-LABEL: sil @_TF7dynamic21foreignMethodDispatchFT_T_
func foreignMethodDispatch() {
  // CHECK: function_ref @_TFCSo9GuisemeauCfMS_FT_S_
  let g = Guisemeau()
  // CHECK: class_method [volatile] {{%.*}} : $Gizmo, #Gizmo.frob!1.foreign
  g.frob()
  // CHECK: class_method [volatile] {{%.*}} : $Gizmo, #Gizmo.count!getter.1.foreign
  let x = g.count
  // CHECK: class_method [volatile] {{%.*}} : $Gizmo, #Gizmo.count!setter.1.foreign
  g.count = x
  // CHECK: class_method [volatile] {{%.*}} : $Guisemeau, #Guisemeau.subscript!getter.1.foreign
  let y = g[0]
  // CHECK: class_method [volatile] {{%.*}} : $Guisemeau, #Guisemeau.subscript!setter.1.foreign
  g[0] = y
}

// Vtable contains entries for native and @objc methods, but not dynamic ones
// CHECK-LABEL: sil_vtable Foo {
// CHECK-LABEL:   #Foo.init!initializer.1:   _TFC7dynamic3FoocfMS0_FT6nativeSi_S0_        // dynamic.Foo.init (dynamic.Foo.Type)(native : Swift.Int) -> dynamic.Foo
// CHECK-LABEL:   #Foo.nativeMethod!1:       _TFC7dynamic3Foo12nativeMethodfS0_FT_T_  // dynamic.Foo.nativeMethod (dynamic.Foo)() -> ()
// CHECK-LABEL:   #Foo.subscript!getter.1:   _TFC7dynamic3Foog9subscriptFT6nativeSi_Si    // dynamic.Foo.subscript.getter (native : Swift.Int) -> Swift.Int
// CHECK-LABEL:   #Foo.subscript!setter.1:   _TFC7dynamic3Foos9subscriptFT6nativeSi_Si    // dynamic.Foo.subscript.setter (native : Swift.Int) -> Swift.Int
// CHECK-LABEL:   #Foo.init!initializer.1:   _TFC7dynamic3FoocfMS0_FT4objcSi_S0_  // dynamic.Foo.init (dynamic.Foo.Type)(objc : Swift.Int) -> dynamic.Foo
// CHECK-LABEL:   #Foo.objcMethod!1:         _TFC7dynamic3Foo10objcMethodfS0_FT_T_      // dynamic.Foo.objcMethod (dynamic.Foo)() -> ()
// CHECK-LABEL:   #Foo.subscript!getter.1:   _TFC7dynamic3Foog9subscriptFT4objcSi_Si      // dynamic.Foo.subscript.getter (objc : Swift.Int) -> Swift.Int
// CHECK-LABEL:   #Foo.subscript!setter.1:   _TFC7dynamic3Foos9subscriptFT4objcSi_Si      // dynamic.Foo.subscript.setter (objc : Swift.Int) -> Swift.Int
// CHECK-LABEL:   #Foo.nativeProp!getter.1:  _TFC7dynamic3Foog10nativePropSi     // dynamic.Foo.nativeProp.getter : Swift.Int
// CHECK-LABEL:   #Foo.nativeProp!setter.1:  _TFC7dynamic3Foos10nativePropSi     // dynamic.Foo.nativeProp.setter : Swift.Int
// CHECK-LABEL:   #Foo.objcProp!getter.1:    _TFC7dynamic3Foog8objcPropSi  // dynamic.Foo.objcProp.getter : Swift.Int
// CHECK-LABEL:   #Foo.objcProp!setter.1:    _TFC7dynamic3Foos8objcPropSi  // dynamic.Foo.objcProp.setter : Swift.Int

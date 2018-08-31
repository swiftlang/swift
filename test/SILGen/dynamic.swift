
// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs -I %t) -module-name dynamic -Xllvm -sil-full-demangle -primary-file %s %S/Inputs/dynamic_other.swift | %FileCheck %s
// RUN: %target-swift-emit-sil(mock-sdk: -sdk %S/Inputs -I %t) -module-name dynamic -Xllvm -sil-full-demangle -primary-file %s %S/Inputs/dynamic_other.swift -verify

// REQUIRES: objc_interop

import Foundation
import gizmo

class Foo: Proto {
  // Not objc or dynamic, so only a vtable entry
  init(native: Int) {}
  func nativeMethod() {}
  var nativeProp: Int = 0
  subscript(native native: Int) -> Int {
    get { return native }
    set {}
  }

  // @objc, so it has an ObjC entry point but can also be dispatched
  // by vtable
  @objc init(objc: Int) {}
  @objc func objcMethod() {}
  @objc var objcProp: Int = 0
  @objc subscript(objc objc: AnyObject) -> Int {
    get { return 0 }
    set {}
  }

  // dynamic, so it has only an ObjC entry point
  @objc dynamic init(dynamic: Int) {}
  @objc dynamic func dynamicMethod() {}
  @objc dynamic var dynamicProp: Int = 0
  @objc dynamic subscript(dynamic dynamic: Int) -> Int {
    get { return dynamic }
    set {}
  }

  func overriddenByDynamic() {}

  @NSManaged var managedProp: Int
}

protocol Proto {
  func nativeMethod()
  var nativeProp: Int { get set }
  subscript(native native: Int) -> Int { get set }

  func objcMethod()
  var objcProp: Int { get set }
  subscript(objc objc: AnyObject) -> Int { get set }

  func dynamicMethod()
  var dynamicProp: Int { get set }
  subscript(dynamic dynamic: Int) -> Int { get set }
}

// ObjC entry points for @objc and dynamic entry points

// normal and @objc initializing ctors can be statically dispatched
// CHECK-LABEL: sil hidden @$S7dynamic3FooC{{.*}}tcfC
// CHECK:         function_ref @$S7dynamic3FooC{{.*}}tcfc

// CHECK-LABEL: sil hidden @$S7dynamic3FooC{{.*}}tcfC
// CHECK:         function_ref @$S7dynamic3FooC{{.*}}tcfc

// CHECK-LABEL: sil hidden [thunk] @$S7dynamic3{{[_0-9a-zA-Z]*}}fcTo
// CHECK-LABEL: sil hidden [thunk] @$S7dynamic3FooC10objcMethod{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil hidden [transparent] [thunk] @$S7dynamic3FooC8objcPropSivgTo
// CHECK-LABEL: sil hidden [transparent] [thunk] @$S7dynamic3FooC8objcPropSivsTo
// CHECK-LABEL: sil hidden [thunk] @$S7dynamic3FooC4objcSiyXl_tcigTo
// CHECK-LABEL: sil hidden [thunk] @$S7dynamic3FooC4objcSiyXl_tcisTo

// TODO: dynamic initializing ctor must be objc dispatched
// CHECK-LABEL: sil hidden @$S7dynamic3{{[_0-9a-zA-Z]*}}fC
// CHECK:         function_ref @$S7dynamic3{{[_0-9a-zA-Z]*}}fcTD
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$S7dynamic3{{[_0-9a-zA-Z]*}}fcTD
// CHECK:         objc_method {{%.*}} : $Foo, #Foo.init!initializer.1.foreign :

// CHECK-LABEL: sil hidden [thunk] @$S7dynamic3{{[_0-9a-zA-Z]*}}fcTo
// CHECK-LABEL: sil hidden [thunk] @$S7dynamic3FooC0A6Method{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil hidden [transparent] [thunk] @$S7dynamic3FooC0A4PropSivgTo
// CHECK-LABEL: sil hidden [transparent] [thunk] @$S7dynamic3FooC0A4PropSivsTo
// CHECK-LABEL: sil hidden [thunk] @$S7dynamic3FooCAAS2i_tcigTo
// CHECK-LABEL: sil hidden [thunk] @$S7dynamic3FooCAAS2i_tcisTo

// Protocol witnesses use best appropriate dispatch

// Native witnesses use vtable dispatch:
// CHECK-LABEL: sil private [transparent] [thunk] @$S7dynamic3FooCAA5ProtoA2aDP12nativeMethod{{[_0-9a-zA-Z]*}}FTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.nativeMethod!1 :
// CHECK-LABEL: sil private [transparent] [thunk] @$S7dynamic3FooCAA5ProtoA2aDP10nativePropSivgTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.nativeProp!getter.1 :
// CHECK-LABEL: sil private [transparent] [thunk] @$S7dynamic3FooCAA5ProtoA2aDP10nativePropSivsTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.nativeProp!setter.1 :
// CHECK-LABEL: sil private [transparent] [thunk] @$S7dynamic3FooCAA5ProtoA2aDP6nativeS2i_tcigTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.subscript!getter.1 :
// CHECK-LABEL: sil private [transparent] [thunk] @$S7dynamic3FooCAA5ProtoA2aDP6nativeS2i_tcisTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.subscript!setter.1 :

// @objc witnesses use vtable dispatch:
// CHECK-LABEL: sil private [transparent] [thunk] @$S7dynamic3FooCAA5ProtoA2aDP10objcMethod{{[_0-9a-zA-Z]*}}FTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.objcMethod!1 :
// CHECK-LABEL: sil private [transparent] [thunk] @$S7dynamic3FooCAA5ProtoA2aDP8objcPropSivgTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.objcProp!getter.1 :
// CHECK-LABEL: sil private [transparent] [thunk] @$S7dynamic3FooCAA5ProtoA2aDP8objcPropSivsTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.objcProp!setter.1 :
// CHECK-LABEL: sil private [transparent] [thunk] @$S7dynamic3FooCAA5ProtoA2aDP4objcSiyXl_tcigTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.subscript!getter.1 :
// CHECK-LABEL: sil private [transparent] [thunk] @$S7dynamic3FooCAA5ProtoA2aDP4objcSiyXl_tcisTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.subscript!setter.1 :

// Dynamic witnesses use objc dispatch:
// CHECK-LABEL: sil private [transparent] [thunk] @$S7dynamic3FooCAA5ProtoA2aDP0A6Method{{[_0-9a-zA-Z]*}}FTW
// CHECK:         function_ref @$S7dynamic3FooC0A6Method{{[_0-9a-zA-Z]*}}FTD
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$S7dynamic3FooC0A6Method{{[_0-9a-zA-Z]*}}FTD
// CHECK:         objc_method {{%.*}} : $Foo, #Foo.dynamicMethod!1.foreign :

// CHECK-LABEL: sil private [transparent] [thunk] @$S7dynamic3FooCAA5ProtoA2aDP0A4PropSivgTW
// CHECK:         function_ref @$S7dynamic3FooC0A4PropSivgTD
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$S7dynamic3FooC0A4PropSivgTD
// CHECK:         objc_method {{%.*}} : $Foo, #Foo.dynamicProp!getter.1.foreign :

// CHECK-LABEL: sil private [transparent] [thunk] @$S7dynamic3FooCAA5ProtoA2aDP0A4PropSivsTW
// CHECK:         function_ref @$S7dynamic3FooC0A4PropSivsTD
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$S7dynamic3FooC0A4PropSivsTD
// CHECK:         objc_method {{%.*}} : $Foo, #Foo.dynamicProp!setter.1.foreign :

// CHECK-LABEL: sil private [transparent] [thunk] @$S7dynamic3FooCAA5ProtoA2aDPAAS2i_tcigTW
// CHECK:         function_ref @$S7dynamic3FooCAAS2i_tcigTD
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$S7dynamic3FooCAAS2i_tcigTD
// CHECK:         objc_method {{%.*}} : $Foo, #Foo.subscript!getter.1.foreign :

// CHECK-LABEL: sil private [transparent] [thunk] @$S7dynamic3FooCAA5ProtoA2aDPAAS2i_tcisTW
// CHECK:         function_ref @$S7dynamic3FooCAAS2i_tcisTD
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$S7dynamic3FooCAAS2i_tcisTD
// CHECK:         objc_method {{%.*}} : $Foo, #Foo.subscript!setter.1.foreign :

// Superclass dispatch
class Subclass: Foo {
  // Native and objc methods can directly reference super members
  override init(native: Int) {
    super.init(native: native)
  }
  // CHECK-LABEL: sil hidden @$S7dynamic8SubclassC{{[_0-9a-zA-Z]*}}fC
  // CHECK:         function_ref @$S7dynamic8SubclassC{{[_0-9a-zA-Z]*}}fc

  override func nativeMethod() {
    super.nativeMethod()
  }
  // CHECK-LABEL: sil hidden @$S7dynamic8SubclassC12nativeMethod{{[_0-9a-zA-Z]*}}F
  // CHECK:         function_ref @$S7dynamic3FooC12nativeMethodyyF : $@convention(method) (@guaranteed Foo) -> ()

  override var nativeProp: Int {
    get { return super.nativeProp }
    // CHECK-LABEL: sil hidden @$S7dynamic8SubclassC10nativePropSivg
    // CHECK:         function_ref @$S7dynamic3FooC10nativePropSivg : $@convention(method) (@guaranteed Foo) -> Int
    set { super.nativeProp = newValue }
    // CHECK-LABEL: sil hidden @$S7dynamic8SubclassC10nativePropSivs
    // CHECK:         function_ref @$S7dynamic3FooC10nativePropSivs : $@convention(method) (Int, @guaranteed Foo) -> ()
  }

  override subscript(native native: Int) -> Int {
    get { return super[native: native] }
    // CHECK-LABEL: sil hidden @$S7dynamic8SubclassC6nativeS2i_tcig
    // CHECK:         function_ref @$S7dynamic3FooC6nativeS2i_tcig : $@convention(method) (Int, @guaranteed Foo) -> Int
    set { super[native: native] = newValue }
    // CHECK-LABEL: sil hidden @$S7dynamic8SubclassC6nativeS2i_tcis
    // CHECK:         function_ref @$S7dynamic3FooC6nativeS2i_tcis : $@convention(method) (Int, Int, @guaranteed Foo) -> ()
  }

  override init(objc: Int) {
    super.init(objc: objc)
  }
  // CHECK-LABEL: sil hidden @$S7dynamic8SubclassC4objcACSi_tcfc
  // CHECK:         function_ref @$S7dynamic3FooC4objcACSi_tcfc : $@convention(method) (Int, @owned Foo) -> @owned Foo

  override func objcMethod() {
    super.objcMethod()
  }
  // CHECK-LABEL: sil hidden @$S7dynamic8SubclassC10objcMethod{{[_0-9a-zA-Z]*}}F
  // CHECK:         function_ref @$S7dynamic3FooC10objcMethodyyF : $@convention(method) (@guaranteed Foo) -> ()

  override var objcProp: Int {
    get { return super.objcProp }
    // CHECK-LABEL: sil hidden @$S7dynamic8SubclassC8objcPropSivg
    // CHECK:         function_ref @$S7dynamic3FooC8objcPropSivg : $@convention(method) (@guaranteed Foo) -> Int
    set { super.objcProp = newValue }
    // CHECK-LABEL: sil hidden @$S7dynamic8SubclassC8objcPropSivs
    // CHECK:         function_ref @$S7dynamic3FooC8objcPropSivs : $@convention(method) (Int, @guaranteed Foo) -> ()
  }

  override subscript(objc objc: AnyObject) -> Int {
    get { return super[objc: objc] }
    // CHECK-LABEL: sil hidden @$S7dynamic8SubclassC4objcSiyXl_tcig
    // CHECK:         function_ref @$S7dynamic3FooC4objcSiyXl_tcig : $@convention(method) (@guaranteed AnyObject, @guaranteed Foo) -> Int
    set { super[objc: objc] = newValue }
    // CHECK-LABEL: sil hidden @$S7dynamic8SubclassC4objcSiyXl_tcis
    // CHECK:         function_ref @$S7dynamic3FooC4objcSiyXl_tcis : $@convention(method) (Int, @owned AnyObject, @guaranteed Foo) -> ()
  }

  // Dynamic methods are super-dispatched by objc_msgSend
  override init(dynamic: Int) {
    super.init(dynamic: dynamic)
  }
  // CHECK-LABEL: sil hidden @$S7dynamic8SubclassC{{[_0-9a-zA-Z]*}}fc
  // CHECK:         objc_super_method {{%.*}} : $Subclass, #Foo.init!initializer.1.foreign :

  override func dynamicMethod() {
    super.dynamicMethod()
  }
  // CHECK-LABEL: sil hidden @$S7dynamic8SubclassC0A6Method{{[_0-9a-zA-Z]*}}F
  // CHECK:         objc_super_method {{%.*}} : $Subclass, #Foo.dynamicMethod!1.foreign :

  override var dynamicProp: Int {
    get { return super.dynamicProp }
    // CHECK-LABEL: sil hidden @$S7dynamic8SubclassC0A4PropSivg
    // CHECK:         objc_super_method {{%.*}} : $Subclass, #Foo.dynamicProp!getter.1.foreign :
    set { super.dynamicProp = newValue }
    // CHECK-LABEL: sil hidden @$S7dynamic8SubclassC0A4PropSivs
    // CHECK:         objc_super_method {{%.*}} : $Subclass, #Foo.dynamicProp!setter.1.foreign :
  }

  override subscript(dynamic dynamic: Int) -> Int {
    get { return super[dynamic: dynamic] }
    // CHECK-LABEL: sil hidden @$S7dynamic8SubclassCAAS2i_tcig
    // CHECK:         objc_super_method {{%.*}} : $Subclass, #Foo.subscript!getter.1.foreign :
    set { super[dynamic: dynamic] = newValue }
    // CHECK-LABEL: sil hidden @$S7dynamic8SubclassCAAS2i_tcis
    // CHECK:         objc_super_method {{%.*}} : $Subclass, #Foo.subscript!setter.1.foreign :
  }

  @objc dynamic override func overriddenByDynamic() {}
}

class SubclassWithInheritedInits: Foo {
  // CHECK-LABEL: sil hidden @$S7dynamic26SubclassWithInheritedInitsC{{[_0-9a-zA-Z]*}}fc
  // CHECK:         objc_super_method {{%.*}} : $SubclassWithInheritedInits, #Foo.init!initializer.1.foreign :
}
class GrandchildWithInheritedInits: SubclassWithInheritedInits {
  // CHECK-LABEL: sil hidden @$S7dynamic28GrandchildWithInheritedInitsC{{[_0-9a-zA-Z]*}}fc
  // CHECK:         objc_super_method {{%.*}} : $GrandchildWithInheritedInits, #SubclassWithInheritedInits.init!initializer.1.foreign :
}
class GrandchildOfInheritedInits: SubclassWithInheritedInits {
  // Dynamic methods are super-dispatched by objc_msgSend
  override init(dynamic: Int) {
    super.init(dynamic: dynamic)
  }
  // CHECK-LABEL: sil hidden @$S7dynamic26GrandchildOfInheritedInitsC{{[_0-9a-zA-Z]*}}fc
  // CHECK:         objc_super_method {{%.*}} : $GrandchildOfInheritedInits, #SubclassWithInheritedInits.init!initializer.1.foreign :
}

// CHECK-LABEL: sil hidden @$S7dynamic20nativeMethodDispatchyyF : $@convention(thin) () -> ()
func nativeMethodDispatch() {
  // CHECK: function_ref @$S7dynamic3{{[_0-9a-zA-Z]*}}fC
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

// CHECK-LABEL: sil hidden @$S7dynamic18objcMethodDispatchyyF : $@convention(thin) () -> ()
func objcMethodDispatch() {
  // CHECK: function_ref @$S7dynamic3{{[_0-9a-zA-Z]*}}fC
  let c = Foo(objc: 0)
  // CHECK: class_method {{%.*}} : $Foo, #Foo.objcMethod!1 :
  c.objcMethod()
  // CHECK: class_method {{%.*}} : $Foo, #Foo.objcProp!getter.1 :
  let x = c.objcProp
  // CHECK: class_method {{%.*}} : $Foo, #Foo.objcProp!setter.1 :
  c.objcProp = x
  // CHECK: class_method {{%.*}} : $Foo, #Foo.subscript!getter.1 :
  let y = c[objc: 0 as NSNumber]
  // CHECK: class_method {{%.*}} : $Foo, #Foo.subscript!setter.1 :
  c[objc: 0 as NSNumber] = y
}

// CHECK-LABEL: sil hidden @$S7dynamic0A14MethodDispatchyyF : $@convention(thin) () -> ()
func dynamicMethodDispatch() {
  // CHECK: function_ref @$S7dynamic3{{[_0-9a-zA-Z]*}}fC
  let c = Foo(dynamic: 0)
  // CHECK: objc_method {{%.*}} : $Foo, #Foo.dynamicMethod!1.foreign 
  c.dynamicMethod()
  // CHECK: objc_method {{%.*}} : $Foo, #Foo.dynamicProp!getter.1.foreign
  let x = c.dynamicProp
  // CHECK: objc_method {{%.*}} : $Foo, #Foo.dynamicProp!setter.1.foreign
  c.dynamicProp = x
  // CHECK: objc_method {{%.*}} : $Foo, #Foo.subscript!getter.1.foreign
  let y = c[dynamic: 0]
  // CHECK: objc_method {{%.*}} : $Foo, #Foo.subscript!setter.1.foreign
  c[dynamic: 0] = y
}

// CHECK-LABEL: sil hidden @$S7dynamic15managedDispatchyyAA3FooCF
func managedDispatch(_ c: Foo) {
  // CHECK: objc_method {{%.*}} : $Foo, #Foo.managedProp!getter.1.foreign 
  let x = c.managedProp
  // CHECK: objc_method {{%.*}} : $Foo, #Foo.managedProp!setter.1.foreign
  c.managedProp = x
}

// CHECK-LABEL: sil hidden @$S7dynamic21foreignMethodDispatchyyF
func foreignMethodDispatch() {
  // CHECK: function_ref @$SSo9GuisemeauC{{[_0-9a-zA-Z]*}}fC
  let g = Guisemeau()!
  // CHECK: objc_method {{%.*}} : $Gizmo, #Gizmo.frob!1.foreign
  g.frob()
  // CHECK: objc_method {{%.*}} : $Gizmo, #Gizmo.count!getter.1.foreign
  let x = g.count
  // CHECK: objc_method {{%.*}} : $Gizmo, #Gizmo.count!setter.1.foreign
  g.count = x
  // CHECK: objc_method {{%.*}} : $Guisemeau, #Guisemeau.subscript!getter.1.foreign
  let y: Any! = g[0]
  // CHECK: objc_method {{%.*}} : $Guisemeau, #Guisemeau.subscript!setter.1.foreign
  g[0] = y
  // CHECK: objc_method {{%.*}} : $NSObject, #NSObject.description!getter.1.foreign
  _ = g.description
}

extension Gizmo {
  // CHECK-LABEL: sil hidden @$SSo5GizmoC7dynamicE{{[_0-9a-zA-Z]*}}fc
  // CHECK:         objc_method {{%.*}} : $Gizmo, #Gizmo.init!initializer.1.foreign
  convenience init(convenienceInExtension: Int) {
    self.init(bellsOn: convenienceInExtension)
  }

  // CHECK-LABEL: sil hidden @$SSo5GizmoC7dynamicE{{[_0-9a-zA-Z]*}}fC
  // CHECK:         objc_method {{%.*}} : $@objc_metatype Gizmo.Type, #Gizmo.init!allocator.1.foreign
  convenience init(foreignClassFactory x: Int) {
    self.init(stuff: x)
  }

  // CHECK-LABEL: sil hidden @$SSo5GizmoC7dynamicE{{[_0-9a-zA-Z]*}}fC
  // CHECK:         objc_method {{%.*}} : $@objc_metatype Gizmo.Type, #Gizmo.init!allocator.1.foreign
  convenience init(foreignClassExactFactory x: Int) {
    self.init(exactlyStuff: x)
  }

  @objc func foreignObjCExtension() { }
  @objc dynamic func foreignDynamicExtension() { }
}

// CHECK-LABEL: sil hidden @$S7dynamic24foreignExtensionDispatchyySo5GizmoCF
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Gizmo):
func foreignExtensionDispatch(_ g: Gizmo) {
  // CHECK: objc_method [[ARG]] : $Gizmo, #Gizmo.foreignObjCExtension!1.foreign : (Gizmo)
  g.foreignObjCExtension()
  // CHECK: objc_method [[ARG]] : $Gizmo, #Gizmo.foreignDynamicExtension!1.foreign
  g.foreignDynamicExtension()
}


// CHECK-LABEL: sil hidden @$S7dynamic33nativeMethodDispatchFromOtherFileyyF : $@convention(thin) () -> ()
func nativeMethodDispatchFromOtherFile() {
  // CHECK: function_ref @$S7dynamic13FromOtherFile{{[_0-9a-zA-Z]*}}fC
  let c = FromOtherFile(native: 0)
  // CHECK: class_method {{%.*}} : $FromOtherFile, #FromOtherFile.nativeMethod!1 :
  c.nativeMethod()
  // CHECK: class_method {{%.*}} : $FromOtherFile, #FromOtherFile.nativeProp!getter.1 :
  let x = c.nativeProp
  // CHECK: class_method {{%.*}} : $FromOtherFile, #FromOtherFile.nativeProp!setter.1 :
  c.nativeProp = x
  // CHECK: class_method {{%.*}} : $FromOtherFile, #FromOtherFile.subscript!getter.1 :
  let y = c[native: 0]
  // CHECK: class_method {{%.*}} : $FromOtherFile, #FromOtherFile.subscript!setter.1 :
  c[native: 0] = y
}

// CHECK-LABEL: sil hidden @$S7dynamic31objcMethodDispatchFromOtherFileyyF : $@convention(thin) () -> ()
func objcMethodDispatchFromOtherFile() {
  // CHECK: function_ref @$S7dynamic13FromOtherFile{{[_0-9a-zA-Z]*}}fC
  let c = FromOtherFile(objc: 0)
  // CHECK: class_method {{%.*}} : $FromOtherFile, #FromOtherFile.objcMethod!1 :
  c.objcMethod()
  // CHECK: class_method {{%.*}} : $FromOtherFile, #FromOtherFile.objcProp!getter.1 :
  let x = c.objcProp
  // CHECK: class_method {{%.*}} : $FromOtherFile, #FromOtherFile.objcProp!setter.1 :
  c.objcProp = x
  // CHECK: class_method {{%.*}} : $FromOtherFile, #FromOtherFile.subscript!getter.1 :
  let y = c[objc: 0]
  // CHECK: class_method {{%.*}} : $FromOtherFile, #FromOtherFile.subscript!setter.1 :
  c[objc: 0] = y
}

// CHECK-LABEL: sil hidden @$S7dynamic0A27MethodDispatchFromOtherFileyyF : $@convention(thin) () -> ()
func dynamicMethodDispatchFromOtherFile() {
  // CHECK: function_ref @$S7dynamic13FromOtherFile{{[_0-9a-zA-Z]*}}fC
  let c = FromOtherFile(dynamic: 0)
  // CHECK: objc_method {{%.*}} : $FromOtherFile, #FromOtherFile.dynamicMethod!1.foreign
  c.dynamicMethod()
  // CHECK: objc_method {{%.*}} : $FromOtherFile, #FromOtherFile.dynamicProp!getter.1.foreign
  let x = c.dynamicProp
  // CHECK: objc_method {{%.*}} : $FromOtherFile, #FromOtherFile.dynamicProp!setter.1.foreign
  c.dynamicProp = x
  // CHECK: objc_method {{%.*}} : $FromOtherFile, #FromOtherFile.subscript!getter.1.foreign
  let y = c[dynamic: 0]
  // CHECK: objc_method {{%.*}} : $FromOtherFile, #FromOtherFile.subscript!setter.1.foreign
  c[dynamic: 0] = y
}

// CHECK-LABEL: sil hidden @$S7dynamic28managedDispatchFromOtherFileyyAA0deF0CF
func managedDispatchFromOtherFile(_ c: FromOtherFile) {
  // CHECK: objc_method {{%.*}} : $FromOtherFile, #FromOtherFile.managedProp!getter.1.foreign
  let x = c.managedProp
  // CHECK: objc_method {{%.*}} : $FromOtherFile, #FromOtherFile.managedProp!setter.1.foreign
  c.managedProp = x
}

// CHECK-LABEL: sil hidden @$S7dynamic0A16ExtensionMethodsyyAA13ObjCOtherFileCF
func dynamicExtensionMethods(_ obj: ObjCOtherFile) {
  // CHECK: objc_method {{%.*}} : $ObjCOtherFile, #ObjCOtherFile.extensionMethod!1.foreign
  obj.extensionMethod()
  // CHECK: objc_method {{%.*}} : $ObjCOtherFile, #ObjCOtherFile.extensionProp!getter.1.foreign
  _ = obj.extensionProp

  // CHECK: thick_to_objc_metatype {{%.*}} : $@thick ObjCOtherFile.Type to $@objc_metatype ObjCOtherFile.Type
  // CHECK-NEXT: objc_method {{%.*}} : $@objc_metatype ObjCOtherFile.Type, #ObjCOtherFile.extensionClassProp!getter.1.foreign
  _ = type(of: obj).extensionClassProp

  // CHECK: objc_method {{%.*}} : $ObjCOtherFile, #ObjCOtherFile.dynExtensionMethod!1.foreign
  obj.dynExtensionMethod()
  // CHECK: objc_method {{%.*}} : $ObjCOtherFile, #ObjCOtherFile.dynExtensionProp!getter.1.foreign
  _ = obj.dynExtensionProp

  // CHECK: thick_to_objc_metatype {{%.*}} : $@thick ObjCOtherFile.Type to $@objc_metatype ObjCOtherFile.Type
  // CHECK-NEXT: objc_method {{%.*}} : $@objc_metatype ObjCOtherFile.Type, #ObjCOtherFile.dynExtensionClassProp!getter.1.foreign
  _ = type(of: obj).dynExtensionClassProp
}

public class Base {
  @objc dynamic var x: Bool { return false }
}

public class Sub : Base {
  // CHECK-LABEL: sil hidden @$S7dynamic3SubC1xSbvg : $@convention(method) (@guaranteed Sub) -> Bool {
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $Sub):
  // CHECK:     [[AUTOCLOSURE:%.*]] = function_ref @$S7dynamic3SubC1xSbvgSbyKXKfu_ : $@convention(thin) (@guaranteed Sub) -> (Bool, @error Error)
  // CHECK:     [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:     = partial_apply [callee_guaranteed] [[AUTOCLOSURE]]([[SELF_COPY]])
  // CHECK:     return {{%.*}} : $Bool
  // CHECK: } // end sil function '$S7dynamic3SubC1xSbvg'

  // CHECK-LABEL: sil private [transparent] @$S7dynamic3SubC1xSbvgSbyKXKfu_ : $@convention(thin) (@guaranteed Sub) -> (Bool, @error Error) {
  // CHECK: bb0([[VALUE:%.*]] : @guaranteed $Sub):
  // CHECK:     [[VALUE_COPY:%.*]] = copy_value [[VALUE]]
  // CHECK:     [[CASTED_VALUE_COPY:%.*]] = upcast [[VALUE_COPY]]
  // CHECK:     [[BORROWED_CASTED_VALUE_COPY:%.*]] = begin_borrow [[CASTED_VALUE_COPY]]
  // CHECK:     [[DOWNCAST_FOR_SUPERMETHOD:%.*]] = unchecked_ref_cast [[BORROWED_CASTED_VALUE_COPY]]
  // CHECK:     [[SUPER:%.*]] = objc_super_method [[DOWNCAST_FOR_SUPERMETHOD]] : $Sub, #Base.x!getter.1.foreign : (Base) -> () -> Bool, $@convention(objc_method) (Base) -> ObjCBool
  // CHECK:     end_borrow [[BORROWED_CASTED_VALUE_COPY]]
  // CHECK:     = apply [[SUPER]]([[CASTED_VALUE_COPY]])
  // CHECK:     destroy_value [[CASTED_VALUE_COPY]]
  // CHECK: } // end sil function '$S7dynamic3SubC1xSbvgSbyKXKfu_'
  override var x: Bool { return false || super.x }
}

public class BaseExt : NSObject {}

extension BaseExt {
  @objc public var count: Int {
    return 0
  }
}

public class SubExt : BaseExt {
  public override var count: Int {
    return 1
  }
}

public class GenericBase<T> {
  public func method(_: T) {}
}

public class ConcreteDerived : GenericBase<Int> {
  @objc public override dynamic func method(_: Int) {}
}

// The dynamic override has a different calling convention than the base,
// so after re-abstracting the signature we must dispatch to the dynamic
// thunk.

// CHECK-LABEL: sil private @$S7dynamic15ConcreteDerivedC6methodyySiFAA11GenericBaseCADyyxFTV : $@convention(method) (@in_guaranteed Int, @guaranteed ConcreteDerived) -> ()
// CHECK: bb0(%0 : @trivial $*Int, %1 : @guaranteed $ConcreteDerived):
// CHECK-NEXT:  [[VALUE:%.*]] = load [trivial] %0 : $*Int
// CHECK:       [[DYNAMIC_THUNK:%.*]] = function_ref @$S7dynamic15ConcreteDerivedC6methodyySiFTD : $@convention(method) (Int, @guaranteed ConcreteDerived) -> ()
// CHECK-NEXT:  apply [[DYNAMIC_THUNK]]([[VALUE]], %1) : $@convention(method) (Int, @guaranteed ConcreteDerived) -> ()
// CHECK:       return

// Vtable contains entries for native and @objc methods, but not dynamic ones
// CHECK-LABEL: sil_vtable Foo {
// CHECK-NEXT:   #Foo.init!initializer.1: {{.*}} :   @$S7dynamic3FooC6nativeACSi_tcfc
// CHECK-NEXT:   #Foo.nativeMethod!1: {{.*}} :       @$S7dynamic3FooC12nativeMethodyyF
// CHECK-NEXT:   #Foo.nativeProp!getter.1: {{.*}} :  @$S7dynamic3FooC10nativePropSivg     // dynamic.Foo.nativeProp.getter : Swift.Int
// CHECK-NEXT:   #Foo.nativeProp!setter.1: {{.*}} :  @$S7dynamic3FooC10nativePropSivs     // dynamic.Foo.nativeProp.setter : Swift.Int
// CHECK-NEXT:   #Foo.nativeProp!modify.1:
// CHECK-NEXT:   #Foo.subscript!getter.1: {{.*}} :   @$S7dynamic3FooC6nativeS2i_tcig    // dynamic.Foo.subscript.getter : (native: Swift.Int) -> Swift.Int
// CHECK-NEXT:   #Foo.subscript!setter.1: {{.*}} :   @$S7dynamic3FooC6nativeS2i_tcis    // dynamic.Foo.subscript.setter : (native: Swift.Int) -> Swift.Int
// CHECK-NEXT:   #Foo.subscript!modify.1:
// CHECK-NEXT:   #Foo.init!initializer.1: {{.*}} :   @$S7dynamic3FooC4objcACSi_tcfc
// CHECK-NEXT:   #Foo.objcMethod!1: {{.*}} :         @$S7dynamic3FooC10objcMethodyyF
// CHECK-NEXT:   #Foo.objcProp!getter.1: {{.*}} :    @$S7dynamic3FooC8objcPropSivg  // dynamic.Foo.objcProp.getter : Swift.Int
// CHECK-NEXT:   #Foo.objcProp!setter.1: {{.*}} :    @$S7dynamic3FooC8objcPropSivs  // dynamic.Foo.objcProp.setter : Swift.Int
// CHECK-NEXT:   #Foo.objcProp!modify.1:
// CHECK-NEXT:   #Foo.subscript!getter.1: {{.*}} : @$S7dynamic3FooC4objcSiyXl_tcig // dynamic.Foo.subscript.getter : (objc: Swift.AnyObject) -> Swift.Int
// CHECK-NEXT:   #Foo.subscript!setter.1: {{.*}} : @$S7dynamic3FooC4objcSiyXl_tcis // dynamic.Foo.subscript.setter : (objc: Swift.AnyObject) -> Swift.Int
// CHECK-NEXT:   #Foo.subscript!modify.1:
// CHECK-NEXT:   #Foo.overriddenByDynamic!1: {{.*}} : @$S7dynamic3FooC19overriddenByDynamic{{[_0-9a-zA-Z]*}}
// CHECK-NEXT:   #Foo.deinit!deallocator.1: {{.*}}
// CHECK-NEXT: }

// Vtable uses a dynamic thunk for dynamic overrides
// CHECK-LABEL: sil_vtable Subclass {
// CHECK:   #Foo.overriddenByDynamic!1: {{.*}} : public @$S7dynamic8SubclassC19overriddenByDynamic{{[_0-9a-zA-Z]*}}FTD
// CHECK: }

// Check vtables for implicitly-inherited initializers
// CHECK-LABEL: sil_vtable SubclassWithInheritedInits {
// CHECK:   #Foo.init!initializer.1: (Foo.Type) -> (Int) -> Foo : @$S7dynamic26SubclassWithInheritedInitsC6nativeACSi_tcfc
// CHECK:   #Foo.init!initializer.1: (Foo.Type) -> (Int) -> Foo : @$S7dynamic26SubclassWithInheritedInitsC4objcACSi_tcfc
// CHECK-NOT: .init!
// CHECK: }

// CHECK-LABEL: sil_vtable GrandchildWithInheritedInits {
// CHECK:   #Foo.init!initializer.1: (Foo.Type) -> (Int) -> Foo : @$S7dynamic28GrandchildWithInheritedInitsC6nativeACSi_tcfc
// CHECK:   #Foo.init!initializer.1: (Foo.Type) -> (Int) -> Foo : @$S7dynamic28GrandchildWithInheritedInitsC4objcACSi_tcfc
// CHECK-NOT: .init!
// CHECK: }

// CHECK-LABEL: sil_vtable GrandchildOfInheritedInits {
// CHECK:   #Foo.init!initializer.1: (Foo.Type) -> (Int) -> Foo : @$S7dynamic26GrandchildOfInheritedInitsC6nativeACSi_tcfc
// CHECK:   #Foo.init!initializer.1: (Foo.Type) -> (Int) -> Foo : @$S7dynamic26GrandchildOfInheritedInitsC4objcACSi_tcfc
// CHECK-NOT: .init!
// CHECK: }

// No vtable entry for override of @objc extension property
// CHECK-LABEL: sil_vtable [serialized] SubExt {
// CHECK-NEXT: #SubExt.deinit!deallocator.1: @$S7dynamic6SubExtCfD // dynamic.SubExt.__deallocating_deinit
// CHECK-NEXT: }

// Dynamic thunk + vtable re-abstraction
// CHECK-LABEL: sil_vtable [serialized] ConcreteDerived {
// CHECK-NEXT: #GenericBase.method!1: <T> (GenericBase<T>) -> (T) -> () : public @$S7dynamic15ConcreteDerivedC6methodyySiFAA11GenericBaseCADyyxFTV [override]     // vtable thunk for dynamic.GenericBase.method(A) -> () dispatching to dynamic.ConcreteDerived.method(Swift.Int) -> ()
// CHECK-NEXT: #GenericBase.init!initializer.1: <T> (GenericBase<T>.Type) -> () -> GenericBase<T> : @$S7dynamic15ConcreteDerivedCACycfc [override]      // dynamic.ConcreteDerived.init() -> dynamic.ConcreteDerived
// CHECK-NEXT: #ConcreteDerived.deinit!deallocator.1: @$S7dynamic15ConcreteDerivedCfD  // dynamic.ConcreteDerived.__deallocating_deinit
// CHECK-NEXT: }

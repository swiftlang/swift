// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -Xllvm -sil-full-demangle -primary-file %s %S/Inputs/dynamic_other.swift -emit-silgen | %FileCheck %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -Xllvm -sil-full-demangle -primary-file %s %S/Inputs/dynamic_other.swift -emit-sil -verify

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
  dynamic init(dynamic: Int) {}
  dynamic func dynamicMethod() {}
  dynamic var dynamicProp: Int = 0
  dynamic subscript(dynamic dynamic: Int) -> Int {
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
// CHECK-LABEL: sil hidden @_T07dynamic3FooC{{.*}}tcfC
// CHECK:         function_ref @_T07dynamic3FooC{{.*}}tcfc

// CHECK-LABEL: sil hidden @_T07dynamic3FooC{{.*}}tcfC
// CHECK:         function_ref @_T07dynamic3FooC{{.*}}tcfc

// CHECK-LABEL: sil hidden [thunk] @_T07dynamic3{{[_0-9a-zA-Z]*}}fcTo
// CHECK-LABEL: sil hidden [thunk] @_T07dynamic3FooC10objcMethod{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil hidden [transparent] [thunk] @_T07dynamic3FooC8objcPropSivgTo
// CHECK-LABEL: sil hidden [transparent] [thunk] @_T07dynamic3FooC8objcPropSivsTo
// CHECK-LABEL: sil hidden [thunk] @_T07dynamic3FooCSiyXl4objc_tcigTo
// CHECK-LABEL: sil hidden [thunk] @_T07dynamic3FooCSiyXl4objc_tcisTo

// TODO: dynamic initializing ctor must be objc dispatched
// CHECK-LABEL: sil hidden @_T07dynamic3{{[_0-9a-zA-Z]*}}fC
// CHECK:         function_ref @_T07dynamic3{{[_0-9a-zA-Z]*}}fcTD
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @_T07dynamic3{{[_0-9a-zA-Z]*}}fcTD
// CHECK:         objc_method {{%.*}} : $Foo, #Foo.init!initializer.1.foreign :

// CHECK-LABEL: sil hidden [thunk] @_T07dynamic3{{[_0-9a-zA-Z]*}}fcTo
// CHECK-LABEL: sil hidden [thunk] @_T07dynamic3FooC0A6Method{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil hidden [transparent] [thunk] @_T07dynamic3FooC0A4PropSivgTo
// CHECK-LABEL: sil hidden [transparent] [thunk] @_T07dynamic3FooC0A4PropSivsTo
// CHECK-LABEL: sil hidden [thunk] @_T07dynamic3FooCS2iAA_tcigTo
// CHECK-LABEL: sil hidden [thunk] @_T07dynamic3FooCS2iAA_tcisTo

// Protocol witnesses use best appropriate dispatch

// Native witnesses use vtable dispatch:
// CHECK-LABEL: sil private [transparent] [thunk] @_T07dynamic3FooCAA5ProtoA2aDP12nativeMethod{{[_0-9a-zA-Z]*}}FTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.nativeMethod!1 :
// CHECK-LABEL: sil private [transparent] [thunk] @_T07dynamic3FooCAA5ProtoA2aDP10nativePropSivgTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.nativeProp!getter.1 :
// CHECK-LABEL: sil private [transparent] [thunk] @_T07dynamic3FooCAA5ProtoA2aDP10nativePropSivsTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.nativeProp!setter.1 :
// CHECK-LABEL: sil private [transparent] [thunk] @_T07dynamic3FooCAA5ProtoA2aDPS2i6native_tcigTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.subscript!getter.1 :
// CHECK-LABEL: sil private [transparent] [thunk] @_T07dynamic3FooCAA5ProtoA2aDPS2i6native_tcisTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.subscript!setter.1 :

// @objc witnesses use vtable dispatch:
// CHECK-LABEL: sil private [transparent] [thunk] @_T07dynamic3FooCAA5ProtoA2aDP10objcMethod{{[_0-9a-zA-Z]*}}FTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.objcMethod!1 :
// CHECK-LABEL: sil private [transparent] [thunk] @_T07dynamic3FooCAA5ProtoA2aDP8objcPropSivgTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.objcProp!getter.1 :
// CHECK-LABEL: sil private [transparent] [thunk] @_T07dynamic3FooCAA5ProtoA2aDP8objcPropSivsTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.objcProp!setter.1 :
// CHECK-LABEL: sil private [transparent] [thunk] @_T07dynamic3FooCAA5ProtoA2aDPSiyXl4objc_tcigTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.subscript!getter.1 :
// CHECK-LABEL: sil private [transparent] [thunk] @_T07dynamic3FooCAA5ProtoA2aDPSiyXl4objc_tcisTW
// CHECK:         class_method {{%.*}} : $Foo, #Foo.subscript!setter.1 :

// Dynamic witnesses use objc dispatch:
// CHECK-LABEL: sil private [transparent] [thunk] @_T07dynamic3FooCAA5ProtoA2aDP0A6Method{{[_0-9a-zA-Z]*}}FTW
// CHECK:         function_ref @_T07dynamic3FooC0A6Method{{[_0-9a-zA-Z]*}}FTD
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @_T07dynamic3FooC0A6Method{{[_0-9a-zA-Z]*}}FTD
// CHECK:         objc_method {{%.*}} : $Foo, #Foo.dynamicMethod!1.foreign :

// CHECK-LABEL: sil private [transparent] [thunk] @_T07dynamic3FooCAA5ProtoA2aDP0A4PropSivgTW
// CHECK:         function_ref @_T07dynamic3FooC0A4PropSivgTD
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @_T07dynamic3FooC0A4PropSivgTD
// CHECK:         objc_method {{%.*}} : $Foo, #Foo.dynamicProp!getter.1.foreign :

// CHECK-LABEL: sil private [transparent] [thunk] @_T07dynamic3FooCAA5ProtoA2aDP0A4PropSivsTW
// CHECK:         function_ref @_T07dynamic3FooC0A4PropSivsTD
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @_T07dynamic3FooC0A4PropSivsTD
// CHECK:         objc_method {{%.*}} : $Foo, #Foo.dynamicProp!setter.1.foreign :

// CHECK-LABEL: sil private [transparent] [thunk] @_T07dynamic3FooCAA5ProtoA2aDPS2iAA_tcigTW
// CHECK:         function_ref @_T07dynamic3FooCS2iAA_tcigTD
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @_T07dynamic3FooCS2iAA_tcigTD
// CHECK:         objc_method {{%.*}} : $Foo, #Foo.subscript!getter.1.foreign :

// CHECK-LABEL: sil private [transparent] [thunk] @_T07dynamic3FooCAA5ProtoA2aDPS2iAA_tcisTW
// CHECK:         function_ref @_T07dynamic3FooCS2iAA_tcisTD
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @_T07dynamic3FooCS2iAA_tcisTD
// CHECK:         objc_method {{%.*}} : $Foo, #Foo.subscript!setter.1.foreign :

// Superclass dispatch
class Subclass: Foo {
  // Native and objc methods can directly reference super members
  override init(native: Int) {
    super.init(native: native)
  }
  // CHECK-LABEL: sil hidden @_T07dynamic8SubclassC{{[_0-9a-zA-Z]*}}fC
  // CHECK:         function_ref @_T07dynamic8SubclassC{{[_0-9a-zA-Z]*}}fc

  override func nativeMethod() {
    super.nativeMethod()
  }
  // CHECK-LABEL: sil hidden @_T07dynamic8SubclassC12nativeMethod{{[_0-9a-zA-Z]*}}F
  // CHECK:         function_ref @_T07dynamic3FooC12nativeMethodyyF : $@convention(method) (@guaranteed Foo) -> ()

  override var nativeProp: Int {
    get { return super.nativeProp }
    // CHECK-LABEL: sil hidden @_T07dynamic8SubclassC10nativePropSivg
    // CHECK:         function_ref @_T07dynamic3FooC10nativePropSivg : $@convention(method) (@guaranteed Foo) -> Int
    set { super.nativeProp = newValue }
    // CHECK-LABEL: sil hidden @_T07dynamic8SubclassC10nativePropSivs
    // CHECK:         function_ref @_T07dynamic3FooC10nativePropSivs : $@convention(method) (Int, @guaranteed Foo) -> ()
  }

  override subscript(native native: Int) -> Int {
    get { return super[native: native] }
    // CHECK-LABEL: sil hidden @_T07dynamic8SubclassCS2i6native_tcig
    // CHECK:         function_ref @_T07dynamic3FooCS2i6native_tcig : $@convention(method) (Int, @guaranteed Foo) -> Int
    set { super[native: native] = newValue }
    // CHECK-LABEL: sil hidden @_T07dynamic8SubclassCS2i6native_tcis
    // CHECK:         function_ref @_T07dynamic3FooCS2i6native_tcis : $@convention(method) (Int, Int, @guaranteed Foo) -> ()
  }

  override init(objc: Int) {
    super.init(objc: objc)
  }
  // CHECK-LABEL: sil hidden @_T07dynamic8SubclassCACSi4objc_tcfc
  // CHECK:         function_ref @_T07dynamic3FooCACSi4objc_tcfc : $@convention(method) (Int, @owned Foo) -> @owned Foo

  override func objcMethod() {
    super.objcMethod()
  }
  // CHECK-LABEL: sil hidden @_T07dynamic8SubclassC10objcMethod{{[_0-9a-zA-Z]*}}F
  // CHECK:         function_ref @_T07dynamic3FooC10objcMethodyyF : $@convention(method) (@guaranteed Foo) -> ()

  override var objcProp: Int {
    get { return super.objcProp }
    // CHECK-LABEL: sil hidden @_T07dynamic8SubclassC8objcPropSivg
    // CHECK:         function_ref @_T07dynamic3FooC8objcPropSivg : $@convention(method) (@guaranteed Foo) -> Int
    set { super.objcProp = newValue }
    // CHECK-LABEL: sil hidden @_T07dynamic8SubclassC8objcPropSivs
    // CHECK:         function_ref @_T07dynamic3FooC8objcPropSivs : $@convention(method) (Int, @guaranteed Foo) -> ()
  }

  override subscript(objc objc: AnyObject) -> Int {
    get { return super[objc: objc] }
    // CHECK-LABEL: sil hidden @_T07dynamic8SubclassCSiyXl4objc_tcig
    // CHECK:         function_ref @_T07dynamic3FooCSiyXl4objc_tcig : $@convention(method) (@owned AnyObject, @guaranteed Foo) -> Int
    set { super[objc: objc] = newValue }
    // CHECK-LABEL: sil hidden @_T07dynamic8SubclassCSiyXl4objc_tcis
    // CHECK:         function_ref @_T07dynamic3FooCSiyXl4objc_tcis : $@convention(method) (Int, @owned AnyObject, @guaranteed Foo) -> ()
  }

  // Dynamic methods are super-dispatched by objc_msgSend
  override init(dynamic: Int) {
    super.init(dynamic: dynamic)
  }
  // CHECK-LABEL: sil hidden @_T07dynamic8SubclassC{{[_0-9a-zA-Z]*}}fc
  // CHECK:         objc_super_method {{%.*}} : $Subclass, #Foo.init!initializer.1.foreign :

  override func dynamicMethod() {
    super.dynamicMethod()
  }
  // CHECK-LABEL: sil hidden @_T07dynamic8SubclassC0A6Method{{[_0-9a-zA-Z]*}}F
  // CHECK:         objc_super_method {{%.*}} : $Subclass, #Foo.dynamicMethod!1.foreign :

  override var dynamicProp: Int {
    get { return super.dynamicProp }
    // CHECK-LABEL: sil hidden @_T07dynamic8SubclassC0A4PropSivg
    // CHECK:         objc_super_method {{%.*}} : $Subclass, #Foo.dynamicProp!getter.1.foreign :
    set { super.dynamicProp = newValue }
    // CHECK-LABEL: sil hidden @_T07dynamic8SubclassC0A4PropSivs
    // CHECK:         objc_super_method {{%.*}} : $Subclass, #Foo.dynamicProp!setter.1.foreign :
  }

  override subscript(dynamic dynamic: Int) -> Int {
    get { return super[dynamic: dynamic] }
    // CHECK-LABEL: sil hidden @_T07dynamic8SubclassCS2iAA_tcig
    // CHECK:         objc_super_method {{%.*}} : $Subclass, #Foo.subscript!getter.1.foreign :
    set { super[dynamic: dynamic] = newValue }
    // CHECK-LABEL: sil hidden @_T07dynamic8SubclassCS2iAA_tcis
    // CHECK:         objc_super_method {{%.*}} : $Subclass, #Foo.subscript!setter.1.foreign :
  }

  dynamic override func overriddenByDynamic() {}
}

class SubclassWithInheritedInits: Foo {
  // CHECK-LABEL: sil hidden @_T07dynamic26SubclassWithInheritedInitsC{{[_0-9a-zA-Z]*}}fc
  // CHECK:         objc_super_method {{%.*}} : $SubclassWithInheritedInits, #Foo.init!initializer.1.foreign :
}
class GrandchildWithInheritedInits: SubclassWithInheritedInits {
  // CHECK-LABEL: sil hidden @_T07dynamic28GrandchildWithInheritedInitsC{{[_0-9a-zA-Z]*}}fc
  // CHECK:         objc_super_method {{%.*}} : $GrandchildWithInheritedInits, #SubclassWithInheritedInits.init!initializer.1.foreign :
}
class GrandchildOfInheritedInits: SubclassWithInheritedInits {
  // Dynamic methods are super-dispatched by objc_msgSend
  override init(dynamic: Int) {
    super.init(dynamic: dynamic)
  }
  // CHECK-LABEL: sil hidden @_T07dynamic26GrandchildOfInheritedInitsC{{[_0-9a-zA-Z]*}}fc
  // CHECK:         objc_super_method {{%.*}} : $GrandchildOfInheritedInits, #SubclassWithInheritedInits.init!initializer.1.foreign :
}

// CHECK-LABEL: sil hidden @_T07dynamic20nativeMethodDispatchyyF : $@convention(thin) () -> ()
func nativeMethodDispatch() {
  // CHECK: function_ref @_T07dynamic3{{[_0-9a-zA-Z]*}}fC
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

// CHECK-LABEL: sil hidden @_T07dynamic18objcMethodDispatchyyF : $@convention(thin) () -> ()
func objcMethodDispatch() {
  // CHECK: function_ref @_T07dynamic3{{[_0-9a-zA-Z]*}}fC
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

// CHECK-LABEL: sil hidden @_T07dynamic0A14MethodDispatchyyF : $@convention(thin) () -> ()
func dynamicMethodDispatch() {
  // CHECK: function_ref @_T07dynamic3{{[_0-9a-zA-Z]*}}fC
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

// CHECK-LABEL: sil hidden @_T07dynamic15managedDispatchyAA3FooCF
func managedDispatch(_ c: Foo) {
  // CHECK: objc_method {{%.*}} : $Foo, #Foo.managedProp!getter.1.foreign 
  let x = c.managedProp
  // CHECK: objc_method {{%.*}} : $Foo, #Foo.managedProp!setter.1.foreign
  c.managedProp = x
}

// CHECK-LABEL: sil hidden @_T07dynamic21foreignMethodDispatchyyF
func foreignMethodDispatch() {
  // CHECK: function_ref @_T0So9GuisemeauC{{[_0-9a-zA-Z]*}}fC
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
  // CHECK-LABEL: sil hidden @_T0So5GizmoC7dynamicE{{[_0-9a-zA-Z]*}}fc
  // CHECK:         objc_method {{%.*}} : $Gizmo, #Gizmo.init!initializer.1.foreign
  convenience init(convenienceInExtension: Int) {
    self.init(bellsOn: convenienceInExtension)
  }

  // CHECK-LABEL: sil hidden @_T0So5GizmoC7dynamicE{{[_0-9a-zA-Z]*}}fC
  // CHECK:         objc_method {{%.*}} : $@objc_metatype Gizmo.Type, #Gizmo.init!allocator.1.foreign
  convenience init(foreignClassFactory x: Int) {
    self.init(stuff: x)
  }

  // CHECK-LABEL: sil hidden @_T0So5GizmoC7dynamicE{{[_0-9a-zA-Z]*}}fC
  // CHECK:         objc_method {{%.*}} : $@objc_metatype Gizmo.Type, #Gizmo.init!allocator.1.foreign
  convenience init(foreignClassExactFactory x: Int) {
    self.init(exactlyStuff: x)
  }

  @objc func foreignObjCExtension() { }
  dynamic func foreignDynamicExtension() { }
}

// CHECK-LABEL: sil hidden @_T07dynamic24foreignExtensionDispatchySo5GizmoCF
// CHECK: bb0([[ARG:%.*]] : $Gizmo):
func foreignExtensionDispatch(_ g: Gizmo) {
  // CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK: objc_method [[BORROWED_ARG]] : $Gizmo, #Gizmo.foreignObjCExtension!1.foreign : (Gizmo)
  g.foreignObjCExtension()
  // CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK: objc_method [[BORROWED_ARG]] : $Gizmo, #Gizmo.foreignDynamicExtension!1.foreign
  g.foreignDynamicExtension()
}


// CHECK-LABEL: sil hidden @_T07dynamic33nativeMethodDispatchFromOtherFileyyF : $@convention(thin) () -> ()
func nativeMethodDispatchFromOtherFile() {
  // CHECK: function_ref @_T07dynamic13FromOtherFile{{[_0-9a-zA-Z]*}}fC
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

// CHECK-LABEL: sil hidden @_T07dynamic31objcMethodDispatchFromOtherFileyyF : $@convention(thin) () -> ()
func objcMethodDispatchFromOtherFile() {
  // CHECK: function_ref @_T07dynamic13FromOtherFile{{[_0-9a-zA-Z]*}}fC
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

// CHECK-LABEL: sil hidden @_T07dynamic0A27MethodDispatchFromOtherFileyyF : $@convention(thin) () -> ()
func dynamicMethodDispatchFromOtherFile() {
  // CHECK: function_ref @_T07dynamic13FromOtherFile{{[_0-9a-zA-Z]*}}fC
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

// CHECK-LABEL: sil hidden @_T07dynamic28managedDispatchFromOtherFileyAA0deF0CF
func managedDispatchFromOtherFile(_ c: FromOtherFile) {
  // CHECK: objc_method {{%.*}} : $FromOtherFile, #FromOtherFile.managedProp!getter.1.foreign
  let x = c.managedProp
  // CHECK: objc_method {{%.*}} : $FromOtherFile, #FromOtherFile.managedProp!setter.1.foreign
  c.managedProp = x
}

// CHECK-LABEL: sil hidden @_T07dynamic0A16ExtensionMethodsyAA13ObjCOtherFileCF
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
  dynamic var x: Bool { return false }
}

public class Sub : Base {
  // CHECK-LABEL: sil hidden @_T07dynamic3SubC1xSbvg : $@convention(method) (@guaranteed Sub) -> Bool {
  // CHECK: bb0([[SELF:%.*]] : $Sub):
  // CHECK:     [[AUTOCLOSURE:%.*]] = function_ref @_T07dynamic3SubC1xSbvgSbyKXKfu_ : $@convention(thin) (@guaranteed Sub) -> (Bool, @error Error)
  // CHECK:     [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:     = partial_apply [callee_guaranteed] [[AUTOCLOSURE]]([[SELF_COPY]])
  // CHECK:     return {{%.*}} : $Bool
  // CHECK: } // end sil function '_T07dynamic3SubC1xSbvg'

  // CHECK-LABEL: sil private [transparent] @_T07dynamic3SubC1xSbvgSbyKXKfu_ : $@convention(thin) (@guaranteed Sub) -> (Bool, @error Error) {
  // CHECK: bb0([[VALUE:%.*]] : $Sub):
  // CHECK:     [[VALUE_COPY:%.*]] = copy_value [[VALUE]]
  // CHECK:     [[CASTED_VALUE_COPY:%.*]] = upcast [[VALUE_COPY]]
  // CHECK:     [[BORROWED_CASTED_VALUE_COPY:%.*]] = begin_borrow [[CASTED_VALUE_COPY]]
  // CHECK:     [[DOWNCAST_FOR_SUPERMETHOD:%.*]] = unchecked_ref_cast [[BORROWED_CASTED_VALUE_COPY]]
  // CHECK:     [[SUPER:%.*]] = objc_super_method [[DOWNCAST_FOR_SUPERMETHOD]] : $Sub, #Base.x!getter.1.foreign : (Base) -> () -> Bool, $@convention(objc_method) (Base) -> ObjCBool
  // CHECK:     end_borrow [[BORROWED_CASTED_VALUE_COPY]]
  // CHECK:     = apply [[SUPER]]([[CASTED_VALUE_COPY]])
  // CHECK:     destroy_value [[CASTED_VALUE_COPY]]
  // CHECK: } // end sil function '_T07dynamic3SubC1xSbvgSbyKXKfu_'
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
  public override dynamic func method(_: Int) {}
}

// The dynamic override has a different calling convention than the base,
// so after re-abstracting the signature we must dispatch to the dynamic
// thunk.

// CHECK-LABEL: sil private @_T07dynamic15ConcreteDerivedC6methodySiFAA11GenericBaseCADyxFTV : $@convention(method) (@in Int, @guaranteed ConcreteDerived) -> ()
// CHECK: bb0(%0 : $*Int, %1 : $ConcreteDerived):
// CHECK-NEXT:  [[VALUE:%.*]] = load [trivial] %0 : $*Int
// CHECK:       [[DYNAMIC_THUNK:%.*]] = function_ref @_T07dynamic15ConcreteDerivedC6methodySiFTD : $@convention(method) (Int, @guaranteed ConcreteDerived) -> ()
// CHECK-NEXT:  apply [[DYNAMIC_THUNK]]([[VALUE]], %1) : $@convention(method) (Int, @guaranteed ConcreteDerived) -> ()
// CHECK:       return

// Vtable contains entries for native and @objc methods, but not dynamic ones
// CHECK-LABEL: sil_vtable Foo {
// CHECK-NEXT:   #Foo.init!initializer.1: {{.*}} :   _T07dynamic3FooCACSi6native_tcfc
// CHECK-NEXT:   #Foo.nativeMethod!1: {{.*}} :       _T07dynamic3FooC12nativeMethodyyF
// CHECK-NEXT:   #Foo.nativeProp!getter.1: {{.*}} :  _T07dynamic3FooC10nativePropSivg     // dynamic.Foo.nativeProp.getter : Swift.Int
// CHECK-NEXT:   #Foo.nativeProp!setter.1: {{.*}} :  _T07dynamic3FooC10nativePropSivs     // dynamic.Foo.nativeProp.setter : Swift.Int
// CHECK-NEXT:   #Foo.nativeProp!materializeForSet.1
// CHECK-NEXT:   #Foo.subscript!getter.1: {{.*}} :   _T07dynamic3FooCS2i6native_tcig    // dynamic.Foo.subscript.getter : (native: Swift.Int) -> Swift.Int
// CHECK-NEXT:   #Foo.subscript!setter.1: {{.*}} :   _T07dynamic3FooCS2i6native_tcis    // dynamic.Foo.subscript.setter : (native: Swift.Int) -> Swift.Int
// CHECK-NEXT:   #Foo.subscript!materializeForSet.1
// CHECK-NEXT:   #Foo.init!initializer.1: {{.*}} :   _T07dynamic3FooCACSi4objc_tcfc
// CHECK-NEXT:   #Foo.objcMethod!1: {{.*}} :         _T07dynamic3FooC10objcMethodyyF
// CHECK-NEXT:   #Foo.objcProp!getter.1: {{.*}} :    _T07dynamic3FooC8objcPropSivg  // dynamic.Foo.objcProp.getter : Swift.Int
// CHECK-NEXT:   #Foo.objcProp!setter.1: {{.*}} :    _T07dynamic3FooC8objcPropSivs  // dynamic.Foo.objcProp.setter : Swift.Int
// CHECK-NEXT:   #Foo.objcProp!materializeForSet.1
// CHECK-NEXT:   #Foo.subscript!getter.1: {{.*}} : _T07dynamic3FooCSiyXl4objc_tcig // dynamic.Foo.subscript.getter : (objc: Swift.AnyObject) -> Swift.Int
// CHECK-NEXT:   #Foo.subscript!setter.1: {{.*}} : _T07dynamic3FooCSiyXl4objc_tcis // dynamic.Foo.subscript.setter : (objc: Swift.AnyObject) -> Swift.Int
// CHECK-NEXT:   #Foo.subscript!materializeForSet
// CHECK-NEXT:   #Foo.overriddenByDynamic!1: {{.*}} : _T07dynamic3FooC19overriddenByDynamic{{[_0-9a-zA-Z]*}}
// CHECK-NEXT:   #Foo.deinit!deallocator: {{.*}}
// CHECK-NEXT: }

// Vtable uses a dynamic thunk for dynamic overrides
// CHECK-LABEL: sil_vtable Subclass {
// CHECK:   #Foo.overriddenByDynamic!1: {{.*}} : public _T07dynamic8SubclassC19overriddenByDynamic{{[_0-9a-zA-Z]*}}FTD
// CHECK: }

// Check vtables for implicitly-inherited initializers
// CHECK-LABEL: sil_vtable SubclassWithInheritedInits {
// CHECK:   #Foo.init!initializer.1: (Foo.Type) -> (Int) -> Foo : _T07dynamic26SubclassWithInheritedInitsCACSi6native_tcfc
// CHECK:   #Foo.init!initializer.1: (Foo.Type) -> (Int) -> Foo : _T07dynamic26SubclassWithInheritedInitsCACSi4objc_tcfc
// CHECK-NOT: .init!
// CHECK: }

// CHECK-LABEL: sil_vtable GrandchildWithInheritedInits {
// CHECK:   #Foo.init!initializer.1: (Foo.Type) -> (Int) -> Foo : _T07dynamic28GrandchildWithInheritedInitsCACSi6native_tcfc
// CHECK:   #Foo.init!initializer.1: (Foo.Type) -> (Int) -> Foo : _T07dynamic28GrandchildWithInheritedInitsCACSi4objc_tcfc
// CHECK-NOT: .init!
// CHECK: }

// CHECK-LABEL: sil_vtable GrandchildOfInheritedInits {
// CHECK:   #Foo.init!initializer.1: (Foo.Type) -> (Int) -> Foo : _T07dynamic26GrandchildOfInheritedInitsCACSi6native_tcfc
// CHECK:   #Foo.init!initializer.1: (Foo.Type) -> (Int) -> Foo : _T07dynamic26GrandchildOfInheritedInitsCACSi4objc_tcfc
// CHECK-NOT: .init!
// CHECK: }

// No vtable entry for override of @objc extension property
// CHECK-LABEL: sil_vtable [serialized] SubExt {
// CHECK-NEXT: #BaseExt.init!initializer.1: (BaseExt.Type) -> () -> BaseExt : _T07dynamic6SubExtCACycfc [override] // dynamic.SubExt.init() -> dynamic.SubExt
// CHECK-NEXT: #SubExt.deinit!deallocator: _T07dynamic6SubExtCfD // dynamic.SubExt.__deallocating_deinit
// CHECK-NEXT: }

// Dynamic thunk + vtable re-abstraction
// CHECK-LABEL: sil_vtable [serialized] ConcreteDerived {
// CHECK-NEXT: #GenericBase.method!1: <T> (GenericBase<T>) -> (T) -> () : public _T07dynamic15ConcreteDerivedC6methodySiFAA11GenericBaseCADyxFTV [override]     // vtable thunk for dynamic.GenericBase.method(A) -> () dispatching to dynamic.ConcreteDerived.method(Swift.Int) -> ()
// CHECK-NEXT: #GenericBase.init!initializer.1: <T> (GenericBase<T>.Type) -> () -> GenericBase<T> : _T07dynamic15ConcreteDerivedCACycfc [override]      // dynamic.ConcreteDerived.init() -> dynamic.ConcreteDerived
// CHECK-NEXT: #ConcreteDerived.deinit!deallocator: _T07dynamic15ConcreteDerivedCfD  // dynamic.ConcreteDerived.__deallocating_deinit
// CHECK-NEXT: }

// RUN: %swift -target x86_64-apple-macosx10.9 -sdk %S/Inputs -I %S/Inputs -enable-source-import -primary-file %s %S/Inputs/dynamic_other.swift -emit-silgen | FileCheck %s
// RUN: %swift -target x86_64-apple-macosx10.9 -sdk %S/Inputs -I %S/Inputs -enable-source-import -primary-file %s %S/Inputs/dynamic_other.swift -emit-sil -verify

import Foundation
import gizmo

class Foo: Proto {
  // Not objc or dynamic, so only a vtable entry
  init(native: Int) {}
  func nativeMethod() {}
  var nativeProp: Int = 0
  subscript(#native: Int) -> Int {
    get { return native }
    set {}
  }

  // @objc, so it has an ObjC entry point but can also be dispatched
  // by vtable
  @objc init(objc: Int) {}
  @objc func objcMethod() {}
  @objc var objcProp: Int = 0
  @objc subscript(#objc: AnyObject) -> Int {
    get { return 0 }
    set {}
  }

  // dynamic, so it has only an ObjC entry point
  dynamic init(dynamic: Int) {}
  dynamic func dynamicMethod() {}
  dynamic var dynamicProp: Int = 0
  dynamic subscript(#dynamic: Int) -> Int {
    get { return dynamic }
    set {}
  }

  func overriddenByDynamic() {}

  @NSManaged var managedProp: Int
}

protocol Proto {
  func nativeMethod()
  var nativeProp: Int { get set }
  subscript(#native: Int) -> Int { get set }

  func objcMethod()
  var objcProp: Int { get set }
  subscript(#objc: AnyObject) -> Int { get set }

  func dynamicMethod()
  var dynamicProp: Int { get set }
  subscript(#dynamic: Int) -> Int { get set }
}

// ObjC entry points for @objc and dynamic entry points

// normal and @objc initializing ctors can be statically dispatched
// CHECK-LABEL: sil hidden @_TFC7dynamic3FooCfMS0_FT6nativeSi_S0_
// CHECK:         function_ref @_TFC7dynamic3FoocfMS0_FT6nativeSi_S0_

// CHECK-LABEL: sil hidden @_TFC7dynamic3FooCfMS0_FT4objcSi_S0_
// CHECK:         function_ref @_TFC7dynamic3FoocfMS0_FT4objcSi_S0_

// CHECK-LABEL: sil hidden @_TToFC7dynamic3FoocfMS0_FT4objcSi_S0_
// CHECK-LABEL: sil hidden @_TToFC7dynamic3Foo10objcMethodfS0_FT_T_
// CHECK-LABEL: sil hidden [transparent] @_TToFC7dynamic3Foog8objcPropSi
// CHECK-LABEL: sil hidden [transparent] @_TToFC7dynamic3Foos8objcPropSi
// CHECK-LABEL: sil hidden @_TToFC7dynamic3Foog9subscriptFT4objcPSs9AnyObject__Si
// CHECK-LABEL: sil hidden @_TToFC7dynamic3Foos9subscriptFT4objcPSs9AnyObject__Si

// TODO: dynamic initializing ctor must be objc dispatched
// CHECK-LABEL: sil hidden @_TFC7dynamic3FooCfMS0_FT7dynamicSi_S0_
// CHECK:         function_ref @_TTDFC7dynamic3FoocfMS0_FT7dynamicSi_S0_
// CHECK-LABEL: sil shared [transparent] @_TTDFC7dynamic3FoocfMS0_FT7dynamicSi_S0_
// CHECK:         class_method [volatile] {{%.*}} : $Foo, #Foo.init!initializer.1.foreign :

// CHECK-LABEL: sil hidden @_TToFC7dynamic3FoocfMS0_FT7dynamicSi_S0_
// CHECK-LABEL: sil hidden @_TToFC7dynamic3Foo13dynamicMethodfS0_FT_T_
// CHECK-LABEL: sil hidden [transparent] @_TToFC7dynamic3Foog11dynamicPropSi
// CHECK-LABEL: sil hidden [transparent] @_TToFC7dynamic3Foos11dynamicPropSi
// CHECK-LABEL: sil hidden @_TToFC7dynamic3Foog9subscriptFT7dynamicSi_Si
// CHECK-LABEL: sil hidden @_TToFC7dynamic3Foos9subscriptFT7dynamicSi_Si

// Protocol witnesses use best appropriate dispatch

// Native witnesses use vtable dispatch:
// CHECK-LABEL: sil hidden @_TTWC7dynamic3FooS_5ProtoS_FS1_12nativeMethodUS1___fQPS1_FT_T_
// CHECK:         class_method {{%.*}} : $Foo, #Foo.nativeMethod!1 :
// CHECK-LABEL: sil hidden @_TTWC7dynamic3FooS_5ProtoS_FS1_g10nativePropSi
// CHECK:         class_method {{%.*}} : $Foo, #Foo.nativeProp!getter.1 :
// CHECK-LABEL: sil hidden @_TTWC7dynamic3FooS_5ProtoS_FS1_s10nativePropSi
// CHECK:         class_method {{%.*}} : $Foo, #Foo.nativeProp!setter.1 :
// CHECK-LABEL: sil hidden @_TTWC7dynamic3FooS_5ProtoS_FS1_g9subscriptFT6nativeSi_Si
// CHECK:         class_method {{%.*}} : $Foo, #Foo.subscript!getter.1 :
// CHECK-LABEL: sil hidden @_TTWC7dynamic3FooS_5ProtoS_FS1_s9subscriptFT6nativeSi_Si
// CHECK:         class_method {{%.*}} : $Foo, #Foo.subscript!setter.1 :

// @objc witnesses use vtable dispatch:
// CHECK-LABEL: sil hidden @_TTWC7dynamic3FooS_5ProtoS_FS1_10objcMethodUS1___fQPS1_FT_T_
// CHECK:         class_method {{%.*}} : $Foo, #Foo.objcMethod!1 :
// CHECK-LABEL: sil hidden @_TTWC7dynamic3FooS_5ProtoS_FS1_g8objcPropSi
// CHECK:         class_method {{%.*}} : $Foo, #Foo.objcProp!getter.1 :
// CHECK-LABEL: sil hidden @_TTWC7dynamic3FooS_5ProtoS_FS1_s8objcPropSi
// CHECK:         class_method {{%.*}} : $Foo, #Foo.objcProp!setter.1 :
// CHECK-LABEL: sil hidden @_TTWC7dynamic3FooS_5ProtoS_FS1_g9subscriptFT4objcPSs9AnyObject__Si
// CHECK:         class_method {{%.*}} : $Foo, #Foo.subscript!getter.1 :
// CHECK-LABEL: sil hidden @_TTWC7dynamic3FooS_5ProtoS_FS1_s9subscriptFT4objcPSs9AnyObject__Si
// CHECK:         class_method {{%.*}} : $Foo, #Foo.subscript!setter.1 :

// Dynamic witnesses use objc dispatch:
// CHECK-LABEL: sil hidden @_TTWC7dynamic3FooS_5ProtoS_FS1_13dynamicMethodUS1___fQPS1_FT_T_
// CHECK:         function_ref @_TTDFC7dynamic3Foo13dynamicMethodfS0_FT_T_
// CHECK-LABEL: sil shared [transparent] @_TTDFC7dynamic3Foo13dynamicMethodfS0_FT_T_
// CHECK:         class_method [volatile] {{%.*}} : $Foo, #Foo.dynamicMethod!1.foreign :

// CHECK-LABEL: sil hidden @_TTWC7dynamic3FooS_5ProtoS_FS1_g11dynamicPropSi
// CHECK:         function_ref @_TTDFC7dynamic3Foog11dynamicPropSi
// CHECK-LABEL: sil shared [transparent] @_TTDFC7dynamic3Foog11dynamicPropSi
// CHECK:         class_method [volatile] {{%.*}} : $Foo, #Foo.dynamicProp!getter.1.foreign :

// CHECK-LABEL: sil hidden @_TTWC7dynamic3FooS_5ProtoS_FS1_s11dynamicPropSi
// CHECK:         function_ref @_TTDFC7dynamic3Foos11dynamicPropSi
// CHECK-LABEL: sil shared [transparent] @_TTDFC7dynamic3Foos11dynamicPropSi
// CHECK:         class_method [volatile] {{%.*}} : $Foo, #Foo.dynamicProp!setter.1.foreign :

// CHECK-LABEL: sil hidden @_TTWC7dynamic3FooS_5ProtoS_FS1_g9subscriptFT7dynamicSi_Si
// CHECK:         function_ref @_TTDFC7dynamic3Foog9subscriptFT7dynamicSi_Si
// CHECK-LABEL: sil shared [transparent] @_TTDFC7dynamic3Foog9subscriptFT7dynamicSi_Si
// CHECK:         class_method [volatile] {{%.*}} : $Foo, #Foo.subscript!getter.1.foreign :

// CHECK-LABEL: sil hidden @_TTWC7dynamic3FooS_5ProtoS_FS1_s9subscriptFT7dynamicSi_Si
// CHECK:         function_ref @_TTDFC7dynamic3Foos9subscriptFT7dynamicSi_Si
// CHECK-LABEL: sil shared [transparent] @_TTDFC7dynamic3Foos9subscriptFT7dynamicSi_Si
// CHECK:         class_method [volatile] {{%.*}} : $Foo, #Foo.subscript!setter.1.foreign :

// Superclass dispatch
class Subclass: Foo {
  // Native and objc methods can directly reference super members
  override init(native: Int) {
    super.init(native: native)
  }
  // CHECK-LABEL: sil hidden @_TFC7dynamic8SubclassCfMS0_FT6nativeSi_S0_
  // CHECK:         function_ref @_TFC7dynamic8SubclasscfMS0_FT6nativeSi_S0_

  override func nativeMethod() {
    super.nativeMethod()
  }
  // CHECK-LABEL: sil hidden @_TFC7dynamic8Subclass12nativeMethodfS0_FT_T_
  // CHECK:         function_ref @_TFC7dynamic3Foo12nativeMethodfS0_FT_T_

  override var nativeProp: Int {
    get { return super.nativeProp }
    // CHECK-LABEL: sil hidden @_TFC7dynamic8Subclassg10nativePropSi
    // CHECK:         function_ref @_TFC7dynamic3Foog10nativePropSi
    set { super.nativeProp = newValue }
    // CHECK-LABEL: sil hidden @_TFC7dynamic8Subclasss10nativePropSi
    // CHECK:         function_ref @_TFC7dynamic3Foos10nativePropSi
  }

  override subscript(#native: Int) -> Int {
    get { return super[native: native] }
    // CHECK-LABEL: sil hidden @_TFC7dynamic8Subclassg9subscriptFT6nativeSi_Si
    // CHECK:         function_ref @_TFC7dynamic3Foog9subscriptFT6nativeSi_Si
    set { super[native: native] = newValue }
    // CHECK-LABEL: sil hidden @_TFC7dynamic8Subclasss9subscriptFT6nativeSi_Si
    // CHECK:         function_ref @_TFC7dynamic3Foos9subscriptFT6nativeSi_Si
  }

  override init(objc: Int) {
    super.init(objc: objc)
  }
  // CHECK-LABEL: sil hidden @_TFC7dynamic8SubclasscfMS0_FT4objcSi_S0_
  // CHECK:         function_ref @_TFC7dynamic3FoocfMS0_FT4objcSi_S0_

  override func objcMethod() {
    super.objcMethod()
  }
  // CHECK-LABEL: sil hidden @_TFC7dynamic8Subclass10objcMethodfS0_FT_T_
  // CHECK:         function_ref @_TFC7dynamic3Foo10objcMethodfS0_FT_T_

  override var objcProp: Int {
    get { return super.objcProp }
    // CHECK-LABEL: sil hidden @_TFC7dynamic8Subclassg8objcPropSi
    // CHECK:         function_ref @_TFC7dynamic3Foog8objcPropSi
    set { super.objcProp = newValue }
    // CHECK-LABEL: sil hidden @_TFC7dynamic8Subclasss8objcPropSi
    // CHECK:         function_ref @_TFC7dynamic3Foos8objcPropSi
  }

  override subscript(#objc: AnyObject) -> Int {
    get { return super[objc: objc] }
    // CHECK-LABEL: sil hidden @_TFC7dynamic8Subclassg9subscriptFT4objcPSs9AnyObject__Si
    // CHECK:         function_ref @_TFC7dynamic3Foog9subscriptFT4objcPSs9AnyObject__Si
    set { super[objc: objc] = newValue }
    // CHECK-LABEL: sil hidden @_TFC7dynamic8Subclasss9subscriptFT4objcPSs9AnyObject__Si
    // CHECK:         function_ref @_TFC7dynamic3Foos9subscriptFT4objcPSs9AnyObject__Si
  }

  // Dynamic methods are super-dispatched by objc_msgSend
  override init(dynamic: Int) {
    super.init(dynamic: dynamic)
  }
  // CHECK-LABEL: sil hidden @_TFC7dynamic8SubclasscfMS0_FT7dynamicSi_S0_
  // CHECK:         super_method [volatile] {{%.*}} : $Subclass, #Foo.init!initializer.1.foreign :

  override func dynamicMethod() {
    super.dynamicMethod()
  }
  // CHECK-LABEL: sil hidden @_TFC7dynamic8Subclass13dynamicMethodfS0_FT_T_
  // CHECK:         super_method [volatile] {{%.*}} : $Subclass, #Foo.dynamicMethod!1.foreign :

  override var dynamicProp: Int {
    get { return super.dynamicProp }
    // CHECK-LABEL: sil hidden @_TFC7dynamic8Subclassg11dynamicPropSi
    // CHECK:         super_method [volatile] {{%.*}} : $Subclass, #Foo.dynamicProp!getter.1.foreign :
    set { super.dynamicProp = newValue }
    // CHECK-LABEL: sil hidden @_TFC7dynamic8Subclasss11dynamicPropSi
    // CHECK:         super_method [volatile] {{%.*}} : $Subclass, #Foo.dynamicProp!setter.1.foreign :
  }

  override subscript(#dynamic: Int) -> Int {
    get { return super[dynamic: dynamic] }
    // CHECK-LABEL: sil hidden @_TFC7dynamic8Subclassg9subscriptFT7dynamicSi_Si
    // CHECK:         super_method [volatile] {{%.*}} : $Subclass, #Foo.subscript!getter.1.foreign :
    set { super[dynamic: dynamic] = newValue }
    // CHECK-LABEL: sil hidden @_TFC7dynamic8Subclasss9subscriptFT7dynamicSi_Si
    // CHECK:         super_method [volatile] {{%.*}} : $Subclass, #Foo.subscript!setter.1.foreign :
  }

  dynamic override func overriddenByDynamic() {}
}

// CHECK-LABEL: sil hidden @_TF7dynamic20nativeMethodDispatchFT_T_ : $@thin () -> ()
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

// CHECK-LABEL: sil hidden @_TF7dynamic18objcMethodDispatchFT_T_ : $@thin () -> ()
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
  let y = c[objc: 0 as NSNumber]
  // CHECK: class_method {{%.*}} : $Foo, #Foo.subscript!setter.1 :
  c[objc: 0 as NSNumber] = y
}

// CHECK-LABEL: sil hidden @_TF7dynamic21dynamicMethodDispatchFT_T_ : $@thin () -> ()
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

// CHECK-LABEL: sil hidden @_TF7dynamic15managedDispatchFCS_3FooT_
func managedDispatch(c: Foo) {
  // CHECK: class_method [volatile] {{%.*}} : $Foo, #Foo.managedProp!getter.1.foreign 
  let x = c.managedProp
  // CHECK: class_method [volatile] {{%.*}} : $Foo, #Foo.managedProp!setter.1.foreign
  c.managedProp = x
}

// CHECK-LABEL: sil hidden @_TF7dynamic21foreignMethodDispatchFT_T_
func foreignMethodDispatch() {
  // CHECK: function_ref @_TFCSo9GuisemeauCfMS_FT_GSQS__
  let g = Guisemeau()
  // CHECK: class_method [volatile] {{%.*}} : $Gizmo, #Gizmo.frob!1.foreign
  g.frob()
  // CHECK: class_method [volatile] {{%.*}} : $Gizmo, #Gizmo.count!getter.1.foreign
  let x = g.count
  // CHECK: class_method [volatile] {{%.*}} : $Gizmo, #Gizmo.count!setter.1.foreign
  g.count = x
  // CHECK: class_method [volatile] {{%.*}} : $Guisemeau, #Guisemeau.subscript!getter.1.foreign
  let y: AnyObject! = g[0]
  // CHECK: class_method [volatile] {{%.*}} : $Guisemeau, #Guisemeau.subscript!setter.1.foreign
  g[0] = y
  // CHECK: class_method [volatile] {{%.*}} : $NSObject, #NSObject.description!getter.1.foreign
  let z = g.description
}

extension Gizmo {
  // CHECK-LABEL: sil hidden @_TFE7dynamicCSo5GizmocfMS0_FT22convenienceInExtensionSi_S0_
  // CHECK:         class_method [volatile] {{%.*}} : $Gizmo, #Gizmo.init!initializer.1.foreign
  convenience init(convenienceInExtension: Int) {
    self.init(bellsOn: convenienceInExtension)
  }

  // CHECK-LABEL: sil hidden @_TFE7dynamicCSo5GizmoCfMS0_FT19foreignClassFactorySi_S0_
  // CHECK:         class_method [volatile] {{%.*}} : $@thick Gizmo.Type, #Gizmo.init!allocator.1.foreign
  convenience init(foreignClassFactory x: Int) {
    self.init(stuff: x)
  }

  // CHECK-LABEL: sil hidden @_TFE7dynamicCSo5GizmoCfMS0_FT24foreignClassExactFactorySi_S0_
  // CHECK:         class_method [volatile] {{%.*}} : $@thick Gizmo.Type, #Gizmo.init!allocator.1.foreign
  convenience init(foreignClassExactFactory x: Int) {
    self.init(exactlyStuff: x)
  }

  @objc func foreignObjCExtension() { }
  dynamic func foreignDynamicExtension() { }
}

// CHECK-LABEL: sil hidden @_TF7dynamic24foreignExtensionDispatchFCSo5GizmoT_
func foreignExtensionDispatch(g: Gizmo) {
  // CHECK: class_method [volatile] %0 : $Gizmo, #Gizmo.foreignObjCExtension!1.foreign : Gizmo
  g.foreignObjCExtension()
  // CHECK: class_method [volatile] %0 : $Gizmo, #Gizmo.foreignDynamicExtension!1.foreign
  g.foreignDynamicExtension()
}


// CHECK-LABEL: sil hidden @_TF7dynamic33nativeMethodDispatchFromOtherFileFT_T_ : $@thin () -> ()
func nativeMethodDispatchFromOtherFile() {
  // CHECK: function_ref @_TFC7dynamic13FromOtherFileCfMS0_FT6nativeSi_S0_
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

// CHECK-LABEL: sil hidden @_TF7dynamic31objcMethodDispatchFromOtherFileFT_T_ : $@thin () -> ()
func objcMethodDispatchFromOtherFile() {
  // CHECK: function_ref @_TFC7dynamic13FromOtherFileCfMS0_FT4objcSi_S0_
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

// CHECK-LABEL: sil hidden @_TF7dynamic34dynamicMethodDispatchFromOtherFileFT_T_ : $@thin () -> ()
func dynamicMethodDispatchFromOtherFile() {
  // CHECK: function_ref @_TFC7dynamic13FromOtherFileCfMS0_FT7dynamicSi_S0_
  let c = FromOtherFile(dynamic: 0)
  // CHECK: class_method [volatile] {{%.*}} : $FromOtherFile, #FromOtherFile.dynamicMethod!1.foreign
  c.dynamicMethod()
  // CHECK: class_method [volatile] {{%.*}} : $FromOtherFile, #FromOtherFile.dynamicProp!getter.1.foreign
  let x = c.dynamicProp
  // CHECK: class_method [volatile] {{%.*}} : $FromOtherFile, #FromOtherFile.dynamicProp!setter.1.foreign
  c.dynamicProp = x
  // CHECK: class_method [volatile] {{%.*}} : $FromOtherFile, #FromOtherFile.subscript!getter.1.foreign
  let y = c[dynamic: 0]
  // CHECK: class_method [volatile] {{%.*}} : $FromOtherFile, #FromOtherFile.subscript!setter.1.foreign
  c[dynamic: 0] = y
}

// CHECK-LABEL: sil hidden @_TF7dynamic28managedDispatchFromOtherFileFCS_13FromOtherFileT_
func managedDispatchFromOtherFile(c: FromOtherFile) {
  // CHECK: class_method [volatile] {{%.*}} : $FromOtherFile, #FromOtherFile.managedProp!getter.1.foreign
  let x = c.managedProp
  // CHECK: class_method [volatile] {{%.*}} : $FromOtherFile, #FromOtherFile.managedProp!setter.1.foreign
  c.managedProp = x
}

// CHECK-LABEL: sil hidden @_TF7dynamic23dynamicExtensionMethodsFCS_13ObjCOtherFileT_
func dynamicExtensionMethods(obj: ObjCOtherFile) {
  // CHECK: class_method [volatile] {{%.*}} : $ObjCOtherFile, #ObjCOtherFile.extensionMethod!1.foreign
  obj.extensionMethod()
  // CHECK: class_method [volatile] {{%.*}} : $ObjCOtherFile, #ObjCOtherFile.extensionProp!getter.1.foreign
  _ = obj.extensionProp

  // CHECK: class_method [volatile] {{%.*}} : $@thick ObjCOtherFile.Type, #ObjCOtherFile.extensionClassProp!getter.1.foreign
  // CHECK-NEXT: thick_to_objc_metatype {{%.*}} : $@thick ObjCOtherFile.Type to $@objc_metatype ObjCOtherFile.Type
  _ = obj.dynamicType.extensionClassProp

  // CHECK: class_method [volatile] {{%.*}} : $ObjCOtherFile, #ObjCOtherFile.dynExtensionMethod!1.foreign
  obj.dynExtensionMethod()
  // CHECK: class_method [volatile] {{%.*}} : $ObjCOtherFile, #ObjCOtherFile.dynExtensionProp!getter.1.foreign
  _ = obj.dynExtensionProp

  // CHECK: class_method [volatile] {{%.*}} : $@thick ObjCOtherFile.Type, #ObjCOtherFile.dynExtensionClassProp!getter.1.foreign
  // CHECK-NEXT: thick_to_objc_metatype {{%.*}} : $@thick ObjCOtherFile.Type to $@objc_metatype ObjCOtherFile.Type
  _ = obj.dynamicType.dynExtensionClassProp
}


// Vtable contains entries for native and @objc methods, but not dynamic ones
// CHECK-LABEL: sil_vtable Foo {
// CHECK-LABEL:   #Foo.init!initializer.1:   _TFC7dynamic3FoocfMS0_FT6nativeSi_S0_        // dynamic.Foo.init (dynamic.Foo.Type)(native : Swift.Int) -> dynamic.Foo
// CHECK-LABEL:   #Foo.nativeMethod!1:       _TFC7dynamic3Foo12nativeMethodfS0_FT_T_  // dynamic.Foo.nativeMethod (dynamic.Foo)() -> ()
// CHECK-LABEL:   #Foo.subscript!getter.1:   _TFC7dynamic3Foog9subscriptFT6nativeSi_Si    // dynamic.Foo.subscript.getter (native : Swift.Int) -> Swift.Int
// CHECK-LABEL:   #Foo.subscript!setter.1:   _TFC7dynamic3Foos9subscriptFT6nativeSi_Si    // dynamic.Foo.subscript.setter (native : Swift.Int) -> Swift.Int
// CHECK-LABEL:   #Foo.init!initializer.1:   _TFC7dynamic3FoocfMS0_FT4objcSi_S0_  // dynamic.Foo.init (dynamic.Foo.Type)(objc : Swift.Int) -> dynamic.Foo
// CHECK-LABEL:   #Foo.objcMethod!1:         _TFC7dynamic3Foo10objcMethodfS0_FT_T_      // dynamic.Foo.objcMethod (dynamic.Foo)() -> ()
// CHECK-LABEL:   #Foo.subscript!getter.1: _TFC7dynamic3Foog9subscriptFT4objcPSs9AnyObject__Si // dynamic.Foo.subscript.getter (objc : Swift.AnyObject) -> Swift.Int
// CHECK-LABEL:   #Foo.subscript!setter.1: _TFC7dynamic3Foos9subscriptFT4objcPSs9AnyObject__Si // dynamic.Foo.subscript.setter (objc : Swift.AnyObject) -> Swift.Int
// CHECK-NOT:     dynamic.Foo.init (dynamic.Foo.Type)(dynamic : Swift.Int) -> dynamic.Foo
// CHECK-NOT:     dynamic.Foo.dynamicMethod
// CHECK-NOT:     dynamic.Foo.subscript.getter (dynamic : Swift.Int) -> Swift.Int
// CHECK-NOT:     dynamic.Foo.subscript.setter (dynamic : Swift.Int) -> Swift.Int
// CHECK-LABEL:   #Foo.overriddenByDynamic!1: _TFC7dynamic3Foo19overriddenByDynamicfS0_FT_T_
// CHECK-LABEL:   #Foo.nativeProp!getter.1:  _TFC7dynamic3Foog10nativePropSi     // dynamic.Foo.nativeProp.getter : Swift.Int
// CHECK-LABEL:   #Foo.nativeProp!setter.1:  _TFC7dynamic3Foos10nativePropSi     // dynamic.Foo.nativeProp.setter : Swift.Int
// CHECK-LABEL:   #Foo.objcProp!getter.1:    _TFC7dynamic3Foog8objcPropSi  // dynamic.Foo.objcProp.getter : Swift.Int
// CHECK-LABEL:   #Foo.objcProp!setter.1:    _TFC7dynamic3Foos8objcPropSi  // dynamic.Foo.objcProp.setter : Swift.Int
// CHECK-NOT:     dynamic.Foo.dynamicProp.getter
// CHECK-NOT:     dynamic.Foo.dynamicProp.setter

// Vtable uses a dynamic thunk for dynamic overrides
// CHECK-LABEL: sil_vtable Subclass {
// CHECK-LABEL:   #Foo.overriddenByDynamic!1: _TTDFC7dynamic8Subclass19overriddenByDynamicfS0_FT_T_



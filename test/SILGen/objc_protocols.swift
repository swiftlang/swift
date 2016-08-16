// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen -disable-objc-attr-requires-foundation-module | %FileCheck %s

// REQUIRES: objc_interop

import gizmo
import objc_protocols_Bas

@objc protocol NSRuncing {
  func runce() -> NSObject
  func copyRuncing() -> NSObject

  func foo()

  static func mince() -> NSObject
}

@objc protocol NSFunging {
  func funge()

  func foo()
}

protocol Ansible {
  func anse()
}

// CHECK-LABEL: sil hidden  @_TF14objc_protocols12objc_generic
func objc_generic<T : NSRuncing>(_ x: T) -> (NSObject, NSObject) {
  return (x.runce(), x.copyRuncing())
  // -- Result of runce is autoreleased according to default objc conv
  // CHECK: [[METHOD:%.*]] = witness_method [volatile] {{\$.*}},  #NSRuncing.runce!1.foreign
  // CHECK: [[RESULT1:%.*]] = apply [[METHOD]]<T>([[THIS1:%.*]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : NSRuncing> (τ_0_0) -> @autoreleased NSObject

  // -- Result of copyRuncing is received retained according to -copy family
  // CHECK: [[METHOD:%.*]] = witness_method [volatile] {{\$.*}},  #NSRuncing.copyRuncing!1.foreign
  // CHECK: [[RESULT2:%.*]] = apply [[METHOD]]<T>([[THIS2:%.*]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : NSRuncing> (τ_0_0) -> @owned NSObject

  // -- Arguments are not consumed by objc calls
  // CHECK: release [[THIS2]]
}

func objc_generic_partial_apply<T : NSRuncing>(_ x: T) {
  // CHECK:      [[FN:%.*]] = function_ref @_TTOFP14objc_protocols9NSRuncing5runceFT_CSo8NSObject
  // CHECK-NEXT: strong_retain %0
  // CHECK-NEXT: [[METHOD:%.*]] = apply [[FN]]<T>(%0)
  // CHECK-NEXT: strong_release [[METHOD]]
  _ = x.runce

  // CHECK:      [[FN:%.*]] = function_ref @_TTOFP14objc_protocols9NSRuncing5runceFT_CSo8NSObject
  // CHECK-NEXT: [[METHOD:%.*]] = partial_apply [[FN]]<T>()
  // CHECK-NEXT: strong_release [[METHOD]]
  _ = T.runce

  // CHECK:      [[FN:%.*]] = function_ref @_TTOZFP14objc_protocols9NSRuncing5minceFT_CSo8NSObject
  // CHECK-NEXT: [[METATYPE:%.*]] = metatype $@thick T.Type
  // CHECK-NEXT: [[METHOD:%.*]] = apply [[FN]]<T>([[METATYPE]])
  // CHECK-NEXT: strong_release [[METHOD:%.*]]
  _ = T.mince
}

// CHECK-LABEL: sil shared [thunk] @_TTOFP14objc_protocols9NSRuncing5runceFT_CSo8NSObject
// CHECK:      [[FN:%.*]] = function_ref @_TTOFP14objc_protocols9NSRuncing5runcefT_CSo8NSObject
// CHECK-NEXT: [[METHOD:%.*]] = partial_apply [[FN]]<Self>(%0)
// CHECK-NEXT: return [[METHOD]]

// CHECK-LABEL: sil shared [thunk] @_TTOFP14objc_protocols9NSRuncing5runcefT_CSo8NSObject
// CHECK:      strong_retain %0
// CHECK-NEXT: [[FN:%.*]] = witness_method $Self, #NSRuncing.runce!1.foreign
// CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]<Self>(%0)
// CHECK-NEXT: strong_release %0
// CHECK-NEXT: return [[RESULT]]

// CHECK-LABEL: sil shared [thunk] @_TTOZFP14objc_protocols9NSRuncing5minceFT_CSo8NSObject
// CHECK:      [[FN:%.*]] = function_ref @_TTOZFP14objc_protocols9NSRuncing5mincefT_CSo8NSObject
// CHECK-NEXT: [[METHOD:%.*]] = partial_apply [[FN]]<Self>(%0)
// CHECK-NEXT: return [[METHOD]]

// CHECK-LABEL: sil shared [thunk] @_TTOZFP14objc_protocols9NSRuncing5mincefT_CSo8NSObject
// CHECK:      [[FN:%.*]] = witness_method $Self, #NSRuncing.mince!1.foreign
// CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]<Self>(%0)
// CHECK-NEXT: return [[RESULT]]

// CHECK-LABEL: sil hidden  @_TF14objc_protocols13objc_protocol
func objc_protocol(_ x: NSRuncing) -> (NSObject, NSObject) {
  return (x.runce(), x.copyRuncing())
  // -- Result of runce is autoreleased according to default objc conv
  // CHECK: [[THIS1:%.*]] = open_existential_ref [[THIS1_ORIG:%.*]] : $NSRuncing to $[[OPENED:@opened(.*) NSRuncing]]
  // CHECK: [[METHOD:%.*]] = witness_method [volatile] $[[OPENED]], #NSRuncing.runce!1.foreign
  // CHECK: [[RESULT1:%.*]] = apply [[METHOD]]<[[OPENED]]>([[THIS1]])

  // -- Result of copyRuncing is received retained according to -copy family
  // CHECK: [[THIS2:%.*]] = open_existential_ref [[THIS2_ORIG:%.*]] : $NSRuncing to $[[OPENED2:@opened(.*) NSRuncing]]
  // CHECK: [[METHOD:%.*]] = witness_method [volatile] $[[OPENED2]], #NSRuncing.copyRuncing!1.foreign
  // CHECK: [[RESULT2:%.*]] = apply [[METHOD]]<[[OPENED2]]>([[THIS2:%.*]])

  // -- Arguments are not consumed by objc calls
  // CHECK: release [[THIS2_ORIG]]
}

func objc_protocol_partial_apply(_ x: NSRuncing) {
  // CHECK: [[THIS1:%.*]] = open_existential_ref %0 : $NSRuncing to $[[OPENED:@opened(.*) NSRuncing]]
  // CHECK: [[FN:%.*]] = function_ref @_TTOFP14objc_protocols9NSRuncing5runceFT_CSo8NSObject
  // CHECK: strong_retain [[THIS1]]
  // CHECK: [[RESULT:%.*]] = apply [[FN]]<[[OPENED]]>([[THIS1]])
  // CHECK: strong_release [[RESULT]]
  // CHECK: strong_release %0
  _ = x.runce

  // FIXME: rdar://21289579
  // _ = NSRuncing.runce
}

// CHECK-LABEL: sil hidden  @_TF14objc_protocols25objc_protocol_composition
func objc_protocol_composition(_ x: NSRuncing & NSFunging) {
  // CHECK: [[THIS:%.*]] = open_existential_ref [[THIS_ORIG:%.*]] : $NSFunging & NSRuncing to $[[OPENED:@opened(.*) NSFunging & NSRuncing]]
  // CHECK: [[METHOD:%.*]] = witness_method [volatile] $[[OPENED]], #NSRuncing.runce!1.foreign
  // CHECK: apply [[METHOD]]<[[OPENED]]>([[THIS]])
  x.runce()

  // CHECK: [[THIS:%.*]] = open_existential_ref [[THIS_ORIG:%.*]] : $NSFunging & NSRuncing to $[[OPENED:@opened(.*) NSFunging & NSRuncing]]
  // CHECK: [[METHOD:%.*]] = witness_method [volatile] $[[OPENED]], #NSFunging.funge!1.foreign
  // CHECK: apply [[METHOD]]<[[OPENED]]>([[THIS]])
  x.funge()
}
// -- ObjC thunks get emitted for ObjC protocol conformances

class Foo : NSRuncing, NSFunging, Ansible {
  // -- NSRuncing
  @objc func runce() -> NSObject { return NSObject() }
  @objc func copyRuncing() -> NSObject { return NSObject() }

  @objc static func mince() -> NSObject { return NSObject() }

  // -- NSFunging
  @objc func funge() {}

  // -- Both NSRuncing and NSFunging
  @objc func foo() {}

  // -- Ansible
  func anse() {}
}

// CHECK-LABEL: sil hidden [thunk] @_TToFC14objc_protocols3Foo5runce
// CHECK-LABEL: sil hidden [thunk] @_TToFC14objc_protocols3Foo11copyRuncing
// CHECK-LABEL: sil hidden [thunk] @_TToFC14objc_protocols3Foo5funge
// CHECK-LABEL: sil hidden [thunk] @_TToFC14objc_protocols3Foo3foo
// CHECK-NOT: sil hidden @_TToF{{.*}}anse{{.*}}

class Bar { }

extension Bar : NSRuncing {
  @objc func runce() -> NSObject { return NSObject() }
  @objc func copyRuncing() -> NSObject { return NSObject() }
  @objc func foo() {}
  @objc static func mince() -> NSObject { return NSObject() }
}

// CHECK-LABEL: sil hidden [thunk] @_TToFC14objc_protocols3Bar5runce
// CHECK-LABEL: sil hidden [thunk] @_TToFC14objc_protocols3Bar11copyRuncing
// CHECK-LABEL: sil hidden [thunk] @_TToFC14objc_protocols3Bar3foo

// class Bas from objc_protocols_Bas module
extension Bas : NSRuncing {
  // runce() implementation from the original definition of Bas
  @objc func copyRuncing() -> NSObject { return NSObject() }
  @objc func foo() {}
  @objc static func mince() -> NSObject { return NSObject() }
}

// CHECK-LABEL: sil hidden [thunk] @_TToFE14objc_protocolsC18objc_protocols_Bas3Bas11copyRuncing
// CHECK-LABEL: sil hidden [thunk] @_TToFE14objc_protocolsC18objc_protocols_Bas3Bas3foo

// -- Inherited objc protocols

protocol Fungible : NSFunging { }

class Zim : Fungible {
  @objc func funge() {}
  @objc func foo() {}
}

// CHECK-LABEL: sil hidden [thunk] @_TToFC14objc_protocols3Zim5funge
// CHECK-LABEL: sil hidden [thunk] @_TToFC14objc_protocols3Zim3foo

// class Zang from objc_protocols_Bas module
extension Zang : Fungible {
  // funge() implementation from the original definition of Zim
  @objc func foo() {}
}

// CHECK-LABEL: sil hidden [thunk] @_TToFE14objc_protocolsC18objc_protocols_Bas4Zang3foo

// -- objc protocols with property requirements in extensions
//    <rdar://problem/16284574>

@objc protocol NSCounting {
  var count: Int {get}
}

class StoredPropertyCount {
  @objc let count = 0
}

extension StoredPropertyCount: NSCounting {}
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TToFC14objc_protocols19StoredPropertyCountg5countSi

class ComputedPropertyCount {
  @objc var count: Int { return 0 }
}

extension ComputedPropertyCount: NSCounting {}
// CHECK-LABEL: sil hidden [thunk] @_TToFC14objc_protocols21ComputedPropertyCountg5countSi

// -- adding @objc protocol conformances to native ObjC classes should not
//    emit thunks since the methods are already available to ObjC.

// Gizmo declared in Inputs/usr/include/Gizmo.h
extension Gizmo : NSFunging { }

// CHECK-NOT: _TTo{{.*}}5Gizmo{{.*}}

@objc class InformallyFunging {
  @objc func funge() {}
  @objc func foo() {}
}

extension InformallyFunging: NSFunging { }

@objc protocol Initializable {
  init(int: Int)
}

// CHECK-LABEL: sil hidden @_TF14objc_protocols28testInitializableExistential
func testInitializableExistential(_ im: Initializable.Type, i: Int) -> Initializable {
  // CHECK: bb0([[META:%[0-9]+]] : $@thick Initializable.Type, [[I:%[0-9]+]] : $Int):
// CHECK:   [[I2_BOX:%[0-9]+]] = alloc_box $Initializable
// CHECK:   [[PB:%.*]] = project_box [[I2_BOX]]
// CHECK:   [[ARCHETYPE_META:%[0-9]+]] = open_existential_metatype [[META]] : $@thick Initializable.Type to $@thick (@opened([[N:".*"]]) Initializable).Type
// CHECK:   [[ARCHETYPE_META_OBJC:%[0-9]+]] = thick_to_objc_metatype [[ARCHETYPE_META]] : $@thick (@opened([[N]]) Initializable).Type to $@objc_metatype (@opened([[N]]) Initializable).Type
// CHECK:   [[I2_ALLOC:%[0-9]+]] = alloc_ref_dynamic [objc] [[ARCHETYPE_META_OBJC]] : $@objc_metatype (@opened([[N]]) Initializable).Type, $@opened([[N]]) Initializable
// CHECK:   [[INIT_WITNESS:%[0-9]+]] = witness_method [volatile] $@opened([[N]]) Initializable, #Initializable.init!initializer.1.foreign, [[ARCHETYPE_META]]{{.*}} : $@convention(objc_method) <τ_0_0 where τ_0_0 : Initializable> (Int, @owned τ_0_0) -> @owned τ_0_0
// CHECK:   [[I2:%[0-9]+]] = apply [[INIT_WITNESS]]<@opened([[N]]) Initializable>([[I]], [[I2_ALLOC]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : Initializable> (Int, @owned τ_0_0) -> @owned τ_0_0
// CHECK:   [[I2_EXIST_CONTAINER:%[0-9]+]] = init_existential_ref [[I2]] : $@opened([[N]]) Initializable : $@opened([[N]]) Initializable, $Initializable
// CHECK:   store [[I2_EXIST_CONTAINER]] to [[PB]] : $*Initializable
// CHECK:   [[I2:%[0-9]+]] = load [[PB]] : $*Initializable
// CHECK:   strong_retain [[I2]] : $Initializable
// CHECK:   strong_release [[I2_BOX]] : $@box Initializable
// CHECK:   return [[I2]] : $Initializable
  var i2 = im.init(int: i)
  return i2
}

class InitializableConformer: Initializable {
  @objc required init(int: Int) {}
}
// CHECK-LABEL: sil hidden [thunk] @_TToFC14objc_protocols22InitializableConformerc

final class InitializableConformerByExtension {
  init() {}
}

extension InitializableConformerByExtension: Initializable {
  @objc convenience init(int: Int) {
    self.init()
  }
}
// CHECK-LABEL: sil hidden [thunk] @_TToFC14objc_protocols33InitializableConformerByExtensionc

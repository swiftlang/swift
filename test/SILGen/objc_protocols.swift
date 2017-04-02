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

// CHECK-LABEL: sil hidden @_T014objc_protocols0A8_generic{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[THIS:%.*]] : $T):
// -- Result of runce is autoreleased according to default objc conv
// CHECK: [[METHOD:%.*]] = witness_method [volatile] {{\$.*}},  #NSRuncing.runce!1.foreign
// CHECK: [[BORROWED_THIS:%.*]] = begin_borrow [[THIS]]
// CHECK: [[RESULT1:%.*]] = apply [[METHOD]]<T>([[BORROWED_THIS:%.*]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : NSRuncing> (τ_0_0) -> @autoreleased NSObject
// CHECK: end_borrow [[BORROWED_THIS]] from [[THIS]]

// -- Result of copyRuncing is received copy_valued according to -copy family
// CHECK: [[METHOD:%.*]] = witness_method [volatile] {{\$.*}},  #NSRuncing.copyRuncing!1.foreign
// CHECK: [[BORROWED_THIS:%.*]] = begin_borrow [[THIS]]
// CHECK: [[RESULT2:%.*]] = apply [[METHOD]]<T>([[BORROWED_THIS:%.*]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : NSRuncing> (τ_0_0) -> @owned NSObject
// CHECK: end_borrow [[BORROWED_THIS]] from [[THIS]]

// -- Arguments are not consumed by objc calls
// CHECK: destroy_value [[THIS]]
func objc_generic<T : NSRuncing>(_ x: T) -> (NSObject, NSObject) {
  return (x.runce(), x.copyRuncing())
}

// CHECK-LABEL: sil hidden @_T014objc_protocols0A22_generic_partial_applyyxAA9NSRuncingRzlF : $@convention(thin) <T where T : NSRuncing> (@owned T) -> () {
func objc_generic_partial_apply<T : NSRuncing>(_ x: T) {
  // CHECK: bb0([[ARG:%.*]] : $T):
  // CHECK:   [[FN:%.*]] = function_ref @[[THUNK1:_T014objc_protocols9NSRuncingP5runceSo8NSObjectCyFTcTO]] :
  // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
  // CHECK:   [[METHOD:%.*]] = apply [[FN]]<T>([[ARG_COPY]])
  // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
  // CHECK:   destroy_value [[METHOD]]
  _ = x.runce

  // CHECK:   [[FN:%.*]] = function_ref @[[THUNK1]] :
  // CHECK:   [[METHOD:%.*]] = partial_apply [[FN]]<T>()
  // CHECK:   destroy_value [[METHOD]]
  _ = T.runce

  // CHECK:   [[FN:%.*]] = function_ref @[[THUNK2:_T014objc_protocols9NSRuncingP5minceSo8NSObjectCyFZTcTO]]
  // CHECK:   [[METATYPE:%.*]] = metatype $@thick T.Type
  // CHECK:   [[METHOD:%.*]] = apply [[FN]]<T>([[METATYPE]])
  // CHECK:   destroy_value [[METHOD:%.*]]
  _ = T.mince
  // CHECK:   destroy_value [[ARG]]
}
// CHECK: } // end sil function '_T014objc_protocols0A22_generic_partial_applyyxAA9NSRuncingRzlF'

// CHECK: sil shared [serializable] [thunk] @[[THUNK1]] :
// CHECK: bb0([[SELF:%.*]] : $Self):
// CHECK:   [[FN:%.*]] = function_ref @[[THUNK1_THUNK:_T014objc_protocols9NSRuncingP5runceSo8NSObjectCyFTO]] :
// CHECK:   [[METHOD:%.*]] = partial_apply [[FN]]<Self>([[SELF]])
// CHECK:   return [[METHOD]]
// CHECK: } // end sil function '[[THUNK1]]'

// CHECK: sil shared [serializable] [thunk] @[[THUNK1_THUNK]]
// CHECK: bb0([[SELF:%.*]] : $Self):
// CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK:   [[FN:%.*]] = witness_method $Self, #NSRuncing.runce!1.foreign
// CHECK:   [[RESULT:%.*]] = apply [[FN]]<Self>([[SELF_COPY]])
// CHECK:   destroy_value [[SELF_COPY]]
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '[[THUNK1_THUNK]]'

// CHECK: sil shared [serializable] [thunk] @[[THUNK2]] :
// CHECK:   [[FN:%.*]] = function_ref @[[THUNK2_THUNK:_T014objc_protocols9NSRuncingP5minceSo8NSObjectCyFZTO]]
// CHECK:   [[METHOD:%.*]] = partial_apply [[FN]]<Self>(%0)
// CHECK:   return [[METHOD]]
// CHECK: } // end sil function '[[THUNK2]]'

// CHECK: sil shared [serializable] [thunk] @[[THUNK2_THUNK]] :
// CHECK:      [[FN:%.*]] = witness_method $Self, #NSRuncing.mince!1.foreign
// CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]<Self>(%0)
// CHECK-NEXT: return [[RESULT]]
// CHECK: } // end sil function '[[THUNK2_THUNK]]'

// CHECK-LABEL: sil hidden @_T014objc_protocols0A9_protocol{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[THIS:%.*]] : $NSRuncing):
// -- Result of runce is autoreleased according to default objc conv
// CHECK: [[BORROWED_THIS_1:%.*]] = begin_borrow [[THIS]]
// CHECK: [[THIS1:%.*]] = open_existential_ref [[BORROWED_THIS_1]] : $NSRuncing to $[[OPENED:@opened(.*) NSRuncing]]
// CHECK: [[METHOD:%.*]] = witness_method [volatile] $[[OPENED]], #NSRuncing.runce!1.foreign
// CHECK: [[RESULT1:%.*]] = apply [[METHOD]]<[[OPENED]]>([[THIS1]])

// -- Result of copyRuncing is received copy_valued according to -copy family
// CHECK: [[BORROWED_THIS_2:%.*]] = begin_borrow [[THIS]]
// CHECK: [[THIS2:%.*]] = open_existential_ref [[BORROWED_THIS_2]] : $NSRuncing to $[[OPENED2:@opened(.*) NSRuncing]]
// CHECK: [[METHOD:%.*]] = witness_method [volatile] $[[OPENED2]], #NSRuncing.copyRuncing!1.foreign
// CHECK: [[RESULT2:%.*]] = apply [[METHOD]]<[[OPENED2]]>([[THIS2:%.*]])

// -- Arguments are not consumed by objc calls
// CHECK: end_borrow [[BORROWED_THIS_2]] from [[THIS]]
// CHECK: end_borrow [[BORROWED_THIS_1]] from [[THIS]]
// CHECK: destroy_value [[THIS]]
func objc_protocol(_ x: NSRuncing) -> (NSObject, NSObject) {
  return (x.runce(), x.copyRuncing())
}

// CHECK-LABEL: sil hidden @_T014objc_protocols0A23_protocol_partial_applyyAA9NSRuncing_pF : $@convention(thin) (@owned NSRuncing) -> () {
func objc_protocol_partial_apply(_ x: NSRuncing) {
  // CHECK: bb0([[ARG:%.*]] : $NSRuncing):
  // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:   [[OPENED_ARG:%.*]] = open_existential_ref [[BORROWED_ARG]] : $NSRuncing to $[[OPENED:@opened(.*) NSRuncing]]
  // CHECK:   [[FN:%.*]] = function_ref @_T014objc_protocols9NSRuncingP5runceSo8NSObjectCyFTcTO
  // CHECK:   [[OPENED_ARG_COPY:%.*]] = copy_value [[OPENED_ARG]]
  // CHECK:   [[RESULT:%.*]] = apply [[FN]]<[[OPENED]]>([[OPENED_ARG_COPY]])
  // CHECK:   destroy_value [[RESULT]]
  // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
  // CHECK:   destroy_value [[ARG]]
  _ = x.runce

  // FIXME: rdar://21289579
  // _ = NSRuncing.runce
}
// CHECK : } // end sil function '_T014objc_protocols0A23_protocol_partial_applyyAA9NSRuncing_pF'

// CHECK-LABEL: sil hidden @_T014objc_protocols0A21_protocol_composition{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden [thunk] @_T014objc_protocols3FooC5runce{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil hidden [thunk] @_T014objc_protocols3FooC11copyRuncing{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil hidden [thunk] @_T014objc_protocols3FooC5funge{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil hidden [thunk] @_T014objc_protocols3FooC3foo{{[_0-9a-zA-Z]*}}FTo
// CHECK-NOT: sil hidden @_TToF{{.*}}anse{{.*}}

class Bar { }

extension Bar : NSRuncing {
  @objc func runce() -> NSObject { return NSObject() }
  @objc func copyRuncing() -> NSObject { return NSObject() }
  @objc func foo() {}
  @objc static func mince() -> NSObject { return NSObject() }
}

// CHECK-LABEL: sil hidden [thunk] @_T014objc_protocols3BarC5runce{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil hidden [thunk] @_T014objc_protocols3BarC11copyRuncing{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil hidden [thunk] @_T014objc_protocols3BarC3foo{{[_0-9a-zA-Z]*}}FTo

// class Bas from objc_protocols_Bas module
extension Bas : NSRuncing {
  // runce() implementation from the original definition of Bas
  @objc func copyRuncing() -> NSObject { return NSObject() }
  @objc func foo() {}
  @objc static func mince() -> NSObject { return NSObject() }
}

// CHECK-LABEL: sil hidden [thunk] @_T018objc_protocols_Bas0C0C0a1_B0E11copyRuncing{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil hidden [thunk] @_T018objc_protocols_Bas0C0C0a1_B0E3foo{{[_0-9a-zA-Z]*}}FTo

// -- Inherited objc protocols

protocol Fungible : NSFunging { }

class Zim : Fungible {
  @objc func funge() {}
  @objc func foo() {}
}

// CHECK-LABEL: sil hidden [thunk] @_T014objc_protocols3ZimC5funge{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil hidden [thunk] @_T014objc_protocols3ZimC3foo{{[_0-9a-zA-Z]*}}FTo

// class Zang from objc_protocols_Bas module
extension Zang : Fungible {
  // funge() implementation from the original definition of Zim
  @objc func foo() {}
}

// CHECK-LABEL: sil hidden [thunk] @_T018objc_protocols_Bas4ZangC0a1_B0E3foo{{[_0-9a-zA-Z]*}}FTo

// -- objc protocols with property requirements in extensions
//    <rdar://problem/16284574>

@objc protocol NSCounting {
  var count: Int {get}
}

class StoredPropertyCount {
  @objc let count = 0
}

extension StoredPropertyCount: NSCounting {}
// CHECK-LABEL: sil hidden [transparent] [thunk] @_T014objc_protocols19StoredPropertyCountC5countSifgTo

class ComputedPropertyCount {
  @objc var count: Int { return 0 }
}

extension ComputedPropertyCount: NSCounting {}
// CHECK-LABEL: sil hidden [thunk] @_T014objc_protocols21ComputedPropertyCountC5countSifgTo

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

// CHECK-LABEL: sil hidden @_T014objc_protocols28testInitializableExistentialAA0D0_pAaC_pXp_Si1itF : $@convention(thin) (@thick Initializable.Type, Int) -> @owned Initializable {
func testInitializableExistential(_ im: Initializable.Type, i: Int) -> Initializable {
  // CHECK: bb0([[META:%[0-9]+]] : $@thick Initializable.Type, [[I:%[0-9]+]] : $Int):
  // CHECK:   [[I2_BOX:%[0-9]+]] = alloc_box ${ var Initializable }
  // CHECK:   [[PB:%.*]] = project_box [[I2_BOX]]
  // CHECK:   [[ARCHETYPE_META:%[0-9]+]] = open_existential_metatype [[META]] : $@thick Initializable.Type to $@thick (@opened([[N:".*"]]) Initializable).Type
  // CHECK:   [[ARCHETYPE_META_OBJC:%[0-9]+]] = thick_to_objc_metatype [[ARCHETYPE_META]] : $@thick (@opened([[N]]) Initializable).Type to $@objc_metatype (@opened([[N]]) Initializable).Type
  // CHECK:   [[I2_ALLOC:%[0-9]+]] = alloc_ref_dynamic [objc] [[ARCHETYPE_META_OBJC]] : $@objc_metatype (@opened([[N]]) Initializable).Type, $@opened([[N]]) Initializable
  // CHECK:   [[INIT_WITNESS:%[0-9]+]] = witness_method [volatile] $@opened([[N]]) Initializable, #Initializable.init!initializer.1.foreign : {{.*}}, [[ARCHETYPE_META]]{{.*}} : $@convention(objc_method) <τ_0_0 where τ_0_0 : Initializable> (Int, @owned τ_0_0) -> @owned τ_0_0
  // CHECK:   [[I2_COPY:%.*]] = copy_value [[I2_ALLOC]]
  // CHECK:   [[I2:%[0-9]+]] = apply [[INIT_WITNESS]]<@opened([[N]]) Initializable>([[I]], [[I2_COPY]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : Initializable> (Int, @owned τ_0_0) -> @owned τ_0_0
  // CHECK:   [[I2_EXIST_CONTAINER:%[0-9]+]] = init_existential_ref [[I2]] : $@opened([[N]]) Initializable : $@opened([[N]]) Initializable, $Initializable
  // CHECK:   store [[I2_EXIST_CONTAINER]] to [init] [[PB]] : $*Initializable
  // CHECK:   [[I2:%[0-9]+]] = load [copy] [[PB]] : $*Initializable
  // CHECK:   destroy_value [[I2_BOX]] : ${ var Initializable }
  // CHECK:   return [[I2]] : $Initializable
  var i2 = im.init(int: i)
  return i2
}
// CHECK: } // end sil function '_T014objc_protocols28testInitializableExistentialAA0D0_pAaC_pXp_Si1itF'

class InitializableConformer: Initializable {
  @objc required init(int: Int) {}
}
// CHECK-LABEL: sil hidden [thunk] @_T014objc_protocols22InitializableConformerC{{[_0-9a-zA-Z]*}}fcTo

final class InitializableConformerByExtension {
  init() {}
}

extension InitializableConformerByExtension: Initializable {
  @objc convenience init(int: Int) {
    self.init()
  }
}
// CHECK-LABEL: sil hidden [thunk] @_T014objc_protocols33InitializableConformerByExtensionC{{[_0-9a-zA-Z]*}}fcTo

// Make sure we're crashing from trying to use materializeForSet here.
@objc protocol SelectionItem {
  var time: Double { get set }
}

func incrementTime(contents: SelectionItem) {
  contents.time += 1.0
}

// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name objc_protocols -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -disable-objc-attr-requires-foundation-module | %FileCheck %s
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

// CHECK-LABEL: sil hidden [ossa] @$s14objc_protocols0A8_generic{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[THIS:%.*]] : @guaranteed $T):
// -- Result of runce is autoreleased according to default objc conv
// CHECK: [[METHOD:%.*]] = objc_method [[THIS]] : {{\$.*}},  #NSRuncing.runce!foreign
// CHECK: [[RESULT1:%.*]] = apply [[METHOD]]<T>([[THIS:%.*]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : NSRuncing> (τ_0_0) -> @autoreleased NSObject

// -- Result of copyRuncing is received copy_valued according to -copy family
// CHECK: [[METHOD:%.*]] = objc_method [[THIS]] : {{\$.*}},  #NSRuncing.copyRuncing!foreign
// CHECK: [[RESULT2:%.*]] = apply [[METHOD]]<T>([[THIS:%.*]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : NSRuncing> (τ_0_0) -> @owned NSObject

// -- Arguments are not consumed by objc calls
// CHECK-NOT: destroy_value [[THIS]]
func objc_generic<T : NSRuncing>(_ x: T) -> (NSObject, NSObject) {
  return (x.runce(), x.copyRuncing())
}

// CHECK-LABEL: sil hidden [ossa] @$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlF : $@convention(thin) <T where T : NSRuncing> (@guaranteed T) -> () {
func objc_generic_partial_apply<T : NSRuncing>(_ x: T) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $T):
  // CHECK:   [[FN:%.*]] = function_ref @$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlFSo8NSObjectCycxcfu_
  _ = x.runce

  // CHECK:   [[FN:%.*]] = function_ref @$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlFSo8NSObjectCycxcfu1_
  _ = T.runce

  // CHECK:   [[FN:%.*]] = function_ref @$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlFSo8NSObjectCycfu3_
  _ = T.mince
  // CHECK-NOT:   destroy_value [[ARG]]
}
// CHECK: } // end sil function '$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlF'


// CHECK: sil private [ossa] @$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlFSo8NSObjectCycxcfu_ : $@convention(thin) <T where T : NSRuncing> (@guaranteed T) -> @owned @callee_guaranteed () -> @owned NSObject {
// CHECK: function_ref @$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlFSo8NSObjectCycxcfu_AEycfu0_ : $@convention(thin) <τ_0_0 where τ_0_0 : NSRuncing> (@guaranteed τ_0_0) -> @owned NSObject
// CHECK: } // end sil function '$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlFSo8NSObjectCycxcfu_'

// CHECK: sil private [ossa] @$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlFSo8NSObjectCycxcfu_AEycfu0_ : $@convention(thin) <T where T : NSRuncing> (@guaranteed T) -> @owned NSObject {
// CHECK: objc_method %0 : $T, #NSRuncing.runce!foreign : <Self where Self : NSRuncing> (Self) -> () -> NSObject, $@convention(objc_method) <τ_0_0 where τ_0_0 : NSRuncing> (τ_0_0) -> @autoreleased NSObject
// CHECK: } // end sil function '$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlFSo8NSObjectCycxcfu_AEycfu0_'


// CHECK: sil private [ossa] @$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlFSo8NSObjectCycxcfu1_ : $@convention(thin) <T where T : NSRuncing> (@guaranteed T) -> @owned @callee_guaranteed () -> @owned NSObject
// CHECK: function_ref @$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlFSo8NSObjectCycxcfu1_AEycfu2_ : $@convention(thin) <τ_0_0 where τ_0_0 : NSRuncing> (@guaranteed τ_0_0) -> @owned NSObject
// CHECK: } // end sil function '$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlFSo8NSObjectCycxcfu1_'

// CHECK: sil private [ossa] @$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlFSo8NSObjectCycxcfu1_AEycfu2_ : $@convention(thin) <T where T : NSRuncing> (@guaranteed T) -> @owned NSObject
// CHECK: objc_method %0 : $T, #NSRuncing.runce!foreign : <Self where Self : NSRuncing> (Self) -> () -> NSObject, $@convention(objc_method) <τ_0_0 where τ_0_0 : NSRuncing> (τ_0_0) -> @autoreleased NSObject
// CHECK: } // end sil function '$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlFSo8NSObjectCycxcfu1_AEycfu2_'


// CHECK-LABEL: sil private [ossa] @$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlFSo8NSObjectCycfu3_ : $@convention(thin) <T where T : NSRuncing> (@thick T.Type) -> @owned NSObject
// CHECK: objc_method %2 : $@objc_metatype T.Type, #NSRuncing.mince!foreign : <Self where Self : NSRuncing> (Self.Type) -> () -> NSObject, $@convention(objc_method) <τ_0_0 where τ_0_0 : NSRuncing> (@objc_metatype τ_0_0.Type) -> @autoreleased NSObject
// CHECK: } // end sil function '$s14objc_protocols0A22_generic_partial_applyyyxAA9NSRuncingRzlFSo8NSObjectCycfu3_'


// CHECK-LABEL: sil hidden [ossa] @$s14objc_protocols0A9_protocol{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[THIS:%.*]] : @guaranteed $any NSRuncing):
// -- Result of runce is autoreleased according to default objc conv
// CHECK: [[THIS1:%.*]] = open_existential_ref [[THIS]] : $any NSRuncing to $[[OPENED:@opened\(.*, any NSRuncing\) Self]]
// CHECK: [[METHOD:%.*]] = objc_method [[THIS1]] : $[[OPENED]], #NSRuncing.runce!foreign
// CHECK: [[RESULT1:%.*]] = apply [[METHOD]]<[[OPENED]]>([[THIS1]])

// -- Result of copyRuncing is received copy_valued according to -copy family
// CHECK: [[THIS2:%.*]] = open_existential_ref [[THIS]] : $any NSRuncing to $[[OPENED2:@opened\(.*, any NSRuncing\) Self]]
// CHECK: [[METHOD:%.*]] = objc_method [[THIS2]] : $[[OPENED2]], #NSRuncing.copyRuncing!foreign
// CHECK: [[RESULT2:%.*]] = apply [[METHOD]]<[[OPENED2]]>([[THIS2:%.*]])

// -- Arguments are not consumed by objc calls
// CHECK-NOT: destroy_value [[THIS]]
func objc_protocol(_ x: NSRuncing) -> (NSObject, NSObject) {
  return (x.runce(), x.copyRuncing())
}

// CHECK-LABEL: sil hidden [ossa] @$s14objc_protocols0A23_protocol_partial_applyyyAA9NSRuncing_pF : $@convention(thin) (@guaranteed any NSRuncing) -> () {
func objc_protocol_partial_apply(_ x: NSRuncing) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $any NSRuncing):
  // CHECK:   [[FN:%.*]] = function_ref @$s14objc_protocols0A23_protocol_partial_applyyyAA9NSRuncing_pFSo8NSObjectCycAaC_pcfu_
  // CHECK:   [[RESULT:%.*]] = apply [[FN]]([[ARG]])
  _ = x.runce

  // rdar://21289579
  _ = NSRuncing.runce
}
// CHECK: } // end sil function '$s14objc_protocols0A23_protocol_partial_applyyyAA9NSRuncing_pF'

// CHECK-LABEL: sil hidden [ossa] @$s14objc_protocols0A21_protocol_composition{{[_0-9a-zA-Z]*}}F
func objc_protocol_composition(_ x: NSRuncing & NSFunging) {
  // CHECK: [[THIS:%.*]] = open_existential_ref [[THIS_ORIG:%.*]] : $any NSFunging & NSRuncing to $[[OPENED:@opened\(.*, any NSFunging & NSRuncing\) Self]]
  // CHECK: [[METHOD:%.*]] = objc_method [[THIS]] : $[[OPENED]], #NSRuncing.runce!foreign
  // CHECK: apply [[METHOD]]<[[OPENED]]>([[THIS]])
  x.runce()

  // CHECK: [[THIS:%.*]] = open_existential_ref [[THIS_ORIG:%.*]] : $any NSFunging & NSRuncing to $[[OPENED:@opened\(.*, any NSFunging & NSRuncing\) Self]]
  // CHECK: [[METHOD:%.*]] = objc_method [[THIS]] : $[[OPENED]], #NSFunging.funge!foreign
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

// CHECK-LABEL: sil private [thunk] [ossa] @$s14objc_protocols3FooC5runce{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil private [thunk] [ossa] @$s14objc_protocols3FooC11copyRuncing{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil private [thunk] [ossa] @$s14objc_protocols3FooC5funge{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil private [thunk] [ossa] @$s14objc_protocols3FooC3foo{{[_0-9a-zA-Z]*}}FTo
// CHECK-NOT: sil hidden [ossa] @_TToF{{.*}}anse{{.*}}

class Bar { }

extension Bar : NSRuncing {
  @objc func runce() -> NSObject { return NSObject() }
  @objc func copyRuncing() -> NSObject { return NSObject() }
  @objc func foo() {}
  @objc static func mince() -> NSObject { return NSObject() }
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s14objc_protocols3BarC5runce{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil private [thunk] [ossa] @$s14objc_protocols3BarC11copyRuncing{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil private [thunk] [ossa] @$s14objc_protocols3BarC3foo{{[_0-9a-zA-Z]*}}FTo

// class Bas from objc_protocols_Bas module
extension Bas : NSRuncing {
  // runce() implementation from the original definition of Bas
  @objc func copyRuncing() -> NSObject { return NSObject() }
  @objc func foo() {}
  @objc static func mince() -> NSObject { return NSObject() }
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s18objc_protocols_Bas0C0C0a1_B0E11copyRuncing{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil private [thunk] [ossa] @$s18objc_protocols_Bas0C0C0a1_B0E3foo{{[_0-9a-zA-Z]*}}FTo

// -- Inherited objc protocols

protocol Fungible : NSFunging { }

class Zim : Fungible {
  @objc func funge() {}
  @objc func foo() {}
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s14objc_protocols3ZimC5funge{{[_0-9a-zA-Z]*}}FTo
// CHECK-LABEL: sil private [thunk] [ossa] @$s14objc_protocols3ZimC3foo{{[_0-9a-zA-Z]*}}FTo

// class Zang from objc_protocols_Bas module
extension Zang : Fungible {
  // funge() implementation from the original definition of Zim
  @objc func foo() {}
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s18objc_protocols_Bas4ZangC0a1_B0E3foo{{[_0-9a-zA-Z]*}}FTo

// -- objc protocols with property requirements in extensions
//    <rdar://problem/16284574>

@objc protocol NSCounting {
  var count: Int {get}
}

class StoredPropertyCount {
  @objc let count = 0
}

extension StoredPropertyCount: NSCounting {}
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s14objc_protocols19StoredPropertyCountC5countSivgTo

class ComputedPropertyCount {
  @objc var count: Int { return 0 }
}

extension ComputedPropertyCount: NSCounting {}
// CHECK-LABEL: sil private [thunk] [ossa] @$s14objc_protocols21ComputedPropertyCountC5countSivgTo

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

// CHECK-LABEL: sil hidden [ossa] @$s14objc_protocols28testInitializableExistential_1iAA0D0_pAaD_pXp_SitF : $@convention(thin) (@thick any Initializable.Type, Int) -> @owned any Initializable {
func testInitializableExistential(_ im: Initializable.Type, i: Int) -> Initializable {
  // CHECK: bb0([[META:%[0-9]+]] : $@thick any Initializable.Type, [[I:%[0-9]+]] : $Int):
  // CHECK:   [[I2_BOX:%[0-9]+]] = alloc_box ${ var any Initializable }
  // CHECK:   [[I2_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [var_decl] [[I2_BOX]]
  // CHECK:   [[PB:%.*]] = project_box [[I2_LIFETIME]]
  // CHECK:   [[ARCHETYPE_META:%[0-9]+]] = open_existential_metatype [[META]] : $@thick any Initializable.Type to $@thick (@opened([[N:".*"]], any Initializable) Self).Type
  // CHECK:   [[ARCHETYPE_META_OBJC:%[0-9]+]] = thick_to_objc_metatype [[ARCHETYPE_META]] : $@thick (@opened([[N]], any Initializable) Self).Type to $@objc_metatype (@opened([[N]], any Initializable) Self).Type
  // CHECK:   [[I2_ALLOC:%[0-9]+]] = alloc_ref_dynamic [objc] [[ARCHETYPE_META_OBJC]] : $@objc_metatype (@opened([[N]], any Initializable) Self).Type, $@opened([[N]], any Initializable) Self
  // CHECK:   [[INIT_WITNESS:%[0-9]+]] = objc_method [[I2_ALLOC]] : $@opened([[N]], any Initializable) Self, #Initializable.init!initializer.foreign : {{.*}}
  // CHECK:   [[I2:%[0-9]+]] = apply [[INIT_WITNESS]]<@opened([[N]], any Initializable) Self>([[I]], [[I2_ALLOC]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : Initializable> (Int, @owned τ_0_0) -> @owned τ_0_0
  // CHECK:   [[I2_EXIST_CONTAINER:%[0-9]+]] = init_existential_ref [[I2]] : $@opened([[N]], any Initializable) Self : $@opened([[N]], any Initializable) Self, $any Initializable
  // CHECK:   store [[I2_EXIST_CONTAINER]] to [init] [[PB]] : $*any Initializable
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PB]] : $*any Initializable
  // CHECK:   [[I2:%[0-9]+]] = load [copy] [[READ]] : $*any Initializable
  // CHECK:   end_borrow [[I2_LIFETIME]]
  // CHECK:   destroy_value [[I2_BOX]] : ${ var any Initializable }
  // CHECK:   return [[I2]] : $any Initializable
  var i2 = im.init(int: i)
  return i2
}
// CHECK: } // end sil function '$s14objc_protocols28testInitializableExistential_1iAA0D0_pAaD_pXp_SitF'

class InitializableConformer: Initializable {
  @objc required init(int: Int) {}
}
// CHECK-LABEL: sil private [thunk] [ossa] @$s14objc_protocols22InitializableConformerC{{[_0-9a-zA-Z]*}}fcTo

final class InitializableConformerByExtension {
  init() {}
}

extension InitializableConformerByExtension: Initializable {
  @objc convenience init(int: Int) {
    self.init()
  }
}
// CHECK-LABEL: sil private [thunk] [ossa] @$s14objc_protocols33InitializableConformerByExtensionC{{[_0-9a-zA-Z]*}}fcTo

// Make sure we're crashing from trying to use materializeForSet here.
@objc protocol SelectionItem {
  var time: Double { get set }
}

func incrementTime(contents: SelectionItem) {
  contents.time += 1.0
}

@objc
public protocol DangerousEscaper {
  @objc
  func malicious(_ mayActuallyEscape: () -> ())
}

// Make sure we emit an withoutActuallyEscaping sentinel.

// CHECK-LABEL: sil [ossa] @$s14objc_protocols19couldActuallyEscapeyyyyc_AA16DangerousEscaper_ptF : $@convention(thin) (@guaranteed @callee_guaranteed () -> (), @guaranteed any DangerousEscaper) -> () {
// CHECK: bb0([[CLOSURE_ARG:%.*]] : @guaranteed $@callee_guaranteed () -> (), [[SELF:%.*]] : @guaranteed $any DangerousEscaper):
// CHECK:  [[OE:%.*]] = open_existential_ref [[SELF]]
// CHECK:  [[CLOSURE_COPY1:%.*]]  = copy_value [[CLOSURE_ARG]]
// CHECK:  [[NOESCAPE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE_COPY1]]
// CHECK:  [[NOESCAPEC:%.*]] = copy_value [[NOESCAPE]]
// CHECK:  [[WITHOUT_ACTUALLY_ESCAPING_THUNK:%.*]] = function_ref @$sIg_Ieg_TR : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> ()) -> ()
// CHECK:  [[WITHOUT_ACTUALLY_ESCAPING_SENTINEL:%.*]] = partial_apply [callee_guaranteed] [[WITHOUT_ACTUALLY_ESCAPING_THUNK]]([[NOESCAPEC]])
// CHECK:  [[WITHOUT_ESCAPING:%.*]] = mark_dependence [[WITHOUT_ACTUALLY_ESCAPING_SENTINEL]] : $@callee_guaranteed () -> () on [[NOESCAPE]]
// CHECK:  [[WITHOUT_ESCAPING_COPY:%.*]] = copy_value [[WITHOUT_ESCAPING]] : $@callee_guaranteed () -> ()
// CHECK:  [[BLOCK:%.*]] = alloc_stack $@block_storage @callee_guaranteed () -> ()
// CHECK:  [[BLOCK_ADDR:%.*]] = project_block_storage [[BLOCK]] : $*@block_storage @callee_guaranteed () -> ()
// CHECK:  store [[WITHOUT_ESCAPING_COPY]] to [init] [[BLOCK_ADDR]] : $*@callee_guaranteed () -> ()
// CHECK:  [[BLOCK_INVOKE:%.*]] = function_ref @$sIeg_IyB_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed () -> ()) -> ()
// CHECK:  [[BLOCK_CLOSURE:%.*]] = init_block_storage_header [[BLOCK]] : $*@block_storage @callee_guaranteed () -> (), invoke [[BLOCK_INVOKE]] : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed () -> ()) -> (), type $@convention(block) @noescape () -> ()
// CHECK:  [[BLOCK_CLOSURE_COPY:%.*]] = copy_block_without_escaping [[BLOCK_CLOSURE]] : $@convention(block) @noescape () -> () withoutEscaping [[WITHOUT_ESCAPING]] : $@callee_guaranteed () -> ()
// CHECK:  destroy_addr [[BLOCK_ADDR]] : $*@callee_guaranteed () -> ()
// CHECK:  dealloc_stack [[BLOCK]] : $*@block_storage @callee_guaranteed () -> ()
// CHECK:  destroy_value [[CLOSURE_COPY1]] : $@callee_guaranteed () -> ()
// CHECK:  [[METH:%.*]] = objc_method [[OE]] : $@opened("{{.*}}", any DangerousEscaper) Self, #DangerousEscaper.malicious!foreign
// CHECK:  apply [[METH]]<@opened("{{.*}}", any DangerousEscaper) Self>([[BLOCK_CLOSURE_COPY]], [[OE]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : DangerousEscaper> (@convention(block) @noescape () -> (), τ_0_0) -> ()
// CHECK:  destroy_value [[BLOCK_CLOSURE_COPY]] : $@convention(block) @noescape () -> ()
// CHECK:  return {{.*}} : $()

public func couldActuallyEscape(_ closure: @escaping () -> (), _ villain: DangerousEscaper) {
  villain.malicious(closure)
}


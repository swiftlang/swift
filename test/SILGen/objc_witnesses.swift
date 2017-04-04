// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import gizmo

protocol Fooable {
  func foo() -> String!
}

// Witnesses Fooable.foo with the original ObjC-imported -foo method .
extension Foo: Fooable {}

class Phoûx : NSObject, Fooable {
  @objc func foo() -> String! {
    return "phoûx!"
  }
}

// witness for Foo.foo uses the foreign-to-native thunk:
// CHECK-LABEL: sil hidden [transparent] [thunk] @_T0So3FooC14objc_witnesses7FooableA2cDP3foo{{[_0-9a-zA-Z]*}}FTW
// CHECK:         function_ref @_T0So3FooC3foo{{[_0-9a-zA-Z]*}}FTO

// *NOTE* We have an extra copy here for the time being right
// now. This will change once we teach SILGen how to not emit the
// extra copy.
//
// witness for Phoûx.foo uses the Swift vtable
// CHECK-LABEL: _T014objc_witnesses008Phox_xraC3foo{{[_0-9a-zA-Z]*}}F
// CHECK:      bb0([[IN_ADDR:%.*]] : 
// CHECK:         [[STACK_SLOT:%.*]] = alloc_stack $Phoûx
// CHECK:         copy_addr [[IN_ADDR]] to [initialization] [[STACK_SLOT]]
// CHECK:         [[VALUE:%.*]] = load [take] [[STACK_SLOT]]
// CHECK:         [[BORROWED_VALUE:%.*]] = begin_borrow [[VALUE]]
// CHECK:         [[CLS_METHOD:%.*]] = class_method [[BORROWED_VALUE]] : $Phoûx, #Phoûx.foo!1
// CHECK:         apply [[CLS_METHOD]]([[BORROWED_VALUE]])
// CHECK:         end_borrow [[BORROWED_VALUE]] from [[VALUE]]
// CHECK:         destroy_value [[VALUE]]
// CHECK:         dealloc_stack [[STACK_SLOT]]

protocol Bells {
  init(bellsOn: Int)
}

extension Gizmo : Bells {
}

// CHECK: sil hidden [transparent] [thunk] @_T0So5GizmoC14objc_witnesses5BellsA2cDP{{[_0-9a-zA-Z]*}}fCTW
// CHECK: bb0([[SELF:%[0-9]+]] : $*Gizmo, [[I:%[0-9]+]] : $Int, [[META:%[0-9]+]] : $@thick Gizmo.Type):

// CHECK:   [[INIT:%[0-9]+]] = function_ref @_T0So5GizmoC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (Int, @thick Gizmo.Type) -> @owned Optional<Gizmo>
// CHECK:   [[IUO_RESULT:%[0-9]+]] = apply [[INIT]]([[I]], [[META]]) : $@convention(method) (Int, @thick Gizmo.Type) -> @owned Optional<Gizmo>
// CHECK:   switch_enum [[IUO_RESULT]]
// CHECK: bb2([[UNWRAPPED_RESULT:%.*]] : $Gizmo):
// CHECK:   store [[UNWRAPPED_RESULT]] to [init] [[SELF]] : $*Gizmo

// Test extension of a native @objc class to conform to a protocol with a
// subscript requirement. rdar://problem/20371661

protocol Subscriptable {
  subscript(x: Int) -> Any { get }
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_T0So7NSArrayC14objc_witnesses13SubscriptableA2cDP9subscriptypSicfgTW : $@convention(witness_method) (Int, @in_guaranteed NSArray) -> @out Any {
// CHECK:         function_ref @_T0So7NSArrayC9subscriptypSicfgTO : $@convention(method) (Int, @guaranteed NSArray) -> @out Any
// CHECK-LABEL: sil shared [thunk] @_T0So7NSArrayC9subscriptypSicfgTO : $@convention(method) (Int, @guaranteed NSArray) -> @out Any {
// CHECK:         class_method [volatile] {{%.*}} : $NSArray, #NSArray.subscript!getter.1.foreign
extension NSArray: Subscriptable {}

// witness is a dynamic thunk:

protocol Orbital {
  var quantumNumber: Int { get set }
}

class Electron : Orbital {
  dynamic var quantumNumber: Int = 0
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_T014objc_witnesses8ElectronCAA7OrbitalA2aDP13quantumNumberSifgTW
// CHECK-LABEL: sil shared [transparent] [thunk] @_T014objc_witnesses8ElectronC13quantumNumberSifgTD

// CHECK-LABEL: sil hidden [transparent] [thunk] @_T014objc_witnesses8ElectronCAA7OrbitalA2aDP13quantumNumberSifsTW
// CHECK-LABEL: sil shared [transparent] [thunk] @_T014objc_witnesses8ElectronC13quantumNumberSifsTD

// witness is a dynamic thunk and is public:

public protocol Lepton {
  var spin: Float { get }
}

public class Positron : Lepton {
  public dynamic var spin: Float = 0.5
}

// CHECK-LABEL: sil [transparent] [fragile] [thunk] @_T014objc_witnesses8PositronCAA6LeptonA2aDP4spinSffgTW
// CHECK-LABEL: sil shared [transparent] [fragile] [thunk] @_T014objc_witnesses8PositronC4spinSffgTD

// Override of property defined in @objc extension

class Derived : NSObject {
  override var valence: Int {
    get { return 2 } set { }
  }
}

extension NSObject : Atom {
  var valence: Int { get { return 1 } set { } }
}

// CHECK-LABEL: sil hidden @_T0So8NSObjectC14objc_witnessesE7valenceSifmytfU_ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout NSObject, @thick NSObject.Type) -> () {
// CHECK: class_method [volatile] %4 : $NSObject, #NSObject.valence!setter.1.foreign
// CHECK: }

// CHECK-LABEL: sil hidden @_T0So8NSObjectC14objc_witnessesE7valenceSifm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed NSObject) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK: class_method [volatile] %2 : $NSObject, #NSObject.valence!getter.1.foreign
// CHECK: }

protocol Atom : class {
  var valence: Int { get set }
}

// RUN: %target-swift-emit-silgen -enable-sil-ownership -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s

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
// CHECK-LABEL: sil private [transparent] [thunk] @$SSo3FooC14objc_witnesses7FooableA2cDP3foo{{[_0-9a-zA-Z]*}}FTW
// CHECK:         function_ref @$SSo3FooC3foo{{[_0-9a-zA-Z]*}}FTO

// witness for Phoûx.foo uses the Swift vtable
// CHECK-LABEL: $S14objc_witnesses008Phox_xraC3foo{{[_0-9a-zA-Z]*}}F
// CHECK:      bb0([[IN_ADDR:%.*]] : 
// CHECK:         [[VALUE:%.*]] = load_borrow [[IN_ADDR]]
// CHECK:         [[CLS_METHOD:%.*]] = class_method [[VALUE]] : $Phoûx, #Phoûx.foo!1
// CHECK:         apply [[CLS_METHOD]]([[VALUE]])
// CHECK:         end_borrow [[VALUE]] from [[IN_ADDR]]

protocol Bells {
  init(bellsOn: Int)
}

extension Gizmo : Bells {
}

// CHECK: sil private [transparent] [thunk] @$SSo5GizmoC14objc_witnesses5BellsA2cDP{{[_0-9a-zA-Z]*}}fCTW
// CHECK: bb0([[SELF:%[0-9]+]] : @trivial $*Gizmo, [[I:%[0-9]+]] : @trivial $Int, [[META:%[0-9]+]] : @trivial $@thick Gizmo.Type):

// CHECK:   [[INIT:%[0-9]+]] = function_ref @$SSo5GizmoC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (Int, @thick Gizmo.Type) -> @owned Optional<Gizmo>
// CHECK:   [[IUO_RESULT:%[0-9]+]] = apply [[INIT]]([[I]], [[META]]) : $@convention(method) (Int, @thick Gizmo.Type) -> @owned Optional<Gizmo>
// CHECK:   switch_enum [[IUO_RESULT]]
// CHECK: bb1:
// CHECK-NEXT:   [[FILESTR:%.*]] = string_literal utf8 "
// CHECK-NEXT:   [[FILESIZ:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:   [[FILEASC:%.*]] = integer_literal $Builtin.Int1, 
// CHECK-NEXT:   [[LINE:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:   [[COLUMN:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:   [[IMPLICIT:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK:   [[PRECOND:%.*]] = function_ref @$Ss30_diagnoseUnexpectedNilOptional{{[_0-9a-zA-Z]*}}F
// CHECK:   apply [[PRECOND]]([[FILESTR]], [[FILESIZ]], [[FILEASC]], [[LINE]], [[IMPLICIT]])
// CHECK: bb2([[UNWRAPPED_RESULT:%.*]] : @owned $Gizmo):
// CHECK:   store [[UNWRAPPED_RESULT]] to [init] [[SELF]] : $*Gizmo

// Test extension of a native @objc class to conform to a protocol with a
// subscript requirement. rdar://problem/20371661

protocol Subscriptable {
  subscript(x: Int) -> Any { get }
}

// CHECK-LABEL: sil private [transparent] [thunk] @$SSo7NSArrayC14objc_witnesses13SubscriptableA2cDPyypSicigTW : $@convention(witness_method: Subscriptable) (Int, @in_guaranteed NSArray) -> @out Any {
// CHECK:         function_ref @$SSo7NSArrayCyypSicigTO : $@convention(method) (Int, @guaranteed NSArray) -> @out Any
// CHECK-LABEL: sil shared [serializable] [thunk] @$SSo7NSArrayCyypSicigTO : $@convention(method) (Int, @guaranteed NSArray) -> @out Any {
// CHECK:         objc_method {{%.*}} : $NSArray, #NSArray.subscript!getter.1.foreign
extension NSArray: Subscriptable {}

// witness is a dynamic thunk:

protocol Orbital {
  var quantumNumber: Int { get set }
}

class Electron : Orbital {
  @objc dynamic var quantumNumber: Int = 0
}

// CHECK-LABEL: sil private [transparent] [thunk] @$S14objc_witnesses8ElectronCAA7OrbitalA2aDP13quantumNumberSivgTW
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$S14objc_witnesses8ElectronC13quantumNumberSivgTD

// CHECK-LABEL: sil private [transparent] [thunk] @$S14objc_witnesses8ElectronCAA7OrbitalA2aDP13quantumNumberSivsTW
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$S14objc_witnesses8ElectronC13quantumNumberSivsTD

// witness is a dynamic thunk and is public:

public protocol Lepton {
  var spin: Float { get }
}

public class Positron : Lepton {
  @objc public dynamic var spin: Float = 0.5
}

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] @$S14objc_witnesses8PositronCAA6LeptonA2aDP4spinSfvgTW
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$S14objc_witnesses8PositronC4spinSfvgTD

// Override of property defined in @objc extension

class Derived : NSObject {
  @objc override var valence: Int {
    get { return 2 } set { }
  }
}

extension NSObject : Atom {
  @objc var valence: Int { get { return 1 } set { } }
}

// CHECK-LABEL: sil shared @$SSo8NSObjectC14objc_witnessesE7valenceSivM : $@yield_once @convention(method) (@guaranteed NSObject) -> @yields @inout Int {
// CHECK: objc_method %0 : $NSObject, #NSObject.valence!getter.1.foreign
// CHECK: yield
// CHECK: objc_method %0 : $NSObject, #NSObject.valence!setter.1.foreign
// CHECK: }

protocol Atom : class {
  var valence: Int { get set }
}

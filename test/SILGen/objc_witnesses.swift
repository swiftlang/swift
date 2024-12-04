// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s

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
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$sSo3FooC14objc_witnesses7FooableA2cDP3foo{{[_0-9a-zA-Z]*}}FTW
// CHECK:         function_ref @$sSo3FooC3foo{{[_0-9a-zA-Z]*}}FTO

// witness for Phoûx.foo uses the Swift vtable
// CHECK-LABEL: $s14objc_witnesses008Phox_xraC3foo{{[_0-9a-zA-Z]*}}F
// CHECK:      bb0([[IN_ADDR:%.*]] : 
// CHECK:         [[VALUE:%.*]] = load_borrow [[IN_ADDR]]
// CHECK:         [[CLS_METHOD:%.*]] = class_method [[VALUE]] : $Phoûx, #Phoûx.foo :
// CHECK:         apply [[CLS_METHOD]]([[VALUE]])
// CHECK:         end_borrow [[VALUE]]

protocol Bells {
  init(bellsOn: Int)
}

extension Gizmo : Bells {
}

// CHECK: sil private [transparent] [thunk] [ossa] @$sSo5GizmoC14objc_witnesses5BellsA2cDP{{[_0-9a-zA-Z]*}}fCTW
// CHECK: bb0([[SELF:%[0-9]+]] : $*Gizmo, [[I:%[0-9]+]] : $Int, [[META:%[0-9]+]] : $@thick Gizmo.Type):

// CHECK:   [[INIT:%[0-9]+]] = function_ref @$sSo5GizmoC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (Int, @thick Gizmo.Type) -> @owned Optional<Gizmo>
// CHECK:   [[IUO_RESULT:%[0-9]+]] = apply [[INIT]]([[I]], [[META]]) : $@convention(method) (Int, @thick Gizmo.Type) -> @owned Optional<Gizmo>
// CHECK:   switch_enum [[IUO_RESULT]]
// CHECK: bb1:
// CHECK-NEXT:   [[FILESTR:%.*]] = string_literal utf8 "
// CHECK-NEXT:   [[FILESIZ:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:   [[FILEASC:%.*]] = integer_literal $Builtin.Int1, 
// CHECK-NEXT:   [[LINE:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:   [[COLUMN:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:   [[IMPLICIT:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK:   [[PRECOND:%.*]] = function_ref @$ss30_diagnoseUnexpectedNilOptional{{[_0-9a-zA-Z]*}}F
// CHECK:   apply [[PRECOND]]([[FILESTR]], [[FILESIZ]], [[FILEASC]], [[LINE]], [[IMPLICIT]])
// CHECK: bb2([[UNWRAPPED_RESULT:%.*]] : @owned $Gizmo):
// CHECK:   store [[UNWRAPPED_RESULT]] to [init] [[SELF]] : $*Gizmo

// Test extension of a native @objc class to conform to a protocol with a
// subscript requirement. rdar://problem/20371661

protocol Subscriptable {
  subscript(x: Int) -> Any { get }
}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$sSo7NSArrayC14objc_witnesses13SubscriptableA2cDPyypSicigTW :
// CHECK:         function_ref @$sSo7NSArrayCyypSicigTO : $@convention(method) (Int, @guaranteed NSArray) -> @out Any
// CHECK-LABEL: sil shared [serialized] [thunk] [ossa] @$sSo7NSArrayCyypSicigTO : $@convention(method) (Int, @guaranteed NSArray) -> @out Any {
// CHECK:         objc_method {{%.*}} : $NSArray, #NSArray.subscript!getter.foreign
extension NSArray: Subscriptable {}

// witness is a dynamic thunk:

protocol Orbital {
  var quantumNumber: Int { get set }
}

class Electron : Orbital {
  @objc dynamic var quantumNumber: Int = 0
}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s14objc_witnesses8ElectronCAA7OrbitalA2aDP13quantumNumberSivgTW
// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s14objc_witnesses8ElectronC13quantumNumberSivgTD

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s14objc_witnesses8ElectronCAA7OrbitalA2aDP13quantumNumberSivsTW
// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s14objc_witnesses8ElectronC13quantumNumberSivsTD

// witness is a dynamic thunk and is public:

public protocol Lepton {
  var spin: Float { get }
}

public class Positron : Lepton {
  @objc public dynamic var spin: Float = 0.5
}

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s14objc_witnesses8PositronCAA6LeptonA2aDP4spinSfvgTW
// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s14objc_witnesses8PositronC4spinSfvgTD

// Override of property defined in @objc extension

class Derived : NSObject {
  @objc override var valence: Int {
    get { return 2 } set { }
  }
}

extension NSObject : Atom {
  @objc var valence: Int { get { return 1 } set { } }
}

// CHECK-LABEL: sil shared [ossa] @$sSo8NSObjectC14objc_witnessesE7valenceSivM : $@yield_once @convention(method) (@guaranteed NSObject) -> @yields @inout Int {
// CHECK: objc_method %0 : $NSObject, #NSObject.valence!getter.foreign
// CHECK: yield
// CHECK: objc_method %0 : $NSObject, #NSObject.valence!setter.foreign
// CHECK: }

protocol Atom : class {
  var valence: Int { get set }
}

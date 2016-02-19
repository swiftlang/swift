// RUN: %target-swift-frontend -emit-silgen -enable-experimental-property-behaviors %s | FileCheck %s
protocol behavior {
  associatedtype Value
}
extension behavior {
  var value: Value {
    get { }
    set { }
  }
}

// TODO: global accessor doesn't get walked because it's in DerivedFileUnit
var [behavior] global: Int

struct S1<T> {
  var [behavior] instance: T
  // CHECK-LABEL: sil hidden @_TFV17property_behavior2S1g8instancex
  // CHECK:         [[BEHAVIOR_IMP:%.*]] = function_ref @_TFE17property_behaviorPS_8behaviorg5valuewx5Value
  // CHECK:         apply [[BEHAVIOR_IMP]]<S1<T>, T>
  // CHECK-LABEL: sil hidden @_TFV17property_behavior2S1s8instancex
  // CHECK:         [[BEHAVIOR_IMP:%.*]] = function_ref @_TFE17property_behaviorPS_8behaviors5valuewx5Value
  // CHECK:         apply [[BEHAVIOR_IMP]]<S1<T>, T>

  static var [behavior] typeLevel: T
  // CHECK-LABEL: sil hidden @_TZFV17property_behavior2S1g9typeLevelx
  // CHECK:         [[BEHAVIOR_IMP:%.*]] = function_ref @_TFE17property_behaviorPS_8behaviorg5valuewx5Value
  // CHECK:         apply [[BEHAVIOR_IMP]]<S1<T>.Type, T>
  // CHECK-LABEL: sil hidden @_TZFV17property_behavior2S1s9typeLevelx
  // CHECK:         [[BEHAVIOR_IMP:%.*]] = function_ref @_TFE17property_behaviorPS_8behaviors5valuewx5Value
  // CHECK:         apply [[BEHAVIOR_IMP]]<S1<T>.Type, T>
}

class C1<T> {
  var [behavior] instance: T
  static var [behavior] typeLevel: T
}

var zero: Int { get { } }

func exerciseBehavior<T>(inout _ sx: S1<T>, inout _ sy: S1<Int>,
                         _ cx: C1<T>, _ cy: C1<Int>,
                         _ z: T) {
  /* FIXME
  var [behavior] local: T

  _ = local
  local = z

  var [behavior] localInt: Int

  _ = localInt
  localInt = zero
   */

  _ = global
  global = zero

  _ = sx.instance
  sx.instance = z

  _ = S1<T>.typeLevel
  S1<T>.typeLevel = z

  _ = sy.instance
  sy.instance = zero

  _ = S1<Int>.typeLevel
  S1<Int>.typeLevel = zero

  _ = cx.instance
  cx.instance = z

  _ = C1<T>.typeLevel
  C1<T>.typeLevel = z

  _ = cy.instance
  cy.instance = zero

  _ = C1<Int>.typeLevel
  C1<Int>.typeLevel = zero
}

protocol withStorage {
  associatedtype Value
  var storage: Value? { get set }
}
extension withStorage {
  var value: Value {
    get { }
    set { }
  }

  static func initStorage() -> Value? { }
}

// TODO: storage behaviors in non-instance context
struct S2<T> {
  var [withStorage] instance: T
  lazy var ninstance = 0
}
class C2<T> {
  var [withStorage] instance: T
}

func exerciseStorage<T>(inout _ sx: S2<T>, inout _ sy: S2<Int>,
                        _ cx: C2<T>, _ cy: C2<Int>,
                        _ z: T) {
  _ = sx.instance
  sx.instance = z

  _ = sy.instance
  sy.instance = zero

  _ = cx.instance
  cx.instance = z

  _ = cy.instance
  cy.instance = zero
}

// FIXME: printing sil_witness_tables for behaviors should indicate what
//        var decl the behavior is attached to
// CHECK-LABEL: sil_witness_table private (): behavior module property_behavior
// CHECK:         associated_type Value: Int

// CHECK-LABEL: sil_witness_table private <T> S1<T>: behavior module property_behavior
// CHECK:         associated_type Value: T

// CHECK-LABEL: sil_witness_table private <T> S1<T>.Type: behavior module property_behavior
// CHECK:         associated_type Value: T

// CHECK-LABEL: sil_witness_table private <T> C1<T>: behavior module property_behavior
// CHECK:         associated_type Value: T

// CHECK-LABEL: sil_witness_table private <T> C1<T>.Type: behavior module property_behavior
// CHECK:         associated_type Value: T

// CHECK-LABEL: sil_witness_table private <T> S2<T>: withStorage module property_behavior {
// CHECK:         associated_type Value: T
// CHECK:         method #withStorage.storage!getter.1
// CHECK:         method #withStorage.storage!setter.1
// CHECK:         method #withStorage.storage!materializeForSet.1

// CHECK-LABEL: sil_witness_table private <T> C2<T>: withStorage module property_behavior {
// CHECK:         associated_type Value: T
// CHECK:         method #withStorage.storage!getter.1
// CHECK:         method #withStorage.storage!setter.1
// CHECK:         method #withStorage.storage!materializeForSet.1

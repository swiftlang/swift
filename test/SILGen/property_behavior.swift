// RUN: %target-swift-emit-silgen -enable-experimental-property-behaviors -enable-sil-ownership %s | %FileCheck %s
// REQUIRES: property_behavior_value_substitution
protocol behavior {
  associatedtype Value
}
extension behavior {
  var value: Value {
    get { }
    set { }
  }
}

// TODO: global accessor doesn't get walked??
var global: Int __behavior behavior

struct S1<T> {
  var instance: T __behavior behavior
  // CHECK-LABEL: sil hidden @_TFV17property_behavior2S1g8instancex
  // CHECK:         [[BEHAVIOR_IMP:%.*]] = function_ref @_TFE17property_behaviorPS_8behaviorg5valuewx5Value
  // CHECK:         apply [[BEHAVIOR_IMP]]<S1<T>, T>
  // CHECK-LABEL: sil hidden @_TFV17property_behavior2S1s8instancex
  // CHECK:         [[BEHAVIOR_IMP:%.*]] = function_ref @_TFE17property_behaviorPS_8behaviors5valuewx5Value
  // CHECK:         apply [[BEHAVIOR_IMP]]<S1<T>, T>

  static var typeLevel: T __behavior behavior
  // CHECK-LABEL: sil hidden @_TZFV17property_behavior2S1g9typeLevelx
  // CHECK:         [[BEHAVIOR_IMP:%.*]] = function_ref @_TFE17property_behaviorPS_8behaviorg5valuewx5Value
  // CHECK:         apply [[BEHAVIOR_IMP]]<S1<T>.Type, T>
  // CHECK-LABEL: sil hidden @_TZFV17property_behavior2S1s9typeLevelx
  // CHECK:         [[BEHAVIOR_IMP:%.*]] = function_ref @_TFE17property_behaviorPS_8behaviors5valuewx5Value
  // CHECK:         apply [[BEHAVIOR_IMP]]<S1<T>.Type, T>
}

class C1<T> {
  var instance: T __behavior behavior
  static var typeLevel: T __behavior behavior
}

var zero: Int { get { } }

func exerciseBehavior<T>(_ sx: inout S1<T>, _ sy: inout S1<Int>,
                         _ cx: C1<T>, _ cy: C1<Int>,
                         _ z: T) {
  /* FIXME
  var local: T __behavior behavior

  _ = local
  local = z

  var localInt: Int __behavior behavior

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
  var instance: T __behavior withStorage

  // FIXME: Hack because we can't find the synthesized associated type witness
  // during witness matching.
  typealias Value = T
}
class C2<T> {
  var instance: T __behavior withStorage

  // FIXME: Hack because we can't find the synthesized associated type witness
  // during witness matching.
  typealias Value = T
}

func exerciseStorage<T>(_ sx: inout S2<T>, _ sy: inout S2<Int>,
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

protocol withInit {
  associatedtype Value
  var storage: Value? { get set }
  func parameter() -> Value
}
extension withInit {
  var value: Value {
    get { }
    set { }
  }

  static func initStorage() -> Value? { }
}

// TODO: parameterized behaviors in non-instance context
func any<T>() -> T { }

struct S3<T> {

  // FIXME: Hack because we can't find the synthesized associated type witness
  // during witness matching.
  typealias Value = T
  var instance: T __behavior withInit { any() }
}
class C3<T> {
  var instance: T __behavior withInit { any() }

  // FIXME: Hack because we can't find the synthesized associated type witness
  // during witness matching.
  typealias Value = T
}

func exerciseStorage<T>(_ sx: inout S3<T>, _ sy: inout S3<Int>,
                        _ cx: C3<T>, _ cy: C3<Int>,
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

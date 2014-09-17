// RUN: %swift -emit-silgen %s | FileCheck %s

protocol ProtocolA: class {
    var intProp: Int { get }
}

protocol ProtocolB {
    var intProp: Int { get }
}

@objc protocol ProtocolO: class {
    var intProp: Int { get }
}


// CHECK-LABEL: sil @_TF30generic_property_base_lifetime21getIntPropExistentialFPS_9ProtocolA_Si
// CHECK:         strong_retain %0
// CHECK:         [[PROJECTION:%.*]] = project_existential_ref %0
// CHECK:         apply {{%.*}}([[PROJECTION]])
// CHECK:         strong_release %0
func getIntPropExistential(a: ProtocolA) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil @_TF30generic_property_base_lifetime17getIntPropGenericUS_9ProtocolA__FQ_Si
// CHECK:         strong_retain %0
// CHECK:         apply {{%.*}}<T>(%0)
// CHECK:         strong_release %0
func getIntPropGeneric<T: ProtocolA>(a: T) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil @_TF30generic_property_base_lifetime21getIntPropExistentialFPS_9ProtocolB_Si
// CHECK:         [[PROJECTION:%.*]] = project_existential %0
// CHECK:         apply {{%.*}}([[PROJECTION]])
// CHECK:         destroy_addr %0
func getIntPropExistential(a: ProtocolB) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil @_TF30generic_property_base_lifetime17getIntPropGenericUS_9ProtocolB__FQ_Si
// CHECK:         apply {{%.*}}<T>(%0)
// CHECK:         destroy_addr %0
func getIntPropGeneric<T: ProtocolB>(a: T) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil @_TF30generic_property_base_lifetime21getIntPropExistentialFPS_9ProtocolO_Si
// CHECK:         strong_retain %0
// CHECK:         [[PROJECTION:%.*]] = project_existential_ref %0
// CHECK:         apply {{%.*}}([[PROJECTION]])
// CHECK:         strong_release %0
// CHECK:         strong_release %0
func getIntPropExistential(a: ProtocolO) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil @_TF30generic_property_base_lifetime17getIntPropGenericUS_9ProtocolO__FQ_Si
// CHECK:         strong_retain %0
// CHECK:         apply {{%.*}}<T>(%0)
// CHECK:         strong_release %0
// CHECK:         strong_release %0
func getIntPropGeneric<T: ProtocolO>(a: T) -> Int {
  return a.intProp
}

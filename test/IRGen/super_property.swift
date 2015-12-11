// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -module-name OutsideClasses -o %t %S/../Inputs/outside_classes_before.swift
// RUN: %target-swift-frontend -use-native-super-method -emit-ir -I %t %s | FileCheck %s --check-prefix=IRGEN

import OutsideClasses

class GrandchildToOutside : OutsideChild {
  // IRGEN-LABEL: define hidden void @_TFC14super_property19GrandchildToOutside11usePropertyfT_T_(%C14super_property19GrandchildToOutside*)
  // IRGEN: [[METADATA:%[0-9]+]] = call %swift.type* @_TMaC14super_property19GrandchildToOutside()
  // IRGEN: [[OPAQUE_METADATA:%[0-9]+]] = bitcast %swift.type* [[METADATA]] to %swift.type**
  // IRGEN: [[SUPER_METADATA_PTR:%[0-9]+]] = getelementptr inbounds %swift.type*, %swift.type** [[OPAQUE_METADATA]], i32 1
  // IRGEN: [[SUPER_METADATA:%[0-9]+]] = load %swift.type*, %swift.type** [[SUPER_METADATA_PTR]]
  // IRGEN: [[OPAQUE_SUPER_METADATA:%[0-9]+]] = bitcast %swift.type* [[SUPER_METADATA]] to { i8*, i64, i64 } (%C14OutsideClasses13OutsideParent*)**
  // IRGEN: [[VTABLE_SLOT:%[0-9]+]] = getelementptr inbounds { i8*, i64, i64 } (%C14OutsideClasses13OutsideParent*)*, { i8*, i64, i64 } (%C14OutsideClasses13OutsideParent*)** [[OPAQUE_SUPER_METADATA]]
  // IRGEN: [[GETTER_FN_PTR:%[0-9]+]] = load { i8*, i64, i64 } (%C14OutsideClasses13OutsideParent*)*, { i8*, i64, i64 } (%C14OutsideClasses13OutsideParent*)** [[VTABLE_SLOT]]
  func useProperty() {
    _ = super.property
  }
}

class GenericGrandchildToOutside<A> : GenericOutsideChild<A> {
  // IRGEN-LABEL: define hidden void @_TFC14super_property26GenericGrandchildToOutside11usePropertyfT_T_(%C14super_property26GenericGrandchildToOutside*)
  // IRGEN: [[METADATA:%[0-9]+]] = call %swift.type* @swift_getGenericMetadata1
  // IRGEN: [[OPAQUE_METADATA:%[0-9]+]] = bitcast %swift.type* [[METADATA]] to %swift.type**
  // IRGEN: [[SUPER_METADATA_PTR:%[0-9]+]] = getelementptr inbounds %swift.type*, %swift.type** [[OPAQUE_METADATA]], i32 1
  // IRGEN: [[SUPER_METADATA:%[0-9]+]] = load %swift.type*, %swift.type** [[SUPER_METADATA_PTR]]
  // IRGEN: [[OPAQUE_SUPER_METADATA:%[0-9]+]] = bitcast %swift.type* [[SUPER_METADATA]] to void (%swift.opaque*, %C14OutsideClasses20GenericOutsideParent*)**
  // IRGEN: [[VTABLE_SLOT:%[0-9]+]] = getelementptr inbounds void (%swift.opaque*, %C14OutsideClasses20GenericOutsideParent*)*, void (%swift.opaque*, %C14OutsideClasses20GenericOutsideParent*)** [[OPAQUE_SUPER_METADATA]]
  // IRGEN: [[GETTER_FN_PTR:%[0-9]+]] = load void (%swift.opaque*, %C14OutsideClasses20GenericOutsideParent*)*, void (%swift.opaque*, %C14OutsideClasses20GenericOutsideParent*)** [[VTABLE_SLOT]]
  func useProperty() {
    _ = super.property
  }
}

class ConcreteGrandchildToOutside : ConcreteOutsideChild {
  // IRGEN-LABEL: define hidden void @_TFC14super_property27ConcreteGrandchildToOutside11usePropertyfT_T_(%C14super_property27ConcreteGrandchildToOutside*)
  // IRGEN: [[METADATA:%[0-9]+]] = call %swift.type* @_TMaC14super_property27ConcreteGrandchildToOutside()
  // IRGEN: [[OPAQUE_METADATA:%[0-9]+]] = bitcast %swift.type* [[METADATA]] to %swift.type**
  // IRGEN: [[SUPER_METADATA_PTR:%[0-9]+]] = getelementptr inbounds %swift.type*, %swift.type** [[OPAQUE_METADATA]], i32 1
  // IRGEN: [[SUPER_METADATA:%[0-9]+]] = load %swift.type*, %swift.type** [[SUPER_METADATA_PTR]]
  // IRGEN: [[OPAQUE_SUPER_METADATA:%[0-9]+]] = bitcast %swift.type* [[SUPER_METADATA]] to void (%swift.opaque*, %C14OutsideClasses20GenericOutsideParent*)**
  // IRGEN: [[VTABLE_SLOT:%[0-9]+]] = getelementptr inbounds { i8*, i64, i64 } (%C14OutsideClasses13OutsideParent*)*, { i8*, i64, i64 } (%C14OutsideClasses13OutsideParent*)** [[SUPER_METADATA]]
  // IRGEN: [[GETTER_FN_PTR:%[0-9]+]] = load { i8*, i64, i64 } (%C14OutsideClasses13OutsideParent*)*, { i8*, i64, i64 } (%C14OutsideClasses13OutsideParent*)** [[VTABLE_SLOT]]
  func useProperty() {
    _ = super.property
  }
}

let c = {
  class GrandchildToOutside : OutsideChild {
    func useProperty() {
      _ = super.property
    }
  }
}

let cHasGenerics = {
  class GenericGrandchildToOutside<A> : GenericOutsideChild<A> {
    // IRGEN-LABEL: define linkonce_odr hidden void @_TFCF14super_propertyU0_FT_T_L_26GenericGrandchildToOutside11usePropertyfT_T_(%CF14super_propertyU0_FT_T_L_26GenericGrandchildToOutside*)
    // IRGEN: [[METADATA:%[0-9]+]] = call %swift.type* @swift_getGenericMetadata1
    // IRGEN: [[OPAQUE_METADATA:%[0-9]+]] = bitcast %swift.type* [[METADATA]] to %swift.type**
    // IRGEN: [[SUPER_METADATA_PTR:%[0-9]+]] = getelementptr inbounds %swift.type*, %swift.type** [[OPAQUE_METADATA]], i32 1
    // IRGEN: [[SUPER_METADATA:%[0-9]+]] = load %swift.type*, %swift.type** [[SUPER_METADATA_PTR]]
    // IRGEN: [[OPAQUE_SUPER_METADATA:%[0-9]+]] = bitcast %swift.type* [[SUPER_METADATA]] to void (%swift.opaque*, %C14OutsideClasses20GenericOutsideParent*)**
    // IRGEN: [[VTABLE_SLOT:%[0-9]+]] = getelementptr inbounds void (%swift.opaque*, %C14OutsideClasses20GenericOutsideParent*)*, void (%swift.opaque*, %C14OutsideClasses20GenericOutsideParent*)** [[OPAQUE_SUPER_METADATA]]
    // IRGEN: [[GETTER_FN_PTR:%[0-9]+]] = load void (%swift.opaque*, %C14OutsideClasses20GenericOutsideParent*)*, void (%swift.opaque*, %C14OutsideClasses20GenericOutsideParent*)** [[VTABLE_SLOT]]
    func useProperty() {
      _ = super.property
    }
  }
}

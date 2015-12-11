// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -module-name OutsideClasses -o %t %S/../Inputs/outside_classes_before.swift
// RUN: %target-swift-frontend -use-native-super-method -emit-ir -I %t %s | FileCheck %s --check-prefix=IRGEN

import OutsideClasses

public class GrandchildToOutside : OutsideChild {
  // IRGEN-LABEL: define void @_TFC12super_method19GrandchildToOutside6methodfT_T_(%C12super_method19GrandchildToOutside*)
  // IRGEN: [[METADATA:%[0-9]+]] = call %swift.type* @_TMaC12super_method19GrandchildToOutside() #4
  // IRGEN: [[OPAQUE_METADATA:%[0-9]+]] = bitcast %swift.type* [[METADATA]] to %swift.type**
  // IRGEN: [[SUPER_METADATA_PTR:%[0-9]+]] = getelementptr inbounds %swift.type*, %swift.type** [[OPAQUE_METADATA]], i32 1
  // IRGEN: [[OPAQUE_SUPER_METADATA:%[0-9]+]] = load %swift.type*, %swift.type** [[SUPER_METADATA_PTR]]
  // IRGEN: [[SUPER_METADATA:%[0-9]+]] = bitcast %swift.type* [[OPAQUE_SUPER_METADATA]] to void (%C14OutsideClasses12OutsideChild*)**
  // IRGEN: [[VTABLE_SLOT:%[0-9]+]] = getelementptr inbounds void (%C14OutsideClasses12OutsideChild*)*, void (%C14OutsideClasses12OutsideChild*)** [[SUPER_METADATA]]
  // IRGEN: [[FN_PTR:%[0-9]+]] = load void (%C14OutsideClasses12OutsideChild*)*, void (%C14OutsideClasses12OutsideChild*)** [[VTABLE_SLOT]]
  public override func method() {
    super.method()
  }
}

public class GenericGrandchildToOutside<A> : GenericOutsideChild<A> {
  // IRGEN-LABEL: define void @_TFC12super_method26GenericGrandchildToOutside6methodfT_T_(%C12super_method26GenericGrandchildToOutside*)
  // IRGEN: [[METADATA:%[0-9]+]] = call %swift.type* @swift_getGenericMetadata1
  // IRGEN: [[OPAQUE_METADATA:%[0-9]+]] = bitcast %swift.type* [[METADATA]] to %swift.type**
  // IRGEN: [[SUPER_METADATA_PTR:%[0-9]+]] = getelementptr inbounds %swift.type*, %swift.type** [[OPAQUE_METADATA]], i32 1
  // IRGEN: [[OPAQUE_SUPER_METADATA:%[0-9]+]] = load %swift.type*, %swift.type** [[SUPER_METADATA_PTR]]
  // IRGEN: [[SUPER_METADATA:%[0-9]+]] = bitcast %swift.type* [[OPAQUE_SUPER_METADATA]] to void (%C14OutsideClasses19GenericOutsideChild*)**
  // IRGEN: [[VTABLE_SLOT:%[0-9]+]] = getelementptr inbounds void (%C14OutsideClasses19GenericOutsideChild*)*, void (%C14OutsideClasses19GenericOutsideChild*)** [[SUPER_METADATA]]
  // IRGEN: [[FN_PTR:%[0-9]+]] = load void (%C14OutsideClasses19GenericOutsideChild*)*, void (%C14OutsideClasses19GenericOutsideChild*)** [[VTABLE_SLOT]]
  // IRGEN: call void
  public override func method() {
    super.method()
  }
}

public class ConcreteGrandchildToOutside : ConcreteOutsideChild {
  // IRGEN-LABEL: define void @_TFC12super_method27ConcreteGrandchildToOutside6methodfT_T_(%C12super_method27ConcreteGrandchildToOutside*)
  // IRGEN: [[METADATA:%[0-9]+]] = call %swift.type* @_TMaC12super_method27ConcreteGrandchildToOutside()
  // IRGEN: [[OPAQUE_METADATA:%4]] = bitcast %swift.type* %3 to %swift.type**
  // IRGEN: [[SUPER_METADATA_PTR:%[0-9]+]] = getelementptr inbounds %swift.type*, %swift.type** [[OPAQUE_METADATA]], i32 1
  // IRGEN: [[OPAQUE_SUPER_METADATA:%[0-9]+]] = load %swift.type*, %swift.type** [[SUPER_METADATA_PTR]]
  // IRGEN: [[SUPER_METADATA:%[0-9]+]] = bitcast %swift.type* [[OPAQUE_SUPER_METADATA]] to void (%C14OutsideClasses20ConcreteOutsideChild*)**
  // IRGEN: [[VTABLE_SLOT:%[0-9]+]] = getelementptr inbounds void (%C14OutsideClasses20ConcreteOutsideChild*)*, void (%C14OutsideClasses20ConcreteOutsideChild*)** [[SUPER_METADATA]]
  // IRGEN: [[FN_PTR:%[0-9]+]] = load void (%C14OutsideClasses20ConcreteOutsideChild*)*, void (%C14OutsideClasses20ConcreteOutsideChild*)** [[VTABLE_SLOT]]
  public override func method() {
    super.method()
  }
}

let c = {
  class GrandchildToOutside : OutsideChild {
    // IRGEN-LABEL: define linkonce_odr hidden void @_TFCF12super_methodU_FT_T_L_19GrandchildToOutside6methodfT_T_(%CF12super_methodU_FT_T_L_19GrandchildToOutside*)
    // IRGEN: [[METADATA:%[0-9]+]] = call %swift.type* @_TMaCF12super_methodU_FT_T_L_19GrandchildToOutside()
    // IRGEN: [[OPAQUE_METADATA:%[0-9]+]] = bitcast %swift.type* [[METADATA]] to %swift.type**
    // IRGEN: [[SUPER_METADATA_PTR:%[0-9]+]] = getelementptr inbounds %swift.type*, %swift.type** [[OPAQUE_METADATA]], i32 1
    // IRGEN: [[OPAQUE_SUPER_METADATA:%[0-9]+]] = load %swift.type*, %swift.type** [[SUPER_METADATA_PTR]]
    // IRGEN: [[SUPER_METADATA:%[0-9]+]] = bitcast %swift.type* [[OPAQUE_SUPER_METADATA]] to void (%C14OutsideClasses12OutsideChild*)**
    // IRGEN: [[VTABLE_SLOT:%[0-9]+]] = getelementptr inbounds void (%C14OutsideClasses12OutsideChild*)*, void (%C14OutsideClasses12OutsideChild*)** [[SUPER_METADATA]]
    // IRGEN: [[FN_PTR:%[0-9]+]] = load void (%C14OutsideClasses12OutsideChild*)*, void (%C14OutsideClasses12OutsideChild*)** [[VTABLE_SLOT]]
    override func method() {
      doFoo(super.method)
    }
  }
}

let cHasGenerics = {
  class GenericGrandchildToOutside<A> : GenericOutsideChild<A> {
    // IRGEN-LABEL: define linkonce_odr hidden void @_TFCF12super_methodU0_FT_T_L_26GenericGrandchildToOutside6methodfT_T_(%CF12super_methodU0_FT_T_L_26GenericGrandchildToOutside*)
    // IRGEN: [[METADATA:%[0-9]+]] = call %swift.type* @swift_getGenericMetadata1
    // IRGEN: [[OPAQUE_METADATA:%[0-9]+]] = bitcast %swift.type* [[METADATA]] to %swift.type**
    // IRGEN: [[SUPER_METADATA_PTR:%[0-9]+]] = getelementptr inbounds %swift.type*, %swift.type** [[OPAQUE_METADATA]], i32 1
    // IRGEN: [[OPAQUE_SUPER_METADATA:%[0-9]+]] = load %swift.type*, %swift.type** [[SUPER_METADATA_PTR]]
    // IRGEN: [[SUPER_METADATA:%[0-9]+]] = bitcast %swift.type* [[OPAQUE_SUPER_METADATA]] to void (%C14OutsideClasses19GenericOutsideChild*)**
    // IRGEN: [[VTABLE_SLOT:%[0-9]+]] = getelementptr inbounds void (%C14OutsideClasses19GenericOutsideChild*)*, void (%C14OutsideClasses19GenericOutsideChild*)** [[SUPER_METADATA]]
    // IRGEN: [[FN_PTR:%[0-9]+]] = load void (%C14OutsideClasses19GenericOutsideChild*)*, void (%C14OutsideClasses19GenericOutsideChild*)** [[VTABLE_SLOT]]
    // IRGEN: call void
    override func method() {
      doFoo(super.method)
    }
  }
}

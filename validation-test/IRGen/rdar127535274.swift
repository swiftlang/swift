// RUN: %target-build-swift -target %target-future-triple -g -Xfrontend -enable-experimental-feature -Xfrontend LayoutStringValueWitnesses -Xfrontend -enable-experimental-feature -Xfrontend LayoutStringValueWitnessesInstantiation -Xfrontend -enable-layout-string-value-witnesses -Xfrontend -enable-layout-string-value-witnesses-instantiation -Xfrontend -enable-library-evolution -c -parse-as-library -emit-ir %s | %FileCheck %s

// REQUIRES: swift_feature_LayoutStringValueWitnesses
// REQUIRES: swift_feature_LayoutStringValueWitnessesInstantiation

// CHECK: define internal ptr @"$s13rdar1275352744TestVMi"
// CHECK:  [[METADATA:%.*]] = call ptr @swift_cvw_allocateGenericValueMetadataWithLayoutString
// CHECK:  call void @swift_cvw_instantiateLayoutString(ptr @"type_layout_string l13rdar1275352744TestVyxG", ptr [[METADATA]])
// CHECK:  ret ptr [[METADATA]]
// CHECK: }
public struct Test<T> {
    let x: [T]
    let y: [T]
}

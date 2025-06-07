// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -target %target-future-triple -enable-experimental-feature LayoutStringValueWitnesses -enable-experimental-feature LayoutStringValueWitnessesInstantiation -enable-layout-string-value-witnesses -enable-layout-string-value-witnesses-instantiation -emit-ir -import-objc-header %S/Inputs/ObjCBaseClasses.h %s | %FileCheck %s

// REQUIRES: PTRSIZE=64
// REQUIRES: objc_interop
// REQUIRES: swift_feature_LayoutStringValueWitnesses
// REQUIRES: swift_feature_LayoutStringValueWitnessesInstantiation

protocol P {}

extension ObjCBase: P {}

// CHECK: @"type_layout_string 41layout_string_witnesses_objc_existentials27MultiPayloadObjCExistentialO" =
// CHECK: internal constant <{ i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64 }>
// CHECK: <{ i64 -9223372036854775808, i64 88, i64 1441151880758558720, i64 sub (i64 ptrtoint (ptr @"get_enum_tag_for_layout_string 41layout_string_witnesses_objc_existentials27MultiPayloadObjCExistentialO" to i64),
// CHECK: i64 ptrtoint (ptr getelementptr inbounds (<{ i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64 }>,
// CHECK: ptr @"type_layout_string 41layout_string_witnesses_objc_existentials27MultiPayloadObjCExistentialO", i32 0, i32 3) to i64)),
// CHECK: i64 2, i64 32, i64 16, i64 0, i64 16, i64 360287970189639680, i64 0, i64 720575940379279360, i64 0, i64 0 }>
enum MultiPayloadObjCExistential {
    case x(AnyObject)
    case y(P & ObjCBase)
}

// CHECK: @"type_layout_string 41layout_string_witnesses_objc_existentials34MultiPayloadObjCExistentialWrapperV" =
// CHECK: internal constant <{ i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64 }>
// CHECK: <{ i64 -9223372036854775808, i64 88, i64 1441151880758558720, i64 sub (i64 ptrtoint (ptr @"get_enum_tag_for_layout_string 41layout_string_witnesses_objc_existentials27MultiPayloadObjCExistentialO" to i64),
// CHECK: i64 ptrtoint (ptr getelementptr inbounds (<{ i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64 }>,
// CHECK: ptr @"type_layout_string 41layout_string_witnesses_objc_existentials34MultiPayloadObjCExistentialWrapperV", i32 0, i32 3) to i64)),
// CHECK: i64 2, i64 32, i64 16, i64 0, i64 16, i64 360287970189639680, i64 0, i64 720575940379279360, i64 0, i64 8 }>
struct MultiPayloadObjCExistentialWrapper {
    let x: MultiPayloadObjCExistential
    let y: Int = 0
}

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-as-library -enable-library-evolution -module-name MyKit %S/Inputs/back_deployed_type_module.swift -o %t/MyKit.swiftmodule
// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name Client %s -I %t > %t/client.ll
// RUN: %FileCheck %s < %t/client.ll
// RUN: %FileCheck %s --check-prefix=THUNK < %t/client.ll
// RUN: %FileCheck %s --check-prefix=MAKE < %t/client.ll
// RUN: %FileCheck %s --check-prefix=WEAK < %t/client.ll
// RUN: %FileCheck %s --check-prefix=NEGATIVE < %t/client.ll
// RUN: %FileCheck %s --check-prefix=ACCESSOR < %t/client.ll
// RUN: %FileCheck %s --check-prefix=NEWER < %t/client.ll
// RUN: %FileCheck %s --check-prefix=NEWER-NEGATIVE < %t/client.ll
// RUN: %FileCheck %s --check-prefix=GENERIC < %t/client.ll

// With a deployment target that guarantees MyKit provides the type, the
// client uses MyKit's definitions directly.
// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name Client %s -I %t -target %target-cpu-apple-macosx99 > %t/client-new.ll
// RUN: %FileCheck %s --check-prefix=HIGH < %t/client-new.ll
// RUN: %FileCheck %s --check-prefix=HIGH-NEGATIVE < %t/client-new.ll

// REQUIRES: OS=macosx
// REQUIRES: CPU=arm64 || CPU=x86_64

// A client whose deployment target predates a back-deployed type from
// another module emits its own copy of the type, along with the conformances
// declared with it, so that it works where the type's module predates the
// type.

import MyKit

// The type's metadata and its conformances are emitted with non-unique,
// hidden linkage.
// CHECK-DAG: @"$s5MyKit5PointVSQAAMc" = linkonce_odr hidden constant
// CHECK-DAG: @"$s5MyKit5PointVMn" = linkonce_odr hidden constant
// CHECK-DAG: @"$s5MyKit5PointVMf" = linkonce_odr hidden constant
// CHECK-DAG: @"$s5MyKit5PointVMF" = linkonce_odr hidden constant
// CHECK-DAG: @"__swift_back_deployment_anchor_$s5MyKit5PointVMn" = private global

// It has records for the type and conformance, which reference them
// directly.
// CHECK-DAG: @"$s5MyKit5PointVHn" = private constant %swift.type_metadata_record { i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s5MyKit5PointVMn" to i64)
// CHECK-DAG: @"$s5MyKit5PointVSQAAHc" = private constant i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s5MyKit5PointVSQAAMc" to i64)

// A type nested in a back-deployed type is emitted the same way, along with
// the type descriptor of its parent.
// CHECK-DAG: @"$s5MyKit5OuterV5InnerVMn" = linkonce_odr hidden constant
// CHECK-DAG: @"$s5MyKit5OuterVMn" = linkonce_odr hidden constant
// CHECK-DAG: define linkonce_odr hidden swiftcc %swift.metadata_response @"$s5MyKit5OuterV5InnerVMaTwa"(i64 %0)

// CHECK-DAG: define linkonce_odr hidden swiftcc %swift.metadata_response @"$s5MyKit5PointVMaTwa"(i64 %0)
// CHECK-DAG: define linkonce_odr hidden ptr @"__swift_back_deployment_canonical_descriptor_$s5MyKit5PointVMn"()

// The copy of the access function has a distinct name, so that it can
// reference MyKit's by its usual name. It defers to MyKit's when MyKit provides
// the type, even if MyKit predates the type's back deployment. Otherwise, it
// uses the canonical copy among those in the process.
// ACCESSOR-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$s5MyKit5PointVMaTwa"(i64 %0)
// ACCESSOR: icmp ne ptr @"$s5MyKit5PointVMa", null
// ACCESSOR: call swiftcc %swift.metadata_response @"$s5MyKit5PointVMa"(i64 0)
// ACCESSOR: call ptr @"__swift_back_deployment_canonical_descriptor_$s5MyKit5PointVMn"()
// ACCESSOR: declare extern_weak swiftcc %swift.metadata_response @"$s5MyKit5PointVMa"(i64)

// Members are called through back deployment thunks, and the witness thunk
// does the same.
// THUNK-LABEL: define {{.*}}swiftcc i1 @"$s5MyKit5PointVSQAASQ2eeoiySbx_xtFZTW"(
// THUNK: call swiftcc i1 @"$s5MyKit5PointV2eeoiySbAC_ACtFZTwb"(
// MAKE-LABEL: define {{.*}}@"$s6Client9makePointypyF"(
// MAKE: call swiftcc i64 @"$s5MyKit5PointV1xACSi_tcfCTwb"(i64 1)
// MAKE: call swiftcc %swift.metadata_response @"$s5MyKit5PointVMaTwa"(i64 0)

// Only the original members of the back-deployed types are referenced from
// MyKit, weakly. (Its protocol isn't back-deployed, so it's referenced
// normally.)
// WEAK-DAG: declare extern_weak swiftcc i1 @"$s5MyKit5PointV2eeoiySbAC_ACtFZ"(
// WEAK-DAG: declare extern_weak swiftcc i64 @"$s5MyKit5PointV1xACSi_tcfC"(
// NEGATIVE-NOT: {{^declare (swiftcc |ptr |void |i[0-9]+ ).*@"\$s5MyKit(5Point|5Outer)}}
// NEGATIVE-NOT: {{^@"\$s5MyKit(5Point|5Outer)[^"]*" = external}}

// NEWER-NEGATIVE-NOT: $s5MyKit5PointV5newerSiyFTw

// HIGH-DAG: @"$s5MyKit5PointVN" = external global %swift.type
// HIGH-DAG: declare swiftcc i64 @"$s5MyKit5PointV1xACSi_tcfC"(i64)
// HIGH-DAG: declare swiftcc i1 @"$s5MyKit5PointV2eeoiySbAC_ACtFZ"(i64, i64)
// HIGH-NEGATIVE-NOT: linkonce_odr {{.*}}@"$s5MyKit
// HIGH-NEGATIVE-NOT: __swift_back_deployment_
// HIGH-NEGATIVE-NOT: Twa
// HIGH-NEGATIVE-NOT: Twb
// HIGH-NEGATIVE-NOT: swift_conformsToProtocol

// The client emits witness thunks for generic requirements of conformances
// declared with the type, rebuilding the thunk signature of the deserialized
// witness. A conformance to a protocol from the type's own module is still
// resilient in the client.
// GENERIC-DAG: @"$s5MyKit5PointVAA11TransformerAAMc" = linkonce_odr hidden constant
// GENERIC-DAG: define linkonce_odr hidden swiftcc ptr @"$s5MyKit5PointVAA11TransformerA2aDP9transformySayqd__Gqd__7ElementQzXElFTW"(
// GENERIC-DAG: call swiftcc ptr @"$s5MyKit5PointV9transformySayxGxSiXElFTwb"(

// A member that is only available where MyKit provides the type isn't
// back-deployed, so it's called directly.
// NEWER-LABEL: define {{.*}}@"$s6Client9callNewerySi5MyKit5PointVF"(
// NEWER: call swiftcc i64 @"$s5MyKit5PointV5newerSiyF"(
// NEWER: declare extern_weak swiftcc i64 @"$s5MyKit5PointV5newerSiyF"(
@available(macOS 99, *)
public func callNewer(_ p: Point) -> Int { p.newer() }

public func makePoint() -> Any { Point(x: 1) }
public func makeInner() -> Any { Outer.Inner(v: 1) }
public func same(_ a: Point, _ b: Point) -> Bool { a == b }

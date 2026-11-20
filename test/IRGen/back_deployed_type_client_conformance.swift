// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-as-library -enable-experimental-feature BackDeployFrozenValueTypes -enable-library-evolution -module-name MyKit %S/Inputs/back_deployed_type_module.swift -o %t/MyKit.swiftmodule
// RUN: %target-swift-frontend -emit-ir -parse-as-library -enable-library-evolution -module-name Client %s -I %t > %t/client.ll
// RUN: %FileCheck %s < %t/client.ll
// RUN: %FileCheck %s --check-prefix=NEGATIVE < %t/client.ll

// With a deployment target that guarantees MyKit provides the type, there is
// no copy of the type and one record for the conformance.
// RUN: %target-swift-frontend -emit-ir -parse-as-library -enable-library-evolution -module-name Client %s -I %t -target %target-cpu-apple-macosx99 > %t/client-new.ll
// RUN: %FileCheck %s --check-prefix=HIGH < %t/client-new.ll

// REQUIRES: swift_feature_BackDeployFrozenValueTypes
// REQUIRES: OS=macosx
// REQUIRES: CPU=arm64 || CPU=x86_64

import MyKit

public protocol LocalProto {}
extension Point: LocalProto {}

// A conformance that this module declares for a type that it emits a copy of
// has two records. One refers to the copy of the type descriptor, and applies
// when MyKit doesn't provide the type. The runtime only matches the metadata
// from MyKit with a record that refers to MyKit's descriptor, so the other
// does. It refers to that descriptor through a slot that is null when MyKit
// doesn't provide the type.

// CHECK-DAG: @"$s5MyKit5PointVMn" = extern_weak constant %swift.type_descriptor
// CHECK-DAG: @"got.$s5MyKit5PointVMn" = private unnamed_addr constant ptr @"$s5MyKit5PointVMn"
// CHECK-DAG: @"$s5MyKit5PointV6Client10LocalProto{{A[A-Z]}}Mc" = constant %swift.protocol_conformance_descriptor {{.*}}ptr @"$s5MyKit5PointVMnTwa"
// CHECK-DAG: @"__swift_back_deployment_canonical_conformance_$s5MyKit5PointV6Client10LocalProto{{A[A-Z]}}Mc" = internal constant %swift.protocol_conformance_descriptor {{.*}}ptr @"got.$s5MyKit5PointVMn"
// CHECK-DAG: @"record___swift_back_deployment_canonical_conformance_$s5MyKit5PointV6Client10LocalProto{{A[A-Z]}}Mc" = private constant i32 {{.*}}section "__TEXT, __swift5_proto, regular"

// Both descriptors use the same witness table.
// CHECK-DAG: ptr @"$s5MyKit5PointV6Client10LocalProto{{A[A-Z]}}WP"

// The copies of conformances that come with the type already have records in
// MyKit.
// NEGATIVE-NOT: canonical_conformance_$s5MyKit5PointVSQ

// HIGH-NOT: canonical_conformance
// HIGH-NOT: extern_weak constant %swift.type_descriptor

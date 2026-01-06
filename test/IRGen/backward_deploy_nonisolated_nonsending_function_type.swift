// RUN: %target-swift-frontend -target %target-cpu-apple-macos12 -emit-ir -o - -primary-file %s | %FileCheck %s
// REQUIRES: concurrency
// REQUIRES: OS=macosx

func useGenericMetatype(_ type: Any.Type) { }

// CHECK-LABEL: define hidden swiftcc void @"$s52backward_deploy_nonisolated_nonsending_function_type29testNonisolatedNonsendingTypeyyF"()
func testNonisolatedNonsendingType() {
  typealias Fn = nonisolated(nonsending) () async throws -> Int

  // CHECK: call swiftcc %swift.metadata_response @"$sSiyYaKYCcMa"
  // CHECK: call swiftcc void @"$s52backward_deploy_nonisolated_nonsending_function_type18useGenericMetatypeyyypXpF"
  useGenericMetatype(Fn.self)
}

// CHECK-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$sSiyYaKYCcMa"
// CHECK: call ptr @swift_getExtendedFunctionTypeMetadata(i{{32|64}} 2768240640, {{i32|i64}} 0, ptr null, ptr null, ptr @"$sSiN"

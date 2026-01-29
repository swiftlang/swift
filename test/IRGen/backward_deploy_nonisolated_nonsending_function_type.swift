// RUN: %target-swift-frontend -module-name nonisolated_nonsending_metadata_backdeploy -target %target-cpu-apple-macosx26 -emit-ir -o - -primary-file %s | %FileCheck --check-prefix=MANGLED %s
// RUN: %target-swift-frontend -module-name nonisolated_nonsending_metadata_backdeploy -target %target-cpu-apple-macosx26 -emit-ir -o - -disable-concrete-type-metadata-mangled-name-accessors -primary-file %s | %FileCheck --check-prefix=NO-MANGLED %s
// RUN: %target-swift-frontend -module-name nonisolated_nonsending_metadata_backdeploy -target %target-cpu-apple-macosx14 -emit-ir -o - -primary-file %s | %FileCheck %s --check-prefix=NO-MANGLED-BD

// REQUIRES: concurrency
// REQUIRES: OS=macosx

func useMetatype(_ type: Any.Type) {}

func testNonisolatedNonsendingFunctionType() {
  typealias Fn = nonisolated(nonsending) () async throws -> Int

  useMetatype(Fn.self)
}

// Check if we are allowed to mangle.

// MANGLED-LABEL: define hidden swiftcc void @"$s42nonisolated_nonsending_metadata_backdeploy37testNonisolatedNonsendingFunctionTypeyyF"()
// MANGLED-NEXT: entry:
// MANGLED-NEXT:   call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$sSiyYaKYCcMD")
// MANGLED-NEXT:   call swiftcc void @"$s42nonisolated_nonsending_metadata_backdeploy11useMetatypeyyypXpF"(ptr
// MANGLED-NEXT:   ret void
// MANGLED-NEXT: }

// Check if we are not allowed to mangle, but we are not backdeploying.

// NO-MANGLED-LABEL: define hidden swiftcc void @"$s42nonisolated_nonsending_metadata_backdeploy37testNonisolatedNonsendingFunctionTypeyyF"()
// NO-MANGLED-NEXT: entry:
// NO-MANGLED-NEXT:   call swiftcc %swift.metadata_response @"$sSiyYaKYCcMa"(
// NO-MANGLED-NEXT:   extractvalue
// NO-MANGLED-NEXT:   call swiftcc void @"$s42nonisolated_nonsending_metadata_backdeploy11useMetatypeyyypXpF"
// NO-MANGLED-NEXT:   ret void
// NO-MANGLED-NEXT: }

// NO-MANGLED-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$sSiyYaKYCcMa"(i64 %0)
// NO-MANGLED: entry:
// NO-MANGLED: [[LAZY_CACHE_VAR:%.*]] = load ptr, ptr @"$sSiyYaKYCcML"
// NO-MANGLED: [[LAZY_CACHE_VAR_CMP:%.*]] = icmp eq ptr [[LAZY_CACHE_VAR]], null
// NO-MANGLED: br i1 [[LAZY_CACHE_VAR_CMP]], label %[[CACHE_IS_NULL_BB:.*]], label %[[CONT_BB:.*]]
//
// NO-MANGLED: [[CACHE_IS_NULL_BB]]:
// NO-MANGLED:   [[HAVE_FUNCTION_METADATA:%.*]] = call ptr @swift_getExtendedFunctionTypeMetadata(
// NO-MANGLED:   store atomic ptr [[HAVE_FUNCTION_METADATA]], ptr @"$sSiyYaKYCcML" release
// NO-MANGLED:   br label %[[CONT_BB]]
//
// NO-MANGLED: [[CONT_BB]]:
// NO-MANGLED:   [[RESULT_PHI:%.*]] = phi ptr [ [[LAZY_CACHE_VAR]], %entry ], [ [[HAVE_FUNCTION_METADATA]], %[[CACHE_IS_NULL_BB]] ]
// NO-MANGLED:   [[RESULT_1:%.*]] = insertvalue %swift.metadata_response undef, ptr [[RESULT_PHI]], 0
// NO-MANGLED:   [[RESULT_2:%.*]] = insertvalue %swift.metadata_response [[RESULT_1]], i64 0, 1
// NO-MANGLED:   ret %swift.metadata_response [[RESULT_2]]
// NO-MANGLED-NEXT: }

// NO-MANGLED: declare ptr @swift_getExtendedFunctionTypeMetadata(i64, i64, ptr, ptr, ptr, ptr, i32, ptr)


// Check if we are not allowed to mangle and we backdeploy.

// NO-MANGLED-BD-LABEL: define hidden swiftcc void @"$s42nonisolated_nonsending_metadata_backdeploy37testNonisolatedNonsendingFunctionTypeyyF"()
// NO-MANGLED-BD-NEXT: entry:
// NO-MANGLED-BD-NEXT:   call swiftcc %swift.metadata_response @"$sSiyYaKYCcMa"(
// NO-MANGLED-BD-NEXT:   extractvalue
// NO-MANGLED-BD-NEXT:   call swiftcc void @"$s42nonisolated_nonsending_metadata_backdeploy11useMetatypeyyypXpF"
// NO-MANGLED-BD-NEXT:   ret void
// NO-MANGLED-BD-NEXT: }

// NO-MANGLED-BD-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$sSiyYaKYCcMa"(i64 %0)
// NO-MANGLED-BD: entry:
// NO-MANGLED-BD: [[LAZY_CACHE_VAR:%.*]] = load ptr, ptr @"$sSiyYaKYCcML"
// NO-MANGLED-BD: [[LAZY_CACHE_VAR_CMP:%.*]] = icmp eq ptr [[LAZY_CACHE_VAR]], null
// NO-MANGLED-BD: br i1 [[LAZY_CACHE_VAR_CMP]], label %[[CACHE_IS_NULL_BB:.*]], label %[[CONT_BB:.*]]
//
// NO-MANGLED-BD: [[CACHE_IS_NULL_BB]]:
// NO-MANGLED-BD:   [[VALUE:%.*]] = call ptr @__swift_getExtendedFunctionTypeMetadata_backDeploy(
// NO-MANGLED-BD:   store atomic ptr [[VALUE]], ptr @"$sSiyYaKYCcML" release
// NO-MANGLED-BD:   br label %[[CONT_BB]]
//
// NO-MANGLED-BD: [[CONT_BB]]:
// NO-MANGLED-BD:   [[RESULT_PHI:%.*]] = phi ptr [ [[LAZY_CACHE_VAR]], %entry ], [ [[VALUE]], %[[CACHE_IS_NULL_BB]] ]
// NO-MANGLED-BD:   [[RESULT_1:%.*]] = insertvalue %swift.metadata_response undef, ptr [[RESULT_PHI]], 0
// NO-MANGLED-BD:   [[RESULT_2:%.*]] = insertvalue %swift.metadata_response [[RESULT_1]], i64 0, 1
// NO-MANGLED-BD:   ret %swift.metadata_response [[RESULT_2]]
// NO-MANGLED-BD-NEXT: }

// NO-MANGLED-BD: declare extern_weak ptr @swift_getExtendedFunctionTypeMetadata(i64, i64, ptr, ptr, ptr, ptr, i32, ptr)

// NO-MANGLED-BD-LABEL: define linkonce_odr hidden ptr @__swift_getExtendedFunctionTypeMetadata_backDeploy(
// NO-MANGLED-BD:   [[HAVE_EXTENDED_FUNCTION_TYPE_METADATA:%.*]] = icmp ne ptr @swift_getExtendedFunctionTypeMetadata, null
// NO-MANGLED-BD:   br i1 [[HAVE_EXTENDED_FUNCTION_TYPE_METADATA]], label %[[HAVE_FUNCTION_BB:.*]], label %[[DONOT_HAVE_FUNCTION_BB:.*]]
//
// NO-MANGLED-BD  [[HAVE_FUNCTION_BB]]:
// NO-MANGLED-BD:   [[HAVE_FUNCTION_METADATA:%.*]] = call ptr @swift_getExtendedFunctionTypeMetadata(
// NO-MANGLED-BD:   br label %[[CONT_BB:.*]]
//
// NO-MANGLED-BD: [[DONOT_HAVE_FUNCTION_BB]]:
// NO-MANGLED-BD:   [[DONOT_HAVE_FUNCTION_METADATA:%.*]] = call ptr @swift_getFunctionTypeMetadata(
// NO-MANGLED-BD:   br label %[[CONT_BB]]
//
// NO-MANGLED-BD: [[CONT_BB]]:
// NO-MANGLED-BD:   [[PHI:%.*]] = phi ptr [ [[HAVE_FUNCTION_METADATA]], %[[HAVE_FUNCTION_BB]] ], [ [[DONOT_HAVE_FUNCTION_METADATA]], %[[DONOT_HAVE_FUNCTION_BB]] ]
// NO-MANGLED-BD:   ret ptr [[PHI]]
// NO-MANGLED-BD-NEXT:  }

// NO-MANGLED-BD: declare ptr @swift_getFunctionTypeMetadata(i64, ptr, ptr, ptr)

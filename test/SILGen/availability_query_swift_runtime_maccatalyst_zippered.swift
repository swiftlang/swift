// RUN: %target-swift-emit-sil %s -target %target-cpu-apple-macosx11 -target-variant %target-cpu-apple-ios14-macabi -min-swift-runtime-version 5.0 -verify -enable-experimental-feature StandaloneSwiftAvailability
// RUN: %target-swift-emit-silgen %s -target %target-cpu-apple-macosx11 -target-variant %target-cpu-apple-ios14-macabi -min-swift-runtime-version 5.0 -enable-experimental-feature StandaloneSwiftAvailability | %FileCheck %s

// REQUIRES: OS=macosx || OS=maccatalyst
// REQUIRES: swift_feature_StandaloneSwiftAvailability

// CHECK-LABEL: sil [ossa] @$s53availability_query_swift_runtime_maccatalyst_zippered15testIfAvailableyyF : $@convention(thin) () -> () {
// CHECK:         [[MAJOR:%.*]] = integer_literal $Builtin.Word, 6
// CHECK:         [[MINOR:%.*]] = integer_literal $Builtin.Word, 2
// CHECK:         [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:         [[FUNC:%.*]] = function_ref @$ss29_isSwiftRuntimeVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:         [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
public func testIfAvailable() {
  if #available(Swift 6.2, *) { }
}

// CHECK-LABEL: sil [ossa] @$s53availability_query_swift_runtime_maccatalyst_zippered17testIfUnavailableyyF : $@convention(thin) () -> () {
// CHECK:         [[MAJOR:%.*]] = integer_literal $Builtin.Word, 5
// CHECK:         [[MINOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK:         [[PATCH:%.*]] = integer_literal $Builtin.Word, 1
// CHECK:         [[FUNC:%.*]] = function_ref @$ss29_isSwiftRuntimeVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:         [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:         [[MINUSONE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK:         [[QUERY_INVERSION:%.*]] = builtin "xor_Int1"([[QUERY_RESULT]], [[MINUSONE]]) : $Builtin.Int1
public func testIfUnavailable() {
  if #unavailable(Swift 5.10.1) { }
}

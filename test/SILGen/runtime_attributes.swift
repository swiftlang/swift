// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature RuntimeDiscoverableAttrs -emit-module -o %t -enable-library-evolution %S/Inputs/runtime_metadata_defs.swift

// This uses '-primary-file' to ensure we're conservative with lazy SIL emission.
// RUN: %target-swift-emit-silgen -enable-experimental-feature RuntimeDiscoverableAttrs -primary-file %s -I %t | %FileCheck %s

// REQUIRES: asserts

import runtime_metadata_defs

/// Test that generator has source locations for both explicit and inferred attributes.

// CHECK-LABEL: sil hidden [runtime_accessible] [ossa] @$s18runtime_attributes4TestAaBVmvpfa0A14_metadata_defs6Ignore : $@convention(thin) () -> @out Optional<Ignore>
// CHECK: [[FILE_ID:%.*]] = string_literal utf8 "runtime_attributes/runtime_attributes.swift"
// CHECK: [[STRING_INIT:%.*]] = function_ref @$sSS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfC
// CHECK-NEXT: [[FILE_STR:%.*]] = apply [[STRING_INIT]]([[FILE_ID]], {{.*}})
// CHECK: [[LINE_RAW:%.*]] = integer_literal $Builtin.IntLiteral, 25
// CHECK: [[INT_INIT:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC
// CHECK-NEXT: [[LINE:%.*]] = apply [[INT_INIT]]([[LINE_RAW]], {{.*}})
// CHECK: [[COLUMN_RAW:%.*]] = integer_literal $Builtin.IntLiteral, 1
// CHECK: [[INT_INIT:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC
// CHECK-NEXT: [[COLUMN:%.*]] = apply [[INT_INIT]]([[COLUMN_RAW]], {{.*}})
// CHECK: [[IGNORE_INIT:%.*]] = function_ref @$s21runtime_metadata_defs6IgnoreV10attachedTo6fileID4line6columnACx_SSS2itclufC
// CHECK-NEXT: {{.*}} = apply [[IGNORE_INIT]]<Test.Type>({{.*}}, {{.*}}, [[FILE_STR]], [[LINE]], [[COLUMN]], {{.*}})
struct Test : Ignorable {}

// CHECK-LABEL: sil hidden [runtime_accessible] [ossa] @$s18runtime_attributes8globalFnyycvpfa0A14_metadata_defs6Ignore : $@convention(thin) () -> @out Optional<Ignore>
// CHECK: {{.*}} = string_literal utf8 "runtime_attributes/runtime_attributes.swift"
// CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 33
// CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 2
// CHECK: [[IGNORE_INIT:%.*]] = function_ref @$s21runtime_metadata_defs6IgnoreV10attachedTo6fileID4line6columnACx_SSS2itclufC
// CHECK-NEXT: {{.*}} = apply [[IGNORE_INIT]]<() -> ()>({{.*}})
@Ignore func globalFn() {}

struct MemberTests {
  // CHECK-LABEL: sil hidden [runtime_accessible] [ossa] @$s18runtime_attributes11MemberTestsV1xSivpfa0A14_metadata_defs6Ignore : $@convention(thin) () -> @out Optional<Ignore>
  // CHECK: {{.*}} = string_literal utf8 "runtime_attributes/runtime_attributes.swift"
  // CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 42
  // CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 4
  // CHECK: [[IGNORE_INIT:%.*]] = function_ref @$s21runtime_metadata_defs6IgnoreV10attachedTo6fileID4line6columnACx_SSS2itclufC
  // CHECK-NEXT: {{.*}} = apply [[IGNORE_INIT]]<WritableKeyPath<MemberTests, Int>>({{.*}})
  @Ignore var x: Int = 42

  // CHECK-LABEL: sil hidden [runtime_accessible] [ossa] @$s18runtime_attributes11MemberTestsV6instFn_1xSSSi_SaySiGtcvpfa0A14_metadata_defs6Ignore : $@convention(thin) () -> @out Optional<Ignore>
  // CHECK: {{.*}} = string_literal utf8 "runtime_attributes/runtime_attributes.swift"
  // CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 50
  // CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 4
  // CHECK: [[IGNORE_INIT:%.*]] = function_ref @$s21runtime_metadata_defs6IgnoreV10attachedTo6fileID4line6columnACx_SSS2itclufC
  // CHECK-NEXT: {{.*}} = apply [[IGNORE_INIT]]<(MemberTests, Int, [Int]) -> String>({{.*}})
  @Ignore func instFn(_: Int, x: [Int]) -> String { "" }

  // CHECK-LABEL: sil hidden [runtime_accessible] [ossa] @$s18runtime_attributes11MemberTestsV8staticFn_1ySi_SStSS_SiztcvpZfa0A14_metadata_defs6Ignore : $@convention(thin) () -> @out Optional<Ignore>
  // CHECK: {{.*}} = string_literal utf8 "runtime_attributes/runtime_attributes.swift"
  // CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 58
  // CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 4
  // CHECK: [[IGNORE_INIT:%.*]] = function_ref @$s21runtime_metadata_defs6IgnoreV10attachedTo6fileID4line6columnACx_SSS2itclufC
  // CHECK-NEXT: {{.*}} = apply [[IGNORE_INIT]]<(MemberTests.Type, String, inout Int) -> (Int, String)>({{.*}})
  @Ignore static func staticFn(_ x: String, y: inout Int) -> (Int, String) { (42, "") }

  // CHECK-LABEL: sil hidden [runtime_accessible] [ossa] @$s18runtime_attributes11MemberTestsV10mutatingFnSiycvpfa0A14_metadata_defs6Ignore : $@convention(thin) () -> @out Optional<Ignore>
  // CHECK: {{.*}} = string_literal utf8 "runtime_attributes/runtime_attributes.swift"
  // CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 66
  // CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 4
  // CHECK: [[IGNORE_INIT:%.*]] = function_ref @$s21runtime_metadata_defs6IgnoreV10attachedTo6fileID4line6columnACx_SSS2itclufC
  // CHECK-NEXT: {{.*}} = apply [[IGNORE_INIT]]<(inout MemberTests) -> Int>({{.*}})
  @Ignore mutating func mutatingFn() -> Int { 42 }
}

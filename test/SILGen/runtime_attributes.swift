// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature RuntimeDiscoverableAttrs -emit-module -o %t -enable-library-evolution %S/Inputs/runtime_metadata_defs.swift

// This uses '-primary-file' to ensure we're conservative with lazy SIL emission.
// RUN: %target-swift-emit-silgen -enable-experimental-feature RuntimeDiscoverableAttrs -primary-file %s -I %t | %FileCheck %s

// REQUIRES: asserts

import runtime_metadata_defs

/// Test that generator has source locations for both explicit and inferred attributes.

// CHECK-LABEL: sil hidden [runtime_accessible] [ossa] @$s21runtime_metadata_defs6IgnoreV0A11_attributes4TestVfa : $@convention(thin) () -> @out Optional<Ignore>
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

// CHECK-LABEL: sil hidden [runtime_accessible] [ossa] @$s21runtime_metadata_defs6IgnoreV0A11_attributes8globalFnyyFfa : $@convention(thin) () -> @out Optional<Ignore>
// CHECK: {{.*}} = string_literal utf8 "runtime_attributes/runtime_attributes.swift"
// CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 33
// CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 2
// CHECK: [[IGNORE_INIT:%.*]] = function_ref @$s21runtime_metadata_defs6IgnoreV10attachedTo6fileID4line6columnACx_SSS2itclufC
// CHECK-NEXT: {{.*}} = apply [[IGNORE_INIT]]<() -> ()>({{.*}})
@Ignore func globalFn() {}

struct MemberTests {
  // CHECK-LABEL: sil hidden [runtime_accessible] [ossa] @$s21runtime_metadata_defs6IgnoreV0A11_attributes11MemberTestsV1xSivpfa : $@convention(thin) () -> @out Optional<Ignore>
  // CHECK: {{.*}} = string_literal utf8 "runtime_attributes/runtime_attributes.swift"
  // CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 42
  // CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 4
  // CHECK: [[IGNORE_INIT:%.*]] = function_ref @$s21runtime_metadata_defs6IgnoreV10attachedTo6fileID4line6columnACx_SSS2itclufC
  // CHECK-NEXT: {{.*}} = apply [[IGNORE_INIT]]<WritableKeyPath<MemberTests, Int>>({{.*}})
  @Ignore var x: Int = 42

  // CHECK-LABEL: sil hidden [runtime_accessible] [ossa] @$s21runtime_metadata_defs6IgnoreV0A11_attributes11MemberTestsV6instFn_1xSSSi_SaySiGtFfa : $@convention(thin) () -> @out Optional<Ignore>
  // CHECK: {{.*}} = string_literal utf8 "runtime_attributes/runtime_attributes.swift"
  // CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 50
  // CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 4
  // CHECK: [[IGNORE_INIT:%.*]] = function_ref @$s21runtime_metadata_defs6IgnoreV10attachedTo6fileID4line6columnACx_SSS2itclufC
  // CHECK-NEXT: {{.*}} = apply [[IGNORE_INIT]]<(MemberTests, Int, [Int]) -> String>({{.*}})
  @Ignore func instFn(_: Int, x: [Int]) -> String { "" }

  // CHECK-LABEL: sil hidden [runtime_accessible] [ossa] @$s21runtime_metadata_defs6IgnoreV0A11_attributes11MemberTestsV8staticFn_1ySi_SStSS_SiztFZfa : $@convention(thin) () -> @out Optional<Ignore>
  // CHECK: {{.*}} = string_literal utf8 "runtime_attributes/runtime_attributes.swift"
  // CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 58
  // CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 4
  // CHECK: [[IGNORE_INIT:%.*]] = function_ref @$s21runtime_metadata_defs6IgnoreV10attachedTo6fileID4line6columnACx_SSS2itclufC
  // CHECK-NEXT: {{.*}} = apply [[IGNORE_INIT]]<(MemberTests.Type, String, inout Int) -> (Int, String)>({{.*}})
  @Ignore static func staticFn(_ x: String, y: inout Int) -> (Int, String) { (42, "") }

  // CHECK-LABEL: sil hidden [runtime_accessible] [ossa] @$s21runtime_metadata_defs6IgnoreV0A11_attributes11MemberTestsV10mutatingFnSiyFfa : $@convention(thin) () -> @out Optional<Ignore>
  // CHECK: {{.*}} = string_literal utf8 "runtime_attributes/runtime_attributes.swift"
  // CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 66
  // CHECK: {{.*}} = integer_literal $Builtin.IntLiteral, 4
  // CHECK: [[IGNORE_INIT:%.*]] = function_ref @$s21runtime_metadata_defs6IgnoreV10attachedTo6fileID4line6columnACx_SSS2itclufC
  // CHECK-NEXT: {{.*}} = apply [[IGNORE_INIT]]<(inout MemberTests) -> Int>({{.*}})
  @Ignore mutating func mutatingFn() -> Int { 42 }
}

@runtimeMetadata
struct Flag<U> {
  init<T>(attachedTo: T, value: U, function: String = #function) {}
}

struct TestSelfUse {
  static var answer: Int = 42
  static var question: String = ""

  // CHECK-LABEL: sil hidden [runtime_accessible] [ossa] @$s18runtime_attributes4FlagVAA11TestSelfUseV1xSSvpfa : $@convention(thin) () -> @out Optional<Flag<Int>>
  // CHECK: [[ADDRESSOR:%.*]] = function_ref @$s18runtime_attributes11TestSelfUseV6answerSivau
  // CHECK-NEXT: [[ADDR_RESULT:%.*]] = apply [[ADDRESSOR]]() : $@convention(thin) () -> Builtin.RawPointer
  // CHECK-NEXT: [[PROP_ADDR:%.*]] = pointer_to_address [[ADDR_RESULT]] : $Builtin.RawPointer to [strict] $*Int
  // CHECK-NEXT: [[PROP_ACCESS:%.*]] = begin_access [read] [dynamic] [[PROP_ADDR]] : $*Int
  // CHECK-NEXT: [[PROP_VALUE:%.*]] = load [trivial] [[PROP_ACCESS]] : $*Int
  // CHECK-NEXT: end_access [[PROP_ACCESS]] : $*Int
  // CHECK: [[PROP_VAL_COPY:%.*]] = alloc_stack $Int
  // CHECK: store [[PROP_VALUE]] to [trivial] [[PROP_VAL_COPY]] : $*Int
  // CHECK: [[FUNC_NAME:%.*]] = string_literal utf8 "x"
  // CHECK: [[FUNC_NAME_STR:%.*]] = apply {{.*}}([[FUNC_NAME]], {{.*}})
  // CHECK: [[FLAG_INIT_REF:%.*]] = function_ref @$s18runtime_attributes4FlagV10attachedTo5value8functionACyxGqd___xSStclufC
  // CHECK-NEXT: {{.*}} = apply [[FLAG_INIT_REF]]<Int, WritableKeyPath<TestSelfUse, String>>({{.*}}, [[PROP_VAL_COPY]], [[FUNC_NAME_STR]], {{.*}})
  @Flag(value: Self.answer) var x: String = ""

  // CHECK-LABEL: sil hidden [runtime_accessible] [ossa] @$s18runtime_attributes4FlagVAA11TestSelfUseV4testyyFfa : $@convention(thin) () -> @out Optional<Flag<String>>
  // CHECK: [[ADDRESSOR:%.*]] = function_ref @$s18runtime_attributes11TestSelfUseV8questionSSvau
  // CHECK-NEXT: [[ADDR_RESULT:%.*]] = apply [[ADDRESSOR]]() : $@convention(thin) () -> Builtin.RawPointer
  // CHECK-NEXT: [[PROP_ADDR:%.*]] = pointer_to_address [[ADDR_RESULT]] : $Builtin.RawPointer to [strict] $*String
  // CHECK-NEXT: [[PROP_ACCESS:%.*]] = begin_access [read] [dynamic] [[PROP_ADDR]] : $*String
  // CHECK-NEXT: [[PROP_VALUE:%.*]] = load [copy] [[PROP_ACCESS]] : $*String
  // CHECK-NEXT: end_access [[PROP_ACCESS]] : $*String
  // CHECK: [[PROP_VAL_COPY:%.*]] = alloc_stack $String
  // CHECK: store [[PROP_VALUE]] to [init] [[PROP_VAL_COPY]] : $*String
  // CHECK: [[FUNC_NAME:%.*]] = string_literal utf8 "test()"
  // CHECK: [[FUNC_NAME_STR:%.*]] = apply {{.*}}([[FUNC_NAME]], {{.*}})
  // CHECK: [[FLAG_INIT_REF:%.*]] = function_ref @$s18runtime_attributes4FlagV10attachedTo5value8functionACyxGqd___xSStclufC
  // CHECK-NEXT: {{.*}} = apply [[FLAG_INIT_REF]]<String, (TestSelfUse) -> ()>({{.*}}, [[PROP_VAL_COPY]], [[FUNC_NAME_STR]], {{.*}})
  @Flag(value: Self.question) func test() {}
}

// This make sure that Child is valid even though it opted-out of attribute.
_ = Child()
REQUIRES: updating_for_owned_noescape

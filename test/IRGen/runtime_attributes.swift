// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path=%t/RAD.swiftmodule -module-name=RAD -enable-experimental-feature RuntimeDiscoverableAttrs %S/Inputs/runtime_attrs.swift
// RUN: %target-swift-frontend -primary-file %s -emit-ir -I %t -swift-version 5 -enable-experimental-feature RuntimeDiscoverableAttrs | %IRGenFileCheck %s

// REQUIRES: asserts
// REQUIRES: OS=macosx

// First, make sure that we emit:
// - accessible function records for each generator
// - runtime attribute records with correct number of trailing objects

// CHECK: @"$s18runtime_attributes8globalFnyycvpfaAA4FlagHF"
// CHECK: @"$s18runtime_attributes1AV2v1SSvpfaAA4FlagHF"
// CHECK: @"$s18runtime_attributes1AV4compSivpfaAA4FlagHF"
// CHECK: @"$s18runtime_attributes1AV5test1SiycvpZfaAA4FlagHF"
// CHECK: @"$s18runtime_attributes1AV5test2yycvpfaAA4FlagHF"
// CHECK: @"$s18runtime_attributes1AV1xSaySiGSgvpfaAA4FlagHF"
// CHECK: @"$s18runtime_attributes1AV1xSaySiGSgvpfaAA13OnlyPropsTestHF"
// CHECK: @"$s18runtime_attributes1AV5InnerC4testSaySiGSgvpfaAA4FlagHF"
// CHECK: @"$s18runtime_attributes1AV5InnerC4testSaySiGSgvpfaAA13OnlyPropsTestHF"
// CHECK: @"$s18runtime_attributes1AV5InnerAcDCmvpfaAA4FlagHF"
// CHECK: @"$s18runtime_attributes1AAaBVmvpfaAA4FlagHF"
// CHECK: @"$s18runtime_attributes1AV5InnerC1BV03extC10StaticTestyycvpZfaAA4FlagHF"
// CHECK: @"$s18runtime_attributes1AV5InnerC1BV03extC4TestyycvpZfaAA4FlagHF"
// CHECK: @"$s18runtime_attributes1AV5InnerC1BV6storedSivpfaAA13OnlyPropsTestHF"
// CHECK: @"$s18runtime_attributes1AV5InnerC1BV6storedSivpfaAA4FlagHF"
// CHECK: @"$s18runtime_attributes1AV5InnerC1BAeFVmvpfaAA4FlagHF"
// CHECK: @"$s18runtime_attributes1AV5InnerC13extStaticTestyycvpZfaAA4FlagHF"
// CHECK: @"$s18runtime_attributes1AV5InnerC7extTestyycvpZfaAA4FlagHF"
// CHECK: @"$s18runtime_attributes1AV5InnerC11extComputedSivpfa3RAD6IgnoreHF"
// CHECK: @"$s18runtime_attributes1AV5InnerC11extComputedSivpfaAA4FlagHF"
// CHECK: @"$s18runtime_attributes1AV5InnerC11extComputedSivpfaAA13OnlyPropsTestHF"
// CHECK: @"$s18runtime_attributes16WithExternalAttrAaBVmvpfa3RAD6IgnoreHF"
// CHECK: @"$s18runtime_attributes4testyySicvpfa3RAD13TestAmbiguityHF"
// CHECK: @"$s18runtime_attributes4testyySScvpfa3RAD13TestAmbiguityHF"
// CHECK: @"$s18runtime_attributes15TestNoAmbiguityV10testStaticSiycvpZfa3RAD0cE0HF"
// CHECK: @"$s18runtime_attributes15TestNoAmbiguityV10testStaticyycvpZfa3RAD0cE0HF"
// CHECK: @"$s18runtime_attributes15TestNoAmbiguityV8testInstyySi_SStcvpfa3RAD0cE0HF"
// CHECK: @"$s18runtime_attributes15TestNoAmbiguityV8testInstyySi_Sitcvpfa3RAD0cE0HF"
// CHECK: @"$s18runtime_attributes14TestInference1AaBVmvpfa3RAD6IgnoreHF"
// CHECK: @"$s18runtime_attributes14TestInference2AaBCmvpfa3RAD6IgnoreHF"
// CHECK: @"$s18runtime_attributes14TestInference3AaBOmvpfa3RAD6IgnoreHF"
// CHECK: @"$s18runtime_attributes20testWithAvailabilityyySicvpfaAA0E5TestsHF"
// CHECK: @"$s18runtime_attributes20TypeWithAvailabilityAaBCmvpfaAA0E5TestsHF"
// CHECK: @"$s18runtime_attributes23MembersWithAvailabilityC8staticFnyycvpZfaAA0E5TestsHF"
// CHECK: @"$s18runtime_attributes23MembersWithAvailabilityC6instFnyycvpfaAA0E5TestsHF"
// CHECK: @"$s18runtime_attributes23MembersWithAvailabilityC4propSivpfaAA0E5TestsHF"
// CHECK: @"$s18runtime_attributes20attrIsHigherThanFuncyycvpfaAA13FlagWithAvailHF"
// CHECK: @"$s18runtime_attributes9OuterTypeV5InnerV15innerInstFnTestyycvpfaAA07FlagForE7MethodsHF"
// CHECK: @"$s18runtime_attributes9OuterTypeV5InnerV17innerStaticFnTestyycvpZfaAA07FlagForE7MethodsHF"
// CHECK: @"$s18runtime_attributes9OuterTypeV5InnerV07mutableE6FnTest1x_ySS_SiztcvpfaAA07FlagForE7MethodsHF"
// CHECK: @"$s18runtime_attributes9OuterTypeV15outerMutatingFnSiycvpfaAA19FlagForInnerMethodsHF"

// CHECK: @"$s18runtime_attributes4FlagVHa" = internal constant
// CHECK-SAME: i32 0
// CHECK-SAME: @"$s18runtime_attributes4FlagVMn"
// CHECK-SAME: i32 16
// CHECK-SAME: @"$s18runtime_attributes8globalFnyycvpfaAA4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes1AV2v1SSvpfaAA4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes1AV4compSivpfaAA4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes1AV5test1SiycvpZfaAA4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes1AV5test2yycvpfaAA4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes1AV1xSaySiGSgvpfaAA4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes1AV5InnerC4testSaySiGSgvpfaAA4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes1AV5InnerAcDCmvpfaAA4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes1AAaBVmvpfaAA4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes1AV5InnerC1BV03extC10StaticTestyycvpZfaAA4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes1AV5InnerC1BV03extC4TestyycvpZfaAA4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes1AV5InnerC1BV6storedSivpfaAA4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes1AV5InnerC1BAeFVmvpfaAA4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes1AV5InnerC13extStaticTestyycvpZfaAA4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes1AV5InnerC7extTestyycvpZfaAA4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes1AV5InnerC11extComputedSivpfaAA4FlagHF"
// CHECK-SAME: section "__TEXT, __swift5_rattrs, regular"

// CHECK: @"$s18runtime_attributes13OnlyPropsTestVHa" = internal constant
// CHECK-SAME: i32 0
// CHECK-SAME: @"$s18runtime_attributes13OnlyPropsTestVMn"
// CHECK-SAME: i32 4
// CHECK-SAME: @"$s18runtime_attributes1AV1xSaySiGSgvpfaAA13OnlyPropsTestHF"
// CHECK-SAME: @"$s18runtime_attributes1AV5InnerC4testSaySiGSgvpfaAA13OnlyPropsTestHF"
// CHECK-SAME: @"$s18runtime_attributes1AV5InnerC1BV6storedSivpfaAA13OnlyPropsTestHF"
// CHECK-SAME: @"$s18runtime_attributes1AV5InnerC11extComputedSivpfaAA13OnlyPropsTestHF"
// CHECK-SAME: section "__TEXT, __swift5_rattrs, regular"

// CHECK: @"$s3RAD6IgnoreVHa" = internal constant
// CHECK-SAME: i32 0
// CHECK-SAME: %swift.type_descriptor** @"got.$s3RAD6IgnoreVMn"
// CHECK-SAME: i32 5
// CHECK-SAME: @"$s18runtime_attributes1AV5InnerC11extComputedSivpfa3RAD6IgnoreHF"
// CHECK-SAME: @"$s18runtime_attributes16WithExternalAttrAaBVmvpfa3RAD6IgnoreHF"
// CHECK-SAME: @"$s18runtime_attributes14TestInference1AaBVmvpfa3RAD6IgnoreHF"
// CHECK-SAME: @"$s18runtime_attributes14TestInference2AaBCmvpfa3RAD6IgnoreHF"
// CHECK-SAME: @"$s18runtime_attributes14TestInference3AaBOmvpfa3RAD6IgnoreHF"
// CHECK-SAME: section "__TEXT, __swift5_rattrs, regular"

// CHECK: @"$s3RAD13TestAmbiguityVHa" = internal constant
// CHECK-SAME: i32 0
// CHECK-SAME: %swift.type_descriptor** @"got.$s3RAD13TestAmbiguityVMn"
// CHECK-SAME: i32 6
// CHECK-SAME: @"$s18runtime_attributes4testyySicvpfa3RAD13TestAmbiguityHF"
// CHECK-SAME: @"$s18runtime_attributes4testyySScvpfa3RAD13TestAmbiguityHF"
// CHECK-SAME: @"$s18runtime_attributes15TestNoAmbiguityV10testStaticSiycvpZfa3RAD0cE0HF"
// CHECK-SAME: @"$s18runtime_attributes15TestNoAmbiguityV10testStaticyycvpZfa3RAD0cE0HF"
// CHECK-SAME: @"$s18runtime_attributes15TestNoAmbiguityV8testInstyySi_SStcvpfa3RAD0cE0HF"
// CHECK-SAME: @"$s18runtime_attributes15TestNoAmbiguityV8testInstyySi_Sitcvpfa3RAD0cE0HF"
// CHECK-SAME: section "__TEXT, __swift5_rattrs, regular"

// CHECK: @"$s18runtime_attributes17AvailabilityTestsVHa" = internal constant
// CHECK-SAME: i32 0
// CHECK-SAME: @"$s18runtime_attributes17AvailabilityTestsVMn"
// CHECK-SAME: i32 5
// CHECK-SAME: @"$s18runtime_attributes20testWithAvailabilityyySicvpfaAA0E5TestsHF
// CHECK-SAME: @"$s18runtime_attributes20TypeWithAvailabilityAaBCmvpfaAA0E5TestsHF"
// CHECK-SAME: @"$s18runtime_attributes23MembersWithAvailabilityC8staticFnyycvpZfaAA0E5TestsHF"
// CHECK-SAME: @"$s18runtime_attributes23MembersWithAvailabilityC6instFnyycvpfaAA0E5TestsHF"
// CHECK-SAME: @"$s18runtime_attributes23MembersWithAvailabilityC4propSivpfaAA0E5TestsHF"
// CHECK-SAME: section "__TEXT, __swift5_rattrs, regular"

// CHECK: @"$s18runtime_attributes13FlagWithAvailVHa" = internal constant
// CHECK-SAME: i32 0
// CHECK-SAME: @"$s18runtime_attributes13FlagWithAvailVMn"
// CHECK-SAME: i32 1
// CHECK-SAME: @"$s18runtime_attributes20attrIsHigherThanFuncyycvpfaAA13FlagWithAvailHF"
// CHECK-SAME: section "__TEXT, __swift5_rattrs, regular"

// CHECK: @"$s18runtime_attributes19FlagForInnerMethodsVHa" = internal constant
// CHECK-SAME: i32 0
// CHECK-SAME: @"$s18runtime_attributes19FlagForInnerMethodsVMn"
// CHECK-SAME: i32 4
// CHECK-SAME: @"$s18runtime_attributes9OuterTypeV5InnerV15innerInstFnTestyycvpfaAA07FlagForE7MethodsHF"
// CHECK-SAME: @"$s18runtime_attributes9OuterTypeV5InnerV17innerStaticFnTestyycvpZfaAA07FlagForE7MethodsHF"
// CHECK-SAME: @"$s18runtime_attributes9OuterTypeV5InnerV07mutableE6FnTest1x_ySS_SiztcvpfaAA07FlagForE7MethodsHF"
// CHECK-SAME: @"$s18runtime_attributes9OuterTypeV15outerMutatingFnSiycvpfaAA19FlagForInnerMethodsHF"
// CHECK-SAME: section "__TEXT, __swift5_rattrs, regular"

// CHECK: @"$s3RAD8EnumFlagOHa" = internal constant
// CHECK-SAME: i32 0
// CHECK-SAME: %swift.type_descriptor** @"got.$s3RAD8EnumFlagOMn"
// CHECK-SAME: i32 5
// CHECK-SAME: @"$s18runtime_attributes14globalEnumTestSi_SaySSGtSgycvpfa3RAD0D4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes12EnumTypeTestV1xSivpfa3RAD0C4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes12EnumTypeTestV8testInstyycvpfa3RAD0C4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes12EnumTypeTestV10testStaticSiycvpZfa3RAD0C4FlagHF"
// CHECK-SAME: @"$s18runtime_attributes12EnumTypeTestAaBVmvpfa3RAD0C4FlagHF"
// CHECK-SAME: section "__TEXT, __swift5_rattrs, regular"

import RAD

@runtimeMetadata
struct Flag<T> {
  init(attachedTo: T.Type, _ description: String = "") {}
  init<Args>(attachedTo: (Args) -> T, _ description: String = "") {}
  init<Base>(attachedTo: KeyPath<Base, T>, _ description: String = "") {}
}

@runtimeMetadata
struct OnlyPropsTest<B, V> {
  init(attachedTo: KeyPath<B, V>) {}
}

// - Check that all of the generator functions have been emitted

// CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes8globalFnyycvpfaAA4Flag"(%T18runtime_attributes4FlagVyytGSg* noalias nocapture sret(%T18runtime_attributes4FlagVyytGSg) %0)
@Flag("global") func globalFn() {}

@Flag
struct A {
  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes1AV2v1SSvpfaAA4Flag"(%T18runtime_attributes4FlagVySSGSg* noalias nocapture sret(%T18runtime_attributes4FlagVySSGSg) %0)
  @Flag("v1") var v1: String = ""

  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes1AV4compSivpfaAA4Flag"(%T18runtime_attributes4FlagVySiGSg* noalias nocapture sret(%T18runtime_attributes4FlagVySiGSg) %0)
  @Flag var comp: Int {
    get { 42 }
  }

  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes1AV5test1SiycvpZfaAA4Flag"(%T18runtime_attributes4FlagVySiGSg* noalias nocapture sret(%T18runtime_attributes4FlagVySiGSg) %0)
  @Flag static func test1() -> Int { 42 }

  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes1AV5test2yycvpfaAA4Flag"(%T18runtime_attributes4FlagVyytGSg* noalias nocapture sret(%T18runtime_attributes4FlagVyytGSg) %0)
  @Flag("test2") func test2() {} // Ok

  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes1AV1xSaySiGSgvpfaAA4Flag"(%T18runtime_attributes4FlagVySaySiGSgGSg* noalias nocapture sret(%T18runtime_attributes4FlagVySaySiGSgGSg) %0)
  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes1AV1xSaySiGSgvpfaAA13OnlyPropsTest"(%T18runtime_attributes13OnlyPropsTestVyAA1AVSaySiGSgGSg* noalias nocapture sret(%T18runtime_attributes13OnlyPropsTestVyAA1AVSaySiGSgGSg) %0)
  @OnlyPropsTest @Flag("x") var x: [Int]? = [] // Ok

  @Flag("Inner type") class Inner {
    // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes1AV5InnerC4testSaySiGSgvpfaAA4Flag"(%T18runtime_attributes4FlagVySaySiGSgGSg* noalias nocapture sret(%T18runtime_attributes4FlagVySaySiGSgGSg) %0)
    // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes1AV5InnerC4testSaySiGSgvpfaAA13OnlyPropsTest"(%T18runtime_attributes13OnlyPropsTestVyAA1AV5InnerCSaySiGSgGSg* noalias nocapture sret(%T18runtime_attributes13OnlyPropsTestVyAA1AV5InnerCSaySiGSgGSg) %0)
    @OnlyPropsTest @Flag("test property") var test: [Int]? = nil // Ok
  }
}

// The generator for `struct A` is emitted last.
// CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes1AAaBVmvpfaAA4Flag"(%T18runtime_attributes4FlagVyAA1AVGSg* noalias nocapture sret(%T18runtime_attributes4FlagVyAA1AVGSg) %0)

extension A.Inner {
  @Flag("B type") struct B {
    // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes1AV5InnerC1BV03extC10StaticTestyycvpZfaAA4Flag"(%T18runtime_attributes4FlagVyytGSg* noalias nocapture sret(%T18runtime_attributes4FlagVyytGSg) %0)
    @Flag static func extInnerStaticTest() {}
    // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes1AV5InnerC1BV03extC4TestyycvpZfaAA4Flag"(%T18runtime_attributes4FlagVyytGSg* noalias nocapture sret(%T18runtime_attributes4FlagVyytGSg) %0)
    @Flag static func extInnerTest() {}

    @Flag("stored prop") @OnlyPropsTest let stored: Int = 42
  } // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes1AV5InnerC1BAeFVmvpfaAA4Flag"(%T18runtime_attributes4FlagVyAA1AV5InnerC1BVGSg* noalias nocapture sret(%T18runtime_attributes4FlagVyAA1AV5InnerC1BVGSg) %0

  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes1AV5InnerC13extStaticTestyycvpZfaAA4Flag"(%T18runtime_attributes4FlagVyytGSg* noalias nocapture sret(%T18runtime_attributes4FlagVyytGSg) %0)
  @Flag static func extStaticTest() {}
  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes1AV5InnerC7extTestyycvpZfaAA4Flag"(%T18runtime_attributes4FlagVyytGSg* noalias nocapture sret(%T18runtime_attributes4FlagVyytGSg) %0)
  @Flag static func extTest() {}

  @OnlyPropsTest @Flag @Ignore var extComputed: Int {
    get { 42 }
  }
}

// CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes16WithExternalAttrAaBVmvpfa3RAD6Ignore"(%T3RAD6IgnoreVy18runtime_attributes16WithExternalAttrVmGSg* noalias nocapture sret(%T3RAD6IgnoreVy18runtime_attributes16WithExternalAttrVmGSg) %0)
@Ignore struct WithExternalAttr {}

// CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes4testyySicvpfa3RAD13TestAmbiguity"(%T3RAD13TestAmbiguityVSg* noalias nocapture sret(%T3RAD13TestAmbiguityVSg) %0)
@TestAmbiguity public func test(_: Int) {}
// CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes4testyySScvpfa3RAD13TestAmbiguity"(%T3RAD13TestAmbiguityVSg* noalias nocapture sret(%T3RAD13TestAmbiguityVSg) %0)
@TestAmbiguity public func test(_: String) {}

public struct TestNoAmbiguity {
  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes15TestNoAmbiguityV10testStaticSiycvpZfa3RAD0cE0"(%T3RAD13TestAmbiguityVSg* noalias nocapture sret(%T3RAD13TestAmbiguityVSg) %0)
  @TestAmbiguity static func testStatic() -> Int { 42 }
  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes15TestNoAmbiguityV10testStaticyycvpZfa3RAD0cE0"(%T3RAD13TestAmbiguityVSg* noalias nocapture sret(%T3RAD13TestAmbiguityVSg) %0)
  @TestAmbiguity static func testStatic() {}

  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes15TestNoAmbiguityV8testInstyySi_SStcvpfa3RAD0cE0"(%T3RAD13TestAmbiguityVSg* noalias nocapture sret(%T3RAD13TestAmbiguityVSg) %0)
  @TestAmbiguity func testInst(_: Int, _: String) {}
  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes15TestNoAmbiguityV8testInstyySi_Sitcvpfa3RAD0cE0"(%T3RAD13TestAmbiguityVSg* noalias nocapture sret(%T3RAD13TestAmbiguityVSg) %0)
  @TestAmbiguity func testInst(_: Int, _: Int) {}
}

// CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes14TestInference1AaBVmvpfa3RAD6Ignore"(%T3RAD6IgnoreVy18runtime_attributes14TestInference1VmGSg* noalias nocapture sret(%T3RAD6IgnoreVy18runtime_attributes14TestInference1VmGSg) %0)
public struct TestInference1 : Ignored {}
// CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes14TestInference2AaBCmvpfa3RAD6Ignore"(%T3RAD6IgnoreVy18runtime_attributes14TestInference2CmGSg* noalias nocapture sret(%T3RAD6IgnoreVy18runtime_attributes14TestInference2CmGSg) %0)
public class  TestInference2 : Ignored {}
// CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes14TestInference3AaBOmvpfa3RAD6Ignore"(%T3RAD6IgnoreVy18runtime_attributes14TestInference3OmGSg* noalias nocapture sret(%T3RAD6IgnoreVy18runtime_attributes14TestInference3OmGSg) %0)
public enum   TestInference3 : Ignored {}

@runtimeMetadata
struct AvailabilityTests {
  init(attachedTo: Any) {}
}

// CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes20testWithAvailabilityyySicvpfaAA0E5Tests"(%T18runtime_attributes17AvailabilityTestsVSg* noalias nocapture sret(%T18runtime_attributes17AvailabilityTestsVSg) %0)
// CHECK: entry:
// CHECK: {{.*}} = call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(i64 42, i64 0, i64 0)
@available(macOS, introduced: 42.0)
@AvailabilityTests
func testWithAvailability(_: Int) {}

// CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes20TypeWithAvailabilityAaBCmvpfaAA0E5Tests"(%T18runtime_attributes17AvailabilityTestsVSg* noalias nocapture sret(%T18runtime_attributes17AvailabilityTestsVSg) %0)
// CHECK: entry:
// CHECK: {{.*}} = call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(i64 100, i64 0, i64 0)
@available(macOS, introduced: 100.0)
@AvailabilityTests
class TypeWithAvailability {}

class MembersWithAvailability {
  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes23MembersWithAvailabilityC8staticFnyycvpZfaAA0E5Tests"(%T18runtime_attributes17AvailabilityTestsVSg* noalias nocapture sret(%T18runtime_attributes17AvailabilityTestsVSg) %0)
  // CHECK: entry:
  // CHECK: {{.*}} = call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(i64 201, i64 0, i64 0)
  @AvailabilityTests
  @available(macOS, introduced: 201.0)
  static func staticFn() {}

  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes23MembersWithAvailabilityC6instFnyycvpfaAA0E5Tests"(%T18runtime_attributes17AvailabilityTestsVSg* noalias nocapture sret(%T18runtime_attributes17AvailabilityTestsVSg) %0)
  // CHECK: entry:
  // CHECK: {{.*}} = call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(i64 202, i64 0, i64 0)
  @AvailabilityTests
  @available(macOS, introduced: 202.0)
  func instFn() {}

  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes23MembersWithAvailabilityC4propSivpfaAA0E5Tests"(%T18runtime_attributes17AvailabilityTestsVSg* noalias nocapture sret(%T18runtime_attributes17AvailabilityTestsVSg) %0)
  // CHECK: entry:
  // CHECK: {{.*}} = call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(i64 203, i64 0, i64 0)
  @AvailabilityTests
  @available(macOS, introduced: 203.0)
  var prop: Int { get { 42 } }
}

@runtimeMetadata
@available(macOS, introduced: 110.0)
struct FlagWithAvail {
  init(attachedTo: Any) {}
}

// CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes20attrIsHigherThanFuncyycvpfaAA13FlagWithAvail"(%T18runtime_attributes13FlagWithAvailVSg* noalias nocapture sret(%T18runtime_attributes13FlagWithAvailVSg) %0)
// CHECK: entry:
// CHECK: {{.*}} = call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(i64 110, i64 0, i64 0)
@FlagWithAvail
@available(macOS, obsoleted: 100.0)
func attrIsHigherThanFunc() {}

func test_local_type_with_protocol_conformance() {
  // CHECK-NOT: define hidden swiftcc void @"$s18runtime_attributes41test_local_type_with_protocol_conformanceyyF9LocalTest{{.*}}"
  struct LocalTest : Ignored {} // Ok but attribute is not inferred.
}

@runtimeMetadata
struct FlagForInnerMethods<Result> {
  init(attachedTo: () -> Result) {}
  init<T>(attachedTo: (T) -> Result) {}
  init<T>(attachedTo: (T.Type) -> Result) {}
  init<T>(attachedTo: (inout T) -> Result) {}
  init<T>(attachedTo: (inout T, String, inout Int) -> Result) {}
}

struct OuterType {
  struct Inner {
    // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes9OuterTypeV5InnerV15innerInstFnTestyycvpfaAA07FlagForE7Methods"(%T18runtime_attributes19FlagForInnerMethodsVyytGSg* noalias nocapture sret(%T18runtime_attributes19FlagForInnerMethodsVyytGSg) %0)
    @FlagForInnerMethods func innerInstFnTest() {}
    // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes9OuterTypeV5InnerV17innerStaticFnTestyycvpZfaAA07FlagForE7Methods"(%T18runtime_attributes19FlagForInnerMethodsVyytGSg* noalias nocapture sret(%T18runtime_attributes19FlagForInnerMethodsVyytGSg) %0)
    @FlagForInnerMethods static func innerStaticFnTest() {}

    // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes9OuterTypeV5InnerV07mutableE6FnTest1x_ySS_SiztcvpfaAA07FlagForE7Methods"(%T18runtime_attributes19FlagForInnerMethodsVyytGSg* noalias nocapture sret(%T18runtime_attributes19FlagForInnerMethodsVyytGSg) %0)
    @FlagForInnerMethods mutating func mutableInnerFnTest(x: String, _ y: inout Int) {}
  }
}

extension OuterType {
  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes9OuterTypeV15outerMutatingFnSiycvpfaAA19FlagForInnerMethods"(%T18runtime_attributes19FlagForInnerMethodsVySiGSg* noalias nocapture sret(%T18runtime_attributes19FlagForInnerMethodsVySiGSg) %0)
  @FlagForInnerMethods mutating func outerMutatingFn() -> Int { 42 }
}

// CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes14globalEnumTestSi_SaySSGtSgycvpfa3RAD0D4Flag"(%T3RAD8EnumFlagOyytSi_SaySSGtSgGSg* noalias nocapture sret(%T3RAD8EnumFlagOyytSi_SaySSGtSgGSg) %0)
@EnumFlag func globalEnumTest() -> (Int, [String])? {
  nil
}

@EnumFlag struct EnumTypeTest {
  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes12EnumTypeTestV1xSivpfa3RAD0C4Flag"(%T3RAD8EnumFlagOy18runtime_attributes0B8TypeTestVSiGSg* noalias nocapture sret(%T3RAD8EnumFlagOy18runtime_attributes0B8TypeTestVSiGSg) %0)
  @EnumFlag var x: Int = 42
  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes12EnumTypeTestV8testInstyycvpfa3RAD0C4Flag"(%T3RAD8EnumFlagOy18runtime_attributes0B8TypeTestVytGSg* noalias nocapture sret(%T3RAD8EnumFlagOy18runtime_attributes0B8TypeTestVytGSg) %0)
  @EnumFlag func testInst() {}
  // CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes12EnumTypeTestV10testStaticSiycvpZfa3RAD0C4Flag"(%T3RAD8EnumFlagOy18runtime_attributes0B8TypeTestVmSiGSg* noalias nocapture sret(%T3RAD8EnumFlagOy18runtime_attributes0B8TypeTestVmSiGSg) %0)
  @EnumFlag static func testStatic() -> Int { 42 }
}
// CHECK-LABEL: define hidden swiftcc void @"$s18runtime_attributes12EnumTypeTestAaBVmvpfa3RAD0C4Flag"(%T3RAD8EnumFlagOy18runtime_attributes0B8TypeTestVytGSg* noalias nocapture sret(%T3RAD8EnumFlagOy18runtime_attributes0B8TypeTestVytGSg) %0)

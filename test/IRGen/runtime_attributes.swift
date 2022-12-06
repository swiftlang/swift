// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path=%t/RAD.swiftmodule -module-name=RAD -enable-experimental-feature RuntimeDiscoverableAttrs %S/Inputs/runtime_attrs.swift
// RUN: %target-swift-frontend -primary-file %s -emit-ir -I %t -swift-version 5 -enable-experimental-feature RuntimeDiscoverableAttrs | %IRGenFileCheck %s

// REQUIES: asserts

// First, make sure that we emit:
// - accessible function records for each generator
// - runtime attribute records with correct number of trailing objects

// CHECK: @"$s18runtime_attributes9gloabalFnyycvpfaAA4FlagHF"
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

// CHECK: @"$s18runtime_attributes4FlagVHa" = internal constant { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 } { i32 0, i32 trunc (i64 sub (i64 ptrtoint (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i16, i16, i8, i8, i8, i8 }>* @"$s18runtime_attributes4FlagVMn" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }* @"$s18runtime_attributes4FlagVHa", i32 0, i32 1) to i64)) to i32), i32 16, {{.*}} @"$s18runtime_attributes4FlagVHa", i32 0, i32 34) to i64)) to i32) }, section "__TEXT, __swift5_rattrs, regular", align 4

// CHECK: @"$s18runtime_attributes13OnlyPropsTestVHa" = internal constant { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 } { i32 0, i32 trunc (i64 sub (i64 ptrtoint (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i16, i16, i8, i8, i8, i8 }>* @"$s18runtime_attributes13OnlyPropsTestVMn" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }* @"$s18runtime_attributes13OnlyPropsTestVHa", i32 0, i32 1) to i64)) to i32), i32 4, {{.*}} @"$s18runtime_attributes13OnlyPropsTestVHa", i32 0, i32 10) to i64)) to i32) }, section "__TEXT, __swift5_rattrs, regular", align 4

// CHECK: @"$s3RAD6IgnoreVHa" = internal constant { i32, i32, i32, i32, i32, i32, i32 } { i32 0, i32 trunc (i64 sub (i64 ptrtoint (%swift.type_descriptor* @"$s3RAD6IgnoreVMn" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD6IgnoreVHa", i32 0, i32 1) to i64)) to i32), i32 2, i32 trunc (i64 sub (i64 ptrtoint (<{ [2 x i8], i8 }>* @"symbolic Si" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD6IgnoreVHa", i32 0, i32 3) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (%swift.accessible_function* @"$s18runtime_attributes1AV5InnerC11extComputedSivpfa3RAD6IgnoreHF" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD6IgnoreVHa", i32 0, i32 4) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (<{ i8, i32, [1 x i8], i8 }>* @"symbolic _____m 18runtime_attributes16WithExternalAttrV" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD6IgnoreVHa", i32 0, i32 5) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (%swift.accessible_function* @"$s18runtime_attributes16WithExternalAttrAaBVmvpfa3RAD6IgnoreHF" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD6IgnoreVHa", i32 0, i32 6) to i64)) to i32) }, section "__TEXT, __swift5_rattrs, regular", align 4

// CHECK: @"$s3RAD13TestAmbiguityVHa" = internal constant { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 } { i32 0, i32 trunc (i64 sub (i64 ptrtoint (%swift.type_descriptor* @"$s3RAD13TestAmbiguityVMn" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD13TestAmbiguityVHa", i32 0, i32 1) to i64)) to i32), i32 6, i32 trunc (i64 sub (i64 ptrtoint (<{ [4 x i8], i8 }>* @"symbolic ySic" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD13TestAmbiguityVHa", i32 0, i32 3) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (%swift.accessible_function* @"$s18runtime_attributes4testyySicvpfa3RAD13TestAmbiguityHF" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD13TestAmbiguityVHa", i32 0, i32 4) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (<{ [4 x i8], i8 }>* @"symbolic ySSc" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD13TestAmbiguityVHa", i32 0, i32 5) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (%swift.accessible_function* @"$s18runtime_attributes4testyySScvpfa3RAD13TestAmbiguityHF" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD13TestAmbiguityVHa", i32 0, i32 6) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (<{ [4 x i8], i8, i32, [2 x i8], i8 }>* @"symbolic Siyc_____mc 18runtime_attributes15TestNoAmbiguityV" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD13TestAmbiguityVHa", i32 0, i32 7) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (%swift.accessible_function* @"$s18runtime_attributes15TestNoAmbiguityV10testStaticSiycvpZfa3RAD0cE0HF" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD13TestAmbiguityVHa", i32 0, i32 8) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (<{ [3 x i8], i8, i32, [2 x i8], i8 }>* @"symbolic yyc_____mc 18runtime_attributes15TestNoAmbiguityV" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD13TestAmbiguityVHa", i32 0, i32 9) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (%swift.accessible_function* @"$s18runtime_attributes15TestNoAmbiguityV10testStaticyycvpZfa3RAD0cE0HF" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD13TestAmbiguityVHa", i32 0, i32 10) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (<{ [8 x i8], i8, i32, [1 x i8], i8 }>* @"symbolic ySi_SStc_____c 18runtime_attributes15TestNoAmbiguityV" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD13TestAmbiguityVHa", i32 0, i32 11) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (%swift.accessible_function* @"$s18runtime_attributes15TestNoAmbiguityV8testInstyySi_SStcvpfa3RAD0cE0HF" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD13TestAmbiguityVHa", i32 0, i32 12) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (<{ [8 x i8], i8, i32, [1 x i8], i8 }>* @"symbolic ySi_Sitc_____c 18runtime_attributes15TestNoAmbiguityV" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD13TestAmbiguityVHa", i32 0, i32 13) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (%swift.accessible_function* @"$s18runtime_attributes15TestNoAmbiguityV8testInstyySi_Sitcvpfa3RAD0cE0HF" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }* @"$s3RAD13TestAmbiguityVHa", i32 0, i32 14) to i64)) to i32) }, section "__TEXT, __swift5_rattrs, regular", align 4

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

// CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes9gloabalFnyycvpfaAA4Flag"()
@Flag("global") func gloabalFn() {}

@Flag
struct A {
  // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes1AV2v1SSvpfaAA4Flag"()
  @Flag("v1") var v1: String = ""

  // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes1AV4compSivpfaAA4Flag"()
  @Flag var comp: Int {
    get { 42 }
  }

  // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes1AV5test1SiycvpZfaAA4Flag"()
  @Flag static func test1() -> Int { 42 }

  // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes1AV5test2yycvpfaAA4Flag"()
  @Flag("test2") func test2() {} // Ok

  // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes1AV1xSaySiGSgvpfaAA4Flag"()
  // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes1AV1xSaySiGSgvpfaAA13OnlyPropsTest"()
  @OnlyPropsTest @Flag("x") var x: [Int]? = [] // Ok

  @Flag("Inner type") class Inner {
    // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes1AV5InnerC4testSaySiGSgvpfaAA4Flag"()
    // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes1AV5InnerC4testSaySiGSgvpfaAA13OnlyPropsTest"()
    @OnlyPropsTest @Flag("test property") var test: [Int]? = nil // Ok
  }
}

// The generator for `struct A` is emitted last.
// CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes1AAaBVmvpfaAA4Flag"()

extension A.Inner {
  @Flag("B type") struct B {
    // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes1AV5InnerC1BV03extC10StaticTestyycvpZfaAA4Flag"()
    @Flag static func extInnerStaticTest() {}
    // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes1AV5InnerC1BV03extC4TestyycvpZfaAA4Flag"()
    @Flag static func extInnerTest() {}

    @Flag("stored prop") @OnlyPropsTest let stored: Int = 42
  } // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes1AV5InnerC1BAeFVmvpfaAA4Flag"()

  // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes1AV5InnerC13extStaticTestyycvpZfaAA4Flag"()
  @Flag static func extStaticTest() {}
  // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes1AV5InnerC7extTestyycvpZfaAA4Flag"()
  @Flag static func extTest() {}

  @OnlyPropsTest @Flag @Ignore var extComputed: Int {
    get { 42 }
  }
}

// CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes16WithExternalAttrAaBVmvpfa3RAD6Ignore"()
@Ignore struct WithExternalAttr {}

// CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes4testyySicvpfa3RAD13TestAmbiguity"()
@TestAmbiguity public func test(_: Int) {}
// CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes4testyySScvpfa3RAD13TestAmbiguity"()
@TestAmbiguity public func test(_: String) {}

public struct TestNoAmbiguity {
  // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes15TestNoAmbiguityV10testStaticSiycvpZfa3RAD0cE0"()
  @TestAmbiguity static func testStatic() -> Int { 42 }
  // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes15TestNoAmbiguityV10testStaticyycvpZfa3RAD0cE0"()
  @TestAmbiguity static func testStatic() {}

  // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes15TestNoAmbiguityV8testInstyySi_SStcvpfa3RAD0cE0"()
  @TestAmbiguity func testInst(_: Int, _: String) {}
  // CHECK-LABEL: define hidden swiftcc i8 @"$s18runtime_attributes15TestNoAmbiguityV8testInstyySi_Sitcvpfa3RAD0cE0"()
  @TestAmbiguity func testInst(_: Int, _: Int) {}
}

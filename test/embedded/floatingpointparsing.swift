// RUN: %target-swift-frontend -emit-ir -o - %s -enable-experimental-feature Embedded -parse-as-library | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

// Check that initializing a Double with a string literal doesn't result in unresolved symbols
// CHECK-NOT: stdlib_isOSVersionAtLeast

// CHECK: define {{.*}} @"$e20floatingpointparsing11testLiteralSdSgyF"()
public func testLiteral() -> Double? {
  // CHECK: call swiftcc {{.*}} @"$eSdySdSgxcSyRzlufCSS_Tt0g5"
  // CHECK-NOT: stdlib_isOSVersionAtLeast
  Double("1.5")
}

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/has_symbol_helper.swiftmodule -parse-as-library %S/Inputs/has_symbol/has_symbol_helper.swift -enable-library-evolution -target %target-swift-5.1-abi-triple -DCONCURRENCY
// RUN: %target-swift-frontend -emit-irgen %s -I %t -module-name test | %FileCheck %s

// REQUIRES: concurrency
// UNSUPPORTED: OS=windows-msvc

@_weakLinked import has_symbol_helper

public func testGlobalFunctions() {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper9asyncFuncyyYaFTwS"()
  if #_hasSymbol(asyncFunc) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper12isolatedFuncyyFTwS"()
  if #_hasSymbol(isolatedFunc) {}
}

// --- asyncFunc() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper9asyncFuncyyYaFTwS"()
// CHECK:   [[V0:%.*]] = icmp ne ptr @"$s17has_symbol_helper9asyncFuncyyYaF", null
// CHECK:   [[V1:%.*]] = icmp ne ptr @"$s17has_symbol_helper9asyncFuncyyYaFTu", null
// CHECK:   [[RES:%.*]] = and i1 [[V0]], [[V1]]
// CHECK:   ret i1 [[RES]]

// --- isolatedFunc() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper12isolatedFuncyyFTwS"()
// CHECK:   [[RES:%.*]] = icmp ne ptr @"$s17has_symbol_helper12isolatedFuncyyF", null
// CHECK:   ret i1 [[RES]]

public func testActor(_ a: A) {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1ACACycfcTwS"()
  if #_hasSymbol(A.init) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1AC11asyncMethodyyYaFTwS"()
  if #_hasSymbol(a.asyncMethod) {}

  // FIXME: Add support for actor-isolated methods
}

// --- A.init() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1ACACycfcTwS"()
// CHECK:   [[V0:%.*]] = icmp ne ptr @"$s17has_symbol_helper1ACACycfc", null
// CHECK:   [[V1:%.*]] = icmp ne ptr @"$s17has_symbol_helper1ACACycfC", null
// CHECK:   [[RES:%.*]] = and i1 [[V0]], [[V1]]
// CHECK:   ret i1 [[RES]]

// --- A.asyncMethod() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1AC11asyncMethodyyYaFTwS"()
// CHECK:   [[V0:%.*]] = icmp ne ptr @"$s17has_symbol_helper1AC11asyncMethodyyYaFTj", null
// CHECK:   [[V1:%.*]] = icmp ne ptr @"$s17has_symbol_helper1AC11asyncMethodyyYaFTjTu", null
// CHECK:   [[V2:%.*]] = and i1 [[V0]], [[V1]]
// CHECK:   [[V3:%.*]] = icmp ne ptr @"$s17has_symbol_helper1AC11asyncMethodyyYaFTq", null
// CHECK:   [[RES:%.*]] = and i1 [[V2]], [[V3]]
// CHECK:   ret i1 [[RES]]

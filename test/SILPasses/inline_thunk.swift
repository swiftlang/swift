// RUN: %target-swift-frontend -primary-file %s -parse-as-library -Xllvm -sil-inline-test-threshold=0 -emit-ir -O | FileCheck %s

// Two thunks are generated:
// 1. from function signature opts
// 2. the witness thunk
// Both should not inline the testit function and should set the noinline-attribute for llvm.

// CHECK-LABEL: define hidden i64 @_TFV{{.*}}testit
// CHECK: call i64 @_TTS{{.*}}testit{{.*}} #[[ATTR:[0-9]+]]
// CHECK: ret

// CHECK-LABEL: define hidden i64 @_TTW{{.*}}testit
// CHECK: call i64 @_TTS{{.*}}testit{{.*}} #[[ATTR]]
// CHECK: ret

// CHECK: attributes #[[ATTR]] = { noinline }

protocol Proto {
  func testit(x: Int) -> Int
}


struct TestStruct : Proto {
  func testit(x: Int) -> Int {
    return x + 1
  }
}



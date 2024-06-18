// RUN: %target-swiftxx-frontend -I %S/Inputs -emit-irgen %s | %FileCheck %s

// REQUIRES: OS=macosx || OS=linux-android

import Closure

// CHECK: define internal void @"$s4main36testClosureToFuncPtrReturnNonTrivialyyFSo0hI0VycfU_To"(ptr noalias sret(%{{.*}}) %[[V0:.*]])
// CHECK: call swiftcc void @"$s4main36testClosureToFuncPtrReturnNonTrivialyyFSo0hI0VycfU_"(ptr noalias sret(%{{.*}}) %[[V0]])
// CHECK: ret void

public func testClosureToFuncPtrReturnNonTrivial() {
  cfuncReturnNonTrivial2({() -> NonTrivial in return NonTrivial()});
}

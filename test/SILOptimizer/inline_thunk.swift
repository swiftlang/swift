// RUN: %target-swift-frontend -primary-file %s -parse-as-library -emit-ir -O | %FileCheck %s

// Two thunks are generated:
// 1. from function signature opts
// 2. the witness thunk
// Both should not inline the testit function and should set the noinline-attribute for llvm.

// CHECK-LABEL: define hidden swiftcc i32 @{{.*}}testit{{.*}}F(i32)
// CHECK: call swiftcc i32 @{{.*}}testit{{.*}}Tf{{.*}} #[[ATTR:[0-9]+]]
// CHECK: ret

// CHECK-LABEL: define hidden swiftcc i32 @{{.*}}testit{{.*}}FTW(i32
// CHECK: call swiftcc i32 @{{.*}}testit{{.*}}Tf{{.*}} #[[ATTR]]
// CHECK: ret

// CHECK: attributes #[[ATTR]] = { noinline }

protocol Proto {
  func testit(x: Int32) -> Int32
}


struct TestStruct : Proto {
  func testit(x: Int32) -> Int32 {
	var y = x * 2
	y += 1
	y *= x
	y += 1
	y *= x
	y += 1
	y *= x
	y += 1
	y *= x
	y += 1
	y *= x
	y += 1
    return y
  }
}



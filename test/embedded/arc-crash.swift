// RUN: %target-swift-frontend -target armv7-apple-none-macho -assert-config Debug -Osize -Xcc -D__MACH__ -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s
// RUN: %target-swift-frontend -target arm64-apple-none-macho -assert-config Debug -Osize -Xcc -D__MACH__ -Xcc -D__arm64__ -Xcc -D__APPLE__ -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: VENDOR=apple
// REQUIRES: optimized_stdlib

public func test() {}
test()

// CHECK: define {{.*}}i32 @main

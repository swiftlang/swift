// RUN: %target-swift-frontend -import-objc-header %S/Inputs/fnptr.h -swift-version 5 -target arm64e-apple-ios13.0 %s -emit-ir -module-name test -use-clang-function-types -Xcc -Xclang -Xcc -fptrauth-function-pointer-type-discrimination | %FileCheck %s

// REQUIRES: CPU=arm64e
// REQUIRES: OS=ios

// This test used to crash in IRGen because of mismatching pointer types.

// CHECK: define{{.*}} swiftcc void @"$s4testAAyyF"()
// CHECK:   store i8* bitcast {{.*}} @"$s4testAAyyFySpySiGSgcfU_To.ptrauth" to i8*), i8**

public func test() {
  var tmp = Container(
    fnptr: { (x) in return }
  )
}

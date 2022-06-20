// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name X -num-threads 1 -O -enable-single-module-llvm-emission -emit-ir %s %S/Inputs/single-module-num-threads-2.swift -o %t/single-module-num-threads.ll -o %t/single-module-num-threads-2.ll
// RUN: %FileCheck %s < %t/single-module-num-threads.ll
// RUN: %FileCheck %s --check-prefix=EMPTY < %t/single-module-num-threads-2.ll

// CHECK: define{{.*}} swiftcc void @"$s1X1AyyF"()
// CHECK: define{{.*}} swiftcc void @"$s1X1ByyF"()

// EMPTY-NOT: s1X1AyyF
// EMPTY-NOT: s1X1ByyF

// Make sure that with enable-single-module-llvm-emission we emit all code into
// one llvm module.
public func A() {
    print("A")
}

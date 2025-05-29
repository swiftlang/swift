// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name main %s -emit-ir | %FileCheck --check-prefix=CHECK-IR %s
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name main %s -c -o %t/a.o
// RUN: %target-clang %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos -lswift_Concurrency -lswift_ConcurrencyDefaultExecutor -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

import _Concurrency

actor MyActor {
    var value: Int = 42
    func foo() async {
        print("value: \(value)")
    }

    func thisIsUnused() async {
        print("unused")
    }
}

@main struct Main {
    static func main() async {
        let n = MyActor()
        await n.foo()
    }
}

// CHECK-IR:      @swift_deletedAsyncMethodErrorTu =
// CHECK-IR:      @"$e4main7MyActorCN" = global <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }> <{ 
// CHECK-IR-SAME:   ptr null,
// CHECK-IR-SAME:   ptr @"$e4main7MyActorCfD",
// CHECK-IR-SAME:   ptr null,
// CHECK-IR-SAME:   ptr @swift_deletedMethodError,
// CHECK-IR-SAME:   ptr @swift_deletedMethodError,
// CHECK-IR-SAME:   ptr @swift_deletedMethodError,
// CHECK-IR-SAME:   ptr @"$e4main7MyActorC3fooyyYaFTu",
// CHECK-IR-SAME:   ptr @got.swift_deletedAsyncMethodErrorTu,
// CHECK-IR-SAME:   ptr @"$e4main7MyActorCACycfC"
// CHECK-IR-SAME: }>, align 8

// CHECK-IR-NOT:  $e4main7MyActorC12thisIsUnusedyyYaF

// CHECK-IR: define swifttailcc void @swift_deletedAsyncMethodError(ptr swiftasync %0)

// CHECK: value: 42

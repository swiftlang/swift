// RUN: %swift -swift-version 5 -target arm64e-apple-macos11.0 -parse-stdlib %s -emit-ir -disable-llvm-optzns -enable-experimental-concurrency -o - | %FileCheck %s

// REQUIRES: CODEGENERATOR=ARM
// REQUIRES: concurrency
// REQUIRES: CPU=arm64e
// REQUIRES: OS=macosx

import Swift
import _Concurrency

public actor class A1 {
  var x: Int = 17
}

open actor class A3<T>: Actor {
  open func f() { }
}

// enqueue(partialTask:) has the same discriminator across all classes.
// CHECK: s4main2A1C7enqueue11partialTasky12_Concurrency012PartialAsyncE0V_tF.ptrauth{{.*}}i64 36669

// CHECK: @"$s4main2A3CyxG12_Concurrency5ActorAaeFP7enqueue11partialTaskyAE012PartialAsyncG0V_tFTW"
// CHECK-NOT: ret void
// CHECK: call i64 @llvm.ptrauth.blend.i64(i64 %{{[0-9]+}}, i64 36669)

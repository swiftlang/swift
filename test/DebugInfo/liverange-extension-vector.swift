// RUN: %target-swift-frontend %s -g -emit-ir -o - | %FileCheck %s
// REQUIRES: objc_interop, CPU=x86_64
import simd

func use<T>(_ x: T) {}

func getInt32() -> Int32 { return -1 }

public func rangeExtension(x: Int32, y: Int32) {
  let p = int2(x, y)
  // CHECK: define {{.*}}rangeExtension
  // CHECK: llvm.dbg.value(metadata <2 x i32> %[[P:.*]], metadata {{.*}}, metadata
  use(p)
  // CHECK: asm sideeffect "", "r"{{.*}}[[P]]
}

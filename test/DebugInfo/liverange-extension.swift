// RUN: %target-swift-frontend %s -g -emit-ir -o - | %FileCheck %s

func use<T>(_ x: T) {}

func getInt32() -> Int32 { return -1 }

public func rangeExtension(_ b: Bool) {
  // CHECK: define {{.*}}rangeExtension
  let i = getInt32()
  // CHECK: llvm.dbg.value(metadata i32 [[I:.*]], metadata {{.*}}, metadata
  use(i)
  if b {
    let j = getInt32()
    // CHECK: llvm.dbg.value(metadata i32 [[I]], metadata {{.*}}, metadata
    // CHECK: llvm.dbg.value(metadata i32 [[J:.*]], metadata {{.*}}, metadata
    use(j)
    // CHECK-DAG: {{(asm sideeffect "", "r".*)|(zext i32)}} [[J]]
    // CHECK-DAG: asm sideeffect "", "r"
  }
  let z = getInt32()
  use(z)
  // CHECK-NOT: llvm.dbg.value(metadata i32 [[J]], metadata {{.*}}, metadata
  // CHECK-DAG: llvm.dbg.value(metadata i32 [[I]], metadata {{.*}}, metadata
  // CHECK-DAG: llvm.dbg.value(metadata i32 [[Z:.*]], metadata {{.*}}, metadata
  // CHECK-DAG: {{(asm sideeffect "", "r".*)|(zext i32)}} [[I]]
  // CHECK-DAG: asm sideeffect "", "r"
}

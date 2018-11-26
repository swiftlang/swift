// RUN: %target-swift-frontend %s -g -emit-ir -o - | %FileCheck %s

// REQUIRES: CPU=x86_64
//
// We require x86_64 to make the test easier to read. Without this,
// writing check lines that ensure our asm gadgets match up with the
// right values is painfully hard to do.

func use<T>(_ x: T) {}

func getInt32() -> Int32 { return -1 }

// CHECK-LABEL: define {{.*}}rangeExtension
public func rangeExtension(_ b: Bool) {
  let i = getInt32()
  // CHECK: [[I:%.*]] = call swiftcc i32 @"{{.*}}getInt32
  // CHECK-NEXT: [[I_ZEXT:%.*]] = zext i32 [[I]] to i64
  // CHECK-NEXT: call void asm sideeffect "", "r"(i64 [[I_ZEXT]])
  // CHECK-NEXT: llvm.dbg.value(metadata i32 [[I]], metadata [[MD_I:!.*]], metadata

  use(i)

  // CHECK: br i1

  if b {
    // CHECK: llvm.dbg.value(metadata i32 [[I]], metadata [[MD_I]]

    let j = getInt32()
    // CHECK: [[J:%.*]] = call swiftcc i32 @"{{.*}}getInt32
    // CHECK-NEXT: [[J_ZEXT:%.*]] = zext i32 [[J]] to i64
    // CHECK-NEXT: call void asm sideeffect "", "r"(i64 [[J_ZEXT]])
    // CHECK: llvm.dbg.value(metadata i32 [[J]], metadata [[MD_J:!.*]], metadata

    use(j)
    // CHECK: call swiftcc void @"{{.*}}use

    // CHECK: [[I_ZEXT:%.*]] = zext i32 [[I]] to i64
    // CHECK-NEXT: call void asm sideeffect "", "r"(i64 [[I_ZEXT]])
    // CHECK-NEXT: [[J_ZEXT:%.*]] = zext i32 [[J]] to i64
    // CHECK-NEXT: call void asm sideeffect "", "r"(i64 [[J_ZEXT]])

    // CHECK: br label
  }

  // CHECK-NOT: llvm.dbg.value(metadata i32 [[J]]
  // CHECK: llvm.dbg.value(metadata i32 [[I]]

  let z = getInt32()
  // CHECK: [[Z:%.*]] = call swiftcc i32 @"{{.*}}getInt32
  // CHECK: llvm.dbg.value(metadata i32 [[Z]], metadata [[MD_Z:!.*]], metadata

  use(z)
  // CHECK: call swiftcc void @"{{.*}}use

  // CHECK: [[I_ZEXT:%.*]] = zext i32 [[I]] to i64
  // CHECK-NEXT: call void asm sideeffect "", "r"(i64 [[I_ZEXT]])
  // CHECK-NEXT: [[Z_ZEXT:%.*]] = zext i32 [[Z]] to i64
  // CHECK-NEXT: call void asm sideeffect "", "r"(i64 [[Z_ZEXT]])
}

// CHECK-DAG: [[MD_I]] = !DILocalVariable(name: "i"
// CHECK-DAG: [[MD_J]] = !DILocalVariable(name: "j"
// CHECK-DAG: [[MD_Z]] = !DILocalVariable(name: "z"

// RUN: %target-swift-frontend -enable-objc-interop -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64

class Tag {}

struct Scalar {
  var str = ""
  var x = Tag()
  var style: BinaryChoice  = .zero
  enum BinaryChoice: UInt32 {
    case zero = 0
    case one
  }
}

public struct Sequence {
  var tag: Tag = Tag()
  var tag2: Tag = Tag()
}

enum Node {
  case scalar(Scalar)
  case sequence(Sequence)
}

// CHECK: define internal i32 @"$s22multi_payload_shifting4NodeOwet"(ptr noalias %value, i32 %numEmptyCases, ptr %Node)
// CHECK:  [[ADDR:%.*]] = getelementptr inbounds{{.*}} { i64, i64, i64, i8 }, ptr {{.*}}, i32 0, i32 3
// CHECK:  [[BYTE:%.*]] = load i8, ptr [[ADDR]]
// Make sure we zext before we shift.
// CHECK:  [[ZEXT:%.*]] = zext i8 [[BYTE]] to i32
// CHECK:  shl i32 [[ZEXT]], 10

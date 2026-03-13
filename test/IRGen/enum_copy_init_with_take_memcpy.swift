// RUN: %target-swift-frontend -I %t -primary-file %s -O -emit-ir | %FileCheck %s

// REQUIRES: PTRSIZE=64

enum MyEnum {
  case one
  case four
}

struct HasAnEnum {
  var s1: String
  var s2: String
  var s3: String
  var s4: String
  var s5: String
  var s6: String
  var s7: String
  var s8: String
  var s9: String
  var s10: String
  var value: MyEnum?

  func readValue() -> Int {
    let x = value
    if case .four = x { return 4 }
    return -1
  }


// CHECK: define {{.*}} swiftcc range(i64 -1, 5) i64 @"$s31enum_copy_init_with_take_memcpy9HasAnEnumV9readValueSiyF"(ptr {{.*}} %0)
// CHECK:  [[T0:%.*]] = getelementptr inbounds{{.*}} i8, ptr %0, i64 160
// CHECK:  [[T1:%.*]] = load i8, ptr [[T0]]
// CHECK:  [[T2:%.*]] = and i8 [[T1]], -3
// CHECK:  [[T3:%.*]] = icmp eq i8 [[T2]], 0
// CHECK:  [[R:%.*]] = select i1 [[T3]], i64 -1, i64 4
// CHECK:  ret i64 [[R]]

}

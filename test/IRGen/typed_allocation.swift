// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -enable-experimental-feature TypedAllocation -target arm64-apple-macos99.99 -wmo | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_TypedAllocation

import Swift

// CHECK: define swiftcc ptr @"$e16typed_allocation7MyClassC1x1yACSi_SitcfC"(i64 [[P0:%.*]], i64 [[P1:%.*]], ptr swiftself [[PSELF:%.*]])
// CHECK:   call noalias ptr @swift_allocObjectTyped(ptr getelementptr inbounds (%swift.embedded_existential_type, ptr @"$e16typed_allocation7MyClassCMf", i32 0, i32 1), i64 {{.*}}, i64 {{.*}}, i64 {{.*}})
// CHECK:   ret ptr {{%.*}}
// CHECK: }
public class MyClass {
  let x: Int
  let y: Int

  init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }
}

// CHECK-LABEL: define swiftcc ptr @"$e16typed_allocation12MyOtherClassC1x1yACSi_AA0cE0CtcfC"(i64 %0, ptr %1, ptr swiftself %2)
// CHECK:   call noalias ptr @swift_allocObjectTyped(ptr getelementptr inbounds (%swift.embedded_existential_type, ptr @"$e16typed_allocation12MyOtherClassCMf", i32 0, i32 1), i64 32, i64 7, i64 {{.*}})
// CHECK:   ret ptr {{%.*}}
// CHECK: }
public class MyOtherClass {
  let x: Int
  let y: MyClass

  init(x: Int, y: MyClass) {
    self.x = x
    self.y = y
  }
}

// CHECK-LABEL: define swiftcc { ptr, ptr } @"$e16typed_allocation11makeClosure1x1yyycSi_AA7MyClassCtF"(i64 %0, ptr %1)
// CHECK:   call noalias ptr @swift_allocObjectTyped(ptr @metadata, i64 32, i64 7, i64 {{.*}})
// CHECK:   ret { ptr, ptr } {{%.*}}
// CHECK: }
@inline(never)
public func makeClosure(x: Int, y: MyClass) -> () -> Void {
  return {
    _ = (MyClass(x: x, y: y.x), MyOtherClass(x: x, y: y))
  }
}

public indirect enum Indirect {
  case x(Int, Int)
  case y(Int, Int)
}

// TODO: For some boxes we can't compute the typed malloc ID, because some of the fields are opaque,
//       so this test would not pass right now.

// DISABLED-CHECK-LABEL: define swiftcc i64 @"$e16typed_allocation8makeEnum1x1y1bAA8IndirectOSi_SiSbtF"(i64 %0, i64 %1, i1 %2) #0 {
// DISABLED-CHECK: entry:
// DISABLED-CHECK:   br i1 {{%.*}}, label %[[L1:.*]], label %[[L2:.*]]
// DISABLED-CHECK: [[L1]]:
// DISABLED-CHECK:   call noalias ptr @swift_allocObjectTyped(ptr getelementptr inbounds (%swift.full_boxmetadata, ptr @metadata{{.*}}, i32 0, i32 2), i64 {{.*}}, i64 7, i64 {{.*}})
// DISABLED-CHECK: [[L2]]:
// DISABLED-CHECK:   call noalias ptr @swift_allocObjectTyped(ptr getelementptr inbounds (%swift.full_boxmetadata, ptr @metadata{{.*}}, i32 0, i32 2), i64 {{.*}}, i64 7, i64 {{.*}})
// DISABLED-CHECK:   ret i64 {{%.*}}
// DISABLED-CHECK: }
public func makeEnum(x: Int, y: Int, b: Bool) -> Indirect {
  if b {
    return .x(y, x)
  }

  return .y(x, y)
}

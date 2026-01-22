// RUN: %target-swift-frontend -primary-file %s -emit-ir -target %module-target-future | %FileCheck %s

// REQUIRES: VENDOR=apple

// CHECK-LABEL: define hidden swiftcc ptr @"$s16typed_allocation7MyClassCACycfC"(ptr swiftself %0)
// CHECK:   call noalias ptr @swift_allocObjectTyped(ptr %0, i64 32, i64 7, i64 {{.*}})
// CHECK:   ret ptr {{%.*}}
// CHECK: }
public class MyClass {
  let x: Int = 42
  let y: Int = 23
}

// CHECK-LABEL: define hidden swiftcc ptr @"$s16typed_allocation12MyOtherClassC1x1yACSi_AA0cE0CtcfC"(i64 %0, ptr %1, ptr swiftself %2)
// CHECK:   call noalias ptr @swift_allocObjectTyped(ptr %2, i64 32, i64 7, i64 {{.*}})
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

// CHECK-LABEL: define swiftcc { ptr, ptr } @"$s16typed_allocation11makeClosure1x1yyycSi_AA7MyClassCtF"(i64 %0, ptr %1)
// CHECK:   call noalias ptr @swift_allocObjectTyped(ptr getelementptr inbounds (%swift.full_boxmetadata, ptr @metadata, i32 0, i32 2), i64 32, i64 7, i64 {{.*}})
// CHECK:   ret { ptr, ptr } {{%.*}}
// CHECK: }
@inline(never)
public func makeClosure(x: Int, y: MyClass) -> () -> Void {
  return {
    print("\(MyOtherClass(x: x, y: y))")
  }
}

public indirect enum Indirect {
  case x(Int, String)
  case y(String, String)
}

// CHECK-LABEL: define swiftcc i64 @"$s16typed_allocation8makeEnum1x1y1bAA8IndirectOSS_SiSbtF"(i64 %0, ptr %1, i64 %2, i1 %3) #0 {
// CHECK: entry:
// CHECK:   br i1 {{%.*}}, label %[[L1:.*]], label %[[L2:.*]]
// CHECK: [[L1]]:
// CHECK:   call noalias ptr @swift_allocObjectTyped(ptr getelementptr inbounds (%swift.full_boxmetadata, ptr @metadata.6, i32 0, i32 2), i64 40, i64 7, i64 {{.*}})
// CHECK: [[L2]]:
// CHECK:   call noalias ptr @swift_allocObjectTyped(ptr getelementptr inbounds (%swift.full_boxmetadata, ptr @metadata.3, i32 0, i32 2), i64 48, i64 7, i64 {{.*}})
// CHECK:   ret i64 {{%.*}}
// CHECK: }
public func makeEnum(x: String, y: Int, b: Bool) -> Indirect {
  if b {
    return .x(y, x)
  }

  return .y(x, x)
}

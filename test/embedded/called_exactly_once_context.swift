// RUN: %target-swift-emit-ir %s -parse-as-library -wmo -enable-experimental-feature Embedded -enable-experimental-feature CalledAttribute | %FileCheck %s

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_CalledAttribute

public struct Resource: ~Copyable {
  var x: Int
  deinit {}
}

public func make(_ resource: consuming Resource) -> @called(exactlyOnce) () -> Void {
  return { _ = consume resource }
}

// CHECK-NOT: swift_onceClosureDestroyedError
// CHECK-LABEL: define internal swiftcc void @__swift_exactly_once_closure_destructor(ptr swiftself %0)
// CHECK-NEXT: entry:
// CHECK-NEXT: call void @llvm.trap()
// CHECK-NEXT: unreachable
// CHECK-NOT: swift_onceClosureDestroyedError

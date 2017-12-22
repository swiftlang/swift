// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
class MyClass {}

// CHECK: define {{.*}} @_T013uninitialized1fyyF
public func f() {
  var object: MyClass
  // CHECK: %[[OBJ:.*]] = alloca %[[T:.*]]*, align
  // CHECK: call void @llvm.dbg.declare(metadata %[[T]]** %[[OBJ]],
  // CHECK: %[[BC:.*]] = bitcast %[[T]]** %[[OBJ]] to %swift.opaque**, !dbg
  // CHECK: store %swift.opaque* null, %swift.opaque** %[[BC]], align {{.*}}, !dbg
}

// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -O -emit-ir -g -o - | %FileCheck %s --check-prefix=OPT
class MyClass {}

// CHECK-LABEL: define {{.*}} @"$S13uninitialized1fyyF"
// OPT-LABEL: define {{.*}} @"$S13uninitialized1fyyF"
public func f() {
  var object: MyClass
  // CHECK: %[[OBJ:.*]] = alloca %[[T1:.*]]*, align
  // CHECK: call void @llvm.dbg.declare(metadata %[[T1]]** %[[OBJ]],
  // CHECK: %[[BC1:.*]] = bitcast %[[T1]]** %[[OBJ]] to %swift.opaque**, !dbg
  // CHECK: store %swift.opaque* null, %swift.opaque** %[[BC1]], align {{.*}}, !dbg
  // OPT-NOT: store
  // OPT: ret
}

// CHECK-LABEL: define {{.*}} @"$S13uninitialized1gyyF"
// OPT-LABEL: define {{.*}} @"$S13uninitialized1gyyF"
public func g() {
  var dict: Dictionary<Int64, Int64>
  // CHECK: %[[DICT:.*]] = alloca %[[T2:.*]], align
  // CHECK: call void @llvm.dbg.declare(metadata %[[T2]]* %[[DICT]],
  // CHECK: %[[BC2:.*]] = bitcast %[[T2]]* %[[DICT]] to %swift.opaque**, !dbg
  // CHECK: store %swift.opaque* null, %swift.opaque** %[[BC2]], align {{.*}}, !dbg
  // OPT-NOT: store
  // OPT: ret
}

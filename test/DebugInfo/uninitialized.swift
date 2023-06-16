// RUN: %target-swift-frontend %use_no_opaque_pointers %s -c -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -c -emit-ir -g -o -
// RUN: %target-swift-frontend %use_no_opaque_pointers %s -O -c -emit-ir -g -o - | %FileCheck %s --check-prefix=OPT
// RUN: %target-swift-frontend %s -O -c -emit-ir -g -o -
class MyClass {}

// CHECK-LABEL: define {{.*}} @"$s13uninitialized1fyyF"
// OPT-LABEL: define {{.*}} @"$s13uninitialized1fyyF"
public func f() {
  var object: MyClass
  // CHECK: %[[OBJ:.*]] = alloca %[[T1:.*]]*, align
  // CHECK: call void @llvm.dbg.declare(metadata %[[T1]]** %[[OBJ]],
  // CHECK: %[[BC1:.*]] = bitcast %[[T1]]** %[[OBJ]] to i8*{{$}}
  // CHECK: void @llvm.memset.{{.*}}(i8* align {{(4|8)}} %[[BC1]], i8 0,
  // CHECK-SAME:                    ){{$}}
  // OPT-NOT: @llvm.memset
  // OPT: ret
}

// CHECK-LABEL: define {{.*}} @"$s13uninitialized1gyyF"
// OPT-LABEL: define {{.*}} @"$s13uninitialized1gyyF"
public func g() {
  var dict: Dictionary<Int64, Int64>
  // CHECK: %[[DICT:.*]] = alloca %[[T2:.*]], align
  // CHECK: call void @llvm.dbg.declare(metadata %[[T2]]* %[[DICT]],
  // CHECK: %[[BC2:.*]] = bitcast %[[T2]]* %[[DICT]] to i8*
  // CHECK: void @llvm.memset.{{.*}}(i8* align {{(4|8)}} %[[BC2]], i8 0,
  // CHECK-SAME:                    ){{$}}
  // OPT-NOT: @llvm.memset
  // OPT: ret
}

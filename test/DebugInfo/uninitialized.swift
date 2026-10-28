// RUN: %target-swift-frontend %s -c -emit-ir -g -swift-version 5 -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -O -c -emit-ir -g -swift-version 5 -o - | %FileCheck %s --check-prefix=OPT
class MyClass {}

// CHECK-LABEL: define {{.*}} @"$s13uninitialized1fyyF"
// OPT-LABEL: define {{.*}} @"$s13uninitialized1fyyF"
public func f() {
  var object: MyClass
  // CHECK: %[[OBJ:.*]] = alloca ptr, align
  // CHECK: #dbg_declare(ptr %[[OBJ]],
  // CHECK: void @llvm.memset.{{.*}}(ptr align {{(4|8)}} %[[OBJ]], i8 0,
  // CHECK-SAME:                    ), !Swift.isSwiftLLDBpreinit
  // OPT-NOT: @llvm.memset
  // OPT: ret
}

// CHECK-LABEL: define {{.*}} @"$s13uninitialized1gyyF"
// OPT-LABEL: define {{.*}} @"$s13uninitialized1gyyF"
public func g() {
  var dict: Dictionary<Int64, Int64>
  // CHECK: %[[DICT:.*]] = alloca
  // CHECK: #dbg_declare(ptr %[[DICT]],
  // CHECK: void @llvm.memset.{{.*}}(ptr align {{(4|8)}} %[[DICT]], i8 0,
  // CHECK-SAME:                    ), !Swift.isSwiftLLDBpreinit
  // OPT-NOT: @llvm.memset
  // OPT: ret
}

// In Swift 5 mode, 'self' in a convenience initializer of a non-final class
// has dynamic Self type.
public class Base {
  public init() {}

  // CHECK-LABEL: define {{.*}} @"$s13uninitialized4BaseC1xACSi_tcfC"
  // OPT-LABEL: define {{.*}} @"$s13uninitialized4BaseC1xACSi_tcfC"
  public convenience init(x: Int) {
    // CHECK: %[[SELF:.*]] = alloca ptr, align
    // CHECK: #dbg_declare(ptr %[[SELF]],
    // CHECK: void @llvm.memset.{{.*}}(ptr align {{(4|8)}} %[[SELF]], i8 0,
    // CHECK-SAME:                    ), !Swift.isSwiftLLDBpreinit
    // OPT-NOT: @llvm.memset
    // OPT: ret
    self.init()
  }
}

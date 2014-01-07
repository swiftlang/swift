// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs -I=%S/Inputs %s -emit-llvm | FileCheck %s

import gizmo

extension Gizmo {
  // CHECK-LABEL: define %CSo5Gizmo* @_TFCSo5GizmocfMS_FT7withIntSi_S_(i64, %CSo5Gizmo*)
  init withInt(i: Int) {
    // CHECK: [[SELF:%[0-9a-zA-Z_]+]] = alloca %CSo5Gizmo*, align 8
    // CHECK: [[I:%[0-9a-zA-Z_]+]] = alloca %Si, align 8
    // CHECK: [[OBJC_SUPER:%[0-9a-zA-Z_]+]] = alloca %objc_super, align 8
    // CHECK: [[I]].value = getelementptr inbounds %Si* %i, i32 0, i32 0
    // CHECK: store i64 %0, i64* [[I]].value, align 8
    // CHECK: store %CSo5Gizmo* %1, %CSo5Gizmo** [[SELF]], align 8
    // CHECK: [[SELF_RET:%[0-9]+]] = call %CSo5Gizmo* bitcast (%objc_object* (%objc_object*)* @objc_retain to %CSo5Gizmo* (%CSo5Gizmo*)*)(%CSo5Gizmo* %1) #0
    // CHECK: [[SELF_OBJ:%[0-9]+]] = bitcast %CSo5Gizmo* %1 to %objc_object*
    // CHECK: [[SUPER_RECEIVER:%[0-9]+]] = getelementptr %objc_super* %objc_super, i32 0, i32 0
    // CHECK: store %objc_object* [[SELF_OBJ]], %objc_object** [[SUPER_RECEIVER]], align 8
    // CHECK: [[SUPER_CLASS:%[0-9]+]] = getelementptr %objc_super* %objc_super, i32 0, i32 1
    // CHECK: store %objc_class* bitcast (%swift.type* @"OBJC_CLASS_$_Gizmo" to %objc_class*), %objc_class** [[SUPER_CLASS]], align 8
    // CHECK:   [[SELECTOR:%[0-9]+]] = load i8** @"\01L_selector(initWithBellsOn:)", align 8
    // CHECK:   [[DELEG_SELF:%[0-9]+]] = call %CSo5Gizmo* bitcast (void ()* @objc_msgSendSuper to %CSo5Gizmo* (%objc_super*, i8*, i64)*)(%objc_super* %objc_super, i8* [[SELECTOR]], i64 %0)
    // CHECK:   store %CSo5Gizmo* [[DELEG_SELF]], %CSo5Gizmo** %self, align 8
    // CHECK:   call void bitcast (void (%objc_object*)* @objc_release to void (%CSo5Gizmo*)*)(%CSo5Gizmo* %1) #0
    // CHECK:   ret %CSo5Gizmo* [[DELEG_SELF]]
    self.init(withBellsOn:i)
  }
}

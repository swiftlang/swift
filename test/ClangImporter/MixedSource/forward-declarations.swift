// RUN: %target-swift-frontend -emit-ir -primary-file %s %S/Inputs/forward-declarations-other.swift -import-objc-header %S/Inputs/forward-declarations.h -enable-objc-interop -disable-objc-attr-requires-foundation-module -module-name main | %FileCheck %s

class Sub: Base {
  // CHECK-LABEL: define{{.*}} void @"$s4main3SubC4testyyF"
  func test() {
    // CHECK: [[BASE_SELF:%.+]] = bitcast %T4main3SubC* %0 to %TSo4BaseC*
    // CHECK: [[SELECTOR:%.+]] = load i8*, i8** @"\01L_selector(getClassInstanceWithoutMentioningItsName)"
    // CHECK: [[OPAQUE_SELF:%.+]] = bitcast %TSo4BaseC* %2 to {{%.+}}*
    // CHECK: [[RESULT:%.+]] = call {{%.+}}* bitcast (void ()* @objc_msgSend to {{%.+}}* ({{%.+}}*, i8*)*)({{%.+}}* [[OPAQUE_SELF]], i8* [[SELECTOR]])
    // CHECK: [[OPAQUE_RESULT:%.+]] = bitcast {{%.+}}* [[RESULT]] to i8*
    // CHECK: call i8* @objc_retainAutoreleasedReturnValue(i8* [[OPAQUE_RESULT]])
    _ = self.getClassInstanceWithoutMentioningItsName()
    // CHECK: call void @swift_release(%swift.refcounted* {{%.+}})
    // CHECK: ret void
  }
}
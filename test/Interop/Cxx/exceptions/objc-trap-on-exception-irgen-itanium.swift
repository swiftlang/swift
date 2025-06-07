// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-emit-ir -target %target-future-triple -min-runtime-version 6.0 %t/test.swift -I %t/Inputs -enable-experimental-cxx-interop | %FileCheck %s
// RUN: %target-swift-emit-ir -target %target-triple -min-runtime-version 5.9 %t/test.swift -I %t/Inputs -enable-experimental-cxx-interop | %FileCheck %s --check-prefix=GXX

// REQUIRES: objc_interop
// UNSUPPORTED: OS=windows-msvc

//--- Inputs/module.modulemap
module ObjCXXModule {
    header "objCXXHeader.h"
}

//--- Inputs/objCXXHeader.h

@interface ObjCInterface

- (void)method;

@end

ObjCInterface * _Nonnull retObjCInterface() noexcept;

//--- test.swift

import ObjCXXModule

func testObjCMethodCall() {
  let obj = retObjCInterface();
  obj.method();
}

testObjCMethodCall()

// CHECK: define {{.*}} @"$s4test0A14ObjCMethodCallyyF"() #[[#UWATTR:]] personality ptr @_swift_exceptionPersonality
// CHECK: invoke void {{.*}}@objc_msgSend
// CHECK-NEXT: to label %[[CONT1:.*]] unwind label %[[UNWIND1:.*]]
// CHECK-EMPTY:
// CHECK-NEXT: [[CONT1]]:
// CHECK:  ret
// CHECK-EMPTY:
// CHECK-NEXT: [[UNWIND1]]:
// CHECK-NEXT: landingpad { ptr, i32 }
// CHECK-NEXT:    catch ptr null
// CHECK-NEXT: call void @llvm.trap()
// CHECK-NEXT: unreachable
// CHECK-NEXT: }

// GXX: define {{.*}} @"$s4test0A14ObjCMethodCallyyF"() #[[#UWATTR:]] personality ptr @__gxx_personality_v0
// GXX: invoke void {{.*}}@objc_msgSend
// GXX-NEXT: to label %[[CONT1:.*]] unwind label %[[UNWIND1:.*]]
// GXX-EMPTY:
// GXX-NEXT: [[CONT1]]:
// GXX:  ret
// GXX-EMPTY:
// GXX-NEXT: [[UNWIND1]]:
// GXX-NEXT: landingpad { ptr, i32 }
// GXX-NEXT:    catch ptr null
// GXX-NEXT: call void @llvm.trap()
// GXX-NEXT: unreachable
// GXX-NEXT: }

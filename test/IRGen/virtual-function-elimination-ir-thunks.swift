// Tests that under -enable-llvm-vfe, virtual function calls to classes defined
// by other modules are using thunk calls (instead of direct vtable loads).

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-objc-interop -Xfrontend -enable-llvm-vfe -parse-as-library %s -DLIBRARY -module-name Library -emit-module -o %t/Library.swiftmodule
// RUN: %target-build-swift %use_no_opaque_pointers -Xfrontend -disable-objc-interop -Xfrontend -enable-llvm-vfe -parse-as-library %s -DCLIENT -module-name Main -I%t -emit-ir -o - | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -disable-objc-interop -Xfrontend -enable-llvm-vfe -parse-as-library %s -DCLIENT -module-name Main -I%t -emit-ir -o -

// UNSUPPORTED: OS=windows-msvc

#if LIBRARY

public class MyLibraryClass {
  public func foo() { }
}

#endif

#if CLIENT

import Library

public class MyLocalClass {
  public func bar() { }
}

func func1(o: MyLocalClass) {
  // CHECK: define hidden swiftcc void @"$s4Main5func11oyAA12MyLocalClassC_tF"(%T4Main12MyLocalClassC* %0)
  o.bar()
  // CHECK:  [[SLOT:%.*]] = getelementptr inbounds void (%T4Main12MyLocalClassC*)*, void (%T4Main12MyLocalClassC*)** {{.*}}, {{i64|i32}} {{.*}}
  // CHECK:  [[SLOTASPTR:%.*]] = bitcast void (%T4Main12MyLocalClassC*)** [[SLOT]] to i8*
  // CHECK:  call { i8*, i1 } @llvm.type.checked.load(i8* [[SLOTASPTR]], i32 0, metadata !"$s4Main12MyLocalClassC3baryyFTq")

  // CHECK: ret void
}

func func2(o: MyLibraryClass) {
  // CHECK: define hidden swiftcc void @"$s4Main5func21oy7Library02MyC5ClassC_tF"(%T7Library02MyA5ClassC* %0)
  o.foo()
  // CHECK: call swiftcc void @"$s7Library02MyA5ClassC3fooyyFTj"

  // CHECK-NOT: @llvm.type.checked.load
  // CHECK: ret void
}

#endif

// RUN: %target-swift-frontend -module-name cxx_ir -I %S/Inputs/custom-modules -module-cache-path %t -enable-cxx-interop -emit-ir -o - -primary-file %s | %FileCheck %s

import CXXInterop

// CHECK-LABEL: define hidden swiftcc void @"$s6cxx_ir13indirectUsageyyF"()
// CHECK: %0 = call %"class.ns::T"* @{{_Z5makeTv|"\?makeT@@YAPE?AVT@ns@@XZ"}}()
// CHECK: call void @{{_Z4useTPN2ns1TE|"\?useT@@YAXPE?AVT@ns@@@Z"}}(%"class.ns::T"* %2)
func indirectUsage() {
  useT(makeT())
}

// CHECK: define hidden swiftcc void @"$s6cxx_ir24namespaceManglesIntoName3argySo2nsV1TV_tF"
func namespaceManglesIntoName(arg: namespacedT) {
}

// CHECK-LABEL: define hidden swiftcc i32 @"$s6cxx_ir12basicMethods1as5Int32VSpySo0D0VG_tF"(i8*)
// CHECK: [[THIS_PTR1:%.*]] = bitcast i8* %0 to %TSo7MethodsV*
// CHECK: [[THIS_PTR2:%.*]] = bitcast %TSo7MethodsV* [[THIS_PTR1]] to %class.Methods*
// CHECK: [[RESULT:%.*]] = call i32 @_ZN7Methods12SimpleMethodEi(%class.Methods* [[THIS_PTR2]], i32 4)
// CHECK: ret i32 [[RESULT]]
func basicMethods(a: UnsafeMutablePointer<Methods>) -> Int32 {
  return a.pointee.SimpleMethod(4)
}

// CHECK-LABEL: define hidden swiftcc i32 @"$s6cxx_ir17basicMethodsConst1as5Int32VSpySo0D0VG_tF"(i8*)
// CHECK: [[THIS_PTR1:%.*]] = bitcast i8* %0 to %TSo7MethodsV*
// CHECK: [[THIS_PTR2:%.*]] = bitcast %TSo7MethodsV* [[THIS_PTR1]] to %class.Methods*
// CHECK: [[RESULT:%.*]] = call i32 @_ZNK7Methods17SimpleConstMethodEi(%class.Methods* [[THIS_PTR2]], i32 3)
// CHECK: ret i32 [[RESULT]]
func basicMethodsConst(a: UnsafeMutablePointer<Methods>) -> Int32 {
  return a.pointee.SimpleConstMethod(3)
}

// CHECK-LABEL: define hidden swiftcc i32 @"$s6cxx_ir18basicMethodsStatics5Int32VyF"()
// CHECK: [[RESULT:%.*]] = call i32 @_ZN7Methods18SimpleStaticMethodEi(i32 5)
// CHECK: ret i32 [[RESULT]]
func basicMethodsStatic() -> Int32 {
  return Methods.SimpleStaticMethod(5)
}

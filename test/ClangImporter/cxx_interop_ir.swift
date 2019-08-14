// RUN: %target-swift-frontend -module-name cxx_ir -I %S/Inputs/custom-modules -module-cache-path %t -enable-cxx-interop -emit-ir -o - -primary-file %s | %FileCheck %s

import CXXInterop

// CHECK-LABEL: define hidden swiftcc void @"$s6cxx_ir13indirectUsageyyF"()
// CHECK: %0 = call %"class.ns::T"* @{{_Z5makeTv|"\?makeT@@YAPE?AVT@ns@@XZ"}}()
// CHECK: call void @{{_Z4useTPN2ns1TE|"\?useT@@YAXPE?AVT@ns@@@Z"}}(%"class.ns::T"* %2)
func indirectUsage() {
  useT(makeT())
}

// CHECK-LABEL: define hidden swiftcc %swift.type* @"$s6cxx_ir14reflectionInfo3argypXpSo2nsV1TV_tF"
// CHECK: %0 = call swiftcc %swift.metadata_response @"$sSo2nsV1TVMa"({{i64|i32}} 0)
func reflectionInfo(arg: namespacedT) -> Any.Type {
  return type(of: arg)
}

// CHECK: define hidden swiftcc void @"$s6cxx_ir24namespaceManglesIntoName3argySo2nsV1TV_tF"
func namespaceManglesIntoName(arg: namespacedT) {
}

// CHECK: define hidden swiftcc void @"$s6cxx_ir42namespaceManglesIntoNameForUsingShadowDecl3argySo2nsV14NamespacedTypeV_tF"
func namespaceManglesIntoNameForUsingShadowDecl(arg: NamespacedType) {
}

// CHECK-LABEL: define hidden swiftcc void @"$s6cxx_ir14accessNSMemberyyF"()
// CHECK: %0 = call %"class.ns::T"* @{{_ZN2ns7doMakeTEv|"\?doMakeT@ns@@YAPEAVT@1@XZ"}}()
// CHECK: call void @{{_Z4useTPN2ns1TE|"\?useT@@YAXPE?AVT@ns@@@Z"}}(%"class.ns::T"* %2)
func accessNSMember() {
  useT(ns.doMakeT())
}

// CHECK-LABEL: define hidden swiftcc i32 @"$s6cxx_ir12basicMethods1as5Int32VSpySo0D0VG_tF"(i8*)
// CHECK: [[THIS_PTR1:%.*]] = bitcast i8* %0 to %TSo7MethodsV*
// CHECK: [[THIS_PTR2:%.*]] = bitcast %TSo7MethodsV* [[THIS_PTR1]] to %class.Methods*
// CHECK: [[RESULT:%.*]] = call {{(signext )?}}i32 @{{_ZN7Methods12SimpleMethodEi|"\?SimpleMethod@Methods@@QEAAHH@Z"}}(%class.Methods* [[THIS_PTR2]], i32{{( signext)?}} 4)
// CHECK: ret i32 [[RESULT]]
func basicMethods(a: UnsafeMutablePointer<Methods>) -> Int32 {
  return a.pointee.SimpleMethod(4)
}

// CHECK-LABEL: define hidden swiftcc i32 @"$s6cxx_ir17basicMethodsConst1as5Int32VSpySo0D0VG_tF"(i8*)
// CHECK: [[THIS_PTR1:%.*]] = bitcast i8* %0 to %TSo7MethodsV*
// CHECK: [[THIS_PTR2:%.*]] = bitcast %TSo7MethodsV* [[THIS_PTR1]] to %class.Methods*
// CHECK: [[RESULT:%.*]] = call {{(signext )?}}i32 @{{_ZNK7Methods17SimpleConstMethodEi|"\?SimpleConstMethod@Methods@@QEBAHH@Z"}}(%class.Methods* [[THIS_PTR2]], i32{{( signext)?}} 3)
// CHECK: ret i32 [[RESULT]]
func basicMethodsConst(a: UnsafeMutablePointer<Methods>) -> Int32 {
  return a.pointee.SimpleConstMethod(3)
}

// CHECK-LABEL: define hidden swiftcc i32 @"$s6cxx_ir18basicMethodsStatics5Int32VyF"()
// CHECK: [[RESULT:%.*]] = call {{(signext )?}}i32 @{{_ZN7Methods18SimpleStaticMethodEi|"\?SimpleStaticMethod@Methods@@SAHH@Z"}}(i32{{( signext)?}} 5)
// CHECK: ret i32 [[RESULT]]
func basicMethodsStatic() -> Int32 {
  return Methods.SimpleStaticMethod(5)
}

// CHECK-LABEL: define hidden swiftcc i32 @"$s6cxx_ir12basicMethods1as5Int32VSpySo8Methods2VG_tF"(i8*)
// CHECK: [[THIS_PTR1:%.*]] = bitcast i8* %0 to %TSo8Methods2V*
// CHECK: [[THIS_PTR2:%.*]] = bitcast %TSo8Methods2V* [[THIS_PTR1]] to %class.Methods2*
// CHECK: [[RESULT:%.*]] = call {{(signext )?}}i32 @{{_ZN8Methods212SimpleMethodEi|"\?SimpleMethod@Methods2@@QEAAHH@Z"}}(%class.Methods2* [[THIS_PTR2]], i32{{( signext)?}} 4)
// CHECK: ret i32 [[RESULT]]
func basicMethods(a: UnsafeMutablePointer<Methods2>) -> Int32 {
  return a.pointee.SimpleMethod(4)
}

// CHECK-LABEL: define hidden swiftcc i32 @"$s6cxx_ir13virtualMethod1as5Int32VSpySo14MethodsVirtualVG_tF"(i8*) #0 {
// CHECK: [[THIS_PTR1:%.*]] = bitcast i8* %0 to %TSo14MethodsVirtualV*
// CHECK: [[THIS_PTR2:%.*]] = bitcast %TSo14MethodsVirtualV* [[THIS_PTR1]] to i32 (%class.MethodsVirtual*, i32)***
// CHECK: [[VTABLE:%.*]] = load i32 (%class.MethodsVirtual*, i32)**, i32 (%class.MethodsVirtual*, i32)*** [[THIS_PTR2]], align 8
// CHECK: [[VFN_PTR:%.*]] = getelementptr inbounds i32 (%class.MethodsVirtual*, i32)*, i32 (%class.MethodsVirtual*, i32)** [[VTABLE]], i64 0
// CHECK: [[VFN:%.*]] = load i32 (%class.MethodsVirtual*, i32)*, i32 (%class.MethodsVirtual*, i32)** [[VFN_PTR]], align 8
// CHECK: [[THIS_PTR3:%.*]] = bitcast %TSo14MethodsVirtualV* [[THIS_PTR1]] to %class.MethodsVirtual*
// CHECK: [[RESULT:%.*]] = call i32{{( signext)?}} [[VFN]](%class.MethodsVirtual* [[THIS_PTR3]], i32{{( signext)?}} 7)
// CHECK: ret i32 [[RESULT]]
func virtualMethod(a: UnsafeMutablePointer<MethodsVirtual>) -> Int32 {
  return a.pointee.SimpleVirtualMethod(7);
}

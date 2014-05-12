// RUN: rm -rf %t/clang-module-cache
// RUN: %target-build-swift -Xfrontend %clang-importer-sdk -module-cache-path %t/clang-module-cache -I=%S/Inputs/abi %s -emit-ir | FileCheck -check-prefix=%target-cpu-%target-os %s

import gadget
import Foundation

@class_protocol @objc protocol P1 {}
@class_protocol @objc protocol P2 {}
@class_protocol @objc protocol Work {
  func doStuff(x: Int64)
}

// arm64-ios: [[ARM64MYRECT:%.*]] = type { float, float, float, float }

class Foo {
  // x86_64-macosx: define void @_TFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, %C8abitypes3Foo*) {
  // x86_64-macosx: define internal { <2 x float>, <2 x float> } @_TToFC8abitypes3Foo3barfS0_FT_VSC6MyRect(i8*, i8*) unnamed_addr {
  // x86_64-ios: define void @_TFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, %C8abitypes3Foo*) {
  // x86_64-ios: define internal { <2 x float>, <2 x float> } @_TToFC8abitypes3Foo3barfS0_FT_VSC6MyRect(i8*, i8*) unnamed_addr {
  // i386-ios: define void @_TFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, %C8abitypes3Foo*) {
  // i386-ios: define internal void @_TToFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, i8*, i8*) unnamed_addr {
  // armv7-ios: define void @_TFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, %C8abitypes3Foo*) {
  // armv7-ios: define internal void @_TToFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, i8*, i8*) unnamed_addr {
  // arm64-ios: define { float, float, float, float } @_TFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%C8abitypes3Foo*) {
  // arm64-ios: define internal [[ARM64MYRECT]] @_TToFC8abitypes3Foo3barfS0_FT_VSC6MyRect(i8*, i8*) unnamed_addr {
  @objc func bar() -> MyRect {
    return MyRect(x: 1, y: 2, width: 3, height: 4)
  }


  // x86_64-macosx: define double @_TFC8abitypes3Foo14getXFromNSRect{{.*}}(double, double, double, double, %C8abitypes3Foo*) {
  // x86_64-macosx: define internal double @_TToFC8abitypes3Foo14getXFromNSRect{{.*}}(i8*, i8*, %VSC6CGRect* byval align 8) unnamed_addr {
  // armv7-ios: define double @_TFC8abitypes3Foo14getXFromNSRect{{.*}}(float, float, float, float, %C8abitypes3Foo*) {
  // armv7-ios: define internal double @_TToFC8abitypes3Foo14getXFromNSRect{{.*}}(i8*, i8*, { [4 x i32] }) unnamed_addr {
  @objc func getXFromNSRect(r: NSRect) -> Double {
    // FIXME: return Double(r.origin.x) fails when x is already Double <rdar://16219891>
#if arch(arm) || arch(i386)
    return Double(r.origin.x)
#else
    return r.origin.x
#endif
  }

  // x86_64-macosx: define float @_TFC8abitypes3Foo12getXFromRect{{.*}}(float, float, float, float, %C8abitypes3Foo*) {
  // x86_64-macosx: define internal float @_TToFC8abitypes3Foo12getXFromRect{{.*}}(i8*, i8*, { <2 x float>, <2 x float> }) unnamed_addr {
  // armv7-ios: define float @_TFC8abitypes3Foo12getXFromRect{{.*}}(float, float, float, float, %C8abitypes3Foo*) {
  // armv7-ios: define internal float @_TToFC8abitypes3Foo12getXFromRect{{.*}}(i8*, i8*, { [4 x i32] }) unnamed_addr {
  @objc func getXFromRect(r: MyRect) -> Float {
    return r.x
  }

  // Call from Swift entrypoint with exploded Rect to @objc entrypoint
  // with unexploaded ABI-coerced type.
  // x86_64-macosx: define float @_TFC8abitypes3Foo17getXFromRectSwift{{.*}}(float, float, float, float, [[SELF:%.*]]*) {
  // x86_64-macosx: [[COERCED:%.*]] = alloca [[MYRECT:%.*]], align 4
  // x86_64-macosx: [[SEL:%.*]] = load i8** @"\01L_selector(getXFromRect:)", align 8
  // x86_64-macosx: [[CAST:%.*]] = bitcast [[MYRECT]]* [[COERCED]] to { <2 x float>, <2 x float> }*
  // x86_64-macosx: [[LOADED:%.*]] = load { <2 x float>, <2 x float> }* [[CAST]]
  // x86_64-macosx: [[SELFCAST:%.*]] = bitcast [[SELF]]* %4 to i8*
  // x86_64-macosx: [[RESULT:%.*]] = call float bitcast (void ()* @objc_msgSend to float (i8*, i8*, { <2 x float>, <2 x float> })*)(i8* [[SELFCAST]], i8* [[SEL]], { <2 x float>, <2 x float> } [[LOADED]])
  // armv7-ios: define float @_TFC8abitypes3Foo17getXFromRectSwift{{.*}}(float, float, float, float, [[SELF:%.*]]*) {
  // armv7-ios: [[COERCED:%.*]] = alloca [[MYRECT:%.*]], align 4
  // armv7-ios: [[SEL:%.*]] = load i8** @"\01L_selector(getXFromRect:)", align 4
  // armv7-ios: [[CAST:%.*]] = bitcast [[MYRECT]]* [[COERCED]] to { [4 x i32] }*
  // armv7-ios: [[LOADED:%.*]] = load { [4 x i32] }* [[CAST]]
  // armv7-ios: [[SELFCAST:%.*]] = bitcast [[SELF]]* %4 to i8*
  // armv7-ios: [[RESULT:%.*]] = call float bitcast (void ()* @objc_msgSend to float (i8*, i8*, { [4 x i32] })*)(i8* [[SELFCAST]], i8* [[SEL]], { [4 x i32] } [[LOADED]])
  func getXFromRectSwift(r: MyRect) -> Float {
    return getXFromRect(r)
  }

  // Ensure that MyRect is passed as an indirect-byval on x86-64 because we run out of registers for direct arguments
  // x86_64-macosx: define internal float @_TToFC8abitypes3Foo25getXFromRectIndirectByVal{{.*}}(i8*, i8*, float, float, float, float, float, float, float, %VSC6MyRect* byval align 4) unnamed_addr {
  @objc func getXFromRectIndirectByVal(_: Float, second _: Float, 
                                       third _: Float, fourth _: Float,
                                       fifth _: Float, sixth _: Float,
                                       seventh _: Float, withRect r: MyRect)
               -> Float {
    return r.x
  }

  // Make sure the caller-side from Swift also uses indirect-byval for the argument
  // x86_64-macosx: define float @_TFC8abitypes3Foo25getXFromRectIndirectSwift{{.*}}(float, float, float, float, %C8abitypes3Foo*) {
  func getXFromRectIndirectSwift(r: MyRect) -> Float {
    let f : Float = 1.0;
    // x86_64-macosx: [[TEMP:%.*]] = alloca [[TEMPTYPE:%.*]], align 4
    // x86_64-macosx: [[RESULT:%.*]] = call float bitcast (void ()* @objc_msgSend to float (i8*, i8*, float, float, float, float, float, float, float, [[TEMPTYPE]]*)*)(i8* %7, i8* %6, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, [[TEMPTYPE]]* byval align 4 [[TEMP]])
    // x86_64-macosx: ret float [[RESULT]]
    return getXFromRectIndirectByVal(f, second: f, third: f, fourth: f, fifth: f, sixth: f, seventh: f, withRect: r);
  }

  // x86-64 returns an HA of four floats directly in two <2 x float>
  // x86_64-macosx:      define float @_TFC8abitypes3Foo4barc{{.*}}(%CSo13StructReturns*, %C8abitypes3Foo*) {
  // x86_64-macosx:      load i8** @"\01L_selector(newRect)", align 8
  // x86_64-macosx:      [[RESULT:%.*]] = call { <2 x float>, <2 x float> } bitcast (void ()* @objc_msgSend
  // x86_64-macosx:      store { <2 x float>, <2 x float> } [[RESULT]]
  // x86_64-macosx:      [[CAST:%.*]] = bitcast { <2 x float>, <2 x float> }*
  // x86_64-macosx:      load { float, float, float, float }* [[CAST]]
  // x86_64-macosx:      ret float
  // armv7 returns an HA of four floats indirectly
  // armv7-ios: define float @_TFC8abitypes3Foo4barc{{.*}}(%CSo13StructReturns*, %C8abitypes3Foo*) {
  // armv7-ios: [[RESULT:%.*]] = alloca [[RECTTYPE:%.*]], align 4
  // armv7-ios: load i8** @"\01L_selector(newRect)", align 4
  // armv7-ios: call void bitcast (void ()* @objc_msgSend_stret to void ([[RECTTYPE]]*, [[RECEIVER:.*]]*, i8*)*)([[RECTTYPE]]* noalias sret %call.aggresult
  // armv7-ios: [[GEP1:%.*]] = getelementptr inbounds [[RECTTYPE]]* [[RESULT]], i32 0, i32 1
  // armv7-ios: [[GEP2:%.*]] = getelementptr inbounds {{.*}}* [[GEP1]], i32 0, i32 0
  // armv7-ios: [[RETVAL:%.*]] = load float* [[GEP2]], align 4
  // armv7-ios: ret float [[RETVAL]]
  func barc(p: StructReturns) -> Float {
    return p.newRect().y
  }

  // x86_64-macosx: define { double, double, double } @_TFC8abitypes3Foo3baz{{.*}}(%C8abitypes3Foo*) {
  // x86_64-macosx: define internal void @_TToFC8abitypes3Foo3baz{{.*}}(%VSC4Trio* noalias sret, i8*, i8*) unnamed_addr {
  @objc func baz() -> Trio {
    return Trio(i: 1.0, j: 2.0, k: 3.0)
  }

  // x86_64-macosx:      define double @_TFC8abitypes3Foo4bazc{{.*}}(%CSo13StructReturns*, %C8abitypes3Foo*) {
  // x86_64-macosx:      load i8** @"\01L_selector(newTrio)", align 8
  // x86_64-macosx:      [[CAST:%[0-9]+]] = bitcast {{%.*}}* %0
  // x86_64-macosx:      call void bitcast (void ()* @objc_msgSend_stret to void (%VSC4Trio*, [[OPAQUE:.*]]*, i8*)*)
  func bazc(p: StructReturns) -> Double {
    return p.newTrio().j
  }

  // x86_64-macosx:      define { i32, i32 } @_TFC8abitypes3Foo7getpair{{.*}}(%CSo13StructReturns*, %C8abitypes3Foo*) {
  // x86_64-macosx:      [[RESULT:%.*]] = call i64 bitcast (void ()* @objc_msgSend to i64 ([[OPAQUE:.*]]*, i8*)*)
  // x86_64-macosx:      store i64 [[RESULT]]
  // x86_64-macosx:      [[CAST:%.*]] = bitcast i64* {{%.*}} to { i32, i32 }*
  // x86_64-macosx:      load { i32, i32 }* [[CAST]]
  // x86_64-macosx:      ret { i32, i32 }
  func getpair(p: StructReturns) -> IntPair {
    return p.newPair()
  }

  // x86_64-macosx:      define internal i64 @_TToFC8abitypes3Foo8takepair{{.*}}(i8*, i8*, i64) unnamed_addr {
  @objc func takepair(p: IntPair) -> IntPair {
    return p
  }

  // x86_64-macosx:      define { i32, i32 } @_TFC8abitypes3Foo9getnested{{.*}}(%CSo13StructReturns*, %C8abitypes3Foo*) {
  // x86_64-macosx:      call i64 bitcast (void ()* @objc_msgSend to i64 ([[OPAQUE:.*]]*, i8*)*)
  // x86_64-macosx-NEXT: store i64
  // x86_64-macosx-NEXT: bitcast i64* {{[^ ]*}} to { i32, i32 }*
  // x86_64-macosx-NEXT: load { i32, i32 }*
  // x86_64-macosx:      ret { i32, i32 }
  func getnested(p: StructReturns) -> NestedInts {
    return p.newNestedInts()
  }

  // x86_64-macosx:      define internal i8* @_TToFC8abitypes3Foo9copyClass{{.*}}(i8*, i8*, i8*) unnamed_addr {
  // x86_64-macosx:      [[VALUE:%[0-9]+]] = call [[TYPE:%.*]]* @_TFC8abitypes3Foo9copyClass
  // x86_64-macosx:      [[T0:%.*]] = phi [[TYPE]]* [ [[VALUE]],
  // x86_64-macosx:      [[T1:%.*]] = bitcast [[TYPE]]* [[T0]] to [[OBJC:%objc_class]]*
  // x86_64-macosx:      [[RESULT:%[0-9]+]] = bitcast [[OBJC]]* [[T1]] to i8*
  // x86_64-macosx:      ret i8* [[RESULT]]
  @objc func copyClass(a: AnyClass) -> AnyClass {
    return a
  }

  // x86_64-macosx:      define internal i8* @_TToFC8abitypes3Foo9copyProto{{.*}}(i8*, i8*, i8*) unnamed_addr {
  // x86_64-macosx:      [[VALUE:%[0-9]+]] = call [[TYPE:%.*]] @_TFC8abitypes3Foo9copyProt
  // x86_64-macosx:      [[RESULT:%[0-9]+]] = bitcast [[TYPE]] [[VALUE]] to i8*
  // x86_64-macosx:      ret i8* [[RESULT]]
  @objc func copyProto(a: AnyObject) -> AnyObject {
    return a
  }

  // x86_64-macosx:      define internal i8* @_TToFC8abitypes3Foo13copyProtoComp{{.*}}(i8*, i8*, i8*) unnamed_addr {
  // x86_64-macosx:      [[VALUE:%[0-9]+]] = call [[TYPE:%.*]] @_TFC8abitypes3Foo13copyProtoComp
  // x86_64-macosx:      [[RESULT:%[0-9]+]] = bitcast [[TYPE]] [[VALUE]] to i8*
  // x86_64-macosx:      ret i8* [[RESULT]]
  @objc func copyProtoComp(a: protocol<P1, P2>) -> protocol<P1, P2> {
    return a
  }

#if os(OSX)
  // x86_64-macosx:       define i1 @_TFC8abitypes3Foo6negate{{.*}}(i1, %C8abitypes3Foo*) {
  // x86_64-macosx:       define internal signext i8 @_TToFC8abitypes3Foo6negate{{.*}}(i8*, i8*, i8 signext) unnamed_addr {
  // x86_64-macosx:       [[R1:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool
  // x86_64-macosx:       [[R2:%[0-9]+]] = call i1 @_TFC8abitypes3Foo6negate
  // x86_64-macosx:       [[R3:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[R2]]
  // x86_64-macosx:       ret i8 [[R3]]
  //
  // x86_64-ios-fixme:          define i1 @_TFC8abitypes3Foo6negatefS0_FTSb_Sb(i1, %C8abitypes3Foo*) {
  // x86_64-ios-fixme:          define internal zeroext i1 @_TToFC8abitypes3Foo6negatefS0_FT
  // x86_64-ios-fixme:          [[R1:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBoolFT1xVS_8ObjCBool_Sb(i1 %2)
  // x86_64-ios-fixme:          [[R2:%[0-9]+]] = call i1 @_TFC8abitypes3Foo6negatefS0_FTSb_Sb(i1 [[R1]]
  // x86_64-ios-fixme:          [[R3:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertBoolToObjCBoolFT1xSb_VS_8ObjCBool(i1 [[R2]])
  // x86_64-ios-fixme:          ret i1 [[R3]]
  //
  // armv7-ios-fixme:     define i1 @_TFC8abitypes3Foo6negatefS0_FTSb_Sb(i1, %C8abitypes3Foo*) {
  // armv7-ios-fixme:     define internal signext i8 @_TToFC8abitypes3Foo6negatefS0_FTSb_Sb(i8*, i8*, i8 signext) unnamed_addr {
  // armv7-ios-fixme:     [[R1:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBoolFT1xVS_8ObjCBool_Sb
  // armv7-ios-fixme:     [[R2:%[0-9]+]] = call i1 @_TFC8abitypes3Foo6negatefS0_FTSb_Sb(i1 [[R1]]
  // armv7-ios-fixme:     [[R3:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBoolFT1xSb_VS_8ObjCBool(i1 [[R2]]
  // armv7-ios-fixme:     ret i8 [[R3]]
  //
  // arm64-ios-fixme:     define i1 @_TFC8abitypes3Foo6negate{{.*}}(i1, %C8abitypes3Foo*) {
  // arm64-ios-fixme:     define internal zeroext i1 @_TToFC8abitypes3Foo6negate
  // arm64-ios-fixme:     [[R1:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool
  // arm64-ios-fixme:     [[R2:%[0-9]+]] = call i1 @_TFC8abitypes3Foo6negate
  // arm64-ios-fixme:     [[R3:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[R2]])
  // arm64-ios-fixme:     ret i1 [[R3]]
  //
  // i386-ios-fixme:      define i1 @_TFC8abitypes3Foo6negate{{.*}}(i1, %C8abitypes3Foo*) {
  // i386-ios-fixme:      define internal signext i8 @_TToFC8abitypes3Foo6negate{{.*}}(i8*, i8*, i8 signext) unnamed_addr {
  // i386-ios-fixme:     [[R1:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool
  // i386-ios-fixme:     [[R2:%[0-9]+]] = call i1 @_TFC8abitypes3Foo6negate{{.*}}(i1 [[R1]]
  // i386-ios-fixme:     [[R3:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[R2]]
  // i386-ios-fixme:     ret i8 [[R3]]
  @objc func negate(b: Bool) -> Bool {
    return !b
  }

  // x86_64-macosx: define i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1, %C8abitypes3Foo*) {
  // x86_64-macosx: [[TOOBJCBOOL:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 %0)
  // x86_64-macosx: [[SEL:%[0-9]+]] = load i8** @"\01L_selector(negate:)", align 8
  // x86_64-macosx: [[NEG:%[0-9]+]] = call i8 bitcast (void ()* @objc_msgSend to i8 ([[RECEIVER:.*]]*, i8*, i8)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i8 [[TOOBJCBOOL]])
  // x86_64-macosx: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool{{.*}}(i8 [[NEG]])
  // x86_64-macosx: ret i1 [[TOBOOL]]
  //
  // x86_64-macosx: define internal signext i8 @_TToFC8abitypes3Foo7negate2{{.*}}(i8*, i8*, i8 signext)
  // x86_64-macosx: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool
  // x86_64-macosx: [[NEG:%[0-9]+]] = call i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1 [[TOBOOL]]
  // x86_64-macosx: [[TOOBJCBOOL:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[NEG]])
  // x86_64-macosx: ret i8 [[TOOBJCBOOL]]
  //
  // x86_64-ios-fixme: define i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1, %C8abitypes3Foo*) {
  // x86_64-ios-fixme: [[TOOBJCBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 %0)
  // x86_64-ios-fixme: [[SEL:%[0-9]+]] = load i8** @"\01L_selector(negate:)", align 8
  // x86_64-ios-fixme: [[NEG:%[0-9]+]] = call i1 bitcast (void ()* @objc_msgSend to i1 ([[RECEIVER:.*]]*, i8*, i1)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i1 [[TOOBJCBOOL]])
  // x86_64-ios-fixme: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool{{.*}}(i1 [[NEG]])
  // x86_64-ios-fixme: ret i1 [[TOBOOL]]
  //
  // x86_64-ios-fixme: define internal zeroext i1 @_TToFC8abitypes3Foo7negate2{{.*}}(i8*, i8*, i1 zeroext)
  // x86_64-ios-fixme: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool
  // x86_64-ios-fixme: [[NEG:%[0-9]+]] = call i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1 [[TOBOOL]]
  // x86_64-ios-fixme: [[TOOBJCBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[NEG]])
  // x86_64-ios-fixme: ret i1 [[TOOBJCBOOL]]
  //
  // armv7-ios-fixme: define i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1, %C8abitypes3Foo*) {
  // armv7-ios-fixme: [[TOOBJCBOOL:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 %0)
  // armv7-ios-fixme: [[SEL:%[0-9]+]] = load i8** @"\01L_selector(negate:)", align 4
  // armv7-ios-fixme: [[NEG:%[0-9]+]] = call i8 bitcast (void ()* @objc_msgSend to i8 ([[RECEIVER:.*]]*, i8*, i8)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i8 [[TOOBJCBOOL]])
  // armv7-ios-fixme: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool{{.*}}(i8 [[NEG]])
  // armv7-ios-fixme: ret i1 [[TOBOOL]]
  //
  // armv7-ios-fixme: define internal signext i8 @_TToFC8abitypes3Foo7negate2{{.*}}(i8*, i8*, i8 signext)
  // armv7-ios-fixme: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool
  // armv7-ios-fixme: [[NEG:%[0-9]+]] = call i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1 [[TOBOOL]]
  // armv7-ios-fixme: [[TOOBJCBOOL:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[NEG]])
  // armv7-ios-fixme: ret i8 [[TOOBJCBOOL]]
  //
  // arm64-ios-fixme: define i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1, %C8abitypes3Foo*) {
  // arm64-ios-fixme: [[TOOBJCBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 %0)
  // arm64-ios-fixme: [[SEL:%[0-9]+]] = load i8** @"\01L_selector(negate:)", align 8
  // arm64-ios-fixme: [[NEG:%[0-9]+]] = call i1 bitcast (void ()* @objc_msgSend to i1 ([[RECEIVER:.*]]*, i8*, i1)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i1 [[TOOBJCBOOL]])
  // arm64-ios-fixme: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool{{.*}}(i1 [[NEG]])
  // arm64-ios-fixme: ret i1 [[TOBOOL]]
  //
  // arm64-ios-fixme: define internal zeroext i1 @_TToFC8abitypes3Foo7negate2{{.*}}(i8*, i8*, i1 zeroext)
  // arm64-ios-fixme: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool
  // arm64-ios-fixme: [[NEG:%[0-9]+]] = call i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1 [[TOBOOL]]
  // arm64-ios-fixme: [[TOOBJCBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[NEG]])
  // arm64-ios-fixme: ret i1 [[TOOBJCBOOL]]
  //
  // i386-ios-fixme: define i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1, %C8abitypes3Foo*) {
  // i386-ios-fixme: [[TOOBJCBOOL:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 %0)
  // i386-ios-fixme: [[SEL:%[0-9]+]] = load i8** @"\01L_selector(negate:)", align 4
  // i386-ios-fixme: [[NEG:%[0-9]+]] = call i8 bitcast (void ()* @objc_msgSend to i8 ([[RECEIVER:.*]]*, i8*, i8)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i8 [[TOOBJCBOOL]])
  // i386-ios-fixme: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool{{.*}}(i8 [[NEG]])
  // i386-ios-fixme: ret i1 [[TOBOOL]]
  //
  // i386-ios-fixme: define internal signext i8 @_TToFC8abitypes3Foo7negate2{{.*}}(i8*, i8*, i8 signext)
  // i386-ios-fixme: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool
  // i386-ios-fixme: [[NEG:%[0-9]+]] = call i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1 [[TOBOOL]]
  // i386-ios-fixme: [[TOOBJCBOOL:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[NEG]])
  // i386-ios-fixme: ret i8 [[TOOBJCBOOL]]
  //
  @objc func negate2(b: Bool) -> Bool {
    var g = Gadget()
    return g.negate(b)
  }
#else
// FIXME: rdar://16785731
#endif

  // x86_64-macosx: define internal i32* @_TToFC8abitypes3Foo17copyUnsafePointer{{.*}}(i8*, i8*, i32*) unnamed_addr {
  @objc func copyUnsafePointer(p: UnsafePointer<Int32>) -> UnsafePointer<Int32> {
    return p
  }

  // x86_64-macosx: define internal i64 @_TToFC8abitypes3Foo17returnNSEnumValue{{.*}}(i8*, i8*) unnamed_addr {
  @objc func returnNSEnumValue() -> NSByteCountFormatterCountStyle {
    return .File
  }

  // x86_64-macosx: define internal zeroext i16 @_TToFC8abitypes3Foo20returnOtherEnumValue{{.*}}(i8*, i8*, i16 zeroext) unnamed_addr {
  @objc func returnOtherEnumValue(choice: ChooseTo) -> ChooseTo {
    switch choice {
      case .TakeIt: return .LeaveIt
      case .LeaveIt: return .TakeIt
    }
  }

  // x86_64-macosx: define i32 @_TFC8abitypes3Foo10getRawEnum{{.*}}(%C8abitypes3Foo*) {
  // x86_64-macosx: define internal i32 @_TToFC8abitypes3Foo10getRawEnum{{.*}}(i8*, i8*) unnamed_addr {
  @objc func getRawEnum() -> RawEnum {
    return Intergalactic
  }

  var work : Work
  init (work: Work) {
    self.work = work
  }

  // x86_64-macosx: define internal void @_TToFC8abitypes3Foo13testArchetypef{{.*}}(i8*, i8*, i8*) unnamed_addr {
  @objc func testArchetype(work: Work) {
    work.doStuff(1)
    // x86_64-macosx: [[PROTOCOL:%.*]] = alloca i8*, align 8
    // x86_64-macosx: store i8* %2, i8** [[PROTOCOL]]
    // x86_64-macosx: [[WORKPTR:%.*]] = bitcast i8** %.coerced to %P8abitypes4Work_*
    // x86_64-macosx: [[GEP:%.*]] = getelementptr inbounds %P8abitypes4Work_* [[WORKPTR]], i32 0, i32 0
    // x86_64-macosx: [[OBJCPTR:%.*]] = load %objc_object** [[GEP]], align 8
    // x86_64-macosx: call void @_TFC8abitypes3Foo13testArchetype{{.*}}(%objc_object* [[OBJCPTR]], %C8abitypes3Foo* %{{.*}})
  }

  @objc func foo(x: @objc_block (Int) -> Int) -> Int {
    // FIXME: calling blocks is currently unimplemented
    // return x(5)
    return 1
  }

  // x86_64-macosx: define void @_TFC8abitypes3Foo20testGenericTypeParam{{.*}}(%objc_object*, %C8abitypes3Foo*, %swift.type* %T) {
  func testGenericTypeParam<T: Pasta>(x: T) {
    // x86_64-macosx: [[CAST:%.*]] = bitcast %objc_object* %0 to i8*
    // x86_64-macosx: call void bitcast (void ()* @objc_msgSend to void (i8*, i8*)*)(i8* [[CAST]], i8* %{{.*}})
    x.alDente()
  }

  // arm64-ios: define void @_TFC8abitypes3Foo14callJustReturn{{.*}}(%VSC9BigStruct* noalias sret, %CSo13StructReturns*, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, %C8abitypes3Foo*) {
  // arm64-ios: define internal void @_TToFC8abitypes3Foo14callJustReturnfS0_FTCSo13StructReturns4withVSC9BigStruct_S2_(%VSC9BigStruct* noalias sret, i8*, i8*, [[OPAQUE:.*]]*, %VSC9BigStruct*) unnamed_addr {
  @objc func callJustReturn(r: StructReturns, with v: BigStruct) -> BigStruct {
    return r.justReturn(v)
  }
}

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi %s -emit-ir | FileCheck -check-prefix=%target-cpu-%target-os %s

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

import gadget
import Foundation

@objc protocol P1 {}
@objc protocol P2 {}
@objc protocol Work {
  func doStuff(x: Int64)
}

// arm64-ios: [[ARM64_MYRECT:%.*]] = type { float, float, float, float }
// arm64-tvos: [[ARM64_MYRECT:%.*]] = type { float, float, float, float }
// armv7k-watchos: [[ARMV7K_MYRECT:%.*]] = type { float, float, float, float }

class Foo {
  // x86_64-macosx: define hidden void @_TFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, %C8abitypes3Foo*) {{.*}} {
  // x86_64-macosx: define hidden { <2 x float>, <2 x float> } @_TToFC8abitypes3Foo3barfS0_FT_VSC6MyRect(i8*, i8*) unnamed_addr {{.*}} {
  // x86_64-ios: define hidden void @_TFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, %C8abitypes3Foo*) {{.*}} {
  // x86_64-ios: define hidden { <2 x float>, <2 x float> } @_TToFC8abitypes3Foo3barfS0_FT_VSC6MyRect(i8*, i8*) unnamed_addr {{.*}} {
  // i386-ios: define hidden void @_TFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, %C8abitypes3Foo*) {{.*}} {
  // i386-ios: define hidden void @_TToFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, i8*, i8*) unnamed_addr {{.*}} {
  // armv7-ios: define hidden void @_TFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, %C8abitypes3Foo*) {{.*}} {
  // armv7-ios: define hidden void @_TToFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, i8*, i8*) unnamed_addr {{.*}} {
  // arm64-ios: define hidden void @_TFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, %C8abitypes3Foo*) {{.*}} {
  // arm64-ios: define hidden [[ARM64_MYRECT]] @_TToFC8abitypes3Foo3barfS0_FT_VSC6MyRect(i8*, i8*) unnamed_addr {{.*}} {
  // x86_64-tvos: define hidden void @_TFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, %C8abitypes3Foo*) {{.*}} {
  // x86_64-tvos: define hidden { <2 x float>, <2 x float> } @_TToFC8abitypes3Foo3barfS0_FT_VSC6MyRect(i8*, i8*) unnamed_addr {{.*}} {
  // arm64-tvos: define hidden void @_TFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, %C8abitypes3Foo*) {{.*}} {
  // arm64-tvos: define hidden [[ARM64_MYRECT]] @_TToFC8abitypes3Foo3barfS0_FT_VSC6MyRect(i8*, i8*) unnamed_addr {{.*}} {
  // i386-watchos: define hidden void @_TFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, %C8abitypes3Foo*) {{.*}} {
  // i386-watchos: define hidden void @_TToFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, i8*, i8*) unnamed_addr {{.*}} {
  // armv7k-watchos: define hidden void @_TFC8abitypes3Foo3barfS0_FT_VSC6MyRect(%VSC6MyRect* noalias sret, %C8abitypes3Foo*) {{.*}} {
  // armv7k-watchos: define hidden [[ARMV7K_MYRECT]] @_TToFC8abitypes3Foo3barfS0_FT_VSC6MyRect(i8*, i8*) unnamed_addr {{.*}} {
  dynamic func bar() -> MyRect {
    return MyRect(x: 1, y: 2, width: 3, height: 4)
  }


  // x86_64-macosx: define hidden double @_TFC8abitypes3Foo14getXFromNSRect{{.*}}(%VSC6CGRect*, %C8abitypes3Foo*) {{.*}} {
  // x86_64-macosx: define hidden double @_TToFC8abitypes3Foo14getXFromNSRect{{.*}}(i8*, i8*, %VSC6CGRect* byval align 8) unnamed_addr {{.*}} {
  // armv7-ios: define hidden double @_TFC8abitypes3Foo14getXFromNSRect{{.*}}(%VSC6CGRect*, %C8abitypes3Foo*) {{.*}} {
  // armv7-ios: define hidden double @_TToFC8abitypes3Foo14getXFromNSRect{{.*}}(i8*, i8*, [4 x i32]) unnamed_addr {{.*}} {
  // armv7k-watchos: define hidden double @_TFC8abitypes3Foo14getXFromNSRect{{.*}}(float, float, float, float, %C8abitypes3Foo*) {{.*}} {
  // armv7k-watchos: define hidden double @_TToFC8abitypes3Foo14getXFromNSRect{{.*}}(i8*, i8*, [4 x float]) unnamed_addr {{.*}} {
  dynamic func getXFromNSRect(r: NSRect) -> Double {
    return Double(r.origin.x)
  }

  // x86_64-macosx: define hidden float @_TFC8abitypes3Foo12getXFromRect{{.*}}(%VSC6MyRect*, %C8abitypes3Foo*) {{.*}} {
  // x86_64-macosx: define hidden float @_TToFC8abitypes3Foo12getXFromRect{{.*}}(i8*, i8*, <2 x float>, <2 x float>) unnamed_addr {{.*}} {
  // armv7-ios: define hidden float @_TFC8abitypes3Foo12getXFromRect{{.*}}(%VSC6MyRect*, %C8abitypes3Foo*) {{.*}} {
  // armv7-ios: define hidden float @_TToFC8abitypes3Foo12getXFromRect{{.*}}(i8*, i8*, [4 x i32]) unnamed_addr {{.*}} {
  // armv7k-watchos: define hidden float @_TFC8abitypes3Foo12getXFromRect{{.*}}(float, float, float, float, %C8abitypes3Foo*) {{.*}} {
  // armv7k-watchos: define hidden float @_TToFC8abitypes3Foo12getXFromRect{{.*}}(i8*, i8*, [4 x float]) unnamed_addr {{.*}} {
  dynamic func getXFromRect(r: MyRect) -> Float {
    return r.x
  }

  // Call from Swift entrypoint with exploded Rect to @objc entrypoint
  // with unexploaded ABI-coerced type.
  // x86_64-macosx: define hidden float @_TFC8abitypes3Foo17getXFromRectSwift{{.*}}(%VSC6MyRect*, [[SELF:%.*]]*) {{.*}} {
  // x86_64-macosx: [[COERCED:%.*]] = alloca [[MYRECT:%.*MyRect.*]], align 4
  // x86_64-macosx: [[SEL:%.*]] = load i8*, i8** @"\01L_selector(getXFromRect:)", align 8
  // x86_64-macosx: [[CAST:%.*]] = bitcast [[MYRECT]]* [[COERCED]] to { <2 x float>, <2 x float> }*
  // x86_64-macosx: [[T0:%.*]] = getelementptr inbounds { <2 x float>, <2 x float> }, { <2 x float>, <2 x float> }* [[CAST]], i32 0, i32 0
  // x86_64-macosx: [[FIRST_HALF:%.*]] = load <2 x float>, <2 x float>* [[T0]]
  // x86_64-macosx: [[T0:%.*]] = getelementptr inbounds { <2 x float>, <2 x float> }, { <2 x float>, <2 x float> }* [[CAST]], i32 0, i32 1
  // x86_64-macosx: [[SECOND_HALF:%.*]] = load <2 x float>, <2 x float>* [[T0]]
  // x86_64-macosx: [[SELFCAST:%.*]] = bitcast [[SELF]]* %1 to i8*
  // x86_64-macosx: [[RESULT:%.*]] = call float bitcast (void ()* @objc_msgSend to float (i8*, i8*,  <2 x float>, <2 x float>)*)(i8* [[SELFCAST]], i8* [[SEL]], <2 x float> [[FIRST_HALF]], <2 x float> [[SECOND_HALF]])
  // armv7-ios: define hidden float @_TFC8abitypes3Foo17getXFromRectSwift{{.*}}(%VSC6MyRect*, [[SELF:%.*]]*) {{.*}} {
  // armv7-ios: [[COERCED:%.*]] = alloca [[MYRECT:%.*MyRect.*]], align 4
  // armv7-ios: [[SEL:%.*]] = load i8*, i8** @"\01L_selector(getXFromRect:)", align 4
  // armv7-ios: [[CAST:%.*]] = bitcast [[MYRECT]]* [[COERCED]] to [4 x i32]*
  // armv7-ios: [[LOADED:%.*]] = load [4 x i32], [4 x i32]* [[CAST]]
  // armv7-ios: [[SELFCAST:%.*]] = bitcast [[SELF]]* %1 to i8*
  // armv7-ios: [[RESULT:%.*]] = call float bitcast (void ()* @objc_msgSend to float (i8*, i8*, [4 x i32])*)(i8* [[SELFCAST]], i8* [[SEL]], [4 x i32] [[LOADED]])
  // armv7k-watchos: define hidden float @_TFC8abitypes3Foo17getXFromRectSwift{{.*}}(float, float, float, float, [[SELF:%.*]]*) {{.*}} {
  // armv7k-watchos: [[COERCED:%.*]] = alloca [[MYRECT:%.*MyRect.*]], align 4
  // armv7k-watchos: [[SEL:%.*]] = load i8** @"\01L_selector(getXFromRect:)", align 4
  // armv7k-watchos: [[CAST:%.*]] = bitcast [[MYRECT]]* [[COERCED]] to [4 x float]*
  // armv7k-watchos: [[LOADED:%.*]] = load [4 x float]* [[CAST]]
  // armv7k-watchos: [[SELFCAST:%.*]] = bitcast [[SELF]]* %4 to i8*
  // armv7k-watchos: [[RESULT:%.*]] = call float bitcast (void ()* @objc_msgSend to float (i8*, i8*, [4 x float])*)(i8* [[SELFCAST]], i8* [[SEL]], [4 x float] [[LOADED]])
  func getXFromRectSwift(r: MyRect) -> Float {
    return getXFromRect(r)
  }

  // Ensure that MyRect is passed as an indirect-byval on x86_64 because we run out of registers for direct arguments
  // x86_64-macosx: define hidden float @_TToFC8abitypes3Foo25getXFromRectIndirectByVal{{.*}}(i8*, i8*, float, float, float, float, float, float, float, %VSC6MyRect* byval align 4) unnamed_addr {{.*}} {
  dynamic func getXFromRectIndirectByVal(_: Float, second _: Float, 
                                       third _: Float, fourth _: Float,
                                       fifth _: Float, sixth _: Float,
                                       seventh _: Float, withRect r: MyRect)
               -> Float {
    return r.x
  }

  // Make sure the caller-side from Swift also uses indirect-byval for the argument
  // x86_64-macosx: define hidden float @_TFC8abitypes3Foo25getXFromRectIndirectSwift{{.*}}(%VSC6MyRect*, %C8abitypes3Foo*) {{.*}} {
  func getXFromRectIndirectSwift(r: MyRect) -> Float {
    let f : Float = 1.0;
    // x86_64-macosx: [[TEMP:%.*]] = alloca [[TEMPTYPE:%.*]], align 4
    // x86_64-macosx: [[RESULT:%.*]] = call float bitcast (void ()* @objc_msgSend to float (i8*, i8*, float, float, float, float, float, float, float, [[TEMPTYPE]]*)*)(i8* %{{.*}}, i8* %{{.*}}, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, [[TEMPTYPE]]* byval align 4 [[TEMP]])
    // x86_64-macosx: ret float [[RESULT]]
    return getXFromRectIndirectByVal(f, second: f, third: f, fourth: f, fifth: f, sixth: f, seventh: f, withRect: r);
  }

  // x86_64 returns an HA of four floats directly in two <2 x float>
  // x86_64-macosx:      define hidden float @_TFC8abitypes3Foo4barc{{.*}}(%CSo13StructReturns*, %C8abitypes3Foo*) {{.*}} {
  // x86_64-macosx:      load i8*, i8** @"\01L_selector(newRect)", align 8
  // x86_64-macosx:      [[RESULT:%.*]] = call { <2 x float>, <2 x float> } bitcast (void ()* @objc_msgSend
  // x86_64-macosx:      store { <2 x float>, <2 x float> } [[RESULT]]
  // x86_64-macosx:      [[CAST:%.*]] = bitcast { <2 x float>, <2 x float> }*
  // x86_64-macosx:      load { float, float, float, float }, { float, float, float, float }* [[CAST]]
  // x86_64-macosx:      ret float
  //
  // armv7 returns an HA of four floats indirectly
  // armv7-ios: define hidden float @_TFC8abitypes3Foo4barc{{.*}}(%CSo13StructReturns*, %C8abitypes3Foo*) {{.*}} {
  // armv7-ios: [[RESULT:%.*]] = alloca [[RECTTYPE:%.*MyRect.*]], align 4
  // armv7-ios: load i8*, i8** @"\01L_selector(newRect)", align 4
  // armv7-ios: call void bitcast (void ()* @objc_msgSend_stret to void ([[RECTTYPE]]*, [[RECEIVER:.*]]*, i8*)*)([[RECTTYPE]]* noalias sret %call.aggresult
  // armv7-ios: [[GEP1:%.*]] = getelementptr inbounds [[RECTTYPE]], [[RECTTYPE]]* [[RESULT]], i32 0, i32 1
  // armv7-ios: [[GEP2:%.*]] = getelementptr inbounds {{.*}}, {{.*}}* [[GEP1]], i32 0, i32 0
  // armv7-ios: [[RETVAL:%.*]] = load float, float* [[GEP2]], align 4
  // armv7-ios: ret float [[RETVAL]]
  //
  // armv7k returns an HA of four floats directly
  // armv7k-watchos:      define hidden float @_TFC8abitypes3Foo4barc{{.*}}(%CSo13StructReturns*, %C8abitypes3Foo*) {{.*}} {
  // armv7k-watchos:      load i8** @"\01L_selector(newRect)", align 4
  // armv7k-watchos:      [[RESULT:%.*]] = call [[ARMV7K_MYRECT]] bitcast (void ()* @objc_msgSend
  // armv7k-watchos:      store [[ARMV7K_MYRECT]] [[RESULT]]
  // armv7k-watchos:      [[CAST:%.*]] = bitcast [[ARMV7K_MYRECT]]*
  // armv7k-watchos:      load { float, float, float, float }* [[CAST]]
  // armv7k-watchos:      ret float
  func barc(p: StructReturns) -> Float {
    return p.newRect().y
  }

  // x86_64-macosx: define hidden { double, double, double } @_TFC8abitypes3Foo3baz{{.*}}(%C8abitypes3Foo*) {{.*}} {
  // x86_64-macosx: define hidden void @_TToFC8abitypes3Foo3baz{{.*}}(%VSC4Trio* noalias sret, i8*, i8*) unnamed_addr {{.*}} {
  dynamic func baz() -> Trio {
    return Trio(i: 1.0, j: 2.0, k: 3.0)
  }

  // x86_64-macosx:      define hidden double @_TFC8abitypes3Foo4bazc{{.*}}(%CSo13StructReturns*, %C8abitypes3Foo*) {{.*}} {
  // x86_64-macosx:      load i8*, i8** @"\01L_selector(newTrio)", align 8
  // x86_64-macosx:      [[CAST:%[0-9]+]] = bitcast {{%.*}}* %0
  // x86_64-macosx:      call void bitcast (void ()* @objc_msgSend_stret to void (%VSC4Trio*, [[OPAQUE:.*]]*, i8*)*)
  func bazc(p: StructReturns) -> Double {
    return p.newTrio().j
  }

  // x86_64-macosx:      define hidden { i32, i32 } @_TFC8abitypes3Foo7getpair{{.*}}(%CSo13StructReturns*, %C8abitypes3Foo*) {{.*}} {
  // x86_64-macosx:      [[RESULT:%.*]] = call i64 bitcast (void ()* @objc_msgSend to i64 ([[OPAQUE:.*]]*, i8*)*)
  // x86_64-macosx:      store i64 [[RESULT]]
  // x86_64-macosx:      [[CAST:%.*]] = bitcast i64* {{%.*}} to { i32, i32 }*
  // x86_64-macosx:      load { i32, i32 }, { i32, i32 }* [[CAST]]
  // x86_64-macosx:      ret { i32, i32 }
  func getpair(p: StructReturns) -> IntPair {
    return p.newPair()
  }

  // x86_64-macosx:      define hidden i64 @_TToFC8abitypes3Foo8takepair{{.*}}(i8*, i8*, i64) unnamed_addr {{.*}} {
  dynamic func takepair(p: IntPair) -> IntPair {
    return p
  }

  // x86_64-macosx:      define hidden { i32, i32 } @_TFC8abitypes3Foo9getnested{{.*}}(%CSo13StructReturns*, %C8abitypes3Foo*) {{.*}} {
  // x86_64-macosx:      call i64 bitcast (void ()* @objc_msgSend to i64 ([[OPAQUE:.*]]*, i8*)*)
  // x86_64-macosx-NEXT: store i64
  // x86_64-macosx-NEXT: bitcast i64* {{[^ ]*}} to { i32, i32 }*
  // x86_64-macosx-NEXT: load { i32, i32 }, { i32, i32 }*
  // x86_64-macosx:      ret { i32, i32 }
  func getnested(p: StructReturns) -> NestedInts {
    return p.newNestedInts()
  }

  // x86_64-macosx:      define hidden i8* @_TToFC8abitypes3Foo9copyClass{{.*}}(i8*, i8*, i8*) unnamed_addr {{.*}} {
  // x86_64-macosx:      [[VALUE:%[0-9]+]] = call [[TYPE:%.*]]* @_TFC8abitypes3Foo9copyClass
  // x86_64-macosx:      [[T0:%.*]] = phi [[TYPE]]* [ [[VALUE]],
  // x86_64-macosx:      [[T1:%.*]] = bitcast [[TYPE]]* [[T0]] to [[OBJC:%objc_class]]*
  // x86_64-macosx:      [[RESULT:%[0-9]+]] = bitcast [[OBJC]]* [[T1]] to i8*
  // x86_64-macosx:      ret i8* [[RESULT]]
  dynamic func copyClass(a: AnyClass) -> AnyClass {
    return a
  }

  // x86_64-macosx:      define hidden i8* @_TToFC8abitypes3Foo9copyProto{{.*}}(i8*, i8*, i8*) unnamed_addr {{.*}} {
  // x86_64-macosx:      [[VALUE:%[0-9]+]] = call [[TYPE:%.*]] @_TFC8abitypes3Foo9copyProt
  // x86_64-macosx:      [[RESULT:%[0-9]+]] = bitcast [[TYPE]] [[VALUE]] to i8*
  // x86_64-macosx:      ret i8* [[RESULT]]
  dynamic func copyProto(a: AnyObject) -> AnyObject {
    return a
  }

  // x86_64-macosx:      define hidden i8* @_TToFC8abitypes3Foo13copyProtoComp{{.*}}(i8*, i8*, i8*) unnamed_addr {{.*}} {
  // x86_64-macosx:      [[VALUE:%[0-9]+]] = call [[TYPE:%.*]] @_TFC8abitypes3Foo13copyProtoComp
  // x86_64-macosx:      [[RESULT:%[0-9]+]] = bitcast [[TYPE]] [[VALUE]] to i8*
  // x86_64-macosx:      ret i8* [[RESULT]]
  dynamic func copyProtoComp(a: protocol<P1, P2>) -> protocol<P1, P2> {
    return a
  }

  // x86_64-macosx:       define hidden i1 @_TFC8abitypes3Foo6negate{{.*}}(i1, %C8abitypes3Foo*) {{.*}} {
  // x86_64-macosx:       define hidden signext i8 @_TToFC8abitypes3Foo6negate{{.*}}(i8*, i8*, i8 signext) unnamed_addr {{.*}} {
  // x86_64-macosx:       [[R1:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool
  // x86_64-macosx:       [[R2:%[0-9]+]] = call i1 @_TFC8abitypes3Foo6negate
  // x86_64-macosx:       [[R3:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[R2]]
  // x86_64-macosx:       ret i8 [[R3]]
  //
  // x86_64-ios-fixme:          define hidden i1 @_TFC8abitypes3Foo6negatefS0_FTSb_Sb(i1, %C8abitypes3Foo*) {{.*}} {
  // x86_64-ios-fixme:          define internal zeroext i1 @_TToFC8abitypes3Foo6negatefS0_FT
  // x86_64-ios-fixme:          [[R1:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBoolFT1xVS_8ObjCBool_Sb(i1 %2)
  // x86_64-ios-fixme:          [[R2:%[0-9]+]] = call i1 @_TFC8abitypes3Foo6negatefS0_FTSb_Sb(i1 [[R1]]
  // x86_64-ios-fixme:          [[R3:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertBoolToObjCBoolFT1xSb_VS_8ObjCBool(i1 [[R2]])
  // x86_64-ios-fixme:          ret i1 [[R3]]
  //
  // armv7-ios-fixme:     define hidden i1 @_TFC8abitypes3Foo6negatefS0_FTSb_Sb(i1, %C8abitypes3Foo*) {{.*}} {
  // armv7-ios-fixme:     define internal signext i8 @_TToFC8abitypes3Foo6negatefS0_FTSb_Sb(i8*, i8*, i8 signext) unnamed_addr {{.*}} {
  // armv7-ios-fixme:     [[R1:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBoolFT1xVS_8ObjCBool_Sb
  // armv7-ios-fixme:     [[R2:%[0-9]+]] = call i1 @_TFC8abitypes3Foo6negatefS0_FTSb_Sb(i1 [[R1]]
  // armv7-ios-fixme:     [[R3:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBoolFT1xSb_VS_8ObjCBool(i1 [[R2]]
  // armv7-ios-fixme:     ret i8 [[R3]]
  //
  // arm64-ios-fixme:     define hidden i1 @_TFC8abitypes3Foo6negate{{.*}}(i1, %C8abitypes3Foo*) {{.*}} {
  // arm64-ios-fixme:     define internal zeroext i1 @_TToFC8abitypes3Foo6negate
  // arm64-ios-fixme:     [[R2:%[0-9]+]] = call i1 @_TFC8abitypes3Foo6negate
  // arm64-ios-fixme:     ret i1 [[R2]]
  //
  // i386-ios-fixme:      define hidden i1 @_TFC8abitypes3Foo6negate{{.*}}(i1, %C8abitypes3Foo*) {{.*}} {
  // i386-ios-fixme:      define internal signext i8 @_TToFC8abitypes3Foo6negate{{.*}}(i8*, i8*, i8 signext) unnamed_addr {{.*}} {
  // i386-ios-fixme:     [[R1:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool
  // i386-ios-fixme:     [[R2:%[0-9]+]] = call i1 @_TFC8abitypes3Foo6negate{{.*}}(i1 [[R1]]
  // i386-ios-fixme:     [[R3:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[R2]]
  // i386-ios-fixme:     ret i8 [[R3]]
  //
  // x86_64-tvos-fixme:          define hidden i1 @_TFC8abitypes3Foo6negatefS0_FTSb_Sb(i1, %C8abitypes3Foo*) {{.*}} {
  // x86_64-tvos-fixme:          define internal zeroext i1 @_TToFC8abitypes3Foo6negatefS0_FT
  // x86_64-tvos-fixme:          [[R1:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBoolFT1xVS_8ObjCBool_Sb(i1 %2)
  // x86_64-tvos-fixme:          [[R2:%[0-9]+]] = call i1 @_TFC8abitypes3Foo6negatefS0_FTSb_Sb(i1 [[R1]]
  // x86_64-tvos-fixme:          [[R3:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertBoolToObjCBoolFT1xSb_VS_8ObjCBool(i1 [[R2]])
  // x86_64-tvos-fixme:          ret i1 [[R3]]
  //
  // arm64-tvos-fixme:     define hidden i1 @_TFC8abitypes3Foo6negate{{.*}}(i1, %C8abitypes3Foo*) {{.*}} {
  // arm64-tvos-fixme:     define internal zeroext i1 @_TToFC8abitypes3Foo6negate
  // arm64-tvos-fixme:     [[R2:%[0-9]+]] = call i1 @_TFC8abitypes3Foo6negate
  // arm64-tvos-fixme:     ret i1 [[R2]]
  dynamic func negate(b: Bool) -> Bool {
    return !b
  }

  // x86_64-macosx: define hidden i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1, %C8abitypes3Foo*) {{.*}} {
  // x86_64-macosx: [[TOOBJCBOOL:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 %0)
  // x86_64-macosx: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 8
  // x86_64-macosx: [[NEG:%[0-9]+]] = call signext i8 bitcast (void ()* @objc_msgSend to i8 ([[RECEIVER:.*]]*, i8*, i8)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i8 signext [[TOOBJCBOOL]])
  // x86_64-macosx: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool{{.*}}(i8 [[NEG]])
  // x86_64-macosx: ret i1 [[TOBOOL]]
  //
  // x86_64-macosx: define hidden signext i8 @_TToFC8abitypes3Foo7negate2{{.*}}(i8*, i8*, i8 signext)
  // x86_64-macosx: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool
  // x86_64-macosx: [[NEG:%[0-9]+]] = call i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1 [[TOBOOL]]
  // x86_64-macosx: [[TOOBJCBOOL:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[NEG]])
  // x86_64-macosx: ret i8 [[TOOBJCBOOL]]
  //
  // x86_64-ios: define hidden i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1, %C8abitypes3Foo*) {{.*}} {
  // x86_64-ios: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 8
  // x86_64-ios: [[NEG:%[0-9]+]] = call zeroext i1 bitcast (void ()* @objc_msgSend to i1 ([[RECEIVER:.*]]*, i8*, i1)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i1 zeroext %0)
  // x86_64-ios: ret i1 [[NEG]]
  //
  // x86_64-ios: define hidden zeroext i1 @_TToFC8abitypes3Foo7negate2{{.*}}(i8*, i8*, i1 zeroext)
  // x86_64-ios: [[NEG:%[0-9]+]] = call i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1
  // x86_64-ios: [[TOOBJCBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[NEG]])
  // x86_64-ios: ret i1 [[TOOBJCBOOL]]
  //
  // armv7-ios: define hidden i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1, %C8abitypes3Foo*) {{.*}} {
  // armv7-ios: [[TOOBJCBOOL:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 %0)
  // armv7-ios: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 4
  // armv7-ios: [[NEG:%[0-9]+]] = call signext i8 bitcast (void ()* @objc_msgSend to i8 ([[RECEIVER:.*]]*, i8*, i8)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i8 signext [[TOOBJCBOOL]])
  // armv7-ios: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool{{.*}}(i8 [[NEG]])
  // armv7-ios: ret i1 [[TOBOOL]]
  //
  // armv7-ios: define hidden signext i8 @_TToFC8abitypes3Foo7negate2{{.*}}(i8*, i8*, i8 signext)
  // armv7-ios: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool
  // armv7-ios: [[NEG:%[0-9]+]] = call i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1 [[TOBOOL]]
  // armv7-ios: [[TOOBJCBOOL:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[NEG]])
  // armv7-ios: ret i8 [[TOOBJCBOOL]]
  //
  // arm64-ios: define hidden i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1, %C8abitypes3Foo*) {{.*}} {
  // arm64-ios: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 8
  // arm64-ios: [[NEG:%[0-9]+]] = call zeroext i1 bitcast (void ()* @objc_msgSend to i1 ([[RECEIVER:.*]]*, i8*, i1)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i1 zeroext %0)
  // arm64-ios: ret i1 [[NEG]]
  //
  // arm64-ios: define hidden zeroext i1 @_TToFC8abitypes3Foo7negate2{{.*}}(i8*, i8*, i1 zeroext)
  // arm64-ios: [[NEG:%[0-9]+]] = call i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1
  // arm64-ios: [[TOOBJCBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[NEG]])
  // arm64-ios: ret i1 [[TOOBJCBOOL]]
  //
  // i386-ios: define hidden i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1, %C8abitypes3Foo*) {{.*}} {
  // i386-ios: [[TOOBJCBOOL:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 %0)
  // i386-ios: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 4
  // i386-ios: [[NEG:%[0-9]+]] = call signext i8 bitcast (void ()* @objc_msgSend to i8 ([[RECEIVER:.*]]*, i8*, i8)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i8 signext [[TOOBJCBOOL]])
  // i386-ios: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool{{.*}}(i8 [[NEG]])
  // i386-ios: ret i1 [[TOBOOL]]
  //
  // i386-ios: define hidden signext i8 @_TToFC8abitypes3Foo7negate2{{.*}}(i8*, i8*, i8 signext)
  // i386-ios: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool
  // i386-ios: [[NEG:%[0-9]+]] = call i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1 [[TOBOOL]]
  // i386-ios: [[TOOBJCBOOL:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[NEG]])
  // i386-ios: ret i8 [[TOOBJCBOOL]]
  //
  // x86_64-tvos: define hidden i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1, %C8abitypes3Foo*) {{.*}} {
  // x86_64-tvos: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 8
  // x86_64-tvos: [[NEG:%[0-9]+]] = call zeroext i1 bitcast (void ()* @objc_msgSend to i1 ([[RECEIVER:.*]]*, i8*, i1)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i1 zeroext %0)
  // x86_64-tvos: ret i1 [[NEG]]
  //
  // x86_64-tvos: define hidden zeroext i1 @_TToFC8abitypes3Foo7negate2{{.*}}(i8*, i8*, i1 zeroext)
  // x86_64-tvos: [[NEG:%[0-9]+]] = call i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1
  // x86_64-tvos: [[TOOBJCBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[NEG]])
  // x86_64-tvos: ret i1 [[TOOBJCBOOL]]
  //
  // arm64-tvos: define hidden i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1, %C8abitypes3Foo*) {{.*}} {
  // arm64-tvos: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 8
  // arm64-tvos: [[NEG:%[0-9]+]] = call zeroext i1 bitcast (void ()* @objc_msgSend to i1 ([[RECEIVER:.*]]*, i8*, i1)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i1 zeroext %0)
  // arm64-tvos: ret i1 [[NEG]]
  //
  // arm64-tvos: define hidden zeroext i1 @_TToFC8abitypes3Foo7negate2{{.*}}(i8*, i8*, i1 zeroext)
  // arm64-tvos: [[NEG:%[0-9]+]] = call i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1
  // arm64-tvos: [[TOOBJCBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[NEG]])
  // arm64-tvos: ret i1 [[TOOBJCBOOL]]
  //
  // armv7k-watchos: define hidden i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1, %C8abitypes3Foo*) {{.*}} {
  // armv7k-watchos: [[SEL:%[0-9]+]] = load i8** @"\01L_selector(negate:)", align 4
  // armv7k-watchos: [[NEG:%[0-9]+]] = call zeroext i1 bitcast (void ()* @objc_msgSend to i1 ([[RECEIVER:.*]]*, i8*, i1)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i1 zeroext %0)
  // armv7k-watchos: ret i1 [[NEG]]
  //
  // armv7k-watchos: define hidden zeroext i1 @_TToFC8abitypes3Foo7negate2{{.*}}(i8*, i8*, i1 zeroext)
  // armv7k-watchos: [[NEG:%[0-9]+]] = call i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1
  // armv7k-watchos: [[TOOBJCBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[NEG]])
  // armv7k-watchos: ret i1 [[TOOBJCBOOL]]
  //
  // i386-watchos: define hidden i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1, %C8abitypes3Foo*) {{.*}} {
  // i386-watchos: [[TOOBJCBOOL:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 %0)
  // i386-watchos: [[SEL:%[0-9]+]] = load i8** @"\01L_selector(negate:)", align 4
  // i386-watchos: [[NEG:%[0-9]+]] = call signext i8 bitcast (void ()* @objc_msgSend to i8 ([[RECEIVER:.*]]*, i8*, i8)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i8 signext [[TOOBJCBOOL]])
  // i386-watchos: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool{{.*}}(i8 [[NEG]])
  // i386-watchos: ret i1 [[TOBOOL]]
  //
  // i386-watchos: define hidden signext i8 @_TToFC8abitypes3Foo7negate2{{.*}}(i8*, i8*, i8 signext)
  // i386-watchos: [[TOBOOL:%[0-9]+]] = call i1 @_TF10ObjectiveC22_convertObjCBoolToBool
  // i386-watchos: [[NEG:%[0-9]+]] = call i1 @_TFC8abitypes3Foo7negate2{{.*}}(i1 [[TOBOOL]]
  // i386-watchos: [[TOOBJCBOOL:%[0-9]+]] = call i8 @_TF10ObjectiveC22_convertBoolToObjCBool{{.*}}(i1 [[NEG]])
  // i386-watchos: ret i8 [[TOOBJCBOOL]]
  //
  dynamic func negate2(b: Bool) -> Bool {
    var g = Gadget()
    return g.negate(b)
  }

  // x86_64-macosx: define hidden i32* @_TToFC8abitypes3Foo24copyUnsafeMutablePointer{{.*}}(i8*, i8*, i32*) unnamed_addr {{.*}} {
  dynamic func copyUnsafeMutablePointer(p: UnsafeMutablePointer<Int32>) -> UnsafeMutablePointer<Int32> {
    return p
  }

  // x86_64-macosx: define hidden i64 @_TToFC8abitypes3Foo17returnNSEnumValue{{.*}}(i8*, i8*) unnamed_addr {{.*}} {
  dynamic func returnNSEnumValue() -> NSByteCountFormatterCountStyle {
    return .File
  }

  // x86_64-macosx: define hidden zeroext i16 @_TToFC8abitypes3Foo20returnOtherEnumValue{{.*}}(i8*, i8*, i16 zeroext) unnamed_addr {{.*}} {
  dynamic func returnOtherEnumValue(choice: ChooseTo) -> ChooseTo {
    switch choice {
      case .TakeIt: return .LeaveIt
      case .LeaveIt: return .TakeIt
    }
  }

  // x86_64-macosx: define hidden i32 @_TFC8abitypes3Foo10getRawEnum{{.*}}(%C8abitypes3Foo*) {{.*}} {
  // x86_64-macosx: define hidden i32 @_TToFC8abitypes3Foo10getRawEnum{{.*}}(i8*, i8*) unnamed_addr {{.*}} {
  dynamic func getRawEnum() -> RawEnum {
    return Intergalactic
  }

  var work : Work
  init (work: Work) {
    self.work = work
  }

  // x86_64-macosx: define hidden void @_TToFC8abitypes3Foo13testArchetypef{{.*}}(i8*, i8*, i8*) unnamed_addr {{.*}} {
  dynamic func testArchetype(work: Work) {
    work.doStuff(1)
    // x86_64-macosx: [[OBJCPTR:%.*]] = bitcast i8* %2 to %objc_object*
    // x86_64-macosx: call void @_TFC8abitypes3Foo13testArchetype{{.*}}(%objc_object* [[OBJCPTR]], %C8abitypes3Foo* %{{.*}})
  }

  dynamic func foo(x: @convention(block) (Int) -> Int) -> Int {
    // FIXME: calling blocks is currently unimplemented
    // return x(5)
    return 1
  }

  // x86_64-macosx: define hidden void @_TFC8abitypes3Foo20testGenericTypeParam{{.*}}(%objc_object*, %swift.type* %T, %C8abitypes3Foo*) {{.*}} {
  func testGenericTypeParam<T: Pasta>(x: T) {
    // x86_64-macosx: [[CAST:%.*]] = bitcast %objc_object* %0 to i8*
    // x86_64-macosx: call void bitcast (void ()* @objc_msgSend to void (i8*, i8*)*)(i8* [[CAST]], i8* %{{.*}})
    x.alDente()
  }

  // arm64-ios: define hidden void @_TFC8abitypes3Foo14callJustReturn{{.*}}(%VSC9BigStruct* noalias sret, %CSo13StructReturns*, %VSC9BigStruct*, %C8abitypes3Foo*) {{.*}} {
  // arm64-ios: define hidden void @_TToFC8abitypes3Foo14callJustReturnfS0_FTCSo13StructReturns4withVSC9BigStruct_S2_(%VSC9BigStruct* noalias sret, i8*, i8*, [[OPAQUE:.*]]*, %VSC9BigStruct*) unnamed_addr {{.*}} {
  //
  // arm64-tvos: define hidden void @_TFC8abitypes3Foo14callJustReturn{{.*}}(%VSC9BigStruct* noalias sret, %CSo13StructReturns*, %VSC9BigStruct*, %C8abitypes3Foo*) {{.*}} {
  // arm64-tvos: define hidden void @_TToFC8abitypes3Foo14callJustReturnfS0_FTCSo13StructReturns4withVSC9BigStruct_S2_(%VSC9BigStruct* noalias sret, i8*, i8*, [[OPAQUE:.*]]*, %VSC9BigStruct*) unnamed_addr {{.*}} {
  dynamic func callJustReturn(r: StructReturns, with v: BigStruct) -> BigStruct {
    return r.justReturn(v)
  }

  // Test that the makeOne() that we generate somewhere below doesn't
  // use arm_aapcscc for armv7.
  func callInline() -> Float {
    return makeOne(3,5).second;
  }
}

// armv7-ios: define internal void @makeOne(%struct.One* noalias sret %agg.result, float %f, float %s)
// armv7k-watchos: define internal %struct.One @makeOne(float %f, float %s)

// rdar://17631440 - Expand direct arguments that are coerced to aggregates.
// x86_64-macosx: define float @_TF8abitypes13testInlineAggFVSC6MyRectSf(%VSC6MyRect*) {{.*}} {
// x86_64-macosx: [[COERCED:%.*]] = alloca %VSC6MyRect, align 4
// x86_64-macosx: store float %
// x86_64-macosx: store float %
// x86_64-macosx: store float %
// x86_64-macosx: store float %
// x86_64-macosx: [[CAST:%.*]] = bitcast %VSC6MyRect* [[COERCED]] to { <2 x float>, <2 x float> }*
// x86_64-macosx: [[T0:%.*]] = getelementptr inbounds { <2 x float>, <2 x float> }, { <2 x float>, <2 x float> }* [[CAST]], i32 0, i32 0
// x86_64-macosx: [[FIRST_HALF:%.*]] = load <2 x float>, <2 x float>* [[T0]], align 4
// x86_64-macosx: [[T0:%.*]] = getelementptr inbounds { <2 x float>, <2 x float> }, { <2 x float>, <2 x float> }* [[CAST]], i32 0, i32 1
// x86_64-macosx: [[SECOND_HALF:%.*]] = load <2 x float>, <2 x float>* [[T0]], align 4
// x86_64-macosx: [[RESULT:%.*]] = call float @MyRect_Area(<2 x float> [[FIRST_HALF]], <2 x float> [[SECOND_HALF]])
// x86_64-macosx: ret float [[RESULT]]
public func testInlineAgg(rect: MyRect) -> Float {
  return MyRect_Area(rect)
}

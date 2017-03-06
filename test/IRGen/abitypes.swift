// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -Xllvm -new-mangling-for-tests -I %S/Inputs/abi %s -emit-ir | %FileCheck -check-prefix=%target-cpu-%target-os %s

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

import gadget
import Foundation

@objc protocol P1 {}
@objc protocol P2 {}
@objc protocol Work {
  func doStuff(_ x: Int64)
}

// armv7s-ios: [[ARMV7S_MYRECT:%.*]] = type { float, float, float, float }
// arm64-ios: [[ARM64_MYRECT:%.*]] = type { float, float, float, float }
// arm64-tvos: [[ARM64_MYRECT:%.*]] = type { float, float, float, float }
// armv7k-watchos: [[ARMV7K_MYRECT:%.*]] = type { float, float, float, float }

class Foo {
  // x86_64-macosx: define hidden swiftcc { i64, i64 } @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F(%T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-macosx: define hidden { <2 x float>, <2 x float> } @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo(i8*, i8*) unnamed_addr {{.*}} {
  // x86_64-ios: define hidden swiftcc { i64, i64 } @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F(%T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-ios: define hidden { <2 x float>, <2 x float> } @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo(i8*, i8*) unnamed_addr {{.*}} {
  // i386-ios: define hidden swiftcc void @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F(%TSC6MyRectV* noalias nocapture sret, %T8abitypes3FooC* swiftself) {{.*}} {
  // i386-ios: define hidden void @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo(%TSC6MyRectV* noalias nocapture sret, i8*, i8*) unnamed_addr {{.*}} {
  // armv7-ios: define hidden swiftcc { float, float, float, float } @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F(%T8abitypes3FooC* swiftself) {{.*}} {
  // armv7-ios: define hidden void @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo(%TSC6MyRectV* noalias nocapture sret, i8*, i8*) unnamed_addr {{.*}} {
  // armv7s-ios: define hidden swiftcc { float, float, float, float } @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F(%T8abitypes3FooC* swiftself) {{.*}} {
  // armv7s-ios: define hidden void @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo(%TSC6MyRectV* noalias nocapture sret, i8*, i8*) unnamed_addr {{.*}} {
  // arm64-ios: define hidden swiftcc { i64, i64 } @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F(%T8abitypes3FooC* swiftself) {{.*}} {
  // arm64-ios: define hidden [[ARM64_MYRECT]] @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo(i8*, i8*) unnamed_addr {{.*}} {
  // x86_64-tvos: define hidden swiftcc { i64, i64 } @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F(%T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-tvos: define hidden { <2 x float>, <2 x float> } @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo(i8*, i8*) unnamed_addr {{.*}} {
  // arm64-tvos: define hidden swiftcc { i64, i64 }  @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F(%T8abitypes3FooC* swiftself) {{.*}} {
  // arm64-tvos: define hidden [[ARM64_MYRECT]] @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo(i8*, i8*) unnamed_addr {{.*}} {
  // i386-watchos: define hidden swiftcc void @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F(%TSC6MyRectV* noalias nocapture sret, %T8abitypes3FooC* swiftself) {{.*}} {
  // i386-watchos: define hidden void @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo(%TSC6MyRectV* noalias nocapture sret, i8*, i8*) unnamed_addr {{.*}} {
  // armv7k-watchos: define hidden swiftcc { float, float, float, float } @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F(%T8abitypes3FooC* swiftself) {{.*}} {
  // armv7k-watchos: define hidden [[ARMV7K_MYRECT]] @_T08abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo(i8*, i8*) unnamed_addr {{.*}} {
  dynamic func bar() -> MyRect {
    return MyRect(x: 1, y: 2, width: 3, height: 4)
  }


  // x86_64-macosx: define hidden swiftcc double @_T08abitypes3FooC14getXFromNSRect{{[_0-9a-zA-Z]*}}F(double, double, double, double, %T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-macosx: define hidden double @_T08abitypes3FooC14getXFromNSRect{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, %TSC6CGRectV* byval align 8) unnamed_addr {{.*}} {
  // armv7-ios: define hidden swiftcc double @_T08abitypes3FooC14getXFromNSRect{{[_0-9a-zA-Z]*}}F(float, float, float, float, %T8abitypes3FooC* swiftself) {{.*}} {
  // armv7-ios: define hidden double @_T08abitypes3FooC14getXFromNSRect{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, [4 x i32]) unnamed_addr {{.*}} {
  // armv7s-ios: define hidden swiftcc double @_T08abitypes3FooC14getXFromNSRect{{[_0-9a-zA-Z]*}}F(float, float, float, float, %T8abitypes3FooC* swiftself) {{.*}} {
  // armv7s-ios: define hidden double @_T08abitypes3FooC14getXFromNSRect{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, [4 x i32]) unnamed_addr {{.*}} {
  // armv7k-watchos: define hidden swiftcc double @_T08abitypes3FooC14getXFromNSRect{{[_0-9a-zA-Z]*}}F(float, float, float, float, %T8abitypes3FooC* swiftself) {{.*}} {
  // armv7k-watchos: define hidden double @_T08abitypes3FooC14getXFromNSRect{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, [4 x float]) unnamed_addr {{.*}} {
  dynamic func getXFromNSRect(_ r: NSRect) -> Double {
    return Double(r.origin.x)
  }

  // x86_64-macosx: define hidden swiftcc float @_T08abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}F(i64, i64, %T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-macosx: define hidden float @_T08abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, <2 x float>, <2 x float>) unnamed_addr {{.*}} {
  // armv7-ios: define hidden swiftcc float @_T08abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}F(float, float, float, float, %T8abitypes3FooC* swiftself) {{.*}} {
  // armv7-ios: define hidden float @_T08abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, [4 x i32]) unnamed_addr {{.*}} {
  // armv7s-ios: define hidden swiftcc float @_T08abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}F(float, float, float, float, %T8abitypes3FooC* swiftself) {{.*}} {
  // armv7s-ios: define hidden float @_T08abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, [4 x i32]) unnamed_addr {{.*}} {
  // armv7k-watchos: define hidden swiftcc float @_T08abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}F(float, float, float, float, %T8abitypes3FooC* swiftself) {{.*}} {
  // armv7k-watchos: define hidden float @_T08abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, [4 x float]) unnamed_addr {{.*}} {
  dynamic func getXFromRect(_ r: MyRect) -> Float {
    return r.x
  }

  // Call from Swift entrypoint with exploded Rect to @objc entrypoint
  // with unexploded ABI-coerced type.
  // x86_64-macosx: define hidden swiftcc float @_T08abitypes3FooC17getXFromRectSwift{{.*}}(i64, i64, [[SELF:%.*]]* swiftself) {{.*}} {
  // x86_64-macosx: [[COERCED:%.*]] = alloca [[MYRECT:%.*MyRect.*]], align 8
  // x86_64-macosx: [[SEL:%.*]] = load i8*, i8** @"\01L_selector(getXFromRect:)", align 8
  // x86_64-macosx: [[CAST:%.*]] = bitcast [[MYRECT]]* [[COERCED]] to { <2 x float>, <2 x float> }*
  // x86_64-macosx: [[T0:%.*]] = getelementptr inbounds { <2 x float>, <2 x float> }, { <2 x float>, <2 x float> }* [[CAST]], i32 0, i32 0
  // x86_64-macosx: [[FIRST_HALF:%.*]] = load <2 x float>, <2 x float>* [[T0]]
  // x86_64-macosx: [[T0:%.*]] = getelementptr inbounds { <2 x float>, <2 x float> }, { <2 x float>, <2 x float> }* [[CAST]], i32 0, i32 1
  // x86_64-macosx: [[SECOND_HALF:%.*]] = load <2 x float>, <2 x float>* [[T0]]
  // x86_64-macosx: [[SELFCAST:%.*]] = bitcast [[SELF]]* %2 to i8*
  // x86_64-macosx: [[RESULT:%.*]] = call float bitcast (void ()* @objc_msgSend to float (i8*, i8*,  <2 x float>, <2 x float>)*)(i8* [[SELFCAST]], i8* [[SEL]], <2 x float> [[FIRST_HALF]], <2 x float> [[SECOND_HALF]])
  // armv7-ios: define hidden swiftcc float @_T08abitypes3FooC17getXFromRectSwift{{[_0-9a-zA-Z]*}}F(float, float, float, float, [[SELF:%.*]]* swiftself) {{.*}} {
  // armv7-ios: [[COERCED:%.*]] = alloca [[MYRECT:%.*MyRect.*]], align 4
  // armv7-ios: [[SEL:%.*]] = load i8*, i8** @"\01L_selector(getXFromRect:)", align 4
  // armv7-ios: [[CAST:%.*]] = bitcast [[MYRECT]]* [[COERCED]] to [4 x i32]*
  // armv7-ios: [[LOADED:%.*]] = load [4 x i32], [4 x i32]* [[CAST]]
  // armv7-ios: [[SELFCAST:%.*]] = bitcast [[SELF]]* %4 to i8*
  // armv7-ios: [[RESULT:%.*]] = call float bitcast (void ()* @objc_msgSend to float (i8*, i8*, [4 x i32])*)(i8* [[SELFCAST]], i8* [[SEL]], [4 x i32] [[LOADED]])

  // armv7s-ios: define hidden swiftcc float @_T08abitypes3FooC17getXFromRectSwift{{[_0-9a-zA-Z]*}}F(float, float, float, float, [[SELF:%.*]]* swiftself) {{.*}} {
  // armv7s-ios: [[COERCED:%.*]] = alloca [[MYRECT:%.*MyRect.*]], align 4
  // armv7s-ios: [[SEL:%.*]] = load i8*, i8** @"\01L_selector(getXFromRect:)", align 4
  // armv7s-ios: [[CAST:%.*]] = bitcast [[MYRECT]]* [[COERCED]] to [4 x i32]*
  // armv7s-ios: [[LOADED:%.*]] = load [4 x i32], [4 x i32]* [[CAST]]
  // armv7s-ios: [[SELFCAST:%.*]] = bitcast [[SELF]]* %4 to i8*
  // armv7s-ios: [[RESULT:%.*]] = call float bitcast (void ()* @objc_msgSend to float (i8*, i8*, [4 x i32])*)(i8* [[SELFCAST]], i8* [[SEL]], [4 x i32] [[LOADED]])

  // armv7k-watchos: define hidden swiftcc float @_T08abitypes3FooC17getXFromRectSwift{{[_0-9a-zA-Z]*}}F(float, float, float, float, [[SELF:%.*]]* swiftself) {{.*}} {
  // armv7k-watchos: [[COERCED:%.*]] = alloca [[MYRECT:%.*MyRect.*]], align 4
  // armv7k-watchos: [[SEL:%.*]] = load i8*, i8** @"\01L_selector(getXFromRect:)", align 4
  // armv7k-watchos: [[CAST:%.*]] = bitcast [[MYRECT]]* [[COERCED]] to [4 x float]*
  // armv7k-watchos: [[LOADED:%.*]] = load [4 x float], [4 x float]* [[CAST]]
  // armv7k-watchos: [[SELFCAST:%.*]] = bitcast [[SELF]]* %4 to i8*
  // armv7k-watchos: [[RESULT:%.*]] = call float bitcast (void ()* @objc_msgSend to float (i8*, i8*, [4 x float])*)(i8* [[SELFCAST]], i8* [[SEL]], [4 x float] [[LOADED]])
  func getXFromRectSwift(_ r: MyRect) -> Float {
    return getXFromRect(r)
  }

  // Ensure that MyRect is passed as an indirect-byval on x86_64 because we run out of registers for direct arguments
  // x86_64-macosx: define hidden float @_T08abitypes3FooC25getXFromRectIndirectByVal{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, float, float, float, float, float, float, float, %TSC6MyRectV* byval align 4) unnamed_addr {{.*}} {
  dynamic func getXFromRectIndirectByVal(_: Float, second _: Float, 
                                       third _: Float, fourth _: Float,
                                       fifth _: Float, sixth _: Float,
                                       seventh _: Float, withRect r: MyRect)
               -> Float {
    return r.x
  }

  // Make sure the caller-side from Swift also uses indirect-byval for the argument
  // x86_64-macosx: define hidden swiftcc float @_T08abitypes3FooC25getXFromRectIndirectSwift{{[_0-9a-zA-Z]*}}F(i64, i64, %T8abitypes3FooC* swiftself) {{.*}} {
  func getXFromRectIndirectSwift(_ r: MyRect) -> Float {
    let f : Float = 1.0
    // x86_64-macosx: [[TEMP:%.*]] = alloca [[TEMPTYPE:%.*]], align 4
    // x86_64-macosx: [[RESULT:%.*]] = call float bitcast (void ()* @objc_msgSend to float (i8*, i8*, float, float, float, float, float, float, float, [[TEMPTYPE]]*)*)(i8* %{{.*}}, i8* %{{.*}}, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, [[TEMPTYPE]]* byval align 4 [[TEMP]])
    // x86_64-macosx: ret float [[RESULT]]
    return getXFromRectIndirectByVal(f, second: f, third: f, fourth: f, fifth: f, sixth: f, seventh: f, withRect: r);
  }

  // x86_64 returns an HA of four floats directly in two <2 x float>
  // x86_64-macosx:      define hidden swiftcc float @_T08abitypes3FooC4barc{{[_0-9a-zA-Z]*}}F(%TSo13StructReturnsC*, %T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-macosx:      load i8*, i8** @"\01L_selector(newRect)", align 8
  // x86_64-macosx:      [[RESULT:%.*]] = call { <2 x float>, <2 x float> } bitcast (void ()* @objc_msgSend
  // x86_64-macosx:      store { <2 x float>, <2 x float> } [[RESULT]]
  // x86_64-macosx:      [[CAST:%.*]] = bitcast { <2 x float>, <2 x float> }*
  // x86_64-macosx:      load { i64, i64 }, { i64, i64 }* [[CAST]]
  // x86_64-macosx:      ret float
  //
  // armv7 returns an HA of four floats indirectly
  // armv7-ios: define hidden swiftcc float @_T08abitypes3FooC4barc{{[_0-9a-zA-Z]*}}F(%TSo13StructReturnsC*, %T8abitypes3FooC* swiftself) {{.*}} {
  // armv7-ios: [[RESULT:%.*]] = alloca [[RECTTYPE:%.*MyRect.*]], align 4
  // armv7-ios: load i8*, i8** @"\01L_selector(newRect)", align 4
  // armv7-ios: call void bitcast (void ()* @objc_msgSend_stret to void ([[RECTTYPE]]*, [[RECEIVER:.*]]*, i8*)*)([[RECTTYPE]]* noalias nocapture sret %call.aggresult
  // armv7-ios: [[GEP1:%.*]] = getelementptr inbounds [[RECTTYPE]], [[RECTTYPE]]* [[RESULT]], i32 0, i32 1
  // armv7-ios: [[GEP2:%.*]] = getelementptr inbounds {{.*}}, {{.*}}* [[GEP1]], i32 0, i32 0
  // armv7-ios: [[RETVAL:%.*]] = load float, float* [[GEP2]], align 4
  // armv7-ios: ret float [[RETVAL]]
  //
  // armv7s returns an HA of four floats indirectly
  // armv7s-ios: define hidden swiftcc float @_T08abitypes3FooC4barc{{[_0-9a-zA-Z]*}}F(%TSo13StructReturnsC*, %T8abitypes3FooC* swiftself) {{.*}} {
  // armv7s-ios: [[RESULT:%.*]] = alloca [[RECTTYPE:%.*MyRect.*]], align 4
  // armv7s-ios: load i8*, i8** @"\01L_selector(newRect)", align 4
  // armv7s-ios: call void bitcast (void ()* @objc_msgSend_stret to void ([[RECTTYPE]]*, [[RECEIVER:.*]]*, i8*)*)([[RECTTYPE]]* noalias nocapture sret %call.aggresult
  // armv7s-ios: [[GEP1:%.*]] = getelementptr inbounds [[RECTTYPE]], [[RECTTYPE]]* [[RESULT]], i32 0, i32 1
  // armv7s-ios: [[GEP2:%.*]] = getelementptr inbounds {{.*}}, {{.*}}* [[GEP1]], i32 0, i32 0
  // armv7s-ios: [[RETVAL:%.*]] = load float, float* [[GEP2]], align 4
  // armv7s-ios: ret float [[RETVAL]]
  //
  // armv7k returns an HA of four floats directly
  // armv7k-watchos:      define hidden swiftcc float @_T08abitypes3FooC4barc{{[_0-9a-zA-Z]*}}F(%TSo13StructReturnsC*, %T8abitypes3FooC* swiftself) {{.*}} {
  // armv7k-watchos:      load i8*, i8** @"\01L_selector(newRect)", align 4
  // armv7k-watchos:      [[RESULT:%.*]] = call [[ARMV7K_MYRECT]] bitcast (void ()* @objc_msgSend
  // armv7k-watchos:      store [[ARMV7K_MYRECT]] [[RESULT]]
  // armv7k-watchos:      [[CAST:%.*]] = bitcast [[ARMV7K_MYRECT]]*
  // armv7k-watchos:      load { float, float, float, float }, { float, float, float, float }* [[CAST]]
  // armv7k-watchos:      ret float
  func barc(_ p: StructReturns) -> Float {
    return p.newRect().y
  }

  // x86_64-macosx: define hidden swiftcc { double, double, double } @_T08abitypes3FooC3baz{{[_0-9a-zA-Z]*}}F(%T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-macosx: define hidden void @_T08abitypes3FooC3baz{{[_0-9a-zA-Z]*}}FTo(%TSC4TrioV* noalias nocapture sret, i8*, i8*) unnamed_addr {{.*}} {
  dynamic func baz() -> Trio {
    return Trio(i: 1.0, j: 2.0, k: 3.0)
  }

  // x86_64-macosx:      define hidden swiftcc double @_T08abitypes3FooC4bazc{{[_0-9a-zA-Z]*}}F(%TSo13StructReturnsC*, %T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-macosx:      load i8*, i8** @"\01L_selector(newTrio)", align 8
  // x86_64-macosx:      [[CAST:%[0-9]+]] = bitcast {{%.*}}* %0
  // x86_64-macosx:      call void bitcast (void ()* @objc_msgSend_stret to void (%TSC4TrioV*, [[OPAQUE:.*]]*, i8*)*)
  func bazc(_ p: StructReturns) -> Double {
    return p.newTrio().j
  }

  // x86_64-macosx:      define hidden swiftcc i64 @_T08abitypes3FooC7getpair{{[_0-9a-zA-Z]*}}F(%TSo13StructReturnsC*, %T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-macosx:      [[RESULT:%.*]] = call i64 bitcast (void ()* @objc_msgSend to i64 ([[OPAQUE:.*]]*, i8*)*)
  // x86_64-macosx:      [[GEP1:%.*]] = getelementptr inbounds { i64 }, { i64 }* {{.*}}, i32 0, i32 0
  // x86_64-macosx:      store i64 [[RESULT]], i64* [[GEP1]]
  // x86_64-macosx:      [[GEP2:%.*]] = getelementptr inbounds { i64 }, { i64 }* {{.*}}, i32 0, i32 0
  // x86_64-macosx:      load i64, i64* [[GEP2]]
  // x86_64-macosx:      ret i64
  func getpair(_ p: StructReturns) -> IntPair {
    return p.newPair()
  }

  // x86_64-macosx:      define hidden i64 @_T08abitypes3FooC8takepair{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i64) unnamed_addr {{.*}} {
  dynamic func takepair(_ p: IntPair) -> IntPair {
    return p
  }

  // x86_64-macosx:      define hidden swiftcc i64 @_T08abitypes3FooC9getnested{{[_0-9a-zA-Z]*}}F(%TSo13StructReturnsC*, %T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-macosx:      call i64 bitcast (void ()* @objc_msgSend to i64 ([[OPAQUE:.*]]*, i8*)*)
  // x86_64-macosx:      bitcast
  // x86_64-macosx:      call void @llvm.lifetime.start
  // x86_64-macosx:      store i32 {{.*}}
  // x86_64-macosx:      store i32 {{.*}}
  // x86_64-macosx:      getelementptr inbounds { i64 }, { i64 }
  // x86_64-macosx:      load i64, i64* %12, align 8
  // x86_64-macosx:      bitcast
  // x86_64-macosx:      call void @llvm.lifetime.end
  // x86_64-macosx:      ret i64
  func getnested(_ p: StructReturns) -> NestedInts {
    return p.newNestedInts()
  }

  // x86_64-macosx:      define hidden i8* @_T08abitypes3FooC9copyClass{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i8*) unnamed_addr {{.*}} {
  // x86_64-macosx:      [[VALUE:%[0-9]+]] = call swiftcc [[TYPE:%.*]]* @_T08abitypes3FooC9copyClass{{[_0-9a-zA-Z]*}}F
  // x86_64-macosx:      [[T0:%.*]] = phi [[TYPE]]* [ [[VALUE]],
  // x86_64-macosx:      [[T1:%.*]] = bitcast [[TYPE]]* [[T0]] to [[OBJC:%objc_class]]*
  // x86_64-macosx:      [[RESULT:%[0-9]+]] = bitcast [[OBJC]]* [[T1]] to i8*
  // x86_64-macosx:      ret i8* [[RESULT]]
  dynamic func copyClass(_ a: AnyClass) -> AnyClass {
    return a
  }

  // x86_64-macosx:      define hidden i8* @_T08abitypes3FooC9copyProto{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i8*) unnamed_addr {{.*}} {
  // x86_64-macosx:      [[VALUE:%[0-9]+]] = call swiftcc [[TYPE:%.*]] @_T08abitypes3FooC9copyProto{{[_0-9a-zA-Z]*}}F
  // x86_64-macosx:      [[RESULT:%[0-9]+]] = bitcast [[TYPE]] [[VALUE]] to i8*
  // x86_64-macosx:      ret i8* [[RESULT]]
  dynamic func copyProto(_ a: AnyObject) -> AnyObject {
    return a
  }

  // x86_64-macosx:      define hidden i8* @_T08abitypes3FooC13copyProtoComp{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i8*) unnamed_addr {{.*}} {
  // x86_64-macosx:      [[VALUE:%[0-9]+]] = call swiftcc [[TYPE:%.*]] @_T08abitypes3FooC13copyProtoComp{{[_0-9a-zA-Z]*}}F
  // x86_64-macosx:      [[RESULT:%[0-9]+]] = bitcast [[TYPE]] [[VALUE]] to i8*
  // x86_64-macosx:      ret i8* [[RESULT]]
  dynamic func copyProtoComp(_ a: P1 & P2) -> P1 & P2 {
    return a
  }

  // x86_64-macosx:       define hidden swiftcc i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-macosx:       define hidden signext i8 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i8 signext) unnamed_addr {{.*}} {
  // x86_64-macosx:       [[R1:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F
  // x86_64-macosx:       [[R2:%[0-9]+]] = call swiftcc i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F
  // x86_64-macosx:       [[R3:%[0-9]+]] = call swiftcc i8 @_T010ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F(i1 [[R2]]
  // x86_64-macosx:       ret i8 [[R3]]
  //
  // x86_64-ios-fixme:          define hidden i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC*) {{.*}} {
  // x86_64-ios-fixme:          define internal zeroext i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo
  // x86_64-ios-fixme:          [[R1:%[0-9]+]] = call i1 @_T010ObjectiveC22_convertObjCBoolToBoolSbAA0cD0V1x_tF(i1 %2)
  // x86_64-ios-fixme:          [[R2:%[0-9]+]] = call i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F(i1 [[R1]]
  // x86_64-ios-fixme:          [[R3:%[0-9]+]] = call i1 @_T010ObjectiveC22_convertBoolToObjCBoolAA0eF0VSb1x_tF(i1 [[R2]])
  // x86_64-ios-fixme:          ret i1 [[R3]]
  //
  // armv7-ios-fixme:     define hidden i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC*) {{.*}} {
  // armv7-ios-fixme:     define internal signext i8 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i8 signext) unnamed_addr {{.*}} {
  // armv7-ios-fixme:     [[R1:%[0-9]+]] = call i1 @_T010ObjectiveC22_convertObjCBoolToBoolSbAA0cD0V1x_tF
  // armv7-ios-fixme:     [[R2:%[0-9]+]] = call i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F(i1 [[R1]]
  // armv7-ios-fixme:     [[R3:%[0-9]+]] = call i8 @_T010ObjectiveC22_convertBoolToObjCBoolAA0eF0VSb1x_tF(i1 [[R2]]
  // armv7-ios-fixme:     ret i8 [[R3]]
  //
  // armv7s-ios-fixme:     define hidden i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC*) {{.*}} {
  // armv7s-ios-fixme:     define internal signext i8 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i8 signext) unnamed_addr {{.*}} {
  // armv7s-ios-fixme:     [[R1:%[0-9]+]] = call i1 @_T010ObjectiveC22_convertObjCBoolToBoolSbAA0cD0V1x_tF
  // armv7s-ios-fixme:     [[R2:%[0-9]+]] = call i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F(i1 [[R1]]
  // armv7s-ios-fixme:     [[R3:%[0-9]+]] = call i8 @_T010ObjectiveC22_convertBoolToObjCBoolAA0eF0VSb1x_tF(i1 [[R2]]
  // armv7s-ios-fixme:     ret i8 [[R3]]
  //
  // arm64-ios-fixme:     define hidden i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC*) {{.*}} {
  // arm64-ios-fixme:     define internal zeroext i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo
  // arm64-ios-fixme:     [[R2:%[0-9]+]] = call i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F
  // arm64-ios-fixme:     ret i1 [[R2]]
  //
  // i386-ios-fixme:      define hidden i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC*) {{.*}} {
  // i386-ios-fixme:      define internal signext i8 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i8 signext) unnamed_addr {{.*}} {
  // i386-ios-fixme:     [[R1:%[0-9]+]] = call i1 @_T010ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F
  // i386-ios-fixme:     [[R2:%[0-9]+]] = call i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F(i1 [[R1]]
  // i386-ios-fixme:     [[R3:%[0-9]+]] = call i8 @_T010ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F(i1 [[R2]]
  // i386-ios-fixme:     ret i8 [[R3]]
  //
  // x86_64-tvos-fixme:          define hidden i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC*) {{.*}} {
  // x86_64-tvos-fixme:          define internal zeroext i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo
  // x86_64-tvos-fixme:          [[R1:%[0-9]+]] = call i1 @_T010ObjectiveC22_convertObjCBoolToBoolSbAA0cD0V1x_tF(i1 %2)
  // x86_64-tvos-fixme:          [[R2:%[0-9]+]] = call i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F(i1 [[R1]]
  // x86_64-tvos-fixme:          [[R3:%[0-9]+]] = call i1 @_T010ObjectiveC22_convertBoolToObjCBoolAA0eF0VSb1x_tF(i1 [[R2]])
  // x86_64-tvos-fixme:          ret i1 [[R3]]
  //
  // arm64-tvos-fixme:     define hidden i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC*) {{.*}} {
  // arm64-tvos-fixme:     define internal zeroext i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo
  // arm64-tvos-fixme:     [[R2:%[0-9]+]] = call i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F
  // arm64-tvos-fixme:     ret i1 [[R2]]

  // i386-watchos:  define hidden swiftcc i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC* swiftself)
  // i386-watchos:  define hidden zeroext i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo
  // i386-watchos:  [[R1:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertObjCBoolToBoolSbAA0cD0VF(i1 %2)
  // i386-watchos:  [[R2:%[0-9]+]] = call swiftcc i1 @_T08abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F(i1 [[R1]]
  // i386-watchos:  [[R3:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertBoolToObjCBoolAA0eF0VSbF(i1 [[R2]])
  // i386-watchos:  ret i1 [[R3]]

  dynamic func negate(_ b: Bool) -> Bool {
    return !b
  }

  // x86_64-macosx: define hidden swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-macosx: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i8 @_T010ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F(i1 %0)
  // x86_64-macosx: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 8
  // x86_64-macosx: [[NEG:%[0-9]+]] = call signext i8 bitcast (void ()* @objc_msgSend to i8 ([[RECEIVER:.*]]*, i8*, i8)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i8 signext [[TOOBJCBOOL]])
  // x86_64-macosx: [[TOBOOL:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F(i8 [[NEG]])
  // x86_64-macosx: ret i1 [[TOBOOL]]
  //
  // x86_64-macosx: define hidden signext i8 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i8 signext)
  // x86_64-macosx: [[TOBOOL:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F
  // x86_64-macosx: [[NEG:%[0-9]+]] = call swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1 [[TOBOOL]]
  // x86_64-macosx: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i8 @_T010ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F(i1 [[NEG]])
  // x86_64-macosx: ret i8 [[TOOBJCBOOL]]
  //
  // x86_64-ios: define hidden swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-ios: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 8
  // x86_64-ios: [[NEG:%[0-9]+]] = call zeroext i1 bitcast (void ()* @objc_msgSend to i1 ([[RECEIVER:.*]]*, i8*, i1)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i1 zeroext %0)
  // x86_64-ios: ret i1 [[NEG]]
  //
  // x86_64-ios: define hidden zeroext i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i1 zeroext)
  // x86_64-ios: [[NEG:%[0-9]+]] = call swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1
  // x86_64-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F(i1 [[NEG]])
  // x86_64-ios: ret i1 [[TOOBJCBOOL]]
  //
  // armv7-ios: define hidden swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC* swiftself) {{.*}} {
  // armv7-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i8 @_T010ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F(i1 %0)
  // armv7-ios: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 4
  // armv7-ios: [[NEG:%[0-9]+]] = call signext i8 bitcast (void ()* @objc_msgSend to i8 ([[RECEIVER:.*]]*, i8*, i8)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i8 signext [[TOOBJCBOOL]])
  // armv7-ios: [[TOBOOL:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F(i8 [[NEG]])
  // armv7-ios: ret i1 [[TOBOOL]]
  //
  // armv7-ios: define hidden signext i8 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i8 signext)
  // armv7-ios: [[TOBOOL:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F
  // armv7-ios: [[NEG:%[0-9]+]] = call swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1 [[TOBOOL]]
  // armv7-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i8 @_T010ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F(i1 [[NEG]])
  // armv7-ios: ret i8 [[TOOBJCBOOL]]
  //
  // armv7s-ios: define hidden swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC* swiftself) {{.*}} {
  // armv7s-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i8 @_T010ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F(i1 %0)
  // armv7s-ios: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 4
  // armv7s-ios: [[NEG:%[0-9]+]] = call signext i8 bitcast (void ()* @objc_msgSend to i8 ([[RECEIVER:.*]]*, i8*, i8)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i8 signext [[TOOBJCBOOL]])
  // armv7s-ios: [[TOBOOL:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F(i8 [[NEG]])
  // armv7s-ios: ret i1 [[TOBOOL]]
  //
  // armv7s-ios: define hidden signext i8 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i8 signext)
  // armv7s-ios: [[TOBOOL:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F
  // armv7s-ios: [[NEG:%[0-9]+]] = call swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1 [[TOBOOL]]
  // armv7s-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i8 @_T010ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F(i1 [[NEG]])
  // armv7s-ios: ret i8 [[TOOBJCBOOL]]
  //
  // arm64-ios: define hidden swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC* swiftself) {{.*}} {
  // arm64-ios: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 8
  // arm64-ios: [[NEG:%[0-9]+]] = call zeroext i1 bitcast (void ()* @objc_msgSend to i1 ([[RECEIVER:.*]]*, i8*, i1)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i1 zeroext %0)
  // arm64-ios: ret i1 [[NEG]]
  //
  // arm64-ios: define hidden zeroext i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i1 zeroext)
  // arm64-ios: [[NEG:%[0-9]+]] = call swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1
  // arm64-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F(i1 [[NEG]])
  // arm64-ios: ret i1 [[TOOBJCBOOL]]
  //
  // i386-ios: define hidden swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC* swiftself) {{.*}} {
  // i386-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i8 @_T010ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F(i1 %0)
  // i386-ios: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 4
  // i386-ios: [[NEG:%[0-9]+]] = call signext i8 bitcast (void ()* @objc_msgSend to i8 ([[RECEIVER:.*]]*, i8*, i8)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i8 signext [[TOOBJCBOOL]])
  // i386-ios: [[TOBOOL:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F(i8 [[NEG]])
  // i386-ios: ret i1 [[TOBOOL]]
  //
  // i386-ios: define hidden signext i8 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i8 signext)
  // i386-ios: [[TOBOOL:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F
  // i386-ios: [[NEG:%[0-9]+]] = call swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1 [[TOBOOL]]
  // i386-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i8 @_T010ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F(i1 [[NEG]])
  // i386-ios: ret i8 [[TOOBJCBOOL]]
  //
  // x86_64-tvos: define hidden swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-tvos: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 8
  // x86_64-tvos: [[NEG:%[0-9]+]] = call zeroext i1 bitcast (void ()* @objc_msgSend to i1 ([[RECEIVER:.*]]*, i8*, i1)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i1 zeroext %0)
  // x86_64-tvos: ret i1 [[NEG]]
  //
  // x86_64-tvos: define hidden zeroext i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i1 zeroext)
  // x86_64-tvos: [[NEG:%[0-9]+]] = call swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1
  // x86_64-tvos: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F(i1 [[NEG]])
  // x86_64-tvos: ret i1 [[TOOBJCBOOL]]
  //
  // arm64-tvos: define hidden swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC* swiftself) {{.*}} {
  // arm64-tvos: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 8
  // arm64-tvos: [[NEG:%[0-9]+]] = call zeroext i1 bitcast (void ()* @objc_msgSend to i1 ([[RECEIVER:.*]]*, i8*, i1)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i1 zeroext %0)
  // arm64-tvos: ret i1 [[NEG]]
  //
  // arm64-tvos: define hidden zeroext i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i1 zeroext)
  // arm64-tvos: [[NEG:%[0-9]+]] = call swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1
  // arm64-tvos: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F(i1 [[NEG]])
  // arm64-tvos: ret i1 [[TOOBJCBOOL]]

  // i386-watchos: define hidden swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC* swiftself) {{.*}} {
  // i386-watchos: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 4
  // i386-watchos: [[NEG:%[0-9]+]] = call zeroext i1 bitcast (void ()* @objc_msgSend to i1 ([[RECEIVER:.*]]*, i8*, i1)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i1 zeroext %0)
  // i386-watchos: ret i1 [[NEG]]
  //
  // i386-watchos: define hidden zeroext i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i1 zeroext)
  // i386-watchos: [[NEG:%[0-9]+]] = call swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1
  // i386-watchos: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F(i1 [[NEG]])
  // i386-watchos: ret i1 [[TOOBJCBOOL]]
  //
  // armv7k-watchos: define hidden swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1, %T8abitypes3FooC* swiftself) {{.*}} {
  // armv7k-watchos: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negate:)", align 4
  // armv7k-watchos: [[NEG:%[0-9]+]] = call zeroext i1 bitcast (void ()* @objc_msgSend to i1 ([[RECEIVER:.*]]*, i8*, i1)*)([[RECEIVER]]* {{%[0-9]+}}, i8* [[SEL]], i1 zeroext %0)
  // armv7k-watchos: ret i1 [[NEG]]
  //
  // armv7k-watchos: define hidden zeroext i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i1 zeroext)
  // armv7k-watchos: [[NEG:%[0-9]+]] = call swiftcc i1 @_T08abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F(i1
  // armv7k-watchos: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i1 @_T010ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F(i1 [[NEG]])
  // armv7k-watchos: ret i1 [[TOOBJCBOOL]]
  //
  dynamic func negate2(_ b: Bool) -> Bool {
    var g = Gadget()
    return g.negate(b)
  }

  // x86_64-macosx: define hidden swiftcc i1 @_T08abitypes3FooC7negate3S2bF(i1, %T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-macosx: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(invert:)", align 8
  // x86_64-macosx: [[NEG:%[0-9]+]] = call zeroext i1 bitcast (void ()* @objc_msgSend to i1 (%1*, i8*, i1)*)(%1* [[RECEIVER:%[0-9]+]], i8* [[SEL]], i1 zeroext %0)
  // x86_64-macosx: ret i1 [[NEG]]
  // x86_64-macosx: }

  // x86_64-ios: define hidden swiftcc i1 @_T08abitypes3FooC7negate3S2bF(i1, %T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-ios: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(invert:)", align 8
  // x86_64-ios: [[NEG:%[0-9]+]] = call zeroext i1 bitcast (void ()* @objc_msgSend to i1 (%1*, i8*, i1)*)(%1* [[RECEIVER:%[0-9]+]], i8* [[SEL]], i1 zeroext %0)
  // x86_64-ios: ret i1 [[NEG]]
  // x86_64-ios: }

  // i386-ios: define hidden swiftcc i1 @_T08abitypes3FooC7negate3S2bF(i1, %T8abitypes3FooC* swiftself) {{.*}} {
  // i386-ios: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(invert:)", align 4
  // i386-ios: [[NEG:%[0-9]+]] = call zeroext i1 bitcast (void ()* @objc_msgSend to i1 (%1*, i8*, i1)*)(%1* [[RECEIVER:%[0-9]+]], i8* [[SEL]], i1 zeroext %0)
  // i386-ios: ret i1 [[NEG]]
  // i386-ios: }

  dynamic func negate3(_ b: Bool) -> Bool {
    var g = Gadget()
    return g.invert(b)
  }

  // x86_64-macosx: define hidden swiftcc void @_T08abitypes3FooC10throwsTestySbKF(i1, %T8abitypes3FooC* swiftself, %swift.error** swifterror) {{.*}} {
  // x86_64-macosx: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negateThrowing:error:)", align 8
  // x86_64-macosx: call signext i8 bitcast (void ()* @objc_msgSend to i8 (%1*, i8*, i8, %2**)*)(%1* {{%[0-9]+}}, i8* [[SEL]], i8 signext {{%[0-9]+}}, %2** {{%[0-9]+}})
  // x86_64-macosx: }

  // x86_64-ios: define hidden swiftcc void @_T08abitypes3FooC10throwsTestySbKF(i1, %T8abitypes3FooC* swiftself, %swift.error** swifterror) {{.*}} {
  // x86_64-ios: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negateThrowing:error:)", align 8
  // x86_64-ios: call zeroext i1 bitcast (void ()* @objc_msgSend to i1 (%1*, i8*, i1, %2**)*)(%1* {{%[0-9]+}}, i8* [[SEL]], i1 zeroext {{%[0-9]+}}, %2** {{%[0-9]+}})
  // x86_64-ios: }

  // i386-ios: define hidden swiftcc void @_T08abitypes3FooC10throwsTestySbKF(i1, %T8abitypes3FooC* swiftself, %swift.error**) {{.*}} {
  // i386-ios: [[SEL:%[0-9]+]] = load i8*, i8** @"\01L_selector(negateThrowing:error:)", align 4
  // i386-ios: call signext i8 bitcast (void ()* @objc_msgSend to i8 (%1*, i8*, i8, %2**)*)(%1* {{%[0-9]+}}, i8* [[SEL]], i8 signext {{%[0-9]+}}, %2** {{%[0-9]+}})
  // i386-ios: }
  dynamic func throwsTest(_ b: Bool) throws {
    var g = Gadget()
    try g.negateThrowing(b)
  }

  // x86_64-macosx: define hidden i32* @_T08abitypes3FooC24copyUnsafeMutablePointer{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i32*) unnamed_addr {{.*}} {
  dynamic func copyUnsafeMutablePointer(_ p: UnsafeMutablePointer<Int32>) -> UnsafeMutablePointer<Int32> {
    return p
  }

  // x86_64-macosx: define hidden i64 @_T08abitypes3FooC17returnNSEnumValue{{[_0-9a-zA-Z]*}}FTo(i8*, i8*) unnamed_addr {{.*}} {
  dynamic func returnNSEnumValue() -> ByteCountFormatter.CountStyle {
    return .file
  }

  // x86_64-macosx: define hidden zeroext i16 @_T08abitypes3FooC20returnOtherEnumValue{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i16 zeroext) unnamed_addr {{.*}} {
  dynamic func returnOtherEnumValue(_ choice: ChooseTo) -> ChooseTo {
    switch choice {
      case .takeIt: return .leaveIt
      case .leaveIt: return .takeIt
    }
  }

  // x86_64-macosx: define hidden swiftcc i32 @_T08abitypes3FooC10getRawEnum{{[_0-9a-zA-Z]*}}F(%T8abitypes3FooC* swiftself) {{.*}} {
  // x86_64-macosx: define hidden i32 @_T08abitypes3FooC10getRawEnum{{[_0-9a-zA-Z]*}}FTo(i8*, i8*) unnamed_addr {{.*}} {
  dynamic func getRawEnum() -> RawEnum {
    return Intergalactic
  }

  var work : Work
  init (work: Work) {
    self.work = work
  }

  // x86_64-macosx: define hidden void @_T08abitypes3FooC13testArchetype{{[_0-9a-zA-Z]*}}FTo(i8*, i8*, i8*) unnamed_addr {{.*}} {
  dynamic func testArchetype(_ work: Work) {
    work.doStuff(1)
    // x86_64-macosx: [[OBJCPTR:%.*]] = bitcast i8* %2 to %objc_object*
    // x86_64-macosx: call swiftcc void @_T08abitypes3FooC13testArchetype{{[_0-9a-zA-Z]*}}F(%objc_object* [[OBJCPTR]], %T8abitypes3FooC* swiftself %{{.*}})
  }

  dynamic func foo(_ x: @convention(block) (Int) -> Int) -> Int {
    // FIXME: calling blocks is currently unimplemented
    // return x(5)
    return 1
  }

  // x86_64-macosx: define hidden swiftcc void @_T08abitypes3FooC20testGenericTypeParam{{[_0-9a-zA-Z]*}}F(%objc_object*, %swift.type* %T, %T8abitypes3FooC* swiftself) {{.*}} {
  func testGenericTypeParam<T: Pasta>(_ x: T) {
    // x86_64-macosx: [[CAST:%.*]] = bitcast %objc_object* %0 to i8*
    // x86_64-macosx: call void bitcast (void ()* @objc_msgSend to void (i8*, i8*)*)(i8* [[CAST]], i8* %{{.*}})
    x.alDente()
  }

  // arm64-ios: define hidden swiftcc { i64, i64, i64, i64 } @_T08abitypes3FooC14callJustReturn{{[_0-9a-zA-Z]*}}F(%TSo13StructReturnsC*, i64, i64, i64, i64, %T8abitypes3FooC* swiftself) {{.*}} {
  // arm64-ios: define hidden void @_T08abitypes3FooC14callJustReturn{{[_0-9a-zA-Z]*}}FTo(%TSC9BigStructV* noalias nocapture sret, i8*, i8*, [[OPAQUE:.*]]*, %TSC9BigStructV*) unnamed_addr {{.*}} {
  //
  // arm64-tvos: define hidden swiftcc { i64, i64, i64, i64 } @_T08abitypes3FooC14callJustReturn{{[_0-9a-zA-Z]*}}F(%TSo13StructReturnsC*, i64, i64, i64, i64, %T8abitypes3FooC* swiftself) {{.*}} {
  // arm64-tvos: define hidden void @_T08abitypes3FooC14callJustReturn{{[_0-9a-zA-Z]*}}FTo(%TSC9BigStructV* noalias nocapture sret, i8*, i8*, [[OPAQUE:.*]]*, %TSC9BigStructV*) unnamed_addr {{.*}} {
  dynamic func callJustReturn(_ r: StructReturns, with v: BigStruct) -> BigStruct {
    return r.justReturn(v)
  }

  // Test that the makeOne() that we generate somewhere below doesn't
  // use arm_aapcscc for armv7.
  func callInline() -> Float {
    return makeOne(3,5).second
  }
}

// armv7-ios: define internal void @makeOne(%struct.One* noalias sret %agg.result, float %f, float %s)
// armv7s-ios: define internal void @makeOne(%struct.One* noalias sret %agg.result, float %f, float %s)
// armv7k-watchos: define internal %struct.One @makeOne(float %f, float %s)

// rdar://17631440 - Expand direct arguments that are coerced to aggregates.
// x86_64-macosx: define{{( protected)?}} swiftcc float @_T08abitypes13testInlineAggSfSC6MyRectVF(i64, i64) {{.*}} {
// x86_64-macosx: [[COERCED:%.*]] = alloca %TSC6MyRectV, align 8
// x86_64-macosx: store i64 %
// x86_64-macosx: store i64 %
// x86_64-macosx: [[CAST:%.*]] = bitcast %TSC6MyRectV* [[COERCED]] to { <2 x float>, <2 x float> }*
// x86_64-macosx: [[T0:%.*]] = getelementptr inbounds { <2 x float>, <2 x float> }, { <2 x float>, <2 x float> }* [[CAST]], i32 0, i32 0
// x86_64-macosx: [[FIRST_HALF:%.*]] = load <2 x float>, <2 x float>* [[T0]], align 8
// x86_64-macosx: [[T0:%.*]] = getelementptr inbounds { <2 x float>, <2 x float> }, { <2 x float>, <2 x float> }* [[CAST]], i32 0, i32 1
// x86_64-macosx: [[SECOND_HALF:%.*]] = load <2 x float>, <2 x float>* [[T0]], align 8
// x86_64-macosx: [[RESULT:%.*]] = call float @MyRect_Area(<2 x float> [[FIRST_HALF]], <2 x float> [[SECOND_HALF]])
// x86_64-macosx: ret float [[RESULT]]
public func testInlineAgg(_ rect: MyRect) -> Float {
  return MyRect_Area(rect)
}

// We need to allocate enough memory on the stack to hold the argument value we load.
// arm64-ios: define swiftcc void @_T08abitypes14testBOOLStructyyF()
// arm64-ios:  [[COERCED:%.*]] = alloca i64
// arm64-ios:  [[STRUCTPTR:%.*]] = bitcast i64* [[COERCED]] to %TSC14FiveByteStructV
// arm64-ios:  [[PTR0:%.*]] = getelementptr inbounds %TSC14FiveByteStructV, %TSC14FiveByteStructV* [[STRUCTPTR]], {{i.*}} 0, {{i.*}} 0
// arm64-ios:  [[PTR1:%.*]] = getelementptr inbounds %T10ObjectiveC8ObjCBoolV, %T10ObjectiveC8ObjCBoolV* [[PTR0]], {{i.*}} 0, {{i.*}} 0
// arm64-ios:  [[PTR2:%.*]] = getelementptr inbounds %TSb, %TSb* [[PTR1]], {{i.*}} 0, {{i.*}} 0
// arm64-ios:  store i1 false, i1* [[PTR2]], align 8
// arm64-ios:  [[ARG:%.*]] = load i64, i64* [[COERCED]]
// arm64-ios:  call void bitcast (void ()* @objc_msgSend to void (i8*, i8*, i64)*)(i8* {{.*}}, i8* {{.*}}, i64 [[ARG]])
public func testBOOLStruct() {
  let s = FiveByteStruct()
  MyClass.mymethod(s)
}

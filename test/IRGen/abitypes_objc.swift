// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi %s -emit-ir -module-name abitypes -enable-objc-interop | %FileCheck -check-prefix=%target-cpu-%target-os-abi %s

// REQUIRES: objc_interop

import gadget
import c_gadget
import Foundation

@objc protocol P1 {}
@objc protocol P2 {}
@objc protocol Work {
  func doStuff(_ x: Int64)
}

// armv7s-ios: [[ARMV7S_MYRECT:%.*]] = type { float, float, float, float }
// arm64-ios: [[ARM64_MYRECT:%.*]] = type { float, float, float, float }
// arm64e-ios: [[ARM64E_MYRECT:%.*]] = type { float, float, float, float }
// arm64-tvos: [[ARM64_MYRECT:%.*]] = type { float, float, float, float }
// armv7k-watchos: [[ARMV7K_MYRECT:%.*]] = type { float, float, float, float }
// arm64_32-watchos: [[ARM64_MYRECT:%.*]] = type { float, float, float, float }
// arm64-watchos: [[ARM64_MYRECT:%.*]] = type { float, float, float, float }
// arm64-macosx: [[ARM64_MYRECT:%.*]] = type { float, float, float, float }
// arm64-xros: [[ARM64_MYRECT:%.*]] = type { float, float, float, float }

class Foo {
  // x86_64-macosx: define hidden swiftcc { float, float, float, float } @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F"(ptr swiftself %0) {{.*}} {
  // x86_64-macosx: define internal { <2 x float>, <2 x float> } @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1) {{[#0-9]*}} {
  // x86_64-ios: define hidden swiftcc { float, float, float, float } @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F"(ptr swiftself %0) {{.*}} {
  // x86_64-ios: define internal { <2 x float>, <2 x float> } @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1) {{[#0-9]*}} {
  // i386-ios: define hidden swiftcc void @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F"(ptr noalias nocapture sret({{.*}}) %0, ptr swiftself %1) {{.*}} {
  // i386-ios: define internal void @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo"(ptr noalias nocapture sret({{.*}}) %0, ptr %1, ptr %2) {{[#0-9]*}} {
  // armv7-ios: define hidden swiftcc { float, float, float, float } @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F"(ptr swiftself %0) {{.*}} {
  // armv7-ios: define internal void @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo"(ptr noalias nocapture sret({{.*}}) %0, ptr %1, ptr %2) {{[#0-9]*}} {
  // armv7s-ios: define hidden swiftcc { float, float, float, float } @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F"(ptr swiftself %0) {{.*}} {
  // armv7s-ios: define internal void @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo"(ptr noalias nocapture sret({{.*}}) %0, ptr %1, ptr %2) {{[#0-9]*}} {
  // arm64-ios: define hidden swiftcc { float, float, float, float } @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F"(ptr swiftself %0) {{.*}} {
  // arm64-ios: define internal [[ARM64_MYRECT]] @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1) {{[#0-9]*}} {
  // x86_64-tvos: define hidden swiftcc { float, float, float, float } @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F"(ptr swiftself %0) {{.*}} {
  // x86_64-tvos: define internal { <2 x float>, <2 x float> } @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1) {{[#0-9]*}} {
  // arm64-tvos: define hidden swiftcc { float, float, float, float }  @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F"(ptr swiftself %0) {{.*}} {
  // arm64-tvos: define internal [[ARM64_MYRECT]] @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1) {{[#0-9]*}} {
  // i386-watchos: define hidden swiftcc void @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F"(ptr noalias nocapture sret({{.*}}) %0, ptr swiftself %1) {{.*}} {
  // i386-watchos: define internal void @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo"(ptr noalias nocapture sret({{.*}}) %0, ptr %1, ptr %2) {{[#0-9]*}} {
  // armv7k-watchos: define hidden swiftcc { float, float, float, float } @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F"(ptr swiftself %0) {{.*}} {
  // armv7k-watchos: define internal [[ARMV7K_MYRECT]] @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1) {{[#0-9]*}} {
  // armv64_32-watchos: define hidden swiftcc { float, float, float, float } @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F"(ptr swiftself %0) {{.*}} {
  // armv64_32-watchos: define internal [[ARMV7K_MYRECT]] @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1) {{[#0-9]*}} {
  // arm64-watchos: define hidden swiftcc { float, float, float, float }  @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F"(ptr swiftself %0) {{.*}} {
  // arm64-watchos: define internal [[ARM64_MYRECT]] @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1) {{[#0-9]*}} {
  // x86_64-watchos: define hidden swiftcc { float, float, float, float } @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F"(ptr swiftself %0) {{.*}} {
  // x86_64-watchos: define internal { <2 x float>, <2 x float> } @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1) {{[#0-9]*}} {
  // arm64-xros: define hidden swiftcc { float, float, float, float } @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}F"(ptr swiftself %0) {{.*}} {
  // arm64-xros: define internal [[ARM64_MYRECT]] @"$s8abitypes3FooC3bar{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1) {{[#0-9]*}} {
  @objc dynamic func bar() -> MyRect {
    return MyRect(x: 1, y: 2, width: 3, height: 4)
  }


  // x86_64-macosx: define hidden swiftcc double @"$s8abitypes3FooC14getXFromNSRect{{[_0-9a-zA-Z]*}}F"(double %0, double %1, double %2, double %3, ptr swiftself %4) {{.*}} {
  // x86_64-macosx: define internal double @"$s8abitypes3FooC14getXFromNSRect{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, ptr byval({{.*}}) align 8 %2) {{[#0-9]*}} {
  // armv7-ios: define hidden swiftcc double @"$s8abitypes3FooC14getXFromNSRect{{[_0-9a-zA-Z]*}}F"(float %0, float %1, float %2, float %3, ptr swiftself %4) {{.*}} {
  // armv7-ios: define internal double @"$s8abitypes3FooC14getXFromNSRect{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, [4 x i32] %2) {{[#0-9]*}} {
  // armv7s-ios: define hidden swiftcc double @"$s8abitypes3FooC14getXFromNSRect{{[_0-9a-zA-Z]*}}F"(float %0, float %1, float %2, float %3, ptr swiftself %4) {{.*}} {
  // armv7s-ios: define internal double @"$s8abitypes3FooC14getXFromNSRect{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, [4 x i32] %2) {{[#0-9]*}} {
  // armv7k-watchos: define hidden swiftcc double @"$s8abitypes3FooC14getXFromNSRect{{[_0-9a-zA-Z]*}}F"(float %0, float %1, float %2, float %3, ptr swiftself %4) {{.*}} {
  // armv7k-watchos: define internal double @"$s8abitypes3FooC14getXFromNSRect{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, [4 x float] %2) {{[#0-9]*}} {
  @objc dynamic func getXFromNSRect(_ r: NSRect) -> Double {
    return Double(r.origin.x)
  }

  // x86_64-macosx: define hidden swiftcc float @"$s8abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}F"(float %0, float %1, float %2, float %3, ptr swiftself %4) {{.*}} {
  // x86_64-macosx: define internal float @"$s8abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, <2 x float> %2, <2 x float> %3) {{[#0-9]*}} {
  // armv7-ios: define hidden swiftcc float @"$s8abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}F"(float %0, float %1, float %2, float %3, ptr swiftself %4) {{.*}} {
  // armv7-ios: define internal float @"$s8abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, [4 x i32] %2) {{[#0-9]*}} {
  // armv7s-ios: define hidden swiftcc float @"$s8abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}F"(float %0, float %1, float %2, float %3, ptr swiftself %4) {{.*}} {
  // armv7s-ios: define internal float @"$s8abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, [4 x i32] %2) {{[#0-9]*}} {
  // armv7k-watchos: define hidden swiftcc float @"$s8abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}F"(float %0, float %1, float %2, float %3, ptr swiftself %4) {{.*}} {
  // armv7k-watchos: define internal float @"$s8abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, [4 x float] %2) {{[#0-9]*}} {
  // arm64_32-watchos: define hidden swiftcc float @"$s8abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}F"(float %0, float %1, float %2, float %3, ptr swiftself %4) {{.*}} {
  // arm64_32-watchos: define internal float @"$s8abitypes3FooC12getXFromRect{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, [4 x float] %2) {{[#0-9]*}} {
  @objc dynamic func getXFromRect(_ r: MyRect) -> Float {
    return r.x
  }

  // Call from Swift entrypoint with exploded Rect to @objc entrypoint
  // with unexploded ABI-coerced type.
  // x86_64-macosx: define hidden swiftcc float @"$s8abitypes3FooC17getXFromRectSwift{{.*}}"(float %0, float %1, float %2, float %3, ptr swiftself %4) {{.*}} {
  // x86_64-macosx: [[COERCED:%.*]] = alloca [[MYRECT:%.*MyRect.*]], align 8
  // x86_64-macosx: [[SEL:%.*]] = load ptr, ptr @"\01L_selector(getXFromRect:)", align 8
  // x86_64-macosx: [[T0:%.*]] = getelementptr inbounds { <2 x float>, <2 x float> }, ptr [[COERCED]], i32 0, i32 0
  // x86_64-macosx: [[FIRST_HALF:%.*]] = load <2 x float>, ptr [[T0]]
  // x86_64-macosx: [[T0:%.*]] = getelementptr inbounds { <2 x float>, <2 x float> }, ptr [[COERCED]], i32 0, i32 1
  // x86_64-macosx: [[SECOND_HALF:%.*]] = load <2 x float>, ptr [[T0]]
  // x86_64-macosx: [[RESULT:%.*]] = call float @objc_msgSend(ptr %4, ptr [[SEL]], <2 x float> [[FIRST_HALF]], <2 x float> [[SECOND_HALF]])
  // armv7-ios: define hidden swiftcc float @"$s8abitypes3FooC17getXFromRectSwift{{[_0-9a-zA-Z]*}}F"(float %0, float %1, float %2, float %3, ptr swiftself %4) {{.*}} {
  // armv7-ios: [[DEBUGVAR:%.*]] = alloca [[MYRECT:%.*MyRect.*]], align 4
  // armv7-ios: [[COERCED:%.*]] = alloca [[MYRECT:%.*MyRect.*]], align 4
  // armv7-ios: [[SEL:%.*]] = load ptr, ptr @"\01L_selector(getXFromRect:)", align 4
  // armv7-ios: [[LOADED:%.*]] = load [4 x i32], ptr [[COERCED]]
  // armv7-ios: [[RESULT:%.*]] = call float @objc_msgSend(ptr %4, ptr [[SEL]], [4 x i32] [[LOADED]])

  // armv7s-ios: define hidden swiftcc float @"$s8abitypes3FooC17getXFromRectSwift{{[_0-9a-zA-Z]*}}F"(float %0, float %1, float %2, float %3, ptr swiftself %4) {{.*}} {
  // armv7s-ios: [[DEBUGVAR:%.*]] = alloca [[MYRECT:%.*MyRect.*]], align 4
  // armv7s-ios: [[COERCED:%.*]] = alloca [[MYRECT:%.*MyRect.*]], align 4
  // armv7s-ios: [[SEL:%.*]] = load ptr, ptr @"\01L_selector(getXFromRect:)", align 4
  // armv7s-ios: [[LOADED:%.*]] = load [4 x i32], ptr [[COERCED]]
  // armv7s-ios: [[RESULT:%.*]] = call float @objc_msgSend(ptr %4, ptr [[SEL]], [4 x i32] [[LOADED]])

  // armv7k-watchos: define hidden swiftcc float @"$s8abitypes3FooC17getXFromRectSwift{{[_0-9a-zA-Z]*}}F"(float %0, float %1, float %2, float %3, ptr swiftself %4) {{.*}} {
  // armv7k-watchos: [[DEBUGVAR:%.*]] = alloca [[MYRECT:%.*MyRect.*]], align 4
  // armv7k-watchos: [[COERCED:%.*]] = alloca [[MYRECT:%.*MyRect.*]], align 4
  // armv7k-watchos: [[SEL:%.*]] = load ptr, ptr @"\01L_selector(getXFromRect:)", align 4
  // armv7k-watchos: [[LOADED:%.*]] = load [4 x float], ptr [[COERCED]]
  // armv7k-watchos: [[RESULT:%.*]] = call float @objc_msgSend(ptr %4, ptr [[SEL]], [4 x float] [[LOADED]])

  // arm64_32-watchos: define hidden swiftcc float @"$s8abitypes3FooC17getXFromRectSwift{{[_0-9a-zA-Z]*}}F"(float %0, float %1, float %2, float %3, ptr swiftself %4) {{.*}} {
  // arm64_32-watchos: [[DEBUGVAR:%.*]] = alloca [[MYRECT:%.*MyRect.*]], align 4
  // arm64_32-watchos: [[COERCED:%.*]] = alloca [[MYRECT:%.*MyRect.*]], align 4
  // arm64_32-watchos: [[SEL:%.*]] = load ptr, ptr @"\01L_selector(getXFromRect:)", align 4
  // arm64_32-watchos: [[LOADED:%.*]] = load [4 x float], ptr [[COERCED]]
  // arm64_32-watchos: [[RESULT:%.*]] = call float @objc_msgSend(ptr %4, ptr [[SEL]], [4 x float] [[LOADED]])
  func getXFromRectSwift(_ r: MyRect) -> Float {
    return getXFromRect(r)
  }

  // Ensure that MyRect is passed as an indirect-byval on x86_64 because we run out of registers for direct arguments
  // x86_64-macosx: define internal float @"$s8abitypes3FooC25getXFromRectIndirectByVal{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, float %2, float %3, float %4, float %5, float %6, float %7, float %8, ptr byval({{.*}}) align 8 %9) {{[#0-9]*}} {
  @objc dynamic func getXFromRectIndirectByVal(_: Float, second _: Float,
                                       third _: Float, fourth _: Float,
                                       fifth _: Float, sixth _: Float,
                                       seventh _: Float, withRect r: MyRect)
               -> Float {
    return r.x
  }

  // Make sure the caller-side from Swift also uses indirect-byval for the argument
  // x86_64-macosx: define hidden swiftcc float @"$s8abitypes3FooC25getXFromRectIndirectSwift{{[_0-9a-zA-Z]*}}F"(float %0, float %1, float %2, float %3, ptr swiftself %4) {{.*}} {
  func getXFromRectIndirectSwift(_ r: MyRect) -> Float {
    let f : Float = 1.0
    // x86_64-macosx: alloca
    // x86_64-macosx: alloca
    // x86_64-macosx: [[TEMP:%.*]] = alloca [[TEMPTYPE:%.*]], align 8
    // x86_64-macosx: [[RESULT:%.*]] = call float @objc_msgSend(ptr %{{.*}}, ptr %{{.*}}, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, ptr byval({{.*}}) align 8 [[TEMP]])
    // x86_64-macosx: ret float [[RESULT]]
    return getXFromRectIndirectByVal(f, second: f, third: f, fourth: f, fifth: f, sixth: f, seventh: f, withRect: r);
  }

  // x86_64 returns an HA of four floats directly in two <2 x float>
  // x86_64-macosx:      define hidden swiftcc float @"$s8abitypes3FooC4barc{{[_0-9a-zA-Z]*}}F"(ptr %0, ptr swiftself %1) {{.*}} {
  // x86_64-macosx:      load ptr, ptr @"\01L_selector(newRect)", align 8
  // x86_64-macosx:      [[RESULT:%.*]] = call { <2 x float>, <2 x float> } @objc_msgSend
  // x86_64-macosx:      store { <2 x float>, <2 x float> } [[RESULT]]
  // x86_64-macosx:      load { float, float, float, float }, ptr
  // x86_64-macosx:      ret float
  //
  // armv7 returns an HA of four floats indirectly
  // armv7-ios: define hidden swiftcc float @"$s8abitypes3FooC4barc{{[_0-9a-zA-Z]*}}F"(ptr %0, ptr swiftself %1) {{.*}} {
  // armv7-ios: [[RESULT:%.*]] = alloca [[RECTTYPE:%.*MyRect.*]], align 4
  // armv7-ios: load ptr, ptr @"\01L_selector(newRect)", align 4
  // armv7-ios: call void @objc_msgSend_stret(ptr noalias nocapture sret({{.*}}) %call.aggresult
  // armv7-ios: [[GEP1:%.*]] = getelementptr inbounds [[RECTTYPE]], ptr [[RESULT]], i32 0, i32 1
  // armv7-ios: [[GEP2:%.*]] = getelementptr inbounds {{.*}}, ptr [[GEP1]], i32 0, i32 0
  // armv7-ios: [[RETVAL:%.*]] = load float, ptr [[GEP2]], align 4
  // armv7-ios: ret float [[RETVAL]]
  //
  // armv7s returns an HA of four floats indirectly
  // armv7s-ios: define hidden swiftcc float @"$s8abitypes3FooC4barc{{[_0-9a-zA-Z]*}}F"(ptr %0, ptr swiftself %1) {{.*}} {
  // armv7s-ios: [[RESULT:%.*]] = alloca [[RECTTYPE:%.*MyRect.*]], align 4
  // armv7s-ios: load ptr, ptr @"\01L_selector(newRect)", align 4
  // armv7s-ios: call void @objc_msgSend_stret(ptr noalias nocapture sret({{.*}}) %call.aggresult
  // armv7s-ios: [[GEP1:%.*]] = getelementptr inbounds [[RECTTYPE]], ptr [[RESULT]], i32 0, i32 1
  // armv7s-ios: [[GEP2:%.*]] = getelementptr inbounds {{.*}}, ptr [[GEP1]], i32 0, i32 0
  // armv7s-ios: [[RETVAL:%.*]] = load float, ptr [[GEP2]], align 4
  // armv7s-ios: ret float [[RETVAL]]
  //
  // armv7k returns an HA of four floats directly
  // armv7k-watchos:      define hidden swiftcc float @"$s8abitypes3FooC4barc{{[_0-9a-zA-Z]*}}F"(ptr %0, ptr swiftself %1) {{.*}} {
  // armv7k-watchos:      load ptr, ptr @"\01L_selector(newRect)", align 4
  // armv7k-watchos:      [[RESULT:%.*]] = call [[ARMV7K_MYRECT]] @objc_msgSend
  // armv7k-watchos:      store [[ARMV7K_MYRECT]] [[RESULT]]
  // armv7k-watchos:      load { float, float, float, float }, ptr
  // armv7k-watchos:      ret float
  func barc(_ p: StructReturns) -> Float {
    return p.newRect().y
  }

  // x86_64-macosx: define hidden swiftcc { double, double, double } @"$s8abitypes3FooC3baz{{[_0-9a-zA-Z]*}}F"(ptr swiftself %0) {{.*}} {
  // x86_64-macosx: define internal void @"$s8abitypes3FooC3baz{{[_0-9a-zA-Z]*}}FTo"(ptr noalias nocapture sret({{.*}}) %0, ptr %1, ptr %2) {{[#0-9]*}} {
  @objc dynamic func baz() -> Trio {
    return Trio(i: 1.0, j: 2.0, k: 3.0)
  }

  // x86_64-macosx:      define hidden swiftcc double @"$s8abitypes3FooC4bazc{{[_0-9a-zA-Z]*}}F"(ptr %0, ptr swiftself %1) {{.*}} {
  // x86_64-macosx:      load ptr, ptr @"\01L_selector(newTrio)", align 8
  // x86_64-macosx:      call void @objc_msgSend_stret
  func bazc(_ p: StructReturns) -> Double {
    return p.newTrio().j
  }

  // x86_64-macosx:      define hidden swiftcc i64 @"$s8abitypes3FooC7getpair{{[_0-9a-zA-Z]*}}F"(ptr %0, ptr swiftself %1) {{.*}} {
  // x86_64-macosx:      [[RESULT:%.*]] = call i64 @objc_msgSend
  // x86_64-macosx:      [[GEP1:%.*]] = getelementptr inbounds { i64 }, ptr {{.*}}, i32 0, i32 0
  // x86_64-macosx:      store i64 [[RESULT]], ptr [[GEP1]]
  // x86_64-macosx:      [[GEP2:%.*]] = getelementptr inbounds { i64 }, ptr {{.*}}, i32 0, i32 0
  // x86_64-macosx:      load i64, ptr [[GEP2]]
  // x86_64-macosx:      ret i64
  func getpair(_ p: StructReturns) -> IntPair {
    return p.newPair()
  }

  // x86_64-macosx:      define internal i64 @"$s8abitypes3FooC8takepair{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i64 %2) {{[#0-9]*}} {
  @objc dynamic func takepair(_ p: IntPair) -> IntPair {
    return p
  }

  // x86_64-macosx:      define hidden swiftcc i64 @"$s8abitypes3FooC9getnested{{[_0-9a-zA-Z]*}}F"(ptr %0, ptr swiftself %1) {{.*}} {
  // x86_64-macosx:      call i64 @objc_msgSend
  // x86_64-macosx:      call void @llvm.lifetime.start
  // x86_64-macosx:      store i32 {{.*}}
  // x86_64-macosx:      store i32 {{.*}}
  // x86_64-macosx:      [[T0:%.*]] = getelementptr inbounds { i64 }, ptr
  // x86_64-macosx:      load i64, ptr [[T0]], align 8
  // x86_64-macosx:      call void @llvm.lifetime.end
  // x86_64-macosx:      ret i64
  func getnested(_ p: StructReturns) -> NestedInts {
    return p.newNestedInts()
  }

  // x86_64-macosx:      define internal ptr @"$s8abitypes3FooC9copyClass{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, ptr %2) {{[#0-9]*}} {
  // x86_64-macosx:      [[VALUE:%[0-9]+]] = call swiftcc ptr @"$s8abitypes3FooC9copyClass{{[_0-9a-zA-Z]*}}F"
  // x86_64-macosx:      [[T0:%.*]] = call ptr @swift_getObjCClassFromMetadata(ptr [[VALUE]])
  // x86_64-macosx:      ret ptr [[T0]]
  @objc dynamic func copyClass(_ a: AnyClass) -> AnyClass {
    return a
  }

  // x86_64-macosx:      define internal ptr @"$s8abitypes3FooC9copyProto{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, ptr %2) {{[#0-9]*}} {
  // x86_64-macosx:      [[VALUE:%[0-9]+]] = call swiftcc ptr @"$s8abitypes3FooC9copyProto{{[_0-9a-zA-Z]*}}F"
  // x86_64-macosx:      ret ptr [[VALUE]]
  @objc dynamic func copyProto(_ a: AnyObject) -> AnyObject {
    return a
  }

  // x86_64-macosx:      define internal ptr @"$s8abitypes3FooC13copyProtoComp{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, ptr %2) {{[#0-9]*}} {
  // x86_64-macosx:      [[VALUE:%[0-9]+]] = call swiftcc ptr @"$s8abitypes3FooC13copyProtoComp{{[_0-9a-zA-Z]*}}F"
  // x86_64-macosx:      ret ptr [[VALUE]]
  @objc dynamic func copyProtoComp(_ a: P1 & P2) -> P1 & P2 {
    return a
  }

  // x86_64-macosx:       define hidden swiftcc i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1 %0, ptr swiftself %1) {{.*}} {
  // x86_64-macosx:       define internal signext i8 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i8 signext %2) {{[#0-9]*}} {
  // x86_64-macosx:       [[R1:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F"
  // x86_64-macosx:       [[R2:%[0-9]+]] = call swiftcc i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"
  // x86_64-macosx:       [[R3:%[0-9]+]] = call swiftcc i8 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 [[R2]]
  // x86_64-macosx:       ret i8 [[R3]]
  //
  // x86_64-ios-fixme:          define hidden i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1 %0, ptr %1) {{.*}} {
  // x86_64-ios-fixme:          define internal zeroext i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo"
  // x86_64-ios-fixme:          [[R1:%[0-9]+]] = call i1 @"$s10ObjectiveC22_convertObjCBoolToBoolSbAA0cD0V1x_tF"(i1 %2)
  // x86_64-ios-fixme:          [[R2:%[0-9]+]] = call i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1 [[R1]]
  // x86_64-ios-fixme:          [[R3:%[0-9]+]] = call i1 @"$s10ObjectiveC22_convertBoolToObjCBoolAA0eF0VSb1x_tF"(i1 [[R2]])
  // x86_64-ios-fixme:          ret i1 [[R3]]
  //
  // armv7-ios-fixme:     define hidden i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1 %0, ptr %1) {{.*}} {
  // armv7-ios-fixme:     define internal signext i8 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i8 signext %2) {{[#0-9]*}} {
  // armv7-ios-fixme:     [[R1:%[0-9]+]] = call i1 @"$s10ObjectiveC22_convertObjCBoolToBool1xSbAA0cD0V_tF"
  // armv7-ios-fixme:     [[R2:%[0-9]+]] = call i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1 [[R1]]
  // armv7-ios-fixme:     [[R3:%[0-9]+]] = call i8 @"$s10ObjectiveC22_convertBoolToObjCBoolAA0eF0VSb1x_tF"(i1 [[R2]]
  // armv7-ios-fixme:     ret i8 [[R3]]
  //
  // armv7s-ios-fixme:     define hidden i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1, ptr) {{.*}} {
  // armv7s-ios-fixme:     define internal signext i8 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo"(ptr, ptr, i8 signext) {{[#0-9]*}} {
  // armv7s-ios-fixme:     [[R1:%[0-9]+]] = call i1 @"$s10ObjectiveC22_convertObjCBoolToBool1xSbAA0cD0V_tF"
  // armv7s-ios-fixme:     [[R2:%[0-9]+]] = call i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1 [[R1]]
  // armv7s-ios-fixme:     [[R3:%[0-9]+]] = call i8 @"$s10ObjectiveC22_convertBoolToObjCBoolAA0eF0VSb1x_tF"(i1 [[R2]]
  // armv7s-ios-fixme:     ret i8 [[R3]]
  //
  // arm64-ios-fixme:     define hidden i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1, ptr) {{.*}} {
  // arm64-ios-fixme:     define internal zeroext i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo"
  // arm64-ios-fixme:     [[R2:%[0-9]+]] = call i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"
  // arm64-ios-fixme:     ret i1 [[R2]]
  //
  // arm64e-ios-fixme:     define hidden i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1, ptr) {{.*}} {
  // arm64e-ios-fixme:     define internal zeroext i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo"
  // arm64e-ios-fixme:     [[R2:%[0-9]+]] = call i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"
  // arm64e-ios-fixme:     ret i1 [[R2]]
  //
  // i386-ios-fixme:      define hidden i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1, ptr) {{.*}} {
  // i386-ios-fixme:      define internal signext i8 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo"(ptr, ptr, i8 signext) {{[#0-9]*}} {
  // i386-ios-fixme:     [[R1:%[0-9]+]] = call i1 @"$s10ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F"
  // i386-ios-fixme:     [[R2:%[0-9]+]] = call i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1 [[R1]]
  // i386-ios-fixme:     [[R3:%[0-9]+]] = call i8 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 [[R2]]
  // i386-ios-fixme:     ret i8 [[R3]]
  //
  // x86_64-tvos-fixme:          define hidden i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1, ptr) {{.*}} {
  // x86_64-tvos-fixme:          define internal zeroext i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo"
  // x86_64-tvos-fixme:          [[R1:%[0-9]+]] = call i1 @"$s10ObjectiveC22_convertObjCBoolToBoolSbAA0cD0V1x_tF"(i1 %2)
  // x86_64-tvos-fixme:          [[R2:%[0-9]+]] = call i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1 [[R1]]
  // x86_64-tvos-fixme:          [[R3:%[0-9]+]] = call i1 @"$s10ObjectiveC22_convertBoolToObjCBoolAA0eF0VSb1x_tF"(i1 [[R2]])
  // x86_64-tvos-fixme:          ret i1 [[R3]]
  //
  // arm64-tvos-fixme:     define hidden i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1, ptr) {{.*}} {
  // arm64-tvos-fixme:     define internal zeroext i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo"
  // arm64-tvos-fixme:     [[R2:%[0-9]+]] = call i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"
  // arm64-tvos-fixme:     ret i1 [[R2]]

  // i386-watchos:  define hidden swiftcc i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1 %0, ptr swiftself %1)
  // i386-watchos:  define internal zeroext i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo"
  // i386-watchos:  [[R1:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertObjCBoolToBoolySbAA0cD0VF"(i1 %2)
  // i386-watchos:  [[R2:%[0-9]+]] = call swiftcc i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1 [[R1]]
  // i386-watchos:  [[R3:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertBoolToObjCBoolyAA0eF0VSbF"(i1 [[R2]])
  // i386-watchos:  ret i1 [[R3]]
  //
  // arm64-watchos-fixme:     define hidden i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"(i1, ptr) {{.*}} {
  // arm64-watchos-fixme:     define internal zeroext i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}FTo"
  // arm64-watchos-fixme:     [[R2:%[0-9]+]] = call i1 @"$s8abitypes3FooC6negate{{[_0-9a-zA-Z]*}}F"
  // arm64-watchos-fixme:     ret i1 [[R2]]

  @objc dynamic func negate(_ b: Bool) -> Bool {
    return !b
  }

  // x86_64-macosx: define hidden swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1 %0, ptr swiftself %1) {{.*}} {
  // x86_64-macosx: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i8 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 %0)
  // x86_64-macosx: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(negate:)", align 8
  // x86_64-macosx: [[NEG:%[0-9]+]] = call signext i8 @objc_msgSend(ptr {{%[0-9]+}}, ptr [[SEL]], i8 signext [[TOOBJCBOOL]])
  // x86_64-macosx: [[TOBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F"(i8 [[NEG]])
  // x86_64-macosx: ret i1 [[TOBOOL]]
  //
  // x86_64-macosx: define internal signext i8 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i8 signext %2)
  // x86_64-macosx: [[TOBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F"
  // x86_64-macosx: [[NEG:%[0-9]+]] = call swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1 [[TOBOOL]]
  // x86_64-macosx: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i8 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 [[NEG]])
  // x86_64-macosx: ret i8 [[TOOBJCBOOL]]
  //
  // x86_64-ios: define hidden swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1 %0, ptr swiftself %1) {{.*}} {
  // x86_64-ios: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(negate:)", align 8
  // x86_64-ios: [[NEG:%[0-9]+]] = call zeroext i1 @objc_msgSend(ptr {{%[0-9]+}}, ptr [[SEL]], i1 zeroext %0)
  // x86_64-ios: ret i1 [[NEG]]
  //
  // x86_64-ios: define internal zeroext i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i1 zeroext %2)
  // x86_64-ios: [[NEG:%[0-9]+]] = call swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1
  // x86_64-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 [[NEG]])
  // x86_64-ios: ret i1 [[TOOBJCBOOL]]
  //
  // armv7-ios: define hidden swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1 %0, ptr swiftself %1) {{.*}} {
  // armv7-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i8 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 %0)
  // armv7-ios: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(negate:)", align 4
  // armv7-ios: [[NEG:%[0-9]+]] = call signext i8 @objc_msgSend(ptr {{%[0-9]+}}, ptr [[SEL]], i8 signext [[TOOBJCBOOL]])
  // armv7-ios: [[TOBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F"(i8 [[NEG]])
  // armv7-ios: ret i1 [[TOBOOL]]
  //
  // armv7-ios: define internal signext i8 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i8 signext %2)
  // armv7-ios: [[TOBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F"
  // armv7-ios: [[NEG:%[0-9]+]] = call swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1 [[TOBOOL]]
  // armv7-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i8 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 [[NEG]])
  // armv7-ios: ret i8 [[TOOBJCBOOL]]
  //
  // armv7s-ios: define hidden swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1 %0, ptr swiftself %1) {{.*}} {
  // armv7s-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i8 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 %0)
  // armv7s-ios: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(negate:)", align 4
  // armv7s-ios: [[NEG:%[0-9]+]] = call signext i8 @objc_msgSend(ptr {{%[0-9]+}}, ptr [[SEL]], i8 signext [[TOOBJCBOOL]])
  // armv7s-ios: [[TOBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F"(i8 [[NEG]])
  // armv7s-ios: ret i1 [[TOBOOL]]
  //
  // armv7s-ios: define internal signext i8 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i8 signext %2)
  // armv7s-ios: [[TOBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F"
  // armv7s-ios: [[NEG:%[0-9]+]] = call swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1 [[TOBOOL]]
  // armv7s-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i8 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 [[NEG]])
  // armv7s-ios: ret i8 [[TOOBJCBOOL]]
  //
  // arm64-ios: define hidden swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1 %0, ptr swiftself %1) {{.*}} {
  // arm64-ios: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(negate:)", align 8
  // arm64-ios: [[NEG:%[0-9]+]] = call zeroext i1 @objc_msgSend(ptr {{%[0-9]+}}, ptr [[SEL]], i1 zeroext %0)
  // arm64-ios: ret i1 [[NEG]]
  //
  // arm64-ios: define internal zeroext i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i1 zeroext %2)
  // arm64-ios: [[NEG:%[0-9]+]] = call swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1
  // arm64-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 [[NEG]])
  // arm64-ios: ret i1 [[TOOBJCBOOL]]
  //
  // arm64e-ios: define internal zeroext i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i1 zeroext %2)
  // arm64e-ios: [[NEG:%[0-9]+]] = call swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1
  // arm64e-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 [[NEG]])
  // arm64e-ios: ret i1 [[TOOBJCBOOL]]
  //
  // i386-ios: define hidden swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1 %0, ptr swiftself %1) {{.*}} {
  // i386-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i8 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 %0)
  // i386-ios: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(negate:)", align 4
  // i386-ios: [[NEG:%[0-9]+]] = call signext i8 @objc_msgSend(ptr {{%[0-9]+}}, ptr [[SEL]], i8 signext [[TOOBJCBOOL]])
  // i386-ios: [[TOBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F"(i8 [[NEG]])
  // i386-ios: ret i1 [[TOBOOL]]
  //
  // i386-ios: define internal signext i8 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i8 signext %2)
  // i386-ios: [[TOBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertObjCBoolToBool{{[_0-9a-zA-Z]*}}F"
  // i386-ios: [[NEG:%[0-9]+]] = call swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1 [[TOBOOL]]
  // i386-ios: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i8 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 [[NEG]])
  // i386-ios: ret i8 [[TOOBJCBOOL]]
  //
  // x86_64-tvos: define hidden swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1 %0, ptr swiftself %1) {{.*}} {
  // x86_64-tvos: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(negate:)", align 8
  // x86_64-tvos: [[NEG:%[0-9]+]] = call zeroext i1 @objc_msgSend(ptr {{%[0-9]+}}, ptr [[SEL]], i1 zeroext %0)
  // x86_64-tvos: ret i1 [[NEG]]
  //
  // x86_64-tvos: define internal zeroext i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i1 zeroext %2)
  // x86_64-tvos: [[NEG:%[0-9]+]] = call swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1
  // x86_64-tvos: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 [[NEG]])
  // x86_64-tvos: ret i1 [[TOOBJCBOOL]]
  //
  // arm64-tvos: define hidden swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1 %0, ptr swiftself %1) {{.*}} {
  // arm64-tvos: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(negate:)", align 8
  // arm64-tvos: [[NEG:%[0-9]+]] = call zeroext i1 @objc_msgSend(ptr {{%[0-9]+}}, ptr [[SEL]], i1 zeroext %0)
  // arm64-tvos: ret i1 [[NEG]]
  //
  // arm64-tvos: define internal zeroext i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i1 zeroext %2)
  // arm64-tvos: [[NEG:%[0-9]+]] = call swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1
  // arm64-tvos: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 [[NEG]])
  // arm64-tvos: ret i1 [[TOOBJCBOOL]]

  // i386-watchos: define hidden swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1 %0, ptr swiftself %1) {{.*}} {
  // i386-watchos: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(negate:)", align 4
  // i386-watchos: [[NEG:%[0-9]+]] = call zeroext i1 @objc_msgSend(ptr {{%[0-9]+}}, ptr [[SEL]], i1 zeroext %0)
  // i386-watchos: ret i1 [[NEG]]
  //
  // i386-watchos: define internal zeroext i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i1 zeroext %2)
  // i386-watchos: [[NEG:%[0-9]+]] = call swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1
  // i386-watchos: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 [[NEG]])
  // i386-watchos: ret i1 [[TOOBJCBOOL]]
  //
  // armv7k-watchos: define hidden swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1 %0, ptr swiftself %1) {{.*}} {
  // armv7k-watchos: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(negate:)", align 4
  // armv7k-watchos: [[NEG:%[0-9]+]] = call zeroext i1 @objc_msgSend(ptr {{%[0-9]+}}, ptr [[SEL]], i1 zeroext %0)
  // armv7k-watchos: ret i1 [[NEG]]
  //
  // armv7k-watchos: define internal zeroext i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i1 zeroext %2)
  // armv7k-watchos: [[NEG:%[0-9]+]] = call swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1
  // armv7k-watchos: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 [[NEG]])
  // armv7k-watchos: ret i1 [[TOOBJCBOOL]]
  //
  // arm64-watchos: define hidden swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1 %0, ptr swiftself %1) {{.*}} {
  // arm64-watchos: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(negate:)", align 8
  // arm64-watchos: [[NEG:%[0-9]+]] = call zeroext i1 @objc_msgSend(ptr {{%[0-9]+}}, ptr [[SEL]], i1 zeroext %0)
  // arm64-watchos: ret i1 [[NEG]]
  //
  // arm64-watchos: define internal zeroext i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i1 zeroext %2)
  // arm64-watchos: [[NEG:%[0-9]+]] = call swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1
  // arm64-watchos: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 [[NEG]])
  // arm64-watchos: ret i1 [[TOOBJCBOOL]]
  //
  // arm64-macosx: define internal zeroext i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i1 zeroext %2)
  // arm64-macosx: [[NEG:%[0-9]+]] = call swiftcc i1 @"$s8abitypes3FooC7negate2{{[_0-9a-zA-Z]*}}F"(i1
  // arm64-macosx: [[TOOBJCBOOL:%[0-9]+]] = call swiftcc i1 @"$s10ObjectiveC22_convertBoolToObjCBool{{[_0-9a-zA-Z]*}}F"(i1 [[NEG]])
  // arm64-macosx: ret i1 [[TOOBJCBOOL]]
@objc dynamic func negate2(_ b: Bool) -> Bool {
    var g = Gadget()
    return g.negate(b)
  }

  // x86_64-macosx: define hidden swiftcc i1 @"$s8abitypes3FooC7negate3yS2bF"(i1 %0, ptr swiftself %1) {{.*}} {
  // x86_64-macosx: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(invert:)", align 8
  // x86_64-macosx: [[NEG:%[0-9]+]] = call zeroext i1 @objc_msgSend(ptr [[RECEIVER:%[0-9]+]], ptr [[SEL]], i1 zeroext %0)
  // x86_64-macosx: ret i1 [[NEG]]
  // x86_64-macosx: }

  // x86_64-ios: define hidden swiftcc i1 @"$s8abitypes3FooC7negate3yS2bF"(i1 %0, ptr swiftself %1) {{.*}} {
  // x86_64-ios: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(invert:)", align 8
  // x86_64-ios: [[NEG:%[0-9]+]] = call zeroext i1 @objc_msgSend(ptr [[RECEIVER:%[0-9]+]], ptr [[SEL]], i1 zeroext %0)
  // x86_64-ios: ret i1 [[NEG]]
  // x86_64-ios: }

  // i386-ios: define hidden swiftcc i1 @"$s8abitypes3FooC7negate3yS2bF"(i1 %0, ptr swiftself %1) {{.*}} {
  // i386-ios: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(invert:)", align 4
  // i386-ios: [[NEG:%[0-9]+]] = call zeroext i1 @objc_msgSend(ptr [[RECEIVER:%[0-9]+]], ptr [[SEL]], i1 zeroext %0)
  // i386-ios: ret i1 [[NEG]]
  // i386-ios: }

  @objc dynamic func negate3(_ b: Bool) -> Bool {
    var g = Gadget()
    return g.invert(b)
  }

  // x86_64-macosx: define hidden swiftcc void @"$s8abitypes3FooC10throwsTestyySbKF"(i1 %0, ptr swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2) {{.*}} {
  // x86_64-macosx: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(negateThrowing:error:)", align 8
  // x86_64-macosx: call signext i8 @objc_msgSend(ptr {{%[0-9]+}}, ptr [[SEL]], i8 signext {{%[0-9]+}}, ptr {{%[0-9]+}})
  // x86_64-macosx: }

  // x86_64-ios: define hidden swiftcc void @"$s8abitypes3FooC10throwsTestyySbKF"(i1 %0, ptr swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2) {{.*}} {
  // x86_64-ios: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(negateThrowing:error:)", align 8
  // x86_64-ios: call zeroext i1 @objc_msgSend(ptr {{%[0-9]+}}, ptr [[SEL]], i1 zeroext {{%[0-9]+}}, ptr {{%[0-9]+}})
  // x86_64-ios: }

  // i386-ios: define hidden swiftcc void @"$s8abitypes3FooC10throwsTestyySbKF"(i1 %0, ptr swiftself %1, ptr noalias nocapture dereferenceable(4) %2) {{.*}} {
  // i386-ios: [[SEL:%[0-9]+]] = load ptr, ptr @"\01L_selector(negateThrowing:error:)", align 4
  // i386-ios: call signext i8 @objc_msgSend(ptr {{%[0-9]+}}, ptr [[SEL]], i8 signext {{%[0-9]+}}, ptr {{%[0-9]+}})
  // i386-ios: }
  @objc dynamic func throwsTest(_ b: Bool) throws {
    var g = Gadget()
    try g.negateThrowing(b)
  }

  // x86_64-macosx: define internal ptr @"$s8abitypes3FooC24copyUnsafeMutablePointer{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, ptr %2) {{[#0-9]*}} {
  @objc dynamic func copyUnsafeMutablePointer(_ p: UnsafeMutablePointer<Int32>) -> UnsafeMutablePointer<Int32> {
    return p
  }

  // x86_64-macosx: define internal i64 @"$s8abitypes3FooC17returnNSEnumValue{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1) {{[#0-9]*}} {
  @objc dynamic func returnNSEnumValue() -> ByteCountFormatter.CountStyle {
    return .file
  }

  // x86_64-macosx: define internal zeroext i16 @"$s8abitypes3FooC20returnOtherEnumValue{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, i16 zeroext %2) {{[#0-9]*}} {
  @objc dynamic func returnOtherEnumValue(_ choice: ChooseTo) -> ChooseTo {
    switch choice {
      case .takeIt: return .leaveIt
      case .leaveIt: return .takeIt
    }
  }

  // x86_64-macosx: define hidden swiftcc i32 @"$s8abitypes3FooC10getRawEnum{{[_0-9a-zA-Z]*}}F"(ptr swiftself %0) {{.*}} {
  // x86_64-macosx: define internal i32 @"$s8abitypes3FooC10getRawEnum{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1) {{[#0-9]*}} {
  @objc dynamic func getRawEnum() -> RawEnum {
    return Intergalactic
  }

  var work : Work
  init (work: Work) {
    self.work = work
  }

  // x86_64-macosx: define internal void @"$s8abitypes3FooC13testArchetype{{[_0-9a-zA-Z]*}}FTo"(ptr %0, ptr %1, ptr %2) {{[#0-9]*}} {
  @objc dynamic func testArchetype(_ work: Work) {
    work.doStuff(1)
    // x86_64-macosx: call swiftcc void @"$s8abitypes3FooC13testArchetype{{[_0-9a-zA-Z]*}}F"(ptr %2, ptr swiftself %{{.*}})
  }

  @objc dynamic func foo(_ x: @convention(block) (Int) -> Int) -> Int {
    // FIXME: calling blocks is currently unimplemented
    // return x(5)
    return 1
  }

  // x86_64-macosx: define hidden swiftcc void @"$s8abitypes3FooC20testGenericTypeParam{{[_0-9a-zA-Z]*}}F"(ptr %0, ptr %T, ptr swiftself %1) {{.*}} {
  func testGenericTypeParam<T: Pasta>(_ x: T) {
    // x86_64-macosx: call void @objc_msgSend(ptr %0, ptr %{{.*}})
    x.alDente()
  }

  // arm64-ios: define hidden swiftcc { i64, i64, i64, i64 } @"$s8abitypes3FooC14callJustReturn{{[_0-9a-zA-Z]*}}F"(ptr %0, i64 %1, i64 %2, i64 %3, i64 %4, ptr swiftself %5) {{.*}} {
  // arm64-ios: define internal void @"$s8abitypes3FooC14callJustReturn{{[_0-9a-zA-Z]*}}FTo"(ptr noalias nocapture sret({{.*}}) %0, ptr %1, ptr %2, ptr %3, ptr %4) {{[#0-9]*}} {
  //
  // arm64e-ios: define hidden swiftcc { i64, i64, i64, i64 } @"$s8abitypes3FooC14callJustReturn{{[_0-9a-zA-Z]*}}F"(ptr %0, i64 %1, i64 %2, i64 %3, i64 %4, ptr swiftself %5) {{.*}} {
  // arm64e-ios: define internal void @"$s8abitypes3FooC14callJustReturn{{[_0-9a-zA-Z]*}}FTo"(ptr noalias nocapture sret({{.*}}) %0, ptr %1, ptr %2, ptr %3, ptr %4) {{.*}} {
  //
  // arm64-tvos: define hidden swiftcc { i64, i64, i64, i64 } @"$s8abitypes3FooC14callJustReturn{{[_0-9a-zA-Z]*}}F"(ptr %0, i64 %1, i64 %2, i64 %3, i64 %4, ptr swiftself %5) {{.*}} {
  // arm64-tvos: define internal void @"$s8abitypes3FooC14callJustReturn{{[_0-9a-zA-Z]*}}FTo"(ptr noalias nocapture sret({{.*}}) %0, ptr %1, ptr %2, ptr %3, ptr %4) {{[#0-9]*}} {
  // arm64-macosx: define hidden swiftcc { i64, i64, i64, i64 } @"$s8abitypes3FooC14callJustReturn{{[_0-9a-zA-Z]*}}F"(ptr %0, i64 %1, i64 %2, i64 %3, i64 %4, ptr swiftself %5) {{.*}} {
  // arm64-macosx: define internal void @"$s8abitypes3FooC14callJustReturn{{[_0-9a-zA-Z]*}}FTo"(ptr noalias nocapture sret({{.*}}) %0, ptr %1, ptr %2, ptr %3, ptr %4) {{.*}} {
  //
  // arm64-watchos: define hidden swiftcc { i64, i64, i64, i64 } @"$s8abitypes3FooC14callJustReturn{{[_0-9a-zA-Z]*}}F"(ptr %0, i64 %1, i64 %2, i64 %3, i64 %4, ptr swiftself %5) {{.*}} {
  // arm64-watchos: define internal void @"$s8abitypes3FooC14callJustReturn{{[_0-9a-zA-Z]*}}FTo"(ptr noalias nocapture sret({{.*}}) %0, ptr %1, ptr %2, ptr %3, ptr %4) {{[#0-9]*}} {
  @objc dynamic func callJustReturn(_ r: StructReturns, with v: BigStruct) -> BigStruct {
    return r.justReturn(v)
  }
}

// We need to allocate enough memory on the stack to hold the argument value we load.
// arm64-ios: define swiftcc void @"$s8abitypes14testBOOLStructyyF"()
// arm64-ios:  [[COERCED:%.*]] = alloca i64
// arm64-ios:  [[PTR0:%.*]] = getelementptr inbounds %TSo14FiveByteStructV, ptr [[COERCED]], {{i.*}} 0, {{i.*}} 0
// arm64-ios:  [[PTR1:%.*]] = getelementptr inbounds %T10ObjectiveC8ObjCBoolV, ptr [[PTR0]], {{i.*}} 0, {{i.*}} 0
// arm64-ios:  [[PTR2:%.*]] = getelementptr inbounds %TSb, ptr [[PTR1]], {{i.*}} 0, {{i.*}} 0
// arm64-ios:  store i8 0, ptr [[PTR2]], align 8
// arm64-ios:  [[ARG:%.*]] = load i64, ptr [[COERCED]]
// arm64-ios:  call void @objc_msgSend(ptr {{.*}}, ptr {{.*}}, i64 [[ARG]])
//
// arm64e-ios: define swiftcc void @"$s8abitypes14testBOOLStructyyF"()
// arm64e-ios:  [[COERCED:%.*]] = alloca i64
// arm64e-ios:  [[PTR0:%.*]] = getelementptr inbounds %TSo14FiveByteStructV, ptr [[COERCED]], {{i.*}} 0, {{i.*}} 0
// arm64e-ios:  [[PTR1:%.*]] = getelementptr inbounds %T10ObjectiveC8ObjCBoolV, ptr [[PTR0]], {{i.*}} 0, {{i.*}} 0
// arm64e-ios:  [[PTR2:%.*]] = getelementptr inbounds %TSb, ptr [[PTR1]], {{i.*}} 0, {{i.*}} 0
// arm64e-ios:  store i8 0, ptr [[PTR2]], align 8
// arm64e-ios:  [[ARG:%.*]] = load i64, ptr [[COERCED]]
// arm64e-ios:  call void @objc_msgSend(ptr {{.*}}, ptr {{.*}}, i64 [[ARG]])
// arm64-macosx: define swiftcc void @"$s8abitypes14testBOOLStructyyF"()
// arm64-macosx:  [[COERCED:%.*]] = alloca i64
// arm64-macosx:  [[PTR0:%.*]] = getelementptr inbounds %TSo14FiveByteStructV, ptr [[COERCED]], {{i.*}} 0, {{i.*}} 0
// arm64-macosx:  [[PTR1:%.*]] = getelementptr inbounds %T10ObjectiveC8ObjCBoolV, ptr [[PTR0]], {{i.*}} 0, {{i.*}} 0
// arm64-macosx:  [[PTR2:%.*]] = getelementptr inbounds %TSb, ptr [[PTR1]], {{i.*}} 0, {{i.*}} 0
// arm64-macosx:  store i8 0, ptr [[PTR2]], align 8
// arm64-macosx:  [[ARG:%.*]] = load i64, ptr [[COERCED]]
// arm64-macosx:  call void @objc_msgSend(ptr {{.*}}, ptr {{.*}}, i64 [[ARG]])
//
// arm64-watchos: define swiftcc void @"$s8abitypes14testBOOLStructyyF"()
// arm64-watchos:  [[COERCED:%.*]] = alloca i64
// arm64-watchos:  [[PTR0:%.*]] = getelementptr inbounds %TSo14FiveByteStructV, ptr [[COERCED]], {{i.*}} 0, {{i.*}} 0
// arm64-watchos:  [[PTR1:%.*]] = getelementptr inbounds %T10ObjectiveC8ObjCBoolV, ptr [[PTR0]], {{i.*}} 0, {{i.*}} 0
// arm64-watchos:  [[PTR2:%.*]] = getelementptr inbounds %TSb, ptr [[PTR1]], {{i.*}} 0, {{i.*}} 0
// arm64-watchos:  store i8 0, ptr [[PTR2]], align 8
// arm64-watchos:  [[ARG:%.*]] = load i64, ptr [[COERCED]]
// arm64-watchos:  call void @objc_msgSend(ptr {{.*}}, ptr {{.*}}, i64 [[ARG]])
public func testBOOLStruct() {
  let s = FiveByteStruct()
  MyClass.mymethod(s)
}

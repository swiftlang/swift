// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name abitypes -I %S/Inputs/abi %s -emit-ir | %FileCheck -check-prefix=%target-cpu-%target-os-abi -check-prefix=%target-cpu %s
// REQUIRES: CPU=x86_64
// UNSUPPORTED: OS=windows-msvc

import c_gadget

// rdar://17631440 - Expand direct arguments that are coerced to aggregates.
// x86_64: define{{( dllexport)?}}{{( protected)?}} swiftcc float @"$s8abitypes13testInlineAggySfSo6MyRectVF"(float %0, float %1, float %2, float %3) {{.*}} {
// x86_64: [[COERCED:%.*]] = alloca %TSo6MyRectV, align 8
// x86_64: store float %0,
// x86_64: store float %1,
// x86_64: store float %2,
// x86_64: store float %3,
// x86_64: [[T0:%.*]] = getelementptr inbounds{{.*}} { <2 x float>, <2 x float> }, ptr [[COERCED]], i32 0, i32 0
// x86_64: [[FIRST_HALF:%.*]] = load <2 x float>, ptr [[T0]], align 8
// x86_64: [[T0:%.*]] = getelementptr inbounds{{.*}} { <2 x float>, <2 x float> }, ptr [[COERCED]], i32 0, i32 1
// x86_64: [[SECOND_HALF:%.*]] = load <2 x float>, ptr [[T0]], align 8
// x86_64: [[RESULT:%.*]] = call float @MyRect_Area(<2 x float> [[FIRST_HALF]], <2 x float> [[SECOND_HALF]])
// x86_64: ret float [[RESULT]]
public func testInlineAgg(_ rect: MyRect) -> Float {
  return MyRect_Area(rect)
}

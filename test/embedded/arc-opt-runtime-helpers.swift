// RUN: %target-swift-frontend -emit-ir -O -wmo -parse-as-library \
// RUN:   -enable-experimental-feature Embedded \
// RUN:   -enable-experimental-feature CodeGenerationModel=interface \
// RUN:   %s -o - | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_CodeGenerationModel

public final class C { public var x: Int = 0 }

public func use(_ a: C, _ b: C) -> Int {
  return a.x &+ b.x
}

// CHECK: define {{.*}} @"$e{{.*}}3useySiAA1CC_AEtF" 

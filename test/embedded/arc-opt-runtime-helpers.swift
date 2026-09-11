// RUN: %target-swift-frontend -emit-ir -O -wmo -parse-as-library \
// RUN:   -module-name main \
// RUN:   -enable-experimental-feature Embedded \
// RUN:   -enable-experimental-feature CodeGenerationModel=interface \
// RUN:   %s -o - | %FileCheck %s

// REQUIRES: swift_feature_Embedded

public final class C { public var x: Int = 0 }

public func use(_ a: C, _ b: C) -> Int {
  return a.x &+ b.x
}

// CHECK: define {{.*}} @"$e4main3useySiAA1CC_ADtF"

// RUN: %target-swift-frontend -g -emit-sil %s -enable-experimental-feature Embedded -module-name trap -wmo -o - | %FileCheck %s

// RUN: %target-swift-frontend -g -emit-ir %s -enable-experimental-feature Embedded -module-name trap -wmo -o - | %FileCheck -check-prefix IR %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded
// REQUIRES: CPU=x86_64 || CPU=arm64 || CPU=arm64e

// CHECK-LABEL: sil @$e4trap11rebindCheck3ptr8capacityySpySuG_SitFySpys5Int64VGXEfU_
public func rebindCheck(ptr: UnsafeMutablePointer<UInt>, capacity: Int) {
  // CHECK-NOT: _assertionFa
  // CHECK: cond_fail
  // CHECK-NOT: _assertionFa
  // CHECK: return
  ptr.withMemoryRebound(to: Int64.self, capacity: capacity) {
    $0.pointee.negate()
  }
}

// IR: distinct !DISubprogram(name: "Swift runtime failure: arithmetic overflow"

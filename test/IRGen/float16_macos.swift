// RUN: %target-swift-frontend -emit-ir -unavailable-decl-optimization=none %s -target x86_64-apple-macos10.15 | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -unavailable-decl-optimization=none %s -target x86_64-apple-macos11 | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64
// UNSUPPORTED: use_os_stdlib

@inline(never)
func blackHole<T>(_ t: T.Type) {}

@available(macOS, unavailable)
public func useFloat16() {
  blackHole(Float16.self)
}

// CHECK-LABEL: @"$ss7Float16VN" = extern_weak global %swift.type

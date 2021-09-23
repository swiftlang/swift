// RUN: %target-swift-frontend -target x86_64-unknown-linux-gnu -parse-stdlib -disable-objc-interop %s -module-name main -emit-ir -o - | %FileCheck %s
// RUN: %target-swift-frontend -target x86_64-unknown-windows-msvc -parse-stdlib -disable-objc-interop %s -module-name main -emit-ir -o - | %FileCheck %s
// RUN: %target-swift-frontend -target x86_64-unknown-freebsd -parse-stdlib -disable-objc-interop %s -module-name main -emit-ir -o - | %FileCheck %s
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.9 -parse-stdlib -module-name main %s -emit-ir -o - | %FileCheck %s

// REQUIRES: CODEGENERATOR=X86

public func test() {
}

// We expect double-wide atomic intrinsics to always be available on x86_64.
// CHECK: "target-features"="{{.*}}+cx16,

// RUN: %target-swift-frontend -debug-callsite-info -emit-ir -parse-as-library -g -O -module-name S %s -o - | %FileCheck %s
// Make sure -debug-callsite-info is turned on by default in -O.
// RUN: %target-swift-frontend -emit-ir -parse-as-library -g -O -module-name S %s -o - | %FileCheck %s

// Only backends that can describe call sites turn this on by default, so the
// rest of the targets have nothing to check here.
// REQUIRES: CPU=x86_64 || CPU=i386 || CPU=arm64 || CPU=arm64e || CPU=aarch64

// CHECK: {{[0-9]+}} = distinct !DISubprogram(name: "f", linkageName: {{.*}}, scope: !{{[0-9]+}}, file: !{{[0-9]+}}, line: {{[0-9]+}}, type: !{{[0-9]+}}, scopeLine: {{[0-9]+}}, flags: DIFlagAllCallsDescribed
// CHECK: {{[0-9]+}} = distinct !DISubprogram(linkageName: {{.*}}, scope: !{{[0-9]+}}, file: !{{[0-9]+}}, type: !{{[0-9]+}}, flags: {{.*}} DIFlagAllCallsDescribed

public struct S {
  public func f() {}
}

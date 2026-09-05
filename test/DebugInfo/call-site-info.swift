// Dont pass -debug-callsite-info as it should be turned on by default in -O
// RUN: %target-swift-frontend -emit-ir -parse-as-library -g -O -module-name S %s -o - | %FileCheck %s
// CHECK: {{[0-9]+}} = distinct !DISubprogram(name: "f", linkageName: {{.*}}, scope: !{{[0-9]+}}, file: !{{[0-9]+}}, line: {{[0-9]+}}, type: !{{[0-9]+}}, scopeLine: {{[0-9]+}}, flags: DIFlagAllCallsDescribed
// CHECK: {{[0-9]+}} = distinct !DISubprogram(linkageName: {{.*}}, scope: !{{[0-9]+}}, file: !{{[0-9]+}}, type: !{{[0-9]+}}, flags: {{.*}} DIFlagAllCallsDescribed

public struct S {
  public func f() {}
}

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -sdk %t/SOME_SDK -g %s -emit-ir | %FileCheck -check-prefix=WITHOUT-PREFIX-MAP %s
// RUN: %target-swift-frontend -sdk %t/SOME_SDK -g %s -emit-ir -debug-prefix-map %t/SOME_SDK=foo | %FileCheck -check-prefix=WITH-PREFIX-MAP -implicit-check-not=SOME_SDK %s

func foo() {}

// WITHOUT-PREFIX-MAP: sysroot: "{{.*}}SOME_SDK
// WITH-PREFIX-MAP: sysroot: "foo

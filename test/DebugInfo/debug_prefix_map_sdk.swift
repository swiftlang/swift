// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -sdk %t -g %s -emit-ir | %FileCheck -check-prefix=WITHOUT-PREFIX-MAP %s
// RUN: %target-swift-frontend -sdk %t -g %s -emit-ir -debug-prefix-map %t=foo | %FileCheck -check-prefix=WITH-PREFIX-MAP %s

func foo() {}

// WITHOUT-PREFIX-MAP: sysroot: "BUILD_DIR
// WITH-PREFIX-MAP: sysroot: "foo

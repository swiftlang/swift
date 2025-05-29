// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/sugar -emit-module
// RUN: sed -ne '/\/\/ *DEMANGLE: /s/\/\/ *DEMANGLE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/sugar -type-from-mangled=%t/input | %FileCheck %s

func blackHole(_: Any...) {}

func foo() {
  let a: (Int?, [Float], [Double : String], (Bool)) = (nil, [], [:], false)
  blackHole(a)
}

// ParenType (XSp) has been removed, make sure it demangles for compatibility.
// DEMANGLE: $sSiXSq_SfXSaSdSSXSDSbXSptD
// CHECK: (Int?, [Float], [Double : String], Bool)

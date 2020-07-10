// REQUIRES: OS=macosx

var p: CoolInt

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/custom-resource-dir/macosx/Swift.swiftmodule)

// RUN: %target-swift-frontend -emit-module -module-name Swift -parse-stdlib -target %target-cpu-apple-macosx10.12 %S/Inputs/custom-resource-stdlib.swift -o %t/custom-resource-dir/macosx/Swift.swiftmodule/%target-swiftmodule-name
// RUN: %sourcekitd-test -req=cursor -pos=3:8 %s -- -resource-dir %t/custom-resource-dir -target %target-cpu-apple-macosx10.12 %s | %FileCheck %s

// CHECK: source.lang.swift.ref.struct
// CHECK-NEXT: CoolInt

// REQUIRES: OS=macosx

var p: CoolInt

// RUN: mkdir -p %t/custom-resource-dir/macosx/x86_64
// RUN: %target-swift-frontend -emit-module -module-name Swift -parse-stdlib -target x86_64-apple-macosx10.12 %S/Inputs/custom-resource-stdlib.swift -o %t/custom-resource-dir/macosx/x86_64
// RUN: %sourcekitd-test -req=cursor -pos=3:8 %s -- -resource-dir %t/custom-resource-dir -target x86_64-apple-macosx10.12 %s | %FileCheck %s

// CHECK: source.lang.swift.ref.struct
// CHECK-NEXT: CoolInt

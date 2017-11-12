// Checks that we don't crash
// RUN: %sourcekitd-test -req=cursor -pos=6:30 %s -- %s | %FileCheck %s
// CHECK: source.lang.swift.ref.var.global

let y = 1
print("text: \( "he\(/*here*/y)lo" )")

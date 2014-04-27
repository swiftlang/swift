// RUN: %swift -parse -I=%t -serialize-diagnostics-path %t.dia %s -verify
// RUN: c-index-test -read-diagnostics %t.dia > %t.deserialized_diagnostics.txt 2>&1
// RUN: FileCheck --input-file=%t.deserialized_diagnostics.txt %s

func foo(y : @unchecked Int?) { // expected-error {{'@unchecked T?' syntax is going away, use 'T!'}}
  let x:@unchecked Int? = (42 as (Int!)) // expected-error {{'@unchecked T?' syntax is going away, use 'T!'}}
}

// CHECK: error: '@unchecked T?' syntax is going away, use 'T!' [] []
// CHECK: Number FIXITs = 3
// CHECK: FIXIT: ([[FILE:.*unchecked_optional_going_away.swift]]:5:14 - [[FILE]]:5:15): ""
// CHECK: FIXIT: ([[FILE]]:5:15 - [[FILE]]:5:24): ""
// CHECK: FIXIT: ([[FILE]]:5:28 - [[FILE]]:5:29): "!"
// CHECK: [[FILE]]:6:10: error: '@unchecked T?' syntax is going away, use 'T!' [] []
// CHECK: Number FIXITs = 3
// CHECK: FIXIT: ([[FILE]]:6:9 - [[FILE]]:6:10): ""
// CHECK: FIXIT: ([[FILE]]:6:10 - [[FILE]]:6:19): ""
// CHECK: FIXIT: ([[FILE]]:6:23 - [[FILE]]:6:24): "!"
// CHECK: Number of diagnostics: 2


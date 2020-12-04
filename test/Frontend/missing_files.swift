// RUN: %empty-directory(%t)

// RUN: not %target-swift-frontend -c -parse-as-library /tmp/SOMETHING_DOES_NOT_EXIST_1.swift -primary-file %s /tmp/SOMETHING_DOES_NOT_EXIST_2.swift -o %t/out.o 2> %t/error1.output
// RUN: not test -f %t/out.o
// RUN: %FileCheck %s -input-file %t/error1.output --check-prefixes=CHECK

// RUN: not %target-swift-frontend -c -parse-as-library -primary-file /tmp/SOMETHING_DOES_NOT_EXIST_1.swift -primary-file %s /tmp/SOMETHING_DOES_NOT_EXIST_2.swift -o %t/out1.o -o %t/out2.o 2> %t/error2.output
// RUN: not test -f %t/out1.o
// RUN: not test -f %t/out2.o
// RUN: %FileCheck %s -input-file %t/error2.output --check-prefixes=CHECK

// CHECK-DAG: <unknown>:0: error: error opening input file '{{[/\\]}}tmp{{[/\\]}}SOMETHING_DOES_NOT_EXIST_1.swift' ({{.*}})
// CHECK-DAG: <unknown>:0: error: error opening input file '{{[/\\]}}tmp{{[/\\]}}SOMETHING_DOES_NOT_EXIST_2.swift' ({{.*}})

public var x = INVALID_DECL
// CHECK-NOT: INVALID_DECL


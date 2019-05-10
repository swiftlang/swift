// Ensure that a bogus output-file-map path does not crash the driver,
// but instead outputs a nice diagnostic.
//
// RUN: %empty-directory(%t)
// RUN: not %swiftc_driver -c %S/../Inputs/empty.swift -output-file-map %t/something-which-should-not-exist.json 2>&1 | %FileCheck %s
//
// CHECK: error: unable to load output file map '{{.*}}/something-which-should-not-exist.json': {{[Nn]}}o such file or directory
// CHECK-NOT: Assertion failed

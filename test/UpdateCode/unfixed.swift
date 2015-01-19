// RUN: not %swift-update -target %target-triple %s -o %t.remap -serialize-diagnostics-path %t.dia
// RUN: c-arcmt-test %t.remap | arcmt-test -verify-transformed-files %s.result
// RUN: c-index-test -read-diagnostics %t.dia > %t.deserialized_diagnostics.txt 2>&1
// RUN: FileCheck --input-file=%t.deserialized_diagnostics.txt %s

// CHECK: error: use of unresolved identifier
// CHECK: Number of diagnostics: 1

class Base {}
class Derived : Base {}

var b : Base
b as Derived

undeclared()

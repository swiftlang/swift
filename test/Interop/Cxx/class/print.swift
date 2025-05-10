// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out -Xfrontend -enable-experimental-cxx-interop -I %S/Inputs
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --check-prefix=CHECK 

// REQUIRES: executable_test

import SimpleStructs

func printCxxStructPrivateFields() {
    let s = HasPrivateFieldsOnly(42, "Hello")
    print(s)
}

func printCxxStructPublicFields() {
    let s = HasPublicFieldsOnly(42, "Hello")
    print(s)
}

func printCxxStructIntFields() {
    let s = HasIntFieldsOnly()
    print(s)
}

func printCxxStructPrivateAndPublicFields() {
    let s = HasPrivateAndPublicFields(24, 42, "Hello", "World")
    print(s)
}

printCxxStructPrivateFields() 
// CHECK: HasPrivateFieldsOnly()

printCxxStructIntFields()
// CHECK: HasIntFieldsOnly(b: 2, d: 4)

printCxxStructPublicFields()
// CHECK: HasPublicFieldsOnly(pubInt: 42, pubStr: {{.*}}.basic_string<CChar, {{.*}}, {{.*}})

printCxxStructPrivateAndPublicFields()
// CHECK: HasPrivateAndPublicFields(pubInt: 42, pubStr: {{.*}}.basic_string<CChar, {{.*}}, {{.*}})

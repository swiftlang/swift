// RUN: %target-run-simple-swift(-cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -I %S/Inputs) | %FileCheck %s

// REQUIRES: executable_test
// Metadata for foreign reference types is not supported on Windows.
// UNSUPPORTED: OS=windows-msvc

import SimpleStructs

func printCxxStructPrivateFields() {
    let s = HasPrivateFieldsOnly(1, 2)
    print(s)
}

func printCxxStructPublicFields() {
    let s = HasPublicFieldsOnly(1, 2)
    print(s)
}

func printCxxStructPrivatePublicProtectedFields() {
    let s = HasPrivatePublicProtectedFields(1, 2, 3, 4, 5, 6)
    print(s)
}

func printCxxStructNested() {
    let s = Outer()
    print(s)
}

func printCxxImmortalFRT() {
    let s = ImmortalFRT()
    print(s)
}

extension FRTCustomStringConvertible : CustomStringConvertible {
    public var description: String {
        return "FRTCustomStringConvertible(publ: \(publ))"
    }
}

func printCxxFRTCustomStringConvertible() {
    let s = FRTCustomStringConvertible()
    print(s)
}

func printCxxFRType() {
    let s = FRType()
    print(s)
}


printCxxStructPrivateFields() 
// CHECK: HasPrivateFieldsOnly()

printCxxStructPublicFields()
// CHECK: HasPublicFieldsOnly(publ1: 1, publ2: 2)

printCxxStructPrivatePublicProtectedFields()
// CHECK: HasPrivatePublicProtectedFields(publ1: 2, publ2: 6)

printCxxStructNested()
// CHECK: Outer(publStruct: {{.*}}.HasPrivatePublicProtectedFields(publ1: 8, publ2: 12))

printCxxImmortalFRT()
// CHECK: ImmortalFRT()

printCxxFRTCustomStringConvertible()
// CHECK: FRTCustomStringConvertible(publ: 2)

printCxxFRType()
// CHECK: FRType()

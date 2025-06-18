// RUN: %target-run-simple-swift(-cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -I %S/Inputs) | %FileCheck %s

// REQUIRES: executable_test

// Metadata for foreign reference types is not supported on Windows.
// UNSUPPORTED: OS=windows-msvc

// Temporarily disable when running with an older runtime (rdar://153205860)
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Printed

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

printCxxImmortalFRT()
// CHECK: ImmortalFRT()

printCxxFRTCustomStringConvertible()
// CHECK: FRTCustomStringConvertible(publ: 2)

printCxxFRType()
// CHECK: FRType()

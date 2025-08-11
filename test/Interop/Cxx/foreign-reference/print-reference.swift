// RUN: %target-run-simple-swift(-cxx-interoperability-mode=default -I %S/Inputs) | %FileCheck %s

// REQUIRES: executable_test

// Metadata for foreign reference types is not supported on Windows.
// UNSUPPORTED: OS=windows-msvc

// Temporarily disable when running with an older runtime (rdar://153205860)
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Printed

func printCxxImmortalFRT() {
    if #available(SwiftStdlib 6.2, *) {
        let s = ImmortalFRT()
        print(s)
    } else {
        print("runtime too old")
    }
}

@available(SwiftStdlib 5.9, *)
extension FRTCustomStringConvertible : CustomStringConvertible {
    public var description: String {
        return "FRTCustomStringConvertible(publ: \(publ))"
    }
}

func printCxxFRTCustomStringConvertible() {
    if #available(SwiftStdlib 5.9, *) {
        let s = FRTCustomStringConvertible()
        print(s)
    } else {
        print("runtime too old")
    }
}

func printCxxFRType() {
    if #available(SwiftStdlib 6.2, *) {
        let s = FRType()
        print(s)
    } else {
        print("runtime too old")
    }
}

printCxxImmortalFRT()
// CHECK: {{ImmortalFRT()|runtime too old}}

printCxxFRTCustomStringConvertible()
// CHECK: FRTCustomStringConvertible(publ: 2)

printCxxFRType()
// CHECK: {{FRType()|runtime too old}}

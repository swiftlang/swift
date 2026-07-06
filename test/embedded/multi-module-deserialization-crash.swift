// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t/A.swiftmodule %t/A.swift -enable-experimental-feature Embedded -enable-experimental-feature CheckImplementationOnly -parse-as-library
// RUN: %target-swift-frontend -c -I %t %t/B.swift -enable-experimental-feature Embedded -enable-experimental-feature CheckImplementationOnly

// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1
// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_CheckImplementationOnly

//--- A.swift
public final class Foo {
    @export(interface)
    public init() {}
    
    @export(interface)
    deinit {}
}

// this is load-bearing somehow: without this, no crash
func foo() {}

//--- B.swift
import A

func bar() {
    _ = Foo()
}

// UNSUPPORTED: windows

// Temporarily disable on AArch64 Linux (rdar://88451721)
// UNSUPPORTED: OS=linux-gnu && CPU=aarch64

// Temporarily disable on arm64e (rdar://88579818)
// UNSUPPORTED: CPU=arm64e

// RUN: %empty-directory(%t)

// RUN: %target-build-swift -Xfrontend -enable-anonymous-context-mangled-names %S/Inputs/PropertyWrappers.swift -parse-as-library -emit-module -emit-library -module-name Wrapper -o %t/Wrapper
// RUN: %target-build-swift -Xfrontend -enable-anonymous-context-mangled-names %s -parse-as-library -emit-module -emit-library -module-name property_wrapper_test %t/Wrapper -I%t/ -o %t/property_wrapper_test
// RUN: %target-swift-reflection-dump -binary-filename %t/property_wrapper_test | %FileCheck %s

// CHECK: FIELDS:
// CHECK-NEXT: =======
// CHECK: property_wrapper_test.Foo
// CHECK-NEXT: -------------------------
// CHECK-DAG: _bar: Wrapper.Doubled
// CHECK-NEXT: (struct Wrapper.Doubled)
// CHECK-DAG: _baz: property_wrapper_test.Tripled
// CHECK-NEXT: (struct property_wrapper_test.Tripled)

import Wrapper
@propertyWrapper
public struct Tripled {
    private var value: Int
    public var wrappedValue: Int {
        get { value }
        set { value = newValue * 3 }
    }
    public init(wrappedValue: Int) {
        value = wrappedValue * 3
    }
}

struct Foo {
    @Doubled
    var bar: Int
    @Tripled
    var baz: Int
}

// RUN: %target-swift-frontend -emit-ir %s -target %target-swift-5.9-abi-triple | %FileCheck %s

// REQUIRES: swift_feature_ParameterPacks

// Test that key paths on types containing parameter packs compile successfully.

struct Entry<each T> {
    let input: (repeat each T)
}

struct Container<each T> {
    let entries: [Entry<repeat each T>]

    var inputs: [(repeat each T)] {
        entries.map(\.input)
    }
}

// CHECK: @keypath = private global

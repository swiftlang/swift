// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-module-path %t/replace_opaque_type_public_assoc_type_m.swiftmodule %S/Inputs/replace_opaque_type_public_assoc_type_m.swift
// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -target %target-swift-5.1-abi-triple -I %t %s -verify
// RUN: %target-swift-emit-silgen -target %target-swift-5.1-abi-triple -I %t %s -verify

import replace_opaque_type_public_assoc_type_m

struct PiggyBack: Gesture {
    var action: () -> Void

    var body: some Gesture {
        action()
        return self
    }
}

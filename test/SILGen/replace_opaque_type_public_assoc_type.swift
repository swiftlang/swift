// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -disable-availability-checking -emit-module-path %t/replace_opaque_type_public_assoc_type_m.swiftmodule %S/Inputs/replace_opaque_type_public_assoc_type_m.swift
// RUN: %target-swift-emit-silgen -disable-availability-checking -I %t %s -verify

import replace_opaque_type_public_assoc_type_m

struct PiggyBack: Gesture {
    var action: () -> Void

    var body: some Gesture {
        action()
        return self
    }
}

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -O -target %target-swift-5.1-abi-triple -emit-module -emit-module-path=%t/R.swiftmodule -module-name=R %S/Inputs/opaque_result_type_internal_inlinable.swift
// RUN: %target-swift-frontend -O -I %t -target %target-swift-5.1-abi-triple -c -primary-file %s

import R

// This used to crash because when we were importing E.a the serialized sil
// contained underlying types that this module does not have access to (internal type `I`).

public func testIt() {
    var e = E.a
    print(e)
}

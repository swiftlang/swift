// RUN: %empty-directory(%t)
// RUN: %target-swiftxx-frontend %S/Inputs/swift-string.swift -module-name StringBridge \
// RUN:   -typecheck -emit-clang-header-path %t/StringBridge.h
// RUN: %target-swiftxx-frontend %s -typecheck -verify -I %t \
// RUN:   -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}swift-string.h \
// RUN:   -Xcc -fmodule-map-file=%S/Inputs/swift-string.modulemap -Xcc -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR

import SwiftStringRoundTrip

func testDisabled(_ value: Swift.String) {
  hello(value) // expected-error {{cannot find 'hello' in scope}}
  _ = makeString() // expected-error {{'makeString()' is unavailable: return type is unavailable in Swift}}
}

func testUnrelated(_ value: SwiftStringRoundTrip.String) -> SwiftStringRoundTrip.String {
  echoUnrelatedString(value)
}

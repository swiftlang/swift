// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Lib.swiftmodule %S/Inputs/attr_usableFromInline_protocol_hole_helper.swift
// RUN: %target-typecheck-verify-swift -I %t -verify-ignore-unknown

import Lib

func test(_ obj: PublicProtocol) {
  obj.publicExtensionMethod()
  obj.ufiExtensionMethod() // expected-error {{inaccessible}}
  obj.internalExtensionMethod() // expected-error {{inaccessible}}
}

func test(_ obj: PublicImpl) {
  obj.publicExtensionMethod()
  obj.ufiExtensionMethod() // expected-error {{inaccessible}}
  obj.internalExtensionMethod() // expected-error {{inaccessible}}
}

func test(_ obj: UFIImpl) {
  obj.publicExtensionMethod() // This being accessible is the "hole".
  obj.ufiExtensionMethod() // expected-error {{inaccessible}}
  obj.internalExtensionMethod() // expected-error {{inaccessible}}
}

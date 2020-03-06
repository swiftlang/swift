// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/modules

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -module-name MismatchedNames -F %S/Inputs/frameworks -o %t/modules/MismatchedNames.swiftmodule %S/Inputs/frameworks/MismatchedNames.framework/MismatchedNamesSwift.swift

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks -I %t/modules %s -verify -verify-ignore-unknown

// REQUIRES: objc_interop


import Foundation
import ObjectiveC
import MismatchedNames

// Ensure that we don't crash when looking up via Objective-C selector and
// finding a method in the generated header that (due to a missing swift_name
// attribute) isn't "obviously" matching its corresponding Swift method.
//
// rdar://problem/60046206 covers this crash.
func test() {
  _ = Selector(("usingIndex:")) // expected-warning{{use '#selector' instead of explicitly constructing a 'Selector'}}
}

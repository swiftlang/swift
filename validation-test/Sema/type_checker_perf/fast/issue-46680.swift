// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -solver-scope-threshold=100
// REQUIRES: objc_interop

// https://github.com/swiftlang/swift/issues/46680

import Foundation

// FIXME: This should be a scale-test but it doesn't allow passing `%clang-importer-sdk`

func slow() {
  let x = [
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0),
    NSNumber(value: 0)
  ]
}

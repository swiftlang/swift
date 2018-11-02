// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s
// REQUIRES: objc_interop

import Foundation

protocol P {}

class C {
    init<T: NSObject>(values: [T]) where T: P {}
}

func foo<T: NSObject>(value: T) where T: P {
  _ = C(values: [value]) // Ok
}

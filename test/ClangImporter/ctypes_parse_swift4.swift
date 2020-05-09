// RUN: %target-typecheck-verify-swift %clang-importer-sdk -swift-version 4

import ctypes

func testArrays() {
  // It would also be nice to warn here about the arrays being too short, but
  // that's probably beyond us for a while.
  staticBoundsArray([])
  staticBoundsArray(nil) // expected-error {{'nil' is not compatible with expected argument type 'UnsafePointer<Int8>'}}
}

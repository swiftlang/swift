// RUN: %target-typecheck-verify-swift -enable-experimental-feature Embedded

// REQUIRES: swift_feature_Embedded

import SwiftPlatform

func create() -> Int {
  _ = _swift_thread_create(nil, nil) // expected-error {{'nil' is not compatible with expected argument type '__swift_thread_start_t'}}
  return _swift_thread_create({ _ in nil }, nil)
}

// RUN: %target-parse-verify-swift %clang-importer-sdk

// REQUIRES: objc_interop

// This test is deliberately using the importer SDK because not all SDKs have
// the protocols underlying the dispatch types ultimately inheriting from
// NSObjectProtocol.

import Dispatch
import Foundation

func test(_ queue: dispatch_queue_t) {
  let base: NSObjectProtocol = queue
  let _: dispatch_object_t = queue

  let _ = base as? dispatch_queue_t

  // Make sure the dispatch types are actually distinct types!
  let _ = queue as dispatch_source_t // expected-error {{'dispatch_queue_t' (aka 'OS_dispatch_queue') is not convertible to 'dispatch_source_t'}} {{17-19=as!}}
}

// Make sure you can extend a dispatch type via its common name.
extension dispatch_queue_t {
  func async(_ block: () -> Void) {
    dispatch_async(self, block)
  }
}

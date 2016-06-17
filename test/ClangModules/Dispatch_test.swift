// RUN: %target-parse-verify-swift %clang-importer-sdk

// REQUIRES: objc_interop

// This test is deliberately using the importer SDK because not all SDKs have
// the protocols underlying the dispatch types ultimately inheriting from
// NSObjectProtocol.

import Dispatch
import Foundation

func test(_ queue: dispatch_queue_t) {} // expected-error {{'dispatch_queue_t' is unavailable}}

// Make sure you can extend a dispatch type via its common name.
extension dispatch_queue_t {} // expected-error {{'dispatch_queue_t' is unavailable}}

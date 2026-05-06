// RUN: %target-swift-frontend -strict-concurrency=complete -swift-version 5 -parse-as-library -emit-sil -verify %s -o /dev/null

// REQUIRES: objc_interop
// REQUIRES: concurrency

import ObjectiveC

@MainActor
final class SimpleDeinitTestWithObjC: NSObject {
    let name: String = ""
    deinit {
        _ = { self.name }
    }
}

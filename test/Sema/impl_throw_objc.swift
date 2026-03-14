// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

import Foundation
@objc protocol Tracker {
    func track(event: String) throws
}

class ApplicationTracker: NSObject, Tracker {
    func track(event: String) {} // expected-error {{satisfying a throwing '@objc' method with a non-throwing method is not supported}}
}

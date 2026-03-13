// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10
// REQUIRES: objc_interop

// https://github.com/swiftlang/swift/issues/54143

import Foundation

extension Int {

    public var days: TimeInterval {
        return TimeInterval(self) * 24.hours
    }

    public var hours: TimeInterval {
        return TimeInterval(self) * 60.minutes
    }

    public var minutes: TimeInterval {
        return TimeInterval(self) * 60.seconds
    }

    public var seconds: TimeInterval {
        return TimeInterval(self)
    }
}

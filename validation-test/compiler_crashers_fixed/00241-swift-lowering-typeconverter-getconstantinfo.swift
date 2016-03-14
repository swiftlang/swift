// RUN: %target-swift-frontend %s -parse

// REQUIRES: objc_interop

// Issue found by https://github.com/fluidsonic (Marc Knaup)

import Foundation
extension NSSet {
    convenience init<T>(array: Array<T>) {
    }
}

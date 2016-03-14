// RUN: not %target-swift-frontend %s -parse

// Issue found by https://github.com/fluidsonic (Marc Knaup)

import Foundation
extension NSSet {
    convenience init(array: Array) {
    }
}

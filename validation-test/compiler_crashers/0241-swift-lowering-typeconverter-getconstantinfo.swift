// RUN: rm -rf %t/clang-module-cache
// RUN: not --crash %swift %s -sdk %sdk -module-cache-path %t/clang-module-cache -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/fluidsonic (Marc Knaup)

import Foundation
extension NSSet {
    convenience init<T>(array: Array<T>) {
    }
}

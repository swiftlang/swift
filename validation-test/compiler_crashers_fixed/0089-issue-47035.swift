// RUN: %target-swift-frontend -primary-file %s -emit-ir

// REQUIRES: objc_interop

// https://github.com/apple/swift/issues/47035

import Foundation

extension MutableCollection {
  typealias SubSequence = Slice<Data>
}

print(type(of: Data.self.SubSequence.self))

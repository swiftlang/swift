// RUN: %target-swift-frontend -primary-file %s -emit-ir

// REQUIRES: objc_interop

import Foundation

extension MutableCollection {
  typealias SubSequence = MutableRangeReplaceableRandomAccessSlice<Data>
}

print(type(of: Data.self.SubSequence.self))

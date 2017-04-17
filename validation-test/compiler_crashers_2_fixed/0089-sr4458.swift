// RUN: %target-swift-frontend -primary-file %s -emit-ir
import Foundation

extension _MutableIndexable {
  typealias SubSequence = MutableRangeReplaceableRandomAccessSlice<Data>
}

print(type(of: Data.self.SubSequence.self))

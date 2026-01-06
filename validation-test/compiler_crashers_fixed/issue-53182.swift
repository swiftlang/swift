// RUN: %target-swift-frontend -emit-ir %s

// https://github.com/apple/swift/issues/53182

protocol DateGroupedCollection: Collection {
  typealias DictionaryType = [String: Int]

  typealias Index = DictionaryType.Index
  typealias Element = DictionaryType.Element
}

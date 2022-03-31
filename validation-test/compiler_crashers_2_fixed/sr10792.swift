// RUN: %target-swift-frontend -emit-ir %s

protocol DateGroupedCollection: Collection {
  typealias DictionaryType = [String: Int]

  typealias Index = DictionaryType.Index
  typealias Element = DictionaryType.Element
}

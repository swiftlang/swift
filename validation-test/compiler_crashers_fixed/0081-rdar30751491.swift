// RUN: not %target-swift-frontend %s -typecheck -o /dev/null

class FakeDictionary<KeyType, ValueType> : ExpressibleByDictionaryLiteral {
  convenience required init(dictionaryLiteral elements: (FakeDictionary.Key, FakeDictionary.Value)...) {
    self.init()
  }
}

// RUN: %target-swift-frontend -primary-file %s -emit-ir

extension Dictionary  {
  init<S: Sequence>(grouping elements: S, by keyForValue: (S.Iterator.Element) -> Key)
  where Array<S.Iterator.Element> == Value
  {
    self = [:]
    for value in elements {
      var values = self[keyForValue(value)] ?? []
      values.append(value)
      self[keyForValue(value)] = values
    }
  }
}

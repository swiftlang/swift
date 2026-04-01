// RUN: %target-swift-frontend -emit-ir %s

extension Dictionary {
  init<S: Sequence>(grouping elements: S, by keyForValue: (S.Iterator.Element) -> Key)
    where Value : RangeReplaceableCollection, Value.Iterator.Element == S.Iterator.Element {
    self = [:]
  }
}

let names = ["Patti", "Aretha", "Anita", "Gladys"]
print(Dictionary(grouping: names, by: { $0.utf16.count }))

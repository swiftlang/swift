// RUN: %target-typecheck-verify-swift

// With filtering Set.filter is preferred over Sequence.filter but that
// results in a worse solution than if it did use Sequence.filter.

extension Set {
  func myFilter<E: Error>(_: (Element) throws(E) -> Bool) throws(E) -> Set<Element> { self }
  func myFilter2(_: (Element) throws -> Bool) rethrows -> Set<Element> { self }
}

extension Sequence {
  func myFilter<E: Error>(_: (Element) throws(E) -> Bool) throws(E) -> [Self.Element] { [] }
  func myFilter2(_: (Element) throws -> Bool) rethrows -> [Self.Element] { [] }
}

func test(clusterID: Int, values: [Int: Set<Int>]) {
  let matching1 = values[clusterID]?.myFilter { _ in false } ?? []
  let matching2 = values[clusterID]?.myFilter2 { _ in false } ?? []
  
  let _: [Int] = matching1 // Ok
  let _: [Int] = matching2 // Ok
}

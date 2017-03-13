// RUN: %target-swift-frontend -typecheck -primary-file %s

protocol AnyCodeUnits_ {
  typealias IndexDistance = Int64
  typealias Index = Int64
  typealias Element = UInt32
  var startIndex: Index { get }
  var endIndex: Index { get }
  func index(after: Index) -> Index
  func index(before: Index) -> Index
  func index(_ i: Index, offsetBy: Int64) -> Index
  subscript(i: Index) -> Element { get }
  subscript(r: Range<Index>) -> AnyCodeUnits { get }
}

struct AnyCodeUnits : RandomAccessCollection, AnyCodeUnits_ {
  let me: AnyCodeUnits_
  typealias Indices = DefaultRandomAccessIndices<AnyCodeUnits>
  var startIndex: Int64 { return me.startIndex }
  var endIndex: Int64 { return me.endIndex }
  func index(after i: Index) -> Index { return me.index(after: i) }
  func index(before i: Index) -> Index { return me.index(before: i) }
  func index(_ i: Index, offsetBy: Int64) -> Index { return me.index(i, offsetBy: i) }
  subscript(i: Index) -> Element { return me[i] }
  subscript(r: Range<Index>) -> AnyCodeUnits { return me[r] }
}


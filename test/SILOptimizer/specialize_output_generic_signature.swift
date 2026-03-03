// RUN: %target-swift-frontend -emit-sil -O %s

extension Array: Comparable where Element: Comparable {
  public static func < (lhs: Array<Element>, rhs: Array<Element>) -> Bool {
    // Contents do not matter
    for (l, r) in zip(lhs, rhs) { if l != r { return l < r } }
    return lhs.count < rhs.count
  }
}

public struct Wrapper<Symbol: Hashable & Comparable>: Hashable {
  public var partitions: PartitionSet<Array<Symbol>>
}

public struct PartitionSet<Symbol: Hashable & Comparable>: Equatable, Hashable {
  public var partitions: Set<Set<Symbol>>
}

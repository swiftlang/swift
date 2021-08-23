// RUN: %target-swift-frontend -emit-ir -g -primary-file %s

// https://bugs.swift.org/browse/SR-14016

public struct PowerCollection<C : Collection> : Collection {
  public typealias Index = [C.Index]
  public typealias Element = [C.Element]

  public var startIndex, endIndex: Index

  public subscript(position: Index) -> [C.Element] {
    return []
  }

  public func index(after i: Index) -> Index {
    return i
  }

}

extension Array : Comparable where Element : Comparable {
  public static func < (lhs: [Element], rhs: [Element]) -> Bool {
    return false
  }
}

/*
 This source file is part of the SE-0529 reference implementation

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift System project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
*/

// MARK: - Internal invariants

#if FILEPATH_PACKAGE
@inline(__always)
internal func _internalInvariant(
  _ condition: @autoclosure () -> Bool,
  _ message: @autoclosure () -> String = "",
  file: StaticString = #file,
  line: UInt = #line
) {
  assert(condition(), message(), file: file, line: line)
}
#endif

// MARK: - Slice helpers

extension Slice where Element: Equatable {
  internal mutating func _eat(if p: (Element) -> Bool) -> Element? {
    guard let s = self.first, p(s) else { return nil }
    self = self.dropFirst()
    return s
  }
  internal mutating func _eat(_ e: Element) -> Element? {
    _eat(if: { $0 == e })
  }

  internal mutating func _eat(count c: Int) -> Slice {
    defer { self = self.dropFirst(c) }
    return self.prefix(c)
  }

  internal mutating func _eatSequence<C: Collection>(
    _ es: C
  ) -> Slice? where C.Element == Element {
    guard self.starts(with: es) else { return nil }
    return _eat(count: es.count)
  }

  internal mutating func _eatUntil(_ idx: Index) -> Slice {
    precondition(idx >= startIndex && idx <= endIndex)
    defer { self = self[idx...] }
    return self[..<idx]
  }

  internal mutating func _eatWhile(
    _ p: (Element) -> Bool
  ) -> Slice? {
    let idx = firstIndex(where: { !p($0) }) ?? endIndex
    guard idx != startIndex else { return nil }
    return _eatUntil(idx)
  }
}

extension MutableCollection where Element: Equatable {
  mutating func _replaceAll(_ e: Element, with new: Element) {
    for idx in self.indices {
      if self[idx] == e { self[idx] = new }
    }
  }
}

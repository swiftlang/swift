/*
 This source file is part of the Swift.org open source project

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
 See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

// MARK: - ASCII byte conversion

@available(SwiftStdlib 9999, *)
extension String {
  /// A view over the string's bytes as `FilePath.CodeUnit`s, lazily mapped.
  ///
  /// Intended for ASCII string literals used as match/replace tokens — no
  /// allocation, materialized only when iterated. Non-ASCII scalars trap.
  internal var _asciiBytes: LazyMapCollection<
    String.UnicodeScalarView, FilePath.CodeUnit
  > {
    self.unicodeScalars.lazy.map { FilePath.CodeUnit(_ascii: $0) }
  }
}

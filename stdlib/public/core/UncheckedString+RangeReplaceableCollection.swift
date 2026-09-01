//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 9999, *)
extension UncheckedString: RangeReplaceableCollection {

  public mutating func replaceSubrange<C>(
    _ subrange: Range<Self.Index>,
    with newElements: C
  ) where C: Collection, Self.Element == C.Element {
    precondition(subrange.lowerBound >= 0 && subrange.upperBound <= storage.count)

    let newCount = storage.count + newElements.count
    if newCount == 0 {
      storage = .empty
    } else if newCount <= SmallUncheckedStringStorage<Element>.capacity {
      switch storage {
        case .empty:
          storage = .small(SmallUncheckedStringStorage(newElements))
        case .small(_):
          var chars = withCharacterData { $0.withUnsafeBufferPointer { unsafe Array($0) } }
          chars.replaceSubrange(subrange, with: newElements)
          storage = .small(SmallUncheckedStringStorage(chars))
        default:
          fatalError("UncheckedString is unexpectedly not small when it should be")
      }
    } else {
      switch storage {
        case .empty:
          var chars = Array(newElements)
          chars.append(0)
          storage = .dynamic(
            DynamicUncheckedStringStorage(
              characters: chars,
              flags: [.nulTerminated]
            )
          )
        case .small(_), .immortal(_):
          var chars = withCharacterData { $0.withUnsafeBufferPointer { unsafe Array($0) } }
          chars.replaceSubrange(subrange, with: newElements)
          chars.append(0)
          storage = .dynamic(
            DynamicUncheckedStringStorage(
              characters: chars,
              flags: [.nulTerminated]
            )
          )
        case .dynamic(var rawStorage):
          rawStorage.characters.replaceSubrange(subrange, with: newElements)
          storage = .dynamic(rawStorage)
      }
    }
  }

}

@available(SwiftStdlib 9999, *)
extension UncheckedString {
  /// Concatenates two `UncheckedString` values.
  ///
  /// `RangeReplaceableCollection` already provides a fully generic `+`
  /// (`Self, Other: RangeReplaceableCollection where Other.Element == Self.Element`),
  /// but its `Other` parameter is a bare generic parameter that doesn't name
  /// any concrete type. When an operand is a string literal, the constraint
  /// solver has nothing there to offer as a candidate binding beyond the
  /// literal's own default (`String`) -- which doesn't satisfy
  /// `Element == Element`, so the whole expression fails to type-check
  /// whenever both operands are literals, regardless of context.
  ///
  /// This concrete, non-generic overload gives the solver a directly
  /// nameable target type (`UncheckedString<Element>`) to try instead, which
  /// resolves the problem for the common case of concatenating two
  /// `UncheckedString` values (including literals).
  public static func + (
    lhs: UncheckedString<Element>,
    rhs: UncheckedString<Element>
  ) -> UncheckedString<Element> {
    var result = lhs
    result.append(contentsOf: rhs)
    return result
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedSubString: RangeReplaceableCollection {

  public mutating func replaceSubrange<C>(
    _ subrange: Range<Self.Index>,
    with newElements: C
  ) where C: Collection, Self.Element == C.Element {
    precondition(subrange.lowerBound >= startIndex && subrange.lowerBound < endIndex)
    precondition(subrange.upperBound >= startIndex && subrange.upperBound <= endIndex)

    base.replaceSubrange(subrange, with: newElements)
  }

}

//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// CR and LF are common special cases in grapheme breaking logic
private var _CR: UInt8 { return 0x0d }
private var _LF: UInt8 { return 0x0a }

internal func _hasGraphemeBreakBetween(
  _ lhs: Unicode.Scalar, _ rhs: Unicode.Scalar
) -> Bool {

  // CR-LF is a special case: no break between these
  if lhs == Unicode.Scalar(_CR) && rhs == Unicode.Scalar(_LF) {
    return false
  }

  // Whether the given scalar, when it appears paired with another scalar
  // satisfying this property, has a grapheme break between it and the other
  // scalar.
  func hasBreakWhenPaired(_ x: Unicode.Scalar) -> Bool {
    // TODO: This doesn't generate optimal code, tune/re-write at a lower
    // level.
    //
    // NOTE: Order of case ranges affects codegen, and thus performance. All
    // things being equal, keep existing order below.
    switch x.value {
    // Unified CJK Han ideographs, common and some supplemental, amongst
    // others:
    //   U+3400 ~ U+A4CF
    case 0x3400...0xa4cf: return true

    // Repeat sub-300 check, this is beneficial for common cases of Latin
    // characters embedded within non-Latin script (e.g. newlines, spaces,
    // proper nouns and/or jargon, punctuation).
    //
    // NOTE: CR-LF special case has already been checked.
    case 0x0000...0x02ff: return true

    // Non-combining kana:
    //   U+3041 ~ U+3096
    //   U+30A1 ~ U+30FC
    case 0x3041...0x3096: return true
    case 0x30a1...0x30fc: return true

    // Non-combining modern (and some archaic) Cyrillic:
    //   U+0400 ~ U+0482 (first half of Cyrillic block)
    case 0x0400...0x0482: return true

    // Modern Arabic, excluding extenders and prependers:
    //   U+061D ~ U+064A
    case 0x061d...0x064a: return true

    // Precomposed Hangul syllables:
    //   U+AC00 ~ U+D7AF
    case 0xac00...0xd7af: return true

    // Common general use punctuation, excluding extenders:
    //   U+2010 ~ U+2029
    case 0x2010...0x2029: return true

    // CJK punctuation characters, excluding extenders:
    //   U+3000 ~ U+3029
    case 0x3000...0x3029: return true

    // Full-width forms:
    //   U+FF01 ~ U+FF9D
    case 0xFF01...0xFF9D: return true

    default: return false
    }
  }
  return hasBreakWhenPaired(lhs) && hasBreakWhenPaired(rhs)
}

extension _StringGuts {
  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func isOnGraphemeClusterBoundary(_ i: String.Index) -> Bool {
    guard i.transcodedOffset == 0 else { return false }

    let offset = i._encodedOffset
    if offset == 0 || offset == self.count { return true }

    guard isOnUnicodeScalarBoundary(i) else { return false }

    let str = String(self)
    return i == str.index(before: str.index(after: i))
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _opaqueCharacterStride(startingAt i: Int) -> Int {
    if _slowPath(isForeign) {
      return _foreignOpaqueCharacterStride(startingAt: i)
    }

    let nextIdx = withFastUTF8 { utf8 in
      nextBoundary(startingAt: i) {
        let (scalar, len) = _decodeScalar(utf8, startingAt: $0)
        return (scalar, $0 &+ len)
      }
    }

    return nextIdx &- i
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _opaqueCharacterStride(endingAt i: Int) -> Int {
    if _slowPath(isForeign) {
      return _foreignOpaqueCharacterStride(endingAt: i)
    }

    let previousIdx = withFastUTF8 { utf8 in
      previousBoundary(endingAt: i) {
        let (scalar, len) = _decodeScalar(utf8, endingAt: $0)
        return (scalar, $0 &- len)
      }
    }

    return i &- previousIdx
  }

  @inline(never)
  @_effects(releasenone)
  private func _foreignOpaqueCharacterStride(startingAt i: Int) -> Int {
#if _runtime(_ObjC)
    _internalInvariant(isForeign)

    let nextIdx = nextBoundary(startingAt: i) {
      let scalars = String.UnicodeScalarView(self)
      let idx = String.Index(_encodedOffset: $0)

      let scalar = scalars[idx]
      let nextIdx = scalars.index(after: idx)

      return (scalar, nextIdx._encodedOffset)
    }

    return nextIdx &- i
#else
  fatalError("No foreign strings on Linux in this version of Swift")
#endif
  }

  @inline(never)
  @_effects(releasenone)
  private func _foreignOpaqueCharacterStride(endingAt i: Int) -> Int {
#if _runtime(_ObjC)
    _internalInvariant(isForeign)

    let previousIdx = previousBoundary(endingAt: i) {
      let scalars = String.UnicodeScalarView(self)
      let idx = String.Index(_encodedOffset: $0)

      let previousIdx = scalars.index(before: idx)
      let scalar = scalars[previousIdx]

      return (scalar, previousIdx._encodedOffset)
    }

    return i &- previousIdx
#else
  fatalError("No foreign strings on Linux in this version of Swift")
#endif
  }
}

internal struct _GraphemeBreakingState {
  // When walking forwards in a string, we need to know whether or not we've
  // entered an emoji sequence to be able to eventually break after all of the
  // emoji's various extenders and zero width joiners. This bit allows us to
  // keep track of whether or not we're still in an emoji sequence when deciding
  // to break.
  var isInEmojiSequence: Bool = false

  // When walking forward in a string, we need to not break on emoji flag
  // sequences. Emoji flag sequences are composed of 2 regional indicators, so
  // when we see our first (.regionalIndicator, .regionalIndicator) decision,
  // we need to know to return false in this case. However, if the next scalar
  // is another regional indicator, we reach the same decision rule, but in this
  // case we actually need to break there's a boundary between emoji flag
  // sequences.
  var shouldBreakRI: Bool = false
}

extension _StringGuts {
  // Returns the stride of the next grapheme cluster at the previous boundary
  // offset.
  internal func nextBoundary(
    startingAt index: Int,
    nextScalar: (Int) -> (Unicode.Scalar, end: Int)
  ) -> Int {
    _internalInvariant(index != endIndex._encodedOffset)
    var state = _GraphemeBreakingState()
    var index = index

    while true {
      let (scalar1, nextIdx) = nextScalar(index)
      index = nextIdx

      guard index != endIndex._encodedOffset else {
        break
      }

      let (scalar2, _) = nextScalar(index)

      if shouldBreak(scalar1, between: scalar2, &state, index) {
        break
      }
    }

    return index
  }

  // Returns the stride of the previous grapheme cluster at the current boundary
  // offset.
  internal func previousBoundary(
    endingAt index: Int,
    previousScalar: (Int) -> (Unicode.Scalar, start: Int)
  ) -> Int {
    _internalInvariant(index != startIndex._encodedOffset)
    var state = _GraphemeBreakingState()
    var index = index

    while true {
      let (scalar2, previousIdx) = previousScalar(index)
      index = previousIdx

      guard index != startIndex._encodedOffset else {
        break
      }

      let (scalar1, _) = previousScalar(index)

      if shouldBreak(
        scalar1,
        between: scalar2,
        &state,
        index,
        isBackwards: true
      ) {
        break
      }
    }

    return index
  }
}

extension _StringGuts {
  // The "algorithm" that determines whether or not we should break between
  // certain grapheme break properties.
  //
  // This is based off of the Unicode Annex #29 for [Grapheme Cluster Boundary
  // Rules](https://unicode.org/reports/tr29/#Grapheme_Cluster_Boundary_Rules).
  internal func shouldBreak(
    _ scalar1: Unicode.Scalar,
    between scalar2: Unicode.Scalar,
    _ state: inout _GraphemeBreakingState,
    _ index: Int,
    isBackwards: Bool = false
  ) -> Bool {
    // GB3
    if scalar1.value == 0xD, scalar2.value == 0xA {
      return false
    }

    if _hasGraphemeBreakBetween(scalar1, scalar2) {
      return true
    }

    let x = Unicode._GraphemeBreakProperty(from: scalar1)
    let y = Unicode._GraphemeBreakProperty(from: scalar2)

    // This variable and the defer statement help toggle the isInEmojiSequence
    // state variable to false after every decision of 'shouldBreak'. If we
    // happen to see a rhs .extend or .zwj, then it's a signal that we should
    // continue treating the current grapheme cluster as an emoji sequence.
    var enterEmojiSequence = false

    defer {
      state.isInEmojiSequence = enterEmojiSequence
    }

    switch (x, y) {

    // Fast path: If we know our scalars have no properties the decision is
    //            trivial and we don't need to crawl to the default statement.
    case (.any, .any):
      return true

    // GB4
    case (.control, _):
      return true

    // GB5
    case (_, .control):
      return true

    // GB6
    case (.l, .l),
         (.l, .v),
         (.l, .lv),
         (.l, .lvt):
      return false

    // GB7
    case (.lv, .v),
         (.v, .v),
         (.lv, .t),
         (.v, .t):
      return false

    // GB8
    case (.lvt, .t),
         (.t, .t):
      return false

    // GB9 (partial GB11)
    case (_, .extend),
         (_, .zwj):

      // If we're currently in an emoji sequence, then extends and ZWJ help
      // continue the grapheme cluster by combining more scalars later. If we're
      // not currently in an emoji sequence, but our lhs scalar is a pictograph,
      // then that's a signal that it's the start of an emoji sequence.
      if state.isInEmojiSequence || x == .extendedPictographic {
        enterEmojiSequence = true
      }

      return false

    // GB9a
    case (_, .spacingMark):
      return false

    // GB9b
    case (.prepend, _):
      return false

    // GB11
    case (.zwj, .extendedPictographic):
      if isBackwards {
        return !checkIfInEmojiSequence(index)
      }

      return !state.isInEmojiSequence

    // GB12 & GB13
    case (.regionalIndicator, .regionalIndicator):
      if isBackwards {
        return countRIs(index)
      }

      defer {
        state.shouldBreakRI.toggle()
      }

      return state.shouldBreakRI

    // GB999
    default:
      return true
    }
  }

  // When walking backwards, it's impossible to know whether we were in an emoji
  // sequence without walking further backwards. This walks the string backwards
  // enough until we figure out whether or not to break our
  // (.zwj, .extendedPictographic) question. For example:
  //
  // Scalar view #1:
  //
  //     [.control, .zwj, .extendedPictographic]
  //                     ^
  //                     | = To determine whether or not we break here, we need
  //                         to see the previous scalar's grapheme property.
  //          ^
  //          | = This is neither .extendedPictographic nor .extend, thus we
  //              were never in an emoji sequence, so break between the .zwj
  //              and .extendedPictographic.
  //
  // Scalar view #2:
  //
  //     [.extendedPictographic, .zwj, .extendedPictographic]
  //                                  ^
  //                                  | = Same as above, move backwards one to
  //                                      view the previous scalar's property.
  //                ^
  //                | = This is an .extendedPictographic, so this indicates that
  //                    we are in an emoji sequence, so we should NOT break
  //                    between the .zwj and .extendedPictographic.
  //
  // Scalar view #3:
  //
  //     [.extendedPictographic, .extend, .extend, .zwj, .extendedPictographic]
  //                                                    ^
  //                                                    | = Same as above
  //                                         ^
  //                                         | = This is an .extend which means
  //                                             there is a potential emoji
  //                                             sequence, walk further backwards
  //                                             to find an .extendedPictographic.
  //
  //                               <-- = Another extend, go backwards more.
  //                ^
  //                | = We found our starting .extendedPictographic letting us
  //                    know that we are in an emoji sequence so our initial
  //                    break question is answered as NO.
  internal func checkIfInEmojiSequence(
    _ index: Int
  ) -> Bool {
    var emojiIdx = String.Index(_encodedOffset: index)

    guard emojiIdx != startIndex else {
      return false
    }

    let scalars = String.UnicodeScalarView(self)
    scalars.formIndex(before: &emojiIdx)

    while emojiIdx != startIndex {
      scalars.formIndex(before: &emojiIdx)
      let scalar = scalars[emojiIdx]

      let gbp = Unicode._GraphemeBreakProperty(from: scalar)

      switch gbp {
      case .extend:
        continue
      case .extendedPictographic:
        return true
      default:
        return false
      }
    }

    return false
  }
  
  // When walking backwards, it's impossible to know whether we break when we
  // see our first (.regionalIndicator, .regionalIndicator) without walking
  // further backwards. This walks the string backwards enough until we figure
  // out whether or not to break these RIs. For example:
  //
  // Scalar view #1:
  //
  //     [.control, .regionalIndicator, .regionalIndicator]
  //                                   ^
  //                                   | = To be able to know whether or not to
  //                                       break these two, we need to walk
  //                                       backwards to determine if there were
  //                                       any previous .regionalIndicators in
  //                                       a row.
  //         ^
  //         | = Not a .regionalIndicator, so our total riCount is 0 and 0 is
  //             even thus we do not break.
  //
  // Scalar view #2:
  //
  //     [.control, .regionalIndicator, .regionalIndicator, .regionalIndicator]
  //                                                       ^
  //                                                       | = Same as above
  //                         ^
  //                         | = This is a .regionalIndicator, so continue
  //                             walking backwards for more of them. riCount is
  //                             now equal to 1.
  //         ^
  //         | = Not a .regionalIndicator. riCount = 1 which is odd, so break
  //             the last two .regionalIndicators.
  internal func countRIs(
    _ index: Int
  ) -> Bool {
    var riIdx = String.Index(_encodedOffset: index)

    guard riIdx != startIndex else {
      return false
    }

    var riCount = 0

    let scalars = String.UnicodeScalarView(self)
    scalars.formIndex(before: &riIdx)

    while riIdx != startIndex {
      scalars.formIndex(before: &riIdx)
      let scalar = scalars[riIdx]

      let gbp = Unicode._GraphemeBreakProperty(from: scalar)

      guard gbp == .regionalIndicator else {
        break
      }

      riCount += 1
    }

    return riCount & 1 != 0
  }
}

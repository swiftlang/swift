//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// GraphemeWalker is a tool to determine the Unicode grapheme break boundaries.
// This works on strings of UTF8 and UTF16 (for bridged NSString) and returns
// either the next or previous stride of the grapheme cluster. For example:
//
//   let someFlags = "🇺🇸🇬🇧"
//
// Byte and Scalar Representation:
//
//   |(       🇺       ) (        🇸       )|(        🇬       ) (        🇧      )|
//   [240, 159, 135, 186, 240, 159, 135, 184, 240, 159, 135, 172, 240, 159, 135, 167]
//    ^                                        ^
//    | = String's start index                 | = String's next index
//
//
// In this representation we see that there are a total of 16 utf8 code units,
// but we don't want to report the count of a string as 16 to a user to sees
// two emoji "character"s. We also see a total of 4 Unicode scalars, 2 of which
// make up the single U.S. flag (two regional indicators), i.e. these two create
// a "grapheme cluster" who represent a singular grapheme or in Swift's terms,
// "Character". The job of GraphemeWalker is to determine these grapheme
// cluster boundaries and to correctly report that the string "🇺🇸🇬🇧" has two
// characters (🇺🇸 and 🇬🇧).
extension Unicode {
  internal struct _GraphemeWalker {
    let scalars: String.UnicodeScalarView

    init(_ scalars: String.UnicodeScalarView) {
      self.scalars = scalars
    }
  }
}

extension Unicode._GraphemeWalker {
  internal struct State {
    var index: String.Index

    var isBackwards: Bool = false
    var isInEmojiSequence: Bool = false
    var shouldBreakRI: Bool = false

    static func forward(_ index: String.Index) -> State {
      State(index: index)
    }

    static func backward(_ index: String.Index) -> State {
      State(index: index, isBackwards: true)
    }
  }
}

extension Unicode._GraphemeWalker {
  func advanceScalar(_ state: inout State) -> Unicode.Scalar? {
    guard state.index != scalars.endIndex else {
      return nil
    }

    let scalar = scalars[state.index]
    scalars.formIndex(after: &state.index)
    
    return scalar
  }

  func retreatScalar(
    _ state: inout State
  ) -> (scalar: Unicode.Scalar, scalarIndex: String.Index)? {
    guard state.index != scalars.startIndex else {
      return nil
    }

    let scalar = scalars[state.index]
    let scalarIndex = state.index
    scalars.formIndex(before: &state.index)

    // When we go to ask if we should break between the scalar before ours and
    // the one we return, the index will be a scalar before the breakpoint, so
    // provide the index of the current scalar and retreat state's a scalar
    // back.
    return (scalar, scalarIndex)
  }

  // Returns the index of the next grapheme cluster boundary.
  func nextBoundary(
    startingAt index: String.Index
  ) -> String.Index {
    var state = State.forward(index)

    while let scalar1 = advanceScalar(&state) {
      guard state.index != scalars.endIndex else {
        break
      }

      let scalar2 = scalars[state.index]

      if shouldBreak(scalar1, between: scalar2, &state) {
        break
      }
    }

    return state.index
  }

  // Returns the index of the previous grapheme cluster boundary.
  func previousBoundary(
    endingAt index: String.Index
  ) -> String.Index {
    var state = State.backward(index)

    scalars.formIndex(before: &state.index)

    while let (scalar2, scalar2Idx) = retreatScalar(&state) {
      let scalar1 = scalars[state.index]

      if shouldBreak(scalar1, between: scalar2, &state) {
        return scalar2Idx
      }
    }

    return state.index
  }
}

extension Unicode._GraphemeWalker {
  func shouldBreak(
    _ scalar1: Unicode.Scalar,
    between scalar2: Unicode.Scalar,
    _ state: inout State
  ) -> Bool {
    // Go through our fast path first when determining if we should break
    // between these two scalars.
    //
    // This also emcompasses the GB3 rule.
    if let fastCheck = _hasGraphemeBreakBetween(scalar1, scalar2) {
      return fastCheck
    }

    return shouldBreakSlow(scalar1, between: scalar2, &state)
  }

  // The "algorithm" that determines whether or not we should break between
  // certain grapheme break properties.
  //
  // This is based off of the Unicode Annex #29 for [Grapheme Cluster Boundary
  // Rules](https://unicode.org/reports/tr29/#Grapheme_Cluster_Boundary_Rules).
  func shouldBreakSlow(
    _ scalar1: Unicode.Scalar,
    between scalar2: Unicode.Scalar,
    _ state: inout State
  ) -> Bool {
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
      if state.isBackwards {
        checkIfInEmojiSequence(&state)
      }

      if state.isInEmojiSequence {
        return false
      } else {
        return true
      }

    // GB12 & GB13
    case (.regionalIndicator, .regionalIndicator):
      if state.isBackwards {
        countRIs(&state)
      }

      state.shouldBreakRI.toggle()
      return !state.shouldBreakRI

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
  func checkIfInEmojiSequence(_ state: inout State) {
    var emojiIdx = state.index

    guard emojiIdx != scalars.startIndex else {
      state.isInEmojiSequence = false
      return
    }

    repeat {
      scalars.formIndex(before: &emojiIdx)

      let scalar = scalars[emojiIdx]
      let gbp = Unicode._GraphemeBreakProperty(from: scalar)

      switch gbp {
      case .extend:
        continue
      case .extendedPictographic:
        state.isInEmojiSequence = true
        return
      default:
        state.isInEmojiSequence = false
        return
      }
    } while emojiIdx != scalars.startIndex
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
  func countRIs(_ state: inout State) {
    var riIdx = state.index

    guard riIdx != scalars.startIndex else {
      state.shouldBreakRI = false
      return
    }

    var riCount = 0

    repeat {
      scalars.formIndex(before: &riIdx)

      let scalar = scalars[riIdx]
      let gbp = Unicode._GraphemeBreakProperty(from: scalar)

      guard gbp == .regionalIndicator else {
        break
      }

      riCount += 1
    } while riIdx != scalars.startIndex

    state.shouldBreakRI = riCount & 1 != 0
  }
}

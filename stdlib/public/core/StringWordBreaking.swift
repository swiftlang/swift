//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

extension _StringGuts {
  internal func roundDownToNearestWord(
    _ i: String.Index
  ) -> String.Index {
    _internalInvariant(i._encodedOffset <= count)

    let offset = i._encodedOffset

    if offset == 0 || offset == count {
      return i
    }

    let start = previousWordIndex(endingAt: offset)
    let end = nextWordIndex(startingAt: start)
    _internalInvariant(offset <= end, "Word breaking inconsistency")

    if offset == end {
      return i
    }

    return String.Index(_encodedOffset: start)
  }

  @inline(never)
  @_effects(releasenone)
  internal func nextWordIndex(startingAt i: Int) -> Int {
    if _slowPath(isForeign) {
      return _foreignNextWordIndex(startingAt: i)
    }

    return unsafe withFastUTF8 { utf8 in
      nextWordBoundary(startingAt: i) {
        _internalInvariant($0 >= 0)

        guard $0 < utf8.count else {
          return nil
        }

        let (scalar, len) = unsafe _decodeScalar(utf8, startingAt: $0)
        return (scalar, $0 &+ len)
      }
    }
  }

  internal func _foreignNextWordIndex(startingAt i: Int) -> Int {
#if _runtime(_ObjC)
    return nextWordBoundary(startingAt: i) {
      _internalInvariant($0 >= 0)

      guard $0 < count else {
        return nil
      }

      let scalars = String.UnicodeScalarView(self)
      let idx = String.Index(_encodedOffset: $0)

      let scalar = scalars[idx]
      let nextIndex = scalars.index(after: idx)

      return (scalar, nextIndex._encodedOffset)
    }
#else
    fatalError("No foreign strings on this platform in this version of Swift.")
#endif
  }

  internal func previousWordIndex(endingAt i: Int) -> Int {
    if _slowPath(isForeign) {
      return _foreignPreviousWordIndex(endingAt: i)
    }

    return unsafe withFastUTF8 { utf8 in
      previousWordBoundary(endingAt: i) {
        _internalInvariant($0 <= count)

        guard $0 > 0 else {
          return nil
        }

        let (scalar, len) = unsafe _decodeScalar(utf8, endingAt: $0)
        return (scalar, $0 &- len)
      }
    }
  }

  @inline(never)
  internal func _foreignPreviousWordIndex(endingAt i: Int) -> Int {
#if _runtime(_ObjC)
    return previousWordBoundary(endingAt: i) {
      _internalInvariant($0 <= count)

      guard $0 > 0 else {
        return nil
      }

      let scalars = String.UnicodeScalarView(self)
      let idx = String.Index(_encodedOffset: $0)

      let previousIndex = scalars.index(before: idx)
      let scalar = scalars[previousIndex]

      return (scalar, previousIndex._encodedOffset)
    }
#else
    fatalError("No foreign strings on this platform in this version of Swift.")
#endif
  }
}

internal enum _WordQuestion {
  case checkingRegionalIndicator(count: Int, previousRIIndex: Int)
  case requireAHLetter
  case requireNumeric
  case requireHebrewLetter
}

extension _WordQuestion: Equatable {}

internal struct _WordBreakingState {
  var constraint: (question: _WordQuestion, index: Int)? = nil

  var index: Int

  var previousIndex: Int? = nil
  var previousProperty: Unicode._WordBreakProperty? = nil

  // When walking forward in a string, we need to not break on emoji flag
  // sequences. Emoji flag sequences are composed of 2 regional indicators, so
  // when we see our first (.regionalIndicator, .regionalIndicator) decision,
  // we need to know to return false in this case. However, if the next scalar
  // is another regional indicator, we reach the same decision rule, but in this
  // case we actually need to break there's a boundary between emoji flag
  // sequences.
  var shouldBreakRI = false
}

extension _StringGuts {
  // Returns the stride of the next word at the previous boundary offset.
  internal func nextWordBoundary(
    startingAt index: Int,
    nextScalar: (Int) -> (scalar: Unicode.Scalar, end: Int)?
  ) -> Int {
    _precondition(index < endIndex._encodedOffset)

    var (scalar, index) = nextScalar(index)!
    var state = _WordBreakingState(index: index)

    while let (scalar2, nextIndex) = nextScalar(state.index) {
      if shouldBreak(between: scalar, and: scalar2, with: &state) {
        break
      }

      scalar = scalar2
      state.index = nextIndex
    }

    // If we have a leftover constraint, return the index
    if let constraint = state.constraint {
      return constraint.index
    }

    return state.index
  }

  // Returns the stride of the previous word at the current boundary offset.
  internal func previousWordBoundary(
    endingAt index: Int,
    previousScalar: (Int) -> (scalar: Unicode.Scalar, start: Int)?
  ) -> Int {
    var (scalar2, index) = previousScalar(index)!
    var state = _WordBreakingState(index: index)

    while let (scalar, previousIndex) = previousScalar(state.index) {
      if shouldBreakBackward(between: scalar, and: scalar2, with: &state) {
        break
      }

      scalar2 = scalar
      state.index = previousIndex
    }

    if let previousIndex = state.previousIndex {
      return previousIndex
    }

    if let constraint = state.constraint {
      if let riIndex = handleRIConstraint(constraint, with: state) {
        return riIndex
      }

      return constraint.index
    }

    return state.index
  }
}

extension _StringGuts {
  // The "algorithm" that determines whether or not we should break between
  // certain word break properties.
  //
  // This is based off of the Unicode Annex #29 for [Word Boundary
  // Rules](https://unicode.org/reports/tr29/#Word_Boundary_Rules).
  internal func shouldBreak(
    between scalar1: Unicode.Scalar,
    and scalar2: Unicode.Scalar,
    with state: inout _WordBreakingState
  ) -> Bool {
    // WB3
    if scalar1.value == 0xD, scalar2.value == 0xA {
      return false
    }

    let x = Unicode._WordBreakProperty(from: scalar1)
    
    // WB3a, handled here since we don't need to look up `y` for this
    if x == .newlineCRLF {
      return true
    }
    
    let y = Unicode._WordBreakProperty(from: scalar2)

    switch (x, y) {

    // Fast path: If we know our scalars have no properties the decision is
    //            trivial and we don't need to crawl to the default statement.
    case (.any, .any):
      return true

    // WB3b
    case (_, .newlineCRLF):
      return true

    // WB3c
    case (.zwj, .extendedPictographic):
      return false

    // WB3d
    case (.wSegSpace, .wSegSpace):
      return false

    // WB4
    case (_, .format),
         (_, .extend),
         (_, .zwj):
      if x != .format && x != .extend && x != .zwj {
        state.previousProperty = x
      }

      return false

    default:
      let newX = state.previousProperty ?? x

      return decidePostFormat(between: newX, and: y, with: &state)
    }
  }

  internal func decidePostFormat(
    between x: Unicode._WordBreakProperty,
    and y: Unicode._WordBreakProperty,
    with state: inout _WordBreakingState
  ) -> Bool {
    state.previousProperty = nil

    switch (x, y) {
    // WB5
    case (.aLetter, .aLetter),
         (.aLetter, .hebrewLetter),
         (.hebrewLetter, .aLetter),
         (.hebrewLetter, .hebrewLetter):
      return false

    // WB6
    case (.aLetter, .midLetter),
         (.hebrewLetter, .midLetter),
         (.aLetter, .midNumLet),
         (.hebrewLetter, .midNumLet),
         (.aLetter, .singleQuote):
      state.constraint = (question: .requireAHLetter, index: state.index)

      return false

    // WB7
    case (.midLetter, .aLetter),
         (.midLetter, .hebrewLetter),
         (.midNumLet, .aLetter),
         (.midNumLet, .hebrewLetter),
         (.singleQuote, .aLetter),
         (.singleQuote, .hebrewLetter):
      if let constraint = state.constraint {
        if constraint.question == .requireAHLetter {
          state.constraint = nil
          return false
        }

        state.index = constraint.index
        return true
      }

      return true

    // WB7a
    case (.hebrewLetter, .singleQuote):
      return false

    // WB7b
    case (.hebrewLetter, .doubleQuote):
      state.constraint = (question: .requireHebrewLetter, index: state.index)

      return false

    // WB7c
    case (.doubleQuote, .hebrewLetter):
      if let constraint = state.constraint {
        if constraint.question == .requireHebrewLetter {
          state.constraint = nil
          return false
        }

        state.index = constraint.index
        return true
      }

      return true

    // WB8
    case (.numeric, .numeric):
      return false

    // WB9
    case (.aLetter, .numeric),
         (.hebrewLetter, .numeric):
      return false

    // WB10
    case (.numeric, .aLetter),
         (.numeric, .hebrewLetter):
      return false

    // WB11
    case (.midNum, .numeric),
         (.midNumLet, .numeric),
         (.singleQuote, .numeric):
      if let constraint = state.constraint {
        if constraint.question == .requireNumeric {
          state.constraint = nil
          return false
        }

        state.index = constraint.index
        return true
      }

      return true

    // WB12
    case (.numeric, .midNum),
         (.numeric, .midNumLet),
         (.numeric, .singleQuote):
      state.constraint = (question: .requireNumeric, index: state.index)

      return false

    // WB13
    case (.katakana, .katakana):
      return false

    // WB13a
    case (.aLetter, .extendNumLet),
         (.hebrewLetter, .extendNumLet),
         (.numeric, .extendNumLet),
         (.katakana, .extendNumLet),
         (.extendNumLet, .extendNumLet):
      return false

    // WB13b
    case (.extendNumLet, .aLetter),
         (.extendNumLet, .hebrewLetter),
         (.extendNumLet, .numeric),
         (.extendNumLet, .katakana):
      return false

    // WB15
    case (.regionalIndicator, .regionalIndicator):
      defer {
        state.shouldBreakRI.toggle()
      }

      return state.shouldBreakRI

    default:
      return true
    }
  }
}

extension _StringGuts {
  // The "algorithm" that determines whether or not we should break between
  // certain word break properties.
  //
  // This is based off of the Unicode Annex #29 for [Word Boundary
  // Rules](https://unicode.org/reports/tr29/#Word_Boundary_Rules).
  internal func shouldBreakBackward(
    between scalar1: Unicode.Scalar,
    and scalar2: Unicode.Scalar,
    with state: inout _WordBreakingState
  ) -> Bool {
    // WB3
    if scalar1.value == 0xD, scalar2.value == 0xA {
      return false
    }

    let x = Unicode._WordBreakProperty(from: scalar1)
    let y = Unicode._WordBreakProperty(from: scalar2)

    switch (x, y) {

    // Fast path: If we know our scalars have no properties the decision is
    //            trivial and we don't need to crawl to the default statement.
    case (.any, .any):
      return true

    // WB3a and WB3b
    case (.newlineCRLF, _),
         (_, .newlineCRLF):
      return true

    // WB3c
    case (.zwj, .extendedPictographic):
      return false

    // WB3d
    case (.wSegSpace, .wSegSpace):
      return false

    // WB4
    case (.format, _),
         (.extend, _),
         (.zwj, _):
      if y != .format && y != .extend && y != .zwj {
        state.previousProperty = y

        // If we already have a constraint in flight, then use that as our base
        // previous index. Otherwise, use where we're at right now.
        if let constraint = state.constraint {
          state.previousIndex = constraint.index
        } else {
          state.previousIndex = state.index
        }
      }

      return false

    // WB4
    case (_, .format),
         (_, .extend),
         (_, .zwj):
      if state.previousProperty != nil {
        fallthrough
      }

      return false

    default:
      var newY = y

      if let previousProperty = state.previousProperty {
        newY = previousProperty
      }

      return decidePostFormatBackward(between: x, and: newY, with: &state)
    }
  }

  internal func decidePostFormatBackward(
    between x: Unicode._WordBreakProperty,
    and y: Unicode._WordBreakProperty,
    with state: inout _WordBreakingState
  ) -> Bool {
    state.previousProperty = nil

    switch (x, y) {
    case (.any, .any):
      return true

    // WB5
    case (.aLetter, .aLetter),
         (.aLetter, .hebrewLetter),
         (.hebrewLetter, .aLetter),
         (.hebrewLetter, .hebrewLetter):
      state.previousIndex = nil
      return false

    // WB6
    case (.aLetter, .midLetter),
         (.hebrewLetter, .midLetter),
         (.aLetter, .midNumLet),
         (.hebrewLetter, .midNumLet),
         (.aLetter, .singleQuote):
      if let constraint = state.constraint {
        if constraint.question == .requireAHLetter {
          state.constraint = nil
          state.previousIndex = nil
          return false
        }

        state.index = constraint.index
        return true
      }

      return true

    // WB7
    case (.midLetter, .aLetter),
         (.midLetter, .hebrewLetter),
         (.midNumLet, .aLetter),
         (.midNumLet, .hebrewLetter),
         (.singleQuote, .aLetter),
         (.singleQuote, .hebrewLetter):
      state.constraint = (question: .requireAHLetter, index: state.index)

      return false

    // WB7a
    case (.hebrewLetter, .singleQuote):
      state.previousIndex = nil
      return false

    // WB7b
    case (.hebrewLetter, .doubleQuote):
      if let constraint = state.constraint {
        if constraint.question == .requireHebrewLetter {
          state.constraint = nil
          state.previousIndex = nil
          return false
        }

        state.index = constraint.index
        return true
      }

      return true

    // WB7c
    case (.doubleQuote, .hebrewLetter):
      state.constraint = (question: .requireHebrewLetter, index: state.index)

      return false

    // WB8
    case (.numeric, .numeric):
      state.previousIndex = nil
      return false

    // WB9
    case (.aLetter, .numeric),
         (.hebrewLetter, .numeric):
      state.previousIndex = nil
      return false

    // WB10
    case (.numeric, .aLetter),
         (.numeric, .hebrewLetter):
      state.previousIndex = nil
      return false

    // WB11
    case (.midNum, .numeric),
         (.midNumLet, .numeric),
         (.singleQuote, .numeric):
      state.constraint = (question: .requireNumeric, index: state.index)

      return false

    // WB12
    case (.numeric, .midNum),
         (.numeric, .midNumLet),
         (.numeric, .singleQuote):
      if let constraint = state.constraint {
        if constraint.question == .requireNumeric {
          state.constraint = nil
          state.previousIndex = nil
          return false
        }

        state.index = constraint.index
        return true
      }

      return true

    // WB13
    case (.katakana, .katakana):
      state.previousIndex = nil
      return false

    // WB13a
    case (.aLetter, .extendNumLet),
         (.hebrewLetter, .extendNumLet),
         (.numeric, .extendNumLet),
         (.katakana, .extendNumLet),
         (.extendNumLet, .extendNumLet):
      state.previousIndex = nil
      return false

    // WB13b
    case (.extendNumLet, .aLetter),
         (.extendNumLet, .hebrewLetter),
         (.extendNumLet, .numeric),
         (.extendNumLet, .katakana):
      state.previousIndex = nil
      return false

    // WB15
    case (.regionalIndicator, .regionalIndicator):
      var riCount = 0
      var previousRIIndex = state.index
      var constraintIndex = state.index

      if let constraint = state.constraint {
        if case let .checkingRegionalIndicator(count, riIndex) =
            constraint.question {
          riCount = count + 1
          previousRIIndex = count == 0 ? state.index : riIndex
          constraintIndex = constraint.index
        }
      } else {
        if let previousIndex = state.previousIndex {
          constraintIndex = previousIndex
        }
      }

      state.constraint = (
        question: .checkingRegionalIndicator(
          count: riCount,
          previousRIIndex: previousRIIndex
        ),
        index: constraintIndex
      )

      state.previousIndex = nil

      return false

    default:
      return true
    }
  }

  internal func handleRIConstraint(
    _ constraint: (question: _WordQuestion, index: Int),
    with state: _WordBreakingState
  ) -> Int? {
    if case let .checkingRegionalIndicator(count, previousRIIndex) =
        constraint.question {
      // If our count is 0, then we were unable to update previousRIIndex.
      // However, that index is now equal to state.index.
      if count == 0 {
        return state.index
      }

      // We were able to update previousRIIndex!
      if count.isMultiple(of: 2) {
        return previousRIIndex
      }
    }

    return nil
  }
}


//===----------------------------------------------------------------------===//
//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension Unicode {
  /// A state machine for recognizing word boundaries in an arbitrary series of
  /// Unicode scalars, based on the specification in [Unicode Annex
  /// #29](https://unicode.org/reports/tr29/#Word_Boundary_Rules).
  ///
  /// The text segmentation algorithm is not stable, and it allows implementers
  /// to tailor it to their needs. Accordingly, reported word boundaries may
  /// vary in arbitrary ways between Unicode implementations and system
  /// configurations, including between versions of the Swift Standard Library.
  ///
  /// To implement the rules as specified, this low-level construct has built-in
  /// support to defer making a decision on whether there is a word boundary
  /// between two Unicode scalars until more scalars are fed to the state
  /// machine; that is to say, it implements limited lookahead. The API surface
  /// only allows one such candidate position to exist at any given time -- this
  /// corresponds to allowing looking ahead up to the next word boundary. (In
  /// the unlikely case the rules evolve to require looking ahead even further,
  /// then this interface will need to be modified or replaced accordingly.)
  ///
  /// To detect word breaks in a sequence of Unicode scalars, feed each of them
  /// to the recognizer by calling its `hasBreak(before:)` method. The method
  /// indicates if there is a word break preceding the given scalar, or at a
  /// previously reported candidate position. When every scalar in the text has
  /// been fed to the recognizer, the `hasCandidateBreakAtEnd()` method should
  /// be called to determine if there is a break at the last reported candidate
  /// position. There is also an (implicit) word break at the end of text
  /// position.
  ///
  /// Note that `_WordRecognizer` does not take or return actual text positions
  /// (such as a string index); it is entirely independent of the underlying
  /// text representation, and it is able to work with any container model. (For
  /// example, it can be used to incrementally recognize word breaks in UTF-16
  /// data streamed from a network connection, or iterate over word boundaries
  /// in piecewise contiguous UTF-8 buffers stored in a rope data structure.) Of
  /// course, it is also possible to use it to detect word breaks in a standard
  /// `String` value, such as done by this example function:
  ///
  ///     func collectWordBreaks(in string: String) -> [String.Index] {
  ///        var result: [String.Index] = []
  ///        var recognizer = Unicode._WordRecognizer()
  ///        var candidate = string.startIndex
  ///        for i in string.unicodeScalars.indices {
  ///           let r = recognizer.hasBreak(before: string.unicodeScalars[i])
  ///           if r.setCandidate { candidate = i }
  ///           if r.breakAtCandidate { result.append(candidate) }
  ///           if r.breakHere { result.append(i) }
  ///        }
  ///        if recognizer.hasCandidateBreakAtEnd() {
  ///           result.append(candidate)
  ///        }
  ///        result.append(string.endIndex)
  ///        return result
  ///     }
  ///
  /// When used this way, the state machine is able to efficiently iterate over
  /// all breaks within the string by visiting each scalar exactly once, without
  /// any backtracking.
  ///
  /// It is also possible to discard the recognizer after each detected
  /// boundary, reinitializing it from scratch for each iteration step:
  ///
  ///     func wordBreak(
  ///        after knownBreak: String.Index, in string: String
  ///     ) -> String.Index {
  ///        var recognizer = Unicode._WordRecognizer(after: string.unicodeScalars[knownBreak])
  ///        var i = string.unicodeScalars.index(after: knownBreak)
  ///        var candidate = i
  ///        while i < string.endIndex {
  ///           let r = recognizer.hasBreak(before: string.unicodeScalars[i])
  ///           if r.setCandidate { candidate = i }
  ///           if r.breakAtCandidate { return candidate }
  ///           if r.breakHere { return i }
  ///           string.uncodeScalars.formIndex(after: &i)
  ///        }
  ///        if recognizer.hasCandidateBreakAtEnd() {
  ///           return candidate
  ///        }
  ///        return i
  ///     }
  ///
  /// However, note that iterating this way is less efficient, because it
  /// discards lookahead information -- some scalars will be processed multiple
  /// times. The rules are carefully constructed so that the algorithm reports
  /// the same word boundaries whether or not recognizer state is preserved.
  @available(StdlibDeploymentTarget 6.3, *)
  public // Core primitive
  struct _WordRecognizer: Sendable {
    // FIXME: We also need proper public API for this

    /// The last scalar that was fed to `hasBreak(before:)`.
    var _prevScalar: Unicode.Scalar
    /// The cached word break property of `_prevScalar`.
    var _prevCategory: _WordBreakProperty
    /// The word break property of the last preceding scalar that wasn't ignored by rule WB4.
    var _baseCategory: _WordBreakProperty
    /// The current state of the recognizer.
    var _state: _State

    /// Initialize a new word recognizer at the _start of text_ (sot)
    /// position.
    ///
    /// The resulting state machine will report a word break before the first
    /// scalar that is fed to it.
    public init() {
      // To avoid having to handle the empty case specially, we use LF as the
      // placeholder before the first scalar. Per WB3a, we always produce a break
      // following a line feed.
      _baseCategory = .newlineCRLF
      _prevScalar = Unicode.Scalar(0x0A as UInt8)
      _prevCategory = .newlineCRLF
      _state = .ordinary
    }

    /// Initialize a new word recognizer with a state after a previously
    /// recognized word boundary.
    ///
    /// This enables clients to iterate over word boundaries without maintaining
    /// a persistent recognizer state. However, iterating this way may result in
    /// an arbitrarily large amount of duplicate work. (This is because the word
    /// segmentation algorithm requires looking ahead by as much as a full
    /// word's worth of Unicode scalars, and when the old recognizer is
    /// discarded, the information thus collected has to be recreated from
    /// scratch.) Whenever possible, it is therefore preferable to iterate over
    /// multiple word boundaries using a single recognizer instance.
    ///
    /// - Parameter scalar: The Unicode scalar immediately following a known
    ///    word boundary position.
    public init(after scalar: Unicode.Scalar) {
      // We assume that the state machine provides stable results even if the
      // start position was a retroactive candidate.
      _prevScalar = scalar
      _prevCategory = Unicode._WordBreakProperty(from: scalar)
      _baseCategory = _prevCategory
      _state = .ordinary
    }
  }
}

@available(StdlibDeploymentTarget 6.3, *)
extension Unicode._WordRecognizer {
  /// The parts of the word recognizer state that are in addition to saved
  /// information about previous scalars.
  ///
  /// This is used to implement stateful lookahead when a break at a particular
  /// candidate position may need to be suppressed or activated based on a
  /// subsequent scalar value. It is also used to keep track of the number of
  /// contiguous regional indicator scalars we have seen so far.
  internal enum _State: Int, Sendable {
    case ordinary
    case afterWB6 // AHLetter × (MidLetter | MidNumLetQ) AHLetter
    case afterWB7b // Hebrew_Letter × Double_Quote Hebrew_Letter
    case afterWB12 // Numeric × (MidNum | MidNumLetQ) Numeric
    case afterMidFlag // [^RI] (RI RI)* RI × RI

    var hasPendingCandidate: Bool {
      switch self {
      case .afterWB6, .afterWB7b, .afterWB12: return true
      default: return false
      }
    }
  }

  /// Reject a break at the current position, triggering a break at the current
  /// candidate (if any).
  internal mutating func _reject(
  ) -> (setCandidate: Bool, breakAtCandidate: Bool, breakHere: Bool) {
    let breakAtCandidate = _state.hasPendingCandidate
    _state = .ordinary
    return (
      setCandidate: false,
      breakAtCandidate: breakAtCandidate,
      breakHere: false)
  }

  /// Skip this position, ignoring the current Unicode scalar.
  internal mutating func _ignore(
  ) -> (setCandidate: Bool, breakAtCandidate: Bool, breakHere: Bool) {
    // Note: not updating _baseCategory
    return (setCandidate: false, breakAtCandidate: false, breakHere: false)
  }

  /// Signal a break at the current position, also triggering a break at the
  /// current candidate (if any).
  internal mutating func _accept(
  ) -> (setCandidate: Bool, breakAtCandidate: Bool, breakHere: Bool) {
    // If we have a pending candidate, put a break at it
    let breakAtCandidate = _state.hasPendingCandidate
    _state = .ordinary
    return (
      setCandidate: false,
      breakAtCandidate: breakAtCandidate,
      breakHere: true)
  }

  /// Set the current position as the active break candidate, and transition into the
  /// specified state.
  internal mutating func _transition(
    into state: _State
  ) -> (setCandidate: Bool, breakAtCandidate: Bool, breakHere: Bool) {
    _internalInvariant(_state == .ordinary)
    _state = state
    return (setCandidate: true, breakAtCandidate: false, breakHere: false)
  }

  /// If the current state matches the given expectation, suppress a break at
  /// this position and discard the active candidate; otherwise, report a break
  /// at both positions.
  internal mutating func _expect(
    _ expectedState: _State
  ) -> (setCandidate: Bool, breakAtCandidate: Bool, breakHere: Bool) {
    let breakHere = (_state != expectedState)
    let breakAtCandidate = breakHere && _state.hasPendingCandidate
    _state = .ordinary
    return (
      setCandidate: false,
      breakAtCandidate: breakAtCandidate,
      breakHere: breakHere)
  }

  /// Feeds the next scalar to the state machine, reporting if there is a word
  /// boundary at the current position or a previously reported candidate.
  ///
  /// To decide whether there is a word break at the current position, the
  /// segmentation algorithm sometimes needs to look ahead by visiting
  /// additional scalars following the break, up to the next word boundary. To
  /// allow this, the state machine can report that the current position is a
  /// provisional break "candidate". Clients are expected to remember the
  /// position of the last reported candidate, so that it can be retroactively
  /// promoted to a full break as needed.
  ///
  /// - Parameter nextScalar: The scalar at the current position in the text.
  /// - Returns: A triple of Boolean values `setCandidate`, `breakAtCandidate`,
  ///     `breakHere`. If `setCandidate` is true, then the caller is expected to
  ///     save the current text position as a potential word boundary. If
  ///     `breakAtCandidate` is true, then there is a word boundary at the last
  ///     candidate position. If `breakHere` is true, then there is a word
  ///     boundary at the current position. The caller is expected to process
  ///     these three components in this specific order.
  public mutating func hasBreak(
    before nextScalar: Unicode.Scalar
  ) -> (setCandidate: Bool, breakAtCandidate: Bool, breakHere: Bool) {
    let nextCategory = Unicode._WordBreakProperty(from: nextScalar)
    var nextBase = nextCategory

    // FIXME: Implement a proper state machine here, dispatching on
    // (state, nextProperty), ideally through a static look-up table

    defer {
      _prevScalar = nextScalar
      _prevCategory = nextCategory
      _baseCategory = nextBase
    }

    switch (_prevCategory, nextCategory) {
    case (.any, .any): // WB999
      // Fast path: If we know our scalars have no properties then the decision
      // is trivial and we don't need to crawl to the default statement.
      return _accept()

    case (.newlineCRLF, _), // WB3a
         (_, .newlineCRLF): // WB3b
      if _prevScalar.value == 0xD, nextScalar.value == 0xA { // WB3
        _internalInvariant(_prevCategory == .newlineCRLF)
        return _reject()
      }
      return _accept()

    case (.zwj, .extendedPictographic), // WB3c
         (.wSegSpace, .wSegSpace): // WB3d
      return _reject()

    case (_, .format), // WB4
         (_, .extend),
         (_, .zwj):
      nextBase = _baseCategory // Cancel _baseCategory update
      return _ignore()

    default:
      break
    }

    switch (_baseCategory, nextCategory) {
    case (.aLetter, .aLetter), // WB5
         (.aLetter, .hebrewLetter),
         (.hebrewLetter, .aLetter),
         (.hebrewLetter, .hebrewLetter):
      return _reject()

    case (.aLetter, .midLetter), // WB6
         (.hebrewLetter, .midLetter),
         (.aLetter, .midNumLet),
         (.hebrewLetter, .midNumLet),
         (.aLetter, .singleQuote):
      return _transition(into: .afterWB6)

    case (.midLetter, .aLetter), // WB7
         (.midLetter, .hebrewLetter),
         (.midNumLet, .aLetter),
         (.midNumLet, .hebrewLetter),
         (.singleQuote, .aLetter),
         (.singleQuote, .hebrewLetter):
      return _expect(.afterWB6)

    case (.hebrewLetter, .singleQuote): // WB7a
      return _reject()

    case (.hebrewLetter, .doubleQuote): // WB7b
      return _transition(into: .afterWB7b)

    case (.doubleQuote, .hebrewLetter): // WB7c
      return _expect(.afterWB7b)

    case (.numeric, .numeric), // WB8
         (.aLetter, .numeric), // WB9
         (.hebrewLetter, .numeric), // WB9
         (.numeric, .aLetter), // WB10
         (.numeric, .hebrewLetter): // WB10
      return _reject()

    case (.midNum, .numeric), // WB11
         (.midNumLet, .numeric),
         (.singleQuote, .numeric):
      return _expect(.afterWB12)

    case (.numeric, .midNum), // WB12
         (.numeric, .midNumLet),
         (.numeric, .singleQuote):
      return _transition(into: .afterWB12)

    case (.katakana, .katakana), // WB13
         (.aLetter, .extendNumLet), // WB13a
         (.hebrewLetter, .extendNumLet),
         (.numeric, .extendNumLet),
         (.katakana, .extendNumLet),
         (.extendNumLet, .extendNumLet),
         (.extendNumLet, .aLetter), // WB13b
         (.extendNumLet, .hebrewLetter),
         (.extendNumLet, .numeric),
         (.extendNumLet, .katakana):
      return _reject()

    case (.regionalIndicator, .regionalIndicator): // WB15/WB16
      let breakHere: Bool
      if _state == .afterMidFlag {
        _state = .ordinary
        breakHere = true
      } else {
        _state = .afterMidFlag
        breakHere = false
      }
      return (setCandidate: false, breakAtCandidate: false, breakHere: breakHere)

    default: // WB999
      return _accept()
    }
  }

  /// Returns true if the previously reported word boundary candidate needs to
  /// be promoted to a full break if there are no more scalars in the input text.
  ///
  /// There is always an (implicit) word boundary at position at the end of
  /// text, following the last scalar; however, the end may also trigger a
  /// pending unreported break at the last candidate previously set by
  /// `hasBreak`. This method returns true in that case, allowing clients to
  /// reliably detect such boundaries.
  public func hasCandidateBreakAtEnd() -> Bool {
    _state.hasPendingCandidate
  }
}

extension Unicode {
  /// A state machine for recognizing safe word boundaries in a backward
  /// sequence of Unicode scalars, based on the specification in [Unicode Annex
  /// #29](https://unicode.org/reports/tr29/#Word_Boundary_Rules).
  ///
  /// The text segmentation algorithm is not stable, and it allows implementers
  /// to tailor it to their needs. Accordingly, reported word boundaries may
  /// vary in arbitrary ways between Unicode implementations and system
  /// configurations, including between versions of the Swift Standard Library.
  ///
  /// This is intended to help implement searching for word boundaries near an
  /// arbitrary position in a middle of a larger text, as described in [section
  /// 6.4 of Annex #29](https://unicode.org/reports/tr29/#Random_Access). The
  /// start position may be at at an arbitrary scalar anywhere in the input text
  /// -- there is no expectation that the first scalar addresses a known word
  /// boundary. The state machine scans backwards from that position until it
  /// detects a reliable word boundary.
  ///
  /// To detect a word break near a particular position in a series of Unicode
  /// scalars, start iterating scalars backward from the start position, feeding
  /// each of them to the `hasGuaranteedBreak(after:)` method. The method
  /// indicates if there is a word break preceding the given scalar, or at a
  /// previously reported candidate position. There is always a word break at
  /// the start of the text; so if we run out of scalars, the start position is
  /// going to be a suitable safe word boundary.
  ///
  /// Note that this construct may skip over an arbitrary number of word
  /// boundaries while it is searching for a safe break position. Once a safe
  /// boundary is found, callers are usually expected to use it as the start
  /// position to iterate forward using the standard segmentation algorithm
  /// (implemented by `Unicode._WordRecognizer`), for example until they find
  /// the break nearest to their original start position. In such cases, it is
  /// usually a good idea to incrementally memoize word boundaries as they are
  /// detected, to avoid repeating this process on the same positions later.
  @available(StdlibDeploymentTarget 6.3, *)
  public // Core primitive
  struct _RandomAccessWordRecognizer: Sendable {
    // FIXME: We also need proper public API for this

    /// The last scalar that was fed to `hasGuaranteedBreak`; i.e., the scalar
    /// immediately following the current one in the text.
    var _nextScalar: Unicode.Scalar
    /// The cached word break property of `_nextScalar`.
    var _nextCategory: _WordBreakProperty
    /// The word break property of the most recently seen scalar that wasn't
    /// ignored by rule WB4.
    var _baseCategory: _WordBreakProperty
    /// Additional recognizer state.
    var _state: _State
    var _hasPendingCandidate: Bool

    /// Initialize a new word recognizer at an arbitrary text position preceding
    /// the given scalar.
    public init(before scalar: Unicode.Scalar) {
      _nextScalar = scalar
      _nextCategory = Unicode._WordBreakProperty(from: scalar)
      _baseCategory = _nextCategory
      _state = .initial
      _hasPendingCandidate = false
    }
  }
}


@available(StdlibDeploymentTarget 6.3, *)
extension Unicode._RandomAccessWordRecognizer {
  internal enum _State: Int, Sendable {
    case initial
    case ordinary
    case beforeWB7
    case beforeWB7c
    case beforeWB11

    var hasPendingCandidate: Bool {
      switch self {
      case .beforeWB7, .beforeWB7c, .beforeWB11: return true
      default: return false
      }
    }
  }

  internal mutating func _reject(
  ) -> (setCandidate: Bool, breakAtCandidate: Bool, breakHere: Bool) {
    _hasPendingCandidate = false
    return (setCandidate: false, breakAtCandidate: false, breakHere: false)
  }

  internal mutating func _ignore(
  ) -> (setCandidate: Bool, breakAtCandidate: Bool, breakHere: Bool) {
    return (setCandidate: false, breakAtCandidate: false, breakHere: false)
  }

  internal mutating func _accept(
  ) -> (setCandidate: Bool, breakAtCandidate: Bool, breakHere: Bool) {
    if _hasPendingCandidate {
      _hasPendingCandidate = false
      return (setCandidate: false, breakAtCandidate: true, breakHere: false)
    }
    return (setCandidate: false, breakAtCandidate: false, breakHere: true)
  }

  internal mutating func _placeCandidate(
  ) -> (setCandidate: Bool, breakAtCandidate: Bool, breakHere: Bool) {
    if _state == .initial {
      return _reject()
    }
    _hasPendingCandidate = true
    return (setCandidate: true, breakAtCandidate: false, breakHere: false)
  }

  internal mutating func _placeSoftCandidate(
  ) -> (setCandidate: Bool, breakAtCandidate: Bool, breakHere: Bool) {
    if _hasPendingCandidate || _state == .initial {
      return (false, false, false)
    }
    return _placeCandidate()
  }

  public mutating func hasGuaranteedBreak(
    after previousScalar: Unicode.Scalar
  ) -> (setCandidate: Bool, breakAtCandidate: Bool, breakHere: Bool) {
    let prevCategory = Unicode._WordBreakProperty(from: previousScalar)
    var newState: _State = .ordinary
    var newBase = prevCategory
    defer {
      _nextCategory = prevCategory
      _nextScalar = previousScalar
      _baseCategory = newBase
      _state = newState
    }

    switch (prevCategory, _nextCategory) {
    case (.any, .any): // WB999 shortcut
      return _accept()

    case (.newlineCRLF, _), // WB3a
         (_, .newlineCRLF): // WB3b
      if previousScalar.value == 0xD, _nextScalar.value == 0xA { // WB3
        return _reject()
      }
      return _accept()

    case (.zwj, .extendedPictographic), // WB3c
         (.wSegSpace, .wSegSpace): // WB3d
      newBase = _baseCategory
      newState = _state
      return _reject()

    case (.format, _), // WB4
         (.extend, _),
         (.zwj, _):
      newBase = _baseCategory
      newState = _state
      if _state == .initial || _nextCategory == .format || _nextCategory == .extend || _nextCategory == .zwj {
        return _ignore()
      }
      return _placeSoftCandidate()

    default:
      break
    }

    switch (prevCategory, _baseCategory) {
    case (.aLetter, .aLetter), // WB5
         (.aLetter, .hebrewLetter),
         (.hebrewLetter, .aLetter),
         (.hebrewLetter, .hebrewLetter):
      return _reject()

    case (.midLetter, .aLetter), // WB7
         (.midLetter, .hebrewLetter),
         (.midNumLet, .aLetter),
         (.midNumLet, .hebrewLetter),
         (.singleQuote, .aLetter),
         (.singleQuote, .hebrewLetter):
      newState = .beforeWB7
      return _placeSoftCandidate()

    case (.aLetter, .midLetter), // WB6
         (.hebrewLetter, .midLetter),
         (.aLetter, .midNumLet),
         (.hebrewLetter, .midNumLet),
         (.aLetter, .singleQuote):
      if _state == .beforeWB7 || _state == .initial {
        return _reject()
      }
      return _accept()

    case (.hebrewLetter, .singleQuote): // WB7a
      return _reject()

    case (.doubleQuote, .hebrewLetter): // WB7c
      newState = .beforeWB7c
      return _placeSoftCandidate()

    case (.hebrewLetter, .doubleQuote): // WB7b
      if _state == .beforeWB7c || _state == .initial {
        return _reject()
      }
      return _accept()

    case (.numeric, .numeric), // WB8
         (.aLetter, .numeric), // WB9
         (.hebrewLetter, .numeric),
         (.numeric, .aLetter), // WB10
         (.numeric, .hebrewLetter):
      return _reject()

    case (.midNum, .numeric), // WB11
         (.midNumLet, .numeric),
         (.singleQuote, .numeric):
      newState = .beforeWB11
      return _placeSoftCandidate()

    case (.numeric, .midNum), // WB12
         (.numeric, .midNumLet),
         (.numeric, .singleQuote):
      if _state == .beforeWB11 || _state == .initial {
        return _reject()
      }
      return _accept()

    case (.katakana, .katakana), // WB13
         (.aLetter, .extendNumLet), // WB13a
         (.hebrewLetter, .extendNumLet),
         (.numeric, .extendNumLet),
         (.katakana, .extendNumLet),
         (.extendNumLet, .extendNumLet),
         (.extendNumLet, .aLetter), // WB13b
         (.extendNumLet, .hebrewLetter),
         (.extendNumLet, .numeric),
         (.extendNumLet, .katakana):
      return _reject()

    case (.regionalIndicator, .regionalIndicator): // WB15/WB16
      return _reject()

    case (_, .format),
         (_, .extend),
         (_, .zwj):
      _internalInvariant(!_hasPendingCandidate)
      newState = .initial
      return _reject()

    default:
      if
        !_hasPendingCandidate,
        _nextCategory == .format || _nextCategory == .extend || _nextCategory == .zwj
      {
        return _ignore()
      }
      return _accept()
    }
  }
}

extension String {
  /// Find and return a word boundary position at or before an arbitrary index
  /// within this string. The result may not be the closest word break to the
  /// start position.
  ///
  /// This implements the core algorithm for finding a "safe" starting point for
  /// random access to word breaks, following [section 6.4 of Unicode Annex
  /// #29](https://unicode.org/reports/tr29/#Random_Access). Unicode defines
  /// word boundaries using a forward-only state machine; this algorithm does
  /// its best to run the state machine backwards until it finds a guaranteed
  /// break position.
  ///
  /// This process is inherently an approximation: the algorithm may need to
  /// skip over an arbitrary number of actual word boundaries before finding one
  /// that it can judge with confidence. This makes it relatively expensive to
  /// iterate over word boundaries backwards; in the worst case, a naive
  /// implementation may have quadratic complexity. The recommended way to
  /// mitigate this is to maintain a cache of word breaks already traversed,
  /// only calling this method to extend the range of known breaks backwards as
  /// needed.
  ///
  /// - Parameter i: An arbitrary index within the string, not necessarily
  ///     addressing a word boundary.
  /// - Returns: A valid index less than equal to the input that is guaranteed to
  ///     identify some word boundary at or before `i`.
  @available(SwiftStdlib 6.3, *)
  public func _wordIndex(somewhereAtOrBefore i: Index) -> Index {
    var j = _guts.validateInclusiveScalarIndex(i)
    if j == endIndex {
      return j
    }
    var recognizer = Unicode._RandomAccessWordRecognizer(
      before: self.unicodeScalars[j])
    var candidate = j
    while j > self.startIndex {
      let p = self.unicodeScalars.index(before: j)
      let b = recognizer.hasGuaranteedBreak(after: self.unicodeScalars[p])
      if b.setCandidate { candidate = j }
      if b.breakAtCandidate { return candidate }
      if b.breakHere { return j }
      j = p
    }
    return j
  }

  /// Return the word boundary position following a known word boundary within
  /// this string.
  ///
  /// This implements the word boundary specification of [Unicode Annex
  /// #29](https://unicode.org/reports/tr29/#Default_Word_Boundaries). The
  /// algorithm is not stable, and it allows implementers to tailor it to their
  /// needs; accordingly, the result of this operation may vary between Unicode
  /// implementations and system configurations, including versions of the Swift
  /// Standard Library.
  ///
  /// Note: The input index must be on a known word boundary, otherwise the
  /// result of this operation is unspecified. The start and end indices are
  /// always known word boundaries, in every string.
  ///
  /// - Parameter i: A valid index addressing a word boundary within this
  ///     string.
  /// - Returns: The first word break strictly following `i` in the string.
  @available(StdlibDeploymentTarget 5.7, *)
  public func _wordIndex(after i: String.Index) -> String.Index {
    guard #available(StdlibDeploymentTarget 6.3, *) else {
      fatalError("Unreachable")
    }
    let i = _guts.validateScalarIndex(i)
    if _slowPath(_guts.isForeign) {
      return _guts._nextForeignWordIndex(after: i)
    }
    return _guts._nextUTF8WordIndex(after: i)
  }
}

extension _StringGuts {
  @available(StdlibDeploymentTarget 6.3, *)
  @inline(never)
  @_effects(releasenone)
  internal func _nextUTF8WordIndex(after index: Index) -> Index {
    _internalInvariant(self.isFastUTF8)
    let result = unsafe self.withFastUTF8 { utf8 in
      var offset = index._encodedOffset
      let first = unsafe _decodeScalar(utf8, startingAt: offset)
      offset &+= first.scalarLength
      var recognizer = Unicode._WordRecognizer(after: first.0)
      var candidate = offset
      while offset < utf8.count {
        let (scalar, len) = unsafe _decodeScalar(utf8, startingAt: offset)
        let r = recognizer.hasBreak(before: scalar)
        if r.setCandidate { candidate = offset }
        if r.breakAtCandidate { return candidate }
        if r.breakHere { return offset }
        offset &+= len
      }
      if recognizer.hasCandidateBreakAtEnd() {
        return candidate
      }
      return offset
    }
    // Note: We only signal that the result is scalar aligned, not
    // character-aligned. Unicode does attempt to ensure that word breaks are
    // always character-aligned, but this is not a strict guarantee, especially
    // not if either segmentation algorithm has a tailored implementation. (As
    // of 6.3, we do not tailor our implementations, but we used to do so and we
    // may choose to do it again in the future.)
    return Index(_encodedOffset: result)._scalarAligned._knownUTF8
  }

  @available(StdlibDeploymentTarget 6.3, *)
  @inline(never)
  internal func _nextForeignWordIndex(after index: Index) -> Index {
    #if _runtime(_ObjC)
    _internalInvariant(self.isForeign)
    let scalars = String.UnicodeScalarView(self)
    var recognizer = Unicode._WordRecognizer(after: scalars[index])
    var i = scalars.index(after: index)
    var candidate = i
    while i < scalars.endIndex {
      let r = recognizer.hasBreak(before: scalars[i])
      if r.setCandidate { candidate = i }
      if r.breakAtCandidate { return candidate }
      if r.breakHere { return i }
      scalars.formIndex(after: &i)
    }
    if recognizer.hasCandidateBreakAtEnd() {
      return candidate
    }
    return i
    #else
    fatalError("Foreign strings are unsupported on this platform")
    #endif
  }
}

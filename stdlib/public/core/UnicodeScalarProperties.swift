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
// Exposes advanced properties of Unicode.Scalar defined by the Unicode
// Standard.
//===----------------------------------------------------------------------===//

import SwiftShims

extension Unicode.Scalar {

  /// A value that provides access to properties of a Unicode scalar that are
  /// defined by the Unicode standard.
  public struct Properties {
    internal init(_value: UInt32) {
      // We convert the value to the underlying UChar32 type here and store it
      // in that form to make calling the ICU APIs cleaner below.
      self._value = __swift_stdlib_UChar32(bitPattern: _value)
    }

    internal var _value: __swift_stdlib_UChar32
  }

  /// A value that provides access to properties of the Unicode scalar that are
  /// defined by the Unicode standard.
  public var properties: Properties {
    return Properties(_value: _value)
  }
}

/// Boolean properties that are defined by the Unicode Standard (i.e., not
/// ICU-specific).
extension Unicode.Scalar.Properties {

  @_transparent
  internal func _hasBinaryProperty(
    _ property: __swift_stdlib_UProperty
  ) -> Bool {
    return __swift_stdlib_u_hasBinaryProperty(_value, property) != 0
  }

  /// A Boolean property indicating whether the scalar is alphabetic.
  ///
  /// Alphabetic scalars are the primary units of alphabets and/or syllabaries.
  ///
  /// This property corresponds to the `Alphabetic` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isAlphabetic: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_ALPHABETIC)
  }

  /// A Boolean property indicating whether the scalar is an ASCII character
  /// commonly used for the representation of hexadecimal numbers.
  ///
  /// The only scalars for which this property is true are:
  ///
  /// * U+0030...U+0039: DIGIT ZERO...DIGIT NINE
  /// * U+0041...U+0046: LATIN CAPITAL LETTER A...LATIN CAPITAL LETTER F
  /// * U+0061...U+0066: LATIN SMALL LETTER A...LATIN SMALL LETTER F
  ///
  /// This property corresponds to the `ASCII_Hex_Digit` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isASCIIHexDigit: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_ASCII_HEX_DIGIT)
  }

  /// A Boolean property indicating whether the scalar is a format control
  /// character that has a specific function in the Unicode Bidrectional
  /// Algorithm.
  ///
  /// This property corresponds to the `Bidi_Control` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isBidiControl: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_BIDI_CONTROL)
  }

  /// A Boolean property indicating whether the scalar is mirrored in
  /// bidirectional text.
  ///
  /// This property corresponds to the `Bidi_Mirrored` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isBidiMirrored: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_BIDI_MIRRORED)
  }

  /// A Boolean property indicating whether the scalar is a punctuation
  /// symbol explicitly called out as a dash in the Unicode Standard or a
  /// compatibility equivalent.
  ///
  /// This property corresponds to the `Dash` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isDash: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_DASH)
  }

  /// A Boolean property indicating whether the scalar is a default-ignorable
  /// code point.
  ///
  /// Default-ignorable code points are those that should be ignored by default
  /// in rendering (unless explicitly supported). They have no visible glyph or
  /// advance width in and of themselves, although they may affect the display,
  /// positioning, or adornment of adjacent or surrounding characters.
  ///
  /// This property corresponds to the `Default_Ignorable_Code_Point` property
  /// in the [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isDefaultIgnorableCodePoint: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_DEFAULT_IGNORABLE_CODE_POINT)
  }

  /// A Boolean property indicating whether the scalar is deprecated.
  ///
  /// Scalars are never removed from the Unicode Standard, but the usage of
  /// deprecated scalars is strongly discouraged.
  ///
  /// This property corresponds to the `Deprecated` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isDeprecated: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_DEPRECATED)
  }

  /// A Boolean property indicating whether the scalar is a diacritic.
  ///
  /// Diacritics are scalars that linguistically modify the meaning of another
  /// scalar to which they apply. Scalars for which this property is true are
  /// frequently, but not always, combining marks or modifiers.
  ///
  /// This property corresponds to the `Diacritic` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isDiacritic: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_DIACRITIC)
  }

  /// A Boolean property indicating whether the scalar's principal function is
  /// to extend the value or shape of a preceding alphabetic scalar.
  ///
  /// Typical extenders are length and iteration marks.
  ///
  /// This property corresponds to the `Extender` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isExtender: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_EXTENDER)
  }

  /// A Boolean property indicating whether the scalar is excluded from
  /// composition when performing Unicode normalization.
  ///
  /// This property corresponds to the `Full_Composition_Exclusion` property in
  /// the [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isFullCompositionExclusion: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_FULL_COMPOSITION_EXCLUSION)
  }

  /// A Boolean property indicating whether the scalar is a grapheme base.
  ///
  /// A grapheme base can be thought of as a space-occupying glyph above or
  /// below which other non-spacing modifying glyphs can be applied. For
  /// example, when the character `é` is represented in NFD form, the grapheme
  /// base is "e" (U+0065 LATIN SMALL LETTER E) and it is followed by a single
  /// grapheme extender, U+0301 COMBINING ACUTE ACCENT.
  ///
  /// The set of scalars for which `isGraphemeBase` is true is disjoint by
  /// definition from the set for which `isGraphemeExtend` is true.
  ///
  /// This property corresponds to the `Grapheme_Base` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isGraphemeBase: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_GRAPHEME_BASE)
  }

  /// A Boolean property indicating whether the scalar is a grapheme extender.
  ///
  /// A grapheme extender can be thought of primarily as a non-spacing glyph
  /// that is applied above or below another glyph.
  ///
  /// The set of scalars for which `isGraphemeExtend` is true is disjoint by
  /// definition from the set for which `isGraphemeBase` is true.
  ///
  /// This property corresponds to the `Grapheme_Extend` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isGraphemeExtend: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_GRAPHEME_EXTEND)
  }

  /// A Boolean property indicating whether the scalar is one that is commonly
  /// used for the representation of hexadecimal numbers or a compatibility
  /// equivalent.
  ///
  /// This property is true for all scalars for which `isASCIIHexDigit` is true
  /// as well as for their CJK halfwidth and fullwidth variants.
  ///
  /// This property corresponds to the `Hex_Digit` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isHexDigit: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_HEX_DIGIT)
  }

  /// A Boolean property indicating whether the scalar is one which is
  /// recommended to be allowed to appear in a non-starting position in a
  /// programming language identifier.
  ///
  /// Applications that store identifiers in NFKC normalized form should instead
  /// use `isXIDContinue` to check whether a scalar is a valid identifier
  /// character.
  ///
  /// This property corresponds to the `ID_Continue` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isIDContinue: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_ID_CONTINUE)
  }

  /// A Boolean property indicating whether the scalar is one which is
  /// recommended to be allowed to appear in a starting position in a
  /// programming language identifier.
  ///
  /// Applications that store identifiers in NFKC normalized form should instead
  /// use `isXIDStart` to check whether a scalar is a valid identifier
  /// character.
  ///
  /// This property corresponds to the `ID_Start` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isIDStart: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_ID_START)
  }

  /// A Boolean property indicating whether the scalar is considered to be a
  /// CJKV (Chinese, Japanese, Korean, and Vietnamese) or other siniform
  /// (Chinese writing-related) ideograph.
  ///
  /// This property roughly defines the class of "Chinese characters" and does
  /// not include characters of other logographic scripts such as Cuneiform or
  /// Egyptian Hieroglyphs
  ///
  /// This property corresponds to the `Ideographic` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isIdeographic: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_IDEOGRAPHIC)
  }

  /// A Boolean property indicating whether the scalar is an ideographic
  /// description character that determines how the two ideographic characters
  /// or ideographic description sequences that follow it are to be combined to
  /// form a single character.
  ///
  /// Ideographic description characters are technically printable characters,
  /// but advanced rendering engines may use them to approximate ideographs that
  /// are otherwise unrepresentable.
  ///
  /// This property corresponds to the `IDS_Binary_Operator` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isIDSBinaryOperator: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_IDS_BINARY_OPERATOR)
  }

  /// A Boolean property indicating whether the scalar is an ideographic
  /// description character that determines how the three ideographic characters
  /// or ideographic description sequences that follow it are to be combined to
  /// form a single character.
  ///
  /// Ideographic description characters are technically printable characters,
  /// but advanced rendering engines may use them to approximate ideographs that
  /// are otherwise unrepresentable.
  ///
  /// This property corresponds to the `IDS_Trinary_Operator` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isIDSTrinaryOperator: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_IDS_TRINARY_OPERATOR)
  }

  /// A Boolean property indicating whether the scalar is a format control
  /// character that has a specific function in controlling cursive joining and
  /// ligation.
  ///
  /// There are two scalars for which this property is true:
  ///
  /// * When U+200C ZERO WIDTH NON-JOINER is inserted between two characters, it
  ///   directs the rendering engine to render them separately/disconnected when
  ///   it might otherwise render them as a ligature. For example, a rendering
  ///   engine might display "fl" in English as a connected glyph; inserting the
  ///   zero width non-joiner would force them to be rendered as disconnected
  ///   glyphs.
  ///
  /// * When U+200D ZERO WIDTH JOINER is inserted between two characters, it
  ///   directs the rendering engine to render them as a connected glyph when it
  ///   would otherwise render them independently. The zero width joiner is also
  ///   used to construct complex emoji from sequences of base emoji characters.
  ///   For example, "family" emoji are created by joining sequences of man,
  ///   woman, and child emoji with the zero width joiner.
  ///
  /// This property corresponds to the `Join_Control` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isJoinControl: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_JOIN_CONTROL)
  }

  /// A Boolean property indicating whether the scalar requires special handling
  /// for operations involving ordering, such as sorting and searching.
  ///
  /// This property applies to a small number of spacing vowel letters occurring
  /// in some Southeast Asian scripts like Thai and Lao, which use a visual
  /// order display model. Such letters are stored in text ahead of
  /// syllable-initial consonants.
  ///
  /// This property corresponds to the `Logical_Order_Exception` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isLogicalOrderException: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_LOGICAL_ORDER_EXCEPTION)
  }

  /// A Boolean property indicating whether the scalar's letterform is
  /// considered lowercase.
  ///
  /// This property corresponds to the `Lowercase` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isLowercase: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_LOWERCASE)
  }

  /// A Boolean property indicating whether the scalar is one that naturally
  /// appears in mathematical contexts.
  ///
  /// The set of scalars for which this property is true includes mathematical
  /// operators and symbols as well as specific Greek and Hebrew letter
  /// variants that are categorized as symbols. Notably, it does _not_ contain
  /// the standard digits or Latin/Greek letter blocks; instead, it contains the
  /// mathematical Latin, Greek, and Arabic letters and numbers defined in the
  /// Supplemental Multilingual Plane.
  ///
  /// This property corresponds to the `Math` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isMath: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_MATH)
  }

  /// A Boolean property indicating whether the scalar is permanently reserved
  /// for internal use.
  ///
  /// This property corresponds to the `Noncharacter_Code_Point` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isNoncharacterCodePoint: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_NONCHARACTER_CODE_POINT)
  }

  /// A Boolean property indicating whether the scalar is one that is used in
  /// writing to surround quoted text.
  ///
  /// This property corresponds to the `Quotation_Mark` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isQuotationMark: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_QUOTATION_MARK)
  }

  /// A Boolean property indicating whether the scalar is a radical component of
  /// CJK characters, Tangut characters, or Yi syllables.
  ///
  /// These scalars are often the components of ideographic description
  /// sequences, as defined by the `isIDSBinaryOperator` and
  /// `isIDSTrinaryOperator` properties.
  ///
  /// This property corresponds to the `Radical` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isRadical: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_RADICAL)
  }

  /// A Boolean property indicating whether the scalar has a "soft dot" that
  /// disappears when a diacritic is placed over the scalar.
  ///
  /// For example, "i" is soft dotted because the dot disappears when adding an
  /// accent mark, as in "í".
  ///
  /// This property corresponds to the `Soft_Dotted` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isSoftDotted: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_SOFT_DOTTED)
  }

  /// A Boolean property indicating whether the scalar is a punctuation symbol
  /// that typically marks the end of a textual unit.
  ///
  /// This property corresponds to the `Terminal_Punctuation` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isTerminalPunctuation: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_TERMINAL_PUNCTUATION)
  }

  /// A Boolean property indicating whether the scalar is one of the unified
  /// CJK ideographs in the Unicode Standard.
  ///
  /// This property is false for CJK punctuation and symbols, as well as for
  /// compatibility ideographs (which canonically decompose to unified
  /// ideographs).
  ///
  /// This property corresponds to the `Unified_Ideograph` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isUnifiedIdeograph: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_UNIFIED_IDEOGRAPH)
  }

  /// A Boolean property indicating whether the scalar's letterform is
  /// considered uppercase.
  ///
  /// This property corresponds to the `Uppercase` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isUppercase: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_UPPERCASE)
  }

  /// A Boolean property indicating whether the scalar is a whitespace
  /// character.
  ///
  /// This property is true for scalars that are spaces, separator characters,
  /// and other control characters that should be treated as whitespace for the
  /// purposes of parsing text elements.
  ///
  /// This property corresponds to the `White_Space` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isWhitespace: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_WHITE_SPACE)
  }

  /// A Boolean property indicating whether the scalar is one which is
  /// recommended to be allowed to appear in a non-starting position in a
  /// programming language identifier, with adjustments made for NFKC normalized
  /// form.
  ///
  /// The set of scalars `[:XID_Continue:]` closes the set `[:ID_Continue:]`
  /// under NFKC normalization by removing any scalars whose normalized form is
  /// not of the form `[:ID_Continue:]*`.
  ///
  /// This property corresponds to the `XID_Continue` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isXIDContinue: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_XID_CONTINUE)
  }

  /// A Boolean property indicating whether the scalar is one which is
  /// recommended to be allowed to appear in a starting position in a
  /// programming language identifier, with adjustments made for NFKC normalized
  /// form.
  ///
  /// The set of scalars `[:XID_Start:]` closes the set `[:ID_Start:]` under
  /// NFKC normalization by removing any scalars whose normalized form is not of
  /// the form `[:ID_Start:] [:ID_Continue:]*`.
  ///
  /// This property corresponds to the `XID_Start` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isXIDStart: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_XID_START)
  }

  /// A Boolean property indicating whether the scalar is a punctuation mark
  /// that generally marks the end of a sentence.
  ///
  /// This property corresponds to the `Sentence_Terminal` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isSentenceTerminal: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_S_TERM)
  }

  /// A Boolean property indicating whether the scalar is a variation selector.
  ///
  /// Variation selectors allow rendering engines that support them to choose
  /// different glyphs to display for a particular code point.
  ///
  /// This property corresponds to the `Variation_Selector` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isVariationSelector: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_VARIATION_SELECTOR)
  }

  /// A Boolean property indicating whether the scalar is recommended to have
  /// syntactic usage in patterns represented in source code.
  ///
  /// This property corresponds to the `Pattern_Syntax` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isPatternSyntax: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_PATTERN_SYNTAX)
  }

  /// A Boolean property indicating whether the scalar is recommended to be
  /// treated as whitespace when parsing patterns represented in source code.
  ///
  /// This property corresponds to the `Pattern_White_Space` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isPatternWhitespace: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_PATTERN_WHITE_SPACE)
  }

  /// A Boolean property indicating whether the scalar is considered to be
  /// either lowercase, uppercase, or titlecase.
  ///
  /// Though similar in name, this property is _not_ equivalent to
  /// `changesWhenCaseMapped`. The set of scalars for which `isCased` is true is
  /// a superset of those for which `changesWhenCaseMapped` is true. An example
  /// of scalars that only have `isCased` as true are the Latin small capitals
  /// that are used by the International Phonetic Alphabet. These letters have a
  /// case but do not change when they are mapped to any of the other cases.
  ///
  /// This property corresponds to the `Cased` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isCased: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_CASED)
  }

  /// A Boolean property indicating whether the scalar is ignored for casing
  /// purposes.
  ///
  /// This property corresponds to the `Case_Ignorable` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isCaseIgnorable: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_CASE_IGNORABLE)
  }

  /// A Boolean property indicating whether the scalar is one whose normalized
  /// form is not stable under a `toLowercase` mapping.
  ///
  /// This property corresponds to the `Changes_When_Lowercased` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var changesWhenLowercased: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_CHANGES_WHEN_LOWERCASED)
  }

  /// A Boolean property indicating whether the scalar is one whose normalized
  /// form is not stable under a `toUppercase` mapping.
  ///
  /// This property corresponds to the `Changes_When_Uppercased` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var changesWhenUppercased: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_CHANGES_WHEN_UPPERCASED)
  }

  /// A Boolean property indicating whether the scalar is one whose normalized
  /// form is not stable under a `toTitlecase` mapping.
  ///
  /// This property corresponds to the `Changes_When_Titlecased` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var changesWhenTitlecased: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_CHANGES_WHEN_TITLECASED)
  }

  /// A Boolean property indicating whether the scalar is one whose normalized
  /// form is not stable under case folding.
  ///
  /// This property corresponds to the `Changes_When_Casefolded` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var changesWhenCaseFolded: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_CHANGES_WHEN_CASEFOLDED)
  }

  /// A Boolean property indicating whether the scalar may change when it
  /// undergoes a case mapping.
  ///
  /// For any scalar `s`, it holds by definition that
  ///
  /// ```
  /// s.changesWhenCaseMapped = s.changesWhenLowercased ||
  ///                           s.changesWhenUppercased ||
  ///                           s.changesWhenTitlecased
  /// ```
  ///
  /// This property corresponds to the `Changes_When_Casemapped` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var changesWhenCaseMapped: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_CHANGES_WHEN_CASEMAPPED)
  }

  /// A Boolean property indicating whether the scalar is one that is not
  /// identical to its NFKC case-fold mapping.
  ///
  /// This property corresponds to the `Changes_When_NFKC_Casefolded` property
  /// in the [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var changesWhenNFKCCaseFolded: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_CHANGES_WHEN_NFKC_CASEFOLDED)
  }

  /// A Boolean property indicating whether the scalar has an emoji
  /// presentation, whether or not it is the default.
  ///
  /// This property is true for scalars that are rendered as emoji by default
  /// and also for scalars that have a non-default emoji rendering when followed
  /// by U+FE0F VARIATION SELECTOR-16. This includes some scalars that are not
  /// typically considered to be emoji:
  ///
  /// ```
  /// let sunglasses: Unicode.Scalar = "😎"
  /// let dollar: Unicode.Scalar = "$"
  /// let zero: Unicode.Scalar = "0"
  ///
  /// print(sunglasses.isEmoji)
  /// // Prints "true"
  /// print(dollar.isEmoji)
  /// // Prints "false"
  /// print(zero.isEmoji)
  /// // Prints "true"
  /// ```
  ///
  /// The final result is true because the ASCII digits have non-default emoji
  /// presentations; some platforms render these with an alternate appearance.
  ///
  /// Because of this behavior, testing `isEmoji` alone on a single scalar is
  /// insufficient to determine if a unit of text is rendered as an emoji; a
  /// correct test requires inspecting multiple scalars in a `Character`. In
  /// addition to checking whether the base scalar has `isEmoji == true`, you
  /// must also check its default presentation (see `isEmojiPresentation`) and
  /// determine whether it is followed by a variation selector that would modify
  /// the presentation.
  ///
  /// This property corresponds to the `Emoji` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isEmoji: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_EMOJI)
  }

  /// A Boolean property indicating whether the scalar is one that should be
  /// rendered with an emoji presentation, rather than a text presentation, by
  /// default.
  ///
  /// Scalars that have emoji presentation by default can be followed by
  /// U+FE0E VARIATION SELECTOR-15 to request the text presentation of the
  /// scalar instead. Likewise, scalars that default to text presentation can
  /// be followed by U+FE0F VARIATION SELECTOR-16 to request the emoji
  /// presentation.
  ///
  /// This property corresponds to the `Emoji_Presentation` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isEmojiPresentation: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_EMOJI_PRESENTATION)
  }

  /// A Boolean property indicating whether the scalar is one that can modify
  /// a base emoji that precedes it.
  ///
  /// The Fitzpatrick skin types are examples of emoji modifiers; they change
  /// the appearance of the preceding emoji base (that is, a scalar for which
  /// `isEmojiModifierBase` is true) by rendering it with a different skin tone.
  ///
  /// This property corresponds to the `Emoji_Modifier` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isEmojiModifier: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_EMOJI_MODIFIER)
  }

  /// A Boolean property indicating whether the scalar is one whose appearance
  /// can be changed by an emoji modifier that follows it.
  ///
  /// This property corresponds to the `Emoji_Modifier_Base` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isEmojiModifierBase: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_EMOJI_MODIFIER_BASE)
  }
}

extension Unicode {

  /// A version of the Unicode Standard represented by its `major.minor`
  /// components.
  public typealias Version = (major: Int, minor: Int)
}

extension Unicode.Scalar.Properties {

  /// The earliest version of the Unicode Standard in which the scalar was
  /// assigned.
  ///
  /// This value will be nil for code points that have not yet been assigned.
  ///
  /// This property corresponds to the `Age` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var age: Unicode.Version? {
    var versionInfo: __swift_stdlib_UVersionInfo = (0, 0, 0, 0)
    withUnsafeMutablePointer(to: &versionInfo.0) { versionInfoPointer in
      __swift_stdlib_u_charAge(_value, versionInfoPointer)
    }
    guard versionInfo.0 != 0 else { return nil }
    return (major: Int(versionInfo.0), Int(versionInfo.1))
  }
}

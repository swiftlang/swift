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
    @usableFromInline
    internal var _scalar: Unicode.Scalar

    internal init(_ scalar: Unicode.Scalar) {
      self._scalar = scalar
    }

    // Provide the value as UChar32 to make calling the ICU APIs cleaner
    internal var icuValue: __swift_stdlib_UChar32 {
      return __swift_stdlib_UChar32(bitPattern: self._scalar._value)
    }
  }

  /// A value that provides access to properties of the Unicode scalar that are
  /// defined by the Unicode standard.
  public var properties: Properties {
    return Properties(self)
  }
}

/// Boolean properties that are defined by the Unicode Standard (i.e., not
/// ICU-specific).
extension Unicode.Scalar.Properties {
  internal func _hasBinaryProperty(
    _ property: __swift_stdlib_UProperty
  ) -> Bool {
    return __swift_stdlib_u_hasBinaryProperty(icuValue, property) != 0
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
  /// Egyptian Hieroglyphs.
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
  ///   For example, the various "family" emoji are encoded as sequences of man,
  ///   woman, or child emoji that are interleaved with zero width joiners.
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

  /// A Boolean property indicating whether the scalar's normalized form differs
  /// from the `lowercaseMapping` of each constituent scalar.
  ///
  /// This property corresponds to the `Changes_When_Lowercased` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var changesWhenLowercased: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_CHANGES_WHEN_LOWERCASED)
  }

  /// A Boolean property indicating whether the scalar's normalized form differs
  /// from the `uppercaseMapping` of each constituent scalar.
  ///
  /// This property corresponds to the `Changes_When_Uppercased` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var changesWhenUppercased: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_CHANGES_WHEN_UPPERCASED)
  }

  /// A Boolean property indicating whether the scalar's normalized form differs
  /// from the `titlecaseMapping` of each constituent scalar.
  ///
  /// This property corresponds to the `Changes_When_Titlecased` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var changesWhenTitlecased: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_CHANGES_WHEN_TITLECASED)
  }

  /// A Boolean property indicating whether the scalar's normalized form differs
  /// from the case-fold mapping of each constituent scalar.
  ///
  /// This property corresponds to the `Changes_When_Casefolded` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var changesWhenCaseFolded: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_CHANGES_WHEN_CASEFOLDED)
  }

  /// A Boolean property indicating whether the scalar may change when it
  /// undergoes case mapping.
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

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
  // FIXME: These properties were introduced in ICU 57, but Ubuntu 16.04 comes
  // with ICU 55 so the values won't be correct there. Exclude them on
  // non-Darwin platforms for now; bundling ICU with the toolchain would resolve
  // this and other inconsistencies (https://bugs.swift.org/browse/SR-6076).

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
  @available(macOS 10.12.2, iOS 10.2, tvOS 10.1, watchOS 3.1.1, *)
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
  @available(macOS 10.12.2, iOS 10.2, tvOS 10.1, watchOS 3.1.1, *)
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
  @available(macOS 10.12.2, iOS 10.2, tvOS 10.1, watchOS 3.1.1, *)
  public var isEmojiModifier: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_EMOJI_MODIFIER)
  }

  /// A Boolean property indicating whether the scalar is one whose appearance
  /// can be changed by an emoji modifier that follows it.
  ///
  /// This property corresponds to the `Emoji_Modifier_Base` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  @available(macOS 10.12.2, iOS 10.2, tvOS 10.1, watchOS 3.1.1, *)
  public var isEmojiModifierBase: Bool {
    return _hasBinaryProperty(__swift_stdlib_UCHAR_EMOJI_MODIFIER_BASE)
  }
#endif
}

/// Case mapping properties.
extension Unicode.Scalar.Properties {
  // The type of ICU case conversion functions.
  internal typealias _U_StrToX = (
    /* dest */ UnsafeMutablePointer<__swift_stdlib_UChar>,
    /* destCapacity */ Int32,
    /* src */ UnsafePointer<__swift_stdlib_UChar>,
    /* srcLength */ Int32,
    /* locale */ UnsafePointer<Int8>,
    /* pErrorCode */ UnsafeMutablePointer<__swift_stdlib_UErrorCode>
  ) -> Int32

  /// Applies the given ICU string mapping to the scalar.
  ///
  /// This function attempts first to write the mapping into a stack-based
  /// UTF-16 buffer capable of holding 16 code units, which should be enough for
  /// all current case mappings. In the event more space is needed, it will be
  /// allocated on the heap.
  internal func _applyMapping(_ u_strTo: _U_StrToX) -> String {
    let utf16Length = UnicodeScalar(UInt32(_value))!.utf16.count
    var utf16 = _utf16CodeUnits

    // TODO(UTF8 perf): Stack buffer first and then detect real count
    let count = 64
    var array = Array<UInt16>(repeating: 0, count: count)
    let len: Int = array.withUnsafeMutableBufferPointer { bufPtr in
      return withUnsafePointer(to: &utf16) {
        (tuplePtr) -> Int in
        return tuplePtr.withMemoryRebound(to: UInt16.self, capacity: 2) {
          (utf16Pointer) -> Int in
          var err = __swift_stdlib_U_ZERO_ERROR
          let correctSize = u_strTo(
            bufPtr.baseAddress._unsafelyUnwrappedUnchecked,
            Int32(bufPtr.count),
            utf16Pointer,
            Int32(utf16Length),
            "",
            &err)
          guard err.isSuccess else {
            fatalError("Unexpected error case-converting Unicode scalar.")
          }
          // TODO: _sanityCheck(count == correctSize, "inconsistent ICU behavior")
          return Int(correctSize)
        }
      }
    }
    // TODO: replace `len` with `count`
    return array[..<len].withUnsafeBufferPointer {
      return String._uncheckedFromUTF16($0)
    }
  }

  /// The lowercase mapping of the scalar.
  ///
  /// This property is a `String`, not a `Unicode.Scalar` or `Character`,
  /// because some mappings may transform a scalar into multiple scalars or
  /// graphemes. For example, the character "İ" (U+0130 LATIN CAPITAL LETTER I
  /// WITH DOT ABOVE) becomes two scalars (U+0069 LATIN SMALL LETTER I, U+0307
  /// COMBINING DOT ABOVE) when converted to lowercase.
  ///
  /// This property corresponds to the `Lowercase_Mapping` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var lowercaseMapping: String {
    return _applyMapping(__swift_stdlib_u_strToLower)
  }

  /// The titlecase mapping of the scalar.
  ///
  /// This property is a `String`, not a `Unicode.Scalar` or `Character`,
  /// because some mappings may transform a scalar into multiple scalars or
  /// graphemes. For example, the ligature "ﬁ" (U+FB01 LATIN SMALL LIGATURE FI)
  /// becomes "Fi" (U+0046 LATIN CAPITAL LETTER F, U+0069 LATIN SMALL LETTER I)
  /// when converted to titlecase.
  ///
  /// This property corresponds to the `Titlecase_Mapping` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var titlecaseMapping: String {
    return _applyMapping { ptr, cap, src, len, locale, err in
      return __swift_stdlib_u_strToTitle(ptr, cap, src, len, nil, locale, err)
    }
  }

  /// The uppercase mapping of the scalar.
  ///
  /// This property is a `String`, not a `Unicode.Scalar` or `Character`,
  /// because some mappings may transform a scalar into multiple scalars or
  /// graphemes. For example, the German letter "ß" (U+00DF LATIN SMALL LETTER
  /// SHARP S) becomes "SS" (U+0053 LATIN CAPITAL LETTER S, U+0053 LATIN CAPITAL
  /// LETTER S) when converted to uppercase.
  ///
  /// This property corresponds to the `Uppercase_Mapping` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var uppercaseMapping: String {
    return _applyMapping(__swift_stdlib_u_strToUpper)
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
    withUnsafeMutablePointer(to: &versionInfo) { tuplePtr in
      tuplePtr.withMemoryRebound(to: UInt8.self, capacity: 4) {
        versionInfoPtr in
        __swift_stdlib_u_charAge(icuValue, versionInfoPtr)
      }
    }
    guard versionInfo.0 != 0 else { return nil }
    return (major: Int(versionInfo.0), minor: Int(versionInfo.1))
  }
}

extension Unicode {

  /// The most general classification of a Unicode scalar.
  ///
  /// The general category of a scalar is its "first-order, most usual
  /// categorization". It does not attempt to cover multiple uses of some
  /// scalars, such as the use of letters to represent Roman numerals.
  public enum GeneralCategory {

    /// An uppercase letter.
    ///
    /// This value corresponds to the category `Uppercase_Letter` (abbreviated
    /// `Lu`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case uppercaseLetter

    /// A lowercase letter.
    ///
    /// This value corresponds to the category `Lowercase_Letter` (abbreviated
    /// `Ll`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case lowercaseLetter

    /// A digraph character whose first part is uppercase.
    ///
    /// This value corresponds to the category `Titlecase_Letter` (abbreviated
    /// `Lt`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case titlecaseLetter

    /// A modifier letter.
    ///
    /// This value corresponds to the category `Modifier_Letter` (abbreviated
    /// `Lm`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case modifierLetter

    /// Other letters, including syllables and ideographs.
    ///
    /// This value corresponds to the category `Other_Letter` (abbreviated
    /// `Lo`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case otherLetter

    /// A non-spacing combining mark with zero advance width (abbreviated Mn).
    ///
    /// This value corresponds to the category `Nonspacing_Mark` (abbreviated
    /// `Mn`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case nonspacingMark

    /// A spacing combining mark with positive advance width.
    ///
    /// This value corresponds to the category `Spacing_Mark` (abbreviated `Mc`)
    /// in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case spacingMark

    /// An enclosing combining mark.
    ///
    /// This value corresponds to the category `Enclosing_Mark` (abbreviated
    /// `Me`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case enclosingMark

    /// A decimal digit.
    ///
    /// This value corresponds to the category `Decimal_Number` (abbreviated
    /// `Nd`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case decimalNumber

    /// A letter-like numeric character.
    ///
    /// This value corresponds to the category `Letter_Number` (abbreviated
    /// `Nl`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case letterNumber

    /// A numeric character of another type.
    ///
    /// This value corresponds to the category `Other_Number` (abbreviated `No`)
    /// in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case otherNumber

    /// A connecting punctuation mark, like a tie.
    ///
    /// This value corresponds to the category `Connector_Punctuation`
    /// (abbreviated `Pc`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case connectorPunctuation

    /// A dash or hyphen punctuation mark.
    ///
    /// This value corresponds to the category `Dash_Punctuation` (abbreviated
    /// `Pd`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case dashPunctuation

    /// An opening punctuation mark of a pair.
    ///
    /// This value corresponds to the category `Open_Punctuation` (abbreviated
    /// `Ps`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case openPunctuation

    /// A closing punctuation mark of a pair.
    ///
    /// This value corresponds to the category `Close_Punctuation` (abbreviated
    /// `Pe`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case closePunctuation

    /// An initial quotation mark.
    ///
    /// This value corresponds to the category `Initial_Punctuation`
    /// (abbreviated `Pi`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case initialPunctuation

    /// A final quotation mark.
    ///
    /// This value corresponds to the category `Final_Punctuation` (abbreviated
    /// `Pf`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case finalPunctuation

    /// A punctuation mark of another type.
    ///
    /// This value corresponds to the category `Other_Punctuation` (abbreviated
    /// `Po`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case otherPunctuation

    /// A symbol of mathematical use.
    ///
    /// This value corresponds to the category `Math_Symbol` (abbreviated `Sm`)
    /// in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case mathSymbol

    /// A currency sign.
    ///
    /// This value corresponds to the category `Currency_Symbol` (abbreviated
    /// `Sc`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case currencySymbol

    /// A non-letterlike modifier symbol.
    ///
    /// This value corresponds to the category `Modifier_Symbol` (abbreviated
    /// `Sk`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case modifierSymbol

    /// A symbol of another type.
    ///
    /// This value corresponds to the category `Other_Symbol` (abbreviated
    /// `So`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case otherSymbol

    /// A space character of non-zero width.
    ///
    /// This value corresponds to the category `Space_Separator` (abbreviated
    /// `Zs`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case spaceSeparator

    /// A line separator, which is specifically (and only) U+2028 LINE
    /// SEPARATOR.
    ///
    /// This value corresponds to the category `Line_Separator` (abbreviated
    /// `Zl`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case lineSeparator

    /// A paragraph separator, which is specifically (and only) U+2029 PARAGRAPH
    /// SEPARATOR.
    ///
    /// This value corresponds to the category `Paragraph_Separator`
    /// (abbreviated `Zp`) in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case paragraphSeparator

    /// A C0 or C1 control code.
    ///
    /// This value corresponds to the category `Control` (abbreviated `Cc`) in
    /// the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case control

    /// A format control character.
    ///
    /// This value corresponds to the category `Format` (abbreviated `Cf`) in
    /// the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case format

    /// A surrogate code point.
    ///
    /// This value corresponds to the category `Surrogate` (abbreviated `Cs`) in
    /// the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case surrogate

    /// A private-use character.
    ///
    /// This value corresponds to the category `Private_Use` (abbreviated `Co`)
    /// in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case privateUse

    /// A reserved unassigned code point or a non-character.
    ///
    /// This value corresponds to the category `Unassigned` (abbreviated `Cn`)
    /// in the
    /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
    case unassigned

    internal init(rawValue: __swift_stdlib_UCharCategory) {
      switch rawValue {
      case __swift_stdlib_U_UNASSIGNED: self = .unassigned
      case __swift_stdlib_U_UPPERCASE_LETTER: self = .uppercaseLetter
      case __swift_stdlib_U_LOWERCASE_LETTER: self = .lowercaseLetter
      case __swift_stdlib_U_TITLECASE_LETTER: self = .titlecaseLetter
      case __swift_stdlib_U_MODIFIER_LETTER: self = .modifierLetter
      case __swift_stdlib_U_OTHER_LETTER: self = .otherLetter
      case __swift_stdlib_U_NON_SPACING_MARK: self = .nonspacingMark
      case __swift_stdlib_U_ENCLOSING_MARK: self = .enclosingMark
      case __swift_stdlib_U_COMBINING_SPACING_MARK: self = .spacingMark
      case __swift_stdlib_U_DECIMAL_DIGIT_NUMBER: self = .decimalNumber
      case __swift_stdlib_U_LETTER_NUMBER: self = .letterNumber
      case __swift_stdlib_U_OTHER_NUMBER: self = .otherNumber
      case __swift_stdlib_U_SPACE_SEPARATOR: self = .spaceSeparator
      case __swift_stdlib_U_LINE_SEPARATOR: self = .lineSeparator
      case __swift_stdlib_U_PARAGRAPH_SEPARATOR: self = .paragraphSeparator
      case __swift_stdlib_U_CONTROL_CHAR: self = .control
      case __swift_stdlib_U_FORMAT_CHAR: self = .format
      case __swift_stdlib_U_PRIVATE_USE_CHAR: self = .privateUse
      case __swift_stdlib_U_SURROGATE: self = .surrogate
      case __swift_stdlib_U_DASH_PUNCTUATION: self = .dashPunctuation
      case __swift_stdlib_U_START_PUNCTUATION: self = .openPunctuation
      case __swift_stdlib_U_END_PUNCTUATION: self = .closePunctuation
      case __swift_stdlib_U_CONNECTOR_PUNCTUATION: self = .connectorPunctuation
      case __swift_stdlib_U_OTHER_PUNCTUATION: self = .otherPunctuation
      case __swift_stdlib_U_MATH_SYMBOL: self = .mathSymbol
      case __swift_stdlib_U_CURRENCY_SYMBOL: self = .currencySymbol
      case __swift_stdlib_U_MODIFIER_SYMBOL: self = .modifierSymbol
      case __swift_stdlib_U_OTHER_SYMBOL: self = .otherSymbol
      case __swift_stdlib_U_INITIAL_PUNCTUATION: self = .initialPunctuation
      case __swift_stdlib_U_FINAL_PUNCTUATION: self = .finalPunctuation
      default: fatalError("Unknown general category \(rawValue)")
      }
    }
  }
}

extension Unicode.Scalar.Properties {

  /// The general category (most usual classification) of the scalar.
  ///
  /// This property corresponds to the `General_Category` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var generalCategory: Unicode.GeneralCategory {
    let rawValue = __swift_stdlib_UCharCategory(
      __swift_stdlib_UCharCategory.RawValue(
      __swift_stdlib_u_getIntPropertyValue(
        icuValue, __swift_stdlib_UCHAR_GENERAL_CATEGORY)))
    return Unicode.GeneralCategory(rawValue: rawValue)
  }
}

extension Unicode.Scalar.Properties {

  internal func _scalarName(
    _ choice: __swift_stdlib_UCharNameChoice
  ) -> String? {
    var err = __swift_stdlib_U_ZERO_ERROR
    let count = Int(__swift_stdlib_u_charName(icuValue, choice, nil, 0, &err))
    guard count > 0 else { return nil }

    // ICU writes a trailing null, so we have to save room for it as well.
    var array = Array<UInt8>(repeating: 0, count: count + 1)
    return array.withUnsafeMutableBufferPointer { bufPtr in
      var err = __swift_stdlib_U_ZERO_ERROR
      let correctSize = __swift_stdlib_u_charName(
        icuValue,
        choice,
        UnsafeMutableRawPointer(bufPtr.baseAddress._unsafelyUnwrappedUnchecked)
          .assumingMemoryBound(to: Int8.self),
        Int32(bufPtr.count),
        &err)
      guard err.isSuccess else {
        fatalError("Unexpected error case-converting Unicode scalar.")
      }
      _sanityCheck(count == correctSize, "inconsistent ICU behavior")
      return String._fromASCII(
        UnsafeBufferPointer(rebasing: bufPtr[..<count]))
    }
  }

  /// The published name of the scalar.
  ///
  /// Some scalars, such as control characters, do not have a value for this
  /// property in the UCD. For such scalars, this property will be nil.
  ///
  /// This property corresponds to the `Name` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var name: String? {
    return _scalarName(__swift_stdlib_U_UNICODE_CHAR_NAME)
  }

  /// The normative formal alias of the scalar, or nil if it has no alias.
  ///
  /// The name of a scalar is immutable and never changed in future versions of
  /// the Unicode Standard. The `nameAlias` property is provided to issue
  /// corrections if a name was issued erroneously. For example, the `name` of
  /// U+FE18 is "PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET"
  /// (note that "BRACKET" is misspelled). The `nameAlias` property then
  /// contains the corrected name.
  ///
  /// This property corresponds to the `Name_Alias` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var nameAlias: String? {
    return _scalarName(__swift_stdlib_U_CHAR_NAME_ALIAS)
  }
}

extension Unicode {

  /// The classification of a scalar used in the Canonical Ordering Algorithm
  /// defined by the Unicode Standard.
  ///
  /// Canonical combining classes are used by the ordering algorithm to
  /// determine if two sequences of combining marks should be considered
  /// canonically equivalent (that is, identical in interpretation). Two
  /// sequences are canonically equivalent if they are equal when sorting the
  /// scalars in ascending order by their combining class.
  ///
  /// For example, consider the sequence `"\u{0041}\u{0301}\u{0316}"` (LATIN
  /// CAPITAL LETTER A, COMBINING ACUTE ACCENT, COMBINING GRAVE ACCENT BELOW).
  /// The combining classes of these scalars have the numeric values 0, 230, and
  /// 220, respectively. Sorting these scalars by their combining classes yields
  /// `"\u{0041}\u{0316}\u{0301}"`, so two strings that differ only by the
  /// ordering of those scalars would compare as equal:
  ///
  /// ```
  /// print("\u{0041}\u{0316}\u{0301}" == "\u{0041}\u{0301}\u{0316}")
  /// // Prints "true"
  /// ```
  ///
  /// Named and Unnamed Combining Classes
  /// ===================================
  ///
  /// Canonical combining classes are defined in the Unicode Standard as
  /// integers in the range `0...254`. For convenience, the standard assigns
  /// symbolic names to a subset of these combining classes.
  ///
  /// The `CanonicalCombiningClass` type conforms to `RawRepresentable` with a
  /// raw value of type `UInt8`. Instances of the type can be created from the
  /// actual numeric value using the `init(rawValue:)` initializer, and
  /// combining classes with symbolic names can also be referenced using the
  /// static members that share those names.
  ///
  /// ```
  /// print(Unicode.CanonicalCombiningClass(rawValue: 1) == .overlay)
  /// // Prints "true"
  /// ```
  public struct CanonicalCombiningClass:
    Comparable, Hashable, RawRepresentable
  {
    /// Base glyphs that occupy their own space and do not combine with others.
    public static let notReordered = CanonicalCombiningClass(rawValue: 0)

    /// Marks that overlay a base letter or symbol.
    public static let overlay = CanonicalCombiningClass(rawValue: 1)

    /// Diacritic nukta marks in Brahmi-derived scripts.
    public static let nukta = CanonicalCombiningClass(rawValue: 7)

    /// Combining marks that are attached to hiragana and katakana to indicate
    /// voicing changes.
    public static let kanaVoicing = CanonicalCombiningClass(rawValue: 8)

    /// Diacritic virama marks in Brahmi-derived scripts.
    public static let virama = CanonicalCombiningClass(rawValue: 9)

    /// Marks attached at the bottom left.
    public static let attachedBelowLeft = CanonicalCombiningClass(rawValue: 200)

    /// Marks attached directly below.
    public static let attachedBelow = CanonicalCombiningClass(rawValue: 202)

    /// Marks attached directly above.
    public static let attachedAbove = CanonicalCombiningClass(rawValue: 214)

    /// Marks attached at the top right.
    public static let attachedAboveRight =
      CanonicalCombiningClass(rawValue: 216)

    /// Distinct marks at the bottom left.
    public static let belowLeft = CanonicalCombiningClass(rawValue: 218)

    /// Distinct marks directly below.
    public static let below = CanonicalCombiningClass(rawValue: 220)

    /// Distinct marks at the bottom right.
    public static let belowRight = CanonicalCombiningClass(rawValue: 222)

    /// Distinct marks to the left.
    public static let left = CanonicalCombiningClass(rawValue: 224)

    /// Distinct marks to the right.
    public static let right = CanonicalCombiningClass(rawValue: 226)

    /// Distinct marks at the top left.
    public static let aboveLeft = CanonicalCombiningClass(rawValue: 228)

    /// Distinct marks directly above.
    public static let above = CanonicalCombiningClass(rawValue: 230)

    /// Distinct marks at the top right.
    public static let aboveRight = CanonicalCombiningClass(rawValue: 232)

    /// Distinct marks subtending two bases.
    public static let doubleBelow = CanonicalCombiningClass(rawValue: 233)

    /// Distinct marks extending above two bases.
    public static let doubleAbove = CanonicalCombiningClass(rawValue: 234)

    /// Greek iota subscript only (U+0345 COMBINING GREEK YPOGEGRAMMENI).
    public static let iotaSubscript = CanonicalCombiningClass(rawValue: 240)

    /// The raw integer value of the canonical combining class.
    public let rawValue: UInt8

    /// Creates a new canonical combining class with the given raw integer
    /// value.
    ///
    /// - Parameter rawValue: The raw integer value of the canonical combining
    ///   class.
    public init(rawValue: UInt8) {
      self.rawValue = rawValue
    }

    public static func < (
      lhs: CanonicalCombiningClass,
      rhs: CanonicalCombiningClass
    ) -> Bool {
      return lhs.rawValue < rhs.rawValue
    }
  }
}

extension Unicode.Scalar.Properties {

  /// The canonical combining class of the scalar.
  ///
  /// This property corresponds to the `Canonical_Combining_Class` property in
  /// the [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var canonicalCombiningClass: Unicode.CanonicalCombiningClass {
    let rawValue = UInt8(__swift_stdlib_u_getIntPropertyValue(
      icuValue, __swift_stdlib_UCHAR_CANONICAL_COMBINING_CLASS))
    return Unicode.CanonicalCombiningClass(rawValue: rawValue)
  }
}

extension Unicode {

  /// The numeric type of a scalar.
  ///
  /// Scalars with a non-nil numeric type include numbers, fractions, numeric
  /// superscripts and subscripts, and circled or otherwise decorated number
  /// glyphs.
  ///
  /// Some letterlike scalars used in numeric systems, such as Greek or Latin
  /// letters, do not have a non-nil numeric type, in order to prevent programs
  /// from incorrectly interpreting them as numbers in non-numeric contexts.
  public enum NumericType {

    /// Digits that are commonly understood to form base-10 numbers.
    ///
    /// Specifically, scalars have this numeric type if they occupy a contiguous
    /// range of code points representing numeric values `0...9`.
    case decimal

    /// Decimal digits that otherwise do not meet the requirements of numeric
    /// type `decimal`.
    ///
    /// Scalars with this numeric type are often those that represent a decimal
    /// digit but would not typically be used to write a base-10 number, such as
    /// "④" (U+2463 CIRCLED DIGIT FOUR).
    ///
    /// In practice, the distinction between `digit` and `numeric` has not
    /// proven to be valuable. As of Unicode 6.3, any new scalars that represent
    /// numbers but do not meet the requirements of `decimal` will have numeric
    /// type `numeric`, and programs can treat `digit` and `numeric`
    /// equivalently.
    case digit

    /// Numbers that are not decimal digits.
    ///
    /// This numeric type includes fractions such as "⅕" (U+2155 VULGAR FRACITON
    /// ONE FIFTH), numerical CJK ideographs like "兆" (U+5146 CJK UNIFIED
    /// IDEOGRAPH-5146), and other scalars that are not decimal digits used
    /// positionally in the writing of base-10 numbers.
    case numeric

    internal init?(rawValue: __swift_stdlib_UNumericType) {
      switch rawValue {
      case __swift_stdlib_U_NT_NONE: return nil
      case __swift_stdlib_U_NT_DECIMAL: self = .decimal
      case __swift_stdlib_U_NT_DIGIT: self = .digit
      case __swift_stdlib_U_NT_NUMERIC: self = .numeric
      default: fatalError("Unknown numeric type \(rawValue)")
      }
    }
  }
}

/// Numeric properties of scalars.
extension Unicode.Scalar.Properties {

  /// The numeric type of the scalar.
  ///
  /// The value of this property is nil for scalars that do not represent a
  /// number.
  ///
  /// ```
  /// print("X", ("X" as Unicode.Scalar).properties.numericType ?? "nil")
  /// // Prints "X nil"
  /// print("4", ("4" as Unicode.Scalar).properties.numericType ?? "nil")
  /// // Prints "4 decimal"
  /// print("\u{2463}", ("\u{2463}" as Unicode.Scalar).properties.numericType ?? "nil")
  /// // Prints "④ digit"
  /// print("\u{2155}", ("\u{2155}" as Unicode.Scalar).properties.numericType ?? "nil")
  /// // Prints "⅕ numeric"
  /// ```
  ///
  /// This property corresponds to the `Numeric_Type` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var numericType: Unicode.NumericType? {
    let rawValue = __swift_stdlib_UNumericType(
      __swift_stdlib_UNumericType.RawValue(
      __swift_stdlib_u_getIntPropertyValue(
        icuValue, __swift_stdlib_UCHAR_NUMERIC_TYPE)))
    return Unicode.NumericType(rawValue: rawValue)
  }

  /// The numeric value of the scalar.
  ///
  /// The value of this property is nil for scalars that do not represent a
  /// number.
  ///
  /// The numeric value of a scalar is represented as a `Double` because some
  /// scalars represent fractions:
  ///
  /// ```
  /// print("X", ("X" as Unicode.Scalar).properties.numericValue ?? "nil")
  /// // Prints "X nil"
  /// print("4", ("4" as Unicode.Scalar).properties.numericValue ?? "nil")
  /// // Prints "4 4.0"
  /// print("\u{2463}", ("\u{2463}" as Unicode.Scalar).properties.numericValue ?? "nil")
  /// // Prints "④ 4.0"
  /// print("\u{2155}", ("\u{2155}" as Unicode.Scalar).properties.numericValue ?? "nil")
  /// // Prints "⅕ 0.2"
  /// ```
  ///
  /// This property corresponds to the `Numeric_Value` property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var numericValue: Double? {
    let icuNoNumericValue: Double = -123456789
    let result = __swift_stdlib_u_getNumericValue(icuValue)
    return result != icuNoNumericValue ? result : nil
  }
}

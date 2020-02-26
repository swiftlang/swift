//===----------------- OSLogIntegerFormatting.swift -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This file defines types and functions for specifying formatting of
// integer-valued interpolations passed to the os log APIs.

@frozen
public struct OSLogIntegerFormatting {
  /// The base to use for the string representation. `radix` must be at least 2
  /// and at most 36. The default is 10.
  public var radix: Int

  /// When set, a `+` will be printed for all non-negative integers.
  public var explicitPositiveSign: Bool

  /// When set, a prefix: 0b or 0o or 0x will be added when the radix is 2, 8 or
  /// 16 respectively.
  public var includePrefix: Bool

  /// Whether to use uppercase letters to represent numerals
  /// greater than 9 (default is to use lowercase).
  public var uppercase: Bool

  /// Minimum number of digits to display. Numbers having fewer digits than
  /// minDigits will be displayed with leading zeros.
  public var minDigits: (() -> Int)?

  /// - Parameters:
  ///   - radix: The base to use for the string representation. `radix` must be
  ///     at least 2 and at most 36. The default is 10.
  ///   - explicitPositiveSign: Pass `true` to add a + sign to non-negative
  ///     numbers. Default is `false`.
  ///   - includePrefix: Pass `true` to add a prefix: 0b, 0o, 0x to corresponding
  ///     radices. Default is `false`.
  ///   - uppercase: Pass `true` to use uppercase letters to represent numerals
  ///     greater than 9, or `false` to use lowercase letters. The default is
  ///     `false`.
  ///   - minDigits: minimum number of digits to display. Numbers will be
  ///     prefixed with zeros if necessary to meet the minimum. The default is 1.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal init(
    radix: Int = 10,
    explicitPositiveSign: Bool = false,
    includePrefix: Bool = false,
    uppercase: Bool = false,
    minDigits: (() -> Int)?
  ) {
    precondition(radix >= 2 && radix <= 36)

    self.radix = radix
    self.explicitPositiveSign = explicitPositiveSign
    self.includePrefix = includePrefix
    self.uppercase = uppercase
    self.minDigits = minDigits
  }

  /// - Parameters:
  ///   - explicitPositiveSign: Pass `true` to add a + sign to non-negative
  ///     numbers. Default is `false`.
  ///   - minDigits: minimum number of digits to display. Numbers will be
  ///     prefixed with zeros if necessary to meet the minimum. The default is 1.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static func decimal(
    explicitPositiveSign: Bool = false,
    minDigits: @escaping @autoclosure () -> Int
  ) -> OSLogIntegerFormatting {
    return OSLogIntegerFormatting(
      radix: 10,
      explicitPositiveSign: explicitPositiveSign,
      minDigits: minDigits)
  }

  /// - Parameters:
  ///   - explicitPositiveSign: Pass `true` to add a + sign to non-negative
  ///     numbers. Default is `false`.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static func decimal(
    explicitPositiveSign: Bool = false
  ) -> OSLogIntegerFormatting {
    return OSLogIntegerFormatting(
      radix: 10,
      explicitPositiveSign: explicitPositiveSign,
      minDigits: nil)
  }

  /// Default decimal format.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static var decimal: OSLogIntegerFormatting { .decimal() }

  /// - Parameters:
  ///   - explicitPositiveSign: Pass `true` to add a + sign to non-negative
  ///     numbers. Default is `false`.
  ///   - includePrefix: Pass `true` to add a prefix: 0b, 0o, 0x to corresponding
  ///     radices. Default is `false`.
  ///   - uppercase: Pass `true` to use uppercase letters to represent numerals
  ///     greater than 9, or `false` to use lowercase letters. The default is
  ///     `false`.
  ///   - minDigits: minimum number of digits to display. Numbers will be
  ///     prefixed with zeros if necessary to meet the minimum. The default is 1.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static func hex(
    explicitPositiveSign: Bool = false,
    includePrefix: Bool = false,
    uppercase: Bool = false,
    minDigits: @escaping @autoclosure () -> Int
  ) -> OSLogIntegerFormatting {
    return OSLogIntegerFormatting(
      radix: 16,
      explicitPositiveSign: explicitPositiveSign,
      includePrefix: includePrefix,
      uppercase: uppercase,
      minDigits: minDigits)
  }

  /// - Parameters:
  ///   - explicitPositiveSign: Pass `true` to add a + sign to non-negative
  ///     numbers. Default is `false`.
  ///   - includePrefix: Pass `true` to add a prefix: 0b, 0o, 0x to corresponding
  ///     radices. Default is `false`.
  ///   - uppercase: Pass `true` to use uppercase letters to represent numerals
  ///     greater than 9, or `false` to use lowercase letters. The default is
  ///     `false`.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static func hex(
    explicitPositiveSign: Bool = false,
    includePrefix: Bool = false,
    uppercase: Bool = false
  ) -> OSLogIntegerFormatting {
    return OSLogIntegerFormatting(
      radix: 16,
      explicitPositiveSign: explicitPositiveSign,
      includePrefix: includePrefix,
      uppercase: uppercase,
      minDigits: nil)
  }

  /// Default hexadecimal format.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static var hex: OSLogIntegerFormatting { .hex() }

  /// - Parameters:
  ///   - explicitPositiveSign: Pass `true` to add a + sign to non-negative
  ///     numbers. Default is `false`.
  ///   - includePrefix: Pass `true` to add a prefix: 0b, 0o, 0x to corresponding
  ///     radices. Default is `false`.
  ///   - uppercase: Pass `true` to use uppercase letters to represent numerals
  ///     greater than 9, or `false` to use lowercase letters. The default is
  ///     `false`.
  ///   - minDigits: minimum number of digits to display. Numbers will be
  ///     prefixed with zeros if necessary to meet the minimum. The default is 1.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static func octal(
    explicitPositiveSign: Bool = false,
    includePrefix: Bool = false,
    uppercase: Bool = false,
    minDigits: @autoclosure @escaping () -> Int
  ) -> OSLogIntegerFormatting {
    OSLogIntegerFormatting(
      radix: 8,
      explicitPositiveSign: explicitPositiveSign,
      includePrefix: includePrefix,
      uppercase: uppercase,
      minDigits: minDigits)
  }

  /// - Parameters:
  ///   - explicitPositiveSign: Pass `true` to add a + sign to non-negative
  ///     numbers. Default is `false`.
  ///   - includePrefix: Pass `true` to add a prefix: 0b, 0o, 0x to corresponding
  ///     radices. Default is `false`.
  ///   - uppercase: Pass `true` to use uppercase letters to represent numerals
  ///     greater than 9, or `false` to use lowercase letters. The default is
  ///     `false`.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static func octal(
    explicitPositiveSign: Bool = false,
    includePrefix: Bool = false,
    uppercase: Bool = false
  ) -> OSLogIntegerFormatting {
    OSLogIntegerFormatting(
      radix: 8,
      explicitPositiveSign: explicitPositiveSign,
      includePrefix: includePrefix,
      uppercase: uppercase,
      minDigits: nil)
  }

  /// Default octal format.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static var octal: OSLogIntegerFormatting { .octal() }
}

extension OSLogIntegerFormatting {
  /// The prefix for the radix in the Swift literal syntax.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal var _prefix: String {
    guard includePrefix else { return "" }
    switch radix {
    case 2: return "0b"
    case 8: return "0o"
    case 16: return "0x"
    default: return ""
    }
  }
}

extension OSLogIntegerFormatting {

  /// Returns a fprintf-compatible length modifier for a given argument type.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal static func formatSpecifierLengthModifier<I: FixedWidthInteger>(
    _ type: I.Type
  ) -> String? {
    // IEEE Std 1003.1-2017, length modifiers:
    switch type {
    //   hh - d, i, o, u, x, or X conversion specifier applies to
    // (signed|unsigned) char
    case is CChar.Type: return "hh"
    case is CUnsignedChar.Type: return "hh"

    //   h  - d, i, o, u, x, or X conversion specifier applies to
    // (signed|unsigned) short
    case is CShort.Type: return "h"
    case is CUnsignedShort.Type: return "h"

    case is CInt.Type: return ""
    case is CUnsignedInt.Type: return ""

    //   l  - d, i, o, u, x, or X conversion specifier applies to
    // (signed|unsigned) long
    case is CLong.Type: return "l"
    case is CUnsignedLong.Type: return "l"

    //   ll - d, i, o, u, x, or X conversion specifier applies to
    // (signed|unsigned) long long
    case is CLongLong.Type: return "ll"
    case is CUnsignedLongLong.Type: return "ll"

    default: return nil
    }
  }

  /// Constructs an os_log format specifier for the given type `type`
  /// using the specified alignment `align` and privacy qualifier `privacy`.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  @_effects(readonly)
  internal func formatSpecifier<I: FixedWidthInteger>(
    for type: I.Type,
    align: OSLogStringAlignment,
    privacy: OSLogPrivacy
  ) -> String {
    // Based on IEEE Std 1003.1-2017
    // `d`/`i` is the only signed integral conversions allowed
    if (type.isSigned && radix != 10) {
      fatalError("Signed integers must be formatted using .decimal")
    }

    // IEEE: Each conversion specification is introduced by the '%' character
    // after which the following appear in sequence:
    //   1. Zero or more flags (in any order), which modify the meaning of the
    //      conversion specification.
    //   2. An optional minimum field width (for alignment). If the converted
    //      value has fewer bytes than the field width, it shall be padded with
    //      <space> characters by default on the left; it shall be padded on the
    //      right if the left-adjustment flag ( '-' ), is given to the
    //      field width.
    //   3. An optional precision that gives the minimum number of digits to
    //      display for the d, i, o, u, x, and X conversion specifiers.
    //   4. An optional length modifier that specifies the size of the argument.
    //   5. A conversion specifier character that indicates the type of
    //      conversion to be applied.

    // Use Swift style prefixes rather than fprintf style prefixes.
    var specification = _prefix
    specification += "%"

    // Add privacy qualifier after % sign within curly braces. This is an
    // os log specific flag.
    switch privacy {
    case .private:
      specification += "{private}"
    case .public:
      specification += "{public}"
    default:
      break
    }

    //
    // 1. Flags
    //
    // Use `+` flag if signed, otherwise prefix a literal `+` for unsigned.
    if explicitPositiveSign {
      // IEEE: `+` The result of a signed conversion shall always begin with a
      // sign ( '+' or '-' )
      if type.isSigned {
        specification += "+"
      } else {
        var newSpecification = "+"
        newSpecification += specification
        specification = newSpecification
      }
    }

    // IEEE: `-` The result of the conversion shall be left-justified within
    // the field. The conversion is right-justified if this flag is not
    // specified.
    if case .start = align.anchor {
      specification += "-"
    }

    // 2. Minimumn field width
    // IEEE: When field width is prefixed by `0`, leading zeros (following any
    // indication of sign or base) are used to pad to the field width rather
    // than performing space padding. If the '0' and '-' flags both appear,
    // the '0' flag is ignored. If a precision is specified, the '0' flag shall
    // be ignored.
    //
    // Commentary: `0` is prefixed to field width when the user doesn't want to
    // use precision (minDigits). This allows sign and prefix characters to be
    // counted towards field width (they wouldn't be counted towards precision).
    // This is more useful for floats, where precision is digits after the radix.
    // In our case, we're already handling prefix ourselves; we choose not to
    // support this functionality. In our case, alignment always pads spaces (
    // to the left or right) until the minimum field width is met.
    if let _ = align.minimumColumnWidth {
      // The alignment could be a dynamic value. Therefore, use a star here and pass it
      // as an additional argument.
      specification += "*"
    }

    // 3. Precision

    // Default precision for integers is 1, otherwise use the requested precision.
    // The precision could be a dynamic value. Therefore, use a star here and pass it
    // as an additional argument.
    if let _ = minDigits {
      specification += ".*"
    }

    // 4. Length modifier
    guard let lengthModifier =
      OSLogIntegerFormatting.formatSpecifierLengthModifier(type) else {
      fatalError("Integer type has unknown byte length")
    }
    specification += lengthModifier

    // 5. The conversion specifier
    switch radix {
    case 10:
      specification += type.isSigned ? "d" : "u"
    case 8:
      specification += "o"
    case 16:
      specification += uppercase ? "X" : "x"
    default:
      fatalError("radix must be 10, 8 or 16")
    }
    return specification
  }
}

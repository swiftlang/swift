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

extension String {
    /// Replace all percents "%" in the string by "%%" so that the string can be
    /// interpreted as a C format string. This function is constant evaluable
    /// and its semantics is modeled within the evaluator.
    @inlinable
    internal var percentEscapedString: String {
        @_semantics("string.escapePercent.get")
        @_effects(readonly)
        @_optimize(none)
        get {
            return self
                .split(separator: "%", omittingEmptySubsequences: false)
                .joined(separator: "%%")
        }
    }
}

extension MyCustomInterpolation {

    /// Defines interpolation for UnsafeRawBufferPointer.
    ///
    /// Do not call this function directly.
    ///
    /// - Parameters:
    ///   - pointer: The interpolated expression of type `UnsafeRawBufferPointer`,
    ///     which is autoclosured.
    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    @_semantics("oslog.requires_constant_arguments")
    public mutating func appendInterpolation(
        _ pointer: @autoclosure @escaping () -> UnsafeRawBufferPointer
    ) {
        appendInterpolation(pointer().baseAddress!)
    }

    /// Defines interpolation for UnsafeRawPointer.
    ///
    /// Do not call this function directly.
    ///
    /// - Parameters:
    ///   - pointer: The interpolated expression of type `UnsafeRawPointer`, which is autoclosured.
    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    @_semantics("oslog.requires_constant_arguments")
    public mutating func appendInterpolation(
        _ pointer: @autoclosure @escaping () -> UnsafeRawPointer
    ) {
        formatString += "%p"
    }
}

@frozen
public struct MyCustomIntegerFormatting {
    /// The base to use for the string representation. `radix` must be at least 2
    /// and at most 36. The default is 10.
    @usableFromInline
    internal var radix: Int

    /// When set, a `+` will be printed for all non-negative integers.
    @usableFromInline
    internal var explicitPositiveSign: Bool

    /// When set, a prefix: 0b or 0o or 0x will be added when the radix is 2, 8 or
    /// 16 respectively.
    @usableFromInline
    internal var includePrefix: Bool

    /// Whether to use uppercase letters to represent numerals
    /// greater than 9 (default is to use lowercase).
    @usableFromInline
    internal var uppercase: Bool

    /// Minimum number of digits to display. Numbers having fewer digits than
    /// minDigits will be displayed with leading zeros.
    @usableFromInline
    internal var minDigits: (() -> Int)?

    /// Initializes all stored properties.
    ///
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
    @_transparent
    @usableFromInline
    internal init(
        radix: Int = 10,
        explicitPositiveSign: Bool = false,
        includePrefix: Bool = false,
        uppercase: Bool = false,
        minDigits: (() -> Int)?
    ) {
        self.radix = radix
        self.explicitPositiveSign = explicitPositiveSign
        self.includePrefix = includePrefix
        self.uppercase = uppercase
        self.minDigits = minDigits
    }

    /// Displays an interpolated integer as a decimal number with the specified number
    /// of digits and an optional sign.
    ///
    /// The parameter `explicitPositiveSign` must be a boolean literal. The
    /// parameter `minDigits` can be an arbitrary expression.
    ///
    /// - Parameters:
    ///   - explicitPositiveSign: Pass `true` to add a + sign to non-negative
    ///     numbers.
    ///   - minDigits: minimum number of digits to display. Numbers will be
    ///     prefixed with zeros if necessary to meet the minimum.
    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    public static func decimal(
        explicitPositiveSign: Bool = false,
        minDigits: @escaping @autoclosure () -> Int
    ) -> MyCustomIntegerFormatting {
        return MyCustomIntegerFormatting(
            radix: 10,
            explicitPositiveSign: explicitPositiveSign,
            minDigits: minDigits)
    }

    /// Displays an interpolated integer as a decimal number with an optional sign.
    ///
    /// The parameter `explicitPositiveSign` must be a boolean literal.
    ///
    /// - Parameters:
    ///   - explicitPositiveSign: Pass `true` to add a + sign to non-negative
    ///     numbers.
    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    public static func decimal(
        explicitPositiveSign: Bool = false
    ) -> MyCustomIntegerFormatting {
        return MyCustomIntegerFormatting(
            radix: 10,
            explicitPositiveSign: explicitPositiveSign,
            minDigits: nil)
    }

    /// Displays an interpolated integer as a decimal number. This is the default format for
    /// integers.
    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    public static var decimal: MyCustomIntegerFormatting { .decimal() }

    /// Displays an interpolated unsigned integer as a hexadecimal number with the
    /// specified parameters. This formatting option should be used only with unsigned
    /// integers.
    ///
    /// All parameters except `minDigits` should be boolean literals. `minDigits`
    /// can be an arbitrary expression.
    ///
    /// - Parameters:
    ///   - explicitPositiveSign: Pass `true` to add a + sign to non-negative
    ///     numbers.
    ///   - includePrefix: Pass `true` to add a prefix 0x.
    ///   - uppercase: Pass `true` to use uppercase letters to represent numerals
    ///     greater than 9, or `false` to use lowercase letters. The default is `false`.
    ///   - minDigits: minimum number of digits to display. Numbers will be
    ///     prefixed with zeros if necessary to meet the minimum.
    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    public static func hex(
        explicitPositiveSign: Bool = false,
        includePrefix: Bool = false,
        uppercase: Bool = false,
        minDigits: @escaping @autoclosure () -> Int
    ) -> MyCustomIntegerFormatting {
        return MyCustomIntegerFormatting(
            radix: 16,
            explicitPositiveSign: explicitPositiveSign,
            includePrefix: includePrefix,
            uppercase: uppercase,
            minDigits: minDigits)
    }

    /// Displays an interpolated unsigned integer as a hexadecimal number with the specified
    /// parameters. This formatting option should be used only with unsigned integers.
    ///
    /// All parameters  should be boolean literals.
    ///
    /// - Parameters:
    ///   - explicitPositiveSign: Pass `true` to add a + sign to non-negative
    ///     numbers.
    ///   - includePrefix: Pass `true` to add a prefix 0x.
    ///   - uppercase: Pass `true` to use uppercase letters to represent numerals
    ///     greater than 9, or `false` to use lowercase letters. The default is `false`.
    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    public static func hex(
        explicitPositiveSign: Bool = false,
        includePrefix: Bool = false,
        uppercase: Bool = false
    ) -> MyCustomIntegerFormatting {
        return MyCustomIntegerFormatting(
            radix: 16,
            explicitPositiveSign: explicitPositiveSign,
            includePrefix: includePrefix,
            uppercase: uppercase,
            minDigits: nil)
    }

    /// Displays an interpolated unsigned integer as a hexadecimal number.
    /// This formatting option should be used only with unsigned integers.
    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    public static var hex: MyCustomIntegerFormatting { .hex() }

    /// Displays an interpolated unsigned integer as an octal number with the specified
    /// parameters. This formatting option should be used only with unsigned
    /// integers.
    ///
    /// All parameters except `minDigits` should be boolean literals. `minDigits`
    /// can be an arbitrary expression.
    ///
    /// - Parameters:
    ///   - explicitPositiveSign: Pass `true` to add a + sign to non-negative
    ///     numbers.
    ///   - includePrefix: Pass `true` to add a prefix 0o.
    ///   - uppercase: Pass `true` to use uppercase letters to represent numerals
    ///     greater than 9, or `false` to use lowercase letters. The default is `false`.
    ///   - minDigits: minimum number of digits to display. Numbers will be
    ///     prefixed with zeros if necessary to meet the minimum.
    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    public static func octal(
        explicitPositiveSign: Bool = false,
        includePrefix: Bool = false,
        uppercase: Bool = false,
        minDigits: @autoclosure @escaping () -> Int
    ) -> MyCustomIntegerFormatting {
        MyCustomIntegerFormatting(
            radix: 8,
            explicitPositiveSign: explicitPositiveSign,
            includePrefix: includePrefix,
            uppercase: uppercase,
            minDigits: minDigits)
    }

    /// Displays an interpolated unsigned integer as an octal number with the specified parameters.
    /// This formatting option should be used only with unsigned integers.
    ///
    /// All parameters must be boolean literals.
    ///
    /// - Parameters:
    ///   - explicitPositiveSign: Pass `true` to add a + sign to non-negative
    ///     numbers.
    ///   - includePrefix: Pass `true` to add a prefix 0o.
    ///   - uppercase: Pass `true` to use uppercase letters to represent numerals
    ///     greater than 9, or `false` to use lowercase letters.
    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    public static func octal(
        explicitPositiveSign: Bool = false,
        includePrefix: Bool = false,
        uppercase: Bool = false
    ) -> MyCustomIntegerFormatting {
        MyCustomIntegerFormatting(
            radix: 8,
            explicitPositiveSign: explicitPositiveSign,
            includePrefix: includePrefix,
            uppercase: uppercase,
            minDigits: nil)
    }

    /// Displays an interpolated unsigned integer as an octal number.
    /// This formatting option should be used only with unsigned integers.
    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    public static var octal: MyCustomIntegerFormatting { .octal() }
}

extension MyCustomIntegerFormatting {
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


extension MyCustomIntegerFormatting {

    /// Returns a fprintf-compatible length modifier for a given argument type.
    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    internal static func formatSpecifierLengthModifier<I: FixedWidthInteger>(_ type: I.Type) -> String? {
        switch MemoryLayout<I>.size {
        case 1: return "hh"
        case 2: return "h"
        case 4: return ""
        case 8: return "ll"
        default: return nil
        }
    }

    /// Constructs an os_log format specifier for the given `type`.
    @_semantics("constant_evaluable")
    @_alwaysEmitIntoClient
    @_optimize(none)
    @_effects(readonly)
    internal func formatSpecifier<I: FixedWidthInteger>(
        for type: I.Type,
        attributes: String
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

        // 3. Precision

        // Default precision for integers is 1, otherwise use requested precision.
        // The precision could be a dynamic value.
        // Therefore, use a star here and pass it as an additional argument.
        if let _ = minDigits {
            specification += ".*"
        }

        // 4. Length modifier
        guard let lengthModifier =
                MyCustomIntegerFormatting.formatSpecifierLengthModifier(type) else {
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

@frozen
@usableFromInline
internal struct MyCustomArguments {
    @usableFromInline
    internal var argumentClosures: [(([Int]) -> ()) -> ()]

    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    internal init() {
        argumentClosures = []
    }

    /// `append` for other types must be implemented by extensions.
}

extension MyCustomInterpolation {

    /// Defines interpolation for expressions of type Int.
    ///
    /// Do not call this function directly. It will be called automatically when interpolating
    /// a value of type `Int` in the string interpolations passed to the log APIs.
    ///
    /// - Parameters:
    ///   - number: The interpolated expression of type Int, which is autoclosured.
    ///   - format: A formatting option available for integer types, defined by the
    ///     type: `MyCustomIntegerFormatting`. The default is `.decimal`.
    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    @_semantics("oslog.requires_constant_arguments")
    public mutating func appendInterpolation(
        _ number: @autoclosure @escaping () -> Int,
        format: MyCustomIntegerFormatting = .decimal
    ) {
        appendInteger(number, format: format)
    }

    // Define appendInterpolation overloads for fixed-size integers.

    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    @_semantics("oslog.requires_constant_arguments")
    public mutating func appendInterpolation(
        _ number: @autoclosure @escaping () -> Int8,
        format: MyCustomIntegerFormatting = .decimal
    ) {
        appendInteger(number, format: format)
    }

    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    @_semantics("oslog.requires_constant_arguments")
    public mutating func appendInterpolation(
        _ number: @autoclosure @escaping () -> Int16,
        format: MyCustomIntegerFormatting = .decimal
    ) {
        appendInteger(number, format: format)
    }

    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    @_semantics("oslog.requires_constant_arguments")
    public mutating func appendInterpolation(
        _ number: @autoclosure @escaping () -> Int32,
        format: MyCustomIntegerFormatting = .decimal
    ) {
        appendInteger(number, format: format)
    }

    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    @_semantics("oslog.requires_constant_arguments")
    public mutating func appendInterpolation(
        _ number: @autoclosure @escaping () -> Int64,
        format: MyCustomIntegerFormatting = .decimal
    ) {
        appendInteger(number, format: format)
    }

    /// Defines interpolation for expressions of type UInt.
    ///
    /// Do not call this function directly. It will be called automatically when interpolating
    /// a value of type `Int` in the string interpolations passed to the log APIs.
    ///
    /// - Parameters:
    ///   - number: The interpolated expression of type UInt, which is autoclosured.
    ///   - format: A formatting option available for integer types, defined by the
    ///     type `MyCustomIntegerFormatting`.
    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    @_semantics("oslog.requires_constant_arguments")
    public mutating func appendInterpolation(
        _ number: @autoclosure @escaping () -> UInt,
        format: MyCustomIntegerFormatting = .decimal
    ) {
        appendInteger(number, format: format)
    }

    // Define appendInterpolation overloads for unsigned integers.

    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    @_semantics("oslog.requires_constant_arguments")
    public mutating func appendInterpolation(
        _ number: @autoclosure @escaping () -> UInt8,
        format: MyCustomIntegerFormatting = .decimal
    ) {
        appendInteger(number, format: format)
    }

    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    @_semantics("oslog.requires_constant_arguments")
    public mutating func appendInterpolation(
        _ number: @autoclosure @escaping () -> UInt16,
        format: MyCustomIntegerFormatting = .decimal
    ) {
        appendInteger(number, format: format)
    }

    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    @_semantics("oslog.requires_constant_arguments")
    public mutating func appendInterpolation(
        _ number: @autoclosure @escaping () -> UInt32,
        format: MyCustomIntegerFormatting = .decimal
    ) {
        appendInteger(number, format: format)
    }

    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    @_semantics("oslog.requires_constant_arguments")
    public mutating func appendInterpolation(
        _ number: @autoclosure @escaping () -> UInt64,
        format: MyCustomIntegerFormatting = .decimal
    ) {
        appendInteger(number, format: format)
    }

    /// Defines interpolation for expressions of type Int.
    ///
    /// Do not call this function directly. It will be called automatically when interpolating
    /// a value of type `Int` in the string interpolations passed to the log APIs.
    ///
    /// - Parameters:
    ///   - number: The interpolated expression of type Int, which is autoclosured.
    ///   - format: A formatting option available for integer types, defined by the
    ///     type: `MyCustomIntegerFormatting`. The default is `.decimal`.
    ///   - attributes: A string that specifies an attribute for the interpolated value,
    ///     which can be used to provide additional information about the interpolated
    ///     value to tools such as Xcode that can process and render os_log and os_signpost
    ///     messages. An example of an attribute is "xcode:size-in-bytes". If the target tool
    ///     that processes these messages doesn't understand the attribute it would be ignored.
    @_semantics("constant_evaluable")
    @_alwaysEmitIntoClient
    @_optimize(none)
    @_semantics("oslog.requires_constant_arguments")
    public mutating func appendInterpolation<T: FixedWidthInteger>(
        _ number: @autoclosure @escaping () -> T,
        format: MyCustomIntegerFormatting = .decimal,
        attributes: String
    ) {
        appendInteger(number, format: format, attributes: attributes)
    }

    /// Given an integer, create and append a format specifier for the integer to the
    /// format string property. Also, append the integer along with necessary headers
    /// to the MyCustomArguments property.
    @_semantics("constant_evaluable")
    @_alwaysEmitIntoClient
    @_optimize(none)
    internal mutating func appendInteger<T>(
        _ number: @escaping () -> T,
        format: MyCustomIntegerFormatting,
        attributes: String = ""
    ) where T: FixedWidthInteger {
        formatString += format.formatSpecifier(for: T.self,  attributes: attributes)
    }
}

@frozen
public struct MyCustomInterpolation : StringInterpolationProtocol {
    /// A format string constructed from the given string interpolation to be
    /// passed to vprintf
    @usableFromInline
    internal var formatString: String

    @usableFromInline
    internal var arguments: MyCustomArguments

    // Some methods defined below are marked @_optimize(none) to prevent inlining
    // of string internals (such as String._StringGuts) which will interfere with
    // constant evaluation and folding. Note that these methods will be inlined,
    // constant evaluated/folded and optimized in the context of a caller.

    @_semantics("oslog.interpolation.init")
    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    public init(literalCapacity: Int, interpolationCount: Int) {
        // Since the format string and the arguments array are fully constructed
        // at compile time, the parameters are ignored.
        formatString = ""
        arguments = MyCustomArguments()
    }

    @_semantics("constant_evaluable")
    @inlinable
    @_optimize(none)
    public mutating func appendLiteral(_ literal: String) {
        formatString += literal.percentEscapedString
    }

    /// `appendInterpolation` conformances will be added by extensions to this type.
}

extension MyCustomInterpolation {
    /// Defines interpolation for expressions of type String.
    ///
    /// Do not call this function directly. It will be called automatically when interpolating
    /// a value of type `String` in the string interpolations passed to the log APIs.
    ///
    /// - Parameters:
    ///   - argumentString: The interpolated expression of type String, which is autoclosured.
    @_semantics("constant_evaluable")
    @_alwaysEmitIntoClient
    @_optimize(none)
    @_semantics("oslog.requires_constant_arguments")
    public mutating func appendInterpolation(
        _ argumentString: @autoclosure @escaping () -> String
    ) {
        formatString += "%s"
    }
}

extension MyCustomInterpolation {

    /// Defines interpolation for values conforming to CustomStringConvertible. The values
    /// are displayed using the description methods on them.
    ///
    /// Do not call this function directly. It will be called automatically when interpolating
    /// a value conforming to CustomStringConvertible in the string interpolations passed
    /// to the log APIs.
    ///
    /// - Parameters:
    ///   - value: The interpolated expression conforming to CustomStringConvertible.
    @_optimize(none)
    @_transparent
    @_semantics("oslog.requires_constant_arguments")
    public mutating func appendInterpolation<T : CustomStringConvertible>(
        _ value: @autoclosure @escaping () -> T
    ) {
        appendInterpolation(value().description)
    }
}

@frozen @_semantics("oslog.message.type")
public struct MyCustomMessage :
    ExpressibleByStringInterpolation, ExpressibleByStringLiteral
{
    public let interpolation: MyCustomInterpolation

    @inlinable
    @_optimize(none)
    @_semantics("oslog.message.init_interpolation")
    @_semantics("constant_evaluable")
    public init(stringInterpolation: MyCustomInterpolation) {
        var s = stringInterpolation
        s.appendLiteral("\n")
        self.interpolation = s
    }

    @inlinable
    @_optimize(none)
    @_semantics("oslog.message.init_stringliteral")
    @_semantics("constant_evaluable")
    public init(stringLiteral value: String) {
        var s = MyCustomInterpolation(
            literalCapacity: 1,
            interpolationCount: 0
        )
        s.appendLiteral(value)
        s.appendLiteral("\n")
        self.interpolation = s
    }
}

@_semantics("oslog.requires_constant_arguments")
@inlinable
@_transparent
@_alwaysEmitIntoClient
@_optimize(none)
public func xprint(_ message: MyCustomMessage) {
    let formatString = message.interpolation.formatString
    let formatStringPointer = _getGlobalStringTablePointer(formatString)
    print(String(cString: formatStringPointer))
}

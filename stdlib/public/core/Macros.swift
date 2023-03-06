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

#if $Macros && hasAttribute(attached)

/// Specifies the module and type name for an externally-defined macro, which
/// must conform to the appropriate set of `Macro` protocols.
///
/// This macro can only be used to define other macros. For example:
///
///    macro stringify<T>(_ value: T) -> (T, String) =
///        #externalMacro(module: "ExampleMacros", type :"StringifyMacro")
///
/// Use of this macro in any other context is an error.
@freestanding(expression)
public macro externalMacro<T>(module: String, type: String) -> T =
  Builtin.ExternalMacro

// File and path-related information

/// Produces a unique identifier for the given source file, comprised of
/// the module and file name.
@freestanding(expression)
public macro fileID<T: ExpressibleByStringLiteral>() -> T =
  Builtin.FileIDMacro

/// Produces the complete path for the given source file.
@freestanding(expression)
public macro filePath<T: ExpressibleByStringLiteral>() -> T =
  Builtin.FilePathMacro

/// Produces either the result of `#filePath` or `#file`, depending
/// on whether concise file paths (SE-0274) are enabled.
@freestanding(expression)
public macro file<T: ExpressibleByStringLiteral>() -> T =
  Builtin.FileMacro

/// Produces a string representation of the current function name.
@freestanding(expression)
public macro function<T: ExpressibleByStringLiteral>() -> T =
  Builtin.FunctionMacro

/// Produces the line number at which the macro is expanded.
@freestanding(expression)
public macro line<T: ExpressibleByIntegerLiteral>() -> T =
  Builtin.LineMacro

/// Produces the column number at which the macro is expanded.
@freestanding(expression)
public macro column<T: ExpressibleByIntegerLiteral>() -> T =
  Builtin.ColumnMacro

/// Produces the shared object handle for the macro expansion location.
@freestanding(expression)
public macro dsohandle() -> UnsafeRawPointer = Builtin.DSOHandleMacro

/// Create an option set from a struct that contains a nested `Options` enum.
///
/// Attach this macro to a struct that contains a nested `Options` enum
/// with an integer raw value. The struct will be transformed to conform to
/// `OptionSet` by
///   1. Introducing a `rawValue` stored property to track which options are set,
///    along with the necessary `RawType` typealias and initializers to satisfy
///    the `OptionSet` protocol. The raw type is specified after `@OptionSet`,
///    e.g., `@OptionSet<UInt8>`.
///   2. Introducing static properties for each of the cases within the `Options`
///    enum, of the type of the struct.
///
/// The `Options` enum must have a raw value, where its case elements
/// each indicate a different option in the resulting option set. For example,
/// the struct and its nested `Options` enum could look like this:
///
///     @OptionSet<UInt8>
///     struct ShippingOptions {
///       private enum Options: Int {
///         case nextDay
///         case secondDay
///         case priority
///         case standard
///       }
///     }
@attached(member, names: named(RawValue), named(rawValue), named(`init`), arbitrary)
@attached(conformance)
public macro OptionSet<RawType>() =
  #externalMacro(module: "SwiftMacros", type: "OptionSetMacro")

#endif

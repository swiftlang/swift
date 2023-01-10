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

#if hasAttribute(expression)
/// Specifies the module and type name for an externally-defined macro, which
/// must conform to the appropriate set of `Macro` protocols.
///
/// This macro can only be used to define other macros. For example:
///
///    macro stringify<T>(_ value: T) -> (T, String) =
///        #externalMacro(module: "ExampleMacros", type :"StringifyMacro")
///
/// Use of this macro in any other context is an error.
@expression
public macro externalMacro<T>(module: String, type: String) -> T =
    Builtin.ExternalMacro

#endif

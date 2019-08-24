//===--- Strings.h - Shared string constants across components --*- C++ -*-===//
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

#ifndef SWIFT_STRINGS_H
#define SWIFT_STRINGS_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

/// The name of the standard library, which is a reserved module name.
constexpr static const StringLiteral STDLIB_NAME = "Swift";
/// The name of the Onone support library, which is a reserved module name.
constexpr static const StringLiteral SWIFT_ONONE_SUPPORT = "SwiftOnoneSupport";
/// The name of the SwiftShims module, which contains private stdlib decls.
constexpr static const StringLiteral SWIFT_SHIMS_NAME = "SwiftShims";
/// The name of the Builtin module, which contains Builtin functions.
constexpr static const StringLiteral BUILTIN_NAME = "Builtin";
/// The prefix of module names used by LLDB to capture Swift expressions
constexpr static const StringLiteral LLDB_EXPRESSIONS_MODULE_NAME_PREFIX =
    "__lldb_expr_";

/// The name of the fake module used to hold imported Objective-C things.
constexpr static const StringLiteral MANGLING_MODULE_OBJC = "__C";
/// The name of the fake module used to hold synthesized ClangImporter things.
constexpr static const StringLiteral MANGLING_MODULE_CLANG_IMPORTER =
    "__C_Synthesized";

constexpr static const StringLiteral SEMANTICS_PROGRAMTERMINATION_POINT =
    "programtermination_point";

/// The name of the Builtin type prefix
constexpr static const StringLiteral BUILTIN_TYPE_NAME_PREFIX = "Builtin.";

/// A composition class containing a StringLiteral for the names of
/// Swift builtins. The reason we use this is to ensure that we when
/// necessary slice off the "Builtin." prefix from these names in a
/// constexpr way from a global constant string.
///
/// NOTE: StringLiteral is a weird class to compose with. For this to
/// work properly, one must always initialize these classes using an
/// initializer list as shown below.
struct BuiltinNameStringLiteral {
  const StringLiteral literal;

  constexpr operator StringRef() const { return literal; }

  StringRef getWithoutPrefix() const {
    return literal.drop_front(BUILTIN_TYPE_NAME_PREFIX.size());
  }
};

/// The name of the Builtin type for Int
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_INT = {
    "Builtin.Int"};
/// The name of the Builtin type for Int8
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_INT8 = {
    "Builtin.Int8"};
/// The name of the Builtin type for Int16
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_INT16 = {
    "Builtin.Int16"};
/// The name of the Builtin type for Int32
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_INT32 = {
    "Builtin.Int32"};
/// The name of the Builtin type for Int64
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_INT64 = {
    "Builtin.Int64"};
/// The name of the Builtin type for Int128
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_INT128 = {
    "Builtin.Int128"};
/// The name of the Builtin type for Int256
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_INT256 = {
    "Builtin.Int256"};
/// The name of the Builtin type for Int512
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_INT512 = {
    "Builtin.Int512"};
/// The name of the Builtin type for IntLiteral
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_INTLITERAL = {
    "Builtin.IntLiteral"};
/// The name of the Builtin type for IEEE Floating point types.
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_FLOAT = {
    "Builtin.FPIEEE"};
// The name of the builtin type for power pc specific floating point types.
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_FLOAT_PPC = {
    "Builtin.FPPPC"};
/// The name of the Builtin type for NativeObject
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_NATIVEOBJECT = {
    "Builtin.NativeObject"};
/// The name of the Builtin type for BridgeObject
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_BRIDGEOBJECT = {
    "Builtin.BridgeObject"};
/// The name of the Builtin type for RawPointer
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_RAWPOINTER = {
    "Builtin.RawPointer"};
/// The name of the Builtin type for UnsafeValueBuffer
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_UNSAFEVALUEBUFFER =
    {"Builtin.UnsafeValueBuffer"};
/// The name of the Builtin type for UnknownObject
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_UNKNOWNOBJECT = {
    "Builtin.UnknownObject"};
/// The name of the Builtin type for Vector
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_VEC = {
    "Builtin.Vec"};
/// The name of the Builtin type for SILToken
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_SILTOKEN = {
    "Builtin.SILToken"};
/// The name of the Builtin type for Word
constexpr static BuiltinNameStringLiteral BUILTIN_TYPE_NAME_WORD = {
    "Builtin.Word"};

} // end namespace swift

#endif // SWIFT_STRINGS_H

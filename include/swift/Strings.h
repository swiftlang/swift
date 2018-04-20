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

/// The extension for serialized modules.
constexpr static const char SERIALIZED_MODULE_EXTENSION[] = "swiftmodule";
/// The extension for serialized documentation comments.
constexpr static const char SERIALIZED_MODULE_DOC_EXTENSION[] = "swiftdoc";
/// The extension for PCH files.
constexpr static const char PCH_EXTENSION[] = "pch";
/// The extension for replacement map files.
constexpr static const char REMAP_EXTENSION[] = "remap";
/// The extension for SIL files.
constexpr static const char SIL_EXTENSION[] = "sil";
/// The extension for SIB files.
constexpr static const char SIB_EXTENSION[] = "sib";
/// The extension for LLVM IR files.
constexpr static const char LLVM_BC_EXTENSION[] = "bc";
constexpr static const char LLVM_IR_EXTENSION[] = "ll";
/// The name of the standard library, which is a reserved module name.
constexpr static const char STDLIB_NAME[] = "Swift";
/// The name of the Onone support library, which is a reserved module name.
constexpr static const char SWIFT_ONONE_SUPPORT[] = "SwiftOnoneSupport";
/// The name of the SwiftShims module, which contains private stdlib decls.
constexpr static const char SWIFT_SHIMS_NAME[] = "SwiftShims";
/// The name of the Builtin module, which contains Builtin functions.
constexpr static const char BUILTIN_NAME[] = "Builtin";
/// The prefix of module names used by LLDB to capture Swift expressions
constexpr static const char LLDB_EXPRESSIONS_MODULE_NAME_PREFIX[] =
    "__lldb_expr_";

/// The name of the fake module used to hold imported Objective-C things.
constexpr static const char MANGLING_MODULE_OBJC[] = "__C";
/// The name of the fake module used to hold synthesized ClangImporter things.
constexpr static const char MANGLING_MODULE_CLANG_IMPORTER[] =
    "__C_Synthesized";

/// The name of the Builtin type prefix
constexpr static const char BUILTIN_TYPE_NAME_PREFIX[] = "Builtin.";
/// The name of the Builtin type for Int
constexpr static const char BUILTIN_TYPE_NAME_INT[] = "Builtin.Int";
/// The name of the Builtin type for Int8
constexpr static const char BUILTIN_TYPE_NAME_INT8[] = "Builtin.Int8";
/// The name of the Builtin type for Int16
constexpr static const char BUILTIN_TYPE_NAME_INT16[] = "Builtin.Int16";
/// The name of the Builtin type for Int32
constexpr static const char BUILTIN_TYPE_NAME_INT32[] = "Builtin.Int32";
/// The name of the Builtin type for Int64
constexpr static const char BUILTIN_TYPE_NAME_INT64[] = "Builtin.Int64";
/// The name of the Builtin type for Int128
constexpr static const char BUILTIN_TYPE_NAME_INT128[] = "Builtin.Int128";
/// The name of the Builtin type for Int256
constexpr static const char BUILTIN_TYPE_NAME_INT256[] = "Builtin.Int256";
/// The name of the Builtin type for Int512
constexpr static const char BUILTIN_TYPE_NAME_INT512[] = "Builtin.Int512";
/// The name of the Builtin type for Float
constexpr static const char BUILTIN_TYPE_NAME_FLOAT[] = "Builtin.Float";
/// The name of the Builtin type for NativeObject
constexpr static const char BUILTIN_TYPE_NAME_NATIVEOBJECT[] =
    "Builtin.NativeObject";
/// The name of the Builtin type for BridgeObject
constexpr static const char BUILTIN_TYPE_NAME_BRIDGEOBJECT[] =
    "Builtin.BridgeObject";
/// The name of the Builtin type for RawPointer
constexpr static const char BUILTIN_TYPE_NAME_RAWPOINTER[] =
    "Builtin.RawPointer";
/// The name of the Builtin type for UnsafeValueBuffer
constexpr static const char BUILTIN_TYPE_NAME_UNSAFEVALUEBUFFER[] =
    "Builtin.UnsafeValueBuffer";
/// The name of the Builtin type for UnknownObject
constexpr static const char BUILTIN_TYPE_NAME_UNKNOWNOBJECT[] =
    "Builtin.UnknownObject";
/// The name of the Builtin type for Vector
constexpr static const char BUILTIN_TYPE_NAME_VEC[] = "Builtin.Vec";
/// The name of the Builtin type for SILToken
constexpr static const char BUILTIN_TYPE_NAME_SILTOKEN[] = "Builtin.SILToken";
/// The name of the Builtin type for Word
constexpr static const char BUILTIN_TYPE_NAME_WORD[] = "Builtin.Word";

constexpr static StringLiteral OPTIMIZE_SIL_PRESERVE_EXCLUSIVITY =
    "optimize.sil.preserve_exclusivity";

} // end namespace swift

#endif // SWIFT_STRINGS_H

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

namespace swift {
  /// The extension for serialized modules.
  static const char SERIALIZED_MODULE_EXTENSION[] = "swiftmodule";
  /// The extension for serialized documentation comments.
  static const char SERIALIZED_MODULE_DOC_EXTENSION[] = "swiftdoc";
  /// The extension for PCH files.
  static const char PCH_EXTENSION[] = "pch";
  /// The extension for replacement map files.
  static const char REMAP_EXTENSION[] = "remap";
  /// The extension for SIL files.
  static const char SIL_EXTENSION[] = "sil";
  /// The extension for SIB files.
  static const char SIB_EXTENSION[] = "sib";
  /// The extension for LLVM IR files.
  static const char LLVM_BC_EXTENSION[] = "bc";
  static const char LLVM_IR_EXTENSION[] = "ll";
  /// The name of the standard library, which is a reserved module name.
  static const char STDLIB_NAME[] = "Swift";
  /// The name of the Onone support library, which is a reserved module name.
  static const char SWIFT_ONONE_SUPPORT[] = "SwiftOnoneSupport";
  /// The name of the SwiftShims module, which contains private stdlib decls.
  static const char SWIFT_SHIMS_NAME[] = "SwiftShims";
  /// The name of the Builtin module, which contains Builtin functions.
  static const char BUILTIN_NAME[] = "Builtin";
  /// The prefix of module names used by LLDB to capture Swift expressions
  static const char LLDB_EXPRESSIONS_MODULE_NAME_PREFIX[] = "__lldb_expr_";

  /// The name of the fake module used to hold imported Objective-C things.
  static const char MANGLING_MODULE_OBJC[] = "__C";
  /// The name of the fake module used to hold synthesized ClangImporter things.
  static const char MANGLING_MODULE_CLANG_IMPORTER[] = "__C_Synthesized";

  /// The name of the Builtin type prefix
  static const char BUILTIN_TYPE_NAME_PREFIX[] = "Builtin.";
  /// The name of the Builtin type for Int
  static const char BUILTIN_TYPE_NAME_INT[] = "Builtin.Int";
  /// The name of the Builtin type for Int8
  static const char BUILTIN_TYPE_NAME_INT8[] = "Builtin.Int8";
  /// The name of the Builtin type for Int16
  static const char BUILTIN_TYPE_NAME_INT16[] = "Builtin.Int16";
  /// The name of the Builtin type for Int32
  static const char BUILTIN_TYPE_NAME_INT32[] = "Builtin.Int32";
  /// The name of the Builtin type for Int64
  static const char BUILTIN_TYPE_NAME_INT64[] = "Builtin.Int64";
  /// The name of the Builtin type for Int128
  static const char BUILTIN_TYPE_NAME_INT128[] = "Builtin.Int128";
  /// The name of the Builtin type for Int256
  static const char BUILTIN_TYPE_NAME_INT256[] = "Builtin.Int256";
  /// The name of the Builtin type for Int512
  static const char BUILTIN_TYPE_NAME_INT512[] = "Builtin.Int512";
  /// The name of the Builtin type for Float
  static const char BUILTIN_TYPE_NAME_FLOAT[] = "Builtin.Float";
  /// The name of the Builtin type for NativeObject
  static const char BUILTIN_TYPE_NAME_NATIVEOBJECT[] = "Builtin.NativeObject";
  /// The name of the Builtin type for BridgeObject
  static const char BUILTIN_TYPE_NAME_BRIDGEOBJECT[] = "Builtin.BridgeObject";
  /// The name of the Builtin type for RawPointer
  static const char BUILTIN_TYPE_NAME_RAWPOINTER[] = "Builtin.RawPointer";
  /// The name of the Builtin type for UnsafeValueBuffer
  static const char BUILTIN_TYPE_NAME_UNSAFEVALUEBUFFER[] = "Builtin.UnsafeValueBuffer";
  /// The name of the Builtin type for UnknownObject
  static const char BUILTIN_TYPE_NAME_UNKNOWNOBJECT[] = "Builtin.UnknownObject";
  /// The name of the Builtin type for Vector
  static const char BUILTIN_TYPE_NAME_VEC[] = "Builtin.Vec";
  /// The name of the Builtin type for SILToken
  static const char BUILTIN_TYPE_NAME_SILTOKEN[] = "Builtin.SILToken";
  /// The name of the Builtin type for Word
  static const char BUILTIN_TYPE_NAME_WORD[] = "Builtin.Word";
} // end namespace swift

#endif // SWIFT_STRINGS_H

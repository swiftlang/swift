//===--- CompilerPlugin.h - Compiler Plugin Support -------------*- C++ -*-===//
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
//
//  This file defines supporting data structures for compiler plugins.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Type.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/StringExtras.h"

#ifndef SWIFT_AST_COMPILER_PLUGIN_H
#define SWIFT_AST_COMPILER_PLUGIN_H

namespace swift {

class ASTContext;

/// A compiler plugin that corresponds to a dynamically loaded Swift type that
/// conforms to the `_CompilerPluginSupport._CompilerPlugin` protocol.
class CompilerPlugin {
  friend class ASTContext;

public:
  // Must be modified together with `_CompilerPluginKind` in
  // stdlib/toolchain/CompilerPluginSupport.swift.
  enum class Kind: uint32_t {
    ExpressionMacro,
  };

private:
  // Must be modified together with `_CompilerPlugin` in
  // stdlib/toolchain/CompilerPluginSupport.swift.
  enum class WitnessTableEntry: unsigned {
    ConformanceDescriptor = 0,
    // static func _name() -> (UnsafePointer<UInt8>, count: Int8)
    Name = 1,
    // static func _kind() -> _CompilerPluginKind
    Kind = 2,
    // static func _rewrite(...) -> (UnsafePointer<UInt8>?, count: Int)
    Rewrite = 3,
    // static func _genericSignature(...) -> (UnsafePointer<UInt8>?, count: Int)
    GenericSignature = 4,
    // static func _typeSignature(...) -> (UnsafePointer<UInt8>, count: Int)
    TypeSignature = 5,
    // static func _owningModule(...) -> (UnsafePointer<UInt8>, count: Int)
    OwningModule = 6,
    // static func _supplementalSignatureModules(...)
    //     -> (UnsafePointer<UInt8>, count: Int)
    SupplementalSignatureModules = 7,
  };

  /// The plugin type metadata.
  const void *metadata;
  /// The parent dynamic library containing the plugin.
  void *parentLibrary;
  /// The witness table proving that the plugin conforms to `_CompilerPlugin`.
  const void *witnessTable;
  /// The plugin name, aka. result of the `_name()` method.
  StringRef name;
  /// The plugin's kind, aka. result of the `_kind()` method.
  Kind kind;

  template<typename Func>
  const Func *getWitnessMethodUnsafe(WitnessTableEntry entry) const {
    return reinterpret_cast<const Func *const *>(witnessTable)[(unsigned)entry];
  }

protected:
  CompilerPlugin(const void *metadata, void *parentLibrary, ASTContext &ctx);

private:
  /// Invoke the `_name` method. The caller assumes ownership of the result
  /// string buffer.
  StringRef invokeName() const;

  /// Invoke the `_kind` method.
  Kind invokeKind() const;

public:
  ~CompilerPlugin();
  CompilerPlugin(const CompilerPlugin &) = delete;
  CompilerPlugin(CompilerPlugin &&) = default;

  /// Invoke the `_rewrite` method. The caller assumes ownership of the result
  /// string buffer.
  Optional<NullTerminatedStringRef> invokeRewrite(
      StringRef targetModuleName, StringRef filePath, StringRef sourceFileText,
      CharSourceRange range,  ASTContext &ctx) const;

  /// Invoke the `_genericSignature` method. The caller assumes ownership of the
  /// result string buffer.
  Optional<StringRef> invokeGenericSignature() const;

  /// Invoke the `_typeSignature` method. The caller assumes ownership of the
  /// result string buffer.
  StringRef invokeTypeSignature() const;

  /// Invoke the `_owningModule` method. The caller assumes ownership of the
  /// result string buffer.
  StringRef invokeOwningModule() const;

  /// Invoke the `_supplementalSignatureModules` method. The caller assumes
  /// ownership of the result string buffer.
  StringRef invokeSupplementalSignatureModules() const;

  StringRef getName() const {
    return name;
  }

  Kind getKind() const {
    return kind;
  }
};

} // end namespace swift

#endif // SWIFT_AST_COMPILER_PLUGIN_H

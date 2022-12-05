//===--- MacroDefinition.h - Swift Macro Definition -------------*- C++ -*-===//
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
//
// This provides the definition of a macro, which gives access to its
// implementation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_MACRO_DEFINITION_H
#define SWIFT_AST_MACRO_DEFINITION_H

#include "llvm/ADT/PointerUnion.h"

namespace swift {

class CompilerPlugin;

/// Provides the definition of a macro.
struct MacroDefinition {
    /// The kind of macro, which determines how it can be used in source code.
  enum Kind: uint8_t {
      /// An expression macro.
    Expression,
  };

    /// Describes how the macro is implemented.
  enum class ImplementationKind: uint8_t {
      /// The macro is built-in to the compiler, linked against the same
      /// underlying syntax tree libraries.
    Builtin,

      /// The macro was defined in a compiler plugin.
    Plugin,
  };

public:
  Kind kind;
  ImplementationKind implKind;

private:
  void *opaqueHandle;

  MacroDefinition(Kind kind, ImplementationKind implKind, void *opaqueHandle)
    : kind(kind), implKind(implKind), opaqueHandle(opaqueHandle) { }

public:
  static MacroDefinition forInvalid() {
    return MacroDefinition{
      Kind::Expression, ImplementationKind::Builtin, nullptr
    };
  }

  static MacroDefinition forBuiltin(Kind kind, void *opaqueHandle) {
    return MacroDefinition{kind, ImplementationKind::Builtin, opaqueHandle};
  }

  static MacroDefinition forCompilerPlugin(Kind kind, CompilerPlugin *plugin) {
    return MacroDefinition{kind, ImplementationKind::Plugin, plugin};
  }

  bool isInvalid() const { return opaqueHandle == nullptr; }

  explicit operator bool() const { return !isInvalid(); }

  void *getAsBuiltin() const {
    switch (implKind) {
    case ImplementationKind::Builtin:
      return opaqueHandle;

    case ImplementationKind::Plugin:
      return nullptr;
    }
  }

  CompilerPlugin *getAsCompilerPlugin() const {
    switch (implKind) {
    case ImplementationKind::Builtin:
      return nullptr;

    case ImplementationKind::Plugin:
      return (CompilerPlugin *)opaqueHandle;
    }
  }
};

}

#endif // SWIFT_AST_MACRO_DEFINITION_H

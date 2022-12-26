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

/// Provides the definition of a macro.
class MacroDefinition {
public:
  /// The kind of macro, which determines how it can be used in source code.
  enum Kind: uint8_t {
    /// An expression macro.
    Expression,
  };

  /// Describes a missing macro definition.
  struct MissingDefinition {
    Identifier externalModuleName;
    Identifier externalMacroTypeName;
  };

    /// Describes how the macro is implemented.
  enum class ImplementationKind: uint8_t {
    /// The macro has no definition.
    Undefined,

    /// The macro has a definition, but it could not be found.
    Missing,

    /// The macro is in the same process as the compiler, whether built-in or
    /// loaded via a compiler plugin.
    InProcess,
  };

  Kind kind;
  ImplementationKind implKind;

private:
  void *opaqueHandle;

  MacroDefinition(Kind kind, ImplementationKind implKind, void *opaqueHandle)
    : kind(kind), implKind(implKind), opaqueHandle(opaqueHandle) { }

public:
  static MacroDefinition forUndefined() {
    return MacroDefinition{
      Kind::Expression, ImplementationKind::Undefined, nullptr
    };
  }

  static MacroDefinition forMissing(
      ASTContext &ctx,
      Identifier externalModuleName,
      Identifier externalMacroTypeName
  );

  static MacroDefinition forInProcess(Kind kind, void *opaqueHandle) {
    return MacroDefinition{kind, ImplementationKind::InProcess, opaqueHandle};
  }

  /// Return the opaque handle for an in-process macro definition.
  void *getInProcessOpaqueHandle() const {
    assert(implKind == ImplementationKind::InProcess);
    return opaqueHandle;
  }

  /// Return more information about a missing macro definition.
  MissingDefinition *getMissingDefinition() const {
    assert(implKind == ImplementationKind::Missing);
    return static_cast<MissingDefinition *>(opaqueHandle);
  }
};

}

#endif // SWIFT_AST_MACRO_DEFINITION_H

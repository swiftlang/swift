//===--- Macro.h - Swift Macro Definition -----------------------*- C++ -*-===//
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
// This file defines the `Macro` type that describes a macro definition.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_MACRO_H
#define SWIFT_AST_MACRO_H

#include "swift/AST/ASTAllocated.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Type.h"

namespace swift {
  class ModuleDecl;

  class Macro : public ASTAllocated<Macro> {
  public:
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

    /// The kind of macro.
    const Kind kind;

    /// How the macro is implemented.
    const ImplementationKind implementationKind;
    
    /// The name of the macro in the source, e.g., "stringify".
    const Identifier name;

    /// The generic signature, used to describe the signature of macros that
    /// involve generic parameters.
    const GenericSignature genericSignature;

    /// The type signature of the macro.
    const Type signature;

    /// The module with which this macro is associated.
    ModuleDecl * const owningModule;

    /// Supplemental modules that should be imported when 
    const ArrayRef<ModuleDecl *> supplementalSignatureModules;

    /// An opaque handle to the representation of the macro.
    void * const opaqueHandle;

  public:
    Macro(
      Kind kind, ImplementationKind implementationKind, Identifier name,
      GenericSignature genericSignature, Type signature,
      ModuleDecl *owningModule,
      ArrayRef<ModuleDecl *> supplementalSignatureModules,
      void *opaqueHandle
    ) : kind(kind), implementationKind(implementationKind), name(name),
        genericSignature(genericSignature), signature(signature),
        owningModule(owningModule),
        supplementalSignatureModules(supplementalSignatureModules),
        opaqueHandle(opaqueHandle) { }
  };
}

#endif // SWIFT_AST_MACRO_H

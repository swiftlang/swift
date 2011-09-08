//===--- Module.h - Swift Language Module ASTs ------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Module class and its subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_MODULE_H
#define SWIFT_MODULE_H

#include "swift/AST/DeclContext.h"
#include "llvm/ADT/ArrayRef.h"

namespace swift {
  class ASTContext;
  class BraceStmt;
  class OneOfElementDecl;
  class NameAliasType;
  class TypeAliasDecl;
  
enum class ModuleKind {
  // The indentation of the members of this enum describe the inheritance
  // hierarchy.  Commented out members are abstract classes.  This formation
  // allows for range checks in classof.
  Module,
    TranslationUnit
};
  
/// Module - A unit of modularity.  The current translation unit is a
/// module, as is an imported module.
class Module : public DeclContext {
public:
  ASTContext &Ctx;
  ModuleKind Kind;

protected:
  Module(ModuleKind Kind, ASTContext &Ctx)
    : DeclContext(DeclContextKind::Module, nullptr),
      Ctx(Ctx), Kind(Kind) {
  }

public:
  Module(ASTContext &Ctx)
    : DeclContext(DeclContextKind::Module, nullptr),
      Ctx(Ctx), Kind(ModuleKind::Module) {
  }

  static bool classof(const Module *M) {
    return true;
  }
  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::Module;
  }

private:
  // Make placement new and vanilla new/delete illegal for DeclVarNames.
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;
public:
  // Only allow allocation of Modules using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8) throw();
};
  
/// TranslationUnit - This contains information about all of the decls and
/// external references in a translation unit, which is one file.
class TranslationUnit : public Module {
public:
  /// Body - This is a synthesized BraceStmt that holds the top level
  /// expressions and declarations for a translation unit.
  BraceStmt *Body;
  
  /// UnresolvedTypes - This is a list of types that were unresolved at the end
  /// of the translation unit's parse phase.
  ArrayRef<TypeAliasDecl*> UnresolvedTypesForParser;

  /// UnresolvedScopedTypes - This is a list of scope-qualified types
  /// that were unresolved at the end of the translation unit's parse
  /// phase.
  ArrayRef<std::pair<TypeAliasDecl*,TypeAliasDecl*> >
    UnresolvedScopedTypesForParser;
  
  TranslationUnit(ASTContext &C)
    : Module(ModuleKind::TranslationUnit, C) {
  }

  void dump() const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Module *M) {
    return M->Kind == ModuleKind::TranslationUnit;
  }
  static bool classof(const TranslationUnit *TU) { return true; }
  static bool classof(const DeclContext *DC) {
    if (const Module *M = dyn_cast<Module>(DC))
      return isa<TranslationUnit>(M);
    return false;
  }
};
  
} // end namespace swift

#endif

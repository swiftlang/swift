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
#include "swift/AST/Identifier.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"

namespace swift {
  class ASTContext;
  class BraceStmt;
  class OneOfElementDecl;
  class NameAliasType;
  class TypeAliasDecl;
  class LookupCache;
  class ValueDecl;
  
  /// NLKind - This is a specifier for the kind of name lookup being performed
  /// by various query methods.
  enum class NLKind {
    UnqualifiedLookup,
    QualifiedLookup,
    DotLookup
  };
 
/// Module - A unit of modularity.  The current translation unit is a
/// module, as is an imported module.
class Module : public DeclContext {
  void *LookupCachePimpl;
public:
  ASTContext &Ctx;
  Identifier Name;
protected:
  Module(DeclContextKind Kind, Identifier Name, ASTContext &Ctx)
    : DeclContext(Kind, nullptr), LookupCachePimpl(0), Ctx(Ctx), Name(Name) {
  }

public:
  typedef ArrayRef<std::pair<Identifier, SourceLoc>> AccessPathTy;

  /// lookupType - Look up a type at top-level scope (but with the specified 
  /// access path, which may come from an import decl) within the current
  /// module. This does a simple local lookup, not recursively looking  through
  /// imports.  
  TypeAliasDecl *lookupType(AccessPathTy AccessPath, Identifier Name,
                            NLKind LookupKind);
  
  /// lookupValue - Look up a (possibly overloaded) value set at top-level scope
  /// (but with the specified access path, which may come from an import decl)
  /// within the current module. This does a simple local lookup, not
  /// recursively looking through imports.  
  void lookupValue(AccessPathTy AccessPath, Identifier Name, NLKind LookupKind, 
                   SmallVectorImpl<ValueDecl*> &Result);


  /// lookupGlobalValue - Perform a value lookup within the current Module.
  /// Unlike lookupValue, this does look through import declarations to resolve
  /// the name.
  void lookupGlobalValue(Identifier Name, NLKind LookupKind, 
                         SmallVectorImpl<ValueDecl*> &Result);
  
  static bool classof(const Module *M) {
    return true;
  }
  static bool classof(const DeclContext *DC) {
    return DC->isModuleContext();
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
  
  /// ImportedModules - This is the list of modules that are imported by this
  /// module.  This is filled in by the Name Binding phase.
  typedef std::pair<Module::AccessPathTy, Module*> ImportedModule;
  ArrayRef<ImportedModule> ImportedModules;
  
  TranslationUnit(Identifier Name, ASTContext &C)
    : Module(DeclContextKind::TranslationUnit, Name, C) {
  }

  void dump() const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TranslationUnit *TU) { return true; }
  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::TranslationUnit;
  }
};

  
/// BuiltinModule - This module represents the compiler's implicitly generated
/// declarations in the builtin module.
class BuiltinModule : public Module {
public:
  BuiltinModule(Identifier Name, ASTContext &Ctx)
    : Module(DeclContextKind::BuiltinModule, Name, Ctx) {
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const BuiltinModule *TU) { return true; }
  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::BuiltinModule;
  }
};
  
} // end namespace swift

#endif

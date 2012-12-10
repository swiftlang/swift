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

namespace clang {
  class Module;
}

namespace swift {
  class ASTContext;
  class BraceStmt;
  class Component;
  class Decl;
  class ExtensionDecl;
  class OneOfElementDecl;
  class NameAliasType;
  struct PrintOptions;
  class TupleType;
  class Type;
  class TypeAliasDecl;
  class LookupCache;
  class ValueDecl;
  class IdentifierType;
  
  /// NLKind - This is a specifier for the kind of name lookup being performed
  /// by various query methods.
  enum class NLKind {
    UnqualifiedLookup,
    QualifiedLookup
  };
 
/// Module - A unit of modularity.  The current translation unit is a
/// module, as is an imported module.
class Module : public DeclContext {
protected:
  void *LookupCachePimpl;
  void *ExtensionCachePimpl;
  Component *Comp;
public:
  ASTContext &Ctx;
  Identifier Name;
  
  //===--------------------------------------------------------------------===//
  // AST Phase of Translation
  //===--------------------------------------------------------------------===//
  
  /// ASTStage - Defines what phases of parsing and semantic analysis are
  /// complete for the given AST.  This should only be used for assertions and
  /// verification purposes.
  enum {
    /// Parsing is underway.
    Parsing,
    /// Parsing has completed.
    Parsed,
    /// Name binding has completed.
    NameBound,
    /// Type checking has completed.
    TypeChecked
  } ASTStage;

protected:
  Module(DeclContextKind Kind, Identifier Name, Component *C, ASTContext &Ctx)
  : DeclContext(Kind, nullptr), LookupCachePimpl(0), ExtensionCachePimpl(0),
    Comp(C), Ctx(Ctx), Name(Name), ASTStage(Parsing) {
    assert(Comp != nullptr || Kind == DeclContextKind::BuiltinModule);
  }

public:
  typedef ArrayRef<std::pair<Identifier, SourceLoc>> AccessPathTy;

  Component *getComponent() const {
    assert(Comp && "fetching component for the builtin module");
    return Comp;
  }
  
  /// lookupValue - Look up a (possibly overloaded) value set at top-level scope
  /// (but with the specified access path, which may come from an import decl)
  /// within the current module. This does a simple local lookup, not
  /// recursively looking through imports.  
  void lookupValue(AccessPathTy AccessPath, Identifier Name, NLKind LookupKind, 
                   SmallVectorImpl<ValueDecl*> &Result);

  /// lookupExtensions - Look up all of the extensions in the module that are
  /// extending the specified type and return a list of them.
  ArrayRef<ExtensionDecl*> lookupExtensions(Type T);

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
                     unsigned Alignment = 8);
};
  
/// TranslationUnit - This contains information about all of the decls and
/// external references in a translation unit, which is one file.
class TranslationUnit : public Module {
public:
  typedef std::pair<Module::AccessPathTy, Module*> ImportedModule;
  typedef std::pair<IdentifierType*, DeclContext*> IdentTypeAndContext;
  typedef std::pair<TupleType*, DeclContext*> TupleTypeAndContext;
private:
  /// UnresolvedIdentifierTypes - This is a list of scope-qualified dotted types
  /// that were unresolved at the end of the translation unit's parse
  /// phase.
  ArrayRef<IdentTypeAndContext> UnresolvedIdentifierTypes;

  /// UnresolvedIdentifierTypes - This is a list of tuples containing a default
  /// value expression, along with the context containing the type with the
  /// expression.
  ArrayRef<TupleTypeAndContext> TypesWithDefaultValues;

  /// ImportedModules - This is the list of modules that are imported by this
  /// module.  This is filled in by the Name Binding phase.
  ArrayRef<ImportedModule> ImportedModules;

public:
  enum {
    Library,
    Main,
    Repl,
  } Kind;

  /// Decls; the list of top-level declarations for a translation unit.
  std::vector<Decl*> Decls;

  TranslationUnit(Identifier Name, Component *Comp, ASTContext &C,
                  bool IsMainModule, bool IsReplModule)
    : Module(DeclContextKind::TranslationUnit, Name, Comp, C) {
    if (IsReplModule)
      Kind = Repl;
    else if (IsMainModule)
      Kind = Main;
    else
      Kind = Library;
  }
  
  /// getUnresolvedIdentifierTypes - This is a list of scope-qualified types
  /// that were unresolved at the end of the translation unit's parse
  /// phase.
  ArrayRef<IdentTypeAndContext> getUnresolvedIdentifierTypes() const {
    assert(ASTStage >= Parsed);
    return UnresolvedIdentifierTypes;
  }
  void setUnresolvedIdentifierTypes(ArrayRef<IdentTypeAndContext> T) {
    assert(ASTStage == Parsing);
    UnresolvedIdentifierTypes = T;
  }
  void clearUnresolvedIdentifierTypes() {
    UnresolvedIdentifierTypes = ArrayRef<IdentTypeAndContext>();
  }

  ArrayRef<TupleTypeAndContext> getTypesWithDefaultValues() const {
    assert(ASTStage == NameBound);
    return TypesWithDefaultValues;
  }
  void setTypesWithDefaultValues(ArrayRef<TupleTypeAndContext> T) {
    assert(ASTStage == Parsing);
    TypesWithDefaultValues = T;
  }
  void clearTypesWithDefaultValues() {
    TypesWithDefaultValues = ArrayRef<TupleTypeAndContext>();
  }

  /// ImportedModules - This is the list of modules that are imported by this
  /// module.  This is filled in as the first thing that the Name Binding phase
  /// does.
  ArrayRef<ImportedModule> getImportedModules() const {
    assert(ASTStage >= Parsed);
    return ImportedModules;
  }
  void setImportedModules(ArrayRef<ImportedModule> IM) {
    assert(ASTStage == Parsed);
    ImportedModules = IM;
  }

  void clearLookupCache();

  void dump() const;

  /// \brief Pretty-print the entire contents of this translation unit.
  ///
  /// \param os The stream to which the contents will be printed.
  void print(raw_ostream &os);

  /// \brief Pretty-print the contents of this translation unit.
  ///
  /// \param os The stream to which the contents will be printed.
  ///
  /// \param options Options controlling the printing process.
  void print(raw_ostream &os, const PrintOptions &options);

  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::TranslationUnit;
  }
};

  
/// BuiltinModule - This module represents the compiler's implicitly generated
/// declarations in the builtin module.
class BuiltinModule : public Module {
public:
  BuiltinModule(Identifier Name, ASTContext &Ctx)
    : Module(DeclContextKind::BuiltinModule, Name, nullptr, Ctx) {
    // The Builtin module is always well formed.
    ASTStage = TypeChecked;
  }

  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::BuiltinModule;
  }
};

/// \brief Describes an externally-synthesized definition.
///
/// Externally synthesized definitions are generated when the Clang module
/// importer generates a Swift stub definition to provide a better Swift
/// interface for a C/Objective-C/C++ construct, such a Swift constructor
/// implemented on top of Objective-C alloc/init.
class ExternalDefinition {
public:
  /// \brief Describes the stage to which this external definition has been
  /// processed.
  ///
  /// External definitions always start in the 'name-bound' stage.
  enum Stage {
    /// \brief All names in the definition have been bound.
    NameBound,
    /// \brief The definition has been type-checked.
    TypeChecked,
    /// \brief IR for the definition has been generated.
    IRGenerated
  };

private:
  llvm::PointerIntPair<Decl *, 2, Stage> Data;

public:
  ExternalDefinition(Decl *decl, Stage stage = NameBound) : Data(decl, stage) {}

  Decl *getDecl() const { return Data.getPointer(); }
  
  Stage getStage() const { return Data.getInt(); }
  void setStage(Stage stage) { Data.setInt(stage); }
};

/// \brief Represents a Clang module that has been imported into Swift.
class ClangModule : public Module {
  clang::Module *clangModule;

  /// \brief The list of definitions that were synthesized while importing
  /// from the Clang module.
  ///
  /// Various external definitions can be synthesized by the Clang module
  /// importer, such as a constructor in an imported Objective-C class, which
  /// actually invokes the Objective-C alloc/init or new.
  ///
  /// FIXME: Make sure this gets freed.
  std::vector<ExternalDefinition> ExternalDefs;

public:
  ClangModule(ASTContext &ctx, Component *comp, clang::Module *clangModule);

  /// \brief Retrieve the underlying Clang module.
  clang::Module *getClangModule() const { return clangModule; }

  /// \brief Add the given external definition to this module.
  void addExternalDefinition(Decl *def) {
    ExternalDefs.push_back(def);
  }

  /// \brief Retrieve the list of definitions that were synthesized while
  /// importing from the Clang module.
  ///
  /// Various external definitions can be synthesized by the Clang module
  /// importer, such as a constructor in an imported Objective-C class, which
  /// actually invokes the Objective-C alloc/init or new.
  MutableArrayRef<ExternalDefinition> getExternalDefinitions() {
    return ExternalDefs;
  }

  /// \brief Retrieve the list of definitions that were synthesized while
  /// importing from the Clang module.
  ///
  /// Various external definitions can be synthesized by the Clang module
  /// importer, such as a constructor in an imported Objective-C class, which
  /// actually invokes the Objective-C alloc/init or new.
  ArrayRef<ExternalDefinition> getExternalDefinitions() const {
    return ExternalDefs;
  }

  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::ClangModule;
  }
};


} // end namespace swift

#endif

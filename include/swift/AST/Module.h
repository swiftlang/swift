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
#include "swift/AST/Type.h"
#include "swift/Basic/Optional.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/StringMap.h"

namespace clang {
  class Module;
}

namespace swift {
  class ASTContext;
  class BraceStmt;
  class Component;
  class Decl;
  enum class DeclKind : uint8_t;
  class ExtensionDecl;
  class IdentifierType;
  class InfixOperatorDecl;
  class LookupCache;
  class ModuleLoader;
  class NameAliasType;
  class OneOfElementDecl;
  class OperatorDecl;
  class PostfixOperatorDecl;
  class PrefixOperatorDecl;
  struct PrintOptions;
  class TupleType;
  class Type;
  class TypeAliasDecl;
  class ValueDecl;
  class VisibleDeclConsumer;
  
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
  enum ASTStage_t {
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
  : DeclContext(Kind, nullptr), LookupCachePimpl(0),
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
  
  /// lookupVisibleDecls - Find ValueDecls in the module and pass them to the
  /// given consumer object.
  void lookupVisibleDecls(AccessPathTy AccessPath,
                          VisibleDeclConsumer &Consumer,
                          NLKind LookupKind);

  /// Look up an InfixOperatorDecl for the given operator
  /// name in this module (which must be NameBound) and return it, or return
  /// null if there is no operator decl. Returns Nothing if there was an error
  /// resolving the operator name (such as if there were conflicting importing
  /// operator declarations).
  Optional<InfixOperatorDecl *> lookupInfixOperator(Identifier name,
                                              SourceLoc diagLoc = SourceLoc());
  
  /// Look up an PrefixOperatorDecl for the given operator
  /// name in this module (which must be NameBound) and return it, or return
  /// null if there is no operator decl. Returns Nothing if there was an error
  /// resolving the operator name (such as if there were conflicting importing
  /// operator declarations).
  Optional<PrefixOperatorDecl *> lookupPrefixOperator(Identifier name,
                                              SourceLoc diagLoc = SourceLoc());
  /// Look up an PostfixOperatorDecl for the given operator
  /// name in this module (which must be NameBound) and return it, or return
  /// null if there is no operator decl. Returns Nothing if there was an error
  /// resolving the operator name (such as if there were conflicting importing
  /// operator declarations).
  Optional<PostfixOperatorDecl *> lookupPostfixOperator(Identifier name,
                                              SourceLoc diagLoc = SourceLoc());

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
                     unsigned Alignment = alignof(Module));
};
  
/// TranslationUnit - This contains information about all of the decls and
/// external references in a translation unit, which is one file.
class TranslationUnit : public Module {
public:
  typedef std::pair<Module::AccessPathTy, Module*> ImportedModule;
  typedef std::pair<IdentifierType*, DeclContext*> IdentTypeAndContext;
  typedef std::pair<TupleType*, DeclContext*> TupleTypeAndContext;
private:
  /// TypesWithDefaultValues - This is a list of tuples containing a default
  /// value expression, along with the context containing the type with the
  /// expression.
  ArrayRef<TupleTypeAndContext> TypesWithDefaultValues;

  /// ImportedModules - This is the list of modules that are imported by this
  /// module.  This is filled in by the Name Binding phase.
  ArrayRef<ImportedModule> ImportedModules;

public:
  enum TU_Kind {
    StandardLibrary,
    Library,
    Main,
    Repl,
  } Kind;

  /// Decls; the list of top-level declarations for a translation unit.
  std::vector<Decl*> Decls;
  
  /// A map of operator names to InfixOperatorDecls.
  /// Populated during name binding; the mapping will be incomplete until name
  /// binding is complete.
  llvm::StringMap<InfixOperatorDecl*> InfixOperators;

  /// A map of operator names to PostfixOperatorDecls.
  /// Populated during name binding; the mapping will be incomplete until name
  /// binding is complete.
  llvm::StringMap<PostfixOperatorDecl*> PostfixOperators;

  /// A map of operator names to PrefixOperatorDecls.
  /// Populated during name binding; the mapping will be incomplete until name
  /// binding is complete.
  llvm::StringMap<PrefixOperatorDecl*> PrefixOperators;

  /// This keeps track of whether the swift standard library is autoimported
  /// already.  This is lazily done the first time name binding is performed
  /// when there are decls in a module.  This doesn't happen for SIL mode.
  bool ShouldAutoImportStandardLibrary = true;

  TranslationUnit(Identifier Name, Component *Comp, ASTContext &C, TU_Kind Kind)
    : Module(DeclContextKind::TranslationUnit, Name, Comp, C), Kind(Kind) {
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


/// Represents a serialized module that has been imported into Swift.
///
/// This may be a Swift module or a Clang module.
class LoadedModule : public Module {
  ModuleLoader &Owner;

protected:
  LoadedModule(DeclContextKind kind, Identifier name, Component *comp,
               ASTContext &ctx, ModuleLoader &owner)
    : Module(kind, name, comp, ctx), Owner(owner) {
    // Loaded modules are always well-formed.
    ASTStage = TypeChecked;
  }

public:
  // Inherited from Module.
  void lookupValue(AccessPathTy accessPath, Identifier name, NLKind lookupKind,
                   SmallVectorImpl<ValueDecl*> &result);

  /// Look up an operator declaration.
  ///
  /// \param name The operator name ("+", ">>", etc.)
  ///
  /// \param fixity One of PrefixOperator, InfixOperator, or PostfixOperator.
  OperatorDecl *lookupOperator(Identifier name, DeclKind fixity);

  /// Look up an operator declaration.
  template <typename T>
  T *lookupOperator(Identifier name) {
    // Make any non-specialized instantiations fail with a "nice" error message.
    static_assert(static_cast<T*>(nullptr),
                  "Must specify prefix, postfix, or infix operator decl");
  }

  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() >= DeclContextKind::First_LoadedModule &&
           DC->getContextKind() <= DeclContextKind::Last_LoadedModule;
  }
};

/// \brief Represents a Clang module that has been imported into Swift.
///
/// This is exposed at the AST level because we want to do special things with
/// the module's synthesized definitions.
class ClangModule : public LoadedModule {
  clang::Module *clangModule;

public:
  ClangModule(ASTContext &ctx, ModuleLoader &owner, Component *comp,
              clang::Module *clangModule);

  /// \brief Retrieve the underlying Clang module.
  clang::Module *getClangModule() const { return clangModule; }

  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::ClangModule;
  }
};

template <>
PrefixOperatorDecl *
LoadedModule::lookupOperator<PrefixOperatorDecl>(Identifier name);

template <>
PostfixOperatorDecl *
LoadedModule::lookupOperator<PostfixOperatorDecl>(Identifier name);

template <>
InfixOperatorDecl *
LoadedModule::lookupOperator<InfixOperatorDecl>(Identifier name);


} // end namespace swift

#endif

//===--- SymbolGraphASTWalker.h - Symbol Graph AST Walker -----------------===//
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

#ifndef SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPHASTWALKER_H
#define SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPHASTWALKER_H

#include "llvm/ADT/DenseMap.h"
#include "swift/AST/Module.h"
#include "swift/Basic/LLVM.h"
#include "swift/IDE/SourceEntityWalker.h"
#include "swift/Markup/Markup.h"

#include "SymbolGraph.h"

#include <string>

namespace swift {

class Decl;
class Type;
class ValueDecl;

namespace symbolgraphgen {

struct SymbolGraph;
struct SymbolGraphOptions;

/**
 The `SymbolGraphASTWalker` is the core implementation that builds a
 symbol graph. It walks a module for declarations, recording facts about
 symbols and relationships between them.
 */
struct SymbolGraphASTWalker : public SourceEntityWalker {
  /// Options for collecting and serialization.
  const SymbolGraphOptions &Options;

  /// A context for allocations.
  markup::MarkupContext Ctx;

  /// The module that this symbol graph will represent.
  const ModuleDecl &M;

  // FIXME: these should be tracked per-graph, rather than at the top level
  const SmallPtrSet<const ModuleDecl *, 4> ExportedImportedModules;

  const llvm::SmallDenseMap<const ModuleDecl *, SmallPtrSet<Decl *, 4>, 4>
      QualifiedExportedImports;

  /// The symbol graph for the main module of interest.
  SymbolGraph MainGraph;

  /// A map of modules whose types were extended by the main module of interest `M`.
  llvm::StringMap<SymbolGraph *> ExtendedModuleGraphs;

  /// A temporary pointer to a base decl when crawling symbols to synthesize.
  const ValueDecl *BaseDecl = nullptr;

  /// A temporary pointer to the top-level decl being crawled when synthesizing
  /// child symbols.
  const Decl *SynthesizedChildrenBaseDecl = nullptr;

  /// Maps any internal symbol with a public type alias of that symbol.
  llvm::DenseMap<const ValueDecl *, const ValueDecl *> PublicPrivateTypeAliases;

  // MARK: - Initialization

  SymbolGraphASTWalker(
      ModuleDecl &M,
      const SmallPtrSet<const ModuleDecl *, 4> ExportedImportedModules,
      const llvm::SmallDenseMap<const ModuleDecl *, SmallPtrSet<Decl *, 4>, 4>
          QualifiedExportedImports,
      const SymbolGraphOptions &Options);

  SymbolGraphASTWalker(ModuleDecl &M, const SymbolGraphOptions &Options);
  virtual ~SymbolGraphASTWalker() {}

  // MARK: - Utilities

  /// Get a "sub" symbol graph for the appropriate module concerning a declaration.
  ///
  /// This will get the "rootmost" module responsible for a declaration's
  /// documentation. For example:
  ///
  /// Module A:
  ///
  /// ```swift
  /// public struct AStruct {}
  /// ```
  ///
  /// Module B:
  ///
  /// ```swift
  /// import A
  /// extension AStruct {
  ///   public struct BStruct {}
  /// }
  ///
  /// `BStruct` will go in module A's extension symbol graph, because `BStruct`
  /// is a member of `AStruct`, and module A owns `AStruct`, and so on for
  /// further nestings.
  SymbolGraph *getModuleSymbolGraph(const Decl *D);

  // MARK: - SourceEntityWalker

  virtual bool walkToDeclPre(Decl *D, CharSourceRange Range) override;
    
  // MARK: - Utilities

  /// Walk the given decl and add its children as synthesized children of the
  /// given base decl.
  bool synthesizeChildSymbols(Decl *D, const ValueDecl *BaseDecl);

  /// Returns whether the given declaration was itself imported via an `@_exported import`
  /// statement, or if it is an extension or child symbol of something else that was.
  virtual bool isConsideredExportedImported(const Decl *D) const;
  
  /// Returns whether the given declaration comes from an `@_exported import` module.
  ///
  /// If `countUnderlyingClangModule` is `false`, decls from Clang modules will not be considered
  /// re-exported unless the Clang module was itself directly re-exported.
  virtual bool isFromExportedImportedModule(const Decl *D, bool countUnderlyingClangModule = true) const;

  /// Returns whether the given declaration was imported via an `@_exported import <type>` declaration.
  virtual bool isQualifiedExportedImport(const Decl *D) const;

  /// Returns whether the given module is an `@_exported import` module.
  ///
  /// If `countUnderlyingClangModule` is `false`, Clang modules will not be considered re-exported
  /// unless the Clang module itself was directly re-exported.
  virtual bool isExportedImportedModule(const ModuleDecl *M, bool countUnderlyingClangModule = true) const;

  /// Returns whether the given module is the main module, or is an `@_exported import` module.
  virtual bool isOurModule(const ModuleDecl *M) const;

public:
  /// Returns whether the given ExtensionDecl is to be recorded as an extra
  /// extension block symbol, or if its members should be directly associated
  /// with its extended nominal.
  virtual bool shouldBeRecordedAsExtension(const ExtensionDecl *ED) const;

  /// Returns the owning module of the given decl. Loads the module from Clang if necessary, to
  /// correctly fetch owning submodules.
  virtual ModuleDecl *getRealModuleOf(const Decl *D) const;
};

LLVM_ATTRIBUTE_USED
static std::string getFullModuleName(const ModuleDecl *M) {
    if (!M) return "";

    std::string fullName;
    llvm::raw_string_ostream OS(fullName);
    M->getReverseFullModuleName().printForward(OS);
    return fullName;
}

} // end namespace symbolgraphgen
} // end namespace swift

#endif // SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPHASTWALKER_H

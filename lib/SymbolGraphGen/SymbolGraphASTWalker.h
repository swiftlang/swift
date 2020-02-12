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

  /// The symbol graph for a module.
  SymbolGraph Graph;

  /// A map of modules whose types were extended by the main module of interest `M`.
  llvm::DenseMap<ModuleDecl *, SymbolGraph *> ExtendedModuleGraphs;

  // MARK: - Initialization
  
  SymbolGraphASTWalker(ModuleDecl &M, const SymbolGraphOptions &Options);
  virtual ~SymbolGraphASTWalker() {}

  // MARK: - Utilities

  /// Returns `true` if the symbol should be included as a node in the graph.
  bool shouldIncludeNode(const Decl *D) const;

  /// Get a "sub" symbol graph for the parent module of a type that the main module `M` is extending.
  SymbolGraph &getExtendedModuleSymbolGraph(ModuleDecl *M);

  // MARK: - SourceEntityWalker

  virtual bool walkToDeclPre(Decl *D, CharSourceRange Range);
};

} // end namespace symbolgraphgen
} // end namespace swift

#endif // SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPHASTWALKER_H

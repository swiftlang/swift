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

  /// The symbol graph for the main module of interest.
  SymbolGraph MainGraph;

  /// A map of modules whose types were extended by the main module of interest `M`.
  llvm::StringMap<SymbolGraph *> ExtendedModuleGraphs;

  // MARK: - Initialization
  
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
};

} // end namespace symbolgraphgen
} // end namespace swift

#endif // SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPHASTWALKER_H

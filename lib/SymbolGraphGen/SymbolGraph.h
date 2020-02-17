//===--- SymbolGraph.h - Symbol Graph Data Structure ----------------------===//
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

#ifndef SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPH_H
#define SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPH_H

#include "llvm/ADT/SmallSet.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/VersionTuple.h"
#include "swift/Basic/LLVM.h"
#include "Edge.h"
#include "JSON.h"

namespace swift {
namespace symbolgraphgen {

/// A graph of symbols and the relationships between them.
struct SymbolGraph {
  /**
   The module this symbol graph represents.
  */
  ModuleDecl &M;

  /**
   The module's target triple.
  */
  llvm::Triple Target;

  /**
   The semantic version of the module that this symbol graph describes,
   if known.
   */
  Optional<llvm::VersionTuple> ModuleVersion;

  /**
   The symbols in a module: the nodes in the graph.
   */
  llvm::SmallPtrSet<const ValueDecl *, 32> Nodes;

  /**
   The relationships between symbols: the edges in the graph.
   */
  llvm::DenseSet<Edge> Edges;

  SymbolGraph(ModuleDecl &M, llvm::Triple Target,
              Optional<llvm::VersionTuple> ModuleVersion = None);

  void serialize(SymbolGraphASTWalker &Walker,
                 llvm::json::OStream &OS) const;
};

} // end namespace symbolgraphgen
} // end namespace swift 

#endif // SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPH_H

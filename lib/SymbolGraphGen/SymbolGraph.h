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
#include "swift/Markup/Markup.h"
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
   The module whose types were extended in `M`.
   */
  Optional<ModuleDecl *> ExtendedModule;
  
  /**
   The module's target triple.
  */
  llvm::Triple Target;

  /**
   A context for allocations.
   */
  markup::MarkupContext &Ctx;

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

  /// A cache of USRs for declarations.
  llvm::DenseMap<const ValueDecl *, StringRef> USRCache;

  SymbolGraph(ModuleDecl &M,
              Optional<ModuleDecl *> ExtendedModule,
              llvm::Triple Target,
              markup::MarkupContext &Ctx,
              Optional<llvm::VersionTuple> ModuleVersion = None);

  // MARK: - Utilities

  /// Get the USR of a declaration and add it to the local allocator.
  StringRef getUSR(const ValueDecl *VD);

  /// Returns an array of path components for a declaration.
  void getPathComponents(const ValueDecl *VD,
                         SmallVectorImpl<SmallString<32>> &Components);

  /// Get the base print options for declaration fragments.
  PrintOptions getDeclarationFragmentsPrintOptions() const;

  // MARK: - Symbols (Nodes)

  /**
   Record a symbol as a node in the graph.
   */
  void recordNode(const ValueDecl *VD);

  // MARK: - Relationships (Edges)

  /**
   Record a relationship between two declarations as an edge in the graph.

   \param Source The declaration serving as the source of the edge in the
   directed graph.
   \param Target The declaration serving as the target of the edge in the
   directed graph.
   \param Kind The kind of relationship the edge represents.
   */
  void recordEdge(const ValueDecl *Source, const ValueDecl *Target,
                  RelationshipKind Kind);

  /**
   Record a MemberOf relationship, if the given declaration is nested
   in another.
   */
  void recordMemberRelationship(const ValueDecl *VD);

  /**
   Record InheritsFrom relationships for every class from which the
   declaration inherits.
   */
  void recordInheritanceRelationships(const ValueDecl *VD);

  /**
   If the declaration is a default implementation in a protocol extension,
   record a DefaultImplementationOf relationship between the declaration and
   the requirement.
   */
  void recordDefaultImplementationRelationships(const ValueDecl *VD);

  /**
   Record a RequirementOf relationship if the declaration is a requirement
   of a protocol.
   */
  void recordRequirementRelationships(const ValueDecl *VD);

  /**
   If the declaration is an Objective-C-based optional protocol requirement,
   record an OptionalRequirementOf relationship between the declaration
   and its containing protocol.
   */
  void recordOptionalRequirementRelationships(const ValueDecl *VD);

  /**
   Record ConformsTo relationships for each protocol conformance of
   the declaration.
   */
  void recordConformanceRelationships(const ValueDecl *VD);

  /**
   Records an Overrides relationship if the given declaration
   overrides another.
   */
  void recordOverrideRelationship(const ValueDecl *VD);

  // MARK: - Serialization

  /// Serialize this symbol graph's JSON to an output stream.
  void serialize(llvm::json::OStream &OS);

  /// Serialize the overall declaration fragments for a `ValueDecl`.
  void
  serializeDeclarationFragments(StringRef Key, const ValueDecl *VD,
                                llvm::json::OStream &OS);

  /// Get the overall declaration fragments for a `ValueDecl` when it is viewed
  /// as a subheading and/or part of a larger group of symbol listings.
  void
  serializeSubheadingDeclarationFragments(StringRef Key, const ValueDecl *VD,
                                          llvm::json::OStream &OS);

  /// Get the overall declaration for a type declaration.
  void
  serializeDeclarationFragments(StringRef Key, Type T,
                                llvm::json::OStream &OS);
};

} // end namespace symbolgraphgen
} // end namespace swift 

#endif // SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPH_H

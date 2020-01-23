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

#include "swift/Basic/LLVM.h"
#include "swift/IDE/SourceEntityWalker.h"
#include "swift/Markup/Markup.h"

#include "SymbolGraph.h"

namespace swift {

class Decl;
class Type;
class ValueDecl;

namespace symbolgraphgen {

struct SymbolIdentifier;
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

  /// The module that this symbol graph will represent.
  const ModuleDecl &M;

  /// The symbol graph.
  SymbolGraph Graph;

  /// A context for allocations.
  markup::MarkupContext Ctx;

  /// A cache of identifiers for declarations that may be seen more than once.
  llvm::DenseMap<const Decl *, SymbolIdentifier> SymbolIdentifierCache;

  /// A cache of USRs for declarations.
  llvm::DenseMap<const ValueDecl *, StringRef> USRCache;

  // MARK: -
  
  SymbolGraphASTWalker(ModuleDecl &M, const SymbolGraphOptions &Options);
  virtual ~SymbolGraphASTWalker() {}

  // MARK: - 

  /// Returns `true` if the symbol should be included as a node in the graph.
  bool shouldIncludeNode(const Decl *D) const;

  virtual bool walkToDeclPre(Decl *D, CharSourceRange Range);

  // MARK: - Utilities and Conversions

  /// Get the USR of a declaration and add it to the local allocator.
  StringRef getUSR(const ValueDecl *VD);

  /// Returns a `SymbolIdentifier` for a given declaration.
  SymbolIdentifier getSymbolIdentifier(const ValueDecl *VD);

  // MARK: - Declaration Fragments

  /// Get the base print options for declaration fragments.
  PrintOptions getDeclarationFragmentsPrintOptions() const;

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
};

} // end namespace symbolgraphgen
} // end namespace swift

#endif // SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPHASTWALKER_H

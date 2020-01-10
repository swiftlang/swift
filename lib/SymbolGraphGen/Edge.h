//===--- Edge.h - Symbol Graph Edge ---------------------------------------===//
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

#ifndef SWIFT_SYMBOLGRAPHGEN_EDGE_H
#define SWIFT_SYMBOLGRAPHGEN_EDGE_H

#include "llvm/Support/JSON.h"
#include "swift/AST/Decl.h"
#include "swift/Basic/LLVM.h"

#include "JSON.h"
#include "Symbol.h"

namespace swift {
namespace symbolgraphgen {
  
/// The kind of relationship, tagging an edge in the graph.
struct RelationshipKind {
  StringRef Name;
  
  RelationshipKind(llvm::StringRef Name) : Name(Name) {}
  
  /**
   A symbol A is a member of another symbol B.
   
   For example, a method or field of a class would be a member of that class.
   
   The implied inverse of this relationship is a symbol B is the owner
   of a member symbol A.
   */
  static inline RelationshipKind MemberOf() {
    return RelationshipKind { "memberOf" };
  }

  /**
   A symbol A conforms to an interface/protocol symbol B.
   
   For example, a class `C` that conforms to protocol `P` in Swift would use
   this relationship.
   
   The implied inverse of this relationship is a symbol B that has
   a conformer A.
   */
  static inline RelationshipKind ConformsTo() {
    return RelationshipKind { "conformsTo" };
  }
  /**
   A symbol A inherits from another symbol B.
   
   For example, a derived class inherits from a base class, or a protocol
   that refines another protocol would use this relationship.
   
   The implied inverse of this relationship is a symbol B is a base
   of another symbol A.
   */
  static inline RelationshipKind InheritsFrom() {
    return RelationshipKind { "inheritsFrom" };
  }
  /**
   A symbol A serves as a default implementation of an interface requirement B.
   
   The implied inverse of this relationship is an interface requirement B
   has a default implementation of A.
   */
  static inline RelationshipKind DefaultImplementationOf() {
    return RelationshipKind { "defaultImplementationOf" };
  }
  /**
   A symbol A overrides another symbol B, such as through inheritance.
   
   The implied inverse of this relationship is a symbol A is the base
   of symbol B.
   */
  static inline RelationshipKind Overrides() {
    return RelationshipKind { "overrides" };
  }
  /**
   A symbol A is a requirement of interface B.
   
   The implied inverse of this relationship is an interface B
   has a requirement of A.
   */
  static inline RelationshipKind RequirementOf() {
    return RelationshipKind { "requirementOf" };
  }
  /**
   A symbol A is an optional requirement of interface B.
   
   The implied inverse of this relationship is an interface B
   has an optional requirement of A.
   */
  static inline RelationshipKind OptionalRequirementOf() {
    return RelationshipKind { "optionalRequirementOf" };
  }
  
  bool operator==(const RelationshipKind &Other) const {
    return Name == Other.Name;
  }

  bool operator<(const RelationshipKind &Other) const {
    return Name < Other.Name;
  }
};

/// A relationship between two symbols: an edge in a directed graph.
struct Edge {
  SymbolGraphASTWalker *Walker;

  /// The kind of relationship this edge represents.
  RelationshipKind Kind;

  /// The precise identifier of the source symbol node.
  const ValueDecl *Source;
  
  /// The precise identifier of the target symbol node.
  const ValueDecl *Target;
  
  void serialize(llvm::json::OStream &OS) const;
};
  
} // end namespace symbolgraphgen 
} // end namespace swift

namespace llvm {
using Edge = swift::symbolgraphgen::Edge;
template <> struct DenseMapInfo<Edge> {
  static inline Edge getEmptyKey() {
    return {
      nullptr,
      { "Empty" },
      nullptr,
      nullptr,
    };
  }
  static inline Edge getTombstoneKey() {
    return {
      nullptr,
      { "Tombstone" },
      nullptr,
      nullptr,
    };
  }
  static unsigned getHashValue(const Edge E) {
    unsigned H = 0;
    H ^= DenseMapInfo<StringRef>::getHashValue(E.Kind.Name);
    H ^= DenseMapInfo<uintptr_t>::getHashValue(reinterpret_cast<uintptr_t>(E.Source));
    H ^= DenseMapInfo<uintptr_t>::getHashValue(reinterpret_cast<uintptr_t>(E.Target));
    return H;
  }
  static bool isEqual(const Edge LHS, const Edge RHS) {
    return LHS.Kind == RHS.Kind &&
      LHS.Source == RHS.Source &&
      LHS.Target == RHS.Target;
  }
};
} // end namespace llvm

#endif // SWIFT_SYMBOLGRAPHGEN_EDGE_H

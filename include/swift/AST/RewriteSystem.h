//===--- RewriteSystem.h - Generics with term rewriting ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REWRITESYSTEM_H
#define SWIFT_REWRITESYSTEM_H

#include "swift/AST/Decl.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/LayoutConstraint.h"
#include "swift/AST/ProtocolGraph.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Statistic.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/TrailingObjects.h"
#include <algorithm>

namespace llvm {
  class raw_ostream;
}

namespace swift {

namespace rewriting {

class RewriteContext;

/// The smallest element in the rewrite system.
///
/// enum Atom {
///   case name(Identifier)
///   case protocol(Protocol)
///   case type([Protocol], Identifier)
///   case genericParam(index: Int, depth: Int)
///   case layout(LayoutConstraint)
/// }
///
/// Out-of-line methods are documented in RewriteSystem.cpp.
class Atom final {
public:
  enum class Kind : uint8_t {
    /// An associated type [P:T] or [P&Q&...:T]. The parent term
    /// must be known to conform to P (or P, Q, ...).
    AssociatedType,

    /// A generic parameter, uniquely identified by depth and
    /// index. Can only appear at the beginning of a term, where
    /// it represents a generic parameter of the top-level generic
    /// signature.
    GenericParam,

    /// An unbound identifier name.
    Name,

    /// When appearing at the start of a term, represents a nested
    /// type of a protocol 'Self' type.
    ///
    /// When appearing at the end of a term, represents that the
    /// term conforms to the protocol.
    Protocol,

    /// When appearring at the end of a term, represents that the
    /// term conforms to the layout.
    Layout
  };

private:
  friend class RewriteContext;

  /// Atoms are uniqued and immutable, stored as a single pointer;
  /// the Storage type is the allocated backing storage.
  struct Storage final
    : public llvm::FoldingSetNode,
      public llvm::TrailingObjects<Storage, const ProtocolDecl *> {
    friend class Atom;

    unsigned Kind : 16;
    unsigned NumProtocols : 16;

    union {
      Identifier Name;
      LayoutConstraint Layout;
      const ProtocolDecl *Proto;
      GenericTypeParamType *GenericParam;
    };

    explicit Storage(Identifier name) {
      Kind = unsigned(Atom::Kind::Name);
      NumProtocols = 0;
      Name = name;
    }

    explicit Storage(LayoutConstraint layout) {
      Kind = unsigned(Atom::Kind::Layout);
      NumProtocols = 0;
      Layout = layout;
    }

    explicit Storage(const ProtocolDecl *proto) {
      Kind = unsigned(Atom::Kind::Protocol);
      NumProtocols = 0;
      Proto = proto;
    }

    explicit Storage(GenericTypeParamType *param) {
      Kind = unsigned(Atom::Kind::GenericParam);
      NumProtocols = 0;
      GenericParam = param;
    }

    Storage(ArrayRef<const ProtocolDecl *> protos, Identifier name) {
      assert(!protos.empty());

      Kind = unsigned(Atom::Kind::AssociatedType);
      NumProtocols = protos.size();
      Name = name;

      for (unsigned i : indices(protos))
        getProtocols()[i] = protos[i];
    }

    size_t numTrailingObjects(OverloadToken<const ProtocolDecl *>) const {
      return NumProtocols;
    }

    MutableArrayRef<const ProtocolDecl *> getProtocols() {
      return {getTrailingObjects<const ProtocolDecl *>(), NumProtocols};
    }

    ArrayRef<const ProtocolDecl *> getProtocols() const {
      return {getTrailingObjects<const ProtocolDecl *>(), NumProtocols};
    }

    void Profile(llvm::FoldingSetNodeID &id) {
      id.AddInteger(Kind);

      switch (Atom::Kind(Kind)) {
      case Atom::Kind::Name:
        id.AddPointer(Name.get());
        return;

      case Atom::Kind::Layout:
        id.AddPointer(Layout.getPointer());
        return;

      case Atom::Kind::Protocol:
        id.AddPointer(Proto);
        return;

      case Atom::Kind::GenericParam:
        id.AddPointer(GenericParam);
        return;

      case Atom::Kind::AssociatedType: {
        auto protos = getProtocols();
        id.AddInteger(protos.size());

        for (const auto *proto : protos)
          id.AddPointer(proto);

        id.AddPointer(Name.get());
        return;
      }
      }

      llvm_unreachable("Bad atom kind");
    }
  };

private:
  const Storage *Ptr;

  Atom(const Storage *ptr) : Ptr(ptr) {}

public:
  Kind getKind() const {
    return Kind(Ptr->Kind);
  }

  /// Get the identifier associated with an unbound name atom or an
  /// associated type atom.
  Identifier getName() const {
    assert(getKind() == Kind::Name ||
           getKind() == Kind::AssociatedType);
    return Ptr->Name;
  }

  /// Get the single protocol declaration associate with a protocol atom.
  const ProtocolDecl *getProtocol() const {
    assert(getKind() == Kind::Protocol);
    return Ptr->Proto;
  }

  /// Get the list of protocols associated with a protocol or associated
  /// type atom.
  ArrayRef<const ProtocolDecl *> getProtocols() const {
    assert(getKind() == Kind::AssociatedType);
    auto protos = Ptr->getProtocols();
    assert(!protos.empty());
    return protos;
  }

  /// Get the generic parameter associated with a generic parameter atom.
  GenericTypeParamType *getGenericParam() const {
    assert(getKind() == Kind::GenericParam);
    return Ptr->GenericParam;
  }

  /// Get the layout constraint associated with a layout constraint atom.
  LayoutConstraint getLayoutConstraint() const {
    assert(getKind() == Kind::Layout);
    return Ptr->Layout;
  }

  /// Creates a new name atom.
  static Atom forName(Identifier name,
                      RewriteContext &ctx);

  /// Creates a new protocol atom.
  static Atom forProtocol(const ProtocolDecl *proto,
                          RewriteContext &ctx);

  /// Creates a new associated type atom for a single protocol.
  static Atom forAssociatedType(const ProtocolDecl *proto,
                                Identifier name,
                                RewriteContext &ctx);

  /// Creates a merged associated type atom to represent a nested
  /// type that conforms to multiple protocols, all of which have
  /// an associated type with the same name.
  static Atom forAssociatedType(ArrayRef<const ProtocolDecl *> protos,
                                Identifier name,
                                RewriteContext &ctx);

  /// Creates a generic parameter atom, representing a generic
  /// parameter in the top-level generic signature from which the
  /// rewrite system is built.
  static Atom forGenericParam(GenericTypeParamType *param,
                              RewriteContext &ctx);

  /// Creates a layout atom, representing a layout constraint.
  static Atom forLayout(LayoutConstraint layout,
                        RewriteContext &ctx);

  int compare(Atom other, const ProtocolGraph &protos) const;

  void dump(llvm::raw_ostream &out) const;

  friend bool operator==(Atom lhs, Atom rhs) {
    return lhs.Ptr == rhs.Ptr;
  }

  friend bool operator!=(Atom lhs, Atom rhs) {
    return !(lhs == rhs);
  }
};

/// A term is a sequence of one or more atoms.
///
/// The first atom in the term must be a protocol, generic parameter, or
/// associated type atom.
///
/// A layout constraint atom must only appear at the end of a term.
///
/// Out-of-line methods are documented in RewriteSystem.cpp.
class Term final {
  llvm::SmallVector<Atom, 3> Atoms;

public:
  Term() {}

  explicit Term(llvm::SmallVector<Atom, 3> &&atoms)
    : Atoms(std::move(atoms)) {}

  explicit Term(ArrayRef<Atom> atoms)
    : Atoms(atoms.begin(), atoms.end()) {}

  void add(Atom atom) {
    Atoms.push_back(atom);
  }

  int compare(const Term &other, const ProtocolGraph &protos) const;

  size_t size() const { return Atoms.size(); }

  decltype(Atoms)::const_iterator begin() const { return Atoms.begin(); }
  decltype(Atoms)::const_iterator end() const { return Atoms.end(); }

  decltype(Atoms)::iterator begin() { return Atoms.begin(); }
  decltype(Atoms)::iterator end() { return Atoms.end(); }

  const Atom &back() const {
    return Atoms.back();
  }

  Atom &back() {
    return Atoms.back();
  }

  const Atom &operator[](size_t index) const {
    return Atoms[index];
  }

  Atom &operator[](size_t index) {
    return Atoms[index];
  }

  decltype(Atoms)::const_iterator findSubTerm(const Term &other) const;

  decltype(Atoms)::iterator findSubTerm(const Term &other);

  /// Returns true if this term contains, or is equal to, \p other.
  bool containsSubTerm(const Term &other) const {
    return findSubTerm(other) != end();
  }

  bool rewriteSubTerm(const Term &lhs, const Term &rhs);

  bool checkForOverlap(const Term &other, Term &result) const;

  void dump(llvm::raw_ostream &out) const;
};

/// A global object that can be shared by multiple rewrite systems.
///
/// It stores uniqued atoms and terms.
///
/// Out-of-line methods are documented in RewriteSystem.cpp.
class RewriteContext final {
  friend class Atom;

  /// Allocator for uniquing atoms and terms.
  llvm::BumpPtrAllocator Allocator;

  /// Folding set for uniquing atoms.
  llvm::FoldingSet<Atom::Storage> Atoms;

  RewriteContext(const RewriteContext &) = delete;
  RewriteContext(RewriteContext &&) = delete;
  RewriteContext &operator=(const RewriteContext &) = delete;
  RewriteContext &operator=(RewriteContext &&) = delete;

public:
  /// Statistical counters.
  UnifiedStatsReporter *Stats;

  RewriteContext(UnifiedStatsReporter *stats) : Stats(stats) {}

  Term getTermForType(CanType paramType,
                      const ProtocolDecl *proto);
};

/// A rewrite rule that replaces occurrences of LHS with RHS.
///
/// LHS must be greater than RHS in the linear order over terms.
///
/// Out-of-line methods are documented in RewriteSystem.cpp.
class Rule final {
  Term LHS;
  Term RHS;
  bool deleted;

public:
  Rule(const Term &lhs, const Term &rhs)
      : LHS(lhs), RHS(rhs), deleted(false) {}

  const Term &getLHS() const { return LHS; }
  const Term &getRHS() const { return RHS; }

  bool apply(Term &term) const {
    return term.rewriteSubTerm(LHS, RHS);
  }

  bool checkForOverlap(const Rule &other, Term &result) const {
    return LHS.checkForOverlap(other.LHS, result);
  }

  bool canReduceLeftHandSide(const Rule &other) const {
    return LHS.containsSubTerm(other.LHS);
  }

  /// Returns if the rule was deleted.
  bool isDeleted() const {
    return deleted;
  }

  /// Deletes the rule, which removes it from consideration in term
  /// simplification and completion. Deleted rules are simply marked as
  /// such instead of being physically removed from the rules vector
  /// in the rewrite system, to ensure that indices remain valid across
  /// deletion.
  void markDeleted() {
    assert(!deleted);
    deleted = true;
  }

  /// Returns the length of the left hand side.
  unsigned getDepth() const {
    return LHS.size();
  }

  /// Partial order on rules orders rules by their left hand side.
  int compare(const Rule &other,
              const ProtocolGraph &protos) const {
    return LHS.compare(other.LHS, protos);
  }

  void dump(llvm::raw_ostream &out) const;
};

/// A term rewrite system for working with types in a generic signature.
///
/// Out-of-line methods are documented in RewriteSystem.cpp.
class RewriteSystem final {
  /// Rewrite context for memory allocation.
  RewriteContext &Context;

  /// The rules added so far, including rules from our client, as well
  /// as rules introduced by the completion procedure.
  std::vector<Rule> Rules;

  /// The graph of all protocols transitively referenced via our set of
  /// rewrite rules, used for the linear order on atoms.
  ProtocolGraph Protos;

  /// A list of pending terms for the associated type merging completion
  /// heuristic.
  ///
  /// The pair (lhs, rhs) satisfies the following conditions:
  /// - lhs > rhs
  /// - all atoms but the last are pair-wise equal in lhs and rhs
  /// - the last atom in both lhs and rhs is an associated type atom
  /// - the last atom in both lhs and rhs has the same name
  ///
  /// See RewriteSystem::processMergedAssociatedTypes() for details.
  std::vector<std::pair<Term, Term>> MergedAssociatedTypes;

  /// A list of pending pairs for checking overlap in the completion
  /// procedure.
  std::deque<std::pair<unsigned, unsigned>> Worklist;

  /// Set these to true to enable debugging output.
  unsigned DebugSimplify : 1;
  unsigned DebugAdd : 1;
  unsigned DebugMerge : 1;
  unsigned DebugCompletion : 1;

public:
  explicit RewriteSystem(RewriteContext &ctx) : Context(ctx) {
    DebugSimplify = false;
    DebugAdd = false;
    DebugMerge = false;
    DebugCompletion = false;
  }

  RewriteSystem(const RewriteSystem &) = delete;
  RewriteSystem(RewriteSystem &&) = delete;
  RewriteSystem &operator=(const RewriteSystem &) = delete;
  RewriteSystem &operator=(RewriteSystem &&) = delete;

  /// Return the rewrite context used for allocating memory.
  RewriteContext &getRewriteContext() const { return Context; }

  /// Return the object recording information about known protocols.
  const ProtocolGraph &getProtocols() const { return Protos; }

  void initialize(std::vector<std::pair<Term, Term>> &&rules,
                  ProtocolGraph &&protos);

  bool addRule(Term lhs, Term rhs);

  bool simplify(Term &term) const;

  enum class CompletionResult {
    /// Confluent completion was computed successfully.
    Success,

    /// Maximum number of iterations reached.
    MaxIterations,

    /// Completion produced a rewrite rule whose left hand side has a length
    /// exceeding the limit.
    MaxDepth
  };

  CompletionResult computeConfluentCompletion(
      unsigned maxIterations,
      unsigned maxDepth);

  void dump(llvm::raw_ostream &out) const;

private:
  Atom mergeAssociatedTypes(Atom lhs, Atom rhs) const;
  void processMergedAssociatedTypes();
};

} // end namespace rewriting

} // end namespace swift

#endif

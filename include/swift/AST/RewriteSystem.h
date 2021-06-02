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
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include <algorithm>

namespace llvm {
  class raw_ostream;
}

namespace swift {

namespace rewriting {

/// The most primitive term in the rewrite system.
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
  using Storage = llvm::PointerUnion<Identifier,
                                     GenericTypeParamType *,
                                     LayoutConstraint>;

  llvm::TinyPtrVector<const ProtocolDecl *> Protos;
  Storage Value;

  explicit Atom(llvm::TinyPtrVector<const ProtocolDecl *> protos,
                Storage value)
      : Protos(protos), Value(value) {
    // Triggers assertion if the atom is not valid.
    (void) getKind();
  }

public:
  /// Creates a new name atom.
  static Atom forName(Identifier name) {
    return Atom({}, name);
  }

  /// Creates a new protocol atom.
  static Atom forProtocol(const ProtocolDecl *proto) {
    return Atom({proto}, Storage());
  }

  /// Creates a new associated type atom for a single protocol.
  static Atom forAssociatedType(const ProtocolDecl *proto,
                                Identifier name) {
    assert(proto != nullptr);
    return Atom({proto}, name);
  }

  /// Creates a merged associated type atom to represent a nested
  /// type that conforms to multiple protocols, all of which have
  /// an associated type with the same name.
  static Atom forAssociatedType(
      llvm::TinyPtrVector<const ProtocolDecl *>protos,
      Identifier name) {
    assert(!protos.empty());
    return Atom(protos, name);
  }

  /// Creates a generic parameter atom, representing a generic
  /// parameter in the top-level generic signature from which the
  /// rewrite system is built.
  static Atom forGenericParam(GenericTypeParamType *param) {
    assert(param->isCanonical());
    return Atom({}, param);
  }

  /// Creates a layout atom, representing a layout constraint.
  static Atom forLayout(LayoutConstraint layout) {
    return Atom({}, layout);
  }

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

  Kind getKind() const {
    if (!Value) {
      assert(Protos.size() == 1);
      return Kind::Protocol;
    }

    if (Value.is<Identifier>()) {
      if (!Protos.empty())
        return Kind::AssociatedType;
      return Kind::Name;
    }

    if (Value.is<GenericTypeParamType *>()) {
      assert(Protos.empty());
      return Kind::GenericParam;
    }

    if (Value.is<LayoutConstraint>()) {
      assert(Protos.empty());
      return Kind::Layout;
    }

    llvm_unreachable("Bad term rewriting atom");
  }

  /// Get the identifier associated with an unbound name atom or an
  /// associated type atom.
  Identifier getName() const {
    assert(getKind() == Kind::Name ||
           getKind() == Kind::AssociatedType);
    return Value.get<Identifier>();
  }

  /// Get the single protocol declaration associate with a protocol atom.
  const ProtocolDecl *getProtocol() const {
    assert(getKind() == Kind::Protocol);
    assert(Protos.size() == 1);
    return Protos.front();
  }

  /// Get the list of protocols associated with a protocol or associated
  /// type atom.
  llvm::TinyPtrVector<const ProtocolDecl *> getProtocols() const {
    assert(getKind() == Kind::Protocol ||
           getKind() == Kind::AssociatedType);
    assert(!Protos.empty());
    return Protos;
  }

  /// Get the generic parameter associated with a generic parameter atom.
  GenericTypeParamType *getGenericParam() const {
    assert(getKind() == Kind::GenericParam);
    return Value.get<GenericTypeParamType *>();
  }

  /// Get the layout constraint associated with a layout constraint atom.
  LayoutConstraint getLayoutConstraint() const {
    assert(getKind() == Kind::Layout);
    return Value.get<LayoutConstraint>();
  }

  int compare(Atom other, const ProtocolGraph &protos) const;

  void dump(llvm::raw_ostream &out) const;

  friend bool operator==(Atom lhs, Atom rhs) {
    return (lhs.Protos.size() == rhs.Protos.size() &&
            std::equal(lhs.Protos.begin(), lhs.Protos.end(),
                       rhs.Protos.begin()) &&
            lhs.Value == rhs.Value);
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
    assert(!deleted);
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

public:
  explicit RewriteSystem() {
    DebugSimplify = false;
    DebugAdd = false;
    DebugMerge = false;
  }

  RewriteSystem(const RewriteSystem &) = delete;
  RewriteSystem(RewriteSystem &&) = delete;
  RewriteSystem &operator=(const RewriteSystem &) = delete;
  RewriteSystem &operator=(RewriteSystem &&) = delete;

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

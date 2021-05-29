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
#include "swift/AST/Types.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {

namespace rewriting {

using ProtocolOrder = std::function<int (const ProtocolDecl *,
                                         const ProtocolDecl *)>;

class Atom final {
  using Storage = llvm::PointerUnion<Identifier, GenericTypeParamType *>;

  const ProtocolDecl *Proto;
  Storage Value;

  explicit Atom(const ProtocolDecl *proto, Storage value)
      : Proto(proto), Value(value) {
    // Triggers assertion if the atom is not valid.
    (void) getKind();
  }

public:
  static Atom forName(Identifier name) {
    return Atom(nullptr, name);
  }

  static Atom forProtocol(const ProtocolDecl *proto) {
    return Atom(proto, Storage());
  }

  static Atom forAssociatedType(const ProtocolDecl *proto,
                                Identifier name) {
    assert(proto != nullptr);
    return Atom(proto, name);
  }

  static Atom forGenericParam(GenericTypeParamType *param) {
    assert(param->isCanonical());
    return Atom(nullptr, param);
  }

  enum class Kind : uint8_t {
    AssociatedType,
    GenericParam,
    Name,
    Protocol,
  };

  Kind getKind() const {
    if (!Value) {
      assert(Proto != nullptr);
      return Kind::Protocol;
    }

    if (Value.is<Identifier>()) {
      if (Proto != nullptr)
        return Kind::AssociatedType;
      return Kind::Name;
    }

    if (Value.is<GenericTypeParamType *>()) {
      assert(Proto == nullptr);
      return Kind::GenericParam;
    }

    llvm_unreachable("Bad term rewriting atom");
  }

  Identifier getName() const {
    assert(getKind() == Kind::Name ||
           getKind() == Kind::AssociatedType);
    return Value.get<Identifier>();
  }

  const ProtocolDecl *getProtocol() const {
    assert(getKind() == Kind::Protocol ||
           getKind() == Kind::AssociatedType);
    return Proto;
  }

  GenericTypeParamType *getGenericParam() const {
    assert(getKind() == Kind::GenericParam);
    return Value.get<GenericTypeParamType *>();
  }

  int compare(Atom other, ProtocolOrder compare) const;

  void dump(llvm::raw_ostream &out) const;

  friend bool operator==(Atom lhs, Atom rhs) {
    return (lhs.Proto == rhs.Proto &&
            lhs.Value == rhs.Value);
  }

  friend bool operator!=(Atom lhs, Atom rhs) {
    return !(lhs == rhs);
  }
};

class Term final {
  llvm::SmallVector<Atom, 3> Atoms;

public:
  Term() {}

  explicit Term(llvm::SmallVector<Atom, 3> &&atoms)
    : Atoms(atoms) {}

  explicit Term(ArrayRef<Atom> atoms)
    : Atoms(atoms.begin(), atoms.end()) {}

  void add(Atom atom) {
    Atoms.push_back(atom);
  }

  int compare(const Term &other, ProtocolOrder order) const;

  size_t size() const { return Atoms.size(); }

  decltype(Atoms)::const_iterator begin() const { return Atoms.begin(); }
  decltype(Atoms)::const_iterator end() const { return Atoms.end(); }

  decltype(Atoms)::iterator begin() { return Atoms.begin(); }
  decltype(Atoms)::iterator end() { return Atoms.end(); }

  const Atom &operator[](size_t index) const {
    return Atoms[index];
  }

  decltype(Atoms)::const_iterator findSubTerm(const Term &other) const;

  decltype(Atoms)::iterator findSubTerm(const Term &other);

  bool containsSubTerm(const Term &other) const {
    return findSubTerm(other) != end();
  }

  bool rewriteSubTerm(const Term &lhs, const Term &rhs);

  bool checkForOverlap(const Term &other, Term &result) const;

  void dump(llvm::raw_ostream &out) const;
};

class Rule final {
  Term LHS;
  Term RHS;
  bool deleted;

public:
  Rule(const Term &lhs, const Term &rhs)
      : LHS(lhs), RHS(rhs), deleted(false) {}

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

  bool isDeleted() const {
    return deleted;
  }

  void markDeleted() {
    assert(!deleted);
    deleted = true;
  }

  unsigned getDepth() const {
    return LHS.size();
  }

  int compare(const Rule &other,
              ProtocolOrder protocolOrder) const {
    return LHS.compare(other.LHS, protocolOrder);
  }

  void dump(llvm::raw_ostream &out) const;
};

class RewriteSystem final {
  std::vector<Rule> Rules;
  ProtocolOrder Order;

  unsigned DebugSimplify : 1;
  unsigned DebugAdd : 1;

public:
  explicit RewriteSystem(ProtocolOrder order) : Order(order) {
    DebugSimplify = false;
    DebugAdd = false;
  }

  RewriteSystem(const RewriteSystem &) = delete;
  RewriteSystem(RewriteSystem &&) = delete;
  RewriteSystem &operator=(const RewriteSystem &) = delete;
  RewriteSystem &operator=(RewriteSystem &&) = delete;

  bool addRule(Term lhs, Term rhs);

  bool simplify(Term &term) const;

  enum class CompletionResult {
    Success,
    MaxIterations,
    MaxDepth
  };

  CompletionResult computeConfluentCompletion(
      unsigned maxIterations,
      unsigned maxDepth);

  void dump(llvm::raw_ostream &out) const;
};

} // end namespace rewriting

} // end namespace swift

#endif

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

class Atom final {
  using Storage = llvm::PointerUnion<Identifier,
                                     const ProtocolDecl *,
                                     const AssociatedTypeDecl *,
                                     GenericTypeParamType *>;
  Storage Value;

  explicit Atom(Storage value) : Value(value) {}

public:
  static Atom forName(Identifier name) {
    return Atom(name);
  }

  static Atom forProtocol(const ProtocolDecl *proto) {
    assert(proto != nullptr);
    return Atom(proto);
  }

  static Atom forAssociatedType(const AssociatedTypeDecl *type) {
    assert(type != nullptr);
    return Atom(type);
  }

  static Atom forGenericParam(GenericTypeParamType *param) {
    assert(param->isCanonical());
    return Atom(param);
  }

  enum class Kind : uint8_t {
    AssociatedType,
    GenericParam,
    Name,
    Protocol,
  };

  Kind getKind() const {
    if (Value.is<Identifier>())
      return Kind::Name;
    if (Value.is<const ProtocolDecl *>())
      return Kind::Protocol;
    if (Value.is<const AssociatedTypeDecl *>())
      return Kind::AssociatedType;
    if (Value.is<GenericTypeParamType *>())
      return Kind::GenericParam;
    llvm_unreachable("Bad term rewriting atom");
  }

  Identifier getName() const {
    return Value.get<Identifier>();
  }

  const ProtocolDecl *getProtocol() const {
    return Value.get<const ProtocolDecl *>();
  }

  const AssociatedTypeDecl *getAssociatedType() const {
    return Value.get<const AssociatedTypeDecl *>();
  }

  GenericTypeParamType *getGenericParam() const {
    return Value.get<GenericTypeParamType *>();
  }

  int compare(Atom other) const;

  void dump(llvm::raw_ostream &out) const;

  friend bool operator==(Atom lhs, Atom rhs) {
    return lhs.Value == rhs.Value;
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

  int compare(const Term &other) const;

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
      : LHS(lhs), RHS(rhs), deleted(false) {
    assert(LHS.compare(RHS) > 0);
  }

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

  void dump(llvm::raw_ostream &out) const;
};

class RewriteSystem final {
  std::vector<Rule> Rules;

public:
  bool addRule(Term lhs, Term rhs);

  bool simplify(Term &term) const;

  void computeConfluentCompletion(unsigned maxIterations);

  void dump(llvm::raw_ostream &out) const;
};

} // end namespace rewriting

} // end namespace swift

#endif

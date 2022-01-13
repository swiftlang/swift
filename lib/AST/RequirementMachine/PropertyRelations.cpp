//===--- PropertyRelations.cpp - Relations between property rules ---------===//
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

#include "swift/AST/Type.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include "RewriteSystem.h"

using namespace swift;
using namespace rewriting;

RewriteSystem::Relation
RewriteSystem::getRelation(unsigned index) const {
  return Relations[index];
}

/// Record a relation that transforms the left hand side when it appears
/// at the end of a term to the right hand side. Returns the relation ID,
/// which can be passed to RewriteStep::forRelation().
unsigned RewriteSystem::recordRelation(Term lhs, Term rhs) {
  auto key = std::make_pair(lhs, rhs);
  auto found = RelationMap.find(key);
  if (found != RelationMap.end())
    return found->second;

  unsigned index = Relations.size();
  Relations.push_back(key);
  auto inserted = RelationMap.insert(std::make_pair(key, index));
  assert(inserted.second);
  (void) inserted;

  return index;
}

/// Given a left-hand side symbol [p1] and a right-hand side symbol
/// [p2], record a relation ([p1].[p2] => [p1]), which denotes that
/// the property p1 implies the property p2.
///
/// An example is a superclass requirement that implies a layout
/// requirement.
unsigned RewriteSystem::recordRelation(Symbol lhsProperty,
                                       Symbol rhsProperty) {
  assert(lhsProperty.isProperty());
  assert(rhsProperty.isProperty());

  MutableTerm lhsTerm;
  lhsTerm.add(lhsProperty);
  lhsTerm.add(rhsProperty);

  MutableTerm rhsTerm;
  rhsTerm.add(lhsProperty);

  // Record a relation ([p1].[p2] => [p1]).
  return recordRelation(
      Term::get(lhsTerm, Context),
      Term::get(rhsTerm, Context));
}

RewriteSystem::TypeWitness::TypeWitness(
    Term lhs, llvm::PointerUnion<Symbol, Term> rhs)
  : LHS(lhs), RHS(rhs) {
  assert(LHS.size() >= 2);
  assert(getConcreteConformance().getKind() ==
         Symbol::Kind::ConcreteConformance);
  assert(getAssocType().getKind() == Symbol::Kind::AssociatedType);
  if (RHS.is<Symbol>())
    assert(RHS.get<Symbol>().getKind() == Symbol::Kind::ConcreteType);
  assert(getAssocType().getProtocols().size() == 1);
  assert(getAssocType().getProtocols()[0] ==
         getConcreteConformance().getProtocol());
}

namespace swift {
namespace rewriting {
bool operator==(const RewriteSystem::TypeWitness &lhs,
                const RewriteSystem::TypeWitness &rhs) {
  return lhs.LHS == rhs.LHS && lhs.RHS == rhs.RHS;
}
}
}

void RewriteSystem::TypeWitness::dump(llvm::raw_ostream &out) const {
  out << "Subject type: " << LHS << "\n";
  if (RHS.is<Symbol>())
    out << "Concrete type witness: " << RHS.get<Symbol>() << "\n";
  else
    out << "Abstract type witness: " << RHS.get<Term>() << "\n";
}

unsigned RewriteSystem::recordTypeWitness(
    RewriteSystem::TypeWitness witness) {
  unsigned index = TypeWitnesses.size();
  auto inserted = TypeWitnessMap.insert(std::make_pair(witness.LHS, index));

  if (!inserted.second) {
    index = inserted.first->second;
  } else {
    TypeWitnesses.push_back(witness);
  }

  assert(TypeWitnesses[index] == witness);
  return index;
}

const RewriteSystem::TypeWitness &
RewriteSystem::getTypeWitness(unsigned index) const {
  return TypeWitnesses[index];
}
//===--- Constraint.cpp - Constraint in the Type Checker --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the \c Constraint class and its related types,
// which is used by the constraint-based type checker to describe a
// constraint that must be solved.
//
//===----------------------------------------------------------------------===//
#include "Constraint.h"
#include "ConstraintSystem.h"
#include "swift/AST/Types.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;
using namespace constraints;

Constraint::Constraint(ConstraintKind Kind, Type First, Type Second, 
                       Identifier Member,
                       ConstraintLocator *locator)
  : Kind(Kind), HasRestriction(false), Types { First, Second, Member },
    Locator(locator)
{
  switch (Kind) {
  case ConstraintKind::Bind:
  case ConstraintKind::Equal:
  case ConstraintKind::TrivialSubtype:
  case ConstraintKind::Subtype:
  case ConstraintKind::Conversion:
  case ConstraintKind::Construction:
  case ConstraintKind::ConformsTo:
  case ConstraintKind::SelfObjectOfProtocol:
    assert(Member.empty() && "Relational constraint cannot have a member");
    break;
  case ConstraintKind::ApplicableFunction:
    assert(First->is<FunctionType>()
           && "The left-hand side type should be a function type");
    assert(Member.empty() && "Relational constraint cannot have a member");
    break;

  case ConstraintKind::TypeMember:
  case ConstraintKind::ValueMember:
    assert(!Member.empty() && "Member constraint has no member");
    break;

  case ConstraintKind::Archetype:
  case ConstraintKind::Class:
  case ConstraintKind::DynamicLookupValue:
    assert(Member.empty() && "Type property cannot have a member");
    assert(Second.isNull() && "Type property with second type");
    break;

  case ConstraintKind::BindOverload:
    llvm_unreachable("Wrong constructor for overload binding constraint");

  case ConstraintKind::Conjunction:
    llvm_unreachable("Conjunction constraints should use create()");

  case ConstraintKind::Disjunction:
    llvm_unreachable("Disjunction constraints should use create()");
  }
}

ProtocolDecl *Constraint::getProtocol() const {
  assert((Kind == ConstraintKind::ConformsTo ||
          Kind == ConstraintKind::SelfObjectOfProtocol)
          && "Not a conformance constraint");
  return Types.Second->castTo<ProtocolType>()->getDecl();
}

void Constraint::print(llvm::raw_ostream &Out, SourceManager *sm) const {
  if (Kind == ConstraintKind::Conjunction ||
      Kind == ConstraintKind::Disjunction) {
    bool isConjunction = (Kind == ConstraintKind::Conjunction);
    if (isConjunction)
      Out << "conjunction";
    else
      Out << "disjunction";
    if (Locator) {
      Out << " [[";
      Locator->dump(sm, Out);
      Out << "]]";
    }
    Out << ":";

    bool first = true;
    for (auto constraint : getNestedConstraints()) {
      if (first)
        first = false;
      else if (isConjunction)
        Out << " and ";
      else
        Out << " or ";

      constraint->print(Out, sm);
    }

    return;
  }

  Types.First->print(Out);

  bool skipSecond = false;

  switch (Kind) {
  case ConstraintKind::Bind: Out << " := "; break;
  case ConstraintKind::Equal: Out << " == "; break;
  case ConstraintKind::TrivialSubtype: Out << " <t "; break;
  case ConstraintKind::Subtype: Out << " < "; break;
  case ConstraintKind::Conversion: Out << " <c "; break;
  case ConstraintKind::Construction: Out << " <C "; break;
  case ConstraintKind::ConformsTo: Out << " conforms to "; break;
  case ConstraintKind::SelfObjectOfProtocol: Out << " Self type of "; break;
  case ConstraintKind::ApplicableFunction: Out << " ==Fn "; break;
  case ConstraintKind::BindOverload: {
    // FIXME: Better output here.
    skipSecond = true;
    break;
  }

  case ConstraintKind::ValueMember:
    Out << "[." << Types.Member.str() << ": value] == ";
    break;
  case ConstraintKind::TypeMember:
    Out << "[." << Types.Member.str() << ": type] == ";
    break;
  case ConstraintKind::Archetype:
    Out << " is an archetype";
    skipSecond = true;
    break;
  case ConstraintKind::Class:
    Out << " is a class";
    skipSecond = true;
    break;
  case ConstraintKind::DynamicLookupValue:
    Out << " is a DynamicLookup value";
    skipSecond = true;
    break;
  case ConstraintKind::Conjunction:
  case ConstraintKind::Disjunction:
    llvm_unreachable("Conjunction/disjunction handled above");
  }

  if (!skipSecond)
    Types.Second->print(Out);

  if (auto restriction = getRestriction()) {
    switch (*restriction) {
    case ConversionRestrictionKind::TupleToTuple:
      Out << " [tuple-to-tuple]";
      break;

    case ConversionRestrictionKind::ScalarToTuple:
      Out << " [scalar-to-tuple]";
      break;

    case ConversionRestrictionKind::TupleToScalar:
      Out << " [tuple-to-scalar]";
      break;

    case ConversionRestrictionKind::DeepEquality:
      Out << " [deep equality]";
      break;

    case ConversionRestrictionKind::Superclass:
      Out << " [superclass]";
      break;

    case ConversionRestrictionKind::LValueToRValue:
      Out << " [lvalue-to-rvalue]";
      break;

    case ConversionRestrictionKind::Existential:
      Out << " [existential]";
      break;

    case ConversionRestrictionKind::ValueToOptional:
      Out << " [value-to-optional]";
      break;

    case ConversionRestrictionKind::OptionalToOptional:
      Out << " [optional-to-optional]";
      break;
      
    case ConversionRestrictionKind::User:
      Out << " [user]";
      break;
    }
  }

  if (Locator) {
    Out << " [[";
    Locator->dump(sm, Out);
    Out << "]];";
  }
}

void Constraint::dump(SourceManager *sm) const {
  print(llvm::errs(), sm);
}

Constraint *Constraint::createConjunction(ConstraintSystem &cs,
                                          ArrayRef<Constraint *> constraints,
                                          ConstraintLocator *locator) {
  // Unwrap any conjunctions inside the conjunction constraint.
  bool unwrappedAny = false;
  SmallVector<Constraint *, 1> unwrapped;
  unsigned index = 0;
  for (auto constraint : constraints) {
    // If we have a nested conjunction, unwrap it.
    if (constraint->getKind() == ConstraintKind::Conjunction) {
      // If we haven't unwrapped anything before, copy all of the constraints
      // we skipped.
      if (!unwrappedAny) {
        unwrapped.append(constraints.begin(), constraints.begin() + index);
        unwrappedAny = true;
      }

      // Add all of the constraints in the conjunction.
      unwrapped.append(constraint->getNestedConstraints().begin(),
                       constraint->getNestedConstraints().end());
    } else if (unwrappedAny) {
      // Since we unwrapped constraints before, add this constraint.
      unwrapped.push_back(constraint);
    }

    // FIXME: If we find any disjunctions in here, should we distribute them?

    ++index;
  }

  // If we unwrapped anything, our list of constraints is the unwrapped list.
  if (unwrappedAny)
    constraints = unwrapped;

  assert(!constraints.empty() && "Empty conjunction constraint");

  // If there is a single constraint, this isn't a disjunction at all.
  if (constraints.size() == 1)
    return constraints.front();

  // Create the conjunction constraint.
  return new (cs) Constraint(ConstraintKind::Conjunction,
                             cs.allocateCopy(constraints), locator);

}

Constraint *Constraint::createDisjunction(ConstraintSystem &cs,
                                          ArrayRef<Constraint *> constraints,
                                          ConstraintLocator *locator) {
  // Unwrap any disjunctions inside the disjunction constraint; we only allow
  // disjunctions at the top level.
  bool unwrappedAny = false;
  SmallVector<Constraint *, 1> unwrapped;
  unsigned index = 0;
  for (auto constraint : constraints) {
    // If we have a nested disjunction, unwrap it.
    if (constraint->getKind() == ConstraintKind::Disjunction) {
      // If we haven't unwrapped anything before, copy all of the constraints
      // we skipped.
      if (!unwrappedAny) {
        unwrapped.append(constraints.begin(), constraints.begin() + index);
        unwrappedAny = true;
      }

      // Add all of the constraints in the disjunction.
      unwrapped.append(constraint->getNestedConstraints().begin(),
                       constraint->getNestedConstraints().end());
    } else if (unwrappedAny) {
      // Since we unwrapped constraints before, add this constraint.
      unwrapped.push_back(constraint);
    }
    ++index;
  }

  // If we unwrapped anything, our list of constraints is the unwrapped list.
  if (unwrappedAny)
    constraints = unwrapped;

  assert(!constraints.empty() && "Empty disjunction constraint");

  // If there is a single constraint, this isn't a disjunction at all.
  if (constraints.size() == 1)
    return constraints.front();

  // Create the disjunction constraint.
  return new (cs) Constraint(ConstraintKind::Disjunction,
                             cs.allocateCopy(constraints), locator);
}

void *Constraint::operator new(size_t bytes, ConstraintSystem& cs,
                               size_t alignment) {
  return ::operator new (bytes, cs, alignment);
}

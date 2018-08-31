//===--- ConstraintLocator.cpp - Constraint Locator -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the \c ConstraintLocator class and its related types,
// which is used by the constraint-based type checker to describe how
// a particular constraint was derived.
//
//===----------------------------------------------------------------------===//
#include "ConstraintLocator.h"
#include "ConstraintSystem.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace constraints;

void ConstraintLocator::Profile(llvm::FoldingSetNodeID &id, Expr *anchor,
                                ArrayRef<PathElement> path) {
  id.AddPointer(anchor);
  id.AddInteger(path.size());
  for (auto elt : path) {
    id.AddInteger(elt.getKind());
    switch (elt.getKind()) {
    case Archetype:
      id.AddPointer(elt.getArchetype()->getCanonicalType().getPointer());
      break;

    case Requirement:
      id.AddPointer(elt.getRequirement());
      break;

    case Witness:
      id.AddPointer(elt.getWitness());
      break;

    case AssociatedType:
      id.AddPointer(elt.getAssociatedType());
      break;

    case ApplyArgument:
    case ApplyFunction:
    case FunctionArgument:
    case FunctionResult:
    case OptionalPayload:
    case Member:
    case MemberRefBase:
    case UnresolvedMember:
    case SubscriptMember:
    case ConstructorMember:
    case LValueConversion:
    case RValueAdjustment:
    case ClosureResult:
    case ParentType:
    case InstanceType:
    case SequenceIteratorProtocol:
    case GeneratorElementType:
    case AutoclosureResult:
    case GenericArgument:
    case NamedTupleElement:
    case TupleElement:
    case ApplyArgToParam:
    case OpenedGeneric:
    case KeyPathComponent:
    case ConditionalRequirement:
    case TypeParameterRequirement:
    case ImplicitlyUnwrappedDisjunctionChoice:
    case DynamicLookupResult:
    case ContextualType:
      if (unsigned numValues = numNumericValuesInPathElement(elt.getKind())) {
        id.AddInteger(elt.getValue());
        if (numValues > 1)
          id.AddInteger(elt.getValue2());
      }
      break;
    }
  }
}

void ConstraintLocator::dump(SourceManager *sm) {
  dump(sm, llvm::errs());
  llvm::errs() << "\n";
}

void ConstraintLocator::dump(ConstraintSystem *CS) {
  dump(&CS->TC.Context.SourceMgr, llvm::errs());
  llvm::errs() << "\n";
}


void ConstraintLocator::dump(SourceManager *sm, raw_ostream &out) {
  out << "locator@" << (void*) this << " [";

  if (anchor) {
    out << Expr::getKindName(anchor->getKind());
    if (sm) {
      out << '@';
      anchor->getLoc().print(out, *sm);
    }
  }

  for (auto elt : getPath()) {
    out << " -> ";
    switch (elt.getKind()) {
    case Archetype:
      out << "archetype '" << elt.getArchetype()->getString() << "'";
      break;

    case AssociatedType:
      out << "associated type '"
          << elt.getAssociatedType()->getNameStr() << "'";
      break;

    case ApplyArgument:
      out << "apply argument";
      break;

    case ApplyFunction:
      out << "apply function";
      break;

    case OptionalPayload:
      out << "optional payload";
      break;

    case ApplyArgToParam:
      out << "comparing call argument #" << llvm::utostr(elt.getValue())
          << " to parameter #" << llvm::utostr(elt.getValue2());
      break;
        
    case ClosureResult:
      out << "closure result";
      break;

    case ConstructorMember:
      out << "constructor member";
      break;

    case FunctionArgument:
      out << "function argument";
      break;

    case FunctionResult:
      out << "function result";
      break;

    case GeneratorElementType:
      out << "generator element type";
      break;

    case GenericArgument:
      out << "generic argument #" << llvm::utostr(elt.getValue());
      break;

    case InstanceType:
      out << "instance type";
      break;

    case AutoclosureResult:
      out << "@autoclosure result";
      break;

    case Member:
      out << "member";
      break;

    case MemberRefBase:
      out << "member reference base";
      break;

    case NamedTupleElement:
      out << "named tuple element #" << llvm::utostr(elt.getValue());
      break;

    case UnresolvedMember:
      out << "unresolved member";
      break;
        
    case ParentType:
      out << "parent type";
      break;

    case LValueConversion:
      out << "@lvalue-to-inout conversion";
      break;

    case RValueAdjustment:
      out << "rvalue adjustment";
      break;

    case SequenceIteratorProtocol:
      out << "sequence iterator type";
      break;

    case SubscriptMember:
      out << "subscript member";
      break;

    case TupleElement:
      out << "tuple element #" << llvm::utostr(elt.getValue());
      break;

    case KeyPathComponent:
      out << "key path component #" << llvm::utostr(elt.getValue());
      break;

    case Requirement:
      out << "requirement ";
      elt.getRequirement()->dumpRef(out);
      break;

    case Witness:
      out << "witness ";
      elt.getWitness()->dumpRef(out);
      break;
        
    case OpenedGeneric:
      out << "opened generic";
      break;

    case ConditionalRequirement:
      out << "conditional requirement #" << llvm::utostr(elt.getValue());
      break;

    case TypeParameterRequirement: {
      out << "type parameter requirement #" << llvm::utostr(elt.getValue())
          << " (";
      switch (static_cast<RequirementKind>(elt.getValue2())) {
      case RequirementKind::Conformance:
        out << "conformance";
        break;
      case RequirementKind::Superclass:
        out << "superclass";
        break;
      case RequirementKind::SameType:
        out << "same-type";
        break;
      case RequirementKind::Layout:
        out << "layout";
        break;
      }
      out << ")";
      break;
    }

    case ImplicitlyUnwrappedDisjunctionChoice:
      out << "implicitly unwrapped disjunction choice";
      break;

    case DynamicLookupResult:
      out << "dynamic lookup result";
      break;

    case ContextualType:
      out << "contextual type";
      break;
    }
  }

  out << ']';
}

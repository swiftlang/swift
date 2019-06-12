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
    case GenericParameter:
      id.AddPointer(elt.getGenericParameter());
      break;

    case Requirement:
      id.AddPointer(elt.getRequirement());
      break;

    case Witness:
      id.AddPointer(elt.getWitness());
      break;

    case KeyPathDynamicMember:
      id.AddPointer(elt.getKeyPath());
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
    case SequenceElementType:
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
    case SynthesizedArgument:
    case KeyPathType:
    case KeyPathRoot:
    case KeyPathValue:
    case KeyPathComponentResult:
      if (unsigned numValues = numNumericValuesInPathElement(elt.getKind())) {
        id.AddInteger(elt.getValue());
        if (numValues > 1)
          id.AddInteger(elt.getValue2());
      }
      break;
    }
  }
}

/// Determine whether given locator points to the subscript reference
/// e.g. `foo[0]` or `\Foo.[0]`
bool ConstraintLocator::isSubscriptMemberRef() const {
  auto *anchor = getAnchor();
  auto path = getPath();

  if (!anchor || path.empty())
    return false;

  return path.back().getKind() == ConstraintLocator::SubscriptMember;
}

bool ConstraintLocator::isKeyPathType() const {
  auto *anchor = getAnchor();
  auto path = getPath();
  // The format of locator should be `<keypath expr> -> key path type`
  if (!anchor || !isa<KeyPathExpr>(anchor) || path.size() != 1)
    return false;
  return path.back().getKind() == ConstraintLocator::KeyPathType;
}

bool ConstraintLocator::isKeyPathRoot() const {
  auto *anchor = getAnchor();
  auto path = getPath();

  if (!anchor || path.empty())
    return false;

  return path.back().getKind() == ConstraintLocator::KeyPathRoot;
}

bool ConstraintLocator::isKeyPathValue() const {
  auto *anchor = getAnchor();
  auto path = getPath();

  if (!anchor || path.empty())
    return false;

  return path.back().getKind() == ConstraintLocator::KeyPathValue;
}

bool ConstraintLocator::isResultOfKeyPathDynamicMemberLookup() const {
  return llvm::any_of(getPath(), [](const LocatorPathElt &elt) {
    return elt.isKeyPathDynamicMember();
  });
}

bool ConstraintLocator::isKeyPathSubscriptComponent() const {
  auto *anchor = getAnchor();
  auto *KPE = dyn_cast_or_null<KeyPathExpr>(anchor);
  if (!KPE)
    return false;

  using ComponentKind = KeyPathExpr::Component::Kind;
  return llvm::any_of(getPath(), [&](const LocatorPathElt &elt) {
    if (!elt.isKeyPathComponent())
      return false;

    auto index = elt.getValue();
    auto &component = KPE->getComponents()[index];
    return component.getKind() == ComponentKind::Subscript ||
           component.getKind() == ComponentKind::UnresolvedSubscript;
  });
}

bool ConstraintLocator::isForKeyPathDynamicMemberLookup() const {
  auto path = getPath();
  return !path.empty() && path.back().isKeyPathDynamicMember();
}

bool ConstraintLocator::isForKeyPathComponent() const {
  return llvm::any_of(getPath(), [&](const LocatorPathElt &elt) {
    return elt.isKeyPathComponent();
  });
}

static bool isLastElement(const ConstraintLocator *locator,
                          ConstraintLocator::PathElementKind expectedKind) {
  auto path = locator->getPath();
  return !path.empty() && path.back().getKind() == expectedKind;
}

bool ConstraintLocator::isForGenericParameter() const {
  return isLastElement(this, ConstraintLocator::GenericParameter);
}

bool ConstraintLocator::isForSequenceElementType() const {
  return isLastElement(this, ConstraintLocator::SequenceElementType);
}

bool ConstraintLocator::isForContextualType() const {
  return isLastElement(this, ConstraintLocator::ContextualType);
}

GenericTypeParamType *ConstraintLocator::getGenericParameter() const {
  assert(isForGenericParameter());
  auto path = getPath();
  return path.back().getGenericParameter();
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

  auto dumpReqKind = [&out](RequirementKind kind) {
    out << " (";
    switch (kind) {
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
  };

  for (auto elt : getPath()) {
    out << " -> ";
    switch (elt.getKind()) {
    case GenericParameter:
      out << "generic parameter '" << elt.getGenericParameter()->getString() << "'";
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

    case SequenceElementType:
      out << "sequence element type";
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
      dumpReqKind(static_cast<RequirementKind>(elt.getValue2()));
      break;

    case TypeParameterRequirement: {
      out << "type parameter requirement #" << llvm::utostr(elt.getValue());
      dumpReqKind(static_cast<RequirementKind>(elt.getValue2()));
      break;
    }

    case ImplicitlyUnwrappedDisjunctionChoice:
      out << "implicitly unwrapped disjunction choice";
      break;

    case DynamicLookupResult:
      out << "dynamic lookup result";
      break;

    case ContextualType:
      if (elt.isResultOfSingleExprFunction())
        out << "expected result type of the function with a single expression";
      else
        out << "contextual type";
      break;

    case SynthesizedArgument:
      out << "synthesized argument #" << llvm::utostr(elt.getValue());
      break;

    case KeyPathDynamicMember:
      out << "key path dynamic member lookup";
      break;

    case KeyPathType:
      out << "key path type";
      break;

    case KeyPathRoot:
      out << "key path root";
      break;

    case KeyPathValue:
      out << "key path value";
      break;

    case KeyPathComponentResult:
      out << "key path component result";
      break;
    }
  }
  out << ']';
}

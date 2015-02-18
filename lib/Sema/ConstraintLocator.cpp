//===--- ConstraintLocator.cpp - Constraint Locator -----------------------===//
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
// This file implements the \c ConstraintLocator class and its related types,
// which is used by the constraint-based type checker to describe how
// a particular constraint was derived.
//
//===----------------------------------------------------------------------===//
#include "ConstraintLocator.h"
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
    unsigned numValues = numNumericValuesInPathElement(elt.getKind());
    if (numValues > 0) {
      id.AddInteger(elt.getValue());
      if (numValues > 1)
        id.AddInteger(elt.getValue2());
    }
    else if (elt.getKind() == ConstraintLocator::Archetype)
      id.AddPointer(elt.getArchetype()->getCanonicalType().getPointer());
  }
}

void ConstraintLocator::dump(SourceManager *sm) {
  dump(sm, llvm::errs());
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
    case AddressOf:
      out << "address of";
      break;

    case ArrayElementType:
      out << "array element";
      break;

    case Archetype:
      out << "archetype '" << elt.getArchetype()->getString() << "'";
      break;
        
    case ApplyArgument:
      out << "apply argument";
      break;

    case ApplyFunction:
      out << "apply function";
      break;

    case ApplyArgToParam:
      out << "comparing call argument #" << llvm::utostr(elt.getValue())
          << " to parameter #" << llvm::utostr(elt.getValue2());
      break;

    case AssignDest:
      out << "assignment destination";
      break;

    case AssignSource:
      out << "assignment source";
      break;
        
    case CheckedCastOperand:
      out << "checked cast operand";
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

    case GenericArgument:
      out << "generic argument #" << llvm::utostr(elt.getValue());
      break;

    case IfElse:
      out << "'else' branch of ternary" ;
      break;

    case IfThen:
      out << "'then' branch of ternary" ;
      break;

    case InstanceType:
      out << "instance type";
      break;

    case InterpolationArgument:
      out << "interpolation argument #" << llvm::utostr(elt.getValue());
      break;

    case Load:
      out << "load";
      break;

    case LvalueObjectType:
      out << "lvalue object type";
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

    case RvalueAdjustment:
      out << "rvalue adjustment";
      break;

    case ScalarToTuple:
      out << "scalar to tuple";
      break;

    case SubscriptIndex:
      out << "subscript index";
      break;

    case SubscriptMember:
      out << "subscript member";
      break;

    case SubscriptResult:
      out << "subscript result";
      break;

    case TupleElement:
      out << "tuple element #" << llvm::utostr(elt.getValue());
      break;

    case Witness:
      out << "witness ";
      elt.getWitness()->dumpRef(out);
      break;
    }
  }

  out << ']';
}


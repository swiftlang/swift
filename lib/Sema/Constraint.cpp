//===--- Constraint.cpp - Constraint in the Type Checker ------------------===//
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
//
// This file implements the \c Constraint class and its related types,
// which is used by the constraint-based type checker to describe a
// constraint that must be solved.
//
//===----------------------------------------------------------------------===//
#include "Constraint.h"
#include "ConstraintSystem.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Compiler.h"
#include "swift/Basic/Fallthrough.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
#include <algorithm>

using namespace swift;
using namespace constraints;

Constraint::Constraint(ConstraintKind kind, ArrayRef<Constraint *> constraints,
                       ConstraintLocator *locator, 
                       ArrayRef<TypeVariableType *> typeVars)
  : Kind(kind), HasRestriction(false), HasFix(false), IsActive(false),
    RememberChoice(false), IsFavored(false), NumTypeVariables(typeVars.size()),
    Nested(constraints), Locator(locator)
{
  assert(kind == ConstraintKind::Disjunction);
  std::uninitialized_copy(typeVars.begin(), typeVars.end(),
                          getTypeVariablesBuffer().begin());
}

Constraint::Constraint(ConstraintKind Kind, Type First, Type Second, 
                       ConstraintLocator *locator,
                       ArrayRef<TypeVariableType *> typeVars)
  : Kind(Kind), HasRestriction(false), HasFix(false), IsActive(false),
    RememberChoice(false), IsFavored(false), NumTypeVariables(typeVars.size()),
    Types { First, Second }, Locator(locator)
{
  switch (Kind) {
  case ConstraintKind::Bind:
  case ConstraintKind::Equal:
  case ConstraintKind::BindParam:
  case ConstraintKind::BindToPointerType:
  case ConstraintKind::Subtype:
  case ConstraintKind::Conversion:
  case ConstraintKind::BridgingConversion:
  case ConstraintKind::ArgumentConversion:
  case ConstraintKind::ArgumentTupleConversion:
  case ConstraintKind::OperatorArgumentTupleConversion:
  case ConstraintKind::OperatorArgumentConversion:
  case ConstraintKind::ConformsTo:
  case ConstraintKind::Layout:
  case ConstraintKind::LiteralConformsTo:
  case ConstraintKind::CheckedCast:
  case ConstraintKind::SelfObjectOfProtocol:
  case ConstraintKind::DynamicTypeOf:
  case ConstraintKind::EscapableFunctionOf:
  case ConstraintKind::OptionalObject:
    assert(!First.isNull());
    assert(!Second.isNull());
    break;
  case ConstraintKind::ApplicableFunction:
    assert(First->is<FunctionType>()
           && "The left-hand side type should be a function type");
    break;

  case ConstraintKind::ValueMember:
  case ConstraintKind::UnresolvedValueMember:
    llvm_unreachable("Wrong constructor for member constraint");

  case ConstraintKind::Defaultable:
    assert(!First.isNull());
    assert(!Second.isNull());
    break;

  case ConstraintKind::BindOverload:
    llvm_unreachable("Wrong constructor for overload binding constraint");

  case ConstraintKind::Disjunction:
    llvm_unreachable("Disjunction constraints should use create()");
  }

  std::copy(typeVars.begin(), typeVars.end(), getTypeVariablesBuffer().begin());
}

Constraint::Constraint(ConstraintKind kind, Type first, Type second,
                       DeclName member, DeclContext *useDC,
                       FunctionRefKind functionRefKind,
                       ConstraintLocator *locator,
                       ArrayRef<TypeVariableType *> typeVars)
  : Kind(kind), HasRestriction(false), HasFix(false), IsActive(false),
    RememberChoice(false), IsFavored(false), NumTypeVariables(typeVars.size()),
    Member { first, second, member, useDC }, Locator(locator)
{
  assert(kind == ConstraintKind::ValueMember ||
         kind == ConstraintKind::UnresolvedValueMember);
  TheFunctionRefKind = static_cast<unsigned>(functionRefKind);
  assert(getFunctionRefKind() == functionRefKind);
  assert(member && "Member constraint has no member");
  assert(useDC && "Member constraint has no use DC");

  std::copy(typeVars.begin(), typeVars.end(), getTypeVariablesBuffer().begin());
}

Constraint::Constraint(Type type, OverloadChoice choice, DeclContext *useDC,
                       ConstraintLocator *locator,
                       ArrayRef<TypeVariableType *> typeVars)
  : Kind(ConstraintKind::BindOverload),
    HasRestriction(false), HasFix(false), IsActive(false),
    RememberChoice(false), IsFavored(false), NumTypeVariables(typeVars.size()),
    Overload{type, choice, useDC}, Locator(locator)
{ 
  std::copy(typeVars.begin(), typeVars.end(), getTypeVariablesBuffer().begin());
}

Constraint::Constraint(ConstraintKind kind, 
                       ConversionRestrictionKind restriction,
                       Type first, Type second, ConstraintLocator *locator,
                       ArrayRef<TypeVariableType *> typeVars)
    : Kind(kind), Restriction(restriction),
      HasRestriction(true), HasFix(false), IsActive(false),
      RememberChoice(false), IsFavored(false), NumTypeVariables(typeVars.size()),
      Types{ first, second }, Locator(locator)
{
  assert(!first.isNull());
  assert(!second.isNull());
  std::copy(typeVars.begin(), typeVars.end(), getTypeVariablesBuffer().begin());
}

Constraint::Constraint(ConstraintKind kind, Fix fix,
                       Type first, Type second, ConstraintLocator *locator,
                       ArrayRef<TypeVariableType *> typeVars)
  : Kind(kind), FixData(fix.getData()), TheFix(fix.getKind()),
    HasRestriction(false), HasFix(true),
    IsActive(false), RememberChoice(false), IsFavored(false),
    NumTypeVariables(typeVars.size()),
    Types{ first, second }, Locator(locator)
{
  assert(!first.isNull());
  assert(!second.isNull());
  std::copy(typeVars.begin(), typeVars.end(), getTypeVariablesBuffer().begin());
}

ProtocolDecl *Constraint::getProtocol() const {
  assert((Kind == ConstraintKind::ConformsTo ||
          Kind == ConstraintKind::LiteralConformsTo ||
          Kind == ConstraintKind::SelfObjectOfProtocol)
          && "Not a conformance constraint");
  return Types.Second->castTo<ProtocolType>()->getDecl();
}

Constraint *Constraint::clone(ConstraintSystem &cs) const {
  switch (getKind()) {
  case ConstraintKind::Bind:
  case ConstraintKind::Equal:
  case ConstraintKind::BindParam:
  case ConstraintKind::BindToPointerType:
  case ConstraintKind::Subtype:
  case ConstraintKind::Conversion:
  case ConstraintKind::BridgingConversion:
  case ConstraintKind::ArgumentConversion:
  case ConstraintKind::ArgumentTupleConversion:
  case ConstraintKind::OperatorArgumentTupleConversion:
  case ConstraintKind::OperatorArgumentConversion:
  case ConstraintKind::ConformsTo:
  case ConstraintKind::Layout:
  case ConstraintKind::LiteralConformsTo:
  case ConstraintKind::CheckedCast:
  case ConstraintKind::DynamicTypeOf:
  case ConstraintKind::EscapableFunctionOf:
  case ConstraintKind::SelfObjectOfProtocol:
  case ConstraintKind::ApplicableFunction:
  case ConstraintKind::OptionalObject:
  case ConstraintKind::Defaultable:
    return create(cs, getKind(), getFirstType(), getSecondType(), getLocator());

  case ConstraintKind::BindOverload:
    return createBindOverload(cs, getFirstType(), getOverloadChoice(),
                              getOverloadUseDC(), getLocator());

  case ConstraintKind::ValueMember:
  case ConstraintKind::UnresolvedValueMember:
    return createMember(cs, getKind(), getFirstType(), getSecondType(),
                        getMember(), getMemberUseDC(), getFunctionRefKind(),
                        getLocator());

  case ConstraintKind::Disjunction:
    return createDisjunction(cs, getNestedConstraints(), getLocator());
  }

  llvm_unreachable("Unhandled ConstraintKind in switch.");
}

void Constraint::print(llvm::raw_ostream &Out, SourceManager *sm) const {
  if (Kind == ConstraintKind::Disjunction) {
    Out << "disjunction";
    if (shouldRememberChoice())
      Out << " (remembered)";
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
      else
        Out << " or ";

      constraint->print(Out, sm);
    }

    return;
  }

  getFirstType()->print(Out);

  bool skipSecond = false;

  switch (Kind) {
  case ConstraintKind::Bind: Out << " bind "; break;
  case ConstraintKind::Equal: Out << " equal "; break;
  case ConstraintKind::BindParam: Out << " bind param "; break;
  case ConstraintKind::BindToPointerType: Out << " bind to pointer "; break;
  case ConstraintKind::Subtype: Out << " subtype "; break;
  case ConstraintKind::Conversion: Out << " conv "; break;
  case ConstraintKind::BridgingConversion: Out << " bridging conv "; break;
  case ConstraintKind::ArgumentConversion: Out << " arg conv "; break;
  case ConstraintKind::ArgumentTupleConversion:
      Out << " arg tuple conv "; break;
  case ConstraintKind::OperatorArgumentTupleConversion:
      Out << " operator arg tuple conv "; break;
  case ConstraintKind::OperatorArgumentConversion:
      Out << " operator arg conv "; break;
  case ConstraintKind::ConformsTo: Out << " conforms to "; break;
  case ConstraintKind::Layout: Out << " layout of "; break;
  case ConstraintKind::LiteralConformsTo: Out << " literal conforms to "; break;
  case ConstraintKind::CheckedCast: Out << " checked cast to "; break;
  case ConstraintKind::SelfObjectOfProtocol: Out << " Self type of "; break;
  case ConstraintKind::ApplicableFunction: Out << " applicable fn "; break;
  case ConstraintKind::DynamicTypeOf: Out << " dynamicType type of "; break;
  case ConstraintKind::EscapableFunctionOf: Out << " @escaping type of "; break;
  case ConstraintKind::OptionalObject:
      Out << " optional with object type "; break;
  case ConstraintKind::BindOverload: {
    Out << " bound to ";
    auto overload = getOverloadChoice();
    auto printDecl = [&] {
      auto decl = overload.getDecl();
      decl->dumpRef(Out);
      Out << " : " << decl->getInterfaceType();
      if (!sm || !decl->getLoc().isValid()) return;
      Out << " at ";
      decl->getLoc().print(Out, *sm);
    };

    switch (overload.getKind()) {
    case OverloadChoiceKind::Decl:
      Out << "decl ";
      printDecl();
      break;
    case OverloadChoiceKind::TypeDecl:
      Out << "type decl ";
      printDecl();
      break;
    case OverloadChoiceKind::DeclViaDynamic:
      Out << "decl-via-dynamic ";
      printDecl();
      break;
    case OverloadChoiceKind::DeclViaBridge:
      Out << "decl-via-bridge ";
      printDecl();
      break;
    case OverloadChoiceKind::DeclViaUnwrappedOptional:
      Out << "decl-via-unwrapped-optional ";
      printDecl();
      break;
    case OverloadChoiceKind::BaseType:
      Out << "base type";
      break;
    case OverloadChoiceKind::TupleIndex:
      Out << "tuple index " << overload.getTupleIndex();
      break;
    }

    skipSecond = true;
    break;
  }

  case ConstraintKind::ValueMember:
    Out << "[." << Member.Member << ": value] == ";
    break;
  case ConstraintKind::UnresolvedValueMember:
    Out << "[(implicit) ." << Member.Member << ": value] == ";
    break;
  case ConstraintKind::Defaultable:
    Out << " can default to ";
    break;
  case ConstraintKind::Disjunction:
    llvm_unreachable("disjunction handled above");
  }

  if (!skipSecond)
    getSecondType()->print(Out);

  if (auto restriction = getRestriction()) {
    Out << ' ' << getName(*restriction);
  }

  if (auto fix = getFix()) {
    Out << ' ';
    fix->print(Out, nullptr);
  }

  if (Locator) {
    Out << " [[";
    Locator->dump(sm, Out);
    Out << "]];";
  }
}

void Constraint::dump(SourceManager *sm) const {
  print(llvm::errs(), sm);
  llvm::errs() << "\n";
}

void Constraint::dump(ConstraintSystem *CS) const {
  // Print all type variables as $T0 instead of _ here.
  llvm::SaveAndRestore<bool> X(CS->getASTContext().LangOpts.
                               DebugConstraintSolver, true);
  // Disable MSVC warning: only for use within the debugger.
#if SWIFT_COMPILER_IS_MSVC
#pragma warning(push)
#pragma warning(disable: 4996)
#endif
  dump(&CS->getASTContext().SourceMgr);
#if SWIFT_COMPILER_IS_MSVC
#pragma warning(pop)
#endif
}


StringRef swift::constraints::getName(ConversionRestrictionKind kind) {
  switch (kind) {
  case ConversionRestrictionKind::TupleToTuple:
    return "[tuple-to-tuple]";
  case ConversionRestrictionKind::ScalarToTuple:
    return "[scalar-to-tuple]";
  case ConversionRestrictionKind::DeepEquality:
    return "[deep equality]";
  case ConversionRestrictionKind::Superclass:
    return "[superclass]";
  case ConversionRestrictionKind::LValueToRValue:
    return "[lvalue-to-rvalue]";
  case ConversionRestrictionKind::Existential:
    return "[existential]";
  case ConversionRestrictionKind::MetatypeToExistentialMetatype:
    return "[metatype-to-existential-metatype]";
  case ConversionRestrictionKind::ValueToOptional:
    return "[value-to-optional]";
  case ConversionRestrictionKind::OptionalToOptional:
    return "[optional-to-optional]";
  case ConversionRestrictionKind::ClassMetatypeToAnyObject:
    return "[class-metatype-to-object]";
  case ConversionRestrictionKind::ExistentialMetatypeToAnyObject:
    return "[existential-metatype-to-object]";
  case ConversionRestrictionKind::ProtocolMetatypeToProtocolClass:
    return "[protocol-metatype-to-object]";
  case ConversionRestrictionKind::ArrayToPointer:
    return "[array-to-pointer]";
  case ConversionRestrictionKind::StringToPointer:
    return "[string-to-pointer]";
  case ConversionRestrictionKind::InoutToPointer:
    return "[inout-to-pointer]";
  case ConversionRestrictionKind::PointerToPointer:
    return "[pointer-to-pointer]";
  case ConversionRestrictionKind::ForceUnchecked:
    return "[force-unchecked]";
  case ConversionRestrictionKind::ArrayUpcast:
    return "[array-upcast]";
  case ConversionRestrictionKind::DictionaryUpcast:
    return "[dictionary-upcast]";
  case ConversionRestrictionKind::SetUpcast:
    return "[set-upcast]";
  case ConversionRestrictionKind::HashableToAnyHashable:
    return "[hashable-to-anyhashable]";
  case ConversionRestrictionKind::CFTollFreeBridgeToObjC:
    return "[cf-toll-free-bridge-to-objc]";
  case ConversionRestrictionKind::ObjCTollFreeBridgeToCF:
    return "[objc-toll-free-bridge-to-cf]";
  }
  llvm_unreachable("bad conversion restriction kind");
}

Fix Fix::getForcedDowncast(ConstraintSystem &cs, Type toType) {
  unsigned index = cs.FixedTypes.size();
  cs.FixedTypes.push_back(toType);
  return Fix(FixKind::ForceDowncast, index);
}

Type Fix::getTypeArgument(ConstraintSystem &cs) const {
  assert(getKind() == FixKind::ForceDowncast);
  return cs.FixedTypes[Data];
}

StringRef Fix::getName(FixKind kind) {
  switch (kind) {
  case FixKind::None:
    return "prevent fixes";
  case FixKind::ForceOptional:
    return "fix: force optional";
  case FixKind::OptionalChaining:
    return "fix: optional chaining";
  case FixKind::ForceDowncast:
    return "fix: force downcast";
  case FixKind::AddressOf:
    return "fix: add address-of";
  case FixKind::CoerceToCheckedCast:
    return "fix: as to as!";
  }

  llvm_unreachable("Unhandled FixKind in switch.");
}

void Fix::print(llvm::raw_ostream &Out, ConstraintSystem *cs) const {
  Out << '[' << getName(getKind());

  if (getKind() == FixKind::ForceDowncast && cs) {
    Out << " as! ";
    Out << getTypeArgument(*cs).getString();
  }
  Out << ']';
}

void Fix::dump(ConstraintSystem *cs) const {
  print(llvm::errs(), cs);
  llvm::errs() << "\n";
}

/// Recursively gather the set of type variables referenced by this constraint.
static void
gatherReferencedTypeVars(Constraint *constraint,
                         SmallVectorImpl<TypeVariableType *> &typeVars) {
  switch (constraint->getKind()) {
  case ConstraintKind::Disjunction:
    for (auto nested : constraint->getNestedConstraints())
      gatherReferencedTypeVars(nested, typeVars);
    return;

  case ConstraintKind::ApplicableFunction:
  case ConstraintKind::Bind:
  case ConstraintKind::BindParam:
  case ConstraintKind::BindToPointerType:
  case ConstraintKind::ArgumentConversion:
  case ConstraintKind::Conversion:
  case ConstraintKind::BridgingConversion:
  case ConstraintKind::ArgumentTupleConversion:
  case ConstraintKind::OperatorArgumentTupleConversion:
  case ConstraintKind::OperatorArgumentConversion:
  case ConstraintKind::CheckedCast:
  case ConstraintKind::Equal:
  case ConstraintKind::Subtype:
  case ConstraintKind::UnresolvedValueMember:
  case ConstraintKind::ValueMember:
  case ConstraintKind::DynamicTypeOf:
  case ConstraintKind::EscapableFunctionOf:
  case ConstraintKind::OptionalObject:
  case ConstraintKind::Defaultable:
    constraint->getSecondType()->getTypeVariables(typeVars);
    SWIFT_FALLTHROUGH;

  case ConstraintKind::BindOverload:
  case ConstraintKind::ConformsTo:
  case ConstraintKind::Layout:
  case ConstraintKind::LiteralConformsTo:
  case ConstraintKind::SelfObjectOfProtocol:
    constraint->getFirstType()->getTypeVariables(typeVars);

    // Special case: the base type of an overloading binding.
    if (constraint->getKind() == ConstraintKind::BindOverload) {
      if (auto baseType = constraint->getOverloadChoice().getBaseType()) {
        baseType->getTypeVariables(typeVars);
      }
    }

    break;
  }
}

/// Unique the given set of type variables.
static void uniqueTypeVariables(SmallVectorImpl<TypeVariableType *> &typeVars) {
  // Remove any duplicate type variables.
  llvm::SmallPtrSet<TypeVariableType *, 4> knownTypeVars;
  typeVars.erase(std::remove_if(typeVars.begin(), typeVars.end(),
                                [&](TypeVariableType *typeVar) {
                                  return !knownTypeVars.insert(typeVar).second;
                                }),
                 typeVars.end());
}

Constraint *Constraint::create(ConstraintSystem &cs, ConstraintKind kind, 
                               Type first, Type second,
                               ConstraintLocator *locator) {
  // Collect type variables.
  SmallVector<TypeVariableType *, 4> typeVars;
  if (first->hasTypeVariable())
    first->getTypeVariables(typeVars);
  if (second && second->hasTypeVariable())
    second->getTypeVariables(typeVars);
  uniqueTypeVariables(typeVars);

  // Conformance constraints expect a protocol on the right-hand side, always.
  assert((kind != ConstraintKind::ConformsTo &&
          kind != ConstraintKind::LiteralConformsTo &&
          kind != ConstraintKind::SelfObjectOfProtocol) ||
         second->is<ProtocolType>());

  // Create the constraint.
  unsigned size = totalSizeToAlloc<TypeVariableType*>(typeVars.size());
  void *mem = cs.getAllocator().Allocate(size, alignof(Constraint));
  return new (mem) Constraint(kind, first, second, locator, typeVars);
}

Constraint *Constraint::createMember(ConstraintSystem &cs, ConstraintKind kind, 
                                     Type first, Type second, DeclName member,
                                     DeclContext *useDC,
                                     FunctionRefKind functionRefKind,
                                     ConstraintLocator *locator) {
  // Collect type variables.
  SmallVector<TypeVariableType *, 4> typeVars;
  if (first->hasTypeVariable())
    first->getTypeVariables(typeVars);
  if (second->hasTypeVariable())
    second->getTypeVariables(typeVars);
  uniqueTypeVariables(typeVars);

  // Create the constraint.
  unsigned size = totalSizeToAlloc<TypeVariableType*>(typeVars.size());
  void *mem = cs.getAllocator().Allocate(size, alignof(Constraint));
  return new (mem) Constraint(kind, first, second, member, useDC,
                              functionRefKind, locator, typeVars);
}

Constraint *Constraint::createBindOverload(ConstraintSystem &cs, Type type, 
                                           OverloadChoice choice, 
                                           DeclContext *useDC,
                                           ConstraintLocator *locator) {
  // Collect type variables.
  SmallVector<TypeVariableType *, 4> typeVars;
  if (type->hasTypeVariable())
    type->getTypeVariables(typeVars);
  if (auto baseType = choice.getBaseType()) {
    baseType->getTypeVariables(typeVars);
  }

  // Create the constraint.
  unsigned size = totalSizeToAlloc<TypeVariableType*>(typeVars.size());
  void *mem = cs.getAllocator().Allocate(size, alignof(Constraint));
  return new (mem) Constraint(type, choice, useDC, locator, typeVars);
}

Constraint *Constraint::createRestricted(ConstraintSystem &cs, 
                                         ConstraintKind kind, 
                                         ConversionRestrictionKind restriction,
                                         Type first, Type second, 
                                         ConstraintLocator *locator) {
  // Collect type variables.
  SmallVector<TypeVariableType *, 4> typeVars;
  if (first->hasTypeVariable())
    first->getTypeVariables(typeVars);
  if (second->hasTypeVariable())
    second->getTypeVariables(typeVars);
  uniqueTypeVariables(typeVars);

  // Create the constraint.
  unsigned size = totalSizeToAlloc<TypeVariableType*>(typeVars.size());
  void *mem = cs.getAllocator().Allocate(size, alignof(Constraint));
  return new (mem) Constraint(kind, restriction, first, second, locator,
                              typeVars);
}

Constraint *Constraint::createFixed(ConstraintSystem &cs, ConstraintKind kind,
                                    Fix fix,
                                    Type first, Type second,
                                    ConstraintLocator *locator) {
  // Collect type variables.
  SmallVector<TypeVariableType *, 4> typeVars;
  if (first->hasTypeVariable())
    first->getTypeVariables(typeVars);
  if (second->hasTypeVariable())
    second->getTypeVariables(typeVars);
  uniqueTypeVariables(typeVars);

  // Create the constraint.
  unsigned size = totalSizeToAlloc<TypeVariableType*>(typeVars.size());
  void *mem = cs.getAllocator().Allocate(size, alignof(Constraint));
  return new (mem) Constraint(kind, fix, first, second, locator, typeVars);
}

Constraint *Constraint::createDisjunction(ConstraintSystem &cs,
                                          ArrayRef<Constraint *> constraints,
                                          ConstraintLocator *locator,
                                          RememberChoice_t rememberChoice) {
  // Unwrap any disjunctions inside the disjunction constraint; we only allow
  // disjunctions at the top level.
  SmallVector<TypeVariableType *, 4> typeVars;
  bool unwrappedAny = false;
  SmallVector<Constraint *, 1> unwrapped;
  unsigned index = 0;
  for (auto constraint : constraints) {
    // Gather type variables from this constraint.
    gatherReferencedTypeVars(constraint, typeVars);

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
  if (constraints.size() == 1) {
    assert(!rememberChoice && "simplified an important disjunction?");
    return constraints.front();
  }

  // Create the disjunction constraint.
  uniqueTypeVariables(typeVars);
  unsigned size = totalSizeToAlloc<TypeVariableType*>(typeVars.size());
  void *mem = cs.getAllocator().Allocate(size, alignof(Constraint));
  auto disjunction =  new (mem) Constraint(ConstraintKind::Disjunction,
                              cs.allocateCopy(constraints), locator, typeVars);
  disjunction->RememberChoice = (bool) rememberChoice;
  return disjunction;
}

void *Constraint::operator new(size_t bytes, ConstraintSystem& cs,
                               size_t alignment) {
  return ::operator new (bytes, cs, alignment);
}

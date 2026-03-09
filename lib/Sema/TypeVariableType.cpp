//===--- TypeVariableType.cpp - Type variable type implementation ---------===//
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
// This file implements those parts of TypeVariableType that live in Sema.
//
//===----------------------------------------------------------------------===//

#include "swift/Sema/TypeVariableType.h"
#include "TypeChecker.h"
#include "swift/AST/Expr.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/PreparedOverload.h"

using namespace swift;
using namespace constraints;

#define DEBUG_TYPE "Constraint solver overall"
#define JOIN2(X,Y) X##Y
STATISTIC(TotalNumTypeVariables, "# of type variables created");

void TypeVariableType::Implementation::print(llvm::raw_ostream &OS) {
  getTypeVariable()->print(OS, PrintOptions::forDebugging());

  SmallVector<TypeVariableOptions, 4> bindingOptions;
  if (canBindToLValue())
    bindingOptions.push_back(TypeVariableOptions::TVO_CanBindToLValue);
  if (canBindToInOut())
    bindingOptions.push_back(TypeVariableOptions::TVO_CanBindToInOut);
  if (canBindToNoEscape())
    bindingOptions.push_back(TypeVariableOptions::TVO_CanBindToNoEscape);
  if (canBindToHole())
    bindingOptions.push_back(TypeVariableOptions::TVO_CanBindToHole);
  if (canBindToPack())
    bindingOptions.push_back(TypeVariableOptions::TVO_CanBindToPack);
  if (isPackExpansion())
    bindingOptions.push_back(TypeVariableOptions::TVO_PackExpansion);
  if (!bindingOptions.empty()) {
    OS << " [can bind to: ";
    interleave(bindingOptions, OS,
               [&](TypeVariableOptions option) {
                  (OS << getTypeVariableOptions(option));},
               ", ");
               OS << "]";
  }
}

GenericTypeParamType *
TypeVariableType::Implementation::getGenericParameter() const {
  return locator ? locator->getGenericParameter() : nullptr;
}

std::optional<ExprKind>
TypeVariableType::Implementation::getAtomicLiteralKind() const {
  if (!locator || !locator->directlyAt<LiteralExpr>())
    return std::nullopt;

  auto kind = getAsExpr(locator->getAnchor())->getKind();
  switch (kind) {
  case ExprKind::IntegerLiteral:
  case ExprKind::FloatLiteral:
  case ExprKind::StringLiteral:
  case ExprKind::BooleanLiteral:
  case ExprKind::NilLiteral:
    return kind;
  default:
    return std::nullopt;
  }
}

bool TypeVariableType::Implementation::isClosureType() const {
  if (!(locator && locator->getAnchor()))
    return false;

  return isExpr<ClosureExpr>(locator->getAnchor()) && locator->getPath().empty();
}

bool TypeVariableType::Implementation::isTapType() const {
  return locator && locator->directlyAt<TapExpr>();
}

bool TypeVariableType::Implementation::isClosureParameterType() const {
  if (!(locator && locator->getAnchor()))
    return false;

  return isExpr<ClosureExpr>(locator->getAnchor()) &&
         locator->isLastElement<LocatorPathElt::TupleElement>();
}

bool TypeVariableType::Implementation::isClosureResultType() const {
  if (!(locator && locator->getAnchor()))
    return false;

  return isExpr<ClosureExpr>(locator->getAnchor()) &&
         locator->isLastElement<LocatorPathElt::ClosureResult>();
}

bool TypeVariableType::Implementation::isKeyPathType() const {
  return locator && locator->isKeyPathType();
}

bool TypeVariableType::Implementation::isKeyPathRoot() const {
  return locator && locator->isKeyPathRoot();
}

bool TypeVariableType::Implementation::isKeyPathValue() const {
  return locator && locator->isKeyPathValue();
}

bool TypeVariableType::Implementation::isKeyPathSubscriptIndex() const {
  return locator &&
         locator->isLastElement<LocatorPathElt::KeyPathSubscriptIndex>();
}

bool TypeVariableType::Implementation::isParameterPack() const {
  return locator
      && locator->isForGenericParameter()
      && locator->getGenericParameter()->isParameterPack();
}

bool TypeVariableType::Implementation::isCodeCompletionToken() const {
  return locator && locator->directlyAt<CodeCompletionExpr>();
}

bool TypeVariableType::Implementation::isOpaqueType() const {
  if (!locator)
    return false;

  auto GP = locator->getLastElementAs<LocatorPathElt::GenericParameter>();
  if (!GP)
    return false;

  if (auto *GPT = GP->getType()->getAs<GenericTypeParamType>())
    return (GPT->getOpaqueDecl() != nullptr);

  return false;
}

bool TypeVariableType::Implementation::isCollectionLiteralType() const {
  return locator && (locator->directlyAt<ArrayExpr>() ||
                     locator->directlyAt<DictionaryExpr>());
}

bool TypeVariableType::Implementation::isNumberLiteralType() const {
  return locator && locator->directlyAt<NumberLiteralExpr>();
}

bool TypeVariableType::Implementation::isFunctionResult() const {
  return locator && locator->isLastElement<LocatorPathElt::FunctionResult>();
}

bool TypeVariableType::Implementation::isTernary() const {
  return locator && locator->directlyAt<TernaryExpr>();
}

TypeVariableType *ConstraintSystem::createTypeVariable(
                                     ConstraintLocator *locator,
                                     unsigned options,
                                     PreparedOverloadBuilder *preparedOverload) {
  ++TotalNumTypeVariables;
  auto tv = TypeVariableType::getNew(getASTContext(), assignTypeVariableID(),
                                     locator, options);
  if (preparedOverload) {
    ASSERT(PreparingOverload);
    preparedOverload->addedTypeVariable(tv);
  } else {
    addTypeVariable(tv);
  }
  return tv;
}

void ConstraintSystem::addTypeVariable(TypeVariableType *typeVar) {
  ASSERT(!PreparingOverload);

  TypeVariables.insert(typeVar);

  // Notify the constraint graph.
  CG.addTypeVariable(typeVar);
}

TypeVariableType *ConstraintSystem::getRepresentative(TypeVariableType *typeVar) const {
  return typeVar->getImpl().getRepresentative(getTrail());
}

Type ConstraintSystem::getFixedType(TypeVariableType *typeVar) const {
  return typeVar->getImpl().getFixedType(getTrail());
}

void ConstraintSystem::mergeEquivalenceClasses(TypeVariableType *typeVar1,
                                               TypeVariableType *typeVar2,
                                               bool updateWorkList) {
  assert(typeVar1 == getRepresentative(typeVar1) &&
         "typeVar1 is not the representative");
  assert(typeVar2 == getRepresentative(typeVar2) &&
         "typeVar2 is not the representative");
  assert(typeVar1 != typeVar2 && "cannot merge type with itself");

  // Always merge 'up' the constraint stack, because it is simpler.
  if (typeVar1->getImpl().getID() > typeVar2->getImpl().getID())
    std::swap(typeVar1, typeVar2);

  CG.mergeNodesPre(typeVar2);
  typeVar1->getImpl().mergeEquivalenceClasses(typeVar2, getTrail());
  CG.mergeNodes(typeVar1, typeVar2);

  if (updateWorkList) {
    addTypeVariableConstraintsToWorkList(typeVar1);
  }
}

void ConstraintSystem::assignFixedType(TypeVariableType *typeVar, Type type,
                                       bool updateState,
                                       bool notifyBindingInference) {
  assert(!type->hasError() &&
         "Should not be assigning a type involving ErrorType!");

  CG.retractFromInference(typeVar);
  typeVar->getImpl().assignFixedType(type, getTrail());

  if (!updateState)
    return;

  if (!type->isTypeVariableOrMember()) {
    // If this type variable represents a literal, check whether we picked the
    // default literal type. First, find the corresponding protocol.
    //
    // If we have the constraint graph, we can check all type variables in
    // the equivalence class. This is the More Correct path.
    // FIXME: Eliminate the less-correct path.
    auto typeVarRep = getRepresentative(typeVar);
    for (auto *tv : CG[typeVarRep].getEquivalenceClass()) {
      auto locator = tv->getImpl().getLocator();
      if (!(locator && (locator->directlyAt<CollectionExpr>() ||
                        locator->directlyAt<LiteralExpr>())))
          continue;

      auto *literalProtocol = TypeChecker::getLiteralProtocol(
          getASTContext(), castToExpr(locator->getAnchor()));
      if (!literalProtocol)
        continue;

      // If the protocol has a default type, check it.
      if (auto defaultType = TypeChecker::getDefaultType(literalProtocol, DC)) {
        auto isDefaultType = [&defaultType](Type type) {
          // Check whether the nominal types match. This makes sure that we
          // properly handle Array vs. Array<T>.
          return defaultType->getAnyNominal() == type->getAnyNominal();
        };

        if (!isDefaultType(type)) {
          // Treat `std.string` as a default type just like we do
          // Swift standard library `String`. This helps to disambiguate
          // operator overloads that use `std.string` vs. a custom C++
          // type that conforms to `ExpressibleByStringLiteral` as well.
          bool isCxxDefaultType =
              literalProtocol->isSpecificProtocol(
                  KnownProtocolKind::ExpressibleByStringLiteral) &&
              type->isCxxString();

          increaseScore(SK_NonDefaultLiteral, locator,
                        isCxxDefaultType ? 1 : 2);
        }
      }

      break;
    }
  }

  // Notify the constraint graph.
  CG.bindTypeVariable(typeVar, type);

  addTypeVariableConstraintsToWorkList(typeVar);

  if (notifyBindingInference)
    CG.introduceToInference(typeVar, type);
}
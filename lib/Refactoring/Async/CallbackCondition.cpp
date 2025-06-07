//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "AsyncRefactoring.h"

using namespace swift;
using namespace swift::refactoring::asyncrefactorings;

CallbackCondition::CallbackCondition(const BinaryExpr *BE,
                                     const FuncDecl *Operator) {
  bool FoundNil = false;
  BooleanLiteralExpr *FoundBool = nullptr;
  bool DidUnwrapOptional = false;

  for (auto *Operand : {BE->getLHS(), BE->getRHS()}) {
    Operand = Operand->getSemanticsProvidingExpr();
    if (auto *IIOE = dyn_cast<InjectIntoOptionalExpr>(Operand)) {
      Operand = IIOE->getSubExpr()->getSemanticsProvidingExpr();
      DidUnwrapOptional = true;
    }
    if (isa<NilLiteralExpr>(Operand)) {
      FoundNil = true;
    } else if (auto *BLE = dyn_cast<BooleanLiteralExpr>(Operand)) {
      FoundBool = BLE;
    } else if (auto *DRE = dyn_cast<DeclRefExpr>(Operand)) {
      Subject = DRE->getDecl();
    }
  }

  if (!Subject)
    return;

  if (FoundNil) {
    if (Operator->getBaseName() == "==") {
      Type = ConditionType::NIL;
    } else if (Operator->getBaseName() == "!=") {
      Type = ConditionType::NOT_NIL;
    }
  } else if (FoundBool) {
    if (Operator->getBaseName() == "==") {
      Type = FoundBool->getValue() ? ConditionType::IS_TRUE
                                   : ConditionType::IS_FALSE;
    } else if (Operator->getBaseName() == "!=" && !DidUnwrapOptional) {
      // Note that we don't consider this case if we unwrapped an optional,
      // as e.g optBool != false is a check for true *or* nil.
      Type = FoundBool->getValue() ? ConditionType::IS_FALSE
                                   : ConditionType::IS_TRUE;
    }
  }
}

CallbackCondition::CallbackCondition(const Expr *E) {
  // FIXME: Sema should produce ErrorType.
  if (!E->getType() || !E->getType()->isBool())
    return;

  auto CondType = ConditionType::IS_TRUE;
  E = E->getSemanticsProvidingExpr();

  // If we have a prefix negation operator, this is a check for false.
  if (auto *PrefixOp = dyn_cast<PrefixUnaryExpr>(E)) {
    auto *Callee = PrefixOp->getCalledValue();
    if (Callee && Callee->isOperator() && Callee->getBaseName() == "!") {
      CondType = ConditionType::IS_FALSE;
      E = PrefixOp->getOperand()->getSemanticsProvidingExpr();
    }
  }

  auto *DRE = dyn_cast<DeclRefExpr>(E);
  if (!DRE)
    return;

  Subject = DRE->getDecl();
  Type = CondType;
}

CallbackCondition::CallbackCondition(const Pattern *P, const Expr *Init) {
  Init = Init->getSemanticsProvidingExpr();
  P = P->getSemanticsProvidingPattern();

  if (auto *DRE = dyn_cast<DeclRefExpr>(Init)) {
    if (auto *OSP = dyn_cast<OptionalSomePattern>(P)) {
      // `let bind = <Subject>`
      Type = ConditionType::NOT_NIL;
      Subject = DRE->getDecl();
      BindPattern = OSP->getSubPattern();
    } else if (auto *EEP = dyn_cast<EnumElementPattern>(P)) {
      // `case .<func>(let <bind>) = <Subject>`
      initFromEnumPattern(DRE->getDecl(), EEP);
    }
  } else if (auto *OTE = dyn_cast<OptionalTryExpr>(Init)) {
    // `let bind = try? <Subject>.get()`
    if (auto *OSP = dyn_cast<OptionalSomePattern>(P))
      initFromOptionalTry(OSP->getSubPattern(), OTE);
  }
}

CallbackCondition::CallbackCondition(const Decl *Subject,
                                     const CaseLabelItem *CaseItem) {
  if (auto *EEP = dyn_cast<EnumElementPattern>(
          CaseItem->getPattern()->getSemanticsProvidingPattern())) {
    // `case .<func>(let <bind>)`
    initFromEnumPattern(Subject, EEP);
  }
}

void CallbackCondition::initFromEnumPattern(const Decl *D,
                                            const EnumElementPattern *EEP) {
  if (auto *EED = EEP->getElementDecl()) {
    auto eedTy = EED->getParentEnum()->getDeclaredType();
    if (!eedTy || !eedTy->isResult())
      return;
    if (EED->getNameStr() == StringRef("failure")) {
      Type = ConditionType::FAILURE_PATTEN;
    } else {
      Type = ConditionType::SUCCESS_PATTERN;
    }
    Subject = D;
    BindPattern = EEP->getSubPattern();
  }
}

void CallbackCondition::initFromOptionalTry(const class Pattern *P,
                                            const OptionalTryExpr *OTE) {
  auto *ICE = dyn_cast<ImplicitConversionExpr>(OTE->getSubExpr());
  if (!ICE)
    return;
  auto *CE = dyn_cast<CallExpr>(ICE->getSyntacticSubExpr());
  if (!CE)
    return;
  auto *DSC = dyn_cast<DotSyntaxCallExpr>(CE->getFn());
  if (!DSC)
    return;

  auto *BaseDRE = dyn_cast<DeclRefExpr>(DSC->getBase());
  if (!BaseDRE->getType() || !BaseDRE->getType()->isResult())
    return;

  auto *FnDRE = dyn_cast<DeclRefExpr>(DSC->getFn());
  if (!FnDRE)
    return;
  auto *FD = dyn_cast<FuncDecl>(FnDRE->getDecl());
  if (!FD || FD->getNameStr() != StringRef("get"))
    return;

  Type = ConditionType::NOT_NIL;
  Subject = BaseDRE->getDecl();
  BindPattern = P;
}

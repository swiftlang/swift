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

#include "RefactoringActions.h"

using namespace swift::refactoring;

static void generateMemberwiseInit(SourceEditConsumer &EditConsumer,
                                   SourceManager &SM, NominalTypeDecl *nominal,
                                   SourceLoc targetLocation) {
  llvm::SmallString<64> buffer;
  llvm::raw_svector_ostream OS(buffer);
  OS << "\ninternal ";
  // For the refactoring we want to include all private properties, since it's
  // easier to delete code than add code, so print the compatibility overload.
  printMemberwiseInit(nominal, MemberwiseInitKind::Compatibility, OS);
  OS << "\n";

  // Accept the entire edit.
  EditConsumer.accept(SM, targetLocation, buffer);
}

static NominalTypeDecl *getMemberwiseNominal(ResolvedCursorInfoPtr CursorInfo) {
  auto ValueRefInfo = dyn_cast<ResolvedValueRefCursorInfo>(CursorInfo);
  if (!ValueRefInfo || !ValueRefInfo->getValueD())
    return nullptr;

  auto *nominalDecl = dyn_cast<NominalTypeDecl>(ValueRefInfo->getValueD());
  if (!nominalDecl || nominalDecl->getStoredProperties().empty() ||
      ValueRefInfo->isRef()) {
    return nullptr;
  }
  return nominalDecl;
}

bool RefactoringActionMemberwiseInitLocalRefactoring::isApplicable(
    ResolvedCursorInfoPtr Tok, DiagnosticEngine &Diag) {
  auto nominal = getMemberwiseNominal(Tok);
  if (!nominal)
    return false;

  return nominal->getBraces().isValid();
}

bool RefactoringActionMemberwiseInitLocalRefactoring::performChange() {
  auto nominal = getMemberwiseNominal(CursorInfo);
  if (!nominal)
    return true;

  auto targetLocation = nominal->getBraces().Start.getAdvancedLocOrInvalid(1);
  if (!targetLocation.isValid())
    return true;

  generateMemberwiseInit(EditConsumer, SM, nominal, targetLocation);
  return false;
}

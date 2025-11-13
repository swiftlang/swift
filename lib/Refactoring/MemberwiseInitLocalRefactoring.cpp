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

namespace {
struct MemberwiseParameter {
  Identifier Name;
  Type MemberType;
  Expr *DefaultExpr;

  MemberwiseParameter(Identifier name, Type type, Expr *initialExpr)
      : Name(name), MemberType(type), DefaultExpr(initialExpr) {}
};
} // namespace

static void printMemberName(Identifier name, llvm::raw_ostream &OS) {
  if (escapeIdentifierInContext(name, PrintNameContext::TypeMember)) {
    OS << '`' << name << '`';
  } else {
    OS << name;
  }
}

static void generateMemberwiseInit(SourceEditConsumer &EditConsumer,
                                   SourceManager &SM,
                                   ArrayRef<MemberwiseParameter> memberVector,
                                   SourceLoc targetLocation) {

  EditConsumer.accept(SM, targetLocation, "\ninternal init(");
  auto insertMember = [&SM](const MemberwiseParameter &memberData,
                            raw_ostream &OS, bool wantsSeparator) {
    {
      printMemberName(memberData.Name, OS);
      OS << ": ";
      // Unconditionally print '@escaping' if we print out a function type -
      // the assignments we generate below will escape this parameter.
      if (isa<AnyFunctionType>(memberData.MemberType->getCanonicalType())) {
        OS << "@" << TypeAttribute::getAttrName(TypeAttrKind::Escaping) << " ";
      }
      OS << memberData.MemberType.getString();
    }

    bool HasAddedDefault = false;
    if (auto *expr = memberData.DefaultExpr) {
      if (expr->getSourceRange().isValid()) {
        auto range = Lexer::getCharSourceRangeFromSourceRange(
            SM, expr->getSourceRange());
        OS << " = " << SM.extractText(range);
        HasAddedDefault = true;
      }
    }
    if (!HasAddedDefault && memberData.MemberType->isOptional()) {
      OS << " = nil";
    }

    if (wantsSeparator) {
      OS << ", ";
    }
  };

  // Process the initial list of members, inserting commas as appropriate.
  std::string Buffer;
  llvm::raw_string_ostream OS(Buffer);
  for (const auto &memberData : llvm::enumerate(memberVector)) {
    bool wantsSeparator = (memberData.index() != memberVector.size() - 1);
    insertMember(memberData.value(), OS, wantsSeparator);
  }

  // Synthesize the body.
  OS << ") {\n";
  for (auto &member : memberVector) {
    // self.<property> = <property>
    OS << "self.";
    printMemberName(member.Name, OS);
    OS << " = ";
    printMemberName(member.Name, OS);
    OS << "\n";
  }
  OS << "}\n";

  // Accept the entire edit.
  EditConsumer.accept(SM, targetLocation, OS.str());
}

static SourceLoc
collectMembersForInit(ResolvedCursorInfoPtr CursorInfo,
                      SmallVectorImpl<MemberwiseParameter> &memberVector) {
  auto ValueRefInfo = dyn_cast<ResolvedValueRefCursorInfo>(CursorInfo);
  if (!ValueRefInfo || !ValueRefInfo->getValueD())
    return SourceLoc();

  NominalTypeDecl *nominalDecl =
      dyn_cast<NominalTypeDecl>(ValueRefInfo->getValueD());
  if (!nominalDecl || nominalDecl->getStoredProperties().empty() ||
      ValueRefInfo->isRef()) {
    return SourceLoc();
  }

  SourceLoc bracesStart = nominalDecl->getBraces().Start;
  if (!bracesStart.isValid())
    return SourceLoc();

  SourceLoc targetLocation = bracesStart.getAdvancedLoc(1);
  if (!targetLocation.isValid())
    return SourceLoc();

  for (auto member : nominalDecl->getMemberwiseInitProperties()) {
    auto varDecl = dyn_cast<VarDecl>(member);
    if (!varDecl) {
      continue;
    }
    if (varDecl->getAttrs().hasAttribute<LazyAttr>()) {
      // Exclude lazy members from the memberwise initializer. This is
      // inconsistent with the implicitly synthesized memberwise initializer but
      // we think it makes more sense because otherwise the lazy variable's
      // initializer gets evaluated eagerly.
      continue;
    }

    auto patternBinding = varDecl->getParentPatternBinding();
    if (!patternBinding)
      continue;

    const auto i = patternBinding->getPatternEntryIndexForVarDecl(varDecl);
    Expr *defaultInit = nullptr;
    if (patternBinding->isExplicitlyInitialized(i) ||
        patternBinding->isDefaultInitializable()) {
      defaultInit = patternBinding->getOriginalInit(i);
    }

    memberVector.emplace_back(varDecl->getName(), varDecl->getTypeInContext(),
                              defaultInit);
  }

  return targetLocation;
}

bool RefactoringActionMemberwiseInitLocalRefactoring::isApplicable(
    ResolvedCursorInfoPtr Tok, DiagnosticEngine &Diag) {

  SmallVector<MemberwiseParameter, 8> memberVector;
  return collectMembersForInit(Tok, memberVector).isValid();
}

bool RefactoringActionMemberwiseInitLocalRefactoring::performChange() {

  SmallVector<MemberwiseParameter, 8> memberVector;
  SourceLoc targetLocation = collectMembersForInit(CursorInfo, memberVector);
  if (targetLocation.isInvalid())
    return true;

  generateMemberwiseInit(EditConsumer, SM, memberVector, targetLocation);

  return false;
}

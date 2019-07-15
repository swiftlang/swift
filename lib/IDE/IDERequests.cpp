//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookup.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/CommentConversion.h"
#include "swift/IDE/Utils.h"
#include "swift/Markup/XMLUtils.h"
#include "swift/Subsystems.h"
#include "swift/IDE/IDERequests.h"

using namespace swift;
using namespace swift::ide;

namespace swift {
// Implement the IDE type zone.
#define SWIFT_TYPEID_ZONE SWIFT_IDE_REQUESTS_TYPEID_ZONE
#define SWIFT_TYPEID_HEADER "swift/IDE/IDERequestIDZone.def"
#include "swift/Basic/ImplementTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER
}

//----------------------------------------------------------------------------//
// Cusor info resolver
//----------------------------------------------------------------------------//

class CursorInfoResolver : public SourceEntityWalker {
  SourceFile &SrcFile;
  SourceLoc LocToResolve;
  ResolvedCursorInfo CursorInfo;
  Type ContainerType;
  llvm::SmallVector<Expr*, 4> TrailingExprStack;

public:
  explicit CursorInfoResolver(SourceFile &SrcFile) :
    SrcFile(SrcFile), CursorInfo(&SrcFile) {}
  ResolvedCursorInfo resolve(SourceLoc Loc);
  SourceManager &getSourceMgr() const;
private:
  bool walkToExprPre(Expr *E) override;
  bool walkToExprPost(Expr *E) override;
  bool walkToDeclPre(Decl *D, CharSourceRange Range) override;
  bool walkToDeclPost(Decl *D) override;
  bool walkToStmtPre(Stmt *S) override;
  bool walkToStmtPost(Stmt *S) override;
  bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                          TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef, Type T,
                          ReferenceMetaData Data) override;
  bool visitCallArgName(Identifier Name, CharSourceRange Range,
                        ValueDecl *D) override;
  bool visitDeclarationArgumentName(Identifier Name, SourceLoc StartLoc,
                                    ValueDecl *D) override;
  bool visitModuleReference(ModuleEntity Mod, CharSourceRange Range) override;
  bool rangeContainsLoc(SourceRange Range) const;
  bool rangeContainsLoc(CharSourceRange Range) const;
  bool isDone() const { return CursorInfo.isValid(); }
  bool tryResolve(ValueDecl *D, TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef,
                  SourceLoc Loc, bool IsRef, Type Ty = Type());
  bool tryResolve(ModuleEntity Mod, SourceLoc Loc);
  bool tryResolve(Stmt *St);
  bool visitSubscriptReference(ValueDecl *D, CharSourceRange Range,
                               ReferenceMetaData Data,
                               bool IsOpenBracket) override;
};

SourceManager &CursorInfoResolver::getSourceMgr() const
{
  return SrcFile.getASTContext().SourceMgr;
}

bool CursorInfoResolver::tryResolve(ValueDecl *D, TypeDecl *CtorTyRef,
                                 ExtensionDecl *ExtTyRef, SourceLoc Loc,
                                 bool IsRef, Type Ty) {
  if (!D->hasName())
    return false;

  if (Loc != LocToResolve)
    return false;

  if (auto *VD = dyn_cast<VarDecl>(D)) {
    // Handle references to the implicitly generated vars in case statements
    // matching multiple patterns
    if (VD->isImplicit()) {
      if (auto * Parent = VD->getParentVarDecl()) {
        D = Parent;
        VD = Parent;
      }
    }
  }
  CursorInfo.setValueRef(D, CtorTyRef, ExtTyRef, IsRef, Ty, ContainerType);
  return true;
}

bool CursorInfoResolver::tryResolve(ModuleEntity Mod, SourceLoc Loc) {
  if (Loc == LocToResolve) {
    CursorInfo.setModuleRef(Mod);
    return true;
  }
  return false;
}

bool CursorInfoResolver::tryResolve(Stmt *St) {
  if (auto *LST = dyn_cast<LabeledStmt>(St)) {
    if (LST->getStartLoc() == LocToResolve) {
      CursorInfo.setTrailingStmt(St);
      return true;
    }
  }
  if (auto *CS = dyn_cast<CaseStmt>(St)) {
    if (CS->getStartLoc() == LocToResolve) {
      CursorInfo.setTrailingStmt(St);
      return true;
    }
  }
  return false;
}

bool CursorInfoResolver::visitSubscriptReference(ValueDecl *D,
                                                 CharSourceRange Range,
                                                 ReferenceMetaData Data,
                                                 bool IsOpenBracket) {
  // We should treat both open and close brackets equally
  return visitDeclReference(D, Range, nullptr, nullptr, Type(), Data);
}

ResolvedCursorInfo CursorInfoResolver::resolve(SourceLoc Loc) {
  assert(Loc.isValid());
  LocToResolve = Loc;
  CursorInfo.Loc = Loc;
  walk(SrcFile);
  return CursorInfo;
}

bool CursorInfoResolver::walkToDeclPre(Decl *D, CharSourceRange Range) {
  if (!rangeContainsLoc(D->getSourceRangeIncludingAttrs()))
    return false;

  if (isa<ExtensionDecl>(D))
    return true;

  if (auto *VD = dyn_cast<ValueDecl>(D))
    return !tryResolve(VD, /*CtorTyRef=*/nullptr, /*ExtTyRef=*/nullptr,
                       Range.getStart(), /*IsRef=*/false);

  return true;
}

bool CursorInfoResolver::walkToDeclPost(Decl *D) {
  if (isDone())
    return false;
  if (getSourceMgr().isBeforeInBuffer(LocToResolve, D->getStartLoc()))
    return false;
  return true;
}

bool CursorInfoResolver::walkToStmtPre(Stmt *S) {
  // Getting the character range for the statement, to account for interpolation
  // strings. The token range for the interpolation string is the whole string,
  // with begin/end locations pointing at the beginning of the string, so if
  // there is a token location inside the string, it will seem as if it is out
  // of the source range, unless we convert to character range.

  // FIXME: Even implicit Stmts should have proper ranges that include any
  // non-implicit Stmts (fix Stmts created for lazy vars).
  if (!S->isImplicit() &&
      !rangeContainsLoc(Lexer::getCharSourceRangeFromSourceRange(
          getSourceMgr(), S->getSourceRange())))
    return false;
  return !tryResolve(S);
}

bool CursorInfoResolver::walkToStmtPost(Stmt *S) {
  if (isDone())
    return false;
  // FIXME: Even implicit Stmts should have proper ranges that include any
  // non-implicit Stmts (fix Stmts created for lazy vars).
  if (!S->isImplicit() && getSourceMgr().isBeforeInBuffer(LocToResolve,
                                                          S->getStartLoc()))
    return false;
  return true;
}

bool CursorInfoResolver::visitDeclReference(ValueDecl *D,
                                            CharSourceRange Range,
                                            TypeDecl *CtorTyRef,
                                            ExtensionDecl *ExtTyRef, Type T,
                                            ReferenceMetaData Data) {
  if (isDone())
    return false;
  if (Data.isImplicit)
    return true;
  return !tryResolve(D, CtorTyRef, ExtTyRef, Range.getStart(), /*IsRef=*/true, T);
}

bool CursorInfoResolver::walkToExprPre(Expr *E) {
  if (!isDone()) {
    if (auto SAE = dyn_cast<SelfApplyExpr>(E)) {
      if (SAE->getFn()->getStartLoc() == LocToResolve) {
        ContainerType = SAE->getBase()->getType();
      }
    } else if (auto ME = dyn_cast<MemberRefExpr>(E)) {
      SourceLoc MemberLoc = ME->getNameLoc().getBaseNameLoc();
      if (MemberLoc.isValid() && MemberLoc == LocToResolve) {
        ContainerType = ME->getBase()->getType();
      }
    }
    auto IsProperCursorLocation = E->getStartLoc() == LocToResolve;
    // Handle cursor placement after `try` in ForceTry and OptionalTry Expr.
    auto CheckLocation = [&IsProperCursorLocation, this](SourceLoc Loc) {
      IsProperCursorLocation = Loc == LocToResolve || IsProperCursorLocation;
    };
    if (auto *FTE = dyn_cast<ForceTryExpr>(E)) {
      CheckLocation(FTE->getExclaimLoc());
    }
    if (auto *OTE = dyn_cast<OptionalTryExpr>(E)) {
      CheckLocation(OTE->getQuestionLoc());
    }
    // Keep track of trailing expressions.
    if (!E->isImplicit() && IsProperCursorLocation)
      TrailingExprStack.push_back(E);
  }
  return true;
}

bool CursorInfoResolver::walkToExprPost(Expr *E) {
  if (isDone())
    return false;
  if (!TrailingExprStack.empty() && TrailingExprStack.back() == E) {
    // We return the outtermost expression in the token info.
    CursorInfo.setTrailingExpr(TrailingExprStack.front());
    return false;
  }
  return true;
}

bool CursorInfoResolver::visitCallArgName(Identifier Name,
                                          CharSourceRange Range,
                                          ValueDecl *D) {
  if (isDone())
    return false;
  bool Found = tryResolve(D, nullptr, nullptr, Range.getStart(), /*IsRef=*/true);
  if (Found)
    CursorInfo.IsKeywordArgument = true;
  return !Found;
}

bool CursorInfoResolver::
visitDeclarationArgumentName(Identifier Name, SourceLoc StartLoc, ValueDecl *D) {
  if (isDone())
    return false;
  return !tryResolve(D, nullptr, nullptr, StartLoc, /*IsRef=*/false);
}

bool CursorInfoResolver::visitModuleReference(ModuleEntity Mod,
                                              CharSourceRange Range) {
  if (isDone())
    return false;
  if (Mod.isBuiltinModule())
    return true; // Ignore.
  return !tryResolve(Mod, Range.getStart());
}

bool CursorInfoResolver::rangeContainsLoc(SourceRange Range) const {
  return getSourceMgr().rangeContainsTokenLoc(Range, LocToResolve);
}

bool CursorInfoResolver::rangeContainsLoc(CharSourceRange Range) const {
  return Range.contains(LocToResolve);
}

llvm::Expected<ide::ResolvedCursorInfo>
CursorInfoRequest::evaluate(Evaluator &eval, CursorInfoOwner CI) const {
  if (!CI.isValid())
    return ResolvedCursorInfo();
  CursorInfoResolver Resolver(*CI.File);
  return Resolver.resolve(CI.Loc);
}

SourceLoc CursorInfoRequest::getNearestLoc() const {
  return std::get<0>(getStorage()).Loc;
}

void swift::simple_display(llvm::raw_ostream &out, const CursorInfoOwner &owner) {
  if (!owner.isValid())
    return;
  auto &SM = owner.File->getASTContext().SourceMgr;
  out << SM.getIdentifierForBuffer(*owner.File->getBufferID());
  auto LC = SM.getLineAndColumn(owner.Loc);
  out << ":" << LC.first << ":" << LC.second;
}

// Define request evaluation functions for each of the IDE requests.
static AbstractRequestFunction *ideRequestFunctions[] = {
#define SWIFT_TYPEID(Name)                                    \
  reinterpret_cast<AbstractRequestFunction *>(&Name::evaluateRequest),
#include "swift/IDE/IDERequestIDZone.def"
#undef SWIFT_TYPEID
};

void swift::registerIDERequestFunctions(Evaluator &evaluator) {
  evaluator.registerRequestFunctions(SWIFT_IDE_REQUESTS_TYPEID_ZONE,
                                     ideRequestFunctions);
}

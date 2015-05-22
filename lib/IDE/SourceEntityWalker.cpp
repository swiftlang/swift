//===- SourceEntityWalker.cpp - Routines for semantic source info ---------===//
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

#include "swift/IDE/SourceEntityWalker.h"
#include "swift/IDE/Utils.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"
#include "swift/AST/Types.h"
#include "swift/Basic/SourceManager.h"
#include "clang/Basic/Module.h"

using namespace swift;
using namespace ide;

namespace {

class SemaAnnotator : public ASTWalker {
  SourceEntityWalker &SEWalker;
  SmallVector<ConstructorRefCallExpr *, 2> CtorRefs;
  bool Cancelled = false;

public:
  explicit SemaAnnotator(SourceEntityWalker &SEWalker)
    : SEWalker(SEWalker) { }

  bool isDone() const { return Cancelled; }

private:
  bool walkToDeclPre(Decl *D) override;
  std::pair<bool, Expr *> walkToExprPre(Expr *E) override;
  bool walkToTypeReprPre(TypeRepr *T) override;

  bool walkToDeclPost(Decl *D) override;
  Expr *walkToExprPost(Expr *E) override;
  bool walkToTypeReprPost(TypeRepr *T) override;

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override;
  Stmt *walkToStmtPost(Stmt *S) override;

  std::pair<bool, Pattern *> walkToPatternPre(Pattern *P) override;

  bool handleImports(ImportDecl *Import);
  bool passModulePathElements(ArrayRef<ImportDecl::AccessPathElement> Path,
                              const clang::Module *ClangMod);

  bool passReference(ValueDecl *D, SourceLoc Loc);
  bool passReference(ModuleEntity Mod, std::pair<Identifier, SourceLoc> IdLoc);

  bool passCallArgNames(Expr *Fn, TupleExpr *TupleE);

  TypeDecl *getTypeDecl(Type Ty);

  bool shouldIgnore(Decl *D, bool &ShouldVisitChildren);

  ValueDecl *extractDecl(Expr *Fn) const {
    if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(Fn))
      return DRE->getDecl();
    if (auto ApplyE = dyn_cast<ApplyExpr>(Fn))
      return extractDecl(ApplyE->getFn());
    return nullptr;
  }
};

}

bool SemaAnnotator::walkToDeclPre(Decl *D) {
  if (isDone())
    return false;

  bool ShouldVisitChildren;
  if (shouldIgnore(D, ShouldVisitChildren))
    return ShouldVisitChildren;

  SourceLoc Loc = D->getLoc();
  unsigned NameLen = 0;

  if (ValueDecl *VD = dyn_cast<ValueDecl>(D)) {
    if (VD->hasName())
      NameLen = VD->getName().getLength();

  } else if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(D)) {
    SourceRange SR = ED->getExtendedTypeLoc().getSourceRange();
    Loc = SR.Start;
    if (Loc.isValid())
      NameLen = ED->getASTContext().SourceMgr.getByteDistance(SR.Start, SR.End);

  } else if (auto Import = dyn_cast<ImportDecl>(D)) {
    if (!handleImports(Import))
      return false;

  } else {
    return true;
  }

  CharSourceRange Range = (Loc.isValid()) ? CharSourceRange(Loc, NameLen)
                                          : CharSourceRange();
  return SEWalker.walkToDeclPre(D, Range);
}

bool SemaAnnotator::walkToDeclPost(Decl *D) {
  if (isDone())
    return false;

  bool ShouldVisitChildren;
  if (shouldIgnore(D, ShouldVisitChildren))
    return true;

  // FIXME: rdar://17671977 the initializer for a lazy property has already
  // been moved into its implicit getter.
  if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
    if (auto *VD = PBD->getSingleVar()) {
      if (VD->getAttrs().hasAttribute<LazyAttr>()) {
        if (auto *Get = VD->getGetter()) {
          assert((Get->isImplicit() || Get->isInvalid())
            && "lazy var getter must be either implicitly computed or invalid");

          // Note that an implicit getter may not have the body synthesized
          // in case the owning PatternBindingDecl is invalid.
          if (auto *Body = Get->getBody()) {
            Body->walk(*this);
          }
        }
      }
    }
  }

  if (!isa<ValueDecl>(D) && !isa<ExtensionDecl>(D) && !isa<ImportDecl>(D))
    return true;

  bool Continue = SEWalker.walkToDeclPost(D);
  if (!Continue)
    Cancelled = true;
  return Continue;
}

std::pair<bool, Stmt *> SemaAnnotator::walkToStmtPre(Stmt *S) {
  bool TraverseChildren = SEWalker.walkToStmtPre(S);
  return { TraverseChildren, S };
}

Stmt *SemaAnnotator::walkToStmtPost(Stmt *S) {
  bool Continue = SEWalker.walkToStmtPost(S);
  if (!Continue)
    Cancelled = true;
  return Continue ? S : nullptr;
}

std::pair<bool, Expr *> SemaAnnotator::walkToExprPre(Expr *E) {
  if (isDone())
    return { false, nullptr };

  if (!SEWalker.walkToExprPre(E))
    return { false, E };

  if (ConstructorRefCallExpr *CtorRefE = dyn_cast<ConstructorRefCallExpr>(E))
    CtorRefs.push_back(CtorRefE);

  if (E->isImplicit())
    return { true, E };

  if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E)) {
    if (auto *module = dyn_cast<ModuleDecl>(DRE->getDecl())) {
      if (!passReference(ModuleEntity(module), std::make_pair(module->getName(), E->getLoc())))
        return { false, nullptr };
    } else if (!passReference(DRE->getDecl(), E->getLoc())) {
      return { false, nullptr };
    }
  } else if (MemberRefExpr *MRE = dyn_cast<MemberRefExpr>(E)) {
    // Visit in source order.
    if (!MRE->getBase()->walk(*this))
      return { false, nullptr };
    if (!passReference(MRE->getMember().getDecl(), E->getLoc()))
      return { false, nullptr };

    // We already visited the children.
    if (!walkToExprPost(E))
      return { false, nullptr };
    return { false, E };

  } else if (auto OtherCtorE = dyn_cast<OtherConstructorDeclRefExpr>(E)) {
    if (!passReference(OtherCtorE->getDecl(), OtherCtorE->getConstructorLoc()))
      return { false, nullptr };

  } else if (SubscriptExpr *SE = dyn_cast<SubscriptExpr>(E)) {
    if (SE->hasDecl())
      if (!passReference(SE->getDecl().getDecl(), E->getLoc()))
        return { false, nullptr };

  } else if (BinaryExpr *BinE = dyn_cast<BinaryExpr>(E)) {
    // Visit in source order.
    if (!BinE->getArg()->getElement(0)->walk(*this))
      return { false, nullptr };
    if (!BinE->getFn()->walk(*this))
      return { false, nullptr };
    if (!BinE->getArg()->getElement(1)->walk(*this))
      return { false, nullptr };

    // We already visited the children.
    if (!walkToExprPost(E))
      return { false, nullptr };
    return { false, E };

  } else if (auto TupleE = dyn_cast<TupleExpr>(E)) {
    if (auto CallE = dyn_cast_or_null<CallExpr>(Parent.getAsExpr())) {
      if (!passCallArgNames(CallE->getFn(), TupleE))
        return { false, nullptr };
    }
  }

  return { true, E };
}

bool SemaAnnotator::walkToTypeReprPre(TypeRepr *T) {
  if (isDone())
    return false;

  if (auto IdT = dyn_cast<ComponentIdentTypeRepr>(T)) {
    if (ValueDecl *VD = IdT->getBoundDecl())
      return passReference(VD, IdT->getIdLoc());
    if (TypeDecl *TyD = getTypeDecl(IdT->getBoundType())) {
      if (ModuleDecl *ModD = dyn_cast<ModuleDecl>(TyD))
        return passReference(ModD, std::make_pair(IdT->getIdentifier(),
                                                  IdT->getIdLoc()));

      return passReference(TyD, IdT->getIdLoc());
    }
  }
  return true;
}

Expr *SemaAnnotator::walkToExprPost(Expr *E) {
  if (isa<ConstructorRefCallExpr>(E)) {
    assert(CtorRefs.back() == E);
    CtorRefs.pop_back();
  }

  bool Continue = SEWalker.walkToExprPost(E);
  if (!Continue)
    Cancelled = true;
  return Continue ? E : nullptr;
}

bool SemaAnnotator::walkToTypeReprPost(TypeRepr *T) {
  if (isDone())
    return false;
  return true;
}

std::pair<bool, Pattern *> SemaAnnotator::walkToPatternPre(Pattern *P) {
  auto *TP = dyn_cast<TypedPattern>(P);
  if (!TP || !TP->isPropagatedType())
    return { true, P };

  // If the typed pattern was propagated from somewhere, just walk the
  // subpattern.  The type will be walked as a part of another TypedPattern.
  TP->getSubPattern()->walk(*this);
  return { false, P };
}

bool SemaAnnotator::handleImports(ImportDecl *Import) {
  auto Mod = Import->getModule();
  if (!Mod)
    return true;

  auto ClangMod = Mod->findUnderlyingClangModule();
  if (ClangMod && ClangMod->isSubModule()) {
    if (!passModulePathElements(Import->getModulePath(), ClangMod))
      return false;
  } else {
    if (!passReference(Mod, Import->getModulePath().front()))
      return false;
  }

  auto Decls = Import->getDecls();
  if (Decls.size() == 1) {
    if (!passReference(Decls.front(), Import->getEndLoc()))
      return false;
  }

  return true;
}

bool SemaAnnotator::passModulePathElements(
    ArrayRef<ImportDecl::AccessPathElement> Path,
    const clang::Module *ClangMod) {

  if (Path.empty() || !ClangMod)
    return true;

  if (!passModulePathElements(Path.drop_back(1), ClangMod->Parent))
    return false;

  return passReference(ClangMod, Path.back());
}

bool SemaAnnotator::passReference(ValueDecl *D, SourceLoc Loc) {
  unsigned NameLen = D->getName().getLength();
  TypeDecl *CtorTyRef = nullptr;

  if (TypeDecl *TD = dyn_cast<TypeDecl>(D)) {
    if (!CtorRefs.empty() && Loc.isValid()) {
      Expr *Fn = CtorRefs.back()->getFn();
      if (Fn->getLoc() == Loc) {
        D = extractDecl(Fn);
        CtorTyRef = TD;
      }
    }
  }

  CharSourceRange Range = (Loc.isValid()) ? CharSourceRange(Loc, NameLen)
                                          : CharSourceRange();
  bool Continue = SEWalker.visitDeclReference(D, Range, CtorTyRef);
  if (!Continue)
    Cancelled = true;
  return Continue;
}

bool SemaAnnotator::passReference(ModuleEntity Mod,
                                  std::pair<Identifier, SourceLoc> IdLoc) {
  if (IdLoc.second.isInvalid())
    return true;
  unsigned NameLen = IdLoc.first.getLength();
  CharSourceRange Range{ IdLoc.second, NameLen };
  bool Continue = SEWalker.visitModuleReference(Mod, Range);
  if (!Continue)
    Cancelled = true;
  return Continue;
}

bool SemaAnnotator::passCallArgNames(Expr *Fn, TupleExpr *TupleE) {
  ValueDecl *D = extractDecl(Fn);
  if (!D)
    return true; // continue.

  ArrayRef<Identifier> ArgNames = TupleE->getElementNames();
  ArrayRef<SourceLoc> ArgLocs = TupleE->getElementNameLocs();
  assert(ArgNames.size() == ArgLocs.size());
  for (auto i : indices(ArgNames)) {
    Identifier Name = ArgNames[i];
    SourceLoc Loc = ArgLocs[i];
    if (Name.empty() || Loc.isInvalid())
      continue;

    CharSourceRange Range{ Loc, Name.getLength() };
    bool Continue = SEWalker.visitCallArgName(Name, Range, D);
    if (!Continue) {
      Cancelled = true;
      return false;
    }
  }

  return true;
}

TypeDecl *SemaAnnotator::getTypeDecl(Type Ty) {
  if (Ty.isNull())
    return nullptr;

  if (NameAliasType *NAT = dyn_cast<NameAliasType>(Ty.getPointer()))
    return NAT->getDecl();
  if (ModuleType *MT = dyn_cast<ModuleType>(Ty.getPointer()))
    return MT->getModule();

  return Ty->getAnyNominal();
}

bool SemaAnnotator::shouldIgnore(Decl *D, bool &ShouldVisitChildren) {
  if (D->isImplicit() && !isa<PatternBindingDecl>(D)) {
    ShouldVisitChildren = false;
    return true;
  }
  return false;
}

bool SourceEntityWalker::walk(SourceFile &SrcFile) {
  SemaAnnotator Annotator(*this);
  return SrcFile.walk(Annotator);
}

bool SourceEntityWalker::walk(Module &Mod) {
  SemaAnnotator Annotator(*this);
  return Mod.walk(Annotator);
}

bool SourceEntityWalker::walk(Decl *D) {
  SemaAnnotator Annotator(*this);
  return D->walk(Annotator);
}

bool SourceEntityWalker::walk(DeclContext *DC) {
  SemaAnnotator Annotator(*this);
  return DC->walkContext(Annotator);
}

bool SourceEntityWalker::visitCallArgName(Identifier Name,
                                          CharSourceRange Range,
                                          ValueDecl *D) {
  return true;
}

bool SourceEntityWalker::visitModuleReference(ModuleEntity Mod,
                                              CharSourceRange Range) {
  return true;
}

void SourceEntityWalker::anchor() {}

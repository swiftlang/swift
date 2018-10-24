//===--- SourceEntityWalker.cpp - Routines for semantic source info -------===//
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

#include "swift/IDE/SourceEntityWalker.h"
#include "swift/Parse/Lexer.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"
#include "swift/AST/Types.h"
#include "swift/Basic/SourceManager.h"
#include "clang/Basic/Module.h"

using namespace swift;

namespace {

class SemaAnnotator : public ASTWalker {
  SourceEntityWalker &SEWalker;
  SmallVector<ConstructorRefCallExpr *, 2> CtorRefs;
  SmallVector<ExtensionDecl *, 2> ExtDecls;
  llvm::SmallDenseMap<OpaqueValueExpr *, Expr *, 4> OpaqueValueMap;
  bool Cancelled = false;
  Optional<AccessKind> OpAccess;

public:
  explicit SemaAnnotator(SourceEntityWalker &SEWalker)
    : SEWalker(SEWalker) { }

  bool isDone() const { return Cancelled; }

private:
  bool shouldWalkIntoGenericParams() override {
    return SEWalker.shouldWalkIntoGenericParams();
  }
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

  bool passReference(ValueDecl *D, Type Ty, SourceLoc Loc, SourceRange Range,
                     ReferenceMetaData Data);
  bool passReference(ValueDecl *D, Type Ty, DeclNameLoc Loc, ReferenceMetaData Data);
  bool passReference(ModuleEntity Mod, std::pair<Identifier, SourceLoc> IdLoc);

  bool passSubscriptReference(ValueDecl *D, SourceLoc Loc,
                              Optional<AccessKind> AccKind,
                              bool IsOpenBracket);

  bool passCallArgNames(Expr *Fn, TupleExpr *TupleE);

  bool shouldIgnore(Decl *D, bool &ShouldVisitChildren);

  ValueDecl *extractDecl(Expr *Fn) const {
    if (auto *DRE = dyn_cast<DeclRefExpr>(Fn))
      return DRE->getDecl();
    if (auto ApplyE = dyn_cast<ApplyExpr>(Fn))
      return extractDecl(ApplyE->getFn());
    return nullptr;
  }
};

} // end anonymous namespace

bool SemaAnnotator::walkToDeclPre(Decl *D) {
  if (isDone())
    return false;

  bool ShouldVisitChildren;
  if (shouldIgnore(D, ShouldVisitChildren))
    return ShouldVisitChildren;

  SourceLoc Loc = D->getLoc();
  unsigned NameLen = 0;
  bool IsExtension = false;

  if (auto *VD = dyn_cast<ValueDecl>(D)) {
    if (VD->hasName() && !VD->isImplicit())
      NameLen = VD->getBaseName().userFacingName().size();

    auto ReportParamList = [&](ParameterList *PL) {
      for (auto *PD : *PL) {
        auto Loc = PD->getArgumentNameLoc();
        if (Loc.isInvalid())
          continue;
        if (!SEWalker.visitDeclarationArgumentName(PD->getArgumentName(), Loc,
                                                   VD)) {
          Cancelled = true;
          return true;
        }
      }
      return false;
    };

    if (auto AF = dyn_cast<AbstractFunctionDecl>(VD)) {
      if (ReportParamList(AF->getParameters()))
        return false;
    }
    if (auto SD = dyn_cast<SubscriptDecl>(VD)) {
      if (ReportParamList(SD->getIndices()))
        return false;
    }
  } else if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
    SourceRange SR = ED->getExtendedTypeLoc().getSourceRange();
    Loc = SR.Start;
    if (Loc.isValid())
      NameLen = ED->getASTContext().SourceMgr.getByteDistance(SR.Start, SR.End);
    IsExtension = true;
  } else if (auto Import = dyn_cast<ImportDecl>(D)) {
    if (!handleImports(Import))
      return false;

  } else if (auto OpD = dyn_cast<OperatorDecl>(D)) {
    Loc = OpD->getLoc();
    if (Loc.isValid())
      NameLen = OpD->getName().getLength();

  } else if (auto PrecD = dyn_cast<PrecedenceGroupDecl>(D)) {
    Loc = PrecD->getLoc();
    if (Loc.isValid())
      NameLen = PrecD->getName().getLength();

  } else if (auto *ICD = dyn_cast<IfConfigDecl>(D)) {
    if (SEWalker.shouldWalkInactiveConfigRegion()) {
      for (auto Clause : ICD->getClauses()) {
        for (auto Member : Clause.Elements) {
          Member.walk(*this);
        }
      }
      return false;
    }
  } else {
    return true;
  }

  CharSourceRange Range = (Loc.isValid()) ? CharSourceRange(Loc, NameLen)
                                          : CharSourceRange();
  ShouldVisitChildren = SEWalker.walkToDeclPre(D, Range);
  if (ShouldVisitChildren && IsExtension) {
    ExtDecls.push_back(static_cast<ExtensionDecl*>(D));
  }
  return ShouldVisitChildren;
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

  if (isa<ExtensionDecl>(D)) {
    assert(ExtDecls.back() == D);
    ExtDecls.pop_back();
  }

  if (!isa<ValueDecl>(D) && !isa<ExtensionDecl>(D) && !isa<ImportDecl>(D) &&
      !isa<IfConfigDecl>(D))
    return true;

  bool Continue = SEWalker.walkToDeclPost(D);
  if (!Continue)
    Cancelled = true;
  return Continue;
}

std::pair<bool, Stmt *> SemaAnnotator::walkToStmtPre(Stmt *S) {
  bool TraverseChildren = SEWalker.walkToStmtPre(S);
  if (TraverseChildren) {
    if (auto *DeferS = dyn_cast<DeferStmt>(S)) {
      // Since 'DeferStmt::getTempDecl()' is marked as implicit, we manually
      // walk into the body.
      if (auto *FD = DeferS->getTempDecl()) {
        auto *RetS = FD->getBody()->walk(*this);
        if (!RetS)
          return { false, nullptr };
        assert(RetS == FD->getBody());
      }
      bool Continue = SEWalker.walkToStmtPost(DeferS);
      if (!Continue)
        Cancelled = true;
      // Already walked children.
      return { false, Continue ? DeferS : nullptr };
    }
  }
  return { TraverseChildren, S };
}

Stmt *SemaAnnotator::walkToStmtPost(Stmt *S) {
  bool Continue = SEWalker.walkToStmtPost(S);
  if (!Continue)
    Cancelled = true;
  return Continue ? S : nullptr;
}

static SemaReferenceKind getReferenceKind(Expr *Parent, Expr *E) {
  if (auto SA = dyn_cast_or_null<SelfApplyExpr>(Parent)) {
    if (SA->getFn() == E)
      return SemaReferenceKind::DeclMemberRef;
  }
  return SemaReferenceKind::DeclRef;
}

std::pair<bool, Expr *> SemaAnnotator::walkToExprPre(Expr *E) {
  assert(E);

  if (isDone())
    return { false, nullptr };

  if (!SEWalker.walkToExprPre(E))
    return { false, E };

  if (auto *CtorRefE = dyn_cast<ConstructorRefCallExpr>(E))
    CtorRefs.push_back(CtorRefE);

  if (!isa<InOutExpr>(E) &&
      !isa<LoadExpr>(E) &&
      !isa<OpenExistentialExpr>(E) &&
      !isa<MakeTemporarilyEscapableExpr>(E) &&
      !isa<CollectionUpcastConversionExpr>(E) &&
      !isa<OpaqueValueExpr>(E) &&
      E->isImplicit())
    return { true, E };

  if (auto *DRE = dyn_cast<DeclRefExpr>(E)) {
    if (auto *module = dyn_cast<ModuleDecl>(DRE->getDecl())) {
      if (!passReference(ModuleEntity(module),
                         std::make_pair(module->getName(), E->getLoc())))
        return { false, nullptr };
    } else if (!passReference(DRE->getDecl(), DRE->getType(),
                              DRE->getNameLoc(),
                      ReferenceMetaData(getReferenceKind(Parent.getAsExpr(), DRE),
                                        OpAccess))) {
      return { false, nullptr };
    }
  } else if (auto *MRE = dyn_cast<MemberRefExpr>(E)) {
    {
      // This could be made more accurate if the member is nonmutating,
      // or whatever.
      Optional<AccessKind> NewOpAccess;
      if (OpAccess) {
        if (*OpAccess == AccessKind::Write)
          NewOpAccess = AccessKind::ReadWrite;
        else
          NewOpAccess = OpAccess;
      }

      llvm::SaveAndRestore<Optional<AccessKind>>
        C(this->OpAccess, NewOpAccess);

      // Visit in source order.
      if (!MRE->getBase()->walk(*this))
        return { false, nullptr };
    }

    if (!passReference(MRE->getMember().getDecl(), MRE->getType(),
                       MRE->getNameLoc(),
                       ReferenceMetaData(SemaReferenceKind::DeclMemberRef,
                                         OpAccess)))
      return { false, nullptr };

    // We already visited the children.
    if (!walkToExprPost(E))
      return { false, nullptr };
    return { false, E };

  } else if (auto OtherCtorE = dyn_cast<OtherConstructorDeclRefExpr>(E)) {
    if (!passReference(OtherCtorE->getDecl(), OtherCtorE->getType(),
                       OtherCtorE->getConstructorLoc(),
                       ReferenceMetaData(SemaReferenceKind::DeclConstructorRef,
                                         OpAccess)))
      return { false, nullptr };

  } else if (auto *SE = dyn_cast<SubscriptExpr>(E)) {
    // Visit in source order.
    if (!SE->getBase()->walk(*this))
      return { false, nullptr };

    ValueDecl *SubscrD = nullptr;
    if (SE->hasDecl())
      SubscrD = SE->getDecl().getDecl();

    if (SubscrD) {
      if (!passSubscriptReference(SubscrD, E->getLoc(), OpAccess, true))
        return { false, nullptr };
    }

    if (!SE->getIndex()->walk(*this))
      return { false, nullptr };

    if (SubscrD) {
      if (!passSubscriptReference(SubscrD, E->getEndLoc(), OpAccess, false))
        return { false, nullptr };
    }

    // We already visited the children.
    if (!walkToExprPost(E))
      return { false, nullptr };
    return { false, E };

  } else if (auto *KPE = dyn_cast<KeyPathExpr>(E)) {
    for (auto &component : KPE->getComponents()) {
      switch (component.getKind()) {
      case KeyPathExpr::Component::Kind::Property:
      case KeyPathExpr::Component::Kind::Subscript:
      case KeyPathExpr::Component::Kind::Type: {
        auto *decl = component.getDeclRef().getDecl();
        auto loc = component.getLoc();
        SourceRange range(loc, loc);
        passReference(decl, component.getComponentType(), loc, range,
                      ReferenceMetaData(
                        (isa<SubscriptDecl>(decl)
                          ? SemaReferenceKind::SubscriptRef
                          : SemaReferenceKind::DeclMemberRef),
                        OpAccess));
        break;
      }

      case KeyPathExpr::Component::Kind::Invalid:
      case KeyPathExpr::Component::Kind::UnresolvedProperty:
      case KeyPathExpr::Component::Kind::UnresolvedSubscript:
      case KeyPathExpr::Component::Kind::OptionalChain:
      case KeyPathExpr::Component::Kind::OptionalWrap:
      case KeyPathExpr::Component::Kind::OptionalForce:
      case KeyPathExpr::Component::Kind::Identity:
        break;
      }
    }
  } else if (auto *BinE = dyn_cast<BinaryExpr>(E)) {
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
  } else if (auto IOE = dyn_cast<InOutExpr>(E)) {
    llvm::SaveAndRestore<Optional<AccessKind>>
      C(this->OpAccess, AccessKind::ReadWrite);

    if (!IOE->getSubExpr()->walk(*this))
      return { false, nullptr };

    // We already visited the children.
    if (!walkToExprPost(E))
      return { false, nullptr };
    return { false, E };
  } else if (auto LE = dyn_cast<LoadExpr>(E)) {
    llvm::SaveAndRestore<Optional<AccessKind>>
      C(this->OpAccess, AccessKind::Read);

    if (!LE->getSubExpr()->walk(*this))
      return { false, nullptr };

    // We already visited the children.
    if (!walkToExprPost(E))
      return { false, nullptr };
    return { false, E };
  } else if (auto AE = dyn_cast<AssignExpr>(E)) {
    {
      llvm::SaveAndRestore<Optional<AccessKind>>
        C(this->OpAccess, AccessKind::Write);

      if (AE->getDest() && !AE->getDest()->walk(*this))
        return { false, nullptr };
    }

    if (AE->getSrc() && !AE->getSrc()->walk(*this))
      return { false, nullptr };

    // We already visited the children.
    if (!walkToExprPost(E))
      return { false, nullptr };
    return { false, E };
  } else if (auto OEE = dyn_cast<OpenExistentialExpr>(E)) {
    // Record opaque value.
    OpaqueValueMap[OEE->getOpaqueValue()] = OEE->getExistentialValue();
    SWIFT_DEFER {
      OpaqueValueMap.erase(OEE->getOpaqueValue());
    };

    if (!OEE->getSubExpr()->walk(*this))
      return { false, nullptr };
    if (!walkToExprPost(E))
      return { false, nullptr };
    return { false, E };
  } else if (auto MTEE = dyn_cast<MakeTemporarilyEscapableExpr>(E)) {
    // Manually walk to original arguments in order. We don't handle
    // OpaqueValueExpr here.

    // Original non-escaping closure.
    if (!MTEE->getNonescapingClosureValue()->walk(*this))
      return { false, nullptr };

    // Body, which is called by synthesized CallExpr.
    auto *callExpr = cast<CallExpr>(MTEE->getSubExpr());
    if (!callExpr->getFn()->walk(*this))
      return { false, nullptr };

    if (!walkToExprPost(E))
      return { false, nullptr };
    return { false, E };
  } else if (auto CUCE = dyn_cast<CollectionUpcastConversionExpr>(E)) {
    // Ignore conversion expressions. We don't handle OpaqueValueExpr here
    // because it's only in conversion expressions. Instead, just walk into
    // sub expression.
    if (!CUCE->getSubExpr()->walk(*this))
      return { false, nullptr };
    if (!walkToExprPost(E))
      return { false, nullptr };
    return { false, E };
  } else if (auto OVE = dyn_cast<OpaqueValueExpr>(E)) {
    // Walk into mapped value.
    auto value = OpaqueValueMap.find(OVE);
    if (value != OpaqueValueMap.end()) {
      if (!value->second->walk(*this))
        return { false, nullptr };
      if (!walkToExprPost(E))
        return { false, nullptr };
      return { false, E };
    }
  }

  return { true, E };
}

bool SemaAnnotator::walkToTypeReprPre(TypeRepr *T) {
  if (isDone())
    return false;

  if (auto IdT = dyn_cast<ComponentIdentTypeRepr>(T)) {
    if (ValueDecl *VD = IdT->getBoundDecl()) {
      if (auto *ModD = dyn_cast<ModuleDecl>(VD))
        return passReference(ModD, std::make_pair(IdT->getIdentifier(),
                                                  IdT->getIdLoc()));

      return passReference(VD, Type(), DeclNameLoc(IdT->getIdLoc()),
                           ReferenceMetaData(SemaReferenceKind::TypeRef, None));
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
  return !isDone();
}

std::pair<bool, Pattern *> SemaAnnotator::walkToPatternPre(Pattern *P) {
  if (P->isImplicit())
    return { true, P };

  if (auto *EP = dyn_cast<EnumElementPattern>(P)) {
    auto *Element = EP->getElementDecl();
    if (!Element)
      return { true, P };
    Type T = EP->hasType() ? EP->getType() : Type();
    return { passReference(Element, T, DeclNameLoc(EP->getLoc()),
                ReferenceMetaData(SemaReferenceKind::EnumElementRef, None)), P };
  }

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
    // FIXME: ImportDecl should store a DeclNameLoc.
    // FIXME: Handle overloaded funcs too by passing a reference for each?
    if (!passReference(Decls.front(), Type(), DeclNameLoc(Import->getEndLoc()),
        ReferenceMetaData(SemaReferenceKind::DeclRef, None)))
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

bool SemaAnnotator::passSubscriptReference(ValueDecl *D, SourceLoc Loc,
                                           Optional<AccessKind> AccKind,
                                           bool IsOpenBracket) {
  CharSourceRange Range = Loc.isValid()
                        ? CharSourceRange(Loc, 1)
                        : CharSourceRange();

  bool Continue = SEWalker.visitSubscriptReference(D, Range, AccKind,
                                                   IsOpenBracket);
  if (!Continue)
    Cancelled = true;
  return Continue;
}

bool SemaAnnotator::
passReference(ValueDecl *D, Type Ty, DeclNameLoc Loc, ReferenceMetaData Data) {
  return passReference(D, Ty, Loc.getBaseNameLoc(), Loc.getSourceRange(), Data);
}

bool SemaAnnotator::
passReference(ValueDecl *D, Type Ty, SourceLoc BaseNameLoc, SourceRange Range,
              ReferenceMetaData Data) {
  TypeDecl *CtorTyRef = nullptr;
  ExtensionDecl *ExtDecl = nullptr;

  if (auto *TD = dyn_cast<TypeDecl>(D)) {
    if (!CtorRefs.empty() && BaseNameLoc.isValid()) {
      Expr *Fn = CtorRefs.back()->getFn();
      if (Fn->getLoc() == BaseNameLoc) {
        D = extractDecl(Fn);
        CtorTyRef = TD;
      }
    }

    if (!ExtDecls.empty() && BaseNameLoc.isValid()) {
      auto ExtTyLoc = ExtDecls.back()->getExtendedTypeLoc().getLoc();
      if (ExtTyLoc.isValid() && ExtTyLoc == BaseNameLoc) {
        ExtDecl = ExtDecls.back();
      }
    }
  }

  CharSourceRange CharRange =
    Lexer::getCharSourceRangeFromSourceRange(D->getASTContext().SourceMgr,
                                             Range);
  bool Continue = SEWalker.visitDeclReference(D, CharRange, CtorTyRef, ExtDecl,
                                              Ty, Data);
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
  for (auto i : indices(ArgNames)) {
    Identifier Name = ArgNames[i];
    if (Name.empty())
      continue;

    SourceLoc Loc = ArgLocs[i];
    if (Loc.isInvalid())
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

bool SemaAnnotator::shouldIgnore(Decl *D, bool &ShouldVisitChildren) {
  if (D->isImplicit() &&
      !isa<PatternBindingDecl>(D) &&
      !isa<ConstructorDecl>(D)) {
    ShouldVisitChildren = false;
    return true;
  }
  return false;
}

bool SourceEntityWalker::walk(SourceFile &SrcFile) {
  SemaAnnotator Annotator(*this);
  return SrcFile.walk(Annotator);
}

bool SourceEntityWalker::walk(ModuleDecl &Mod) {
  SemaAnnotator Annotator(*this);
  return Mod.walk(Annotator);
}

bool SourceEntityWalker::walk(Stmt *S) {
  SemaAnnotator Annotator(*this);
  return S->walk(Annotator);
}

bool SourceEntityWalker::walk(Expr *E) {
  SemaAnnotator Annotator(*this);
  return E->walk(Annotator);
}

bool SourceEntityWalker::walk(Decl *D) {
  SemaAnnotator Annotator(*this);
  return D->walk(Annotator);
}

bool SourceEntityWalker::walk(DeclContext *DC) {
  SemaAnnotator Annotator(*this);
  return DC->walkContext(Annotator);
}

bool SourceEntityWalker::walk(ASTNode N) {
  if (auto *E = N.dyn_cast<Expr*>())
    return walk(E);
  if (auto *S = N.dyn_cast<Stmt*>())
    return walk(S);
  if (auto *D = N.dyn_cast<Decl*>())
    return walk(D);

  llvm_unreachable("unsupported AST node");
}

bool SourceEntityWalker::visitDeclReference(ValueDecl *D, CharSourceRange Range,
                                            TypeDecl *CtorTyRef,
                                            ExtensionDecl *ExtTyRef, Type T,
                                            ReferenceMetaData Data) {
  return true;
}

bool SourceEntityWalker::visitSubscriptReference(ValueDecl *D,
                                                 CharSourceRange Range,
                                                 Optional<AccessKind> AccKind,
                                                 bool IsOpenBracket) {
  // Most of the clients treat subscript reference the same way as a
  // regular reference when called on the open bracket and
  // ignore the closing one.
  return IsOpenBracket ? visitDeclReference(D, Range, nullptr, nullptr, Type(),
    ReferenceMetaData(SemaReferenceKind::SubscriptRef, AccKind)) : true;
}

bool SourceEntityWalker::visitCallArgName(Identifier Name,
                                          CharSourceRange Range,
                                          ValueDecl *D) {
  return true;
}

bool SourceEntityWalker::
visitDeclarationArgumentName(Identifier Name, SourceLoc Start, ValueDecl *D) {
  return true;
}

bool SourceEntityWalker::visitModuleReference(ModuleEntity Mod,
                                              CharSourceRange Range) {
  return true;
}

void SourceEntityWalker::anchor() {}

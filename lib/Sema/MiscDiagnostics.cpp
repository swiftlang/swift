//===--- MiscDiagnostics.cpp - AST-Level Diagnostics ----------------------===//
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
// This file implements AST-level diagnostics.
//
//===----------------------------------------------------------------------===//

#include "MiscDiagnostics.h"
#include "TypeChecker.h"
#include "swift/Basic/SourceManager.h"
#include "swift/AST/ASTWalker.h"
#include "swift/Parse/Lexer.h"

using namespace swift;

//===--------------------------------------------------------------------===//
// Diagnose assigning variable to itself.
//===--------------------------------------------------------------------===//

static Decl *findSimpleReferencedDecl(const Expr *E) {
  if (auto *LE = dyn_cast<LoadExpr>(E))
    E = LE->getSubExpr();

  if (auto *DRE = dyn_cast<DeclRefExpr>(E))
    return DRE->getDecl();

  return nullptr;
}

static std::pair<Decl *, Decl *> findReferencedDecl(const Expr *E) {
  if (auto *LE = dyn_cast<LoadExpr>(E))
    E = LE->getSubExpr();

  if (auto *D = findSimpleReferencedDecl(E))
    return std::make_pair(nullptr, D);

  if (auto *MRE = dyn_cast<MemberRefExpr>(E)) {
    if (auto *BaseDecl = findSimpleReferencedDecl(MRE->getBase()))
      return std::make_pair(BaseDecl, MRE->getMember().getDecl());
  }

  return std::make_pair(nullptr, nullptr);
}

/// Diagnose assigning variable to itself.
static void diagSelfAssignment(TypeChecker &TC, const Expr *E) {
  auto *AE = dyn_cast<AssignExpr>(E);
  if (!AE)
    return;

  auto LHSDecl = findReferencedDecl(AE->getDest());
  auto RHSDecl = findReferencedDecl(AE->getSrc());
  if (LHSDecl.second && LHSDecl == RHSDecl) {
    TC.diagnose(AE->getLoc(), LHSDecl.first ? diag::self_assignment_prop
                                            : diag::self_assignment_var)
        .highlight(AE->getDest()->getSourceRange())
        .highlight(AE->getSrc()->getSourceRange());
  }
}


/// Issue a warning on code where a returned expression is on a different line
/// than the return keyword, but both have the same indentation.
///
/// \code
///   ...
///   return
///   foo()
/// \endcode
static void diagUnreachableCode(TypeChecker &TC, const Stmt *S) {
  auto *RS = dyn_cast<ReturnStmt>(S);
  if (!RS)
    return;
  if (!RS->hasResult())
    return;

  auto RetExpr = RS->getResult();
  auto RSLoc = RS->getStartLoc();
  auto RetExprLoc = RetExpr->getStartLoc();
  // FIXME: Expose getColumnNumber() in LLVM SourceMgr to make this check
  // cheaper.
  if (RSLoc.isInvalid() || RetExprLoc.isInvalid() || (RSLoc == RetExprLoc))
    return;
  SourceManager &SM = TC.Context.SourceMgr;
  if (SM.getLineAndColumn(RSLoc).second ==
      SM.getLineAndColumn(RetExprLoc).second) {
    TC.diagnose(RetExpr->getStartLoc(), diag::unindented_code_after_return);
    TC.diagnose(RetExpr->getStartLoc(), diag::indent_expression_to_silence);
    return;
  }
  return;
}


/// Diagnose use of module or metatype values outside of dot expressions.
static void diagModuleOrMetatypeValue(TypeChecker &TC, const Expr *E) {
  class DiagnoseWalker : public ASTWalker {
  public:
    TypeChecker &TC;

    DiagnoseWalker(TypeChecker &TC) : TC(TC) {}

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      // Diagnose module values that don't appear as part of a qualification.
      if (auto *ME = dyn_cast<ModuleExpr>(E)) {
        bool Diagnose = true;
        if (auto *ParentExpr = Parent.getAsExpr()) {
          // Allow module values as a part of:
          // - ignored base expressions;
          // - expressions that failed to type check.
          if (isa<DotSyntaxBaseIgnoredExpr>(ParentExpr) ||
              isa<UnresolvedDotExpr>(ParentExpr))
            Diagnose = false;
        }
        if (Diagnose)
          TC.diagnose(ME->getStartLoc(), diag::value_of_module_type);
        return { true, E };
      }
      
      // Diagnose metatype values that don't appear as part of a property,
      // method, or constructor reference.
      
      // See through implicit conversions.
      auto Base = E;
      while (auto Conv = dyn_cast<ImplicitConversionExpr>(Base))
        Base = Conv->getSubExpr();
      
      auto *DRE = dyn_cast<DeclRefExpr>(Base);
      auto *MRE = dyn_cast<MemberRefExpr>(Base);
      if ((DRE && isa<TypeDecl>(DRE->getDecl()))
          || (MRE && isa<TypeDecl>(MRE->getMember().getDecl()))) {
        // Allow references to types as a part of:
        // - member references T.foo, T.Type, T.self, etc. (but *not* T.type)
        // - constructor calls T()
        
        enum class Diagnostic {
          None, // OK
          UnqualifiedMetatypeValue, // type named without being accessed
          TypeOfMetatypeValue, // .type applied to a type
        } Diagnose;
        
        if (auto *ParentExpr = Parent.getAsExpr()) {
          switch (ParentExpr->getKind()) {
          case ExprKind::Error:
          case ExprKind::Call:
          case ExprKind::MemberRef:
          case ExprKind::DotSelf:
          case ExprKind::DotSyntaxCall:
          case ExprKind::ConstructorRefCall:
          case ExprKind::UnresolvedMember:
          case ExprKind::UnresolvedDot:
          case ExprKind::UnresolvedSelector:
          case ExprKind::UnresolvedSpecialize:
          case ExprKind::DotSyntaxBaseIgnored:
            Diagnose = Diagnostic::None;
            break;
              
          case ExprKind::Metatype:
            Diagnose = Diagnostic::TypeOfMetatypeValue;
            break;
              
          case ExprKind::LValueConversion:
          case ExprKind::InOutConversion:
          case ExprKind::IntegerLiteral:
          case ExprKind::FloatLiteral:
          case ExprKind::CharacterLiteral:
          case ExprKind::StringLiteral:
          case ExprKind::InterpolatedStringLiteral:
          case ExprKind::MagicIdentifierLiteral:
          case ExprKind::DiscardAssignment:
          case ExprKind::DeclRef:
          case ExprKind::SuperRef:
          case ExprKind::OtherConstructorDeclRef:
          case ExprKind::UnresolvedConstructor:
          case ExprKind::OverloadedDeclRef:
          case ExprKind::OverloadedMemberRef:
          case ExprKind::UnresolvedDeclRef:
          case ExprKind::DynamicMemberRef:
          case ExprKind::DynamicSubscript:
          case ExprKind::Sequence:
          case ExprKind::Paren:
          case ExprKind::Tuple:
          case ExprKind::Array:
          case ExprKind::Dictionary:
          case ExprKind::Subscript:
          case ExprKind::TupleElement:
          case ExprKind::Closure:
          case ExprKind::AutoClosure:
          case ExprKind::Module:
          case ExprKind::InOut:
          case ExprKind::NewArray:
          case ExprKind::RebindSelfInConstructor:
          case ExprKind::OpaqueValue:
          case ExprKind::BindOptional:
          case ExprKind::OptionalEvaluation:
          case ExprKind::ForceValue:
          case ExprKind::OpenExistential:
          case ExprKind::PrefixUnary:
          case ExprKind::PostfixUnary:
          case ExprKind::Binary:
          case ExprKind::Load:
          case ExprKind::TupleShuffle:
          case ExprKind::FunctionConversion:
          case ExprKind::CovariantFunctionConversion:
          case ExprKind::CovariantReturnConversion:
          case ExprKind::MetatypeConversion:
          case ExprKind::Erasure:
          case ExprKind::DerivedToBase:
          case ExprKind::ArchetypeToSuper:
          case ExprKind::ScalarToTuple:
          case ExprKind::InjectIntoOptional:
          case ExprKind::LValueToPointer:
          case ExprKind::ConditionalCheckedCast:
          case ExprKind::Isa:
          case ExprKind::Coerce:
          case ExprKind::If:
          case ExprKind::Assign:
          case ExprKind::DefaultValue:
          case ExprKind::UnresolvedPattern:
            Diagnose = Diagnostic::UnqualifiedMetatypeValue;
            break;
          }
        } else {
          Diagnose = Diagnostic::UnqualifiedMetatypeValue;
        }
        
        switch (Diagnose) {
        case Diagnostic::None:
          break;
            
        case Diagnostic::UnqualifiedMetatypeValue: {
          TC.diagnose(E->getStartLoc(), diag::value_of_metatype_type);
          // Add fixits to insert '()' or '.self'.
          auto endLoc = Lexer::getLocForEndOfToken(TC.Context.SourceMgr,
                                                   E->getEndLoc());
          TC.diagnose(endLoc, diag::add_parens_to_type)
            .fixItInsert(endLoc, "()");
          TC.diagnose(endLoc, diag::add_self_to_type)
            .fixItInsert(endLoc, ".self");
          break;
        }
            
        case Diagnostic::TypeOfMetatypeValue: {
          TC.diagnose(E->getStartLoc(), diag::type_of_metatype);
          // Add a fixit to replace '.type' with '.self'.
          auto metaExpr = cast<MetatypeExpr>(Parent.getAsExpr());
          auto endLoc = Lexer::getLocForEndOfToken(TC.Context.SourceMgr,
                                                   metaExpr->getMetatypeLoc());
          
          TC.diagnose(metaExpr->getMetatypeLoc(),
                      diag::add_self_to_type)
            .fixItReplaceChars(metaExpr->getMetatypeLoc(),
                               endLoc, "self");
          break;
        }
        }
        // We don't need to visit the children of a type member reference.
        return { false, E };
      }
      return { true, E };
    }
  };

  DiagnoseWalker Walker(TC);
  const_cast<Expr *>(E)->walk(Walker);
}


/// Diagnose recursive use of properties within their own accessors
static void diagRecursivePropertyAccess(TypeChecker &TC, const Expr *E,
                                        const DeclContext *DC) {
  auto fn = dyn_cast<FuncDecl>(DC);
  if (!fn || !fn->isGetterOrSetter())
    return;

  auto var = dyn_cast<VarDecl>(fn->getAccessorStorageDecl());
  if (!var)  // Ignore subscripts
    return;

  class DiagnoseWalker : public ASTWalker {
    TypeChecker &TC;
    VarDecl *Var;
    bool IsSetter;

  public:
    explicit DiagnoseWalker(TypeChecker &TC, VarDecl *var, bool isSetter)
      : TC(TC), Var(var), IsSetter(isSetter) {}

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (auto *DRE = dyn_cast<DeclRefExpr>(E)) {
        // Handle local and top-level computed variables.
        if (DRE->getDecl() == Var &&
            !DRE->isDirectPropertyAccess()) {
          bool shouldDiagnose = true;
          if (auto *ParentExpr = Parent.getAsExpr()) {
            if (isa<DotSyntaxBaseIgnoredExpr>(ParentExpr))
              shouldDiagnose = false;
            else if (IsSetter)
              shouldDiagnose = !isa<LoadExpr>(ParentExpr);
          }
          if (shouldDiagnose) {
            TC.diagnose(E->getLoc(), diag::recursive_accessor_reference,
                        Var->getName(), IsSetter);
          }
        }

      } else if (auto *MRE = dyn_cast<MemberRefExpr>(E)) {
        // Handle instance and type computed variables.
        // Find MemberRefExprs that have an implicit "self" base.
        if (MRE->getMember().getDecl() == Var &&
            isa<DeclRefExpr>(MRE->getBase()) &&
            MRE->getBase()->isImplicit() &&
            !MRE->isDirectPropertyAccess()) {
          bool shouldDiagnose = true;
          if (IsSetter)
            shouldDiagnose = !dyn_cast_or_null<LoadExpr>(Parent.getAsExpr());

          if (shouldDiagnose) {
            TC.diagnose(E->getLoc(), diag::recursive_accessor_reference,
                        Var->getName(), IsSetter);
            TC.diagnose(E->getLoc(),
                        diag::recursive_accessor_reference_silence)
              .fixItInsert(E->getStartLoc(), "self.");
          }
        }

      } else if (auto *PE = dyn_cast<IdentityExpr>(E)) {
        // Look through ParenExprs because a function argument of a single
        // rvalue will have a LoadExpr /outside/ the ParenExpr.
        return { true, PE->getSubExpr() };
      }

      return { true, E };
    }
  };

  DiagnoseWalker walker(TC, var, fn->isSetter());
  const_cast<Expr *>(E)->walk(walker);
}

/// Look for any property references in closures that lack a "self." qualifier.
/// Within a closure, we require that the source code contain "self." explicitly
/// because 'self' is captured, not the property value.  This is a common source
/// of confusion, so we force an explicit self.
static void diagnoseImplicitSelfUseInClosure(TypeChecker &TC, const Expr *E) {
  class DiagnoseWalker : public ASTWalker {
    TypeChecker &TC;
    unsigned InClosure = 0;

    // Keep track of DeclRefExpr's we've emitted diagnostics for, so we don't
    // emit the same error on both the property access and on the underlying
    // self reference.
    SmallVector<Expr*, 2> DiagnosedSelfs;

  public:
    explicit DiagnoseWalker(TypeChecker &TC) : TC(TC) {}

    /// Return true if this is an implicit reference to self.
    static bool isImplicitSelfUse(Expr *E) {
      auto *DRE = dyn_cast<DeclRefExpr>(E);
      return DRE && DRE->isImplicit() &&
             DRE->getDecl()->getName().str() == "self";
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      // If this is an explicit closure expression - not an autoclosure - then
      // we keep track of the fact that recursive walks are within the closure.
      if (isa<ClosureExpr>(E))
        ++InClosure;

      // If we see a property reference with an implicit base from within a
      // closure, then reject it as requiring an explicit "self." qualifier.  We
      // do this in explicit closures, not autoclosures, because otherwise the
      // transparence of autoclosures is lost.
      if (auto *MRE = dyn_cast<MemberRefExpr>(E))
        if (InClosure && isImplicitSelfUse(MRE->getBase())) {
          TC.diagnose(MRE->getLoc(),
                      diag::property_use_in_closure_without_explicit_self,
                      MRE->getMember().getDecl()->getName())
            .fixItInsert(MRE->getLoc(), "self.");
          DiagnosedSelfs.push_back(MRE->getBase());
        }

      // Handle method calls with a specific diagnostic + fixit.
      if (auto *DSCE = dyn_cast<DotSyntaxCallExpr>(E))
        if (InClosure && isImplicitSelfUse(DSCE->getBase()) &&
            isa<DeclRefExpr>(DSCE->getFn())) {
          auto MethodExpr = cast<DeclRefExpr>(DSCE->getFn());

          TC.diagnose(DSCE->getLoc(),
                      diag::method_call_in_closure_without_explicit_self,
                      MethodExpr->getDecl()->getName())
            .fixItInsert(DSCE->getLoc(), "self.");
          DiagnosedSelfs.push_back(DSCE->getBase());
        }

      // Catch any other implicit uses of self with a generic diagnostic.
      if (InClosure && isImplicitSelfUse(E)) {
        // Make sure this isn't a subexpression of something we've already
        // emitted a diagnostic for.
        if (!std::count(DiagnosedSelfs.begin(), DiagnosedSelfs.end(), E))
          TC.diagnose(E->getLoc(), diag::implicit_use_of_self_in_closure);
      }

      return { true, E };
    }
    
    Expr *walkToExprPost(Expr *E) {
      if (isa<ClosureExpr>(E)) {
        assert(InClosure);
        --InClosure;
      }
      
      return E;
    }
  };
  
  const_cast<Expr *>(E)->walk(DiagnoseWalker(TC));
}

//===--------------------------------------------------------------------===//
// Diagnose availability.
//===--------------------------------------------------------------------===//

/// Diagnose specific availability for a declaration.
///
/// Returns true if no further availability checking is needed to reject
/// the use of this declaration.
static bool diagAvailability(TypeChecker &TC, const AvailabilityAttr *Attr,
                             const ValueDecl *D, SourceRange R,
                             const DeclContext *DC) {


  // FIXME: Implement matching on the platform.  For now just
  // do the '*' platform (all platforms).
  if (Attr->hasPlatform())
    return false;

  if (Attr->IsUnvailable) {
    auto Name = D->getName();
    auto Message = Attr->Message;
    SourceLoc Loc = R.Start;

    if (Message.empty()) {
      TC.diagnose(Loc, diag::availability_decl_unavailable, Name)
        .highlight(R);
    } else {
      TC.diagnose(Loc, diag::availability_decl_unavailable_msg,
                  Name, Message)
        .highlight(SourceRange(Loc, Loc));
    }

    auto DLoc = D->getLoc();
    if (DLoc.isValid())
      TC.diagnose(DLoc, diag::availability_marked_unavailable, Name)
        .highlight(Attr->getRange());
  }

  return false;
}

/// Diagnose uses of unavailable declarations.
static void diagAvailability(TypeChecker &TC, const ValueDecl *D,
                             SourceRange R, const DeclContext *DC) {
  if (!D)
    return;

  for (auto Attr : D->getAttrs())
    if (auto AvailAttr = dyn_cast<AvailabilityAttr>(Attr))
      if (diagAvailability(TC, AvailAttr, D, R, DC))
        return;
}


namespace {
class AvailabilityWalker : public ASTWalker {
  TypeChecker &TC;
  const DeclContext *DC;
public:
  AvailabilityWalker(TypeChecker &TC, const DeclContext *DC)
    : TC(TC), DC(DC) {}

  virtual Expr *walkToExprPost(Expr *E) override {
    if (auto DR = dyn_cast<DeclRefExpr>(E))
      diagAvailability(TC, DR->getDecl(), DR->getSourceRange(), DC);
    if (auto MR = dyn_cast<MemberRefExpr>(E))
      diagAvailability(TC, MR->getMember().getDecl(), MR->getSourceRange(), DC);
    return E;
  }
};
}

/// Diagnose uses of unavailable declarations.
static void diagAvailability(TypeChecker &TC, const Expr *E,
                             const DeclContext *DC) {
  AvailabilityWalker walker(TC, DC);
  const_cast<Expr*>(E)->walk(walker);
}

//===--------------------------------------------------------------------===//
// High-level entry points.
//===--------------------------------------------------------------------===//

void swift::performExprDiagnostics(TypeChecker &TC, const Expr *E,
                                   const DeclContext *DC) {
  diagSelfAssignment(TC, E);
  diagModuleOrMetatypeValue(TC, E);
  diagRecursivePropertyAccess(TC, E, DC);
  diagnoseImplicitSelfUseInClosure(TC, E);
  diagAvailability(TC, E, DC);
}

void swift::performStmtDiagnostics(TypeChecker &TC, const Stmt *S) {
  return diagUnreachableCode(TC, S);
}


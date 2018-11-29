//===--- TypeCheckCaptures.cpp - Capture Analysis -------------------------===//
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
// This file implements computing capture info for closure expressions and named
// local functions.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "TypeCheckObjC.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/TypeWalker.h"
#include "swift/Basic/Defer.h"
#include "llvm/ADT/SmallPtrSet.h"
using namespace swift;

static SourceLoc getCaptureLoc(AnyFunctionRef AFR) {
  if (auto AFD = AFR.getAbstractFunctionDecl()) {
    if (auto *FD = dyn_cast<FuncDecl>(AFD)) {
      if (FD->isDeferBody()) {
        // HACK: Defer statements generate implicit FuncDecls, and hence do
        // not have valid source locations.  Instead, use the location of
        // the body.
        return FD->getBody()->getLBraceLoc();
      }
    }

    return AFD->getLoc();
  } else {
    auto ACE = AFR.getAbstractClosureExpr();
    if (auto CE = dyn_cast<ClosureExpr>(ACE)) {
      if (!CE->getInLoc().isInvalid())
        return CE->getInLoc();
    }

    return ACE->getLoc();
  }
}

namespace {

class FindCapturedVars : public ASTWalker {
  TypeChecker &TC;
  SmallVectorImpl<CapturedValue> &Captures;
  llvm::SmallDenseMap<ValueDecl*, unsigned, 4> captureEntryNumber;
  SourceLoc CaptureLoc;
  SourceLoc &GenericParamCaptureLoc;
  SourceLoc &DynamicSelfCaptureLoc;
  DynamicSelfType *&DynamicSelf;
  llvm::SmallPtrSet<ValueDecl *, 2> Diagnosed;
  /// The AbstractClosureExpr or AbstractFunctionDecl being analyzed.
  AnyFunctionRef AFR;
public:
  FindCapturedVars(TypeChecker &tc,
                   SmallVectorImpl<CapturedValue> &Captures,
                   SourceLoc &GenericParamCaptureLoc,
                   SourceLoc &DynamicSelfCaptureLoc,
                   DynamicSelfType *&DynamicSelf,
                   AnyFunctionRef AFR)
      : TC(tc), Captures(Captures),
        GenericParamCaptureLoc(GenericParamCaptureLoc),
        DynamicSelfCaptureLoc(DynamicSelfCaptureLoc),
        DynamicSelf(DynamicSelf),
        AFR(AFR) {
    CaptureLoc = getCaptureLoc(AFR);
  }

  /// \brief Check if the type of an expression references any generic
  /// type parameters, or the dynamic Self type.
  ///
  /// Note that we do not need to distinguish inner from outer generic
  /// parameters here -- if a local function has its own inner parameter
  /// list, it also implicitly captures outer parameters, even if they're
  /// not used anywhere inside the body.
  void checkType(Type type, SourceLoc loc) {

    if (!type)
      return;

    // We want to look through type aliases here.
    type = type->getCanonicalType();
    
    class TypeCaptureWalker : public TypeWalker {
      AnyFunctionRef AFR;
      std::function<void(Type)> Callback;
    public:
      explicit TypeCaptureWalker(AnyFunctionRef AFR,
                                 std::function<void(Type)> callback)
        : AFR(AFR), Callback(std::move(callback)) {}
    
      Action walkToTypePre(Type ty) override {
        Callback(ty);
        // Pseudogeneric classes don't use their generic parameters so we
        // don't need to visit them.
        if (AFR.isObjC()) {
          if (auto clas = dyn_cast_or_null<ClassDecl>(ty->getAnyNominal())) {
            if (clas->usesObjCGenericsModel()) {
              return Action::SkipChildren;
            }
          }
        }
        return Action::Continue;
      }
    };
    // If the type contains dynamic 'Self', conservatively assume we will
    // need 'Self' metadata at runtime. We could generalize the analysis
    // used below for usages of generic parameters in Objective-C
    // extensions, and re-use it here.
    //
    // For example, forming an existential from a value of type 'Self'
    // does not need the dynamic 'Self' type -- the static type will
    // suffice. Also, just passing around a value of type 'Self' does
    // not need metadata either, since it is represented as a single
    // retainable pointer. Similarly stored property access does not
    // need it, etc.
    if (type->hasDynamicSelfType()) {
      type.walk(TypeCaptureWalker(AFR, [&](Type t) {
        if (auto *dynamicSelf = t->getAs<DynamicSelfType>()) {
          if (DynamicSelfCaptureLoc.isInvalid()) {
            DynamicSelfCaptureLoc = loc;
            DynamicSelf = dynamicSelf;
          }
        }
      }));
    }

    // Similar to dynamic 'Self', IRGen doesn't really need type metadata
    // for class-bound archetypes in nearly as many cases as with opaque
    // archetypes.
    //
    // Perhaps this entire analysis should happen at the SILGen level,
    // instead, but even there we don't really have enough information to
    // perform it accurately.
    if (type->hasArchetype() || type->hasTypeParameter()) {
      type.walk(TypeCaptureWalker(AFR, [&](Type t) {
        if ((t->is<ArchetypeType>() ||
             t->is<GenericTypeParamType>()) &&
            !t->isOpenedExistential() &&
            GenericParamCaptureLoc.isInvalid()) {
          GenericParamCaptureLoc = loc;
        }
      }));
    }

    if (auto *gft = type->getAs<GenericFunctionType>()) {
      TypeCaptureWalker walker(AFR, [&](Type t) {
        if (t->is<GenericTypeParamType>() &&
            GenericParamCaptureLoc.isInvalid()) {
          GenericParamCaptureLoc = loc;
        }
      });

      for (const auto &param : gft->getParams())
        param.getOldType().walk(walker);

      gft->getResult().walk(walker);
    }
  }

  /// Add the specified capture to the closure's capture list, diagnosing it
  /// if invalid.
  void addCapture(CapturedValue capture, SourceLoc Loc) {
    auto VD = capture.getDecl();

    // Check to see if we already have an entry for this decl.
    unsigned &entryNumber = captureEntryNumber[VD];
    if (entryNumber == 0) {
      Captures.push_back(capture);
      entryNumber = Captures.size();
    } else {
      // If this already had an entry in the capture list, make sure to merge
      // the information together.  If one is noescape but the other isn't,
      // then the result is escaping.
      unsigned Flags =
        Captures[entryNumber-1].getFlags() & capture.getFlags();
      capture = CapturedValue(VD, Flags);
      Captures[entryNumber-1] = capture;
    }

    // Visit the type of the capture, if it isn't a class reference, since
    // we'd need the metadata to do so.
    if (VD->hasInterfaceType()
        && (!AFR.isObjC()
            || !isa<VarDecl>(VD)
            || !cast<VarDecl>(VD)->getType()->hasRetainablePointerRepresentation()))
      checkType(VD->getInterfaceType(), VD->getLoc());

    // If VD is a noescape decl, then the closure we're computing this for
    // must also be noescape.
    if (AFR.hasType() &&
        !AFR.getType()->hasError() &&
        VD->hasInterfaceType() &&
        VD->getInterfaceType()->is<AnyFunctionType>() &&
        VD->getInterfaceType()->castTo<AnyFunctionType>()->isNoEscape() &&
        !capture.isNoEscape() &&
        // Don't repeatedly diagnose the same thing.
        Diagnosed.insert(VD).second) {

      // Otherwise, diagnose this as an invalid capture.
      bool isDecl = AFR.getAbstractFunctionDecl() != nullptr;

      TC.diagnose(Loc, isDecl ? diag::decl_closure_noescape_use
                              : diag::closure_noescape_use,
                  VD->getBaseName().getIdentifier());

      // If we're a parameter, emit a helpful fixit to add @escaping
      auto paramDecl = dyn_cast<ParamDecl>(VD);
      if (paramDecl) {
        TC.diagnose(paramDecl->getStartLoc(), diag::noescape_parameter,
                    paramDecl->getName())
            .fixItInsert(paramDecl->getTypeLoc().getSourceRange().Start,
                         "@escaping ");
      }
    }
  }

  bool shouldWalkIntoLazyInitializers() override {
    // We don't want to walk into lazy initializers because they're not
    // really present at this level.  We'll catch them when processing
    // the getter.
    return false;
  }

  std::pair<bool, Expr *> walkToDeclRefExpr(DeclRefExpr *DRE) {
    auto *D = DRE->getDecl();

    // HACK: $interpolation variables are seen as needing to be captured. 
    // The good news is, we literally never need to capture them, so we 
    // can safely ignore them.
    // FIXME(TapExpr): This is probably caused by the scoping 
    // algorithm's ignorance of TapExpr. We should fix that.
    if (D->getBaseName() == D->getASTContext().Id_dollarInterpolation)
      return { false, DRE };

    // Capture the generic parameters of the decl, unless it's a
    // local declaration in which case we will pick up generic
    // parameter references transitively.
    if (!D->getDeclContext()->isLocalContext()) {
      if (!AFR.isObjC() || !D->isObjC() || isa<ConstructorDecl>(D)) {
        if (auto subMap = DRE->getDeclRef().getSubstitutions()) {
          auto genericSig = subMap.getGenericSignature();
          for (auto gp : genericSig->getGenericParams()) {
            if (auto type = Type(gp).subst(subMap)) {
              checkType(type, DRE->getLoc());
            }
          }
        }
      }
    }

    // DC is the DeclContext where D was defined
    // CurDC is the DeclContext where D was referenced
    auto DC = D->getDeclContext();
    auto CurDC = AFR.getAsDeclContext();

    // A local reference is not a capture.
    if (CurDC == DC)
      return { false, DRE };

    auto TmpDC = CurDC;

    if (!isa<TopLevelCodeDecl>(DC)) {
      while (TmpDC != nullptr) {
        if (TmpDC == DC)
          break;

        // The initializer of a lazy property will eventually get
        // recontextualized into it, so treat it as if it's already there.
        if (auto init = dyn_cast<PatternBindingInitializer>(TmpDC)) {
          if (auto lazyVar = init->getInitializedLazyVar()) {
            // Referring to the 'self' parameter is fine.
            if (D == init->getImplicitSelfDecl())
              return { false, DRE };

            // Otherwise, act as if we're in the getter.
            auto getter = lazyVar->getGetter();
            assert(getter && "lazy variable without getter");
            TmpDC = getter;
            continue;
          }
        }

        // We have an intervening nominal type context that is not the
        // declaration context, and the declaration context is not global.
        // This is not supported since nominal types cannot capture values.
        if (auto NTD = dyn_cast<NominalTypeDecl>(TmpDC)) {
          if (DC->isLocalContext()) {
            TC.diagnose(DRE->getLoc(), diag::capture_across_type_decl,
                        NTD->getDescriptiveKind(),
                        D->getBaseName().getIdentifier());

            TC.diagnose(NTD->getLoc(), diag::kind_declared_here,
                        DescriptiveDeclKind::Type);

            TC.diagnose(D, diag::decl_declared_here, D->getFullName());
            return { false, DRE };
          }
        }

        TmpDC = TmpDC->getParent();
      }

      // We walked all the way up to the root without finding the declaration,
      // so this is not a capture.
      if (TmpDC == nullptr)
        return { false, DRE };
    }

    // Only capture var decls at global scope.  Other things can be captured
    // if they are local.
    if (!isa<VarDecl>(D) && !DC->isLocalContext())
      return { false, DRE };

    // Can only capture a variable that is declared before the capturing
    // entity.
    llvm::DenseSet<ValueDecl *> checkedCaptures;
    llvm::SmallVector<FuncDecl *, 2> capturePath;

    std::function<bool (ValueDecl *)>
    validateForwardCapture = [&](ValueDecl *capturedDecl) -> bool {
      if (!checkedCaptures.insert(capturedDecl).second)
        return true;

      // Captures at nonlocal scope are order-invariant.
      if (!capturedDecl->getDeclContext()->isLocalContext())
        return true;

      // Assume implicit decl captures are OK.
      if (!CaptureLoc.isValid() || !capturedDecl->getLoc().isValid())
        return true;

      // Check the order of the declarations.
      if (!TC.Context.SourceMgr.isBeforeInBuffer(CaptureLoc,
                                                 capturedDecl->getLoc()))
        return true;

      // Forward captures of functions are OK, if the function doesn't
      // transitively capture variables ahead of the original function.
      if (auto func = dyn_cast<FuncDecl>(capturedDecl)) {
        if (!func->getCaptureInfo().hasBeenComputed()) {
          // Check later.
          TC.ForwardCapturedFuncs[func].push_back(AFR);
          return true;
        }
        // Recursively check the transitive captures.
        capturePath.push_back(func);
        SWIFT_DEFER { capturePath.pop_back(); };
        for (auto capture : func->getCaptureInfo().getCaptures())
          if (!validateForwardCapture(capture.getDecl()))
            return false;
        return true;
      }

      // Diagnose the improper forward capture.
      if (Diagnosed.insert(capturedDecl).second) {
        if (capturedDecl == DRE->getDecl()) {
          TC.diagnose(DRE->getLoc(), diag::capture_before_declaration,
                      capturedDecl->getBaseName().getIdentifier());
        } else {
          TC.diagnose(DRE->getLoc(),
                      diag::transitive_capture_before_declaration,
                      DRE->getDecl()->getBaseName().getIdentifier(),
                      capturedDecl->getBaseName().getIdentifier());
          ValueDecl *prevDecl = capturedDecl;
          for (auto path : reversed(capturePath)) {
            TC.diagnose(path->getLoc(),
                        diag::transitive_capture_through_here,
                        path->getName(),
                        prevDecl->getBaseName().getIdentifier());
            prevDecl = path;
          }
        }
        TC.diagnose(capturedDecl, diag::decl_declared_here,
                    capturedDecl->getFullName());
      }
      return false;
    };

    if (!validateForwardCapture(DRE->getDecl()))
      return { false, DRE };

    bool isInOut = (isa<ParamDecl>(D) && cast<ParamDecl>(D)->isInOut());
    bool isNested = false;
    if (auto f = AFR.getAbstractFunctionDecl())
      isNested = f->getDeclContext()->isLocalContext();

    if (isInOut && !AFR.isKnownNoEscape() && !isNested) {
      if (D->getBaseName() == D->getASTContext().Id_self) {
        TC.diagnose(DRE->getLoc(),
          diag::closure_implicit_capture_mutating_self);
      } else {
        TC.diagnose(DRE->getLoc(),
          diag::closure_implicit_capture_without_noescape);
      }
      return { false, DRE };
    }

    // We're going to capture this, compute flags for the capture.
    unsigned Flags = 0;

    // If this is a direct reference to underlying storage, then this is a
    // capture of the storage address - not a capture of the getter/setter.
    if (auto var = dyn_cast<VarDecl>(D)) {
      auto *DC = AFR.getAsDeclContext();
      if (var->getAccessStrategy(DRE->getAccessSemantics(),
                                 var->supportsMutation()
                                   ? AccessKind::ReadWrite
                                   : AccessKind::Read,
                                 DC->getParentModule(),
                                 DC->getResilienceExpansion())
          .getKind() == AccessStrategy::Storage)
        Flags |= CapturedValue::IsDirect;
    }

    // If the closure is noescape, then we can capture the decl as noescape.
    if (AFR.isKnownNoEscape())
      Flags |= CapturedValue::IsNoEscape;

    addCapture(CapturedValue(D, Flags), DRE->getStartLoc());
    return { false, DRE };
  }

  void propagateCaptures(AnyFunctionRef innerClosure, SourceLoc captureLoc) {
    TC.computeCaptures(innerClosure);

    auto CurDC = AFR.getAsDeclContext();
    bool isNoEscapeClosure = AFR.isKnownNoEscape();

    auto &captureInfo = innerClosure.getCaptureInfo();

    for (auto capture : captureInfo.getCaptures()) {
      // If the decl was captured from us, it isn't captured *by* us.
      if (capture.getDecl()->getDeclContext() == CurDC)
        continue;

      // Compute adjusted flags.
      unsigned Flags = capture.getFlags();

      // The decl is captured normally, even if it was captured directly
      // in the subclosure.
      Flags &= ~CapturedValue::IsDirect;

      // If this is an escaping closure, then any captured decls are also
      // escaping, even if they are coming from an inner noescape closure.
      if (!isNoEscapeClosure)
        Flags &= ~CapturedValue::IsNoEscape;

      addCapture(CapturedValue(capture.getDecl(), Flags), captureLoc);
    }

    if (GenericParamCaptureLoc.isInvalid())
      if (captureInfo.hasGenericParamCaptures())
        GenericParamCaptureLoc = getCaptureLoc(innerClosure);

    if (DynamicSelfCaptureLoc.isInvalid())
      if (captureInfo.hasDynamicSelfCapture()) {
        DynamicSelfCaptureLoc = getCaptureLoc(innerClosure);
        DynamicSelf = captureInfo.getDynamicSelfType();
      }
  }

  bool walkToDeclPre(Decl *D) override {
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
      propagateCaptures(AFD, AFD->getLoc());

      // Can default parameter initializers capture state?  That seems like
      // a really bad idea.
      for (auto *param : *AFD->getParameters())
        if (auto E = param->getDefaultValue())
          E->walk(*this);

      return false;
    }

    return true;
  }

  bool usesTypeMetadataOfFormalType(Expr *E) {
    // For non-ObjC closures, assume the type metadata is always used.
    if (!AFR.isObjC())
      return true;

    if (!E->getType() || E->getType()->hasError())
      return false;

    // We can use Objective-C generics in limited ways without reifying
    // their type metadata, meaning we don't need to capture their generic
    // params.

    // Look through one layer of optionality when considering the class-

    // Referring to a class-constrained generic or metatype
    // doesn't require its type metadata.
    if (auto declRef = dyn_cast<DeclRefExpr>(E))
      return (!declRef->getDecl()->isObjC()
              && !E->getType()->getWithoutSpecifierType()
                              ->hasRetainablePointerRepresentation()
              && !E->getType()->getWithoutSpecifierType()
                              ->is<AnyMetatypeType>());

    // Loading classes or metatypes doesn't require their metadata.
    if (isa<LoadExpr>(E))
      return (!E->getType()->hasRetainablePointerRepresentation()
              && !E->getType()->is<AnyMetatypeType>());

    // Accessing @objc members doesn't require type metadata.
    // rdar://problem/27796375 -- allocating init entry points for ObjC
    // initializers are generated as true Swift generics, so reify type
    // parameters.
    if (auto memberRef = dyn_cast<MemberRefExpr>(E))
      return !memberRef->getMember().getDecl()->hasClangNode();

    if (auto applyExpr = dyn_cast<ApplyExpr>(E)) {
      if (auto methodApply = dyn_cast<ApplyExpr>(applyExpr->getFn())) {
        if (auto callee = dyn_cast<DeclRefExpr>(methodApply->getFn())) {
          return !callee->getDecl()->isObjC()
            || isa<ConstructorDecl>(callee->getDecl());
        }
      }
      if (auto callee = dyn_cast<DeclRefExpr>(applyExpr->getFn())) {
        return !callee->getDecl()->isObjC()
          || isa<ConstructorDecl>(callee->getDecl());
      }
    }

    if (auto subscriptExpr = dyn_cast<SubscriptExpr>(E)) {
      return (subscriptExpr->hasDecl() &&
              !subscriptExpr->getDecl().getDecl()->isObjC());
    }

    // Getting the dynamic type of a class doesn't require type metadata.
    if (isa<DynamicTypeExpr>(E))
      return (!E->getType()->castTo<AnyMetatypeType>()->getInstanceType()
                  ->hasRetainablePointerRepresentation());

    // Building a fixed-size tuple doesn't require type metadata.
    // Approximate this for the purposes of being able to invoke @objc methods
    // by considering tuples of ObjC-representable types to not use metadata.
    if (auto tuple = dyn_cast<TupleExpr>(E)) {
      for (auto elt : tuple->getType()->castTo<TupleType>()->getElements()) {
        if (!elt.getType()->isRepresentableIn(ForeignLanguage::ObjectiveC,
                                              AFR.getAsDeclContext()))
          return true;
      }
      return false;
    }

    // Coercion by itself is a no-op.
    if (isa<CoerceExpr>(E))
      return false;

    // Upcasting doesn't require type metadata.
    if (isa<DerivedToBaseExpr>(E))
      return false;
    if (isa<ArchetypeToSuperExpr>(E))
      return false;
    if (isa<CovariantReturnConversionExpr>(E))
      return false;
    if (isa<MetatypeConversionExpr>(E))
      return false;

    // Identity expressions are no-ops.
    if (isa<IdentityExpr>(E))
      return false;

    // Discarding an assignment is a no-op.
    if (isa<DiscardAssignmentExpr>(E))
      return false;

    // Opening an @objc existential or metatype is a no-op.
    if (auto open = dyn_cast<OpenExistentialExpr>(E))
      return (!open->getSubExpr()->getType()->isObjCExistentialType()
              && !open->getSubExpr()->getType()->is<AnyMetatypeType>());

    // Erasure to an ObjC existential or between metatypes doesn't require
    // type metadata.
    if (auto erasure = dyn_cast<ErasureExpr>(E)) {
      if (E->getType()->isObjCExistentialType()
          || E->getType()->is<AnyMetatypeType>())
        return false;
      
      // We also special case Any erasure in pseudogeneric contexts
      // not to rely on concrete type metadata by erasing from AnyObject
      // as a waypoint.
      if (E->getType()->isAny()
          && erasure->getSubExpr()->getType()->is<ArchetypeType>())
        return false;

      // Erasure to a Swift protocol always captures the type metadata from
      // its subexpression.
      checkType(erasure->getSubExpr()->getType(),
                erasure->getSubExpr()->getLoc());
      return true;
    }

    
    // Converting an @objc metatype to AnyObject doesn't require type
    // metadata.
    if (isa<ClassMetatypeToObjectExpr>(E)
        || isa<ExistentialMetatypeToObjectExpr>(E))
      return false;
    
    // Casting to an ObjC class doesn't require the metadata of its type
    // parameters, if any.
    if (auto cast = dyn_cast<CheckedCastExpr>(E)) {
      // If we failed to resolve the written type, we've emitted an
      // earlier diagnostic and should bail.
      auto toTy = cast->getCastTypeLoc().getType();
      if (!toTy || toTy->hasError())
        return false;

      if (auto clas = dyn_cast_or_null<ClassDecl>(
                         cast->getCastTypeLoc().getType()->getAnyNominal())) {
        if (clas->usesObjCGenericsModel()) {
          return false;
        }
      }
    }
    
    // Assigning an object doesn't require type metadata.
    if (auto assignment = dyn_cast<AssignExpr>(E))
      return assignment->getSrc()->getType() &&
        !assignment->getSrc()->getType()
            ->hasRetainablePointerRepresentation();

    return true;
  }

  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    if (usesTypeMetadataOfFormalType(E)) {
      checkType(E->getType(), E->getLoc());
    }

    // Some kinds of expression don't really evaluate their subexpression,
    // so we don't need to traverse.
    if (isa<ObjCSelectorExpr>(E)) {
      return { false, E };
    }

    if (auto *ECE = dyn_cast<ExplicitCastExpr>(E)) {
      checkType(ECE->getCastTypeLoc().getType(), ECE->getLoc());
      return { true, E };
    }

    if (auto *DRE = dyn_cast<DeclRefExpr>(E))
      return walkToDeclRefExpr(DRE);

    // Look into lazy initializers.
    if (auto *LIE = dyn_cast<LazyInitializerExpr>(E)) {
      LIE->getSubExpr()->walk(*this);
      return { true, E };
    }

    // When we see a reference to the 'super' expression, capture 'self' decl.
    if (auto *superE = dyn_cast<SuperRefExpr>(E)) {
      auto CurDC = AFR.getAsDeclContext();
      if (CurDC->isChildContextOf(superE->getSelf()->getDeclContext()))
        addCapture(CapturedValue(superE->getSelf(), 0), superE->getLoc());
      return { false, superE };
    }

    // Don't recur into child closures. They should already have a capture
    // list computed; we just propagate it, filtering out stuff that they
    // capture from us.
    if (auto *SubCE = dyn_cast<AbstractClosureExpr>(E)) {
      propagateCaptures(SubCE, SubCE->getStartLoc());
      return { false, E };
    }

    return { true, E };
  }
};

} // end anonymous namespace

void TypeChecker::maybeDiagnoseCaptures(Expr *E, AnyFunctionRef AFR) {
  if (!AFR.getCaptureInfo().hasBeenComputed()) {
    // The capture list is not always initialized by the point we reference
    // it. Remember we formed a C function pointer so we can diagnose later
    // if necessary.
    LocalCFunctionPointers[AFR].push_back(E);
    return;
  }

  if (AFR.getCaptureInfo().hasGenericParamCaptures() ||
      AFR.getCaptureInfo().hasDynamicSelfCapture() ||
      AFR.getCaptureInfo().hasLocalCaptures()) {
    unsigned kind;
    if (AFR.getCaptureInfo().hasLocalCaptures())
      kind = 0;
    else if (AFR.getCaptureInfo().hasGenericParamCaptures())
      kind = 1;
    else
      kind = 2;
    diagnose(E->getLoc(),
             diag::c_function_pointer_from_function_with_context,
             /*closure*/ AFR.getAbstractClosureExpr() != nullptr,
             kind);
  }
}

void TypeChecker::computeCaptures(AnyFunctionRef AFR) {
  if (AFR.getCaptureInfo().hasBeenComputed())
    return;

  if (!AFR.getBody())
    return;

  SmallVector<CapturedValue, 4> Captures;
  SourceLoc GenericParamCaptureLoc;
  SourceLoc DynamicSelfCaptureLoc;
  DynamicSelfType *DynamicSelf = nullptr;
  FindCapturedVars finder(*this, Captures,
                          GenericParamCaptureLoc,
                          DynamicSelfCaptureLoc,
                          DynamicSelf,
                          AFR);
  if (AFR.getBody())
    AFR.getBody()->walk(finder);

  unsigned inoutCount = 0;
  for (auto C : Captures) {
    if (auto PD = dyn_cast<ParamDecl>(C.getDecl()))
      if (PD->isInOut())
        inoutCount++;
  }

  if (inoutCount > 0) {
    if (auto e = AFR.getAbstractFunctionDecl()) {
      for (auto returnOccurrence : getEscapingFunctionAsReturnValue(e)) {
        diagnose(returnOccurrence->getReturnLoc(),
          diag::nested_function_escaping_inout_capture);
      }
      auto occurrences = getEscapingFunctionAsArgument(e);
      for (auto occurrence : occurrences) {
        diagnose(occurrence->getLoc(),
          diag::nested_function_with_implicit_capture_argument,
          inoutCount > 1);
      }
    }
  }

  if (AFR.hasType() && !AFR.isObjC()) {
    finder.checkType(AFR.getType(), getCaptureLoc(AFR));
  }

  // If this is an init(), explicitly walk the initializer values for members of
  // the type.  They will be implicitly emitted by SILGen into the generated
  // initializer.
  if (auto CD =
        dyn_cast_or_null<ConstructorDecl>(AFR.getAbstractFunctionDecl())) {
    auto *typeDecl = dyn_cast<NominalTypeDecl>(CD->getDeclContext());
    if (typeDecl && CD->isDesignatedInit()) {
      for (auto member : typeDecl->getMembers()) {
        // Ignore everything other than PBDs.
        auto *PBD = dyn_cast<PatternBindingDecl>(member);
        if (!PBD) continue;
        // Walk the initializers for all properties declared in the type with
        // an initializer.
        for (auto &elt : PBD->getPatternList()) {
          if (elt.isInitializerLazy())
            continue;

          if (auto *init = elt.getInit())
            init->walk(finder);
        }
      }
    }
  }

  // A generic function always captures outer generic parameters.
  auto *AFD = AFR.getAbstractFunctionDecl();
  if (AFD) {
    if (AFD->getGenericParams())
      AFR.getCaptureInfo().setGenericParamCaptures(true);
  }

  // Only local functions capture dynamic 'Self'.
  if (AFR.getAsDeclContext()->getParent()->isLocalContext()) {
    if (GenericParamCaptureLoc.isValid())
      AFR.getCaptureInfo().setGenericParamCaptures(true);

    if (DynamicSelfCaptureLoc.isValid())
      AFR.getCaptureInfo().setDynamicSelfType(DynamicSelf);
  }

  if (Captures.empty())
    AFR.getCaptureInfo().setCaptures(None);
  else
    AFR.getCaptureInfo().setCaptures(Context.AllocateCopy(Captures));

  // Extensions of generic ObjC functions can't use generic parameters from
  // their context.
  if (AFD && GenericParamCaptureLoc.isValid()) {
    if (auto Clas = AFD->getParent()->getSelfClassDecl()) {
      if (Clas->usesObjCGenericsModel()) {
        diagnose(AFD->getLoc(),
                 diag::objc_generic_extension_using_type_parameter);

        // If it's possible, suggest adding @objc.
        Optional<ForeignErrorConvention> errorConvention;
        if (!AFD->isObjC() &&
            isRepresentableInObjC(AFD, ObjCReason::MemberOfObjCMembersClass,
                                  errorConvention)) {
          diagnose(AFD->getLoc(),
                   diag::objc_generic_extension_using_type_parameter_try_objc)
            .fixItInsert(AFD->getAttributeInsertionLoc(false), "@objc ");
        }

        diagnose(GenericParamCaptureLoc,
                 diag::objc_generic_extension_using_type_parameter_here);
      }
    }
  }

  // Diagnose if we have local captures and there were C pointers formed to
  // this function before we computed captures.
  auto cFunctionPointers = LocalCFunctionPointers.find(AFR);
  if (cFunctionPointers != LocalCFunctionPointers.end())
    for (auto *expr : cFunctionPointers->second)
      maybeDiagnoseCaptures(expr, AFR);
}

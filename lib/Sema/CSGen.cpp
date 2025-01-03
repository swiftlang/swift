//===--- CSGen.cpp - Constraint Generator ---------------------------------===//
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
// This file implements constraint generation for the type checker.
//
//===----------------------------------------------------------------------===//
#include "TypeCheckConcurrency.h"
#include "TypeCheckDecl.h"
#include "TypeCheckMacros.h"
#include "TypeCheckType.h"
#include "TypeChecker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Assertions.h"
#include "swift/Sema/ConstraintGraph.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include <utility>

using namespace swift;
using namespace swift::constraints;

static bool mergeRepresentativeEquivalenceClasses(ConstraintSystem &CS,
                                                  TypeVariableType* tyvar1,
                                                  TypeVariableType* tyvar2) {
  if (tyvar1 && tyvar2) {
    auto rep1 = CS.getRepresentative(tyvar1);
    auto rep2 = CS.getRepresentative(tyvar2);

    if (rep1 != rep2) {
      auto fixedType2 = CS.getFixedType(rep2);

      // If the there exists fixed type associated with the second
      // type variable, and we simply merge two types together it would
      // mean that portion of the constraint graph previously associated
      // with that (second) variable is going to be disconnected from its
      // new equivalence class, which is going to lead to incorrect solutions,
      // so we need to make sure to re-bind fixed to the new representative.
      if (fixedType2) {
        CS.addConstraint(ConstraintKind::Bind, fixedType2, rep1,
                         rep1->getImpl().getLocator());
      }

      CS.mergeEquivalenceClasses(rep1, rep2, /*updateWorkList*/ false);
      return true;
    }
  }

  return false;
}

namespace {
  /// If \p expr is a call and that call contains the code completion token,
  /// add the expressions of all arguments after the code completion token to
  /// \p ignoredArguments.
  /// Otherwise, returns an empty vector.
  /// Assumes that we are solving for code completion.
  void getArgumentsAfterCodeCompletionToken(
      Expr *expr, ConstraintSystem &CS,
      SmallVectorImpl<Expr *> &ignoredArguments) {
    assert(CS.isForCodeCompletion());

    /// Don't ignore the rhs argument if the code completion token is the lhs of
    /// an operator call. Main use case is the implicit `<complete> ~= $match`
    /// call created for pattern matching, in which we need to type-check
    /// `$match` to get a contextual type for `<complete>`
    if (isa<BinaryExpr>(expr)) {
      return;
    }

    auto args = expr->getArgs();
    auto argInfo = getCompletionArgInfo(expr, CS);
    if (!args || !argInfo) {
      return;
    }

    for (auto argIndex : indices(*args)) {
      if (argInfo->isBefore(argIndex)) {
        ignoredArguments.push_back(args->get(argIndex).getExpr());
      }
    }
  }
} // end anonymous namespace

void TypeVarRefCollector::inferTypeVars(Decl *D) {
  // We're only interested in VarDecls.
  if (!isa_and_nonnull<VarDecl>(D))
    return;

  auto ty = CS.getTypeIfAvailable(D);
  if (!ty)
    return;

  SmallPtrSet<TypeVariableType *, 4> typeVars;
  ty->getTypeVariables(typeVars);
  TypeVars.insert(typeVars.begin(), typeVars.end());
}

void TypeVarRefCollector::inferTypeVars(PackExpansionExpr *E) {
  auto expansionType = CS.getType(E)->castTo<PackExpansionType>();

  SmallPtrSet<TypeVariableType *, 4> referencedVars;
  expansionType->getTypeVariables(referencedVars);
  TypeVars.insert(referencedVars.begin(), referencedVars.end());
}

ASTWalker::PreWalkResult<Expr *>
TypeVarRefCollector::walkToExprPre(Expr *expr) {
  if (isa<ClosureExpr>(expr))
    DCDepth += 1;

  if (auto *DRE = dyn_cast<DeclRefExpr>(expr))
    inferTypeVars(DRE->getDecl());

  // FIXME: We can see UnresolvedDeclRefExprs here because we don't walk into
  // patterns when running preCheckTarget, since we don't resolve patterns
  // until CSGen. We ought to consider moving pattern resolution into
  // pre-checking, which would allow us to pre-check patterns normally.
  if (auto *declRef = dyn_cast<UnresolvedDeclRefExpr>(expr)) {
    auto name = declRef->getName();
    auto loc = declRef->getLoc();
    if (name.isSimpleName() && loc.isValid()) {
      auto *SF = CS.DC->getParentSourceFile();
      auto *D = ASTScope::lookupSingleLocalDecl(SF, name.getFullName(), loc);
      inferTypeVars(D);
    }
  }

  if (auto *packElement = getAsExpr<PackElementExpr>(expr)) {
    // If environment hasn't been established yet, it means that pack expansion
    // appears inside of this closure.
    if (auto *outerEnvironment = CS.getPackEnvironment(packElement))
      inferTypeVars(outerEnvironment);
  }

  return Action::Continue(expr);
}

ASTWalker::PostWalkResult<Expr *>
TypeVarRefCollector::walkToExprPost(Expr *expr) {
  if (isa<ClosureExpr>(expr))
    DCDepth -= 1;

  return Action::Continue(expr);
}

ASTWalker::PreWalkResult<Stmt *>
TypeVarRefCollector::walkToStmtPre(Stmt *stmt) {
  // If we have a return without any intermediate DeclContexts in a ClosureExpr,
  // we need to include any type variables in the closure's result type, since
  // the conjunction will generate constraints using that type. We don't need to
  // connect to returns in e.g nested closures since we'll connect those when we
  // generate constraints for those closures. We also don't need to bother if
  // we're generating constraints for the closure itself, since we'll connect
  // the conjunction to the closure type variable itself.
  if (auto *CE = dyn_cast<ClosureExpr>(DC)) {
    if (isa<ReturnStmt>(stmt) && DCDepth == 0 &&
        !Locator->directlyAt<ClosureExpr>()) {
      SmallPtrSet<TypeVariableType *, 4> typeVars;
      CS.getClosureType(CE)->getResult()->getTypeVariables(typeVars);
      TypeVars.insert(typeVars.begin(), typeVars.end());
    }
  }
  return Action::Continue(stmt);
}

namespace {
  class ConstraintGenerator : public ExprVisitor<ConstraintGenerator, Type> {
    ConstraintSystem &CS;
    DeclContext *CurDC;
    ConstraintSystemPhase CurrPhase;

    /// A map from each UnresolvedMemberExpr to the respective (implicit) base
    /// found during our walk.
    llvm::MapVector<UnresolvedMemberExpr *, Type> UnresolvedBaseTypes;

    /// A stack of pack expansions that can open pack elements.
    llvm::SmallVector<PackExpansionExpr *, 1> OuterExpansions;

    /// Returns false and emits the specified diagnostic if the member reference
    /// base is a nil literal. Returns true otherwise.
    bool isValidBaseOfMemberRef(Expr *base, Diag<> diagnostic) {
      if (auto nilLiteral = dyn_cast<NilLiteralExpr>(base)) {
        CS.getASTContext().Diags.diagnose(nilLiteral->getLoc(), diagnostic);
        return false;
      }
      return true;
    }

    /// Retrieves a matching set of function params for an argument list.
    void getMatchingParams(ArgumentList *argList,
                           SmallVectorImpl<AnyFunctionType::Param> &result) {
      for (auto arg : *argList) {
        ParameterTypeFlags flags;
        auto ty = CS.getType(arg.getExpr());
        if (arg.isInOut()) {
          ty = ty->getInOutObjectType();
          flags = flags.withInOut(true);
        }
        if (arg.isConst()) {
          flags = flags.withCompileTimeConst(true);
        }
        result.emplace_back(ty, arg.getLabel(), flags);
      }
    }

    /// If the provided type is a tuple, decomposes it into a matching set of
    /// function params. Otherwise produces a single parameter of the type.
    void decomposeTuple(Type ty,
                        SmallVectorImpl<AnyFunctionType::Param> &result) {
      switch (ty->getKind()) {
      case TypeKind::Tuple: {
        auto tupleTy = cast<TupleType>(ty.getPointer());
        for (auto &elt : tupleTy->getElements())
          result.emplace_back(elt.getType(), elt.getName());
        return;
      }
      default:
        result.emplace_back(ty, Identifier());
      }
    }

    /// Add constraints for a reference to a named member of the given
    /// base type, and return the type of such a reference.
    Type addMemberRefConstraints(Expr *expr, Expr *base, DeclNameRef name,
                                 FunctionRefInfo functionRefInfo,
                                 ArrayRef<ValueDecl *> outerAlternatives) {
      // The base must have a member of the given name, such that accessing
      // that member through the base returns a value convertible to the type
      // of this expression.
      auto baseTy = CS.getType(base);
      if (isa<ErrorExpr>(base)) {
        return CS.createTypeVariable(
            CS.getConstraintLocator(expr, ConstraintLocator::Member),
            TVO_CanBindToHole);
      }
      unsigned options = (TVO_CanBindToLValue |
                          TVO_CanBindToNoEscape);
      if (!OuterExpansions.empty())
        options |= TVO_CanBindToPack;

      auto tv = CS.createTypeVariable(
                  CS.getConstraintLocator(expr, ConstraintLocator::Member),
                  options);
      SmallVector<OverloadChoice, 4> outerChoices;
      for (auto decl : outerAlternatives) {
        outerChoices.push_back(OverloadChoice(Type(), decl, functionRefInfo));
      }
      CS.addValueMemberConstraint(
          baseTy, name, tv, CurDC, functionRefInfo, outerChoices,
          CS.getConstraintLocator(expr, ConstraintLocator::Member));
      return tv;
    }

    /// Add constraints for a reference to a specific member of the given
    /// base type, and return the type of such a reference.
    Type addMemberRefConstraints(Expr *expr, Expr *base, ValueDecl *decl,
                                 FunctionRefInfo functionRefInfo) {
      // If we're referring to an invalid declaration, fail.
      if (!decl)
        return nullptr;
      
      if (decl->isInvalid())
        return nullptr;

      auto memberLocator =
        CS.getConstraintLocator(expr, ConstraintLocator::Member);
      auto tv = CS.createTypeVariable(memberLocator,
                                      TVO_CanBindToLValue | TVO_CanBindToNoEscape);

      OverloadChoice choice =
          OverloadChoice(CS.getType(base), decl, functionRefInfo);

      auto locator = CS.getConstraintLocator(expr, ConstraintLocator::Member);
      CS.addBindOverloadConstraint(tv, choice, locator, CurDC);
      return tv;
    }

    /// Add constraints for a subscript operation.
    Type addSubscriptConstraints(
        Expr *anchor, Type baseTy, ValueDecl *declOrNull, ArgumentList *argList,
        ConstraintLocator *locator = nullptr,
        SmallVectorImpl<TypeVariableType *> *addedTypeVars = nullptr) {
      // Locators used in this expression.
      if (locator == nullptr)
        locator = CS.getConstraintLocator(anchor);

      auto fnLocator =
        CS.getConstraintLocator(locator,
                                ConstraintLocator::ApplyFunction);
      auto memberLocator =
        CS.getConstraintLocator(locator,
                                ConstraintLocator::SubscriptMember);
      auto resultLocator =
        CS.getConstraintLocator(locator,
                                ConstraintLocator::FunctionResult);

      CS.associateArgumentList(memberLocator, argList);

      Type outputTy;

      // For an integer subscript expression on an array slice type, instead of
      // introducing a new type variable we can easily obtain the element type.
      if (isa<SubscriptExpr>(anchor)) {

        auto isLValueBase = false;
        auto baseObjTy = baseTy;
        if (baseObjTy->is<LValueType>()) {
          isLValueBase = true;
          baseObjTy = baseObjTy->getWithoutSpecifierType();
        }

        if (baseObjTy->isArrayType()) {

          if (auto arraySliceTy = 
                dyn_cast<ArraySliceType>(baseObjTy.getPointer())) {
            baseObjTy = arraySliceTy->getDesugaredType();
          }

          if (argList->isUnlabeledUnary() &&
              isa<IntegerLiteralExpr>(argList->getExpr(0))) {

            outputTy = baseObjTy->getAs<BoundGenericType>()->getGenericArgs()[0];
            
            if (isLValueBase)
              outputTy = LValueType::get(outputTy);
          }
        }
      }
      
      if (outputTy.isNull()) {
        outputTy = CS.createTypeVariable(resultLocator,
                                         TVO_CanBindToLValue | TVO_CanBindToNoEscape);
        if (addedTypeVars)
          addedTypeVars->push_back(outputTy->castTo<TypeVariableType>());
      }

      // FIXME: This can only happen when diagnostics successfully type-checked
      // sub-expression of the subscript and mutated AST, but under normal
      // circumstances subscript should never have InOutExpr as a direct child
      // until type checking is complete and expression is re-written.
      // Proper fix for such situation requires preventing diagnostics from
      // re-writing AST after successful type checking of the sub-expressions.
      if (auto inoutTy = baseTy->getAs<InOutType>()) {
        baseTy = LValueType::get(inoutTy->getObjectType());
      }

      // Add the member constraint for a subscript declaration.
      // FIXME: weak name!
      auto memberTy = CS.createTypeVariable(
          memberLocator, TVO_CanBindToLValue | TVO_CanBindToNoEscape);
      if (addedTypeVars)
        addedTypeVars->push_back(memberTy);

      // FIXME: synthesizeMaterializeForSet() wants to statically dispatch to
      // a known subscript here. This might be cleaner if we split off a new
      // UnresolvedSubscriptExpr from SubscriptExpr.
      if (auto decl = declOrNull) {
        OverloadChoice choice = OverloadChoice(
            baseTy, decl, FunctionRefInfo::doubleBaseNameApply());
        CS.addBindOverloadConstraint(memberTy, choice, memberLocator,
                                     CurDC);
      } else {
        CS.addValueMemberConstraint(baseTy, DeclNameRef::createSubscript(),
                                    memberTy, CurDC,
                                    FunctionRefInfo::doubleBaseNameApply(),
                                    /*outerAlternatives=*/{}, memberLocator);
      }

      SmallVector<AnyFunctionType::Param, 8> params;
      getMatchingParams(argList, params);

      // Add the constraint that the index expression's type be convertible
      // to the input type of the subscript operator.
      CS.addConstraint(ConstraintKind::ApplicableFunction,
                       FunctionType::get(params, outputTy),
                       memberTy,
                       fnLocator);

      Type fixedOutputType =
          CS.getFixedTypeRecursive(outputTy, /*wantRValue=*/false);
      if (!fixedOutputType->isTypeVariableOrMember()) {
        outputTy = fixedOutputType;
      }

      return outputTy;
    }

    Type openPackElement(Type packType, ConstraintLocator *locator,
                         PackExpansionExpr *packElementEnvironment) {
      if (!packElementEnvironment) {
        return CS.createTypeVariable(locator,
                                     TVO_CanBindToHole | TVO_CanBindToNoEscape);
      }

      // The type of a PackElementExpr is the opened pack element archetype
      // of the pack reference.
      OpenPackElementType openPackElement(CS, locator, packElementEnvironment);
      return openPackElement(packType, /*packRepr*/ nullptr);
    }

  public:
    ConstraintGenerator(ConstraintSystem &CS, DeclContext *DC)
        : CS(CS), CurDC(DC ? DC : CS.DC), CurrPhase(CS.getPhase()) {
      // Although constraint system is initialized in `constraint
      // generation` phase, we have to set it here manually because e.g.
      // result builders could generate constraints for its body
      // in the middle of the solving.
      CS.setPhase(ConstraintSystemPhase::ConstraintGeneration);

      // Pick up the saved stack of pack expansions so we can continue
      // to handle pack element references inside the closure body.
      if (auto *ACE = dyn_cast<AbstractClosureExpr>(CurDC)) {
        OuterExpansions = CS.getCapturedExpansions(ACE);
      }
    }

    virtual ~ConstraintGenerator() {
      CS.setPhase(CurrPhase);
    }

    ConstraintSystem &getConstraintSystem() const { return CS; }

    void pushPackExpansionExpr(PackExpansionExpr *expr) {
      OuterExpansions.push_back(expr);

      SmallVector<ASTNode, 2> expandedPacks;
      collectExpandedPacks(expr, expandedPacks);
      for (auto pack : expandedPacks) {
        if (auto *elementExpr = getAsExpr<PackElementExpr>(pack)) {
          CS.addPackEnvironment(elementExpr, expr);
        }
      }

      auto *patternLoc = CS.getConstraintLocator(
          expr, ConstraintLocator::PackExpansionPattern);
      auto patternType = CS.createTypeVariable(
          patternLoc,
          TVO_CanBindToPack | TVO_CanBindToNoEscape | TVO_CanBindToHole);
      auto *shapeLoc =
          CS.getConstraintLocator(expr, ConstraintLocator::PackShape);
      auto *shapeTypeVar = CS.createTypeVariable(
          shapeLoc, TVO_CanBindToPack | TVO_CanBindToHole);

      auto expansionType = PackExpansionType::get(patternType, shapeTypeVar);
      CS.setType(expr, expansionType);
    }

    /// Records a fix for an invalid AST node, and returns a potential hole
    /// type variable for it.
    Type recordInvalidNode(ASTNode node) {
      CS.recordFix(
          IgnoreInvalidASTNode::create(CS, CS.getConstraintLocator(node)));

      return CS.createTypeVariable(CS.getConstraintLocator(node),
                                   TVO_CanBindToHole);
    }

    virtual Type visitErrorExpr(ErrorExpr *E) {
      return recordInvalidNode(E);
    }

    virtual Type visitCodeCompletionExpr(CodeCompletionExpr *E) {
      CS.Options |= ConstraintSystemFlags::SuppressDiagnostics;
      auto locator = CS.getConstraintLocator(E);
      return CS.createTypeVariable(locator, TVO_CanBindToLValue |
                                                TVO_CanBindToNoEscape |
                                                TVO_CanBindToHole);
    }

    Type visitNilLiteralExpr(NilLiteralExpr *expr) {
      auto literalTy = visitLiteralExpr(expr);
      // Allow `nil` to be a hole so we can diagnose it via a fix
      // if it turns out that there is no contextual information.
      if (auto *typeVar = literalTy->getAs<TypeVariableType>())
        CS.recordPotentialHole(typeVar);

      return literalTy;
    }

    Type visitFloatLiteralExpr(FloatLiteralExpr *expr) {
      auto &ctx = CS.getASTContext();
      // Get the _MaxBuiltinFloatType decl, or look for it if it's not cached.
      auto maxFloatTypeDecl = ctx.get_MaxBuiltinFloatTypeDecl();

      if (!maxFloatTypeDecl ||
          !maxFloatTypeDecl->getDeclaredInterfaceType()->is<BuiltinFloatType>()) {
        ctx.Diags.diagnose(expr->getLoc(), diag::no_MaxBuiltinFloatType_found);
        return nullptr;
      }

      return visitLiteralExpr(expr);
    }

    Type visitLiteralExpr(LiteralExpr *expr) {
      // If the expression has already been assigned a type; just use that type.
      if (expr->getType())
        return expr->getType();

      auto protocol = TypeChecker::getLiteralProtocol(CS.getASTContext(), expr);
      if (!protocol)
        return nullptr;

      auto tv = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                      TVO_PrefersSubtypeBinding |
                                      TVO_CanBindToNoEscape);
      CS.addConstraint(ConstraintKind::LiteralConformsTo, tv,
                       protocol->getDeclaredInterfaceType(),
                       CS.getConstraintLocator(expr));
      return tv;
    }

    Type
    visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *expr) {
      // Dig out the ExpressibleByStringInterpolation protocol.
      auto &ctx = CS.getASTContext();
      auto interpolationProto = TypeChecker::getProtocol(
          ctx, expr->getLoc(),
          KnownProtocolKind::ExpressibleByStringInterpolation);
      if (!interpolationProto) {
        ctx.Diags.diagnose(expr->getStartLoc(),
                           diag::interpolation_missing_proto);
        return nullptr;
      }

      // The type of the expression must conform to the
      // ExpressibleByStringInterpolation protocol.
      auto locator = CS.getConstraintLocator(expr);
      auto tv = CS.createTypeVariable(locator,
                                      TVO_PrefersSubtypeBinding |
                                      TVO_CanBindToNoEscape);
      CS.addConstraint(ConstraintKind::LiteralConformsTo, tv,
                       interpolationProto->getDeclaredInterfaceType(),
                       locator);

      if (auto appendingExpr = expr->getAppendingExpr()) {
        auto associatedTypeDecl = interpolationProto->getAssociatedType(
          ctx.Id_StringInterpolation);
        if (associatedTypeDecl == nullptr) {
          ctx.Diags.diagnose(expr->getStartLoc(),
                             diag::interpolation_broken_proto);
          return nullptr;
        }

        auto interpolationTV =
            CS.createTypeVariable(locator, TVO_CanBindToNoEscape);
        auto interpolationType =
            DependentMemberType::get(tv, associatedTypeDecl);

        CS.addConstraint(ConstraintKind::Equal, interpolationTV,
                         interpolationType, locator);

        auto appendingExprType = CS.getType(appendingExpr);
        auto appendingLocator = CS.getConstraintLocator(appendingExpr);

        SmallVector<TypeVariableType *, 2> referencedVars;

        if (auto *tap = getAsExpr<TapExpr>(appendingExpr)) {
          // Collect all of the variable references that appear
          // in the tap body, otherwise tap expression is going
          // to get disconnected from the context.
          if (auto *body = tap->getBody()) {
            TypeVarRefCollector refCollector(
                CS, tap->getVar()->getDeclContext(), locator);

            body->walk(refCollector);

            auto vars = refCollector.getTypeVars();
            referencedVars.append(vars.begin(), vars.end());
          }
        }

        // Must be Conversion; if it's Equal, then in semi-rare cases, the
        // interpolation temporary variable cannot be @lvalue.
        CS.addUnsolvedConstraint(Constraint::create(
            CS, ConstraintKind::Conversion, appendingExprType, interpolationTV,
            appendingLocator, referencedVars));
      }

      return tv;
    }

    Type visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *expr) {
#ifdef SWIFT_BUILD_SWIFT_SYNTAX
      auto &ctx = CS.getASTContext();
      if (ctx.LangOpts.hasFeature(Feature::BuiltinMacros)) {
        auto kind = MagicIdentifierLiteralExpr::getKindString(expr->getKind())
                        .drop_front();

        auto protocol =
            TypeChecker::getLiteralProtocol(CS.getASTContext(), expr);
        if (!protocol)
          return Type();

        auto macroIdent = ctx.getIdentifier(kind);
        auto macros = lookupMacros(Identifier(), macroIdent,
                                   FunctionRefInfo::unappliedBaseName(),
                                   MacroRole::Expression);
        if (!macros.empty()) {
          // Introduce an overload set for the macro reference.
          auto locator = CS.getConstraintLocator(expr);
          auto macroRefType = Type(CS.createTypeVariable(locator, 0));
          CS.addOverloadSet(macroRefType, macros, CurDC, locator);

          // FIXME: Can this be encoded in the macro definition somehow?
          CS.addConstraint(ConstraintKind::LiteralConformsTo, macroRefType,
                           protocol->getDeclaredInterfaceType(),
                           CS.getConstraintLocator(expr));

          return macroRefType;
        }
      }

      // Fall through to use old implementation.
#endif

      switch (expr->getKind()) {
      // Magic pointer identifiers are of type UnsafeMutableRawPointer.
#define MAGIC_POINTER_IDENTIFIER(NAME, STRING, SYNTAX_KIND) \
      case MagicIdentifierLiteralExpr::NAME:
#include "swift/AST/MagicIdentifierKinds.def"
      {
        auto &ctx = CS.getASTContext();
        if (TypeChecker::requirePointerArgumentIntrinsics(ctx, expr->getLoc()))
          return nullptr;

        return ctx.getUnsafeRawPointerType();
      }

      default:
        // Others are actual literals and should be handled like any literal.
        return visitLiteralExpr(expr);
      }

      llvm_unreachable("Unhandled MagicIdentifierLiteralExpr in switch.");
    }

    Type visitObjectLiteralExpr(ObjectLiteralExpr *expr) {
      auto *exprLoc = CS.getConstraintLocator(expr);
      CS.associateArgumentList(exprLoc, expr->getArgs());

      // If the expression has already been assigned a type; just use that type.
      if (expr->getType())
        return expr->getType();

      auto &ctx = CS.getASTContext();
      auto &de = ctx.Diags;
      auto protocol = TypeChecker::getLiteralProtocol(ctx, expr);
      if (!protocol) {
        de.diagnose(expr->getLoc(), diag::use_unknown_object_literal_protocol,
                    expr->getLiteralKindPlainName());
        return nullptr;
      }

      auto witnessType = CS.createTypeVariable(
          exprLoc, TVO_PrefersSubtypeBinding | TVO_CanBindToNoEscape |
                       TVO_CanBindToHole);

      CS.addConstraint(ConstraintKind::LiteralConformsTo, witnessType,
                       protocol->getDeclaredInterfaceType(), exprLoc);

      // The arguments are required to be argument-convertible to the
      // idealized parameter type of the initializer, which generally
      // simplifies the first label (e.g. "colorLiteralRed:") by stripping
      // all the redundant stuff about literals (leaving e.g. "red:").
      // Constraint application will quietly rewrite the type of 'args' to
      // use the right labels before forming the call to the initializer.
      auto constrName = TypeChecker::getObjectLiteralConstructorName(ctx, expr);
      assert(constrName);
      auto *constr = dyn_cast_or_null<ConstructorDecl>(
          protocol->getSingleRequirement(constrName));
      if (!constr) {
        de.diagnose(protocol, diag::object_literal_broken_proto);
        return nullptr;
      }

      auto *memberLoc =
          CS.getConstraintLocator(expr, ConstraintLocator::ConstructorMember);

      auto *fnLoc =
          CS.getConstraintLocator(expr, ConstraintLocator::ApplyFunction);

      auto *memberTypeLoc = CS.getConstraintLocator(
          fnLoc, LocatorPathElt::ConstructorMemberType());

      auto *memberType =
          CS.createTypeVariable(memberTypeLoc, TVO_CanBindToNoEscape);

      CS.addValueMemberConstraint(MetatypeType::get(witnessType, ctx),
                                  DeclNameRef(constrName), memberType, CurDC,
                                  FunctionRefInfo::doubleBaseNameApply(), {},
                                  memberLoc);

      SmallVector<AnyFunctionType::Param, 8> params;
      getMatchingParams(expr->getArgs(), params);

      auto resultType = CS.createTypeVariable(
          CS.getConstraintLocator(expr, ConstraintLocator::FunctionResult),
          TVO_CanBindToNoEscape);

      CS.addConstraint(ConstraintKind::ApplicableFunction,
                       FunctionType::get(params, resultType), memberType,
                       fnLoc);

      if (constr->isFailable())
        return OptionalType::get(witnessType);

      return witnessType;
    }

    Type visitRegexLiteralExpr(RegexLiteralExpr *E) {
      // Retrieve the computed Regex type from the compiler regex library.
      auto ty = E->getRegexType();
      if (!ty)
        return recordInvalidNode(E);

      return ty;
    }

    PackExpansionExpr *getParentPackExpansionExpr(Expr *E) const {
      auto *current = E;
      while (auto *parent = CS.getParentExpr(current)) {
        if (auto *expansion = dyn_cast<PackExpansionExpr>(parent)) {
          return expansion;
        }
        current = parent;
      }
      return nullptr;
    }

    Type visitDeclRefExpr(DeclRefExpr *E) {
      auto locator = CS.getConstraintLocator(E);

      auto invalidateReference = [&]() -> Type {
        auto *hole = CS.createTypeVariable(locator, TVO_CanBindToHole);
        (void)CS.recordFix(AllowRefToInvalidDecl::create(CS, locator));
        CS.setType(E, hole);
        return hole;
      };

      Type knownType;
      if (auto *VD = dyn_cast<VarDecl>(E->getDecl())) {
        knownType = CS.getTypeIfAvailable(VD);
        if (!knownType)
          knownType = VD->getTypeInContext();

        if (knownType) {
          // An out-of-scope type variable(s) could appear the type of
          // a declaration only in diagnostic mode when invalid variable
          // declaration is recursively referenced inside of a multi-statement
          // closure located somewhere within its initializer e.g.:
          // `let x = [<call>] { ... print(x) }`. It happens because the
          // variable assumes the result type of its initializer unless
          // its specified explicitly.
          if (isa<ClosureExpr>(CurDC) && knownType->hasTypeVariable()) {
            if (knownType.findIf([&](Type type) {
                  auto *typeVar = type->getAs<TypeVariableType>();
                  if (!typeVar || CS.getFixedType(typeVar))
                    return false;

                  return !CS.isActiveTypeVariable(typeVar);
                }))
              return invalidateReference();
          }

          // If the known type has an error, bail out.
          if (knownType->hasError()) {
            return invalidateReference();
          }

          // value packs cannot be referenced without `each` immediately
          // preceding them.
          if (auto *expansionType = knownType->getAs<PackExpansionType>()) {
            if (auto *parentExpansionExpr = getParentPackExpansionExpr(E);
                parentExpansionExpr &&
                !isExpr<PackElementExpr>(CS.getParentExpr(E))) {
              auto packType = expansionType->getPatternType();
              (void)CS.recordFix(
                  IgnoreMissingEachKeyword::create(CS, packType, locator));
              auto eltType =
                  openPackElement(packType, locator, parentExpansionExpr);
              CS.setType(E, eltType);
              return eltType;
            }
          }
        }
      }

      // If declaration is invalid, let's turn it into a potential hole
      // and keep generating constraints.
      // For code completion, we still resolve the overload and replace error
      // types inside the function decl with placeholders
      // (in getTypeOfReference) so we can match non-error param types.
      if (!knownType && E->getDecl()->isInvalid() &&
          !CS.isForCodeCompletion()) {
        return invalidateReference();
      }

      unsigned options = (TVO_CanBindToLValue |
                          TVO_CanBindToNoEscape);
      if (!OuterExpansions.empty())
        options |= TVO_CanBindToPack;

      // Create an overload choice referencing this declaration and immediately
      // resolve it. This records the overload for use later.
      auto tv = CS.createTypeVariable(locator, options);

      OverloadChoice choice =
          OverloadChoice(Type(), E->getDecl(), E->getFunctionRefInfo());
      CS.resolveOverload(locator, tv, choice, CurDC);
      return tv;
    }

    Type visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E) {
      return E->getType();
    }

    Type visitSuperRefExpr(SuperRefExpr *E) {
      if (E->getType())
        return E->getType();

      // Resolve the super type of 'self'.
      auto *selfDecl = E->getSelf();

      DeclContext *typeContext = selfDecl->getDeclContext()->getParent();
      assert(typeContext);

      auto selfTy =
          CS.DC->mapTypeIntoContext(typeContext->getDeclaredInterfaceType());
      auto superclassTy = selfTy->getSuperclass();

      if (!superclassTy)
        return Type();

      if (selfDecl->getInterfaceType()->is<MetatypeType>())
        superclassTy = MetatypeType::get(superclassTy);

      return superclassTy;
    }

    Type
    resolveTypeReferenceInExpression(TypeRepr *repr,
                                     TypeResolutionOptions options,
                                     const ConstraintLocatorBuilder &locator) {
      // Introduce type variables for unbound generics.
      const auto genericOpener = OpenUnboundGenericType(CS, locator);
      const auto placeholderHandler = HandlePlaceholderType(CS, locator);

      // Add a PackElementOf constraint for 'each T' type reprs.
      PackExpansionExpr *elementEnv = nullptr;
      if (!OuterExpansions.empty()) {
        options |= TypeResolutionFlags::AllowPackReferences;
        elementEnv = OuterExpansions.back();
      }
      const auto packElementOpener = OpenPackElementType(CS, locator, elementEnv);

      const auto result = TypeResolution::resolveContextualType(
          repr, CS.DC, options, genericOpener, placeholderHandler,
          packElementOpener);
      if (result->hasError()) {
        CS.recordFix(
            IgnoreInvalidASTNode::create(CS, CS.getConstraintLocator(locator)));

        return CS.createTypeVariable(CS.getConstraintLocator(repr),
                                     TVO_CanBindToHole);
      }
      // Diagnose top-level usages of placeholder types.
      if (auto *ty = dyn_cast<PlaceholderTypeRepr>(repr->getWithoutParens())) {
        auto *loc = CS.getConstraintLocator(locator, {LocatorPathElt::PlaceholderType(ty)});
        CS.recordFix(IgnoreInvalidPlaceholder::create(CS, loc));
      }
      return result;
    }

    Type visitTypeExpr(TypeExpr *E) {
      Type type;
      // If this is an implicit TypeExpr, don't validate its contents.
      auto *const locator = CS.getConstraintLocator(E);
      if (E->isImplicit()) {
        type = CS.getInstanceType(CS.cacheType(E));
        assert(type && "Implicit type expr must have type set!");
        type = CS.replaceInferableTypesWithTypeVars(type, locator);
      } else {
        auto *repr = E->getTypeRepr();
        assert(repr && "Explicit node has no type repr!");
        type = resolveTypeReferenceInExpression(
            repr, TypeResolverContext::InExpression, locator);
      }

      if (!type || type->hasError()) return Type();

      return MetatypeType::get(type);
    }

    Type visitTypeValueExpr(TypeValueExpr *E) {
      auto locator = CS.getConstraintLocator(E);
      auto type = resolveTypeReferenceInExpression(E->getParamTypeRepr(),
                                              TypeResolverContext::InExpression,
                                                  locator);

      if (!type || type->hasError()) {
        return Type();
      }

      auto archetype = type->castTo<ArchetypeType>();
      E->setParamType(archetype);
      return archetype->getValueType();
    }

    Type visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *expr) {
      // For a reference to an overloaded declaration, we create a type variable
      // that will be equal to different types depending on which overload
      // is selected.
      auto locator = CS.getConstraintLocator(expr);
      auto tv = CS.createTypeVariable(locator,
                                      TVO_CanBindToLValue | TVO_CanBindToNoEscape);
      ArrayRef<ValueDecl*> decls = expr->getDecls();
      SmallVector<OverloadChoice, 4> choices;

      for (unsigned i = 0, n = decls.size(); i != n; ++i) {
        // If the result is invalid, skip it.
        // FIXME: Note this as invalid, in case we don't find a solution,
        // so we don't let errors cascade further.
        if (decls[i]->isInvalid())
          continue;

        OverloadChoice choice =
            OverloadChoice(Type(), decls[i], expr->getFunctionRefInfo());
        choices.push_back(choice);
      }

      if (choices.empty()) {
        // There are no suitable overloads. Just fail.
        return nullptr;
      }

      // Record this overload set.
      CS.addOverloadSet(tv, choices, CurDC, locator);
      return tv;
    }

    Type visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *expr) {
      // This is an error case, where we're trying to use type inference
      // to help us determine which declaration the user meant to refer to.
      // FIXME: Do we need to note that we're doing some kind of recovery?
      return CS.createTypeVariable(CS.getConstraintLocator(expr),
                                   TVO_CanBindToLValue |
                                   TVO_CanBindToNoEscape);
    }
    
    Type visitMemberRefExpr(MemberRefExpr *expr) {
      return addMemberRefConstraints(
          expr, expr->getBase(), expr->getMember().getDecl(),
          /*FIXME:*/ FunctionRefInfo::doubleBaseNameApply());
    }
    
    Type visitDynamicMemberRefExpr(DynamicMemberRefExpr *expr) {
      llvm_unreachable("Already typechecked");
    }

    void setUnresolvedBaseType(UnresolvedMemberExpr *UME, Type ty) {
      UnresolvedBaseTypes.insert({UME, ty});
    }

    Type getUnresolvedBaseType(UnresolvedMemberExpr *UME) {
      auto result = UnresolvedBaseTypes.find(UME);
      assert(result != UnresolvedBaseTypes.end());
      return result->second;
    }
    
    virtual Type visitUnresolvedMemberExpr(UnresolvedMemberExpr *expr) {
      auto baseLocator = CS.getConstraintLocator(
                            expr,
                            ConstraintLocator::MemberRefBase);
      auto memberLocator
        = CS.getConstraintLocator(expr, ConstraintLocator::UnresolvedMember);

      // Since base type in this case is completely dependent on context it
      // should be marked as a potential hole.
      auto baseTy = CS.createTypeVariable(baseLocator, TVO_CanBindToNoEscape |
                                                           TVO_CanBindToHole);
      setUnresolvedBaseType(expr, baseTy);

      auto memberTy = CS.createTypeVariable(
          memberLocator, TVO_CanBindToLValue | TVO_CanBindToNoEscape);

      // An unresolved member expression '.member' is modeled as a value member
      // constraint
      //
      //   T0.Type[.member] == T1
      //
      // for fresh type variables T0 and T1, which pulls out a static
      // member, i.e., an enum case or a static variable.
      auto baseMetaTy = MetatypeType::get(baseTy);
      CS.addUnresolvedValueMemberConstraint(baseMetaTy, expr->getName(),
                                            memberTy, CurDC,
                                            expr->getFunctionRefInfo(),
                                            memberLocator);
      return memberTy;
    }

    Type visitUnresolvedMemberChainResultExpr(
        UnresolvedMemberChainResultExpr *expr) {
      auto *tail = expr->getSubExpr();
      auto memberTy = CS.getType(tail);
      auto *base = expr->getChainBase();
      assert(base == TypeChecker::getUnresolvedMemberChainBase(tail));

      // The result type of the chain is represented by a new type variable.
      auto locator = CS.getConstraintLocator(
          expr, ConstraintLocator::UnresolvedMemberChainResult);
      auto chainResultTy = CS.createTypeVariable(
          locator,
          TVO_CanBindToLValue | TVO_CanBindToHole | TVO_CanBindToNoEscape);
      auto chainBaseTy = getUnresolvedBaseType(base);

      // The result of the last element of the chain must be convertible to the
      // whole chain, and the type of the whole chain must be equal to the base.
      CS.addConstraint(ConstraintKind::Conversion, memberTy, chainResultTy,
                       locator);
      CS.addConstraint(ConstraintKind::UnresolvedMemberChainBase, chainResultTy,
                       chainBaseTy, locator);

      return chainResultTy;
    }

    Type visitUnresolvedDotExpr(UnresolvedDotExpr *expr) {
      // UnresolvedDot applies the base to remove a single curry level from a
      // member reference without using an applicable function constraint so
      // we record the call argument matching here so it can be found later when
      // a solution is applied to the AST.
      CS.recordMatchCallArgumentResult(
          CS.getConstraintLocator(expr, ConstraintLocator::ApplyArgument),
          MatchCallArgumentResult::forArity(1));

      // If this is Builtin.type_join*, just return any type and move
      // on since we're going to discard this, and creating any type
      // variables for the reference will cause problems.
      auto &ctx = CS.getASTContext();
      auto typeOperation = getTypeOperation(expr, ctx);
      if (typeOperation != TypeOperation::None)
        return ctx.getAnyExistentialType();

      // If this is `Builtin.trigger_fallback_diagnostic()`, fail
      // without producing any diagnostics, in order to test fallback error.
      if (isTriggerFallbackDiagnosticBuiltin(expr, ctx))
        return Type();

      // Open a member constraint for constructor delegations on the
      // subexpr type.
      if (TypeChecker::getSelfForInitDelegationInConstructor(CS.DC, expr)) {
        auto baseTy = CS.getType(expr->getBase())
                        ->getWithoutSpecifierType();

        // 'self' or 'super' will reference an instance, but the constructor
        // is semantically a member of the metatype. This:
        //   self.init()
        //   super.init()
        // is really more like:
        //   self = Self.init()
        //   self.super = Super.init()
        baseTy = MetatypeType::get(baseTy, ctx);

        auto memberTypeLoc = CS.getConstraintLocator(
            expr, LocatorPathElt::ConstructorMemberType(
                      /*shortFormOrSelfDelegating*/ true));

        auto methodTy =
            CS.createTypeVariable(memberTypeLoc, TVO_CanBindToNoEscape);

        // HACK: Bind the function's parameter list as a tuple to a type
        // variable. This only exists to preserve compatibility with
        // rdar://85263844, as it can affect the prioritization of bindings,
        // which can affect behavior for tuple matching as tuple subtyping is
        // currently a *weaker* constraint than tuple conversion.
        if (!CS.getASTContext().isSwiftVersionAtLeast(6)) {
          auto paramTypeVar = CS.createTypeVariable(
              CS.getConstraintLocator(expr, ConstraintLocator::ApplyArgument),
              TVO_CanBindToLValue | TVO_CanBindToInOut | TVO_CanBindToNoEscape |
                  TVO_CanBindToPack);
          CS.addConstraint(ConstraintKind::BindTupleOfFunctionParams, methodTy,
                           paramTypeVar, CS.getConstraintLocator(expr));
        }

        CS.addValueMemberConstraint(
            baseTy, expr->getName(), methodTy, CurDC,
            expr->getFunctionRefInfo(),
            /*outerAlternatives=*/{},
            CS.getConstraintLocator(expr,
                                    ConstraintLocator::ConstructorMember));

        // The result of the expression is the partial application of the
        // constructor to the subexpression.
        return methodTy;
      }

      return addMemberRefConstraints(expr, expr->getBase(), expr->getName(),
                                     expr->getFunctionRefInfo(),
                                     expr->getOuterAlternatives());
    }

    /// Given a set of specialization arguments, resolve those arguments and
    /// introduce them as an explicit generic arguments constraint.
    ///
    /// \returns true if resolving any of the specialization types failed.
    void addSpecializationConstraint(ConstraintLocator *locator, Type boundType,
                                     SourceLoc lAngleLoc,
                                     ArrayRef<TypeRepr *> specializationArgs) {
      // Resolve each type.
      SmallVector<Type, 2> specializationArgTypes;
      auto options =
          TypeResolutionOptions(TypeResolverContext::InExpression);
      for (auto specializationArg : specializationArgs) {
        PackExpansionExpr *elementEnv = nullptr;
        if (!OuterExpansions.empty()) {
          options |= TypeResolutionFlags::AllowPackReferences;
          elementEnv = OuterExpansions.back();
        }
        auto result = TypeResolution::resolveContextualType(
            specializationArg, CurDC, options,
            // Introduce type variables for unbound generics.
            OpenUnboundGenericType(CS, locator),
            HandlePlaceholderType(CS, locator),
            OpenPackElementType(CS, locator, elementEnv));
        if (result->hasError()) {
          auto &ctxt = CS.getASTContext();
          result = PlaceholderType::get(ctxt, specializationArg);
          ctxt.Diags.diagnose(lAngleLoc,
                              diag::while_parsing_as_left_angle_bracket);
        }
        specializationArgTypes.push_back(result);
      }

      auto constraint = Constraint::create(
          CS, ConstraintKind::ExplicitGenericArguments, boundType,
          PackType::get(CS.getASTContext(), specializationArgTypes), locator);
      CS.addUnsolvedConstraint(constraint);
      CS.activateConstraint(constraint);
    }

    Type visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *expr) {
      auto baseTy = CS.getType(expr->getSubExpr());
      auto *overloadLocator = CS.getConstraintLocator(expr->getSubExpr());
      addSpecializationConstraint(
          overloadLocator, baseTy->getMetatypeInstanceType(),
          expr->getLAngleLoc(), expr->getUnresolvedParams());
      return baseTy;
    }
    
    Type visitSequenceExpr(SequenceExpr *expr) {
      // If a SequenceExpr survived until CSGen, then there was an upstream
      // error that was already reported.
      return Type();
    }

    Type visitArrowExpr(ArrowExpr *expr) {
      // If an ArrowExpr survived until CSGen, then there was an upstream
      // error that was already reported.
      return Type();
    }

    Type visitIdentityExpr(IdentityExpr *expr) {
      return CS.getType(expr->getSubExpr());
    }

    Type visitCopyExpr(CopyExpr *expr) {
      auto valueTy = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                           TVO_PrefersSubtypeBinding |
                                               TVO_CanBindToNoEscape);
      CS.addConstraint(ConstraintKind::Equal, valueTy,
                       CS.getType(expr->getSubExpr()),
                       CS.getConstraintLocator(expr));
      return valueTy;
    }

    Type visitConsumeExpr(ConsumeExpr *expr) {
      auto valueTy = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                           TVO_PrefersSubtypeBinding |
                                               TVO_CanBindToNoEscape);
      CS.addConstraint(ConstraintKind::Equal, valueTy,
                       CS.getType(expr->getSubExpr()),
                       CS.getConstraintLocator(expr));
      return valueTy;
    }

    Type visitAnyTryExpr(AnyTryExpr *expr) {
      return CS.getType(expr->getSubExpr());
    }

    Type visitOptionalTryExpr(OptionalTryExpr *expr) {
      auto valueTy = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                           TVO_PrefersSubtypeBinding |
                                           TVO_CanBindToNoEscape);

      Type optTy = getOptionalType(expr->getSubExpr()->getLoc(), valueTy);
      if (!optTy)
        return Type();

      bool isDelegationToOptionalInit = false;
      {
        Expr *e = expr->getSubExpr();
        while (true) {
          e = e->getSemanticsProvidingExpr();

          // Look through force-value expressions.
          if (auto *FVE = dyn_cast<ForceValueExpr>(e)) {
            e = FVE->getSubExpr();
            continue;
          }

          break;
        }

        if (auto *CE = dyn_cast<CallExpr>(e)) {
          if (auto *UDE = dyn_cast<UnresolvedDotExpr>(CE->getFn())) {
            if (!CS.getType(UDE->getBase())->is<AnyMetatypeType>()) {
              auto overload =
                  CS.findSelectedOverloadFor(CS.getConstraintLocator(
                      UDE, ConstraintLocator::ConstructorMember));
              if (overload) {
                auto *decl = overload->choice.getDeclOrNull();
                if (decl && isa<ConstructorDecl>(decl) &&
                    decl->getDeclContext()
                        ->getSelfNominalTypeDecl()
                        ->isOptionalDecl())
                  isDelegationToOptionalInit = true;
              }
            }
          }
        }
      }

      // Prior to Swift 5, 'try?' always adds an additional layer of
      // optionality, even if the sub-expression was already optional.
      //
      // NB Keep adding the additional layer in Swift 5 and on if this 'try?'
      // applies to a delegation to an 'Optional' initializer, or else we won't
      // discern the difference between a failure and a constructed value.
      if (CS.getASTContext().LangOpts.isSwiftVersionAtLeast(5) &&
          !isDelegationToOptionalInit) {
        CS.addConstraint(ConstraintKind::Conversion,
                         CS.getType(expr->getSubExpr()), optTy,
                         CS.getConstraintLocator(expr));
      } else {
        CS.addConstraint(ConstraintKind::OptionalObject,
                         optTy, CS.getType(expr->getSubExpr()),
                         CS.getConstraintLocator(expr));
      }
      return optTy;
    }

    virtual Type visitParenExpr(ParenExpr *expr) {
      // If the ParenExpr contains a pack expansion, generate a tuple
      // type containing the pack expansion type.
      if (CS.getType(expr->getSubExpr())->getAs<PackExpansionType>()) {
        return TupleType::get({CS.getType(expr->getSubExpr())},
                              CS.getASTContext());
      }

      return CS.getType(expr->getSubExpr());
    }

    Type visitTupleExpr(TupleExpr *expr) {
      // The type of a tuple expression is simply a tuple of the types of
      // its subexpressions.
      SmallVector<TupleTypeElt, 4> elements;
      elements.reserve(expr->getNumElements());
      for (unsigned i = 0, n = expr->getNumElements(); i != n; ++i) {
        elements.emplace_back(CS.getType(expr->getElement(i)),
                              expr->getElementName(i));
      }

      return TupleType::get(elements, CS.getASTContext());
    }

    Type visitSubscriptExpr(SubscriptExpr *expr) {
      ValueDecl *decl = nullptr;
      if (expr->hasDecl()) {
        decl = expr->getDecl().getDecl();
        if (decl->isInvalid())
          return Type();
      }

      auto *base = expr->getBase();
      if (!isValidBaseOfMemberRef(base, diag::cannot_subscript_nil_literal))
        return nullptr;

      return addSubscriptConstraints(expr, CS.getType(base), decl,
                                     expr->getArgs());
    }
    
    Type visitArrayExpr(ArrayExpr *expr) {
      auto &ctx = CS.getASTContext();

      // An array expression can be of a type T that conforms to the
      // ExpressibleByArrayLiteral protocol.
      auto *arrayProto = TypeChecker::getProtocol(
          ctx, expr->getLoc(),
          KnownProtocolKind::ExpressibleByArrayLiteral);
      if (!arrayProto) {
        return Type();
      }

      // Assume that ExpressibleByArrayLiteral contains a single associated type.
      auto *elementAssocTy = arrayProto->getAssociatedTypeMembers()[0];
      if (!elementAssocTy)
        return Type();

      auto locator = CS.getConstraintLocator(expr);
      auto contextualType = CS.getContextualType(expr, /*forConstraint=*/false);
      auto contextualPurpose = CS.getContextualTypePurpose(expr);

      auto joinElementTypes = [&](std::optional<Type> elementType) {
        const auto elements = expr->getElements();
        unsigned index = 0;

        using Iterator = decltype(elements)::iterator;
        CS.addJoinConstraint<Iterator>(
            locator, elements.begin(), elements.end(), elementType,
            [&](const auto it) {
              auto *locator = CS.getConstraintLocator(
                  expr, LocatorPathElt::TupleElement(index++));
              return std::make_pair(CS.getType(*it), locator);
            });
      };

      // If a contextual type exists for this expression, apply it directly.
      if (contextualType && contextualType->isArrayType()) {
        // Now that we know we're actually going to use the type, get the
        // version for use in a constraint.
        contextualType = CS.getContextualType(expr, /*forConstraint=*/true);
        // FIXME: This is the wrong place to be opening the opaque type.
        contextualType = CS.openOpaqueType(
            contextualType, contextualPurpose, locator, /*ownerDecl=*/nullptr);
        Type arrayElementType = contextualType->isArrayType();
        CS.addConstraint(ConstraintKind::LiteralConformsTo, contextualType,
                         arrayProto->getDeclaredInterfaceType(),
                         locator);
        joinElementTypes(arrayElementType);
        return contextualType;
      }

      // Produce a specialized diagnostic if this is an attempt to initialize
      // or convert an array literal to a dictionary e.g.
      // `let _: [String: Int] = ["A", 0]`
      auto isDictionaryContextualType = [&](Type contextualType) -> bool {
        if (!contextualType)
          return false;

        auto type = contextualType->lookThroughAllOptionalTypes();

        if (lookupConformance(type, arrayProto))
          return false;

        if (auto *proto = ctx.getProtocol(KnownProtocolKind::ExpressibleByDictionaryLiteral))
          if (lookupConformance(type, proto))
            return true;

        return false;
      };

      if (isDictionaryContextualType(contextualType)) {
        auto &DE = CS.getASTContext().Diags;
        auto numElements = expr->getNumElements();

        // Empty and single element array literals with dictionary contextual
        // types are fixed during solving, so continue as normal in those
        // cases.
        if (numElements > 1) {
          bool isIniting =
              CS.getContextualTypePurpose(expr) == CTP_Initialization;
          DE.diagnose(expr->getStartLoc(), diag::should_use_dictionary_literal,
                      contextualType->lookThroughAllOptionalTypes(), isIniting);

          auto diagnostic =
              DE.diagnose(expr->getStartLoc(), diag::meant_dictionary_lit);

          // If there is an even number of elements in the array, let's produce
          // a fix-it which suggests to replace "," with ":" to form a dictionary
          // literal.
          if ((numElements & 1) == 0) {
            const auto commaLocs = expr->getCommaLocs();
            if (commaLocs.size() == numElements - 1) {
              for (unsigned i = 0, e = numElements / 2; i != e; ++i)
                diagnostic.fixItReplace(commaLocs[i * 2], ":");
            }
          }

          return nullptr;
        }
      }

      auto arrayTy = CS.createTypeVariable(locator,
                                           TVO_PrefersSubtypeBinding |
                                           TVO_CanBindToNoEscape);

      // The array must be an array literal type.
      CS.addConstraint(ConstraintKind::LiteralConformsTo, arrayTy,
                       arrayProto->getDeclaredInterfaceType(),
                       locator);
      
      // Its subexpression should be convertible to a tuple (T.Element...).
      Type arrayElementTy = DependentMemberType::get(arrayTy, elementAssocTy);

      // Introduce conversions from each element to the element type of the
      // array.
      joinElementTypes(arrayElementTy);

      // The array element type defaults to 'Any'.
      CS.addConstraint(ConstraintKind::Defaultable, arrayElementTy,
                       ctx.getAnyExistentialType(), locator);

      return arrayTy;
    }

    static bool isMergeableValueKind(Expr *expr) {
      return isa<StringLiteralExpr>(expr) || isa<IntegerLiteralExpr>(expr) ||
             isa<FloatLiteralExpr>(expr);
    }

    Type visitDictionaryExpr(DictionaryExpr *expr) {
      ASTContext &C = CS.getASTContext();
      // A dictionary expression can be of a type T that conforms to the
      // ExpressibleByDictionaryLiteral protocol.
      // FIXME: This isn't actually used for anything at the moment.
      ProtocolDecl *dictionaryProto = TypeChecker::getProtocol(
          C, expr->getLoc(), KnownProtocolKind::ExpressibleByDictionaryLiteral);
      if (!dictionaryProto) {
        return Type();
      }

      // FIXME: Protect against broken standard library.
      auto keyAssocTy = dictionaryProto->getAssociatedType(C.Id_Key);
      auto valueAssocTy = dictionaryProto->getAssociatedType(C.Id_Value);

      auto locator = CS.getConstraintLocator(expr);
      auto contextualType = CS.getContextualType(expr, /*forConstraint=*/false);
      auto contextualPurpose = CS.getContextualTypePurpose(expr);

      // If a contextual type exists for this expression and is a dictionary
      // type, apply it directly.
      if (contextualType && ConstraintSystem::isDictionaryType(contextualType)) {
        // Now that we know we're actually going to use the type, get the
        // version for use in a constraint.
        contextualType = CS.getContextualType(expr, /*forConstraint=*/true);
        // FIXME: This is the wrong place to be opening the opaque type.
        auto openedType =
            CS.openOpaqueType(contextualType, contextualPurpose, locator,
                              /*ownerDecl=*/nullptr);
        auto dictionaryKeyValue =
            ConstraintSystem::isDictionaryType(openedType);
        Type contextualDictionaryKeyType;
        Type contextualDictionaryValueType;
        std::tie(contextualDictionaryKeyType,
                 contextualDictionaryValueType) = *dictionaryKeyValue;
        
        // Form an explicit tuple type from the contextual type's key and value types.
        TupleTypeElt tupleElts[2] = { TupleTypeElt(contextualDictionaryKeyType),
                                      TupleTypeElt(contextualDictionaryValueType) };
        Type contextualDictionaryElementType = TupleType::get(tupleElts, C);

        CS.addConstraint(ConstraintKind::LiteralConformsTo, openedType,
                         dictionaryProto->getDeclaredInterfaceType(), locator);

        unsigned index = 0;
        for (auto element : expr->getElements()) {
          CS.addConstraint(ConstraintKind::Conversion,
                           CS.getType(element),
                           contextualDictionaryElementType,
                           CS.getConstraintLocator(
                               expr, LocatorPathElt::TupleElement(index++)));
        }

        return openedType;
      }

      auto dictionaryTy = CS.createTypeVariable(locator,
                                                TVO_PrefersSubtypeBinding |
                                                TVO_CanBindToNoEscape);

      // The dictionary must be a dictionary literal type.
      CS.addConstraint(ConstraintKind::LiteralConformsTo, dictionaryTy,
                       dictionaryProto->getDeclaredInterfaceType(),
                       locator);


      // Its subexpression should be convertible to a tuple ((T.Key,T.Value)...).
      ConstraintLocatorBuilder locatorBuilder(locator);
      auto dictionaryKeyTy = DependentMemberType::get(dictionaryTy,
                                                      keyAssocTy);
      auto dictionaryValueTy = DependentMemberType::get(dictionaryTy,
                                                        valueAssocTy);
      TupleTypeElt tupleElts[2] = { TupleTypeElt(dictionaryKeyTy),
                                    TupleTypeElt(dictionaryValueTy) };
      Type elementTy = TupleType::get(tupleElts, C);

      // Keep track of which elements have been "merged". This way, we won't create
      // needless conversion constraints for elements whose equivalence classes have
      // been merged.
      llvm::DenseSet<Expr *> mergedElements;

      // If no contextual type is present, Merge equivalence classes of key 
      // and value types as necessary.
      if (!CS.getContextualType(expr, /*forConstraint=*/false)) {
        for (auto element1 : expr->getElements()) {
          for (auto element2 : expr->getElements()) {
            if (element1 == element2)
              continue;

            auto tty1 = CS.getType(element1)->getAs<TupleType>();
            auto tty2 = CS.getType(element2)->getAs<TupleType>();

            if (tty1 && tty2) {
              auto mergedKey = false;
              auto mergedValue = false;

              auto keyTyvar1 = tty1->getElementTypes()[0]->
                                getAs<TypeVariableType>();
              auto keyTyvar2 = tty2->getElementTypes()[0]->
                                getAs<TypeVariableType>();

              auto keyExpr1 = cast<TupleExpr>(element1)->getElements()[0];
              auto keyExpr2 = cast<TupleExpr>(element2)->getElements()[0];

              if (keyExpr1->getKind() == keyExpr2->getKind() &&
                  isMergeableValueKind(keyExpr1)) {
                mergedKey = mergeRepresentativeEquivalenceClasses(CS,
                            keyTyvar1, keyTyvar2);
              }

              auto valueTyvar1 = tty1->getElementTypes()[1]->
                                  getAs<TypeVariableType>();
              auto valueTyvar2 = tty2->getElementTypes()[1]->
                                  getAs<TypeVariableType>();

              auto elemExpr1 = cast<TupleExpr>(element1)->getElements()[1];
              auto elemExpr2 = cast<TupleExpr>(element2)->getElements()[1];

              if (elemExpr1->getKind() == elemExpr2->getKind() &&
                isMergeableValueKind(elemExpr1)) {
                mergedValue = mergeRepresentativeEquivalenceClasses(CS, 
                                valueTyvar1, valueTyvar2);
              }

              if (mergedKey && mergedValue)
                mergedElements.insert(element2);
            }
          }
        }
      }      

      // Introduce conversions from each element to the element type of the
      // dictionary. (If the equivalence class of an element has already been
      // merged with a previous one, skip it.)
      unsigned index = 0;
      for (auto element : expr->getElements()) {
        if (!mergedElements.count(element))
          CS.addConstraint(ConstraintKind::Conversion,
                           CS.getType(element),
                           elementTy,
                           CS.getConstraintLocator(
                               expr, LocatorPathElt::TupleElement(index++)));
      }

      // The dictionary key type defaults to 'AnyHashable'.
      auto &ctx = CS.getASTContext();
      if (dictionaryKeyTy->isTypeVariableOrMember() &&
          ctx.getAnyHashableDecl()) {
        auto anyHashable = ctx.getAnyHashableDecl();
        CS.addConstraint(ConstraintKind::Defaultable, dictionaryKeyTy,
                         anyHashable->getDeclaredInterfaceType(), locator);
      }

      // The dictionary value type defaults to 'Any'.
      if (dictionaryValueTy->isTypeVariableOrMember()) {
        CS.addConstraint(ConstraintKind::Defaultable, dictionaryValueTy,
                         ctx.getAnyExistentialType(), locator);
      }

      return dictionaryTy;
    }

    Type visitDynamicSubscriptExpr(DynamicSubscriptExpr *expr) {
      return addSubscriptConstraints(expr, CS.getType(expr->getBase()),
                                     /*decl*/ nullptr, expr->getArgs());
    }

    Type visitTupleElementExpr(TupleElementExpr *expr) {
      ASTContext &context = CS.getASTContext();
      DeclNameRef name(
          context.getIdentifier(llvm::utostr(expr->getFieldNumber())));
      return addMemberRefConstraints(expr, expr->getBase(), name,
                                     FunctionRefInfo::unappliedBaseName(),
                                     /*outerAlternatives=*/{});
    }

    /// If \p allowResultBindToHole is \c true, we always allow the closure's
    /// result type to bind to a hole, otherwise the result type may only bind
    /// to a hole if the closure does not participate in type inference. Setting
    /// \p allowResultBindToHole to \c true is useful when ignoring a closure
    /// argument in a function call after the code completion token and thus
    /// wanting to ignore the closure's type.
    FunctionType *inferClosureType(ClosureExpr *closure,
                                   bool allowResultBindToHole = false) {
      SmallVector<AnyFunctionType::Param, 4> closureParams;

      if (auto *paramList = closure->getParameters()) {
        for (unsigned i = 0, n = paramList->size(); i != n; ++i) {
          auto *param = paramList->get(i);
          auto *paramLoc =
              CS.getConstraintLocator(closure, LocatorPathElt::TupleElement(i));

          // If one of the parameters represents a destructured tuple
          // e.g. `{ (x: Int, (y: Int, z: Int)) in ... }` let's fail
          // inference here and not attempt to solve the system because:
          //
          // a. Destructuring has already been diagnosed by the parser;
          // b. Body of the closure would have error expressions for
          //    each incorrect parameter reference and solver wouldn't
          //    be able to produce any viable solutions.
          if (param->isDestructured())
            return nullptr;

          Type externalType;
          if (param->getTypeRepr()) {
            auto declaredTy = param->getTypeInContext();

            // If closure parameter couldn't be resolved, let's record
            // a fix to make sure that type resolution diagnosed the
            // problem and replace it with a placeholder, so that solver
            // can make forward progress (especially important for result
            // builders).
            if (declaredTy->hasError()) {
              CS.recordFix(AllowRefToInvalidDecl::create(
                  CS, CS.getConstraintLocator(param)));
              declaredTy = PlaceholderType::get(CS.getASTContext(), param);
            }

            externalType = CS.replaceInferableTypesWithTypeVars(declaredTy,
                                                                paramLoc);
          } else {
            // Let's allow parameters which haven't been explicitly typed
            // to become holes by default, this helps in situations like
            // `foo { a in }` where `foo` doesn't exist.
            externalType = CS.createTypeVariable(
                paramLoc,
                TVO_CanBindToInOut | TVO_CanBindToNoEscape | TVO_CanBindToHole);
          }

          closureParams.push_back(param->toFunctionParam(externalType));
        }

        checkVariadicParameters(paramList, closure);
      }

      auto extInfo = CS.closureEffects(closure);
      auto resultLocator =
          CS.getConstraintLocator(closure, ConstraintLocator::ClosureResult);

      auto thrownErrorLocator =
        CS.getConstraintLocator(closure, ConstraintLocator::ClosureThrownError);

      // Determine the thrown error type, when appropriate.
      Type thrownErrorTy = [&] {
        // Explicitly-specified thrown type.
        if (closure->getExplicitThrownTypeRepr()) {
          if (Type explicitType = closure->getExplicitThrownType())
            return explicitType;
        }

        // Explicitly-specified 'throws' without a type is untyped throws.
        // Use a NULL return here so that the inferred type is written with
        // `throws` instead of `throws(any Error)`, although the two are
        // semantically equivalent.
        if (closure->getThrowsLoc().isValid())
          return Type();

        // Thrown type inferred from context.
        if (auto contextualType = CS.getContextualType(
                closure, /*forConstraint=*/false)) {
          if (auto fnType = contextualType->getAs<AnyFunctionType>()) {
            if (Type thrownErrorTy = fnType->getThrownError())
              return thrownErrorTy;
          }
        }

        // We do not try to infer a thrown error type if one isn't immediately
        // available.
        return Type();
      }();

      if (thrownErrorTy) {
        // Record the thrown error type in the extended info for the function
        // type of the closure.
        extInfo = extInfo.withThrows(true, thrownErrorTy);

        // Ensure that the thrown error type conforms to Error.
        if (auto errorProto =
                CS.getASTContext().getProtocol(KnownProtocolKind::Error)) {
          CS.addConstraint(
              ConstraintKind::ConformsTo, thrownErrorTy,
              errorProto->getDeclaredInterfaceType(), thrownErrorLocator);
        }
      }

      // Closure expressions always have function type. In cases where a
      // parameter or return type is omitted, a fresh type variable is used to
      // stand in for that parameter or return type, allowing it to be inferred
      // from context.
      Type resultTy = [&] {
        if (closure->hasExplicitResultType()) {
          if (auto declaredTy = closure->getExplicitResultType()) {
            return declaredTy;
          }

          auto options =
              TypeResolutionOptions(TypeResolverContext::InExpression);
          options.setContext(TypeResolverContext::ClosureExpr);
          const auto resolvedTy = resolveTypeReferenceInExpression(
              closure->getExplicitResultTypeRepr(), options, resultLocator);
          if (resolvedTy)
            return resolvedTy;
        }

        // Because we are only pulling out the result type from the contextual
        // type, we avoid prematurely converting any inferrable types by setting
        // forConstraint=false. Later on in inferClosureType we call
        // replaceInferableTypesWithTypeVars before returning to ensure we don't
        // introduce any placeholders into the constraint system.
        if (auto contextualType =
                CS.getContextualType(closure, /*forConstraint=*/false)) {
          if (auto fnType = contextualType->getAs<FunctionType>())
            return fnType->getResult();
        }

        // If no return type was specified, create a fresh type
        // variable for it and mark it as possible hole.
        //
        // If this is a multi-statement closure, let's mark result
        // as potential hole right away.
        return Type(CS.createTypeVariable(
            resultLocator, allowResultBindToHole ? TVO_CanBindToHole : 0));
      }();

      // Determine the isolation of the closure.
      auto isolation = [&] {
        // Priority goes to an explicit isolated parameter.
        if (hasIsolatedParameter(closureParams))
          return FunctionTypeIsolation::forParameter();

        // Honor an explicit global actor.  This is suppressed if the
        // closure is async (but should it be?).
        if (!extInfo.isAsync()) {
          if (auto actorType = getExplicitGlobalActor(closure))
            return FunctionTypeIsolation::forGlobalActor(actorType);
        }

        return FunctionTypeIsolation::forNonIsolated();
      }();
      extInfo = extInfo.withIsolation(isolation);
      if (isolation.isGlobalActor() &&
          CS.getASTContext().LangOpts.hasFeature(Feature::GlobalActorIsolatedTypesUsability)) {
        extInfo = extInfo.withSendable();
      }

      auto *fnTy = FunctionType::get(closureParams, resultTy, extInfo);
      return CS.replaceInferableTypesWithTypeVars(
          fnTy, CS.getConstraintLocator(closure))->castTo<FunctionType>();
    }

    /// Produces a type for the given pattern, filling in any missing
    /// type information with fresh type variables.
    ///
    /// \param pattern The pattern.
    ///
    /// \param locator The locator to use for generated constraints and
    /// type variables.
    ///
    /// \param bindPatternVarsOneWay When true, generate fresh type variables
    /// for the types of each variable declared within the pattern, along
    /// with a one-way constraint binding that to the type to which the
    /// variable will be ascribed or inferred.
    Type getTypeForPattern(
       Pattern *pattern, ConstraintLocatorBuilder locator,
       bool bindPatternVarsOneWay,
       PatternBindingDecl *patternBinding = nullptr,
       unsigned patternBindingIndex = 0) {
      assert(pattern);

      // Local function that must be called for each "return" throughout this
      // function, to set the type of the pattern.
      auto setType = [&](Type type) {
        CS.setType(pattern, type);
        return type;
      };

      switch (pattern->getKind()) {
      case PatternKind::Paren: {
        auto *paren = cast<ParenPattern>(pattern);

        auto *subPattern = paren->getSubPattern();
        auto underlyingType = getTypeForPattern(
            subPattern,
            locator.withPathElement(LocatorPathElt::PatternMatch(subPattern)),
            bindPatternVarsOneWay);

        return setType(underlyingType);
      }
      case PatternKind::Binding: {
        auto *subPattern = cast<BindingPattern>(pattern)->getSubPattern();
        auto type = getTypeForPattern(subPattern, locator,
                                      bindPatternVarsOneWay);
        // Var doesn't affect the type.
        return setType(type);
      }
      case PatternKind::Any: {
        Type type;

        // If this is a situation like `[let] _ = <expr>`, return
        // initializer expression.
        auto getInitializerExpr = [&locator]() -> Expr * {
          auto last = locator.last();
          if (!last)
            return nullptr;

          auto contextualTy = last->getAs<LocatorPathElt::ContextualType>();
          return (contextualTy && contextualTy->isFor(CTP_Initialization))
                     ? locator.trySimplifyToExpr()
                     : nullptr;
        };

        auto matchLoc =
            locator.withPathElement(LocatorPathElt::PatternMatch(pattern));

        // Always prefer a contextual type when it's available.
        if (auto *initializer = getInitializerExpr()) {
          // For initialization always assume a type of initializer.
          type = CS.getType(initializer)->getRValueType();
        } else {
          type = CS.createTypeVariable(
              CS.getConstraintLocator(matchLoc,
                                      LocatorPathElt::AnyPatternDecl()),
              TVO_CanBindToNoEscape | TVO_CanBindToHole);
        }
        return setType(type);
      }

      case PatternKind::Named: {
        auto var = cast<NamedPattern>(pattern)->getDecl();

        Type varType;

        // Determine whether optionality will be required.
        auto ROK = ReferenceOwnership::Strong;
        if (auto *OA = var->getAttrs().getAttribute<ReferenceOwnershipAttr>())
          ROK = OA->get();
        auto optionality = optionalityOf(ROK);

        // If we have a type from an initializer expression, and that
        // expression does not produce an InOut type, use it.  This
        // will avoid exponential typecheck behavior in the case of
        // tuples, nested arrays, and dictionary literals.
        //
        // FIXME: This should be handled in the solver, not here.
        //
        // Otherwise, create a new type variable.
        if (var->getParentPatternBinding() &&
            !var->hasAttachedPropertyWrapper() &&
            optionality != ReferenceOwnershipOptionality::Required) {
          if (auto boundExpr = locator.trySimplifyToExpr()) {
            if (!boundExpr->isSemanticallyInOutExpr()) {
              varType = CS.getType(boundExpr)->getRValueType();
            }
          }
        }

        auto matchLoc =
            locator.withPathElement(LocatorPathElt::PatternMatch(pattern));

        if (!varType) {
          varType = CS.createTypeVariable(
              CS.getConstraintLocator(matchLoc,
                                      LocatorPathElt::NamedPatternDecl()),
              TVO_CanBindToNoEscape | TVO_CanBindToHole);

          // If this is either a `weak` declaration or capture e.g.
          // `weak var ...` or `[weak self]`. Let's wrap type variable
          // into an optional.
          if (optionality == ReferenceOwnershipOptionality::Required)
            varType = TypeChecker::getOptionalType(var->getLoc(), varType);
        }

        auto makeTypeLocatableIfPossible = [&var](Type type) -> Type {
          if (auto loc = var->getLoc()) {
            return LocatableType::get(loc, type);
          }
          return type;
        };

        auto useLocatableTypes = [&]() -> bool {
          if (!CS.inSalvageMode())
            return false;

          return var->isImplicit() &&
                 var->getNameStr().starts_with("$__builder");
        };

        // When we are supposed to bind pattern variables, create a fresh
        // type variable and a one-way constraint to assign it to either the
        // deduced type or the externally-imposed type.
        Type oneWayVarType;
        if (bindPatternVarsOneWay) {
          oneWayVarType = CS.createTypeVariable(
              CS.getConstraintLocator(locator), TVO_CanBindToNoEscape);

          // If there is externally-imposed type, and the variable
          // is marked as `weak`, let's fallthrough and allow the
          // `one-way` constraint to be fixed in diagnostic mode.
          //
          // That would make sure that type of this variable is
          // recorded in the constraint  system, which would then
          // be used instead of `getVarType` upon discovering a
          // reference to this variable in subsequent expression(s).
          //
          // If we let constraint generation fail here, it would trigger
          // interface type request via `var->getType()` that would
          // attempt to validate `weak` attribute, and produce a
          // diagnostic in the middle of the solver path.

          CS.addConstraint(ConstraintKind::OneWayEqual, oneWayVarType,
                           varType, locator);

          if (useLocatableTypes())
            oneWayVarType = makeTypeLocatableIfPossible(oneWayVarType);
        }

        // Ascribe a type to the declaration so it's always available to
        // constraint system.
        if (oneWayVarType) {
          CS.setType(var, oneWayVarType);
        } else {
          // Otherwise, let's use the type of the pattern. The type
          // of the declaration has to be r-value, so let's add an
          // equality constraint if pattern type has any type variables
          // that are allowed to be l-value.
          bool foundLValueVars = false;

          // Note that it wouldn't be always correct to allocate a single type
          // variable, that disallows l-value types, to use as a declaration
          // type because equality constraint would drop TVO_CanBindToLValue
          // from the right-hand side (which is not the case for `OneWayEqual`)
          // e.g.:
          //
          // struct S { var x, y: Int }
          //
          // func test(s: S) {
          //   let (x, y) = (s.x, s.y)
          // }
          //
          // Single type variable approach results in the following constraint:
          // `$T_x_y = ($T_s_x, $T_s_y)` where both `$T_s_x` and `$T_s_y` have
          // to allow l-value, but `$T_x_y` does not. Early simplification of `=`
          // constraint (due to right-hand side being a "concrete" tuple type)
          // would drop l-value option from `$T_s_x` and `$T_s_y` which leads to
          // a failure during member lookup because `x` and `y` are both
          // `@lvalue Int`. To avoid that, declaration type would mimic pattern
          // type with all l-value options stripped, so the equality constraint
          // becomes `($T_x, $_T_y) = ($T_s_x, $T_s_y)` which doesn't result in
          // stripping of l-value flag from the right-hand side since
          // simplification can only happen when either side is resolved.
          auto declTy = varType.transformRec([&](Type type) -> std::optional<Type> {
            if (auto *typeVar = type->getAs<TypeVariableType>()) {
              if (typeVar->getImpl().canBindToLValue()) {
                foundLValueVars = true;

                // Drop l-value from the options but preserve the rest.
                auto options = typeVar->getImpl().getRawOptions();
                options &= ~TVO_CanBindToLValue;

                return Type(CS.createTypeVariable(typeVar->getImpl().getLocator(),
                                                  options));
              }
            }
            return std::nullopt;
          });

          // If pattern types allows l-value types, let's create an
          // equality constraint between r-value only declaration type
          // and l-value pattern type that would take care of looking
          // through l-values when necessary.
          if (foundLValueVars) {
            CS.addConstraint(ConstraintKind::Equal, declTy, varType,
                             CS.getConstraintLocator(locator));
          }

          if (useLocatableTypes())
            declTy = makeTypeLocatableIfPossible(declTy);

          CS.setType(var, declTy);
        }

        return setType(varType);
      }

      case PatternKind::Typed: {
        // FIXME: Need a better locator for a pattern as a base.
        // Compute the type ascribed to the pattern.
        auto contextualPattern = patternBinding
            ? ContextualPattern::forPatternBindingDecl(
                patternBinding, patternBindingIndex)
            : ContextualPattern::forRawPattern(pattern, CurDC);

        Type type = TypeChecker::typeCheckPattern(contextualPattern);

        // Look through reference storage types.
        type = type->getReferenceStorageReferent();

        Type replacedType = CS.replaceInferableTypesWithTypeVars(type, locator);
        Type openedType =
            CS.openOpaqueType(replacedType, CTP_Initialization, locator,
                              patternBinding);
        assert(openedType);

        auto *subPattern = cast<TypedPattern>(pattern)->getSubPattern();
        // Determine the subpattern type. It will be convertible to the
        // ascribed type.
        Type subPatternType = getTypeForPattern(
            subPattern,
            locator.withPathElement(LocatorPathElt::PatternMatch(subPattern)),
            bindPatternVarsOneWay);

        // NOTE: The order here is important! Pattern matching equality is
        // not symmetric (we need to fix that either by using a different
        // constraint, or actually making it symmetric).
        CS.addConstraint(
            ConstraintKind::Equal, openedType, subPatternType,
            locator.withPathElement(LocatorPathElt::PatternMatch(pattern)));

        // FIXME [OPAQUE SUPPORT]: the distinction between where we want opaque
        // types in opened vs. un-unopened form is *very* tricky. The pattern
        // ultimately needs the un-opened type and it gets this from the set
        // type of the expression.
        CS.setType(pattern, replacedType);
        return openedType;
      }

      case PatternKind::Tuple: {
        auto tuplePat = cast<TuplePattern>(pattern);

        SmallVector<TupleTypeElt, 4> tupleTypeElts;
        tupleTypeElts.reserve(tuplePat->getNumElements());
        for (unsigned i = 0, e = tuplePat->getNumElements(); i != e; ++i) {
          auto &tupleElt = tuplePat->getElement(i);

          auto *eltPattern = tupleElt.getPattern();
          Type eltTy = getTypeForPattern(
              eltPattern,
              locator.withPathElement(LocatorPathElt::PatternMatch(eltPattern)),
              bindPatternVarsOneWay);

          tupleTypeElts.push_back(TupleTypeElt(eltTy, tupleElt.getLabel()));
        }

        return setType(TupleType::get(tupleTypeElts, CS.getASTContext()));
      }

      case PatternKind::OptionalSome: {
        auto *subPattern = cast<OptionalSomePattern>(pattern)->getSubPattern();
        // The subpattern must have optional type.
        Type subPatternType = getTypeForPattern(
            subPattern,
            locator.withPathElement(LocatorPathElt::PatternMatch(subPattern)),
            bindPatternVarsOneWay);

        return setType(OptionalType::get(subPatternType));
      }

      case PatternKind::Is: {
        auto isPattern = cast<IsPattern>(pattern);

        const Type castType = resolveTypeReferenceInExpression(
            isPattern->getCastTypeRepr(), TypeResolverContext::InExpression,
            locator.withPathElement(LocatorPathElt::PatternMatch(pattern)));

        // Allow `is` pattern to infer type from context which is then going
        // to be propaged down to its sub-pattern via conversion. This enables
        // correct handling of patterns like `_ as Foo` where `_` would
        // get a type of `Foo` but `is` pattern enclosing it could still be
        // inferred from enclosing context.
        auto isType =
            CS.createTypeVariable(CS.getConstraintLocator(pattern),
                                  TVO_CanBindToNoEscape | TVO_CanBindToHole);

        // Make sure we can cast from the subpattern type to the type we're
        // checking; if it's impossible, fail.
        CS.addConstraint(
            ConstraintKind::CheckedCast, isType, castType,
            locator.withPathElement(LocatorPathElt::PatternMatch(pattern)));

        if (auto *subPattern = isPattern->getSubPattern()) {
          auto subPatternType = getTypeForPattern(
              subPattern,
              locator.withPathElement(LocatorPathElt::PatternMatch(subPattern)),
              bindPatternVarsOneWay);

          // NOTE: The order here is important! Pattern matching equality is
          // not symmetric (we need to fix that either by using a different
          // constraint, or actually making it symmetric).
          CS.addConstraint(
              ConstraintKind::Equal, castType, subPatternType,
              locator.withPathElement(LocatorPathElt::PatternMatch(pattern)));
        }
        return setType(isType);
      }

      case PatternKind::Bool:
        return setType(CS.getASTContext().getBoolType());

      case PatternKind::EnumElement: {
        auto enumPattern = cast<EnumElementPattern>(pattern);

        // Create a type variable to represent the pattern.
        Type patternType =
            CS.createTypeVariable(CS.getConstraintLocator(locator),
                                  TVO_CanBindToNoEscape);

        // Form the member constraint for a reference to a member of this
        // type.
        Type baseType;
        Type memberType = CS.createTypeVariable(
            CS.getConstraintLocator(locator),
            TVO_CanBindToLValue | TVO_CanBindToNoEscape);

        // Tuple splat is still allowed for patterns (with a warning in Swift 5)
        // so we need to set a non-compound reference to make sure that e.g.
        // `case test(x: Int, y: Int)` gets the labels preserved when matched
        // with `case let .test(tuple)`.
        auto functionRefInfo = FunctionRefInfo::unappliedBaseName();
        if (enumPattern->hasSubPattern())
          functionRefInfo = functionRefInfo.addingApplicationLevel();

        // If sub-pattern is a tuple we'd need to mark reference as compound,
        // that would make sure that the labels are dropped in cases
        // when `case` has a single tuple argument (tuple explosion) or multiple
        // arguments (tuple-to-tuple conversion).
        // FIXME: We ought to be preserving labels and matching in the solver.
        if (dyn_cast_or_null<TuplePattern>(enumPattern->getSubPattern()))
          functionRefInfo = FunctionRefInfo::singleCompoundNameApply();

        auto patternLocator =
            locator.withPathElement(LocatorPathElt::PatternMatch(pattern));

        if (enumPattern->getParentType() || enumPattern->getParentTypeRepr()) {
          // Resolve the parent type.
          const auto parentType = [&] {
            auto *const patternMatchLoc = CS.getConstraintLocator(
                locator, {LocatorPathElt::PatternMatch(pattern),
                          ConstraintLocator::ParentType});

            // FIXME: Sometimes the parent type is realized eagerly in
            // ResolvePattern::visitUnresolvedDotExpr, so we have to open it
            // ex post facto. Remove this once we learn how to resolve patterns
            // while generating constraints to keep the opening of generic types
            // contained within the type resolver.
            if (const auto preresolvedTy = enumPattern->getParentType()) {
              const auto openedTy =
                  CS.replaceInferableTypesWithTypeVars(preresolvedTy,
                                                       patternMatchLoc);
              assert(openedTy);
              return openedTy;
            }

            return resolveTypeReferenceInExpression(
                enumPattern->getParentTypeRepr(),
                TypeResolverContext::InExpression, patternMatchLoc);
          }();

          // Perform member lookup into the parent's metatype.
          Type parentMetaType = MetatypeType::get(parentType);
          CS.addValueMemberConstraint(parentMetaType, enumPattern->getName(),
                                      memberType, CurDC, functionRefInfo, {},
                                      patternLocator);

          // Parent type needs to be convertible to the pattern type; this
          // accounts for cases where the pattern type is existential.
          CS.addConstraint(
              ConstraintKind::Conversion, parentType, patternType,
              patternLocator.withPathElement(
                  ConstraintLocator::EnumPatternImplicitCastMatch));

          baseType = parentType;
        } else {
          // Use the pattern type for member lookup.
          CS.addUnresolvedValueMemberConstraint(
              MetatypeType::get(patternType), enumPattern->getName(),
              memberType, CurDC, functionRefInfo, patternLocator);

          baseType = patternType;
        }

        if (auto subPattern = enumPattern->getSubPattern()) {
          // When there is a subpattern, the member will have function type,
          // and we're matching the type of that subpattern to the parameter
          // types.
          Type subPatternType = getTypeForPattern(
              subPattern,
              locator.withPathElement(LocatorPathElt::PatternMatch(subPattern)),
              bindPatternVarsOneWay);

          SmallVector<AnyFunctionType::Param, 4> params;
          decomposeTuple(subPatternType, params);

          // Remove parameter labels; they aren't used when matching cases,
          // but outright conflicts will be checked during coercion.
          for (auto &param : params) {
            param = param.getWithoutLabels();
          }

          Type outputType = CS.createTypeVariable(
              CS.getConstraintLocator(locator),
              TVO_CanBindToNoEscape);
          // Equal constraints require ExtInfo comparison.
          // FIXME: Verify ExtInfo state is correct, not working by accident.
          FunctionType::ExtInfo info;
          Type functionType = FunctionType::get(params, outputType, info);

          // TODO: Convert to own constraint? Note that ApplicableFn isn't quite
          // right, as pattern matching has data flowing *into* the apply result
          // and call arguments, not the other way around.
          // NOTE: The order here is important! Pattern matching equality is
          // not symmetric (we need to fix that either by using a different
          // constraint, or actually making it symmetric).
          CS.addConstraint(ConstraintKind::Equal, functionType, memberType,
                           patternLocator);

          CS.addConstraint(ConstraintKind::Conversion, outputType, baseType,
                           patternLocator);
        }

        return setType(patternType);
      }

      case PatternKind::Expr: {
        // We generate constraints for ExprPatterns in a separate pass. For
        // now, just create a type variable.
        return setType(CS.createTypeVariable(CS.getConstraintLocator(locator),
                                             TVO_CanBindToNoEscape));
      }
      }

      llvm_unreachable("Unhandled pattern kind");
    }

    Type visitCaptureListExpr(CaptureListExpr *expr) {
      // The type of the capture list is just the type of its closure.
      return CS.getType(expr->getClosureBody());
    }

    Type visitClosureExpr(ClosureExpr *closure) {
      auto *locator = CS.getConstraintLocator(closure);
      auto closureType = CS.createTypeVariable(locator, TVO_CanBindToNoEscape);

      TypeVarRefCollector refCollector(CS, /*DC*/ closure, locator);
      // Walk the capture list if this closure has one,  because it could
      // reference declarations from the outer closure.
      if (auto *captureList =
              getAsExpr<CaptureListExpr>(CS.getParentExpr(closure))) {
        captureList->walk(refCollector);
      } else {
        closure->walk(refCollector);
      }

      auto inferredType = inferClosureType(closure);
      if (!inferredType || inferredType->hasError())
        return Type();

      auto referencedVars = refCollector.getTypeVars();
      CS.addUnsolvedConstraint(
          Constraint::create(CS, ConstraintKind::FallbackType, closureType,
                             inferredType, locator, referencedVars));

      if (!OuterExpansions.empty())
        CS.setCapturedExpansions(closure, OuterExpansions);

      CS.setClosureType(closure, inferredType);
      return closureType;
    }

    Type visitAutoClosureExpr(AutoClosureExpr *expr) {
      // AutoClosureExpr is introduced by CSApply.
      llvm_unreachable("Already type-checked");
    }

    Type visitInOutExpr(InOutExpr *expr) {
      // The address-of operator produces an explicit inout T from an lvalue T.
      // We model this with the constraint
      //
      //     S < lvalue T
      //
      // where T is a fresh type variable.
      auto lvalue = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                          TVO_CanBindToNoEscape);
      auto bound = LValueType::get(lvalue);
      auto result = InOutType::get(lvalue);
      CS.addConstraint(ConstraintKind::Conversion,
                       CS.getType(expr->getSubExpr()), bound,
                       CS.getConstraintLocator(expr));
      return result;
    }

    Type visitVarargExpansionExpr(VarargExpansionExpr *expr) {
      // Create a fresh type variable.
      auto element = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                           TVO_CanBindToNoEscape);

      // Try to build the appropriate type for a variadic argument list of
      // the fresh element type.  If that failed, just bail out.
      auto variadicSeq = VariadicSequenceType::get(element);

      // Require the operand to be convertible to the array type.
      CS.addConstraint(ConstraintKind::Conversion,
                       CS.getType(expr->getSubExpr()), variadicSeq,
                       CS.getConstraintLocator(expr));
      return variadicSeq;
    }

    void collectExpandedPacks(PackExpansionExpr *expansion,
                              SmallVectorImpl<ASTNode> &packs) {
      struct PackCollector : public ASTWalker {
      private:
        ConstraintSystem &CS;
        SmallVectorImpl<ASTNode> &Packs;

      public:
        PackCollector(ConstraintSystem &cs, SmallVectorImpl<ASTNode> &packs)
            : CS(cs), Packs(packs) {}

        /// Walk everything that's available.
        MacroWalking getMacroWalkingBehavior() const override {
          return MacroWalking::ArgumentsAndExpansion;
        }

        virtual PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
          // Don't walk into nested pack expansions
          if (isa<PackExpansionExpr>(E)) {
            return Action::SkipNode(E);
          }

          if (isa<PackElementExpr>(E)) {
            Packs.push_back(E);
          }

          if (auto *declRef = dyn_cast<DeclRefExpr>(E)) {
            auto type = CS.getTypeIfAvailable(declRef);
            if (!type)
              return Action::Continue(E);

            if (type->is<ElementArchetypeType>() &&
                CS.hasFixFor(CS.getConstraintLocator(declRef),
                             FixKind::IgnoreMissingEachKeyword)) {
              auto *packElementExpr =
                  PackElementExpr::create(CS.getASTContext(),
                                          /*eachLoc=*/{}, declRef,
                                          /*implicit=*/true, type);
              Packs.push_back(CS.cacheType(packElementExpr));
            }
          }

          return Action::Continue(E);
        }

        virtual PreWalkAction walkToTypeReprPre(TypeRepr *T) override {
          // Don't walk into nested pack expansions
          if (isa<PackExpansionTypeRepr>(T)) {
            return Action::SkipNode();
          }

          if (isa<PackElementTypeRepr>(T)) {
            Packs.push_back(T);
          }

          return Action::Continue();
        }
      } packCollector(CS, packs);

      expansion->getPatternExpr()->walk(packCollector);
    }

    Type visitPackExpansionExpr(PackExpansionExpr *expr) {
      assert(OuterExpansions.back() == expr);
      OuterExpansions.pop_back();

      auto expansionType = CS.getType(expr)->castTo<PackExpansionType>();
      auto elementResultType = CS.getType(expr->getPatternExpr());
      CS.addConstraint(ConstraintKind::PackElementOf, elementResultType,
                       expansionType->getPatternType(),
                       CS.getConstraintLocator(expr));

      // Generate ShapeOf constraints between all packs expanded by this
      // pack expansion expression through the shape type variable.
      SmallVector<ASTNode, 2> expandedPacks;
      collectExpandedPacks(expr, expandedPacks);

      if (expandedPacks.empty()) {
        (void)CS.recordFix(AllowValueExpansionWithoutPackReferences::create(
            CS, CS.getConstraintLocator(expr)));
      }

      for (auto pack : expandedPacks) {
        Type packType;
        /// Skipping over pack elements because the relationship to its
        /// environment is now established during \c pushPackExpansionExpr
        /// upon visiting its pack expansion and the Shape constraint added
        /// upon visiting the pack element.
        if (isExpr<PackElementExpr>(pack)) {
          continue;
        } else if (auto *elementType =
                       getAsTypeRepr<PackElementTypeRepr>(pack)) {
          // OpenPackElementType sets types for 'each T' type reprs in
          // expressions. Some invalid code won't make it there, and
          // the constraint system won't have recorded a type.
          if (!CS.hasType(elementType->getPackType()))
            return Type();

          packType = CS.getType(elementType->getPackType());
        } else {
          llvm_unreachable("unsupported pack reference ASTNode");
        }

        auto *elementShape = CS.createTypeVariable(
            CS.getConstraintLocator(pack, ConstraintLocator::PackShape),
            TVO_CanBindToPack);
        CS.addConstraint(
            ConstraintKind::ShapeOf, elementShape, packType,
            CS.getConstraintLocator(pack, ConstraintLocator::PackShape));
        CS.addConstraint(
            ConstraintKind::Equal, elementShape, expansionType->getCountType(),
            CS.getConstraintLocator(expr, ConstraintLocator::PackShape));
      }

      return expansionType;
    }

    Type visitPackElementExpr(PackElementExpr *expr) {
      auto packType = CS.getType(expr->getPackRefExpr());
      auto *packEnvironment = CS.getPackEnvironment(expr);
      auto elementType = openPackElement(
          packType, CS.getConstraintLocator(expr), packEnvironment);
      if (packEnvironment) {
        auto expansionType =
            CS.getType(packEnvironment)->castTo<PackExpansionType>();
        CS.addConstraint(ConstraintKind::ShapeOf, expansionType->getCountType(),
                         packType,
                         CS.getConstraintLocator(packEnvironment,
                                                 ConstraintLocator::PackShape));
      } else {
        CS.recordFix(AllowInvalidPackReference::create(
            CS, packType, CS.getConstraintLocator(expr->getPackRefExpr())));
      }

      return elementType;
    }

    Type visitMaterializePackExpr(MaterializePackExpr *expr) {
      llvm_unreachable("MaterializePackExpr already type-checked");
    }

    Type visitDynamicTypeExpr(DynamicTypeExpr *expr) {
      auto tv = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                      TVO_CanBindToNoEscape);
      CS.addConstraint(
          ConstraintKind::DynamicTypeOf, tv, CS.getType(expr->getBase()),
          CS.getConstraintLocator(expr, ConstraintLocator::DynamicType));
      return tv;
    }

    Type visitOpaqueValueExpr(OpaqueValueExpr *expr) {
      return expr->getType();
    }

    Type visitPropertyWrapperValuePlaceholderExpr(
        PropertyWrapperValuePlaceholderExpr *expr) {
      if (auto ty = expr->getType()) {
        CS.cacheType(expr);
        return ty;
      }

      assert(CS.getType(expr));
      return CS.getType(expr);
    }

    Type visitAppliedPropertyWrapperExpr(AppliedPropertyWrapperExpr *expr) {
      return expr->getType();
    }

    Type visitDefaultArgumentExpr(DefaultArgumentExpr *expr) {
      return expr->getType();
    }

    Type visitApplyExpr(ApplyExpr *expr) {
      auto fnExpr = expr->getFn();

      CS.associateArgumentList(CS.getConstraintLocator(expr), expr->getArgs());

      if (auto *UDE = dyn_cast<UnresolvedDotExpr>(fnExpr)) {
        auto typeOperation = getTypeOperation(UDE, CS.getASTContext());
        if (typeOperation != TypeOperation::None)
          return resultOfTypeOperation(typeOperation, expr->getArgs());
      }

      // The result type is a fresh type variable.
      Type resultType = CS.createTypeVariable(
          CS.getConstraintLocator(expr, ConstraintLocator::FunctionResult),
          TVO_CanBindToNoEscape);

      // A direct call to a ClosureExpr makes it noescape.
      FunctionType::ExtInfo extInfo;
      if (isa<ClosureExpr>(fnExpr->getSemanticsProvidingExpr()))
        extInfo = extInfo.withNoEscape();

      SmallVector<AnyFunctionType::Param, 8> params;
      getMatchingParams(expr->getArgs(), params);

      CS.addConstraint(ConstraintKind::ApplicableFunction,
                       FunctionType::get(params, resultType, extInfo),
                       CS.getType(fnExpr),
        CS.getConstraintLocator(expr, ConstraintLocator::ApplyFunction));

      // If we ended up resolving the result type variable to a concrete type,
      // set it as the favored type for this expression.
      Type fixedType =
          CS.getFixedTypeRecursive(resultType, /*wantRvalue=*/true);
      if (!fixedType->isTypeVariableOrMember()) {
        resultType = fixedType;
      }

      return resultType;
    }

    Type visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *expr) {
      // The result is void.
      return TupleType::getEmpty(CS.getASTContext());
    }

    Type visitTernaryExpr(TernaryExpr *expr) {
      // Condition must convert to Bool.
      auto boolDecl = CS.getASTContext().getBoolDecl();
      if (!boolDecl)
        return Type();

      CS.addConstraint(
          ConstraintKind::Conversion, CS.getType(expr->getCondExpr()),
          boolDecl->getDeclaredInterfaceType(),
          CS.getConstraintLocator(expr, ConstraintLocator::Condition));

      // The branches must be convertible to a common type.
      return CS.addJoinConstraint(
          CS.getConstraintLocator(expr),
          {{CS.getType(expr->getThenExpr()),
            CS.getConstraintLocator(expr, LocatorPathElt::TernaryBranch(true))},
           {CS.getType(expr->getElseExpr()),
            CS.getConstraintLocator(expr,
                                    LocatorPathElt::TernaryBranch(false))}});
    }

    virtual Type visitImplicitConversionExpr(ImplicitConversionExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type
    createTypeVariableAndDisjunctionForIUOCoercion(Type toType,
                                                   ConstraintLocator *locator) {
      auto typeVar = CS.createTypeVariable(locator, TVO_CanBindToNoEscape);
      CS.buildDisjunctionForImplicitlyUnwrappedOptional(typeVar, toType,
                                                        locator);
      return typeVar;
    }

    Type getTypeForCast(ExplicitCastExpr *E) {
      if (auto *const repr = E->getCastTypeRepr()) {
        // Validate the resulting type.
        return resolveTypeReferenceInExpression(
            repr, TypeResolverContext::ExplicitCastExpr,
            CS.getConstraintLocator(E));
      }
      assert(E->isImplicit());
      return E->getCastType();
    }

    Type visitForcedCheckedCastExpr(ForcedCheckedCastExpr *expr) {
      auto fromExpr = expr->getSubExpr();
      if (!fromExpr) // Either wasn't constructed correctly or wasn't folded.
        return nullptr;

      auto toType = getTypeForCast(expr);
      if (!toType)
        return Type();

      auto *const repr = expr->getCastTypeRepr();

      // Cache the type we're casting to.
      if (repr) CS.setType(repr, toType);

      auto fromType = CS.getType(fromExpr);
      auto locator = CS.getConstraintLocator(expr);

      // The source type can be checked-cast to the destination type.
      CS.addConstraint(ConstraintKind::CheckedCast, fromType, toType, locator);

      // If the result type was declared IUO, add a disjunction for
      // bindings for the result of the coercion.
      if (repr && repr->getKind() == TypeReprKind::ImplicitlyUnwrappedOptional)
        return createTypeVariableAndDisjunctionForIUOCoercion(toType, locator);

      return toType;
    }

    Type visitCoerceExpr(CoerceExpr *expr) {
      // Validate the resulting type.
      auto toType = getTypeForCast(expr);
      if (!toType)
        return nullptr;

      auto *const repr = expr->getCastTypeRepr();

      // Cache the type we're casting to.
      if (repr) CS.setType(repr, toType);

      auto fromType = CS.getType(expr->getSubExpr());
      auto locator =
          CS.getConstraintLocator(expr, ConstraintLocator::CoercionOperand);

      // Literal initialization (e.g. `UInt32(0)`) doesn't require
      // a conversion because the literal is supposed to assume the
      // `to` type.
      //
      // `to` type could be a type variable if i.e. the repr is invalid,
      // in such cases a slower conversion path is a better choice to
      // let it be inferred from the context and/or from the literal itself.
      if (expr->isLiteralInit() && !toType->isTypeVariableOrMember()) {
        CS.addConstraint(ConstraintKind::Equal, fromType, toType, locator);
      } else {
        // Add a conversion constraint for the direct conversion between
        // types.
        CS.addExplicitConversionConstraint(fromType, toType, RememberChoice,
                                           locator);
      }

      // If the result type was declared IUO, add a disjunction for
      // bindings for the result of the coercion.
      if (repr &&
          repr->getKind() == TypeReprKind::ImplicitlyUnwrappedOptional) {
        return createTypeVariableAndDisjunctionForIUOCoercion(
            toType, CS.getConstraintLocator(expr));
      }

      return toType;
    }

    Type visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *expr) {
      auto fromExpr = expr->getSubExpr();
      if (!fromExpr) // Either wasn't constructed correctly or wasn't folded.
        return nullptr;

      // Validate the resulting type.
      const auto toType = getTypeForCast(expr);
      if (!toType)
        return nullptr;

      auto *const repr = expr->getCastTypeRepr();

      // Cache the type we're casting to.
      if (repr) CS.setType(repr, toType);

      auto fromType = CS.getType(fromExpr);
      auto locator = CS.getConstraintLocator(expr);

      CS.addConstraint(ConstraintKind::CheckedCast, fromType, toType, locator);

      // If the result type was declared IUO, add a disjunction for
      // bindings for the result of the coercion.
      if (repr && repr->getKind() == TypeReprKind::ImplicitlyUnwrappedOptional)
        return createTypeVariableAndDisjunctionForIUOCoercion(
            OptionalType::get(toType), locator);

      return OptionalType::get(toType);
    }

    Type visitIsExpr(IsExpr *expr) {
      auto toType = getTypeForCast(expr);
      if (!toType)
        return nullptr;

      auto *const repr = expr->getCastTypeRepr();
      // Cache the type we're checking.
      if (repr)
        CS.setType(repr, toType);

      // Add a checked cast constraint.
      auto fromType = CS.getType(expr->getSubExpr());
      
      CS.addConstraint(ConstraintKind::CheckedCast, fromType, toType,
                       CS.getConstraintLocator(expr));

      auto &ctx = CS.getASTContext();
      // The result is Bool.
      auto boolDecl = ctx.getBoolDecl();

      if (!boolDecl) {
        ctx.Diags.diagnose(SourceLoc(), diag::broken_bool);
        return Type();
      }

      return boolDecl->getDeclaredInterfaceType();
    }

    Type visitDiscardAssignmentExpr(DiscardAssignmentExpr *expr) {
      auto locator = CS.getConstraintLocator(expr);
      auto typeVar = CS.createTypeVariable(locator, TVO_CanBindToNoEscape);
      return LValueType::get(typeVar);
    }

    static Type genAssignDestType(Expr *expr, ConstraintSystem &CS) {
      if (auto *TE = dyn_cast<TupleExpr>(expr)) {
        SmallVector<TupleTypeElt, 4> destTupleTypes;
        for (unsigned i = 0; i !=  TE->getNumElements(); ++i) {
          Type subType = genAssignDestType(TE->getElement(i), CS);
          destTupleTypes.push_back(TupleTypeElt(subType, TE->getElementName(i)));
        }
        return TupleType::get(destTupleTypes, CS.getASTContext());
      } else {
        auto *locator = CS.getConstraintLocator(expr);

        auto exprType = CS.getType(expr);
        auto *destTy = CS.createTypeVariable(locator, TVO_CanBindToNoEscape);
        CS.addConstraint(ConstraintKind::LValueObject, exprType, destTy,
                         locator);
        return destTy;
      }
    }

    Type visitAssignExpr(AssignExpr *expr) {
      // Handle invalid code.
      if (!expr->getDest() || !expr->getSrc())
        return Type();
      Type destTy = genAssignDestType(expr->getDest(), CS);
      CS.addConstraint(ConstraintKind::Conversion,
                       CS.getType(expr->getSrc()), destTy,
                       CS.getConstraintLocator(expr));
      return TupleType::getEmpty(CS.getASTContext());
    }
    
    Type visitUnresolvedPatternExpr(UnresolvedPatternExpr *expr) {
      // Encountering an UnresolvedPatternExpr here means we have an invalid
      // ExprPattern with a Pattern node like 'let x' nested in it. Record a
      // fix, and assign ErrorTypes to any VarDecls bound.
      auto *locator = CS.getConstraintLocator(expr);
      auto *P = expr->getSubPattern();
      CS.recordFix(IgnoreInvalidPatternInExpr::create(CS, P, locator));

      P->forEachVariable([&](VarDecl *VD) {
        CS.setType(VD, ErrorType::get(CS.getASTContext()));
      });

      return CS.createTypeVariable(locator, TVO_CanBindToHole);
    }

    /// Get the type T?
    ///
    ///  This is not the ideal source location, but it's only used for
    /// diagnosing ill-formed standard libraries, so it really isn't
    /// worth QoI efforts.
    Type getOptionalType(SourceLoc optLoc, Type valueTy) {
      auto optTy = TypeChecker::getOptionalType(optLoc, valueTy);
      if (optTy->hasError() ||
          TypeChecker::requireOptionalIntrinsics(CS.getASTContext(), optLoc))
        return Type();

      return optTy;
    }

    Type visitBindOptionalExpr(BindOptionalExpr *expr) {
      // The operand must be coercible to T?, and we will have type T.
      auto locator = CS.getConstraintLocator(expr);

      auto objectTy = CS.createTypeVariable(locator,
                                            TVO_PrefersSubtypeBinding |
                                            TVO_CanBindToLValue |
                                            TVO_CanBindToNoEscape);
      
      // The result is the object type of the optional subexpression.
      CS.addConstraint(ConstraintKind::OptionalObject,
                       CS.getType(expr->getSubExpr()), objectTy,
                       locator);
      return objectTy;
    }
    
    Type visitOptionalEvaluationExpr(OptionalEvaluationExpr *expr) {
      // The operand must be coercible to T? for some type T.  We'd
      // like this to be the smallest possible nesting level of
      // optional types, e.g. T? over T??; otherwise we don't really
      // have a preference.
      auto valueTy = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                           TVO_PrefersSubtypeBinding |
                                           TVO_CanBindToNoEscape);

      Type optTy = getOptionalType(expr->getSubExpr()->getLoc(), valueTy);
      if (!optTy)
        return Type();

      CS.addConstraint(ConstraintKind::Conversion,
                       CS.getType(expr->getSubExpr()), optTy,
                       CS.getConstraintLocator(expr));
      return optTy;
    }

    Type visitForceValueExpr(ForceValueExpr *expr) {
      // Force-unwrap an optional of type T? to produce a T.
      auto locator = CS.getConstraintLocator(expr);

      auto objectTy = CS.createTypeVariable(locator,
                                            TVO_PrefersSubtypeBinding |
                                            TVO_CanBindToLValue |
                                            TVO_CanBindToNoEscape);

      auto *valueExpr = expr->getSubExpr();
      // It's invalid to force unwrap `nil` literal e.g. `_ = nil!` or
      // `_ = (try nil)!` and similar constructs.
      if (auto *nilLiteral = dyn_cast<NilLiteralExpr>(
              valueExpr->getSemanticsProvidingExpr())) {
        CS.recordFix(SpecifyContextualTypeForNil::create(
            CS, CS.getConstraintLocator(nilLiteral)));
      }

      // The result is the object type of the optional subexpression.
      CS.addConstraint(ConstraintKind::OptionalObject, CS.getType(valueExpr),
                       objectTy, locator);
      return objectTy;
    }

    Type visitOpenExistentialExpr(OpenExistentialExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    Type visitMakeTemporarilyEscapableExpr(MakeTemporarilyEscapableExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    Type visitKeyPathApplicationExpr(KeyPathApplicationExpr *expr) {
      // This should only appear in already-type-checked solutions, but we may
      // need to re-check for failure diagnosis.
      auto locator = CS.getConstraintLocator(expr);
      auto projectedTy = CS.createTypeVariable(locator,
                                               TVO_CanBindToLValue |
                                               TVO_CanBindToNoEscape);
      CS.addKeyPathApplicationConstraint(CS.getType(expr->getKeyPath()),
                                         CS.getType(expr->getBase()),
                                         projectedTy,
                                         locator);
      return projectedTy;
    }
    
    Type visitEnumIsCaseExpr(EnumIsCaseExpr *expr) {
      return CS.getASTContext().getBoolType();
    }

    Type visitLazyInitializerExpr(LazyInitializerExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitEditorPlaceholderExpr(EditorPlaceholderExpr *E) {
      auto *locator = CS.getConstraintLocator(E);

      if (auto *placeholderRepr = E->getPlaceholderTypeRepr()) {
        // Let's try to use specified type, if that's impossible,
        // fallback to a type variable.
        if (auto preferredTy = resolveTypeReferenceInExpression(
                placeholderRepr, TypeResolverContext::InExpression, locator))
          return preferredTy;
      }

      // A placeholder may have any type, but default to Void type if
      // otherwise unconstrained.
      auto *placeholderTy =
          CS.createTypeVariable(locator, TVO_CanBindToNoEscape);

      CS.addConstraint(ConstraintKind::Defaultable, placeholderTy,
                       TupleType::getEmpty(CS.getASTContext()), locator);

      return placeholderTy;
    }

    Type visitObjCSelectorExpr(ObjCSelectorExpr *E) {
      // #selector only makes sense when we have the Objective-C
      // runtime.
      auto &ctx = CS.getASTContext();
      if (!ctx.LangOpts.EnableObjCInterop) {
        ctx.Diags.diagnose(E->getLoc(), diag::expr_selector_no_objc_runtime);
        return nullptr;
      }

      
      // Make sure we can reference ObjectiveC.Selector.
      // FIXME: Fix-It to add the import?
      auto type = CS.getASTContext().getSelectorType();
      if (!type) {
        ctx.Diags.diagnose(E->getLoc(), diag::expr_selector_module_missing);
        return nullptr;
      }

      return type;
    }

    Type visitKeyPathExpr(KeyPathExpr *E) {
      if (E->isObjC())
        return CS.getType(E->getObjCStringLiteralExpr());
      
      auto kpDecl = CS.getASTContext().getKeyPathDecl();
      
      if (!kpDecl) {
        auto &de = CS.getASTContext().Diags;
        de.diagnose(E->getLoc(), diag::expr_keypath_no_keypath_type);
        return ErrorType::get(CS.getASTContext());
      }
      
      // For native key paths, traverse the key path components to set up
      // appropriate type relationships at each level.
      auto rootLocator =
          CS.getConstraintLocator(E, ConstraintLocator::KeyPathRoot);
      auto locator = CS.getConstraintLocator(E);
      auto *root = CS.createTypeVariable(rootLocator, TVO_CanBindToNoEscape |
                                                          TVO_CanBindToHole);

      // If a root type was explicitly given, then resolve it now.
      if (auto rootRepr = E->getExplicitRootType()) {
        const auto rootObjectTy = resolveTypeReferenceInExpression(
            rootRepr, TypeResolverContext::InExpression, locator);
        if (!rootObjectTy || rootObjectTy->hasError())
          return Type();

        CS.setType(rootRepr, rootObjectTy);
        // Allow \Derived.property to be inferred as \Base.property to
        // simulate a sort of covariant conversion from
        // KeyPath<Derived, T> to KeyPath<Base, T>.
        CS.addConstraint(ConstraintKind::Subtype, rootObjectTy, root,
                         rootLocator);
      }

      bool didOptionalChain = false;
      // We start optimistically from an lvalue base.
      Type base = LValueType::get(root);

      SmallVector<TypeVariableType *, 2> componentTypeVars;
      for (unsigned i : indices(E->getComponents())) {
        auto &component = E->getComponents()[i];
        auto memberLocator = CS.getConstraintLocator(
            locator, LocatorPathElt::KeyPathComponent(i));
        auto resultLocator = CS.getConstraintLocator(
            memberLocator, ConstraintLocator::KeyPathComponentResult);

        switch (auto kind = component.getKind()) {
        case KeyPathExpr::Component::Kind::Invalid:
          break;
        case KeyPathExpr::Component::Kind::CodeCompletion:
          // We don't know what the code completion might resolve to, so we are
          // creating a new type variable for its result, which might be a hole.
          base = CS.createTypeVariable(
              resultLocator,
              TVO_CanBindToLValue | TVO_CanBindToNoEscape | TVO_CanBindToHole);
          break;
        case KeyPathExpr::Component::Kind::UnresolvedProperty:
        // This should only appear in resolved ASTs, but we may need to
        // re-type-check the constraints during failure diagnosis.
        case KeyPathExpr::Component::Kind::Property: {
          auto memberTy = CS.createTypeVariable(resultLocator,
                                                TVO_CanBindToLValue |
                                                TVO_CanBindToNoEscape);
          componentTypeVars.push_back(memberTy);
          auto lookupName = kind == KeyPathExpr::Component::Kind::UnresolvedProperty
            ? DeclNameRef(component.getUnresolvedDeclName()) // FIXME: type change needed
            : component.getDeclRef().getDecl()->createNameRef();

          CS.addValueMemberConstraint(base, lookupName, memberTy, CurDC,
                                      FunctionRefInfo::unapplied(lookupName),
                                      /*outerAlternatives=*/{}, memberLocator);
          base = memberTy;
          break;
        }
          
        case KeyPathExpr::Component::Kind::UnresolvedSubscript:
        // Subscript should only appear in resolved ASTs, but we may need to
        // re-type-check the constraints during failure diagnosis.
        case KeyPathExpr::Component::Kind::Subscript: {
          auto *args = component.getSubscriptArgs();
          base = addSubscriptConstraints(E, base, /*decl*/ nullptr, args,
                                         memberLocator, &componentTypeVars);

          auto &ctx = CS.getASTContext();
          // All of the type variables that appear in subscript arguments
          // need to be connected to a key path, otherwise it won't be
          // possible to determine sendability of the key path type if
          // the arguments are disconnected from it before being fully
          // resolved.
          if (args &&
              ctx.LangOpts.hasFeature(Feature::InferSendableFromCaptures)) {
            SmallPtrSet<TypeVariableType *, 2> referencedVars;
            for (const auto &arg : *args) {
              CS.getType(arg.getExpr())->getTypeVariables(referencedVars);
            }

            componentTypeVars.append(referencedVars.begin(),
                                     referencedVars.end());
          }
          break;
        }

        case KeyPathExpr::Component::Kind::TupleElement: {
          // Note: If implemented, the logic in `getCalleeLocator` will need
          // updating to return the correct callee locator for this.
          llvm_unreachable("not implemented");
          break;
        }
                
        case KeyPathExpr::Component::Kind::OptionalChain: {
          didOptionalChain = true;

          // We can't assign an optional back through an optional chain
          // today. Force the base to an rvalue.
          auto rvalueTy = CS.createTypeVariable(resultLocator,
                                                TVO_CanBindToNoEscape);
          componentTypeVars.push_back(rvalueTy);
          CS.addConstraint(ConstraintKind::Equal, base, rvalueTy,
                           resultLocator);

          base = rvalueTy;
          LLVM_FALLTHROUGH;
        }
        case KeyPathExpr::Component::Kind::OptionalForce: {
          auto optionalObjTy = CS.createTypeVariable(resultLocator,
                                                     TVO_CanBindToLValue |
                                                     TVO_CanBindToNoEscape);
          componentTypeVars.push_back(optionalObjTy);

          CS.addConstraint(ConstraintKind::OptionalObject, base, optionalObjTy,
                           resultLocator);
          base = optionalObjTy;
          break;
        }
        
        case KeyPathExpr::Component::Kind::OptionalWrap: {
          // This should only appear in resolved ASTs, but we may need to
          // re-type-check the constraints during failure diagnosis.
          base = OptionalType::get(base);
          break;
        }
        case KeyPathExpr::Component::Kind::Identity:
          break;
        case KeyPathExpr::Component::Kind::DictionaryKey:
          llvm_unreachable("DictionaryKey only valid in #keyPath");
          break;
        }

        // By now, `base` is the result type of this component. Set it in the
        // constraint system so we can find it later.
        CS.setType(E, i, base);
      }

      auto valueLocator =
        CS.getConstraintLocator(E, ConstraintLocator::KeyPathValue);

      // If there was an optional chaining component, the end result must be
      // optional.
      if (didOptionalChain) {
        auto objTy = CS.createTypeVariable(locator, TVO_CanBindToNoEscape |
                                                        TVO_CanBindToHole);
        componentTypeVars.push_back(objTy);

        auto optTy = OptionalType::get(objTy);
        CS.addConstraint(ConstraintKind::Conversion, base, optTy, valueLocator);
        base = optTy;
      }

      // If we have a malformed KeyPathExpr e.g. let _: KeyPath<A, C> = \A
      // let's record a AllowKeyPathMissingComponent fix.
      if (E->hasSingleInvalidComponent()) {
        (void)CS.recordFix(AllowKeyPathWithoutComponents::create(CS, locator));
      }


      auto *value = CS.createTypeVariable(valueLocator, TVO_CanBindToNoEscape |
                                                            TVO_CanBindToHole);
      CS.addConstraint(ConstraintKind::Equal, base, value, valueLocator);
      CS.recordKeyPath(E, root, value, CurDC);

      // The result is a KeyPath from the root to the end component.
      // The type of key path depends on the overloads chosen for the key
      // path components.
      auto typeLoc =
          CS.getConstraintLocator(locator, LocatorPathElt::KeyPathType());

      Type kpTy = CS.createTypeVariable(typeLoc, TVO_CanBindToNoEscape |
                                                     TVO_CanBindToHole);

      CS.addKeyPathConstraint(kpTy, root, value, componentTypeVars, locator);

      // Add a fallback constraint so we have an anchor to use for
      // capability inference when there is no context.
      CS.addUnsolvedConstraint(Constraint::create(
          CS, ConstraintKind::FallbackType, kpTy,
          BoundGenericType::get(kpDecl, /*parent=*/Type(), {root, value}),
          CS.getConstraintLocator(E, ConstraintLocator::FallbackType)));

      return kpTy;
    }

    Type visitCurrentContextIsolationExpr(CurrentContextIsolationExpr *E) {
      // If this was expanded from the builtin `#isolation` macro, it
      // already has a type.
      if (auto type = E->getType())
        return type;

      // Otherwise, this was created for a `for await` loop, where its
      // type is always `(any Actor)?`.
      auto actorProto = CS.getASTContext().getProtocol(
          KnownProtocolKind::Actor);
      return OptionalType::get(actorProto->getDeclaredExistentialType());
    }

    Type visitExtractFunctionIsolationExpr(ExtractFunctionIsolationExpr *E) {
      llvm_unreachable("found ExtractFunctionIsolationExpr in CSGen");
    }

    Type visitKeyPathDotExpr(KeyPathDotExpr *E) {
      llvm_unreachable("found KeyPathDotExpr in CSGen");
    }

    Type visitSingleValueStmtExpr(SingleValueStmtExpr *E) {
      llvm_unreachable("Handled by the walker directly");
    }

    Type visitOneWayExpr(OneWayExpr *expr) {
      auto locator = CS.getConstraintLocator(expr);
      auto resultTypeVar = CS.createTypeVariable(locator, 0);
      CS.addConstraint(ConstraintKind::OneWayEqual, resultTypeVar,
                       CS.getType(expr->getSubExpr()), locator);
      return resultTypeVar;
    }

    Type visitTapExpr(TapExpr *expr) {
      DeclContext *varDC = expr->getVar()->getDeclContext();
      ASSERT(varDC != nullptr);
      ASSERT((varDC == CS.DC ||
              isa<AbstractClosureExpr>(varDC) ||
              varDC->isChildContextOf(CS.DC)) &&
             "TapExpr var should be in the same DeclContext we're checking it in!");
      
      auto locator = CS.getConstraintLocator(expr);
      auto tv = CS.createTypeVariable(locator, TVO_CanBindToNoEscape);

      if (auto subExpr = expr->getSubExpr()) {
        auto subExprType = CS.getType(subExpr);
        CS.addConstraint(ConstraintKind::Bind, subExprType, tv, locator);
      }

      return tv;
    }

    Type visitTypeJoinExpr(TypeJoinExpr *expr) {
      auto *locator = CS.getConstraintLocator(expr);
      SmallVector<std::pair<Type, ConstraintLocator *>, 4> elements;
      elements.reserve(expr->getNumElements());

      if (auto *SVE = expr->getSingleValueStmtExpr()) {
        // If we have a SingleValueStmtExpr, form a join of the branch types.
        SmallVector<Expr *, 4> scratch;
        auto branches = SVE->getResultExprs(scratch);
        for (auto idx : indices(branches)) {
          auto *eltLoc = CS.getConstraintLocator(
              SVE, {LocatorPathElt::SingleValueStmtResult(idx)});
          elements.emplace_back(CS.getType(branches[idx]), eltLoc);
        }
      } else {
        for (auto *element : expr->getElements()) {
          elements.emplace_back(CS.getType(element),
                                CS.getConstraintLocator(element));
        }
      }

      Type resultTy;

      if (auto *var = expr->getVar()) {
        resultTy = CS.getType(var);
      } else {
        resultTy = expr->getType();
      }

      assert(resultTy);

      // If we have a single branch of a SingleValueStmtExpr, we want a
      // conversion of the result, not a join, which would skip the conversion.
      // This is needed to ensure we apply the Void/Never conversions.
      if (elements.size() == 1 && expr->getSingleValueStmtExpr()) {
        auto &elt = elements[0];
        CS.addConstraint(ConstraintKind::Conversion, elt.first,
                         resultTy, elt.second);
        return resultTy;
      }

      // The type of a join expression is obtained by performing
      // a "join-meet" operation on deduced types of its elements
      // and the underlying variable.
      auto joinedTy = CS.addJoinConstraint(locator, elements);

      CS.addConstraint(ConstraintKind::Equal, resultTy, joinedTy, locator);
      return resultTy;
    }

    /// Lookup all macros with the given macro name.
    SmallVector<OverloadChoice, 1> lookupMacros(Identifier moduleName,
                                                Identifier macroName,
                                                FunctionRefInfo functionRefInfo,
                                                MacroRoles roles) {
      SmallVector<OverloadChoice, 1> choices;
      auto results = namelookup::lookupMacros(CurDC, DeclNameRef(moduleName),
                                              DeclNameRef(macroName), roles);
      for (const auto &result : results) {
        // Ignore invalid results. This matches the OverloadedDeclRefExpr
        // logic.
        if (result->isInvalid())
          continue;
        OverloadChoice choice = OverloadChoice(Type(), result, functionRefInfo);
        choices.push_back(choice);
      }

      // FIXME: At some point, we need to check for function-like macros without
      // arguments and vice-versa.

      return choices;
    }

    Type visitMacroExpansionExpr(MacroExpansionExpr *expr) {
      // Assign a discriminator.
      (void)expr->getDiscriminator();

      auto &ctx = CS.getASTContext();
      auto locator = CS.getConstraintLocator(expr);

      CS.associateArgumentList(locator, expr->getArgs());

      // Look up the macros with this name.
      auto moduleIdent = expr->getModuleName().getBaseIdentifier();
      auto macroIdent = expr->getMacroName().getBaseIdentifier();
      FunctionRefInfo functionRefInfo = FunctionRefInfo::singleBaseNameApply();
      auto macros = lookupMacros(moduleIdent, macroIdent, functionRefInfo,
                                 expr->getMacroRoles());
      if (macros.empty()) {
        ctx.Diags.diagnose(expr->getMacroNameLoc(), diag::macro_undefined,
                           macroIdent)
            .highlight(expr->getMacroNameLoc().getSourceRange());
        return Type();
      }

      // Introduce an overload set for the macro reference.
      auto macroRefType = Type(CS.createTypeVariable(locator, 0));
      CS.addOverloadSet(macroRefType, macros, CurDC, locator);

      // Add explicit generic arguments, if there were any.
      if (expr->getGenericArgsRange().isValid()) {
        addSpecializationConstraint(CS.getConstraintLocator(expr), macroRefType,
                                    expr->getGenericArgsRange().Start,
                                    expr->getGenericArgs());
      }

      // Form the applicable-function constraint. The result type
      // is the result of that call.
      SmallVector<AnyFunctionType::Param, 8> params;
      getMatchingParams(expr->getArgs(), params);

      Type resultType = CS.createTypeVariable(
          CS.getConstraintLocator(expr, ConstraintLocator::FunctionResult),
          TVO_CanBindToNoEscape);

      CS.addConstraint(
          ConstraintKind::ApplicableFunction,
          FunctionType::get(params, resultType),
          macroRefType,
          CS.getConstraintLocator(
            expr, ConstraintLocator::ApplyFunction));

      return resultType;
    }

    static bool isTriggerFallbackDiagnosticBuiltin(UnresolvedDotExpr *UDE,
                                                   ASTContext &Context) {
      auto *DRE = dyn_cast<DeclRefExpr>(UDE->getBase());
      if (!DRE)
        return false;

      if (DRE->getDecl() != Context.TheBuiltinModule)
        return false;

      auto member = UDE->getName().getBaseName().userFacingName();
      return member == "trigger_fallback_diagnostic";
    }

    enum class TypeOperation { None,
                               Join,
                               JoinInout,
                               JoinMeta,
                               JoinNonexistent,
                               OneWay,
    };

    static TypeOperation getTypeOperation(UnresolvedDotExpr *UDE,
                                          ASTContext &Context) {
      auto *DRE = dyn_cast<DeclRefExpr>(UDE->getBase());
      if (!DRE)
        return TypeOperation::None;

      if (DRE->getDecl() != Context.TheBuiltinModule)
        return TypeOperation::None;

      return llvm::StringSwitch<TypeOperation>(
                 UDE->getName().getBaseIdentifier().str())
          .Case("one_way", TypeOperation::OneWay)
          .Case("type_join", TypeOperation::Join)
          .Case("type_join_inout", TypeOperation::JoinInout)
          .Case("type_join_meta", TypeOperation::JoinMeta)
          .Case("type_join_nonexistent", TypeOperation::JoinNonexistent)
          .Default(TypeOperation::None);
    }

    Type resultOfTypeOperation(TypeOperation op, ArgumentList *Args) {
      auto *lhs = Args->getExpr(0);
      auto *rhs = Args->getExpr(1);

      switch (op) {
      case TypeOperation::None:
      case TypeOperation::OneWay:
        llvm_unreachable(
            "We should have a valid type operation at this point!");

      case TypeOperation::Join: {
        auto lhsMeta = CS.getType(lhs)->getAs<MetatypeType>();
        auto rhsMeta = CS.getType(rhs)->getAs<MetatypeType>();
        if (!lhsMeta || !rhsMeta)
          llvm_unreachable("Unexpected argument types for Builtin.type_join!");

        auto &ctx = lhsMeta->getASTContext();

        auto join =
            Type::join(lhsMeta->getInstanceType(), rhsMeta->getInstanceType());

        if (!join)
          return ErrorType::get(ctx);

        return MetatypeType::get(*join, ctx)->getCanonicalType();
      }

      case TypeOperation::JoinInout: {
        auto lhsInOut = CS.getType(lhs)->getAs<InOutType>();
        auto rhsMeta = CS.getType(rhs)->getAs<MetatypeType>();
        if (!lhsInOut || !rhsMeta)
          llvm_unreachable("Unexpected argument types for Builtin.type_join!");

        auto &ctx = lhsInOut->getASTContext();

        auto join =
            Type::join(lhsInOut, rhsMeta->getInstanceType());

        if (!join)
          return ErrorType::get(ctx);

        return MetatypeType::get(*join, ctx)->getCanonicalType();
      }

      case TypeOperation::JoinMeta: {
        auto lhsMeta = CS.getType(lhs)->getAs<MetatypeType>();
        auto rhsMeta = CS.getType(rhs)->getAs<MetatypeType>();
        if (!lhsMeta || !rhsMeta)
          llvm_unreachable("Unexpected argument types for Builtin.type_join!");

        auto &ctx = lhsMeta->getASTContext();

        auto join = Type::join(lhsMeta, rhsMeta);

        if (!join)
          return ErrorType::get(ctx);

        return *join;
      }

      case TypeOperation::JoinNonexistent: {
        auto lhsMeta = CS.getType(lhs)->getAs<MetatypeType>();
        auto rhsMeta = CS.getType(rhs)->getAs<MetatypeType>();
        if (!lhsMeta || !rhsMeta)
          llvm_unreachable("Unexpected argument types for Builtin.type_join_nonexistent!");

        auto &ctx = lhsMeta->getASTContext();

        auto join =
            Type::join(lhsMeta->getInstanceType(), rhsMeta->getInstanceType());

        // Verify that we could not compute a join.
        if (join)
          llvm_unreachable("Unexpected result from join - it should not have been computable!");

        // The return value is unimportant.
        return MetatypeType::get(ctx.getAnyExistentialType())->getCanonicalType();
      }
      }
      llvm_unreachable("unhandled operation");
    }

    /// Assuming that we are solving for code completion, assign \p expr a fresh
    /// and unconstrained type variable as its type.
    void setTypeForArgumentIgnoredForCompletion(Expr *expr) {
      assert(CS.isForCodeCompletion());
      ConstraintSystem &CS = getConstraintSystem();

      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        FunctionType *closureTy =
            inferClosureType(closure, /*allowResultBindToHole=*/true);
        CS.setClosureType(closure, closureTy);
        CS.setType(closure, closureTy);
      } else {
        TypeVariableType *exprType = CS.createTypeVariable(
            CS.getConstraintLocator(expr),
            TVO_CanBindToLValue | TVO_CanBindToInOut | TVO_CanBindToNoEscape |
                TVO_CanBindToHole);
        CS.setType(expr, exprType);
      }
    }
  };

  class ConstraintWalker : public ASTWalker {
    ConstraintGenerator &CG;

  public:
    ConstraintWalker(ConstraintGenerator &CG) : CG(CG) { }

    MacroWalking getMacroWalkingBehavior() const override {
      return MacroWalking::Arguments;
    }

    PreWalkResult<Expr *> walkToExprPre(Expr *expr) override {
      auto &CS = CG.getConstraintSystem();

      if (CS.isArgumentIgnoredForCodeCompletion(expr)) {
        CG.setTypeForArgumentIgnoredForCompletion(expr);
        return Action::SkipNode(expr);
      }

      if (auto *SVE = dyn_cast<SingleValueStmtExpr>(expr)) {
        if (CS.generateConstraints(SVE))
          return Action::Stop();
        return Action::SkipNode(expr);
      }

      // Note that the subexpression of a #selector expression is
      // unevaluated.
      if (auto sel = dyn_cast<ObjCSelectorExpr>(expr)) {
        auto *subExpr = sel->getSubExpr()->getSemanticsProvidingExpr();
        CG.getConstraintSystem().UnevaluatedRootExprs.insert(subExpr);
      }

      // Check an objc key-path expression, which fills in its semantic
      // expression as a string literal.
      if (auto keyPath = dyn_cast<KeyPathExpr>(expr)) {
        if (keyPath->isObjC()) {
          auto &cs = CG.getConstraintSystem();
          (void)TypeChecker::checkObjCKeyPathExpr(cs.DC, keyPath);
        }
      }

      // Generate constraints for each of the entries in the capture list.
      if (auto captureList = dyn_cast<CaptureListExpr>(expr)) {
        TypeChecker::diagnoseDuplicateCaptureVars(captureList);
      }

      // Both multi- and single-statement closures now behave the same way
      // when it comes to constraint generation.
      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        auto &CS = CG.getConstraintSystem();
        auto closureType = CG.visitClosureExpr(closure);
        if (!closureType)
          return Action::Stop();

        CS.setType(expr, closureType);
        return Action::SkipNode(expr);
      }

      // Don't visit CoerceExpr with an empty sub expression. They may occur
      // if the body of a closure was not visited while pre-checking because
      // of an error in the closure's signature.
      if (auto coerceExpr = dyn_cast<CoerceExpr>(expr)) {
        if (!coerceExpr->getSubExpr()) {
          return Action::SkipNode(expr);
        }
      }

      // Don't visit TernaryExpr with empty sub expressions. They may occur
      // if the body of a closure was not visited while pre-checking because
      // of an error in the closure's signature.
      if (auto *ternary = dyn_cast<TernaryExpr>(expr)) {
        if (!ternary->getThenExpr() || !ternary->getElseExpr())
          return Action::SkipNode(expr);
      }

      if (CS.isForCodeCompletion()) {
        SmallVector<Expr *, 2> ignoredArgs;
        getArgumentsAfterCodeCompletionToken(expr, CS, ignoredArgs);
        for (auto ignoredArg : ignoredArgs) {
          CS.markArgumentIgnoredForCodeCompletion(ignoredArg);
        }
      }

      if (auto *expansion = dyn_cast<PackExpansionExpr>(expr)) {
        CG.pushPackExpansionExpr(expansion);
      }

      return Action::Continue(expr);
    }

    /// Once we've visited the children of the given expression,
    /// generate constraints from the expression.
    PostWalkResult<Expr *> walkToExprPost(Expr *expr) override {
      auto &CS = CG.getConstraintSystem();
      // Translate special type-checker Builtin calls into simpler expressions.
      if (auto *apply = dyn_cast<ApplyExpr>(expr)) {
        auto fnExpr = apply->getFn();
        if (auto *UDE = dyn_cast<UnresolvedDotExpr>(fnExpr)) {
          auto typeOperation =
              ConstraintGenerator::getTypeOperation(UDE, CS.getASTContext());

          if (typeOperation == ConstraintGenerator::TypeOperation::OneWay) {
            // For a one-way constraint, create the OneWayExpr node.
            auto *unaryArg = apply->getArgs()->getUnlabeledUnaryExpr();
            assert(unaryArg);
            expr = new (CS.getASTContext()) OneWayExpr(unaryArg);
          } else if (typeOperation !=
                         ConstraintGenerator::TypeOperation::None) {
            // Handle the Builtin.type_join* family of calls by replacing
            // them with dot_self_expr of type_expr with the type being the
            // result of the join.
            auto joinMetaTy =
                CG.resultOfTypeOperation(typeOperation, apply->getArgs());
            auto joinTy = joinMetaTy->castTo<MetatypeType>();

            auto *TE = TypeExpr::createImplicit(joinTy->getInstanceType(),
                                                CS.getASTContext());
            CS.cacheType(TE);

            auto *DSE = new (CS.getASTContext())
                DotSelfExpr(TE, SourceLoc(), SourceLoc(), CS.getType(TE));
            DSE->setImplicit();
            CS.cacheType(DSE);

            return Action::Continue(DSE);
          }
        }
      }
      if (auto type = CG.visit(expr)) {
        auto simplifiedType = CS.simplifyType(type);
        CS.setType(expr, simplifiedType);
        return Action::Continue(expr);
      }
      return Action::Stop();
    }

    /// Ignore statements.
    PreWalkResult<Stmt *> walkToStmtPre(Stmt *stmt) override {
      return Action::SkipNode(stmt);
    }

    /// Ignore declarations.
    PreWalkAction walkToDeclPre(Decl *decl) override {
      return Action::SkipNode();
    }
  };
} // end anonymous namespace

static Expr *generateConstraintsFor(ConstraintSystem &cs, Expr *expr,
                                    DeclContext *DC) {
  // Walk the expression, generating constraints.
  ConstraintGenerator cg(cs, DC);
  ConstraintWalker cw(cg);

  return expr->walk(cw);
}

bool ConstraintSystem::generateWrappedPropertyTypeConstraints(
    VarDecl *wrappedVar, Type initializerType, Type propertyType) {
  Type wrappedValueType;
  Type wrapperType;
  auto wrapperAttributes = wrappedVar->getAttachedPropertyWrappers();
  for (unsigned i : indices(wrapperAttributes)) {
    // FIXME: We should somehow pass an OpenUnboundGenericTypeFn to
    // AttachedPropertyWrapperTypeRequest::evaluate to open up unbound
    // generics on the fly.
    Type rawWrapperType = wrappedVar->getAttachedPropertyWrapperType(i);
    auto wrapperInfo = wrappedVar->getAttachedPropertyWrapperTypeInfo(i);
    if (rawWrapperType->hasError() || !wrapperInfo)
      return true;

    auto *typeExpr = wrapperAttributes[i]->getTypeExpr();

    if (!wrappedValueType) {
      // Equate the outermost wrapper type to the initializer type.
      auto *locator = getConstraintLocator(typeExpr);
      wrapperType =
          replaceInferableTypesWithTypeVars(rawWrapperType, locator);
      if (initializerType)
        addConstraint(ConstraintKind::Equal, wrapperType, initializerType, locator);
    } else {
      // The former wrappedValue type must be equal to the current wrapper type
      auto *locator = getConstraintLocator(
          typeExpr, LocatorPathElt::WrappedValue(wrapperType));
      wrapperType =
          replaceInferableTypesWithTypeVars(rawWrapperType, locator);
      addConstraint(ConstraintKind::Equal, wrapperType, wrappedValueType, locator);
    }

    setType(typeExpr, wrapperType);

    wrappedValueType = wrapperType->getTypeOfMember(
        wrapperInfo.valueVar);
  }

  // The property type must be equal to the wrapped value type
  addConstraint(
      ConstraintKind::Equal, propertyType, wrappedValueType,
      getConstraintLocator(
          wrappedVar, LocatorPathElt::ContextualType(CTP_WrappedProperty)));

  ContextualTypeInfo contextInfo(wrappedValueType, CTP_WrappedProperty);
  setContextualInfo(wrappedVar, contextInfo);
  return false;
}

/// Generate additional constraints for the pattern of an initialization.
static bool generateInitPatternConstraints(ConstraintSystem &cs,
                                           SyntacticElementTarget target,
                                           Expr *initializer) {
  auto locator = cs.getConstraintLocator(
      initializer, LocatorPathElt::ContextualType(CTP_Initialization));

  Type patternType;
  if (auto pattern = target.getInitializationPattern()) {
    patternType = cs.generateConstraints(
        pattern, locator, target.shouldBindPatternVarsOneWay(),
        target.getInitializationPatternBindingDecl(),
        target.getInitializationPatternBindingIndex());
  } else {
    patternType = cs.createTypeVariable(locator, TVO_CanBindToNoEscape);
  }

  if (auto wrappedVar = target.getInitializationWrappedVar())
    return cs.generateWrappedPropertyTypeConstraints(
        wrappedVar, cs.getType(target.getAsExpr()), patternType);

  // Add a conversion constraint between the types.
  cs.addConstraint(ConstraintKind::Conversion, cs.getType(target.getAsExpr()),
                   patternType, locator, /*isFavored*/true);

  return false;
}

/// Generate constraints for a for-in statement preamble where the expression
/// is a `PackExpansionExpr`.
static std::optional<PackIterationInfo>
generateForEachStmtConstraints(ConstraintSystem &cs, DeclContext *dc,
                               PackExpansionExpr *expansion, Type patternType) {
  auto packIterationInfo = PackIterationInfo();
  auto elementLocator = cs.getConstraintLocator(
      expansion, ConstraintLocator::SequenceElementType);

  {
    SyntacticElementTarget target(expansion, dc, CTP_Unused,
                                  /*contextualType=*/Type(),
                                  /*isDiscarded=*/false);

    if (cs.generateConstraints(target))
      return std::nullopt;

    cs.setTargetFor(expansion, target);
  }

  auto elementType = cs.getType(expansion->getPatternExpr());

  cs.addConstraint(ConstraintKind::Conversion, elementType, patternType,
                   elementLocator);

  packIterationInfo.patternType = patternType;
  return packIterationInfo;
}

/// Generate constraints for a for-in statement preamble, expecting an
/// expression that conforms to `Swift.Sequence`.
static std::optional<SequenceIterationInfo>
generateForEachStmtConstraints(ConstraintSystem &cs, DeclContext *dc,
                               ForEachStmt *stmt, Pattern *typeCheckedPattern,
                               bool shouldBindPatternVarsOneWay,
                               bool ignoreForEachWhereClause) {
  ASTContext &ctx = cs.getASTContext();
  bool isAsync = stmt->getAwaitLoc().isValid();
  auto *sequenceExpr = stmt->getParsedSequence();
  auto contextualLocator = cs.getConstraintLocator(
      sequenceExpr, LocatorPathElt::ContextualType(CTP_ForEachSequence));
  auto elementLocator = cs.getConstraintLocator(
      sequenceExpr, ConstraintLocator::SequenceElementType);

  auto sequenceIterationInfo = SequenceIterationInfo();

  // The expression type must conform to the Sequence protocol.
  auto sequenceProto = TypeChecker::getProtocol(
      cs.getASTContext(), stmt->getForLoc(),
      isAsync ? KnownProtocolKind::AsyncSequence : KnownProtocolKind::Sequence);
  if (!sequenceProto)
    return std::nullopt;

  std::string name;
  {
    if (auto np = dyn_cast_or_null<NamedPattern>(stmt->getPattern()))
      name = "$"+np->getBoundName().str().str();
    name += "$generator";
  }

  auto *makeIteratorVar = new (ctx)
      VarDecl(/*isStatic=*/false, VarDecl::Introducer::Var,
              sequenceExpr->getStartLoc(), ctx.getIdentifier(name), dc);
  makeIteratorVar->setImplicit();

  // FIXME: Apply `nonisolated(unsafe)` to async iterators.
  //
  // Async iterators are not `Sendable`; they're only meant to be used from
  // the isolation domain that creates them. But the `next()` method runs on
  // the generic executor, so calling it from an actor-isolated context passes
  // non-`Sendable` state across the isolation boundary. `next()` should
  // inherit the isolation of the caller, but for now, use the opt out.
  if (isAsync) {
    auto *nonisolated = new (ctx)
        NonisolatedAttr(/*unsafe=*/true, /*implicit=*/true);
    makeIteratorVar->getAttrs().add(nonisolated);
  }

  // First, let's form a call from sequence to `.makeIterator()` and save
  // that in a special variable which is going to be used by SILGen.
  {
    FuncDecl *makeIterator = isAsync ? ctx.getAsyncSequenceMakeAsyncIterator()
                                     : ctx.getSequenceMakeIterator();

    auto *makeIteratorRef = UnresolvedDotExpr::createImplicit(
        ctx, sequenceExpr, makeIterator->getName());
    makeIteratorRef->setFunctionRefInfo(FunctionRefInfo::singleBaseNameApply());

    auto *makeIteratorCall =
        CallExpr::createImplicitEmpty(ctx, makeIteratorRef);

    Pattern *pattern = NamedPattern::createImplicit(ctx, makeIteratorVar);
    auto *PB = PatternBindingDecl::createImplicit(
        ctx, StaticSpellingKind::None, pattern, makeIteratorCall, dc);

    auto makeIteratorTarget = SyntacticElementTarget::forInitialization(
        makeIteratorCall, /*patternType=*/Type(), PB, /*index=*/0,
        /*shouldBindPatternsOneWay=*/false);

    ContextualTypeInfo contextInfo(sequenceProto->getDeclaredInterfaceType(),
                                   CTP_ForEachSequence);
    cs.setContextualInfo(sequenceExpr, contextInfo);

    if (cs.generateConstraints(makeIteratorTarget))
      return std::nullopt;

    sequenceIterationInfo.makeIteratorVar = PB;

    // Type of sequence expression has to conform to Sequence protocol.
    //
    // Note that the following emulates having `$generator` separately
    // type-checked by introducing a `TVO_PrefersSubtypeBinding` type
    // variable that would make sure that result of `.makeIterator` would
    // get ranked standalone.
    {
      auto *externalIteratorType = cs.createTypeVariable(
          cs.getConstraintLocator(sequenceExpr), TVO_PrefersSubtypeBinding);

      cs.addConstraint(ConstraintKind::Equal, externalIteratorType,
                       cs.getType(sequenceExpr),
                       externalIteratorType->getImpl().getLocator());

      cs.addConstraint(ConstraintKind::ConformsTo, externalIteratorType,
                       sequenceProto->getDeclaredInterfaceType(),
                       contextualLocator);

      sequenceIterationInfo.sequenceType = cs.getType(sequenceExpr);
    }

    cs.setTargetFor({PB, /*index=*/0}, makeIteratorTarget);
  }

  // Now, result type of `.makeIterator()` is used to form a call to
  // `.next()`. `next()` is called on each iteration of the loop.
  {
    FuncDecl *nextFn = 
        TypeChecker::getForEachIteratorNextFunction(dc, stmt->getForLoc(), isAsync);
    Identifier nextId = nextFn ? nextFn->getName().getBaseIdentifier()
                               : ctx.Id_next;
    TinyPtrVector<Identifier> labels;
    if (nextFn && nextFn->getParameters()->size() == 1)
      labels.push_back(ctx.Id_isolation);
    auto *nextRef = UnresolvedDotExpr::createImplicit(
        ctx,
        new (ctx) DeclRefExpr(makeIteratorVar, DeclNameLoc(stmt->getForLoc()),
                              /*Implicit=*/true),
        nextId, labels);
    nextRef->setFunctionRefInfo(FunctionRefInfo::singleBaseNameApply());

    ArgumentList *nextArgs;
    if (nextFn && nextFn->getParameters()->size() == 1) {
      auto isolationArg =
        new (ctx) CurrentContextIsolationExpr(stmt->getForLoc(), Type());
      nextArgs = ArgumentList::createImplicit(
          ctx, {Argument(SourceLoc(), ctx.Id_isolation, isolationArg)});
    } else {
      nextArgs = ArgumentList::createImplicit(ctx, {});
    }
    Expr *nextCall = CallExpr::createImplicit(ctx, nextRef, nextArgs);

    // `next` is always async but witness might not be throwing
    if (isAsync) {
      nextCall =
          AwaitExpr::createImplicit(ctx, nextCall->getLoc(), nextCall);
    }

    // The iterator type must conform to IteratorProtocol.
    {
      ProtocolDecl *iteratorProto = TypeChecker::getProtocol(
          cs.getASTContext(), stmt->getForLoc(),
          isAsync ? KnownProtocolKind::AsyncIteratorProtocol
                  : KnownProtocolKind::IteratorProtocol);
      if (!iteratorProto)
        return std::nullopt;

      ContextualTypeInfo contextInfo(iteratorProto->getDeclaredInterfaceType(),
                                     CTP_ForEachSequence);
      cs.setContextualInfo(nextRef->getBase(), contextInfo);
    }

    SyntacticElementTarget nextTarget(nextCall, dc, CTP_Unused,
                                      /*contextualType=*/Type(),
                                      /*isDiscarded=*/false);
    if (cs.generateConstraints(nextTarget, FreeTypeVariableBinding::Disallow))
      return std::nullopt;

    sequenceIterationInfo.nextCall = nextTarget.getAsExpr();
    cs.setTargetFor(sequenceIterationInfo.nextCall, nextTarget);
  }

  // Generate constraints for the pattern.
  Type initType =
      cs.generateConstraints(typeCheckedPattern, elementLocator,
                             shouldBindPatternVarsOneWay, nullptr, 0);
  if (!initType)
    return std::nullopt;

  // Add a conversion constraint between the element type of the sequence
  // and the type of the element pattern.
  auto *elementTypeLoc = cs.getConstraintLocator(
      elementLocator, ConstraintLocator::OptionalPayload);
  auto elementType = cs.createTypeVariable(elementTypeLoc,
                                           /*flags=*/0);
  {
    auto nextType = cs.getType(sequenceIterationInfo.nextCall);
    cs.addConstraint(ConstraintKind::OptionalObject, nextType, elementType,
                     elementTypeLoc);
    cs.addConstraint(ConstraintKind::Conversion, elementType, initType,
                     elementLocator);
  }

  // Generate constraints for the "where" expression, if there is one.
  auto *whereExpr = stmt->getWhere();
  if (whereExpr && !ignoreForEachWhereClause) {
    Type boolType = dc->getASTContext().getBoolType();
    if (!boolType)
      return std::nullopt;

    SyntacticElementTarget whereTarget(whereExpr, dc, CTP_Condition, boolType,
                                       /*isDiscarded=*/false);
    if (cs.generateConstraints(whereTarget, FreeTypeVariableBinding::Disallow))
      return std::nullopt;

    cs.setTargetFor(whereExpr, whereTarget);

    ContextualTypeInfo contextInfo(boolType, CTP_Condition);
    cs.setContextualInfo(whereExpr, contextInfo);
  }

  // Populate all of the information for a for-each loop.
  sequenceIterationInfo.elementType = elementType;
  sequenceIterationInfo.initType = initType;

  return sequenceIterationInfo;
}

static std::optional<SyntacticElementTarget>
generateForEachPreambleConstraints(ConstraintSystem &cs,
                                   SyntacticElementTarget target) {
  ForEachStmt *stmt = target.getAsForEachStmt();
  auto *forEachExpr = stmt->getParsedSequence();
  auto *dc = target.getDeclContext();

  auto elementLocator = cs.getConstraintLocator(
      forEachExpr, ConstraintLocator::SequenceElementType);

  Pattern *pattern = TypeChecker::resolvePattern(stmt->getPattern(), dc,
                                                 /*isStmtCondition*/ false);
  if (!pattern)
    return std::nullopt;
  target.setPattern(pattern);

  auto contextualPattern = ContextualPattern::forRawPattern(pattern, dc);

  if (TypeChecker::typeCheckPattern(contextualPattern)->hasError()) {
    return std::nullopt;
  }

  if (isa<PackExpansionExpr>(forEachExpr)) {
    auto *expansion = cast<PackExpansionExpr>(forEachExpr);

    // Generate constraints for the pattern.
    Type patternType = cs.generateConstraints(
        pattern, elementLocator, target.shouldBindPatternVarsOneWay(), nullptr,
        0);
    if (!patternType)
      return std::nullopt;

    if (auto whereClause = stmt->getWhere()) {
      cs.recordFix(IgnoreWhereClauseInPackIteration::create(
          cs, cs.getConstraintLocator(whereClause)));
    }

    auto packIterationInfo =
        generateForEachStmtConstraints(cs, dc, expansion, patternType);
    if (!packIterationInfo) {
      return std::nullopt;
    }

    target.getForEachStmtInfo() = *packIterationInfo;
  } else {
    auto sequenceIterationInfo = generateForEachStmtConstraints(
        cs, dc, stmt, pattern, target.shouldBindPatternVarsOneWay(),
        target.ignoreForEachWhereClause());
    if (!sequenceIterationInfo) {
      return std::nullopt;
    }

    target.getForEachStmtInfo() = *sequenceIterationInfo;
  }
  return target;
}

bool ConstraintSystem::generateConstraints(
    SyntacticElementTarget &target,
    FreeTypeVariableBinding allowFreeTypeVariables) {
  if (Expr *expr = target.getAsExpr()) {
    // If the target requires an optional of some type, form a new appropriate
    // type variable and update the target's type with an optional of that
    // type variable.
    if (target.isOptionalSomePatternInit()) {
      assert(!target.getExprContextualType() &&
             "some pattern cannot have contextual type pre-configured");
      auto *convertTypeLocator = getConstraintLocator(
          expr, LocatorPathElt::ContextualType(
                    target.getExprContextualTypePurpose()));
      Type var = createTypeVariable(convertTypeLocator, TVO_CanBindToNoEscape);
      target.setExprConversionType(TypeChecker::getOptionalType(expr->getLoc(), var));
    }

    // If we have a parent return statement, record whether it's implied.
    if (auto *RS = target.getParentReturnStmt()) {
      if (RS->isImplied())
        recordImpliedResult(expr, ImpliedResultKind::Regular);
    }

    expr = buildTypeErasedExpr(expr, target.getDeclContext(),
                               target.getExprContextualType(),
                               target.getExprContextualTypePurpose());

    // Generate constraints for the main system.
    expr = generateConstraints(expr, target.getDeclContext());
    if (!expr)
      return true;
    target.setExpr(expr);

    // If there is a type that we're expected to convert to, add the conversion
    // constraint.
    if (Type convertType = target.getExprConversionType()) {
      ContextualTypePurpose ctp = target.getExprContextualTypePurpose();

      // If a custom locator wasn't specified, create a locator anchored on
      // the expression itself.
      auto *convertTypeLocator = target.getExprConvertTypeLocator();
      if (!convertTypeLocator) {
        convertTypeLocator =
            getConstraintLocator(expr, LocatorPathElt::ContextualType(ctp));
      }

      auto getLocator = [&](Type ty) -> ConstraintLocator * {
        // If we have a placeholder originating from a PlaceholderTypeRepr,
        // tack that on to the locator.
        if (auto *placeholderTy = ty->getAs<PlaceholderType>())
          if (auto *typeRepr = placeholderTy->getOriginator()
                                          .dyn_cast<TypeRepr *>())
            return getConstraintLocator(
                convertTypeLocator,
                LocatorPathElt::PlaceholderType(typeRepr));
        return convertTypeLocator;
      };

      // Substitute type variables in for placeholder types (and unresolved
      // types, if allowed).
      if (allowFreeTypeVariables == FreeTypeVariableBinding::UnresolvedType) {
        convertType = convertType.transformRec([&](Type type) -> std::optional<Type> {
          if (type->is<UnresolvedType>() || type->is<PlaceholderType>()) {
            return Type(createTypeVariable(getLocator(type),
                                           TVO_CanBindToNoEscape |
                                               TVO_PrefersSubtypeBinding |
                                               TVO_CanBindToHole));
          }
          return std::nullopt;
        });
      } else {
        convertType = convertType.transformRec([&](Type type) -> std::optional<Type> {
          if (type->is<PlaceholderType>()) {
            return Type(createTypeVariable(getLocator(type),
                                           TVO_CanBindToNoEscape |
                                               TVO_PrefersSubtypeBinding |
                                               TVO_CanBindToHole));
          }
          return std::nullopt;
        });
      }

      addContextualConversionConstraint(expr, convertType, ctp,
                                        convertTypeLocator);
    }

    // For an initialization target, generate constraints for the pattern.
    if (target.isForInitialization() &&
        generateInitPatternConstraints(*this, target, expr)) {
      return true;
    }

    if (isDebugMode()) {
      auto &log = llvm::errs();
      log << "\n---Initial constraints for the given expression---\n";
      print(log, expr);
      log << "\n";
      print(log);
      log << "\n";
    }

    return false;
  }

  switch (target.kind) {
  case SyntacticElementTarget::Kind::expression:
    llvm_unreachable("Handled above");

  case SyntacticElementTarget::Kind::closure:
  case SyntacticElementTarget::Kind::caseLabelItem:
  case SyntacticElementTarget::Kind::function:
  case SyntacticElementTarget::Kind::stmtCondition:
    llvm_unreachable("Handled separately");

  case SyntacticElementTarget::Kind::patternBinding: {
    auto patternBinding = target.getAsPatternBinding();
    auto dc = target.getDeclContext();
    bool hadError = false;

    /// Generate constraints for each pattern binding entry
    for (unsigned index : range(patternBinding->getNumPatternEntries())) {
      auto *pattern = TypeChecker::resolvePattern(
          patternBinding->getPattern(index), dc, /*isStmtCondition=*/true);

      if (!pattern)
        return true;

      // Reset binding to point to the resolved pattern. This is required
      // before calling `forPatternBindingDecl`.
      patternBinding->setPattern(index, pattern);

      auto contextualPattern =
          ContextualPattern::forPatternBindingDecl(patternBinding, index);
      Type patternType = TypeChecker::typeCheckPattern(contextualPattern);

      // Fail early if pattern couldn't be type-checked.
      if (!patternType || patternType->hasError())
        return true;

      auto *init = patternBinding->getInit(index);

      if (!init && patternBinding->isDefaultInitializable(index) &&
          pattern->hasStorage()) {
        init = TypeChecker::buildDefaultInitializer(patternType);
      }

      auto target = init ? SyntacticElementTarget::forInitialization(
                               init, patternType, patternBinding, index,
                               /*bindPatternVarsOneWay=*/true)
                         : SyntacticElementTarget::forUninitializedVar(
                               patternBinding, index, patternType);

      if (generateConstraints(target)) {
        hadError = true;
        continue;
      }

      // Keep track of this binding entry.
      setTargetFor({patternBinding, index}, target);
    }

    return hadError;
  }

  case SyntacticElementTarget::Kind::uninitializedVar: {
    if (auto *wrappedVar = target.getAsUninitializedWrappedVar()) {
      auto propertyType = wrappedVar->getTypeInContext();
      if (propertyType->hasError())
        return true;

      return generateWrappedPropertyTypeConstraints(
        wrappedVar, /*initializerType=*/Type(), propertyType);
    } else {
      auto pattern = target.getAsUninitializedVar();
      auto locator = getConstraintLocator(
          pattern, LocatorPathElt::ContextualType(CTP_Initialization));

      // Generate constraints to bind all of the internal declarations
      // and verify the pattern.
      Type patternType = generateConstraints(
          pattern, locator, /*shouldBindPatternVarsOneWay*/ true,
          target.getPatternBindingOfUninitializedVar(),
          target.getIndexOfUninitializedVar());

      return !patternType;
    }
  }

  case SyntacticElementTarget::Kind::forEachPreamble: {

    // Cache the outer generic environment, if it exists.
    if (target.getPackElementEnv()) {
      PackElementGenericEnvironments.push_back(target.getPackElementEnv());
      ASSERT(!solverState && "Need to record a change");
    }

    // For a for-each statement, generate constraints for the pattern, where
    // clause, and sequence traversal.
    auto resultTarget = generateForEachPreambleConstraints(*this, target);
    if (!resultTarget)
      return true;

    target = *resultTarget;
    return false;
  }
  }
}

Expr *ConstraintSystem::generateConstraints(Expr *expr, DeclContext *dc) {
  InputExprs.insert(expr);
  return generateConstraintsFor(*this, expr, dc);
}

Type ConstraintSystem::generateConstraints(
    Pattern *pattern, ConstraintLocatorBuilder locator,
    bool bindPatternVarsOneWay, PatternBindingDecl *patternBinding,
    unsigned patternIndex) {
  ConstraintGenerator cg(*this, nullptr);
  auto ty = cg.getTypeForPattern(pattern, locator, bindPatternVarsOneWay,
                                 patternBinding, patternIndex);
  assert(ty);

  // Gather the ExprPatterns, and form a conjunction for their expressions.
  SmallVector<ExprPattern *, 4> exprPatterns;
  pattern->forEachNode([&](Pattern *P) {
    if (auto *EP = dyn_cast<ExprPattern>(P))
      exprPatterns.push_back(EP);
  });
  if (!exprPatterns.empty())
    generateConstraints(exprPatterns, getConstraintLocator(pattern));

  return ty;
}

bool ConstraintSystem::generateConstraints(StmtCondition condition,
                                           DeclContext *dc) {
  // FIXME: This should be folded into constraint generation for conditions.
  auto boolDecl = getASTContext().getBoolDecl();
  if (!boolDecl) {
    return true;
  }

  Type boolTy = boolDecl->getDeclaredInterfaceType();
  for (auto &condElement : condition) {
    switch (condElement.getKind()) {
    case StmtConditionElement::CK_Availability:
      // Nothing to do here.
      continue;

    case StmtConditionElement::CK_HasSymbol: {
      Expr *symbolExpr = condElement.getHasSymbolInfo()->getSymbolExpr();
      auto target = SyntacticElementTarget(symbolExpr, dc, CTP_Unused, Type(),
                                           /*isDiscarded=*/false);

      if (generateConstraints(target))
        return true;

      setTargetFor(&condElement, target);
      continue;
    }

    case StmtConditionElement::CK_Boolean: {
      Expr *condExpr = condElement.getBoolean();
      auto target = SyntacticElementTarget(condExpr, dc, CTP_Condition, boolTy,
                                           /*isDiscarded=*/false);

      if (generateConstraints(target, FreeTypeVariableBinding::Disallow))
        return true;

      setTargetFor(&condElement, target);
      continue;
    }

    case StmtConditionElement::CK_PatternBinding: {
      auto *pattern = TypeChecker::resolvePattern(
          condElement.getPattern(), dc, /*isStmtCondition*/true);
      if (!pattern)
        return true;

      auto target = SyntacticElementTarget::forInitialization(
          condElement.getInitializer(), dc, Type(), pattern,
          /*bindPatternVarsOneWay=*/true);
      if (generateConstraints(target, FreeTypeVariableBinding::Disallow))
        return true;

      setTargetFor(&condElement, target);
      continue;
    }
    }
  }

  return false;
}

void ConstraintSystem::applyPropertyWrapper(
    Expr *anchor, AppliedPropertyWrapper applied) {
  appliedPropertyWrappers[anchor].push_back(applied);

  if (solverState)
    recordChange(SolverTrail::Change::AppliedPropertyWrapper(anchor));
}

void ConstraintSystem::removePropertyWrapper(Expr *anchor) {
  auto found = appliedPropertyWrappers.find(anchor);
  ASSERT(found != appliedPropertyWrappers.end());
  auto &wrappers = found->second;
  ASSERT(!wrappers.empty());
  wrappers.pop_back();
  if (wrappers.empty()) {
    bool erased = appliedPropertyWrappers.erase(anchor);
    ASSERT(erased);
  }
}

ConstraintSystem::TypeMatchResult
ConstraintSystem::applyPropertyWrapperToParameter(
    Type wrapperType, Type paramType, ParamDecl *param, Identifier argLabel,
    ConstraintKind matchKind, ConstraintLocatorBuilder locator) {
  Expr *anchor = getAsExpr(locator.getAnchor());
  if (auto *apply = dyn_cast<ApplyExpr>(anchor)) {
    anchor = apply->getFn();
  }

  auto recordPropertyWrapperFix = [&](ConstraintFix *fix) -> TypeMatchResult {
    if (!shouldAttemptFixes())
      return getTypeMatchFailure(locator);

    recordAnyTypeVarAsPotentialHole(paramType);

    if (recordFix(fix))
      return getTypeMatchFailure(locator);

    return getTypeMatchSuccess();
  };

  // Incorrect use of projected value argument
  if (argLabel.hasDollarPrefix() &&
      (!param || !param->hasExternalPropertyWrapper())) {
    auto *loc = getConstraintLocator(locator);
    auto *fix =
        RemoveProjectedValueArgument::create(*this, wrapperType, param, loc);
    return recordPropertyWrapperFix(fix);
  }

  // Missing wrapped value initializer
  auto wrapperInfo = param->getAttachedPropertyWrapperTypeInfo(0);
  if (!argLabel.hasDollarPrefix() && !wrapperInfo.wrappedValueInit &&
      param->hasExternalPropertyWrapper()) {
    auto *loc = getConstraintLocator(locator);
    auto *fix = UsePropertyWrapper::create(
        *this, param, /*usingProjection=*/true, paramType, wrapperType, loc);
    return recordPropertyWrapperFix(fix);
  }

  if (argLabel.hasDollarPrefix()) {
    Type projectionType = computeProjectedValueType(param, wrapperType);
    addConstraint(matchKind, paramType, projectionType, locator);
    if (param->hasImplicitPropertyWrapper()) {
      auto wrappedValueType = getType(param->getPropertyWrapperWrappedValueVar());
      addConstraint(ConstraintKind::PropertyWrapper, projectionType, wrappedValueType,
                    getConstraintLocator(param));
      setType(param->getPropertyWrapperProjectionVar(), projectionType);
    }

    applyPropertyWrapper(anchor, { wrapperType, PropertyWrapperInitKind::ProjectedValue });
  } else if (param->hasExternalPropertyWrapper()) {
    Type wrappedValueType = computeWrappedValueType(param, wrapperType);
    addConstraint(matchKind, paramType, wrappedValueType, locator);
    setType(param->getPropertyWrapperWrappedValueVar(), wrappedValueType);

    applyPropertyWrapper(anchor, { wrapperType, PropertyWrapperInitKind::WrappedValue });
  } else {
    return getTypeMatchFailure(locator);
  }

  return getTypeMatchSuccess();
}

struct ResolvedMemberResult::Implementation {
  llvm::SmallVector<ValueDecl*, 4> AllDecls;
  unsigned ViableStartIdx;
  std::optional<unsigned> BestIdx;
};

ResolvedMemberResult::ResolvedMemberResult(): Impl(new Implementation()) {}

ResolvedMemberResult::~ResolvedMemberResult() { delete Impl; }

ResolvedMemberResult::operator bool() const {
  return !Impl->AllDecls.empty();
}

bool ResolvedMemberResult::
hasBestOverload() const { return Impl->BestIdx.has_value(); }

ValueDecl* ResolvedMemberResult::
getBestOverload() const { return Impl->AllDecls[Impl->BestIdx.value()]; }

ArrayRef<ValueDecl*> ResolvedMemberResult::
getMemberDecls(InterestedMemberKind Kind) {
  auto Result = llvm::ArrayRef(Impl->AllDecls);
  switch (Kind) {
  case InterestedMemberKind::Viable:
    return Result.slice(Impl->ViableStartIdx);
  case InterestedMemberKind::Unviable:
    return Result.slice(0, Impl->ViableStartIdx);
  case InterestedMemberKind::All:
    return Result;
  }
  llvm_unreachable("unhandled kind");
}

ResolvedMemberResult swift::resolveValueMember(DeclContext &DC, Type BaseTy,
                                               DeclName Name) {
  ResolvedMemberResult Result;
  ConstraintSystem CS(&DC, std::nullopt);

  // Look up all members of BaseTy with the given Name.
  MemberLookupResult LookupResult =
      CS.performMemberLookup(ConstraintKind::ValueMember, DeclNameRef(Name),
                             BaseTy, FunctionRefInfo::singleBaseNameApply(),
                             CS.getConstraintLocator({}), false);

  // Keep track of all the unviable members.
  for (auto Can : LookupResult.UnviableCandidates)
    Result.Impl->AllDecls.push_back(Can.getDecl());

  // Keep track of the start of viable choices.
  Result.Impl->ViableStartIdx = Result.Impl->AllDecls.size();

  // If no viable members, we are done.
  if (LookupResult.ViableCandidates.empty())
    return Result;

  // If there's only one viable member, that is the best one.
  if (LookupResult.ViableCandidates.size() == 1) {
    Result.Impl->BestIdx = Result.Impl->AllDecls.size();
    Result.Impl->AllDecls.push_back(LookupResult.ViableCandidates[0].getDecl());
    return Result;
  }

  // Try to figure out the best overload.
  ConstraintLocator *Locator = CS.getConstraintLocator({});
  TypeVariableType *TV = CS.createTypeVariable(Locator,
                                               TVO_CanBindToLValue |
                                               TVO_CanBindToNoEscape);
  CS.addOverloadSet(TV, LookupResult.ViableCandidates, &DC, Locator);
  std::optional<Solution> OpSolution = CS.solveSingle();
  ValueDecl *Selected = nullptr;
  if (OpSolution.has_value()) {
    Selected = OpSolution.value().overloadChoices[Locator].choice.getDecl();
  }
  for (OverloadChoice& Choice : LookupResult.ViableCandidates) {
    ValueDecl *VD = Choice.getDecl();

    // If this VD is the best overload, keep track of its index.
    if (VD == Selected)
      Result.Impl->BestIdx = Result.Impl->AllDecls.size();
    Result.Impl->AllDecls.push_back(VD);
  }
  return Result;
}

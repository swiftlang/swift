//===--- CSDiag.cpp - Constraint Diagnostics ------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements diagnostics for the type checker.
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
#include "MiscDiagnostics.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/TypeWalker.h"
#include "swift/AST/TypeMatcher.h"
#include "swift/Basic/StringExtras.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace constraints;

static bool isUnresolvedOrTypeVarType(Type ty) {
  return ty->is<TypeVariableType>() || ty->is<UnresolvedType>();
}

/// Given a subpath of an old locator, compute its summary flags.
static unsigned recomputeSummaryFlags(ConstraintLocator *oldLocator,
                                      ArrayRef<LocatorPathElt> path) {
  if (oldLocator->getSummaryFlags() != 0)
    return ConstraintLocator::getSummaryFlagsForPath(path);
  return 0;
}

ConstraintLocator *
constraints::simplifyLocator(ConstraintSystem &cs, ConstraintLocator *locator,
                             SourceRange &range,
                             ConstraintLocator **targetLocator) {
  // Clear out the target locator result.
  if (targetLocator)
    *targetLocator = nullptr;

  // The path to be tacked on to the target locator to identify the specific
  // target.
  Expr *targetAnchor;
  SmallVector<LocatorPathElt, 4> targetPath;

  auto path = locator->getPath();
  auto anchor = locator->getAnchor();
  simplifyLocator(anchor, path, targetAnchor, targetPath, range);


  // If we have a target anchor, build and simplify the target locator.
  if (targetLocator && targetAnchor) {
    SourceRange targetRange;
    unsigned targetFlags = recomputeSummaryFlags(locator, targetPath);
    auto loc = cs.getConstraintLocator(targetAnchor, targetPath, targetFlags);
    *targetLocator = simplifyLocator(cs, loc, targetRange);
  }

  // If we didn't simplify anything, just return the input.
  if (anchor == locator->getAnchor() &&
      path.size() == locator->getPath().size()) {
    return locator;
  }

  // Recompute the summary flags if we had any to begin with.  This is
  // necessary because we might remove e.g. tuple elements from the path.
  unsigned summaryFlags = recomputeSummaryFlags(locator, path);
  return cs.getConstraintLocator(anchor, path, summaryFlags);
}

void constraints::simplifyLocator(Expr *&anchor,
                                  ArrayRef<LocatorPathElt> &path,
                                  Expr *&targetAnchor,
                                  SmallVectorImpl<LocatorPathElt> &targetPath,
                                  SourceRange &range) {
  range = SourceRange();
  targetAnchor = nullptr;

  while (!path.empty()) {
    switch (path[0].getKind()) {
    case ConstraintLocator::ApplyArgument:
      // Extract application argument.
      if (auto applyExpr = dyn_cast<ApplyExpr>(anchor)) {
        // The target anchor is the function being called.
        targetAnchor = applyExpr->getFn();
        targetPath.push_back(path[0]);

        anchor = applyExpr->getArg();
        path = path.slice(1);
        continue;
      }

      if (auto objectLiteralExpr = dyn_cast<ObjectLiteralExpr>(anchor)) {
        targetAnchor = nullptr;
        targetPath.clear();

        anchor = objectLiteralExpr->getArg();
        path = path.slice(1);
        continue;
      }
      break;

    case ConstraintLocator::ApplyFunction:
      // Extract application function.
      if (auto applyExpr = dyn_cast<ApplyExpr>(anchor)) {
        // No additional target locator information.
        targetAnchor = nullptr;
        targetPath.clear();

        anchor = applyExpr->getFn();
        path = path.slice(1);
        continue;
      }

      // The unresolved member itself is the function.
      if (auto unresolvedMember = dyn_cast<UnresolvedMemberExpr>(anchor)) {
        if (unresolvedMember->getArgument()) {
          // No additional target locator information.
          targetAnchor = nullptr;
          targetPath.clear();

          anchor = unresolvedMember;
          path = path.slice(1);
        }
        continue;
      }

      break;

    case ConstraintLocator::Load:
    case ConstraintLocator::RvalueAdjustment:
    case ConstraintLocator::ScalarToTuple:
    case ConstraintLocator::UnresolvedMember:
      // Loads, rvalue adjustment, and scalar-to-tuple conversions are implicit.
      path = path.slice(1);
      continue;

    case ConstraintLocator::NamedTupleElement:
    case ConstraintLocator::TupleElement:
      // Extract tuple element.
      if (auto tupleExpr = dyn_cast<TupleExpr>(anchor)) {
        unsigned index = path[0].getValue();
        if (index < tupleExpr->getNumElements()) {
          // Append this extraction to the target locator path.
          if (targetAnchor) {
            targetPath.push_back(path[0]);
          }

          anchor = tupleExpr->getElement(index);
          path = path.slice(1);
          continue;
        }
      }
      break;

    case ConstraintLocator::ApplyArgToParam:
      // Extract tuple element.
      if (auto tupleExpr = dyn_cast<TupleExpr>(anchor)) {
        unsigned index = path[0].getValue();
        if (index < tupleExpr->getNumElements()) {
          // Append this extraction to the target locator path.
          if (targetAnchor) {
            targetPath.push_back(path[0]);
          }

          anchor = tupleExpr->getElement(index);
          path = path.slice(1);
          continue;
        }
      }

      // Extract subexpression in parentheses.
      if (auto parenExpr = dyn_cast<ParenExpr>(anchor)) {
        assert(path[0].getValue() == 0);

        // Append this extraction to the target locator path.
        if (targetAnchor) {
          targetPath.push_back(path[0]);
        }

        anchor = parenExpr->getSubExpr();
        path = path.slice(1);
      }
      break;

    case ConstraintLocator::ConstructorMember:
      if (auto typeExpr = dyn_cast<TypeExpr>(anchor)) {
        // This is really an implicit 'init' MemberRef, so point at the base,
        // i.e. the TypeExpr.
        targetAnchor = nullptr;
        targetPath.clear();
        range = SourceRange();
        anchor = typeExpr;
        path = path.slice(1);
        continue;
      }
      SWIFT_FALLTHROUGH;

    case ConstraintLocator::Member:
    case ConstraintLocator::MemberRefBase:
      if (auto UDE = dyn_cast<UnresolvedDotExpr>(anchor)) {
        // No additional target locator information.
        targetAnchor = nullptr;
        targetPath.clear();
        
        range = UDE->getNameLoc().getSourceRange();
        anchor = UDE->getBase();
        path = path.slice(1);
        continue;
      }
      break;

    case ConstraintLocator::InterpolationArgument:
      if (auto interp = dyn_cast<InterpolatedStringLiteralExpr>(anchor)) {
        unsigned index = path[0].getValue();
        if (index < interp->getSegments().size()) {
          // No additional target locator information.
          // FIXME: Dig out the constructor we're trying to call?
          targetAnchor = nullptr;
          targetPath.clear();

          anchor = interp->getSegments()[index];
          path = path.slice(1);
          continue;
        }
      }
      break;

    case ConstraintLocator::SubscriptIndex:
      if (auto subscript = dyn_cast<SubscriptExpr>(anchor)) {
        targetAnchor = subscript->getBase();
        targetPath.clear();

        anchor = subscript->getIndex();
        path = path.slice(1);
        continue;
      }
      break;
    case ConstraintLocator::SubscriptMember:
      if (auto subscript = dyn_cast<SubscriptExpr>(anchor)) {
        anchor = subscript->getBase();
        targetAnchor = nullptr;
        targetPath.clear();
        path = path.slice(1);
        continue;
      }
      break;

    case ConstraintLocator::ClosureResult:
      if (auto CE = dyn_cast<ClosureExpr>(anchor)) {
        if (CE->hasSingleExpressionBody()) {
          targetAnchor = nullptr;
          targetPath.clear();
          anchor = CE->getSingleExpressionBody();
          path = path.slice(1);
          continue;
        }
      }
      break;
        
    default:
      // FIXME: Lots of other cases to handle.
      break;
    }

    // If we get here, we couldn't simplify the path further.
    break;
  }
}

/// Simplify the given locator down to a specific anchor expression,
/// if possible.
///
/// \returns the anchor expression if it fully describes the locator, or
/// null otherwise.
static Expr *simplifyLocatorToAnchor(ConstraintSystem &cs,
                                     ConstraintLocator *locator) {
  if (!locator || !locator->getAnchor())
    return nullptr;

  SourceRange range;
  locator = simplifyLocator(cs, locator, range);
  if (!locator->getAnchor() || !locator->getPath().empty())
    return nullptr;

  return locator->getAnchor();
}




/// \brief Determine the number of distinct overload choices in the
/// provided set.
static unsigned countDistinctOverloads(ArrayRef<OverloadChoice> choices) {
  llvm::SmallPtrSet<void *, 4> uniqueChoices;
  unsigned result = 0;
  for (auto choice : choices) {
    if (uniqueChoices.insert(choice.getOpaqueChoiceSimple()).second)
      ++result;
  }
  return result;
}

/// \brief Determine the name of the overload in a set of overload choices.
static DeclName getOverloadChoiceName(ArrayRef<OverloadChoice> choices) {
  for (auto choice : choices) {
    if (choice.isDecl())
      return choice.getDecl()->getFullName();
  }

  return DeclName();
}

static bool diagnoseAmbiguity(ConstraintSystem &cs,
                              ArrayRef<Solution> solutions,
                              Expr *expr) {
  // Produce a diff of the solutions.
  SolutionDiff diff(solutions);

  // Find the locators which have the largest numbers of distinct overloads.
  Optional<unsigned> bestOverload;
  unsigned maxDistinctOverloads = 0;
  unsigned maxDepth = 0;
  unsigned minIndex = std::numeric_limits<unsigned>::max();

  // Get a map of expressions to their depths and post-order traversal indices.
  // Heuristically, all other things being equal, we should complain about the
  // ambiguous expression that (1) has the most overloads, (2) is deepest, or
  // (3) comes earliest in the expression.
  auto depthMap = expr->getDepthMap();
  auto indexMap = expr->getPreorderIndexMap();

  for (unsigned i = 0, n = diff.overloads.size(); i != n; ++i) {
    auto &overload = diff.overloads[i];

    // If we can't resolve the locator to an anchor expression with no path,
    // we can't diagnose this well.
    auto *anchor = simplifyLocatorToAnchor(cs, overload.locator);
    if (!anchor)
      continue;
    auto it = indexMap.find(anchor);
    if (it == indexMap.end())
      continue;
    unsigned index = it->second;
    it = depthMap.find(anchor);
    if (it == depthMap.end())
      continue;
    unsigned depth = it->second;

    // If we don't have a name to hang on to, it'll be hard to diagnose this
    // overload.
    if (!getOverloadChoiceName(overload.choices))
      continue;

    unsigned distinctOverloads = countDistinctOverloads(overload.choices);

    // We need at least two overloads to make this interesting.
    if (distinctOverloads < 2)
      continue;

    // If we have more distinct overload choices for this locator than for
    // prior locators, just keep this locator.
    
    bool better = false;
    if (bestOverload) {
      if (distinctOverloads > maxDistinctOverloads) {
        better = true;
      } else if (distinctOverloads == maxDistinctOverloads) {
        if (depth > maxDepth) {
          better = true;
        } else if (depth == maxDepth) {
          if (index < minIndex) {
            better = true;
          }
        }
      }
    }

    if (!bestOverload || better) {
      bestOverload = i;
      maxDistinctOverloads = distinctOverloads;
      maxDepth = depth;
      minIndex = index;
      continue;
    }

    // We have better results. Ignore this one.
  }

  // FIXME: Should be able to pick the best locator, e.g., based on some
  // depth-first numbering of expressions.
  if (bestOverload) {
    auto &overload = diff.overloads[*bestOverload];
    auto name = getOverloadChoiceName(overload.choices);
    auto anchor = simplifyLocatorToAnchor(cs, overload.locator);

    // Emit the ambiguity diagnostic.
    auto &tc = cs.getTypeChecker();
    tc.diagnose(anchor->getLoc(),
                name.isOperator() ? diag::ambiguous_operator_ref
                                  : diag::ambiguous_decl_ref,
                name);

    // Emit candidates.  Use a SmallPtrSet to make sure only emit a particular
    // candidate once.  FIXME: Why is one candidate getting into the overload
    // set multiple times?
    SmallPtrSet<Decl*, 8> EmittedDecls;
    for (auto choice : overload.choices) {
      switch (choice.getKind()) {
      case OverloadChoiceKind::Decl:
      case OverloadChoiceKind::DeclViaDynamic:
      case OverloadChoiceKind::TypeDecl:
      case OverloadChoiceKind::DeclViaBridge:
      case OverloadChoiceKind::DeclViaUnwrappedOptional:
        // FIXME: show deduced types, etc, etc.
        if (EmittedDecls.insert(choice.getDecl()).second)
          tc.diagnose(choice.getDecl(), diag::found_candidate);
        break;

      case OverloadChoiceKind::BaseType:
      case OverloadChoiceKind::TupleIndex:
        // FIXME: Actually diagnose something here.
        break;
      }
    }

    return true;
  }

  // FIXME: If we inferred different types for literals (for example),
  // could diagnose ambiguity that way as well.

  return false;
}

static std::string getTypeListString(Type type) {
  // Assemble the parameter type list.
  auto tupleType = type->getAs<TupleType>();
  if (!tupleType) {
    if (auto PT = dyn_cast<ParenType>(type.getPointer()))
      type = PT->getUnderlyingType();

    return "(" + type->getString() + ")";
  }

  std::string result = "(";
  for (auto field : tupleType->getElements()) {
    if (result.size() != 1)
      result += ", ";
    if (!field.getName().empty()) {
      result += field.getName().str();
      result += ": ";
    }

    if (!field.isVararg())
      result += field.getType()->getString();
    else {
      result += field.getVarargBaseTy()->getString();
      result += "...";
    }
  }
  result += ")";
  return result;
}


/// If an UnresolvedDotExpr, SubscriptMember, etc has been resolved by the
/// constraint system, return the decl that it references.
static ValueDecl *findResolvedMemberRef(ConstraintLocator *locator,
                                        ConstraintSystem &CS) {
  auto *resolvedOverloadSets = CS.getResolvedOverloadSets();
  if (!resolvedOverloadSets) return nullptr;

  // Search through the resolvedOverloadSets to see if we have a resolution for
  // this member.  This is an O(n) search, but only happens when producing an
  // error diagnostic.
  for (auto resolved = resolvedOverloadSets;
       resolved; resolved = resolved->Previous) {
    if (resolved->Locator != locator) continue;

    // We only handle the simplest decl binding.
    if (resolved->Choice.getKind() != OverloadChoiceKind::Decl)
      return nullptr;
    return resolved->Choice.getDecl();
  }

  return nullptr;
}


/// Given an expression that has a non-lvalue type, dig into it until we find
/// the part of the expression that prevents the entire subexpression from being
/// mutable.  For example, in a sequence like "x.v.v = 42" we want to complain
/// about "x" being a let property if "v.v" are both mutable.
///
/// This returns the base subexpression that looks immutable (or that can't be
/// analyzed any further) along with a decl extracted from it if we could.
///
static std::pair<Expr*, ValueDecl*>
resolveImmutableBase(Expr *expr, ConstraintSystem &CS) {
  expr = expr->getValueProvidingExpr();

  // Provide specific diagnostics for assignment to subscripts whose base expr
  // is known to be an rvalue.
  if (auto *SE = dyn_cast<SubscriptExpr>(expr)) {
    // If we found a decl for the subscript, check to see if it is a set-only
    // subscript decl.
    SubscriptDecl *member = nullptr;
    if (SE->hasDecl())
      member = dyn_cast_or_null<SubscriptDecl>(SE->getDecl().getDecl());
    
    if (!member) {
      auto loc = CS.getConstraintLocator(SE,ConstraintLocator::SubscriptMember);
      member = dyn_cast_or_null<SubscriptDecl>(findResolvedMemberRef(loc, CS));
    }

    // If it isn't settable, return it.
    if (member) {
      if (!member->isSettable() ||
          !member->isSetterAccessibleFrom(CS.DC))
        return { expr, member };
    }

    // If it is settable, then the base must be the problem, recurse.
    return resolveImmutableBase(SE->getBase(), CS);
  }

  // Look through property references.
  if (auto *UDE = dyn_cast<UnresolvedDotExpr>(expr)) {
    // If we found a decl for the UDE, check it.
    auto loc = CS.getConstraintLocator(UDE, ConstraintLocator::Member);
    auto *member = dyn_cast_or_null<VarDecl>(findResolvedMemberRef(loc, CS));

    // If the member isn't settable, then it is the problem: return it.
    if (member) {
      if (!member->isSettable(nullptr) ||
          !member->isSetterAccessibleFrom(CS.DC))
        return { expr, member };
    }

    // If we weren't able to resolve a member or if it is mutable, then the
    // problem must be with the base, recurse.
    return resolveImmutableBase(UDE->getBase(), CS);
  }

  if (auto *MRE = dyn_cast<MemberRefExpr>(expr)) {
    // If the member isn't settable, then it is the problem: return it.
    if (auto member = dyn_cast<AbstractStorageDecl>(MRE->getMember().getDecl()))
      if (!member->isSettable(nullptr) ||
          !member->isSetterAccessibleFrom(CS.DC))
        return { expr, member };

    // If we weren't able to resolve a member or if it is mutable, then the
    // problem must be with the base, recurse.
    return resolveImmutableBase(MRE->getBase(), CS);
  }

  if (auto *DRE = dyn_cast<DeclRefExpr>(expr))
    return { expr, DRE->getDecl() };

  // Look through x!
  if (auto *FVE = dyn_cast<ForceValueExpr>(expr))
    return resolveImmutableBase(FVE->getSubExpr(), CS);
  
  // Look through x?
  if (auto *BOE = dyn_cast<BindOptionalExpr>(expr))
    return resolveImmutableBase(BOE->getSubExpr(), CS);
  
  return { expr, nullptr };
}

static bool isLoadedLValue(Expr *expr) {
  expr = expr->getSemanticsProvidingExpr();
  if (isa<LoadExpr>(expr))
    return true;
  if (auto ifExpr = dyn_cast<IfExpr>(expr))
    return isLoadedLValue(ifExpr->getThenExpr())
        && isLoadedLValue(ifExpr->getElseExpr());
  return false;
}

static void diagnoseSubElementFailure(Expr *destExpr,
                                      SourceLoc loc,
                                      ConstraintSystem &CS,
                                      Diag<StringRef> diagID,
                                      Diag<Type> unknownDiagID) {
  auto &TC = CS.getTypeChecker();
  
  // Walk through the destination expression, resolving what the problem is.  If
  // we find a node in the lvalue path that is problematic, this returns it.
  auto immInfo = resolveImmutableBase(destExpr, CS);

  // Otherwise, we cannot resolve this because the available setter candidates
  // are all mutating and the base must be mutating.  If we dug out a
  // problematic decl, we can produce a nice tailored diagnostic.
  if (auto *VD = dyn_cast_or_null<VarDecl>(immInfo.second)) {
    std::string message = "'";
    message += VD->getName().str().str();
    message += "'";
 
    if (VD->isImplicit())
      message += " is immutable";
    else if (VD->isLet())
      message += " is a 'let' constant";
    else if (!VD->isSettable(CS.DC))
      message += " is a get-only property";
    else if (!VD->isSetterAccessibleFrom(CS.DC))
      message += " setter is inaccessible";
    else {
      message += " is immutable";
    }
    TC.diagnose(loc, diagID, message)
      .highlight(immInfo.first->getSourceRange());
    
    // If this is a simple variable marked with a 'let', emit a note to fixit
    // hint it to 'var'.
    VD->emitLetToVarNoteIfSimple(CS.DC);
    return;
  }

  // If the underlying expression was a read-only subscript, diagnose that.
  if (auto *SD = dyn_cast_or_null<SubscriptDecl>(immInfo.second)) {
    StringRef message;
    if (!SD->isSettable())
      message = "subscript is get-only";
    else if (!SD->isSetterAccessibleFrom(CS.DC))
      message = "subscript setter is inaccessible";
    else
      message = "subscript is immutable";

    TC.diagnose(loc, diagID, message)
      .highlight(immInfo.first->getSourceRange());
    return;
  }

  // If the expression is the result of a call, it is an rvalue, not a mutable
  // lvalue.
  if (auto *AE = dyn_cast<ApplyExpr>(immInfo.first)) {
    // Handle literals, which are a call to the conversion function.
    auto argsTuple =
      dyn_cast<TupleExpr>(AE->getArg()->getSemanticsProvidingExpr());
    if (isa<CallExpr>(AE) && AE->isImplicit() && argsTuple &&
        argsTuple->getNumElements() == 1 &&
        isa<LiteralExpr>(argsTuple->getElement(0)->
                         getSemanticsProvidingExpr())) {
      TC.diagnose(loc, diagID, "literals are not mutable");
      return;
    }

    std::string name = "call";
    if (isa<PrefixUnaryExpr>(AE) || isa<PostfixUnaryExpr>(AE))
      name = "unary operator";
    else if (isa<BinaryExpr>(AE))
      name = "binary operator";
    else if (isa<CallExpr>(AE))
      name = "function call";
    else if (isa<DotSyntaxCallExpr>(AE) || isa<DotSyntaxBaseIgnoredExpr>(AE))
      name = "method call";

    if (auto *DRE =
          dyn_cast<DeclRefExpr>(AE->getFn()->getValueProvidingExpr()))
      name = std::string("'") + DRE->getDecl()->getName().str().str() + "'";

    TC.diagnose(loc, diagID, name + " returns immutable value")
      .highlight(AE->getSourceRange());
    return;
  }
  
  if (auto *ICE = dyn_cast<ImplicitConversionExpr>(immInfo.first))
    if (isa<LoadExpr>(ICE->getSubExpr())) {
      TC.diagnose(loc, diagID, "implicit conversion from '" +
                  ICE->getSubExpr()->getType()->getString() + "' to '" +
                  ICE->getType()->getString() + "' requires a temporary")
        .highlight(ICE->getSourceRange());
      return;
    }

  if (auto IE = dyn_cast<IfExpr>(immInfo.first)) {
    if (isLoadedLValue(IE)) {
      TC.diagnose(loc, diagID,
                  "result of conditional operator '? :' is never mutable")
        .highlight(IE->getQuestionLoc())
        .highlight(IE->getColonLoc());
      return;
    }
  }

  TC.diagnose(loc, unknownDiagID, destExpr->getType())
    .highlight(immInfo.first->getSourceRange());
}

namespace {
  /// Each match in an ApplyExpr is evaluated for how close of a match it is.
  /// The result is captured in this enum value, where the earlier entries are
  /// most specific.
  enum CandidateCloseness {
    CC_ExactMatch,              ///< This is a perfect match for the arguments.
    CC_Unavailable,             ///< Marked unavailable with @available.
    CC_Inaccessible,            ///< Not accessible from the current context.
    CC_NonLValueInOut,          ///< First arg is inout but no lvalue present.
    CC_SelfMismatch,            ///< Self argument mismatches.
    CC_OneArgumentNearMismatch, ///< All arguments except one match, near miss.
    CC_OneArgumentMismatch,     ///< All arguments except one match.
    CC_OneGenericArgumentNearMismatch, ///< All arguments except one match, guessing generic binding, near miss.
    CC_OneGenericArgumentMismatch,     ///< All arguments except one match, guessing generic binding.
    CC_ArgumentNearMismatch,    ///< Argument list mismatch, near miss.
    CC_ArgumentMismatch,        ///< Argument list mismatch.
    CC_GenericNonsubstitutableMismatch, ///< Arguments match each other, but generic binding not substitutable.
    CC_ArgumentLabelMismatch,   ///< Argument label mismatch.
    CC_ArgumentCountMismatch,   ///< This candidate has wrong # arguments.
    CC_GeneralMismatch          ///< Something else is wrong.
  };

  /// This is a candidate for a callee, along with an uncurry level.
  ///
  /// The uncurry level specifies how far much of a curried value has already
  /// been applied.  For example, in a funcdecl of:
  ///     func f(a:Int)(b:Double) -> Int
  /// Uncurry level of 0 indicates that we're looking at the "a" argument, an
  /// uncurry level of 1 indicates that we're looking at the "b" argument.
  ///
  /// entityType specifies a specific type to use for this decl/expr that may be
  /// more resolved than the concrete type.  For example, it may have generic
  /// arguments substituted in.
  ///
  struct UncurriedCandidate {
    PointerUnion<ValueDecl *, Expr*> declOrExpr;
    unsigned level;
    Type entityType;

    UncurriedCandidate(ValueDecl *decl, unsigned level)
      : declOrExpr(decl), level(level), entityType(decl->getType()) {
      // For some reason, subscripts and properties don't include their self
      // type.  Tack it on for consistency with other members.
      if (isa<AbstractStorageDecl>(decl)) {
        if (decl->getDeclContext()->isTypeContext()) {
          auto instanceTy = decl->getDeclContext()->getSelfTypeInContext();
          entityType = FunctionType::get(instanceTy, entityType);
        }
      }
    }
    UncurriedCandidate(Expr *expr)
      : declOrExpr(expr), level(0), entityType(expr->getType()) {
    }
 
    ValueDecl *getDecl() const {
      return declOrExpr.dyn_cast<ValueDecl*>();
    }

    Expr *getExpr() const {
      return declOrExpr.dyn_cast<Expr*>();
    }

    Type getUncurriedType() const {
      // Start with the known type of the decl.
      auto type = entityType;

      // If this is an operator func decl in a type context, the 'self' isn't
      // actually going to be applied.
      if (auto *fd = dyn_cast_or_null<FuncDecl>(getDecl()))
        if (fd->isOperator() && fd->getDeclContext()->isTypeContext()) {
          if (type->is<ErrorType>())
            return Type();
          type = type->castTo<AnyFunctionType>()->getResult();
        }

      for (unsigned i = 0, e = level; i != e; ++i) {
        auto funcTy = type->getAs<AnyFunctionType>();
        if (!funcTy) return Type();
        type = funcTy->getResult();
      }

      return type;
    }
    
    AnyFunctionType *getUncurriedFunctionType() const {
      if (auto type = getUncurriedType())
        return type->getAs<AnyFunctionType>();
      return nullptr;
    }

    /// Given a function candidate with an uncurry level, return the parameter
    /// type at the specified uncurry level.  If there is an error getting to
    /// the specified input, this returns a null Type.
    Type getArgumentType() const {
      if (auto *funcTy = getUncurriedFunctionType())
        return funcTy->getInput();
      return Type();
    }

    /// Given a function candidate with an uncurry level, return the parameter
    /// type at the specified uncurry level.  If there is an error getting to
    /// the specified input, this returns a null Type.
    Type getResultType() const {
      if (auto *funcTy = getUncurriedFunctionType())
        return funcTy->getResult();
      return Type();
    }

    void dump() const {
      if (auto decl = getDecl())
        decl->dumpRef(llvm::errs());
      else
        llvm::errs() << "<<EXPR>>";
      llvm::errs() << " - uncurry level " << level;

      if (auto FT = getUncurriedFunctionType())
        llvm::errs() << " - type: " << Type(FT) << "\n";
      else
        llvm::errs() << " - type <<NONFUNCTION>>: " << entityType << "\n";
    }
  };


  /// This struct represents an analyzed function pointer to determine the
  /// candidates that could be called, or the one concrete decl that will be
  /// called if not ambiguous.
  class CalleeCandidateInfo {
  public:
    ConstraintSystem *const CS;

    /// This is the name of the callee as extracted from the call expression.
    /// This can be empty in cases like calls to closure exprs.
    std::string declName;

    /// True if the call site for this callee syntactically has a trailing
    /// closure specified.
    bool hasTrailingClosure;
    
    /// This is the list of candidates identified.
    SmallVector<UncurriedCandidate, 4> candidates;

    /// This tracks how close the candidates are, after filtering.
    CandidateCloseness closeness = CC_GeneralMismatch;
    
    /// When we have a candidate that differs by a single argument mismatch, we
    /// keep track of which argument passed to the call is failed, and what the
    /// expected type is.  If the candidate set disagrees, or if there is more
    /// than a single argument mismatch, then this is "{ -1, Type() }".
    struct FailedArgumentInfo {
      int argumentNumber = -1;      ///< Arg # at the call site.
      Type parameterType = Type();  ///< Expected type at the decl site.
      DeclContext *declContext = nullptr; ///< Context at the candidate declaration.
      
      bool isValid() const { return argumentNumber != -1; }
      
      bool operator!=(const FailedArgumentInfo &other) {
        if (argumentNumber != other.argumentNumber) return true;
        if (declContext != other.declContext) return true;
        // parameterType can be null, and isEqual doesn't handle this.
        if (!parameterType || !other.parameterType)
          return parameterType.getPointer() != other.parameterType.getPointer();
        return !parameterType->isEqual(other.parameterType);
      }
    };
    FailedArgumentInfo failedArgument = FailedArgumentInfo();
    
    /// Analyze a function expr and break it into a candidate set.  On failure,
    /// this leaves the candidate list empty.
    CalleeCandidateInfo(Expr *Fn, bool hasTrailingClosure,
                        ConstraintSystem *CS)
      : CS(CS), hasTrailingClosure(hasTrailingClosure) {
      collectCalleeCandidates(Fn);
    }

    CalleeCandidateInfo(Type baseType, ArrayRef<OverloadChoice> candidates,
                        bool hasTrailingClosure, ConstraintSystem *CS,
                        bool selfAlreadyApplied = true);

    typedef std::pair<CandidateCloseness, FailedArgumentInfo> ClosenessResultTy;
    typedef const std::function<ClosenessResultTy(UncurriedCandidate)>
    &ClosenessPredicate;

    /// After the candidate list is formed, it can be filtered down to discard
    /// obviously mismatching candidates and compute a "closeness" for the
    /// resultant set.
    ClosenessResultTy
    evaluateCloseness(DeclContext *dc, Type candArgListType,
                      ArrayRef<CallArgParam> actualArgs);
      
    void filterList(ArrayRef<CallArgParam> actualArgs);
    void filterList(Type actualArgsType) {
      return filterList(decomposeArgParamType(actualArgsType));
    }
    void filterList(ClosenessPredicate predicate);
    void filterContextualMemberList(Expr *argExpr);

    bool empty() const { return candidates.empty(); }
    unsigned size() const { return candidates.size(); }
    UncurriedCandidate operator[](unsigned i) const {
      return candidates[i];
    }

    /// Given a set of parameter lists from an overload group, and a list of
    /// arguments, emit a diagnostic indicating any partially matching
    /// overloads.
    void suggestPotentialOverloads(SourceLoc loc, bool isResult = false);


    /// If the candidate set has been narrowed to a single parameter or single
    /// archetype that has argument type errors, diagnose that error and
    /// return true.
    bool diagnoseGenericParameterErrors(Expr *badArgExpr);
    
    /// Emit a diagnostic and return true if this is an error condition we can
    /// handle uniformly.  This should be called after filtering the candidate
    /// list.
    bool diagnoseSimpleErrors(const Expr *E);
    
    void dump() const LLVM_ATTRIBUTE_USED;
    
  private:
    void collectCalleeCandidates(Expr *fnExpr);
  };
}

void CalleeCandidateInfo::dump() const {
  llvm::errs() << "CalleeCandidateInfo for '" << declName << "': closeness="
               << unsigned(closeness) << "\n";
  llvm::errs() << candidates.size() << " candidates:\n";
  for (auto c : candidates) {
    llvm::errs() << "  ";
    c.dump();
  }
}


/// Given a candidate list, this computes the narrowest closeness to the match
/// we're looking for and filters out any worse matches.  The predicate
/// indicates how close a given candidate is to the desired match.
void CalleeCandidateInfo::filterList(ClosenessPredicate predicate) {
  closeness = CC_GeneralMismatch;

  // If we couldn't find anything, give up.
  if (candidates.empty())
    return;

  // Now that we have the candidate list, figure out what the best matches from
  // the candidate list are, and remove all the ones that aren't at that level.
  SmallVector<ClosenessResultTy, 4> closenessList;
  closenessList.reserve(candidates.size());
  for (auto decl : candidates) {
    auto declCloseness = predicate(decl);
    
    // If we have a decl identified, refine the match.
    if (auto VD = decl.getDecl()) {
      // If this candidate otherwise matched but was marked unavailable, then
      // treat it as unavailable, which is a very close failure.
      if (declCloseness.first == CC_ExactMatch &&
          VD->getAttrs().isUnavailable(CS->getASTContext()) &&
          !CS->TC.getLangOpts().DisableAvailabilityChecking)
        declCloseness.first = CC_Unavailable;
      
      // Likewise, if the candidate is inaccessible from the scope it is being
      // accessed from, mark it as inaccessible or a general mismatch.
      if (VD->hasAccessibility() &&
          !VD->isAccessibleFrom(CS->DC)) {
        // If this was an exact match, downgrade it to inaccessible, so that
        // accessible decls that are also an exact match will take precedence.
        // Otherwise consider it to be a general mismatch so we only list it in
        // an overload set as a last resort.
        if (declCloseness.first == CC_ExactMatch)
          declCloseness.first = CC_Inaccessible;
        else
          declCloseness.first = CC_GeneralMismatch;
      }
    }
    
    closenessList.push_back(declCloseness);
    closeness = std::min(closeness, closenessList.back().first);
  }

  // Now that we know the minimum closeness, remove all the elements that aren't
  // as close.  Keep track of argument failure information if the entire
  // matching candidate set agrees.
  unsigned NextElt = 0;
  for (unsigned i = 0, e = candidates.size(); i != e; ++i) {
    // If this decl in the result list isn't a close match, ignore it.
    if (closeness != closenessList[i].first)
      continue;
    
    // Otherwise, preserve it.
    candidates[NextElt++] = candidates[i];
    
    if (NextElt == 1)
      failedArgument = closenessList[i].second;
    else if (failedArgument != closenessList[i].second)
      failedArgument = FailedArgumentInfo();
  }

  candidates.erase(candidates.begin()+NextElt, candidates.end());
}



/// Given an incompatible argument being passed to a parameter, decide whether
/// it is a "near" miss or not.  We consider something to be a near miss if it
/// is due to a common sort of problem (e.g. function type passed to wrong
/// function type, or T? passed to something expecting T) where a far miss is a
/// completely incompatible type (Int where Float is expected).  The notion of a
/// near miss is used to refine overload sets to a smaller candidate set that is
/// the most relevant options.
static bool argumentMismatchIsNearMiss(Type argType, Type paramType) {
  // If T? was passed to something expecting T, then it is a near miss.
  if (auto argOptType = argType->getOptionalObjectType())
    if (argOptType->isEqual(paramType))
      return true;
  
  // If these are both function types, then they are near misses.  We consider
  // incompatible function types to be near so that functions and non-function
  // types are considered far.
  if (argType->is<AnyFunctionType>() && paramType->is<AnyFunctionType>())
    return true;
  
  // Otherwise, this is some other sort of incompatibility.
  return false;
}

/// Given a parameter type that may contain generic type params and an actual
/// argument type, decide whether the param and actual arg have the same shape
/// and equal fixed type portions, and return by reference each archetype and
/// the matching portion of the actual arg type where that archetype appears.
static bool findGenericSubstitutions(DeclContext *dc, Type paramType,
                                     Type actualArgType,
                                     TypeSubstitutionMap &archetypesMap) {
  // Type visitor doesn't handle unresolved types.
  if (paramType->is<UnresolvedType>() ||
      actualArgType->is<UnresolvedType>())
    return false;
  
  class GenericVisitor : public TypeMatcher<GenericVisitor> {
    DeclContext *dc;
    TypeSubstitutionMap &archetypesMap;

  public:
    GenericVisitor(DeclContext *dc, TypeSubstitutionMap &archetypesMap)
      : dc(dc), archetypesMap(archetypesMap) {}
    
    bool mismatch(TypeBase *paramType, TypeBase *argType) {
      return paramType->isEqual(argType);
    }
    
    bool mismatch(SubstitutableType *paramType, TypeBase *argType) {
      Type type = paramType;
      if (dc && type->isTypeParameter())
        type = ArchetypeBuilder::mapTypeIntoContext(dc, paramType);
      
      if (auto archetype = type->getAs<ArchetypeType>()) {
        auto existing = archetypesMap[archetype];
        if (existing)
          return existing->isEqual(argType);
        archetypesMap[archetype] = argType;
        return true;
      }
      return false;
    }
  };
  
  // If paramType contains any substitutions already, find them and add them
  // to our list before matching the two types to find more.
  paramType.findIf([&](Type type) -> bool {
    if (auto substitution = dyn_cast<SubstitutedType>(type.getPointer())) {
      Type original = substitution->getOriginal();
      if (dc && original->isTypeParameter())
        original = ArchetypeBuilder::mapTypeIntoContext(dc, original);
      
      Type replacement = substitution->getReplacementType();
      // If the replacement is itself an archetype, then the constraint
      // system was asserting equivalencies between different levels of
      // generics, rather than binding a generic to a concrete type (and we
      // don't/won't have a concrete type). In which case, it is the
      // replacement we are interested in, since it is the one in our current
      // context. That generic type should equal itself.
      if (auto ourGeneric = replacement->getAs<ArchetypeType>())
        archetypesMap[ourGeneric] = replacement;
      else if (auto archetype = original->getAs<ArchetypeType>())
        archetypesMap[archetype] = replacement;
    }
    return false;
  });
  
  GenericVisitor visitor(dc, archetypesMap);
  return visitor.match(paramType, actualArgType);
}

/// Determine how close an argument list is to an already decomposed argument
/// list.  If the closeness is a miss by a single argument, then this returns
/// information about that failure.
CalleeCandidateInfo::ClosenessResultTy
CalleeCandidateInfo::evaluateCloseness(DeclContext *dc, Type candArgListType,
                                       ArrayRef<CallArgParam> actualArgs) {
  auto candArgs = decomposeArgParamType(candArgListType);

  struct OurListener : public MatchCallArgumentListener {
    CandidateCloseness result = CC_ExactMatch;
  public:
    CandidateCloseness getResult() const {
      return result;
    }
    void extraArgument(unsigned argIdx) override {
      result = CC_ArgumentCountMismatch;
    }
    void missingArgument(unsigned paramIdx) override {
      result = CC_ArgumentCountMismatch;
    }
    void outOfOrderArgument(unsigned argIdx, unsigned prevArgIdx) override {
      result = CC_ArgumentLabelMismatch;
    }
    bool relabelArguments(ArrayRef<Identifier> newNames) override {
      result = CC_ArgumentLabelMismatch;
      return true;
    }
  } listener;
  
  // Use matchCallArguments to determine how close the argument list is (in
  // shape) to the specified candidates parameters.  This ignores the concrete
  // types of the arguments, looking only at the argument labels etc.
  SmallVector<ParamBinding, 4> paramBindings;
  if (matchCallArguments(actualArgs, candArgs, hasTrailingClosure,
                         /*allowFixes:*/ true,
                         listener, paramBindings))
    // On error, get our closeness from whatever problem the listener saw.
    return { listener.getResult(), {}};

  // If we found a mapping, check to see if the matched up arguments agree in
  // their type and count the number of mismatched arguments.
  unsigned mismatchingArgs = 0;

  // Known mapping of archetypes in all arguments so far. An archetype may map
  // to another archetype if the constraint system substituted one for another.
  TypeSubstitutionMap allGenericSubstitutions;

  // Number of args of one generic archetype which are mismatched because
  // isSubstitutableFor() has failed. If all mismatches are of this type, we'll
  // return a different closeness for better diagnoses.
  Type nonSubstitutableArchetype = nullptr;
  unsigned nonSubstitutableArgs = 0;
  
  // The type of failure is that multiple occurrences of the same generic are
  // being passed arguments with different concrete types.
  bool genericWithDifferingConcreteTypes = false;
  
  // We classify an argument mismatch as being a "near" miss if it is a very
  // likely match due to a common sort of problem (e.g. wrong flags on a
  // function type, optional where none was expected, etc).  This allows us to
  // heuristically filter large overload sets better.
  bool mismatchesAreNearMisses = true;

  CalleeCandidateInfo::FailedArgumentInfo failureInfo;
  
  for (unsigned i = 0, e = paramBindings.size(); i != e; ++i) {
    // Bindings specify the arguments that source the parameter.  The only case
    // this returns a non-singular value is when there are varargs in play.
    auto &bindings = paramBindings[i];
    auto paramType = candArgs[i].Ty;
    
    for (auto argNo : bindings) {
      auto argType = actualArgs[argNo].Ty;
      auto rArgType = argType->getRValueType();
      
      // If the argument has an unresolved type, then we're not actually
      // matching against it.
      if (rArgType->is<UnresolvedType>())
        continue;
      
      // FIXME: Right now, a "matching" overload is one with a parameter whose
      // type is identical to the argument type, or substitutable via handling
      // of functions with primary archetypes in one or more parameters.
      // We can still do something more sophisticated with this.
      // FIXME: Use TC.isConvertibleTo?

      TypeSubstitutionMap archetypesMap;
      bool matched;
      if (paramType->hasUnresolvedType() || rArgType->hasTypeVariable())
        matched = false;
      else {
        auto matchType = paramType;
        // If the parameter is an inout type, and we have a proper lvalue, match
        // against the type contained therein.
        if (paramType->is<InOutType>() && argType->is<LValueType>())
          matchType = matchType->getInOutObjectType();
        matched = findGenericSubstitutions(dc, matchType, rArgType,
                                           archetypesMap);
      }
      
      if (matched) {
        for (auto pair : archetypesMap) {
          auto archetype = pair.first->castTo<ArchetypeType>();
          auto substitution = pair.second;
          
          auto existingSubstitution = allGenericSubstitutions[archetype];
          if (!existingSubstitution) {
            // New substitution for this callee.
            allGenericSubstitutions[archetype] = substitution;
            
            // Not yet handling nested archetypes.
            if (!archetype->isPrimary())
              return { CC_ArgumentMismatch, {}};
            
            if (!CS->TC.isSubstitutableFor(substitution, archetype, CS->DC)) {
              // If we have multiple non-substitutable types, this is just a mismatched mess.
              if (!nonSubstitutableArchetype.isNull())
                return { CC_ArgumentMismatch, {}};
              
              if (auto argOptType = argType->getOptionalObjectType())
                mismatchesAreNearMisses &= CS->TC.isSubstitutableFor(argOptType, archetype, CS->DC);
              else
                mismatchesAreNearMisses = false;
              
              nonSubstitutableArchetype = archetype;
              nonSubstitutableArgs = 1;
              matched = false;
            }
          } else {
            // Substitution for the same archetype as in a previous argument.
            bool isNonSubstitutableArchetype = !nonSubstitutableArchetype.isNull() &&
                                               nonSubstitutableArchetype->isEqual(archetype);
            if (substitution->isEqual(existingSubstitution)) {
              if (isNonSubstitutableArchetype) {
                ++nonSubstitutableArgs;
                matched = false;
              }
            } else {
              // If we have only one nonSubstitutableArg so far, then this different
              // type might be the one that we should be substituting for instead.
              // Note that failureInfo is already set correctly for that case.
              if (isNonSubstitutableArchetype && nonSubstitutableArgs == 1 &&
                  CS->TC.isSubstitutableFor(substitution, archetype, CS->DC)) {
                mismatchesAreNearMisses = argumentMismatchIsNearMiss(existingSubstitution, substitution);
                allGenericSubstitutions[archetype] = substitution;
              } else {
                genericWithDifferingConcreteTypes = true;
                matched = false;
              }
            }
          }
        }
      }
      
      if (matched)
        continue;
      
      if (archetypesMap.empty())
        mismatchesAreNearMisses &= argumentMismatchIsNearMiss(argType, paramType);
      
      ++mismatchingArgs;

      failureInfo.argumentNumber = argNo;
      failureInfo.parameterType = paramType;
      if (paramType->hasTypeParameter())
        failureInfo.declContext = dc;
    }
  }
  
  if (mismatchingArgs == 0)
    return { CC_ExactMatch, {}};
  
  // Check to see if the first argument expects an inout argument, but is not
  // an lvalue.
  Type firstArg = actualArgs[0].Ty;
  if (candArgs[0].Ty->is<InOutType>() && !(firstArg->isLValueType() || firstArg->is<InOutType>()))
    return { CC_NonLValueInOut, {}};
  
  // If we have exactly one argument mismatching, classify it specially, so that
  // close matches are prioritized against obviously wrong ones.
  if (mismatchingArgs == 1) {
    CandidateCloseness closeness;
    if (allGenericSubstitutions.empty()) {
      closeness = mismatchesAreNearMisses ? CC_OneArgumentNearMismatch
                                          : CC_OneArgumentMismatch;
    } else {
      // If the failure is that different occurrences of the same generic have
      // different concrete types, substitute in all the concrete types we've found
      // into the failureInfo to improve diagnosis.
      if (genericWithDifferingConcreteTypes) {
        auto newType = failureInfo.parameterType.transform([&](Type type) -> Type {
          if (auto archetype = type->getAs<ArchetypeType>())
            if (auto replacement = allGenericSubstitutions[archetype])
              return replacement;
          return type;
        });
        failureInfo.parameterType = newType;
      }
      
      closeness = mismatchesAreNearMisses ? CC_OneGenericArgumentNearMismatch
                                          : CC_OneGenericArgumentMismatch;
    }
    // Return information about the single failing argument.
    return { closeness, failureInfo };
  }
    
  if (nonSubstitutableArgs == mismatchingArgs)
    return { CC_GenericNonsubstitutableMismatch, failureInfo };
  
  auto closeness = mismatchesAreNearMisses ? CC_ArgumentNearMismatch
                                           : CC_ArgumentMismatch;
  return { closeness, {}};
}

void CalleeCandidateInfo::collectCalleeCandidates(Expr *fn) {
  fn = fn->getValueProvidingExpr();

  // Treat a call to a load of a variable as a call to that variable, it is just
  // the lvalue'ness being removed.
  if (auto load = dyn_cast<LoadExpr>(fn)) {
    if (isa<DeclRefExpr>(load->getSubExpr()))
      return collectCalleeCandidates(load->getSubExpr());
  }

  if (auto declRefExpr = dyn_cast<DeclRefExpr>(fn)) {
    candidates.push_back({ declRefExpr->getDecl(), 0 });
    declName = declRefExpr->getDecl()->getNameStr().str();
    return;
  }

  if (auto declRefExpr = dyn_cast<OtherConstructorDeclRefExpr>(fn)) {
    auto decl = declRefExpr->getDecl();
    candidates.push_back({ decl, 0 });
    
    if (auto fTy = decl->getType()->getAs<AnyFunctionType>())
      declName = fTy->getInput()->getRValueInstanceType()->getString()+".init";
    else
      declName = "init";
    return;
  }

  if (auto overloadedDRE = dyn_cast<OverloadedDeclRefExpr>(fn)) {
    for (auto cand : overloadedDRE->getDecls()) {
      candidates.push_back({ cand, 0 });
    }

    if (!candidates.empty())
      declName = candidates[0].getDecl()->getNameStr().str();
    return;
  }

  if (auto TE = dyn_cast<TypeExpr>(fn)) {
    // It's always a metatype type, so use the instance type name.
    auto instanceType = TE->getInstanceType();
    
    // TODO: figure out right value for isKnownPrivate
    if (!instanceType->getAs<TupleType>()) {
      auto ctors = CS->TC.lookupConstructors(CS->DC, instanceType,
                                       NameLookupFlags::IgnoreAccessibility);
      for (auto ctor : ctors)
        if (ctor->hasType())
          candidates.push_back({ ctor, 1 });
    }

    declName = instanceType->getString();
    return;
  }

  if (auto *DSBI = dyn_cast<DotSyntaxBaseIgnoredExpr>(fn)) {
    collectCalleeCandidates(DSBI->getRHS());
    return;
  }

  if (auto AE = dyn_cast<ApplyExpr>(fn)) {
    collectCalleeCandidates(AE->getFn());

    // If this is a DotSyntaxCallExpr, then the callee is a method, and the
    // argument list of this apply is the base being applied to the method.
    // If we have a type for that, capture it so that we can calculate a
    // substituted type, which resolves many generic arguments.
    Type baseType;
    if (isa<SelfApplyExpr>(AE) &&
        !isUnresolvedOrTypeVarType(AE->getArg()->getType()))
      baseType = AE->getArg()->getType()->getLValueOrInOutObjectType();

    // If we found a candidate list with a recursive walk, try adjust the curry
    // level for the applied subexpression in this call.
    if (!candidates.empty()) {
      for (auto &C : candidates) {
        C.level += 1;

        // Compute a new substituted type if we have a base type to apply.
        if (baseType && C.level == 1 && C.getDecl())
          C.entityType = baseType->getTypeOfMember(CS->DC->getParentModule(),
                                                   C.getDecl(), nullptr);
      }

      return;
    }
  }

  if (auto *OVE = dyn_cast<OpenExistentialExpr>(fn)) {
    collectCalleeCandidates(OVE->getSubExpr());
    return;
  }

  if (auto *CFCE = dyn_cast<CovariantFunctionConversionExpr>(fn)) {
    collectCalleeCandidates(CFCE->getSubExpr());
    return;
  }


  // Otherwise, we couldn't tell structurally what is going on here, so try to
  // dig something out of the constraint system.
  unsigned uncurryLevel = 0;

  // The candidate list of an unresolved_dot_expr is the candidate list of the
  // base uncurried by one level, and we refer to the name of the member, not to
  // the name of any base.
  if (auto UDE = dyn_cast<UnresolvedDotExpr>(fn)) {
    declName = UDE->getName().getBaseName().str().str();
    uncurryLevel = 1;

    // If we actually resolved the member to use, return it.
    auto loc = CS->getConstraintLocator(UDE, ConstraintLocator::Member);
    if (auto *member = findResolvedMemberRef(loc, *CS)) {
      candidates.push_back({ member, uncurryLevel });
      return;
    }

    // If we resolved the constructor member, return it.
    auto ctorLoc = CS->getConstraintLocator(
                     UDE,
                     ConstraintLocator::ConstructorMember);
    if (auto *member = findResolvedMemberRef(ctorLoc, *CS)) {
      candidates.push_back({ member, uncurryLevel });
      return;
    }

    // If we have useful information about the type we're
    // initializing, provide it.
    if (UDE->getName().getBaseName() == CS->TC.Context.Id_init) {
      auto selfTy = UDE->getBase()->getType()->getLValueOrInOutObjectType();
      if (!selfTy->hasTypeVariable())
        declName = selfTy.getString() + "." + declName;
    }

    // Otherwise, look for a disjunction constraint explaining what the set is.
  }
  
  if (isa<MemberRefExpr>(fn))
    uncurryLevel = 1;

  // Scan to see if we have a disjunction constraint for this callee.
  for (auto &constraint : CS->getConstraints()) {
    if (constraint.getKind() != ConstraintKind::Disjunction) continue;
    
    auto locator = constraint.getLocator();
    if (!locator || locator->getAnchor() != fn) continue;
    
    for (auto *bindOverload : constraint.getNestedConstraints()) {
      if (bindOverload->getKind() != ConstraintKind::BindOverload)
        continue;
      auto c = bindOverload->getOverloadChoice();
      if (c.isDecl())
        candidates.push_back({ c.getDecl(), uncurryLevel });
    }

    // If we found some candidates, then we're done.
    if (candidates.empty()) continue;
    
    if (declName.empty())
      declName = candidates[0].getDecl()->getNameStr().str();
    return;
  }
  
  // Otherwise, just add the expression as a candidate.
  candidates.push_back(fn);
}

/// After the candidate list is formed, it can be filtered down to discard
/// obviously mismatching candidates and compute a "closeness" for the
/// resultant set.
void CalleeCandidateInfo::filterList(ArrayRef<CallArgParam> actualArgs) {
  // Now that we have the candidate list, figure out what the best matches from
  // the candidate list are, and remove all the ones that aren't at that level.
  filterList([&](UncurriedCandidate candidate) -> ClosenessResultTy {
    auto inputType = candidate.getArgumentType();
    // If this isn't a function or isn't valid at this uncurry level, treat it
    // as a general mismatch.
    if (!inputType) return { CC_GeneralMismatch, {}};
    Decl *decl = candidate.getDecl();
    return evaluateCloseness(decl ? decl->getInnermostDeclContext() : nullptr,
                             inputType, actualArgs);
  });
}

void CalleeCandidateInfo::filterContextualMemberList(Expr *argExpr) {
  auto URT = CS->getASTContext().TheUnresolvedType;

  // If the argument is not present then we expect members without arguments.
  if (!argExpr) {
    return filterList([&](UncurriedCandidate candidate) -> ClosenessResultTy {
      auto inputType = candidate.getArgumentType();
      // If this candidate has no arguments, then we're a match.
      if (!inputType) return { CC_ExactMatch, {}};
      
      // Otherwise, if this is a function candidate with an argument, we
      // mismatch argument count.
      return { CC_ArgumentCountMismatch, {}};
    });
  }
  
  // Build an argument list type to filter against based on the expression we
  // have.  This really just provides us a structure to match against.
  // Normally, an argument list is a TupleExpr or a ParenExpr, though sometimes
  // the ParenExpr goes missing.
  auto *argTuple = dyn_cast<TupleExpr>(argExpr);
  if (!argTuple) {
    // If we have a single argument, look through the paren expr.
    if (auto *PE = dyn_cast<ParenExpr>(argExpr))
      argExpr = PE->getSubExpr();
    
    Type argType = URT;
    // If the argument has an & specified, then we expect an lvalue.
    if (isa<InOutExpr>(argExpr))
      argType = LValueType::get(argType);
    
    CallArgParam param;
    param.Ty = argType;
    return filterList(param);
  }
  
  // If we have a tuple expression, form a tuple type.
  SmallVector<CallArgParam, 4> ArgElts;
  for (unsigned i = 0, e = argTuple->getNumElements(); i != e; ++i) {
    // If the argument has an & specified, then we expect an lvalue.
    Type argType = URT;
    if (isa<InOutExpr>(argTuple->getElement(i)))
      argType = LValueType::get(argType);

    CallArgParam param;
    param.Ty = argType;
    param.Label = argTuple->getElementName(i);
    ArgElts.push_back(param);
  }

  return filterList(ArgElts);
}

CalleeCandidateInfo::CalleeCandidateInfo(Type baseType,
                                         ArrayRef<OverloadChoice> overloads,
                                         bool hasTrailingClosure,
                                         ConstraintSystem *CS,
                                         bool selfAlreadyApplied)
  : CS(CS), hasTrailingClosure(hasTrailingClosure) {

  // If we have a useful base type for the candidate set, we'll want to
  // substitute it into each member.  If not, ignore it.
  if (baseType && isUnresolvedOrTypeVarType(baseType))
    baseType = Type();

  for (auto cand : overloads) {
    if (!cand.isDecl()) continue;

    auto decl = cand.getDecl();
    
    // If this is a method or enum case member (not a var or subscript), then
    // the uncurry level is 1 if self has already been applied.
    unsigned uncurryLevel = 0;
    if (decl->getDeclContext()->isTypeContext() &&
        selfAlreadyApplied)
      uncurryLevel = 1;
    
    candidates.push_back({ decl, uncurryLevel });

    // If we have a base type for this member, try to perform substitutions into
    // it to get a simpler and more concrete type.
    //
    if (baseType) {
      auto substType = baseType;
      // If this is a DeclViaUnwrappingOptional, then we're actually looking
      // through an optional to get the member, and baseType is an Optional or
      // Metatype<Optional>.
      if (cand.getKind() == OverloadChoiceKind::DeclViaUnwrappedOptional) {
        bool isMeta = false;
        if (auto MTT = substType->getAs<MetatypeType>()) {
          isMeta = true;
          substType = MTT->getInstanceType();
        }

        // Look through optional or IUO to get the underlying type the decl was
        // found in.
        substType = substType->getAnyOptionalObjectType();
        if (isMeta && substType)
          substType = MetatypeType::get(substType);
      } else if (cand.getKind() != OverloadChoiceKind::Decl) {
        // Otherwise, if it is a remapping we can't handle, don't try to compute
        // a substitution.
        substType = Type();
      }

      if (substType && selfAlreadyApplied)
        substType = substType->getTypeOfMember(CS->DC->getParentModule(),
                                               decl, nullptr);
      if (substType)
        candidates.back().entityType = substType;
    }
  }

  if (!candidates.empty())
    declName = candidates[0].getDecl()->getNameStr().str();
}


/// Given a set of parameter lists from an overload group, and a list of
/// arguments, emit a diagnostic indicating any partially matching overloads.
void CalleeCandidateInfo::
suggestPotentialOverloads(SourceLoc loc, bool isResult) {
  std::string suggestionText = "";
  std::set<std::string> dupes;
  
  // FIXME2: For (T,T) & (Self, Self), emit this as two candidates, one using
  // the LHS and one using the RHS type for T's.
  for (auto cand : candidates) {
    Type type;

    if (auto *SD = dyn_cast_or_null<SubscriptDecl>(cand.getDecl())) {
      type = isResult ? SD->getElementType() : SD->getIndicesType();
    } else {
      type = isResult ? cand.getResultType() : cand.getArgumentType();
    }
    
    if (type.isNull())
      continue;
    
    // If we've already seen this (e.g. decls overridden on the result type),
    // ignore this one.
    auto name = isResult ? type->getString() : getTypeListString(type);
    if (!dupes.insert(name).second)
      continue;
    
    if (!suggestionText.empty())
      suggestionText += ", ";
    suggestionText += name;
  }
  
  if (suggestionText.empty())
    return;
  
  if (dupes.size() == 1) {
    CS->TC.diagnose(loc, diag::suggest_expected_match, isResult,
                    suggestionText);
  } else {
    CS->TC.diagnose(loc, diag::suggest_partial_overloads, isResult, declName,
                    suggestionText);
  }
}

/// If the candidate set has been narrowed to a single parameter or single
/// archetype that has argument type errors, diagnose that error and
/// return true.
bool CalleeCandidateInfo::diagnoseGenericParameterErrors(Expr *badArgExpr) {
  Type argType = badArgExpr->getType();
  
  // FIXME: For protocol argument types, could add specific error
  // similar to could_not_use_member_on_existential.
  if (argType->hasTypeVariable() || argType->is<ProtocolType>() ||
      argType->is<ProtocolCompositionType>())
    return false;
  
  bool foundFailure = false;
  TypeSubstitutionMap archetypesMap;
  
  if (!findGenericSubstitutions(failedArgument.declContext,
                                failedArgument.parameterType,
                                argType, archetypesMap))
    return false;

  for (auto pair : archetypesMap) {
    auto archetype = pair.first->castTo<ArchetypeType>();
    auto substitution = pair.second;
    
    // FIXME: Add specific error for not subclass, if the archetype has a superclass?

    // Check for optional near miss.
    if (auto argOptType = substitution->getOptionalObjectType()) {
      if (CS->TC.isSubstitutableFor(argOptType, archetype, CS->DC)) {
        CS->TC.diagnose(badArgExpr->getLoc(), diag::missing_unwrap_optional, argType);
        foundFailure = true;
        continue;
      }
    }
    
    for (auto proto : archetype->getConformsTo()) {
      if (!CS->TC.conformsToProtocol(substitution, proto, CS->DC, ConformanceCheckOptions(TR_InExpression))) {
        if (substitution->isEqual(argType)) {
          CS->TC.diagnose(badArgExpr->getLoc(), diag::cannot_convert_argument_value_protocol,
                          substitution, proto->getDeclaredType());
        } else {
          CS->TC.diagnose(badArgExpr->getLoc(), diag::cannot_convert_partial_argument_value_protocol,
                          argType, substitution, proto->getDeclaredType());
        }
        foundFailure = true;
      }
    }
  }
  return foundFailure;
}

/// Emit a diagnostic and return true if this is an error condition we can
/// handle uniformly.  This should be called after filtering the candidate
/// list.
bool CalleeCandidateInfo::diagnoseSimpleErrors(const Expr *E) {
  SourceLoc loc = E->getLoc();

  // Handle symbols marked as explicitly unavailable.
  if (closeness == CC_Unavailable) {
    auto decl = candidates[0].getDecl();
    assert(decl && "Only decl-based candidates may be marked unavailable");
    return CS->TC.diagnoseExplicitUnavailability(decl, loc, CS->DC,
                                                 dyn_cast<CallExpr>(E));
  }

  // Handle symbols that are matches, but are not accessible from the current
  // scope.
  if (closeness == CC_Inaccessible) {
    auto decl = candidates[0].getDecl();
    assert(decl && "Only decl-based candidates may be marked inaccessible");
    if (auto *CD = dyn_cast<ConstructorDecl>(decl)) {
      CS->TC.diagnose(loc, diag::init_candidate_inaccessible,
                      CD->getResultType(), decl->getFormalAccess());
     
    } else {
      CS->TC.diagnose(loc, diag::candidate_inaccessible, decl->getName(),
                      decl->getFormalAccess());
    }
    for (auto cand : candidates)
      CS->TC.diagnose(cand.getDecl(),diag::decl_declared_here, decl->getName());
    
    return true;
  }

  return false;
}



/// Flags that can be used to control name lookup.
enum TCCFlags {
  /// Allow the result of the subexpression to be an lvalue.  If this is not
  /// specified, any lvalue will be forced to be loaded into an rvalue.
  TCC_AllowLValue = 0x01,
  
  /// Re-type-check the given subexpression even if the expression has already
  /// been checked already.  The client is asserting that infinite recursion is
  /// not possible because it has relaxed a constraint on the system.
  TCC_ForceRecheck = 0x02,
    
  /// tell typeCheckExpression that it is ok to produce an ambiguous result,
  /// it can just fill in holes with UnresolvedType and we'll deal with it.
  TCC_AllowUnresolvedTypeVariables = 0x04
};

typedef OptionSet<TCCFlags> TCCOptions;

inline TCCOptions operator|(TCCFlags flag1, TCCFlags flag2) {
  return TCCOptions(flag1) | flag2;
}


namespace {
/// If a constraint system fails to converge on a solution for a given
/// expression, this class can produce a reasonable diagnostic for the failure
/// by analyzing the remnants of the failed constraint system. (Specifically,
/// left-over inactive, active and failed constraints.)
/// This class does not tune its diagnostics for a specific expression kind,
/// for that, you'll want to use an instance of the FailureDiagnosis class.
class FailureDiagnosis :public ASTVisitor<FailureDiagnosis, /*exprresult*/bool>{
  friend class ASTVisitor<FailureDiagnosis, /*exprresult*/bool>;
  
  Expr *expr = nullptr;
  ConstraintSystem *const CS;
public:
  FailureDiagnosis(Expr *expr, ConstraintSystem *cs) : expr(expr), CS(cs) {
    assert(expr && CS);
  }

  template<typename ...ArgTypes>
  InFlightDiagnostic diagnose(ArgTypes &&...Args) {
    return CS->TC.diagnose(std::forward<ArgTypes>(Args)...);
  }

  /// Attempt to diagnose a failure without taking into account the specific
  /// kind of expression that could not be type checked.
  bool diagnoseConstraintFailure();

  /// Unless we've already done this, retypecheck the specified child of the
  /// current expression on its own, without including any contextual
  /// constraints or the parent expr nodes.  This is more likely to succeed than
  /// type checking the original expression.
  ///
  /// This mention may only be used on immediate children of the current expr
  /// node, because ClosureExpr parameters need to be treated specially.
  ///
  /// This can return a new expression (for e.g. when a UnresolvedDeclRef gets
  /// resolved) and returns null when the subexpression fails to typecheck.
  ///
  Expr *typeCheckChildIndependently(Expr *subExpr, Type convertType = Type(),
                          ContextualTypePurpose convertTypePurpose = CTP_Unused,
                                    TCCOptions options = TCCOptions(),
                                    ExprTypeCheckListener *listener = nullptr);
  Expr *typeCheckChildIndependently(Expr *subExpr, TCCOptions options) {
    return typeCheckChildIndependently(subExpr, Type(), CTP_Unused, options);
  }

  Type getTypeOfTypeCheckedChildIndependently(Expr *subExpr,
                                            TCCOptions options = TCCOptions()) {
    auto e = typeCheckChildIndependently(subExpr, options);
    return e ? e->getType() : Type();
  }

  /// This is the same as typeCheckChildIndependently, but works on an arbitrary
  /// subexpression of the current node because it handles ClosureExpr parents
  /// of the specified node.
  Expr *typeCheckArbitrarySubExprIndependently(Expr *subExpr,
                                             TCCOptions options = TCCOptions());

  /// Special magic to handle inout exprs and tuples in argument lists.
  Expr *typeCheckArgumentChildIndependently(Expr *argExpr, Type argType,
                                        const CalleeCandidateInfo &candidates,
                                            TCCOptions options = TCCOptions());

  /// Diagnose common failures due to applications of an argument list to an
  /// ApplyExpr or SubscriptExpr.
  bool diagnoseParameterErrors(CalleeCandidateInfo &CCI,
                               Expr *fnExpr, Expr *argExpr);

  /// Attempt to diagnose a specific failure from the info we've collected from
  /// the failed constraint system.
  bool diagnoseExprFailure();

  /// Emit an ambiguity diagnostic about the specified expression.
  void diagnoseAmbiguity(Expr *E);

  /// Attempt to produce a diagnostic for a mismatch between an expression's
  /// type and its assumed contextual type.
  bool diagnoseContextualConversionError();

  /// For an expression being type checked with a CTP_CalleeResult contextual
  /// type, try to diagnose a problem.
  bool diagnoseCalleeResultContextualConversionError();

private:
    
  /// Produce a diagnostic for a general member-lookup failure (irrespective of
  /// the exact expression kind).
  bool diagnoseGeneralMemberFailure(Constraint *constraint);
  
  /// Given a result of name lookup that had no viable results, diagnose the
  /// unviable ones.
  void diagnoseUnviableLookupResults(MemberLookupResult &lookupResults,
                                     Type baseObjTy, Expr *baseExpr,
                                     DeclName memberName, DeclNameLoc nameLoc,
                                     SourceLoc loc);
  
  /// Produce a diagnostic for a general overload resolution failure
  /// (irrespective of the exact expression kind).
  bool diagnoseGeneralOverloadFailure(Constraint *constraint);
  
  /// Produce a diagnostic for a general conversion failure (irrespective of the
  /// exact expression kind).
  bool diagnoseGeneralConversionFailure(Constraint *constraint);
     
  bool visitExpr(Expr *E);
  bool visitIdentityExpr(IdentityExpr *E);
  bool visitTupleExpr(TupleExpr *E);
  
  bool visitUnresolvedMemberExpr(UnresolvedMemberExpr *E);
  bool visitArrayExpr(ArrayExpr *E);
  bool visitDictionaryExpr(DictionaryExpr *E);
  bool visitObjectLiteralExpr(ObjectLiteralExpr *E);

  bool visitForceValueExpr(ForceValueExpr *FVE);
  bool visitBindOptionalExpr(BindOptionalExpr *BOE);

  bool visitSubscriptExpr(SubscriptExpr *SE);
  bool visitApplyExpr(ApplyExpr *AE);
  bool visitAssignExpr(AssignExpr *AE);
  bool visitInOutExpr(InOutExpr *IOE);
  bool visitCoerceExpr(CoerceExpr *CE);
  bool visitIfExpr(IfExpr *IE);
  bool visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E);
  bool visitClosureExpr(ClosureExpr *CE);
};
} // end anonymous namespace.



static bool isMemberConstraint(Constraint *C) {
  return C->getClassification() == ConstraintClassification::Member;
}

static bool isOverloadConstraint(Constraint *C) {
  if (C->getKind() == ConstraintKind::BindOverload)
    return true;

  if (C->getKind() != ConstraintKind::Disjunction)
    return false;
  
  return C->getNestedConstraints().front()->getKind() ==
    ConstraintKind::BindOverload;
}

/// Return true if this constraint is a conversion or requirement between two
/// types.
static bool isConversionConstraint(const Constraint *C) {
  return C->getClassification() == ConstraintClassification::Relational;
}

/// Return true if this member constraint is a low priority for diagnostics, so
/// low that we would only like to issue an error message about it if there is
/// nothing else interesting we can scrape out of the constraint system.
static bool isLowPriorityConstraint(Constraint *C) {
  // If the member constraint is a ".Iterator" lookup to find the iterator
  // type in a foreach loop, or a ".Element" lookup to find its element type,
  // then it is very low priority: We will get a better and more useful
  // diagnostic from the failed conversion to Sequence that will fail as well.
  if (C->getKind() == ConstraintKind::TypeMember) {
    if (auto *loc = C->getLocator())
      for (auto Elt : loc->getPath())
        if (Elt.getKind() == ConstraintLocator::GeneratorElementType ||
            Elt.getKind() == ConstraintLocator::SequenceIteratorProtocol)
          return true;
  }

  return false;
}

/// Attempt to diagnose a failure without taking into account the specific
/// kind of expression that could not be type checked.
bool FailureDiagnosis::diagnoseConstraintFailure() {
  // This is the priority order in which we handle constraints.  Things earlier
  // in the list are considered to have higher specificity (and thus, higher
  // priority) than things lower in the list.
  enum ConstraintRanking {
    CR_MemberConstraint,
    CR_ConversionConstraint,
    CR_OverloadConstraint,
    CR_OtherConstraint
  };

  // Start out by classifying all the constraints.
  typedef std::pair<Constraint*, ConstraintRanking> RCElt;
  std::vector<RCElt> rankedConstraints;

  // This is a predicate that classifies constraints according to our
  // priorities.
  std::function<void (Constraint*)> classifyConstraint = [&](Constraint *C) {
    if (isLowPriorityConstraint(C))
      return rankedConstraints.push_back({C, CR_OtherConstraint});

    if (isMemberConstraint(C))
      return rankedConstraints.push_back({C, CR_MemberConstraint});

    if (isOverloadConstraint(C))
      return rankedConstraints.push_back({C, CR_OverloadConstraint});

    if (isConversionConstraint(C))
      return rankedConstraints.push_back({C, CR_ConversionConstraint});

    // We occasionally end up with disjunction constraints containing an
    // original constraint along with one considered with a fix.  If we find
    // this situation, add the original one to our list for diagnosis.
    if (C->getKind() == ConstraintKind::Disjunction) {
      Constraint *Orig = nullptr;
      bool AllOthersHaveFixes = true;
      for (auto DC : C->getNestedConstraints()) {
        // If this is a constraint inside of the disjunction with a fix, ignore
        // it.
        if (DC->getFix())
          continue;

        // If we already found a candidate without a fix, we can't do this.
        if (Orig) {
          AllOthersHaveFixes = false;
          break;
        }

        // Remember this as the exemplar to use.
        Orig = DC;
      }

      if (Orig && AllOthersHaveFixes)
        return classifyConstraint(Orig);
      
      // If we got all the way down to a truly ambiguous disjunction constraint
      // with a conversion in it, the problem could be that none of the options
      // in the disjunction worked.
      //
      // We don't have a lot of great options here, so (if all else fails),
      // we'll attempt to diagnose the issue as though the first option was the
      // problem.
      rankedConstraints.push_back({
        C->getNestedConstraints()[0],
        CR_OtherConstraint
      });
      return;
    }

    return rankedConstraints.push_back({C, CR_OtherConstraint});
  };
  
  // Look at the failed constraint and the general constraint list.  Processing
  // the failed constraint first slightly biases it in the ranking ahead of
  // other failed constraints at the same level.
  if (CS->failedConstraint)
    classifyConstraint(CS->failedConstraint);
  for (auto &C : CS->getConstraints())
    classifyConstraint(&C);

  // Okay, now that we've classified all the constraints, sort them by their
  // priority and privilege the favored constraints.
  std::stable_sort(rankedConstraints.begin(), rankedConstraints.end(),
                   [&] (RCElt LHS, RCElt RHS) {
    // Rank things by their kind as the highest priority.
    if (LHS.second < RHS.second)
      return true;
    if (LHS.second > RHS.second)
      return false;
    // Next priority is favored constraints.
    if (LHS.first->isFavored() != RHS.first->isFavored())
      return LHS.first->isFavored();
    return false;
  });
 
  // Now that we have a sorted precedence of constraints to diagnose, charge
  // through them.
  for (auto elt : rankedConstraints) {
    auto C = elt.first;
    if (isMemberConstraint(C) && diagnoseGeneralMemberFailure(C))
      return true;

    if (isConversionConstraint(C) && diagnoseGeneralConversionFailure(C))
      return true;

    if (isOverloadConstraint(C) && diagnoseGeneralOverloadFailure(C))
      return true;
    

    // TODO: There can be constraints that aren't handled here!  When this
    // happens, we end up diagnosing them as ambiguities that don't make sense.
    // This isn't as bad as it seems though, because most of these will be
    // diagnosed by expr diagnostics.
  }
  
  // Otherwise, all the constraints look ok, diagnose this as an ambiguous
  // expression.
  return false;
}


bool FailureDiagnosis::diagnoseGeneralMemberFailure(Constraint *constraint) {
  assert(isMemberConstraint(constraint));
  
  auto memberName = constraint->getMember();
  
  // Get the referenced base expression from the failed constraint, along with
  // the SourceRange for the member ref.  In "x.y", this returns the expr for x
  // and the source range for y.
  auto anchor = expr;
  SourceRange memberRange = anchor->getSourceRange();
  auto locator = constraint->getLocator();
  if (locator) {
    locator = simplifyLocator(*CS, locator, memberRange);
    if (locator->getAnchor())
      anchor = locator->getAnchor();
  }
  
  // Check to see if this is a locator referring to something we cannot or do
  // here: in this case, we ignore paths that end on archetypes witnesses, or
  // associated types of the expression.
  if (locator && !locator->getPath().empty()) {
    // TODO: This should only ignore *unresolved* archetypes.  For resolved
    // archetypes
    return false;
  }
  
  // Retypecheck the anchor type, which is the base of the member expression.
  anchor = typeCheckArbitrarySubExprIndependently(anchor, TCC_AllowLValue);
  if (!anchor) return true;
  
  auto baseTy = anchor->getType();
  auto baseObjTy = baseTy->getRValueType();

  // If the base type is an IUO, look through it.  Odds are, the code is not
  // trying to find a member of it.
  if (auto objTy = CS->lookThroughImplicitlyUnwrappedOptionalType(baseObjTy))
    baseTy = baseObjTy = objTy;

  
  if (auto moduleTy = baseObjTy->getAs<ModuleType>()) {
    diagnose(anchor->getLoc(), diag::no_member_of_module,
             moduleTy->getModule()->getName(), memberName)
      .highlight(anchor->getSourceRange()).highlight(memberRange);
    return true;
  }
  
  // If the base of this property access is a function that takes an empty
  // argument list, then the most likely problem is that the user wanted to
  // call the function, e.g. in "a.b.c" where they had to write "a.b().c".
  // Produce a specific diagnostic + fixit for this situation.
  if (auto baseFTy = baseObjTy->getAs<AnyFunctionType>()) {
    if (baseFTy->getInput()->isVoid() &&
        (constraint->getKind() == ConstraintKind::ValueMember ||
         constraint->getKind() == ConstraintKind::UnresolvedValueMember)) {
      SourceLoc insertLoc = anchor->getEndLoc();
    
      if (auto *DRE = dyn_cast<DeclRefExpr>(anchor)) {
        diagnose(anchor->getLoc(), diag::did_not_call_function,
                 DRE->getDecl()->getName())
          .fixItInsertAfter(insertLoc, "()");
        return true;
      }
      
      if (auto *DSCE = dyn_cast<DotSyntaxCallExpr>(anchor))
        if (auto *DRE = dyn_cast<DeclRefExpr>(DSCE->getFn())) {
          diagnose(anchor->getLoc(), diag::did_not_call_method,
                   DRE->getDecl()->getName())
            .fixItInsertAfter(insertLoc, "()");
          return true;
        }
      
      diagnose(anchor->getLoc(), diag::did_not_call_function_value)
      .fixItInsertAfter(insertLoc, "()");
      return true;
    }
  }

  if (baseObjTy->is<TupleType>()) {
    diagnose(anchor->getLoc(), diag::could_not_find_tuple_member,
             baseObjTy, memberName)
      .highlight(anchor->getSourceRange()).highlight(memberRange);
    return true;
  }
  
  MemberLookupResult result =
    CS->performMemberLookup(constraint->getKind(), constraint->getMember(),
                            baseTy, constraint->getLocator(),
                            /*includeInaccessibleMembers*/true);

  switch (result.OverallResult) {
  case MemberLookupResult::Unsolved:      
    // If we couldn't resolve a specific type for the base expression, then we
    // cannot produce a specific diagnostic.
    return false;

  case MemberLookupResult::ErrorAlreadyDiagnosed:
    // If an error was already emitted, then we're done, don't emit anything
    // redundant.
    return true;
      
  case MemberLookupResult::HasResults:
    break;
  }
  
  // If this is a failing lookup, it has no viable candidates here.
  if (result.ViableCandidates.empty()) {
    // Diagnose 'super.init', which can only appear inside another initializer,
    // specially.
    if (result.UnviableCandidates.empty() &&
        memberName.isSimpleName(CS->TC.Context.Id_init) &&
        !baseObjTy->is<MetatypeType>()) {
      if (auto ctorRef = dyn_cast<UnresolvedDotExpr>(expr)) {
        if (isa<SuperRefExpr>(ctorRef->getBase())) {
          diagnose(anchor->getLoc(),
                   diag::super_initializer_not_in_initializer);
          return true;
        }
        
        // Suggest inserting '.dynamicType' to construct another object of the
        // same dynamic type.
        SourceLoc fixItLoc
          = ctorRef->getNameLoc().getBaseNameLoc().getAdvancedLoc(-1);
        
        // Place the '.dynamicType' right before the init.
        diagnose(anchor->getLoc(), diag::init_not_instance_member)
          .fixItInsert(fixItLoc, ".dynamicType");
        return true;
      }
    }

    // FIXME: Dig out the property DeclNameLoc.
    diagnoseUnviableLookupResults(result, baseObjTy, anchor, memberName,
                                  DeclNameLoc(memberRange.Start),
                                  anchor->getLoc());
    return true;
  }
  
  
  bool allUnavailable = !CS->TC.getLangOpts().DisableAvailabilityChecking;
  for (auto match : result.ViableCandidates) {
    if (!match.isDecl() ||
        !match.getDecl()->getAttrs().isUnavailable(CS->getASTContext()))
      allUnavailable = false;
  }
  
  if (allUnavailable) {
    auto firstDecl = result.ViableCandidates[0].getDecl();
    // FIXME: We need the enclosing CallExpr to rewrite the argument labels.
    if (CS->TC.diagnoseExplicitUnavailability(firstDecl, anchor->getLoc(),
                                              CS->DC, /*call*/nullptr))
      return true;
  }
  
  // Otherwise, we don't know why this failed.
  return false;
}


/// Given a result of name lookup that had no viable results, diagnose the
/// unviable ones.
void FailureDiagnosis::
diagnoseUnviableLookupResults(MemberLookupResult &result, Type baseObjTy,
                              Expr *baseExpr,
                              DeclName memberName, DeclNameLoc nameLoc,
                              SourceLoc loc) {
  SourceRange baseRange = baseExpr ? baseExpr->getSourceRange() : SourceRange();
  
  // If we found no results at all, mention that fact.
  if (result.UnviableCandidates.empty()) {
    // TODO: This should handle tuple member lookups, like x.1231 as well.
    if (memberName.isSimpleName("subscript")) {
      diagnose(loc, diag::type_not_subscriptable, baseObjTy)
        .highlight(baseRange);
    } else if (auto MTT = baseObjTy->getAs<MetatypeType>()) {
      diagnose(loc, diag::could_not_find_type_member,
               MTT->getInstanceType(), memberName)
        .highlight(baseRange).highlight(nameLoc.getSourceRange());
    } else {
      diagnose(loc, diag::could_not_find_value_member,
               baseObjTy, memberName)
        .highlight(baseRange).highlight(nameLoc.getSourceRange());
      
      // Check for a few common cases that can cause missing members.
      if (baseObjTy->is<EnumType>() && memberName.isSimpleName("rawValue")) {
        auto loc = baseObjTy->castTo<EnumType>()->getDecl()->getNameLoc();
        if (loc.isValid())
          diagnose(loc, diag::did_you_mean_raw_type);
      }
    }
    return;
  }

  
  // Otherwise, we have at least one (and potentially many) viable candidates
  // sort them out.  If all of the candidates have the same problem (commonly
  // because there is exactly one candidate!) diagnose this.
  bool sameProblem = true;
  auto firstProblem = result.UnviableCandidates[0].second;
  for (auto cand : result.UnviableCandidates)
    sameProblem &= cand.second == firstProblem;
  
  auto instanceTy = baseObjTy;
  if (auto *MTT = instanceTy->getAs<AnyMetatypeType>())
    instanceTy = MTT->getInstanceType();
  
  if (sameProblem) {
    switch (firstProblem) {
    case MemberLookupResult::UR_LabelMismatch:
      break;
    case MemberLookupResult::UR_UnavailableInExistential:
      diagnose(loc, diag::could_not_use_member_on_existential,
               instanceTy, memberName)
        .highlight(baseRange).highlight(nameLoc.getSourceRange());
      return;
    case MemberLookupResult::UR_InstanceMemberOnType:
      // If the base is an implicit self type reference, and we're in a
      // property initializer, then the user wrote something like:
      //
      //   class Foo { let x = 1, y = x }
      //
      // which runs in type context, not instance context.  Produce a tailored
      // diagnostic since this comes up and is otherwise non-obvious what is
      // going on.
      if (baseExpr && baseExpr->isImplicit() && isa<Initializer>(CS->DC) &&
          CS->DC->getParent()->getDeclaredTypeOfContext()->isEqual(instanceTy)){
        CS->TC.diagnose(nameLoc, diag::instance_member_in_initializer,
                        memberName);
        return;
      }
        
      diagnose(loc, diag::could_not_use_instance_member_on_type,
               instanceTy, memberName)
        .highlight(baseRange).highlight(nameLoc.getSourceRange());
      return;
            
    case MemberLookupResult::UR_TypeMemberOnInstance:
      if (instanceTy->isExistentialType() && baseObjTy->is<AnyMetatypeType>()) {
        // If the base of the lookup is an existential metatype, emit an
        // error specific to that
        diagnose(loc, diag::could_not_use_type_member_on_existential,
                 baseObjTy, memberName)
          .highlight(baseRange).highlight(nameLoc.getSourceRange());
      } else {
        // Otherwise the static member lookup was invalid because it was
        // called on an instance
        diagnose(loc, diag::could_not_use_type_member_on_instance,
                 baseObjTy, memberName)
          .highlight(baseRange).highlight(nameLoc.getSourceRange());
      }
      return;
        
    case MemberLookupResult::UR_MutatingMemberOnRValue:
    case MemberLookupResult::UR_MutatingGetterOnRValue: {
      auto diagIDsubelt = diag::cannot_pass_rvalue_mutating_subelement;
      auto diagIDmember = diag::cannot_pass_rvalue_mutating;
      if (firstProblem == MemberLookupResult::UR_MutatingGetterOnRValue) {
        diagIDsubelt = diag::cannot_pass_rvalue_mutating_getter_subelement;
        diagIDmember = diag::cannot_pass_rvalue_mutating_getter;
      }
      assert(baseExpr && "Cannot have a mutation failure without a base");
      diagnoseSubElementFailure(baseExpr, loc, *CS,
                                diagIDsubelt, diagIDmember);
      return;
    }
        
    case MemberLookupResult::UR_Inaccessible: {
      auto decl = result.UnviableCandidates[0].first;
      diagnose(nameLoc, diag::candidate_inaccessible, decl->getName(),
               decl->getFormalAccess());
      for (auto cand : result.UnviableCandidates)
        diagnose(cand.first, diag::decl_declared_here, memberName);
        
      return;
    }
    }
  }

  // FIXME: Emit candidate set....
  
  
  // Otherwise, we don't have a specific issue to diagnose.  Just say the vague
  // 'cannot use' diagnostic.
  if (!baseObjTy->isEqual(instanceTy))
    diagnose(loc, diag::could_not_use_type_member,
             instanceTy, memberName)
    .highlight(baseRange).highlight(nameLoc.getSourceRange());
  else
    diagnose(loc, diag::could_not_use_value_member,
             baseObjTy, memberName)
    .highlight(baseRange).highlight(nameLoc.getSourceRange());
  return;
}

// In the absence of a better conversion constraint failure, point out the
// inability to find an appropriate overload.
bool FailureDiagnosis::diagnoseGeneralOverloadFailure(Constraint *constraint) {
  Constraint *bindOverload = constraint;
  if (constraint->getKind() == ConstraintKind::Disjunction)
    bindOverload = constraint->getNestedConstraints().front();

  auto overloadChoice = bindOverload->getOverloadChoice();
  auto overloadName = overloadChoice.getDecl()->getFullName();

  // Get the referenced expression from the failed constraint.
  auto anchor = expr;
  if (auto locator = bindOverload->getLocator()) {
    anchor = simplifyLocatorToAnchor(*CS, locator);
    if (!anchor)
      return false;
  }

  // The anchor for the constraint is almost always an OverloadedDeclRefExpr or
  // UnresolvedDotExpr.  Look at the parent node in the AST to find the Apply to
  // give a better diagnostic.
  Expr *call = expr->getParentMap()[anchor];
  // We look through some simple things that get in between the overload set
  // and the apply.
  while (call &&
         (isa<IdentityExpr>(call) ||
          isa<TryExpr>(call) || isa<ForceTryExpr>(call))) {
    call = expr->getParentMap()[call];
  }
  
  // FIXME: This is only needed because binops don't respect contextual types.
  if (call && isa<ApplyExpr>(call))
    return false;

  // This happens, for example, with ambiguous OverloadedDeclRefExprs. We should
  // just implement visitOverloadedDeclRefExprs and nuke this.
  
  // If we couldn't resolve an argument, then produce a generic "ambiguity"
  // diagnostic.
  diagnose(anchor->getLoc(), diag::ambiguous_member_overload_set,
           overloadName)
    .highlight(anchor->getSourceRange());

  if (constraint->getKind() == ConstraintKind::Disjunction) {
    for (auto elt : constraint->getNestedConstraints()) {
      if (elt->getKind() != ConstraintKind::BindOverload) continue;
      auto candidate = elt->getOverloadChoice().getDecl();
      diagnose(candidate, diag::found_candidate);
    }
  }

  return true;
}

bool FailureDiagnosis::diagnoseGeneralConversionFailure(Constraint *constraint){
  auto anchor = expr;
  bool resolvedAnchorToExpr = false;
  
  if (auto locator = constraint->getLocator()) {
    anchor = simplifyLocatorToAnchor(*CS, locator);
    if (anchor)
      resolvedAnchorToExpr = true;
    else
      anchor = locator->getAnchor();    
  }

  Type fromType = CS->simplifyType(constraint->getFirstType());
  
  if (fromType->hasTypeVariable() && resolvedAnchorToExpr) {
    TCCOptions options;
    
    // If we know we're removing a contextual constraint, then we can force a
    // type check of the subexpr because we know we're eliminating that
    // constraint.
    if (CS->getContextualTypePurpose() != CTP_Unused)
      options |= TCC_ForceRecheck;
      
    auto sub = typeCheckArbitrarySubExprIndependently(anchor, options);
    if (!sub) return true;
    fromType = sub->getType();
  }

  fromType = fromType->getRValueType();
  auto toType = CS->simplifyType(constraint->getSecondType());
  
  // Try to simplify irrelevant details of function types.  For example, if
  // someone passes a "() -> Float" function to a "() throws -> Int"
  // parameter, then uttering the "throws" may confuse them into thinking that
  // that is the problem, even though there is a clear subtype relation.
  if (auto srcFT = fromType->getAs<FunctionType>())
    if (auto destFT = toType->getAs<FunctionType>()) {
      auto destExtInfo = destFT->getExtInfo();

      if (!srcFT->isNoEscape()) destExtInfo = destExtInfo.withNoEscape(false);
      if (!srcFT->throws()) destExtInfo = destExtInfo.withThrows(false);
      if (destExtInfo != destFT->getExtInfo())
        toType = FunctionType::get(destFT->getInput(),
                                           destFT->getResult(), destExtInfo);

      // If this is a function conversion that discards throwability or
      // noescape, emit a specific diagnostic about that.
      if (srcFT->throws() && !destFT->throws()) {
        diagnose(expr->getLoc(), diag::throws_functiontype_mismatch,
                 fromType, toType)
        .highlight(expr->getSourceRange());
        return true;
      }

      if (srcFT->isNoEscape() && !destFT->isNoEscape()) {
        diagnose(expr->getLoc(), diag::noescape_functiontype_mismatch,
                 fromType, toType)
        .highlight(expr->getSourceRange());
        return true;
      }
    }

  // If this is a callee that mismatches an expected return type, we can emit a
  // very nice and specific error.  In this case, what we'll generally see is
  // a failed conversion constraint of "A -> B" to "_ -> C", where the error is
  // that B isn't convertible to C.
  if (CS->getContextualTypePurpose() == CTP_CalleeResult) {
    auto destFT = toType->getAs<FunctionType>();
    auto srcFT = fromType->getAs<FunctionType>();
    if (destFT && srcFT && !isUnresolvedOrTypeVarType(srcFT->getResult())) {
      // Otherwise, the error is that the result types mismatch.
      diagnose(expr->getLoc(), diag::invalid_callee_result_type,
               srcFT->getResult(), destFT->getResult())
        .highlight(expr->getSourceRange());
      return true;
    }
  }
  
  
  // If simplification has turned this into the same types, then this isn't the
  // broken constraint that we're looking for.
  if (fromType->isEqual(toType) &&
      constraint->getKind() != ConstraintKind::ConformsTo)
    return false;
  
  
  // If we have two tuples with mismatching types, produce a tailored
  // diagnostic.
  if (auto fromTT = fromType->getAs<TupleType>())
    if (auto toTT = toType->getAs<TupleType>()) {
      if (fromTT->getNumElements() != toTT->getNumElements()) {
        diagnose(anchor->getLoc(), diag::tuple_types_not_convertible_nelts,
                 fromTT, toTT)
        .highlight(anchor->getSourceRange());
        return true;
      }
     
      SmallVector<TupleTypeElt, 4> FromElts;
      auto voidTy = CS->getASTContext().TheUnresolvedType;
      
      for (unsigned i = 0, e = fromTT->getNumElements(); i != e; ++i)
        FromElts.push_back({ voidTy, fromTT->getElement(i).getName() });
      auto TEType = TupleType::get(FromElts, CS->getASTContext());
      
      SmallVector<int, 4> sources;
      SmallVector<unsigned, 4> variadicArgs;
      
      // If the shuffle conversion is invalid (e.g. incorrect element labels),
      // then we have a type error.
      if (computeTupleShuffle(TEType->castTo<TupleType>()->getElements(),
                              toTT->getElements(), sources, variadicArgs)) {
        diagnose(anchor->getLoc(), diag::tuple_types_not_convertible,
                 fromTT, toTT)
        .highlight(anchor->getSourceRange());
        return true;
      }
    }
  
  
  // If the second type is a type variable, the expression itself is
  // ambiguous.  Bail out so the general ambiguity diagnosing logic can handle
  // it.
  if (fromType->hasUnresolvedType() || fromType->hasTypeVariable() ||
      toType->hasUnresolvedType() || toType->hasTypeVariable() ||
      // FIXME: Why reject unbound generic types here?
      fromType->is<UnboundGenericType>())
    return false;

  
  if (auto PT = toType->getAs<ProtocolType>()) {
    // Check for "=" converting to Boolean.  The user probably meant ==.
    if (auto *AE = dyn_cast<AssignExpr>(expr->getValueProvidingExpr()))
      if (PT->getDecl()->isSpecificProtocol(KnownProtocolKind::Boolean)) {
        diagnose(AE->getEqualLoc(), diag::use_of_equal_instead_of_equality)
        .fixItReplace(AE->getEqualLoc(), "==")
        .highlight(AE->getDest()->getLoc())
        .highlight(AE->getSrc()->getLoc());
        return true;
      }
 
    if (isa<NilLiteralExpr>(expr->getValueProvidingExpr())) {
      diagnose(expr->getLoc(), diag::cannot_use_nil_with_this_type, toType)
        .highlight(expr->getSourceRange());
      return true;
    }

    // Emit a conformance error through conformsToProtocol.  If this succeeds
    // and yields a valid protocol conformance, then keep searching.
    ProtocolConformance *Conformance = nullptr;
    if (CS->TC.conformsToProtocol(fromType, PT->getDecl(), CS->DC,
                                  ConformanceCheckFlags::InExpression,
                                  &Conformance, expr->getLoc())) {
      if (!Conformance || !Conformance->isInvalid()) {
        return false;
      }
    }
    return true;
  }
  
  diagnose(anchor->getLoc(), diag::types_not_convertible,
           constraint->getKind() == ConstraintKind::Subtype,
           fromType, toType)
    .highlight(anchor->getSourceRange());

  // Check to see if this constraint came from a cast instruction. If so,
  // and if this conversion constraint is different than the types being cast,
  // produce a note that talks about the overall expression.
  //
  // TODO: Using parentMap would be more general, rather than requiring the
  // issue to be related to the root of the expr under study.
  if (auto ECE = dyn_cast<ExplicitCastExpr>(expr))
    if (constraint->getLocator() &&
        constraint->getLocator()->getAnchor() == ECE->getSubExpr()) {
      if (!toType->isEqual(ECE->getCastTypeLoc().getType()))
        diagnose(expr->getLoc(), diag::in_cast_expr_types,
                 ECE->getSubExpr()->getType()->getRValueType(),
                 ECE->getCastTypeLoc().getType()->getRValueType())
        .highlight(ECE->getSubExpr()->getSourceRange())
        .highlight(ECE->getCastTypeLoc().getSourceRange());
  }

  return true;
}

namespace {
  class ExprTypeSaverAndEraser {
    llvm::DenseMap<Expr*, Type> ExprTypes;
    llvm::DenseMap<TypeLoc*, std::pair<Type, bool>> TypeLocTypes;
    llvm::DenseMap<Pattern*, Type> PatternTypes;
    llvm::DenseMap<ParamDecl*, Type> ParamDeclTypes;
    llvm::DenseMap<CollectionExpr*, Expr*> CollectionSemanticExprs;
    llvm::DenseSet<Decl*> InvalidDecls;
    ExprTypeSaverAndEraser(const ExprTypeSaverAndEraser&) = delete;
    void operator=(const ExprTypeSaverAndEraser&) = delete;
  public:

    ExprTypeSaverAndEraser(Expr *E) {
      struct TypeSaver : public ASTWalker {
        ExprTypeSaverAndEraser *TS;
        TypeSaver(ExprTypeSaverAndEraser *TS) : TS(TS) {}
        
        std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
          TS->ExprTypes[expr] = expr->getType();
          
          // Preserve module expr type data to prevent further lookups.
          if (auto *declRef = dyn_cast<DeclRefExpr>(expr))
            if (isa<ModuleDecl>(declRef->getDecl()))
              return { false, expr };
          
          // Don't strip type info off OtherConstructorDeclRefExpr, because
          // CSGen doesn't know how to reconstruct it.
          if (isa<OtherConstructorDeclRefExpr>(expr))
            return { false, expr };
          
          // TypeExpr's are relabeled by CSGen.
          if (isa<TypeExpr>(expr))
            return { false, expr };
          
          // If a literal has a Builtin.Int or Builtin.FP type on it already,
          // then sema has already expanded out a call to
          //   Init.init(<builtinliteral>)
          // and we don't want it to make
          //   Init.init(Init.init(<builtinliteral>))
          // preserve the type info to prevent this from happening.
          if (isa<LiteralExpr>(expr) &&
              !(expr->getType() && expr->getType()->is<ErrorType>()))
            return { false, expr };

          // If a ClosureExpr's parameter list has types on the decls, and the
          // types and remove them so that they'll get regenerated from the
          // associated TypeLocs or resynthesized as fresh typevars.
          if (auto *CE = dyn_cast<ClosureExpr>(expr))
            for (auto P : *CE->getParameters())
              if (P->hasType()) {
                TS->ParamDeclTypes[P] = P->getType();
                P->overwriteType(Type());
                
                if (P->isInvalid()) {
                  P->setInvalid(false);
                  TS->InvalidDecls.insert(P);
                }
              }
          
          // If we have a CollectionExpr with a type checked SemanticExpr,
          // remove it so we can recalculate a new semantic form.
          if (auto *CE = dyn_cast<CollectionExpr>(expr)) {
            if (auto SE = CE->getSemanticExpr()) {
              TS->CollectionSemanticExprs[CE] = SE;
              CE->setSemanticExpr(nullptr);
            }
          }
          
          expr->setType(nullptr);
          expr->clearLValueAccessKind();

          return { true, expr };
        }
        
        // If we find a TypeLoc (e.g. in an as? expr), save and erase it.
        bool walkToTypeLocPre(TypeLoc &TL) override {
          if (TL.getTypeRepr() && TL.getType()) {
            TS->TypeLocTypes[&TL] = { TL.getType(), TL.wasValidated() };
            TL.setType(Type(), /*was validated*/false);
          }
          return true;
        }
        
        std::pair<bool, Pattern*> walkToPatternPre(Pattern *P) override {
          if (P->hasType()) {
            TS->PatternTypes[P] = P->getType();
            P->setType(Type());
          }
          return { true, P };
        }
        
        // Don't walk into statements.  This handles the BraceStmt in
        // non-single-expr closures, so we don't walk into their body.
        std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
          return { false, S };
        }
      };
      
      E->walk(TypeSaver(this));
    }
    
    void restore() {
      for (auto exprElt : ExprTypes)
        exprElt.first->setType(exprElt.second);
      
      for (auto typelocElt : TypeLocTypes)
        typelocElt.first->setType(typelocElt.second.first,
                                  typelocElt.second.second);
      
      for (auto patternElt : PatternTypes)
        patternElt.first->setType(patternElt.second);
      
      for (auto paramDeclElt : ParamDeclTypes)
        paramDeclElt.first->overwriteType(paramDeclElt.second);
      
      for (auto CSE : CollectionSemanticExprs)
        CSE.first->setSemanticExpr(CSE.second);
      
      if (!InvalidDecls.empty())
        for (auto D : InvalidDecls)
          D->setInvalid();
      
      // Done, don't do redundant work on destruction.
      ExprTypes.clear();
      TypeLocTypes.clear();
      PatternTypes.clear();
      InvalidDecls.clear();
    }
    
    // On destruction, if a type got wiped out, reset it from null to its
    // original type.  This is helpful because type checking a subexpression
    // can lead to replacing the nodes in that subexpression.  However, the
    // failed ConstraintSystem still has locators pointing to the old nodes,
    // and if expr-specific diagnostics fail to turn up anything useful to say,
    // we go digging through failed constraints, and expect their locators to
    // still be meaningful.
    ~ExprTypeSaverAndEraser() {
      for (auto CSE : CollectionSemanticExprs)
        if (!CSE.first->getType())
          CSE.first->setSemanticExpr(CSE.second);

      for (auto exprElt : ExprTypes)
        if (!exprElt.first->getType())
          exprElt.first->setType(exprElt.second);
      
      for (auto typelocElt : TypeLocTypes)
        if (!typelocElt.first->getType())
          typelocElt.first->setType(typelocElt.second.first,
                                    typelocElt.second.second);
      
      for (auto patternElt : PatternTypes)
        if (!patternElt.first->hasType())
          patternElt.first->setType(patternElt.second);
      
      for (auto paramDeclElt : ParamDeclTypes)
        if (!paramDeclElt.first->hasType())
          paramDeclElt.first->setType(paramDeclElt.second);

      if (!InvalidDecls.empty())
        for (auto D : InvalidDecls)
          D->setInvalid();
    }
  };
}

/// Erase an expression tree's open existentials after a re-typecheck operation.
///
/// This is done in the case of a typecheck failure, after we re-typecheck
/// partially-typechecked subexpressions in a context-free manner.
///
static void eraseOpenedExistentials(Expr *&expr) {

  class ExistentialEraser : public ASTWalker {
    llvm::SmallDenseMap<OpaqueValueExpr *, Expr *, 4> OpenExistentials;

  public:
    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      if (auto OOE = dyn_cast<OpenExistentialExpr>(expr)) {
        auto archetypeVal = OOE->getOpaqueValue();
        auto base = OOE->getExistentialValue();
        
        // Walk the base expression to ensure we erase any existentials within
        // it.
        base = base->walk(*this);
        
        bool inserted = OpenExistentials.insert({archetypeVal, base}).second;
        assert(inserted && "OpaqueValue appears multiple times?");
        (void)inserted;
        return { true, OOE->getSubExpr() };
      }
      
      if (auto OVE = dyn_cast<OpaqueValueExpr>(expr)) {
        auto value = OpenExistentials.find(OVE);
        assert(value != OpenExistentials.end() &&
               "didn't see this OVE in a containing OpenExistentialExpr?");
        return { true, value->second };
      }
      
      return { true, expr };
    }

    Expr *walkToExprPost(Expr *expr) override {
      Type type = expr->getType();
      if (!type || !type->hasOpenedExistential())
        return expr;

      type = type.transform([&](Type type) -> Type {
        if (auto archetype = type->getAs<ArchetypeType>())
          if (auto existentialType = archetype->getOpenedExistentialType())
            return existentialType;

        return type;
      });
      expr->setType(type);

      return expr;
    }
    
    // Don't walk into statements.  This handles the BraceStmt in
    // non-single-expr closures, so we don't walk into their body.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      return { false, S };
    }
  };

  expr = expr->walk(ExistentialEraser());
}

/// Rewrite any type variables & archetypes in the specified type with
/// UnresolvedType.
static Type replaceArchetypesAndTypeVarsWithUnresolved(Type ty) {
  if (!ty) return ty;

 auto &ctx = ty->getASTContext();

  return ty.transform([&](Type type) -> Type {
    if (type->is<TypeVariableType>() ||
        type->is<ArchetypeType>() ||
        type->isTypeParameter())
      return ctx.TheUnresolvedType;
    return type;
  });
}

/// Unless we've already done this, retypecheck the specified subexpression on
/// its own, without including any contextual constraints or parent expr
/// nodes.  This is more likely to succeed than type checking the original
/// expression.
///
/// This can return a new expression (for e.g. when a UnresolvedDeclRef gets
/// resolved) and returns null when the subexpression fails to typecheck.
Expr *FailureDiagnosis::
typeCheckChildIndependently(Expr *subExpr, Type convertType,
                            ContextualTypePurpose convertTypePurpose,
                            TCCOptions options,
                            ExprTypeCheckListener *listener) {
  
  // If this sub-expression is currently being diagnosed, refuse to recheck the
  // expression (which may lead to infinite recursion).  If the client is
  // telling us that it knows what it is doing, then believe it.
  if (!options.contains(TCC_ForceRecheck)) {
    if (Expr *res = CS->TC.isExprBeingDiagnosed(subExpr))
      return res;
  
    CS->TC.addExprForDiagnosis(subExpr, subExpr);
  }
  
  // If we have a conversion type, but it has type variables (from the current
  // ConstraintSystem), then we can't use it.
  if (convertType) {
    // If we're asked to convert to an autoclosure, then we really want to
    // convert to the result of it.
    if (auto *FT = convertType->getAs<AnyFunctionType>())
      if (FT->isAutoClosure())
        convertType = FT->getResult();

    // Replace archetypes and type parameters with UnresolvedType.
    if (convertType->hasTypeVariable() || convertType->hasArchetype() ||
        convertType->isTypeParameter())
      convertType = replaceArchetypesAndTypeVarsWithUnresolved(convertType);
    
    // If the conversion type contains no info, drop it.
    if (convertType->is<UnresolvedType>() ||
        (convertType->is<MetatypeType>() && convertType->hasUnresolvedType())) {
      convertType = Type();
      convertTypePurpose = CTP_Unused;
    }
  }

  
  // If we have no contextual type information and the subexpr is obviously a
  // overload set, don't recursively simplify this.  The recursive solver will
  // sometimes pick one based on arbitrary ranking behavior (e.g. like
  // which is the most specialized) even then all the constraints are being
  // fulfilled by UnresolvedType, which doesn't tell us anything.
  if (convertTypePurpose == CTP_Unused &&
      (isa<OverloadedDeclRefExpr>(subExpr->getValueProvidingExpr()) ||
       isa<OverloadedMemberRefExpr>(subExpr->getValueProvidingExpr()))) {
    return subExpr;
  }
  
  // Save any existing type data of the subexpr tree, and reset it to null in
  // prep for re-type-checking the tree.  If things fail, we can revert the
  // types back to their original state.
  ExprTypeSaverAndEraser SavedTypeData(subExpr);
  
  // Store off the sub-expression, in case a new one is provided via the
  // type check operation.
  Expr *preCheckedExpr = subExpr;
  
  // Disable structural checks, because we know that the overall expression
  // has type constraint problems, and we don't want to know about any
  // syntactic issues in a well-typed subexpression (which might be because
  // the context is missing).
  TypeCheckExprOptions TCEOptions = TypeCheckExprFlags::DisableStructuralChecks;

  // Don't walk into non-single expression closure bodies, because
  // ExprTypeSaver and TypeNullifier skip them too.
  TCEOptions |= TypeCheckExprFlags::SkipMultiStmtClosures;

  // Claim that the result is discarded to preserve the lvalue type of
  // the expression.
  if (options.contains(TCC_AllowLValue))
    TCEOptions |= TypeCheckExprFlags::IsDiscarded;

  // If there is no contextual type available, tell typeCheckExpression that it
  // is ok to produce an ambiguous result, it can just fill in holes with
  // UnresolvedType and we'll deal with it.
  if (!convertType || options.contains(TCC_AllowUnresolvedTypeVariables))
    TCEOptions |= TypeCheckExprFlags::AllowUnresolvedTypeVariables;
  
  // If we're not passing down contextual type information this time, but the
  // original failure had type info that wasn't an optional type,
  // then set the flag to prefer fixits with force unwrapping.
  if (!convertType) {
    auto previousType = CS->getContextualType();
    if (previousType && previousType->getOptionalObjectType().isNull())
      TCEOptions |= TypeCheckExprFlags::PreferForceUnwrapToOptional;
  }

  // Ensure that the expression we're about to type-check doesn't have
  // anything that the type-checker doesn't expect to see.  This can happen
  // because of repeated type-checking; the removal below, while independently
  // important, isn't itself sufficient because of AST mutation.
  eraseOpenedExistentials(subExpr);

  bool hadError = CS->TC.typeCheckExpression(subExpr, CS->DC,
                                             TypeLoc::withoutLoc(convertType),
                                             convertTypePurpose, TCEOptions,
                                             listener);

  // This is a terrible hack to get around the fact that typeCheckExpression()
  // might change subExpr to point to a new OpenExistentialExpr. In that case,
  // since the caller passed subExpr by value here, they would be left
  // holding on to an expression containing open existential types but
  // no OpenExistentialExpr, which breaks invariants enforced by the
  // ASTChecker.
  eraseOpenedExistentials(subExpr);
  
  // If recursive type checking failed, then an error was emitted.  Return
  // null to indicate this to the caller.
  if (hadError)
    return nullptr;

  // If we type checked the result but failed to get a usable output from it,
  // just pretend as though nothing happened.
  if (subExpr->getType()->is<ErrorType>()) {
    subExpr = preCheckedExpr;
    SavedTypeData.restore();
  }
  
  CS->TC.addExprForDiagnosis(preCheckedExpr, subExpr);
  return subExpr;
}

/// This is the same as typeCheckChildIndependently, but works on an arbitrary
/// subexpression of the current node because it handles ClosureExpr parents
/// of the specified node.
Expr *FailureDiagnosis::
typeCheckArbitrarySubExprIndependently(Expr *subExpr, TCCOptions options) {
  if (subExpr == expr)
    return typeCheckChildIndependently(subExpr, options);
  
  // Construct a parent map for the expr tree we're investigating.
  auto parentMap = expr->getParentMap();
  
  ClosureExpr *NearestClosure = nullptr;
  
  // Walk the parents of the specified expression, handling any ClosureExprs.
  for (Expr *node = parentMap[subExpr]; node; node = parentMap[node]) {
    auto *CE = dyn_cast<ClosureExpr>(node);
    if (!CE) continue;
    
    // Keep track of the innermost closure we see that we're jumping into.
    if (!NearestClosure)
      NearestClosure = CE;
    
    // If we have a ClosureExpr parent of the specified node, check to make sure
    // none of its arguments are type variables.  If so, these type variables
    // would be accessible to name lookup of the subexpression and may thus leak
    // in.  Reset them to UnresolvedTypes for safe measures.
    for (auto param : *CE->getParameters()) {
      auto VD = param;
      if (VD->getType()->hasTypeVariable() || VD->getType()->is<ErrorType>())
        VD->overwriteType(CS->getASTContext().TheUnresolvedType);
    }
  }

  // When we're type checking a single-expression closure, we need to reset the
  // DeclContext to this closure for the recursive type checking.  Otherwise,
  // if there is a closure in the subexpression, we can violate invariants.
  auto newDC = NearestClosure ? NearestClosure : CS->DC;
  llvm::SaveAndRestore<DeclContext*> SavedDC(CS->DC, newDC);
  
  // Otherwise, we're ok to type check the subexpr.
  return typeCheckChildIndependently(subExpr, options);
}

/// For an expression being type checked with a CTP_CalleeResult contextual
/// type, try to diagnose a problem.
bool FailureDiagnosis::diagnoseCalleeResultContextualConversionError() {
  // Try to dig out the conversion constraint in question to find the contextual
  // result type being specified.
  Type contextualResultType;
  for (auto &c : CS->getConstraints()) {
    if (!isConversionConstraint(&c) || !c.getLocator() ||
        c.getLocator()->getAnchor() != expr)
      continue;
    
    // If we found our contextual type, then we know we have a conversion to
    // some function type, and that the result type is concrete.  If not,
    // ignore it.
    auto toType = CS->simplifyType(c.getSecondType());
    if (auto *FT = toType->getAs<AnyFunctionType>())
      if (!isUnresolvedOrTypeVarType(FT->getResult())) {
        contextualResultType = FT->getResult();
        break;
      }
  }
  if (!contextualResultType)
    return false;

  // Retypecheck the callee expression without a contextual type to resolve
  // whatever we can in it.
  auto callee = typeCheckChildIndependently(expr, TCC_ForceRecheck);
  if (!callee)
    return true;
  
  // Based on that, compute an overload set.
  CalleeCandidateInfo calleeInfo(callee, /*hasTrailingClosure*/false, CS);

  switch (calleeInfo.size()) {
  case 0:
    // If we found no overloads, then there is something else going on here.
    return false;
      
  case 1:
    // If the callee isn't of function type, then something else has gone wrong.
    if (!calleeInfo[0].getResultType())
      return false;
      
    diagnose(expr->getLoc(), diag::candidates_no_match_result_type,
             calleeInfo.declName, calleeInfo[0].getResultType(),
             contextualResultType);
    return true;
  default:
    // Check to see if all of the viable candidates produce the same result,
    // this happens for things like "==" and "&&" operators.
    if (auto resultTy = calleeInfo[0].getResultType()) {
      for (unsigned i = 1, e = calleeInfo.size(); i != e; ++i)
        if (auto ty = calleeInfo[i].getResultType())
          if (!resultTy->isEqual(ty)) {
            resultTy = Type();
            break;
          }
      if (resultTy) {
        diagnose(expr->getLoc(), diag::candidates_no_match_result_type,
                 calleeInfo.declName, calleeInfo[0].getResultType(),
                 contextualResultType);
        return true;
      }
    }

    // Otherwise, produce a candidate set.
    diagnose(expr->getLoc(), diag::no_candidates_match_result_type,
             calleeInfo.declName, contextualResultType);
    calleeInfo.suggestPotentialOverloads(expr->getLoc(), /*isResult*/true);
    return true;
  }
}


/// Return true if the conversion from fromType to toType is an invalid string
/// index operation.
static bool isIntegerToStringIndexConversion(Type fromType, Type toType,
                                             ConstraintSystem *CS) {
  auto integerType =
    CS->TC.getProtocol(SourceLoc(),
                       KnownProtocolKind::IntegerLiteralConvertible);
  if (!integerType) return false;

  // If the from type is an integer type, and the to type is
  // String.CharacterView.Index, then we found one.
  if (CS->TC.conformsToProtocol(fromType, integerType, CS->DC,
                                ConformanceCheckFlags::InExpression)) {
    if (toType->getCanonicalType().getString() == "String.CharacterView.Index")
      return true;
  }

  return false;
}


bool FailureDiagnosis::diagnoseContextualConversionError() {
  // If the constraint system has a contextual type, then we can test to see if
  // this is the problem that prevents us from solving the system.
  Type contextualType = CS->getContextualType();
  if (!contextualType) {
    // This contextual conversion constraint doesn't install an actual type.
    if (CS->getContextualTypePurpose() == CTP_CalleeResult)
      return diagnoseCalleeResultContextualConversionError();
 
    return false;
  }

  // Try re-type-checking the expression without the contextual type to see if
  // it can work without it.  If so, the contextual type is the problem.  We
  // force a recheck, because "expr" is likely in our table with the extra
  // contextual constraint that we know we are relaxing.
  TCCOptions options = TCC_ForceRecheck;
  if (contextualType->is<InOutType>())
    options |= TCC_AllowLValue;

  auto recheckedExpr = typeCheckChildIndependently(expr, options);
  auto exprType = recheckedExpr ? recheckedExpr->getType() : Type();
  
  // If it failed and diagnosed something, then we're done.
  if (!exprType) return true;

  // If we contextually had an inout type, and got a non-lvalue result, then
  // we fail with a mutability error.
  if (contextualType->is<InOutType>() && !exprType->is<LValueType>()) {
    diagnoseSubElementFailure(recheckedExpr, recheckedExpr->getLoc(), *CS,
                              diag::cannot_pass_rvalue_inout_subelement,
                              diag::cannot_pass_rvalue_inout);
    return true;
  }

  // Try to find the contextual type in a variety of ways.  If the constraint
  // system had a contextual type specified, we use it - it will have a purpose
  // indicator which allows us to give a very "to the point" diagnostic.
  Diag<Type, Type> diagID;
  Diag<Type, Type> diagIDProtocol;
  Diag<Type> nilDiag;
  std::function<void(void)> nilFollowup;

  // If this is conversion failure due to a return statement with an argument
  // that cannot be coerced to the result type of the function, emit a
  // specific error.
  switch (CS->getContextualTypePurpose()) {
  case CTP_Unused:
  case CTP_CannotFail:
    llvm_unreachable("These contextual type purposes cannot fail with a "
                     "conversion type specified!");
  case CTP_CalleeResult:
    llvm_unreachable("CTP_CalleeResult does not actually install a "
                     "contextual type");
  case CTP_Initialization:
    diagID = diag::cannot_convert_initializer_value;
    diagIDProtocol = diag::cannot_convert_initializer_value_protocol;
    nilDiag = diag::cannot_convert_initializer_value_nil;
    nilFollowup = [this]{
      TypeRepr *patternTR = CS->getContextualTypeLoc().getTypeRepr();
      if (!patternTR)
        return;
      auto diag = diagnose(patternTR->getLoc(), diag::note_make_optional,
                           OptionalType::get(CS->getContextualType()));
      if (patternTR->isSimple()) {
        diag.fixItInsertAfter(patternTR->getEndLoc(), "?");
      } else {
        diag.fixItInsert(patternTR->getStartLoc(), "(");
        diag.fixItInsertAfter(patternTR->getEndLoc(), ")?");
      }
    };
    break;
  case CTP_ReturnStmt:
    // Special case the "conversion to void" case.
    if (contextualType->isVoid()) {
      diagnose(expr->getLoc(), diag::cannot_return_value_from_void_func)
        .highlight(expr->getSourceRange());
      return true;
    }

    diagID = diag::cannot_convert_to_return_type;
    diagIDProtocol = diag::cannot_convert_to_return_type_protocol;
    nilDiag = diag::cannot_convert_to_return_type_nil;
    break;
  case CTP_ThrowStmt:
    if (isa<NilLiteralExpr>(expr->getValueProvidingExpr())) {
      diagnose(expr->getLoc(), diag::cannot_throw_nil);
      return true;
    }

    if (isUnresolvedOrTypeVarType(exprType) ||
        exprType->isEqual(contextualType))
      return false;
      
    // The conversion destination of throw is always ErrorType (at the moment)
    // if this ever expands, this should be a specific form like () is for
    // return.
    diagnose(expr->getLoc(), diag::cannot_convert_thrown_type, exprType)
      .highlight(expr->getSourceRange());
    return true;
      
  case CTP_EnumCaseRawValue:
    diagID = diag::cannot_convert_raw_initializer_value;
    diagIDProtocol = diag::cannot_convert_raw_initializer_value;
    nilDiag = diag::cannot_convert_raw_initializer_value_nil;
    break;
  case CTP_DefaultParameter:
    diagID = diag::cannot_convert_default_arg_value;
    diagIDProtocol = diag::cannot_convert_default_arg_value_protocol;
    nilDiag = diag::cannot_convert_default_arg_value_nil;
    break;

  case CTP_CallArgument:
    diagID = diag::cannot_convert_argument_value;
    diagIDProtocol = diag::cannot_convert_argument_value_protocol;
    nilDiag = diag::cannot_convert_argument_value_nil;
    break;
  case CTP_ClosureResult:
    diagID = diag::cannot_convert_closure_result;
    diagIDProtocol = diag::cannot_convert_closure_result_protocol;
    nilDiag = diag::cannot_convert_closure_result_nil;
    break;
  case CTP_ArrayElement:
    diagID = diag::cannot_convert_array_element;
    diagIDProtocol = diag::cannot_convert_array_element_protocol;
    nilDiag = diag::cannot_convert_array_element_nil;
    break;
  case CTP_DictionaryKey:
    diagID = diag::cannot_convert_dict_key;
    diagIDProtocol = diag::cannot_convert_dict_key_protocol;
    nilDiag = diag::cannot_convert_dict_key_nil;
    break;
  case CTP_DictionaryValue:
    diagID = diag::cannot_convert_dict_value;
    diagIDProtocol = diag::cannot_convert_dict_value_protocol;
    nilDiag = diag::cannot_convert_dict_value_nil;
    break;
  case CTP_CoerceOperand:
    diagID = diag::cannot_convert_coerce;
    diagIDProtocol = diag::cannot_convert_coerce_protocol;
    nilDiag = diag::cannot_convert_coerce_nil;
    break;
  case CTP_AssignSource:
    diagID = diag::cannot_convert_assign;
    diagIDProtocol = diag::cannot_convert_assign_protocol;
    nilDiag = diag::cannot_convert_assign_nil;
    break;
  }

  // If we're diagnostic an issue with 'nil', produce a specific diagnostic,
  // instead of uttering NilLiteralConvertible.
  if (isa<NilLiteralExpr>(expr->getValueProvidingExpr())) {
    diagnose(expr->getLoc(), nilDiag, contextualType);
    if (nilFollowup)
      nilFollowup();
    return true;
  }
  
  // If we don't have a type for the expression, then we cannot use it in
  // conversion constraint diagnostic generation.  If the types match, then it
  // must not be the contextual type that is the problem.
  if (isUnresolvedOrTypeVarType(exprType) ||
      exprType->isEqual(contextualType)) {
    return false;
  }
  
  // If we're trying to convert something of type "() -> T" to T, then we
  // probably meant to call the value.
  if (auto srcFT = exprType->getAs<AnyFunctionType>()) {
    if (srcFT->getInput()->isVoid() &&
        !isUnresolvedOrTypeVarType(srcFT->getResult()) &&
        CS->TC.isConvertibleTo(srcFT->getResult(), contextualType, CS->DC)) {
      diagnose(expr->getLoc(), diag::missing_nullary_call, srcFT->getResult())
        .highlight(expr->getSourceRange())
        .fixItInsertAfter(expr->getEndLoc(), "()");
      return true;
    }
  }

  // If this is a conversion from T to () in a call argument context, it is
  // almost certainly an extra argument being passed in.
  if (CS->getContextualTypePurpose() == CTP_CallArgument &&
      contextualType->isVoid()) {
    diagnose(expr->getLoc(), diag::extra_argument_to_nullary_call)
      .highlight(expr->getSourceRange());
    return true;
  }

  exprType = exprType->getRValueType();

  // Special case of some common conversions involving Swift.String
  // indexes, catching cases where people attempt to index them with an integer.
  if (isIntegerToStringIndexConversion(exprType, contextualType, CS)) {
    diagnose(expr->getLoc(), diag::string_index_not_integer,
             exprType->getRValueType())
      .highlight(expr->getSourceRange());
    diagnose(expr->getLoc(), diag::string_index_not_integer_note);
    return true;
  }

  // When complaining about conversion to a protocol type, complain about
  // conformance instead of "conversion".
  if (contextualType->is<ProtocolType>() ||
      contextualType->is<ProtocolCompositionType>())
    diagID = diagIDProtocol;
  
  // Try to simplify irrelevant details of function types.  For example, if
  // someone passes a "() -> Float" function to a "() throws -> Int"
  // parameter, then uttering the "throws" may confuse them into thinking that
  // that is the problem, even though there is a clear subtype relation.
  if (auto srcFT = exprType->getAs<FunctionType>())
    if (auto destFT = contextualType->getAs<FunctionType>()) {
      auto destExtInfo = destFT->getExtInfo();
      
      if (!srcFT->isNoEscape()) destExtInfo = destExtInfo.withNoEscape(false);
      if (!srcFT->throws()) destExtInfo = destExtInfo.withThrows(false);
      if (destExtInfo != destFT->getExtInfo())
        contextualType = FunctionType::get(destFT->getInput(),
                                           destFT->getResult(), destExtInfo);

      // If this is a function conversion that discards throwability or
      // noescape, emit a specific diagnostic about that.
      if (srcFT->throws() && !destFT->throws())
        diagID = diag::throws_functiontype_mismatch;
      else if (srcFT->isNoEscape() && !destFT->isNoEscape())
        diagID = diag::noescape_functiontype_mismatch;
    }

  diagnose(expr->getLoc(), diagID, exprType, contextualType)
    .highlight(expr->getSourceRange());
  return true;
}


/// When an assignment to an expression is detected and the destination is
/// invalid, emit a detailed error about the condition.
void ConstraintSystem::diagnoseAssignmentFailure(Expr *dest, Type destTy,
                                                 SourceLoc equalLoc) {
  auto &TC = getTypeChecker();

  // Diagnose obvious assignments to literals.
  if (isa<LiteralExpr>(dest->getValueProvidingExpr())) {
    TC.diagnose(equalLoc, diag::cannot_assign_to_literal);
    return;
  }

  Diag<StringRef> diagID;
  if (isa<DeclRefExpr>(dest))
    diagID = diag::assignment_lhs_is_immutable_variable;
  else if (isa<ForceValueExpr>(dest))
    diagID = diag::assignment_bang_has_immutable_subcomponent;
  else if (isa<UnresolvedDotExpr>(dest) || isa<MemberRefExpr>(dest))
    diagID = diag::assignment_lhs_is_immutable_property;
  else if (isa<SubscriptExpr>(dest))
    diagID = diag::assignment_subscript_has_immutable_base;
  else {
    diagID = diag::assignment_lhs_is_immutable_variable;
  }
  
  diagnoseSubElementFailure(dest, equalLoc, *this, diagID,
                            diag::assignment_lhs_not_lvalue);
}

static bool isSymmetricBinaryOperator(const CalleeCandidateInfo &CCI) {
  // If we don't have at least one known candidate, don't trigger.
  if (CCI.candidates.empty()) return false;

  for (auto &candidate : CCI.candidates) {
    // Each candidate must be a non-assignment operator function.
    auto decl = dyn_cast_or_null<FuncDecl>(candidate.getDecl());
    if (!decl) return false;
    auto op = dyn_cast_or_null<InfixOperatorDecl>(decl->getOperatorDecl());
    if (!op || op->isAssignment()) return false;

    // It must have exactly two parameters.
    auto params = decl->getParameterLists().back();
    if (params->size() != 2) return false;

    // Require the types to be the same.
    if (!params->get(0)->getType()->isEqual(params->get(1)->getType()))
      return false;
  }

  return true;
}

/// Special magic to handle inout exprs and tuples in argument lists.
Expr *FailureDiagnosis::
typeCheckArgumentChildIndependently(Expr *argExpr, Type argType,
                                    const CalleeCandidateInfo &candidates,
                                    TCCOptions options) {
  // Grab one of the candidates (if present) and get its input list to help
  // identify operators that have implicit inout arguments.
  Type exampleInputType;
  if (!candidates.empty()) {
    exampleInputType = candidates[0].getArgumentType();

    // If we found a single candidate, and have no contextually known argument
    // type information, use that one candidate as the type information for
    // subexpr checking.
    //
    // TODO: If all candidates have the same type for some argument, we could
    // pass down partial information.
    if (candidates.size() == 1 && !argType)
      argType = candidates[0].getArgumentType();
  }
  
  // If our candidates are instance members at curry level #0, then the argument
  // being provided is the receiver type for the instance.  We produce better
  // diagnostics when we don't force the self type down.
  if (argType && !candidates.empty())
    if (auto decl = candidates[0].getDecl())
      if (decl->isInstanceMember() && candidates[0].level == 0 &&
          !isa<SubscriptDecl>(decl))
        argType = Type();

  // Similarly, we get better results when we don't push argument types down
  // to symmetric operators.
  if (argType && isSymmetricBinaryOperator(candidates))
    argType = Type();
  

  // FIXME: This should all just be a matter of getting the type of the
  // sub-expression, but this doesn't work well when typeCheckChildIndependently
  // is over-conservative w.r.t. TupleExprs.
  auto *TE = dyn_cast<TupleExpr>(argExpr);
  if (!TE) {
    // If the argument isn't a tuple, it is some scalar value for a
    // single-argument call.
    if (exampleInputType && exampleInputType->is<InOutType>())
      options |= TCC_AllowLValue;

    // If the argtype is a tuple type with default arguments, or a labeled tuple
    // with a single element, pull the scalar element type for the subexpression
    // out.  If we can't do that and the tuple has default arguments, we have to
    // punt on passing down the type information, since type checking the
    // subexpression won't be able to find the default argument provider.
    if (argType)
      if (auto argTT = argType->getAs<TupleType>()) {
        int scalarElt = argTT->getElementForScalarInit();
        // If the argument cannot be initialized with a scalar, then it is an
        // error, so we might as well pass down the expected type, to get a
        // specific error involving it.
        if (scalarElt == -1) {
          // However, if there are default values, we don't actually want to do
          // this.  We don't know if the user just forgot a label on a defaulted
          // value.
          if (argTT->hasAnyDefaultValues())
            argType = Type();
        } else {
          // If we found the single argument being initialized, use it.
          auto &arg = argTT->getElement(scalarElt);
          
          // If the argument being specified is actually varargs, then we're
          // just specifying one element of a variadic list.  Use the type of
          // the individual varargs argument, not the overall array type.
          if (arg.isVararg())
            argType = arg.getVarargBaseTy();
          else
            argType = arg.getType();
        }
      }
    
    auto CTPurpose = argType ? CTP_CallArgument : CTP_Unused;
    return typeCheckChildIndependently(argExpr, argType, CTPurpose, options);
  }

  // If we know the requested argType to use, use computeTupleShuffle to produce
  // the shuffle of input arguments to destination values.  It requires a
  // TupleType to compute the mapping from argExpr.  Conveniently, it doesn't
  // care about the actual types though, so we can just use 'void' for them.
  if (argType && argType->is<TupleType>()) {
    auto argTypeTT = argType->castTo<TupleType>();
    SmallVector<TupleTypeElt, 4> ArgElts;
    auto voidTy = CS->getASTContext().TheEmptyTupleType;
    
    for (unsigned i = 0, e = TE->getNumElements(); i != e; ++i)
      ArgElts.push_back({ voidTy, TE->getElementName(i) });
    SmallVector<int, 4> sources;
    SmallVector<unsigned, 4> variadicArgs;
    if (!computeTupleShuffle(ArgElts, argTypeTT->getElements(),
                             sources, variadicArgs)) {
      SmallVector<Expr*, 4> resultElts(TE->getNumElements(), nullptr);
      SmallVector<TupleTypeElt, 4> resultEltTys(TE->getNumElements(), voidTy);

      // If we got a correct shuffle, we can perform the analysis of all of
      // the input elements, with their expected types.
      for (unsigned i = 0, e = sources.size(); i != e; ++i) {
        // If the value is taken from a default argument, ignore it.
        if (sources[i] == TupleShuffleExpr::DefaultInitialize ||
            sources[i] == TupleShuffleExpr::Variadic ||
            sources[i] == TupleShuffleExpr::CallerDefaultInitialize)
          continue;
        
        assert(sources[i] >= 0 && "Unknown sources index");
        
        // Otherwise, it must match the corresponding expected argument type.
        unsigned inArgNo = sources[i];
        auto actualType = argTypeTT->getElementType(i);

        if (actualType->is<InOutType>())
          options |= TCC_AllowLValue;

        auto exprResult =
          typeCheckChildIndependently(TE->getElement(inArgNo), actualType,
                                      CTP_CallArgument, options);
        // If there was an error type checking this argument, then we're done.
        if (!exprResult)
          return nullptr;

        // If the caller expected something inout, but we didn't have
        // something of inout type, diagnose it.
        if (auto IOE =
              dyn_cast<InOutExpr>(exprResult->getSemanticsProvidingExpr())) {
          if (!actualType->is<InOutType>()) {
            diagnose(exprResult->getLoc(), diag::extra_address_of,
                     exprResult->getType()->getInOutObjectType())
              .highlight(exprResult->getSourceRange())
              .fixItRemove(IOE->getStartLoc());
            return nullptr;
          }
        }
        
        resultElts[inArgNo] = exprResult;
        resultEltTys[inArgNo] = {
          exprResult->getType(),
          TE->getElementName(inArgNo)
        };
      }
      
      if (!variadicArgs.empty()) {
        auto varargsTy = argTypeTT->getVarArgsBaseType();
        for (unsigned i = 0, e = variadicArgs.size(); i != e; ++i) {
          unsigned inArgNo = variadicArgs[i];
          
          auto expr =
            typeCheckChildIndependently(TE->getElement(inArgNo), varargsTy,
                                        CTP_CallArgument);
          // If there was an error type checking this argument, then we're done.
          if (!expr)
            return nullptr;
          resultElts[inArgNo] = expr;
          resultEltTys[inArgNo] = { expr->getType() };
        }
      }
      
      auto TT = TupleType::get(resultEltTys, CS->getASTContext());
      return TupleExpr::create(CS->getASTContext(), TE->getLParenLoc(),
                               resultElts, TE->getElementNames(),
                               TE->getElementNameLocs(),
                               TE->getRParenLoc(), TE->hasTrailingClosure(),
                               TE->isImplicit(), TT);
    }
  }
  
  // Get the simplified type of each element and rebuild the aggregate.
  SmallVector<TupleTypeElt, 4> resultEltTys;
  SmallVector<Expr*, 4> resultElts;

  TupleType *exampleInputTuple = nullptr;
  if (exampleInputType)
    exampleInputTuple = exampleInputType->getAs<TupleType>();

  for (unsigned i = 0, e = TE->getNumElements(); i != e; i++) {
    if (exampleInputTuple && i < exampleInputTuple->getNumElements() &&
        exampleInputTuple->getElementType(i)->is<InOutType>())
      options |= TCC_AllowLValue;

    auto elExpr = typeCheckChildIndependently(TE->getElement(i), options);
    if (!elExpr) return nullptr; // already diagnosed.
    
    resultElts.push_back(elExpr);
    resultEltTys.push_back({elExpr->getType(), TE->getElementName(i)});
  }
  
  auto TT = TupleType::get(resultEltTys, CS->getASTContext());
  return TupleExpr::create(CS->getASTContext(), TE->getLParenLoc(),
                           resultElts, TE->getElementNames(),
                           TE->getElementNameLocs(),
                           TE->getRParenLoc(), TE->hasTrailingClosure(),
                           TE->isImplicit(), TT);
}


/// Emit a class of diagnostics that we only know how to generate when there is
/// exactly one candidate we know about.  Return true if an error is emitted.
static bool diagnoseSingleCandidateFailures(CalleeCandidateInfo &CCI,
                                            Expr *fnExpr, Expr *argExpr) {
  // We only handle the situation where there is exactly one candidate here.
  if (CCI.size() != 1)
    return false;

  auto candidate = CCI[0];
  auto &TC = CCI.CS->TC;

  auto argTy = candidate.getArgumentType();
  if (!argTy) return false;

  auto params = decomposeArgParamType(argTy);
  auto args = decomposeArgParamType(argExpr->getType());

  // It is a somewhat common error to try to access an instance method as a
  // curried member on the type, instead of using an instance, e.g. the user
  // wrote:
  //
  //   Foo.doThing(42, b: 19)
  //
  // instead of:
  //
  //   myFoo.doThing(42, b: 19)
  //
  // Check for this situation and handle it gracefully.
  if (params.size() == 1 && candidate.getDecl() &&
      candidate.getDecl()->isInstanceMember() &&
      candidate.level == 0) {
    if (auto UDE = dyn_cast<UnresolvedDotExpr>(fnExpr))
      if (isa<TypeExpr>(UDE->getBase())) {
        auto baseType = candidate.getArgumentType();
        auto DC = CCI.CS->DC;

        // If the base is an implicit self type reference, and we're in a
        // property initializer, then the user wrote something like:
        //
        //   class Foo { let val = initFn() }
        //
        // which runs in type context, not instance context.  Produce a tailored
        // diagnostic since this comes up and is otherwise non-obvious what is
        // going on.
        if (UDE->getBase()->isImplicit() && isa<Initializer>(DC) &&
            DC->getParent()->getDeclaredTypeOfContext()->isEqual(baseType)){
          TC.diagnose(UDE->getLoc(), diag::instance_member_in_initializer,
                          UDE->getName());
          return true;
        }

        // Otherwise, complain about use of instance value on type.
        TC.diagnose(UDE->getLoc(), diag::instance_member_use_on_type,
                    baseType, UDE->getName())
          .highlight(UDE->getBase()->getSourceRange());
        return true;
      }
  }

  // We only handle structural errors here.
  if (CCI.closeness != CC_ArgumentLabelMismatch &&
      CCI.closeness != CC_ArgumentCountMismatch)
    return false;

  SmallVector<Identifier, 4> correctNames;
  unsigned OOOArgIdx = ~0U, OOOPrevArgIdx = ~0U;
  unsigned extraArgIdx = ~0U, missingParamIdx = ~0U;

  // If we have a single candidate that failed to match the argument list,
  // attempt to use matchCallArguments to diagnose the problem.
  struct OurListener : public MatchCallArgumentListener {
    SmallVectorImpl<Identifier> &correctNames;
    unsigned &OOOArgIdx, &OOOPrevArgIdx;
    unsigned &extraArgIdx, &missingParamIdx;

  public:
    OurListener(SmallVectorImpl<Identifier> &correctNames,
                unsigned &OOOArgIdx, unsigned &OOOPrevArgIdx,
                unsigned &extraArgIdx, unsigned &missingParamIdx)
    : correctNames(correctNames),
    OOOArgIdx(OOOArgIdx), OOOPrevArgIdx(OOOPrevArgIdx),
    extraArgIdx(extraArgIdx), missingParamIdx(missingParamIdx) {}
    void extraArgument(unsigned argIdx) override {
      extraArgIdx = argIdx;
    }
    void missingArgument(unsigned paramIdx) override {
      missingParamIdx = paramIdx;
    }
    void outOfOrderArgument(unsigned argIdx, unsigned prevArgIdx) override{
      OOOArgIdx = argIdx;
      OOOPrevArgIdx = prevArgIdx;
    }
    bool relabelArguments(ArrayRef<Identifier> newNames) override {
      correctNames.append(newNames.begin(), newNames.end());
      return true;
    }
  } listener(correctNames, OOOArgIdx, OOOPrevArgIdx,
             extraArgIdx, missingParamIdx);

  // Use matchCallArguments to determine how close the argument list is (in
  // shape) to the specified candidates parameters.  This ignores the
  // concrete types of the arguments, looking only at the argument labels.
  SmallVector<ParamBinding, 4> paramBindings;
  if (!matchCallArguments(args, params, CCI.hasTrailingClosure,
                          /*allowFixes:*/true, listener, paramBindings))
    return false;


  // If we are missing a parameter, diagnose that.
  if (missingParamIdx != ~0U) {
    Identifier name = params[missingParamIdx].Label;
    auto loc = argExpr->getStartLoc();
    if (name.empty())
      TC.diagnose(loc, diag::missing_argument_positional,
                  missingParamIdx+1);
    else
      TC.diagnose(loc, diag::missing_argument_named, name);
    return true;
  }

  if (extraArgIdx != ~0U) {
    auto name = args[extraArgIdx].Label;
    Expr *arg = argExpr;
    auto tuple = dyn_cast<TupleExpr>(argExpr);
    if (tuple)
      arg = tuple->getElement(extraArgIdx);
    auto loc = arg->getLoc();
    if (tuple && extraArgIdx == tuple->getNumElements()-1 &&
        tuple->hasTrailingClosure())
      TC.diagnose(loc, diag::extra_trailing_closure_in_call)
        .highlight(arg->getSourceRange());
    else if (params.empty())
      TC.diagnose(loc, diag::extra_argument_to_nullary_call)
        .highlight(argExpr->getSourceRange());
    else if (name.empty())
      TC.diagnose(loc, diag::extra_argument_positional)
        .highlight(arg->getSourceRange());
    else
      TC.diagnose(loc, diag::extra_argument_named, name)
        .highlight(arg->getSourceRange());
    return true;
  }

  // If this is an argument label mismatch, then diagnose that error now.
  if (!correctNames.empty() &&
      diagnoseArgumentLabelError(TC, argExpr, correctNames,
                                 isa<SubscriptExpr>(fnExpr)))
    return true;

  // If we have an out-of-order argument, diagnose it as such.
  if (OOOArgIdx != ~0U && isa<TupleExpr>(argExpr)) {
    auto tuple = cast<TupleExpr>(argExpr);
    Identifier first = tuple->getElementName(OOOArgIdx);
    Identifier second = tuple->getElementName(OOOPrevArgIdx);

    SourceLoc diagLoc;
    if (!first.empty())
      diagLoc = tuple->getElementNameLoc(OOOArgIdx);
    else
      diagLoc = tuple->getElement(OOOArgIdx)->getStartLoc();

    if (!second.empty()) {
      TC.diagnose(diagLoc, diag::argument_out_of_order, first, second)
        .highlight(tuple->getElement(OOOArgIdx)->getSourceRange())
        .highlight(SourceRange(tuple->getElementNameLoc(OOOPrevArgIdx),
                               tuple->getElement(OOOPrevArgIdx)->getEndLoc()));
      return true;
    }

    TC.diagnose(diagLoc, diag::argument_out_of_order_named_unnamed, first,
                OOOPrevArgIdx)
      .highlight(tuple->getElement(OOOArgIdx)->getSourceRange())
      .highlight(tuple->getElement(OOOPrevArgIdx)->getSourceRange());
    return true;
  }

  return false;
}

/// If the candidate set has been narrowed down to a specific structural
/// problem, e.g. that there are too few parameters specified or that argument
/// labels don't match up, diagnose that error and return true.
bool FailureDiagnosis::diagnoseParameterErrors(CalleeCandidateInfo &CCI,
                                               Expr *fnExpr, Expr *argExpr) {
  // If we are invoking a constructor and there are absolutely no candidates,
  // then they must all be private.
  if (auto *MTT = fnExpr->getType()->getAs<MetatypeType>()) {
    if (!MTT->getInstanceType()->is<TupleType>() &&
        (CCI.size() == 0 ||
         (CCI.size() == 1 && CCI.candidates[0].getDecl() &&
          isa<ProtocolDecl>(CCI.candidates[0].getDecl())))) {
      CS->TC.diagnose(fnExpr->getLoc(), diag::no_accessible_initializers,
                      MTT->getInstanceType());
      return true;
    }
  }

  // Do all the stuff that we only have implemented when there is a single
  // candidate.
  if (diagnoseSingleCandidateFailures(CCI, fnExpr, argExpr))
    return true;

  // If we have a failure where the candidate set differs on exactly one
  // argument, and where we have a consistent mismatch across the candidate set
  // (often because there is only one candidate in the set), then diagnose this
  // as a specific problem of passing something of the wrong type into a
  // parameter.
  //
  // We don't generally want to use this path to diagnose calls to
  // symmetrically-typed binary operators because it's likely that both
  // operands contributed to the type.
  if ((CCI.closeness == CC_OneArgumentMismatch ||
       CCI.closeness == CC_OneArgumentNearMismatch ||
       CCI.closeness == CC_OneGenericArgumentMismatch ||
       CCI.closeness == CC_OneGenericArgumentNearMismatch ||
       CCI.closeness == CC_GenericNonsubstitutableMismatch) &&
      CCI.failedArgument.isValid() &&
      !isSymmetricBinaryOperator(CCI)) {
    // Map the argument number into an argument expression.
    TCCOptions options = TCC_ForceRecheck;
    if (CCI.failedArgument.parameterType->is<InOutType>())
      options |= TCC_AllowLValue;

    Expr *badArgExpr;
    if (auto *TE = dyn_cast<TupleExpr>(argExpr))
      badArgExpr = TE->getElement(CCI.failedArgument.argumentNumber);
    else if (auto *PE = dyn_cast<ParenExpr>(argExpr)) {
      assert(CCI.failedArgument.argumentNumber == 0 &&
             "Unexpected argument #");
      badArgExpr = PE->getSubExpr();
    } else {
      assert(CCI.failedArgument.argumentNumber == 0 &&
             "Unexpected argument #");
      badArgExpr = argExpr;
    }

    // It could be that the argument doesn't conform to an archetype.
    if (CCI.diagnoseGenericParameterErrors(badArgExpr))
      return true;
    
    // Re-type-check the argument with the expected type of the candidate set.
    // This should produce a specific and tailored diagnostic saying that the
    // type mismatches with expectations.
    Type paramType = CCI.failedArgument.parameterType;
    if (!typeCheckChildIndependently(badArgExpr, paramType,
                                     CTP_CallArgument, options))
      return true;
  }
  
  return false;
}



bool FailureDiagnosis::visitSubscriptExpr(SubscriptExpr *SE) {
  auto baseExpr = typeCheckChildIndependently(SE->getBase());
  if (!baseExpr) return true;
  auto baseType = baseExpr->getType();
  
  auto locator =
    CS->getConstraintLocator(SE, ConstraintLocator::SubscriptMember);

  auto subscriptName = CS->getASTContext().Id_subscript;
  
  MemberLookupResult result =
    CS->performMemberLookup(ConstraintKind::ValueMember, subscriptName,
                            baseType, locator,
                            /*includeInaccessibleMembers*/true);

  
  switch (result.OverallResult) {
  case MemberLookupResult::Unsolved:
    return false;
  case MemberLookupResult::ErrorAlreadyDiagnosed:
    // If an error was already emitted, then we're done, don't emit anything
    // redundant.
    return true;
  case MemberLookupResult::HasResults:
    break;    // Interesting case. :-)
  }
  
  // If we have unviable candidates (e.g. because of access control or some
  // other problem) we should diagnose the problem.
  if (result.ViableCandidates.empty()) {
    diagnoseUnviableLookupResults(result, baseType, baseExpr, subscriptName,
                                  DeclNameLoc(SE->getLoc()), SE->getLoc());
    return true;
  }

  
  
  CalleeCandidateInfo calleeInfo(Type(), result.ViableCandidates,
                                 /*FIXME: Subscript trailing closures*/
                                 /*hasTrailingClosure*/false, CS,
                                 /*selfAlreadyApplied*/false);

  // We're about to typecheck the index list, which needs to be processed with
  // self already applied.
  for (unsigned i = 0, e = calleeInfo.size(); i != e; ++i)
    ++calleeInfo.candidates[i].level;
  
  auto indexExpr = typeCheckArgumentChildIndependently(SE->getIndex(),
                                                       Type(), calleeInfo);
  if (!indexExpr) return true;

  // Back to analyzing the candidate list with self applied.
  for (unsigned i = 0, e = calleeInfo.size(); i != e; ++i)
    --calleeInfo.candidates[i].level;

  if (diagnoseParameterErrors(calleeInfo, SE, indexExpr))
    return true;

  auto indexType = indexExpr->getType();

  auto decomposedBaseType = decomposeArgParamType(baseType);
  auto decomposedIndexType = decomposeArgParamType(indexType);
  calleeInfo.filterList([&](UncurriedCandidate cand) ->
                                 CalleeCandidateInfo::ClosenessResultTy
  {
    // Classify how close this match is.  Non-subscript decls don't match.
    auto *SD = dyn_cast_or_null<SubscriptDecl>(cand.getDecl());
    if (!SD) return { CC_GeneralMismatch, {}};
    
    // Check whether the self type matches.
    auto selfConstraint = CC_ExactMatch;
    if (calleeInfo.evaluateCloseness(SD->getInnermostDeclContext(),
                                   cand.getArgumentType(), decomposedBaseType)
          .first != CC_ExactMatch)
      selfConstraint = CC_SelfMismatch;
    
    // Increase the uncurry level to look past the self argument to the indices.
    cand.level++;
    
    // Explode out multi-index subscripts to find the best match.
    auto indexResult =
      calleeInfo.evaluateCloseness(SD->getInnermostDeclContext(),
                                   cand.getArgumentType(), decomposedIndexType);
    if (selfConstraint > indexResult.first)
      return {selfConstraint, {}};
    return indexResult;
  });

  // If the closest matches all mismatch on self, we either have something that
  // cannot be subscripted, or an ambiguity.
  if (calleeInfo.closeness == CC_SelfMismatch) {
    diagnose(SE->getLoc(), diag::cannot_subscript_base, baseType)
    .highlight(SE->getBase()->getSourceRange());
    // FIXME: Should suggest overload set, but we're not ready for that until
    // it points to candidates and identifies the self type in the diagnostic.
    //calleeInfo.suggestPotentialOverloads(SE->getLoc());
    return true;
  }

  // Any other failures relate to the index list.
  for (unsigned i = 0, e = calleeInfo.size(); i != e; ++i)
    ++calleeInfo.candidates[i].level;
  
  // TODO: Is there any reason to check for CC_NonLValueInOut here?
  
  if (calleeInfo.closeness == CC_ExactMatch) {
    // Otherwise, whatever the result type of the call happened to be must not
    // have been what we were looking for.  Lets diagnose it as a conversion
    // or ambiguity failure.
    if (calleeInfo.size() == 1)
      return false;

    diagnose(SE->getLoc(), diag::ambiguous_subscript, baseType, indexType)
      .highlight(indexExpr->getSourceRange())
      .highlight(baseExpr->getSourceRange());
    
    // FIXME: suggestPotentialOverloads should do this.
    //calleeInfo.suggestPotentialOverloads(SE->getLoc());
    for (auto candidate : calleeInfo.candidates)
      if (auto decl = candidate.getDecl())
        diagnose(decl, diag::found_candidate);
      else
        diagnose(candidate.getExpr()->getLoc(), diag::found_candidate);

    return true;
  }

  if (diagnoseParameterErrors(calleeInfo, SE, indexExpr))
    return true;

  // Diagnose some simple and common errors.
  if (calleeInfo.diagnoseSimpleErrors(SE))
    return true;


  diagnose(SE->getLoc(), diag::cannot_subscript_with_index,
           baseType, indexType);

  calleeInfo.suggestPotentialOverloads(SE->getLoc());
  return true;
}


namespace {
  /// Type checking listener for pattern binding initializers.
  class CalleeListener : public ExprTypeCheckListener {
    Type contextualType;
  public:
    explicit CalleeListener(Type contextualType)
      : contextualType(contextualType) { }

    virtual bool builtConstraints(ConstraintSystem &cs, Expr *expr) {
      // If we have no contextual type, there is nothing to do.
      if (!contextualType) return false;

      // If the expression is obviously something that produces a metatype,
      // then don't put a constraint on it.
      auto semExpr = expr->getValueProvidingExpr();
      if (isa<TypeExpr>(semExpr))
        return false;
      
      // We're making the expr have a function type, whose result is the same
      // as our contextual type.
      auto inputLocator =
        cs.getConstraintLocator(expr, ConstraintLocator::FunctionResult);

      auto tv = cs.createTypeVariable(inputLocator,
                                 TVO_CanBindToLValue|TVO_PrefersSubtypeBinding);

      // In order to make this work, we pick the most general function type and
      // use a conversion constraint.  This gives us:
      //    "$T0 throws -> contextualType"
      // this allows things that are throws and not throws, and allows escape
      // and noescape functions.
      auto extInfo = FunctionType::ExtInfo().withThrows();
      auto fTy = FunctionType::get(tv, contextualType, extInfo);

      auto locator = cs.getConstraintLocator(expr);

      // Add a conversion constraint between the types.
      cs.addConstraint(ConstraintKind::Conversion, expr->getType(),
                       fTy, locator, /*isFavored*/true);
      return false;
    }
  };
}

/// Return true if the argument of a CallExpr (or related node) has a trailing
/// closure.
static bool callArgHasTrailingClosure(Expr *E) {
  if (!E) return false;
  if (auto *PE = dyn_cast<ParenExpr>(E))
    return PE->hasTrailingClosure();
  else if (auto *TE = dyn_cast<TupleExpr>(E))
    return TE->hasTrailingClosure();
  return false;
}

/// Return true if this function name is a comparison operator.  This is a
/// simple heuristic used to guide comparison related diagnostics.
static bool isNameOfStandardComparisonOperator(StringRef opName) {
  return opName == "=="  || opName == "!=" ||
         opName == "===" || opName == "!==" ||
         opName == "<"   || opName == ">" ||
         opName == "<="  || opName == ">=";
}

bool FailureDiagnosis::visitApplyExpr(ApplyExpr *callExpr) {
  // Type check the function subexpression to resolve a type for it if possible.
  auto fnExpr = typeCheckChildIndependently(callExpr->getFn());
  if (!fnExpr) return true;
  
  // If we have a contextual type, and if we have an ambiguously typed function
  // result from our previous check, we re-type-check it using this contextual
  // type to inform the result type of the callee.
  //
  // We only do this as a second pass because the first pass we just did may
  // return something of obviously non-function-type.  If this happens, we
  // produce better diagnostics below by diagnosing this here rather than trying
  // to peel apart the failed conversion to function type.
  if (CS->getContextualType() &&
      (isUnresolvedOrTypeVarType(fnExpr->getType()) ||
       (fnExpr->getType()->is<AnyFunctionType>() &&
        fnExpr->getType()->hasUnresolvedType()))) {
    CalleeListener listener(CS->getContextualType());
    fnExpr = typeCheckChildIndependently(callExpr->getFn(), Type(),
                                         CTP_CalleeResult, TCC_ForceRecheck,
                                         &listener);
    if (!fnExpr) return true;
  }
  
  auto fnType = fnExpr->getType()->getRValueType();

  // If we resolved a concrete expression for the callee, and it has
  // non-function/non-metatype type, then we cannot call it!
  if (!isUnresolvedOrTypeVarType(fnType) &&
      !fnType->is<AnyFunctionType>() && !fnType->is<MetatypeType>()) {
    
    // If the argument is a trailing ClosureExpr (i.e. {....}) and it is on a
    // different line than the callee, then the "real" issue is that the user
    // forgot to write "do" before their brace stmt.
    if (auto *PE = dyn_cast<ParenExpr>(callExpr->getArg()))
      if (PE->hasTrailingClosure() && isa<ClosureExpr>(PE->getSubExpr())) {
        auto &SM = CS->getASTContext().SourceMgr;
        if (SM.getLineNumber(callExpr->getFn()->getEndLoc()) !=
            SM.getLineNumber(PE->getStartLoc())) {
          diagnose(PE->getStartLoc(), diag::expected_do_in_statement)
            .fixItInsert(PE->getStartLoc(), "do ");
          return true;
        }
      }
    
    diagnose(callExpr->getArg()->getStartLoc(),
             diag::cannot_call_non_function_value, fnExpr->getType())
    .highlight(fnExpr->getSourceRange());
    return true;
  }
  
  bool hasTrailingClosure = callArgHasTrailingClosure(callExpr->getArg());
  
  // Collect a full candidate list of callees based on the partially type
  // checked function.
  CalleeCandidateInfo calleeInfo(fnExpr, hasTrailingClosure, CS);
  
  // Filter the candidate list based on the argument we may or may not have.
  calleeInfo.filterContextualMemberList(callExpr->getArg());

  if (diagnoseParameterErrors(calleeInfo, callExpr->getFn(),
                              callExpr->getArg()))
    return true;
  
  Type argType;  // Type of the argument list, if knowable.
  if (auto FTy = fnType->getAs<AnyFunctionType>())
    argType = FTy->getInput();
  else if (auto MTT = fnType->getAs<AnyMetatypeType>()) {
    // If we are constructing a tuple with initializer syntax, the expected
    // argument list is the tuple type itself - and there is no initdecl.
    auto instanceTy = MTT->getInstanceType();
    if (instanceTy->is<TupleType>()) {
      argType = instanceTy;
    }
  }
  
  // Get the expression result of type checking the arguments to the call
  // independently, so we have some idea of what we're working with.
  //
  auto argExpr = typeCheckArgumentChildIndependently(callExpr->getArg(),
                                                     argType, calleeInfo,
                                             TCC_AllowUnresolvedTypeVariables);
  if (!argExpr)
    return true; // already diagnosed.

  calleeInfo.filterList(argExpr->getType());

  if (diagnoseParameterErrors(calleeInfo, callExpr->getFn(), argExpr))
    return true;

  // Force recheck of the arg expression because we allowed unresolved types
  // before, and that turned out not to help, and now we want any diagnoses
  // from disallowing them.
  argExpr = typeCheckArgumentChildIndependently(callExpr->getArg(), argType,
                                                calleeInfo, TCC_ForceRecheck);
  if (!argExpr)
    return true; // already diagnosed.
  
  // Diagnose some simple and common errors.
  if (calleeInfo.diagnoseSimpleErrors(callExpr))
    return true;
  
  
  // A common error is to apply an operator that only has inout forms (e.g. +=)
  // to non-lvalues (e.g. a local let).  Produce a nice diagnostic for this
  // case.
  if (calleeInfo.closeness == CC_NonLValueInOut) {
    Diag<StringRef> subElementDiagID;
    Diag<Type> rvalueDiagID;
    Expr *diagExpr = nullptr;
    
    if (isa<PrefixUnaryExpr>(callExpr) || isa<PostfixUnaryExpr>(callExpr)) {
      subElementDiagID = diag::cannot_apply_lvalue_unop_to_subelement;
      rvalueDiagID = diag::cannot_apply_lvalue_unop_to_rvalue;
      diagExpr = argExpr;
    } else if (isa<BinaryExpr>(callExpr)) {
      subElementDiagID = diag::cannot_apply_lvalue_binop_to_subelement;
      rvalueDiagID = diag::cannot_apply_lvalue_binop_to_rvalue;
      
      if (auto argTuple = dyn_cast<TupleExpr>(argExpr))
        diagExpr = argTuple->getElement(0);
    }
    
    if (diagExpr) {
      diagnoseSubElementFailure(diagExpr, callExpr->getFn()->getLoc(), *CS,
                                subElementDiagID, rvalueDiagID);
      return true;
    }
  }
  
  // Handle argument label mismatches when we have multiple candidates.
  if (calleeInfo.closeness == CC_ArgumentLabelMismatch) {
    auto args = decomposeArgParamType(argExpr->getType());

    // If we have multiple candidates that we fail to match, just say we have
    // the wrong labels and list the candidates out.
    
    // TODO: It would be nice to use an analog of getTypeListString that
    // doesn't include the argument types.
    diagnose(callExpr->getLoc(), diag::wrong_argument_labels_overload,
             getParamListAsString(args))
      .highlight(argExpr->getSourceRange());

    // Did the user intend on invoking a different overload?
    calleeInfo.suggestPotentialOverloads(fnExpr->getLoc());
    return true;
  }

  auto overloadName = calleeInfo.declName;
  
  // Otherwise, we have a generic failure.  Diagnose it with a generic error
  // message now.
  if (isa<BinaryExpr>(callExpr) && isa<TupleExpr>(argExpr)) {
    auto argTuple = cast<TupleExpr>(argExpr);
    auto lhsExpr = argTuple->getElement(0), rhsExpr = argTuple->getElement(1);
    auto lhsType = lhsExpr->getType()->getRValueType();
    auto rhsType = rhsExpr->getType()->getRValueType();
    
    // If this is a comparison against nil, then we should produce a specific
    // diagnostic.
    if (isa<NilLiteralExpr>(rhsExpr->getValueProvidingExpr()) &&
        !isUnresolvedOrTypeVarType(lhsType)) {
      if (isNameOfStandardComparisonOperator(overloadName)) {
        diagnose(callExpr->getLoc(), diag::comparison_with_nil_illegal, lhsType)
          .highlight(lhsExpr->getSourceRange());
        return true;
      }
    }

    if (callExpr->isImplicit() && overloadName == "~=") {
      // This binop was synthesized when typechecking an expression pattern.
      diagnose(lhsExpr->getLoc(),
               diag::cannot_match_expr_pattern_with_value, lhsType, rhsType)
        .highlight(lhsExpr->getSourceRange())
        .highlight(rhsExpr->getSourceRange());
      return true;
    }

    // Diagnose attempts to compare reference equality of certain types.
    if (overloadName == "===" || overloadName == "!==") {
      // Functions.
      if (lhsType->is<AnyFunctionType>() || rhsType->is<AnyFunctionType>()) {
        diagnose(callExpr->getLoc(), diag::cannot_reference_compare_types,
                 overloadName, lhsType, rhsType)
          .highlight(lhsExpr->getSourceRange())
          .highlight(rhsExpr->getSourceRange());
        return true;
      }
    }
    
    // If we found an exact match, this must be a problem with a conversion from
    // the result of the call to the expected type.  Diagnose this as a
    // conversion failure.
    if (calleeInfo.closeness == CC_ExactMatch)
      return false;
    
    if (!lhsType->isEqual(rhsType)) {
      diagnose(callExpr->getLoc(), diag::cannot_apply_binop_to_args,
               overloadName, lhsType, rhsType)
      .highlight(lhsExpr->getSourceRange())
      .highlight(rhsExpr->getSourceRange());
    } else {
      diagnose(callExpr->getLoc(), diag::cannot_apply_binop_to_same_args,
               overloadName, lhsType)
      .highlight(lhsExpr->getSourceRange())
      .highlight(rhsExpr->getSourceRange());
    }

    if (lhsType->isEqual(rhsType) &&
        isNameOfStandardComparisonOperator(overloadName) &&
        lhsType->is<EnumType>() &&
        !lhsType->getAs<EnumType>()->getDecl()
          ->hasOnlyCasesWithoutAssociatedValues()) {
      diagnose(callExpr->getLoc(),
               diag::no_binary_op_overload_for_enum_with_payload,
               overloadName);
    } else {
      calleeInfo.suggestPotentialOverloads(callExpr->getLoc());
    }

    return true;
  }
  
  // If we found an exact match, this must be a problem with a conversion from
  // the result of the call to the expected type.  Diagnose this as a
  // conversion failure.
  if (calleeInfo.closeness == CC_ExactMatch)
    return false;
  
  // Generate specific error messages for unary operators.
  if (isa<PrefixUnaryExpr>(callExpr) || isa<PostfixUnaryExpr>(callExpr)) {
    assert(!overloadName.empty());
    diagnose(argExpr->getLoc(), diag::cannot_apply_unop_to_arg, overloadName,
             argExpr->getType());
    
    calleeInfo.suggestPotentialOverloads(argExpr->getLoc());
    return true;
  }

  if (argExpr->getType()->hasUnresolvedType())
    return false;
  
  
  std::string argString = getTypeListString(argExpr->getType());

  // If we couldn't get the name of the callee, then it must be something of a
  // more complex "value of function type".
  if (overloadName.empty()) {
    // If we couldn't infer the result type of the closure expr, then we have
    // some sort of ambiguity, let the ambiguity diagnostic stuff handle this.
    if (auto ffty = fnType->getAs<AnyFunctionType>())
      if (ffty->getResult()->hasTypeVariable()) {
        diagnoseAmbiguity(fnExpr);
        return true;
      }
    
    // The most common unnamed value of closure type is a ClosureExpr, so
    // special case it.
    if (isa<ClosureExpr>(fnExpr->getValueProvidingExpr())) {
      if (fnType->hasTypeVariable())
        diagnose(argExpr->getStartLoc(), diag::cannot_invoke_closure, argString)
          .highlight(fnExpr->getSourceRange());
      else
        diagnose(argExpr->getStartLoc(), diag::cannot_invoke_closure_type,
                 fnType, argString)
          .highlight(fnExpr->getSourceRange());
      
    } else if (fnType->hasTypeVariable()) {
      diagnose(argExpr->getStartLoc(), diag::cannot_call_function_value,
               argString)
        .highlight(fnExpr->getSourceRange());
    } else {
      diagnose(argExpr->getStartLoc(), diag::cannot_call_value_of_function_type,
                fnType, argString)
        .highlight(fnExpr->getSourceRange());
    }
    
    return true;
  }
  
  // If we have an argument list (i.e., a scalar, or a non-zero-element tuple)
  // then diagnose with some specificity about the arguments.
  bool isInitializer = isa<TypeExpr>(fnExpr);
  if (isa<TupleExpr>(argExpr) &&
      cast<TupleExpr>(argExpr)->getNumElements() == 0) {
    // Emit diagnostics that say "no arguments".
    diagnose(fnExpr->getLoc(), diag::cannot_call_with_no_params,
             overloadName, isInitializer);
  } else {
    diagnose(fnExpr->getLoc(), diag::cannot_call_with_params,
             overloadName, argString, isInitializer);
  }
  
  // Did the user intend on invoking a different overload?
  calleeInfo.suggestPotentialOverloads(fnExpr->getLoc());
  return true;
}

bool FailureDiagnosis::visitAssignExpr(AssignExpr *assignExpr) {
  // Diagnose obvious assignments to literals.
  if (isa<LiteralExpr>(assignExpr->getDest()->getValueProvidingExpr())) {
    diagnose(assignExpr->getLoc(), diag::cannot_assign_to_literal);
    return true;
  }

  // Type check the destination first, so we can coerce the source to it.
  auto destExpr = typeCheckChildIndependently(assignExpr->getDest(),
                                              TCC_AllowLValue);
  if (!destExpr) return true;

  auto destType = destExpr->getType();
  if (destType->is<UnresolvedType>() || destType->hasTypeVariable()) {
    // If we have no useful type information from the destination, just type
    // check the source without contextual information.  If it succeeds, then we
    // win, but if it fails, we'll have to diagnose this another way.
    return !typeCheckChildIndependently(assignExpr->getSrc());
  }
  
  
  // If the result type is a non-lvalue, then we are failing because it is
  // immutable and that's not a great thing to assign to.
  if (!destType->isLValueType()) {
    CS->diagnoseAssignmentFailure(destExpr, destType, assignExpr->getLoc());
    return true;
  }

  // If the source type is already an error type, we've already posted an error.
  auto srcExpr = typeCheckChildIndependently(assignExpr->getSrc(),
                                             destType->getRValueType(),
                                             CTP_AssignSource);
  if (!srcExpr) return true;

  // If we are assigning to _ and have unresolved types on the RHS, then we have
  // an ambiguity problem.
  if (isa<DiscardAssignmentExpr>(destExpr->getSemanticsProvidingExpr()) &&
      srcExpr->getType()->hasUnresolvedType()) {
    diagnoseAmbiguity(srcExpr);
    return true;
  }

  return false;
}


/// Return true if this type is known to be an ArrayType.
static bool isKnownToBeArrayType(Type ty) {
  if (!ty) return false;

  auto bgt = ty->getAs<BoundGenericType>();
  if (!bgt) return false;

  auto &ctx = bgt->getASTContext();
  return bgt->getDecl() == ctx.getArrayDecl();
}

bool FailureDiagnosis::visitInOutExpr(InOutExpr *IOE) {
  // If we have a contextual type, it must be an inout type.
  auto contextualType = CS->getContextualType();
  if (contextualType) {
    // If the contextual type is one of the UnsafePointer<T> types, then the
    // contextual type of the subexpression must be T.
    Type unwrappedType = contextualType;
    if (auto unwrapped = contextualType->getAnyOptionalObjectType())
      unwrappedType = unwrapped;

    PointerTypeKind pointerKind;
    if (auto pointerEltType =
          unwrappedType->getAnyPointerElementType(pointerKind)) {

      // If the element type is Void, then we allow any input type, since
      // everything is convertible to UnsafePointer<Void>
      if (pointerEltType->isVoid())
        contextualType = Type();
      else
        contextualType = pointerEltType;
      
      // Furthermore, if the subexpr type is already known to be an array type,
      // then we must have an attempt at an array to pointer conversion.
      if (isKnownToBeArrayType(IOE->getSubExpr()->getType())) {
        // If we're converting to an UnsafeMutablePointer, then the pointer to
        // the first element is being passed in.  The array is ok, so long as
        // it is mutable.
        if (pointerKind == PTK_UnsafeMutablePointer) {
          contextualType = ArraySliceType::get(contextualType);
        } else if (pointerKind == PTK_UnsafePointer) {
          // If we're converting to an UnsafePointer, then the programmer
          // specified an & unnecessarily.  Produce a fixit hint to remove it.
          diagnose(IOE->getLoc(), diag::extra_address_of_unsafepointer,
                   unwrappedType)
            .highlight(IOE->getSourceRange())
            .fixItRemove(IOE->getStartLoc());
          return true;
        }
      }
    } else if (contextualType->is<InOutType>()) {
      contextualType = contextualType->getInOutObjectType();
    } else {
      // If the caller expected something inout, but we didn't have
      // something of inout type, diagnose it.
      diagnose(IOE->getLoc(), diag::extra_address_of, contextualType)
        .highlight(IOE->getSourceRange())
        .fixItRemove(IOE->getStartLoc());
      return true;
    }
  }
  
  auto subExpr = typeCheckChildIndependently(IOE->getSubExpr(), contextualType,
                                             CS->getContextualTypePurpose(),
                                             TCC_AllowLValue);
  if (!subExpr) return true;

  auto subExprType = subExpr->getType();

  // The common cause is that the operand is not an lvalue.
  if (!subExprType->isLValueType()) {
    diagnoseSubElementFailure(subExpr, IOE->getLoc(), *CS,
                              diag::cannot_pass_rvalue_inout_subelement,
                              diag::cannot_pass_rvalue_inout);
    return true;
  }
  
  return false;
}

bool FailureDiagnosis::visitCoerceExpr(CoerceExpr *CE) {
  // Coerce the input to whatever type is specified by the CoerceExpr.
  if (!typeCheckChildIndependently(CE->getSubExpr(),
                                   CE->getCastTypeLoc().getType(),
                                   CTP_CoerceOperand))
    return true;

  return false;
}

bool FailureDiagnosis::visitForceValueExpr(ForceValueExpr *FVE) {
  auto argExpr = typeCheckChildIndependently(FVE->getSubExpr());
  if (!argExpr) return true;
  auto argType = argExpr->getType();

  // If the subexpression type checks as a non-optional type, then that is the
  // error.  Produce a specific diagnostic about this.
  if (!isUnresolvedOrTypeVarType(argType) &&
      argType->getAnyOptionalObjectType().isNull()) {
    diagnose(FVE->getLoc(), diag::invalid_force_unwrap, argType)
      .fixItRemove(FVE->getExclaimLoc())
      .highlight(FVE->getSourceRange());
    return true;
  }
  
  return false;
}

bool FailureDiagnosis::visitBindOptionalExpr(BindOptionalExpr *BOE) {
  auto argExpr = typeCheckChildIndependently(BOE->getSubExpr());
  if (!argExpr) return true;
  auto argType = argExpr->getType();

  // If the subexpression type checks as a non-optional type, then that is the
  // error.  Produce a specific diagnostic about this.
  if (!isUnresolvedOrTypeVarType(argType) &&
      argType->getAnyOptionalObjectType().isNull()) {
    diagnose(BOE->getQuestionLoc(), diag::invalid_optional_chain, argType)
      .highlight(BOE->getSourceRange())
      .fixItRemove(BOE->getQuestionLoc());
    return true;
  }

  return false;
}

bool FailureDiagnosis::visitIfExpr(IfExpr *IE) {
  // Check all of the subexpressions independently.
  auto condExpr = typeCheckChildIndependently(IE->getCondExpr());
  if (!condExpr) return true;
  auto trueExpr = typeCheckChildIndependently(IE->getThenExpr());
  if (!trueExpr) return true;

  auto falseExpr = typeCheckChildIndependently(IE->getElseExpr());
  if (!falseExpr) return true;

  // Check for "=" converting to Boolean.  The user probably meant ==.
  if (auto *AE = dyn_cast<AssignExpr>(condExpr->getValueProvidingExpr())) {
    diagnose(AE->getEqualLoc(), diag::use_of_equal_instead_of_equality)
      .fixItReplace(AE->getEqualLoc(), "==")
      .highlight(AE->getDest()->getLoc())
      .highlight(AE->getSrc()->getLoc());
    return true;
  }

  // If the condition wasn't of boolean type, diagnose the problem.
  auto booleanType = CS->TC.getProtocol(IE->getQuestionLoc(),
                                        KnownProtocolKind::Boolean);
  if (!booleanType) return true;

  if (!CS->TC.conformsToProtocol(condExpr->getType(), booleanType, CS->DC,
                                 ConformanceCheckFlags::InExpression,
                                 nullptr, condExpr->getLoc()))
    return true;
  
  // If the true/false values already match, it must be a contextual problem.
  if (trueExpr->getType()->isEqual(falseExpr->getType()))
    return false;
  
  // Otherwise, the true/false result types must not be matching.
  diagnose(IE->getColonLoc(), diag::if_expr_cases_mismatch,
           trueExpr->getType(), falseExpr->getType())
    .highlight(trueExpr->getSourceRange())
    .highlight(falseExpr->getSourceRange());
  return true;
}


bool FailureDiagnosis::
visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E) {
  // Don't walk the children for this node, it leads to multiple diagnostics
  // because of how sema injects this node into the type checker.
  return false;
}


bool FailureDiagnosis::visitClosureExpr(ClosureExpr *CE) {
  Type expectedResultType;
  
  // If we have a contextual type available for this closure, apply it to the
  // ParamDecls in our parameter list.  This ensures that any uses of them get
  // appropriate types.
  if (CS->getContextualType() &&
      CS->getContextualType()->is<AnyFunctionType>()) {

    auto fnType = CS->getContextualType()->castTo<AnyFunctionType>();
    auto *params = CE->getParameters();
    Type inferredArgType = fnType->getInput();
    
    // It is very common for a contextual type to disagree with the argument
    // list built into the closure expr.  This can be because the closure expr
    // had an explicitly specified pattern, a la:
    //    { a,b in ... }
    // or could be because the closure has an implicitly generated one:
    //    { $0 + $1 }
    // in either case, we want to produce nice and clear diagnostics.
    unsigned actualArgCount = params->size();
    unsigned inferredArgCount = 1;
    if (auto *argTupleTy = inferredArgType->getAs<TupleType>())
      inferredArgCount = argTupleTy->getNumElements();
    
    // If the actual argument count is 1, it can match a tuple as a whole.
    if (actualArgCount != 1 && actualArgCount != inferredArgCount) {
      // If the closure didn't specify any arguments and it is in a context that
      // needs some, produce a fixit to turn "{...}" into "{ _,_ in ...}".
      if (actualArgCount == 0 && CE->getInLoc().isInvalid()) {
        auto diag =
          diagnose(CE->getStartLoc(), diag::closure_argument_list_missing,
                   inferredArgCount);
        StringRef fixText;  // We only handle the most common cases.
        if (inferredArgCount == 1)
          fixText = " _ in ";
        else if (inferredArgCount == 2)
          fixText = " _,_ in ";
        else if (inferredArgCount == 3)
          fixText = " _,_,_ in ";
        
        if (!fixText.empty()) {
          // Determine if there is already a space after the { in the closure to
          // make sure we introduce the right whitespace.
          auto afterBrace = CE->getStartLoc().getAdvancedLoc(1);
          auto text = CS->TC.Context.SourceMgr.extractText({afterBrace, 1});
          if (text.size() == 1 && text == " ")
            fixText = fixText.drop_back();
          else
            fixText = fixText.drop_front();
          diag.fixItInsertAfter(CE->getStartLoc(), fixText);
        }
        return true;
      }
      
      // Okay, the wrong number of arguments was used, complain about that.
      // Before doing so, strip attributes off the function type so that they
      // don't confuse the issue.
      fnType = FunctionType::get(fnType->getInput(), fnType->getResult());
      diagnose(params->getStartLoc(), diag::closure_argument_list_tuple,
               fnType, inferredArgCount, actualArgCount);
      return true;
    }

    if (CS->TC.coerceParameterListToType(params, CE, inferredArgType))
      return true;

    expectedResultType = fnType->getResult();
  } else {
    
    // Defend against type variables from our constraint system leaking into
    // recursive constraints systems formed when checking the body of the
    // closure.  These typevars come into them when the body does name
    // lookups against the parameter decls.
    //
    // Handle this by rewriting the arguments to UnresolvedType().
    for (auto VD : *CE->getParameters()) {
      if (VD->getType()->hasTypeVariable() || VD->getType()->is<ErrorType>())
        VD->overwriteType(CS->getASTContext().TheUnresolvedType);
    }
  }

  // If this is a complex leaf closure, there is nothing more we can do.
  if (!CE->hasSingleExpressionBody())
    return false;

  // If the closure had an expected result type, use it.
  if (CE->hasExplicitResultType())
    expectedResultType = CE->getExplicitResultTypeLoc().getType();

  // When we're type checking a single-expression closure, we need to reset the
  // DeclContext to this closure for the recursive type checking.  Otherwise,
  // if there is a closure in the subexpression, we can violate invariants.
  {
    llvm::SaveAndRestore<DeclContext*> SavedDC(CS->DC, CE);
    
    auto CTP = expectedResultType ? CTP_ClosureResult : CTP_Unused;
    
    if (!typeCheckChildIndependently(CE->getSingleExpressionBody(),
                                     expectedResultType, CTP))
      return true;
  }
  
  // If the body of the closure looked ok, then look for a contextual type
  // error.  This is necessary because FailureDiagnosis::diagnoseExprFailure
  // doesn't do this for closures.
  if (CS->getContextualType() &&
      !CS->getContextualType()->isEqual(CE->getType())) {

    auto fnType = CS->getContextualType()->getAs<AnyFunctionType>();

    // If the closure had an explicitly written return type incompatible with
    // the contextual type, diagnose that.
    if (CE->hasExplicitResultType() &&
        CE->getExplicitResultTypeLoc().getTypeRepr()) {
      auto explicitResultTy = CE->getExplicitResultTypeLoc().getType();
      if (fnType && !explicitResultTy->isEqual(fnType->getResult())) {
        auto repr = CE->getExplicitResultTypeLoc().getTypeRepr();
        diagnose(repr->getStartLoc(), diag::incorrect_explicit_closure_result,
                 explicitResultTy, fnType->getResult())
          .fixItReplace(repr->getSourceRange(),fnType->getResult().getString());
        return true;
      }
    }
  }

  // Otherwise, we can't produce a specific diagnostic.
  return false;
}

static bool isDictionaryLiteralCompatible(Type ty, ConstraintSystem *CS,
                                          SourceLoc loc) {
  auto DLC = CS->TC.getProtocol(loc,
                              KnownProtocolKind::DictionaryLiteralConvertible);
  if (!DLC) return false;
  return CS->TC.conformsToProtocol(ty, DLC, CS->DC,
                                   ConformanceCheckFlags::InExpression);
}

bool FailureDiagnosis::visitArrayExpr(ArrayExpr *E) {
  Type contextualElementType;
  auto elementTypePurpose = CTP_Unused;

  // If we had a contextual type, then it either conforms to
  // ArrayLiteralConvertible or it is an invalid contextual type.
  if (auto contextualType = CS->getContextualType()) {
    // If our contextual type is an optional, look through them, because we're
    // surely initializing whatever is inside.
    contextualType = contextualType->lookThroughAllAnyOptionalTypes();

    // Validate that the contextual type conforms to ArrayLiteralConvertible and
    // figure out what the contextual element type is in place.
    auto ALC = CS->TC.getProtocol(E->getLoc(),
                                  KnownProtocolKind::ArrayLiteralConvertible);
    ProtocolConformance *Conformance = nullptr;
    if (!ALC)
      return visitExpr(E);

    // Check to see if the contextual type conforms.
    bool foundConformance =
      CS->TC.conformsToProtocol(contextualType, ALC, CS->DC,
                                ConformanceCheckFlags::InExpression,
                                &Conformance);
    
    // If not, we may have an implicit conversion going on.  If the contextual
    // type is an UnsafePointer or UnsafeMutablePointer, then that is probably
    // what is happening.
    if (!foundConformance) {
      // TODO: Not handling various string conversions or void conversions.
      Type unwrappedTy = contextualType;
      if (Type unwrapped = contextualType->getAnyOptionalObjectType())
        unwrappedTy = unwrapped;
      PointerTypeKind pointerKind;
      if (Type pointeeTy = unwrappedTy->getAnyPointerElementType(pointerKind)) {
        if (pointerKind == PTK_UnsafePointer) {
          auto arrayTy = ArraySliceType::get(pointeeTy);
          foundConformance =
            CS->TC.conformsToProtocol(arrayTy, ALC, CS->DC,
                                      ConformanceCheckFlags::InExpression,
                                      &Conformance);
          if (foundConformance)
            contextualType = arrayTy;
        }
      }
    }
    
    if (!foundConformance) {
      // If the contextual type conforms to DictionaryLiteralConvertible and
      // this is an empty array, then they meant "[:]".
      if (E->getNumElements() == 0 &&
          isDictionaryLiteralCompatible(contextualType, CS, E->getLoc())) {
        diagnose(E->getStartLoc(), diag::should_use_empty_dictionary_literal)
          .fixItInsert(E->getEndLoc(), ":");
        return true;
      }


      diagnose(E->getStartLoc(), diag::type_is_not_array, contextualType)
        .highlight(E->getSourceRange());

      // If the contextual type conforms to DictionaryLiteralConvertible, then
      // they wrote "x = [1,2]" but probably meant "x = [1:2]".
      if ((E->getElements().size() & 1) == 0 && !E->getElements().empty() &&
          isDictionaryLiteralCompatible(contextualType, CS, E->getLoc())) {
        auto diag = diagnose(E->getStartLoc(), diag::meant_dictionary_lit);

        // Change every other comma into a colon.
        for (unsigned i = 0, e = E->getElements().size()/2; i != e; ++i)
          diag.fixItReplace(E->getCommaLocs()[i*2], ":");
      }

      return true;
    }

    Conformance->forEachTypeWitness(&CS->TC,
                                    [&](AssociatedTypeDecl *ATD,
                          const Substitution &subst, TypeDecl *d)->bool
    {
      if (ATD->getName().str() == "Element")
        contextualElementType = subst.getReplacement()->getDesugaredType();
      return false;
    });
    assert(contextualElementType &&
           "Could not find 'Element' ArrayLiteral associated types from"
           " contextual type conformance");

    elementTypePurpose = CTP_ArrayElement;
  }

  // Type check each of the subexpressions in place, passing down the contextual
  // type information if we have it.
  for (auto elt : E->getElements()) {
    if (typeCheckChildIndependently(elt, contextualElementType,
                                    elementTypePurpose) == nullptr)
      return true;
  }

  // If that didn't turn up an issue, then we don't know what to do.
  // TODO: When a contextual type is missing, we could try to diagnose cases
  // where the element types mismatch... but theoretically they should type
  // unify to Any, so that could never happen?
  return false;
}

bool FailureDiagnosis::visitDictionaryExpr(DictionaryExpr *E) {
  Type contextualKeyType, contextualValueType;
  auto keyTypePurpose = CTP_Unused, valueTypePurpose = CTP_Unused;

  // If we had a contextual type, then it either conforms to
  // DictionaryLiteralConvertible or it is an invalid contextual type.
  if (auto contextualType = CS->getContextualType()) {
    // If our contextual type is an optional, look through them, because we're
    // surely initializing whatever is inside.
    contextualType = contextualType->lookThroughAllAnyOptionalTypes();

    auto DLC = CS->TC.getProtocol(E->getLoc(),
                            KnownProtocolKind::DictionaryLiteralConvertible);
    if (!DLC) return visitExpr(E);

    // Validate the contextual type conforms to DictionaryLiteralConvertible
    // and figure out what the contextual Key/Value types are in place.
    ProtocolConformance *Conformance = nullptr;
    if (!CS->TC.conformsToProtocol(contextualType, DLC, CS->DC,
                                   ConformanceCheckFlags::InExpression,
                                   &Conformance)) {
      diagnose(E->getStartLoc(), diag::type_is_not_dictionary, contextualType)
        .highlight(E->getSourceRange());
      return true;
    }

    Conformance->forEachTypeWitness(&CS->TC,
                                    [&](AssociatedTypeDecl *ATD,
                          const Substitution &subst, TypeDecl *d)->bool
    {
      if (ATD->getName().str() == "Key")
        contextualKeyType = subst.getReplacement()->getDesugaredType();
      else if (ATD->getName().str() == "Value")
        contextualValueType = subst.getReplacement()->getDesugaredType();
      return false;
    });
    assert(contextualKeyType && contextualValueType &&
           "Could not find Key/Value DictionaryLiteral associated types from"
           " contextual type conformance");
    
    keyTypePurpose = CTP_DictionaryKey;
    valueTypePurpose = CTP_DictionaryValue;
  }
  
  // Type check each of the subexpressions in place, passing down the contextual
  // type information if we have it.
  for (auto elt : E->getElements()) {
    auto TE = dyn_cast<TupleExpr>(elt);
    if (!TE || TE->getNumElements() != 2) continue;

    if (!typeCheckChildIndependently(TE->getElement(0),
                                     contextualKeyType, keyTypePurpose))
      return true;
    if (!typeCheckChildIndependently(TE->getElement(1),
                                     contextualValueType, valueTypePurpose))
      return true;
  }

  // If that didn't turn up an issue, then we don't know what to do.
  // TODO: When a contextual type is missing, we could try to diagnose cases
  // where the element types mismatch.  There is no Any equivalent since they
  // keys need to be hashable.
  return false;
}

/// When an object literal fails to typecheck because its protocol's
/// corresponding default type has not been set in the global namespace (e.g.
/// _ColorLiteralType), suggest that the user import the appropriate module for
/// the target.
bool FailureDiagnosis::visitObjectLiteralExpr(ObjectLiteralExpr *E) {
  auto &TC = CS->getTypeChecker();

  // Type check the argument first.
  auto protocol = TC.getLiteralProtocol(E);
  if (!protocol)
    return false;
  DeclName constrName = TC.getObjectLiteralConstructorName(E);
  assert(constrName);
  ArrayRef<ValueDecl *> constrs = protocol->lookupDirect(constrName);
  if (constrs.size() != 1 || !isa<ConstructorDecl>(constrs.front()))
    return false;
  auto *constr = cast<ConstructorDecl>(constrs.front());
  auto paramType = TC.getObjectLiteralParameterType(E, constr);
  if (!typeCheckChildIndependently(
        E->getArg(), paramType, CTP_CallArgument))
    return true;

  // Conditions for showing this diagnostic:
  // * The object literal protocol's default type is unimplemented
  if (TC.getDefaultType(protocol, CS->DC))
    return false;
  // * The object literal has no contextual type
  if (CS->getContextualType())
    return false;

  // Figure out what import to suggest.
  auto &Ctx = CS->getASTContext();
  const auto &target = Ctx.LangOpts.Target;
  StringRef importModule;
  StringRef importDefaultTypeName;
  if (protocol == Ctx.getProtocol(KnownProtocolKind::ColorLiteralConvertible)) {
    if (target.isMacOSX()) {
      importModule = "AppKit";
      importDefaultTypeName = "NSColor";
    } else if (target.isiOS() || target.isTvOS()) {
      importModule = "UIKit";
      importDefaultTypeName = "UIColor";
    }
  } else if (protocol == Ctx.getProtocol(
               KnownProtocolKind::ImageLiteralConvertible)) {
    if (target.isMacOSX()) {
      importModule = "AppKit";
      importDefaultTypeName = "NSImage";
    } else if (target.isiOS() || target.isTvOS()) {
      importModule = "UIKit";
      importDefaultTypeName = "UIImage";
    }
  } else if (protocol == Ctx.getProtocol( 
               KnownProtocolKind::FileReferenceLiteralConvertible)) {
    importModule = "Foundation";
    importDefaultTypeName = Ctx.getSwiftName(KnownFoundationEntity::NSURL);
  }

  // Emit the diagnostic.
  const auto plainName = E->getLiteralKindPlainName();
  TC.diagnose(E->getLoc(), diag::object_literal_default_type_missing,
              plainName);
  if (!importModule.empty()) {
    TC.diagnose(E->getLoc(), diag::object_literal_resolve_import,
                importModule, importDefaultTypeName, plainName);
  }
  return true;
}

bool FailureDiagnosis::visitUnresolvedMemberExpr(UnresolvedMemberExpr *E) {
  // If we have no contextual type, there is no way to resolve this.  Just
  // diagnose this as an ambiguity.
  if (!CS->getContextualType())
    return false;

  // OTOH, if we do have a contextual type, we can provide a more specific
  // error.  Dig out the UnresolvedValueMember constraint for this expr node.
  Constraint *memberConstraint = nullptr;
  auto checkConstraint = [&](Constraint *C) {
    if (C->getKind() == ConstraintKind::UnresolvedValueMember &&
        simplifyLocatorToAnchor(*CS, C->getLocator()) == E)
      memberConstraint = C;
  };
  
  if (CS->failedConstraint)
    checkConstraint(CS->failedConstraint);
  for (auto &C : CS->getConstraints()) {
    if (memberConstraint) break;
    checkConstraint(&C);
  }
  
  // If we can't find the member constraint in question, then we failed.
  if (!memberConstraint)
    return false;
  
  // If we succeeded, get ready to do the member lookup.
  auto baseObjTy = CS->getContextualType()->getRValueType();

  // If the base object is already a metatype type, then something weird is
  // going on.  For now, just generate a generic error.
  if (baseObjTy->is<MetatypeType>())
    return false;

  // Otherwise, we'll perform a lookup against the metatype of our contextual
  // type.
  baseObjTy = MetatypeType::get(baseObjTy);
  MemberLookupResult result =
    CS->performMemberLookup(memberConstraint->getKind(),
                            memberConstraint->getMember(),
                            baseObjTy, memberConstraint->getLocator(),
                            /*includeInaccessibleMembers*/true);

  switch (result.OverallResult) {
  case MemberLookupResult::Unsolved:
    llvm_unreachable("base expr type should be resolved at this point");
  case MemberLookupResult::ErrorAlreadyDiagnosed:
    // If an error was already emitted, then we're done, don't emit anything
    // redundant.
    return true;
  case MemberLookupResult::HasResults:
    break;    // Interesting case. :-)
  }

  // If we have unviable candidates (e.g. because of access control or some
  // other problem) we should diagnose the problem.  Note that we diagnose this
  // here instead of letting diagnoseGeneralMemberFailure handle it, because it
  // doesn't know how to handle lookup into a contextual type for an URME.
  if (result.ViableCandidates.empty()) {
    diagnoseUnviableLookupResults(result, baseObjTy, /*no base expr*/nullptr,
                                  E->getName(), E->getNameLoc(),
                                  E->getLoc());
    return true;
  }
  
  bool hasTrailingClosure = callArgHasTrailingClosure(E->getArgument());

  // Dump all of our viable candidates into a CalleeCandidateInfo & sort it out.
  CalleeCandidateInfo candidateInfo(Type(), result.ViableCandidates,
                                    hasTrailingClosure, CS);

  // Filter the candidate list based on the argument we may or may not have.
  candidateInfo.filterContextualMemberList(E->getArgument());

  // If we have multiple candidates, then we have an ambiguity.
  if (candidateInfo.size() != 1) {
    SourceRange argRange;
    if (auto arg = E->getArgument()) argRange = arg->getSourceRange();
    diagnose(E->getNameLoc(), diag::ambiguous_member_overload_set,
             E->getName())
      .highlight(argRange);
    candidateInfo.suggestPotentialOverloads(E->getNameLoc().getBaseNameLoc());
    return true;
  }
  
  auto argumentTy = candidateInfo[0].getArgumentType();

  // Depending on how we matched, produce tailored diagnostics.
  switch (candidateInfo.closeness) {
  case CC_NonLValueInOut:      // First argument is inout but no lvalue present.
  case CC_OneArgumentMismatch: // All arguments except one match.
  case CC_OneArgumentNearMismatch:
  case CC_OneGenericArgumentMismatch:
  case CC_OneGenericArgumentNearMismatch:
  case CC_GenericNonsubstitutableMismatch:
  case CC_SelfMismatch:        // Self argument mismatches.
  case CC_ArgumentNearMismatch:// Argument list mismatch.
  case CC_ArgumentMismatch:    // Argument list mismatch.
    assert(0 && "These aren't produced by filterContextualMemberList");
    return false;

  case CC_ExactMatch: {        // This is a perfect match for the arguments.

    // If we have an exact match, then we must have an argument list, check it.
    if (argumentTy) {
      assert(E->getArgument() && "Exact match without argument?");
      if (!typeCheckArgumentChildIndependently(E->getArgument(), argumentTy,
                                               candidateInfo))
        return true;
    }
    
    // If the argument is a match, then check the result type.  We might have
    // looked up a contextual member whose result type disagrees with the
    // expected result type.
    auto resultTy = candidateInfo[0].getResultType();
    if (!resultTy)
      resultTy = candidateInfo[0].getUncurriedType();
    
    if (resultTy && !CS->getContextualType()->is<UnboundGenericType>() &&
        !CS->TC.isConvertibleTo(resultTy, CS->getContextualType(), CS->DC)) {
      diagnose(E->getNameLoc(), diag::expected_result_in_contextual_member,
               E->getName(), resultTy, CS->getContextualType());
      return true;
    }
    
    // Otherwise, this is an exact match, return false to diagnose this as an
    // ambiguity.  It must be some other problem, such as failing to infer a
    // generic argument on the enum type.
    return false;
  }

  case CC_Unavailable:
  case CC_Inaccessible:
    // Diagnose some simple and common errors.
    if (candidateInfo.diagnoseSimpleErrors(E))
      return true;
    return false;
      
  case CC_ArgumentLabelMismatch: { // Argument labels are not correct.
    auto argExpr = typeCheckArgumentChildIndependently(E->getArgument(),
                                                       argumentTy,
                                                       candidateInfo);
    if (!argExpr) return true;

    // Construct the actual expected argument labels that our candidate
    // expected.
    assert(argumentTy &&
           "Candidate must expect an argument to have a label mismatch");
    auto arguments = decomposeArgParamType(argumentTy);
    
    // TODO: This is probably wrong for varargs, e.g. calling "print" with the
    // wrong label.
    SmallVector<Identifier, 4> expectedNames;
    for (auto &arg : arguments)
      expectedNames.push_back(arg.Label);

    return diagnoseArgumentLabelError(CS->TC, argExpr, expectedNames,
                                      /*isSubscript*/false);
  }
    
  case CC_GeneralMismatch:        // Something else is wrong.
  case CC_ArgumentCountMismatch:  // This candidate has wrong # arguments.
    // If we have no argument, the candidates must have expected one.
    if (!E->getArgument()) {
      if (!argumentTy)
        return false; // Candidate must be incorrect for some other reason.
      
      // Pick one of the arguments that are expected as an exemplar.
      diagnose(E->getNameLoc(), diag::expected_argument_in_contextual_member,
               E->getName(), argumentTy);
      return true;
    }
     
    // If an argument value was specified, but this is a simple enumerator, then
    // we fail with a nice error message.
    auto argTy = candidateInfo[0].getArgumentType();
    if (!argTy) {
      diagnose(E->getNameLoc(), diag::unexpected_argument_in_contextual_member,
               E->getName());
      return true;
    }

    assert(E->getArgument() && argTy && "Exact match without an argument?");
    return !typeCheckArgumentChildIndependently(E->getArgument(), argTy,
                                                candidateInfo);
  }

  llvm_unreachable("all cases should be handled");
}


/// A TupleExpr propagate contextual type information down to its children and
/// can be erroneous when there is a label mismatch etc.
bool FailureDiagnosis::visitTupleExpr(TupleExpr *TE) {
  // If we know the requested argType to use, use computeTupleShuffle to produce
  // the shuffle of input arguments to destination values.  It requires a
  // TupleType to compute the mapping from argExpr.  Conveniently, it doesn't
  // care about the actual types though, so we can just use 'void' for them.
  if (!CS->getContextualType() || !CS->getContextualType()->is<TupleType>())
    return visitExpr(TE);
  
  auto contextualTT = CS->getContextualType()->castTo<TupleType>();
  
  SmallVector<TupleTypeElt, 4> ArgElts;
  auto voidTy = CS->getASTContext().TheEmptyTupleType;

  for (unsigned i = 0, e = TE->getNumElements(); i != e; ++i)
    ArgElts.push_back({ voidTy, TE->getElementName(i) });
  auto TEType = TupleType::get(ArgElts, CS->getASTContext());
  
  if (!TEType->is<TupleType>())
    return visitExpr(TE);

  SmallVector<int, 4> sources;
  SmallVector<unsigned, 4> variadicArgs;
  
  // If the shuffle is invalid, then there is a type error.  We could diagnose
  // it specifically here, but the general logic does a fine job so we let it
  // do it.
  if (computeTupleShuffle(TEType->castTo<TupleType>()->getElements(),
                          contextualTT->getElements(), sources, variadicArgs))
    return visitExpr(TE);

  // If we got a correct shuffle, we can perform the analysis of all of
  // the input elements, with their expected types.
  for (unsigned i = 0, e = sources.size(); i != e; ++i) {
    // If the value is taken from a default argument, ignore it.
    if (sources[i] == TupleShuffleExpr::DefaultInitialize ||
        sources[i] == TupleShuffleExpr::Variadic ||
        sources[i] == TupleShuffleExpr::CallerDefaultInitialize)
      continue;
    
    assert(sources[i] >= 0 && "Unknown sources index");
    
    // Otherwise, it must match the corresponding expected argument type.
    unsigned inArgNo = sources[i];
    auto actualType = contextualTT->getElementType(i);
    
    TCCOptions options;
    if (actualType->is<InOutType>())
      options |= TCC_AllowLValue;
    
    auto exprResult =
      typeCheckChildIndependently(TE->getElement(inArgNo), actualType,
                                  CS->getContextualTypePurpose(), options);
    // If there was an error type checking this argument, then we're done.
    if (!exprResult) return true;
    
    // If the caller expected something inout, but we didn't have
    // something of inout type, diagnose it.
    if (auto IOE =
          dyn_cast<InOutExpr>(exprResult->getSemanticsProvidingExpr())) {
      if (!actualType->is<InOutType>()) {
        diagnose(exprResult->getLoc(), diag::extra_address_of,
                 exprResult->getType()->getInOutObjectType())
        .highlight(exprResult->getSourceRange())
        .fixItRemove(IOE->getStartLoc());
        return true;
      }
    }
  }
  
  if (!variadicArgs.empty()) {
    auto varargsTy = contextualTT->getVarArgsBaseType();
    for (unsigned i = 0, e = variadicArgs.size(); i != e; ++i) {
      unsigned inArgNo = variadicArgs[i];
      
      auto expr =
        typeCheckChildIndependently(TE->getElement(inArgNo), varargsTy,
                                    CS->getContextualTypePurpose());
      // If there was an error type checking this argument, then we're done.
      if (!expr) return true;
    }
  }
  
  return false;
}

/// An IdentityExpr doesn't change its argument, but it *can* propagate its
/// contextual type information down.
bool FailureDiagnosis::visitIdentityExpr(IdentityExpr *E) {
  auto contextualType = CS->getContextualType();
  
  // If we have a paren expr and our contextual type is a ParenType, remove the
  // paren expr sugar.
  if (isa<ParenExpr>(E) && contextualType)
    if (auto *PT = dyn_cast<ParenType>(contextualType.getPointer()))
      contextualType = PT->getUnderlyingType();
  
  if (!typeCheckChildIndependently(E->getSubExpr(), contextualType,
                                   CS->getContextualTypePurpose()))
    return true;
  return false;
}

bool FailureDiagnosis::visitExpr(Expr *E) {
  // Check each of our immediate children to see if any of them are
  // independently invalid.
  bool errorInSubExpr = false;
  
  E->forEachImmediateChildExpr([&](Expr *Child) -> Expr* {
    // If we already found an error, stop checking.
    if (errorInSubExpr) return Child;

    // Otherwise just type check the subexpression independently.  If that
    // succeeds, then we stitch the result back into our expression.
    if (typeCheckChildIndependently(Child, TCC_AllowLValue))
      return Child;

    // Otherwise, it failed, which emitted a diagnostic.  Keep track of this
    // so that we don't emit multiple diagnostics.
    errorInSubExpr = true;
    return Child;
  });
  
  // If any of the children were errors, we're done.
  if (errorInSubExpr)
    return true;
  
  // Otherwise, produce a more generic error.
  return false;
}


bool FailureDiagnosis::diagnoseExprFailure() {
  assert(CS && expr);

  // Our general approach is to do a depth first traversal of the broken
  // expression tree, type checking as we go.  If we find a subtree that cannot
  // be type checked on its own (even to an incomplete type) then that is where
  // we focus our attention.  If we do find a type, we use it to check for
  // contextual type mismatches.
  return visit(expr);
}


/// Given a specific expression and the remnants of the failed constraint
/// system, produce a specific diagnostic.
///
/// This is guaranteed to always emit an error message.
///
void ConstraintSystem::diagnoseFailureForExpr(Expr *expr) {
  // Continue simplifying any active constraints left in the system.  We can end
  // up with them because the solver bails out as soon as it sees a Failure.  We
  // don't want to leave them around in the system because later diagnostics
  // will assume they are unsolvable and may otherwise leave the system in an
  // inconsistent state.
  simplify(/*ContinueAfterFailures*/true);

  // Look through RebindSelfInConstructorExpr to avoid weird sema issues.
  if (auto *RB = dyn_cast<RebindSelfInConstructorExpr>(expr))
    expr = RB->getSubExpr();
  
  FailureDiagnosis diagnosis(expr, this);
  
  // Now, attempt to diagnose the failure from the info we've collected.
  if (diagnosis.diagnoseExprFailure())
    return;

  // If this is a contextual conversion problem, dig out some information.
  if (diagnosis.diagnoseContextualConversionError())
    return;

  // If we can diagnose a problem based on the constraints left laying around in
  // the system, do so now.
  if (diagnosis.diagnoseConstraintFailure())
    return;

  // If no one could find a problem with this expression or constraint system,
  // then it must be well-formed... but is ambiguous.  Handle this by diagnostic
  // various cases that come up.
  diagnosis.diagnoseAmbiguity(expr);
}

static void noteArchetypeSource(const TypeLoc &loc, ArchetypeType *archetype,
                                TypeChecker &tc) {
  GenericTypeDecl *FoundDecl = nullptr;
  
  // Walk the TypeRepr to find the type in question.
  if (auto typerepr = loc.getTypeRepr()) {
    struct FindGenericTypeDecl : public ASTWalker {
      GenericTypeDecl *&FoundDecl;
      FindGenericTypeDecl(GenericTypeDecl *&FoundDecl) : FoundDecl(FoundDecl) {
      }
      
      bool walkToTypeReprPre(TypeRepr *T) override {
        // If we already emitted the note, we're done.
        if (FoundDecl) return false;
        
        if (auto ident = dyn_cast<ComponentIdentTypeRepr>(T))
          FoundDecl = dyn_cast_or_null<GenericTypeDecl>(ident->getBoundDecl());
        // Keep walking.
        return true;
      }
    } findGenericTypeDecl(FoundDecl);
    
    typerepr->walk(findGenericTypeDecl);
  }
  
  // If we didn't find the type in the TypeRepr, fall back to the type in the
  // type checked expression.
  if (!FoundDecl)
    FoundDecl = loc.getType()->getAnyGeneric();
  
  if (FoundDecl)
    tc.diagnose(FoundDecl, diag::archetype_declared_in_type, archetype,
                FoundDecl->getDeclaredType());
}


/// Emit an error message about an unbound generic parameter existing, and
/// emit notes referring to the target of a diagnostic, e.g., the function
/// or parameter being used.
static void diagnoseUnboundArchetype(Expr *overallExpr,
                                     ArchetypeType *archetype,
                                     ConstraintLocator *targetLocator,
                                     ConstraintSystem &cs) {
  auto &tc = cs.getTypeChecker();
  auto anchor = targetLocator->getAnchor();

  // The archetype may come from the explicit type in a cast expression.
  if (auto *ECE = dyn_cast_or_null<ExplicitCastExpr>(anchor)) {
    tc.diagnose(ECE->getLoc(), diag::unbound_generic_parameter_cast,
                archetype, ECE->getCastTypeLoc().getType())
      .highlight(ECE->getCastTypeLoc().getSourceRange());

    // Emit a note specifying where this came from, if we can find it.
    noteArchetypeSource(ECE->getCastTypeLoc(), archetype, tc);
    if (auto *ND = ECE->getCastTypeLoc().getType()
          ->getNominalOrBoundGenericNominal())
      tc.diagnose(ND, diag::archetype_declared_in_type, archetype,
                  ND->getDeclaredType());
    return;
  }
  
  // Otherwise, emit an error message on the expr we have, and emit a note
  // about where the archetype came from.
  tc.diagnose(overallExpr->getLoc(), diag::unbound_generic_parameter,
              archetype);
  
  // If we have an anchor, drill into it to emit a
  // "note: archetype declared here".
  if (!anchor) return;


  if (auto TE = dyn_cast<TypeExpr>(anchor)) {
    // Emit a note specifying where this came from, if we can find it.
    noteArchetypeSource(TE->getTypeLoc(), archetype, tc);
    return;
  }

  ConcreteDeclRef resolved;
  
  // Simple case: direct reference to a declaration.
  if (auto dre = dyn_cast<DeclRefExpr>(anchor))
    resolved = dre->getDeclRef();
  
  // Simple case: direct reference to a declaration.
  if (auto MRE = dyn_cast<MemberRefExpr>(anchor))
    resolved = MRE->getMember();
  
  if (auto OCDRE = dyn_cast<OtherConstructorDeclRefExpr>(anchor))
    resolved = OCDRE->getDeclRef();

  
  // We couldn't resolve the locator to a declaration, so we're done.
  if (!resolved)
    return;
  
  auto decl = resolved.getDecl();
  if (isa<FuncDecl>(decl)) {
    auto name = decl->getName();
    auto diagID = name.isOperator() ? diag::note_call_to_operator
    : diag::note_call_to_func;
    tc.diagnose(decl, diagID, name);
    return;
  }
  
  // FIXME: Specialize for implicitly-generated constructors.
  if (isa<ConstructorDecl>(decl)) {
    tc.diagnose(decl, diag::note_call_to_initializer);
    return;
  }
  
  if (isa<ParamDecl>(decl)) {
    tc.diagnose(decl, diag::note_init_parameter, decl->getName());
    return;
  }
  
  // FIXME: Other decl types too.
}


/// Emit an ambiguity diagnostic about the specified expression.
void FailureDiagnosis::diagnoseAmbiguity(Expr *E) {

  // Check out all of the type variables lurking in the system.  If any are
  // unbound archetypes, then the problem is that it couldn't be resolved.
  for (auto tv : CS->getTypeVariables()) {
    if (tv->getImpl().hasRepresentativeOrFixed())
      continue;
    
    
    // If this is a conversion to a type variable used to form an archetype,
    // Then diagnose this as a generic parameter that could not be resolved.
    auto archetype = tv->getImpl().getArchetype();
    
    // Only diagnose archetypes that don't have a parent, i.e., ones
    // that correspond to generic parameters.
    if (archetype && !archetype->getParent()) {
      diagnoseUnboundArchetype(expr, archetype, tv->getImpl().getLocator(),*CS);
      return;
    }
    continue;
  }

  // Unresolved/Anonymous ClosureExprs are common enough that we should give
  // them tailored diagnostics.
  if (auto CE = dyn_cast<ClosureExpr>(E->getValueProvidingExpr())) {
    auto CFTy = CE->getType()->getAs<AnyFunctionType>();
    
    // If this is a multi-statement closure with no explicit result type, emit
    // a note to clue the developer in.
    if (!CE->hasExplicitResultType() && CFTy &&
        isUnresolvedOrTypeVarType(CFTy->getResult())) {
      diagnose(CE->getLoc(), diag::cannot_infer_closure_result_type);
      return;
    }
    
    diagnose(E->getLoc(), diag::cannot_infer_closure_type)
      .highlight(E->getSourceRange());
    return;
  }

  // A DiscardAssignmentExpr (spelled "_") needs contextual type information to
  // infer its type. If we see one at top level, diagnose that it must be part
  // of an assignment so we don't get a generic "expression is ambiguous" error.
  if (isa<DiscardAssignmentExpr>(E)) {
    diagnose(E->getLoc(), diag::discard_expr_outside_of_assignment)
      .highlight(E->getSourceRange());
    return;
  }
  
  // Diagnose ".foo" expressions that lack context specifically.
  if (auto UME =
        dyn_cast<UnresolvedMemberExpr>(E->getSemanticsProvidingExpr())) {
    if (!CS->getContextualType()) {
      diagnose(E->getLoc(), diag::unresolved_member_no_inference,UME->getName())
        .highlight(SourceRange(UME->getDotLoc(),
                               UME->getNameLoc().getSourceRange().End));
      return;
    }
  }
  
  // Diagnose empty collection literals that lack context specifically.
  if (auto CE = dyn_cast<CollectionExpr>(E->getSemanticsProvidingExpr())) {
    if (CE->getNumElements() == 0) {
      diagnose(E->getLoc(), diag::unresolved_collection_literal)
        .highlight(E->getSourceRange());
      return;
    }
  }

  // Diagnose 'nil' without a contextual type.
  if (isa<NilLiteralExpr>(E->getSemanticsProvidingExpr())) {
    diagnose(E->getLoc(), diag::unresolved_nil_literal)
      .highlight(E->getSourceRange());
    return;
  }

  
  // Attempt to re-type-check the entire expression, allowing ambiguity, but
  // ignoring a contextual type.
  if (expr == E) {
    auto exprType = getTypeOfTypeCheckedChildIndependently(expr);
    // If it failed and diagnosed something, then we're done.
    if (!exprType) return;

    // If we were able to find something more specific than "unknown" (perhaps
    // something like "[_:_]" for a dictionary literal), include it in the
    // diagnostic.
    if (!isUnresolvedOrTypeVarType(exprType)) {
      diagnose(E->getLoc(), diag::specific_type_of_expression_is_ambiguous,
               exprType)
        .highlight(E->getSourceRange());
      return;
    }
  }

  // If there are no posted constraints or failures, then there was
  // not enough contextual information available to infer a type for the
  // expression.
  diagnose(E->getLoc(), diag::type_of_expression_is_ambiguous)
    .highlight(E->getSourceRange());
}

bool ConstraintSystem::salvage(SmallVectorImpl<Solution> &viable, Expr *expr) {
  // Attempt to solve again, capturing all states that come from our attempts to
  // select overloads or bind type variables.
  //
  // FIXME: can this be removed?  We need to arrange for recordFixes to be
  // eliminated.
  viable.clear();

  {
    // Set up solver state.
    SolverState state(*this);
    state.recordFixes = true;
    this->solverState = &state;

    // Solve the system.
    solveRec(viable, FreeTypeVariableBinding::Disallow);

    // Check whether we have a best solution; this can happen if we found
    // a series of fixes that worked.
    if (auto best = findBestSolution(viable, /*minimize=*/true)) {
      if (*best != 0)
        viable[0] = std::move(viable[*best]);
      viable.erase(viable.begin() + 1, viable.end());
      return false;
    }

    // FIXME: If we were able to actually fix things along the way,
    // we may have to hunt for the best solution. For now, we don't care.

    // Remove solutions that require fixes; the fixes in those systems should
    // be diagnosed rather than any ambiguity.
    auto hasFixes = [](const Solution &sol) { return !sol.Fixes.empty(); };
    auto newEnd = std::remove_if(viable.begin(), viable.end(), hasFixes);
    viable.erase(newEnd, viable.end());

    // If there are multiple solutions, try to diagnose an ambiguity.
    if (viable.size() > 1) {
      if (getASTContext().LangOpts.DebugConstraintSolver) {
        auto &log = getASTContext().TypeCheckerDebug->getStream();
        log << "---Ambiguity error: "
            << viable.size() << " solutions found---\n";
        int i = 0;
        for (auto &solution : viable) {
          log << "---Ambiguous solution #" << i++ << "---\n";
          solution.dump(log);
          log << "\n";
        }
      }        

      if (diagnoseAmbiguity(*this, viable, expr)) {
        return true;
      }
    }

    // Remove the solver state.
    this->solverState = nullptr;

    // Fall through to produce diagnostics.
  }

  if (getExpressionTooComplex()) {
    TC.diagnose(expr->getLoc(), diag::expression_too_complex).
    highlight(expr->getSourceRange());
    return true;
  }

  // If all else fails, diagnose the failure by looking through the system's
  // constraints.
  diagnoseFailureForExpr(expr);
  return true;
}

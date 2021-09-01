//===--- ArgumentList.cpp - Function and subscript argument lists -*- C++ -*==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the logic for the Argument and ArgumentList classes.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ArgumentList.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ParameterList.h"

using namespace swift;

Type swift::__Expr_getType(Expr *E) { return E->getType(); }

SourceRange Argument::getSourceRange() const {
  auto labelLoc = getLabelLoc();
  if (labelLoc.isInvalid())
    return getExpr()->getSourceRange();

  auto exprEndLoc = getExpr()->getEndLoc();
  if (exprEndLoc.isInvalid())
    return labelLoc;

  return SourceRange(labelLoc, exprEndLoc);
}

bool Argument::isInOut() const {
  return ArgExpr->isSemanticallyInOutExpr();
}

/// Attempt to extract the underlying array expr from an implicit variadic
/// expansion expr.
static ArrayExpr *getVariadicArrayFrom(const Expr *E) {
  auto *vargExpr = dyn_cast<VarargExpansionExpr>(E);
  if (!vargExpr || !vargExpr->isImplicit())
    return nullptr;
  auto *vargArray = dyn_cast<ArrayExpr>(vargExpr->getSubExpr());
  if (!vargArray || !vargArray->isImplicit())
    return nullptr;
  return vargArray;
}

namespace {
struct TrailingClosureInfo {
  Optional<unsigned> ArgIndex;
  bool HasMultiple;

  TrailingClosureInfo() : HasMultiple(false) {}
  TrailingClosureInfo(Optional<unsigned> argIndex, bool hasMultiple)
      : ArgIndex(argIndex), HasMultiple(hasMultiple) {}
};
} // end anonymous namespace

static bool argExprHasValidSourceRange(const Expr *expr) {
  // Default arguments may have source info, but it's not a valid argument
  // source range.
  return !isa<DefaultArgumentExpr>(expr) && expr->getSourceRange().isValid();
}

/// Whether a given argument expression in a non-implicit arg list is either a
/// trailing closure, or is a variadic expansion containing a trailing closure.
static bool isTrailingArgOfNonImplicitArgList(const Expr *argExpr,
                                              SourceLoc rparenLoc) {
  // If the argument has no valid source info, we can't count it as a trailing
  // closure.
  if (!argExprHasValidSourceRange(argExpr))
    return false;

  // If the right paren loc is invalid, this is something like 'fn {}'.
  if (rparenLoc.isInvalid())
    return true;

  // Otherwise the argument must come after the r-paren. Note we check the end
  // loc of the argument to match against variadic args that contain both
  // non-trailing and trailing closures.
  return rparenLoc.getOpaquePointerValue() <
         argExpr->getEndLoc().getOpaquePointerValue();
}

static TrailingClosureInfo
computeTrailingClosureInfo(ArrayRef<Argument> args, SourceLoc rparenLoc,
                           bool isImplicit) {
  // Implicit argument lists never have trailing closures.
  if (isImplicit)
    return {};

  auto iter = llvm::find_if(args, [&](const auto &arg) {
    return isTrailingArgOfNonImplicitArgList(arg.getExpr(), rparenLoc);
  });
  if (iter == args.end()) {
    assert(rparenLoc.isValid() &&
           "Explicit argument list with no parens and no trailing closures?");
    return {};
  }
  auto argIdx = iter - args.begin();

  // For the variadic case we need to dig into the variadic array to figure out
  // if we have multiple trailing closures.
  if (auto *array = getVariadicArrayFrom(args[argIdx].getExpr())) {
    auto numTrailing = llvm::count_if(array->getElements(), [&](auto *expr) {
      return isTrailingArgOfNonImplicitArgList(expr, rparenLoc);
    });
    assert(numTrailing > 0 && "Variadic args didn't have trailing closure?");
    if (numTrailing > 1)
      return TrailingClosureInfo(argIdx, /*HasMultiple*/ true);
  }

  // Otherwise, look for the next trailing closure if any.
  auto restOfArgs = args.drop_front(argIdx + 1);
  auto nextIter = llvm::find_if(restOfArgs, [&](const auto &arg) {
    return isTrailingArgOfNonImplicitArgList(arg.getExpr(), rparenLoc);
  });
  return TrailingClosureInfo(argIdx, /*HasMultiple*/ nextIter != args.end());
}

ArgumentList *ArgumentList::create(ASTContext &ctx, SourceLoc lParenLoc,
                                   ArrayRef<Argument> args, SourceLoc rParenLoc,
                                   Optional<unsigned> firstTrailingClosureIndex,
                                   bool hasMultipleTrailingClosures,
                                   bool isImplicit, AllocationArena arena) {
#ifndef NDEBUG
  auto trailingInfo = computeTrailingClosureInfo(args, rParenLoc, isImplicit);
  assert(trailingInfo.ArgIndex == firstTrailingClosureIndex);
  assert(trailingInfo.HasMultiple == hasMultipleTrailingClosures);
#endif

  assert(lParenLoc.isValid() == rParenLoc.isValid());
  SmallVector<Expr *, 4> exprs;
  SmallVector<Identifier, 4> labels;
  SmallVector<SourceLoc, 4> labelLocs;

  bool hasLabels = false;
  bool hasLabelLocs = false;
  for (auto &arg : args) {
    exprs.push_back(arg.getExpr());

    hasLabels |= !arg.getLabel().empty();
    labels.push_back(arg.getLabel());

    hasLabelLocs |= arg.getLabelLoc().isValid();
    labelLocs.push_back(arg.getLabelLoc());
  }
  if (!hasLabels)
    labels.clear();
  if (!hasLabelLocs)
    labelLocs.clear();

  auto numBytes = totalSizeToAlloc<Expr *, Identifier, SourceLoc>(
      exprs.size(), labels.size(), labelLocs.size());
  auto *mem = ctx.Allocate(numBytes, alignof(ArgumentList), arena);
  auto *argList = new (mem) ArgumentList(
      lParenLoc, rParenLoc, args.size(), firstTrailingClosureIndex,
      hasMultipleTrailingClosures, isImplicit, hasLabels, hasLabelLocs);

  std::uninitialized_copy(exprs.begin(), exprs.end(),
                          argList->getExprsBuffer().begin());
  if (hasLabels) {
    std::uninitialized_copy(labels.begin(), labels.end(),
                            argList->getLabelsBuffer().begin());
  }
  if (hasLabelLocs) {
    std::uninitialized_copy(labelLocs.begin(), labelLocs.end(),
                            argList->getLabelLocsBuffer().begin());
  }
  return argList;
}

ArgumentList *ArgumentList::create(ASTContext &ctx, SourceLoc lParenLoc,
                                   ArrayRef<Argument> args, SourceLoc rParenLoc,
                                   bool isImplicit, AllocationArena arena) {
  auto trailingClosureInfo =
      computeTrailingClosureInfo(args, rParenLoc, isImplicit);
  return ArgumentList::create(
      ctx, lParenLoc, args, rParenLoc, trailingClosureInfo.ArgIndex,
      trailingClosureInfo.HasMultiple, isImplicit, arena);
}

ArgumentList *ArgumentList::createImplicit(ASTContext &ctx, SourceLoc lParenLoc,
                                           ArrayRef<Argument> args,
                                           SourceLoc rParenLoc,
                                           AllocationArena arena) {
  return create(ctx, lParenLoc, args, rParenLoc, /*implicit*/ true, arena);
}

ArgumentList *ArgumentList::createImplicit(ASTContext &ctx,
                                           ArrayRef<Argument> args,
                                           AllocationArena arena) {
  return createImplicit(ctx, SourceLoc(), args, SourceLoc());
}

ArgumentList *ArgumentList::forImplicitSingle(ASTContext &ctx, Identifier label,
                                              Expr *arg) {
  return createImplicit(ctx, {Argument(SourceLoc(), label, arg)});
}

ArgumentList *ArgumentList::forImplicitUnlabeled(ASTContext &ctx,
                                                 ArrayRef<Expr *> argExprs) {
  SmallVector<Argument, 4> args;
  for (auto *argExpr : argExprs)
    args.push_back(Argument::unlabeled(argExpr));
  return createImplicit(ctx, args);
}

ArgumentList *ArgumentList::forImplicitCallTo(DeclNameRef fnNameRef,
                                              ArrayRef<Expr *> argExprs,
                                              ASTContext &ctx) {
  auto labels = fnNameRef.getArgumentNames();
  assert(labels.size() == argExprs.size());

  SmallVector<Argument, 8> args;
  for (auto idx : indices(argExprs))
    args.emplace_back(SourceLoc(), labels[idx], argExprs[idx]);

  return createImplicit(ctx, args);
}

ArgumentList *ArgumentList::forImplicitCallTo(ParameterList *params,
                                              ArrayRef<Expr *> argExprs,
                                              ASTContext &ctx) {
  assert(params->size() == argExprs.size());
  SmallVector<Argument, 8> args;
  for (auto idx : indices(argExprs)) {
    auto *param = params->get(idx);
    assert(param->isInOut() == argExprs[idx]->isSemanticallyInOutExpr());
    args.emplace_back(SourceLoc(), param->getArgumentName(), argExprs[idx]);
  }
  return createImplicit(ctx, args);
}

SourceLoc ArgumentList::getLoc() const {
  // If we have an unlabeled unary arg, return the start loc of the expr. This
  // preserves the behavior of when such argument lists were represented by
  // ParenExprs.
  if (auto *unary = getUnlabeledUnaryExpr())
    return unary->getStartLoc();
  return getStartLoc();
}

SourceRange ArgumentList::getSourceRange() const {
  auto start = LParenLoc;
  if (start.isInvalid()) {
    // Scan forward for the first valid source loc.
    for (auto arg : *this) {
      start = arg.getStartLoc();
      if (start.isValid())
        break;
    }
  }
  auto end = RParenLoc;
  if (hasAnyTrailingClosures()) {
    // If we have trailing closures, the end loc is the end loc of the last
    // trailing closure.
    for (auto arg : llvm::reverse(*this)) {
      if (isTrailingArgOfNonImplicitArgList(arg.getExpr(), RParenLoc)) {
        end = arg.getEndLoc();
        break;
      }
    }
  } else if (RParenLoc.isInvalid()) {
    // If we don't have trailing closures and the r-paren loc is invalid, this
    // is an implicit argument list. Take the last valid argument loc.
    assert(isImplicit());
    for (auto arg : llvm::reverse(*this)) {
      if (argExprHasValidSourceRange(arg.getExpr())) {
        end = arg.getEndLoc();
        break;
      }
    }
  }
  if (start.isInvalid() || end.isInvalid())
    return SourceRange();

  return SourceRange(start, end);
}

ArrayRef<Identifier>
ArgumentList::getArgumentLabels(SmallVectorImpl<Identifier> &scratch) const {
  assert(scratch.empty());
  if (HasLabels)
    return getLabelsBuffer();
  scratch.append(size(), Identifier());
  return scratch;
}

Optional<unsigned> ArgumentList::findArgumentExpr(Expr *expr,
                                                  bool allowSemantic) const {
  if (allowSemantic)
    expr = expr->getSemanticsProvidingExpr();
  for (auto idx : indices(*this)) {
    auto *argExpr = getExpr(idx);
    if (allowSemantic)
      argExpr = argExpr->getSemanticsProvidingExpr();

    if (expr == argExpr)
      return idx;
  }
  return None;
}

Expr *ArgumentList::packIntoImplicitTupleOrParen(
    ASTContext &ctx, llvm::function_ref<Type(Expr *)> getType) const {
  assert(!hasAnyInOutArgs() && "Cannot construct bare tuple/paren with inout");
  if (auto *unary = getUnlabeledUnaryExpr()) {
    auto *paren = new (ctx) ParenExpr(SourceLoc(), unary, SourceLoc());
    if (auto ty = getType(unary))
      paren->setType(ParenType::get(ctx, ty));
    paren->setImplicit();
    return paren;
  }

  SmallVector<Expr *, 8> argExprs;
  SmallVector<Identifier, 8> argLabels;
  SmallVector<TupleTypeElt, 8> tupleEltTypes;

  for (auto arg : *this) {
    auto *argExpr = arg.getExpr();
    argExprs.push_back(argExpr);
    argLabels.push_back(arg.getLabel());
    if (auto ty = getType(argExpr))
      tupleEltTypes.emplace_back(ty, arg.getLabel());
  }
  assert(tupleEltTypes.empty() || tupleEltTypes.size() == argExprs.size());

  auto *tuple = TupleExpr::createImplicit(ctx, argExprs, argLabels);
  if (empty() || !tupleEltTypes.empty())
    tuple->setType(TupleType::get(tupleEltTypes, ctx));

  return tuple;
}

Type ArgumentList::composeTupleOrParenType(
    ASTContext &ctx, llvm::function_ref<Type(Expr *)> getType) const {
  if (auto *unary = getUnlabeledUnaryExpr()) {
    auto ty = getType(unary);
    assert(ty);
    ParameterTypeFlags flags;
    if (get(0).isInOut()) {
      ty = ty->getInOutObjectType();
      flags = flags.withInOut(true);
    }
    return ParenType::get(ctx, ty, flags);
  }
  SmallVector<TupleTypeElt, 4> elts;
  for (auto arg : *this) {
    auto ty = getType(arg.getExpr());
    assert(ty);
    ParameterTypeFlags flags;
    if (arg.isInOut()) {
      ty = ty->getInOutObjectType();
      flags = flags.withInOut(true);
    }
    elts.emplace_back(ty, arg.getLabel(), flags);
  }
  return TupleType::get(elts, ctx);
}

bool ArgumentList::matches(ArrayRef<AnyFunctionType::Param> params,
                           llvm::function_ref<Type(Expr *)> getType) const {
  if (size() != params.size())
    return false;

  for (auto i : indices(*this)) {
    auto arg = get(i);
    auto &param = params[i];

    if (arg.getLabel() != param.getLabel())
      return false;

    if (arg.isInOut() != param.isInOut())
      return false;

    auto argTy = getType(arg.getExpr());
    assert(argTy && "Expected type for argument");
    auto paramTy = param.getParameterType();
    assert(paramTy && "Expected a type for param");

    if (arg.isInOut())
      argTy = argTy->getInOutObjectType();

    if (!argTy->isEqual(paramTy))
      return false;
  }
  return true;
}

OriginalArguments ArgumentList::getOriginalArguments() const {
  // We need to sort out the trailing closures separately to handle cases like:
  //
  // func foo(fn: () -> Void, x: Int = 0) {}
  // foo(x: 0) {}
  //
  // where we currently allow the re-ordering of a trailing closure argument
  // such that it appears as the first argument in the type-checked AST. To
  // remedy this, separate out the trailing closures and make sure to append
  // them after the regular arguments.
  OriginalArguments::Storage newArgs;
  SmallVector<Argument, 1> trailingClosures;

  auto addArg = [&](Argument arg) {
    if (hasAnyTrailingClosures() &&
        isTrailingArgOfNonImplicitArgList(arg.getExpr(), getRParenLoc())) {
      trailingClosures.push_back(arg);
      return;
    }
    newArgs.push_back(arg);
  };
  for (auto arg : *this) {
    auto *expr = arg.getExpr();
    if (isa<DefaultArgumentExpr>(expr))
      continue;

    if (auto *vargArray = getVariadicArrayFrom(expr)) {
      auto elts = vargArray->getElements();
      for (auto idx : indices(elts)) {
        // The first element in a variadic expansion takes the argument label,
        // the rest are unlabeled.
        if (idx == 0) {
          addArg(Argument(arg.getLabelLoc(), arg.getLabel(), elts[idx]));
        } else {
          addArg(Argument::unlabeled(elts[idx]));
        }
      }
      continue;
    }
    addArg(arg);
  }
  Optional<unsigned> trailingClosureIdx;
  if (!trailingClosures.empty())
    trailingClosureIdx = newArgs.size();

  newArgs.append(trailingClosures.begin(), trailingClosures.end());
  auto origArgs = OriginalArguments(std::move(newArgs), trailingClosureIdx);
#ifndef NDEBUG
  auto trailingInfo = computeTrailingClosureInfo(origArgs.getArray(),
                                                 getRParenLoc(), isImplicit());
  assert(trailingInfo.ArgIndex == trailingClosureIdx);
  assert(trailingInfo.HasMultiple == origArgs.hasMultipleTrailingClosures());
#endif
  return origArgs;
}

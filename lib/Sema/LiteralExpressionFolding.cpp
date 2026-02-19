//===--- LiteralExpressionFolding.cpp - -------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Simple AST-based evaluator of supported literal expressions
//
//===----------------------------------------------------------------------===//

#include "LiteralExpressionFolding.h"
#include "MiscDiagnostics.h"
#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Unreachable.h"

using namespace swift;
using namespace LiteralExprFolding;

namespace {

class FoldingError : public llvm::ErrorInfo<FoldingError> {
public:
  static char ID;
  IllegalConstError code;
  SourceLoc sourceLocation;
  FoldingError(IllegalConstError code) : code(code), sourceLocation() {}
  FoldingError(IllegalConstError code, SourceLoc loc)
      : code(code), sourceLocation(loc) {}
  void log(llvm::raw_ostream &OS) const override {
    OS << "Const Folding Error: "
       << static_cast<std::underlying_type<IllegalConstError>::type>(code);
  }
  std::error_code convertToErrorCode() const override {
    return llvm::inconvertibleErrorCode();
  }
};
char FoldingError::ID = 0;

static unsigned getTargetPointerBitWidth(ASTContext &ctx) {
  // Matching the compiler's determination of the
  // `#if _pointerBitWidth` value.
  if (ctx.LangOpts.Target.isArch16Bit())
    return 16;
  else if (ctx.LangOpts.Target.isArch32Bit())
    return 32;
  else if (ctx.LangOpts.Target.isArch64Bit())
    return 64;
  swift_unreachable("Unsupported platform kind.");
}

/// Get the bitwidth for a Swift integer type for constant folding.
/// Returns 0 if the type is not a known integer type.
static unsigned getIntegerBitWidth(Type type, ASTContext &ctx) {
  assert(type->isStdlibInteger());
  // Map stdlib integer types to their bitwidths
  if (type->isInt())
    return getTargetPointerBitWidth(ctx);
  if (type->isInt64())
    return 64;
  if (type->isInt32())
    return 32;
  if (type->isInt16())
    return 16;
  if (type->isInt8())
    return 8;

  if (type->isUInt())
    return getTargetPointerBitWidth(ctx);
  if (type->isUInt64())
    return 64;
  if (type->isUInt32())
    return 32;
  if (type->isUInt16())
    return 16;
  if (type->isUInt8())
    return 8;

  swift_unreachable("Unsupported integer type.");
}

static ConcreteDeclRef getIntTypeBuiltinInit(Type type, ASTContext &ctx) {
  assert(type->isStdlibInteger());
  // Map stdlib integer types to their bitwidths
  if (type->isInt())
    return ctx.getIntBuiltinInitDecl(ctx.getIntDecl());
  if (type->isInt64())
    return ctx.getIntBuiltinInitDecl(ctx.getInt64Decl());
  if (type->isInt32())
    return ctx.getIntBuiltinInitDecl(ctx.getInt32Decl());
  if (type->isInt16())
    return ctx.getIntBuiltinInitDecl(ctx.getInt16Decl());
  if (type->isInt8())
    return ctx.getIntBuiltinInitDecl(ctx.getInt8Decl());

  if (type->isUInt())
    return ctx.getIntBuiltinInitDecl(ctx.getUIntDecl());
  if (type->isUInt64())
    return ctx.getIntBuiltinInitDecl(ctx.getUInt64Decl());
  if (type->isUInt32())
    return ctx.getIntBuiltinInitDecl(ctx.getUInt32Decl());
  if (type->isUInt16())
    return ctx.getIntBuiltinInitDecl(ctx.getUInt16Decl());
  if (type->isUInt8())
    return ctx.getIntBuiltinInitDecl(ctx.getUInt8Decl());

  swift_unreachable("Unsupported integer type.");
}

/// Check if the type is a signed integer type
static bool isSignedIntegerType(Type type) {
  return type->isInt() || type->isInt64() || type->isInt32() ||
         type->isInt16() || type->isInt8();
}

/// A move-only value that's either a `FoldingError` or some parameterized type
/// value `T`. A convenience wrapper around `TaggedUnion<T, FoldingError>`.
template <typename T>
class FoldingErrorOr {
  TaggedUnion<T, FoldingError> Value;

public:
  FoldingErrorOr() : Value(FoldingError(IllegalConstError::Default)) {}
  FoldingErrorOr(T &&t) : Value(std::move(t)) {}
  FoldingErrorOr(const FoldingError &fe) : Value(fe) {}

  FoldingErrorOr(FoldingErrorOr &&other) : Value(std::move(other.Value)) {}
  FoldingErrorOr &operator=(FoldingErrorOr &&other) noexcept {
    if (this != &other)
      Value = std::move(other.Value);
    return *this;
  }

  FoldingErrorOr(const FoldingErrorOr &) = delete;
  FoldingErrorOr &operator=(const FoldingErrorOr &) = delete;

  const T *operator->() const { return Value.template dyn_cast<T>(); }
  FoldingError getError() const {
    return *Value.template dyn_cast<FoldingError>();
  }
  bool isError() const {
    return Value.template dyn_cast<FoldingError>() != nullptr;
  }

  /// Return false if there is an error.
  explicit operator bool() const { return !isError(); }
};

class ConstantValue {
public:
  enum class ConstantValueKind : int8_t {
    FirstKind,
    Integer = FirstKind,
    FloatingPoint,
    LastKind = FloatingPoint + 1
  };

  const ConstantValueKind kind;
  ConstantValue(ConstantValueKind kind) : kind(kind) {}
  virtual ~ConstantValue() = default;
  ConstantValueKind getKind() const { return kind; }
};
using ConstantValuePtr = std::unique_ptr<ConstantValue>;

class IntegerValue : public ConstantValue {
  APInt value;
  bool isSigned;

public:
  IntegerValue(APInt value, bool isSigned)
      : ConstantValue(ConstantValueKind::Integer), value(value),
        isSigned(isSigned) {}

  APInt getValue() const { return value; }
  bool getIsSigned() const { return isSigned; }

  static bool classof(const ConstantValue *base) {
    return base->getKind() == ConstantValueKind::Integer;
  }
};

/// A simple constant expression folder to simplify
/// binary expressions of integer type consisting of literal
/// value operands.
class ConstantFolder {
  ASTContext &Ctx;

public:
  ConstantFolder(ASTContext &ctx) : Ctx(ctx) {}
  Expr *fold(const Expr *expr) {
    // If this expression failed to type-check, no need to attempt to
    // fold it since we likely won't be able to do anything meaningful
    // here.
    if (!expr->getType() || expr->getType()->getAs<ErrorType>()) {
      emitFoldingErrorDiagnostic(
          FoldingError(IllegalConstError::UpstreamError, expr->getStartLoc()));
      return nullptr;
    }

    ConstantWalker walker(Ctx);
    const_cast<Expr *>(expr)->walk(walker);
    ASSERT(walker.hasConstantValueFor(expr) &&
           "No value or error computed by constant-folding AST walker");
    const auto &result = walker.getConstantValueOrErrorFor(expr);
    if (result)
      return createIntegerLiteralExpr(expr, result->get());
    else {
      emitFoldingErrorDiagnostic(result.getError());
      return nullptr;
    }
  }

private:
  class ConstantWalker : public ASTWalker {
    ASTContext &Ctx;
    llvm::DenseMap<Expr *, FoldingErrorOr<ConstantValuePtr>>
        ConstValuesOrErrors;
  public:
    ConstantWalker(ASTContext &ctx) : Ctx(ctx) {}

    PostWalkResult<Expr *> walkToExprPost(Expr *expr) override {
      ConstValuesOrErrors.insert({expr, tryFoldExpression(expr)});
      return Action::Continue(expr);
    }

    bool hasConstantValueFor(const Expr *expr) {
      return ConstValuesOrErrors.contains(expr);
    }

    const FoldingErrorOr<ConstantValuePtr> &
    getConstantValueOrErrorFor(const Expr *expr) {
      ASSERT(ConstValuesOrErrors.contains(expr) &&
             "Querying constant value for an unfolded expression.");
      return ConstValuesOrErrors.at(expr);
    }

  private:
    FoldingErrorOr<ConstantValuePtr> tryFoldExpression(const Expr *expr) {
      if (auto *literalExpr = dyn_cast<LiteralExpr>(expr))
        return tryFoldLiteralExpression(literalExpr);
      else if (auto *binaryExpr = dyn_cast<BinaryExpr>(expr))
        return tryFoldBinaryExpr(binaryExpr);
      else if (auto *unaryExpr = dyn_cast<PrefixUnaryExpr>(expr))
        return tryFoldUnaryExpr(unaryExpr);
      else if (auto *parenExpr = dyn_cast<ParenExpr>(expr))
        return tryFoldParenExpr(parenExpr);
      else if (auto *declRefExpr = dyn_cast<DeclRefExpr>(expr))
        return foldDeclRefExpr(declRefExpr);
      else
        return FoldingError(IllegalConstError::Default, expr->getLoc());
    }

    FoldingErrorOr<ConstantValuePtr>
    tryFoldLiteralExpression(const LiteralExpr *expr) {
      if (auto *intLiteralExpr = dyn_cast<IntegerLiteralExpr>(expr))
        return foldIntegerLiteralExpr(intLiteralExpr);
      else
        return FoldingError(IllegalConstError::Default, expr->getLoc());
    }

    ConstantValuePtr foldIntegerLiteralExpr(const IntegerLiteralExpr *expr) {
      auto exprType = expr->getType();
      auto value = expr->getValue();
      auto resultBitWidth = getIntegerBitWidth(exprType, Ctx);
      if (isSignedIntegerType(exprType))
        return std::make_unique<IntegerValue>(value.sextOrTrunc(resultBitWidth),
                                              true);
      else
        return std::make_unique<IntegerValue>(value.zextOrTrunc(resultBitWidth),
                                              false);
    }

    FoldingErrorOr<ConstantValuePtr> tryFoldBinaryExpr(const BinaryExpr *expr) {
      if (!expr->getType()->isStdlibInteger())
        return FoldingError(IllegalConstError::TypeNotSupported,
                            expr->getStartLoc());
      if (!supportedOperator(expr))
        return FoldingError(IllegalConstError::UnsupportedBinaryOperator,
                            expr->getStartLoc());

      if (const auto &lhsOrErr = getConstantValueOrErrorFor(expr->getLHS())) {
        if (const auto &rhsOrErr = getConstantValueOrErrorFor(expr->getRHS())) {
          auto lhsIntPtr = cast<IntegerValue>(lhsOrErr->get());
          auto rhsIntPtr = cast<IntegerValue>(rhsOrErr->get());
          auto operatorDecl = expr->getCalledValue();
          auto operatorIdentifier = operatorDecl->getBaseName().getIdentifier();
          if (operatorIdentifier.isArithmeticOperator())
            return tryFoldIntegerBinaryArithmeticOperator(
                operatorIdentifier, expr->getLoc(), lhsIntPtr, rhsIntPtr);
          else if (operatorIdentifier.isBitwiseOperator() ||
                   operatorIdentifier.isShiftOperator())
            return tryFoldIntegerBinaryBitwiseOperator(
                operatorIdentifier, expr->getLoc(), lhsIntPtr, rhsIntPtr);
          else
            llvm_unreachable("Unsupported operator");
        } else
          return rhsOrErr.getError();
      } else
        return lhsOrErr.getError();
    }

    FoldingErrorOr<ConstantValuePtr>
    tryFoldUnaryExpr(const PrefixUnaryExpr *expr) {
      if (!expr->getType()->isStdlibInteger())
        return FoldingError(IllegalConstError::TypeNotSupported,
                            expr->getLoc());
      if (!supportedOperator(expr))
        return FoldingError(IllegalConstError::UnsupportedBinaryOperator,
                            expr->getLoc());

      const auto &operandOrErr = getConstantValueOrErrorFor(expr->getOperand());
      if (operandOrErr) {
        auto operatorIdentifier =
            expr->getCalledValue()->getBaseName().getIdentifier();
        return tryFoldIntegerUnaryArithmeticOperator(
            operatorIdentifier, expr->getLoc(),
            cast<IntegerValue>(operandOrErr->get()));
      } else
        return operandOrErr.getError();
    }

    FoldingErrorOr<ConstantValuePtr> tryFoldParenExpr(const ParenExpr *expr) {
      const auto &operandOrErr = getConstantValueOrErrorFor(expr->getSubExpr());
      if (operandOrErr) {
        auto *intValue = cast<IntegerValue>(operandOrErr->get());
        return ConstantValuePtr(std::make_unique<IntegerValue>(
            intValue->getValue(), intValue->getIsSigned()));
      } else
        return operandOrErr.getError();
    }

    FoldingErrorOr<ConstantValuePtr> foldDeclRefExpr(const DeclRefExpr *expr) {
      if (const VarDecl *varDecl = dyn_cast<VarDecl>(expr->getDecl())) {
        // For other `@const` or `@section` values, we expect
        // their initializer to be foldable. For other values which
        // have a default value, we attempt to fold the
        // corresponding initializer expression.
        if (varDecl->isConstValue() || varDecl->hasInitialValue())
          if (auto initExpr = varDecl->getParentInitializer())
            return tryFoldDeclRefInitializerExpr(initExpr, expr->getLoc());

        // Clang constants are imported as a ValueDecl
        // with simple getter returning a literal value.
        if (varDecl->hasClangNode() && !varDecl->isDynamic() &&
            !varDecl->isObjC() &&
            varDecl->getImplInfo().getReadImpl() == ReadImplKind::Get)
          if (auto accessor = varDecl->getAccessor(AccessorKind::Get))
            if (auto singleRetStmt = dyn_cast<ReturnStmt>(
                    accessor->getBody()->getSingleActiveStatement()))
              return tryFoldDeclRefInitializerExpr(singleRetStmt->getResult(),
                                                   expr->getLoc());
      }

      return FoldingError(IllegalConstError::OpaqueDeclRef, expr->getLoc());
    }

    FoldingErrorOr<ConstantValuePtr>
    tryFoldDeclRefInitializerExpr(const Expr *expr, SourceLoc referenceLoc) {
      bool previouslyFolded =
          Ctx.evaluator.hasCachedResult(ConstantFoldExpression{expr, &Ctx});
      // Request the init expression of this declaration to be
      // constant-folded.
      if (auto foldedLiteralExpr =
              dyn_cast<LiteralExpr>(swift::foldLiteralExpression(expr, &Ctx)))
        return tryFoldLiteralExpression(foldedLiteralExpr);
      // If this is the first time we have requested to constant-fold this
      // declaration's initializer and have failed to do so, emit a note
      // with a location of the declRef from which we initiated this query.
      else if (!previouslyFolded)
        return FoldingError(IllegalConstError::NonConstDeclRef, referenceLoc);
      return FoldingError(IllegalConstError::OpaqueDeclRef, referenceLoc);
    }

    FoldingErrorOr<ConstantValuePtr> tryFoldIntegerBinaryArithmeticOperator(
        Identifier operatorIdentifier, SourceLoc sourceLocation,
        const IntegerValue *lhsVal, const IntegerValue *rhsVal) {
      assert((lhsVal->getIsSigned() && rhsVal->getIsSigned()) ||
             (!lhsVal->getIsSigned() && !rhsVal->getIsSigned()));
      bool isSigned = lhsVal->getIsSigned();
      auto lhsInt = lhsVal->getValue();
      auto rhsInt = rhsVal->getValue();
      APInt result;
      bool overflow = false;
      if (operatorIdentifier.is("+"))
        result = isSigned ? lhsInt.sadd_ov(rhsInt, overflow)
                          : lhsInt.uadd_ov(rhsInt, overflow);
      else if (operatorIdentifier.is("-"))
        result = isSigned ? lhsInt.ssub_ov(rhsInt, overflow)
                          : lhsInt.usub_ov(rhsInt, overflow);
      else if (operatorIdentifier.is("*"))
        result = isSigned ? lhsInt.smul_ov(rhsInt, overflow)
                          : lhsInt.umul_ov(rhsInt, overflow);
      else if (operatorIdentifier.is("/")) {
        if (rhsInt == 0)
          return FoldingError(IllegalConstError::DivideByZero, sourceLocation);
        result =
            isSigned ? lhsInt.sdiv_ov(rhsInt, overflow) : lhsInt.udiv(rhsInt);
      } else if (operatorIdentifier.is("%")) {
        if (rhsInt == 0)
          return FoldingError(IllegalConstError::DivideByZero, sourceLocation);
        if (isSigned) // Check for overflow
          auto divResult = lhsInt.sdiv_ov(rhsInt, overflow);
        result = isSigned ? lhsInt.srem(rhsInt) : lhsInt.urem(rhsInt);
      } else
        return FoldingError(IllegalConstError::UnsupportedBinaryOperator,
                            sourceLocation);

      if (overflow)
        return FoldingError(IllegalConstError::IntegerOverflow, sourceLocation);

      return ConstantValuePtr(std::make_unique<IntegerValue>(result, isSigned));
    }

    FoldingErrorOr<ConstantValuePtr> tryFoldIntegerBinaryBitwiseOperator(
        Identifier operatorIdentifier, SourceLoc sourceLocation,
        const IntegerValue *lhsVal, const IntegerValue *rhsVal) {
      assert((lhsVal->getIsSigned() && rhsVal->getIsSigned()) ||
             (!lhsVal->getIsSigned() && !rhsVal->getIsSigned()));
      bool isSigned = lhsVal->getIsSigned();
      auto lhsInt = lhsVal->getValue();
      auto rhsInt = rhsVal->getValue();
      APInt result;
      if (operatorIdentifier.is("&"))
        result = lhsInt & rhsInt;
      else if (operatorIdentifier.is("|"))
        result = lhsInt | rhsInt;
      else if (operatorIdentifier.is("^"))
        result = lhsInt ^ rhsInt;
      else if (operatorIdentifier.is("<<"))
        result = lhsInt << rhsInt;
      else if (operatorIdentifier.is(">>"))
        result = lhsInt.lshr(rhsInt);
      else
        return FoldingError(IllegalConstError::UnsupportedBinaryOperator,
                            sourceLocation);

      return ConstantValuePtr(std::make_unique<IntegerValue>(result, isSigned));
    }

    FoldingErrorOr<ConstantValuePtr>
    tryFoldIntegerUnaryArithmeticOperator(Identifier operatorIdentifier,
                                          SourceLoc sourceLocation,
                                          const IntegerValue *operandVal) {
      APInt result;
      auto operand = operandVal->getValue();
      bool overflow = false;
      if (operatorIdentifier.is("-")) {
        APInt zero = APInt(operand.getBitWidth(), 0);
        result = operandVal->getIsSigned() ? zero.ssub_ov(operand, overflow)
                                           : zero.usub_ov(operand, overflow);
      } else if (operatorIdentifier.is("+"))
        result = operand;
      else if (operatorIdentifier.is("~"))
        result = ~operand;
      else
        return FoldingError(IllegalConstError::UnsupportedUnaryOperator,
                            sourceLocation);

      if (overflow)
        return FoldingError(IllegalConstError::IntegerOverflow, sourceLocation);

      return ConstantValuePtr(
          std::make_unique<IntegerValue>(result, operandVal->getIsSigned()));
    }
  };

  Expr *createIntegerLiteralExpr(const Expr *foldedExpr,
                                 const ConstantValue *result) {
    assert(isa<IntegerValue>(result));
    auto intResult = cast<IntegerValue>(result)->getValue();
    auto resultType = foldedExpr->getType();
    assert(resultType->isStdlibInteger());
    bool isSigned = isSignedIntegerType(resultType);

    SmallString<32> resultStr;
    // Get the absolute value for a signed integer
    // because it is represented as a 'negative value' on the resulting
    // `IntegerLiteralExpr`.
    if (isSigned)
      intResult.abs().toString(resultStr, 10, true);
    else
      intResult.toString(resultStr, 10, false);

    auto *newLit = new (Ctx) IntegerLiteralExpr(
        Ctx.getIdentifier(resultStr).str(), foldedExpr->getLoc(),
        /*implicit*/ true);
    newLit->setType(resultType);
    newLit->setImplicit();
    newLit->setBuiltinInitializer(getIntTypeBuiltinInit(resultType, Ctx));
    if (isSigned && intResult.slt(0))
      newLit->setNegative(foldedExpr->getLoc());
    return newLit;
  }

  void emitFoldingErrorDiagnostic(const FoldingError &foldingError) {
    diagnoseError(foldingError.sourceLocation, foldingError.code, Ctx.Diags);
  }
};
} // anonymous namespace

Expr *swift::foldLiteralExpression(const Expr *expr, ASTContext *ctx) {
  return evaluateOrDefault(ctx->evaluator, ConstantFoldExpression{expr, ctx},
                           {});
}

Expr *ConstantFoldExpression::evaluate(Evaluator &evaluator, const Expr *expr,
                                       ASTContext *ctx) const {
  if (ctx->LangOpts.hasFeature(Feature::LiteralExpressions)) {
    ConstantFolder folder(*ctx);
    if (auto result = folder.fold(expr))
      return result;
  }
  return const_cast<Expr *>(expr);
}
